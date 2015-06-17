(in-package :lispkit)

;; Manage key events
(defun active-map-names-to-kmaps (names)
  "dereference keymap names"
  (loop for name in names
     collect (getf *maps* name)))

(defun keys-actions-invoke (kmap-names key browser)
  "Run commands for the key"
  (let* ((kmaps (active-map-names-to-kmaps kmap-names))
         (command (lookup-keys kmaps
                               (append (key-buffer
                                        (browser-key-state browser))
                                       (listify key)))))
    (if command
        (run-hook :key-non-default-action browser)
        (setf command (lookup-keys kmaps t)))
    (if command
        (funcall command browser key)
        (dmesg "Key pressed in a map with no default mapping"))))

;; Window Events
(defcallback on-key-press :boolean
    ((win pobject)
     (gdk-event :pointer))
  (let* ((browser (find-instance 'of-browser 'from-window win))
         (kstate (browser-key-state browser))
         (maps (if (passthrough-state kstate)
                   '(:passthrough) ; Catch key to turn off passthrough
                   (append '(:passthrough)
                           (active-maps kstate))))
         (key (process-gdk-event->key
               gdk-event
               (widgets-x11-xic (browser-gtk browser)))))
    (when key ; XIM may have filtered the event
      (dmesg key)
      ;; Invoke actions
      (keys-actions-invoke maps key browser))
    (null (passthrough-state kstate)))) ; stop propagation

(defcallback on-key-release :boolean
    ((win pobject)
     (event :pointer))
  (declare (ignore event))
  ;; Define this only to not let keys escape to the webviews
  (null ; returning true stops propagation of the event
   (passthrough-state
    (browser-key-state
     (find-instance 'of-browser 'from-window win)))))

(defcallback exit :void
    ((window pobject))
  (let ((b (find-instance 'of-browser 'from-window window)))
    ;; Don't spawn a new tab when closing
    (setf (browser-always-one-tab b) nil)
    (mapcar (lambda (tab) (tab-remove b tab))
            (browser-tabs b))
    (destroy window)
    (setf *browsers* (remove b *browsers*))
    (unless *browsers* ; TODO: also if not using slime
      (leave-gtk-main))))

(defcallback on-focus-in :boolean
    ((widget pobject)
     (event :pointer))
  (declare (ignore event))
  ;; (x11-binding:xic-focus (widgets-x11-xic (browser-gtk b)))
  (let ((b (find-instance 'of-browser 'from-window widget)))
    (setf *browser-current-index* (position b *browsers*)))
  (dmesg *browser-current-index*)
  nil)

;; (defcallback on-focus-out :boolean
;;     ((widget pobject)
;;      (event :pointer))
;;   (declare (ignore event))
;; nil)
;; (gsignal gtk-window "focus-out-event")
;; (callback on-focus-out)

(defun connect-gtk-window-signals (gtk-win)
  "Connect the signals for the window widget"
  (setf (gsignal gtk-win "key-press-event")
        (callback on-key-press)

        (gsignal gtk-win "key-release-event")
        (callback on-key-release)

        (gsignal gtk-win "focus-in-event")
        (callback on-focus-in)

        (gsignal gtk-win "destroy")
        (callback exit)))
