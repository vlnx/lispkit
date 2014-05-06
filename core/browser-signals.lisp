(in-package :lispkit)

;; Manage keyevents
(defun active-map-names-to-kmaps (names)
  "input from (active-maps kstate) => '(:top :other)
output => list of kmaps"
  (mapcar (lambda (name)
            (let ((n (getf *maps* name)))
              (if n
                  n
                  (error "Keymap name doesn't exist"))))
          names))

(defun keys-actions-invoke (kmap-names key browser)
  "Check for default events, extracted from main func; refactor?"
  (let* ((kmaps (active-map-names-to-kmaps kmap-names))
         (command (handle-keymap kmaps key))
         default-command)
    (if command
        (run-hook :key-action
                  browser key
                  :command command)
        (progn
          (setf default-command (handle-keymap kmaps key t))
          (if default-command
              (run-hook :key-action
                        browser key
                        :default-command default-command))))))
;; (print "Key pressed in a map with no default mapping")

;; Window Events
(defcallback on-key-press :boolean
  ((widget :pointer)
   (gdk-event :pointer))
  (let* ((browser (browser-find-instance widget :of 'browser :from 'widget))
         (kstate (browser-key-state browser))
         (key (process-gdk-event->key
               gdk-event
               (widgets-x11-xic (browser-gtk browser)))))
    (when key ;; XIM may have filtered the event
      (print key) (finish-output)
      ;; Find actions to invoke, give pasthrough priority
      (keys-actions-invoke (if (passthrough-state kstate)
                               '(:passthrough) ;; Only catch key to turn it off
                               (append
                                '(:passthrough) ;; First check if to turn it on
                                (active-maps kstate)))
                           key browser))
    (null (passthrough-state kstate)))) ;; returning true stops propagation of the event

(defcallback on-key-release :boolean
  ((widget :pointer)
   (event :pointer))
  (declare (ignore event))
  ;; Define this only to not let keys escape to the webviews
  (null ;; returning true stops propagation of the event
   (passthrough-state
    (browser-key-state
     (browser-find-instance widget :of 'browser :from 'widget)))))

(defcallback exit :void
  ((window pobject))
  (let ((b (browser-find-instance window
                                  :of 'browser
                                  :from 'window)))
    (setf *browsers* (remove b *browsers*)))
  ;; If last instance and not in slime (leave-gtk-main))
  (destroy window)) ;; note: Free each view and widget slot?

;; Reset IC as well?
(defcallback on-focus-in :boolean
    ((widget pobject)
     (event :pointer))
  (declare (ignore event))
  ;; (x11-binding::xic-focus ;;  (widgets-x11-xic ;;   (browser-gtk ;;  t)
  (let ((b (browser-find-instance widget
                                  :of 'browser
                                  :from 'window)))
    (setf *browser-current-index* (position b *browsers*)))
  nil)

;; (defcallback on-focus-out :boolean
;;     ((widget pobject)
;;      (event :pointer))
;;   (declare (ignore event))
;; nil)

(defun connect-gtk-window-signals (gtk-win)
  "Connect the signals for the window widget"
  (setf (gsignal gtk-win "key-press-event")
        (callback on-key-press)

        (gsignal gtk-win "key-release-event")
        (callback on-key-release)

        (gsignal gtk-win "focus-in-event")
        (callback on-focus-in)

        ;; (gsignal gtk-window "focus-out-event")
        ;; (callback on-focus-out)

        (gsignal gtk-win "destroy")
        (callback exit)))
