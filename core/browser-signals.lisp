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
  ;; TODO: Clean up close window's instance
  ;; check each instance in *windows* for win slot to match this arg
  ;; If last instance and not in slime (leave-gtk-main))
  (destroy window))

(defun connect-gtk-window-signals (gtk-win)
  "Connect the signals for the window widget"
  (setf (gsignal gtk-win "key-press-event")
        (callback on-key-press)

        (gsignal gtk-win "key-release-event")
        (callback on-key-release)
        ;; (gsignal gtk-window "focus-in-event") (callback on-focus-in)
        ;; (gsignal gtk-window "focus-out-event") (callback on-focus-out)
        (gsignal gtk-win "destroy")
        (callback exit)))
;; Reset IC as well?
;; (defcallback on-focus-in :boolean
;;     ((widget :pointer)
;;      (event :pointer))
;;   (declare (ignore event))
;;   (x11-binding::xic-focus
;;    (widgets-x11-xic
;;     (browser-gtk
;;      (lispkit::browser-find-instance-from :widget widget)))
;;    t)
;;   nil)
;; (defcallback on-focus-out :boolean
;;     ((widget :pointer)
;;      (event :pointer))
;;   (declare (ignore event))
;;   (x11-binding::xic-focus
;;    (widgets-x11-xic
;;     (browser-gtk
;;      (lispkit::browser-find-instance-from :widget widget)))
;;    nil)
;;   nil)


;; Notebook Signals

(defcallback notebook-page-added :void
    ((notebook :pointer)
     (child-widget :pointer)
     (page-num :int))
  (declare (ignore notebook child-widget page-num))
  ;; (ui-update :update-tab-list t)
  ;; :statusbar-tabs-count
  )
(defcallback notebook-switch-page :void
    ((notebook :pointer)
     (child-widget pobject)
     (page-num :int))
  (declare (ignore notebook))
  (setf (browser-tabs-current-index (current-browser)) page-num)
  (let ((switched-to-tab (browser-find-instance child-widget
                                                :of 'tab
                                                :from 'scrolled-window)))
    (ui-update (current-browser)
               :tabs-switched-page page-num
               :uri (tab-view switched-to-tab))))
;; :update-tab-list t)
;; :statusbar-tabs-count
;; :window-title t
;; :uri t
;; :progress t
(defun connect-gtk-notebook-signals (notebook)
  "Connect the signals for the notebook widget"
  (setf (gsignal notebook "page-added")
        (callback notebook-page-added)

        (gsignal notebook "switch-page")
        (callback notebook-switch-page)
        ))
