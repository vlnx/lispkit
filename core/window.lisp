(in-package :lispkit)

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
  (let* ((browser (browser-find-instance-from :widget widget))
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
  ;; Define this only to not let keys escape to the webviews
  (null ;; returning true stops propagation of the event
   (passthrough-state
    (browser-key-state
     (browser-find-instance-from :widget widget)))))

(defcallback exit :void
    ((window pobject))
  (declare (ignore window))
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

(defmethod initialize-instance :after ((tab tab) &key)
  (setf (tab-view tab) ;; Transform inital uri to view
        (webview-new (tab-view tab)))
  (add (tab-scroll tab) (tab-view tab)))

(defmethod initialize-instance :after ((ui-views ui-views) &key)
  "Directly access the tab-view and tab-scroll of all ui-view slots
in order to hide scrollbars; thus in WebKit1, allow any height in a shrink nil vpane"
  (mapcar (lambda (slot-name)
            (webview-hide-scrollbars
             (tab-view (slot-value ui-views slot-name))
             (tab-scroll (slot-value ui-views slot-name))))
          '(tabs status)))

(defmethod initialize-instance :after ((browser browser) &key)
  "Pack the widgets, created in the initforms"
  (let* ((gtk (browser-gtk browser))
         (pane1 (widgets-pane1 gtk))
         (pane2 (widgets-pane2 gtk))
         (ui (browser-ui browser))
         (notebook (widgets-notebook gtk))
         (gtk-win (widgets-window gtk)))

    ;; Create tab instances
    (setf (browser-tabs browser)
          (mapcar (lambda (uri)
                    (make-instance 'tab :inital-uri uri))
                  (browser-tabs browser))) ;; :initial-tabs uri list

    ;; Show all tab containers, in order to be added to the notebook
    (mapcar (lambda (tab)
              (show (tab-scroll tab)))
            (browser-tabs browser))

    ;; Add tabs to notebook
    (mapcar (lambda (tab)
              (notebook-add-tab notebook (tab-scroll tab)))
            (browser-tabs browser))
    
    ;; Select starting index of the notebook
    (setf (notebook-current-tab-index notebook) 0)

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 (ui-tabs ui 'scroll) :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 notebook :resize t :shrink t)
    (pack pane2 (ui-status ui 'scroll) :resize nil :shrink nil)
    (add gtk-win pane1)

    ;; maybe small problem with growing the bottom beyond the natural page height
    (setf
     (size-request (ui-status ui 'scroll)) '(-1 16)
     (size-request (ui-tabs ui 'scroll)) '(-1 16))
    ;; related but ineffective :height-request 10 :min-content-height 10
    ;; HACK: fixed from Patch!
    ;; (preferred-width (ui-tabs (browser-ui (current-browser))))
    ;; (preferred-height (ui-tabs (browser-ui (current-browser))))
    ;; => 0

    (show gtk-win :all t)

    ;; Needs to be shown to get the window xid from x11
    (setf (widgets-x11-xic (browser-gtk browser))
          (create-xic (gtk-widget-get-window gtk-win)))

    (connect-gtk-window-signals gtk-win)))

(defun browser-find-instance-from (&key widget view)
  "When a view or other widget is passed in a callback argument,
find it's browser instance"
  ;; todo: also take expected widget like gtk-window, and loop over just those
  ;; note: update when multipe windows are supported loop over *windows*
  (cond
    (view
     (if (member view
                 (concatenate 'list
                              (list
                               (ui-tabs (browser-ui (current-browser)))
                               (ui-status (browser-ui (current-browser))))
                              (browser-views (current-browser))))
         (current-browser)
         (error "Unattached view was found")))
    (widget
     (current-browser))))


(defvar *window* nil)
(defun current-browser () *window*)

(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (gdk-threads-init)
  (within-main-loop
    (setf *window* (make-instance 'browser
                                  :inital-tabs
                                  (list *uri-homepage* "http://10.1.7.1/")))))
