(in-package :lispkit)
;; Connect gtk class slots

(defmethod initialize-instance :after ((tab tab) &key)
  "Transform view slot's initarg ':initial-uri' to a view
Also connect view slot to scroll slot"
  (setf (tab-view tab)
        (make-instance 'webkit-webview :uri (tab-view tab)))
  (add (tab-scroll tab) (tab-view tab)))

(defmethod initialize-instance :after ((ui-views ui-views) &key)
  "Directly access the tab-view and tab-scroll of all ui-view slots
in order to hide scrollbars; thus in WebKit1, allow any height in a shrink nil vpane"
  (mapcar (lambda (ui-tab)
            (webview-hide-scrollbars
             (tab-view ui-tab)
             (tab-scroll ui-tab)))
          (get-all-slot-values ui-views)))

(defun proxy-overlay-mouse-events (overlay-content find-dest-widget)
  "Re-route/proxy button events from the transparent overlay
to what is shown underneath, determined by #'find-dest-widget"
  (labels ((prepend-for-scope (str)
             (as-symbol (concatenate 'string
                                     "overlay-content/"
                                     str)))
           (generate-and-set-cb (event-name)
             (eval `(progn
                      (defcallback ,(prepend-for-scope event-name) :boolean
                          ((widget pobject)
                           (event :pointer))
                        (g-object-cffi:g-signal-emit-by-name
                         (funcall ,find-dest-widget widget)
                         ,event-name
                         event g-object-cffi:*g-signal-emit-ret*)
                        t)
                      (setf (gsignal ,overlay-content ,event-name)
                            (callback ,(prepend-for-scope event-name)))))))
    (mapcar #'generate-and-set-cb
            '("button-press-event"
              "button-release-event"
              "scroll-event"
              "motion-notify-event" ; cursor doesn't change icon on hover
              "enter-notify-event"
              "leave-notify-event"
              "key-press-event"
              "key-release-event"
              "drag-begin"
              "drag-data-delete"
              "drag-data-get"
              "drag-data-received"
              "drag-drop"
              "drag-end"
              "drag-failed"
              "drag-leave"
              "drag-motion"))))

(defmethod initialize-instance :after ((browser browser) &key)
  "Pack the widgets, created in the initforms"
  ;; Callbacks depend on *browsers*, so add to it here
  (setf *browsers* (append *browsers*
                           (list browser)))
  (setf *browser-current-index* (position browser *browsers*))
  (let* ((gtk (browser-gtk browser))
         (pane1 (widgets-pane1 gtk))
         (pane2 (widgets-pane2 gtk))
         (ui (browser-ui browser))
         (notebook (widgets-notebook gtk))
         (gtk-win (widgets-window gtk)))

    (proxy-overlay-mouse-events
     (tab-view (ui-hints ui))
     (lambda (widget)
       (tab-view (current-tab
                  (find-instance 'of-browser 'from-hints-view widget)))))

    ;; Overlay
    (webkit-web-view-set-transparent (tab-view (ui-hints ui)) t)
    (widget-set-rgba (widgets-window gtk))
    (add (widgets-overlay gtk)
         (widgets-notebook gtk))
    (gtk-overlay-add-overlay (widgets-overlay gtk)
                             (tab-scroll (ui-hints ui)))

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 (tab-scroll (ui-tabs ui)) :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 (widgets-overlay gtk) :resize t :shrink t)
    (pack pane2 (tab-scroll (ui-status ui)) :resize nil :shrink nil)
    (add gtk-win pane1)

    ;; maybe small problem with growing the bottom beyond the natural page height
    (setf
     (size-request (tab-scroll (ui-status ui))) '(-1 16)
     (size-request (tab-scroll (ui-tabs ui))) '(-1 16))
    ;; related but ineffective :height-request 10 :min-content-height 10
    ;; HACK: fixed from Patch!
    ;; (preferred-width (ui-tabs (browser-ui (current-browser))))
    ;; (preferred-height (ui-tabs (browser-ui (current-browser))))
    ;; => 0

    ;; Connect signals to notebook
    (connect-gtk-notebook-signals notebook)

    (let ((initial-uris (browser-tabs browser))) ; :initial-tabs uri list
      (setf (browser-tabs browser) nil)
      (mapcar (lambda (uri)
                (tab-new browser uri
                         :background nil)) ; Switch to each new tab as created
              initial-uris))

    (show gtk-win :all t)

    ;; Needs to be shown to get the window xid from x11
    (setf (widgets-x11-xic (browser-gtk browser))
          (create-xic (gtk-widget-get-window gtk-win)))

    ;; TODO: fix this from erring
    ;; This, by webkit, happens to be instance wide, not per view
    ;; (setf (property (webkit-get-default-session) :proxy-uri)
    ;;       (soup-uri-new "http://127.0.0.1:8123/"))

    (connect-gtk-window-signals gtk-win)))
