(in-package :lispkit)
;; Connect gtk class slots

(defmethod initialize-instance :after ((tab tab) &key)
  "Transform view slot's initarg ':initial-uri' to a view
Also connect view slot to scroll slot"
  (setf (tab-view tab)
        (make-instance 'webkit-webview :uri (tab-view tab)))
  (add (tab-scroll tab) (tab-view tab)))

(defmethod initialize-instance :after ((ui-views ui-views) &key)
  "Hide scrollbars on all ui-views slots
thus in WebKit1, allow any height in a ':shrink nil' vpane"
  (mapcar (lambda (ui-tab)
            (webview-hide-scrollbars
             (tab-view ui-tab)
             (tab-scroll ui-tab)))
          (get-all-slot-values ui-views)))

(defmacro proxy-overlay-mouse-events (overlay-content
                                      find-dest-widget
                                      &rest events)
  "Re-route/proxy button events from the transparent overlay
to what is shown underneath, determined by `find-dest-widget'"
  (append '(progn)
          (loop for event in events collect
               (let ((event-str (symbol-to-string event))
                     (event-cb (prepend-string-on-to-symbol
                                "overlay-content/" event)))
                 `(progn
                    (defcallback ,event-cb :boolean
                        ((widget pobject)
                         (event :pointer))
                      (g-object-cffi:g-signal-emit-by-name
                       (funcall ,find-dest-widget widget)
                       ,event-str
                       event g-object-cffi:*g-signal-emit-ret*)
                      t)
                    (setf (gsignal ,overlay-content ,event-str)
                          (callback ,event-cb)))))))

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
                  (find-instance 'of-browser 'from-hints-view
                                 widget))))
     button-press-event
     button-release-event
     scroll-event
     motion-notify-event ; cursor doesn't change icon on hover
     enter-notify-event
     leave-notify-event
     key-press-event
     key-release-event
     drag-begin
     drag-data-delete
     drag-data-get
     drag-data-received
     drag-drop
     drag-end
     drag-failed
     drag-leave
     drag-motion)

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

    ;; HACK: the webkit minimal height patch must be applied
    ;; Once the patch is applied the following should be 0
    ;; (preferred-width (ui-tabs (browser-ui (current-browser))))
    ;; (preferred-height (ui-tabs (browser-ui (current-browser))))
    (setf
     (size-request (tab-scroll (ui-status ui))) '(-1 16)
     (size-request (tab-scroll (ui-tabs ui))) '(-1 16))

    ;; Connect signals to notebook
    (connect-gtk-notebook-signals notebook)

    ;; NOTE: Operate on SoupSession before tabs are created
    (setf (property (webkit-get-default-session) :proxy-uri)
          (soup-binding:soup-uri-new "http://127.0.0.1:8123/"))

    (let ((initial-uris (browser-tabs browser))) ; :initial-tabs
      (setf (browser-tabs browser) nil)
      (mapcar (lambda (uri)
                ;; Switch to each new tab as created
                (tab-new browser uri :background nil))
              initial-uris))

    (show gtk-win :all t)

    ;; Needs to be shown to get the window xid from x11
    (setf (widgets-x11-xic (browser-gtk browser))
          (create-xic (gtk-widget-get-window gtk-win)))

    (connect-gtk-window-signals gtk-win)))
