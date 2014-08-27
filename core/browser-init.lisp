(in-package :lispkit)
;; Connect gtk class slots

(defmethod initialize-instance :after ((tab tab) &key)
  "Transform view slot's initarg ':inital-uri' to a view
Also connect view slot to scroll slot"
  (setf (tab-view tab)
        (make-instance 'webkit-webview :uri (tab-view tab)))
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

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 (tab-scroll (ui-tabs ui)) :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 notebook :resize t :shrink t)
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

    (let ((inital-uris (browser-tabs browser))) ;; :initial-tabs uri list
      (setf (browser-tabs browser) nil)
      (mapcar (lambda (uri)
                (tab-new browser uri
                         :background nil)) ;; Switch to each new tab as created
              inital-uris))

    (show gtk-win :all t)

    ;; Needs to be shown to get the window xid from x11
    (setf (widgets-x11-xic (browser-gtk browser))
          (create-xic (gtk-widget-get-window gtk-win)))

    (connect-gtk-window-signals gtk-win)))
