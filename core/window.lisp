(in-package :lispkit)

(defclass ui-views ()
  ((tabs :accessor ui-tabs
         :initform (webview-new (ui-symbol-to-uri 'tabs))
         :documentation "Tabs view")
   (status :accessor ui-status
           :initform (webview-new (ui-symbol-to-uri 'status))
           :documentation "Status bar view")
   (hints :accessor ui-hints
          ;; :initform (webview-new (ui-symbol-to-uri 'hints))
          :documentation "Currently disabled, overlay view for follow hints")))

(defclass gtk-widgets ()
  ((window :accessor widgets-window
           :initform (make-instance 'window
                                    :width 800 :height 600
                                    :title "LispKit"
                                    :has-resize-grip nil)
           :documentation "Toplevel window")
   (pane1 :accessor widgets-pane1
          :initform (make-instance 'v-paned)
          :documentation "Top pane containing the tabs and the other pane")
   (pane2 :accessor widgets-pane2
          :initform (make-instance 'v-paned)
          :documentation "Bottom pane containing the page and status bar")
   (ui-tabs-scroll :accessor widgets-tabs
                   :initform (make-instance 'scrolled-window
                                            :height-request 10
                                            :min-content-height 10)
                   :documentation "Container for ui-tabs")
   (ui-status-scroll :accessor widgets-status
                     :initform (make-instance 'scrolled-window)
                     :documentation "Container for ui-status")
   (notebook :accessor widgets-notebook
             :initform (make-instance 'notebook)
             :documentation "Notebook, allows for tabs")
   (pages-views :accessor widgets-pages-views
                :initform (make-instance 'scrolled-window)
                :documentation "should be a dynamic list of scrolled window containers")
   (x11-xic :accessor widgets-x11-xic
            :initform nil
            :documentation "Hold x11's XIC reference to decode input keys")))

(defclass browser ()
  ((ui :accessor browser-ui
       :initform (make-instance 'ui-views)
       :documentation "The class for the view instances used in the interface")
   (views :accessor browser-views
          :initarg :inital-tabs
          :initform (list *uri-homepage*)
          :documentation "Give initial tabs, then will be replaced with tab instances")
   (gtk :accessor browser-gtk
        :initform (make-instance 'gtk-widgets)
        :documentation "widgets used in the window"))
  (:documentation "Starts an instance of the browser in a toplevel window"))

(defmethod initialize-instance :after ((browser browser) &key)
  (let* ((gtk (browser-gtk browser))
         (pane1 (widgets-pane1 gtk))
         (pane2 (widgets-pane2 gtk))
         (ui-tabs-scroll (widgets-tabs gtk))
         (ui-status-scroll (widgets-status gtk))
         (notebook (widgets-notebook gtk))
         (gtk-win (widgets-window gtk))
         (only-tab (widgets-pages-views gtk)))

    ;; Connect scrolling widgets with their content
    (add ui-tabs-scroll
         (ui-tabs (browser-ui browser)))
    (add ui-status-scroll
         (ui-status (browser-ui browser)))

    (webview-hide-scrollbars
     (ui-tabs (browser-ui browser))
     ui-tabs-scroll)
    (webview-hide-scrollbars
     (ui-status (browser-ui browser))
      ui-status-scroll)


    ;; Incomplete tab stuff here
    ;; Open the first 'tab', latter set up dynamic scrolling widgets
    ;; Replace the initial 'views' slot with loaded URIs
    (setf (browser-views browser)
          (list (webview-new (first (browser-views browser)))))
    (add only-tab (first (browser-views browser)))
    (gtk-notebook-set-show-tabs notebook nil)
    (gtk-notebook-set-show-border notebook nil)
    (show only-tab)
    (gtk-notebook-set-current-page
     notebook
     (gtk-notebook-insert-page notebook
                               only-tab nil 0))

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 ui-tabs-scroll :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 notebook :resize t :shrink t)
    (pack pane2 ui-status-scroll :resize nil :shrink nil)

    (add gtk-win pane1)
    (setf (gsignal gtk-win "destroy") (callback exit))

    ;; Note: Small problem with growing the bottom beyond the natural page height
    (setf
     (size-request ui-status-scroll) '(-1 16)
     (size-request ui-tabs-scroll) '(-1 16))

     ;; XXX: fixed from Patch!
     ;; (preferred-width (ui-tabs (browser-ui (current-browser))))
     ;; (preferred-height (ui-tabs (browser-ui (current-browser))))
     ;; => 0

    (show gtk-win :all t)
    ;; Needs to be shown to get the window xid from x11
    (init-keyevents gtk-win (widgets-x11-xic gtk))))

(defcallback exit :void
    ((window pobject))
  (declare (ignore window))
  ;; Clean up close window's instance
  ;; check each instance in *windows* for win slot to match this arg
  (destroy window))
;; If last instance and not in slime (leave-gtk-main))

(defvar *window* nil)
(defun current-browser () *window*)
(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (gdk-threads-init)
  (within-main-loop
    (setf *window* (make-instance 'browser :inital-tabs (list *uri-homepage*)))))

(defun browser-find-view-s-instance (view)
  "When a view is passed in a callback argument, find it's instance"
  ;; note: update when multipe windows are supported
  ;; loop over *windows*/browsers
  (if (member view
              (concatenate 'list
                           (list
                            (ui-tabs (browser-ui (current-browser)))
                            (ui-status (browser-ui (current-browser))))
                           (browser-views (current-browser))))
      (current-browser)
      (error "Unattached view was found")))
