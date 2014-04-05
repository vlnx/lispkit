(in-package :lispkit)

(defclass tab ()
  ((view  :accessor tab-view
          :initarg :inital-uri
          :documentation "initialy a uri, translated to a new webview")
   (scroll :accessor tab-scroll
           :initform (make-instance 'scrolled-window)
           :documentation "it's scrolled window container"))
  (:documentation "Only for WebKit1, create a container and add it"))

(defclass ui-views ()
  ((tabs 
    :initform (make-instance 'tab :inital-uri (ui-symbol-to-uri 'tabs))
    :documentation "Tabs view")
   (status 
    :initform (make-instance 'tab :inital-uri (ui-symbol-to-uri 'status))
    :documentation "Status bar view")))

;; Wrapper reader functions for 'tab instances
(defun ui-tabs (ui &optional (slot-name 'view))
  "Quick access to slots of tab instances"
  (slot-value (slot-value ui 'tabs) slot-name))
(defun ui-status (ui &optional (slot-name 'view))
  "Quick access to slots of tab instances"
  (slot-value (slot-value ui 'status) slot-name))

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
   (notebook :accessor widgets-notebook
             :initform (make-instance 'notebook
                                      :show-tabs nil
                                      :show-border nil)
             :documentation "Notebook, allows for tabs")
   (x11-xic :accessor widgets-x11-xic
            :initform nil
            :documentation "Hold x11's XIC reference to decode input keys")))

(defclass key-state-class ()
  ((active-maps :accessor active-maps
                :initform '(:top)
                :documentation "Active kmaps for the browser")
   (passthrough-state :accessor passthrough-state
                      :initform nil
                      :documentation "Current passthrough state")))

(defclass browser ()
  ((ui :accessor browser-ui
       :initform (make-instance 'ui-views)
       :documentation "The class for the view instances used in the interface")
   (tabs :accessor browser-tabs ;; List of tab instances
         :initarg :inital-tabs
         :initform (list *uri-homepage*)
         :documentation "Give initial tabs, then will be replaced with tab instances")
   (gtk :accessor browser-gtk
        :initform (make-instance 'gtk-widgets)
        :documentation "widgets used in the window")
   (key-state :accessor browser-key-state
              :initform (make-instance 'key-state-class)
              :documentation "The state of current keymap for the window"))
  (:documentation "Starts an instance of the browser in a toplevel x11 window"))

(defun browser-views (b)
  "access tab views"
  (mapcar (lambda (tab)
            (tab-view tab))
          (browser-tabs b)))

(defun current-tab (&optional (browser (current-browser)))
  "Maybe optimize, keep track of index, so don't have to cffi it each time"
  (tab-view (nth 
             (notebook-current-tab-index
              (widgets-notebook (browser-gtk browser)))
             (browser-tabs browser))))

;; (defun (setf browser-tabs) (browser new-tab-list)
;;   ;; diff -> already there, new, deleted
;;   )
