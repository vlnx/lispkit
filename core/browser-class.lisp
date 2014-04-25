(in-package :lispkit)

(defclass inspector ()
  ((view  :accessor inspector-view
          :initarg :view
          :documentation "the view for the inspector")
   (inspector-pointer :accessor inspector-pointer
                      :initarg :pointer
                      :documentation "the object for the inspector")
   (window :accessor inspector-window
           :initform (make-instance 'window
                                    :width 800 :height 600
                                    :title "LispKit - Inspector"
                                    :has-resize-grip nil)
           :documentation "the toplevel window"))
  (:documentation "Contain an inspector's refs"))

(defclass tab ()
  ((view  :accessor tab-view
          :initarg :inital-uri
          :documentation "initialy a uri, translated to a new webview")
   (scroll :accessor tab-scroll
           :initform (make-instance 'scrolled-window)
           :documentation "it's scrolled window container")
   (inspector :accessor tab-inspector
              :initform nil
              :documentation "inspector class for the tab"))
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
   (tabs :accessor browser-tabs
         :initarg :inital-tabs
         :initform (list *uri-homepage*)
         :documentation "Give initial tabs, then will be replaced with
                         the list of active tab instances")
   (tabs-current-index :accessor browser-tabs-current-index
                       :initform 0
                       :documentation "Current tab index")
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

(defun current-tab (&optional (slot 'view) (browser (current-browser)))
  "Maybe optimize, keep track of index, so don't have to cffi it each time"
  (slot-value (nth
               (browser-tabs-current-index browser)
               (browser-tabs browser)) slot))

;; (defun (setf browser-tabs) (browser new-tab-list)
;;   ;; diff -> already there, new, deleted
;;   )

(defvar *window* nil)
(defun current-browser () *window*)
(defun browsers () (list (current-browser)))
(defun browser-all-tabs (b)
  (concatenate 'list
               (list
                (slot-value (browser-ui b) 'tabs)
                (slot-value (browser-ui b) 'status))
               (browser-tabs b)))

(defun browser-find-instance (widget &key of from)
  "When a view or other widget is passed in a callback argument,
   find it's browser instance
 (browser-find-instance :of 'browser :from 'view widget)
 (browser-find-instance :of 'tab :from 'scrolled-window widget)"
  (let (source ;; List of items to filter
        this-test) ;; Given each source item, will return the first true
    (cond
      ((and (eq of 'browser)
            (eq from 'view))
       (setf source (browsers)
             this-test
             (lambda (browser)
               (let ((views (concatenate 'list
                                         (list
                                          (ui-tabs (browser-ui browser))
                                          (ui-status (browser-ui browser)))
                                         (browser-views browser))))
                 (when (member widget views)
                   browser)))))
      ((and (eq of 'browser)
            (eq from 'widget))
       (setf source (browsers)
             this-test
             (lambda (browser)
               ;; maybe fix, test each gtk widget in the instance
               browser)))
      ((and (eq of 'inspector)
            (eq from 'inspector-pointer))
       (setf source (first (mapcar ;; fix for multiple
                            #'browser-all-tabs
                            (browsers)))
             this-test (lambda (tab)
                         (when (and (tab-inspector tab)
                                    ;; (pointer-eq (if (null-pointer-p
                                    ;;                  (pointer widget))
                                    ;;                 widget
                                    ;;                 (pointer widget))
                                    (pointer-eq (if (pointerp widget)
                                                    widget
                                                    (pointer widget))
                                                (inspector-pointer
                                                 (tab-inspector tab))))
                           (tab-inspector tab)))))
      ((and (eq of 'tab)
            (eq from 'inspector-window))
       (setf source (first (mapcar ;; fix for multiple
                            #'browser-all-tabs
                            (browsers)))
             this-test (lambda (tab)
                         (when (and (tab-inspector tab)
                                    (eq widget
                                        (inspector-window (tab-inspector tab))))
                           tab))))
      ((and (eq of 'tab)
            (eq from 'view))
       (setf source (first (mapcar ;; fix for multiple
                            #'browser-all-tabs
                            (browsers)))
             this-test (lambda (tab)
                         (when (eq widget
                                   (tab-view tab))
                           tab))))
      ((and (eq of 'tab)
            (eq from 'scrolled-window))
       (setf source (first (mapcar #'browser-tabs ;; fix for multiple
                                   (browsers)))
             this-test (lambda (tab)
                         (when (eq widget
                                   (tab-scroll tab))
                           tab))))
      (t (error "Requested argument are not impemented")))
    (first (delete-if #'null (mapcar this-test source)))))
