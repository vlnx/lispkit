(in-package :lispkit)

(defclass inspector ()
  ((view  :accessor inspector-view
          :initarg :view
          :documentation "the view for the inspector")
   (inspector-pointer :accessor inspector-pointer
                      :initarg :pointer
                      :documentation "the object for the inspector")
   (shown :accessor inspector-shown
          :initform nil
          :documentation "If the view is already added to the toplevel window and shown toplevel")
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
    :accessor ui-tabs
    :initform (make-instance 'tab :inital-uri (ui-symbol-to-uri 'tabs))
    :documentation "Tabs view")
   (status
    :accessor ui-status
    :initform (make-instance 'tab :inital-uri (ui-symbol-to-uri 'status))
    :documentation "Status bar view")))

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

(defun current-tab (&optional (browser (current-browser)))
  "Maybe optimize, keep track of index, so don't have to cffi it each time"
  (nth (browser-tabs-current-index browser)
       (browser-tabs browser)))

(defun current-browser ()
  "Use focus callbacks to index the current"
  (nth
   *browser-current-index*
   *browsers*))

(defun browser-all-tabs (b)
  "Return all tab instances including user interface views"
  (concatenate 'list
               (list
                (slot-value (browser-ui b) 'tabs)
                (slot-value (browser-ui b) 'status))
               (browser-tabs b)))

(defun browsers-all-tabs ()
  "Give the tabs of all browsers in a flat list"
  (apply #'concatenate 'list ;; Flatten a list with a depth of '(()()())
         (mapcar #'browser-all-tabs *browsers*)))

(defun browser-find-instance-get-source (requested-instance-type)
  (case requested-instance-type
    (browser *browsers*)
    (inspector (mapcar #'tab-inspector (browsers-all-tabs)))
    (tab (browsers-all-tabs))
    (otherwise (error "Unknown requested type, the 'of' key"))))

(defun browser-find-instance-get-test (&key given-instance given-type
                                         requested-instance-type)
  "Based on given-instance and given-type
return a test that will be given an instance of requested-instance-type
the test will return a requested-instance-type instance that matches the given-instance"
  (macrolet ((req-is (need &body body)
               `(case requested-instance-type
                  (,need ,@body)
                  (otherwise (error
                              "The requested-instance-type is incompatable with the given-type"))))
             (ret (instance-name test)
               `(lambda (,instance-name)
                  (when ,test
                    ,instance-name))))
    (case given-type
      (notebook
       (req-is 'browser
               (ret browser
                    (eq (widgets-notebook
                         (browser-gtk browser))
                        given-instance))))
      (window
       (req-is 'browser
               (ret browser
                    (eq (widgets-window
                         (browser-gtk browser))
                        given-instance))))
      (inspector-pointer
       (req-is 'inspector
               (ret inspector
                    (and inspector
                         (pointer-eq (if (pointerp given-instance)
                                         given-instance
                                         (pointer given-instance))
                                     (inspector-pointer
                                      inspector))))))
      (inspector-window
       (req-is 'tab
               (ret tab
                    (and (tab-inspector tab)
                         (eq given-instance
                             (inspector-window
                              (tab-inspector tab)))))))
      (view
       (case requested-instance-type
         (tab
          (ret tab
               (eq given-instance
                   (tab-view tab))))
         (browser
          (ret browser
               (member given-instance
                       (mapcar #'tab-view
                               (browsers-all-tabs)))))
         (otherwise (error "The given type 'view' is incompatable with the requested type"))))
      (scrolled-window
       (req-is 'tab
               (ret tab
                    (eq given-instance
                        (tab-scroll tab)))))
      (otherwise (error "Unknown given-type, the 'from' key")))))

(defun browser-find-instance (widget &key of from)
  "
   'of' is the type of widget given
   'from' is the requested instance found that is related to the given instance

Created for the following use case:
'when a view or other widget is passed in a callback argument, find it's browser instance.'

Examples:
    (browser-find-instance widget
                           :of 'browser
                           :from 'view)
    (browser-find-instance widget
                           :of 'tab
                           :from 'scrolled-window)"
  (let ((source ;; List of items to filter
         (browser-find-instance-get-source of))
        (this-test ;; Given each source item, will return the first true
         (browser-find-instance-get-test :given-instance widget
                                         :given-type from
                                         :requested-instance-type of))
        ret)
    (setf ret
          (first (delete-if #'null (mapcar this-test source))))
    (unless ret (error "Unattached widget"))
    ret))
