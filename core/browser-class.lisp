(in-package :lispkit)

(defclass inspector ()
  ((view  :accessor inspector-view
          :initarg :view
          :documentation "The webview the inspector uses")
   (gobject :accessor inspector-gobject
            :initarg :gobject
            :documentation "webview-inspector instance")
   (shown :accessor inspector-shown
          :initform nil
          :documentation "If inspector-window had `show' called")
   (window :accessor inspector-window
           :initform (make-instance 'window
                                    :width 800 :height 600
                                    :title "LispKit - Inspector"
                                    :has-resize-grip nil)
           :documentation "top-level gtk window"))
  (:documentation "Contain the webkit-inspector's references"))

(defclass tab ()
  ((view  :accessor tab-view
          :initarg :initial-uri
          :documentation "webview that starts at initial-uri")
   (scroll :accessor tab-scroll
           :initform (make-instance 'scrolled-window)
           :documentation "view container")
   (inspector :accessor tab-inspector
              :initform nil
              :documentation
              "hold the inspector instance once requested"))
  (:documentation "Create a tab from an initial-uri,
automatically add the view to a container"))

(defclass ui-views ()
  ((tabs
    :accessor ui-tabs
    :initform (make-instance 'tab
                             :initial-uri (ui-symbol-to-uri 'tabs))
    :documentation "Tabs")
   (status
    :accessor ui-status
    :initform (make-instance 'tab
                             :initial-uri (ui-symbol-to-uri 'status))
    :documentation "Status bar")
   (hints
    :accessor ui-hints
    :initform (make-instance 'tab
                             :initial-uri (ui-symbol-to-uri 'hints))
    :documentation "follow hints overlay")))

(defclass gtk-widgets ()
  ((window :accessor widgets-window
           :initform (make-instance 'window
                                    :width 800 :height 600
                                    :title "LispKit"
                                    :has-resize-grip nil)
           :documentation "Top-level window")
   (pane1 :accessor widgets-pane1
          :initform (make-instance 'v-paned)
          :documentation
          "Top pane containing the tabs and the other pane")
   (pane2 :accessor widgets-pane2
          :initform (make-instance 'v-paned)
          :documentation
          "Bottom pane containing the page and status bar")
   (notebook :accessor widgets-notebook
             :initform (make-instance 'notebook
                                      :show-tabs nil
                                      :show-border nil)
             :documentation "Notebook, allows for tabs")
   (x11-xic :accessor widgets-x11-xic
            :initform nil
            :documentation
            "Hold X11's XIC reference to decode input keys")
   (content-overlay
    :accessor widgets-overlay
    :initform (make-instance 'overlay)
    :documentation "gtk overlay, holding hints view over notebook")))

(defclass key-state-class ()
  ((active-maps :accessor active-maps
                :initform '(:scroll :top)
                :documentation "Names of active keymaps")
   (buffer :accessor key-buffer
           :initform nil
           :documentation "cache of unmatched keys")
   (passthrough-state :accessor passthrough-state
                      :initform nil
                      :documentation "Current passthrough state")))

(defun set-active-maps (browser maps)
  ;; Couldn't define this as setf method,
  ;; since the browser instance is used
  (setf (active-maps (browser-key-state browser))
        maps)
  (ui-update browser :keymode t))

(defclass browser ()
  ((ui :accessor browser-ui
       :initform (make-instance 'ui-views)
       :documentation
       "Class of webview instances used in the interface")
   (tabs :accessor browser-tabs
         :initarg :initial-tabs
         :initform (list *homepage*)
         :documentation
         "List of initial uri's to load, replaced by current tabs")
   (tabs-current-index :accessor browser-tabs-current-index
                       :initform 0
                       :documentation "Current tab index")
   (always-one-tab
    :accessor browser-always-one-tab
    :initform t
    :documentation
    "If a new tab should be created after the last one is closed")
   (gtk :accessor browser-gtk
        :initform (make-instance 'gtk-widgets)
        :documentation "widgets used in the window")
   (key-state :accessor browser-key-state
              :initform (make-instance 'key-state-class)
              :documentation
              "The state of current keymap for the window"))
  (:documentation
   "Start an instance of the browser in a top-level x11 window"))

(defun browser-views (b)
  (mapcar #'tab-view (browser-tabs b)))

(defun current-tab (&optional (browser (current-browser)))
  (nth (browser-tabs-current-index browser)
       (browser-tabs browser)))

(defun current-browser ()
  (nth *browser-current-index*
       *browsers*))

(defmacro with-current-browser (args &body body)
  `(let ((,(first args) (current-browser)))
     ,@body))

(defun browser-all-tabs (b)
  "Return all tab instances including user interface views"
  (append (get-all-slot-values (browser-ui b))
          (browser-tabs b)))

(defun browsers-all-tabs ()
  (apply #'concatenate 'list
         (mapcar #'browser-all-tabs *browsers*)))

(defgeneric find-instance (of from widget))

(defmacro closure-return-input-when (arg test)
  `(lambda ,arg (when ,test ,(first arg))))

(defmacro find-instance-helper (lex test)
  "Example:
(find-instance-helper ((of-item (item items))
                       (from-arg arg))
  (eq arg item))
(find-instance 'of-item 'from-arg arg) => item"
  (let ((of (first lex))
        (of-value (second (first lex)))
        (from (second lex)))
    `(defmethod find-instance ((of (eql ',(car of)))
                               (from (eql ',(car from)))
                               ,(second from))
       (let* ((source ,(second of-value))
              (test
               (closure-return-input-when (,(first of-value))
                                          ,test))
              (ret (first (delete-if #'null (mapcar test source)))))
         (unless ret
           (error "Unattached widget"))
         ret))))

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-notebook widget))
  (eq widget
      (widgets-notebook (browser-gtk browser))))

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-window widget))
  (eq widget
      (widgets-window (browser-gtk browser))))

(find-instance-helper ((of-inspector (inspector
                                      (mapcar #'tab-inspector
                                              (browsers-all-tabs))))
                       (from-inspector-pointer inspector-pointer))
  (and inspector
       (pointer-eq inspector-pointer
                   (pointer (inspector-gobject inspector)))))

(find-instance-helper ((of-tab (tab (browsers-all-tabs)))
                       (from-inspector-window widget))
  (and (tab-inspector tab)
       (eq widget
           (inspector-window (tab-inspector tab)))))

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-view widget))
  (member widget
          (mapcar #'tab-view
                  (browser-all-tabs browser))))

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-scrolled-window widget))
  (member widget
          (mapcar #'tab-scroll
                  (browser-all-tabs browser))))

(find-instance-helper ((of-tab (tab (browsers-all-tabs)))
                       (from-view widget))
  (eq widget
      (tab-view tab)))

(find-instance-helper ((of-tab (tab (browsers-all-tabs)))
                       (from-scrolled-window widget))
  (eq widget
      (tab-scroll tab)))

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-hints-view widget))
  (eq widget
      (tab-view (ui-hints (browser-ui browser)))))

;; Local Variables:
;; eval: (font-lock-add-keywords nil '(("(\\(find-instance-helper\\) " 1 font-lock-keyword-face t)))
;; eval: (put 'find-instance-helper 'common-lisp-indent-function 1)
;; End:
