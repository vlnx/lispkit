(in-package :lispkit)

(defclass inspector ()
  ((view  :accessor inspector-view
          :initarg :view
          :documentation "the view for the inspector")
   (gobject :accessor inspector-gobject
            :initarg :gobject
            :documentation "the object for the inspector")
   (shown :accessor inspector-shown
          :initform nil
          :documentation "If the view is already added to the top-level window and shown top-level")
   (window :accessor inspector-window
           :initform (make-instance 'window
                                    :width 800 :height 600
                                    :title "LispKit - Inspector"
                                    :has-resize-grip nil)
           :documentation "the top-level window"))
  (:documentation "Contain an inspector's refs"))

(defclass tab ()
  ((view  :accessor tab-view
          :initarg :initial-uri
          :documentation "initially a uri, translated to a new webview")
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
    :initform (make-instance 'tab :initial-uri (ui-symbol-to-uri 'tabs))
    :documentation "Tabs view")
   (status
    :accessor ui-status
    :initform (make-instance 'tab :initial-uri (ui-symbol-to-uri 'status))
    :documentation "Status bar view")
   (hints
    :accessor ui-hints
    :initform (make-instance 'tab :initial-uri (ui-symbol-to-uri 'hints))
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
            :documentation "Hold x11's XIC reference to decode input keys")
   (content-overlay :accessor widgets-overlay
                    :initform (make-instance 'overlay)
                    :documentation "gtk overlay, holding hints view over notebook")))

(defclass key-state-class ()
  ((active-maps :accessor active-maps
                :initform '(:scroll :top)
                :documentation "Active kmaps for the browser")
   (buffer :accessor key-buffer
           :initform nil
           :documentation "cache of unmatched keys")
   (passthrough-state :accessor passthrough-state
                      :initform nil
                      :documentation "Current passthrough state")))

(defun set-active-maps (browser maps)
  ;; Couldn't define this as setf method, since the browser instance is needed
  (setf (active-maps (browser-key-state browser))
        maps)
  (ui-update browser :keymode t))

(defclass browser ()
  ((ui :accessor browser-ui
       :initform (make-instance 'ui-views)
       :documentation "The class for the view instances used in the interface")
   (tabs :accessor browser-tabs
         :initarg :initial-tabs
         :initform (list *homepage*)
         :documentation "Give initial tabs, then will be replaced with
                         the list of active tab instances")
   (tabs-current-index :accessor browser-tabs-current-index
                       :initform 0
                       :documentation "Current tab index")
   (always-one-tab :accessor browser-always-one-tab
                   :initform t
                   :documentation "If a new tab should be created after the last one is closed")
   (gtk :accessor browser-gtk
        :initform (make-instance 'gtk-widgets)
        :documentation "widgets used in the window")
   (key-state :accessor browser-key-state
              :initform (make-instance 'key-state-class)
              :documentation "The state of current keymap for the window"))
  (:documentation "Starts an instance of the browser in a top-level x11 window"))

(defun browser-views (b)
  "access tab views"
  (mapcar (lambda (tab)
            (tab-view tab))
          (browser-tabs b)))

(defun current-tab (&optional (browser (current-browser)))
  "Maybe optimize, keep track of index, so don't have to cffi it each time"
  (nth (browser-tabs-current-index browser)
       (browser-tabs browser)))

;; (with-current-browser (b)
(defun current-browser ()
  "Use the index set by focus callbacks"
  (nth *browser-current-index*
       *browsers*))

(defun browser-all-tabs (b)
  "Return all tab instances including user interface views"
  (append (get-all-slot-values (browser-ui b))
          (browser-tabs b)))

(defun browsers-all-tabs ()
  "Give the tabs of all browsers in a flat list"
  (apply #'concatenate 'list ; Flatten a list with a depth of '(()()())
         (mapcar #'browser-all-tabs *browsers*)))


;; Maybe macro this down but it's manageable enough for now
(defgeneric find-instance (of from widget))

(defmacro closure-return-input-when (arg test)
  `(lambda ,arg (when ,test ,(first arg))))

(defmacro find-instance-helper (lex test)
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
  (eq (widgets-notebook (browser-gtk browser))
      widget))

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-window widget))
  (eq (widgets-window (browser-gtk browser))
      widget))

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
                  (browsers-all-tabs)))) ; review

(find-instance-helper ((of-browser (browser *browsers*))
                       (from-scrolled-window widget))
  (member widget
          (mapcar #'tab-scroll
                  (browsers-all-tabs)))) ; review

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
  (eq (tab-view (ui-hints (browser-ui browser)))
      widget))

;; Local Variables:
;; eval: (font-lock-add-keywords nil '(("(\\(find-instance-helper\\) " 1 font-lock-keyword-face t)))
;; eval: (put 'find-instance-helper 'common-lisp-indent-function 1)
;; End:
