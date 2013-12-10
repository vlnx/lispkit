;; (in-package :gtk-cffi)
;; (load "../cffi/types.lisp")

;; Extension of gtk-cffi/gtk/notebook.lisp

(in-package :lispkit)
(defcfun "gtk_notebook_insert_page" :int
  (notebook pobject)
  (child pobject)
  (inital-label pobject)
  (positon :int))
(defcfun gtk-notebook-set-show-tabs :void
  (notebook pobject)
  (bool :boolean))
(defcfun gtk-notebook-set-show-border :void
  (notebook pobject)
  (bool :boolean))
;; (export 'gtk-notebook-append-page)
(defcfun "gtk_notebook_set_current_page" :void
  (notebook pobject)
  (page-num :int))
(defcfun "gtk_notebook_get_current_page" :int
  (notebook pobject))
;; number of pages
(defcfun "gtk_notebook_get_n_pages" :int
  (notebook pobject))
;; content of n page
(defcfun "gtk_notebook_get_nth_page" :int
  (notebook pobject))
  

;; TODO: window class system
;; make-instance = (win)
;; slots for elements
;; cleaned with window
;; list of instances
(defcallback exit :void
  ((window pobject))
  (declare (ignore window))
  (leave-gtk-main))

(defclass browser ()
  (win
   ui-tabs
   notebook
   new-tab-view
   ui-status
   pane1
   pane2))
(defun set-slots (instance slot-to-values)
  (mapcar (lambda (l)
            (setf (slot-value instance (car l))
                  (car (cdr l))))
          slot-to-values))
(defmethod initialize-instance :after ((browser browser) &key)
  (set-slots browser
             `((win ,(make-instance 'window
                                    :width 800 :height 600
                                    :title "LispKit"
                                    :has-resize-grip nil))
               (ui-tabs ,(make-instance 'scrolled-window))
               (notebook ,(make-instance 'notebook))
               (new-tab-view ,(make-instance 'scrolled-window))
               (ui-status ,(make-instance 'scrolled-window))
               (pane1 ,(make-instance 'v-paned))
               (pane2 ,(make-instance 'v-paned))))

  (with-slots 
        (win
         ui-tabs
         notebook
         new-tab-view
         ui-status
         pane1
         pane2)
      browser
    ;; Connect scrolling widgets with their content
    (add ui-tabs (ui-new-view 'tabs))

    (add new-tab-view (tab-new *uri-homepage*))

    (gtk-notebook-set-show-tabs notebook nil)
    (gtk-notebook-set-show-border notebook nil)
    (show new-tab-view)
    (gtk-notebook-set-current-page notebook
     (gtk-notebook-insert-page notebook new-tab-view nil 0))

    (add ui-status (ui-new-view 'status))

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 ui-tabs :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 notebook :resize t :shrink t)
    (pack pane2 ui-status :resize nil :shrink nil)

    ;; Set the 'minimal' heights of the ui widgets
    (setf 
     (property ui-tabs :min-content-height) 0
     (property ui-tabs :height-request) 16
     (property ui-status :height-request) 16
     (property ui-status :min-content-height) 0
     (size-request ui-tabs) '(-1 16)
     (size-request ui-status) '(-1 16))

    (add win pane1)
    
    (setf (gsignal win "key-press-event") (callback on-key-press)
          (gsignal win "key-release-event") (callback on-key-release)
          (gsignal win "destroy") (callback exit))
    (show win :all t)))

(defvar *window* nil)
(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (gdk-threads-init)
  (within-main-loop                     
   (setf *window* (make-instance 'browser))))

;;       ;; GTK3 Style
;;       ;; BUG: The handle-size only works if it is in ~/etc/gtk-3.0/gtk.css
;;       ;; don't display pane seperator
;;       ;; (load-css (style-context win) "* { -GtkPaned-handle-size: 0; }")
;;       ;; (load-css (style-context win) "* { background-image: url('/usr/share/pixmaps/htop.png'); }")
;;       ;; Connect scrolling widgets with their content
;;       (add ui-tabs (ui-new-view 'tabs))
;;       ;; Note: first the outer ui widget shrink was nil, then min-positon is 70
;;      ;; (setf (foreign-slot-value
;;      ;;        ;; (mem-ref (pointer ui-tabs) :pointer)
;;      ;;        (pointer ui-tabs)
;;      ;;        '(:struct widget-methods)
;;      ;;        'get-preferred-height)
;;      ;;       ;; 'get-preferred-height
;;      ;;       (callback pref-height))
;;                                ;; 'button-press-event)
;;            ;; (null-pointer))
;;      ;; after that class->adjust_size_request is called; if needed

;; defgeneric
;; defmethod append-page
;; LISPKIT> (foreign-slot-value (pointer *stat*) '(:struct widget-methods) 'get-preferred-height)
;; #.(SB-SYS:INT-SAP #X7FFFD4029990)
;; LISPKIT>  (setf (foreign-slot-value (pointer *stat*) '(:struct widget-methods) 'get-preferred-height) (null-pointer))
;; #.(SB-SYS:INT-SAP #X00000000)
;; LISPKIT>  (foreign-slot-value (pointer *stat*) '(:struct widget-methods) 'get-preferred-height)
;; #.(SB-SYS:INT-SAP #X00000000)
;; CL-USER>  (require :lispkit)(in-package :lispkit)
;; LISPKIT> (win)
;; ; No value
;; LISPKIT>  (setf (foreign-slot-value (pointer *stat*) '(:struct widget-methods) 'get-preferred-height) (callback PREF-HEIGHT))
;; #.(SB-SYS:INT-SAP #X20102290)
;; http://common-lisp.net/project/cffi/manual/cffi-manual.html#index-defcallback-144
;; https://github.com/masonh/wireshark/blob/7be4187b398c5082a5c36d69ce9a8fa7e5ed86f3/ui/gtk/bytes_view.c#L338
;; https://github.com/mdsmus/alien/blob/63539d330d0214b8b4a1519c70c080416d8b8413/src/cl%2Bssl/bio.lisp


;; (defvar *stat* nil)
;; (defvar *top* nil)
;; (defvar *pane1* nil)

;; Commented until scrolling mechanism is complete
;; Force the main frame to respond to it's parent container's policy change
;; (defcallback true :boolean () t)
;; (setf (gsignal (make-instance 'g-object :pointer
;;                               (webkit-web-view-get-main-frame view))
;;                "scrollbars-policy-changed")
;;       (cffi:callback true))
;; (setf (policy scrolled-win) '(:never :never))
;; (defcstruct widget-methods
;;   "for height setting"
;;   (button-press-event :pointer)
;;   (adjust-size-request :pointer)
;;   (get-preferred-height :pointer))
;; (defcallback button :boolean
;;     ((widget pobject)
;;      (event :pointer))
;;   (print "b")
;;   t)
;; NOTE: works, real does not
;; (let ((m (foreign-alloc :int))
;;       (n (foreign-alloc :int)))
;;   ;; called with &m; by address so will be changed out side of scope
;;   (foreign-funcall-pointer
;;    (callback pref-height) ()
;;    :pointer (null-pointer)
;;    :pointer m
;;    :pointer n
;;    :void) ;; ret type
;;   (mem-ref m :int))
;; (defcallback adj :void
;;     ((widget pobject)
;;      (orient orientation)
;;      ;; created as 'gint' passed as pointer
;;      (minimum :pointer)
;;      (natural :pointer))
;;   (declare (ignore widget))
;;   (print "n")
;;   (print minimum)
;;   (print natural))
;; (defcallback pref-height :void
;;     ((widget pobject)
;;      (minimum :pointer)
;;      (natural :pointer))
;;   (declare (ignore widget))
;;   (print "set hegiht")
;;   (print minimum)
;;   (print natural)
;;   (setf (mem-ref minimum :int) 16
;;         (mem-ref natural :int) 16))
;; (defcallback (cb-nil-test :convention :stdcall) :void ())
