;; (in-package :gtk-cffi)
;; (load "../cffi/types.lisp")

;; Extension of gtk-cffi/gtk/notebook.lisp


(in-package :gtk-cffi)
(defclass overlay (container) ())
(defcfun "gtk_overlay_new" :pointer)
(defmethod gconstructor ((overlay overlay) &rest rest)
  (declare (ignore rest))
  (gtk-overlay-new))
(export '(overlay))

(in-package :lispkit)
(defcfun "gtk_overlay_add_overlay" :void
  (overlay-obj pobject)
  (widget pobject))

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

;; Allow the webview widget to be transparent if the css wants it
(defcfun "webkit_web_view_set_transparent" :void
  (view pobject)
  (bool :boolean))
(defcfun "webkit_web_view_get_transparent" :boolean
  (view pobject))
;; (webkit-web-view-get-transparent (getf *ui-views* :hints))

(defcfun "gtk_widget_get_screen" :pointer
  (widget pobject))
(defcfun "gdk_screen_get_rgba_visual" :pointer
  (screen :pointer))
(defcfun "gtk_widget_set_visual" :void
  (widget pobject)
  (visual :pointer))
  

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
  ((pages :initarg :pages
          :initform (list *uri-homepage*)
          :accessor browser-pages)
   ;; widgets
   (win :accessor browser-win
        :documentation "The gtk toplevel window")
   ui-tabs
   notebook
   (hints-over-view :accessor browser-over
                    :documentation "A gtk-overlay containing the notebook
with ui-hints on top")
   (ui-hints :accessor browser-hints
             :documentation "A transparent webkit view to display link hints")
   new-tab-view
   ui-status
   pane1
   pane2))

(defmacro defclass-defuse (instance bindings &body body)
  `(progn
     (mapcar (lambda (l)
               (setf (slot-value ,instance (car l))
                     (eval (car (cdr l)))))
             ',bindings)
     (with-slots
           ,(mapcar (lambda (l) (car l)) bindings)
         ,instance
       ,@body)))

(defun widget-set-rgba (widget)
  (gtk-widget-set-visual
   widget 
   (gdk-screen-get-rgba-visual
    (gtk-widget-get-screen widget))))
(defcallback screen-changed :void
    ((widget pobject)
     (prev-screen :pointer))
  (declare (ignore prev-screen))
  (widget-set-rgba widget))

(defcallback on-button-press :boolean
    ((widget pobject)
     (event :pointer))
  (declare (ignore event))
  (show widget)
  (widget-set-rgba widget)
  t)

(defmethod initialize-instance :after ((browser browser) &key)
  (defclass-defuse browser
      ((win (make-instance 'window
                           :width 800 :height 600
                           :title "LispKit"
                           :has-resize-grip nil))
       (ui-tabs (make-instance 'scrolled-window))

       (notebook (make-instance 'notebook))
       (hints-over-view (make-instance 'overlay))
       (ui-hints (make-instance 'scrolled-window))

       (new-tab-view (make-instance 'scrolled-window))
       (ui-status (make-instance 'scrolled-window))
       (pane1 (make-instance 'v-paned))
       (pane2 (make-instance 'v-paned)))

    ;; (widget-set-rgba win)
    ;; (widget-set-rgba hints-over-view)
    (widget-set-rgba ui-hints)

     ;;  (print
     ;; (gdk-screen-get-rgba-visual
     ;;  (gtk-widget-get-screen win)))

    ;; Connect scrolling widgets with their content
    (add ui-tabs (ui-new-view 'tabs))
    (add ui-status (ui-new-view 'status))

    (setf (property ui-hints 'can-focus) nil)
    ;; (setf (property hints-over-view 'can-focus) nil)
    (setf (valign ui-hints) :start)
    (setf (halign ui-hints) :center)
    (setf (size-request ui-hints) '(400 100))
    (ui-new-view 'hints)
    (webkit-web-view-set-transparent (getf *ui-views* :hints) t)
    (add ui-hints (getf *ui-views* :hints))

    (setf (gsignal ui-hints "button-press-event") (callback on-button-press))
    (setf (gsignal hints-over-view "button-press-event") (callback on-button-press))

    ;; First tab
    (mapcar (lambda (uri)
        (add new-tab-view (tab-new uri)))
        ;; todo:
        ;; dynamic scrolled views
        ;; keep list slot in the class
        ;; insert page
            (browser-pages browser))
    
    (add hints-over-view notebook)
    (gtk-overlay-add-overlay hints-over-view ui-hints)

    (gtk-notebook-set-show-tabs notebook nil)
    (gtk-notebook-set-show-border notebook nil)
    (show new-tab-view)
    (gtk-notebook-set-current-page
     notebook
     (gtk-notebook-insert-page notebook new-tab-view nil 0))

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 ui-tabs :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 hints-over-view :resize t :shrink t)
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
          ;; (gsignal win "screen-changed") (callback screen-changed)
          (gsignal win "destroy") (callback exit))

    (show win :all t)))

(defvar *window* nil)
(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (gdk-threads-init)
  (within-main-loop                     
   (setf *window* (make-instance 'browser :pages (list *uri-homepage*)))))

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



  ;; (set-slots browser
  ;;            `((win ,(make-instance 'window
  ;;                                   :width 800 :height 600
  ;;                                   :title "LispKit"
  ;;                                   :has-resize-grip nil))
  ;;              (ui-tabs ,(make-instance 'scrolled-window))

  ;;              (notebook ,(make-instance 'notebook))
  ;;              (hints-over-view ,(make-instance 'overlay))
  ;;              (ui-hints ,(make-instance 'scrolled-window))

  ;;              (new-tab-view ,(make-instance 'scrolled-window))
  ;;              (ui-status ,(make-instance 'scrolled-window))
  ;;              (pane1 ,(make-instance 'v-paned))
  ;;              (pane2 ,(make-instance 'v-paned))))

  ;; (with-slots 
  ;;       (win
  ;;        ui-tabs
  ;;        notebook
  ;;        hints-over-view
  ;;        ui-hints
  ;;        new-tab-view
  ;;        ui-status
  ;;        pane1
  ;;        pane2)
  ;;     browser
