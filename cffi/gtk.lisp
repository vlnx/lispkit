(in-package :gtk-cffi)
(export
 '(overlay
   gtk-overlay-add-overlay
   gtk-notebook-append-page
   gtk-notebook-insert-page
   gtk-notebook-set-show-tabs
   gtk-notebook-set-show-border
   gtk-notebook-set-current-page
   gtk-notebook-get-current-page
   gtk-notebook-get-n-pages
   gtk-widget-get-parent-window
   gtk-widget-get-window
   gtk-widget-get-screen
   gtk-widget-set-visual))


(defclass overlay (container) ())
(defcfun "gtk_overlay_new" :pointer)
(defmethod gconstructor ((overlay overlay) &rest rest)
  (declare (ignore rest))
  (gtk-overlay-new))
(defcfun "gtk_overlay_add_overlay" :void
  (overlay-obj pobject)
  (widget pobject))


;; Extension of gtk-cffi/gtk/notebook.lisp
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

(defcfun "gtk_widget_set_visual" :void
  (widget pobject)
  (visual :pointer))
(defcfun "gtk_widget_get_screen" :pointer
  (widget pobject))

(defcfun "gtk_widget_get_parent_window" :pointer
  (widget pobject))
(defcfun "gtk_widget_get_window" :pointer
  (widget pobject))


(in-package :gdk-cffi)
(export
 '(gdk-screen-get-rgba-visual
   gdk-window-add-filter
   gdk-window-remove-filter))

(defcfun "gdk_screen_get_rgba_visual" :pointer
  (screen :pointer))
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

(defcfun "gdk_window_add_filter" :void
  (window :pointer)
  (func :pointer))
(defcfun "gdk_window_remove_filter" :void
  (window :pointer)
  (func :pointer))
(defcenum :gdk-filter-return
    :gdk-filter-continue ;; Event not handled, continue processesing
    :gdk-filter-translate ;; Native event translated into a GDK event and stored in the "event" structure that was passed in
    :gdk-filter-remove) ;; Terminate processing, removing event



  
;; (let ((pack (find-package :gtk-cffi)))
;;   (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
