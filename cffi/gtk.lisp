(in-package :gtk-cffi)

(export
 '(overlay
   widget-set-rgba
   gtk-widget-set-visual
   gtk-widget-get-screen
   gtk-scrolled-window-set-min-content-height
   gtk-overlay-add-overlay
   gtk-notebook-set-show-tabs
   gtk-notebook-set-show-border
   gtk-notebook-insert-page
   gtk-notebook-set-current-page
   gtk-notebook-get-current-page
   gtk-widget-get-parent-window
   gtk-widget-get-window))

;; GtkOverlay
(defclass overlay (container) ())

(defcfun gtk-overlay-new :pointer)

(defmethod gconstructor ((overlay overlay) &rest rest)
  (declare (ignore rest))
  (gtk-overlay-new))

(defcfun gtk-overlay-add-overlay :void
  (overlay-obj pobject)
  (widget pobject))

;; GtkNotebook

(defcfun gtk-notebook-insert-page :int
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

(defcfun gtk-notebook-set-current-page :void
  (notebook pobject)
  (page-num :int))

(defcfun gtk-notebook-get-current-page :int
  (notebook pobject))

(defcfun gtk-notebook-get-n-pages :int
  "number of pages"
  (notebook pobject))

(defcfun gtk-notebook-get-nth-page :int
  "content of n page"
  (notebook pobject))

(defmethod gconstructor ((notebook notebook)
                         &rest rest &key (show-tabs t) (show-border t))
  "redefine to accept opts"
  (declare (ignore rest))
  (let ((n (gtk-notebook-new)))
    (gtk-notebook-set-show-tabs n show-tabs)
    (gtk-notebook-set-show-border n show-border)
    n))

;; Misc

(defcfun gtk-widget-set-visual :void
  (widget pobject)
  (visual :pointer))

(defcfun gtk-widget-get-screen :pointer
  (widget pobject))

(defcfun gtk-widget-get-parent-window :pointer
  (widget pobject))

(defcfun gtk-widget-get-window :pointer
  (widget pobject))

(defun widget-set-rgba (widget)
  (gtk-widget-set-visual
   widget
   (gdk-cffi:gdk-screen-get-rgba-visual
    (gtk-widget-get-screen widget))))
