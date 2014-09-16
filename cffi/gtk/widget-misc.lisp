(in-package :gtk-cffi)

(export
 '(widget-set-rgba
   gtk-widget-set-visual
   gtk-widget-get-screen
   gtk-widget-get-parent-window
   gtk-widget-get-window))

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
