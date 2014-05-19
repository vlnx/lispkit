(in-package :gdk-cffi)

(export
 '(gdk-x11-window-get-xid
   gdk-x11-get-default-xdisplay
   gdk-x11-get-default-root-xwindow
   gdk-screen-get-rgba-visual
   gdk-window-add-filter
   gdk-window-remove-filter))

(defcfun gdk-x11-window-get-xid :pointer
  (gdk-window :pointer))

(defcfun gdk-x11-get-default-xdisplay :pointer)

(defcfun gdk-x11-get-default-root-xwindow :pointer)

(defcfun gdk-window-add-filter :void
  (window :pointer)
  (func :pointer))

(defcfun gdk-window-remove-filter :void
  (window :pointer)
  (func :pointer))

(defcenum :gdk-filter-return
  :gdk-filter-continue ;; Event not handled, continue processesing
  :gdk-filter-translate ;; Native event translated into a GDK event and stored in the "event" structure that was passed in
  :gdk-filter-remove) ;; Terminate processing, removing event

(defcfun gdk-screen-get-rgba-visual :pointer
  (screen :pointer))
