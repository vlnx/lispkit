(in-package :gdk-cffi)

(export
 '(gdk-x11-window-get-xid
   gdk-x11-get-default-xdisplay
   gdk-x11-get-default-root-xwindow))

(defcfun gdk-x11-window-get-xid :pointer
  (gdk-window :pointer))

(defcfun gdk-x11-get-default-xdisplay :pointer)

(defcfun gdk-x11-get-default-root-xwindow :pointer)
