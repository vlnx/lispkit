(in-package :gdk-cffi)

(export 'gdk-screen-get-rgba-visual)

(defcfun gdk-screen-get-rgba-visual :pointer
  (screen :pointer))
