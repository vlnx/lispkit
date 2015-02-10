(in-package :gdk-cffi)

(export 'gdk-window-add-filter)
(export 'gdk-window-remove-filter)

(defcfun gdk-window-add-filter :void
  (window :pointer)
  (func :pointer))

(defcfun gdk-window-remove-filter :void
  (window :pointer)
  (func :pointer))

(defcenum :gdk-filter-return
  :gdk-filter-continue ; Event not handled, continue processing
  :gdk-filter-translate ; Native event translated into a GDK event and stored in the "event" structure that was passed in
  :gdk-filter-remove) ; Terminate processing, removing event
