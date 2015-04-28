(in-package :g-object-cffi)

(defvar *g-signal-emit-ret* (foreign-alloc :boolean))

(defcfun g-signal-emit-by-name :void
  (widget pobject)
  (name cffi:c-string)
  (signal-param :pointer)
  (ret :pointer))

(export '(g-signal-emit-by-name
          *g-signal-emit-ret*))
