(in-package :g-object-cffi)

(defvar *g-signal-emit-ret* (foreign-alloc :boolean))

(defcfun g-signal-emit-by-name :void
  (widget pobject)
  (name c-string)
  (signal-param pobject)
  (ret :pointer))

