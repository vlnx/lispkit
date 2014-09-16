(in-package :gtk-cffi)
;; GtkOverlay

(export
 '(overlay
   gtk-overlay-add-overlay))

(defclass overlay (container) ())

(defcfun gtk-overlay-new :pointer)

(defmethod gconstructor ((overlay overlay) &rest rest)
  (declare (ignore rest))
  (gtk-overlay-new))

(defcfun gtk-overlay-add-overlay :void
  (overlay-obj pobject)
  (widget pobject))
