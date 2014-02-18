(in-package :gtk-cffi)

;; Extension of gtk-cffi/gtk/notebook.lisp

(defcfun "gtk_notebook_append_page" :int
  (notebook pobject)
  (child pobjcet)
  (inital-label c-string))
(export 'gtk-notebook-append-page)

;; defgeneric
;; defmethod append-page
