(in-package #:cl-user)

(defpackage :soup-binding
  (:use :common-lisp :cffi
        :gtk-cffi
        :cffi-objects :g-object-cffi))

(defpackage :webkit-binding
  (:use :common-lisp :cffi
        :gtk-cffi
        :cffi-objects :g-object-cffi
        :soup-binding))

(defpackage :lispkit
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :gtk-cffi
        :webkit-binding)
  (:shadow #:yes-or-no-p #:y-or-n-p)
  (:export
   #:lispkit))
