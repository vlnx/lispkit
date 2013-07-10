(in-package #:cl-user)

(defpackage :lispkit
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :gtk-cffi
        :webkit-binding)
  (:shadow #:yes-or-no-p #:y-or-n-p)
  (:export
   #:lispkit))
