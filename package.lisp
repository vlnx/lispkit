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
(defpackage :js-binding
  (:use :common-lisp :cffi
        :gtk-cffi
        :cffi-objects :g-object-cffi
        :webkit-binding))

(defpackage :lispkit
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :cffi ;; defcallback
        :gtk-cffi
        :gdk-cffi ;; keys
        :cl-json
        :js-binding
        :webkit-binding)
  (:shadowing-import-from :gtk-cffi #:window #:image)
  (:shadowing-import-from :g-object-cffi #:with-object)
  (:shadow #:yes-or-no-p #:y-or-n-p)
  (:export
   #:lispkit))
