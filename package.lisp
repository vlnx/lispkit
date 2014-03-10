;; http://www.gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html
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

(defpackage :x11-binding
  (:use :common-lisp :cffi
        ;; :gtk-cffi
        :cffi-objects :g-object-cffi))

;; (ql:quickload :bordeaux-threads)
(defpackage :gtk-cffi+threads
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :cffi ;; defcallback
        #:bordeaux-threads
        :gtk-cffi)
  (:export
   #:gdk-threads-init
   #:within-main-loop
   #:leave-gtk-main))

(defpackage :lispkit
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :cffi ;; defcallback
        :gtk-cffi
        :gtk-cffi+threads
        ;; :gdk-cffi ;; keys
        :cl-json
        :js-binding
        :x11-binding
        :webkit-binding)
  ;; Import everything exepct
  (:shadowing-import-from :gtk-cffi
                          #:window
                          #:image
                          )
                          ;;#:tabs)
  (:shadow #:tabs)
  (:shadowing-import-from :g-object-cffi
                          #:with-object)
  (:export
   #:lispkit))
