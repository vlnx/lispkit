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

(defpackage :lispkit/utils
  (:use #:common-lisp)
  (:export
   #:as-keyword
   #:as-symbol
   #:symbol-to-string))

(defpackage :lispkit/transcompile
  (:use #:common-lisp
        #:lispkit/utils)
  (:export
   *transcompiler-cache-dir*
   #:transcompiler
   *transcompilers*
   #:transcompile))

(defpackage :lispkit/keys
  (:use #:common-lisp
        #:cffi #:cffi-objects
        #:x11-binding)
  ;; #:gdk-cffi
  (:export
   #:kbd
   #:define-key
   #:make-kmap
   #:print-key
   #:handle-keymap
   #:create-xic
   #:process-gdk-event->key))

(defpackage :lispkit
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :cffi ;; defcallback
        :gtk-cffi
        :gtk-cffi+threads
        :cl-json
        :x11-binding
        :js-binding
        #:webkit-binding
        :lispkit/utils
        :lispkit/transcompile
        ;; #:lispkit/webviews
        :lispkit/keys)
  ;; Import everything exepct
  (:shadowing-import-from :gtk-cffi
                          #:window
                          #:image)
  (:shadow #:tabs #:uri)
  (:shadowing-import-from :g-object-cffi
                          #:with-object)
  (:export
   #:lispkit))
