(in-package #:cl-user)

(defpackage :soup-binding
  (:use #:cl
        #:cffi
        #:gtk-cffi
        #:cffi-objects
        #:g-object-cffi))

(defpackage :webkit-binding
  (:use #:cl
        #:cffi
        #:gtk-cffi
        #:cffi-objects
        #:g-object-cffi
        #:soup-binding))

(defpackage :js-binding
  (:use #:cl
        :cffi
        #:gtk-cffi
        #:cffi-objects
        #:g-object-cffi
        #:webkit-binding))

(defpackage :x11-binding
  (:use #:cl
        #:cffi
        #:cffi-objects
        #:g-object-cffi)) ; :gtk-cffi

;; Get :bordeaux-threads with quicklisp
(defpackage :gtk-cffi+threads
  (:use #:cl
        #:cffi-objects
        #:g-object-cffi
        #:cffi
        #:bordeaux-threads
        #:gtk-cffi)
  (:export
   #:gdk-threads-init
   #:within-main-loop
   #:leave-gtk-main))

(defpackage :lispkit/utils
  (:use #:cl)
  (:export
   #:listify
   #:as-keyword
   #:as-symbol
   #:symbol-to-string
   #:circular-index
   #:x11-selection
   #:get-slot-names
   #:get-all-slot-values
   #:prepend-string-on-to-symbol
   #:pair-plist))

(defpackage :lispkit/transcompile
  (:use #:cl
        #:lispkit/utils)
  (:export
   *transcompiler-cache-dir*
   #:transcompiler
   *transcompilers*
   #:transcompile))

(defpackage :lispkit/keys
  (:use #:cl
        #:cffi
        #:cffi-objects
        #:x11-binding
        #:lispkit/utils)
  (:export
   #:kbd
   #:parse-key
   #:define-key
   #:make-kmap
   #:print-key
   #:lookup-keys
   #:create-xic
   #:process-gdk-event->key
   #:key-equalp
   #:key-character-p))

(defpackage :lispkit
  (:use #:cl
        #:cffi-objects
        #:g-object-cffi
        #:cffi
        #:gtk-cffi
        #:gtk-cffi+threads
        #:cl-json
        #:x11-binding
        #:js-binding
        #:webkit-binding
        #:lispkit/utils
        #:lispkit/transcompile
        #:lispkit/keys)
  ;; Import everything except
  (:shadowing-import-from #:gtk-cffi
                          #:window
                          #:image)
  (:shadow #:tabs #:uri)
  (:shadowing-import-from #:g-object-cffi
                          #:with-object)
  (:export #:main))
