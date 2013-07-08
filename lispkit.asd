(defpackage :lispkit-system
  (:use :cl :asdf))
(in-package :lispkit-system)

(defsystem :lispkit
  :name "LispKit"
  :author "Nyx"
  :version "0.0.0"
  :maintainer "Nyx"
  :description "A webkit interface" 
  :serial t
  :depends-on (:swank
               :gtk-cffi
               ;; :cl-webkit
               #+sbcl :sb-posix)
  :components ((:file "webkit")
               (:file "lispkit" :depends-on (webkit))))
