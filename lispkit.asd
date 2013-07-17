(defpackage :lispkit-system
  (:use :cl :asdf))
(in-package :lispkit-system)

(defsystem :lispkit
  :name "LispKit"
  :author "Nyx"
  :version "0.0.0"
  :maintainer "Nyx"
  :description "A webkit interface" 
  :depends-on (:swank
               :gtk-cffi
               #+sbcl :sb-posix)
  :serial t
  :components ((:file "package")

               (:file "backend/soup")
               (:file "backend/webkit")

               (:file "interface/tabs")
               (:file "interface/window")

               (:file "system/lispkit")))
