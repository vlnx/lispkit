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
               :cl-json
               :cl-ppcre
               #+sbcl :sb-posix)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "patch-gtk3-main-loop")

               (:file "cffi/soup")
               (:file "cffi/webkit")
               (:file "cffi/js")

               (:file "core/primitives")
               (:file "core/transcompiler")
               (:file "core/views")
               (:file "core/keys")
               (:file "core/window")

               (:file "image/lispkit")))
