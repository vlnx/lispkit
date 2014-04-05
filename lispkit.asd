(defpackage :lispkit-system
  (:use #:cl #:asdf))
(in-package :lispkit-system)

(defsystem :lispkit
  :name "LispKit"
  :author "Nyx"
  :version "0.0.0"
  :maintainer "Nyx"
  :description "A webkit interface" 
  :depends-on (;;:swank
               :gtk-cffi
               :bordeaux-threads
               :cl-json
               :cl-ppcre
               :sb-posix)
  :serial t
  :components ((:file "package")
               (:file "utils")

               (:file "patch-gtk3-main-loop")
               (:file "cffi/types")
               (:file "cffi/x11")
               (:file "cffi/gtk")
               (:file "cffi/soup")
               (:file "cffi/webkit")
               (:file "cffi/js")

               (:file "modules/transcompiler")
               (:file "modules/keys/parse")
               (:file "modules/keys/from-gdk")

               (:file "core/primitives")
               (:file "core/browser-class")
               (:file "core/tabs")
               (:file "core/maps")
               (:file "core/window")

               (:file "core/defexports")
               (:file "core/defscripts")
               (:file "core/scripts")

               (:file "core/views")

               (:file "build/lispkit")))
