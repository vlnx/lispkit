(defpackage :lispkit-system
  (:use #:cl #:asdf))

(in-package :lispkit-system)

(defsystem :lispkit
  :name "LispKit"
  :author "vlnx"
  :version "0.0.0"
  :maintainer "vlnx"
  :description "A webkit interface"
  :depends-on (:gtk-cffi
               :bordeaux-threads
               :cl-json
               :cl-ppcre
               :sb-posix) ; :swank
  :serial t
  :components ((:file "package")
               (:file "utils")

               (:file "cffi/patch-gtk3-main-loop")
               (:file "cffi/types")
               (:file "cffi/x11")
               (:file "cffi/g-object/signals")

               (:file "cffi/gdk/event-filter")
               (:file "cffi/gdk/x11-interface")
               (:file "cffi/gdk/rgba")

               (:file "cffi/gtk/widget-misc")
               (:file "cffi/gtk/notebook")
               (:file "cffi/gtk/overlay")

               (:file "cffi/libsoup/soup-uri")
               (:file "cffi/webkit")
               (:file "cffi/js")

               (:file "modules/transcompiler")
               (:file "modules/keys/parse")
               (:file "modules/keys/from-gdk")

               (:file "core/primitives")
               (:file "core/browser-class")
               (:file "core/tabs")
               (:file "core/maps")
               (:file "core/browser-signals")
               (:file "core/browser-init")
               (:file "core/ui-update")

               (:file "core/defexports")
               (:file "core/defscripts")
               (:file "site/scripts")

               (:file "core/views")

               (:file "main")))
