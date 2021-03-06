(defpackage :lispkit-system
  (:use #:cl #:asdf)
  (:export *root-directory*))

(in-package :lispkit-system)

(defvar *root-directory* (pathname (directory-namestring
                                    *load-pathname*))
  "The string of the root directory of the project")

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
                 :quri
                 :cl-cookie
                 :sb-posix) ; :swank
    :serial t
    :components ((:file "package")
                 (:file "utils")

                 (:file "cffi/cffi-utils")
                 (:file "cffi/types")
                 (:file "cffi/gtk/main-loop-patch")
                 (:file "cffi/x11/input-method")
                 (:file "cffi/g-object/signals")
                 (:file "cffi/gdk/event-filter")
                 (:file "cffi/gdk/x11-interface")
                 (:file "cffi/gdk/rgba")
                 (:file "cffi/gtk/widget-misc")
                 (:file "cffi/gtk/notebook")
                 (:file "cffi/gtk/overlay")

                 (:file "cffi/libsoup/binding/lib")
                 (:file "cffi/libsoup/binding/soup-uri")
                 (:file "cffi/libsoup/binding/soup-message")
                 (:file "cffi/libsoup/binding/disable-internal-jar")
                 (:file "cffi/libsoup/headers/cookie-jar")
                 (:file "cffi/libsoup/headers/headers")

                 (:file "cffi/libwebkitgtk/lib")
                 (:file "cffi/libwebkitgtk/webview")
                 (:file "cffi/libwebkitgtk/settings")
                 (:file "cffi/libwebkitgtk/inspector")
                 (:file "cffi/libwebkitgtk/policy")
                 (:file "cffi/libwebkitgtk/download")

                 (:file "cffi/libjavascriptcoregtk/eval")

                 (:file "modules/transcompiler")
                 (:file "modules/keys/structure")
                 (:file "modules/keys/parse")
                 (:file "modules/keys/from-gdk")

                 (:file "core/primitives")
                 (:file "core/browser-class")
                 (:file "core/ui-update")
                 (:file "core/tabs")
                 (:file "core/webkitgtk-utils")
                 (:file "core/maps")
                 (:file "core/browser-signals")
                 (:file "core/browser-init")

                 (:file "core/defexports")
                 (:file "core/defscripts")
                 (:file "site/scripts")

                 (:file "core/views")

                 (:file "main")))
