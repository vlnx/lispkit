;; Minimal CFFI bindings to WebKitGTK3 using WebKit1 (for now, may try WebKit2)
;; Inspired by github.com/joachifm/cl-webkit
;; Will be using github.com/Kalimehtar/gtk-cffi for a lispy interface for GTK3

(defpackage :webkit-binding
  (:use :common-lisp :cffi
        :gtk-cffi
        :cffi-objects :g-object-cffi))

(in-package :webkit-binding)

(define-foreign-library libwebkit
  (:unix "libwebkitgtk-3.0.so"))

(use-foreign-library libwebkit)  

;; From webkitversion.h
;; (defcfun "webkit_major_version" :uint)
;; (export 'webkit-major-version)

;; Note: `defcfun` takes the C function name, the return value, the arguments
;; and makes a lispifyed function that calles the C function

;; From webkitwebview.h
(defcfun "webkit_web_view_new" pobject)

;; Hack to automaticly set the string argument as the right type
(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (view pobject)
  (uri :string))
(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))

;; Retrive the settings, Manipulate them, Set the new values on the view
(defcfun ("webkit_web_view_get_settings"
          %webkit-web-view-get-settings) pobject
  (view pobject))
(defun webkit-web-view-get-settings (view)
  (make-instance 'g-object
                 :pointer (%webkit-web-view-get-settings view)))

(defcfun ("webkit_web_view_set_settings"
          %webkit-web-view-set-settings) :void
  (view pobject)
  (settings pobject))
(defun webkit-web-view-set-settings (view settings)
    (%webkit-web-view-set-settings view (pointer settings)))

;; ;; Just get the default settings, for testing
;; (defcfun "webkit_web_settings_new" pobject)

;; WEBKIT-BINDING> (property (make-instance 'g-object :pointer (webkit-web-settings-new)) :enable-plugins)

(defcfun "webkit_web_view_get_main_frame" pobject
  (view pobject))

(let ((pack (find-package :webkit-binding)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
