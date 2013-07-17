;; Minimal CFFI bindings to WebKitGTK3 using WebKit1 (for now, may try WebKit2)
;; Inspired by github.com/joachifm/cl-webkit
;; Using github.com/Kalimehtar/gtk-cffi for a lispy interface to GTK3

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

(defcfun "webkit_web_view_get_main_frame" pobject
  (view pobject))

;; Hack to automaticly set the string argument as the right type
(defcfun ("webkit_web_view_load_uri" %webkit-web-view-load-uri) :void
  (view pobject)
  (uri :string))
(defun webkit-web-view-load-uri (view uri)
  (with-foreign-string (c-uri uri)
    (%webkit-web-view-load-uri view c-uri)))

;; -------~-------~--~------------------~------
;; Settings
;; -------~-------~--~------------------~------
;; (defcfun "webkit_web_settings_new" pobject) ;; Default settings object
;; WEBKIT-BINDING> (property (make-instance 'g-object :pointer (webkit-web-settings-new)) :enable-plugins)

;; Retrive settings pointer, translate it to a g-object to allow editing
(defcfun ("webkit_web_view_get_settings"
          %webkit-web-view-get-settings) pobject
  (view pobject))
(defun webkit-web-view-get-settings (view)
  (make-instance 'g-object
                 :pointer (%webkit-web-view-get-settings view)))

;; Set settings pointer, translate from g-object
(defcfun ("webkit_web_view_set_settings"
          %webkit-web-view-set-settings) :void
  (view pobject)
  (settings pobject))
(defun webkit-web-view-set-settings (view settings)
    (%webkit-web-view-set-settings view (pointer settings)))

(defun webview-change-settings (view opts)
  "Given a view pointer and opts, change the settings of the view"
  (let ((settings (webkit-web-view-get-settings view)))
    (flet ((set-prop (i)
             (setf (property settings (first i))
                   (second i))))
      (mapcar #'set-prop opts))
    (webkit-web-view-set-settings view settings)))
;; -------~-------~--~------------------~------

;; Returns 'SoupSession', g-object
(defcfun ("webkit_get_default_session"
          %webkit-get-default-session) pobject)
(defun webkit-get-default-session ()
  (make-instance 'g-object
                 :pointer (%webkit-get-default-session)))

;; (export '(webview-new
;;           webkit-web-view-get-main-frame))

;; Export all functions
(let ((pack (find-package :webkit-binding)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
