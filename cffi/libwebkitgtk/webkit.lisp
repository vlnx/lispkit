;; Minimal CFFI bindings to WebKitGTK3 using WebKit1 (for now, may try WebKit2)
;; Inspired by github.com/joachifm/cl-webkit
;; Using github.com/Kalimehtar/gtk-cffi for a lispy interface to GTK3

;; (progn (require :lispkit)(in-package :lispkit))

(in-package :webkit-binding)

(define-foreign-library libwebkit
    (:unix "libwebkitgtk-3.0.so"))
(use-foreign-library libwebkit)

;; WebKitWebView
(defclass webkit-webview (widget) ())

(defcfun "webkit_web_view_new" :pointer)

(defmethod gconstructor ((webkit-webview webkit-webview) &key &allow-other-keys)
  (webkit-web-view-new))

(defcfun "webkit_web_view_load_uri" :void
  (view pobject)
  (uri c-string))

(defcfun "webkit_web_view_get_main_frame" pobject
  (view pobject))

;; WebKitWebFrame
(defcfun "webkit_web_frame_load_alternate_string" :void
  (frame pobject)
  (content-string c-string)
  (base-uri c-string)
  (base-uri2 c-string))

;; For javascript evaluation context
(defcfun "webkit_web_frame_get_global_context" :pointer
  (frame pobject))

;; -------~-------~--~------------------~------
;; WebKitWebSettings
;; -------~-------~--~------------------~------

(defclass webkit-settings (g-object) ())

(defcfun "webkit_web_view_get_settings" :pointer
  (view pobject))

(defmethod gconstructor ((webkit-settings webkit-settings) &key view)
  (webkit-web-view-get-settings view))

(defcfun "webkit_web_view_set_settings" :void
  (view pobject)
  (settings pobject))

(defun webview-change-settings (view opts)
  "Given a view pointer and opts, change the settings of the view"
  (let ((settings (make-instance 'webkit-settings :view view)))
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

;; (defcfun "webkit_network_request_get_uri" c-string
;;   (request :pointer))

(defcenum load-status-enum
  :webkit-load-provisional
  :webkit-load-committed
  :webkit-load-finished
  :webkit-load-first-visually-non-empty-layout
  :webkit-load-failed)

(defcfun "webkit_web_view_get_load_status" load-status-enum
  (view pobject))

(defcfun "webkit_web_view_get_uri" c-string
  (view pobject))

;; Allow the webview widget to be transparent if the css wants it
(defcfun "webkit_web_view_set_transparent" :void
  (view pobject)
  (bool :boolean))

(defcfun "webkit_web_view_get_transparent" :boolean
  (view pobject))

;; (webkit-web-view-get-transparent (getf *ui-views* :hints))


;; (export '(webview-new
;;           webkit-web-view-get-main-frame))

(defcallback true :boolean () t)

(defun webview-hide-scrollbars (view scrolled-win)
  "Must be run before size-requests are made, otherwise will intefere with
complex logic deep inside webkit regarding visible content sizes"
  ;; Force the main frame to respond to it's parent container's policy change
  (setf (gsignal (make-instance 'g-object :pointer
                                (webkit-web-view-get-main-frame view))
                 "scrollbars-policy-changed")
        (cffi:callback true))
  (setf (policy scrolled-win) '(:never :never)))

(defcfun "webkit_web_view_get_inspector" pobject
  (view pobject))
(defcfun "webkit_web_inspector_close" :void
  (inspector-obj pobject))

;; (defcfun "webkit_web_inspector_get_web_view" pobject
;;   (inspector pobject))
;; (webkit-web-view-get-inspector

(defcfun "webkit_web_view_can_go_back" :boolean
  (view pobject))

(defcfun "webkit_web_view_can_go_forward" :boolean
  (view pobject))

;; Export all functions
(let ((pack (find-package :webkit-binding)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
