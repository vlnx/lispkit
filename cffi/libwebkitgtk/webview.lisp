(in-package :webkit-binding)

(defclass webkit-webview (widget) ())

(defcfun webkit-web-view-new :pointer)

(defmethod gconstructor ((webkit-webview webkit-webview)
                         &key &allow-other-keys)
  (webkit-web-view-new))

(defcfun webkit-web-view-load-uri :void
  (view pobject)
  (uri c-string))

;; WebKitWebFrame
(defcfun webkit-web-view-get-main-frame pobject
  (view pobject))

(defcfun webkit-web-frame-load-alternate-string :void
  (frame pobject)
  (content-string c-string)
  (base-uri c-string)
  (base-uri2 c-string))

;; For javascript evaluation context
(defcfun webkit-web-frame-get-global-context :pointer
  (frame pobject))

(defcenum load-status-enum
  :webkit-load-provisional
  :webkit-load-committed
  :webkit-load-finished
  :webkit-load-first-visually-non-empty-layout
  :webkit-load-failed)

(defcfun webkit-web-view-get-load-status load-status-enum
  (view pobject))

(defcfun webkit-web-view-get-uri c-string
  (view pobject))

;; Allow the webview widget to be transparent if the css wants it
(defcfun webkit-web-view-set-transparent :void
  (view pobject)
  (bool :boolean))

(defcfun webkit-web-view-get-transparent :boolean
  (view pobject))

(defun webview-hide-scrollbars (view scrolled-win)
  "Must be run before size-requests are made, otherwise will interfere with
complex logic deep inside webkit regarding visible content sizes"
  ;; Force the main frame to respond to it's parent container's policy change
  (setf (gsignal (make-instance 'g-object :pointer
                                (webkit-web-view-get-main-frame view))
                 "scrollbars-policy-changed")
        (callback true))
  (setf (policy scrolled-win) '(:never :never)))

(defcfun webkit-web-view-can-go-back :boolean
  (view pobject))

(defcfun webkit-web-view-can-go-forward :boolean
  (view pobject))

(defcfun webkit-web-view-go-back-or-forward :void
  (view pobject)
  (step :int))

(export '(webkit-webview
          webkit-web-view-load-uri
          webkit-web-view-get-main-frame
          webkit-web-frame-load-alternate-string
          webkit-web-frame-get-global-context
          webkit-load-provisional
          webkit-load-committed
          webkit-load-finished
          webkit-load-first-visually-non-empty-layout
          webkit-load-failed
          webkit-web-view-get-load-status
          webkit-web-view-get-uri
          webkit-web-view-get-transparent
          webkit-web-view-set-transparent
          webview-hide-scrollbars
          webkit-web-view-can-go-back
          webkit-web-view-can-go-forward
          webkit-web-view-go-back-or-forward))
