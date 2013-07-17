(in-package :lispkit)

(defvar *views* '()
  "A list of web-view's for the tabs")

(defun webview-new (uri)
  "returns a webview with your uri and settings"
  (let ((view (webkit-web-view-new)))
    (webview-change-settings view
                     '((:enable-plugins nil)
                       (:enable-scripts nil)
                       (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:21.0) Gecko/20100101 Firefox/21.0")))
    ;; (setf (property (webkit-get-default-session) :proxy-uri)
    ;;       (soup-uri-new "http://127.0.0.1:8123/"))
    (webkit-web-view-load-uri view uri)
    (push view *views*)
    view))



(defun tab-new (uri)
  (webview-new uri))
  

(export '(*views*
          tab-new))
