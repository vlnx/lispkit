(in-package :lispkit)

;; (defvar *ui-views* '()
;;   "A list of web-view's for the tabbar and inputbar and hints overlay")

(defvar *views* '()
  "A list of web-view's for the tabs")

(defcallback navigation-request :boolean
    ((source-view :pointer)
     (source-frame :pointer)
     (request :pointer)
     (action :pointer)
     (policy :pointer))
  ;; (declare (ignore widget))
  (write-line "Req")
  (print (webkit-network-request-get-uri request))
  nil)

(defun webview-new (uri)
  "returns a webview with your uri and settings"
  (let ((view (webkit-web-view-new)))
    (webview-change-settings view
                     '((:enable-plugins nil)
                       (:enable-scripts nil)
                       (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:21.0) Gecko/20100101 Firefox/21.0")))
    ;; (setf (property (webkit-get-default-session) :proxy-uri)
    ;;       (soup-uri-new "http://127.0.0.1:8123/"))
    (setf (gsignal (make-instance 'g-object :pointer (pointer view))
                   "navigation-policy-decision-requested")
          (callback navigation-request))
    (webkit-web-view-load-uri view uri)
    (push view *views*)
    view))



(defun tab-new (uri)
  (webview-new uri))

(defun ui-new-view (uri)
  ;; add to *ui-views*
  (webview-new uri))

