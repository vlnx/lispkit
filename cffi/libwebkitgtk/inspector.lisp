(in-package :webkit-binding)

(defclass webview-inspector (g-object) ())

(defcfun webkit-web-view-get-inspector :pointer
  (view pobject))

(defmethod gconstructor ((webview-inspector webview-inspector)
                         &key view)
  (webkit-web-view-get-inspector view))

(defcfun webkit-web-inspector-close :void
  (inspector pobject))

(defcfun webkit-web-inspector-show :void
  (inspector pobject))

(export '(webview-inspector
          webkit-web-inspector-close
          webkit-web-inspector-show))
