(in-package :webkit-binding)

(defcfun webkit-web-view-get-inspector :pointer
  (view pobject))

(defclass webview-inspector (g-object) ())

(defmethod gconstructor ((webview-inspector webview-inspector)
                         &key view)
  (webkit-web-view-get-inspector view))

(defcfun webkit-web-inspector-close :void
  (inspector-obj pobject))

(defcfun webkit-web-inspector-show :void
  (inspector-obj pobject))

(export '(webview-inspector
          webkit-web-inspector-close
          webkit-web-inspector-show))
