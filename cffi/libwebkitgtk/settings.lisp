(in-package :webkit-binding)

(defclass webkit-settings (g-object) ())

(defcfun webkit-web-view-get-settings :pointer
  (view pobject))

(defmethod gconstructor ((webkit-settings webkit-settings) &key view)
  (webkit-web-view-get-settings view))

(defcfun webkit-web-view-set-settings :void
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

(export 'webview-change-settings)
