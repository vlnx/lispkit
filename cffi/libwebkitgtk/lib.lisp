(in-package :webkit-binding)

(define-foreign-library libwebkit
    (:unix "libwebkitgtk-3.0.so"))

(use-foreign-library libwebkit)

;; SoupSession
(defcfun ("webkit_get_default_session"
          %webkit-get-default-session) pobject)

(defun webkit-get-default-session ()
  (make-instance 'g-object
                 :pointer (%webkit-get-default-session)))

(export 'webkit-get-default-session)
