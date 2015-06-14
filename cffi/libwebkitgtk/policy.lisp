(in-package :webkit-binding)

(defcfun webkit-web-policy-decision-download :void
  (decision :pointer))

(defcfun webkit-web-policy-decision-ignore :void
  (decision :pointer))

(defcfun webkit-web-policy-decision-use :void
  (decision :pointer))

(export '(webkit-web-policy-decision-download
          webkit-web-policy-decision-ignore
          webkit-web-policy-decision-use))
