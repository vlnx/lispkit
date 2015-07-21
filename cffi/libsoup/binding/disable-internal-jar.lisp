(in-package :soup-binding)

(defclass soup-cookie-jar (g-object) ())

(defcfun soup-cookie-jar-new pobject)

(defmethod gconstructor ((soup-cookie-jar soup-cookie-jar)
                         &rest rest)
  (soup-cookie-jar-new))

(defcenum soup-cookie-jar-accept-policy
  :soup-cookie-jar-accept-always
  :soup-cookie-jar-accept-never
  :soup-cookie-jar-accept-no-third-party)

;; Hacks for gtk-cffi to set enum property
(defclass soup-cookie-jar-accept-policy (g-object) ())
(register-type 'soup-cookie-jar-accept-policy
               "SoupCookieJarAcceptPolicy")

(defcfun soup-session-add-feature :void
  (session pobject)
  (feature pobject))

(defun disable-internal-jar (session)
  (let ((feature (make-instance 'soup-cookie-jar)))
    (setf (property feature :accept-policy)
          :soup-cookie-jar-accept-never)
    (soup-session-add-feature session feature)))

(export 'disable-internal-jar)
