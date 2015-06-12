(in-package :soup-binding)

(define-foreign-library libsoup
    (:unix "libsoup-2.4.so"))

(use-foreign-library libsoup)

;; Given a string returns a SoupURI
(defcfun "soup-uri-new" pobject
  (uri c-string))

(export 'soup-uri-new)
