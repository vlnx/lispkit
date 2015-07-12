(in-package :soup-binding)

(define-foreign-library libsoup
    (:unix "libsoup-2.4.so"))

(use-foreign-library libsoup)

(defcfun soup-uri-new :pointer
  (uri c-string))
(export 'soup-uri-new)
