(in-package :soup-binding)

(define-foreign-library libsoup
  (:unix "libsoup-2.4.so"))

(use-foreign-library libsoup)
