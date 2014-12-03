(in-package :soup-binding)

(define-foreign-library libsoup
    (:unix "libsoup-2.4.so"))
(use-foreign-library libsoup)


;; Given a string returns a SoupURI
(defcfun ("soup_uri_new" %soup-uri-new) pobject
  (uri :string))
(defun soup-uri-new (uri)
  (with-foreign-string (c-uri uri)
    (%soup-uri-new c-uri)))

(export '(soup-uri-new))
;; Export all functions
;; (let ((pack (find-package :soup-binding)))
;;   (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
