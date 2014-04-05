(in-package :cffi)

;; http://common-lisp.net/project/cffi/manual/cffi-manual.html#Defining-Foreign-Types
(define-foreign-type cffi-string ()
  ((encoding :reader string-type-encoding :initarg :encoding))
  (:actual-type :pointer))

(define-parse-method c-string (&key (encoding :utf-8))
  (make-instance 'cffi-string :encoding encoding))

(defmethod translate-to-foreign (string (type cffi-string))
  (foreign-string-alloc string :encoding (string-type-encoding type)))
(defmethod translate-from-foreign (pointer (type cffi-string))
    (foreign-string-to-lisp pointer :encoding (string-type-encoding type)))
(defmethod free-translated-object (pointer (type cffi-string) param)
    (declare (ignore param))
    (foreign-string-free pointer))

(export 'c-string)
