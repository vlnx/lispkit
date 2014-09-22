(in-package :lispkit/utils)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun symbol-to-string (symbol)
  (string-downcase (symbol-name symbol)))

(defun as-symbol (string)
  (intern (string-upcase string)))

;; (defmacro defclass-defuse (instance bindings &body body)
;;   "Given an instance and new bindings, set the slots and use them in the body"
;;   `(progn
;;      (mapcar (lambda (l)
;;                (setf (slot-value ,instance (car l))
;;                      (eval (car (cdr l)))))
;;              ',bindings)
;;      (with-slots
;;            ,(mapcar (lambda (l) (car l)) bindings)
;;          ,instance
;;        ,@body)))

(defun listify (symbol-or-list)
  (if (listp symbol-or-list)
      symbol-or-list
      (list symbol-or-list)))

(defun circular-index-next (starting-index list)
  "Return the next valid index of a list,
if the next index is out of the list, go back to the first index.
 (circular-index-next 0 '(a b c d))
 => 1
 (circular-index-next 3 '(a b c d))
 => 0"
  (let ((next-index (1+ starting-index))
        (list-max (1- (length list)))
        (list-min 0))
    (if (> next-index list-max)
        list-min
        next-index)))

(defun circular-index-prev (starting-index list)
  "Return the previous index of a list, moving back to end of the list if needed
 (circular-index-prev 2 '(a b c d))
 => 1
 (circular-index-prev 0 '(a b c d))
 => 3"
  (let ((prev-index (1- starting-index))
        (list-max (1- (length list)))
        (list-min 0))
    (if (< prev-index list-min)
        list-max
        prev-index)))
