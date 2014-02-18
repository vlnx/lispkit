(in-package :lispkit)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun symbol-to-string (symbol)
  (string-downcase (symbol-name symbol)))

(defun as-symbol (string)
  (intern (string-upcase string)))

(defmacro defclass-defuse (instance bindings &body body)
  "Given an instance and new bindings, set the slots and use them in the body"
  `(progn
     (mapcar (lambda (l)
               (setf (slot-value ,instance (car l))
                     (eval (car (cdr l)))))
             ',bindings)
     (with-slots
           ,(mapcar (lambda (l) (car l)) bindings)
         ,instance
       ,@body)))
