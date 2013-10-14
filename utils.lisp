(in-package :lispkit)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) :keyword))
