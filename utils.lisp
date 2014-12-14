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

;; From stumpwm
(defun split-seq (seq separators &key test default-value)
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (find c seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
             then (position-if (complement #'sep) seq :start j)
             as j = (position-if #'sep seq :start (or i 0))
             while i
             collect (subseq seq i j)
             while j)
          ;; the empty seq causes the above to return NIL, so help
          ;; it out a little.
          default-value))))
(defun split-string (string &optional (separators "
"))
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that. "
  (split-seq string separators :test #'char= :default-value '("")))
