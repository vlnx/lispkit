(in-package :lispkit/utils)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun symbol-to-string (symbol)
  (string-downcase (symbol-name symbol)))

(defun as-symbol (string)
  (intern (string-upcase string)))

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
  "split a sequence into sub sequences given the list of separators."
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
  "Splits STRING into sub-strings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The sub-strings between the splitting points are made into a list
which is returned.
If there is match for SEPARATORS at the beginning of STRING, we do not
include a null sub-string for that.  Likewise, if there is a match
at the end of STRING, we don't include a null sub-string for that. "
  (split-seq string separators :test #'char= :default-value '("")))


(defun x11-selection (&key primary clipboard)
  "Set/get selection using `xsel"
  (flet ((setit (arg str)
           (with-input-from-string (in str)
             (sb-ext:run-program "/usr/bin/xsel" (list arg)
                                 :input in)))
         (getit (arg)
           (with-output-to-string (out)
             (sb-ext:run-program "/usr/bin/xsel" (list arg)
                                 :output out))))
    (cond
      ((stringp primary)
       (setit "-pi" primary))
      (primary
       (getit "-po"))
      ((stringp clipboard)
       (setit "-bi" clipboard))
      (clipboard
       (getit "-bo")))))

(defun get-slot-names (instance)
  "SBCL way to get a list of the slots a class contains"
  (mapcar #'sb-pcl:slot-definition-name
          (sb-pcl:class-slots (class-of instance))))
(defun get-all-slot-values (instance)
  (mapcar (lambda (name)
            (slot-value instance name))
          (get-slot-names instance)))

(defun prepend-string-on-to-symbol (str symbol)
  (as-symbol (concatenate 'string
                          str
                          (symbol-to-string symbol))))

(defun pair-plist (plist)
  "(:x 1 :y 2) -> ((:x 1) (:y 2))"
  (loop
     for key in (remove-if-not #'keywordp plist)
     for val in (remove-if #'keywordp plist)
     collect (list key val)))
