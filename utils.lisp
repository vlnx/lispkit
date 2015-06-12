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
