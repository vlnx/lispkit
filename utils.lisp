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

(defun circular-index (direction starting-index list)
  "Return the next or previous valid index of a list.

If the next index is out of range, return to the first index.
 (circular-index :next 0 '(a b c d)) => 1
 (circular-index :next 3 '(a b c d)) => 0

If the previous index would be negative, move to end of the list.
 (circular-index :prev 2 '(a b c d)) => 1
 (circular-index :prev 0 '(a b c d)) => 3"
  (declare (type (member :next :prev) direction)
           (type number starting-index)
           (type list list))
  (let ((next-index (1+ starting-index))
        (prev-index (1- starting-index))
        (list-max (1- (length list)))
        (list-min 0))
    (case direction
      (:next (if (> next-index list-max)
                 list-min
                 next-index))
      (:prev (if (< prev-index list-min)
                 list-max
                 prev-index)))))

(defun x11-selection (buffer &optional value)
  "Set/get selection using `xsel'"
  (declare (type (member :primary :clipboard) buffer)
           (type (or string null) value))
  (flet ((set-it (arg str)
           (with-input-from-string (in str)
             (sb-ext:run-program "/usr/bin/xsel" (list arg)
                                 :input in)))
         (get-it (arg)
           (with-output-to-string (out)
             (sb-ext:run-program "/usr/bin/xsel" (list arg)
                                 :output out))))
    (case buffer
      (:primary (if value
                    (set-it "-pi" value)
                    (get-it "-po")))
      (:clipboard (if value
                      (set-it "-bi" value)
                      (get-it "-bo"))))))

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
