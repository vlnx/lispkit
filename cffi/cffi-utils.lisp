(in-package :cffi)

(defmacro defcstruct-keyword-setter (struct-name setter-name)
  "Define a setter function, `setter-name',
to set the foreign slots of `struct-name' using a keyword list"
  (let* ((slots (foreign-slot-names `(:struct ,struct-name)))
         (set-slots
          (loop for slot in slots nconc
               `((foreign-slot-value struct
                                     '(:struct ,struct-name)
                                     ',slot)
                 ,slot))))
    `(defun ,setter-name (&key ,@slots)
       (let ((struct (foreign-alloc '(:struct ,struct-name))))
         (setf ,@set-slots)
         struct))))

(export 'defcstruct-keyword-setter)

(defcallback true :boolean () t)
(export 'true)
