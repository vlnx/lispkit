(in-package :lispkit)
;; Define macro to define lisp functions able to export to javascript

(defun defexport-var-binding (let-binding body-func)
  (eval `(let ,let-binding ,body-func)))

(defun defexport-return-value-pointer (jsc-context lisp-val)
  (declare (ignore lisp-val))
  (finish-output) ;; HACK: sbcl inferior-lisp buffer lags (without this)?
  (js-value-make-undefined jsc-context))

(defmacro js-callback (name args &body body)
  `(defcallback ,name :pointer
       ((context :pointer)
        (function :pointer)
        (this-object :pointer)
        (argument-count :int)
        (arguments :pointer)
        (execption :pointer))
     (declare (ignore function this-object execption))
     (defexport-return-value-pointer context
         (if (= argument-count (length ',args))
             (defexport-var-binding
                 (mapcar
                  (lambda (symbol) ;; Map across args
                    (list symbol ;; returning a list containg the argument
                          (js-result-to-string ;; and it's found value
                           context
                           (mem-ref arguments :pointer 
                                    (position symbol ',args)))))
                  ',args)
                 '(progn ,@body))
             (print
              (concatenate
               'string (symbol-name ',name)
               " was called with invalid number of javascript arguments"))))))

(defun js-exports-symbol-to-callback (symbol)
  "Retrive a callback function to give to js-export-function"
  (getf *js-exports* (as-keyword symbol)))

(defun js-exports-symbol-to-name (symbol)
  "Used to give js-export-function's name"
  (json:lisp-to-camel-case
   (symbol-to-string symbol)))

;; (defun js-prefix-callback (sym)
;;   "Get the callback name"
;;   (as-symbol (concatenate
;;               'string
;;               "lisp-from-js/" (symbol-to-string sym))))

(defmacro defexport (symbol args &body body)
  ;; (let ((cb-name (js-prefix-callback symbol))
  (let ((cb-name 
         (as-symbol (concatenate
                     'string
                     "lisp-from-js/" (symbol-to-string symbol))))
        (key (as-keyword symbol)))
    `(progn
       (js-callback ,cb-name ,args ,@body)
       (setf (getf *js-exports* ,key)
             (callback ,cb-name)))))
