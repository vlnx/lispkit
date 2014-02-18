;; File to define uri's matched to lisp functions to export to javascript
(in-package :lispkit)


;; TODO: find proper return value way to pass it in the macro
;;        support callbacks
;; FIXME: let binding within macro
(defun defexport-var-binding (let-binding body-func)
  (eval `(let ,let-binding ,body-func)))
(defun defexport-return-value-pointer (jsc-context lisp-val)
  (declare (ignore lisp-val))
  (finish-output) ;; HACK: sbcl inferior-lisp buffer lags (without this)?
  (js-value-make-undefined jsc-context))
(defmacro defexport (name args &body body)
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
                 (mapcar (lambda (symbol) ;; Map across args
                           (list symbol ;; returning a list containg the argument
                                 (js-result-to-string ;; and it's found value
                                  context
                                  (mem-ref arguments :pointer 
                                           (position symbol ',args)))))
                         ',args)
                 '(progn ,@body))
             (print
              (concatenate
               'string (symbol-name ',name) " was called with invalid arguments"))))))


(defcallback lisp-from-js/LispFunc :pointer
  ((context :pointer)
   (function :pointer)
   (this-object :pointer)
   (argument-count :int)
   (arguments :pointer)
   (exception :pointer))
  (declare (ignore function this-object argument-count arguments execption))
  (print "Hello from javascript in lisp")
  (finish-output)
  (js-value-make-undefined context))

(defexport lisp-from-js/loadUri (maybe-uri)
  (print "Hello from javascript in lisp")
  (print maybe-uri)
  (webkit-web-view-load-uri (first *views*) maybe-uri))

;; (defcallback lisp-from-js/loadUri :pointer
;;   ((context :pointer)
;;    (function :pointer)
;;    (this-object :pointer)
;;    (argument-count :int)
;;    (arguments :pointer)
;;    (exception :pointer))
;;   (declare (ignore function this-object execption))
;;   (print "Hello from javascript in lisp")
;;   (if (= argument-count 1)
;;       (let ((maybe-uri (js-result-to-string context (mem-ref arguments :pointer 0))))
;;         (print maybe-uri)
;;         (webkit-web-view-load-uri (first *views*) maybe-uri))
;;       (print "no/more argument"))
;;   (finish-output)
;;   (js-value-make-undefined context))

(defcallback lisp-from-js/promptClose :pointer
  ((context :pointer)
   (function :pointer)
   (this-object :pointer)
   (argument-count :int)
   (arguments :pointer)
   (exception :pointer))
  (declare (ignore function this-object execption))
  (setf *active-maps* '(*top-map*))
  (js-value-make-undefined context))

(defvar *js-exports* '())
(setf *js-exports*
      '(("ui://status" . lisp-from-js/loadUri)))
