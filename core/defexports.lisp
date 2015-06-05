(in-package :lispkit)
;; Define macro to define lisp functions able to export to javascript

(defun js-exports-symbol-to-callback (symbol)
  "Retrieve a callback function to give to js-export-function"
  (getf *js-exports* (as-keyword symbol)))

(defun js-exports-symbol-to-name (symbol)
  "Used to give js-export-function's name"
  (json:lisp-to-camel-case (symbol-to-string symbol)))

(defun defexport-return-value-pointer (jsc-context lisp-val)
  "If the body evals to a string return that to the jsc"
  (if (stringp lisp-val)
      (let ((str-ref (js-create-string
                      (convert-to-foreign lisp-val :string)))
            js-value)
        (setf js-value
              (js-value-make-string jsc-context str-ref))
        (js-free-string str-ref)
        js-value)
      (js-value-make-undefined jsc-context)))

(defmacro defexport (symbol args &body body)
  "Define a function to that can be exported into the javascript context"
  (let ((cb-name (prepend-string-on-to-symbol "lisp-from-js/"
                                              symbol))
        (key (as-keyword symbol)))
    `(progn

       ;; Define the function
       (defun ,cb-name ,(append args '(&rest rest))
         (declare (ignore rest))
         ;; (dmesg rest)
         ,@body)

       ;; Define the callback that calls the lisp function with given arguments
       (defcallback ,cb-name :pointer
           ((context :pointer)
            (func :pointer)
            (this-object :pointer)
            (argument-count :int)
            (arguments :pointer)
            (exception :pointer))
         (declare (ignore func this-object exception))

         (let* ((argument-pointers
                 (loop
                    for i
                    from 0 to (1- argument-count)
                    collect
                      (mem-ref arguments :pointer i)))
                (arguments-in-strings
                 (mapcar (lambda (p)
                           (js-result-to-string context p))
                         argument-pointers))
                (val (apply #',cb-name
                            ;; This may not be the source, could store a reference to the jsc in the tab class, and search for that?
                            (current-browser)
                            arguments-in-strings)))
           (defexport-return-value-pointer context val)))

       ;; Store the callback reference
       (setf (getf *js-exports* ,key)
             (callback ,cb-name)))))
