(in-package :js-binding)

(define-foreign-library js-core
  (:unix "libjavascriptcoregtk-3.0.so"))
(use-foreign-library js-core)

;; Foreign string utilites
(defcfun ("JSStringCreateWithUTF8CString" js-create-string) :pointer
  (script :pointer))
(defcfun ("JSStringRelease" js-free-string) :void
  (ref :pointer))
(defcfun ("JSValueToStringCopy" js-value-to-string) :pointer
  (context :pointer)
  (value :pointer)
  (exception :pointer))
(defcfun ("JSStringGetMaximumUTF8CStringSize" js-string-length) :int
  (string-ref :pointer))
(defcfun ("JSStringGetUTF8CString" js-string-value-get) :void
  (input-ref :pointer)
  (buffer-ref :pointer)
  (size :int))


(defcfun ("JSEvaluateScript" %js-eval-script) :pointer
  (context :pointer)
  (script :pointer)
  (thisObject :pointer)
  (source :pointer)
  (startingLineNumber :int)
  (exception :pointer))
(defun js-eval-script (&key context
                         script
                         (this-object nil)
                         source
                         (starting-line-number 1)
                         (exception nil))
  (let* ((ref-script (js-create-string (convert-to-foreign script :string)))
         (ref-source (js-create-string (convert-to-foreign source :string)))
         (ref-result (%js-eval-script
                      context
                      ref-script
                      (if this-object
                          (convert-to-foreign this-object :pointer)
                          (null-pointer))
                      (if ref-source
                          ref-source
                          (null-pointer))
                      (convert-to-foreign starting-line-number :int)
                      (if exception
                          (convert-to-foreign exception :pointer)
                          (null-pointer)))))
    (mapcar #'js-free-string (list ref-script ref-source))
    ref-result))

;; NOTE: Look at luakit/widgets/webview/js.c's uasge of determining result type
;; That may want to be implemnted later
(defun js-result-to-string (context result)
  (let* ((js-str-ref (js-value-to-string context result (null-pointer)))
         (js-str-length (js-string-length js-str-ref))
         (buffer-ref (foreign-alloc :char :count js-str-length))
         output)
    (js-string-value-get js-str-ref
                         buffer-ref
                         js-str-length)
    (setf output (convert-from-foreign buffer-ref :string))
    (foreign-free buffer-ref)
    (js-free-string js-str-ref)
    output))

;; (defun testing ()
;;   "Automatic context for testing"
;;   (gtk-init)
;;   (let ((context (webkit-web-frame-get-global-context
;;                   (webkit-web-view-get-main-frame
;;                    (make-instance 'widget :pointer (webkit-web-view-new))))))
;;     (js-result-to-string context 
;;                          (js-eval-script :context context
;;                                          :script "console.log('Printed');'hello ret';"
;;                                          :source "ui://tabs"))))

(defun js-eval-webview (view script &key (source "") (want-return nil))
  (let* ((context (webkit-web-frame-get-global-context
                   (webkit-web-view-get-main-frame view)))
         (result (js-eval-script :context context
                                 :script script
                                 :source source)))
    (if want-return
        (js-result-to-string context result))))

;; (defun testing ()
;;   "Automatic context for testing"
;;   (gtk-init)
;;   (js-eval-webview (make-instance 'widget :pointer (webkit-web-view-new))
;;                    "console.log('Printed');'hello ret';"
;;                    ;; :source "ui://tabs" :want-return t))
;;                    :source nil :want-return t))


;; /usr/include/webkitgtk-3.0/JavaScriptCore/JSObjectRef.h
(defcfun ("JSObjectMake" js-object-make) :pointer
  (context :pointer)
  (class-def :pointer)
  (context-scope :pointer))
(defcfun ("JSContextGetGlobalObject" js-context-get-global-object) :pointer
  (context :pointer))
(defcfun ("JSObjectSetProperty" js-object-set-property) :void
  (context :pointer)
  (object :pointer)
  (property-name :pointer)
  (value :pointer)
  (attributes :int)
  (execption :pointer))
(defcfun ("JSObjectHasProperty" js-object-has-property) :boolean
  (context :pointer)
  (object :pointer)
  (property-name :pointer))
(defcfun ("JSObjectGetProperty" js-object-get-property) :pointer
  (context :pointer)
  (object :pointer)
  (property-name :pointer)
  (execption :pointer))
(defcfun ("JSObjectMakeFunctionWithCallback" js-object-make-from-function) :pointer
  (context :pointer)
  (name :pointer)
  (func :pointer))

(defun js-export-function (view function-name callback-pointer)
  "Register a function in lisp/C to a javascript context. 
In the javascript context the function is within the global object 'Exported'"
  (let* ((context (webkit-web-frame-get-global-context
                   (webkit-web-view-get-main-frame view)))
         (global-object (js-context-get-global-object context))
         (global-property-name (js-create-string 
                                (convert-to-foreign "Exported" :string)))
         (property-name (js-create-string 
                         (convert-to-foreign function-name :string))))
    
    ;; If property 'Exported' does not exist in the global context
    (unless (js-object-has-property context global-object global-property-name)
      ;; Set the property 'Exported' of the global context to a new empty object
      (js-object-set-property
       context
       global-object
       global-property-name
       (js-object-make context (null-pointer) (null-pointer))
       0 (null-pointer)))

    ;; Set a property within the global context's 'Exported' object
    (js-object-set-property
     context
     (js-object-get-property context global-object global-property-name (null-pointer))
     property-name
     (js-object-make-from-function context property-name callback-pointer)
     0 (null-pointer))

    ;; Manual clean up
    (mapcar #'js-free-string (list property-name global-property-name))))

(defcfun ("JSValueMakeUndefined" js-value-make-undefined) :pointer
  (context :pointer))

(export '(js-eval-webview js-export-function js-value-make-undefined js-result-to-string))

;; (defun js-export-function (view function-name callback-pointer)
;;   "Register a function in lisp/C to a javascript context. 
;; In the javascript context the function is within the global object 'Exported'" ;; TODO:
;;   (let* ((context (webkit-web-frame-get-global-context
;;                    (webkit-web-view-get-main-frame view)))
;;          (global-object (js-context-get-global-object context))
;;          (property-name-c (convert-to-foreign function-name :string))
;;          (property-name (js-create-string property-name-c))
;;          js-class
;;          property-value)
;;       (print "ttt")
;;     ;; (with-foreign-objects ((class-def '(:struct js-class-definition))
;;     ;;                        ;; (static-func '(:struct js-static-function))
;;     ;;                        ;; (static-func-null '(:struct js-static-function))
;;     ;;                        ;; (arr :pointer 2))
;;     (with-foreign-object (class-def '(:struct js-class-definition))
;;       (print "sss")
;;     ;;   (setf
;;     ;;    ;; Main function
;;     ;;    ;; (foreign-slot-value static-func '(:struct js-static-function) 'name) property-name
;;     ;;    ;; (foreign-slot-value static-func '(:struct js-static-function) 'func) callback-pointer
;;     ;;    ;; (foreign-slot-value static-func '(:struct js-static-function) 'property-attributes) 1 ; ReadOnly enum

;;     ;;    ;; ;; Null-terminating static function
;;     ;;    ;; (foreign-slot-value static-func-null '(:struct js-static-function) 'name) (null-pointer)
;;     ;;    ;; (foreign-slot-value static-func-null '(:struct js-static-function) 'func) (null-pointer)
;;     ;;    ;; (foreign-slot-value static-func-null '(:struct js-static-function) 'property-attributes) 0
       
;;     ;;    ;; ;; Set array containing static function
;;     ;;    ;; (mem-aref arr :pointer 1) static-func
;;     ;;    ;; (mem-aref arr :pointer 2) static-func-null
       
;;     ;;    ;; ;; Fill in class definiton
;;     ;;    ;; (foreign-slot-value class-def '(:struct js-class-definition) 'version) 0
;;     ;;    ;; (foreign-slot-value class-def '(:struct js-class-definition) 'attributes) 0
;;     ;;    ;; (foreign-slot-value class-def '(:struct js-class-definition) 'class-name) property-name
;;     ;;    ;; (foreign-slot-value class-def '(:struct js-class-definition) 'static-funcs) arr

;;       ;; (setf
;;       ;;  (foreign-slot-value class-def '(:struct js-class-definition) 'class-name) property-name-c
;;       ;;  (foreign-slot-value class-def '(:struct js-class-definition) 'call-as-function) callback-pointer
       
;;       ;;  ;; Create the javascript object
;;       ;;  js-class (js-class-create class-def)
;;       ;;  )
;;       ;; (print js-class)
;;        ;; (setf property-value (js-object-make context (null-pointer) (null-pointer)))
;;        ;; (setf property-value (js-object-make context js-class (null-pointer)))
;;       ;; 
;;       ;; (setf property-value (js-object-make-from-function context property-name callback-pointer))

;;       (print "1")
;;       ;; Set the property in an object
;;       (js-object-set-property
;;        context
;;        global-object
;;        property-name
;;        ;; property-value
;;        (js-object-make-from-function context property-name callback-pointer)
;;        0 (null-pointer))
;;       (print "")
;;       (print "")
;;     ;;   (print "2")

;;     ;;   ;; Manual clean up
;;       (js-free-string property-name)
;;       ;; (js-free-class js-class))))
;;       )))
;;       ;; )))
