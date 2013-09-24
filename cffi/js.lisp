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

(defun js-eval-webview (view script &key (source "") want-return)
  (let* ((context (webkit-web-frame-get-global-context
                   (webkit-web-view-get-main-frame view)))
         (result (js-eval-script :context context
                                 :script script
                                 :source source)))
    (if want-return
        (js-result-to-string context result))))

(defun testing ()
  "Automatic context for testing"
  (gtk-init)
  (js-eval-webview (make-instance 'widget :pointer (webkit-web-view-new))
                   "console.log('Printed');'hello ret';"
                   ;; :source "ui://tabs" :want-return t))
                   :source nil :want-return t))

(export '(js-eval-webview))
;; Export all functions
;; (let ((pack (find-package :soup-binding)))
;;   (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
