(in-package :lispkit)

(defun ui-scheme-p (uri)
  (ppcre:scan-to-strings "^ui://" uri))
(defun ui-symbol-to-uri (symbol)
  (concatenate 'string "ui://" (string-downcase (symbol-name symbol))))
(defun ui-scheme-uri-to-symbol (uri)
  (as-symbol (ppcre:regex-replace "^ui://" uri "")))

(defun ui-content (element type)
  "Take a symbol name of a ui element, and the type of content needed
Return the transcompiled file content"
  (let ((file (concatenate 'string *ui-dir*
                           (symbol-to-string element)
                           (case type
                             (html ".jade")
                             (css ".stylus")
                             (js ".coffee")))))
    (if (probe-file file)
        (transcompile :file file)
        (error "needed file doesn't exist"))))

(defun ui-update (ui element &rest opts)
  "Take an element of the iterface with any number of arguments, eval what
needs to be done"
  (case element
    (prompt-send-key (js-eval-webview (ui-status ui)
                                      (format nil "prompt.sendKey('~a');" (first (last opts)))))
    (prompt-enter (js-eval-webview (ui-status ui)
                                   (format nil "prompt.open('~a');" (first (last opts)))))
    (prompt-leave (js-eval-webview (ui-status ui)
                                   "prompt.close();"))
    (passthrough (js-eval-webview (ui-status ui)
                                  (if *keys-passthrough*
                                      "statusbar.passthrough(true);"
                                      "statusbar.passthrough(false);")))
    (uri (js-eval-webview (ui-status ui)
                          (format nil "statusbar.updateUri('~a');" 
                                  (or (if (stringp (car opts))
                                          (or (car opts)
                                              (property (car opts) :uri)))
                                      "about:blank"))))))

(defun view-scripts-styles (&key js css ui ui-element view uri)
  "js/css `t` will load those.
ui and ui-element or view and uri"
  (cond
    ((and ui ui-element)
     ;; REFACTOR: Might have some packaging issue here, fix attempt with strings
     (if (equal (symbol-name ui-element) (symbol-name 'tabs))
         (setf ui-element 'tabs))
     (if (equal (symbol-name ui-element) (symbol-name 'status))
         (setf ui-element 'status))
     (setf view (slot-value ui ui-element))
     (when js
         ;; Register exported functions
         (js-export-function view "LispFunc" (callback lisp-from-js/LispFunc))
         (js-export-function view "loadUri" (callback lisp-from-js/loadUri))
         (js-export-function view "promptClose" (callback lisp-from-js/promptClose))

         (js-eval-webview view 
                          (transcompiler 'browserify-coffee
                                         :file "/home/***REMOVED***/dev/lispkit/core/ui/deps.coffee"))
         (js-eval-webview view (ui-content ui-element 'js)))
     (when css
         (js-eval-webview view
                          ;;(transcompiler 'coffee :string ;; Maybe escape with ' and \n -> \\n, rather than coffee
                          ;; NOTE: also escape \'
                          (ppcre:regex-replace-all "\\n"
                                                   (format nil "console.log('style');document.getElementsByTagName('style')[0].innerHTML = '~a'"
                                                           (ui-content ui-element 'css))
                                                   "\\n"))))
    ((and view uri)
     (print "if called, impment me"))))

(defcallback notify-load-status :void
    ((view pobject))
  ;; note: This is where user styles/scripts will be applied
  ;; use an plist to *scripts* -> load-first, finished -> uri -> content description
  (let ((status (webkit-web-view-get-load-status view)))
    (cond
      ((eq status :webkit-load-first-visually-non-empty-layout)
       (let ((uri (property view :uri)))
         (if (ui-scheme-p uri)
             (view-scripts-styles :js t :css t
                                  :ui (browser-ui (browser-find-view-s-instance view))
                                  :ui-element (ui-scheme-uri-to-symbol uri)))))
      ((or (eq status :webkit-load-committed)
           (eq status :webkit-load-finished))
       ;; This may be called too much for the same uri
       (ui-update 
        (browser-ui (browser-find-view-s-instance view))
        'uri (property view :uri))))))

;; Used to load content for ui schemes
(defcallback navigation-request :boolean
    ((source-view pobject)
     (source-frame pobject)
     (request :pointer)
     (action :pointer)
     (policy :pointer))
  (declare (ignore action policy))
  (let ((uri (property 
              (make-instance 'g-object :pointer request)
              :uri)))
    (if (ui-scheme-p uri)
        (webkit-web-frame-load-alternate-string
         source-frame
         (ui-content (ui-scheme-uri-to-symbol uri) 'html)
         uri uri)))
  nil)

;; (defcallback notify-title :void
;;     ((source-view :pointer)
;;      (source-frame :pointer)
;;      (title c-string))
;;   (declare (ignore source-view source-frame))
;;   (print title))

;; Filter common automatic console messages
(defcallback console-message :boolean
;; return true to stop propagation
    ((source-view :pointer)
     (message c-string)
     (line :int)
     (source-id c-string))
  (declare (ignore source-view line source-id))
  ;; (print message)
  ;; if match is true then stop propagation else nil and print like normal
  (ppcre:scan "^Blocked a frame with origin" message))

(defun webview-new (uri)
  "returns a webview with your uri and settings"
  (let ((view (make-instance 'webkit-webview)))
    ;; (webview-change-settings view
    ;;                          '((:enable-plugins nil)
    ;;                            (:enable-scripts nil)
    ;;                            (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:21.0) Gecko/20100101 Firefox/21.0")))
    ;; (setf (property (webkit-get-default-session) :proxy-uri)
    ;;       (soup-uri-new "http://127.0.0.1:8123/"))
    (setf
     (gsignal view "navigation-policy-decision-requested")
     (callback navigation-request)

     (gsignal view "console-message")
     (callback console-message)

     (gsignal view "notify::load-status")
     (callback notify-load-status))

    ;; (gsignal view "notify::title")
    ;;       (callback notify-title))
    (webkit-web-view-load-uri view uri)
    view))
