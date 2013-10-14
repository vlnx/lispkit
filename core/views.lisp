(in-package :lispkit)

;; (setf *chrome-tabs* '(:html
;; (transcompile :type 'jade :string "doctype 5")

(defun apply-ui-css (view css)
  (js-eval-webview view
                   ;; Maybe escape with ' and \n -> \\n, rather than coffee
                   (transcompiler 'coffeescript :string (format nil "document.getElementsByTagName('style')[0].innerHTML = '''~a'''"
                                                                css))
                   :source nil :want-return nil))

(defcallback notify-load-status :void
    ((view pobject))
  (if (eq (webkit-web-view-get-load-status view)
          :webkit-load-first-visually-non-empty-layout)
      (let ((uri (property view :uri)))
        (apply-ui-css view (transcompile :file (concatenate 'string *ui-dir* "tabs.stylus")))
        (js-eval-webview view (transcompiler 'coffeescript :file (concatenate 'string *ui-dir* "tabs.coffee"))
                         :source uri :want-return nil)
        (js-eval-webview view (format nil "window.updateUri('~a');" uri)
                         :source uri :want-return nil)))
        ;; (print "body{background-color:#f00}\n")))
        ;; (apply-ui-css view "body{background-color:#f00}\n")))
        ;; (js-eval-webview view (transcompiler 'coffeescript :string "alert 'hello'") :source uri :want-return nil)))
  ;; (print (webkit-web-view-get-load-status view)))
  )

(defcallback navigation-request :boolean
    ((source-view pobject)
     (source-frame pobject)
     (request :pointer)
     (action :pointer)
     (policy :pointer))
  ;; (declare (ignore widget))
  (let ((uri (webkit-network-request-get-uri request)))
    (if (or (string= uri "ui://status") (string= uri "ui://tabs"))
        (progn
          (webkit-web-frame-load-alternate-string
           source-frame
           (transcompile :file (concatenate 'string *ui-dir* "tabs.jade"))
           uri uri)
          (setf (gsignal source-view "notify::load-status")
                (callback notify-load-status)))))
  nil)

;; (defcallback notify-title :void
;;     ((source-view :pointer)
;;      (source-frame :pointer)
;;      (title c-string))
;;   (declare (ignore source-view source-frame))
;;   (print title))

(defun webview-new (uri)
  "returns a webview with your uri and settings"
  (let ((view (make-instance 'webkit-webview)))
    (webview-change-settings view
                             '((:enable-plugins nil)
                               (:enable-scripts nil)
                               (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:21.0) Gecko/20100101 Firefox/21.0")))
    ;; (setf (property (webkit-get-default-session) :proxy-uri)
    ;;       (soup-uri-new "http://127.0.0.1:8123/"))
    (setf (gsignal view "navigation-policy-decision-requested")
          (callback navigation-request))
    ;; (setf (gsignal view "notify::title")
    ;;       (callback notify-title))
    (webkit-web-view-load-uri view uri)
    view))


(defun tab-new (uri)
  (let ((view (webview-new uri)))
    (push view *views*)
    view))


(defun ui-symbol-to-uri (symbol)
  (concatenate 'string "ui://" (string-downcase (symbol-name symbol))))

(defun ui-new-view (ui-element-symbol)
  "Take a symbol of the ui element
Convert it to a uri to load
Convert it to a keyword to set the view instance"
  (setf (getf *ui-views* (as-keyword ui-element-symbol))
        (webview-new (ui-symbol-to-uri ui-element-symbol))))


;; (require :lispkit)(in-package :lispkit)
;; (win)
;; (within-main-loop
;; (js-eval-webview (getf *ui-views* :status) "window.updateUri('worked')" :source nil :want-return nil))
