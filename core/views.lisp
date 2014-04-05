(in-package :lispkit)

(defcallback notify-load-status :void
    ((view pobject))
  ;; note: This is where user styles/scripts will be applied
  ;; use an plist to *scripts* -> load-first, finished -> uri -> content description
  (let ((status (webkit-web-view-get-load-status view)))
    (cond
      ((eq status :webkit-load-first-visually-non-empty-layout)
       (let ((uri (property view :uri)))
         (if (ui-scheme-p uri)
             (let ((result (lookup-scripts uri)))
               (when result
                 (invoke-scripts view
                                 (uri-scripts/binding-scripts result)))))))
      ;; (view-scripts-styles :js t :css t
      ;;                      :ui (browser-ui (browser-find-instance-from :view view))
      ;;                      :ui-element (ui-scheme-uri-to-symbol uri)))))
      ((or (eq status :webkit-load-committed)
           (eq status :webkit-load-finished))
       ;; This may be called too much for the same uri
       (ui-update 
        (browser-ui (browser-find-instance-from :view view))
        :uri (or (property view :uri) "about:blank"))))))

;; Used to load content for ui schemes
(defcallback navigation-request :boolean
    ((source-view pobject)
     (source-frame pobject)
     (request :pointer)
     (action :pointer)
     (policy :pointer))
  (declare (ignore action policy source-view))
  (let ((uri (property 
              (make-instance 'g-object :pointer request)
              :uri)))
    (if (ui-scheme-p uri)
        (webkit-web-frame-load-alternate-string
         source-frame
         (let ((result (lookup-scripts uri)))
           (if result
               (resource-content
                (uri-scripts/scripts-ui-base-html
                 (uri-scripts/binding-scripts result))
                'jade)
               (error "ui uri info not found")))
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
