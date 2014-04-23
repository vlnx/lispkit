(in-package :lispkit)

(defcallback notify-load-status :void
    ((view pobject))
  (let ((status (webkit-web-view-get-load-status view)))
    (cond
      ((eq status :webkit-load-first-visually-non-empty-layout)
       (let ((uri (property view :uri)))
         (mapcar (lambda (result)
                   (invoke-scripts view
                                   (uri-scripts/binding-scripts result)))
                 (lookup-scripts uri))))
      ((or (eq status :webkit-load-committed)
           (eq status :webkit-load-finished))
       ;; This may be called too much for the same uri
       (ui-update 
        (browser-find-instance view :of 'browser :from 'view)
        :uri view)))))

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
         (let ((result (first (lookup-scripts uri))))
           (if result
               (resource-content
                (uri-scripts/scripts-ui-base-html
                 (uri-scripts/binding-scripts result))
                'jade)
               (error "ui uri info not found")))
         uri uri)))
  nil)

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


;; Inspector Signals
(defcallback inspector-start :pointer
    ((inspector pobject)
     (view pobject)) ;; view to be inspected
  (declare (ignore inspector))
  ;; return new webview to place inspector in
  (pointer (make-instance 'webkit-webview :signals nil)))

(defcallback inspector-show :boolean
    ((inspector pobject))
  ;; This function may be called twice so only create one window
  (unless *window-inspector*
    (setf *window-inspector* (make-instance 'window
                                            :width 800 :height 600
                                            :title "LispKit - Inspector"
                                            :has-resize-grip nil))
    (add *window-inspector*
         (webkit-web-inspector-get-web-view inspector))
    (show *window-inspector* :all t))
  t)

(defcallback notify-title :void
    ((view pobject)
     (source-frame :pointer)
     (title :pointer))
  (declare (ignore source-frame title))
  (ui-update (browser-find-instance view
                         :of 'browser
                         :from 'view)
             :tabs-update-title view))

;; Called on scrolling mouse events
(defcallback scroll-event :boolean
    ((widget pobject)
     (event :pointer))
  (declare (ignore widget event))
  (ui-update (current-browser)
              :scroll-indicator (current-tab 'scroll)))

;; '((:enable-plugins nil)
;;   (:enable-scripts nil)
;;   (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:21.0) Gecko/20100101 Firefox/21.0")))
;; (setf (property (webkit-get-default-session) :proxy-uri)
;;       (soup-uri-new "http://127.0.0.1:8123/"))
(defun connect-webview-signals (view)
  (setf
   (gsignal view "navigation-policy-decision-requested")
   (callback navigation-request)

   (gsignal view "console-message")
   (callback console-message)

   (gsignal view "scroll-event")
   (callback scroll-event)

   (gsignal view "notify::load-status")
   (callback notify-load-status))
  
  (let ((inspector (make-instance 'g-object
                                  :pointer
                                  (webkit-web-view-get-inspector view))))
    (setf (gsignal inspector "inspect-web-view")
          (callback inspector-start))
    (setf (gsignal inspector "show-window")
          (callback inspector-show))))

(defmethod initialize-instance :after ((view webkit-webview)
                                       &key uri
                                         (settings t)
                                         (signals t))
  "Load settings and provided uri for the new webview"
  (when settings
    (webview-change-settings view
                             '((:enable-developer-extras t))))
  (when signals
    (connect-webview-signals view))
  (when uri
    (webkit-web-view-load-uri view uri)))
