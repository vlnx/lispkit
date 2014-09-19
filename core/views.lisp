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
       (unless (ui-scheme-p (uri-fallback (property view :uri)))
         (let ((b
                (browser-find-instance view
                                       :of 'browser
                                       :from 'view)))
           (when b
             (ui-update b :uri view)
             (ui-update b :history view))))))))

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
(defcallback inspector-close :void
  ((window pobject))
  (let ((tab (browser-find-instance window
                                    :of 'tab
                                    :from 'inspector-window)))
    (when tab
      ;; Also destroy the view attached?
      (webkit-web-inspector-close
       (inspector-pointer (tab-inspector tab)))
      (destroy window)
      (setf (tab-inspector tab) nil))))

(defcallback inspector-start :pointer
  ((inspector-obj pobject)
   (view pobject)) ;; view to be inspected
  (let ((tab (browser-find-instance view
                                    :of 'tab :from 'view)))
    (setf (tab-inspector tab)
          (make-instance 'inspector
                         :pointer inspector-obj
                         :view (make-instance 'webkit-webview :signals nil)))
    ;; return new webview to place inspector in
    (pointer (inspector-view (tab-inspector tab)))))

(defcallback inspector-show :boolean
  ((inspector-obj pobject))
  (let ((inspector (browser-find-instance inspector-obj
                                          :of 'inspector
                                          :from 'inspector-pointer)))
    (when (and inspector
               (null (inspector-shown inspector)))
      (setf (gsignal (inspector-window inspector) "destroy")
            (callback inspector-close)
            (inspector-shown inspector) t)
      (add (inspector-window inspector)
           (inspector-view inspector))
      (show (inspector-window inspector) :all t)
      t)))

(defcallback notify-title :void
  ((view pobject)
   (source-frame :pointer)
   (title :pointer))
  (declare (ignore source-frame title))
  ;; TODO: On notify-title, if the connected tab has an inspector
  ;; append new title to the inspector window
  (ui-update (browser-find-instance view
                                    :of 'browser
                                    :from 'view)
             :tabs-update-title view))

;; Connected to "scroll-event" for mouse wheel scrolling
;; Also connected to "draw", called on re-rendering of the view
(defcallback scroll-event :boolean
  ((view pobject)
   (event :pointer))
  (declare (ignore event))
  (let ((b (browser-find-instance view
                         :of 'browser
                         :from 'view)))
    (if (eq (tab-view (current-tab b))
            view)
          (ui-update b
                     :scroll-indicator (tab-scroll (current-tab b)))))
  nil) ; continue

(defcallback notify-progress :void
  ((source-view pobject))
  (when (current-browser) ;; First called before browser is set
    (ui-update (current-browser) :progress source-view)))

;; TODO: Settings
;; '((:enable-plugins nil)
;;   (:enable-scripts nil)
;;   (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:21.0) Gecko/20100101 Firefox/21.0")))
;; TODO: Proxy
;; (setf (property (webkit-get-default-session) :proxy-uri)
;;       (soup-uri-new "http://127.0.0.1:8123/"))

(defun reload-view (view)
  (webkit-web-view-load-uri
   view
   (property view :uri)))

(defun connect-webview-signals (view &key ui-only-view)
  "Connect signals to new webviews, if the view is intended for ui only,
don't connect signals that update the status bar"

  (unless ui-only-view
    (setf
     (gsignal view "scroll-event")
     (callback scroll-event)

     (gsignal view "draw")
     (callback scroll-event)

     (gsignal view "notify::title")
     (callback notify-title)

     (gsignal view "notify::progress")
     (callback notify-progress)))

  (setf
   (gsignal view "navigation-policy-decision-requested")
   (callback navigation-request)

   (gsignal view "notify::load-status")
   (callback notify-load-status)

   (gsignal view "console-message")
   (callback console-message))

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
    (connect-webview-signals view
                             :ui-only-view (ui-scheme-p uri)))
  (when uri
    (webkit-web-view-load-uri view uri)))
