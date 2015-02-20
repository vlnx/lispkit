(in-package :lispkit)

(defcallback notify-load-status :void
    ((view pobject))
  (let ((status (webkit-web-view-get-load-status view)))
    (cond
      ;; Invoke scripts
      ((eq status :webkit-load-first-visually-non-empty-layout)
       (let ((uri (property view :uri)))
         (mapcar (lambda (result)
                   (invoke-scripts view
                                   (uri-scripts/binding-scripts result)))
                 (lookup-scripts uri))))
      ;; Update ui
      ((or (eq status :webkit-load-committed)
           (eq status :webkit-load-finished))
       ;; Because these are not loaded in the main tab, don't update for them
       (unless (or (string= "ui://tabs" (property view :uri))
                   (string= "ui://status" (property view :uri)))
         (let ((b (find-instance 'of-browser 'from-view view)))
           (ui-update b :tabs-update-title
                      (find-instance 'of-tab 'from-view view))
           (ui-update b :uri t)
           (ui-update b :history t)))))))

;; Used to load content for ui schemes
(defcallback navigation-request :boolean
    ((source-view pobject)
     (source-frame pobject)
     (request :pointer)
     (action :pointer)
     (policy :pointer))
  (declare (ignore action policy source-view))
  (let ((uri (property (make-instance 'g-object :pointer request)
                       :uri)))
    (when (ui-scheme-p uri)
      (let ((result (first (lookup-scripts uri))))
        (unless result
          (dmesg "ui uri info not found, loading blank instead")
          (setf uri (ui-symbol-to-uri 'blank))
          (setf result (first (lookup-scripts uri))))
        (webkit-web-frame-load-alternate-string
         source-frame
         (resource-content (uri-scripts/scripts-ui-base-html
                            (uri-scripts/binding-scripts result))
                           'jade)
         uri uri))))
  nil)

;; Filter common automatic console messages
(defcallback console-message :boolean ; return true to stop propagation
    ((source-view :pointer)
     (message c-string)
     (line :int)
     (source-id c-string))
  (declare (ignore source-view line source-id))
  ;; if match is true then stop propagation else nil and print like normal
  (ppcre:scan "^Blocked a frame with origin" message))


;; Inspector Signals
(defcallback inspector-close :void
    ((window pobject))
  (let ((tab (find-instance 'of-tab 'from-inspector-window window)))
    (when tab
      ;; Also destroy the view attached?
      (webkit-web-inspector-close
       (pointer (inspector-gobject (tab-inspector tab))))
      (destroy window)
      (setf (tab-inspector tab) nil))))

(defcallback inspector-start :pointer
    ((inspector-obj :pointer)
     (view pobject)) ; view to be inspected
  (let ((tab (find-instance 'of-tab 'from-view view)))
    (setf (tab-inspector tab)
          (make-instance 'inspector
                         ;; Create a new view
                         :view (make-instance 'webkit-webview :signals nil)
                         :gobject (make-instance 'webview-inspector :view (tab-view tab))))
    ;; return new webview to place inspector in
    (pointer (inspector-view (tab-inspector tab)))))

(defcallback inspector-show :boolean
    ((inspector-obj :pointer))
  (let ((inspector (find-instance 'of-inspector 'from-inspector-pointer
                                  inspector-obj)))
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
  (ui-update (find-instance 'of-browser 'from-view view)
             :tabs-update-title (find-instance 'of-tab 'from-view view)))

;; Connected to "scroll-event" for mouse wheel scrolling
;; Also connected to "draw", called on re-rendering of the view
(defcallback scroll-event :boolean
    ((view pobject)
     (event :pointer))
  (declare (ignore event))
  (let ((b (find-instance 'of-browser 'from-view view)))
    (if (eq (tab-view (current-tab b))
            view)
        (ui-update b :scroll-indicator t)))
  nil) ; continue

(defcallback notify-progress :void
    ((source-view pobject))
  (declare (ignore source-view))
  ;; First called before browser is set
  (when (and (current-browser)
             (current-tab (current-browser)))
    (ui-update (current-browser) :progress t)))

(defcallback hovering-over-link :void
    ((view pobject)
     (title c-string)
     (uri c-string))
  (declare (ignore title))
  (ui-update
   (find-instance 'of-browser 'from-view view)
   :link-hover (or uri "")))

(defcallback create-web-view :pointer
    ((src-view pobject)
     (src-frame pobject))
  (declare (ignore src-frame))
  ;; Create a new tab and return the view that the new content will use
  (pointer (tab-view
            (tab-new (find-instance 'of-browser 'from-view src-view)
                     (parse-uri nil)
                     :background nil))))

(defun reload-view (view)
  (webkit-web-view-load-uri view
                            (property view :uri)))

(defun connect-webview-signals (view &key ui-only-view)
  "Connect signals to new webviews, if the view is intended for ui only,
don't connect signals that update the status bar"

  ;; Don't set these for ui views
  (unless ui-only-view
    (setf
     (gsignal view "scroll-event")
     (callback scroll-event)

     (gsignal view "draw")
     (callback scroll-event)

     (gsignal view "notify::title")
     (callback notify-title)

     (gsignal view "hovering-over-link")
     (callback hovering-over-link)

     (gsignal view "notify::progress")
     (callback notify-progress)

     (gsignal view "create-web-view")
     (callback create-web-view)))

  (setf
   (gsignal view "navigation-policy-decision-requested")
   (callback navigation-request)

   (gsignal view "notify::load-status")
   (callback notify-load-status)

   (gsignal view "console-message")
   (callback console-message))

  ;; Set Inspector signals
  (let ((inspector (make-instance 'webview-inspector :view view)))
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
                             '((:enable-developer-extras t)
                               (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:31.0) Gecko/20100101 Firefox/31.0"))))
  ;; (:enable-plugins nil)
  ;; (:enable-scripts nil)
  (when signals
    (connect-webview-signals view
                             :ui-only-view (ui-scheme-p uri)))
  ;; Even though a view can start without a uri, that point can't be
  ;; reached again, so start by loading some thing that can be reached again
  (webkit-web-view-load-uri view (parse-uri uri)))

(defun parse-uri (maybe-uri)
  "Return a uri that can be loaded by webkit"
  ;; If nil or just doesn't contain :// go to a blank page
  (if (ppcre:scan-to-strings "://" maybe-uri)
      maybe-uri
      (ui-symbol-to-uri 'blank)))
