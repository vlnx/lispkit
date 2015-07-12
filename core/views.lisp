(in-package :lispkit)

(defun uri-expected-in-main-tab-p (uri)
  (null (or (string= uri "ui://tabs")
            (string= uri "ui://status"))))

(defcallback notify-load-status :void
    ((view pobject))
  (case (webkit-web-view-get-load-status view)
    ;; expect this stage to mean:
    ;; not done rendering or loading resources
    (:webkit-load-first-visually-non-empty-layout
     (invoke-scripts (property view :uri) view))
    ;; Update ui
    ((or :webkit-load-committed
         :webkit-load-finished)
     (when (uri-expected-in-main-tab-p (property view :uri))
       (ui-updates
        (find-instance 'of-browser 'from-view view)
        :tabs-update-title (find-instance 'of-tab 'from-view view)
        :uri t
        :history t)))))

;; Used to load content for ui schemes
(defcallback navigation-request :boolean
    ((source-view pobject)
     (source-frame pobject)
     (request :pointer)
     (action :pointer)
     (policy :pointer))
  (declare (ignore action source-view))
  (let ((uri (property (make-instance 'g-object :pointer request)
                       :uri)))
    (cond
      ;; Don't let webkit attempt to load schemas it can't handle,
      ((or (ppcre:scan-to-strings "^mailto:" uri)
           (ppcre:scan-to-strings "^git:" uri)
           (ppcre:scan-to-strings "^javascript:" uri)
           (ppcre:scan-to-strings "^magnet:" uri)
           (ppcre:scan-to-strings ".webm$" uri))
       ;; TODO: offer to yank uri, or open with other program
       (ui-update (current-browser) :notify uri)
       (webkit-web-policy-decision-ignore policy))
      ;; Load ui schema
      ((ui-scheme-p uri)
       (let ((result (first (lookup-scripts uri))))
         (unless result
           (dmesg "ui uri info not found, loading blank instead")
           (setf uri (ui-symbol-to-uri 'blank))
           (setf result (first (lookup-scripts uri))))
         (webkit-web-frame-load-alternate-string
          source-frame
          (resource-content (uri-scripts-ui-base-html result)
                            'jade)
          uri uri))
       (webkit-web-policy-decision-use policy))
      ;; expclit download request
      ((ppcre:scan-to-strings ".js$" uri)
       (ui-update (current-browser) :notify uri)
       (webkit-web-policy-decision-download policy))
      ;; Default, allow request
      (t (webkit-web-policy-decision-use policy))))
  t) ; handled the policy decision

(defcallback download-request :boolean
    ((source-view pobject)
     (webkit-download :pointer))
  "Take info from given download (synchronous), and add
to `*download-queue*', so a new async download can start"
  (declare (ignore source-view))
  (download-queue-add
   :uri (webkit-download-get-uri webkit-download)
   :suggested (webkit-download-get-suggested-filename
               webkit-download))
  nil) ; cancel the given download

;; Filter common automatic console messages
(defcallback console-message :boolean
    ((source-view :pointer)
     (message c-string)
     (line :int)
     (source-id c-string))
  (declare (ignore source-view line source-id))
  ;; if match is true then stop propagation
  (or
   (ppcre:scan "^Blocked a frame with origin" message)
   ;; When polipo replaces a resource with an empty gif
   (ppcre:scan "^SyntaxError: Invalid character '\\\\u0001'"
               message)))

;; Inspector Signals
(defcallback inspector-close :void
    ((window pobject))
  (let ((tab (find-instance 'of-tab 'from-inspector-window window)))
    (when tab
      (webkit-web-inspector-close
       (pointer (inspector-gobject (tab-inspector tab))))
      (destroy window)
      (setf (tab-inspector tab) nil))))

(defcallback inspector-start :pointer
    ((inspector-obj :pointer)
     (view pobject)) ; view to be inspected
  (let ((tab (find-instance 'of-tab 'from-view view)))
    (setf (tab-inspector tab)
          (make-instance
           'inspector
           :view (make-instance 'webkit-webview :signals nil)
           :gobject (make-instance 'webview-inspector
                                   :view (tab-view tab))))
    ;; return new webview to place the inspector in
    (pointer (inspector-view (tab-inspector tab)))))

(defcallback inspector-show :boolean
    ((inspector-obj :pointer))
  (let ((inspector
         (find-instance 'of-inspector 'from-inspector-pointer
                        inspector-obj)))
    (unless (inspector-shown inspector)
      (setf (gsignal (inspector-window inspector) "destroy")
            (callback inspector-close)
            (inspector-shown inspector) t)
      (add (inspector-window inspector)
           (inspector-view inspector))
      (show (inspector-window inspector) :all t)
      t)))

(defun open-inspector (tab)
  (webkit-web-inspector-show
   (make-instance 'webview-inspector
                  :view (tab-view tab))))

(defcallback notify-title :void
    ((view pobject)
     (source-frame :pointer)
     (title :pointer))
  (declare (ignore source-frame title))
  (ui-update
   (find-instance 'of-browser 'from-view view)
   :tabs-update-title (find-instance 'of-tab 'from-view view)))

;; Connect to "scroll-event" for mouse wheel scrolling
;; and to "draw", called on re-rendering of the view
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
  (ui-update (find-instance 'of-browser 'from-view view)
             :link-hover (or uri "")))

(defcallback create-web-view :pointer
    ((src-view pobject)
     (src-frame pobject))
  (declare (ignore src-frame))
  ;; return the view that the new content will use
  (pointer
   (tab-view (tab-new (find-instance 'of-browser 'from-view src-view)
                      (parse-uri nil)
                      :background nil))))

(defun reload-view (view)
  (webkit-web-view-load-uri view (property view :uri)))

(defun connect-webview-signals (view &key ui-only-view)
  "Connect signals. If the view is intended for ui only, don't
connect signals that update the status bar"

  ;; Don't set these for ui views
  (unless ui-only-view
    (setf (gsignal view "scroll-event")
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

  (setf (gsignal view "navigation-policy-decision-requested")
        (callback navigation-request)

        (gsignal view "download-requested")
        (callback download-request)

        (gsignal view "notify::load-status")
        (callback notify-load-status)

        (gsignal view "console-message")
        (callback console-message))

  ;; Set Inspector signals
  (let ((inspector (make-instance 'webview-inspector :view view)))
    (setf (gsignal inspector "inspect-web-view")
          (callback inspector-start)
          (gsignal inspector "show-window")
          (callback inspector-show))))

(defmethod initialize-instance
    :after ((view webkit-webview) &key uri (settings t) (signals t))
  "Load settings and the provided uri for the new webview"
  (when settings
    (webview-change-settings view
                             '((:enable-developer-extras t)
                               (:user-agent "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:31.0) Gecko/20100101 Firefox/31.0"))))
  (when signals
    (connect-webview-signals view
                             :ui-only-view (ui-scheme-p uri)))
  (webkit-web-view-load-uri view (parse-uri uri)))
