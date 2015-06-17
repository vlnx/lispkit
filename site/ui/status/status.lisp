(in-package :lispkit)

(defexport download (b str)
  (download-queue-add :uri str :suggested ""))

(defexport notify (b str)
  (ui-update b :notify str))

(defexport load-uri (b uri)
  (webkit-web-view-load-uri (tab-view (current-tab b))
                            (parse-uri uri)))

(defexport status-bar-new-tab (b uri)
  (tab-new b uri :background nil))

(defexport statusbar-request-height (b height)
  (setf height (parse-integer height))
  (setf (size-request
         (tab-scroll (ui-status (browser-ui b))))
        `(-1 ,height)))

(defexport prompt-close (b)
  (run-hook :prompt-leave b))

(defexport statusbar-init (b)
  (ui-updates b :uri t
              :scroll-indicator t
              :progress t
              :history t
              :keymode t
              :current-tab t))

(defexport quit ()
  (sb-ext:exit :abort t))

(defexport filter-hints (b str)
  (js 'hints b (format nil
                       "hints.collection.filterHints('~a');"
                       (escape-single-quote str))))

(defscript (ui-symbol-to-uri 'status)
           :exports (statusbar-init
                     quit
                     status-bar-new-tab
                     load-uri
                     prompt-close
                     statusbar-request-height
                     notify
                     filter-hints
                     download)
           :scripts ((ui/deps . browserify-coffee)
                     (ui/status/ . browserify-coffee))
           :ui-base-html ui/status/
           :styles ui/status/)
