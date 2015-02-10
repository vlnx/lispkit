(in-package :lispkit)


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
  (setf (active-maps (browser-key-state b))
        '(:scroll :top)))

(defexport statusbar-init (b)
  (ui-update b :uri t)
  (ui-update b :scroll-indicator t)
  (ui-update b :progress t)
  (ui-update b :history t)
  (ui-update b :current-tab t))

(defscript
  :exact-uri (ui-symbol-to-uri 'status)
  :exports '(statusbar-init
             status-bar-new-tab
             load-uri
             prompt-close
             statusbar-request-height
             notify)
  :scripts '(:browserify ((ui/deps ())
                          (ui/status (ui/bar/main
                                      ui/bar/history
                                      ui/bar/keymode
                                      ui/bar/progress
                                      ui/bar/tabs
                                      ui/bar/uri
                                      ui/prompt/main
                                      ui/prompt/input
                                      ui/prompt/commands)))
             :coffee ())
  :ui-base-html 'ui/status
  :styles 'ui/status)
