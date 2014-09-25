(in-package :lispkit)


(defexport load-uri (uri)
  (webkit-web-view-load-uri (tab-view (current-tab))
                            (parse-uri uri)))

(defexport status-bar-new-tab (uri)
  (tab-new (current-browser) uri :background nil))

(defexport statusbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  (setf (size-request
         (tab-scroll (ui-status (browser-ui (current-browser)))))
        `(-1 ,height)))

(defexport prompt-close ()
  (setf (active-maps (browser-key-state (current-browser)))
        '(:top)))

(defexport statusbar-init ()
  (ui-update (current-browser) :uri t)
  (ui-update (current-browser) :scroll-indicator t)
  (ui-update (current-browser) :progress t)
  (ui-update (current-browser) :history t)
  (ui-update (current-browser) :current-tab t))

(defscript
    :exact-uri (ui-symbol-to-uri 'status)
  :exports '(statusbar-init
             status-bar-new-tab
             load-uri
             prompt-close
             statusbar-request-height)
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
