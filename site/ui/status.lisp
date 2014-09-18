(in-package :lispkit)

(defexport load-uri (maybe-uri)
  (print "Hello from javascript in lisp")
  (print maybe-uri)
  (finish-output)
  (webkit-web-view-load-uri (tab-view (current-tab)) maybe-uri))

(defexport status-bar-new-tab (maybe-uri)
  (tab-new (current-browser)
           maybe-uri
           :background nil))

(defexport statusbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  (setf (size-request
         (tab-scroll (ui-status (browser-ui (current-browser)))))
        `(-1 ,height)))

(defexport prompt-close ()
  (setf (active-maps (browser-key-state (current-browser)))
        '(:top)))

(defexport statusbar-init ()
  (ui-update (current-browser) ;; XXX: FIX ui-update syntax
             :uri (tab-view (current-tab))
             :scroll-indicator (tab-scroll (current-tab))
             :progress (tab-view (current-tab))
             :history (tab-view (current-tab))))

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
