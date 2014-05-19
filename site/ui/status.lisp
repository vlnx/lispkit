(in-package :lispkit)

(defexport load-uri (maybe-uri)
  (print "Hello from javascript in lisp")
  (print maybe-uri)
  (finish-output)
  (webkit-web-view-load-uri (current-tab) maybe-uri))

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

(defscript
    :exact-uri (ui-symbol-to-uri 'status)
  :exports '(status-bar-new-tab
             load-uri
             prompt-close
             statusbar-request-height)
  :deps 'ui/deps
  :scripts 'ui/status
  :ui-base-html 'ui/status
  :styles 'ui/status)
