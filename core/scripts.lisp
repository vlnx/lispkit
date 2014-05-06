(in-package :lispkit)

;; Clear exports
(setf *js-exports* '())

(defexport load-uri (maybe-uri)
  (print "Hello from javascript in lisp")
  (print maybe-uri)
  (finish-output)
  (webkit-web-view-load-uri (current-tab) maybe-uri))

(defexport status-bar-new-tab (maybe-uri)
  (tab-new (current-browser)
           maybe-uri
           :background nil))


(defexport tabbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  ;; If height is 0 it still shows 1 px handle? XXX: if so maybe call hide
  (setf (size-request
         (tab-scroll (ui-tabs (browser-ui (current-browser))))
        `(-1 ,height))))

(defexport statusbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  (setf (size-request
         (tab-scroll (ui-status (browser-ui (current-browser)))))
        `(-1 ,height)))

(defexport prompt-close ()
  (setf (active-maps (browser-key-state (current-browser)))
        '(:top)))

;; fixme: still
;; (defmacro once-browser-exists (&body body)
;;   `(if (current-browser)
;;        (progn ,@body)
;;        (loop while (null (current-browser)) do
;;             (progn ,@body))))
;; (defexport uitabs-tabs-exist-p ()
;;   (if (and (current-browser)
;;            (first (browser-tabs (current-browser))))
;;       "true"
;;       "false"))
;; (defexport tabs-init ()
;;    (ui-update (current-browser)
;;               :tabs-reset-list t
;;               :tabs-switched-page (browser-tabs-current-index (current-browser))))

;; Clear resources per uri
(setf *uri-scripts* (make-uri-scripts))

(defscript
    `(:exact-uri ,(ui-symbol-to-uri 'status))
    '(:exports (status-bar-new-tab
                load-uri
                prompt-close
                statusbar-request-height)
      :deps (ui/deps) ;; browserify-coffee
      :scripts (ui/status) ;; look for coffee
      :ui-base-html ui/status ;; look for jade
      :styles (ui/status))) ;; look for css
(defscript
    `(:exact-uri ,(ui-symbol-to-uri 'tabs))
    '(:deps (ui/deps) ;; browserify-coffee
      :exports (tabbar-request-height)
      :scripts (ui/tabs) ;; look for coffee
      :ui-base-html ui/tabs ;; look for jade
      :styles (ui/tabs))) ;; look for css

;; TODO: make optional lists
;; (defscript :exact-uri (ui-symbol-to-uri 'status)
;;   :exports (load-uri prompt-close)
;;   :browserify-coffee ui/deps
;;   :coffee ui/status
;;   :jade ui/status
;;   :stylus ui/status)
;; (defscript :exact-uri (ui-symbol-to-uri 'tabs)
;;   :browserify-coffee ui/deps
;;   :coffee ui/tabs
;;   :jade ui/tabs
;;   :stylus ui/tabs)

(defscript
    '(:regex-uri "http://10.1.7.1/*")
    '(:styles (homepage)))
