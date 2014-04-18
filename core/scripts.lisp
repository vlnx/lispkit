(in-package :lispkit)

;; Lisp to Javascript Functions
(setf *js-exports* '())

(defexport load-uri (maybe-uri)
  (print "Hello from javascript in lisp")
  (print maybe-uri)
  (webkit-web-view-load-uri (current-tab) maybe-uri))

(defexport statusbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  (setf (size-request
         (ui-status (browser-ui (current-browser)) 'scroll))
        `(-1 ,height)))
(defexport prompt-close ()
  (setf (active-maps (browser-key-state (current-browser)))
        '(:top)))

(defexport tabs-init ()
  (ui-update (current-browser)
             :tabs-reset-list t
             :tabs-switched-page (browser-tabs-current-index (current-browser))))

;; Resource content per uri
(setf *uri-scripts* (make-uri-scripts))

(defscript
    `(:exact-uri ,(ui-symbol-to-uri 'status))
    '(:exports (load-uri prompt-close statusbar-request-height)
      :deps (ui/deps) ;; browserify-coffee
      :scripts (ui/status) ;; look for coffee
      :ui-base-html ui/status ;; look for jade
      :styles (ui/status))) ;; look for css
(defscript
    `(:exact-uri ,(ui-symbol-to-uri 'tabs))
    '(:deps (ui/deps) ;; browserify-coffee
      :exports (tabs-init)
      :scripts (ui/tabs) ;; look for coffee
      :ui-base-html ui/tabs ;; look for jade
      :styles (ui/tabs))) ;; look for css

;; make optional lists
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
    ;; '(:exact-uri "http://10.1.7.1/")
    '(:regex-uri "http://10.1.7.1/*")
    '(:styles (homepage)))
