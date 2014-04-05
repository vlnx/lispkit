(in-package :lispkit)

;; Lisp to Javascript Functions
(setf *js-exports* '())

(defexport load-uri (maybe-uri)
  (print "Hello from javascript in lisp")
  (print maybe-uri)
  (webkit-web-view-load-uri (current-tab) maybe-uri))

(defexport prompt-close ()
  (setf (active-maps (browser-key-state (current-browser)))
        '(:top)
        (size-request
         (ui-status (browser-ui (current-browser)) 'scroll))
        '(-1 16)))

;; Resource content per uri
(setf *uri-scripts* (make-uri-scripts))

(defscript
    `(:exact-uri ,(ui-symbol-to-uri 'status))
    '(:exports (load-uri prompt-close)
      :deps (ui/deps) ;; browserify-coffee
      :scripts (ui/status) ;; look for coffee
      :ui-base-html ui/status ;; look for jade
      :styles (ui/status))) ;; look for css
(defscript
    `(:exact-uri ,(ui-symbol-to-uri 'tabs))
    '(:deps (ui/deps) ;; browserify-coffee
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
