(in-package :lispkit)

(defexport tabbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  ;; If height is 0 it still shows 1 px handle? XXX: if so maybe call hide
  (let ((widget (tab-scroll (ui-tabs (browser-ui (current-browser))))))
    (if (= 0 height)
        (hide widget)
        (progn
          (show widget)
          (setf (size-request widget)
                `(-1 ,height))))))

(defexport tabs-init ()
   (ui-update (current-browser) :tabs-reset-list t)
   (ui-update (current-browser) :current-tab t))

(defscript
    :exact-uri (ui-symbol-to-uri 'tabs)
  :exports '(tabbar-request-height
             tabs-init)
  :scripts '(:browserify ((ui/deps ()))
             :coffee ((ui/tabs (:closure nil))))
  :ui-base-html 'ui/tabs
  :styles 'ui/tabs)

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
