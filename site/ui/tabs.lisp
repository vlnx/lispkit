(in-package :lispkit)

(defexport tabbar-request-height (height) ;; Number
  (setf height (parse-integer height))
  ;; If height is 0 it still shows 1 px handle? XXX: if so maybe call hide
  (setf (size-request
         (tab-scroll (ui-tabs (browser-ui (current-browser))))
        `(-1 ,height))))

(defscript
    :exact-uri (ui-symbol-to-uri 'tabs)
  :deps 'ui/deps
  :exports '(tabbar-request-height)
  :scripts 'ui/tabs
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
;; (defexport tabs-init ()
;;    (ui-update (current-browser)
;;               :tabs-reset-list t
;;               :tabs-switched-page (browser-tabs-current-index (current-browser))))
