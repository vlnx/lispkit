(in-package :lispkit)

(defexport tabbar-request-height (b height)
  (setf height (parse-integer height))
  (let ((widget (tab-scroll (ui-tabs (browser-ui b)))))
    (if (= 0 height)
        (hide widget)
        (progn
          (show widget)
          (setf (size-request widget)
                `(-1 ,height))))))

(defexport tabs-init (b)
  (ui-update b :tabs-reset-list t)
  (ui-update b :current-tab t))

(defscript (ui-symbol-to-uri 'tabs)
           :exports (tabbar-request-height
                     tabs-init)
           :scripts ((ui/deps . browserify-coffee)
                     (ui/tabs/ . coffee))
           :ui-base-html ui/tabs/
           :styles ui/tabs/)
