(in-package :lispkit)

(defexport send-click-to-current-tab (b json-string)
  (let ((json (json:decode-json-from-string json-string)))
    (js 'current-tab b 
        (coffee-template 'send-click-to-position
                         :x (cdr (assoc :x json))
                         :y (cdr (assoc :y json))))))

(defexport send-prompt-close (b)
  (js 'status b "bar.prompt.close()"))

(defscript
  :exact-uri (ui-symbol-to-uri 'hints)
  :exports '(send-click-to-current-tab
             send-prompt-close)
  :scripts '(:browserify ((ui/deps ()))
             :coffee ((ui/hints (:closure nil))))
  :ui-base-html 'ui/hints
  :styles 'ui/hints)
