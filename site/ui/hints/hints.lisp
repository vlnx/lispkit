(in-package :lispkit)

(defexport send-click-to-current-tab (b json-string)
  (let ((json (json:decode-json-from-string json-string)))
    (js 'current-tab b
        (coffee-template 'send-click-to-position
                         :x (cdr (assoc :x json))
                         :y (cdr (assoc :y json))))))

(defexport send-prompt-close (b)
  (js 'status b "bar.prompt.close()"))

(defexport yank-string (b str)
  (yank-string b str))

(defexport new-background-tab (b uri)
  (tab-new b uri :background t))

(defscript (ui-symbol-to-uri 'hints)
           :exports (send-click-to-current-tab
                     send-prompt-close
                     yank-string
                     new-background-tab)
           :scripts ((ui/deps . coffeeify-minimal)
                     (ui/hints/ . coffee))
           :ui-base-html ui/hints/
           :styles ui/hints/)
