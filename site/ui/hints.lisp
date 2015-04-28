(in-package :lispkit)

(defscript
  :exact-uri (ui-symbol-to-uri 'hints)
  :scripts '(:coffee ((ui/hints (:closure nil))))
  :ui-base-html 'ui/hints
  :styles 'ui/hints)
