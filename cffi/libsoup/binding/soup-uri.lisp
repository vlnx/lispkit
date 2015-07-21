(in-package :soup-binding)

(defcfun soup-uri-new :pointer
  (uri c-string))

(defcfun soup-uri-to-string c-string
  (soup-uri :pointer)
  (only-path-and-query :boolean))

(export '(soup-uri-new
          soup-uri-to-string))
