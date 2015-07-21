(in-package :soup-binding)

(defcfun soup-message-get-uri :pointer
  (soup-message :pointer))

(defcfun soup-message-headers-replace :void
  (request-headers :pointer)
  (header c-string)
  (value c-string))

(defcfun soup-message-headers-remove :void
  (request-headers :pointer)
  (header c-string))

(defcfun soup-message-headers-get-one c-string
  (headers :pointer)
  (name c-string))

(defcfun soup-message-headers-get-list c-string
  (headers :pointer)
  (name c-string))

(defcfun soup-message-add-header-handler :uint
  (message pobject)
  (signal% c-string)
  (header c-string)
  (callback pobject))

(export '(soup-message-get-uri
          soup-message-headers-replace
          soup-message-headers-remove
          soup-message-headers-get-one
          soup-message-headers-get-list
          soup-message-add-header-handler))
