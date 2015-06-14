(in-package :webkit-binding)

(defcfun webkit-download-get-uri c-string
  (download-obj :pointer))

(defcfun webkit-download-get-suggested-filename c-string
  (download-obj :pointer))

(defcfun webkit-download-new :pointer
  (network-obj :pointer))

(defcfun webkit-network-request-new :pointer
  (uri c-string))

(defcfun webkit-download-start :void
  (download-obj :pointer))

(defcfun webkit-download-set-destination-uri :void
  (download-obj :pointer)
  (uri c-string))

(export '(webkit-download-get-uri
          webkit-download-get-suggested-filename
          webkit-download-new
          webkit-network-request-new
          webkit-download-start
          webkit-download-set-destination-uri))
