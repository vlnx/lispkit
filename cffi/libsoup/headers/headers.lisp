(in-package :soup-headers)

(defun censor-referer (message-headers uri)
  "Censor the http referer header.
NOTE: WebKit will still hand out the real referer with javascript's
=document.referrer="
  (declare (type quri:uri uri))
  (let ((referer (soup-message-headers-get-one
                  message-headers "Referer")))
    (when (and referer
               (string/= (quri:uri-domain uri)
                         (quri:uri-domain (quri:uri referer))))
      (soup-message-headers-replace
       message-headers "Referer"
       ;; Reconstruct the uri as coming from the root
       (concatenate 'string
                    (quri:uri-scheme uri)
                    "://"
                    (quri:uri-domain uri)
                    "/")))))

(defcallback process-set-cookie-header :void
    ((message pobject))
  (let ((res (property (make-instance 'g-object :pointer message)
                       :response-headers))
        (uri (quri:uri (soup-uri-to-string
                        (soup-message-get-uri message) nil))))
    (handle-set-cookie (soup-message-headers-get-list res
                                                      "Set-Cookie")
                       uri)))

(defcallback write-headers :void
    ((message pobject))
  (let ((req (property (make-instance 'g-object :pointer message)
                       :request-headers))
        (uri (quri:uri (soup-uri-to-string
                        (soup-message-get-uri message) nil))))
    (censor-referer req uri)
    (give-cookies req uri)))

(defcallback request-queued :void
    ((session :pointer)
     (message pobject))
  (setf (gsignal (make-instance 'g-object :pointer message)
                 "starting") ; "wrote-headers"
        (callback write-headers))
  (soup-message-add-header-handler
   message "got-headers"
   "Set-Cookie"
   (callback process-set-cookie-header)))

(defun handle-soup-headers (session)
  (disable-internal-jar session)
  (setf (gsignal session "request-queued")
        (callback request-queued)))
