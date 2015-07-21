(in-package :soup-headers)

(defclass cookie-jar ()
  ((content :accessor jar-content
            :initform (cookie:make-cookie-jar)
            :documentation "list of cookies")
   (storage-file :accessor jar-file
                 :initarg :storage-file
                 :initform (error "Must supply a :storage-file")
                 :documentation "pathname file to store the jar")
   (whitelist :accessor jar-whitelist
              :initarg :whitelist
              :initform nil
              :documentation
              "list of domains accepted, when nil accept all")))

(defvar *cookie-jar-directory* #P"/tmp/")

(defvar *cookie-jars* nil
  "plist of jars")

(defun create-cookie-jar (name &key whitelist)
  "Generate a storage-file path from the name of the jar"
  (let ((file (make-pathname
               :directory (pathname-directory *cookie-jar-directory*)
               :name (concatenate 'string
                                  (string-downcase (symbol-name name))
                                  ".dump")
               :type "lisp")))
    (setf (getf *cookie-jars* name)
          (make-instance 'cookie-jar
                         :storage-file file
                         :whitelist whitelist))))

(defvar *cookie-jar-active-name* :default
  "name of the active jar")

(create-cookie-jar *cookie-jar-active-name*)

(defun current-jar ()
  (getf *cookie-jars* *cookie-jar-active-name*))

(defun store-jar (jar)
  (declare (type cookie-jar jar))
  (with-open-file (stream (jar-file jar)
                          :direction :output
                          :if-exists :supersede)
    (with-standard-io-syntax
      (print (jar-content jar) stream))))

(defmethod initialize-instance :after ((jar cookie-jar) &key)
  "Load the `jar-content' if `jar-file' exists"
  (when (probe-file (jar-file jar))
    (with-open-file (stream (jar-file jar))
      (with-standard-io-syntax
        (setf (jar-content jar) (read stream))))))

(defun clear-jar (jar)
  (declare (type cookie-jar jar))
  (setf (jar-content jar)
        (cookie:make-cookie-jar))
  (store-jar jar))

(defun domain-in-whitelist-p (domain whitelist)
  (declare (type string domain)
           (type list whitelist))
  (remove-if-not (lambda (item)
                   (or (string= item domain)
                       (ppcre:scan (concatenate 'string
                                                "^.*\\." item "$")
                                   domain)))
                 whitelist))

(defun accept-cookie-p (cookie jar)
  (declare (type cookie:cookie cookie)
           (type cookie-jar jar))
  (or (domain-in-whitelist-p
       (or (cookie:cookie-domain cookie)
           (cookie:cookie-origin-host cookie))
       (jar-whitelist jar))
      (null (jar-whitelist jar)))) ; Accept all if no whitelist

(defun handle-set-cookie (set-cookie origin-uri)
  "Parse the header and merge cookies if they are accepted"
  (declare (type string set-cookie)
           (type quri:uri origin-uri))
  (let* ((jar (current-jar))
         (cookies (remove-if-not (lambda (cookie)
                                   (accept-cookie-p cookie jar))
                                 (cookie:parse-set-cookie-header
                                  set-cookie
                                  (quri:uri-host origin-uri)
                                  (quri:uri-path origin-uri)))))
    (when cookies
      (cookie:merge-cookies (jar-content jar) cookies)
      (store-jar jar))))

(defun give-cookies (req uri)
  "If cookies in the current jar apply to `uri' set the \"Cookie\"
header on `req'"
  (declare (type quri:uri uri))
  (let ((cookies
         (when (quri:uri-domain uri)
           (cookie:cookie-jar-host-cookies
            (jar-content (current-jar))
            (quri:uri-domain uri)
            (quri:uri-path uri)
            :securep (string= (quri:uri-scheme uri)
                              "https")))))
    (if cookies
        (soup-message-headers-replace req "Cookie"
                                      (cookie:write-cookie-header
                                       cookies))
        (soup-message-headers-remove req "Cookie"))))
