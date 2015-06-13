(in-package :lispkit)
;; In general, use plists when useful, like not polluting the scope

(defvar *lispkit-cache-dir* (concatenate 'string
                                         (sb-ext:posix-getenv "XDG_CACHE_HOME")
                                         "/lispkit/")
  "The directory to store compiled scripts")

(sb-ext:run-program "/bin/mkdir" (list "-p" *lispkit-cache-dir*))

(defvar *site-dir* (concatenate 'string
                                (sb-ext:posix-getenv "DEV_HOME")
                                "/lispkit/site/")
  "Path where all page modification data is located")

(defvar *uri-homepage* "http://vlnx.lan/startpage/"
  "The homepage uri to load by default")

(defvar *maps* '()
  "A plist of map names to a kmap structure")

(defvar *browser-current-index* 0)

(defvar *browsers* nil
  "The list for the open browser instances, that contain top-level windows")

(defun ui-scheme-p (uri)
  (ppcre:scan-to-strings "^ui://" uri))

(defun ui-symbol-to-uri (symbol)
  (concatenate 'string "ui://" (string-downcase (symbol-name symbol))))

(defun ui-scheme-uri-to-symbol (uri)
  (as-symbol (ppcre:regex-replace "^ui://" uri "")))

(defvar *hooks* '()
  "The plist for hooks")

(defun run-hook (hook &rest args)
  "Call each function in HOOK and pass args to it."
  ;; NOTE: look to stumpwm to expand hooks
  (dolist (fn (getf *hooks* hook))
    (apply fn args)))

(defvar *uri-scripts* nil
  "Global var for for uri to scripts structures")

(defvar *js-exports* nil
  "plist of symbols to callback locations")

(defvar *script-list* nil
  "A list of symbols to turn to strings and load lisp files
from when appended to *site-dir*")

(defvar *download-queue* nil
  "List of lists with the uri and intial suggested filename")

(defun download-queue-add (&key uri suggested)
  "Add to the download queue"
  (declare (type string uri suggested))
  (setf *download-queue* (append *download-queue*
                                 (list (list uri suggested))))
  (dmesg *download-queue*))
