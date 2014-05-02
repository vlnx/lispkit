(in-package :lispkit/primitives)
;; In general, use plists when useful, like not polluting the scope

(defvar *lispkit-cache-dir* (concatenate 'string
                                         (sb-ext:posix-getenv "XDG_CACHE_HOME")
                                         "/lispkit/")
  "The directory to store compiled scripts")
(sb-ext:run-program "/bin/mkdir" (list "-p" *lispkit-cache-dir*))

(defvar *site-dir* "/home/***REMOVED***/dev/lispkit/site/")

(defvar *uri-homepage* "http://10.1.7.1/startpage/index.html"
  "The homepage uri to load by default")

(defvar *maps* '()
  "A plist of map names to a kmap structrue")

(defvar *browsers* nil
  "The list for the open browser instances, that contain toplevel windows")

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

(defvar *js-exports* '()
  "plist of symbols to callback locations")
