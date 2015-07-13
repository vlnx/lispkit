(in-package :lispkit)

(defvar *cache-directory*
  (merge-pathnames #P".cache/"
                   lispkit-system:*root-directory*)
  ;; (sb-ext:posix-getenv "XDG_CACHE_HOME") "/lispkit/"
  "Project's cache")

(unless (probe-file *cache-directory*)
  (sb-posix:mkdir *cache-directory* #o755))

(defvar *site-directory*
  (merge-pathnames #P"site/"
                   lispkit-system:*root-directory*)
  "Location of page modification data")

(defvar *homepage* "http://vlnx.lan/startpage/"
  "uri to start at and return to")

(defvar *maps* nil
  "Property list of keymap names to keymap structures")

(defvar *browsers* nil
  "list of open browser instances")

(defvar *browser-current-index* 0
  "index of the active window in the `*browsers*' list")

(defun ui-scheme-p (uri)
  (ppcre:scan-to-strings "^ui://" uri))

(defun ui-symbol-to-uri (symbol)
  (concatenate 'string "ui://"
               (string-downcase (symbol-name symbol))))

(defun ui-scheme-uri-to-symbol (uri)
  (as-symbol (ppcre:regex-replace "^ui://" uri "")))

(defvar *hooks* nil
  "Property list of hook names to a list of hook functions")

(defun run-hook (hook &rest args)
  "Run each hook with the passed arguments"
  (dolist (fn (getf *hooks* hook))
    (apply fn args)))

(defvar *uri-scripts* nil
  "`uri-scripts' structure")

(defvar *js-exports* nil
  "Property list of exported function names to callback references")

(defvar *script-list* nil
  "Symbol list of files relative to `*site-directory*'")

(defvar *download-queue* nil
  "List structure to hold data of requested downloads")

(defun download-queue-add (&key uri suggested)
  "Add to `*download-queue*'"
  (declare (type string uri suggested))
  (setf *download-queue* (append *download-queue*
                                 `((,uri ,suggested))))
  (dmesg *download-queue*))
