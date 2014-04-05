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

(defun ui-scheme-p (uri)
  (ppcre:scan-to-strings "^ui://" uri))
(defun ui-symbol-to-uri (symbol)
  (concatenate 'string "ui://" (string-downcase (symbol-name symbol))))
(defun ui-scheme-uri-to-symbol (uri)
  (as-symbol (ppcre:regex-replace "^ui://" uri "")))

(defvar *hooks* '()
  "The plist for hooks")
;; Hooks
;; add catches
(defun run-hook (hook &rest args)
  "Call each function in HOOK and pass args to it."
  ;; (print args)
  (dolist (fn (getf *hooks* hook))
    (apply fn args)))
;; (handler-case
;;     (with-simple-restart (abort-hooks "Abort running the remaining hooks.")
;;       (with-restarts-menu
;;           (dolist (fn (getf *hooks* hook))
;;             (with-simple-restart (continue-hooks "Continue running the remaining hooks.")
;;               (apply fn args)))))
;;   (t (c) (message "^B^1*Error on hook ^b~S^B!~% ^n~A" hook c) (values nil c))))

;; (defun run-hook (hook)
;;   "Call each function in HOOK."
;;   (run-hook-with-args hook))
;; (defmacro add-hook (hook fn)
;;   "Add @var{function} to the hook @var{hook-variable}. For example, to
;; display a message whenever you switch frames:

;; @example
;; \(defun my-rad-fn (to-frame from-frame)
;;   (stumpwm:message \"Mustard!\"))

;; \(stumpmwm:add-hook stumpwm:*focus-frame-hook* 'my-rad-fn)
;; @end example"
;;   `(setf ,hook (adjoin ,fn ,hook)))

;; (defmacro remove-hook (hook fn)
;;   "Remove the specified function from the hook."
;;   `(setf ,hook (remove ,fn ,hook)))

;; (defmacro replace-hook (hook fn)
;;   `(remove-hook ,hook ,fn)
;;   `(add-hook ,hook ,fn))

(defvar *uri-scripts* nil
  "Global var for for uri to scripts structures")
(defvar *js-exports* '()
  "plist of symbols to callback locations")
