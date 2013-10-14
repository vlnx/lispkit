(in-package :lispkit)

;; BUG: All of these comands are std{in,out} based
;; must use absoulute paths in require's
;; they could take the pwd of the command, but for browserify it takes the 
;; /dev/stdin psuduo file
(defvar *transcompilers*
  '(:coffeescript "/usr/local/bin/coffee --stdio --print --bare"
    :browserify-coffee ("/usr/local/bin/browserify" "--command" "coffee -sc" "--debug" "/dev/stdin")
    :jade "/usr/local/bin/jade --pretty"
    :stylus "/usr/local/bin/stylus --compress")
  "plist of compiler names to std{in,out} accepting commands")

(defvar *lispkit-cache-dir* (concatenate 'string
                                         (sb-ext:posix-getenv "XDG_CACHE_HOME")
                                         "/lispkit/")
  "The directory to store compiled scripts")
(sb-ext:run-program "/bin/mkdir" (list "-p" *lispkit-cache-dir*))

(defvar *ui-dir* "/home/***REMOVED***/dev/lispkit/core/ui/")

(defvar *ui-views* '(:tabs nil
                     :status nil
                     :hints nil)
  "A plist for the view instances used in the interface")

(defvar *views* '()
  "A list of active tab instances")
