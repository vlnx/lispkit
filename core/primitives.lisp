(in-package :lispkit)

;; BUG: All of these comands are std{in,out} based
;; must use absoulute paths in require's
;; they could take the pwd of the command, but for browserify it takes the 
;; /dev/stdin psuduo file
(defvar *transcompilers*
  '(:coffee "/usr/local/bin/coffee --stdio --print --bare"
    :browserify-coffee "/usr/local/bin/browserify --transform coffeeify --debug" ;;needs file
    :jade "/usr/local/bin/jade --pretty"
    :stylus "/usr/local/bin/stylus --compress")
  "plist of compiler names to std{in,out} accepting commands")

(defvar *lispkit-cache-dir* (concatenate 'string
                                         (sb-ext:posix-getenv "XDG_CACHE_HOME")
                                         "/lispkit/")
  "The directory to store compiled scripts")
(sb-ext:run-program "/bin/mkdir" (list "-p" *lispkit-cache-dir*))

(defvar *ui-dir* "/home/***REMOVED***/dev/lispkit/core/ui/")

(defvar *uri-homepage* "http://10.1.7.1/startpage/index.html"
  "The homepage uri to load by default")
