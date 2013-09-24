(in-package :lispkit)

(defvar *transcompilers* nil
  "alist of names to commands and arguments")

(setf *transcompilers*
      '((coffeescript . ("/usr/local/bin/coffee" . ("--stdio" "--print" "--bare")))
        (jade . ("/usr/local/bin/jade" . ("--pretty")))
        (stylus . ("/usr/local/bin/stylus" . ("--compress")))))

(defun transcompiler (comp &key string file)
  (flet ((exec (in out)
           (let* ((l (cdr (assoc comp *transcompilers*)))
                  (program (car l))
                  (args (cdr l)))
             (sb-ext:run-program program args
                                 :input in
                                 :output out))))
    (with-output-to-string (output)
      (if string
          (with-input-from-string (stream string)
            (exec stream output))
          (with-open-file (stream file)
            (exec stream output))))))

;; TODO: Caching for files 
;; Take full path file name, replace / with %
;; if that file exists in var/lispkit/compiled
;; run `stat --formaty=%Y $file`
;; compare that and source file
;; if souce in newer recompile otherwise return content of cached
;; also update cache file
;; maybe use md5 for caching strings
