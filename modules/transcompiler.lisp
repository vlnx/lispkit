(in-package :lispkit/transcompile)
;; Package for cached compilation for various web language transcompliers

(defvar *transcompiler-cache-dir* "/tmp/"
  "Default cache, setf this for your application")
(defvar *transcompilers* '()
  "plist to fill in with transcompliers")

(defun exec (cmd &key stdin file stdout)
  (let ((l (ppcre:split "\\s+" cmd)))
    (if stdin
        (sb-ext:run-program (car l) (cdr l)
                            :input stdin
                            :output stdout)
        (sb-ext:run-program (car l)
                            (append (cdr l) (list file))
                            :directory (directory-namestring (pathname file))
                            :output stdout))))

(defun get-cached-location (filepath)
  (concatenate 'string *transcompiler-cache-dir*
               (ppcre:regex-replace-all "/" filepath "%")))

(defun cache (source)
  "Given source file.
if it's cache file exists and is newer, return that path
eles return nil"
  (let ((cached (get-cached-location source)))
    (if (and (probe-file cached)
             (< (sb-posix:stat-mtime (sb-posix:stat source))
                (sb-posix:stat-mtime (sb-posix:stat cached))))
        cached
        nil)))

(defun file-content-to-string (file)
  (with-output-to-string (output)
    (let ((in (open file)))
      (when in
        (loop for line = (read-line in nil)
           ;; while line do (write line :stream output :escape nil))
           while line do (format output "~a~%" line))
        (close in)))))

(defun compile-cached (cmd source use-stdin)
  "Take a souce file and the type of the lang
Make a cache file for next time
Return the complied string" 
  ;; this output needs to be the same as file-content-to-string
  (let ((content (with-output-to-string (output)
                   (if use-stdin
                       (with-open-file (in source)
                         (exec cmd :stdin in :stdout output))
                       (exec cmd :file source :stdout output)))))
    (with-open-file (out (get-cached-location source)
                         :direction :output :if-exists :supersede)
      (write content :stream out :escape nil))
    content))

(defun transcompiler (cmd &key string file (use-stdin t))
  (if string 
      (with-output-to-string (output)
        (with-input-from-string (in string)
          (exec cmd :stdin in :stdout output)))
      (let ((ret (cache file)))
        (if ret
            (file-content-to-string ret)
            (compile-cached cmd file use-stdin)))))

(defun transcompile (&key file string type cmd (use-stdin t))
  "Infer from optional arguments, pass on to transcompiler
Expamples:
    (transcompile :type 'coffee :string \"command arg\")
    (transcompile :file \"pathname.jade\")
    (transcompile :type 'browserify-coffee :file \"pathname\" :use-stdin nil)"
  (unless cmd
    (setf cmd
          (if type
              (getf *transcompilers* (as-keyword type))
              (getf *transcompilers* (as-keyword 
                                      (as-symbol
                                       (pathname-type (pathname file))))))))
  (if string
      (transcompiler cmd :string string)
      (transcompiler cmd :file file :use-stdin use-stdin)))
