(in-package :lispkit/transcompile)
;; Package for cached compilation for various web language transcompilers

(defvar *transcompiler-cache-dir* "/tmp/"
  "Default cache, setf this for your application")
(defvar *transcompilers* '()
  "plist to fill in with transcompilers")

(defun exec (cmd &key stdin file stdout)
  (let ((l (ppcre:split "\\s+" cmd)))
    (if stdin
        (sb-ext:run-program (car l) (cdr l)
                            :input stdin
                            :output stdout)
        (sb-ext:run-program (car l)
                            (append (cdr l) (list file))
                            :directory
                            (directory-namestring (pathname file))
                            :output stdout))))

(defun get-cached-location (filepath)
  (concatenate 'string *transcompiler-cache-dir*
               (ppcre:regex-replace-all "/" filepath "%")))

(defun mtime (file)
  "Given a file path string evaluate to the modification time of the file"
  (sb-posix:stat-mtime (sb-posix:stat file)))

(defun most-recent (files)
  "Evaluate to the most recent mtime in the given file list"
  (apply #'max (mapcar #'mtime files)))

(defun cache (source &optional cache-invalidation-files &key cached)
  "Find a current cache file for the input file. Optionally accept a list of
files that invalidate the cache besides the source file.
If the cache file is found to be current return the location of the cache file
otherwise nil."
  (unless cached
    (setf cached (get-cached-location source)))
  ;; when provides an implicit nil
  (when (and (probe-file cached)
             (< (most-recent (append (list source)
                                     cache-invalidation-files))
                (mtime cached)))
    cached))

(defun file-content-to-string (file)
  (with-output-to-string (output)
    (let ((in (open file)))
      (when in
        (loop for line = (read-line in nil)
           while line do (format output "~a~%" line))
        (close in)))))

(defun compile-cached (cmd source use-stdin)
  "Take a source file and the type of the lang
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

(defun transcompiler (cmd &key string file
                            (use-stdin t) cache-invalidation-files)
  (if string
      (with-output-to-string (output)
        (with-input-from-string (in string)
          (exec cmd :stdin in :stdout output)))
      (let ((ret (cache file cache-invalidation-files)))
        (if ret
            (file-content-to-string ret)
            (compile-cached cmd file use-stdin)))))

(defun transcompile (&key file string type cmd
                       (use-stdin t) cache-invalidation-files)
  "Infer from optional arguments, pass on to transcompiler
Examples:
    (transcompile :type 'coffee :string \"command arg\")
    (transcompile :file \"pathname.jade\")
    (transcompile :type 'coffeeify :file \"pathname\" :use-stdin nil)"
  (unless cmd
    (setf cmd
          (getf *transcompilers*
                (as-keyword
                 (if type type
                     (as-symbol
                      (pathname-type (pathname file))))))))
  (if string
      (transcompiler cmd :string string)
      (transcompiler cmd :file file :use-stdin use-stdin
                     :cache-invalidation-files
                     cache-invalidation-files)))
