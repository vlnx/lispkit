(in-package :lispkit)

(defun tc/compile (compiler-symbol &key stdin file stdout)
  (let* ((item (getf *transcompilers* (as-keyword compiler-symbol)))
         (l (if (stringp item) (ppcre:split "\\s+" item) item)))
    (if stdin
        (sb-ext:run-program (car l) (cdr l)
                            :input stdin
                            :output stdout)
        (sb-ext:run-program (car l) (append (cdr l) (list file))
                            :output stdout))))

(defun tc/get-cached-location (filepath)
  (concatenate 'string *lispkit-cache-dir*
               (ppcre:regex-replace-all "/" filepath "%")))

(defun tc/cache (source)
  "Given source file.
if it's cache file exists and is newer, return that path
eles return nil"
  (let ((cached (tc/get-cached-location source)))
    (if (and (probe-file cached)
             (< (sb-posix:stat-mtime (sb-posix:stat source))
                (sb-posix:stat-mtime (sb-posix:stat cached))))
        cached
        nil)))

(defun tc/file-content-to-string (file)
  (with-output-to-string (output)
    (let ((in (open file)))
      (when in
        (loop for line = (read-line in nil)
           ;; while line do (write line :stream output :escape nil))
           while line do (format output "~a~%" line))
        (close in)))))

(defun tc/compile-cached (kind source)
  "Take a souce file and the type of the lang
Make a cache file for next time
Return the complied string" 
  (let ((content (with-output-to-string (output)
                   (if (eq kind 'browserify-coffee)
                       (tc/compile kind :file source :stdout output)
                       (with-open-file (in source)
                         (tc/compile kind :stdin in :stdout output))))))
        (with-open-file (out (tc/get-cached-location source)
                             :direction :output :if-exists :supersede)
          ;; (format out "~a~%" content))
          (write content :stream out :escape nil))
        content))
;; (string-right-trim '(#\Newline) content)))
;; FIXME: this output needs to be the same as file-content-to-string

;; Transcompiler declares the type to use, transcomile infers from file type
(defun transcompiler (tc-type &key string file)
  (if string
      (with-output-to-string (output)
        (with-input-from-string (in string)
          (tc/compile (as-symbol tc-type) :stdin in :stdout output)))
      (let ((ret (tc/cache file)))
        (if ret
            (tc/file-content-to-string ret)
            (tc/compile-cached tc-type file)))))

(defun transcompile (&key file string type)
  "Infer from the file type otherwise just pass string and type on"
  (if file
      (transcompiler (as-symbol (pathname-type (pathname file))) :file file)
      (transcompiler type :string string)))


;; Todo: make browserify mode
;; must use a file, use the real file, don't open a separate input stream for it
;; ret conent
;; (defun tc/browserify (coffee-file) 
