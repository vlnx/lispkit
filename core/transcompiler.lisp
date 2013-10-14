(in-package :lispkit)

(defun string-to-symbol (str) (intern (string-upcase str)))

(defun tc/compile (comp in out)
  "'comp' is the transcompiler name
'in' is an input stream
'out' in an output stream"
  (let* ((item (getf *transcompilers* (as-keyword comp)))
         (l (if (stringp item) (ppcre:split "\\s+" item) item)))
    (sb-ext:run-program (car l) (cdr l)
                        :input in
                        :output out)))

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
                   (with-open-file (in source)
                     (tc/compile kind in output)))))
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
     (with-output-to-string (out)
       (with-input-from-string (in string)
         (tc/compile (string-to-symbol tc-type) in out)))
     (let ((ret (tc/cache file)))
       (if ret
           (tc/file-content-to-string ret)
           (tc/compile-cached tc-type file)))))

(defun transcompile (&key file string type)
  "Infer from the file type otherwise just pass string and type on"
  (if file
      (transcompiler (string-to-symbol (pathname-type (pathname file))) :file file)
      (transcompiler type :string string)))
