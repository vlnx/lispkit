(in-package :lispkit)
;; Define rules to apply actions on certain pages

(defstruct uri-scripts
  uri-list
  exports
  scripts
  styles
  ui-base-html)

(defmacro defscript (uris &key
                            exports
                            scripts
                            ui-base-html
                            styles)
  "Add to `*uri-scripts*'"
  `(setf *uri-scripts*
         (append *uri-scripts*
                 (list (make-uri-scripts
                        :uri-list (listify ,uris)
                        :exports (listify ',exports)
                        :scripts ',scripts
                        :ui-base-html ',ui-base-html
                        :styles (listify ',styles))))))

(defun lookup-scripts (uri)
  "Given a uri find any scripts that should apply"
  (flet ((match-uri (item)
           (or (string= item uri)
               ;; regex must encompass the entire uri
               (ppcre:scan (concatenate 'string "^" item "$")
                           uri))))
    (remove-if-not (lambda (script)
                     (remove-if-not #'match-uri
                                    (uri-scripts-uri-list script)))
                   *uri-scripts*)))

;; Setup transcompiler package
(defvar *npm-bin* (merge-pathnames #P"node_modules/.bin/"
                                   *site-directory*))

(setf *transcompiler-cache-directory* *cache-directory*
      *transcompilers*
      (mapcar (lambda (item)
                (if (stringp item)
                    (concatenate 'string
                                 (namestring *npm-bin*)
                                 item)
                    item))
              '(:coffee
                "coffee --stdio --print --bare"
                :coffee-closure "coffee --stdio --print"
                :coffeeify "browserify --transform coffeeify --debug"
                :coffeeify-minimal "browserify --transform coffeeify"
                :jade "jade --pretty"
                :stylus "stylus --compress")))

(defun resource-location (symbol-path type)
  "Get the path of a resource from a relative path and file type"
  (let* ((relative
          ;; If there is a trailing slash,
          ;; take the last directory and add it as a basename
          (pathname
           (ppcre:regex-replace "(\\w+)/$"
                                (symbol-to-string symbol-path)
                                "\\1/\\1")))
         (file (make-pathname
                :directory (pathname-directory
                            (merge-pathnames relative
                                             *site-directory*))
                :name (pathname-name relative)
                :type (symbol-to-string type))))
    (if (probe-file file)
        file
        (error "resource file doesn't exist"))))

(defun resource-content (symbol-path type)
  "implicit `resource-location' and transcompile for given type"
  (transcompile :type type
                :file (resource-location symbol-path type)))

(defun coffee-template (template &rest plist)
  "Use a template system to insert strings into coffeescript output.
'{{{name}}}' will be replaced by the value of the key `:name'"
  (let ((js-str (resource-content
                 (prepend-string-on-to-symbol "util-templates/"
                                              template)
                 'coffee)))
    (loop for item in (pair-plist plist)
       do (setf js-str (ppcre:regex-replace-all
                        (format nil "{{{~a}}}"
                                (string-downcase (princ-to-string
                                                  (car item))))
                        js-str
                        (escape-single-quote
                         (princ-to-string (cadr item))))))
    js-str))

(defun invoke-scripts/exports (exports view)
  (loop for i in exports
     do (js-export-function view
                            (js-exports-symbol-to-name i)
                            (js-exports-symbol-to-callback i))))

(defun invoke-scripts/js (scripts view)
  (loop for (file . type) in scripts
     do (progn
          (setf file (resource-location file 'coffee))
          (js-eval-webview
           view
           (transcompile :type type
                         :file file
                         :use-stdin (null
                                     (or (eq type
                                             'coffeeify)
                                         (eq type
                                             'coffeeify-minimal))))
           :source (namestring file)))))

(defun invoke-scripts/styles (styles view)
  (loop for i in styles
     do (js-eval-webview
         view
         (coffee-template
          'apply-css
          :snip (ppcre:regex-replace-all
                 "\\n" (resource-content i 'stylus) " ")))))

(defun invoke-scripts (uri view)
  "Apply all of the scripts that match `uri' on `view'"
  (mapcar (lambda (result)
            (invoke-scripts/styles
             (uri-scripts-styles result) view)
            (invoke-scripts/exports
             (uri-scripts-exports result) view)
            (invoke-scripts/js
             (uri-scripts-scripts result) view))
          (lookup-scripts uri)))

(defun load-scripts (&optional (scripts *script-list*))
  "Populate `*js-exports*' and `*uri-scripts*' with the files listed
in `*script-list*' or provided argument"
  (setf *js-exports* nil
        *uri-scripts* nil)
  (mapcar (lambda (site)
            (load (resource-location site 'lisp)))
          scripts))

(defun (setf *script-list*) (value)
  (setf *script-list* value)
  (load-scripts *script-list*))

(defun watchify ()
  "Spawn watchify processes.
Use as a main entry function so the processes can be cleaned up
since the GTK3 thread is known to segfault"
  (labels
      ((file-syms (script-list wanted-type)
         (loop for script in (mapcar #'uri-scripts-scripts
                                     script-list)
            nconc (loop for (file . type) in script
                     when (eq type wanted-type)
                     collect file)))
       (get-cmd-for-file (file)
         (format nil
                 "~awatchify --transform coffeeify --debug ~a -o ~a"
                 *npm-bin*
                 file
                 (lispkit/transcompile::get-cached-location file)))
       (spawn (cmd)
         (dmesg cmd)
         (let ((l (ppcre:split "\\s+" cmd)))
           (sb-ext:run-program (car l) (cdr l)
                               :directory *site-directory*
                               :wait nil))))
    (let ((procs
           (mapcar #'spawn
                   (mapcar #'get-cmd-for-file
                           (mapcar (lambda (sym)
                                     (resource-location sym 'coffee))
                                   (file-syms *uri-scripts*
                                              'coffeeify))))))
      (dmesg procs)
      (push (lambda ()
              (loop for process in procs
                 do (progn
                      (sb-ext:process-kill process 15 :pid)
                      (sb-ext:process-wait process)
                      (sb-ext:process-close process)
                      (dmesg (sb-ext:process-exit-code process)))))
            sb-ext:*exit-hooks*))))

(export 'watchify)
