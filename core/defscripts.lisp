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
(defvar *npm-bin*
  (concatenate 'string
               *site-directory*
               "node_modules/.bin/"))

(setf *transcompiler-cache-dir* *cache-directory*
      *transcompilers*
      `(:coffee
        ,(concatenate 'string *npm-bin*
                      "coffee --stdio --print --bare")
        :coffee-closure
        ,(concatenate 'string *npm-bin*
                      "coffee --stdio --print")
        :coffeeify
        ,(concatenate 'string *npm-bin*
                      "browserify --transform coffeeify --debug")
        :coffeeify-minimal
        ,(concatenate 'string *npm-bin*
                      "browserify --transform coffeeify")
        :jade
        ,(concatenate 'string *npm-bin*
                      "jade --pretty")
        :stylus
        ,(concatenate 'string *npm-bin*
                      "stylus --compress")))

(defun resource-location (symbol-path type)
  "Get the path of a resource from a relative path and file type"
  (let* ((relative
          ;; If there is a trailing slash,
          ;; take the last directory and add it as a basename
          (ppcre:regex-replace "(\\w+)/$"
                               (symbol-to-string symbol-path)
                               "\\1/\\1"))
         (file (concatenate 'string *site-directory* relative
                            (case type
                              (lisp ".lisp")
                              (jade ".jade")
                              (stylus ".stylus")
                              (coffee ".coffee")))))
    (if (probe-file file)
        file
        (error "resource file doesn't exist"))))

(defun resource-content (symbol-path type)
  "implicit `resource-location' and transcompile for given type"
  (let ((file (resource-location symbol-path type)))
    (transcompile :type type
                  :file file)))

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
           :source file))))

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
