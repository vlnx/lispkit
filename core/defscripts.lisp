(in-package :lispkit)
;; Define all scripts that interact with webviews

;; Structures 
(defstruct uri-scripts/uri
  exact-uri regex-uri)
(defstruct uri-scripts/scripts
  exports deps scripts styles
  ui-base-html) ;; Maybe add load-time for page load status per script
(defstruct uri-scripts/binding
  uri scripts)
(defstruct uri-scripts
  bindings)

(defun defscript (uri-plist scripts-plist)
  "Manage the *uri-scripts* structure, based on define-key"
  (let* ((uri (make-uri-scripts/uri
               :exact-uri (getf uri-plist :exact-uri)
               :regex-uri (getf uri-plist :regex-uri)))
         (scripts (make-uri-scripts/scripts
                   :exports (getf scripts-plist :exports)
                   :deps (getf scripts-plist :deps)
                   :scripts (getf scripts-plist :scripts)
                   :ui-base-html (getf scripts-plist :ui-base-html)
                   :styles (getf scripts-plist :styles)))
         (found-existing-binding  (find uri
                                        (uri-scripts-bindings *uri-scripts*)
                                        :key 'uri-scripts/binding-uri
                                        :test 'equalp)))
    (setf (uri-scripts-bindings *uri-scripts*)
          (append
           (if found-existing-binding ;; Replace if found
               (delete found-existing-binding
                       (uri-scripts-bindings *uri-scripts*))
               (uri-scripts-bindings *uri-scripts*))
           (list
            (make-uri-scripts/binding
             :uri uri
             :scripts scripts))))))

(defun lookup-scripts (uri)
  "Given a uri find any scripts that should apply"
  (delete-if #'null
             (mapcar
              (lambda (binding)
                (let ((uri-struct (uri-scripts/binding-uri binding)))
                  (cond
                    ((uri-scripts/uri-exact-uri uri-struct)
                     (when (string= (uri-scripts/uri-exact-uri uri-struct)
                                    uri)
                       binding))
                    ((uri-scripts/uri-regex-uri uri-struct)
                     (when (ppcre:scan
                            (uri-scripts/uri-regex-uri uri-struct)
                            uri)
                       binding)))))
              (uri-scripts-bindings *uri-scripts*))))

;; Setup transcompiler package
(setf *transcompiler-cache-dir* *lispkit-cache-dir*)
(setf *transcompilers*
      '(:coffee "/usr/local/bin/coffee --stdio --print --bare"
        :browserify-coffee "/usr/local/bin/browserify --transform coffeeify --debug" ;;needs file
        :jade "/usr/local/bin/jade --pretty"
        :stylus "/usr/local/bin/stylus --compress"))

(defun resource-content (symbol-path type)
  "Take a symbol thats a path in the site dir, and the type of content needed
Return the transcompiled file content"
  (let ((file (concatenate 'string *site-dir*
                           (symbol-to-string symbol-path)
                           (case type
                             (browserify-coffee ".coffee")
                             (jade ".jade")
                             (stylus ".stylus")
                             (coffee ".coffee")))))
    (if (probe-file file)
        (if (eq type 'browserify-coffee)
            (transcompile :type type
                          :file file
                          :use-stdin nil)
            (transcompile :type type
                          :file file))
        (error "needed file doesn't exist"))))


(defun get-js-to-apply-css (css)
  "Given the css, return the js to apply it"
  (let ((template
         (resource-content 'util-templates/apply-css 'coffee))
        (css-one-line
         (ppcre:regex-replace-all "\\n" css " "))) ;; NOTE: also escape \'
    (ppcre:regex-replace-all "{{{snip}}}" template css-one-line)))

;; somehow per page load connected to a view, have status of applyed scripts
(defun invoke-scripts (view scripts)
  (let ((exports (uri-scripts/scripts-exports
                  scripts))
        (deps (uri-scripts/scripts-deps
               scripts))
        (js (uri-scripts/scripts-scripts
             scripts))
        (styles (uri-scripts/scripts-styles
                 scripts)))
    (when exports
      (mapcar (lambda (i)
                (js-export-function
                 view
                 (js-exports-symbol-to-name i)
                 (js-exports-symbol-to-callback i)))
              exports))
    (when deps
      (mapcar (lambda (i)
                (js-eval-webview
                 view
                 (resource-content i 'browserify-coffee)))
              deps))
    (when js
      (mapcar (lambda (i)
                (js-eval-webview
                 view
                 (resource-content i 'coffee)))
              js))
    (when styles
      (mapcar (lambda (i)
                (js-eval-webview
                 view
                 (get-js-to-apply-css
                  (resource-content i 'stylus))))
              styles))))

(defun load-scripts (&optional (scripts *script-list*))
  "Populate *js-exports* and *uri-scripts* from content of files listed
by *script-list* or provided argument"
  (setf *js-exports* '())
  (setf *uri-scripts* (make-uri-scripts))
  (mapcar (lambda (site)
            (load (concatenate 'string
                               *site-dir*
                               (symbol-to-string))))
          scripts))
