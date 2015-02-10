(in-package :lispkit)
;; Define all scripts that interact with webviews

;; Structures
(defstruct uri-scripts/uri
  exact-uri regex-uri)
(defstruct uri-scripts/scripts
  exports scripts styles
  ui-base-html
  enabled) ; TODO: Make an interface to enable/disable scripts, also use this
;; property to filter scripts to invoke
;; Maybe add load-time for page load status per script
(defstruct uri-scripts/binding
  uri scripts)
(defstruct uri-scripts
  bindings)

;; could make a macro interface to this so each key doesn't need a quote
(defun defscript (&key
                    exact-uri
                    regex-uri
                    exports
                    scripts
                    ui-base-html
                    styles)
  "Add to the *uri-scripts* structure, based on define-key"
  ;; TODO: modify input arguments
  ;; list for uri matches?
  ;; minimal syntax for scripts property
  ;; error if ui-base-html is a list
  (let* ((uri (make-uri-scripts/uri
               :exact-uri (first (listify exact-uri))
               :regex-uri (first (listify regex-uri))))
         (scripts (make-uri-scripts/scripts
                   :exports (listify exports)
                   :scripts scripts
                   :ui-base-html (first (listify ui-base-html))
                   :styles (listify styles)))
         (found-existing-binding  (find uri
                                        (uri-scripts-bindings *uri-scripts*)
                                        :key 'uri-scripts/binding-uri
                                        :test 'equalp)))
    (setf (uri-scripts-bindings *uri-scripts*)
          (append
           (if found-existing-binding ; Replace if found
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
        :coffee-closure "/usr/local/bin/coffee --stdio --print"
        :browserify-coffee "/usr/local/bin/browserify --transform coffeeify --debug" ; needs file
        :jade "/usr/local/bin/jade --pretty"
        :stylus "/usr/local/bin/stylus --compress"))

(defun resource-location (symbol-path type)
  "Get the path of a resource from a relative symbol-path and file type"
  (let ((file (concatenate 'string *site-dir*
                           (symbol-to-string symbol-path)
                           (case type
                             (jade ".jade")
                             (stylus ".stylus")
                             (coffee ".coffee")))))
    (if (probe-file file)
        file
        (error "resource file doesn't exist"))))

(defun resource-content (symbol-path type)
  "implicit `resource-location` and transcompile for given type"
  (let ((file (resource-location symbol-path type)))
    (transcompile :type type
                  :file file)))

(defun get-js-to-apply-css (css)
  "Given the css, return the js to apply it"
  (let ((template
         (resource-content 'util-templates/apply-css 'coffee))
        (css-one-line
         (ppcre:regex-replace-all "\\n" css " "))) ; NOTE: also escape \'
    (ppcre:regex-replace-all "{{{snip}}}" template css-one-line)))

;; somehow per page load connected to a view, have status of applied scripts
(defun invoke-scripts (view scripts)
  (let ((exports (uri-scripts/scripts-exports
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
    (when js
      (mapcar (lambda (entry)
                (let ((entry-file (resource-location
                                   (first entry)
                                   'coffee))
                      (deps (mapcar (lambda (f) (resource-location
                                                 f
                                                 'coffee))
                                    (second entry))))
                  ;; If watchify has missed changes to 'deps', recompile
                  (js-eval-webview view
                                   (transcompile :type 'browserify-coffee
                                                 :file entry-file
                                                 :use-stdin nil
                                                 :cache-invalidation-files deps)
                                   :source ; for source maps to be realized
                                   entry-file)))
              (getf js :browserify))
      (mapcar (lambda (entry)
                (let ((file (resource-location (first entry)
                                               'coffee))
                      (opts (second entry)))
                  (js-eval-webview view
                                   (transcompile :type
                                                 (if (getf opts :closure)
                                                     'coffee-closure
                                                     'coffee)
                                                 :file file)
                                   :source file)))
              (getf js :coffee)))
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
                               (symbol-to-string site))))
          scripts))

(defun (setf *script-list*) (value)
  (setf *script-list* value)
  (load-scripts *script-list*))
