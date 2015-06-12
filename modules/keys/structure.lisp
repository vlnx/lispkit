(in-package :lispkit/keys)

;; Adapted from stumpwm key handling functions
;; TODO: Test with (key-equalp (kbd "M-x") (kbd (print-key (kbd ("M-x")))))

;; kmap = key + binding
;; define key = add to map
;; kbd = string->key
;; char-state->key


;;; Key Structure, Parse
(defstruct key
  char
  ;; symbol-string
  shift control meta alt hyper super)
(defstruct binding
  keys ; A list of key structures
  command)
(defstruct kmap
  bindings)

(defun key-equalp (x y)
  "Compare key structures using char= when needed"
  (and (equalp x y)
       (char= (key-char x) (key-char y))))

(defun keys-equalp (x y)
  "Compare lists of key structures"
  ;; (print x) (print y)
  (cond
    ;; ((eq x y) t)
    ((consp x)
     (and (consp y)
          (keys-equalp (car x) (car y))
          (keys-equalp (cdr x) (cdr y))))
    ((and
      (typep x 'key)
      (typep y 'key))
     (key-equalp x y))
    (t (eql x y))))

(defun binding-find-keys (map keys)
  "Given a list of key structures
Return a 'binding' structure that matches"
  (declare (type kmap map))
  (find keys
        (kmap-bindings map)
        :key #'binding-keys
        :test #'keys-equalp))

(defun parse-key (string)
  "Parse `string' into a key structure."
  (let* ((seq (ppcre:split "-" string))
         (mods
          (apply #'nconc
                 (mapcar (lambda (mod)
                           (case (string-to-char mod)
                             (#\M '(:meta t))
                             (#\A '(:alt t))
                             (#\C '(:control t))
                             (#\H '(:hyper t))
                             (#\s '(:super t))
                             (#\S '(:shift t))
                             (t (error "unknown mod"))))
                         (butlast seq))))
         (char
          (string-to-char (car (last seq)))))
    (apply 'make-key :char char mods)))

(defun kbd (key)
  "Give a list of the keys that make up the string"
  (mapcar #'parse-key (ppcre:split " " key)))

;;; Define and lookup through the structure
(defun define-key (map keys command)
  "Given a list of key structures, and a kmap and a command.
If the key binding exists, replace it.
If command is nil remove any existing binding."
  (declare (type kmap map))
  ;; Search through the bindings of the map for the given key
  ;; use binding-key to get the elements to test with key-equalp
  (let ((binding (binding-find-keys map keys)))
    (unless command ; If there is no command
      (setf (kmap-bindings map) ; remove the found binding
            (delete binding (kmap-bindings map))))
    (if command ; If there is a (new) command
        (setf (kmap-bindings map)
              (append (if binding ; If the binding exists, remove it
                          (delete binding (kmap-bindings map))
                          (kmap-bindings map))
                      (list ; Add the new binding
                       (make-binding :keys keys
                                     :command command)))))))

(defun lookup-key (kmaps key buffer)
  "Lookup key in all given maps, return first non nil command"
  (flet ((search-for-key-in-map (map)
           (let ((key-if-found
                  (binding-find-keys map (append buffer
                                                 (if (listp key)
                                                     key
                                                     (list key))))))
             (if key-if-found
                 (binding-command key-if-found)))))
    (find-if-not 'null
                 (mapcar #'search-for-key-in-map kmaps))))

(defun default-action-in-kmap (kmaps)
  (find-if-not 'null
               (mapcar (lambda (map)
                         (let ((match (binding-find-keys map t)))
                           (if match
                               (binding-command match))))
                       kmaps)))

;; Printing the structure
(defun print-mods (key)
  (concatenate 'string
               (when (key-control key) "C-")
               (when (key-meta key) "M-")
               (when (key-alt key) "A-")
               (when (key-shift key) "S-")
               (when (key-super key) "s-")
               (when (key-hyper key) "H-")))

(defun print-key (key)
  (format nil "~a~a"
          (print-mods key)
          (char-to-string (key-char key))))

;; (defun print-key-seq (seq)
;;   (format nil "^5*~{~a~^ ~}^n" (mapcar 'print-key seq)))

(defmethod print-object ((object key) stream)
  (format stream "#S(~a)" (print-key object)))
