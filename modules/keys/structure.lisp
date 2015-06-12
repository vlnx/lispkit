(in-package :lispkit/keys)
;; Adapted from stumpwm key handling functions

(defstruct key
  char
  shift control meta alt hyper super)

(defstruct binding
  keys ; A list of key structures
  command)

(defstruct kmap
  bindings)

(deftype keys () '(or list boolean))

(defun key-equalp (x y)
  "Compare key structures using char= when needed"
  (declare (type key x)
           (type key y))
  (and (equalp x y)
       (char= (key-char x)
              (key-char y))))

(defun keys-equalp (x y)
  "Compare lists of key structures, recursively call `key-equalp'"
  ;; (print x) (print y)
  (cond
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
  (declare (type kmap map)
           (type keys keys))
  (find keys (kmap-bindings map)
        :key #'binding-keys
        :test #'keys-equalp))

(defun parse-key (string)
  "Parse `string' into a key structure."
  (declare (type string string))
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

(defun kbd (keychord)
  "Parse multiple key sequences in to a list of key structures"
  (declare (type string keychord))
  (mapcar #'parse-key (ppcre:split " " keychord)))

(defun define-key (map keys command)
  "Add or remove a key sequence from a kmap"
  (declare (type kmap map)
           (type keys keys)
           (type (or function null) command))
  ;; If the key sequence is found in the map remove it
  (let ((binding (binding-find-keys map keys)))
    (when binding
      (setf (kmap-bindings map)
            (delete binding (kmap-bindings map)))))
  (when command
    (setf (kmap-bindings map)
          (append (kmap-bindings map)
                  (list (make-binding :keys keys
                                      :command command))))))

(defun lookup-keys (kmaps keys)
  "Lookup key sequences in a list of maps"
  (declare (type list kmaps)
           (type keys keys))
  (find-if-not
   'null
   (mapcar (lambda (map)
             (let ((found-key (binding-find-keys map keys)))
               (when found-key
                 (binding-command found-key))))
           kmaps)))

(defun print-key (key)
  (format nil "~a~a"
          (concatenate 'string
                       (when (key-control key) "C-")
                       (when (key-meta key) "M-")
                       (when (key-alt key) "A-")
                       (when (key-shift key) "S-")
                       (when (key-super key) "s-")
                       (when (key-hyper key) "H-"))
          (char-to-string (key-char key))))

(defun print-keys (keys)
  (format nil "~{~a~#[~; ~:; ~]~}"
          (mapcar 'print-key keys)))
;; (defmethod print-object ((object keys) stream)
;;   (format stream "#S(~a)" (print-keysobject)))

(defmethod print-object ((object key) stream)
  (format stream "#S(~a)" (print-key object)))

;;; TEST
;; (let ((str "C-a C-x"))
;;   (keys-equalp (kbd str)
;;                (kbd (print-keys (kbd str)))))
