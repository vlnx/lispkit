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
(defstruct kmap
  bindings)
(defstruct binding
  key command)

(defun key-equalp (key1 key2)
  "Compare key structures using char= when needed"
  (if (equalp key1 key2)
      (char= (key-char key1) (key-char key2))
      nil))

(defun parse-mods (mods end)
  "MODS is a sequence of <MOD CHAR> #\- pairs. Return a list suitable
for passing as the last argument to (apply #'make-key ...)"
  (unless (evenp end)
    (signal 'kbd-parse-error :string mods))
  (apply #'nconc (loop for i from 0 below end by 2
                    if (char/= (char mods (1+ i)) #\-)
                    do (signal 'kbd-parse)
                    collect (case (char mods i)
                              (#\M (list :meta t))
                              (#\A (list :alt t))
                              (#\C (list :control t))
                              (#\H (list :hyper t))
                              (#\s (list :super t))
                              (#\S (list :shift t))))))

(defun parse-key (string)
  "Parse STRING and return a key structure. Raise an error of type
kbd-parse if the key failed to parse."
  (let* ((p (when (> (length string) 2)
              (position #\- string :from-end t :end (- (length string) 1))))
         (mods (parse-mods string (if p (1+ p) 0)))
         (char (string-to-char (subseq string (if p (1+ p) 0)))))
    (if char
        (apply 'make-key :char char mods)
        (signal 'kbd-parse-error :string string))))

;; Maybe parse into multipe keys keys to buffer with?
;; (defun parse-key-seq (keys)
;;   "KEYS is a key sequence. Parse it and return the list of keys."
;;   (mapcar 'parse-key (split-string keys)))
;; XXX: define-key needs to be fixed to handle a list of keys
;; (first (parse-key-seq keys)))

(defun kbd (key)
  "Create a key structure from a string"
  (parse-key key))

(defun define-key (map key command)
  "Given a key structure, and a kmap and a command.
If the key binding exists, replace it.
If command is nil remove any existing binding."
  (declare (type kmap map)
           (type (or key (eql t)) key)) ; Accept a key structure or t
  ;; Search through the bindings of the map for the given key
  ;; use binding-key to get the elements to test with key-equalp
  (let ((binding (find key
                       (kmap-bindings map)
                       :key 'binding-key
                       :test 'key-equalp)))
    (unless command ; If there is no command
      (setf (kmap-bindings map) ; remove the found binding
            (delete binding (kmap-bindings map))))
    (if command ; If there is a (new) command
        (setf (kmap-bindings map)
              (append (if binding ; If the binding exists, remove it
                          (delete binding (kmap-bindings map))
                          (kmap-bindings map))
                      (list ; Add the new binding
                       (make-binding :key key
                                     :command command)))))))

(defun lookup-key (keymap key &optional accept-default)
  (labels ((retcmd (key)
             (when key (binding-command key))))
    (or (retcmd (find key
                      (kmap-bindings keymap)
                      :key 'binding-key
                      :test 'key-equalp))
        (and accept-default
             (retcmd (find t
                           (kmap-bindings keymap)
                           :key 'binding-key))))))

;; (defun kmap-symbol-p (x)
;;   (and (symbolp x)
;;        (boundp x)
;;        (kmap-p (symbol-value x))))
;; (defun kmap-or-kmap-symbol-p (x)
;;   (or (kmap-p x)
;;       (kmap-symbol-p x)))
;; (defun dereference-kmaps (kmaps)
;;   (mapcar (lambda (m)
;;             (if (kmap-symbol-p m)
;;                 (symbol-value m)
;;                 m))
;;           kmaps))
;; (dereference-kmaps kmaps))))
;; (defun lookup-key-sequence (kmap key-seq)
;;   "Return the command bound to the key sequenc, KEY-SEQ, in keymap KMAP."
;;   (when (kmap-symbol-p kmap)
;;     (setf kmap (symbol-value kmap)))
;;   (check-type kmap kmap)
;;   (let* ((key (car key-seq))
;;          (cmd (lookup-key kmap key)))
;;     (cond ((null (cdr key-seq))
;;            cmd)
;;           (cmd
;;            (if (kmap-or-kmap-symbol-p cmd)
;;                (lookup-key-sequence cmd (cdr key-seq))
;;                cmd))
;;           (t nil))))

(defun handle-keymap (kmaps key &optional accept-default)
  "Lookup key in all given maps, return first non nil command"
  (find-if-not
   'null
   (mapcar (lambda (map)
             (lookup-key map key accept-default))
           kmaps)))

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

(defun print-key-seq (seq)
  (format nil "^5*~{~a~^ ~}^n" (mapcar 'print-key seq)))

;; (defvar *keysym-name-translations* (make-hash-table))
;; (defvar *name-keysym-translations* (make-hash-table :test #'equal))
;; (defun define-keysym (keysym name)
;;   "Define a mapping from a keysym name to a keysym."
;;   (setf (gethash keysym *keysym-name-translations*) name
;;         (gethash name *name-keysym-translations*) keysym))
;; (defun keysym-name->keysym (name)
;;   "Return the keysym corresponding to NAME."
;;   (gethash name *name-keysym-translations*))
;; (defun keysym->keysym-name (keysym)
;;   "Return the name corresponding to KEYSYM."
;;   (gethash keysym *keysym-name-translations*))

(defvar *char-to-string-name*
  '(#\Newline   "RET"
    #\Esc       "ESC"
    #\Tab       "TAB"
    #\Backspace "BS"
    #\Rubout    "DEL"
    #\Space     "SPC"))
(defvar *string-name-to-char*
  (reverse *char-to-string-name*))
(defun char-to-string (char)
  ""
  (let ((n (getf *char-to-string-name* char)))
    (if n n
        (coerce (list char) 'string))))
(defun string-to-char (string)
  "length must be one"
  ;; Works but hash would be better
  (let ((n (member string *string-name-to-char* :test #'equal)))
    (if n
        (second n)
        (coerce string 'character))))

(defun char-state->key (char state)
  (let ((shift (keywordp (find :shift state))))
    (if (upper-case-p char) ; if char is already upshifted, remove shift mod
        (setf shift nil))
    (if shift ; If shift, upshift char, may have to revise for mods
        (setf char (char-upcase char)
              shift nil))
    (make-key :char char
              :control (keywordp (find :control state))
              :shift shift
              :meta (keywordp (find :mod1 state)))))

(defvar *keysym-to-char*
  '(#xff08 #\Backspace ; BackSpace
    #xff09 #\Tab       ; Tab
    #xff0d #\Newline   ; Return
    #xff1b #\Esc       ; Escape
    #xffff #\Rubout)   ; Delete
  "Transform keysyms to their character if they have one, if not... work that out")

(defun keysym-or-string->char (sym code)
  "x11 keysym or unicode character code"
  ;; (print sym)
  ;; (print code)
  ;; (print "")
  (let ((n (getf *keysym-to-char* sym)))
    (if n
        n
        ;; If control is pressed, the unicode code will transform it into a
        ;; ascii control character, below code 32, space, in that case
        ;; the sym should be low enough to be accurate
        (if (< code 32)
            (code-char sym)
            (code-char code)))))

;; Keysyms that don't have a character translation
(defvar *ignore-mod-only-keysyms*
  '(#xff20 "Multi_key"
    #xffe1 "Shift_L"
    #xffe2 "Shift_R"
    #xffe3 "Control_L"
    #xffe4 "Control_R"
    #xffe5 "Caps_Lock"
    #xffe6 "Shift_Lock"
    #xffe7 "Meta_L"
    #xffe8 "Meta_R"
    #xffe9 "Alt_L"
    #xffea "Alt_R"
    #xffeb "Super_L"
    #xffec "Super_R"
    #xffed "Hyper_L"
    #xffee "Hyper_R"

    ;; Process these later
    #xff63 "Insert"
    #xff13 "Pause"
    #xff50 "Home"
    #xff51 "Left"
    #xff52 "Up"
    #xff53 "Right"
    #xff54 "Down"
    #xff55 "Prior"
    #xff55 "Page_Up"
    #xff56 "Next"
    #xff56 "Page_Down"
    #xff57 "End"
    #xff58 "Begin"
    #xff61 "Print"
    #xff67 "Menu"
    #xffbe "F1"
    #xffbf "F2"
    #xffc0 "F3"
    #xffc1 "F4"
    #xffc2 "F5"
    #xffc3 "F6"
    #xffc4 "F7"
    #xffc5 "F8"
    #xffc6 "F9"
    #xffc7 "F10"
    #xffc8 "F11"
    #xffc9 "F12"
    #xffca "F13"
    #xffcb "F14"
    #xffcc "F15"
    #xffcd "F16"
    #xffce "F17"
    #xffcf "F18"
    #xffd0 "F19"
    #xffd1 "F20"))
(defun ignorable-keysym-p (sym)
  (getf *ignore-mod-only-keysyms* sym))
