(in-package :lispkit/keys)
;; Adapted from stumpwm key handling functions
;; TODO: Test with (key-equalp (kbd "M-x") (kbd (print-key (kbd ("M-x")))))

;; kmap = key + binding
;; define key = add to map
;; kbd = string->key
;; char-state->key

;; Keysyms that don't map to a control character
;; thus use some higher range that map to random unicode characeters

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
  "Given a list of key structres
Return a 'binding' structure that matches"
  (declare (type kmap map))
  (find keys
        (kmap-bindings map)
        :key #'binding-keys
        :test #'keys-equalp))


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

(defun kbd (key)
  "Give a list of the keys that make up the string"
  (mapcar #'parse-key (split-string key " ")))

;;; Define and lookup through the structrue
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



;;; Parse

(defvar *keysym-to-string*
  '(32 "SPC"
    #xff0d "RET"
    #xff1b "ESC"
    #xff09 "TAB"
    #xff08 "BS"
    #xffff "DEL"
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
    #xffd1 "F20")
  "Map X11 keysyms, that don't have a character, to their string representation")

(defvar *string-to-keysym*
  (reverse *keysym-to-string*))

(defun char-to-string (char)
  "Map a char back to a string"
  (let ((n (getf *keysym-to-string* (char-code char))))
    (if n
        n
        (coerce (list char) 'string))))

(defun string-to-char (string)
  "Take a string and try to map it to a char"
  ;; getf doesn't work on strings
  (let ((n (member string *string-to-keysym* :test #'equal)))
    (if n
        (code-char (second n))
        (coerce string 'character))))


(defun char-state->key (char state)
  (let ((shift (keywordp (find :shift state))))
    ;; Unless it's keysym that doesn't map to a correct character
    (unless (member (char-code char) *keysym-to-string* :test #'equal)
      (if (upper-case-p char) ; if char is already upshifted, remove shift mod
          (setf shift nil))
      (if shift ; If shift, upshift char
          (setf char (char-upcase char)
                shift nil)))
    (make-key :char char
              :control (keywordp (find :control state))
              :shift shift
              :meta (keywordp (find :mod1 state)))))

(defun keysym-or-buffer->char (sym code)
  "Convert and store the X11 keysym into a unicode character.
It may map to a higher unicode character but that is handled by *keysym-to-string*"
  ;; If control is pressed, the unicode code will transform it into a
  ;; ascii control character, below code 32, space, in that case
  ;; the sym should be low enough to be accurate
  (if (< code 32)
      (code-char sym)
      (code-char code)))


;; gdk handles these when it passes state
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
    #xffee "Hyper_R"))

(defun ignorable-keysym-p (sym)
  (getf *ignore-mod-only-keysyms* sym))
