(in-package :lispkit/keys)
;; Parse

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

(defun key-char-real-p (char)
  "Test if the char is valid or if is in the keysym to char list"
  (null (member (char-code char)
                *keysym-to-string* :test #'equal)))

(defun key-character-p (key)
  "If the key has a character
all key structures have a 'char' but here test if it is valid"
  (key-char-real-p (key-char key)))

(defun char-state->key (char state)
  (let ((shift (keywordp (find :shift state))))
    ;; only if the char is valid character
    (when (key-char-real-p char)
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
