(in-package :lispkit/keys)

(defvar *keysym-to-string*
  '(32 "SPC"
    127 "DEL"
    #xff0d "RET"
    #xff1b "ESC"
    #xff09 "TAB"
    #xff08 "BS"
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
  "X11 gives these keysyms that get translated to incorrect unicode
characters when keys don't have a character representation.
Map those to a string representation.")

(defvar *string-to-keysym*
  (reverse *keysym-to-string*))

(defun char-to-string (char)
  "Use `*keysym-to-string*' to translate a char to a string"
  (let ((n (getf *keysym-to-string* (char-code char))))
    (if n
        n
        (coerce (list char) 'string))))

(defun string-to-char (string)
  "Use `*string-to-keysym*' to translate a string to a char"
  ;; getf doesn't work on strings
  (let ((n (member string *string-to-keysym* :test #'equal)))
    (if n
        (code-char (second n))
        (coerce string 'character))))

(defun key-char-real-p (char)
  "Test if the char is in `*keysym-to-string*'"
  (null (member (char-code char)
                *keysym-to-string* :test #'equal)))

(defun key-character-p (key)
  "Test if the key has a valid character slot"
  (key-char-real-p (key-char key)))

(defun char-state->key (char state)
  "Turn the char and gdk event state in to a key structure"
  (let ((shift (keywordp (find :shift state))))
    ;; only if the char is valid character
    (when (key-char-real-p char)
      ;; if char is already up-shifted, remove shift mod
      (if (upper-case-p char)
          (setf shift nil))
      ;; If shift, up-shift char
      (if shift
          (setf char (char-upcase char)
                shift nil)))
    (make-key :char char
              :control (keywordp (find :control state))
              :shift shift
              :meta (keywordp (find :mod1 state)))))

(defun keysym-or-buffer->char (keysym buffer)
  "Choose if the `keysym' or `buffer' should be used for the char slot
If control was held, the `buffer' may be an ascii control character,
in that case the `keysym' should be low enough to be accurate"
  (if (< buffer 32) ; ascii control codes stop at 32
      (code-char keysym)
      (code-char buffer)))

(defvar *ignorable-keysyms*
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
    #xffee "Hyper_R")
  "Modifer keys, GDK informs of these when it passes the key state")

(defun ignorable-keysym-p (sym)
  (getf *ignorable-keysyms* sym))
