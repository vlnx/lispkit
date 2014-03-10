(in-package :lispkit)

;; Adapted from stumpwm key handleing functions

(defstruct key
  keysym shift control meta alt hyper super)
(defstruct kmap
  bindings)
(defstruct binding
  key command)

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
                                 ;; (t (signal 'kbd-parse-error :string mods))))))

(defun parse-key (string)
  "Parse STRING and return a key structure. Raise an error of type
kbd-parse if the key failed to parse."
  (let* ((p (when (> (length string) 2)
              (position #\- string :from-end t :end (- (length string) 1))))
         (mods (parse-mods string (if p (1+ p) 0)))
         (keysym (stumpwm-name->keysym (subseq string (if p (1+ p) 0)))))
    (if keysym
        (apply 'make-key :keysym keysym mods)
        (signal 'kbd-parse-error :string string))))

(defun split-seq (seq separators &key test default-value)
  "split a sequence into sub sequences given the list of seperators."
  (let ((seps separators))
    (labels ((sep (c)
               (find c seps :test test)))
      (or (loop for i = (position-if (complement #'sep) seq)
                then (position-if (complement #'sep) seq :start j)
                as j = (position-if #'sep seq :start (or i 0))
                while i
                collect (subseq seq i j)
                while j)
          ;; the empty seq causes the above to return NIL, so help
          ;; it out a little.
          default-value))))
(defun split-string (string &optional (separators "
"))
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
***If SEPARATORS is absent, it defaults to \"[ \f\t\n\r\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that.

Modifies the match data; use `save-match-data' if necessary."
  (split-seq string separators :test #'char= :default-value '("")))
(defun parse-key-seq (keys)
  "KEYS is a key sequence. Parse it and return the list of keys."
  (mapcar 'parse-key (split-string keys)))

(defun kbd (keys)
  "This compiles a key string into a key structure used by
`define-key', `undefine-key', `set-prefix-key' and
others."
  ;; XXX: define-key needs to be fixed to handle a list of keys
  (first (parse-key-seq keys)))

(defun define-key (map key command)
  "Add a keybinding mapping for the key, @var{key}, to the command,
@var{command}, in the specified keymap. If @var{command} is nil, remove an
exising binding.  For example,

@example
\(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"C-z\") \"echo Zzzzz...\")
@end example

Now when you type C-t C-z, you'll see the text ``Zzzzz...'' pop up."
  (declare (type kmap map) (type (or key (eql t)) key))
  (let ((binding (find key (kmap-bindings map) :key 'binding-key :test 'equalp)))
  (if command
      (setf (kmap-bindings map)
            (append (if binding
                        (delete binding (kmap-bindings map))
                        (kmap-bindings map))
                    (list (make-binding :key key :command command))))
      (setf (kmap-bindings map) (delete binding (kmap-bindings map))))))
(defun lookup-key (keymap key &optional accept-default)
  (labels ((retcmd (key)
             (when key (binding-command key))))
    (or (retcmd (find key (kmap-bindings keymap) :key 'binding-key :test 'equalp))
        (and accept-default
             (retcmd (find t (kmap-bindings keymap) :key 'binding-key))))))
(defun kmap-symbol-p (x)
  (and (symbolp x)
       (boundp x)
       (kmap-p (symbol-value x))))
(defun kmap-or-kmap-symbol-p (x)
  (or (kmap-p x)
      (kmap-symbol-p x)))
(defun dereference-kmaps (kmaps)
  (mapcar (lambda (m)
            (if (kmap-symbol-p m)
                (symbol-value m)
                m))
          kmaps))
(defun lookup-key-sequence (kmap key-seq)
  "Return the command bound to the key sequenc, KEY-SEQ, in keymap KMAP."
  (when (kmap-symbol-p kmap)
    (setf kmap (symbol-value kmap)))
  (check-type kmap kmap)
  (let* ((key (car key-seq))
         (cmd (lookup-key kmap key)))
    (cond ((null (cdr key-seq))
           cmd)
          (cmd
           (if (kmap-or-kmap-symbol-p cmd)
               (lookup-key-sequence cmd (cdr key-seq))
               cmd))
          (t nil))))
(defun handle-keymap (kmaps key)
  "Find the command mapped to the (code state) and return it."
  ;; if the first non-nil thing is another keymap, then grab
  ;; all the keymaps and recurse on them. If the first one is a
  ;; command, then we're done.
  (find-if-not 'null
               (mapcar (lambda (m)
                         ;; (print m)
                         ;; (print key)
                         (lookup-key m key))
                       (dereference-kmaps kmaps))))

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
          (keysym->stumpwm-name (key-keysym key))))

(defun print-key-seq (seq)
  (format nil "^5*~{~a~^ ~}^n" (mapcar 'print-key seq)))


;; (handle-keymap (top-maps) (make-key :keysym 97))

(defun code-state->key (code state)

;; (if (= 0 (char-code (gdk-cffi::keyval-to-unicode code)))
  ;; (print code)
  ;; (print (gdk-cffi::keyval-to-unicode code))

  (let ((shift (keywordp (find :shift state))))
    ;; If the key pressed was a between 'a' and 'z' along with shift
    ;; change the keycode to the upcase character and remove the shift mod
    (if (and shift
             (and (>= code 97)    ;; (char-code #\a)
                  (<= code 122))) ;; (char-code #\z)
        (progn
        (setf code (char-code (char-upcase (code-char code)))
              shift nil)))
    ;; if keycode is already upshifted, remove shift mod
    (if (and (>= code 65)  ;; (char-code #\A)
             (<= code 90)) ;; (char-code #\Z)
        (setf shift nil))
    ;; (print (keysym->keysym-name code))
    (make-key :keysym code
              :control (keywordp (find :control state))
              :shift shift
              :meta (keywordp (find :mod1 state)))))

;; turn-in-to a window class slot
(defvar *keys-passthrough* nil)
(defun (setf *keys-passthrough*) (bool)
  (setf *keys-passthrough* bool)
  (ui-update 'passthrough bool))
(defvar *keys-passthrough-key* (kbd "C-z"))
;; Small bug - When keys-passsthrough is turned on
;; the release of the key combo is sent to the webview

(defvar *active-maps* '())

;; (defcallback on-key-press :boolean
;;     ((widget :pointer)
;;      (event :pointer))
;;   (declare (ignore widget))
  
;;   (let* ((keycode (gdk-cffi::parse-event event :keyval))
;;          (current-mods (gdk-cffi::parse-event event :state))
;;          (key (code-state->key keycode current-mods)))
;;     ;; (print key)
;;     (if (equalp key *keys-passthrough-key*)
;;         (setf (*keys-passthrough*) (null *keys-passthrough*)))
;;     (unless *keys-passthrough*
;;         (let ((match (handle-keymap *active-maps* key)))
;;           (if match
;;               (funcall match (first *views*))
;;               (if (find '*map-prompt* *active-maps*)
;;                   (ui-update 'prompt-send-key (print-key key)))))))
              
;;   ;; (time (funcall match (first *views*)))))
;;   (null *keys-passthrough*)) ;; returning true stops propagation of the event

;; ;; Define this only to not let keys escape to the webviews
;; (defcallback on-key-release :boolean
;;     ((widget :pointer)
;;      (event :pointer))
;;   (declare (ignore widget event))
;;   (null *keys-passthrough*)) ;; returning true stops propagation of the event


;; #include <stdio.h>
;; #include <stdlib.h>
;; #include <locale.h>
;; #include <X11/Xlib.h>
;; #include <X11/Xlocale.h>
;; #include <X11/Xutil.h>
;; #include <X11/Xos.h>
;; #include <X11/Xatom.h>
;; #include <X11/keysym.h>

;; Display *dis;
;; Window win;
;; XEvent event;

;; XIM xim;
;; XIC xic;
(defun key-press-handle (xevent)
    ;; #define KBUFSZ 512 // size of keyboard mapping buffer
    ;; int len;
    ;; KeySym keySym;
    ;; wchar_t wkbuf[KBUFSZ + 1];
    ;; char kbuf[KBUFSZ];
    ;; kbuf[0] = 0;

    ;; if (xic == NULL) exit(1);

    ;; Status status;
    ;; // Retrieve unicode string
    ;; len = XwcLookupString(xic, &ev,
    ;;                             wkbuf, KBUFSZ, &keySym, &status);
    ;; // no string, some funcion key pressed
    ;; if (status == XLookupKeySym) return;

    ;; printf("%d ", (unsigned int)wkbuf[0]);
    ;; utf8print((unsigned int)wkbuf[0]);
    ;; fputc('\n', stdout);
)


(defvar Z nil)
;; http://tronche.com/gui/x/xlib/events/keyboard-pointer/keyboard-pointer.html#XKeyEvent
;; (defcallback recive-events :gdk-filter-return
;;     ((gdk-xevent :pointer)
;;      (gdk-event :pointer))
;;   (declare (ignore gdk-event))
;;   ;; (print "got event")
;;   (let ((ret :gdk-filter-remove))
;;     (unless (x-filter-event gdk-xevent (null-pointer)) ;; skip event
;;       (setf ret :gdk-filter-continue)
;;       (print gdk-xevent)
;;       ;; (setf Z gdk-xevent)
;;       ;; (print (foreign-slot-value gdk-xevent '(:struct x-key-event) 'x11-binding::type))
;;       ;; (print (foreign-slot-value gdk-xevent '(:struct x-key-event) 'x11-binding::keycode))
;;       ;; (print (foreign-slot-value gdk-xevent '(:union x-event) 'x11-binding::xkey))
;;       (print (foreign-slot-value gdk-xevent '(:struct x-key-event) 'x11-binding::type))
;;       (print (foreign-slot-value gdk-xevent '(:struct x-key-event) 'x11-binding::keycode))
;;       ;; (print (foreign-slot-value gdk-xevent 'x-event 'x11-binding::xkey))
;;       ;; (print (foreign-slot-value 
;;       ;;         (foreign-slot-value gdk-xevent '(:union x-event) 'x11-binding::xkey)
;;       ;;        '(:struct x-key-event) 'x11-binding::keycode))
;;       (print "")
;;       ;; (let ((event-type (foreign-slot-value gdk-xevent '(:union x-event) 'x11-binding::type))
;;       ;;       (xkey (foreign-slot-value gdk-xevent '(:union x-event) 'x11-binding::xkey)))
;;       ;;   (if (eql 28 event-type)
;;       ;;       (progn
;;       ;;         (print event-type)
;;       ;;         ;; (print (foreign-slot-value xkey '(:struct x-key-event) 'x11-binding::keycode))))
;;       ;;         ))
;;         )
;;     ret))
  ;;       if (XFilterEvent(&event, None)) continue;
  ;;       switch  (event.type) {
  ;;            case KeyPress:
  ;;               key_press(event.xkey);

  ;; if (setlocale(LC_ALL, "") == NULL) exit(1);
  ;; if (XSetLocaleModifiers("") == NULL) exit(1);
    ;; dis = XOpenDisplay(NULL);
    ;; xim = XOpenIM(dis, NULL, 0, 0);
    ;; xic = XCreateIC(xim,
    ;;                 XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
    ;;                 XNClientWindow, win,
    ;;                 XNFocusWindow, win,
    ;;                 NULL);
    ;; if (xic == NULL) exit(1);

    ;; XSelectInput(dis, win, KeyPressMask);
    ;; XMapWindow(dis, win);

;; LISPKIT> (gdk-cffi:gdk-x11-window-get-xid (GTK-WIDGET-GET-WINDOW (browser-win *WINDOW*)))
(defun init-keyevents (gtk-window xic-slot)
  "Given a gtk window start XIM, and get events from gtk"
    ;; Window must be shown to retrive x11 reference this way
  (if (null-pointer-p (x-set-locale-modifiers ""))
      (print "x-set-locale-modifiers has failed"))
  ;; Start up the XIC
  (let* ((gdk-win (gtk-widget-get-window gtk-window))
         (xwin (gdk-cffi:gdk-x11-window-get-xid gdk-win))
         (dis (gdk-cffi:gdk-x11-get-default-xdisplay))
         (xim (x-open-im dis (null-pointer) (null-pointer) (null-pointer)))
         (xic (x-create-ic xim
                           "inputStyle" '(:xim-pre-edit-nothing :xim-pre-edit-nothing)
                           "clientWindow" xwin
                           ;; "focusWindow" xwin
                           (null-pointer))))
    (print dis)
    (print (x-display-of-im xim))
    (print xwin)
    (print xim)
    (print xic)
    (if (null-pointer-p xic)
        (print "xic is null")
        (setf xic-slot xic)))
    ;; (gdk-cffi::gdk-window-add-filter gdk-win (callback recive-events)))

  (setf (gsignal gtk-window "key-press-event") (callback on-key-press)
        (gsignal gtk-window "key-release-event") (callback on-key-release)))

;; At this point keys have been handled and passed on to here
;; so decied if time to pass it webview
(defcallback on-key-press :boolean
    ((widget :pointer)
     (event :pointer))
  (declare (ignore widget))
  (null *keys-passthrough*)) ;; returning true stops propagation of the event

;; Define this only to not let keys escape to the webviews
(defcallback on-key-release :boolean
    ((widget :pointer)
     (event :pointer))
  (declare (ignore widget event))
  (null *keys-passthrough*)) ;; returning true stops propagation of the event
