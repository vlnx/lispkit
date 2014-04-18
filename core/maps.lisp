(in-package :lispkit)

(defun key-action (src-browser key &key command default-command)
  "If there is an command linked to a keypress handle the call here.
Can decide to give them the source key, or to time the function"
  (if (not (or command default-command))
      (error "No command given"))
  ;; Check the number the command wants
  ;; LISPKIT> (sb-introspect:function-lambda-list #'(lambda (one two three)))
  (if command
      (funcall command src-browser)
      (funcall default-command src-browser key)))
(setf (getf *hooks* :key-action) (list #'key-action))


(defun defkey-auto-kbd (kmap key-arg action-fn)
  "If this were in the macro as a `let`, sbcl can't serialize the kbd structure
for the fasl file, so do this at run time instead"
  (define-key kmap (cond ((eq key-arg t) t)
                         ((stringp key-arg) (kbd key-arg))
                         (t (error "invailid key-arg to defkey-auto-kbd")))
    action-fn))
(defmacro defkey (map key args &body body)
  "Easy key definition
First argument, map, specifies the property name of the keymap
Next argument, key, is a string for kbd or t for default
Next can be #'func a function name or an implcit lambda"
  (let ((func (if body
                  `(lambda ,args
                     ,@body)
                  args)))
    `(defkey-auto-kbd (getf *maps* ,map) ,key
       ,func)))
;; (macroexpand '(defkey :top t #'top-default))
;; (DEFKEY-AUTO-KBD (GETF *MAPS* :TOP) T #'TOP-DEFAULT)
;; (macroexpand '(defkey :top t (a) a))
;; (DEFKEY-AUTO-KBD (GETF *MAPS* :TOP) T (LAMBDA (A) A))

;; Create/Empty keymaps
(setf (getf *maps* :top) (make-kmap)
      (getf *maps* :prompt) (make-kmap)
      (getf *maps* :group-slash-tabs) (make-kmap)
      (getf *maps* :passthrough) (make-kmap))

(defkey :passthrough "C-z" (browser)
  "Toggle browser object's passthrough-state and update ui to reflect it"
  (let ((kstate (browser-key-state browser)))
    (setf (passthrough-state kstate)
          (null (passthrough-state kstate)))
    (ui-update browser :passthrough
               (passthrough-state kstate))))

;; (defun top-default (b key)
;;   (print (print-key key))
;;   (finish-output))
;; (defkey :top t #'top-default)
(defkey :top t (b key)
  "Could buffer keys here
or just like in pure stumpwm one key selects a different map , or both")
;; (defun keypress-buffer-empty (browser)
;;   (setf *keypress-buffer* '())
;;   (ui-update (browser-ui browser)
;;              'buffer-empty))
;; (defkey :top t (browser key)
;;   "Default map action"
;; (print "No action for key"))
;;   (push key *keypress-buffer*)
;;   (ui-update (browser-ui browser)
;;              'buffer-keystroke (print-key key)))
;; (defkey :top "ESC" #'keypress-buffer-empty)
;; (kbd "multiple") => '(#S<KEY> #S<KEY>)
;; (defkey :top "gT" #'keypress-buffer-empty)

;; FIXME: Make keys more buffer centric
;; FIXME: remove group-map
;; FIXME: if do every use other map, show it in the bar
(defkey :group-slash-tabs "t" (b)
  (let ((n (widgets-notebook (browser-gtk b))))
    (setf (notebook-current-tab-index n)
          (+ 1 (notebook-current-tab-index n))))
  (setf (active-maps (browser-key-state b)) '(:top)))
(defkey :group-slash-tabs "T" (b)
  (let ((n (widgets-notebook (browser-gtk b))))
    (setf (notebook-current-tab-index n)
          (- 1 (notebook-current-tab-index n))))
  (setf (active-maps (browser-key-state b)) '(:top)))
(defkey :top "g" (b)
  (setf (active-maps (browser-key-state b))
        '(:group-slash-tabs)))
;; (defkey :top "n" (b)
;;   (let ((n (widgets-notebook (browser-gtk b))))
;;     (setf (notebook-current-tab-index n)
;;           (+ 1 (notebook-current-tab-index n)))))
;; (defkey :top "p" (b)
;;   (let ((n (widgets-notebook (browser-gtk b))))
;;     (setf (notebook-current-tab-index n)
;;           (- 1 (notebook-current-tab-index n)))))

(defkey :top "a" (b)
  (webkit-web-view-load-uri (current-tab b) "http://www.example.com"))
(defkey :top "A" (b)
  (webkit-web-view-load-uri (current-tab b) "http://www.duckduckgo.com"))

(defvar *scroll-step* 40)
(defkey :top "j" (b)
  "Scroll down on the current page by the scroll-step"
  (scroll-to (current-tab 'scroll b) :x t :rel *scroll-step*))
(defkey :top "k" (b)
  "Scroll up on the current page by the scroll-step"
  (scroll-to (current-tab 'scroll b) :x t :rel (- *scroll-step*)))
(defkey :top "h" (b)
  "Scroll to the left"
  (scroll-to (current-tab 'scroll b) :y t :rel (- *scroll-step*)))
(defkey :top "l" (b)
  "Scroll to the right"
  (scroll-to (current-tab 'scroll b) :y t :rel *scroll-step*))
(defkey :top "0" (b)
  "Scroll to the top of the page"
  (scroll-to (current-tab 'scroll b) :x 0))
(defkey :top "G" (b)
  "Scroll to the bottom of the page"
  (scroll-to (current-tab 'scroll b) :x -1))
(defkey :top "SPC" (b)
  "Scroll to down a page"
  (scroll-to (current-tab 'scroll b) :x t :rel t :page 1))
(defkey :top "C-u" (b)
  "Scroll to up half a page"
  (scroll-to (current-tab 'scroll b) :x t :rel t :page -0.5))
(defkey :top "C-d" (b)
  "Scroll to down half a page"
  (scroll-to (current-tab 'scroll b) :x t :rel t :page 0.5))

;; (defkeys :top
;;     (("j") "Scroll down on the current page by the scroll-step"
;;      :x t :rel 20)
;;   (("k") "Scroll up on the current page by the scroll-step"
;;    :x t :rel -20)
;;   (("h") "Scroll to the left"
;;    :y t :rel -20)
;;   (("l") "Scroll to the right"
;;    :y t :rel 20)
;;   (("gg") "Scroll to the top of the page"
;;    :x 0)
;;   (("G") "Scroll to the bottom of the page"
;;    :x -1)
;;   (("SPC") "Scroll to down a page"
;;    :x t :rel t :page 1)
;;   (("C-u") "Scroll to up half a page"
;;    :x t :rel t :page -0.5)
;;   (("C-d") "Scroll to down half a page"
;;    :x t :rel t :page 0.5))


(defun open-prompt-with (b starting-input)
  (setf (active-maps (browser-key-state b))
        '(:prompt))
  (ui-update b :prompt-enter starting-input))

(defkey :top ";" (b)
  (open-prompt-with b ""))
(defkey :prompt "ESC" (b)
  (setf (active-maps (browser-key-state b))
        '(:top))
  (ui-update b
             :prompt-leave t))

(defkey :top "o" (b)
  (open-prompt-with b "open "))
(defkey :top "O" (b)
  (open-prompt-with
   b (format nil "open ~a"
             (property (current-tab 'view b) :uri))))

(defkey :prompt t (b key)
  (ui-update b :prompt-send-key (print-key key)))

;; TODO: Give keys the, browser they were invoked on
;;       Define macros for maps, for javascript action calls
;; list of modes, plists of of *maps*
;; (action-map *prompt-mode*
;;             (t "prompt.insert('#{key}')") ;; Catch all
;;             ("C-t" "prompt.toggleTabOpen()"))

;; NOTE: keys -> invoke javascript -> if needed js invokes exported lisp func that returns information/action
;;;    backend -> ui -> backend actions if needed
