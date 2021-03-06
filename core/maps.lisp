(in-package :lispkit)

(defmacro defkey (map k args &body body)
  "Define a key with an implicit lambda"
  `(mapcar (lambda (key)
             (define-key (getf *maps* ,map) key
               (lambda ,(append args '(&rest rest))
                 (declare (ignore rest))
                 ,@body)))
           (cond
             ((listp ',k)
              (mapcar #'kbd ',k))
             ((stringp ',k)
              (list (kbd ',k)))
             (t (list ',k)))))

;; Create/Empty keymaps
(setf (getf *maps* :top) (make-kmap)
      (getf *maps* :scroll) (make-kmap)
      (getf *maps* :prompt) (make-kmap)
      (getf *maps* :command-input) (make-kmap)
      (getf *maps* :follow) (make-kmap)
      (getf *maps* :passthrough) (make-kmap))

(defkey :passthrough "C-z" (browser)
  "Toggle the passthrough-state and update ui"
  ;; TODO: focus current tab view
  (let ((kstate (browser-key-state browser)))
    (setf (passthrough-state kstate)
          (null (passthrough-state kstate)))
    (ui-update browser :keymode t)))

;; Clear the key-buffer when a command is run
(setf (getf *hooks* :key-non-default-action)
      (list #'(lambda (b)
                (setf (key-buffer (browser-key-state b))
                      nil)
                (ui-update b :buffer-set ""))))

(defkey :top t (b key)
  "Buffer keys from here"
  (let ((kstate (browser-key-state b)))
    (setf (key-buffer kstate)
          (if (key-character-p key)
              (append (key-buffer kstate) (list key))
              nil))
    (ui-update b :buffer-set
               (apply #'concatenate 'string
                      (mapcar #'print-key (key-buffer kstate))))))

(defkey :top "g h" (b)
  "Go home"
  (webkit-web-view-load-uri
   (tab-view (current-tab b)) *homepage*))

(defkey :top "g H" (b)
  "Open homepage in new tab"
  (tab-new b *homepage*))

;; Scroll bindings
(defvar *scroll-step* 40)
(macrolet ((scroll (&rest bindings)
             (append '(progn)
                     (loop for binding in bindings collect
                          `(defkey :scroll ,(first binding) (b)
                             ,(second binding)
                             (apply #'scroll-to
                                    (tab-scroll (current-tab b))
                                    (list ,@(cddr binding))))))))
  (scroll (("j" "Down") "Scroll down by `*scroll-step*'"
           :x t :rel *scroll-step*)
          (("k" "Up") "Scroll up by `*scroll-step*'"
           :x t :rel (- *scroll-step*))
          (("h" "Left") "Scroll to the left"
           :y t :rel (- *scroll-step*))
          (("l" "Right") "Scroll to the right"
           :y t :rel *scroll-step*)
          ("g g" "Scroll to the top of the page"
                 :x 0)
          ("G" "Scroll to the bottom of the page"
               :x -1)
          ("SPC" "Scroll down a page"
                 :x t :rel t :page 1)
          ("C-u" "Scroll up half a page"
                 :x t :rel t :page -0.5)
          ("C-d" "Scroll down half a page"
                 :x t :rel t :page 0.5)))

(defun open-command-prompt-with (b starting-input)
  (set-active-maps b '(:command-input :prompt))
  (ui-update b :prompt-enter starting-input))

(defkey :top ";" (b)
  (open-command-prompt-with b ""))

(setf (getf *hooks* :prompt-leave)
      (list #'(lambda (b)
                (if (member :follow
                            (active-maps (browser-key-state b)))
                    (js 'hints b "hints.collection.clear()"))
                (set-active-maps b '(:scroll :top)))))

(defkey :prompt "ESC" (b)
  (run-hook :prompt-leave b)
  (ui-update b :prompt-leave t))

(defkey :top "o" (b)
  (open-command-prompt-with b "open "))

(defkey :top "O" (b)
  (open-command-prompt-with
   b (format nil "open ~a"
             (property (tab-view (current-tab b)) :uri))))

(defun yank-string (b str)
  "Yank the a string into the primary selection"
  (x11-selection :primary str)
  (ui-update b :notify (format nil "Yanked: ~a" str)))

(defkey :top "y" (b)
  "Yank the current uri into the primary selection"
  (yank-string b (property (tab-view (current-tab b)) :uri)))

(defkey :top "t" (b)
  (open-command-prompt-with b "tabopen "))

(defkey :top "T" (b)
  (open-command-prompt-with
   b (format nil "tabopen ~a"
             (property (tab-view (current-tab b)) :uri))))

;;; Prompt map
(defkey :prompt t (b key)
  (if (key-character-p key)
      (ui-update b :prompt-insert (print-key key))))

(defkey :prompt "S-Insert" (b)
  (ui-update b :prompt-insert (x11-selection :primary)))

(defkey :prompt "SPC" (b)
  (ui-update b :prompt-insert " "))

(defkey :command-input "RET" (b)
  (js 'status b "bar.prompt.evaluateContent();"))

(defkey :prompt ("BS" "C-h") (b)
  (js 'status b "bar.prompt.input.backspace();"))

(defkey :prompt "DEL" (b)
  (js 'status b "bar.prompt.input.delete();"))

(defkey :prompt "Left" (b)
  (js 'status b "bar.prompt.input.moveCursor(-1);"))

(defkey :prompt "Right" (b)
  (js 'status b "bar.prompt.input.moveCursor(1);"))

(defkey :prompt "C-a" (b)
  (js 'status b "bar.prompt.input.startOfLine();"))

(defkey :prompt "C-e" (b)
  (js 'status b "bar.prompt.input.endOfLine();"))

(defkey :command-input "Up" (b)
  (js 'status b "bar.prompt.history.prev();"))

(defkey :command-input "Down" (b)
  (js 'status b "bar.prompt.history.next();"))

(defkey :command-input "M-t" (b)
  (js 'status b "bar.prompt.input.openTabToggle();"))

(defkey :prompt "C-n" (b)
  (js 'status b "bar.completion.collection.next();"))

(defkey :prompt "C-p" (b)
  (js 'status b "bar.completion.collection.prev();"))

(defkey :prompt "TAB" (b)
  "use built in space to complete instead,
well that's if it needs an argument"
  (js 'status b "bar.completion.selectLineForPrompt();"))

;; Scroll durring prompt
(defkey :prompt "C-u" (b)
  "Scroll up half a page"
  (scroll-to (tab-scroll (current-tab b))
             :x t :rel t :page -0.5))

(defkey :prompt "C-d" (b)
  "Scroll down half a page"
  (scroll-to (tab-scroll (current-tab b))
             :x t :rel t :page 0.5))

;;; Tab keys
(defkey :top "g t" (b)
  "Next tab"
  (setf (browser-tabs-current-index b)
        (circular-index :next
                        (browser-tabs-current-index b)
                        (browser-tabs b))))

(defkey :top "g T" (b)
  "Previous tab"
  (setf (browser-tabs-current-index b)
        (circular-index :prev
                        (browser-tabs-current-index b)
                        (browser-tabs b))))

(defkey :top "d" (b)
  "Delete tab"
  (tab-remove b (current-tab b)))

(defkey :top "H" (b)
  "Try to go back one page"
  (webkit-web-view-go-back-or-forward (tab-view (current-tab b))
                                      -1))
(defkey :top "L" (b)
  "Try to go forward one page"
  (webkit-web-view-go-back-or-forward (tab-view (current-tab b))
                                      1))
(defkey :top "r" (b)
  "Reload tab"
  (reload-view (tab-view (current-tab b))))

;;; Current uri manipulation
(defun modify-last-number-in-string (func str)
  "Apply a function to the last number detected in a string"
  (ppcre:regex-replace
   "([0-9]+)([^0-9]*)$" str
   (lambda (match number trail)
     (declare (ignore match))
     (format nil "~a~a"
             (funcall func (parse-integer number))
             (or trail "")))
   :simple-calls t))

(defkey :top "C-a" (b)
  "Increment last number in the current uri"
  (webkit-web-view-load-uri
   (tab-view (current-tab b))
   (modify-last-number-in-string
    #'1+ (property (tab-view (current-tab b)) :uri))))

(defkey :top "C-x" (b)
  "Decrement last number in the current uri"
  (webkit-web-view-load-uri
   (tab-view (current-tab b))
   (modify-last-number-in-string
    #'1- (property (tab-view (current-tab b)) :uri))))

(defun uri-remove-depth (uri)
  "Go up in the uri structure"
  (parse-uri
   (cond ; if there, cut it out
     ;; remove a query
     ((ppcre:scan "\\?.*$" uri)
      (ppcre:regex-replace "\\?.*$" uri ""))
     ;; remove the last slash segment
     ;; todo: should stop at domain
     ((ppcre:scan "[^/]*?/?$" uri)
      (ppcre:regex-replace "[^/]*?/?$" uri ""))
     ;; Haven't implemented subdomain removal
     (t uri))))

(defkey :top "g u" (b)
  "Go up in the uri structure"
  (webkit-web-view-load-uri
   (tab-view (current-tab b))
   (uri-remove-depth (property (tab-view (current-tab b)) :uri))))

;;; Follow mode
(defvar *follow-mode-last-selector* nil)
(defvar *follow-mode-last-evaluator* nil)

(defun follow-invoke (b &key selectors evaluator)
  "Send hint data to the hints layer"
  (setf *follow-mode-last-selector* selectors
        *follow-mode-last-evaluator* evaluator)
  (when (string= (js 'current-tab b
                     "(window._getHintDataForSelectors === undefined) ? 'not' : 'there';"
                     :want-return t)
                 "not")
    (js 'current-tab b (resource-content 'ui/hints/client 'coffee)))
  (js 'hints b
      (format nil "processData('~a', '~a')"
              evaluator
              (escape-single-quote
               (js 'current-tab b
                   (format nil "_getHintDataForSelectors('~a')"
                           selectors)
                   :want-return t)))))

(setf (getf *hooks* :scroll-action)
      (list
       #'(lambda (b)
           (if (member :follow (active-maps (browser-key-state b)))
               (follow-invoke
                b
                :selectors *follow-mode-last-selector*
                :evaluator *follow-mode-last-evaluator*)))))

(defun enter-follow-mode (b phrase &key selectors evaluator)
  "Start follow 'mode'"
  ;; Prompt line will change vertically centered elements
  ;; so give it a chance to update by opening the bar before hinting
  (set-active-maps b '(:follow :prompt))
  (ui-update b :prompt-enter `(:content "" :phrase ,phrase))
  (follow-invoke b :selectors selectors :evaluator evaluator))

(defkey :top "f" (b)
  (enter-follow-mode b "click: "
                     :selectors "clickable"
                     :evaluator "click"))

(defkey :top ", y" (b)
  (enter-follow-mode b "yank: "
                     :selectors "uri"
                     :evaluator "yank"))

(defkey :top "F" (b)
  (enter-follow-mode b "background tab: "
                     :selectors "uri"
                     :evaluator "background-tab"))

(defkey :follow "RET" (b)
  (js 'hints b "selectFirst();"))

;;; TODO:
(defkey :top "u" (b)
  "'unclose' tab"
  ;; when a tab is deleted, take it's uri and save that in a list?
  ;; don't really want to save the webview or it's complete history
  (ui-update b :notify "Implement unclose tab"))

;; TODO: toggle
(defkey :top "F1" (b)
  "Hide status bar"
  (hide (tab-scroll (ui-status (browser-ui b)))))

(defkey :top "F2" (b)
  "show status bar"
  (show (tab-scroll (ui-status (browser-ui b)))))

(defkey :top "g d" (b)
  "Act on the *download-queue*"
  (ui-update b :notify
             "TODO: create prompt mode for download management"))

(defkey :top "z" (b) ; ", z"
  "click on the page to toggle image zoom"
  (js 'current-tab b
      (coffee-template 'send-click-to-position
                       :x 1 :y 1)))
