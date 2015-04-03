(in-package :lispkit)

(defmacro defkey (map key args &body body)
  "Define a key with an implicit lambda"
  `(define-key (getf *maps* ,map) (if (stringp ,key)
                                      (kbd ,key)
                                      ,key)
     (lambda ,(append args '(&rest rest))
       (declare (ignore rest))
       ,@body)))

;;; Work that
;; (defmacro take-function (func)
;;   (print func)
;;   `(,func 1 1))
;; (take-function #'+)
;; ((functionp func-or-args) func-or-args)
;; ((listp func-or-args)


;; Create/Empty keymaps
(setf (getf *maps* :top) (make-kmap)
      (getf *maps* :scroll) (make-kmap)
      (getf *maps* :prompt) (make-kmap)
      (getf *maps* :group-slash-tabs) (make-kmap)
      (getf *maps* :passthrough) (make-kmap))

(defkey :passthrough "C-z" (browser)
  "Toggle browser object's passthrough-state and update ui to reflect it"
  ;; XXX: also focus current tab view
  (let ((kstate (browser-key-state browser)))
    (setf (passthrough-state kstate)
          (null (passthrough-state kstate)))
    (ui-update browser :keymode t)))

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
   (tab-view (current-tab b)) *uri-homepage*))

(defkey :top "g H" (b)
  "Open homepage in new tab"
  (tab-new b *uri-homepage*))

;; Scroll bindings
(defvar *scroll-step* 40)
(mapcar (lambda (binding)
          (defkey :scroll (first binding) (b)
            (second binding)
            (apply #'scroll-to (tab-scroll (current-tab b)) (cddr binding))))
        `(("j" "Scroll down on the current page by the scroll-step"
               :x t :rel ,*scroll-step*)
          ("k" "Scroll up on the current page by the scroll-step"
               :x t :rel ,(- *scroll-step*))
          ("h" "Scroll to the left"
               :y t :rel ,(- *scroll-step*))
          ("l" "Scroll to the right"
               :y t :rel ,*scroll-step*)

          ("Down" "Scroll down on the current page by the scroll-step"
                  :x t :rel ,*scroll-step*)
          ("Up" "Scroll up on the current page by the scroll-step"
                :x t :rel ,(- *scroll-step*))
          ("Left" "Scroll to the left"
                  :y t :rel ,(- *scroll-step*))
          ("Right" "Scroll to the right"
                   :y t :rel ,*scroll-step*)

          ("g g" "Scroll to the top of the page"
                 :x 0)
          ("G" "Scroll to the bottom of the page"
               :x -1)
          ("SPC" "Scroll to down a page"
                 :x t :rel t :page 1)
          ("C-u" "Scroll to up half a page"
                 :x t :rel t :page -0.5)
          ("C-d" "Scroll to down half a page"
                 :x t :rel t :page 0.5)))

(defun open-prompt-with (b starting-input)
  (set-active-maps b '(:prompt))
  (ui-update b :prompt-enter starting-input))

(defkey :top ";" (b)
  (open-prompt-with b ""))

(defkey :prompt "ESC" (b)
  (set-active-maps b '(:scroll :top))
  (ui-update b :prompt-leave t))

(defkey :top "o" (b)
  (open-prompt-with b "open "))

(defkey :top "O" (b)
  (open-prompt-with
   b (format nil "open ~a"
             (property (tab-view (current-tab b)) :uri))))

(defkey :top "y" (b)
  "Yank the current uri into the primary selection"
  (let ((uri (property (tab-view (current-tab b)) :uri)))
    (x11-selection :primary uri)
    (ui-update b :notify (format nil "Yanked uri: ~a" uri))))

(defkey :top "t" (b)
  (open-prompt-with b "tabopen "))

(defkey :top "T" (b)
  (open-prompt-with
   b (format nil "tabopen ~a"
             (property (tab-view (current-tab b)) :uri))))

;;; Prompt map
(defkey :prompt t (b key)
  (if (key-character-p key)
      (ui-update b :prompt-insert (print-key key))))

(defkey :prompt "S-Insert" (b)
  (ui-update b :prompt-insert
             (x11-selection :primary t)))

(defkey :prompt "SPC" (b)
  (ui-update b :prompt-insert " "))

(defkey :prompt "RET" (b)
  (js-status b "bar.prompt.evaluateContent();"))

(defkey :prompt "BS" (b)
  (js-status b "bar.prompt.input.backspace();"))
(defkey :prompt "C-h" (b)
  (js-status b "bar.prompt.input.backspace();"))
(defkey :prompt "DEL" (b)
  (js-status b "bar.prompt.input.delete();"))

(defkey :prompt "Left" (b)
  (js-status b "bar.prompt.input.moveCursor(-1);"))
(defkey :prompt "Right" (b)
  (js-status b "bar.prompt.input.moveCursor(1);"))

(defkey :prompt "C-a" (b)
  (js-status b "bar.prompt.input.startOfLine();"))
(defkey :prompt "C-e" (b)
  (js-status b "bar.prompt.input.endOfLine();"))

(defkey :prompt "Up" (b)
  (js-status b "bar.prompt.history.prev();"))
(defkey :prompt "Down" (b)
  (js-status b "bar.prompt.history.next();"))

(defkey :prompt "M-t" (b)
  (js-status b "bar.prompt.input.openTabToggle();"))

(defkey :prompt "C-n" (b)
  (js-status b "bar.completion.collection.next();"))

(defkey :prompt "C-p" (b)
  (js-status b "bar.completion.collection.prev();"))

(defkey :prompt "TAB" (b)
  ;; NOTE: just use built in space instead, well that's if it needs an argument
  (js-status b "bar.completion.selectLineForPrompt();"))

;;; Tab keys

(defkey :top "g t" (b)
  "Next tab"
  (setf (browser-tabs-current-index b)
        (circular-index-next (browser-tabs-current-index b)
                             (browser-tabs b))))

(defkey :top "g T" (b)
  "Previous tab"
  (setf (browser-tabs-current-index b)
        (circular-index-prev (browser-tabs-current-index b)
                             (browser-tabs b))))

(defkey :top "d" (b)
  "Delete tab"
  (tab-remove b (current-tab b)))


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
   (modify-last-number-in-string #'1+
                                 (property (tab-view (current-tab b)) :uri))))

(defkey :top "C-x" (b)
  "Decrement last number in the current uri"
  (webkit-web-view-load-uri
   (tab-view (current-tab b))
   (modify-last-number-in-string #'1-
                                 (property (tab-view (current-tab b)) :uri))))

(defkey :top "g u" (b)
  "Go up in the uri structure"
  (webkit-web-view-load-uri
   (tab-view (current-tab b))
   (parse-uri
    (let ((uri (property (tab-view (current-tab b)) :uri)))
      (cond ; if there, cut it out
        ;; remove a query
        ((ppcre:scan "\\?.*$" uri)
         (ppcre:regex-replace "\\?.*$" uri ""))
        ;; remove the last slash segment
        ;; todo: should stop at domain
        ((ppcre:scan "[^/]*?/?$" uri)
         (ppcre:regex-replace "[^/]*?/?$" uri ""))
        ;; Haven't implemented subdomain removal
        (t uri))))))

;;; todo features

(defkey :top "f" (b)
  "Start follow 'mode'"
  (ui-update b :notify "Implement follow mode"))
;; go to a :follow keymap where the default action, buffer and reads the keys for the hints

(defkey :top "u" (b)
  "'unclose' tab"
  (ui-update b :notify "Implement unclose tab"))
;; when a tab is deleted, take it's uri and save that in a list?
;; don't really want to save the webview or it's complete history


;; todo toggle
(defkey :top "F1" (b)
  "Hide status bar"
  (hide (tab-scroll (ui-status (browser-ui b)))))
(defkey :top "F2" (b)
  "show status bar"
  (show (tab-scroll (ui-status (browser-ui b)))))

(defkey :top "r" (b)
  "Reload tab"
  (reload-view (tab-view (current-tab b))))

(defkey :top "g d" (b)
  "Act on the *download-queue*"
  (ui-update b :notify "todo create prompt mode for download management"))
