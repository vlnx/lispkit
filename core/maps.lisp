(in-package :lispkit)

(defmacro defkey (map key args &body body)
  "Define a key with an implcit lambda"
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
      (getf *maps* :prompt) (make-kmap)
      (getf *maps* :group-slash-tabs) (make-kmap)
      (getf *maps* :passthrough) (make-kmap))

(defkey :passthrough "C-z" (browser)
  "Toggle browser object's passthrough-state and update ui to reflect it"
  ;; XXX: also focus current tab view
  (let ((kstate (browser-key-state browser)))
    (setf (passthrough-state kstate)
          (null (passthrough-state kstate)))
    (ui-update browser :passthrough t)))

(setf (getf *hooks* :key-non-default-action)
      (list #'(lambda (b)
                (setf (key-buffer (browser-key-state b))
                      nil)
                (ui-update b :buffer-set ""))))

(defkey :top t (b key)
  "Buffer keys from here"
  (let ((kstate (browser-key-state b)))
    (setf (key-buffer kstate)
          (if (key-equalp key (parse-key "ESC"))
              nil
              (append (key-buffer kstate) (list key))))
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
          (defkey :top (first binding) (b)
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
  (setf (active-maps (browser-key-state b))
        '(:prompt))
  (ui-update b :prompt-enter starting-input))

(defkey :top ";" (b)
  (open-prompt-with b ""))

(defkey :prompt "ESC" (b)
  (setf (active-maps (browser-key-state b))
        '(:top))
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

(defkey :prompt t (b key)
  (ui-update b :prompt-send-key (print-key key)))

(defkey :prompt "S-Insert" (b)
  (ui-update b :prompt-send-key
             (x11-selection :primary t)))

;; Tab Comamnds
(defkey :top "g t" (b)
  (setf (browser-tabs-current-index b)
        (circular-index-next (browser-tabs-current-index b)
                             (browser-tabs b))))
(defkey :top "g T" (b)
  (setf (browser-tabs-current-index b)
        (circular-index-prev (browser-tabs-current-index b)
                             (browser-tabs b))))
(defkey :top "d" (b)
  (tab-remove b (current-tab b)))


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
