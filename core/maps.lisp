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
  (webkit-web-view-load-uri
   (tab-view (current-tab b)) *uri-homepage*))

(defkey :top "g H" (b)
  (tab-new b *uri-homepage*))

(defvar *scroll-step* 40)
(defkey :top "j" (b)
  "Scroll down on the current page by the scroll-step"
  (scroll-to (tab-scroll (current-tab b)) :x t :rel *scroll-step*))
(defkey :top "k" (b)
  "Scroll up on the current page by the scroll-step"
  (scroll-to (tab-scroll (current-tab b)) :x t :rel (- *scroll-step*)))
(defkey :top "h" (b)
  "Scroll to the left"
  (scroll-to (tab-scroll (current-tab b)) :y t :rel (- *scroll-step*)))
(defkey :top "l" (b)
  "Scroll to the right"
  (scroll-to (tab-scroll (current-tab b)) :y t :rel *scroll-step*))
(defkey :top "g g" (b)
  "Scroll to the top of the page"
  (scroll-to (tab-scroll (current-tab b)) :x 0))
(defkey :top "G" (b)
  "Scroll to the bottom of the page"
  (scroll-to (tab-scroll (current-tab b)) :x -1))
(defkey :top "SPC" (b)
  "Scroll to down a page"
  (scroll-to (tab-scroll (current-tab b)) :x t :rel t :page 1))
(defkey :top "C-u" (b)
  "Scroll to up half a page"
  (scroll-to (tab-scroll (current-tab b)) :x t :rel t :page -0.5))
(defkey :top "C-d" (b)
  "Scroll to down half a page"
  (scroll-to (tab-scroll (current-tab b)) :x t :rel t :page 0.5))
;; TODO: let macro the scrolling keys
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
    (x11-selection-set :primary uri)
    (ui-update b :notify (format nil "Yanked uri: ~a" uri))))

(defkey :top "t" (b)
  (open-prompt-with b "tabopen "))

(defkey :top "T" (b)
  (open-prompt-with
   b (format nil "tabopen ~a"
             (property (tab-view (current-tab b)) :uri))))

(defkey :prompt t (b key)
  (ui-update b :prompt-send-key (print-key key)))

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
