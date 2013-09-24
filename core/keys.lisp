(in-package :lispkit)

(defun alert ()
  (js-eval-webview view ;; FIXME:
                   ;; "alert('hello');"
                   (transcompiler 'coffeescript :string "alert 'from coffee'")
                   :source "ui://tabs" :want-return nil))

;; returning true stops propagation of the event
(defcallback on-key-press :boolean ((widget :pointer) (event :pointer))
                  (declare (ignore widget))
                  ;; (print (gdk-cffi::parse-event event :hardware-keycode))
                  (let ((key (keyval-name (gdk-cffi::parse-event event :keyval))))
                    (if (string= (string key) "a")
                        (alert)))
                  t)
(defcallback on-key-release :boolean ((widget :pointer) (event :pointer))
                  (declare (ignore widget event))
;; If modifer remove that state
                  t)



;; NOTE: Basicly got this for now, will need to build modes,keywaps, and modifires pressed already
;; check out gtk-cffi/cl-emacs/{main,keymap}.lisp
;; (defun xev-lisp ()
;;   "Key press testing"
;;   (gtk-init)
;;   (let* ((win (make-instance 'window
;;                              :width 200 :height 200
;;                              :title "Keys"
;;                              :has-resize-grip nil
;;                              :signals '(:destroy :gtk-main-quit))))

;;     (cffi:defcallback on-key :boolean ((widget :pointer) (event :pointer))
;;       (declare (ignore widget))
;;       ;; (print (gdk-cffi::parse-event event :hardware-keycode))
;;       (print (gdk-cffi::parse-event event :keyval))
;;       ;; (print "key")
;;       t)
;;     (setf (gsignal win "key-press-event")
;;           (cffi:callback on-key))
    

;;     (show win :all t)
;;     (gtk-main)))
