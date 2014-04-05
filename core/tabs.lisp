(in-package :lispkit)

(defun notebook-add-tab (notebook widget &optional index)
  (let ((i (if index
               (error "fixme to check if in range")
               -1))) ;; append
    (gtk-notebook-insert-page notebook
                              widget nil i)))
(defun notebook-current-tab-index (notebook)
  (let ((ret (gtk-notebook-get-current-page notebook)))
    (if (= ret -1)
        (error "Notebook has no pages, can't get current index")
        ret)))
(defun (setf notebook-current-tab-index) (new-index notebook)
  (gtk-notebook-set-current-page
   notebook new-index)
  ;; FIXME: run-hook *hooks* :switch-tab , arg new-tab
)

;; (setf (getf *hooks* :switch-tab)
;;       (lambda (browser new-tab)
;;         (ui-update (browser-ui browser)
;;                    'uri (property (tab-view new-tab) :uri))))
