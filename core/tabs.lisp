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
   notebook new-index))


;; Scrolling,

(defun scroll-to (scrolled-window &key
                                    x ;; t or num
                                    y ;; t or num
                                    rel
                                    page ;; if rel to page
                                    percent)
  ""
  (let ((adj 
         (if x
             (vadjustment scrolled-window)
             (hadjustment scrolled-window)))
        val)
    ;; (cond (x (vadjustment scrolled-window))
    ;;                    (y (hadjustment scrolled-window))
    ;;                    (t (error "Invalid Arguments")))))
    (setf val
          (cond
            (rel (+ (property adj :value)
                    (if page
                        (ceiling (* (property adj :page-size) page))
                        rel)))
            (percent (ceiling (* (property adj :upper)
                                 (/ percent 100))))
            (t (if (or (eq x -1) (eq y -1))
                   (property adj :upper)
                   (or x y)))))
    (setf (property adj :value) val))
  (ui-update (current-browser)
             :scroll-indicator scrolled-window))

;; (scroll-to (tab-scroll (current-tab)) :y t :rel 20)
;; (scroll-to (tab-scroll (current-tab)) :x t :page t :rel 20)
;; (scroll-to (tab-scroll (current-tab)) :x 0)
;; (scroll-to (tab-scroll (current-tab)) :x -1)
