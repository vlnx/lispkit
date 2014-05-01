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
  (print "try to change index to")
  (print new-index)
  (print notebook)
  (finish-output)
  (gtk-notebook-set-current-page
   notebook new-index))

(defun tab-new (browser uri &key background initializing-class-notebook)
  ""
  (let ((notebook (widgets-notebook (browser-gtk browser)))
        (tab (make-instance 'tab :inital-uri uri))
        new-index)
    (print notebook)
    (finish-output)

    ;; Append new tab to browser's tablist slot
    (setf (browser-tabs browser)
          (append (browser-tabs browser)
                  (list tab)))

    ;; Show tab container, in order to be added to the notebook
    (show (tab-scroll tab))

    ;; Add tab to notebook
    (setf new-index
          (notebook-add-tab
           notebook
           (tab-scroll tab)))

    ;; Maybe switch to the new tab
    (unless background
      (setf (notebook-current-tab-index notebook)
            new-index))))



;; Scrolling,

(defun scroll-to (scrolled-window &key
                                    x ;; t or num
                                    y ;; t or num
                                    rel
                                    page ;; if rel to page
                                    percent)
  "
Examples:
    (scroll-to (tab-scroll (current-tab)) :y t :rel 20)
    (scroll-to (tab-scroll (current-tab)) :x t :page t :rel 20)
    (scroll-to (tab-scroll (current-tab)) :x 0)
    (scroll-to (tab-scroll (current-tab)) :x -1)
"
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
