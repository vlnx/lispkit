(in-package :lispkit)

(defun scroll-to (scrolled-window &key
                                    x ; t or num
                                    y ; t or num
                                    rel
                                    page ; if rel to page
                                    percent)
  "Examples:
    (scroll-to (tab-scroll (current-tab)) :y t :rel 20)
    (scroll-to (tab-scroll (current-tab)) :x t :page t :rel 20)
    (scroll-to (tab-scroll (current-tab)) :x 0)
    (scroll-to (tab-scroll (current-tab)) :x -1)"
  (let* ((adj (if x
                  (vadjustment scrolled-window)
                  (hadjustment scrolled-window)))
         (val (cond
                (rel (+ (property adj :value)
                        (if page
                            (ceiling (* (property adj :page-size) page))
                            rel)))
                (percent (ceiling (* (property adj :upper)
                                     (/ percent 100))))
                (t (if (or (eq x -1) (eq y -1))
                       (property adj :upper)
                       (or x y))))))
    (setf (property adj :value) val)))