(in-package :lispkit)

(defun main ()
  "Open up the gtk window"
  (gtk-init)
  (gdk-threads-init)
  (within-main-loop
    (make-instance 'browser
                   :initial-tabs (list
                                  (uri-remove-depth *homepage*)
                                  *homepage*))))

;; When this is it's own sbcl core:
;; run-swank from etc-stumpwm
