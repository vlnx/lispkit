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
