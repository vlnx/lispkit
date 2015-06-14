(in-package :lispkit)

(defun scroll-to (scrolled-window &key
                                    x
                                    y
                                    rel
                                    page ; if rel to page
                                    percent)
  "Examples:
    (scroll-to (tab-scroll (current-tab)) :y t :rel 20)
    (scroll-to (tab-scroll (current-tab)) :x t :rel t :page 0.5)
    (scroll-to (tab-scroll (current-tab)) :x 0)
    (scroll-to (tab-scroll (current-tab)) :x -1)"
  (declare (type (or number boolean null) x y rel page)
           (type (or number null) percent))
  (let* ((adj (if x
                  (vadjustment scrolled-window)
                  (hadjustment scrolled-window)))
         (val (cond
                (rel (+ (property adj :value)
                        (if page
                            (ceiling (* (property adj :page-size)
                                        page))
                            rel)))
                (percent (ceiling (* (property adj :upper)
                                     (/ percent 100))))
                (t (if (or (eq x -1) (eq y -1))
                       (property adj :upper)
                       (or x y))))))
    (setf (property adj :value) val))
  (run-hook :scroll-action
            (find-instance 'of-browser 'from-scrolled-window
                           scrolled-window)))

;; (start-webkit-download "http://vlnx.lan/startpage/script.js" "file:///tmp/dl.1")
(defun start-webkit-download (uri dest)
  "start a webkit download"
  (declare (type string uri dest))
  (let ((dl (webkit-download-new
             (webkit-network-request-new uri))))
    ;; (setf (property dl :destination-uri) dest)
    (webkit-download-set-destination-uri dl dest)
    ;; (setf (gsignal dl "error") download-error) ; maybe connect to signal "error"
    (webkit-download-start dl)))
