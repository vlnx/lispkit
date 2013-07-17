(in-package :lispkit)

(defvar *W*)

;; (defvar *ui-views* '()
;;   "A list of web-view's for the tabbar and inputbar and hints overlay")

(defvar *uri-homepage* "http://10.1.7.1/startpage/index.html"
  "The homepage uri to load by default")


(defun on-key-press (widget event &rest rest)
  (declare (ignore widget event rest))
  (write-line "Key pressed")
  t) ;; True, to stop propagation ?


(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (let* ((win (make-instance 'window
                             :width 800 :height 600
                             :title "LispKit"
                             :has-resize-grip nil
                             :signals '(:destroy :gtk-main-quit
                                        ;; :key-press-event on-key-press
                                        )))
         (scrolled-win (make-instance 'scrolled-window))
         (view (make-instance 'widget :pointer 
                              (tab-new *uri-homepage*)))
         (scrolled-win2 (make-instance 'scrolled-window))
         (view2 (make-instance 'widget :pointer 
                               (tab-new *uri-homepage*)))
         (vb (make-instance 'v-box)))

    (add scrolled-win view)

    (add scrolled-win2 view2)

    ;; Force the main frame to respond to it's parent container's policy change
    (cffi:defcallback true :boolean () t)
    (setf (gsignal (make-instance 'g-object :pointer
                                  (webkit-web-view-get-main-frame view))
                   "scrollbars-policy-changed")
          (cffi:callback true))
    (setf (policy scrolled-win) '(:never :never))


    (pack vb scrolled-win)
    (pack vb scrolled-win2)

    (setf (property scrolled-win2 :height-request) 50)
    (setf *W* scrolled-win2)

    (add win vb)
    ;; (add win scrolled-win)

    (show vb)
    (show win)
    (gtk-main)))
