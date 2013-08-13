(in-package :gtk-cffi)
(defcfun "gtk_paned_set_position" :void
  (pane pobject)
  (pos :int))

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
         (vpaned (make-instance 'v-paned)))

    ;; Commented until scrolling mechanism is complete
    ;; Force the main frame to respond to it's parent container's policy change
    ;; (cffi:defcallback true :boolean () t)
    ;; (setf (gsignal (make-instance 'g-object :pointer
    ;;                               (webkit-web-view-get-main-frame view))
    ;;                "scrollbars-policy-changed")
    ;;       (cffi:callback true))
    ;; (setf (policy scrolled-win) '(:never :never))

    (add scrolled-win view)
    (add scrolled-win2 view2)

    (pack vpaned scrolled-win :pane-type 1 :resize t :shrink t)
    (pack vpaned scrolled-win2 :resize t :shrink t)
    
    (gtk-cffi::gtk-paned-set-position vpaned 200)

    (add win vpaned)

    (show win :all t)
    (gtk-main)))
