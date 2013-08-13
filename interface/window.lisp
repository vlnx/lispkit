(in-package :gtk-cffi)
(defcfun "gtk_paned_set_position" :void
  (pane pobject)
  (pos :int))
;; (defun gtk-paned-set-position (posInt)
;;   (make-instance 'g-object
;;                  :pointer (%gtk-paned-set-position posInt)))


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
         ;; (vb (make-instance 'v-box :homogeneous nil)))
         (vpaned (make-instance 'v-paned)))

    ;; Force the main frame to respond to it's parent container's policy change
    (cffi:defcallback true :boolean () t)
    (setf (gsignal (make-instance 'g-object :pointer
                                  (webkit-web-view-get-main-frame view))
                   "scrollbars-policy-changed")
          (cffi:callback true))
    (setf (policy scrolled-win) '(:never :never))

    ;; (setf (size-request view) '(-1 100))
    ;; (setf (size-request scrolled-win) '(100 100))

    ;; (cffi:defcallback tabbar_get_preferred_height :void ((widget :pointer)
    ;;                                                      (minimal :pointer)
    ;;                                                      (natural :pointer))
    ;;   (declare (ignore widget))
    ;;   (write-line "size")
    ;;   (let ((min (make-instance 'g-value :pointer minimal))
    ;;         (nat (make-instance 'g-value :pointer natural)))
    ;;     (setf (value min) 100)
    ;;     (setf (value nat) 100)))
    ;; (setf (cffi:foreign-slot-value (pointer view)
    ;;                                '(:struct widget-class)
    ;;                                'gtk-cffi::get-preferred-height)
    ;;       (get-callback 'tabbar_get_preferred_height))

    ;; (add scrolled-win view)
    ;; (add scrolled-win2 view2)
    
    ;; (pack vb scrolled-win :expand nil :fill nil)
    ;; (pack vb scrolled-win2 :expand t :fill t)
    ;; (add win vb)
    ;; (setf *W* scrolled-win)

    (add scrolled-win view)
    (add scrolled-win2 view2)

    ;; (add frame1 scrolled-win)
    ;; (add frame2 scrolled-win2)

    ;; (setf (size-request vpaned) '(200 -1))

    (pack vpaned scrolled-win :pane-type 1 :resize t :shrink t)
    ;; (setf (size-request scrolled-win) '(-1 200))
    (pack vpaned scrolled-win2 :resize t :shrink t)
    ;; (setf (size-request scrolled-win2) '(-1 200))
    
    (gtk-cffi::gtk-paned-set-position vpaned 200)

    (add win vpaned)

    ;; (add win scrolled-win)

    (show win :all t)
    (gtk-main)))


;; (setf (property view :height-request) 100)
;; 
;; (flet ((redraw-size (widget rect &rest data)
;;          (declare (ignore rect))
;;          (setf (size-request widget) '(-1 100))
;;          (write-line "tt")
;;          t))
;;   (setf (gsignal view :size-allocate) #'redraw-size))
;; (flet ((redraw-size (widget context)
;;          (declare (ignore context))
;;          (setf (size-request widget) '(-1 100))
;;          (write-line "tt")
;;          t)
;; (cffi:defcallback bottom-redraw :void ()
;;     (setf (property scrolled-win2 :height-request) 50))
;; (setf (gsignal view :draw)
;;       (cffi:callback bottom-redraw))
