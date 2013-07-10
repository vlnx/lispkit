(in-package :lispkit)

;; LISPKIT> (webkit.foreign:webkit-web-view-get-uri (first *views*))
;; (webkit-web-view-get-settings (first *views*))
;; LISPKIT> (gobject:get-gobject-property (webkit.foreign:webkit-web-settings-new)
;;                             "html5-local-storage-database-path" gobject:+g-type-string+)
;; will have to expornt the set/get func -- ask patch
;; LISPKIT> (gobject:set-gobject-property (gobject:pointer (gobject:get-gobject-property (first *views*) "window-features")) "scrollbar-visible" nil)
;; LISPKIT> (gobject:get-gobject-property (webkit.foreign:webkit-web-view-get-main-frame (first *views*)) "vertical-scrollbar-policy")
;; FIXME: Kill scrollbars
;; (gtk:gtk-scrolled-window-set-policy
;;  scrolled ;; (webkit.foreign:webkit-web-view-get-main-frame view)
;;  :never :never)

(defvar *ui-views* '()
  "A list of web-view's for the tabbar and inputbar and hints overlay")
(defvar *views* '()
  "A list of web-view's for the tabs")

(defvar *uri-homepage* "http://10.1.7.1/startpage/index.html"
  "The homepage uri to load by default")


(defun change-settings (view opts)
  "Given a view pointer and settings, change the settings"
  (let ((settings (webkit-web-view-get-settings view)))
    ;; (mapcar (lambda (i)
    ;;           (setf (property settings (first i))
    ;;                 (second i)))
    ;;         opts)
    (flet ((set-prop (i)
             (setf (property settings (first i))
                   (second i))))
      (mapcar set-prop opts))
    (webkit-web-view-set-settings view settings)))

(defun webview-new (uri)
  "Returns a webview with your uri and settings"
  (let ((view (webkit-web-view-new)))
    (change-settings view
                     '((:enable-plugins nil)
                       (:enable-scripts nil)))
    (webkit-web-view-load-uri view uri)
    (push view *views*) ;; or *ui-views*
    view))

(defun on-key-press (widget event &rest rest)
  (declare (ignore widget event rest))
  (write-line "Key pressed")
  t) ;; True, to stop propagation ?
  

(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (let* ((win (gtk-model 'window
                         :width 800
                         :height 600
                         :title "LispKit"
                         :has-resize-grip nil
                         :signals '(:destroy :gtk-main-quit
                                    :key-press-event on-key-press)))
         (scrolled-win (make-instance 'scrolled-window))
         (view (make-instance 'widget :pointer 
                                     (webview-new *uri-homepage*))))

    (add scrolled-win view)

    ;; Force the main frame to respond to it's parent container's policy change
    (cffi:defcallback true :boolean () t)
    (setf (gsignal (make-instance 'g-object :pointer
                                  (webkit-web-view-get-main-frame view))
                   "scrollbars-policy-changed")
          (cffi:callback true))
    (setf (policy scrolled-win) '(:never :never))

    (add win scrolled-win)
    (show win)
    (gtk-main)))
