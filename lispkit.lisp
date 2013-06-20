(defpackage :lispkit
  (:use :cl :gtk)
  (:shadow #:yes-or-no-p #:y-or-n-p))
(in-package :lispkit)

;; (in-package :lispkit)
;; LISPKIT> (webkit.foreign:webkit-web-view-get-uri (first *views*))

(webkit.foreign:webkit-web-view-get-settings (first *views*))

(gobject:g-object-get-data (webkit.foreign:webkit-web-view-new) "enable-plugins")


(export '(lispkit))

(defvar *swank-server*)

(defvar *ui-views* '()
  "A list of web-view's for the tabbar and inputbar and hints overlay")
(defvar *views* '()
  "A list of web-view's for the tabs")

;; For the inputbar have it be a view/navigation request handled
;; Have it's input be controlled by the lisp/gtk side?
;; eval things like telling it commands/input

(defvar *uri-homepage* "http://10.1.7.1/startpage/index.html"
  "The homepage uri to load by default")


(defun change-settings (settings-pointer opts)
  "Given a GObject pointer for the WebKitGTK Settings object
Use that to change it by the given settings"
  (mapcar (lambda (setting-list-item)
            (let ((property (first setting-list-item))
                  (type (second setting-list-item))
                  (value (third setting-list-item)))
              (cffi:with-foreign-objects ((p 'gobject:g-value))
                (gobject:set-g-value p value type :g-value-init t)
                (gobject:g-object-set-data settings-pointer
                                           property p))))
          opts)
    settings-pointer)

(defun webview-new (uri)
  "Returns a webview with the default settings"
  (let ((settings-pointer (webkit.foreign:webkit-web-settings-new))
        settings
        (view (webkit.foreign:webkit-web-view-new)))
    ;; set settings

    (setq settings (change-settings settings-pointer
                                    '(("enable-plugings" gobject:+g-type-boolean+ nil)
                                      ("enable-webgl" gobject:+g-type-boolean+ t)))

    (webkit.foreign:webkit-web-view-set-settings view settings)


    (webkit.foreign:webkit-web-view-load-uri view uri)
    (push view *views*) ;; or *ui-views*
    view))


;; (defun gval (type content)
;; (cffi:with-foreign-objects ((value1 'gobject:g-value))
;;   (gobject:g-value-init value1 gobject:+g-type-boolean+)
;;   (gobject:g-value-set-boolean value1 nil)
;;   value1)

;; (let ((p (webkit.foreign:webkit-web-settings-new)))
;;   (cffi:with-foreign-objects ((value1 'gobject:g-value))
;;     (gobject:g-value-init value1 gobject:+g-type-boolean+)
;;     (gobject:g-value-set-boolean value1 nil)
;;     (gobject:g-object-set-data p "enable-webgl" value1)
;;     p))

;; http://www.crategus.com/books/cl-cffi-gtk/single-page/index.html#gobject_fun_g-object-class-find-property
;; (gobject:g-type-from-instance p)
;; (let ((p (webkit.foreign:webkit-web-settings-new)))
;;   (cffi:with-foreign-objects ((value1 'gobject:g-value))
;;     (gobject:g-value-init value1 gobject:+g-type-boolean+)
;;     (gobject:g-value-set-boolean value1 nil)
;;     (gobject:g-object-set-data p "enable-webgl" value1)
;;     p))
  


(defun win ()
  "Open up the gtk window"
  (within-main-loop
   (let ((window (make-instance 'gtk-window
                                :type :toplevel
                                :title "LispKit"
                                :default-height 600
                                :default-width 800
                                :has-resize-grip nil
                                :resize-grip-visible nil))
         (view (webview-new *uri-homepage*))
         (scrolled (make-instance 'gtk-scrolled-window
                                  :border-width 0
                                  :hscrollbar-policy :never
                                  :vscrollbar-policy :never)))
     (gobject:g-signal-connect window "destroy"
                       (lambda (widget)
                         (declare (ignore widget))
                         (leave-gtk-main)))
     (gtk-container-add scrolled view)
     (gtk-container-add window scrolled)
     ;; (gtk-scrolled-window-add-with-viewport scrolled view)
     (gtk-widget-show-all window))))


(defun lispkit-internal ()
  "The main function run by starting the image"
  (write-line "Hello World")
  ;; (win)*
  ;; (sleep 100)
  ;; HACK: to keep the swank server open
  (loop while t do
       (sleep 1000)))

(defun lispkit ()
  "Start the images's instance for devel. by starting a swank server, and
trying to catch errors, taken from stumpwm"
  (setf *swank-server* (swank:create-server :port 4555
                                            :style swank:*communication-style*
                                            :dont-close t))
  (loop
     (let ((ret (catch :top-level
                  (lispkit-internal))))
       (setf *last-unhandled-error* nil)
       (cond ((and (consp ret)
                   (typep (first ret) 'condition))
              (format t "~&Caught '~a' at the top level. Please report this.~%~a" 
                      (first ret) (second ret))
              (setf *last-unhandled-error* ret))
             ;; we need to jump out of the event loop in order to hup
             ;; the process because otherwise we get errors.
             ((eq ret :hup-process)
              (apply 'execv (first (argv)) (argv)))
             ((eq ret :restart))
             (t 
              ;; the number is the unix return code
              (return-from lispkit 0))))))
