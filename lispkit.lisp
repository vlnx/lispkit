(defpackage :lispkit
  (:use #:common-lisp
        #:cffi-objects #:g-object-cffi
        :gtk-cffi
        :webkit-binding)
  (:shadow #:yes-or-no-p #:y-or-n-p))
(in-package :lispkit)
(export '(lispkit))

;; (in-package :lispkit)
;; LISPKIT> (webkit.foreign:webkit-web-view-get-uri (first *views*))

;; (webkit.foreign:webkit-web-view-get-settings (first *views*))
;; (gobject:g-object-get-data (webkit.foreign:webkit-web-view-new) "enable-plugins")

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

;; XXX: WERKS!
;; LISPKIT> (gobject:get-gobject-property (webkit.foreign:webkit-web-settings-new)
;;                             "html5-local-storage-database-path" gobject:+g-type-string+)
;; will have to expornt the set/get func -- ask patch
;; LISPKIT> (gobject:set-gobject-property (gobject:pointer (gobject:get-gobject-property (first *views*) "window-features")) "scrollbar-visible" nil)
;; LISPKIT> (gobject:get-gobject-property (webkit.foreign:webkit-web-view-get-main-frame (first *views*)) "vertical-scrollbar-policy")

;; (defun change-settings (view opts)
;;   "Supply a view pointer. Use that to change it by the given settings.
;; Must supply valid props"
;;   (let ((settings-pointer (webkit.foreign:webkit-web-settings-new)))
;;     (mapcar (lambda (setting-list-item)
;;               (let ((property (first setting-list-item))
;;                     (value (second setting-list-item)))
;;                 (gobject:set-gobject-property settings-pointer property
;;                                               value)))
;;             opts)
;;     (webkit.foreign:webkit-web-view-set-settings view settings-pointer)))

(defun webview-new (uri)
  "Returns a webview with the default settings"
  (let ((view (webkit-web-view-new)))
    ;; (change-settings view
    ;;                  '(("enable-plugins" nil)
    ;;                    ("enable-webgl" t)))
    ;; FIXME: Kill scrollbars
    ;; (gtk:gtk-scrolled-window-set-policy
    ;;  scrolled ;; (webkit.foreign:webkit-web-view-get-main-frame view)
    ;;  :never :never)
    (webkit-web-view-load-uri view uri)
    (push view *views*) ;; or *ui-views*
    view))

;; gobject:get-g-object-for-pointer
;; gobject:g-type-from-instance
;; LISPKIT> (gobject:create-gobject-from-pointer (webkit.foreign:webkit-web-view-get-main-frame (first *views*)))
;; LISPKIT> (gobject:g-type-parent (gobject:g-type-parent (gobject:g-type-from-instance (first *views*))))

;; (defun win ()
;;   "Open up the gtk window"
;;   (within-main-loop
;;     (let ((window (make-instance 'gtk-window
;;                                  :type :toplevel
;;                                  :title "LispKit"
;;                                  :default-height 600
;;                                  :default-width 800
;;                                  :has-resize-grip nil
;;                                  :resize-grip-visible nil))
;;           (notebook (make-instance 'gtk-notebook))
;;           (scrolled (make-instance 'gtk-scrolled-window
;;                                    :border-width 0
;;                                    :hscrollbar-policy :never
;;                                    :vscrollbar-policy :never)))
;;       (gobject:g-signal-connect window "destroy"
;;                                 (lambda (widget)
;;                                   (declare (ignore widget))
;;                                   (leave-gtk-main)))
;;       (gtk-container-add scrolled 
;;           (webview-new *uri-homepage*))
;;       (gtk-notebook-append-page notebook scrolled nil)
;;       (gtk-container-add window notebook)
;;       (gtk-widget-show-all window))))

(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (let ((win (gtk-model 'window
                        :width 800
                        :height 600
                        :title "LispKit"
                        :signals '(:destroy :gtk-main-quit)))
        (scrolled-win (make-instance 'scrolled-window)))

    (add scrolled-win
         (make-instance 'widget :pointer (webview-new *uri-homepage*)))

;;     (setf (policy scrolled-win) '(:never :never))
;;     (add win scrolled-win)

    (show win)
    (gtk-main)))

      ;;                            ;; :has-resize-grip nil
      ;;                            ;; :resize-grip-visible nil))
      ;;     (scrolled (make-instance 'gtk-scrolled-window
      ;;                              :border-width 0
      ;;                              :hscrollbar-policy :never
      ;;                              :vscrollbar-policy :never)))
      ;; (gobject:g-signal-connect window "destroy"
      ;;                           (lambda (widget)
      ;;                             (declare (ignore widget))
      ;;                             (leave-gtk-main)))
      ;; (gtk-container-add scrolled 
      ;;     (webview-new *uri-homepage*))
      ;; (gtk-notebook-append-page notebook scrolled nil)
      ;; (gtk-container-add window notebook)
      ;; (gtk-widget-show-all window))))



;; Image startup
(defun lispkit-internal ()
  "The main function run by starting the image"
  (write-line "Hello World")
  ;; (write-line (webkit-binding:webkit-major-version))
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
