(in-package :lispkit)
;; In this file, this would be the executable, process args, swank, open window


;; (defvar *swank-server*)

;; Image startup
;; (defun lispkit-internal ()
;;   "The main function run by starting the image"
;;   (write-line "Hello World")
;;   ;; (write-line (webkit-binding:webkit-major-version))
;;   ;; (win)*
;;   ;; (sleep 100)
;;   ;; HACK: to keep the swank server open
;;   (loop while t do
;;        (sleep 1000)))

;; (defun lispkit ()
;;   "Start the images's instance for devel. by starting a swank server, and
;; trying to catch errors, taken from stumpwm"
;;   (setf *swank-server* (swank:create-server :port 4555
;;                                             :style swank:*communication-style*
;;                                             :dont-close t))
;;   (loop
;;      (let ((ret (catch :top-level
;;                   (lispkit-internal))))
;;        ;; (setf *last-unhandled-error* nil)
;;        (cond ((and (consp ret)
;;                    (typep (first ret) 'condition))
;;               (format t "~&Caught '~a' at the top level. Please report this.~%~a" 
;;                       (first ret) (second ret))
;;               )
;;               ;; (setf *last-unhandled-error* ret))
;;              ;; we need to jump out of the event loop in order to hup
;;              ;; the process because otherwise we get errors.
;;              ((eq ret :hup-process)
;;               )
;;               ;; (apply 'execv (first (argv)) (argv)))
;;              ((eq ret :restart))
;;              (t 
;;               ;; the number is the unix return code
;;               (return-from lispkit 0))))))
