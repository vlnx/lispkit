(in-package :lispkit)

;; Notebook Signals
(defcallback notebook-page-added :void
  ((notebook pobject)
   (child-widget :pointer)
   (page-num :int))
  (declare (ignore notebook child-widget page-num))
  (print "page-added")
  (finish-output))

(defcallback notebook-switch-page :void
  ((notebook pobject)
   (child-widget pobject)
   (page-num :int))
  (let ((browser (or (browser-find-instance notebook
                                            :of 'browser
                                            :from 'notebook)
                     (current-browser)))
        (switched-to-tab (browser-find-instance child-widget
                                                :of 'tab
                                                :from 'scrolled-window)))
    ;; Update the browser current tab slot
    (setf (slot-value browser 'tabs-current-index)
          page-num)
    (ui-update browser :history t)
    (ui-update browser :progress t)
    (ui-update browser :scroll-indicator t)
    (ui-update browser :tabs-switched-page t)
    (ui-update browser :uri t)))

(defun connect-gtk-notebook-signals (notebook)
  "Connect the signals for the notebook widget"
  (setf (gsignal notebook "page-added")
        (callback notebook-page-added)
        (gsignal notebook "switch-page")
        (callback notebook-switch-page)))

(defun notebook-add-tab (notebook widget &optional index)
  (let ((i (if index
               (error "fixme to check if in range")
               -1))) ;; append
    (gtk-notebook-insert-page notebook
                              widget nil i)))
;; won't need this much since the browser slot should be in sync from setting it calling swich-page
;; (defun notebook-current-tab-index (notebook)
;;   (let ((ret (gtk-notebook-get-current-page notebook)))
;;     (if (= ret -1)
;;         (error "Notebook has no pages, can't get current index")
;;         ret)))
(defun (setf browser-tabs-current-index) (new-index browser)
  (gtk-notebook-set-current-page
   (widgets-notebook (browser-gtk browser))
   new-index))

(defun tab-new (browser uri &key background initializing-class-notebook)
  "Add a new tab to the notebook"
  (let ((notebook (widgets-notebook (browser-gtk browser)))
        (tab (make-instance 'tab :inital-uri uri))
        new-index)
    (print notebook)
    (finish-output)

    ;; Append new tab to browser's tablist slot
    (setf (browser-tabs browser)
          (append (browser-tabs browser)
                  (list tab)))

    ;; Show tab container, in order to be added to the notebook
    (show (tab-scroll tab))

    ;; Hide native scrollbars now the scroll indicator is working
    (webview-hide-scrollbars (tab-view tab) (tab-scroll tab))

    ;; Add tab to notebook
    (setf new-index
          (notebook-add-tab
           notebook
           (tab-scroll tab)))

    ;; Maybe switch to the new tab
    (unless background
      (setf (browser-tabs-current-index browser)
            new-index))))
