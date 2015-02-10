(in-package :lispkit)

;; Notebook Signals
(defcallback notebook-page-added :void
    ((notebook pobject)
     (child-widget :pointer)
     (page-num :int))
  (declare (ignore notebook child-widget page-num))
  (dmesg "page-added"))

(defcallback notebook-page-removed :void
    ((notebook pobject)
     (child-widget pobject)
     (page-num :int))
  (declare (ignore page-num))
  (dmesg "page-removed")
  (let ((browser (or (find-instance 'of-browser 'from-notebook notebook)
                     (current-browser)))
        (removed-tab (find-instance 'of-tab 'from-scrolled-window
                                    child-widget)))
    ;; Destroy content, guess the content was already free'd
    ;; (destroy (tab-view removed-tab))
    ;; (destroy (tab-scroll removed-tab))
    ;; Remove from list
    (setf (browser-tabs browser)
          (remove removed-tab (browser-tabs browser)))
    ;; If it was the last tab, Open a new blank tab
    (when (and (browser-always-one-tab browser)
               (= 0 (length (browser-tabs browser))))
      (dmesg "last-tab")
      (tab-new browser nil))
    ;; Update display
    (ui-update browser :tabs-reset-list t)
    ;; Update the current tab index, that was changed by removing this page
    (setf (slot-value browser 'tabs-current-index)
          (gtk-notebook-get-current-page notebook))
    (ui-update browser :current-tab t)))


(setf (getf *hooks* :switch-page)
      (list #'(lambda (browser)
                (ui-update browser :history t)
                (ui-update browser :progress t)
                (ui-update browser :scroll-indicator t)
                (ui-update browser :link-hover "")
                (ui-update browser :tabs-update-title
                           (current-tab browser))
                (ui-update browser :current-tab t)
                (ui-update browser :uri t))))

(defcallback notebook-switch-page :void
    ((notebook pobject)
     (child-widget pobject)
     (page-num :int))
  (let ((browser (or (find-instance 'of-browser 'from-notebook notebook)
                     (current-browser)))
        (switched-to-tab (find-instance 'of-tab 'from-scrolled-window
                                        child-widget)))
    ;; Update the browser current tab slot
    (setf (slot-value browser 'tabs-current-index)
          page-num)
    (run-hook :switch-page browser)))

(defun connect-gtk-notebook-signals (notebook)
  "Connect the signals for the notebook widget"
  (setf (gsignal notebook "page-added")
        (callback notebook-page-added)
        (gsignal notebook "page-removed")
        (callback notebook-page-removed)
        (gsignal notebook "switch-page")
        (callback notebook-switch-page)))

(defun notebook-add-tab (notebook widget &optional index)
  (let ((i (if index
               (error "fixme to check if in range")
               -1))) ; append
    (gtk-notebook-insert-page notebook
                              widget nil i)))

(defun (setf browser-tabs-current-index) (new-index browser)
  (gtk-notebook-set-current-page
   (widgets-notebook (browser-gtk browser))
   new-index))

(defun current-tab-is-blank-p (browser)
  (and (current-tab browser)
       (tab-view (current-tab browser))
       (string= (property (tab-view (current-tab browser)) :uri)
                (parse-uri nil))))

(defun tab-new (browser uri &key background)
  "Add a new tab to the notebook"
  (let (tab new-index)
    (cond ((current-tab-is-blank-p browser) ; Use the blank tab if it exists
           (setf tab (current-tab browser))
           (webkit-web-view-load-uri (tab-view tab) uri))
          (t ; Otherwise create a new tab
           (setf tab (make-instance 'tab :initial-uri uri))
           ;; Append new tab to browser's tablist slot
           (setf (browser-tabs browser)
                 (append (browser-tabs browser)
                         (list tab)))
           ;; Show tab container, in order to be added to the notebook
           (show (tab-scroll tab))
           ;; Hide native scrollbars
           (webview-hide-scrollbars (tab-view tab) (tab-scroll tab))
           ;; Add tab to notebook
           (setf new-index (notebook-add-tab
                            (widgets-notebook (browser-gtk browser))
                            (tab-scroll tab)))
           (ui-update browser :add-tab tab)
           (unless background ; Maybe switch to the new tab
             (setf (browser-tabs-current-index browser)
                   new-index))))
    tab)) ; return used tab

(defun tab-remove (browser tab)
  "Remove a tab"
  (unless (member tab (browser-tabs browser))
    (error "tab is not in browser-tabs"))
  (let ((notebook (widgets-notebook (browser-gtk browser)))
        (index (position tab (browser-tabs browser))))
    (gtk-notebook-remove-page notebook index)))
