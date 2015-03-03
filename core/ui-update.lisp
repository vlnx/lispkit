(in-package :lispkit)

;; Helper functions
(defun js-status (b str)
  (js-eval-webview (tab-view (ui-status (browser-ui b))) str))
(defun js-tabs (b str)
  (js-eval-webview (tab-view (ui-tabs (browser-ui b))) str))
(defun tab-title (tab)
  (or (property (tab-view tab) :title)
      (property (tab-view tab) :uri)))

(defgeneric ui-update (browser symbol value))

;; Prompt
(defmethod ui-update (browser (sym (eql :prompt-insert)) str)
  (js-status browser
             (format nil "bar.prompt.input.insert('~a');" str))) ; XXX: escape

(defmethod ui-update (browser (sym (eql :prompt-enter)) str)
  (js-status browser
             (format nil "bar.prompt.open('~a');" str)))

(defmethod ui-update (browser (sym (eql :prompt-leave)) val)
  (js-status browser "bar.prompt.close();"))

;; Status Display
(defmethod ui-update (browser (sym (eql :passthrough)) val)
  (js-status browser
             (if (passthrough-state (browser-key-state browser))
                 "bar.status.keymode.model.set('mode','Passthrough');"
                 "bar.status.keymode.model.set('mode','top');")))

(defmethod ui-update (browser (sym (eql :buffer-set)) val)
  (js-status browser (format nil "bar.status.buffer.model.set('content','~a');" val))) ;; NOTE:escape

(defmethod ui-update (browser (sym (eql :uri)) view)
  (setf view (tab-view (current-tab browser)))
  (js-status browser
             (format nil "bar.status.uri.model.set('uri','~a');"
                     (property view :uri))))

(defmethod ui-update (browser (sym (eql :link-hover)) uri)
  (js-status browser
             (format nil
                     "bar.status.uri.model.set('hover','~a');" uri)))

(defmethod ui-update (browser (sym (eql :scroll-indicator)) scrolled)
  (setf scrolled (tab-scroll (current-tab browser)))
  (js-status browser
             (format
              nil "bar.status.scrollIndicator.model.set({y: ~a, ymax: ~a, x: ~a, xmax: ~a});"
              (floor (property (vadjustment scrolled) :value))
              (-
               (floor (property (vadjustment scrolled) :upper))
               (floor (property (vadjustment scrolled) :page-size)))
              (floor (property (hadjustment scrolled) :value))
              (-
               (floor (property (hadjustment scrolled) :upper))
               (floor (property (hadjustment scrolled) :page-size))))))

(defmethod ui-update (browser (sym (eql :progress)) view)
  (setf view (tab-view (current-tab browser)))
  (js-status browser (format nil "bar.status.progressIndicator.model.set('progress', '~a');"
                             (let ((p (property view :progress)))
                               (floor (* 100 (or
                                              (if (= p 0.0d0) ; if not during initial load, the value is 0
                                                  1.0d0
                                                  p)
                                              777)))))))

(defmethod ui-update (browser (sym (eql :history)) view)
  (setf view (tab-view (current-tab browser)))
  (js-status browser (format nil "bar.status.history.model.set({backward: ~a, forward: ~a});"
                             (if (webkit-web-view-can-go-back view)
                                 "true" "false")
                             (if (webkit-web-view-can-go-forward view)
                                 "true" "false"))))

;; Tabs
(defmethod ui-update (browser (sym (eql :add-tab)) tab)
  (let ((title (tab-title tab))
        (order (position tab (browser-tabs browser))))
    (js-tabs browser (format
                      nil
                      "tabbar.collection.add({order:~a,title:'~a'});"
                      order title))))

(defmethod ui-update (browser (sym (eql :tabs-reset-list)) val)
  (js-tabs browser "tabbar.collection.remove(tabbar.collection.models);")
  (mapcar (lambda (tab)
            (ui-update browser :add-tab tab))
          (browser-tabs browser)))

(defmethod ui-update (browser (sym (eql :current-tab)) val)
  (let ((index (browser-tabs-current-index browser))
        (zerobased-length (1- (length (browser-tabs browser)))))
    (js-tabs browser (format nil "tabbar.collection.moveCurrentTo(~a);"
                             index))
    (js-status browser (format nil "bar.status.tabIndicator.model.set({current: ~a, total: ~a});"
                               index
                               zerobased-length))))

(defmethod ui-update (browser (sym (eql :tabs-update-title)) tab)
  (let ((order (position tab (browser-tabs browser)))
        (title (tab-title tab)))
    (when (and order title)
      (js-tabs browser (format
                        nil
                        "tabbar.collection.findOrder(~a).set('title','~a');"
                        order
                        title)))))

(defmethod ui-update (browser (sym (eql :notify)) str)
  (ui-update browser :buffer-set str))
