(in-package :lispkit)

;; Helper functions
(defun js-status (b str)
  (js-eval-webview (tab-view (ui-status (browser-ui b))) str))
(defun js-tabs (b str)
  (js-eval-webview (tab-view (ui-tabs (browser-ui b))) str))
(defun tab-title-fallback (property-value)
  (or property-value "(untitled)"))
(defun uri-fallback (property-value)
  (or property-value "about:blank"))


(defgeneric ui-update (browser symbol value))


;; Prompt
(defmethod ui-update (browser (sym (eql :prompt-send-key)) str)
  (js-status browser
             (format nil "bar.prompt.sendKey('~a');" str)))

(defmethod ui-update (browser (sym (eql :prompt-enter)) str)
  (js-status browser
             (format nil "bar.prompt.open('~a');" str)))

(defmethod ui-update (browser (sym (eql :prompt-leave)) val)
  (js-status browser "bar.prompt.close();"))

;; Status Display
(defmethod ui-update (browser (sym (eql :passthrough)) val)
  (js-status browser
             (if val
                 "bar.status.keymode.model.set('mode','Passthrough');"
                 "bar.status.keymode.model.set('mode','top');")))

(defmethod ui-update (browser (sym (eql :uri)) view)
  (js-status browser
             (format nil "bar.status.uri.model.set('uri','~a');"
                     (uri-fallback (property view :uri)))))

(defmethod ui-update (browser (sym (eql :link-hover)) uri)
  (js-status browser
             (format nil
                     "bar.status.uri.model.set('hover','~a');" uri)))

(defmethod ui-update (browser (sym (eql :scroll-indicator)) scrolled)
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
  (js-status browser (format nil "bar.status.progressIndicator.model.set('progress', '~a');"
                             (let ((p (property view :progress)))
                               (floor (* 100 (or
                                              (if (= p 0.0d0) ;; if not during inital load, the value is 0
                                                  1.0d0
                                                  p)
                                              777)))))))

(defmethod ui-update (browser (sym (eql :history)) view)
  (js-status browser (format nil "bar.status.history.model.set({backward: ~a, forward: ~a});"
                             (if (webkit-web-view-can-go-back view)
                                 "true" "false")
                             (if (webkit-web-view-can-go-forward view)
                                 "true" "false"))))

;; Tabs
(defmethod ui-update (browser (sym (eql :tabs-reset-list)) val)
  (js-tabs browser "tabbar.collection.remove(tabbar.collection.models);")
  (let ((views (browser-views browser)))
    (mapcar (lambda (view)
              (let ((title (tab-title-fallback (property view :title)))
                    (order (+ 1 (position view views))))
                (js-tabs browser (format
                                  nil
                                  "tabbar.collection.add({order:~a,title:'~a'});"
                                  order title))))
            views)))

(defmethod ui-update (browser (sym (eql :tabs-switched-page)) new-index)
  ;; gives new page index, also found by browser-tabs-current-index
  (js-tabs browser (format nil "tabbar.collection.moveCurrentTo(~a);"
                           (+ 1 new-index)))
  (js-status browser (format nil "bar.status.tabIndicator.model.set({current: ~a, total: ~a});"
                             (+ 1 new-index)
                             (length (browser-tabs browser)))))

(defmethod ui-update (browser (sym (eql :tabs-update-title)) view)
  (let ((order (position view (browser-views browser)))
        (title (tab-title-fallback (property view :title))))
    (when (and order title)
      (js-tabs browser (format
                        nil
                        "tabbar.collection.findOrder(~a).set('title','~a');"
                        (+ 1 order)
                        title)))))
