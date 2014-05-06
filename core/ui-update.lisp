(in-package :lispkit)

;; Todo: add unbuffered keys when impumnted

(defun ui-update (browser &key
                            prompt-send-key
                            prompt-enter
                            prompt-leave
                            (passthrough 0)
                            history
                            uri
                            scroll-indicator
                            tabs-reset-list
                            progress
                            tabs-update-title
                            tabs-switched-page)
  "Take an element of the iterface with any number of arguments, eval what
needs to be done"
  (flet ((js-status (str)
           (js-eval-webview (tab-view (ui-status (browser-ui browser))) str))
         (js-tabs (str)
           (js-eval-webview (tab-view (ui-tabs (browser-ui browser))) str)))

    (when prompt-send-key (js-status
                           (format nil "bar.prompt.sendKey('~a');" prompt-send-key)))
    (when prompt-enter (js-status
                        (format nil "bar.prompt.open('~a');" prompt-enter)))
    (when prompt-leave (js-status "bar.prompt.close();"))

    (unless (eq 0 passthrough)
      (js-status
       (if passthrough
           "bar.status.keymode.model.set('mode','Passthrough');"
           "bar.status.keymode.model.set('mode','top');")))
    ;; "bar.status.history.model.set('backward',true);"
    ;; "bar.status.history.model.set('forward',true);"
    (when uri ;; Give view, so about:blank can be done here
      (js-status
       (format nil "bar.status.uri.model.set('uri','~a');"
               (or (property uri :uri)
                   "about:blank"))))

    (when tabs-reset-list
      (js-tabs "tabbar.collection.remove(tabbar.collection.models);")
      (let ((views (browser-views browser)))
        (mapcar (lambda (view)
                  (js-tabs (format
                            nil
                            "tabbar.collection.add({order:~a,title:'~a'});"
                            (+ 1 (position view views))
                            (property view :title))))
                views)))

    (when tabs-switched-page ;; gives new page index, also found by browser-tabs-current-index
      (js-tabs (format nil "tabbar.collection.moveCurrentTo(~a);"
                       (+ 1 tabs-switched-page)))
      (js-status (format nil "bar.status.tabIndicator.model.set({current: ~a, total: ~a});"
                         (+ 1 tabs-switched-page)
                         (length (browser-tabs browser)))))

    (when tabs-update-title ;; gives the view that needs the title updated
      (let* ((view tabs-update-title)
             (order (position view (browser-views browser)))
             (title (or (property view :title) "(untitled)")))
        (when (and order title)
          (js-tabs (format
                    nil
                    "tabbar.collection.findOrder(~a).set('title','~a');"
                    (+ 1 order)
                    title)))))

    (when scroll-indicator ;; give a tab-scroll
      (js-status (format nil "bar.status.scrollIndicator.model.set({y: ~a, ymax: ~a, x: ~a, xmax: ~a});"
                         (floor (property (vadjustment scroll-indicator) :value))
                         (-
                          (floor (property (vadjustment scroll-indicator) :upper))
                          (floor (property (vadjustment scroll-indicator) :page-size)))
                         (floor (property (hadjustment scroll-indicator) :value))
                         (-
                          (floor (property (hadjustment scroll-indicator) :upper))
                          (floor (property (hadjustment scroll-indicator) :page-size))))))

    (when progress ;; gives source view
      (js-status (format nil "bar.status.progressIndicator.model.set('progress', '~a');"
                         (let ((p (property progress :progress)))
                           (floor (* 100 (or
                                          (if (= p 0.0d0) ;; if not during inital load, the value is 0
                                              1.0d0
                                              p)
                                          777)))))))

    (when history ;; given view
      (js-status (format nil "bar.status.history.model.set({backward: ~a, forward: ~a});"
                         (if (webkit-web-view-can-go-back history)
                             "true" "false")
                         (if (webkit-web-view-can-go-forward history)
                             "true" "false"))))


    ))
