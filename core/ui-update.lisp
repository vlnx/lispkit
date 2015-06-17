(in-package :lispkit)

;; Helper functions
(defun js (sym b str &rest rest)
  (apply #'js-eval-webview
         (case sym
           (hints
            (tab-view (ui-hints (browser-ui b))))
           (current-tab
            (tab-view (current-tab b)))
           (status
            (tab-view (ui-status (browser-ui b))))
           (tabs
            (tab-view (ui-tabs (browser-ui b))))
           (t (error "js: invalid sym")))
         str
         rest))

(defun tab-title (tab)
  (or (property (tab-view tab) :title)
      (property (tab-view tab) :uri)))

(defgeneric ui-update (browser symbol value))

(defun escape-single-quote (str)
  (ppcre:regex-replace-all "'" str "\\\\'"))

;; Prompt
(defmethod ui-update (browser (sym (eql :prompt-insert)) str)
  (js 'status browser
      (format nil "bar.prompt.input.insert('~a');"
              (escape-single-quote str))))

(defmethod ui-update (browser (sym (eql :prompt-enter)) arg)
  (let ((content arg) (phrase ""))
    (if (listp arg)
        (setf content (getf arg :content)
              phrase (getf arg :phrase)))
    (js 'status browser
        (format nil "bar.prompt.open('~a','~a');"
                (escape-single-quote content)
                (escape-single-quote phrase)))))

(defmethod ui-update (browser (sym (eql :prompt-leave)) val)
  (js 'status browser "bar.prompt.close();"))

;; Status Display
(defmethod ui-update (browser (sym (eql :keymode)) val)
  (let ((modes
         (if (passthrough-state (browser-key-state browser))
             "passthrough"
             (format nil "~{~a~^, ~}"
                     (mapcar #'symbol-to-string
                             (active-maps
                              (browser-key-state browser)))))))
    (js 'status browser
        (format nil
                "bar.status.keymode.model.set('mode','~a');"
                (escape-single-quote modes)))))

(defmethod ui-update (browser (sym (eql :buffer-set)) val)
  (js 'status browser
      (format nil
              "bar.status.buffer.model.set('content','~a');"
              (escape-single-quote val))))

(defmethod ui-update (browser (sym (eql :uri)) view)
  (setf view (tab-view (current-tab browser)))
  (js 'status browser
      (format nil "bar.status.uri.model.set('uri','~a');"
              (escape-single-quote
               (property view :uri)))))

(defmethod ui-update (browser (sym (eql :link-hover)) uri)
  (js 'status browser
      (format nil
              "bar.status.uri.model.set('hover','~a');"
              (escape-single-quote uri))))

(defmethod ui-update (browser (sym (eql :scroll-indicator)) scrolled)
  (setf scrolled (tab-scroll (current-tab browser)))
  (js 'status browser
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
  (js 'status browser
      (format
       nil
       "bar.status.progressIndicator.model.set('progress', '~a');"
       (let ((p (property view :progress)))
         (floor (* 100 (or
                        ;; if not during initial load, the value is 0
                        (if (= p 0.0d0)
                            1.0d0
                            p)
                        777)))))))

(defmethod ui-update (browser (sym (eql :history)) view)
  (setf view (tab-view (current-tab browser)))
  (js 'status browser
      (format
       nil
       "bar.status.history.model.set({backward: ~a, forward: ~a});"
       (if (webkit-web-view-can-go-back view)
           "true" "false")
       (if (webkit-web-view-can-go-forward view)
           "true" "false"))))

;; Tabs
(defmethod ui-update (browser (sym (eql :add-tab)) tab)
  (let ((title (tab-title tab))
        (order (position tab (browser-tabs browser))))
    (js 'tabs browser
        (format
         nil
         "tabbar.collection.add({order:~a,title:'~a'});"
         order (escape-single-quote title)))))

(defmethod ui-update (browser (sym (eql :tabs-reset-list)) val)
  (js 'tabs browser
      "tabbar.collection.remove(tabbar.collection.models);")
  (mapcar (lambda (tab)
            (ui-update browser :add-tab tab))
          (browser-tabs browser)))

(defmethod ui-update (browser (sym (eql :current-tab)) val)
  (let ((index (browser-tabs-current-index browser))
        (zerobased-length (1- (length (browser-tabs browser)))))
    (js 'tabs browser
        (format nil "tabbar.collection.moveCurrentTo(~a);" index))
    (js 'status browser
        (format nil
                "bar.status.tabIndicator.model.set({current: ~a, total: ~a});"
                index
                zerobased-length))))

(defmethod ui-update (browser (sym (eql :tabs-update-title)) tab)
  (let ((order (position tab (browser-tabs browser)))
        (title (tab-title tab)))
    (when (and order title)
      (js 'tabs browser
          (format nil
                  "tabbar.collection.findOrder(~a).set('title','~a');"
                  order
                  (escape-single-quote title))))))

(defmethod ui-update (browser (sym (eql :notify)) str)
  (ui-update browser :buffer-set str))
