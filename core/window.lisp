;; (in-package :gtk-cffi)
;; (defcfun "gtk_paned_set_position" :void
;;   (pane pobject)
;;   (pos :int))
;; (export 'gtk-paned-set-position)

(in-package :lispkit)

(defvar *uri-homepage* "http://10.1.7.1/startpage/index.html"
  "The homepage uri to load by default")

    ;; Commented until scrolling mechanism is complete
    ;; Force the main frame to respond to it's parent container's policy change
    ;; (defcallback true :boolean () t)
    ;; (setf (gsignal (make-instance 'g-object :pointer
    ;;                               (webkit-web-view-get-main-frame view))
    ;;                "scrollbars-policy-changed")
    ;;       (cffi:callback true))
    ;; (setf (policy scrolled-win) '(:never :never))

(defun win ()
  "Open up the gtk window"
  (gtk-init)
  (let ((win (make-instance 'window
                             :width 800 :height 600
                             :title "LispKit"
                             :has-resize-grip nil
                             :signals '(:destroy :gtk-main-quit)))
         (ui-tabs (make-instance 'scrolled-window))
         (notebook (make-instance 'scrolled-window))
         (ui-status (make-instance 'scrolled-window))
         (pane1 (make-instance 'v-paned))
         (pane2 (make-instance 'v-paned)))

    ;; Connect scrolling widgets with their content
    (add ui-tabs (make-instance 'widget :pointer 
                                (ui-new-view "ui://tabs")))
    (add notebook (make-instance 'widget :pointer 
                                 (tab-new *uri-homepage*)))
    (add ui-status (make-instance 'widget :pointer 
                                  (ui-new-view "ui://status")))

    ;; Layout configuration to get static heights on top and bottom
    ;; :shrink when nil respects the child's minimal size
    ;; :resize when t will resize along with the main window
    (pack pane1 ui-tabs :resize nil :shrink nil)
    (pack pane1 pane2 :resize t :shrink t)
    (pack pane2 notebook :resize t :shrink t)
    (pack pane2 ui-status :resize nil :shrink nil)
    
    ;; Set the minimal heights of the ui widgets
    (setf (size-request ui-tabs) '(-1 30)
          (size-request ui-status) '(-1 100))

    (add win pane1)
    
    (setf (gsignal win "key-press-event") (callback on-key-press)
          (gsignal win "key-release-event") (callback on-key-release))

    (show win :all t)
    (gtk-main)))


;; NEW TODO:
;; Load top view with ui://tabs
;; establish keys, look at stumpwm/k{map,eysym,eytrans}.lisp, read keyval directly before translation
;; free views


;; TODO:
;; webikit load string, On this side compile the string from 'inline/file' stylus/jade/coffeescript(!node)
;; DONE: Evaluate js, alert, then try to return a string
;; evaluate a function given a string, exposed from loaded string
;; backbone+ browserifyed libraryes
;; navigation handler, for hosted files too, load them

;; proxy, even loaded scripts sometiems
