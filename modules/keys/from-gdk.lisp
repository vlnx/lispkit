(in-package :lispkit/keys)

(defun create-xic (gdk-window)
  "Create the input context for a gdk window"
  (when (null-pointer-p (c-set-locale 6 ""))
    (error "c-set-locale has failed"))
  (when (null-pointer-p (x-set-locale-modifiers ""))
    (error "x-set-locale-modifiers has failed"))
  ;; Window must be shown to retrieve x11 reference this way
  (let* ((xwin (gdk-cffi:gdk-x11-window-get-xid gdk-window))
         (dis (gdk-cffi:gdk-x11-get-default-xdisplay))
         (xim (x-open-im dis (null-pointer) (null-pointer) (null-pointer)))
         (xic (x-create-ic xim
                           "inputStyle"
                           '(:xim-pre-edit-nothing :xim-status-nothing)
                           "clientWindow" xwin
                           ;; "focusWindow" xwin
                           (null-pointer))))
    (when (null-pointer-p xic)
      (error "xic is null"))
    xic))


(defun gdk-event->x-key-event (gdk-event)
  "Must run `foreign-free` on ret"
  ;; From `gtk_im_context_xim_filter_keypress`
  (with-foreign-slots ((gdk-cffi::send-event
                        gdk-cffi::window
                        gdk-cffi::time
                        gdk-cffi::state
                        gdk-cffi::hardware-keycode)
                       gdk-event (:struct gdk-cffi::event-key))
    (x-create-key-event :type 2 ; KeyPress
                        :serial 0
                        :send-event gdk-cffi::send-event
                        :display
                        (gdk-cffi:gdk-x11-get-default-xdisplay)
                        :window
                        (gdk-cffi::gdk-x11-window-get-xid
                         (pointer gdk-cffi::window))
                        :root
                        (gdk-cffi::gdk-x11-get-default-root-xwindow)
                        :subwindow
                        (gdk-cffi::gdk-x11-window-get-xid
                         (pointer gdk-cffi::window))
                        :time (null-pointer) ; gdk-cffi::time is an int
                        :x 0
                        :y 0
                        :x-root 0
                        :y-root 0
                        :state (foreign-bitfield-value
                                'gdk-cffi::modifier-type
                                gdk-cffi::state)
                        :keycode gdk-cffi::hardware-keycode
                        :same-screen t)))

(defun process-gdk-event->key (gdk-key-event xic)
  (when (null-pointer-p xic)
    (error "xic is null"))
  (let ((key-event (gdk-event->x-key-event gdk-key-event))
        (buffer-size 512)
        key)
    (unless (x-filter-event key-event 0)
      (with-foreign-objects ((buffer :unsigned-int (1+ buffer-size))
                             (sym :int)
                             (status :int))
        (setf (mem-aref buffer :unsigned-int 0) 0)
        (x-wc-lookup-string xic
                            key-event
                            buffer
                            buffer-size
                            sym
                            status)
        (unless (ignorable-keysym-p (mem-ref sym :int))
          (setf key
                (char-state->key
                 (keysym-or-buffer->char
                  (mem-ref sym :int)
                  (mem-aref buffer :unsigned-int 0))
                 (gdk-cffi::parse-event gdk-key-event :state))))))
    (foreign-free key-event)
    key))
