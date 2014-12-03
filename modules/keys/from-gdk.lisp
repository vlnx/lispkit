(in-package :lispkit/keys)

(defun create-xic (gdk-window)
  "Create the input context for a gdk window"
  (if (null-pointer-p (x11-binding::c-set-locale 6 ""))
      (error "c-set-locale has failed"))
  (if (null-pointer-p (x-set-locale-modifiers ""))
      (error "x-set-locale-modifiers has failed"))
  ;; Window must be shown to retrive x11 reference this way
  (let* ((xwin (gdk-cffi:gdk-x11-window-get-xid gdk-window))
         (dis (gdk-cffi:gdk-x11-get-default-xdisplay))
         (xim (x-open-im dis (null-pointer) (null-pointer) (null-pointer)))
         (xic (x-create-ic xim
                           "inputStyle" 1032 ; raw bitfield
                           "clientWindow" xwin
                           ;; "focusWindow" xwin
                           (null-pointer))))
    (if (null-pointer-p xic)
        (error "xic is null"))
    xic))

(defun gdk-event->x-key-event (gdk-event)
  "Must run `foreign-free` on ret"
  ;; From `gtk_im_context_xim_filter_keypress`
  (let ((key-event (foreign-alloc '(:struct x11-binding::x-key-event))))
    (with-foreign-slots ((gdk-cffi::send-event
                          gdk-cffi::window
                          gdk-cffi::time
                          gdk-cffi::state
                          gdk-cffi::hardware-keycode)
                         gdk-event (:struct gdk-cffi::event-key))
      (with-foreign-slots ((x11-binding::type
                            x11-binding::serial
                            x11-binding::send-event
                            x11-binding::display
                            x11-binding::window
                            x11-binding::root
                            x11-binding::subwindow
                            x11-binding::time
                            x11-binding::x
                            x11-binding::y
                            x11-binding::x-root
                            x11-binding::y-root
                            x11-binding::state
                            x11-binding::keycode
                            x11-binding::same-screen)
                           key-event (:struct x11-binding::x-key-event))
        (setf x11-binding::type 2 ; KeyPress
              x11-binding::serial 0
              x11-binding::send-event gdk-cffi::send-event
              x11-binding::display (gdk-cffi:gdk-x11-get-default-xdisplay)
              x11-binding::window (gdk-cffi::gdk-x11-window-get-xid (pointer gdk-cffi::window))
              x11-binding::root (gdk-cffi::gdk-x11-get-default-root-xwindow)
              x11-binding::subwindow (gdk-cffi::gdk-x11-window-get-xid (pointer gdk-cffi::window))
              x11-binding::time gdk-cffi::time
              x11-binding::x 0
              x11-binding::y 0
              x11-binding::x-root 0
              x11-binding::y-root 0
              x11-binding::state (foreign-bitfield-value 'gdk-cffi::modifier-type gdk-cffi::state)
              x11-binding::keycode gdk-cffi::hardware-keycode
              x11-binding::same-screen t)))
    key-event))

(defun process-gdk-event->key (gdk-key-event xic)
  (if (null-pointer-p xic)
      (error "xic is null"))
  (let ((key-event (gdk-event->x-key-event gdk-key-event))
        (buffer-size 512)
        key)
    (unless (x-filter-event key-event 0)
      (with-foreign-objects ((buffer :unsigned-int (1+ buffer-size))
                             (sym :int) (status :int))
        (setf (mem-aref buffer :unsigned-int 0) 0)
        (x11-binding:x-wc-lookup-string
         xic key-event
         buffer buffer-size
         sym status)
        ;; (print (mem-ref status :int))
        ;; (print (gdk-cffi::parse-event gdk-key-event :hardware-keycode))
        ;; refactor:
        (unless (= (mem-ref status :int) 3) ; XLookupKeySym = function key without string
          (unless (ignorable-keysym-p (mem-ref sym :int))
            (setf key
                  (char-state->key
                   (keysym-or-string->char
                    (mem-ref sym :int)
                    (mem-aref buffer :unsigned-int 0))
                   (gdk-cffi::parse-event gdk-key-event :state)))))))
    (foreign-free key-event)
    key))
