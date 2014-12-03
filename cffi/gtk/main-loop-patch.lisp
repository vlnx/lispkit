;; FIXME: Make it so simple-errors on the lisp side have other restarts than Abort gtk-main thread

(in-package :gtk-cffi+threads)
;; may have to patch in to asd file's depends on

;; cl-gtk2/gdk/gdk.threads.lisp
(defcfun gdk-threads-init :void)
;; FIXME: must init this
;; (glib:at-init () (gdk-threads-init))
;; defined in :gdk-cffi, gdk/threads.lisp
;; (defcfun gdk-threads-enter :void)
;; (defcfun gdk-threads-leave :void)

(defmacro with-gdk-threads-lock (&body body)
  `(progn
     (gdk-cffi::gdk-threads-enter)
     (unwind-protect
          (progn ,@body)
       (gdk-cffi::gdk-threads-leave))))

;; :gtk-cffi already has this, as the only main loop function
;; (defcfun (%gtk-main "gtk_main") :void)

;; cl-gtk2/gtk/gtk.main_loop_events.lisp
;; #+thread-support
(progn
  (defvar *main-thread* nil)
  (defvar *main-thread-level* nil)
  (defvar *main-thread-lock* (bordeaux-threads:make-lock "*main-thread* lock"))

  ;; NOTE: may need, from cl-gtk2/glib/glib.lisp
  ;; (at-finalize ()
  ;;   (when (and *main-thread* (bordeaux-threads:thread-alive-p *main-thread*))
  ;;     (bordeaux-threads:destroy-thread *main-thread*)
  ;;     (setf *main-thread* nil)))

  (defun ensure-gtk-main ()
    (bordeaux-threads:with-lock-held (*main-thread-lock*)
      (when (and *main-thread* (not (bordeaux-threads:thread-alive-p *main-thread*)))
        (setf *main-thread* nil))
      (unless *main-thread*
        (setf *main-thread*
              (bordeaux-threads:make-thread
               (lambda () (with-gdk-threads-lock (gtk-main)))
               :name "gtk3 main thread")
              *main-thread-level* 0))
      (incf *main-thread-level*))
    (values))

  (defun join-gtk-main ()
    (when *main-thread*
      (bordeaux-threads:join-thread *main-thread*)))

  (defun leave-gtk-main ()
    (bordeaux-threads:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (zerop *main-thread-level*)
        (gtk-main-quit)))))

;; cl-gtk2/glib/gobect.stable-pointer.lisp
(defvar *registered-stable-pointers* (make-array 0 :adjustable t :fill-pointer t))

(defun find-fresh-id ()
  (or (position nil *registered-stable-pointers*)
      (progn (vector-push-extend nil *registered-stable-pointers*)
             (1- (length *registered-stable-pointers*)))))

(defun allocate-stable-pointer (thing)
  "Allocates the stable pointer for @code{thing}. Stable pointer is an integer that can be dereferenced with @fun{get-stable-pointer-value} and freed with @fun{free-stable-pointer}. Stable pointers are used to pass references to lisp objects to foreign code.
@arg[thing]{any object}
@return{integer}"
  (let ((id (find-fresh-id)))
    (setf (aref *registered-stable-pointers* id) thing)
    (make-pointer id)))

(defun free-stable-pointer (stable-pointer)
  "Frees the stable pointer previously allocated by @fun{allocate-stable-pointer}"
  (setf (aref *registered-stable-pointers* (pointer-address stable-pointer)) nil))

(defun get-stable-pointer-value (stable-pointer)
  "Returns the objects that is referenced by stable pointer previously allocated by @fun{allocate-stable-pointer}. May be called any number of times."
  (when (<= 0 (pointer-address stable-pointer) (length *registered-stable-pointers*))
    (aref *registered-stable-pointers* (pointer-address stable-pointer))))

;; cl-gtk2/glib/glib.lisp
(defcfun "g_idle_add_full" :uint
  "A low-level function for adding callbacks to be called from main loop. Wrapper around g_idle_add_full.
Adds a function to be called whenever there are no higher priority events pending. If the function returns FALSE it is automatically removed from the list of event sources and will not be called again.
@arg[priority]{an integer specifying the priority. See @variable{+g-priority-default+}, @variable{+g-priority-default-idle+}, @variable{+g-priority-high+}, @variable{+g-priority-high-idle+}, @variable{+g-priority-low+}.}
@arg[function]{pointer to callback that will be called. Callback should accept a single pointer argument and return a boolean FALSE if it should be removed}
@arg[data]{pointer that will be passed to callback function}
@arg[notify]{function that will be called when callback is no more needed. It will receive the @code{data} argument}"
  (priority :uint)
  (function :pointer)
  (data :pointer)
  (notify :pointer))

;; cl-gtk2/gtk/gtk.misc.lisp
(defcallback stable-pointer-free-destroy-notify-callback :void
    ((data :pointer))
  (free-stable-pointer data))

(defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))
;; FIXME: not sure where this is from

(defun call-from-gtk-main-loop (function &key (priority 200))
  (g-idle-add-full priority
                   (callback call-from-main-loop-callback)
                   (allocate-stable-pointer function)
                   (callback stable-pointer-free-destroy-notify-callback))
  (ensure-gtk-main))

(defmacro within-main-loop (&body body)
  `(call-from-gtk-main-loop (lambda () ,@body)))
