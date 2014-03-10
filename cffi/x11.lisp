(in-package :x11-binding)

(define-foreign-library x11
  (:unix "libX11.so"))
(use-foreign-library x11)

(export '(x-filter-event
          x-event
          x-key-event
          x-open-im
          x-set-locale-modifiers
          x-display-of-im
          x-create-ic
          x-open-display
          x-key-press))

;; /usr/include/X11/X.h:181
(defvar x-key-press 2)
;; NOTE: sorry no earmuffs

;; man page it
(defcfun ("XFilterEvent" x-filter-event) :boolean
  (event :pointer)
  (window :pointer)) ;; FIXME:'None'?

; Xlib.h:L987-L1019
(defcunion x-event
  (type :int)
  ;; (xany :pointer)
  (xkey :pointer))
  ;; (xbutton :pointer)
  ;; (xmotion :pointer)
  ;; (xcrossing :pointer)
  ;; (xfocus :pointer)
  ;; (xexpose :pointer)
  ;; (xgraphicsexpose :pointer)
  ;; (xnoexpose :pointer)
  ;; (xvisibility :pointer)
  ;; (xcreatewindow :pointer)
  ;; (xdestroywindow :pointer)
  ;; (xunmap :pointer)
  ;; (xmap :pointer)
  ;; (xmaprequest :pointer)
  ;; (xreparent :pointer)
  ;; (xconfigure :pointer)
  ;; (xgravity :pointer)
  ;; (xresizerequest :pointer)
  ;; (xconfigurerequest :pointer)
  ;; (xcirculate :pointer)
  ;; (xcirculaterequest :pointer)
  ;; (xproperty :pointer)
  ;; (xselectionclear :pointer)
  ;; (xselectionrequest :pointer)
  ;; (xselection :pointer)
  ;; (xcolormap :pointer)
  ;; (xclient :pointer)
  ;; (xmapping :pointer)
  ;; (xerror :pointer)
  ;; (xkeymap :pointer)
  ;; (xgeneric :pointer)
  ;; (xcookie :pointer)
  ;; (pad :long :count 24))

(defcstruct x-key-event
  (type :int)
  (serial :pointer)
  (send-event :boolean)
  (display :pointer)
  (window :pointer)
  (root :pointer)
  (subwindow :pointer)
  (time :pointer)
  (x :pointer)
  (y :pointer)
  (x-root :pointer)
  (y-root :pointer)
  (state :unsigned-int)
  (keycode :unsigned-int)
  (same-screen :boolean))


(defcfun ("XSetLocaleModifiers" x-set-locale-modifiers) :pointer
  (modifer-str c-string))
(defcfun ("XOpenDisplay" x-open-display) :pointer
  (something :pointer))
(defcfun ("XOpenIM" x-open-im) :pointer
  (display :pointer)
  (something-1 :pointer)
  (something-2 :pointer)
  (something-3 :pointer))

(defbitfield xim-styles
  (:xim-pre-edit-nothing #x0008)
  (:xim-status-nothing #x0400))
(defcfun ("XCreateIC" x-create-ic) :pointer
  (xim :pointer)
  (input-style c-string) (styles xim-styles)
  (client c-string) (win-ref-1 :pointer)
  ;; (focus c-string) (win-ref-2 :pointer)
  (terminate :pointer))

(defcfun ("XDisplayOfIM" x-display-of-im) :pointer
  (im :pointer))
