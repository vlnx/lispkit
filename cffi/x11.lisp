(in-package :x11-binding)

(define-foreign-library x11
  (:unix "libX11.so"))
(use-foreign-library x11)

(export '(x-filter-event
          xevent-union
          x-wc-lookup-string
          x-key-event
          x-any-event
          x-open-im
          x-set-locale-modifiers
          ;; x-display-of-im
          x-create-ic
          x-open-display
          x-key-press))


;; man page it
(defcfun ("XFilterEvent" x-filter-event) :boolean
  (event :pointer)
  (zero-to-use-event-window :int))
  ;; (window :pointer)) ;; if nil will reley on window slot of xanyevent



(defcunion xevent
  (type :int)
  (pad :long :count 24))
  ;; (any :pointer))
(defcstruct x-any-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display :pointer)
  (window :pointer))
(defcstruct x-key-event
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display :pointer)
  (window :pointer)
  (root :pointer)
  (subwindow :pointer)
  (time :pointer)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :unsigned-int)
  (keycode :unsigned-int)
  (same-screen :boolean))

(defcfun ("XwcLookupString" x-wc-lookup-string) :unsigned-long
  (xic :pointer)
  (event :pointer)
  (buffer :pointer)
  (buffer-size :int)
  (key-sym :pointer)
  (status :pointer))


(defcfun ("XSetLocaleModifiers" x-set-locale-modifiers) :pointer
  (modifer-str c-string))

;; http://stackoverflow.com/questions/6256179/how-can-i-find-the-value-of-lc-xxx-locale-integr-constants-so-that-i-can-use-the
(defcfun ("setlocale" c-set-locale) :pointer
  (con :int)
  (modifer-str c-string))

(defcfun ("XOpenDisplay" x-open-display) :pointer
  (something :pointer))
(defcfun ("XOpenIM" x-open-im) :pointer
  (display :pointer)
  (something-1 :pointer)
  (something-2 :pointer)
  (something-3 :pointer))

;; (defbitfield xim-styles
;;   (:xim-pre-edit-nothing #x0008)
;;   (:xim-status-nothing #x0400))
;; Bitfield BitOr
;; #define XIMPreeditNothing   0x0008L
;; #define XIMStatusNothing    0x0400L
;; 0x0008 | 0x0400 => 1032 in decimal
(defcfun ("XCreateIC" x-create-ic) :pointer
  (xim :pointer)
  (input-style c-string) (styles :int) ;; (input-style c-string) (styles xim-styles)
  (client c-string) (win-ref-1 :pointer)
  ;; (focus c-string) (win-ref-2 :pointer)
  (terminate :pointer))

(defcfun ("XSetICFocus" x-ic-set-focus) :pointer
  (xic :pointer))
(defcfun ("XUnsetICFocus" x-ic-unset-focus) :pointer
  (xic :pointer))

(defun xic-focus (xic focus)
  (if focus
      (x-ic-set-focus xic)
      (x-ic-unset-focus xic)))
      
