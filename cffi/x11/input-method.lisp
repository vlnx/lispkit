(in-package :x11-binding)

(define-foreign-library x11
    (:unix "libX11.so"))

(use-foreign-library x11)

(export '(x-filter-event
          x-wc-lookup-string
          x-key-event
          x-open-im
          c-set-locale
          x-set-locale-modifiers
          x-create-ic
          xic-focus))

;; `man 3 XFilterEvent`
(defcfun ("XFilterEvent" x-filter-event) :boolean
  (event :pointer)
  (zero-to-use-event-window :int))

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
  (modifier-str c-string))

(defcfun ("setlocale" c-set-locale) :pointer
  (con :int)
  (modifier-str c-string))

(defcfun ("XOpenIM" x-open-im) :pointer
  (display :pointer)
  (xrm-database :pointer)
  (res-name :pointer)
  (res-class :pointer))

(defbitfield xim-styles
  (:xim-pre-edit-nothing #x0008)
  (:xim-status-nothing #x0400))
;; #define XIMPreeditNothing   0x0008L
;; #define XIMStatusNothing    0x0400L

(defcfun ("XCreateIC" x-create-ic) :pointer
  (xim :pointer)
  (input-style c-string)
  (styles xim-styles)
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
