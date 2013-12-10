(in-package :lispkit)

(setf *active-maps* '(*top-map*))
(defvar *top-map* (make-kmap))
(defvar *map-prompt* (make-kmap))


(defmacro defkey (map key-str args &body body)
  `(define-key ,map (kbd ,key-str)
     (lambda ,args
       ,@body)))

(defkey *top-map* "a" (v)
  (webkit-web-view-load-uri v "http://www.example.com"))
(defkey *top-map* "A" (v)
  (webkit-web-view-load-uri v "http://www.duckduckgo.com"))

(defkey *top-map* "j" (v)
    (js-eval-webview v "window.alert('Scroll down');"))
(defkey *top-map* "k" (v)
    (js-eval-webview v "window.alert('Scroll Up');"))

(defkey *top-map* ";" (v)
  (setf *active-maps* '(*map-prompt*))
  (ui-update 'prompt-enter ""))
(defkey *map-prompt* "ESC" (v)
  (setf *active-maps* '(*top-map*))
  (ui-update 'prompt-leave))

(defkey *top-map* "o" (v)
  (setf *active-maps* '(*map-prompt*))
  (ui-update 'prompt-enter "open"))
