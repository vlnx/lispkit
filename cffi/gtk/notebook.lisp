(in-package :gtk-cffi)
;; GtkNotebook

(export
 '(gtk-notebook-set-show-tabs
   gtk-notebook-set-show-border
   gtk-notebook-insert-page
   gtk-notebook-set-current-page
   gtk-notebook-remove-page
   gtk-notebook-get-current-page))

(defcfun gtk-notebook-insert-page :int
  (notebook pobject)
  (child pobject)
  (inital-label pobject)
  (position :int))

(defcfun gtk-notebook-set-show-tabs :void
  (notebook pobject)
  (bool :boolean))

(defcfun gtk-notebook-set-show-border :void
  (notebook pobject)
  (bool :boolean))

(defcfun gtk-notebook-set-current-page :void
  (notebook pobject)
  (page-num :int))

(defcfun gtk-notebook-get-current-page :int
  (notebook pobject))

(defcfun gtk-notebook-get-n-pages :int
  "number of pages"
  (notebook pobject))

(defcfun gtk-notebook-get-nth-page :int
  "content of n page"
  (notebook pobject))

(defcfun gtk-notebook-remove-page :void
  "Remove a page by index"
  (notebook pobject)
  (page-num :int))

(defmethod gconstructor ((notebook notebook)
                         &rest rest
                         &key (show-tabs t) (show-border t))
  ""
  (declare (ignore rest))
  (let ((n (gtk-notebook-new)))
    (gtk-notebook-set-show-tabs n show-tabs)
    (gtk-notebook-set-show-border n show-border)
    n))
