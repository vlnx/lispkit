(require 'asdf)
(asdf:oos 'asdf:load-op 'lispkit)

#+sbcl
(sb-ext:save-lisp-and-die "lispkit"
                          :toplevel (lambda ()
                                      ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
                                      (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                                      (lispkit:lispkit)
                                      0)
                          :executable t)
