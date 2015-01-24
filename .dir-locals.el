;; Project local keys/functions
((nil . ((eval . (progn
                   (local-set-key [f1] 'slime-restart-inferior-lisp)
                   (local-set-key [f2] 'lispkit-debug)
                   (local-set-key [f3] 'lispkit-reload-ui-tabs)
                   (defun lispkit-debug ()
                     (interactive)
                     (slime-repl-send-string "(require :lispkit)(in-package :lispkit)(main)"))
                   (defun lispkit-reload-ui-tabs ()
                     (interactive)
                     (slime-repl-send-string "(within-main-loop (reload-view (tab-view (ui-tabs (browser-ui (current-browser))))))")))))))
