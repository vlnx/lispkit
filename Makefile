npmdeps:
	@cd site/ && npm i

# Build in the main static deps in to the core dump
dynamic-loader:
	sbcl --noinform \
		--eval '(require :asdf)' \
		--eval '(require :sb-posix)' \
		--eval '(require :gtk-cffi)' \
		--eval '(require :bordeaux-threads)' \
		--eval '(require :cl-json)' \
		--eval '(require :cl-ppcre)' \
		--eval '(sb-posix:putenv "SBCL_HOME=/usr/lib/sbcl")' \
		--eval '(save-lisp-and-die "lispkit" :toplevel (lambda () (eval-when (:execute) (load "$(PWD)/asdf-core.lisp")) 0) :executable t)' \
		--eval '(quit)'

static-core:
	sbcl --noinform \
		--eval '(require :asdf)' \
		--eval '(require :sb-posix)' \
		--eval '(sb-posix:putenv "SBCL_HOME=/usr/lib/sbcl")' \
		--eval '(require :lispkit)' \
		--eval '(save-lisp-and-die "lispkit.static" :toplevel (lambda () (lispkit:main) 0) :executable t)' \
		--eval '(quit)'

watchify:
	sbcl --noinform \
		--eval '(require :asdf)' \
		--eval '(require :sb-posix)' \
		--eval '(sb-posix:putenv "SBCL_HOME=/usr/lib/sbcl")' \
		--eval '(require :lispkit)' \
		--eval '(lispkit:watchify)'
