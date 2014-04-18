image:
	CL_SOURCE_REGISTRY=$(PWD): sbcl --load ./system/make-image.lisp
# sbcl_BUILDOPTS=--eval '(require :asdf)' --load ./stumpwm.asd --load ./make-image.lisp
	# CL_SOURCE_REGISTRY=$(PWD): $(LISP) $(@LISP@_BUILDOPTS)
npmdeps:
	@cd site/ui/ && npm i
