(require :sb-posix)
(sb-posix:putenv "SBCL_HOME=/usr/lib/sbcl")

(require :lispkit)
(lispkit:main)
