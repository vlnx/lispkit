# WebkitGTK
2.4.1 as `libwebkitgtk-3.0.so`

With the patched source
    sed -i 's/\*minimum = \*natural = view->contents\(Width\|Height\)();/\*minimum = \*natural = 0;/g' ./${pkgbase}-${pkgver}/Source/WebKit/gtk/webkit/webkitwebview.cpp

# GTK3 through gtk-cffi
    git clone https://github.com/Kalimehtar/gtk-cffi.git $DEV_HOME/gtk-cffi/

# ASDF 3
In `$XDG_CONFIG_HOME/common-lisp/source-registry.conf` include:
    (:tree (:home "dev/gtk-cffi"))
    (:tree (:home "dev/lispkit"))

# SBCL
In slime session
    (require :lispkit)
    (in-package :lispkit)
    (main)


