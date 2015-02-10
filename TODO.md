# TODO

## libsoup
https://developer.gnome.org/gobject/unstable/gobject-Signals.html#g-signal-new
https://developer.gnome.org/libsoup/stable/SoupSession.html
https://developer.gnome.org/libsoup/stable/SoupURI.html#soup-uri-new
###  cookies
whitelist
notify when attempted to set
store 'jar' as json rather than sql
### proxy
set http proxy for soup session

## History
Log history, in json format, on disk
unclose tab

# Addon implementation
Javascript enable/disable, whitelist, noscript
requestpolicy, whitelist each requested domain

Don't let webkit attempt to load schemas it can't handle,
ex: js:, javascript:, ftp:, mailto:, magnet:, git:,

# Prompt
syntax highlighting for uri and command and search engine
bindings to change/insert search engine name

## Uri-editing mode in the prompt
break it apart, (e.x: long queries, display in a vertical list with
value being editable)

# Save file dialog, to replace the gtk one
should start in a base directory per filetype/domain
find a binding for 'save image as'
also a file upload handler

* keymaps per sites via `*uri-scripts*`

* Enable/disable `*uri-scripts*`, tune from interface
    ex: for switching between a light and dark theme for a site

* Tab display: favicons

side tabs option
change gtk layout to use vbox instead of hbox for tabs
    ||@@@@@@@@@@@@@@@@@@
    ||@@@@@@@@@@@@@@@@@@
    ||@@@@@@@@@@@@@@@@@@
    ||@@@@@@@@@@@@@@@@@@
    ||@@@@@@@@@@@@@@@@@@
    --------------------

The window focus callback isn't invoked properly enough to set `*browser-current-index*` consistiently

# Optimization
* use declare forms
* experiment will optimization/speed/safety/debug declaim
