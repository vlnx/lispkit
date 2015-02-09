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

## invocation
as a binary / dynamic sbcl core dump like current wm
Manage the env var 'MOZ_PLUGIN_PATH' where webkit last looks for plugins

## History
Log history, in json format, on disk
unclose tab

# Addon replacement
Javascript enable/disable, whitelist, noscript
requestpolicy, whitelist each requested domain

Don't let webkit attempt to load schemas it can't handle,
ex: js:, javascript:, ftp:, mailto:, magnet:, git:,

Prompt
syntax highlighting for uri and command and search engine
bindings to change/insert search engine name

Status - uri display
color uri segments

Uri-editing mode in the prompt
break it apart, (e.x: long queries, display in a vertical list with
value being editable)

Save file dialog, to replace the gtk one
* should start in a base directory per filetype
* also find a binding to 'save image as'
also a file upload handler

key maps for sites, to invoke functions on the page

For my hiatus'ed, node.js webapps/singlepageapplications, interfaces to mpd and rtorrent
instead of them handling the keystrokes through ***REMOVED*** javascript
use this gdk/lisp system's key maps to invoke the proper javascript action on the page

Enable/disable *uri-scripts*, tune from interface
ex: for switching between a light and dark theme for a site

Tab ideas
* favicons then
* side tabs option
    change gtk layout to use vbox instead of hbox for tabs
        ||@@@@@@@@@@@@@@@@@@
        ||@@@@@@@@@@@@@@@@@@
        ||@@@@@@@@@@@@@@@@@@
        ||@@@@@@@@@@@@@@@@@@
        ||@@@@@@@@@@@@@@@@@@
        --------------------

Make `*browser-current-index*` be set properly when needed

Attempt optimization, experiment will optimization/speed/safety/debug declaim
Also use declare type all over

http://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#WebKitWebView-create-web-view
http://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#WebKitWebView-new-window-policy-decision-requested

http://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#WebKitWebView-mime-type-policy-decision-requested
