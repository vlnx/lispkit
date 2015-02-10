# LispKit, a WebKitGTK Interface

Written in Common Lisp using SBCL, using CFFI to interface with WebKitGTK
and GTK3.

Based on LuaKit, written in lisp for the live enviroment.
Major ideas that differ:
* keybinding, keep hidden from the webview, don't let them leak.
* live enviroment
* interface based on loading transcompiled web languages, more freedom than
  gtk labels
* User Interface is loaded using the user(scripts|styles) method, allowing to
  expansion in order to fully control your most accessed sites.

## Previous Impemented Goals

## Fluid data interface
WebKitView javascript function injection, (luakit's export function)
Evaluate javascript in a view (WebKitView evaluate_script)
return basic types from javascript, defcallback?
Trans-compiler support, CoffeeScript, Stylus, Jade
* caching: Save result, Recompile if file mtime changed
* ther files, loaded in to strings to evaluate
Node intragration? Browserify modules, before evaluation in webkit ?

## User Interface Views:
WebKit instances to display information
Tab bar (GTK Notebook Replacement)
Status Bar
Prompts
* GTK replacements, e.x. dropdown, alerts, confirmations, file selection
* emacs ido
GTK Horizontal Containers, variable height

Have 'chrome://' like pages.
Shown in variable, hbox over status bar
All key controlled, backbone.js and jade

## Key press handling
Don't send everything to the active WebKitView
Use 'modes'
* Command mode, respond to commands keys
* Prompt mode, entered to prompt, or prompt commands
* Passthrough mode, send keys to WebKitView widget

## System intragration:
Daemon?
daemon, controlls running windows, kill WebKitView hogs

Invokation call script with url to open it, in current window, definitly have
way to determine witch window to send it to. Have windows, contain, groups?
list groups, change groups, rename groups, new group
default group, like subtle for uris, uri host matching, e.x. youtube for default

Socket to pipe to to find current instances, not dbus
Show current tabs, json written to /tmp ?


For my hiatus'ed, node.js webapps/singlepageapplications, interfaces to mpd and rtorrent
instead of them handling the keystrokes through ***REMOVED*** javascript
use this gdk/lisp system's key maps to invoke the proper javascript action on the page
