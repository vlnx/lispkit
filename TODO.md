# Goals

## Fluid data interface
WebKitView javascript function injection, (luakit's export function)
Evaluate javascript in a view (WebKitView evaluate_script)
return basic types from javascript, defcallback?
Trans-compiler support, CoffeeScript, Stylus, Jade
* caching: Save result, Recompile if file hash changed
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


## WebKit
privoxy proxy

soup session
cookie handling, whitelist, ask when try to set
json cookie storing

NoScript, ask javascript, whitelist

Log history, in json format, on disk

# Ease of use

## Content keys
Navigation functions to be invoked for a website, (reddit, 4chan) comments posts
Save image,page as, sort by directory in ido like mode
CSS,JS per uri, type,domain

## Smart Following
Click but don't follow javascript schemas
Magnet schema handling like my luakit plugin

## Smart Prompt
Syntax highlighting
Search engines, highlight keyword, keys to change keyword

URI mode, break it apart, (e.x: long queries, display in a vertical list with
value being editable)
try cl-uri

Quicklinks, type site keyword to go to

## Web Apps
Node.js based SPAs, with global functions called by lisp side keys, pass key string?
Node.js SPA server, could be automaticaly spawned as a child process of the daemon
