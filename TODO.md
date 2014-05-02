# Todo:

## WebKit
privoxy proxy

soup session
https://developer.gnome.org/gobject/unstable/gobject-Signals.html#g-signal-new
https://developer.gnome.org/libsoup/stable/SoupSession.html
https://developer.gnome.org/libsoup/stable/SoupURI.html#soup-uri-new
cookie handling, whitelist, ask when try to set
json cookie storing

NoScript, ask javascript, whitelist

Log history, in json format, on disk

requestpolicy, whitelist each requested domain

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
Node.js based SPAs, with global functions called by lisp side keys, pass key string
Node.js SPA server, could be automaticaly spawned as a child process of the daemon
;; navigation handler, for hosted files too, load them

;; evaluate a function given a string, exposed from loaded string
;; backbone+ browserifyed libraryes

<!--  -->


# Major milepoints:
* Key events & ComposeKey
* GTK3 minimum vpane height

# Not yet
* Distracted, by impemnteding colored uri segments

# Impentation condiderations
* Buffer Keys
  have differnt key maps way to invoke/change current keymaps

# Next
Tabs
    need to hook up signals to ui actions
    make keymap first, then ui backbone, then signals
    free views
Done: Scrolling
    scroll keys, j/k to current view, scroll window ajustments
    look through luakit's scrolling funcs
    visually update status of scroll
    move relatively x or y

Use a testing framework
http://www.cliki.net/test%20framework

Tabs
    add remove, ui-updates for models
    status bar commands
    buffer keys

Ui-views
    initialize race conditions

Status-bar commands
    :open allow search engines
