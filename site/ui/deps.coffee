window.jade = require 'jade/lib/jade.js'

# DOM utilites built with browserify using ender to combine librarys into a
# jQuery like interface
window.$ = require 'ender-js'

# # Ender shim
# ender = $
# FIXME: vendor shim removed wrapper
require 'bonzo/src/ender.js'
# window.bonzo = require 'bonzo'

# Qwery shim
# From qwery/src/ender.js modifed to rely on bonzo
# Don't have a reason to add 'find', 'and', 'is' methods yet
window.qwery = require 'qwery'
$.pseudos = qwery.pseudos
$._select = (s, r) ->
    if /^\s*</.test(s)
        $.create(s, r)
    else
        qwery(s, r)

# console.log 'n'
# window.A = "aa"

window.Backbone = require 'backbone'
Backbone.$ = $ # Give backbone, ender to function

$.ender cache: require 'kizzy'

window.Deps = {}
window.Deps.url = require 'url'
