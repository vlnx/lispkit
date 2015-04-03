window.jade = require 'jade/lib/jade.js'

window.S = require 'string'
window._ = require 'underscore'

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

window.Backbone = require 'backbone'
Backbone.$ = $ # Give backbone, ender to function

$.ender cache: require 'kizzy'

window.Deps = {}
window.Deps.url = require 'url'

Backbone.Collection::modelRelativeTo = (model, n) -> @at ((@indexOf model) + n)

class Backbone.CollectionNav extends Backbone.Collection
    currentModel: null
    moveTo: (model) ->
        @trigger 'nav-prev', @currentModel if @currentModel
        @currentModel = model
        @trigger 'nav-next', model
    prev: =>
        @currentModel or= @first()
        @moveTo (@modelRelativeTo @currentModel, -1) or @last()
    next: =>
        @currentModel or= @first()
        @moveTo (@modelRelativeTo @currentModel, 1) or @first()
