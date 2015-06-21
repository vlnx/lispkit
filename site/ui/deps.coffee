window.jade = require 'jade/lib/index.js'

window.S = require 'string'
window._ = require 'underscore'

window.$ = require 'ender-js'

# bonzo shim that attaches to $
window.ender = $
require 'bonzo/src/ender.js'

# Qwery shim
# From qwery/src/ender.js modifed to rely on bonzo
window.qwery = require 'qwery'
$.pseudos = qwery.pseudos
$._select = (s, r) ->
    if /^\s*</.test(s)
        $.create(s, r)
    else
        qwery(s, r)

window.Backbone = require 'backbone'
Backbone.$ = $ # Give backbone ender

window.Deps =
    url: require 'url'

Backbone.Collection::modelRelativeTo = (model, n) ->
    @at ((@indexOf model) + n)

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

Backbone.View::toggleClassBasedOnAttribute = (attribute) ->
    if (@model.get attribute)
        $(@el).addClass attribute
    else if ($(@el).hasClass attribute)
        $(@el).removeClass attribute
