class StatusBar extends Backbone.View
    id: 'statusbar'
    uri: new (require './uri.coffee')
    history: new (require './history.coffee')
    progress: new (require './progress.coffee')
    buffer: new (require './buffer.coffee')
    keymode: new (require './keymode.coffee')
    tabs: new (require './tabs.coffee')
    scroll: new (require './scroll.coffee')
    initialize: ->
        $(@el).append @uri.render().el
        $(@el).append @history.render().el
        $(@el).append @progress.render().el
        $(@el).append @buffer.render().el
        $(@el).append @keymode.render().el
        $(@el).append @tabs.render().el
        $(@el).append @scroll.render().el

module.exports = StatusBar
