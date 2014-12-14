class StatusBar extends Backbone.View
    id: 'statusbar'
    uri:               new (require './uri.coffee')
    keymode:           new (require './keymode.coffee')
    buffer:            new (require './buffer.coffee')
    history:           new (require './history.coffee')
    tabIndicator:      new (require './tabs.coffee')
    scrollIndicator:   new (require './scroll.coffee')
    progressIndicator: new (require './progress.coffee')
    initialize: =>
        $(@el).append @uri.render().el
        $(@el).append @history.render().el
        $(@el).append @progressIndicator.render().el
        $(@el).append @buffer.render().el
        $(@el).append @keymode.render().el
        $(@el).append @tabIndicator.render().el
        $(@el).append @scrollIndicator.render().el

module.exports = StatusBar
