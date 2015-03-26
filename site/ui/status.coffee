class AppView extends Backbone.View
    el: $('body')[0]
    status: new (require './bar/main.coffee')
    prompt: new (require './prompt/main.coffee')
    adjustHeight: =>
        Exported.statusbarRequestHeight $(@el).height()
    initialize: =>
        @listenTo @prompt, 'adjustHeight', @adjustHeight
        $(@el).append @status.el
        $(@el).append @prompt.el

window.bar = new AppView()

Exported.statusbarInit()
