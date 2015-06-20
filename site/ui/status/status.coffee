class AppView extends Backbone.View
    el: $('body')[0]

    # TODO: rename to indicators
    status: new (require './bar/main.coffee')

    prompt: new (require './prompt/main.coffee')

    adjustHeight: =>
        Exported.statusbarRequestHeight $(@el).height()

    initialize: ->
        @completion = new (require './completion.coffee')
            prompt: @prompt
            keymodeModel: @status.keymode.model

        @listenTo @prompt, 'adjustHeight', @adjustHeight
        @listenTo @completion, 'adjustHeight', @adjustHeight

        # Layout order
        $(@el).append @status.el
        $(@el).append @completion.el
        $(@el).append @prompt.el

        # If follow mode is active, pass input to filterHints
        @listenTo @prompt.input.model, 'change:content', =>
            if S(@status.keymode.model.get 'mode').contains 'follow'
                Exported.filterHints "#{@prompt.input.model.get 'content'}"

window.bar = new AppView()

Exported.statusbarInit()
