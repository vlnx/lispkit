class Input extends Backbone.Model
    defaults:
        content: ''
        position: 0

class InputView extends Backbone.View
    tagName: 'span'
    id: 'input'
    model: new Input
    render: =>
        $(@el).html @model.get 'content'
        $(@el).append $('<span>').attr('id','cursor').html('&nbsp')
        return this
    addStr: (str) =>
        @model.set 'content', "#{@model.get 'content'}#{str}"
    moveCursor: (key) -> alert key
    backspace: =>
        content = @model.get 'content'
        if content.length is 1
            @trigger 'shouldClosePrompt'
        @model.set 'content', content.substring(0, content.length-1)
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = InputView
