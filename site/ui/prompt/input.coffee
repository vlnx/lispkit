class Input extends Backbone.Model
    defaults:
        content: ''
        position: 0

    setPosition: (value) =>
        value = Number value

        min = 0
        max = (@get 'content').length

        if value < min then value = min
        if value > max then value = max

        @set 'position', value

    # Retrieve segments of `content`, relative to `position`
    before: (position) =>
        unless (_.isNumber position) then position = @get 'position'
        (@get 'content').substring 0, position

    activeChar: (position) =>
        unless (_.isNumber position) then position = @get 'position'
        ((@get 'content').charAt position) or ' '

    after: (position) =>
        unless (_.isNumber position) then position = @get 'position'
        (@get 'content').substring (position + 1), @get('content').length

    afterInclusive: (position) =>
        unless (_.isNumber position) then position = @get 'position'
        (@get 'content').substr position

class InputView extends Backbone.View
    tagName: 'span'
    id: 'input'
    model: new Input

    template: jade.compile '''
    span#promptChar :
    span #{before}
    span#cursor #{over}
    span #{rest}
    '''
    render: =>
        $(@el).html @template
            before: @model.before()
            over: @model.activeChar()
            rest: @model.after()
        return this

    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove
    history: (direction) -> alert direction

    clearLine: =>
        @model.set 'content', ''
        @model.set 'position', 0

    # Insert `str` at `position`, set position to after inserted string
    insert: (str) =>
        @model.set 'content', "#{@model.before()}#{str}#{@model.afterInclusive()}"
        @model.set 'position', "#{@model.before()}#{str}".length

    moveCursor: (distance) ->
        @model.setPosition ((@model.get 'position') + (Number distance))

    # Backspace from `position`
    backspace: =>
        if (@model.get 'content').length is 0
            @trigger 'shouldClosePrompt'
        else
            pos = @model.get 'position'
            @model.set 'content', "#{@model.before (pos-1)}#{@model.afterInclusive()}"
            @model.set 'position', "#{@model.before (pos-1)}".length

module.exports = InputView
