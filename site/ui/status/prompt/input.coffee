class Input extends Backbone.Model
    defaults:
        content: ''
        position: 0
        promptPhrase: ':'

    limits: (value) =>
        value = Number value
        min = 0
        max = (@get 'content').length
        if value < min then value = min
        if value > max then value = max
        return value

    # Retrieve segments of `content`, relative to `position`
    before: (position) =>
        unless _.isNumber position
            position = @get 'position'
        (@get 'content').substring 0, position

    activeChar: (position) =>
        unless _.isNumber position
            position = @get 'position'
        ((@get 'content').charAt position) or ' '

    after: (position) =>
        unless _.isNumber position
            position = @get 'position'
        (@get 'content').substring (position + 1),
            @get('content').length

    afterInclusive: (position) =>
        unless _.isNumber position
            position = @get 'position'
        (@get 'content').substr position

    # utils
    split: => (S(@get 'content').trim().s).split ' '

    firstWord: => @split()[0]

    afterFirstWord: => @split().slice 1

    length: => (@get 'content').length

    # may have only inserted '', the same as default
    changeIntended: =>
        if @length() is 0
            @trigger 'change:content'

class InputView extends Backbone.View
    tagName: 'span'

    id: 'input'

    model: new Input

    template: jade.compile '''
    span#promptPhrase #{phrase}
    span #{before}
    span#cursor #{over}
    span #{rest}
    '''

    render: =>
        $(@el).html @template
            before: @model.before()
            over: @model.activeChar()
            rest: @model.after()
            phrase: @model.get 'promptPhrase'
        return this

    initialize: ->
        @render()
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

    clearLine: =>
        @model.changeIntended()
        @model.set @model.defaults

    # Insert `str` at `position`, set position to after inserted string
    insert: (str) =>
        if (str is ' ') and (@model.firstWord() is @model.get 'content')
            @trigger 'completionSelectLine'
        else
            @model.set 'content',
                "#{@model.before()}#{str}#{@model.afterInclusive()}"
            @model.set 'position', "#{@model.before()}#{str}".length
        @model.changeIntended()

    moveCursor: (distance) ->
        @model.set 'position',
            @model.limits ((@model.get 'position') + (Number distance))

    # Backspace from `position`
    backspace: =>
        if @model.length() is 0
            @trigger 'close'
        else
            pos = @model.get 'position'
            @model.set 'content',
                "#{@model.before (pos-1)}#{@model.afterInclusive()}"
            @model.set 'position', "#{@model.before (pos-1)}".length

    delete: =>
        @model.set 'content', "#{@model.before()}#{@model.after()}"

    startOfLine: =>
        @model.set 'position', 0

    endOfLine: =>
        @model.set 'position', (@model.get 'content').length

    openTabToggle: =>
        line = @model.get 'content'
        pos = @model.get 'position'
        if S(line).startsWith 'open'
            line = "tab#{line}"
        else if S(line).startsWith 'tabopen'
            line = line.replace /^tabopen/, 'open'
        pos = (pos + (line.length - (@model.get 'content').length))
        @model.set 'content', line
        @model.set 'position', pos

module.exports = InputView
