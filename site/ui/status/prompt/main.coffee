window.Commands = require './commands.coffee'

class Prompt extends Backbone.View
    id: 'prompt'
    input: new (require './input.coffee')
    history: new (require './history.coffee')
    open: (startingInput, promptPhrase) =>
        $(@el).show()
        if promptPhrase isnt ''
            @input.model.set 'promptPhrase', promptPhrase
        @input.insert "#{startingInput or ''}"
        @trigger 'adjustHeight'
    close: =>
        Exported.promptClose()
        $(@el).hide()
        @input.clearLine()
        @trigger 'adjustHeight'
    initialize: =>
        @listenTo @input, 'close', @close
        $(@el).append @input.el
        $(@el).hide()
        @history.currentLine = @input.model
    evaluateContent: =>
        line = @input.model.get 'content'
        @history.add value: line, time: ((new Date).getTime())

        cmd = @input.model.firstWord()
        arg = @input.model.afterFirstWord()
        unless Commands[cmd]?
            cmd = 'notify'
            arg = 'command not registered'
        console.log "evaluate prompt content: #{cmd} and #{arg}"
        Commands[cmd] arg

module.exports = Prompt
