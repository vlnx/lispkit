window.Commands = require './commands.coffee'

class Prompt extends Backbone.View
    id: 'prompt'
    input: new (require './input.coffee')
    history: new (require './history.coffee')
    open: (startingInput) =>
        @input.insert "#{startingInput or ''}"
        $(@el).show()
        Exported.statusbarRequestHeight 32
    close: =>
        Exported.promptClose()
        Exported.statusbarRequestHeight 16
        @input.clearLine()
        $(@el).hide()
    initialize: =>
        @listenTo @input, 'shouldClosePrompt', @close
        $(@el).append @input.el
        $(@el).hide()
        @history.currentLine = @input.model
    evaluateContent: =>
        line = @input.model.get 'content'
        @history.add value: line, time: ((new Date).getTime())
        matches = /^(\w+)\s?(.*)$/.exec line
        if Commands[matches[1]]?
            cmd = matches[1]
            arg = matches[2]
        else
            cmd = 'notify'
            arg = 'command not registered'
        console.log "evaluate prompt content: #{cmd} and #{arg}"
        Commands[cmd] arg

module.exports = Prompt
