window.Commands = require './commands.coffee'

class Prompt extends Backbone.View
    id: 'prompt'
    input: new (require './input.coffee')
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
    evaluateContent: =>
        matches = /^(\w+)\s?(.*)$/.exec @input.model.get 'content'
        if Commands[matches[1]]?
            cmd = matches[1]
            arg = matches[2]
        else
            cmd = 'notify'
            arg = 'command not registered'
        console.log "evaluate prompt content: #{cmd} and #{arg}"
        Commands[cmd] arg

module.exports = Prompt

# TODO:close prompt,  put in to history, localStorage or text file
# latest
# JSON.parse('{"123":":opene","456":":tabopen n"}')[_.max(_.keys(JSON.parse('{"123":":opene","456":":tabopen n"}')))]
