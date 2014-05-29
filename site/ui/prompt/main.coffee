window.Commands = require './commands.coffee'

class Prompt extends Backbone.View
    id: 'prompt'
    input: new (require './input.coffee')
    open: (startingInput) =>
        @input.model.set 'content', ":#{startingInput or ''}"
        $(@el).show()
        Exported.statusbarRequestHeight 32
    close: =>
        Exported.promptClose()
        Exported.statusbarRequestHeight 16
        @input.model.set 'content', ''
        $(@el).hide()
    initialize: =>
        @listenTo @input, 'shouldClosePrompt', @close
        $(@el).append @input.el
        $(@el).hide()
    sendKey: (keystr) =>
        console.log "Prompt Key: #{keystr}"
        switch keystr
            when 'SPC' then @input.addStr ' '
            when 'Left', 'Right', 'Up', 'Down'
                @input.moveCursor keystr
            when 'BS', 'C-h' then @input.backspace()
            when 'RET'
                regex = /^:(\w+)\s?(.*)$/
                matches = regex.exec @input.model.get 'content'
                cmd = matches[1]; arg = matches[2]
                # TODO:close prompt,  put in to history, localStorage or text file
    # latest
    # JSON.parse('{"123":":opene","456":":tabopen n"}')[_.max(_.keys(JSON.parse('{"123":":opene","456":":tabopen n"}')))]
                if Commands[cmd]?
                    Commands[cmd] arg, (err) ->
                        if err then throw err
                else
                    notify "Command '#{cmd}' does not exist, called with '#{arg}'"
            else
                if keystr.length is 1
                    @input.addStr keystr

module.exports = Prompt
