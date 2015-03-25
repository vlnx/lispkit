window.Commands = require './commands.coffee'

class PromptHistoryItem extends Backbone.Model
    defaults:
        time: 0
        value: ''
# Well the luakit prompt history isn't persistent so leave that for now
# use Backbone.LocalStorage and @collection.sync on 'add'
class PromptHistory extends Backbone.Collection
    model: PromptHistoryItem
    incompleteLine: null
    currentLine: null
    setCurrentLine: (line) ->
        if (@currentLine.get 'content').length > line.length
            @currentLine.set 'position', line.length
        @currentLine.set 'content', line
    lineInCollection: (line) -> @findWhere value: line
    lineRelativeTo: (model, n) -> @at ((@indexOf model) + n)
    prev: -> @setCurrentLine @changeLine -1
    next: -> @setCurrentLine @changeLine 1
    # Fun history logic, probably won't scale to persistent duplicate entries
    changeLine: (movementInList) ->
        prev = false
        next = false
        if movementInList >= 1 then next = true
        else prev = true
        ret = @currentLine.get 'content'
        historyEntry = @lineInCollection ret
        if historyEntry
            relativeExists = @lineRelativeTo historyEntry, movementInList
            if relativeExists
                ret = relativeExists.get 'value'
            else
                if next
                    ret = @incompleteLine
        else
            if prev
                @incompleteLine = ret
                ret = (@last()).get 'value' if not @isEmpty()
        return ret

class Prompt extends Backbone.View
    id: 'prompt'
    input: new (require './input.coffee')
    history: new PromptHistory
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
