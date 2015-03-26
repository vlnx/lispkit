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

module.exports = PromptHistory
