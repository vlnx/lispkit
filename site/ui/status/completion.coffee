{MenuItem, MenuItemView, MenuItems, Menu} = require './menu.coffee'

class CompletionItem extends MenuItem
class CompletionItemView extends MenuItemView
class CompletionItems extends MenuItems
    model: CompletionItem
class Completion extends Menu
    collection: new CompletionItems
    itemView: CompletionItemView
    id: 'completion'
    initialize: (opts) =>
        {@prompt, @keymodeModel} = opts
        Menu::initialize.call this

        @listenTo @prompt.input, 'completionSelectLine', @selectLineForPrompt
        @listenTo @prompt.input.model, 'change:content', @render

    promptVisible: => @prompt.el.style.display isnt 'none'
    modeOptions: =>
        if S(@keymodeModel.get 'mode').contains 'command-input'
            Object.keys Commands
        else
            []
    list: =>
        _.filter @modeOptions(), (name) =>
            @promptVisible() and
            (S(name).startsWith @prompt.input.model.firstWord()) and
            (name isnt @prompt.input.model.firstWord())

    render: => @collection.set _.map @list(), (name) -> content: name

    selectLineForPrompt: =>
        line = @collection.currentModel.get 'content'
        @prompt.input.model.set 'content', "#{line} "
        @prompt.input.model.set 'position', (line.length + 1)

module.exports = Completion
