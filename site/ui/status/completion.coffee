{MenuItem, MenuItemView, MenuItems, Menu} = require './menu.coffee'

class CompletionItem extends MenuItem

class CompletionItemView extends MenuItemView

class CompletionItems extends MenuItems
    model: CompletionItem

class Completion extends Menu
    collection: new CompletionItems

    itemView: CompletionItemView

    id: 'completion'

    initialize: (opts) ->
        {@prompt, @keymodeModel} = opts
        Menu::initialize.call this

        @listenTo @prompt.input,
            'completionSelectLine', @selectLineForPrompt
        @listenTo @prompt.input.model, 'change:content', @render

    promptVisible: => @prompt.el.style.display isnt 'none'

    # get a list of completion options for the current mode
    modeOptions: =>
        if S(@keymodeModel.get 'mode').contains 'command-input'
            Object.keys Commands
        else
            []

    list: =>
        # filter down the options
        _.filter @modeOptions(), (item) =>
            @promptVisible() and
            (S(item).startsWith @prompt.input.model.firstWord()) and
            (item isnt @prompt.input.model.firstWord())

    render: =>
        # map each item of the list to a model
        @collection.set _.map @list(), (item) -> content: item

    selectLineForPrompt: =>
        line = "#{@collection.currentModel.get 'content'} "
        @prompt.input.model.set 'content', line
        @prompt.input.model.set 'position', line.length

module.exports = Completion
