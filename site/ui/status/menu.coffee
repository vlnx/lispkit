class MenuItem extends Backbone.Model
    defaults:
        content: ''
    clear: =>
        @destroy()
        @view.remove()

class MenuItemView extends Backbone.View
    initialize: =>
        @listenTo @model, 'change', @render
        @model.view = this
        @render
    remove: =>
        $(@el).remove()
    template: jade.compile '''
    span #{content}
    '''
    getRenderData: =>
        content: @model.get 'content'
    render: =>
        $(@el).html @template @getRenderData()
        return this

class MenuItems extends Backbone.CollectionNav
    model: MenuItem

class Menu extends Backbone.View
    collection: new MenuItems
    itemView: MenuItemView
    attributes: => return class: 'menu'
    initialize: =>
        @listenTo @collection, 'nav-prev', (model) ->
            model.view.el.id = null
        @listenTo @collection, 'nav-next', (model) ->
            model.view.el.id = 'current'
        @listenTo @collection, 'add', @addOne
        @listenTo @collection, 'add', (m) =>
            if @collection.length is 1
                @collection.moveTo m
        @listenTo @collection, 'remove', (model, collection, options) =>
            model.clear()
            @trigger 'adjustHeight'
        @listenTo @collection, 'reset', @addAll
    addOne: (model) =>
        $(@el).append (new @itemView model: model).render().el
        @trigger 'adjustHeight'
    addAll: =>
        @collection.each @addOne

module.exports =
    MenuItem: MenuItem
    MenuItemView: MenuItemView
    MenuItems: MenuItems
    Menu: Menu
