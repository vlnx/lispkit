class Tab extends Backbone.Model
    defaults:
        order: 0
        title: '(untitled)'
        current: no

    initialize: =>
        @listenTo this, 'destroy', => @view.remove()

class Tabs extends Backbone.Collection
    model: Tab

    comparator: 'order'

    findOrder: (order) ->
        @where(order: order)[0]

    setCurrent: (index) =>
        (@where current: yes)[0]?.set 'current', false
        (@findOrder index)?.set 'current', yes

class TabView extends Backbone.View
    className: 'tab'

    template: jade.compile '''
    span.num #{order}
    span.title #{title}
    '''

    templateData: =>
        m = @model.toJSON()
        # order is zero-based
        m.order = m.order + 1
        m

    render: =>
        @toggleClassBasedOnAttribute 'current'
        $(@el).html @template @templateData()
        return this

    initialize: ->
        @listenTo @model, 'change', @render
        @model.view = this

class TabBar extends Backbone.View
    el: $('#tablist')[0]

    collection: new Tabs

    shouldShow: =>
        switch @collection.length
            when 1
                Exported.tabbarRequestHeight 0
            when 2
                Exported.tabbarRequestHeight 16

    initialize: =>
        @shouldShow()
        @listenTo @collection, 'add', @shouldShow
        @listenTo @collection, 'remove', @shouldShow
        @listenTo @collection, 'add', (model) =>
            $(@el).append (new TabView model: model).render().el
        @listenTo @collection, 'remove', (model) ->
            model.destroy()

window.tabbar = new TabBar

Exported.tabsInit()
