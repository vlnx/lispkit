# Test if the keys got through, sometimes wanted other times, sec leak
document.onkeydown = (e) -> console.log "keydown #{e.keyCode}"
document.onkeyup = (e) -> console.log "keyup #{e.keyCode}"


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
    findOrder: (order) -> # return tab model with the given order
        @where(order: order)[0]
    moveCurrentTo: (order) => # Move current to new index
        prev = (@where current: yes)[0]
        if prev then prev.set 'current', false
        (@findOrder order).set 'current', yes

class TabView extends Backbone.View
    className: 'tab'
    template: jade.compile '''
    span.num #{order}
    span.title #{title}
    '''
    render: =>
        $(@el).addClass 'current' if @model.get 'current'
        $(@el).removeClass 'current' if not @model.get 'current'
        $(@el).html @template @model.toJSON()
        return this
    initialize: ->
        @listenTo @model, 'change', @render
        @model.view = this

class TabBar extends Backbone.View
    el: $('#tablist')[0]
    collection: new Tabs
    initialize: =>
        @listenTo @collection, 'add', (model) =>
            console.log @collection.indexOf model
            $(@el).append (new TabView model: model).render().el
        @listenTo @collection, 'remove', (model, collection, options) =>
            model.destroy()
    #     @collection.on 'sort', =>
    #         @render
    # bruteForceViewReorder: => # REFACTOR:
    #     @collection.add @collection.remove @collection.models

window.tabbar = new TabBar()

Exported.tabsInit()

# T = tabbar.collection
# T.add title: 'TabOne', order: 1, current: yes
# T.add title: 'TabTwo', order: 2

# T.remove Tabs.findOrder 0

# http://stackoverflow.com/questions/10639842/ordering-backbone-views-together-with-collection
