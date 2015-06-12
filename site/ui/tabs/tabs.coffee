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
    templateData: =>
        # Display the order as starting at 1
        m = @model.toJSON()
        m.order = m.order + 1
        m
    render: =>
        if @model.get 'current'
            $(@el).addClass 'current'
        else
            $(@el).removeClass 'current'
        $(@el).html @template @templateData()
        return this
    initialize: ->
        @listenTo @model, 'change', @render
        @model.view = this

class TabBar extends Backbone.View
    el: $('#tablist')[0]
    collection: new Tabs
    hideBar: ->
        Exported.tabbarRequestHeight 0
    showBar: ->
        Exported.tabbarRequestHeight 16
    initialize: =>
        @hideBar() # Start off empty
        @listenTo @collection, 'add', (model) =>
            $(@el).append (new TabView model: model).render().el
            # Added more than one so show bar
            @showBar() if @collection.models.length is 2
        @listenTo @collection, 'remove', (model, collection, options) =>
            model.destroy()
            # last one left
            @hideBar() if @collection.models.length is 1

    #     @collection.on 'sort', =>
    #         @render
    # bruteForceViewReorder: => # REFACTOR:
    #     @collection.add @collection.remove @collection.models

window.tabbar = new TabBar()
Exported.tabsInit()

# T.remove Tabs.findOrder 0

# http://stackoverflow.com/questions/10639842/ordering-backbone-views-together-with-collection
