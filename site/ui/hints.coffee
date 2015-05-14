class Overlay extends Backbone.View
    className: 'overlay'
    tagName: 'span'
    render: =>
        $(@el).html ''
        $(@el).offset @model.get('x'), @model.get('y')
        $(@el).width @model.get 'width'
        $(@el).height @model.get 'height'
        return this
    initialize: ->
        @listenTo @model, 'change', @render
        @model.viewOverlay = this
        @render
    remove: =>
        $(@el).remove()

class Label extends Backbone.View
    className: 'label'
    tagName: 'span'
    render: =>
        $(@el).html @model.get 'hint'

        # Offset by -10px
        x = (@model.get 'x') - 10
        y = (@model.get 'y') - 10
        if x < 0 then x = 0
        if y < 0 then y = 0

        $(@el).offset x, y
        return this
    initialize: ->
        @listenTo @model, 'change', @render
        @model.viewLabel = this
        @render
    remove: =>
        $(@el).remove()

class Hint extends Backbone.Model
    defaults:
        x: 0
        y: 0
        overlayHeight: 0
        overlayWidth: 0
        hint: 'xx'
    clear: =>
        @destroy()
        @viewLabel.remove()
        @viewOverlay.remove()

class Hints extends Backbone.Collection
    model: Hint
    clear: => @remove @models

numberToHint = (n, total) ->
    S("#{n}").padLeft("#{total}".length, '0')
        .replaceAll('0', 'a')
        .replaceAll('1', 'o')
        .replaceAll('2', 'e')
        .replaceAll('3', 'u')
        .replaceAll('4', 'i')
        .replaceAll('5', 'd')
        .replaceAll('6', 'h')
        .replaceAll('7', 't')
        .replaceAll('8', 'n')
        .replaceAll('9', 's').s

class HintsView extends Backbone.View
    el: $('#hints')[0]
    collection: new Hints
    initialize: =>
        @listenTo @collection, 'add', @addOne
        @listenTo @collection, 'remove', (model, collection, options) =>
            model.clear()
    addOne: (model) =>
        model.set 'hint',
            numberToHint (@collection.indexOf model),
                @collection.length
        $(@el).append (new Label model: model).render().el
        $(@el).append (new Overlay model: model).render().el

window.hints = new HintsView()

window.processData = (jsonStr) ->
    console.log jsonStr
    data = JSON.parse jsonStr
    hints.collection.set _.map data.elements, (h) ->
        x: h.x
        y: h.y
        height: h.height
        width: h.width
        scrollX: data.scrollX
        scrollY: data.scrollY
        winHeight: data.winHeight
        winWidth: data.winWidth


window.filterHints = (str) ->
    hints.collection.each (model) ->
        if S(model.get 'hint').startsWith str
            $(model.viewLabel.el).show()
            $(model.viewOverlay.el).show()
        else
            $(model.viewLabel.el).hide()
            $(model.viewOverlay.el).hide()
