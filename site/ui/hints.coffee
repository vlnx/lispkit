class Overlay extends Backbone.View
    className: 'overlay'
    tagName: 'span'
    render: =>
        @toggleClassBasedOnAttribute 'selected'
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
    template: jade.compile '''
    span.dim #{completed}
    span #{rest}
    '''
    templateData: =>
        completed: @model.get 'incompleteHint'
        rest: @model.hintToEnterStill()
    render: =>
        @toggleClassBasedOnAttribute 'selected'
        $(@el).html @template @templateData()

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
        incompleteHint: ''
        selected: false
    hintToEnterStill: =>
        (@get 'hint').replace (new RegExp "^#{@get 'incompleteHint'}"), ''
    clear: =>
        @destroy()
        @viewLabel.remove()
        @viewOverlay.remove()
    hide: =>
        $(@viewLabel.el).hide()
        $(@viewOverlay.el).hide()
    show: =>
        $(@viewLabel.el).show()
        $(@viewOverlay.el).show()
    visible: =>
        (@viewLabel.el.style.display isnt 'none') and
            (@viewOverlay.el.style.display isnt 'none')

class Hints extends Backbone.Collection
    model: Hint
    visible: => @filter (model) -> model.visible()
    selected: => (@where selected: true)[0]
    setSelected: =>
        @selected()?.set 'selected', false
        @visible()[0].set 'selected', true
    filterHints: (str) =>
        @each (model) ->
            if S(model.get 'hint').startsWith str
                model.show()
                model.set 'incompleteHint', str
            else
                model.hide()
                model.set 'incompleteHint', ''
        shown = @visible()
        @setSelected()
        actOnHint @selected() if shown.length is 1
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
    hints.collection.setSelected()

actOnHint = (hint) ->
    Exported.sendClickToCurrentTab JSON.stringify hint.toJSON()
    Exported.sendPromptClose()

window.selectFirst = ->
    actOnHint hints.collection.selected()
