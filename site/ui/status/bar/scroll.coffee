class ScrollIndicator extends Backbone.Model
    defaults:
        y: 0
        ymax: 0
        x: 0
        xmax: 0

class ScrollIndicatorView extends Backbone.View
    tagName: 'span'
    className: 'scrollIndicator'
    horizontal: =>
        if Number(@model.get 'xmax') is 0
            'All'
        else if Number(@model.get 'x') is 0
            'LeftMost'
        else if Number(@model.get 'x') is Number(@model.get 'xmax')
            'RightMost'
        else
            "#{Math.floor (@model.get('x') / @model.get('xmax')) * 100}%"
    vertical: =>
        if Number(@model.get 'ymax') is 0
            'All'
        else if Number(@model.get 'y') is 0
            'Top'
        else if Number(@model.get 'y') is Number(@model.get 'ymax')
            'Bot'
        else
            "#{Math.floor (@model.get('y') / @model.get('ymax')) * 100}%"
    render: =>
        $(@el).html "[#{@horizontal()}/#{@vertical()}]"
        return this
    model: new ScrollIndicator
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = ScrollIndicatorView
