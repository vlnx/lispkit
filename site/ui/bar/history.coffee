class History extends Backbone.Model
    defaults:
        backward: no
        forward: no

class HistoryView extends Backbone.View
    tagName: 'span'
    className: 'history'
    logic: =>
        if @model.get('forward') and @model.get 'backward'
            '[+-]'
        else if @model.get 'forward'
            '[-]'
        else if @model.get 'backward'
            '[+]'
        else
            ''
    render: =>
        $(@el).html @logic()
        return this
    model: new History
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = HistoryView
