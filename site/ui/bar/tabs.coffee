class TabIndicator extends Backbone.Model
    defaults:
        current: 0
        total: 0

class TabIndicatorView extends Backbone.View
    tagName: 'span'
    className: 'tabIndicator'
    render: =>
        $(@el).html "[#{@model.get 'current'}/#{@model.get 'total'}]"
        return this
    model: new TabIndicator
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = TabIndicatorView
