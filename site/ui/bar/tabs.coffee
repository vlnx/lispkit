class TabIndicator extends Backbone.Model
    defaults:
        current: 0
        total: 0

class TabIndicatorView extends Backbone.View
    tagName: 'span'
    className: 'tabIndicator'
    render: =>
        # Model order zero based index, display as starting at 1
        $(@el).html "[#{@model.get('current') + 1}/#{@model.get('total') + 1}]"
        return this
    model: new TabIndicator
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = TabIndicatorView
