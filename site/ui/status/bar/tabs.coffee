class TabIndicator extends Backbone.Model
    defaults:
        current: 0
        total: 0

class TabIndicatorView extends Backbone.View
    model: new TabIndicator

    tagName: 'span'

    className: 'tabIndicator'

    template: jade.compile ' [#{current}/#{total}]'

    getRenderData: =>
        # model is zero-based
        current: (@model.get 'current') + 1
        total: (@model.get 'total') + 1

    render: =>
        $(@el).html @template @getRenderData()
        return this

    initialize: ->
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = TabIndicatorView
