class ProgressIndicator extends Backbone.Model
    defaults:
        progress: 0

class ProgressIndicatorView extends Backbone.View
    model: new ProgressIndicator

    tagName: 'span'

    className: 'progressIndicator'

    logic: =>
        if (@model.get 'progress') is '100'
            ''
        else
            "(#{@model.get 'progress'})%"

    render: =>
        $(@el).html @logic()
        return this

    initialize: ->
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = ProgressIndicatorView
