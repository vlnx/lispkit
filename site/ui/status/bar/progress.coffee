class ProgressIndicator extends Backbone.Model
    defaults:
        progress: 0

class ProgressIndicatorView extends Backbone.View
    tagName: 'span'
    className: 'progressIndicator'
    logic: =>
        if @model.get('progress') is '100'
            ''
        else
            "(#{@model.get 'progress'})%"
    render: =>
        $(@el).html @logic()
        return this
    model: new ProgressIndicator
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = ProgressIndicatorView
