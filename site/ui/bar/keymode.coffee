class Keymode extends Backbone.Model
    defaults:
        mode: 'top'

class KeymodeView extends Backbone.View
    tagName: 'span'
    className: 'keymode'
    render: =>
        if @model.get('mode') is 'top'
            $(@el).html ''
        else
            $(@el).html "#{@model.get 'mode'} Mode"
        return this
    model: new Keymode
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = KeymodeView
