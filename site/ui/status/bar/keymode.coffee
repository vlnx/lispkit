class Keymode extends Backbone.Model
    defaults:
        mode: 'not-set'

class KeymodeView extends Backbone.View
    tagName: 'span'
    className: 'keymode'
    render: =>
        if @model.get('mode') is 'scroll, top'
            $(@el).html ''
        else
            $(@el).html "--#{@model.get 'mode'}--"
        return this
    model: new Keymode
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = KeymodeView
