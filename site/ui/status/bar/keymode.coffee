class Keymode extends Backbone.Model
    defaults:
        mode: 'not-set'

class KeymodeView extends Backbone.View
    model: new Keymode

    tagName: 'span'

    className: 'keymode'

    render: =>
        $(@el).html do =>
            if @model.get('mode') is 'scroll, top'
                ''
            else
                "--#{@model.get 'mode'}--"
        return this

    initialize: ->
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = KeymodeView
