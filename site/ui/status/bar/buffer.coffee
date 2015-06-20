class Buffer extends Backbone.Model
    defaults:
        content: ''

class View extends Backbone.View
    model: new Buffer

    tagName: 'span'

    className: 'buffer'

    render: =>
        $(@el).html @model.get 'content'
        return this

    initialize: ->
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = View
