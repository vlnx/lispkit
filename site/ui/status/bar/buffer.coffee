class Buffer extends Backbone.Model
    defaults:
        content: ''

class View extends Backbone.View
    tagName: 'span'
    className: 'buffer'
    render: =>
        $(@el).html @model.get 'content'
        return this
    model: new Buffer
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = View
