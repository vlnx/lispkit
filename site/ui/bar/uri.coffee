class Uri extends Backbone.Model
    defaults: uri: 'Give URI'

class UriView extends Backbone.View
    tagName: 'span'
    className: 'uri'
    render: =>
        # Do fun stuff here
        # http://nodejs.org/api/all.html#all_url
        $(@el).html @model.get 'uri'
        return this
    model: new Uri
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = UriView
