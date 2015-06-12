class Uri extends Backbone.Model
    defaults:
        uri: 'Give URI'
        hover: ''

class UriView extends Backbone.View
    tagName: 'span'
    className: 'uri'
    templateContent: =>
        if @model.get('hover') isnt ''
            "Link: #{@model.get 'hover'}"
        else
            # Do fun stuff here
            # http://nodejs.org/api/all.html#all_url
            @model.get 'uri'
    render: =>
        $(@el).html @templateContent()
        return this
    model: new Uri
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = UriView
