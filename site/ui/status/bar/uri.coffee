class Uri extends Backbone.Model
    defaults:
        uri: ''
        hover: ''

class UriView extends Backbone.View
    tagName: 'span'

    className: 'uri'

    model: new Uri

    # # TODO: Split and color the segments
    # # http://nodejs.org/api/all.html#all_url
    template: jade.compile '''
    if hover
        span Link:&nbsp
    span #{uri}
    '''

    getRenderData: =>
        hover: do =>
            if (@model.get 'hover') is ''
                false
            else
                true
        uri: do =>
            if (@model.get 'hover') is ''
                @model.get 'uri'
            else
                @model.get 'hover'

    render: =>
        $(@el).html @template @getRenderData()
        return this

    initialize: ->
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

module.exports = UriView
