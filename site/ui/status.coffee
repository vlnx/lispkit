# window.statusbar = require './status/bar'
# window.prompt = require './status/prompt'

class Uri extends Backbone.Model
    defaults: uri: 'Give URI'
class UriView extends Backbone.View
    tagName: 'span'
    id: 'uri'
    render: =>
        # http://nodejs.org/api/all.html#all_url
        $(@el).html @model.get 'uri'
        return this
    model: new Uri
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

class Keymode extends Backbone.Model
    defaults: mode: 'top'
class KeymodeView extends Backbone.View
    tagName: 'span'
    id: 'keymode'
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

class History extends Backbone.Model
    defaults:
        backward: no
        forward: no
class HistoryView extends Backbone.View
    tagName: 'span'
    id: 'history'
    render: =>
        $(@el).html (if @model.get 'forward' and @model.get 'backward'
            '[+-]'
        else if @model.get 'forward'
            '[-]'
        else if @model.get 'backward'
            '[+]'
        else
            '')
        return this
    model: new History
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

class StatusBar extends Backbone.View
    id: 'statusbar'
    uri: new UriView
    keymode: new KeymodeView
    history: new HistoryView
    initialize: =>
        $(@el).append @uri.render().el
        $(@el).append @history.render().el
        $(@el).append @keymode.render().el


Commands =
    open: (buffer) ->
        console.log buffer
        Exported.loadUri buffer
class Input extends Backbone.Model
    defaults:
        content: ''
        position: 0
class InputView extends Backbone.View
    tagName: 'span'
    id: 'input'
    model: new Input
    render: =>
        $(@el).html @model.get 'content'
        $(@el).append $('<span>').attr('id','cursor').html('&nbsp')
        return this
    addStr: (str) =>
        @model.set 'content', "#{@model.get 'content'}#{str}"
    moveCursor: (key) -> alert key
    backspace: =>
        content = @model.get 'content'
        if content.length is 1 then bar.prompt.close() # trigger shouldClose
        @model.set 'content', content.substring(0, content.length-1)
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

class Prompt extends Backbone.View
    id: 'prompt'
    input: new InputView
    open: (startingInput) =>
        @input.model.set 'content', ":#{startingInput or ''}"
        $(@el).show()
        Exported.statusbarRequestHeight 32
    close: =>
        Exported.promptClose()
        Exported.statusbarRequestHeight 16
        @input.model.set 'content', ''
        $(@el).hide()
    initialize: =>
        $(@el).append @input.el
        $(@el).hide()
    sendKey: (keystr) =>
        console.log "Prompt Key: #{keystr}"
        switch keystr
            when 'SPC' then @input.addStr ' '
            when 'S-:' then @input.addStr ':' # ;?
            when 'Left', 'Right', 'Up', 'Down'
                @input.moveCursor keystr
            when 'BS' then @input.backspace()
            when 'RET'
                regex = /^:(\w+)\s?(.*)$/
                matches = regex.exec @input.model.get 'content'
                cmd = matches[1]; arg = matches[2]
                # TODO:close prompt,  put in to history, localStorage or text file
                if Commands[cmd]?
                    Commands[cmd] arg
                else
                    notify "Command '#{cmd}' does not exist, called with '#{arg}'"
            else
                if keystr.length is 1
                    @input.addStr keystr

class AppView extends Backbone.View
    el: $('body')[0]
    status: new StatusBar
    prompt: new Prompt
    initialize: =>
        $(@el).append @status.el
        $(@el).append @prompt.el
window.bar = new AppView()

notify = (str) -> bar.prompt.input.model.set 'content', str
# promptHistory = $.cache('promptHistory').set('156645',':open last.fm')
