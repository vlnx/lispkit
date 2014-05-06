# window.statusbar = require './status/bar'
# window.prompt = require './status/prompt'

class Uri extends Backbone.Model
    defaults: uri: 'Give URI'
class UriView extends Backbone.View
    tagName: 'span'
    className: 'uri'
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

class History extends Backbone.Model
    defaults:
        backward: no
        forward: no
class HistoryView extends Backbone.View
    tagName: 'span'
    className: 'history'
    logic: =>
        if @model.get('forward') and @model.get 'backward'
            '[+-]'
        else if @model.get 'forward'
            '[-]'
        else if @model.get 'backward'
            '[+]'
        else
            ''
    render: =>
        $(@el).html @logic()
        return this
    model: new History
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

class TabIndicator extends Backbone.Model
    defaults:
        current: 0
        total: 0
class TabIndicatorView extends Backbone.View
    tagName: 'span'
    className: 'tabIndicator'
    render: =>
        $(@el).html "[#{@model.get 'current'}/#{@model.get 'total'}]"
        return this
    model: new TabIndicator
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

class ScrollIndicator extends Backbone.Model
    defaults:
        y: 0
        ymax: 0
        x: 0
        xmax: 0
class ScrollIndicatorView extends Backbone.View
    tagName: 'span'
    className: 'scrollIndicator'
    logic: =>
        if Number(@model.get 'ymax') is 0
            'All'
        else if Number(@model.get 'y') is 0
            'Top'
        else if Number(@model.get 'y') is Number(@model.get 'ymax')
            'Bot'
        else
            "#{Math.floor (@model.get('y') / @model.get('ymax')) * 100}%"
    render: =>
        $(@el).html @logic()
        return this
    model: new ScrollIndicator
    initialize: =>
        @listenTo @model, 'change', @render
        @listenTo @model, 'destroy', @remove

class ProgressIndicator extends Backbone.Model
    defaults: progress: 0
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


class StatusBar extends Backbone.View
    id: 'statusbar'
    uri: new UriView
    keymode: new KeymodeView
    history: new HistoryView
    tabIndicator: new TabIndicatorView
    scrollIndicator: new ScrollIndicatorView
    progressIndicator: new ProgressIndicatorView
    initialize: =>
        $(@el).append @uri.render().el
        $(@el).append @history.render().el
        $(@el).append @progressIndicator.render().el
        $(@el).append @keymode.render().el
        $(@el).append @tabIndicator.render().el
        $(@el).append @scrollIndicator.render().el


searchEngines =
    ddg: "https://duckduckgo.com/?q=%s"
openInterpret = (arg) ->
    # Check blanks
    if S(arg).isEmpty() then return 'about:blank'
    if arg is 'about:blank' then return arg
    # Split off first word
    args = arg.split ' '
    searchArg = S(arg).replaceAll(args[0],'').s
    # If search engines of first word exists
    if searchEngines[args[0]]?
        # Replace the %s with the rest of the commands's argument
        return S(searchEngines[args[0]]).replaceAll('%s',encodeURIComponent(searchArg)).s
    return arg
Commands =
    open: (arg) ->
        console.log arg
        Exported.loadUri openInterpret arg
    tabopen: (arg) ->
        console.log arg
        Exported.statusBarNewTab openInterpret arg
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
        if content.length is 1
            @trigger 'shouldClosePrompt'
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
        @listenTo @input, 'shouldClosePrompt', @close
        $(@el).append @input.el
        $(@el).hide()
    sendKey: (keystr) =>
        console.log "Prompt Key: #{keystr}"
        switch keystr
            when 'SPC' then @input.addStr ' '
            when 'Left', 'Right', 'Up', 'Down'
                @input.moveCursor keystr
            when 'BS', 'C-h' then @input.backspace()
            when 'RET'
                regex = /^:(\w+)\s?(.*)$/
                matches = regex.exec @input.model.get 'content'
                cmd = matches[1]; arg = matches[2]
                # TODO:close prompt,  put in to history, localStorage or text file
    # latest
    # JSON.parse('{"123":":opene","456":":tabopen n"}')[_.max(_.keys(JSON.parse('{"123":":opene","456":":tabopen n"}')))]
                if Commands[cmd]?
                    Commands[cmd] arg, (err) ->
                        if err then throw err
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

# TODO: request information to initialize

notify = (str) -> bar.prompt.input.model.set 'content', str
# promptHistory = $.cache('promptHistory').set('156645',':open last.fm')