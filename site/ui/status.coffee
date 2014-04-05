# window.statusbar = require './status/bar'
# window.prompt = require './status/prompt'

# start hidden
$('#prompt').hide()

notify = (str) -> $('#input').text str

# console.log 'Hello. initializing statusbar js, need input to display'
# console.log A

# statusbar.history forward: yes, backward: yes
# statusbar.history forward: no, backward: no

window.statusbar =

    # http://nodejs.org/api/all.html#all_url
    updateUri: (str) ->
        # url = Deps.url.parse str
        $('#uri').text str

    passthrough: (bool) ->
        $('#passthroughWarning').text (if bool then 'Passthrough Mode' else '')

    history: (opts) ->
        $('#history').text (
            if opts.forward and opts.backward
                '[+-]'
            else if opts.forward
                '[-]'
            else if opts.backward
                '[+]'
            else
                '')


Commands =
    open: (buffer) ->
        console.log buffer
        Exported.loadUri buffer


window.prompt =
    open: (startingInput) ->
        $('#prompt').show()
        i = if startingInput then ":#{startingInput}" else ':'
        $('#input').text i
    close: ->
        Exported.promptClose()
        $('#prompt').hide()
        $('#input').text ''
    insert: (str) ->
        $('#input').text $('#input').text() + str
    moveCursor: (key) ->
        alert key
    bs: ->
        content = $('#input').text()
        if content.length is 1 then prompt.close()
        $('#input').text content.substring(0, content.length-1)
    sendKey: (key) =>
        console.log "Prompt Key: #{key}"
        switch key
            when 'SPC' then prompt.insert ' '
            when 'S-:' then prompt.insert ':'
            when 'Left', 'Right', 'Up', 'Down'
                prompt.moveCursor key
            when 'BS' then prompt.bs()
            when 'RET'
                regex = /^:(\w+)\s?(.*)$/
                matches = regex.exec $('#input').text()
                cmd = matches[1]
                arg = matches[2]

                # TODO:close prompt,  put in to history, localStorage or text file

                if Commands[cmd]?
                    Commands[cmd] arg 
                else
                    notify "Command '#{cmd}' does not exist, called with '#{arg}'"

                # alert "Write code to register function, so defcommands can be called with #{$('#input').text()}"
            else
                if key.length is 1
                    prompt.insert key
