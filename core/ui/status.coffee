window.statusbar = {}

# start hidden
$('#prompt').hide()

console.log 'Hello. initializing statusbar js, need input to display'
console.log A

statusbar.updateUri = (str) ->
    $('#uri').text str

statusbar.passthrough = (bool) ->
    str = if bool then 'Passthrough Mode' else ''
    $('#passthroughWarning').text str

window.prompt = {}
prompt.open = (startingInput) ->
    $('#prompt').show()
    i = if startingInput then ":#{startingInput} " else ':'
    $('#input').text i
prompt.close = ->
    Exported.promptClose()
    $('#prompt').hide()
    $('#input').text ''

prompt.insert = (str) ->
    $('#input').text $('#input').text() + str
prompt.moveCursor = (key) ->
    alert key

prompt.del = ->
    content = $('#input').text()
    if content.length is 1 then prompt.close()
    $('#input').text content.substring(0, content.length-1)

notify = (str) -> $('#input').text str

Commands =
    open: (buffer) ->
        console.log buffer
        Exported.loadUri buffer

prompt.sendKey = (key) =>
    console.log key
    switch key
        when 'SPC' then prompt.insert ' '
        when 'S-:' then prompt.insert ':'
        when 'Left', 'Right', 'Up', 'Down'
            prompt.moveCursor key
        when 'DEL' then prompt.del()
        when 'RET'
            regex = /^:(\w+)\s?(.*)$/
            matches = regex.exec $('#input').text()
            cmd = matches[1]
            arg = matches[2]

            # put in to history, localStorage or text file

            if Commands[cmd]?
                Commands[cmd] arg 
            else
                notify "Command '#{cmd}' does not exist, called with '#{arg}'"

            # alert "Write code to register function, so defcommands can be called with #{$('#input').text()}"
        else
            if key.length is 1
                prompt.insert key
