searchEngines =
    ddg: 'https://duckduckgo.com/?q=%s'

encodeSearchEngine = (engine, phrase) ->
    S(searchEngines[engine])
        .replaceAll('%s', encodeURIComponent(phrase))
        .s

openInterpret = (args) ->
    if _.isEmpty args
        'ui://blank'
    else if S(args[0]).contains('://')
        args[0] # must be uri
    else
        unless searchEngines[args[0]]?
            args = ['ddg'].concat args
        encodeSearchEngine args[0], (args.slice 1).join ' '

Commands =
    open: (args) ->
        Exported.loadUri openInterpret args

    tabopen: (args) ->
        Exported.statusBarNewTab openInterpret args

    notify: (args) ->
        str = (args or []).join ' '
        if str is '' then str = 'empty notify call'
        Exported.notify str

    download: (args) ->
        # TODO: verify as a uri
        Exported.download str
        Commands.notify "Added to the download queue: #{str}"

    quit: Exported.quit

module.exports = Commands
