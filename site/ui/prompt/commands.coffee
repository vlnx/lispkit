searchEngines =
    ddg: "https://duckduckgo.com/?q=%s"

openInterpret = (arg) ->
    # Check blanks
    arg = S(arg).trim().s
    if S(arg).isEmpty() then return ''
    # Split off first word
    args = arg.split ' '
    searchArg = S(arg).replaceAll(args[0],'').s
    # If search engines of first word exists
    if searchEngines[args[0]]?
        # Replace the %s with the rest of the commands's argument
        return S(searchEngines[args[0]]).replaceAll('%s',encodeURIComponent(searchArg)).s
    else if not S(args[0]).startsWith('http://')
        return "http://#{args[0]}"
    return arg # must be uri

Commands =
    open: (arg) ->
        console.log arg
        Exported.loadUri openInterpret arg
    tabopen: (arg) ->
        console.log arg
        Exported.statusBarNewTab openInterpret arg
    notify: (str) ->
        Exported.notify "#{if str then str else 'empty notify call'}"
    download: (str) ->
        # TODO: verify as a uri
        Exported.download str
        Commands.notify "Added to the download queue: #{str}"

module.exports = Commands
