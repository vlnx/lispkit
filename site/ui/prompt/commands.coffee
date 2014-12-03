searchEngines =
    ddg: "https://duckduckgo.com/?q=%s"

openInterpret = (arg) ->
    # Check blanks
    if S(arg).isEmpty() then return ''
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

module.exports = Commands
