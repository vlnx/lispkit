tabs = window

# alert 'onload'
console.log 'Hello. initializing ui side display, need input to display'

tabs.updateUri = (str) ->
    document.getElementById('uri').innerHTML = str