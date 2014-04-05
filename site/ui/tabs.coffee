tabs = window

# # alert 'onload'
# console.log 'Hello. initializing tabbar js'

# tabs.updateUri = (str) ->
#     document.getElementById('uri').innerHTML = str

# Test if the keys got through, sometimes wanted other times, sec leak
document.onkeydown = (e) -> console.log "keydown #{e.keyCode}"
document.onkeyup = (e) -> console.log "keyup #{e.keyCode}"


# luakit/lib/html_tablist.lua
# $('#tablist')
