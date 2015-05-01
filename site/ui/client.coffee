window._getHintData = ->
    data =
        elements: []
        scrollX: document.defaultView.scrollX
        scrollY: document.defaultView.scrollY
        winHeight: window.innerHeight
        winWidth: window.innerWidth

    inView = (e) ->
        vis = true
        if vis then vis = e.x < data.winWidth
        if vis then vis = e.y < data.winHeight
        if vis then vis = e.x > 0
        if vis then vis = e.y > 0
        return vis

    for element in (document.querySelectorAll 'a')
        sizes = element.getClientRects()[0]
        if sizes?
            elementData =
                x: sizes.left
                y: sizes.top
                height: sizes.height
                width: sizes.width
            if inView elementData
                data.elements.push elementData

    return JSON.stringify data

# href: element.href
# text: element.textContent

