window._getHintDataForSelectors = (selectorsName) ->

    selectors =
        clickable: 'a, area, textarea, select, input:not([type=hidden]), button'
        uri: 'a, area'

    elements = document.querySelectorAll selectors[selectorsName]

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

    for element in elements
        sizes = element.getClientRects()[0]
        if sizes?
            elementData =
                x: sizes.left
                y: sizes.top
                height: sizes.height
                width: sizes.width
            if selectorsName is 'uri'
                elementData.href = element.href
            if inView elementData
                data.elements.push elementData

    return JSON.stringify data

# text: element.textContent
