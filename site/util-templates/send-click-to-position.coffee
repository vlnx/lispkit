x = Number '{{{x}}}'
y = Number '{{{y}}}'

event = document.createEvent 'MouseEvent'

event.initMouseEvent 'click', true, true, window, 0, 0, 0, 0, 0,
    false, false, false, false, 0, null

(document.elementFromPoint x, y).dispatchEvent event
