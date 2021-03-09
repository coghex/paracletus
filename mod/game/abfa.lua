-- abfa mod defines an example game
require "mod/base/window"
require "mod/base/base"
require "mod/base/pane"

-- this runs once at the beginning
function initMod ()
    -- basic UI elements can be in any order
    local menu1 = window:new ()
    menu1:initMenu("menu1")
    menu1:newText(-6.0,1.0,"A Bridge Far Away")
    menu1:newLink(-6.0,-1.0,"Create World",link("menu2"))
    menu1:newLink(-6.0,-3.0,"Exit",exit)

    local menu2 = window:new ()
    menu2:initMenu("menu2")
    local pane1 = pane:new ()
    pane1:addPaneBit (textBit ("World Parameters"))
    pane1:initPane(-7.0,4.0,"wparams")
    menu2:addPane(pane1)
    menu2:newLink(-6.0,-4.0,"Back",back)

    menu1:switchWindow ()
    return 0
end
function runMod ()
end
