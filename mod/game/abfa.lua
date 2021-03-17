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
    menu1:newLink(-6.0,-3.0,"Exit","exit")

    local menu2 = window:new ()
    menu2:initMenu("menu2")
    local pane1 = pane:new ()
    pane1:addPaneBit (textBit ("World Parameters"))
    pane1:addPaneBit (sliderBit ("nConts",1,1000,60))
    pane1:addPaneBit (sliderBit ("nSpots",1,1000,60))
    pane1:addPaneBit (sliderBit ("Seed",1,1000,999))
    pane1:addPaneBit (sliderBit ("Width",1,1000,10))
    pane1:addPaneBit (sliderBit ("Height",1,1000,8))
    pane1:initPane(-7.0,4.0,"wparams")
    menu2:addPane(pane1)
    menu2:newLink(-6.0,-4.0,"Back","back")
    menu2:newLink(0.0,-4.0,"Create World", link("game1"))

    local game1 = window:new ()
    game1:initGame("game1")
    game1:newLink(-6.0,-4.0,"Back","back")
    game1:newWorld("dat/tex/world")

    menu1:switchWindow ()
    return 0
end
function runMod ()
end
