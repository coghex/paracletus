-- abfa mod defines an example game
require "mod/base/window"
require "mod/base/base"
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
    menu2:newText (-6.0,1.0,"World Parameters")
    menu2:newLink(-6.0,-3.0,"Back",back)

    local game1 = window:new ()
    game1:initGame("game1")

    menu1:switchWindow ()
    toggleFPS ()

    return 0
end
