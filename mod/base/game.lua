-- game mod defines an example game
require "mod/base/window"
-- this runs once at the beginning
function initParacletus ()
    -- basic UI elements can be in any order
    local menu1 = window:new ()
    menu1:initMenu("menu1")
    menu1:newText(0.0,0.0,"A Bridge Far Away")

    local menu2 = window:new ()
    menu2:initMenu("menu2")

    local game1 = window:new ()
    game1:initGame("game1")

    menu1:switchWindow ()

    return 0
end
