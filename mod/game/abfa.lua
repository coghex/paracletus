-- abfa mod defines an example game
require "mod/base/window"
require "mod/base/base"

-- this runs once at the beginning
function initMod ()
    -- basic UI elements can be in any order
    local menu1 = window:new ()
    menu1:initMenu("menu1")
    menu1:newText(-6.0,1.0,"A Bridge Far Away")
    menu1:switchWindow ()
    return 0
end
function runMod ()
end
