-- base mod defines some basic gameplay
-- this data type represents a classlike
-- interface to the haskell objects
LuaWindow = {lwName = "NULLname"}
function LuaWindow:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.lwName = "NULL"
    return o
end
function LuaWindow:init (n,t)
    self.lwName = n
    newWindow (n,t)
end
-- this runs once at the beginning
function initParacletus ()
    -- basic UI elements can be in any order
    local menu1 = LuaWindow:new ()
    menu1:init("menu1","menu")

    local menu2 = LuaWindow:new ()
    menu2:init("menu2","menu")

    local game1 = LuaWindow:new ()
    game1:init("game1","game")

    return 0
end
