-- base mod defines some basic gameplay
-- this data type represents a classlike
-- interface to the haskell objects
window = {}

function window:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.lwName = "NULL"
    return o
end
function window:initMenu (n)
    self.lwName = n
    rawNewWindow (n,"menu")
end
function window:initGame (n)
    self.lwName = n
    rawNewWindow (n,"game")
end
function window:switchWindow ()
    rawSwitchWindow (self.lwName)
end
function window:newText (x,y,args)
    rawNewText ((self.lwName),x,y,args)
end

return window
