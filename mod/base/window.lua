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
function window:newWorld (dp)
    rawNewWorld ((self.lwName),dp)
end
function window:newText (x,y,args)
    rawNewText (self.lwName,x,y,args,false)
end
function window:newLink (x,y,args,hook)
    if ((type(hook)) == "string") then
        ret = hook
    else
        ret = hook ()
    end
    rawNewText (self.lwName,x,y,args,ret,true)
    rawNewLink (self.lwName,x,y,args,ret)
end
function window:addPane (pane)
    rawNewPane (self.lwName,pane.pPosx,pane.pPosy,pane.pName)
    for i,b in pairs(pane.pBits) do
        rawNewPaneBit (self.lwName,pane.pName,b)
    end
end

return window
