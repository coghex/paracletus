-- menu pane for windows
pane = {}

function pane:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.pName = "NULL"
    self.pPosx = 0.0
    self.pPosy = 0.0
    self.pBits = {}
    return o
end
function pane:initPane (x,y,n)
    self.pName = n
    self.pPosx = x
    self.pPosy = y
    rawNewWindow (n,"menu")
end
function pane:addPaneBit (bit)
    table.insert (self.pBits,bit)
end

return pane
