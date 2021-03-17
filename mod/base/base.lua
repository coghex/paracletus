-- hook functions to make things
-- not look so hardcoded
base = {}

function link (dest)
    return ("link:"..dest)
end
function textBit (text)
    return ("text:"..text)
end
function sliderBit (text,mn,mx,vl)
    return ("slider:"..text..":"..(tostring(mn))..":"..(tostring(mx))..":"..(tostring(vl)))
end

return base
