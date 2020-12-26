-- hook functions to make things
-- not look so hardcoded
base = {}

function link (dest)
    return ("link:"..dest)
end
function textBit (text)
    return ("text:"..text)
end

function exit ()
    return "exit"
end
function back ()
    return "back"
end

return base
