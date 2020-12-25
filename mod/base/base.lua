-- hook functions to make things
-- not look so hardcoded
base = {}

function link (dest)
    return ("link:"..dest)
end

function exit ()
    return "exit"
end

return base
