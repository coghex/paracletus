-- this runs once at the beginning
function initParacletus (files)
    for file,k in string.gmatch(files, "([^;]*)") do
      local f = assert(loadfile(file))
      f ()
      initMod ()
    end
    return 0
end
-- this runs every tick
function runParacletus (files)
    for file,k in string.gmatch(files, "([^;]*)") do
      local f = assert(loadfile(file))
      f ()
      runMod ()
    end
    return 0
end

game = game or {}

function game.print(str)
    rawPrint(str)
end

