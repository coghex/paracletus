-- this runs once at the beginning
function initParacletus (files)
    for file,k in string.gmatch(files, "([^;]*)") do
      local f = assert(loadfile(file))
      f ()
      initMod ()
    end
    return 0
end
