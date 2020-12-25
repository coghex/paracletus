module Epiklesis.Settings where
-- various user defined settings occur
import Prelude()
import UPrelude
import Anamnesis.Data
import qualified Foreign.Lua as Lua
import qualified Paracletus.Oblatum.GLFW as GLFW

initSettings ∷ Lua.State → String → IO (Settings)
initSettings ls fn = Lua.runWith ls $ do
  Lua.openlibs
  _ ← Lua.dofile $ fn
  esckey ← Lua.getglobal "esckey" *> Lua.peek (-1)
  retkey ← Lua.getglobal "retkey" *> Lua.peek (-1)
  delkey ← Lua.getglobal "delkey" *> Lua.peek (-1)
  spckey ← Lua.getglobal "spckey" *> Lua.peek (-1)
  tabkey ← Lua.getglobal "tabkey" *> Lua.peek (-1)
  upkey  ← Lua.getglobal "upkey"  *> Lua.peek (-1)
  lftkey ← Lua.getglobal "lftkey" *> Lua.peek (-1)
  dwnkey ← Lua.getglobal "dwnkey" *> Lua.peek (-1)
  rgtkey ← Lua.getglobal "rgtkey" *> Lua.peek (-1)
  shkey  ← Lua.getglobal "shkey"  *> Lua.peek (-1)
  return $ Settings $ GLFW.KeyLayout esckey retkey delkey spckey tabkey upkey lftkey dwnkey rgtkey shkey
