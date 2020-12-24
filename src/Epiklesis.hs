module Epiklesis
  ( loadEpiklesis ) where
-- an interface to lua is defined
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Command

loadEpiklesis ∷ Env → IO ()
loadEpiklesis env = do
  let ls = envLuaSt env
  _ ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "newWindow" (hsNewWindow env)
    Lua.openlibs
    _ ← Lua.dofile $ "mod/base/base.lua"
    ret ← Lua.callFunc "initParacletus"
    return (ret∷Int)
  let eventQ = envEventQ env
  atomically $ writeQueue eventQ $ EventRecreate
  return ()
