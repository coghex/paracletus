module Epiklesis.ShCmd where
-- some auxillary shell lua functions
-- loaded in by default when shell is run
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.Command (hsLogDebug)

loadShCmds ∷ Env → IO ()
loadShCmds env = do
  let ls = envLuaSt env
  _ ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "echo" (hsEcho env)
  return ()

hsEcho ∷ Env → String → Lua.Lua ()
hsEcho env str = Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdRet str
