module Epiklesis.ShCmd where
-- some auxillary shell lua functions
-- loaded in by default when shell is run
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data

loadShCmds ∷ Env → IO ()
loadShCmds env = do
  let ls = envLuaSt env
  _ ← Lua.runWith ls $ do
    Lua.registerHaskellFunction "echo" (hsEcho env)
    Lua.registerHaskellFunction "history" (hsHistory env)
  return ()

hsEcho ∷ Env → String → Lua.Lua ()
hsEcho env str = Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdEcho str

hsHistory ∷ Env → Lua.Lua ()
hsHistory env = Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdEcho "history"
