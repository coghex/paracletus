module Epiklesis.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data

hsNewWindow ∷ Env → String → String → Lua.Lua ()
hsNewWindow env name "menu" = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewWin win
  where win = Window name WinTypeMenu WinArgNULL (0,0,(-1)) []
hsNewWindow env name "game" = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewWin win
  where win = Window name WinTypeGame WinArgNULL (0,0,(-1)) []
hsNewWindow env _    wintype = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLogDebug errorstr
  where errorstr = "window type " ⧺ wintype ⧺ " not known"
