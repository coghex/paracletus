module Epiklesis.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import qualified Foreign.Lua as Lua
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Paracletus.Elem (calcLinkSize)

hsExit ∷ Env → Lua.Lua ()
hsExit env = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventExit

hsLogDebug ∷ Env → String → Lua.Lua ()
hsLogDebug env str = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLogDebug str

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

hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdSwitchWin name

hsNewText ∷ Env → String → Double → Double → String → Lua.Lua ()
hsNewText env name x y text = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemText (x,y) False text

hsNewLink ∷ Env → String → Double → Double → String → String → Lua.Lua ()
hsNewLink env name x y args "exit" = do
  let eventQ = envLoadQ env
      (w,h)  = calcLinkSize args
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) LinkExit
hsNewLink env name x y args func = case (head (splitOn ":" func)) of
  "link" → do
    let eventQ = envLoadQ env
        (w,h)  = calcLinkSize args
    Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) $ LinkLink $ last $ splitOn ":" func
  _      → do
    let eventQ = envEventQ env
    Lua.liftIO $ atomically $ writeQueue eventQ $ EventLogDebug $ "no known link function " ⧺ func

hsCreateWorld ∷ Env → Lua.Lua ()
hsCreateWorld env = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLogDebug "create world callback"
