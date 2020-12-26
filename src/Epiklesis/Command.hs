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
import Paracletus.Data (TextSize(..))
import Paracletus.Elem (calcTextBoxSize)

hsExit ∷ Env → Lua.Lua ()
hsExit env = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventExit

hsLogDebug ∷ Env → String → Lua.Lua ()
hsLogDebug env str = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLogDebug str

hsToggleFPS ∷ Env → Lua.Lua ()
hsToggleFPS env = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdToggleFPS

hsNewWindow ∷ Env → String → String → Lua.Lua ()
hsNewWindow env name "menu" = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewWin win
  where win = Window name WinTypeMenu WinArgNULL (0,0,(-1)) []
hsNewWindow env name "game" = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewWin win
  where win = Window name WinTypeGame WinArgNULL (0,0,(-1)) []
hsNewWindow env _    wintype = hsLogDebug env $ "window type " ⧺ wintype ⧺ " not known"

hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdSwitchWin name

hsNewPane ∷ Env → String → Double → Double → String → Lua.Lua ()
hsNewPane env name x y pane = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemPane (x,y) pane []

hsNewPaneBit ∷ Env → String → String → String → Lua.Lua ()
hsNewPaneBit env name pane bit = case (head (splitOn ":" bit)) of
  "text" → do
    let loadQ = envLoadQ env
    Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewBit name pane $ PaneBitText $ last $ splitOn ":" bit
  bitbit → hsLogDebug env $ "no know bit: " ⧺ (show bitbit)

hsNewText ∷ Env → String → Double → Double → String → Bool → Lua.Lua ()
hsNewText env name x y text box = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewElem name $ WinElemText (x,y) box text

hsNewLink ∷ Env → String → Double → Double → String → String → Lua.Lua ()
hsNewLink env name x y args "exit" = do
  let eventQ = envLoadQ env
      (w,h)  = calcTextBoxSize TextSize30px args
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) LinkExit
hsNewLink env name x y args "back" = do
  let eventQ = envLoadQ env
      (w,h)  = calcTextBoxSize TextSize30px args
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) LinkBack
hsNewLink env name x y args func = case (head (splitOn ":" func)) of
  "link" → do
    let eventQ = envLoadQ env
        (w,h)  = calcTextBoxSize TextSize30px args
    Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) $ LinkLink $ last $ splitOn ":" func
  _      → hsLogDebug env $ "no known link function " ⧺ func

hsCreateWorld ∷ Env → Lua.Lua ()
hsCreateWorld env = hsLogDebug env $ "create world callback"
