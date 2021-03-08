module Epiklesis.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import Data.List ( sort )
import Data.List.Split ( splitOn )
import qualified Foreign.Lua as Lua
import System.Directory ( getDirectoryContents )
import System.FilePath ( combine )
import System.Random
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
    ( Window(..), WinType(..)
    , WinArgV(..), WinElem(..)
    , LinkAction(..) )
import Epiklesis.Elem ( calcTextBoxSize )

-- quits everything using glfw in parent thread
hsExit ∷ Env → Lua.Lua ()
hsExit env = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventExit

-- TODO: log debug needs to pass through
--       the callstack over the queue
hsLogDebug ∷ Env → String → Lua.Lua ()
hsLogDebug env str = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventLogDebug str

-- adds a window to the lua draw state
hsNewWindow ∷ Env → String → String → Lua.Lua ()
-- menus are the simplest windows, they only
-- contain a series of elements to draw, along
-- with the shell and fps counter
hsNewWindow env name "menu" = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewWin win
  where win = Window name WinTypeMenu WinArgNULL []
-- game windows contain logic to preform
-- camera movement, screen switching,
-- and an animation thread
hsNewWindow env name "game" = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewWin win
  where win = Window name WinTypeGame WinArgNULL []
hsNewWindow env _    wintype = hsLogDebug env $ "window type " ⧺ wintype ⧺ " not known"

-- switches between windows
hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdSwitchWin name

-- adds text as a static element
hsNewText ∷ Env → String → Double → Double → String → Bool → Lua.Lua ()
hsNewText env name x y text box = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewElem name $ WinElemText (x,y) box text

-- adds a link to the specified window,
-- allowing mouse clicks to preform actions
hsNewLink ∷ Env → String → Double → Double → String → String → Lua.Lua ()
hsNewLink env name x y args func = case (head (splitOn ":" func)) of
-- exit closes everything using glfw
  "exit" → do
    let (w,h) = calcTextBoxSize args
    Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) LinkExit
  "link" → do
    let (w,h) = calcTextBoxSize args
    Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) $ LinkLink $ last $ splitOn ":" func
  _ → hsLogDebug env $ "no known link function " ⧺ func
