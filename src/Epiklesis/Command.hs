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
    , LinkAction(..), PaneBit(..)
    , WorldParams(..), WorldData(..)
    , Zone(..), Segment(..)
    , Spot(..), Shell(..) )
import Epiklesis.Elem ( calcTextBoxSize )
import Epiklesis.World ( genWorldData )

-- quits everything using glfw in parent thread
hsExit ∷ Env → Lua.Lua ()
hsExit env = Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventExit

-- TODO: log debug needs to pass through
--       the callstack over the queue
hsLogDebug ∷ Env → String → Lua.Lua ()
hsLogDebug env str = Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventLogDebug str

hsLogInfo ∷ Env → String → Lua.Lua ()
hsLogInfo env str = Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventLogInfo str

hsRecreate ∷ Env → Lua.Lua ()
hsRecreate env = Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventRecreate

hsReload ∷ Env → Lua.Lua ()
hsReload env = Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventReload

hsPrint ∷ Env → String → Lua.Lua ()
hsPrint env "world" = Lua.liftIO $ atomically $ writeQueue (envLoadQ  env) $ LoadCmdPrint PrintWorld
hsPrint env "buff"  = Lua.liftIO $ atomically $ writeQueue (envLoadQ  env) $ LoadCmdPrint PrintBuff
hsPrint env "mem"   = Lua.liftIO $ atomically $ writeQueue (envLoadQ  env) $ LoadCmdPrint PrintMem
hsPrint env "wins"  = Lua.liftIO $ atomically $ writeQueue (envLoadQ  env) $ LoadCmdPrint PrintWins
hsPrint env "cam"   = do
  Lua.liftIO $ atomically $ writeQueue (envLoadQ  env) $ LoadCmdPrint PrintCam
  Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventPrint PrintCam
hsPrint env str     = Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "value " ⧺ str ⧺ " not known"

-- adds a window to the lua draw state
hsNewWindow ∷ Env → String → String → Lua.Lua ()
-- menus are the simplest windows, they only
-- contain a series of elements to draw, along
-- with the shell and fps counter
hsNewWindow env name "menu" = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewWin win
  where win = Window name WinTypeMenu WinArgNULL [64,64,256] [shell]
        shell  = WinElemShell shData False False
        shData = Shell "$> " Nothing 1 "" "" "" "" False (-1) []
-- game windows contain logic to preform
-- camera movement, screen switching,
-- and an animation thread
hsNewWindow env name "game" = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewWin win
  where win    = Window name WinTypeGame WinArgNULL [64,64,256,256] [shell]
        shell  = WinElemShell shData False False
        shData = Shell "$> " Nothing 1 "" "" "" "" False (-1) []
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
    -- calcTextBoxSize returns values for scaling textures,
    -- so we need to perform some scaling to match it to the
    -- alpha offset from the textures
    let (w',h') = calcTextBoxSize args
        (w,h)   = (w'/2.75,h'/1.8)
    Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem name $ WinElemLink (x + 1.5,y - 0.25) (w,h) LinkExit
  -- back returns to the last window
  "back" → do
    let (w',h') = calcTextBoxSize args
        (w,h)   = (w'/2.75,h'/1.8)
    Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem name $ WinElemLink (x + 1.5,y - 0.25) (w,h) LinkBack
  -- link goes to specified window
  "link" → do
    let (w',h') = calcTextBoxSize args
        (w,h)   = (w'/2.75,h'/1.8)
    Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem name $ WinElemLink (x + 1.5,y - 0.25) (w,h) $ LinkLink $ last $ splitOn ":" func
  _ → hsLogDebug env $ "no known link function " ⧺ func

-- adds a pane window element to the specified
-- window, panes are submenus, with various bits
hsNewPane ∷ Env → String → Double → Double → String → Lua.Lua ()
hsNewPane env name x y pane = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemPane (x,y) pane []

-- adds a pane bit to an existing pane
hsNewPaneBit ∷ Env → String → String → String → Lua.Lua ()
hsNewPaneBit env name pane bit = case (head (splitOn ":" bit)) of
  -- text is just text
  "text" → do
    let loadQ = envLoadQ env
    Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewBit name pane $ PaneBitText $ last $ splitOn ":" bit
  -- sliders allow for input to the window's argV
  "slider" → do
    let loadQ = envLoadQ env
    Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewBit name pane $ PaneBitSlider text mn mx $ Just vl
      where args = splitOn ":" bit
            text = head $ tail args
            mn   = read $ head $ tail $ tail args
            mx   = read $ head $ tail $ tail $ tail args
            vl   = read $ head $ tail $ tail $ tail $ tail $ args
  bitbit → hsLogDebug env $ "no known bit: " ⧺ (show bitbit)

-- adds a background world to a game screen
hsNewWorld ∷ Env → String → String → Lua.Lua ()
hsNewWorld env win dp = do
  rawdp ← Lua.liftIO $ getDirectoryContents dp
  let dps = (map (combine dp) $ sort $ filter filterOutPathJunk rawdp) ⧺ ["dat/tex/grayscale.png"]
      filterOutPathJunk ∷ FilePath → Bool
      filterOutPathJunk "."  = False
      filterOutPathJunk ".." = False
      filterOutPathJunk _    = True
      (zx,zy)  = (4,3)
      wp       = WorldParams (10,8) (zx,zy) (10,10) [] []
      wd0      = WorldData (1.0,1.0) [Zone (0,0) initSegs] Nothing
      wd1      = wd0--genWorldData wp wd0
      initSegs = take (zy+2) (repeat (take (zx+2) (repeat (Segment grid))))-- (repeat SegmentNULL)))
      grid     = take sh $ repeat $ take sw $ repeat $ Spot 1 1 Nothing 1
      (sw,sh)  = (10,8)
  Lua.liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem win $ WinElemWorld wp wd1 dps
