module Epiklesis.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Foreign.Lua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import System.Random
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.Noise (makePerlin)
import Paracletus.Data (TextSize(..),PrintArg(..))
import Paracletus.Elem (calcTextBoxSize)

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

-- prints parent thread state memory
-- to stdout, for debugging
hsPrintHs ∷ Env → String → Lua.Lua ()
hsPrintHs env "cam" = do
  let eventQ = envEventQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ EventPrint PrintCam
hsPrintHs env str = hsLogDebug env ("value " ⧺ str ⧺ " not known")

-- turns on or off the FPS counter
hsToggleFPS ∷ Env → Lua.Lua ()
hsToggleFPS env = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdToggleFPS

-- switches the screen within a window
hsSwitchScreen ∷ Env → String → Lua.Lua ()
hsSwitchScreen env "null" = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdSwitchScreen WinScreenNULL
hsSwitchScreen env "elev" = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdSwitchScreen WinScreenElev
hsSwitchScreen env name = hsLogDebug env str
  where str = "unknown screen " ⧺ name

-- adds a window to the lua draw state
hsNewWindow ∷ Env → String → String → Lua.Lua ()
-- menus are the simplest windows, they only
-- contain a series of elements to draw, along
-- with the shell and fps counter
hsNewWindow env name "menu" = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewWin win
  where win = Window name WinTypeMenu WinArgNULL (0,0,(-1)) (0,0) [] WinScreenNULL
-- game windows contain logic to preform
-- camera movement, screen switching,
-- and an animation thread
hsNewWindow env name "game" = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewWin win
  where win = Window name WinTypeGame WinArgNULL (0,0,(-1)) (0,0) [] WinScreenNULL
hsNewWindow env _    wintype = hsLogDebug env $ "window type " ⧺ wintype ⧺ " not known"

-- switches between windows
hsSwitchWindow ∷ Env → String → Lua.Lua ()
hsSwitchWindow env name = do
  let eventQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdSwitchWin name

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

-- adds text as a static element
hsNewText ∷ Env → String → Double → Double → String → Bool → Lua.Lua ()
hsNewText env name x y text box = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewElem name $ WinElemText (x,y) box text

-- adds a link to the specified window,
-- allowing mouse clicks to preform actions
hsNewLink ∷ Env → String → Double → Double → String → String → Lua.Lua ()
-- exit closes everything using glfw
hsNewLink env name x y args "exit" = do
  let eventQ = envLoadQ env
      (w,h)  = calcTextBoxSize TextSize30px args
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) LinkExit
-- back switches to the previous window
hsNewLink env name x y args "back" = do
  let eventQ = envLoadQ env
      (w,h)  = calcTextBoxSize TextSize30px args
  Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) LinkBack
hsNewLink env name x y args func = case (head (splitOn ":" func)) of
-- link switches to a specified window
  "link" → do
    let eventQ = envLoadQ env
        (w,h)  = calcTextBoxSize TextSize30px args
    Lua.liftIO $ atomically $ writeQueue eventQ $ LoadCmdNewElem name $ WinElemLink (x,y) (w,h) $ LinkLink $ last $ splitOn ":" func
  _      → hsLogDebug env $ "no known link function " ⧺ func

-- adds a world with specified textures to
-- the drawstate, requires swapchain recreation
hsNewWorld ∷ Env → String → Int → Int → Int → Int → String → Lua.Lua ()
hsNewWorld env win sx sy zx zy dp = do
  let loadQ = envLoadQ env
  rawdp ← Lua.liftIO $ getDirectoryContents dp
  let dps = (map (combine dp) $ sort $ filter filterOutPathJunk rawdp) ⧺ ["dat/tex/greyscale.png"]
      filterOutPathJunk ∷ FilePath → Bool
      filterOutPathJunk "."  = False
      filterOutPathJunk ".." = False
      filterOutPathJunk _    = True
      sgs = [mkStdGen 0, mkStdGen 1]
      perlin = makePerlin 1 4 0.05 0.5
      wp = WorldParams (sx,sy) (zx,zy) (10,10) [] [] sgs perlin
      wd = WorldData (1.0,1.0) [Zone (0,0) initSegs]
      initSegs = take zy (repeat (take zx (repeat (SegmentNULL))))
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewElem win (WinElemWorld wp wd dps)
