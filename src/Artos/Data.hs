module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude
import Epiklesis.Data ( Window(..), WinElem(..), PaneBit(..), LinkAction(..) )
import qualified Paracletus.Oblatum.GLFW as GLFW
import Paracletus.Data ( Verts, Dyns, FPS, Tile )

-- timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- events processed by the main thread
data Event = EventError !GLFW.Error !String
           | EventLogDebug !String
           | EventPrint !PrintArg
           | EventExit
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | EventScroll !GLFW.Window !Double !Double
           | EventVerts !Verts
           | EventDyns !Dyns
           | EventModTexs ![String]
           | EventNewInput !LinkAction
           | EventInput !LinkAction
           | EventCap !Bool
           | EventRecreate
           | EventReload

-- cmds that can be asked of
-- the loading thread
data LoadCmd = LoadCmdPrint !PrintArg
             | LoadCmdSetFPS !FPS
             | LoadCmdVerts
             | LoadCmdDyns
             | LoadCmdClear
             | LoadCmdLink (Double,Double)
             | LoadCmdInitBuff ![Tile] ![Dyns]
             | LoadCmdBuff !Int !Dyns
             | LoadCmdNewWin !Window
             | LoadCmdSwitchWin !String
             | LoadCmdNewElem !String !WinElem
             | LoadCmdNewBit !String !String !PaneBit
             | LoadCmdInput !LCInput
             | LoadCmdSetNDefTex !Int
             | LoadCmdNULL

-- possible print commands
data PrintArg = PrintCam
              | PrintBuff
              | PrintWinElems
              | PrintNULL deriving (Show, Eq)

-- possible load commands from input
data LCInput = LCISlider Double Int
             | LCIShell ShellCmd
             | LCINULL

-- possible shell commands
data ShellCmd = ShellCmdToggle
              | ShellCmdDelete
              | ShellCmdString String
              | ShellCmdNULL
