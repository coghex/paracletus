module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude
import Epiklesis.Data ( Window(..), WinElem(..), PaneBit(..) )
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
             | LoadCmdNewBit String String PaneBit
             | LoadCmdSetNDefTex !Int
             | LoadCmdNULL

-- possible print commands
data PrintArg = PrintCam
              | PrintNULL deriving (Show, Eq)
