module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

-- timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- events processed by the main thread
data Event = EventError !GLFW.Error !String
           | EventLogDebug !String
           | EventExit
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | EventVerts !Verts
           | EventDyns !Dyns
           | EventToggleFPS
           | EventRecreate
           | EventReload

-- cmds that can be asked of
-- the loading thread
data LoadCmd = LoadCmdNewWin Window
             | LoadCmdSwitchWin String
             | LoadCmdNewElem String WinElem
             | LoadCmdLink (Double,Double)
             | LoadCmdNewBit String String PaneBit
             | LoadCmdVerts
             | LoadCmdSetFPS FPS
             | LoadCmdToggleFPS
             | LoadCmdNULL
