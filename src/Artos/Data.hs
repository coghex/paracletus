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
           | EventPrint !PrintArg
           | EventExit
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           | EventScroll !GLFW.Window !Double !Double
           | EventMoveCam !(Float,Float,Float)
           | EventAccel
           | EventVerts !Verts
           | EventDyns !(Dyns,Dyns,Dyns)
           | EventNewInput !LinkAction
           | EventInput !LinkAction
           | EventCap !Bool
           | EventModTexs ![String]
           | EventToggleFPS
           | EventRecreate
           | EventReload

-- cmds that can be asked of
-- the loading thread
data LoadCmd = LoadCmdNewWin Window
             | LoadCmdSwitchWin String
             | LoadCmdNewElem String WinElem
             | LoadCmdSetNDefTex Int
             | LoadCmdLink (Double,Double)
             | LoadCmdNewBit String String PaneBit
             | LoadCmdMoveSlider Double Int
             | LoadCmdShell ShellCmd
             | LoadCmdScroll Double
             | LoadCmdMoveCam ISKeys
             | LoadCmdVerts
             | LoadCmdDyns
             | LoadCmdWorld
             | LoadCmdSetFPS FPS
             | LoadCmdToggleFPS
             | LoadCmdPrint !PrintArg
             | LoadCmdNULL
