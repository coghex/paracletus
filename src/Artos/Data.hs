module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude
import qualified Paracletus.Oblatum.GLFW as GLFW

-- timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- events processed by the main thread
data Event = EventError !GLFW.Error !String
           | EventLogDebug !String
           | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
           | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
           deriving (Show, Eq)

-- cmds that can be asked of
-- the loading thread
data LoadCmd = LoadCmdNULL
