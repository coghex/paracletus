module Paracletus.Oblatum.Callback where
-- callbacks for GLFW are defined
import Artos.Data
    ( Event(EventScroll, EventMouseButton, EventKey, EventError) )
import Artos.Queue ( Queue, writeQueue )
import Artos.Var ( atomically )
import qualified Paracletus.Oblatum.GLFW as GLFW

errorCallback ∷ Queue Event → GLFW.Error → String → IO ()
errorCallback tc e s = atomically $ writeQueue tc $ EventError e s
keyCallback ∷ Queue Event → GLFW.Window → GLFW.Key → Int → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback tc win k sc ka mk = atomically $ writeQueue tc $ EventKey win k sc ka mk
mouseButtonCallback ∷ Queue Event → GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
mouseButtonCallback tc win mb mbs mk = atomically $ writeQueue tc $ EventMouseButton win mb mbs mk
scrollCallback :: Queue Event -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback tx win x y = atomically $ writeQueue tx $ EventScroll win x y
