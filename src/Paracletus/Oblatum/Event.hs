{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Anamnesis
import Anamnesis.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalKey window k ks mk = return ()

evalMouse ∷ GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalMouse win mb mbs _ = return ()
