{-# LANGUAGE Strict #-}
module Paracletus where
-- a graphics layer is chosen
-- and glfw instance is begun
import Prelude()
import UPrelude ( ($) )
import Anamnesis (Anamnesis(..))
import Anamnesis.Util (logExcept)
import Artos.Except (ExType(..))
import Paracletus.Data (GraphicsLayer(..),ParacResult(..))
import Paracletus.Vulkan (runParacVulkan)

runParacletus ∷ GraphicsLayer → Anamnesis ε σ ()
runParacletus Vulkan   = runParacVulkan
runParacletus OpenGL   = logExcept ParacError ExParacletus $ "OpenGL not yet implimented"
runParacletus OpenGLES = logExcept ParacError ExParacletus $ "OpenGLES not yet implimented"
runParacletus _        = logExcept ParacError ExParacletus $ "unsupported graphics layer..."
