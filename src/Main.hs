module Main where
import Prelude()
import UPrelude
import Artos (checkStatus)
import Paracletus (runParacletus)
import Paracletus.Data (GraphicsLayer(..))
import Anamnesis.Init (runAnamnesis)
-- where the magic happens...
main ∷ IO ()
main = runAnamnesis checkStatus (runParacletus Vulkan)
