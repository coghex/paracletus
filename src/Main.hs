module Main where
import Prelude()
import UPrelude
import Artos
import Paracletus
import Paracletus.Data
import Anamnesis.Init
-- where the magic happens...
main ∷ IO ()
main = runAnamnesis checkStatus (runParacletus Vulkan)
