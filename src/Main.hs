module Main where
-- where the magic happens...
import Prelude()
import UPrelude
import Artos ( checkStatus )
import Paracletus ( runParacletus )
import Paracletus.Data ( GraphicsLayer(..) )
import Anamnesis.Init ( runAnamnesis )
-- runs paracletus in the continuation monad, after
-- initializing state and env, using a status function,
-- and specifying a graphics layer
main ∷ IO ()
main = runAnamnesis checkStatus (runParacletus Vulkan)
