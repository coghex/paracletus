module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data

calcSpots ∷ WorldParams → WorldData → [Tile]
calcSpots wp wd = [GTile (0,0) (1,1) (0,0) (1,1) 2]
