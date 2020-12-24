module Paracletus.Elem where
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data
-- functions to convert winelems to gtiles
calcText ∷ Double → (Double,Double) → String → [Tile]
calcText _  _     []         = []
calcText x0 (_,y) ('\n':str) = calcText x0 (x0,(y - 1)) str
calcText x0 (x,y) (ch:str)   = [textTile] ⧺ calcText x0 (x + (fontOffset ch),y) str
  where textTile = GTile (x,y) (1,1) (fontIndex ch) (16,6) 1
