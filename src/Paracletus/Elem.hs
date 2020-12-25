module Paracletus.Elem where
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Font
-- functions to convert winelems to gtiles
calcText ∷ TextSize → Double → (Double,Double) → String → [Tile]
calcText _        _  _     []         = []
calcText size x0 (_,y) ('\n':str) = calcText size x0 (x0,(y - 1)) str
calcText size x0 (x,y) (' ':str)  = calcText size x0 (x + 0.5,y) str
calcText size x0 (x,y) (ch:str)   = [textTile] ⧺ calcText size x0 (x + chX',y) str
  where textTile = GTile (x+(chX'/2.0),y+chY') (chW',chH') (0,0) (1,1) chIndex
        TTFData chIndex chW chH chX chY = indexTTF size ch
        chW' = case size of
                 TextSize16px → 0.5*chW
                 TextSize30px → chW
        chH' = case size of
                 TextSize16px → 0.5*chH
                 TextSize30px → chH
        chX' = case size of
                 TextSize16px → 0.5*chX
                 TextSize30px → chX
        chY' = case size of
                 TextSize16px → 0.5*chY
                 TextSize30px → chY

