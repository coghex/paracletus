module Epiklesis.World where
import Prelude()
import UPrelude
import Epiklesis.Data
    ( WinElem(..), WorldParams(..), WorldData(..)
    , Zone(..), Segment(..), Spot(..), Window(..) )
import Epiklesis.Window
import Paracletus.Buff ( setTileBuff )
import Paracletus.Data
    ( Dyns(..), DynData(..), DrawState(..) )

-- returns the first world in the winelems
findWorld ∷ [WinElem] → Maybe (WorldParams, WorldData)
findWorld []                           = Nothing
findWorld ((WinElemWorld wp wd _):wes) = Just (wp,wd)
findWorld (we:wes)                     = findWorld wes

-- prints the first world found nicely
printWorld ∷ Maybe Window → String
printWorld Nothing  = "no window found"
printWorld (Just w) = case (findWorld (winElems w)) of
  Nothing      → "no world found"
  Just (_ ,wd) → printWorldData wd
printWorldData ∷ WorldData → String
printWorldData wd = show $ wdZones wd

-- generates the world dyns and plugs it in to the buffer
genWorldBuff ∷ [Dyns] → Int → Int → WorldParams → WorldData → [Dyns]
genWorldBuff buff b nDefTex wp wd = setTileBuff b dyns buff
  where dyns = Dyns $ genWorldDyns nDefTex 256 (0,0) wp wd
genWorldDyns ∷ Int → Int → (Int,Int) → WorldParams → WorldData → [DynData]
genWorldDyns nDefTex size curs wp wd = dyns ⧺ til1 ⧺ til2
  where dyns = take (size - 2) $ repeat $ DynData (nDefTex + 3) (0,0) (1,1) (0,0)
        til1 = [DynData (nDefTex + 3) (2,0) (1,1) (1,1)]
        til2 = [DynData (nDefTex + 3) (4,0) (1,1) (2,2)]
