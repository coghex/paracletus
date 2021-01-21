module Paracletus.Text where
-- methods to set text dyns are found
import Prelude()
import UPrelude
import Epiklesis.Data ( WorldData(..) )
import Paracletus.Buff ( genStrDDs )
import Paracletus.Data
    ( DynData(..), Dyns(..) )

calcTextBuff ∷ WorldData → Dyns
calcTextBuff wd = case (wdSelect wd) of
  Nothing → Dyns $ replicate 196 $ DynData 0 (0,0) (1,1) (0,0)
  Just cs → Dyns $ genTextBuffer (-10,8) $ "cursor: (" ⧺ (show (fst cs)) ⧺ ", " ⧺ (show (snd cs)) ⧺ ")"

genTextBuffer ∷ (Double,Double) → String → [DynData]
genTextBuffer (x,y) str = res ⧺ replicate (196 - length res) (DynData 0 (0,0) (1,1) (0,0))
  where res  = tb ⧺ genStrDDs (x,y) str dyns
        dyns = replicate (length str) $ DynData 0 (0,0) (1,1) (0,0)
        tb   = genTextBoxBuffer (x,y+0.75) (16, 2)

genTextBoxBuffer ∷ (Double,Double) → (Double,Double) → [DynData]
genTextBoxBuffer (x,y) (sx,sy) = [middleTile,leftTile,rightTile,topTile,bottomTile,topLeftTile,topRightTile,botLeftTile,botRightTile]
  where topLeftTile  = DynData 6  (x',y')                                         (0.5,0.5)                (0,0)
        topRightTile = DynData 5  (x' + (0.5*sx') + 0.5,  y')                     (0.5,0.5)                (0,0)
        botLeftTile  = DynData 9  (x',                    y' - (0.5*sy') - 0.5)   (0.5,0.5)                (0,0)
        botRightTile = DynData 8  (x' + (0.5*sx') + 0.5,  y' - (0.5*sy') - 0.5)   (0.5,0.5)                (0,0)
        topTile      = DynData 4  (x' + (0.25*sx') + 0.25,y')                     ((0.25*sx'),0.5)         (0,0)
        leftTile     = DynData 10 (x',                    y' - (0.25*sy') - 0.25) (0.5,(0.25*sy'))         (0,0)
        rightTile    = DynData 3  (x' + (0.5*sx') + 0.5,  y' - (0.25*sy') - 0.25) (0.5,(0.25*sy'))         (0,0)
        bottomTile   = DynData 7  (x' + (0.25*sx') + 0.25,y' - (0.5*sy') - 0.5)   ((0.25*sx'),0.5)         (0,0)
        middleTile   = DynData 2  (x' + (0.25*sx') + 0.25,y' - (0.25*sy') - 0.25) ((0.25*sx'),(0.25*sy'))  (0,0)
        (x',y')      = (realToFrac x,  realToFrac y)
        (sx',sy')    = (realToFrac sx, realToFrac sy)

