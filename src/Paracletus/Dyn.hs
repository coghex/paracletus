module Paracletus.Dyn where
-- dynamic data for tiles is tracked
import Prelude()
import UPrelude
import Paracletus.Data
import Epiklesis.Data

loadDyns ∷ DrawState → [DynData]
loadDyns ds = loadDynData ds $ dsTiles ds
loadDynData ∷ DrawState → [Tile] → [DynData]
loadDynData _  []                     = []
loadDynData ds ((GTile _ _ _ _ _):ts) = [] ⧺ loadDynData ds ts
loadDynData ds ((DTile (DMFPS n) _ _ _ _ _):ts) = [DynData dig (0,0) (0,0)] ⧺ loadDynData ds ts
  where dig = calcDiglet n $ dsFPS ds
loadDynData ds ((DTile (DMSlider n) _ _ _ _ _):ts) = [DynData 0 (x,0) (0,0)] ⧺ loadDynData ds ts
  where x = calcSliderOffset ds n
loadDynData ds ((DTile (DMNULL) _ _ _ _ _):ts) = [DynData 0 (0,0) (0,0)] ⧺ loadDynData ds ts

-- convert fps to singe didget
calcDiglet ∷ Int → FPS → Int
calcDiglet _ (FPS _ _   False) = -36
calcDiglet n (FPS _ fps True)
  | fps > 0    ∧ n < 1 = calcDig n fps
  | fps > 9    ∧ n < 2 = calcDig n fps
  | fps > 99   ∧ n < 3 = calcDig n fps
  | fps > 999  ∧ n < 4 = calcDig n fps
  | otherwise         = -36
calcDig ∷ Int → Int → Int
calcDig 0 fps = fps `mod` 10
calcDig 1 fps = (fps `div` 10) `mod` 10
calcDig 2 fps = (fps `div` 100) `mod` 10
calcDig 3 fps = (fps `div` 1000) `mod` 100
calcDig _ fps = -36

-- convert slider 
calcSliderOffset ∷ DrawState → Int → Float
calcSliderOffset _ _ = 0.0
