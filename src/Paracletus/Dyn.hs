module Paracletus.Dyn where
-- dynamic data for tiles is tracked
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Paracletus.Data
import Epiklesis.Data
import Epiklesis.Shell
import Epiklesis.Window

-- loads in reverse so the trans functions
-- can be mmap recursively without index
loadDyns ∷ DrawState → (Dyns,Dyns,Dyns)
loadDyns ds = (Dyns $ reverse $ loadDynData ds $ dsTiles ds, Dyns $ reverse $ loadCamData ds $ dsTiles ds, Dyns $ reverse $ loadAuxData ds $ dsTiles ds)

loadCamData ∷ DrawState → [Tile] → [DynData]
loadCamData _  []                      = []
loadCamData ds ((DMTile (DMBuff b n) _ _ _ _ _):ts) = [buff !! n] ⧺ loadCamData ds ts
  where Dyns buff = dsBuff ds !! b
loadCamData ds ((DMTile DMNULL _ _ _ _ _):ts) = [DynData 0 (0,0) (1,1) (0,0)] ⧺ loadCamData ds ts
loadCamData ds (_:ts) = loadCamData ds ts

loadAuxData ∷ DrawState → [Tile] → [DynData]
loadAuxData _  []                      = []
loadAuxData ds ((ATile (DMBuff b n) _ _ _ _ _):ts) = [buff !! n] ⧺ loadAuxData ds ts
  where Dyns buff = dsBuff ds !! b
loadAuxData ds ((ATile DMNULL _ _ _ _ _):ts) = [DynData 0 (0,0) (1,1) (0,0)] ⧺ loadAuxData ds ts
loadAuxData ds (_:ts) = loadAuxData ds ts

loadDynData ∷ DrawState → [Tile] → [DynData]
loadDynData _  []                     = []
loadDynData ds ((GTile _ _ _ _ _):ts) = [] ⧺ loadDynData ds ts
loadDynData ds ((MTile _ _ _ _ _):ts) = [] ⧺ loadDynData ds ts
loadDynData ds ((DTile (DMFPS n) _ _ _ _ _):ts) = [DynData dig (0,0) (1,1) (0,0)] ⧺ loadDynData ds ts
  where dig = calcDiglet n $ dsFPS ds
loadDynData ds ((DTile (DMSliderVal n d) _ _ _ _ _):ts) = [DynData dig (0,0) (1,1) (0,0)] ⧺ loadDynData ds ts
  where dig = case (currentWin ds) of
                Just w  → sliderDiglet (winElems w) n d
                Nothing → 0
loadDynData ds ((DTile (DMShCursor) _ _ _ _ _):ts) = [DynData 0 (x,2*(1-y)) (1,1) (0,0)] ⧺ loadDynData ds ts
  where x      = realToFrac $ findCursPos $ take n $ shInpStr sh
        y      = fromIntegral $ length $ splitOn "\n" $ shOutStr sh
        sh     = dsShell ds
        n      = shCursor sh
loadDynData ds ((DTile (DMSlider n) _ _ _ _ _):ts) = [DynData 0 (x,0) (1,1) (0,0)] ⧺ loadDynData ds ts
  where x = case (currentWin ds) of
              Just w  → calcSliderOffset w n
              Nothing → 0
loadDynData ds ((DTile (DMBuff b n) _ _ _ _ _):ts) = [buff !! n] ⧺ loadDynData ds ts
  where Dyns buff = dsBuff ds !! b
--loadDynData ds ((DMTile (DMBuff b n) _ _ _ _ _):ts) = [buff !! n] ⧺ loadDynData ds ts
--  where Dyns buff = dsBuff ds !! b
loadDynData ds ((DMTile _ _ _ _ _ _):ts) = loadDynData ds ts
loadDynData ds ((ATile _ _ _ _ _ _):ts) = loadDynData ds ts
loadDynData ds ((DTile (DMNULL) _ _ _ _ _):ts) = [DynData 0 (0,0) (1,1) (0,0)] ⧺ loadDynData ds ts

-- calcs dyndata for slider val
sliderDiglet ∷ [WinElem] → Int → Int → Int
sliderDiglet []       _ _ = -36
sliderDiglet ((WinElemPane _ _ bits):wes) n d = sliderBitDiglet bits n d
sliderDiglet (we:wes) n d = sliderDiglet wes n d
sliderBitDiglet ∷ [(Int,PaneBit)] → Int → Int → Int
sliderBitDiglet []       _ _ = -36
sliderBitDiglet ((i,(PaneBitSlider _ _ _ (Just val))):pbs) n d
  | n ≡ i = calcSliderDig d val
  | otherwise = sliderBitDiglet pbs n d
sliderBitDiglet (pb:pbs) n d = sliderBitDiglet pbs n d
calcSliderDig ∷ Int → Int → Int
calcSliderDig d val
  | val > 0   ∧ d < 1 = calcDig d val
  | val > 9   ∧ d < 2 = calcDig d val
  | val > 99  ∧ d < 3 = calcDig d val
  | val > 999 ∧ d < 4 = calcDig d val
  | otherwise         = -36

-- convert fps to singe didget
calcDiglet ∷ Int → FPS → Int
calcDiglet _ (FPS _ _   False) = -36
calcDiglet n (FPS _ fps True)
  | fps > 0    ∧ n < 1 = calcDig n fps
  | fps > 9    ∧ n < 2 = calcDig n fps
  | fps > 99   ∧ n < 3 = calcDig n fps
  | fps > 999  ∧ n < 4 = calcDig n fps
  | otherwise          = -36
calcDig ∷ Int → Int → Int
calcDig 0 fps = fps `mod` 10
calcDig 1 fps = (fps `div` 10) `mod` 10
calcDig 2 fps = (fps `div` 100) `mod` 10
calcDig 3 fps = (fps `div` 1000) `mod` 100
calcDig _ fps = -36

-- convert slider 
calcSliderOffset ∷ Window → Int → Float
calcSliderOffset win n = calcWinSliderOffset (winElems win) n
calcWinSliderOffset ∷ [WinElem] → Int → Float
calcWinSliderOffset [] _ = 0.0
calcWinSliderOffset ((WinElemPane _ _ bits):wes) n = calcBitsSliderOffset bits n
calcWinSliderOffset (we:wes) n = calcWinSliderOffset wes n
calcBitsSliderOffset ∷ [(Int,PaneBit)] → Int → Float
calcBitsSliderOffset []       _ = 0.0
calcBitsSliderOffset ((i,(PaneBitSlider text mn mx (Just val))):pbs) n = if (n ≡ i) then 6.0*val'/(mx' - mn') else calcBitsSliderOffset pbs n
  where val' = fromIntegral val
        mn'  = fromIntegral mn
        mx'  = fromIntegral mx
calcBitsSliderOffset (pb:pbs) n = calcBitsSliderOffset pbs n

-- move slider
moveSlider ∷ Double → Int → Window → Window
moveSlider x n win = win { winElems = moveWinSlider (winElems win) x n }
moveWinSlider ∷ [WinElem] → Double → Int → [WinElem]
moveWinSlider []       _ _ = []
moveWinSlider ((WinElemPane pos name bits):wes) x n = [WinElemPane pos name bits'] ⧺ moveWinSlider wes x n
  where bits' = moveBitsSlider (x - (fst pos) - 5.0) n bits
moveWinSlider (we:wes) x n = [we] ⧺ moveWinSlider wes x n

moveBitsSlider ∷ Double → Int → [(Int,PaneBit)] → [(Int,PaneBit)]
moveBitsSlider _ _ [] = []
moveBitsSlider x n ((i,PaneBitSlider text mn mx (Just val)):pbs)
  | (i ≡ n)   = [(i,PaneBitSlider text mn mx (Just val'))] ⧺ moveBitsSlider x n pbs
  | otherwise = [(i,PaneBitSlider text mn mx (Just val))]  ⧺ moveBitsSlider x n pbs
  where val' = min mx $ max mn $ ((round (x*(mx' - mn'))) `div` 3)
        mn'  = fromIntegral mn
        mx'  = fromIntegral mx
moveBitsSlider x n (pb:pbs) = [pb] ⧺ moveBitsSlider x n pbs

-- pretty printer
printDynList ∷ DrawState → String
printDynList ds = case (currentWin ds) of
  Just w  → printWinDynList w
  Nothing → "no window"
printWinDynList ∷ Window → String
printWinDynList win = printElemDynList $ winElems win
printElemDynList ∷ [WinElem] → String
printElemDynList []       = "no elems"
printElemDynList ((WinElemPane pos name bits):wes) = printBitsDynList bits ⧺ printElemDynList wes
printElemDynList (we:wes) = printElemDynList wes
printBitsDynList ∷ [(Int,PaneBit)] → String
printBitsDynList [] = ""
printBitsDynList ((i,(PaneBitSlider text mn mx (Just val))):pbs) = (show i) ⧺ ", " ⧺ text ⧺ ": " ⧺ (show val) ⧺ "\n" ⧺ printBitsDynList pbs
printBitsDynList ((i,pb):pbs) = printBitsDynList pbs
