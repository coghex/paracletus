module Paracletus.Dyn where
-- dynamic data for tiles is tracked
import Prelude()
import UPrelude
import Paracletus.Data
import Epiklesis.Data
import Epiklesis.Window

loadDyns ∷ DrawState → [DynData]
loadDyns ds = loadDynData ds $ dsTiles ds
loadDynData ∷ DrawState → [Tile] → [DynData]
loadDynData _  []                     = []
loadDynData ds ((GTile _ _ _ _ _):ts) = [] ⧺ loadDynData ds ts
loadDynData ds ((DTile (DMFPS n) _ _ _ _ _):ts) = [DynData dig (0,0) (0,0)] ⧺ loadDynData ds ts
  where dig = calcDiglet n $ dsFPS ds
loadDynData ds ((DTile (DMSlider n) _ _ _ _ _):ts) = [DynData 0 (x,0) (0,0)] ⧺ loadDynData ds ts
  where x = case (currentWin ds) of
              Just w  → calcSliderOffset w (1 + len - n)
              Nothing → 0.0
        len = length $ filter isSlider $ dsTiles ds
loadDynData ds ((DTile (DMNULL) _ _ _ _ _):ts) = [DynData 0 (0,0) (0,0)] ⧺ loadDynData ds ts

isSlider ∷ Tile → Bool
isSlider (DTile (DMSlider _) _ _ _ _ _) = True
isSlider (DTile _            _ _ _ _ _) = False
isSlider (GTile              _ _ _ _ _) = False


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
calcSliderOffset ∷ Window → Int → Float
calcSliderOffset win n = calcWinSliderOffset (winElems win) n
calcWinSliderOffset ∷ [WinElem] → Int → Float
calcWinSliderOffset [] _ = 0.0
calcWinSliderOffset ((WinElemPane _ _ bits):wes) n = calcBitsSliderOffset bits n
calcWinSliderOffset (we:wes) n = calcWinSliderOffset wes n
calcBitsSliderOffset ∷ [(Int,PaneBit)] → Int → Float
calcBitsSliderOffset []       _ = 0.0
calcBitsSliderOffset ((i,(PaneBitSlider text mn mx val)):pbs) n = if (n ≡ i) then 6.0*val'/(mx' - mn') else calcBitsSliderOffset pbs n
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
  where bits' = moveBitsSlider (x - 0.25*(fst pos) + 0.25) n bits
moveWinSlider (we:wes) x n = [we] ⧺ moveWinSlider wes x n

moveBitsSlider ∷ Double → Int → [(Int,PaneBit)] → [(Int,PaneBit)]
moveBitsSlider _ _ [] = []
moveBitsSlider x n ((i,PaneBitSlider text mn mx val):pbs)
  | (i ≡ n)   = [(i,PaneBitSlider text mn mx val')] ⧺ moveBitsSlider x n pbs
  | otherwise = [(i,PaneBitSlider text mn mx val)]  ⧺ moveBitsSlider x n pbs
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
printBitsDynList ((i,(PaneBitSlider text mn mx val)):pbs) = (show i) ⧺ ", " ⧺ text ⧺ ": " ⧺ (show val) ⧺ "\n" ⧺ printBitsDynList pbs
printBitsDynList ((i,pb):pbs) = printBitsDynList pbs
