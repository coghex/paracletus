module Paracletus.Buff where
-- a buffer of invisibly dynamic
-- tiles to manipulate
import Prelude ()
import UPrelude
import Epiklesis.Data ( Window(..), WinElem(..), PaneBit(..) )
import Epiklesis.Window ( currentWin )
import Paracletus.Data
    ( DrawState(..), Tile(..), DynMap(..)
    , Dyns(..), DynData(..), FPS(..) )
import Paracletus.Oblatum.Font ( TTFData(..), indexTTF )

-- the buffer dyns are initiated with size n, index b
initBuff ∷ [Int] → [Dyns]
initBuff []     = []
initBuff (n:ns) = dyns : initBuff ns
  where dyns    = Dyns $ take n $ repeat $ DynData 0 (0,0) (1,1) (0,0)

-- b is the buffer index, n is the size
makeBufferTiles ∷ Int → Int → [Tile]
makeBufferTiles b n
  | (n ≡ 0)   = []
  | otherwise = makeBufferTiles b (n - 1) ⧺ [tile]
  where tile = DTile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) (1,1) 0

-- loads dyns from drawState
loadDyns ∷ DrawState → Dyns
loadDyns ds = Dyns $ reverse $ loadDynData ds $ dsTiles ds
loadDynData _  []                     = []
loadDynData ds ((GTile _ _ _ _ _):ts) = [] ⧺ loadDynData ds ts
loadDynData ds ((DTile (DMBuff b n) _ _ _ _ _):ts) = [buff !! n] ⧺ loadDynData ds ts
  where Dyns buff = dsBuff ds !! b
loadDynData ds ((DTile (DMFPS n) _ _ _ _ _):ts) = [DynData dig (0,0) (1,1) (0,0)] ⧺ loadDynData ds ts
  where dig = calcDiglet n $ dsFPS ds
loadDynData ds ((DTile (DMSliderVal n d) _ _ _ _ _):ts) = [DynData dig (0,0) (1,1) (0,0)] ⧺ loadDynData ds ts
  where dig = case (currentWin (dsWins ds)) of
                Just w  → sliderDiglet (winElems w) n d
                Nothing → 0
loadDynData ds ((DTile (DMSlider n) _ _ _ _ _):ts) = [DynData 0 (x,0) (1,1) (0,0)] ⧺ loadDynData ds ts
  where x   = case (currentWin (dsWins ds)) of
                Just w  → calcSliderOffset w n
                Nothing → 0
loadDynData ds ((DTile (DMNULL)     _ _ _ _ _):ts) = [DynData 0 (0,0) (1,1) (0,0)] ⧺ loadDynData ds ts

-- set dyns in buff
setTileBuff ∷ Int → Dyns → [Dyns] → [Dyns]
setTileBuff n dyns buff = (take n buff) ⧺ [dyns] ⧺ (tail (drop n buff))

clearBuff ∷ [Dyns] → Int → [Dyns]
clearBuff buff b = setTileBuff b (clearDDs (buff !! b)) buff
clearDyns ∷ [Dyns] → [Dyns]
clearDyns dyns = map clearDDs dyns
clearDDs ∷ Dyns → Dyns
clearDDs (Dyns dds) = Dyns $ take (length dds) $ repeat $ DynData 0 (0,0) (1,1) (0,0)

-- dyns for some text, used in loading screen,
-- discards the first char, so we add a space
textDyns ∷ Int → (Double,Double) → String → Dyns
textDyns size pos str = Dyns $ strDyns size pos $ " " ⧺ str
strDyns ∷ Int → (Double,Double) → String → [DynData]
strDyns _    _     []       = []
strDyns size (x,y) (ch:str) = res ⧺ replicate (size - length res) (DynData 0 (0,0) (1,1) (0,0))
  where res  = genStrDDs x (x,y) str dyns
        dyns = replicate (length str) $ DynData 0 (0,0) (1,1) (0,0)

genStrDDs ∷ Double → (Double,Double) → String → [DynData] → [DynData]
genStrDDs _  _   _        []       = []
genStrDDs x0 pos []       (_ :dds) = [dd'] ⧺ genStrDDs x0 pos [] dds
  where dd' = DynData 0 (0,0) (1,1) (0,0)
genStrDDs x0 (x,y) ('\n':str) dds  = genStrDDs x0 (x0,y - 1) str dds
genStrDDs x0 pos (ch:str) (_ :dds) = [dd'] ⧺ genStrDDs x0 pos' str dds
  where (dd',x') = genStrDD pos ch
        pos'     = (x',(snd pos))

-- convert char to dyndata
genStrDD ∷ (Double,Double) → Char → (DynData,Double)
genStrDD (x,_) ' ' = (DynData 0 (0,0) (1,1) (0,0),x + 0.5)
genStrDD (x,y) ch  = (DynData chIndex (realToFrac(x + chX + 0.5*(1.0 - chX)),realToFrac(y + chY)) (realToFrac(0.5*chW),realToFrac(0.5*chH)) (0,0), x + chX)
  where TTFData chIndex chW chH chX chY = indexTTF ch

-- convert fps to single didget
calcDiglet ∷ Int → FPS → Int
calcDiglet _ (FPS _ _   False) = -17
calcDiglet n (FPS _ fps True)
  | fps > 0    ∧ n < 1 = calcDig n fps
  | fps > 9    ∧ n < 2 = calcDig n fps
  | fps > 99   ∧ n < 3 = calcDig n fps
  | fps > 999  ∧ n < 4 = calcDig n fps
  | otherwise          = -17
calcDig ∷ Int → Int → Int
calcDig 0 fps = fps `mod` 10
calcDig 1 fps = (fps `div` 10) `mod` 10
calcDig 2 fps = (fps `div` 100) `mod` 10
calcDig 3 fps = (fps `div` 1000) `mod` 100
calcDig _ _   = -17

-- convert slider 
calcSliderOffset ∷ Window → Int → Float
calcSliderOffset win n = calcWinSliderOffset (winElems win) n
calcWinSliderOffset ∷ [WinElem] → Int → Float
calcWinSliderOffset [] _ = 0.0
calcWinSliderOffset ((WinElemPane _ _ bits):_) n = calcBitsSliderOffset bits n
calcWinSliderOffset (_:wes) n = calcWinSliderOffset wes n
calcBitsSliderOffset ∷ [(Int,PaneBit)] → Int → Float
calcBitsSliderOffset []       _ = 0.0
calcBitsSliderOffset ((i,(PaneBitSlider _ mn mx (Just val))):pbs) n = if (n ≡ i) then 6.0*val'/(mx' - mn') else calcBitsSliderOffset pbs n
  where val' = fromIntegral val
        mn'  = fromIntegral mn
        mx'  = fromIntegral mx
calcBitsSliderOffset (_:pbs) n = calcBitsSliderOffset pbs n

-- calcs dyndata for slider val
sliderDiglet ∷ [WinElem] → Int → Int → Int
sliderDiglet []       _ _ = -36
sliderDiglet ((WinElemPane _ _ bits):_) n d = sliderBitDiglet bits n d
sliderDiglet (_:wes) n d = sliderDiglet wes n d
sliderBitDiglet ∷ [(Int,PaneBit)] → Int → Int → Int
sliderBitDiglet []       _ _ = -36
sliderBitDiglet ((i,(PaneBitSlider _ _ _ (Just val))):pbs) n d
  | n ≡ i = calcSliderDig d val
  | otherwise = sliderBitDiglet pbs n d
sliderBitDiglet (_:pbs) n d = sliderBitDiglet pbs n d
calcSliderDig ∷ Int → Int → Int
calcSliderDig d val
  | val > 0   ∧ d < 1 = calcDig d val
  | val > 9   ∧ d < 2 = calcDig d val
  | val > 99  ∧ d < 3 = calcDig d val
  | val > 999 ∧ d < 4 = calcDig d val
  | otherwise         = -36

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

