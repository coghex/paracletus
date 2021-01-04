module Paracletus.Buff where
-- a buffer of invisibly dynamic
-- tiles to manipulate
import Prelude ()
import UPrelude
import Data.List.Split (splitOn)
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Oblatum.Font

initBuff ∷ [Dyns]
initBuff = [shDyns,wDyns,wDyns,wDyns,wDyns,wDyns,wDyns,wDyns,wDyns,wDyns,wDyns]
  where shDyns = Dyns $ take 64 $ repeat $ DynData 0 (0,0) (1,1) (0,0)
        wDyns  = Dyns $ take 128 $ repeat $ DynData 0 (0,0) (1,1) (0,0)

loadTileBuff ∷ [Tile]
loadTileBuff = makeTileBuff 0 64

loadWorldBuff ∷ [Tile]
loadWorldBuff = makeWTileBuff 1 128 ⧺ makeWTileBuff 2 128 ⧺ makeWTileBuff 3 128 ⧺ makeWTileBuff 4 128 ⧺ makeWTileBuff 5 128

loadAuxBuff ∷ [Tile]
loadAuxBuff = makeATileBuff 6 128 ⧺ makeATileBuff 7 128 ⧺ makeATileBuff 8 128 ⧺ makeATileBuff 9 128

makeWTileBuff ∷ Int → Int → [Tile]
makeWTileBuff b n
  | (n ≡ 0)   = []
  | otherwise = makeWTileBuff b (n - 1) ⧺ [tile]
  where tile = DMTile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) (3,20) 0

makeATileBuff ∷ Int → Int → [Tile]
makeATileBuff b n
  | (n ≡ 0)   = []
  | otherwise = makeATileBuff b (n - 1) ⧺ [tile]
  where tile = ATile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) (3,20) 0

makeTileBuff ∷ Int → Int → [Tile]
makeTileBuff b n
  | (n ≡ 0)   = []
  | otherwise = makeTileBuff b (n - 1) ⧺ [tile]
  where tile = DTile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) (1,1) 0

-- set dd in dyns
setTileDyns ∷ Int → DynData → Dyns → Dyns
setTileDyns n dd (Dyns dyns) = Dyns $ setTileDynsF n dd dyns
setTileDynsF ∷ Int → DynData → [DynData] → [DynData]
setTileDynsF n dd dyns = (take n dyns) ⧺ [dd] ⧺ (tail (drop n dyns))

-- set dyns in buff
setTileBuff ∷ Int → Dyns → [Dyns] → [Dyns]
setTileBuff n dyns buff = (take n buff) ⧺ [dyns] ⧺ (tail (drop n buff))

clearBuff ∷ [Dyns] → Int → [Dyns]
clearBuff buff b = setTileBuff b (clearDDs (buff !! b)) buff
clearDyns ∷ [Dyns] → [Dyns]
clearDyns dyns = map clearDDs dyns
clearDDs ∷ Dyns → Dyns
clearDDs (Dyns dds) = Dyns $ take (length dds) $ repeat $ DynData 0 (0,0) (1,1) (0,0)

-- set a buff to the current world screen
genWorldBuff ∷ [Dyns] → Int → WorldData → [Dyns]
genWorldBuff buff b wd = setTileBuff b dyns buff
  where dyns = genWorldDyns (buff !! b)
genWorldDyns ∷ Dyns → Dyns
genWorldDyns dyns = dyns

-- set a buff to equal a string
genShBuff ∷ [Dyns] → Int → Shell → [Dyns]
genShBuff buff b sh
  | (shOpen sh)        = setTileBuff b dyns buff
  | otherwise          = clearBuff buff b
  where dyns = genStrDyns pos str (buff !! b)
        pos  = (-13.0,10.0 - 2.0*y)
        y    = fromIntegral $ length $ splitOn "\n" $ shOutStr sh
        str  = shInpStr sh
genStrDyns ∷ (Double,Double) → String → Dyns → Dyns
genStrDyns pos str (Dyns dyns) = Dyns $ genStrDDs pos str dyns
genStrDDs ∷ (Double,Double) → String → [DynData] → [DynData]
genStrDDs _   _        []       = []
genStrDDs pos []       (_ :dds) = [dd'] ⧺ genStrDDs pos [] dds
  where dd' = DynData 0 (0,0) (1,1) (0,0)
genStrDDs pos (ch:str) (_ :dds) = [dd'] ⧺ genStrDDs pos' str dds
  where (dd',x') = genStrDD pos ch
        pos'     = (x',(snd pos))

-- convert char to dyndata
genStrDD ∷ (Double,Double) → Char → (DynData,Double)
genStrDD (x,y) ' ' = (DynData 0 (0,0) (1,1) (0,0),x + 0.5)
genStrDD (x,y) ch  = (DynData chIndex (realToFrac(x + chX + 0.5*(1.0 - chX)),realToFrac(y + chY)) (realToFrac(0.5*chW),realToFrac(0.5*chH)) (0,0), x + chX)
  where TTFData chIndex chW chH chX chY = indexTTF TextSize30px ch
