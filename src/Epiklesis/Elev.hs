module Epiklesis.Elev where
-- elevation functions modify world draw state
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Map (fixCurs)
import Epiklesis.Window
import Paracletus.Buff
import Paracletus.Data

setElevBuff ∷ Int → (Float,Float) → WorldParams → WorldData → [Dyns] → [Dyns]
setElevBuff nDefTex (cx,cy) wp wd dyns0 = dyns1
  where dyns1   = calcWorldElevBuffs 2 nDefTex wp wd curs dyns0
        curs    = take 9 $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
        segSize = wpSSize wp

calcWorldElevBuffs ∷ Int → Int → WorldParams → WorldData → [(Int,Int)] → [Dyns] → [Dyns]
calcWorldElevBuffs _ _       _  _  []       buff = buff
calcWorldElevBuffs n nDefTex wp wd (sc:scs) buff = calcWorldElevBuffs (n + 1) nDefTex wp wd scs dyns
  where dyns    = setTileBuff n (calcWorldElevBuff nDefTex (sh*sw) wp wd sc) buff
        (sw,sh) = wpSSize wp

calcWorldElevBuff ∷ Int → Int → WorldParams → WorldData → (Int,Int) → Dyns
calcWorldElevBuff nDefTex size wp wd curs = Dyns $ res ⧺ (take (size - (length res)) (repeat (DynData 0 (0,0) (1,1) (0,0))))
  where res = calcElevSpots nDefTex wp wd curs

calcElevSpots ∷ Int → WorldParams → WorldData → (Int,Int) → [DynData]
calcElevSpots nDefTex wp wd curs = calcElevZone nDefTex size zsize c zs
  where c     = fixCurs wp curs
        zs    = wdZones wd
        size  = wpSSize wp
        zsize = wpZSize wp

calcElevZone ∷ Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [Zone] → [DynData]
calcElevZone _       _       _       _          []     = []
calcElevZone nDefTex (sw,sh) (zw,zh) (zind,ind) (z:zs)
  | (zoneIndex z ≡ zind) = dd ⧺ calcElevZone nDefTex (sw,sh) (zw,zh) (zind,ind) zs
  | otherwise            = calcElevZone nDefTex (sw,sh) (zw,zh) (zind,ind) zs
  where dd       = calcElevSeg nDefTex zoneSize (sw,sh) ind (zoneSegs z)
        zoneSize = (sw*zw*(fst zind),sh*zh*(snd zind))


calcElevSeg ∷ Int → (Int,Int) → (Int,Int) → (Int,Int) → [[Segment]] → [DynData]
calcElevSeg nDefTex (zw,zh) (w,h) (i,j) segs = calcElevSpot (zw+w*i,zh+h*j) nDefTex seg
  where seg = (segs !! j) !! i
calcElevSpot ∷ (Int,Int) → Int → Segment → [DynData]
calcElevSpot _   _       SegmentNULL     = []
calcElevSpot ind nDefTex (Segment grid0) = flatten $ map (calcElevGridRow ind nDefTex) (zip yinds grid1)
  where yinds = take (length grid1) [0..]
        grid1 = map init $ map tail $ init $ tail grid0
calcElevGridRow ∷ (Int,Int) → Int → (Int,[Spot]) → [DynData]
calcElevGridRow ind nDefTex (j,spots) = flatten $ map (calcElevGrid ind j nDefTex) (zip xinds spots)
  where xinds = take (length spots) [0..]
calcElevGrid ∷ (Int,Int) → Int → Int → (Int,Spot) → [DynData]
calcElevGrid (cx,cy) y nDefTex (x,Spot _ _ _ e) = [dd]
  where dd = DynData nDefTex (2*x',2*y') (1,1) (ix,iy)
        x' = fromIntegral cx + fromIntegral x
        y' = fromIntegral cy + fromIntegral y
        ix = round e `mod` 3
        iy = round e `div` 3
