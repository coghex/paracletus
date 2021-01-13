module Epiklesis.Elev where
-- we find the elevation of the map
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Map (fixCurs,seedDistance)
import Epiklesis.Window (evalScreenCursor)

-- mirrors the seedConts function so as
-- to generate the elevation based on conts
seedElev ∷ (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [(Int,[(Int,Elev)])] → [(Int,[(Int,Elev)])]
seedElev _   []     _      grid = grid
seedElev ind (c:cs) (r:rs) grid = seedElev ind cs rs elevs
  where elevs = seedElevs ind c r grid
seedElevs ∷ (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,[(Int,Elev)])] → [(Int,[(Int,Elev)])]
seedElevs _   _ _ []             = []
seedElevs ind c r ((j,row):grid) = [(j,(rowElevs j ind c r row))] ⧺ seedElevs ind c r grid
rowElevs ∷ Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,Elev)] → [(Int,Elev)]
rowElevs _ _   _ _ []             = []
rowElevs j ind c r ((i,spot):row) = [(i,spotElevs i j ind c r spot)] ⧺ rowElevs j ind c r row
spotElevs ∷ Int → Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → Elev → Elev
spotElevs i j (zi,zj) (rand,cont) ((w,x),(y,z)) (Elev n)
  | seedDistance i' j' w x y z < (rand*maxS) = Elev (n + 1)
  | otherwise                                = Elev n
  where maxS = 100000
        i'   = i + zi
        j'   = j + zj

-- finds elevation at a certain screen cursor
-- only really useful for debugging
elevAt ∷ (Float,Float,Float) → WorldParams → WorldData → Elev
elevAt (cx,cy,_) wp wd = elevAtCurs fixedcs $ wdZones wd
  where fixedcs = fixCurs wp scs
        scs     = head $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
        segSize = wpSSize wp

elevAtCurs ∷ ((Int,Int),(Int,Int)) → [Zone] → Elev
elevAtCurs _          []     = Elev $ -1
elevAtCurs (zind,ind) (z:zs)
  | (zind ≡ zoneIndex z) = elevAtZone (zoneSegs z) ind
  | otherwise            = elevAtCurs (zind,ind) zs
elevAtZone ∷ [[Segment]] → (Int,Int) → Elev
elevAtZone segs (i,j) = elevAtSeg $ (segs !! j) !! i
elevAtSeg ∷ Segment → Elev
elevAtSeg SegmentNULL   = Elev $ -1
elevAtSeg (Segment _ e) = head $ head $ e
