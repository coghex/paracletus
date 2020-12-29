module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data

calcSpots ∷ Int → WorldParams → WorldData → [Tile]
calcSpots nDefTex wp wd = calcWorldTiles nDefTex wp cam zs
  where cam = wdCam   wd
        zs  = wdZones wd

calcWorldTiles ∷ Int → WorldParams → (Float,Float) → [Zone] → [Tile]
calcWorldTiles _       _  _   []                 = []
calcWorldTiles nDefTex wp cam ((Zone ind segs):zs) = z' ⧺ calcWorldTiles nDefTex wp cam zs
  where z'    = flatten $ map (calcZoneRows nDefTex wp cam ind) (zip yinds segs)
        yinds = take (fst segS) [0..]
        segS  = wpZSize wp

calcZoneRows ∷ Int → WorldParams → (Float,Float) → (Int,Int) → (Integer,[Segment]) → [Tile]
calcZoneRows nDefTex wp cam ind (j,segs) = flatten $ map (calcZoneSpot nDefTex j' wp cam ind) (zip xinds segs)
  where xinds = take (snd segS) [0..]
        segS  = wpZSize wp
        j'    = fromIntegral j

calcZoneSpot ∷ Int → Int → WorldParams → (Float,Float) → (Int,Int) → (Integer,Segment) → [Tile]
calcZoneSpot nDefTex j wp cam ind (i,seg) = calcSegTiles nDefTex (i',j) wp roundCam ind seg
  where roundCam = ((round (fst cam)),(round (snd cam)))
        i' = fromIntegral i

calcSegTiles ∷ Int → (Int,Int) → WorldParams → (Int,Int) → (Int,Int) → Segment → [Tile]
calcSegTiles _       _     _  _   _   (SegmentNULL)  = []
calcSegTiles nDefTex (i,j) wp cam ind (Segment grid) = flatten $ calcSegRow nDefTex cam (x,y) grid
  where (x,y)   = (sw*(i + (fst ind)),sh*(j + (snd ind)))
        (sw,sh) = wpSSize wp

calcSegRow ∷ Int → (Int,Int) → (Int,Int) → [[Spot]] → [[Tile]]
calcSegRow _       _       _     [[]]         = [[]]
calcSegRow _       _       _     []           = []
calcSegRow nDefTex (cx,cy) (x,y) (grow:grows) = [rowTiles] ⧺ calcSegRow nDefTex (cx,cy) (x,(y+1)) grows
  where rowTiles = calcSegSpot nDefTex (cx,cy) (x,y) grow

calcSegSpot ∷ Int → (Int,Int) → (Int,Int) → [Spot] → [Tile]
calcSegSpot nDefTex (cx,cy) (x,y) ((Spot t c):gspots) = [tile] ⧺ calcSegSpot nDefTex (cx,cy) ((x + 1),y) gspots
  where tile = MTile (x',y') (1,1) (ix,iy) (3,15) c
        x' = fromIntegral x - 1
        y' = fromIntegral y - 1
        ix = t `mod` 3
        iy = t `div` 3

