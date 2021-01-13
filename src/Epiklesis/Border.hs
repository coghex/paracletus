module Epiklesis.Border where
-- borders between biomes are set
import Prelude()
import UPrelude
import Epiklesis.Map
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data

-- adds in corners on top of existing edges
calcGridCorners ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridCorners spots cards = map (calcRowCorners) $ zip spots cards
calcRowCorners ∷ ([Spot],[Cards Spot]) → [Spot]
calcRowCorners (spots,cards) = map (calcCorners) $ zip spots cards
calcCorners ∷ (Spot,Cards Spot) → Spot
calcCorners ((Spot c t b), Cards (n,s,e,w)) = case (n,s,e,w) of
  (Nothing,_      ,_      ,_      ) → Spot c t b
  (_      ,Nothing,_      ,_      ) → Spot c t b
  (_      ,_      ,Nothing,_      ) → Spot c t b
  (_      ,_      ,_      ,Nothing) → Spot c t b
  (Just (Spot _ n0 _),Just (Spot _ s0 _),Just (Spot _ e0 _),Just (Spot _ w0 _)) → if (nw ∨ ne ∨ sw ∨ se) then Spot c t (Just (nw,ne,sw,se)) else Spot c t b
    where nw     = nleft  ∧ wright
          ne     = nright ∧ eleft
          sw     = sright ∧ wleft
          se     = sleft  ∧ eright
          wright = (w0≡9) ∨(w0≡10)∨(w0≡18)∨(w0≡19)
          wleft  = (w0≡15)∨(w0≡16)∨(w0≡18)∨(w0≡19)
          eright = (e0≡16)∨(e0≡17)∨(e0≡19)∨(e0≡20)
          eleft  = (e0≡10)∨(e0≡11)∨(e0≡19)∨(e0≡20)
          nright = (s0≡11)∨(s0≡14)∨(s0≡21)∨(s0≡22)
          nleft  = (s0≡9) ∨(s0≡12)∨(s0≡21)∨(s0≡22)
          sright = (n0≡12)∨(n0≡15)∨(n0≡21)∨(n0≡23)
          sleft  = (n0≡14)∨(n0≡17)∨(n0≡21)∨(n0≡23)

calcCornerBuff ∷ Int → WorldParams → WorldData → [(Int,Int)] → [DynData]
calcCornerBuff _       _  _  []       = []
calcCornerBuff nDefTex wp wd (sc:scs) = calcZoneCornerBuff nDefTex zoneSize size (wdZones wd) sc' ⧺ calcCornerBuff nDefTex wp wd scs
  where sc'      = fixCurs wp sc
        zoneSize = wpZSize wp
        size     = wpSSize wp
calcZoneCornerBuff ∷ Int → (Int,Int) → (Int,Int) → [Zone] → ((Int,Int),(Int,Int)) → [DynData]
calcZoneCornerBuff _       _       _     []     _            = []
calcZoneCornerBuff nDefTex (zw,zh) (w,h) (z:zs) (zind,(i,j))
  | zind ≡ zoneIndex z = calcSegCornerBuff (zw'+w*i,zh'+h*j) nDefTex (((zoneSegs z) !! j) !! i) ⧺ calcZoneCornerBuff nDefTex (zw,zh) (w,h) zs (zind,(i,j))
  | otherwise          = calcZoneCornerBuff nDefTex (zw,zh) (w,h) zs (zind,(i,j))
  where (zw',zh') = (w*zw*(fst zind),h*zh*(snd zind))
calcSegCornerBuff ∷ (Int,Int) → Int → Segment → [DynData]
calcSegCornerBuff _   _       SegmentNULL    = []
calcSegCornerBuff ind nDefTex (Segment grid) = flatten $ map (calcGridRowCornerBuff ind nDefTex) (zip yinds grid')
  where yinds = take (length grid') [0..]
        grid' = map init $ map tail $ init $ tail grid
calcGridRowCornerBuff ∷ (Int,Int) → Int → (Int,[Spot]) → [DynData]
calcGridRowCornerBuff ind nDefTex (j,spots) = flatten $ map (calcGridCorner ind j nDefTex) (zip xinds spots)
  where xinds = take (length spots) [0..]
calcGridCorner ∷ (Int,Int) → Int → Int → (Int,Spot) → [DynData]
calcGridCorner _       _ _       (_,(Spot _ _ Nothing)) = []
calcGridCorner (cx,cy) y nDefTex (x,(Spot c _ (Just (nw,ne,sw,se)))) = ddnw ⧺ ddne ⧺ ddsw ⧺ ddse
  where ddnw = if nw then [DynData c' (2*x',2*y') (1,1) (1,8)] else []
        ddne = if ne then [DynData c' (2*x',2*y') (1,1) (0,8)] else []
        ddsw = if sw then [DynData c' (2*x',2*y') (1,1) (0,9)] else []
        ddse = if se then [DynData c' (2*x',2*y') (1,1) (2,8)] else []
        x'   = (fromIntegral cx) + (fromIntegral x)
        y'   = (fromIntegral cy) + (fromIntegral y)
        c'   = c + 1 + nDefTex

-- adds in borders where possible
setTileBorder ∷ WorldParams → [Zone] → (Float,Float) → [Zone]
setTileBorder wp zones (cx,cy) = calcBorder zones cards $ map (fixCurs wp) $ take 9 $ evalScreenCursor (wpSSize wp) (-cx/64.0,-cy/64.0)
  where cards = zoneCards zones
calcBorder ∷ [Zone] → [Cards Zone] → [((Int,Int),(Int,Int))] → [Zone]
calcBorder []     _      _    = []
calcBorder _      []     _    = []
calcBorder (z:zs) (c:cs) inds = [calcZoneBorder z c inds] ⧺ calcBorder zs cs inds
calcZoneBorder ∷ Zone → Cards Zone → [((Int,Int),(Int,Int))] → Zone
calcZoneBorder zone _     []                = zone
calcZoneBorder zone cards ((zind,ind):inds)
  | (zoneIndex zone)≡zind = calcZoneBorder zone' cards inds
  | otherwise = calcZoneBorder zone cards inds
  where zone' = Zone zind $ calcSegsBorder (zoneSegs zone) ind

calcSegsBorder ∷ [[Segment]] → (Int,Int) → [[Segment]]
calcSegsBorder segs ind = replaceSeg ind seg1 segs
  where seg1  = calcSegBorder seg0
        seg0  = (segs !! (snd ind)) !! (fst ind)
calcSegBorder ∷ Segment → Segment
calcSegBorder SegmentNULL    = SegmentNULL
calcSegBorder (Segment grid0) = Segment grid2
  where grid1   = calcGridBorder grid0 scards0
        scards0 = cardinals grid0
        grid2   = calcGridCorners grid1 scards1
        scards1 = cardinals grid1
        --grid3   = calcGridZazz grid2 scards1

calcGridBorder ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridBorder []         _            = []
calcGridBorder _          []           = []
calcGridBorder (row:grid) (crow:cgrid) = [row'] ⧺ calcGridBorder grid cgrid
  where row' = calcRowBorder row crow
calcRowBorder ∷ [Spot] → [Cards Spot] → [Spot]
calcRowBorder []         _            = []
calcRowBorder _          []           = []
calcRowBorder (spot:row) (cards:crow) = [spot'] ⧺ calcRowBorder row crow
  where spot' = calcSpotBorder spot cards
calcSpotBorder ∷ Spot → Cards Spot → Spot
calcSpotBorder (Spot c t b) (Cards (n,s,e,w)) = Spot c t' b
  where t' = if      (n' ∧ s' ∧ e' ∧ w') then 13
             else if (n' ∧ s' ∧ e'     ) then 20
             else if (n' ∧ s' ∧      w') then 18
             else if (n' ∧      e' ∧ w') then 23
             else if (     s' ∧ e' ∧ w') then 22
             else if (n' ∧ s'          ) then 19
             else if (n' ∧      e'     ) then 17
             else if (n' ∧           w') then 15
             else if (     s' ∧      w') then 9
             else if (     s' ∧ e'     ) then 11
             else if (          e' ∧ w') then 21
             else if (n'               ) then 16
             else if (     s'          ) then 10
             else if (          e'     ) then 14
             else if (               w') then 12
             else t
        n' = case n of
               Nothing → False
               Just (Spot c0 _ _)
                 | (c0 ≢ c)  → True
                 | otherwise → False
        s' = case s of
               Nothing → False
               Just (Spot c0 _ _)
                 | (c0 ≢ c)  → True
                 | otherwise → False
        e' = case e of
               Nothing → False
               Just (Spot c0 _ _)
                 | (c0 ≢ c)  → True
                 | otherwise → False
        w' = case w of
               Nothing → False
               Just (Spot c0 _ _)
                 | (c0 ≢ c)  → True
                 | otherwise → False
