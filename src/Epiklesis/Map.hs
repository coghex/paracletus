module Epiklesis.Map where
-- display of a global map when zoomed out
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Buff
import Paracletus.Data

-- takes a grid with indices and gets rid of them
stripGrid ∷ [(α,[(α,β)])] → [[β]]
stripGrid ((a,b):ys) = (stripRow b) : stripGrid ys
stripGrid _          = [[]]
stripRow ∷ [(α,β)] → [β]
stripRow ((a,b):ys) = b : stripRow ys
stripRow _          = []

-- returns in (n,s,e,w) format surrounding elements
cardinals ∷ [[α]] → [[Cards α]]
cardinals [] = []
cardinals gs = cardinalsF (length (head gs)) gs Nothing
cardinalsF ∷ Int → [[α]] → Maybe [α] → [[Cards α]]
cardinalsF _ []     _  = []
cardinalsF w (g:gs) xn = [cardRow g xn xs Nothing] ++ (cardinalsF w gs (Just g))
  where xs = case gs of
               []  → Nothing
               gs0 → Just $ head gs0

cardRow ∷ [α] → Maybe [α] → Maybe [α] → Maybe α → [(Cards α)]
cardRow []     _               _               _       = []
cardRow (g:gs) (Nothing)       (Nothing)       xw = [Cards (Nothing,Nothing,xe,xw)] ++ (cardRow gs Nothing Nothing $ Just g)
  where xe = case gs of
               []  → Nothing
               xe0 → Just $ head xe0
cardRow (g:gs) (Just (xn:xns')) (Nothing)       xw = [Cards (Just xn,Nothing,xe,xw)] ++ (cardRow gs xns Nothing $ Just g)
  where xe  = case gs of
                []  → Nothing
                xe0 → Just $ head xe0
        xns = case xns' of
                []  → Nothing
                xn0 → Just xn0
cardRow (g:gs) (Nothing)       (Just (xs:xss')) xw = [Cards (Nothing,Just xs,xe,xw)] ++ (cardRow gs Nothing xss $ Just g)
  where xe  = case gs of
                []  → Nothing
                xe0 → Just $ head xe0
        xss = case xss' of
                []  → Nothing
                xs0 → Just xs0
cardRow (g:gs) (Just (xn:xns')) (Just (xs:xss')) xw = [Cards (Just xn,Just xs,xe,xw)] ++ (cardRow gs xns xss $ Just g)
  where xe  = case gs of
                []  → Nothing
                xe0 → Just $ head xe0
        xns = case xns' of
                []  → Nothing
                xn0 → Just xn0
        xss = case xss' of
                []  → Nothing
                xs0 → Just xs0

-- returns spots surrounding segs
cardEdges ∷ Cards Segment → ([Spot],[Spot],[Spot],[Spot])
cardEdges (Cards (n,s,e,w)) = (n',s',e',w')
  where n' = case n of
               Just (Segment grid) → last grid
               _                   → [SpotNULL]
        s' = case s of
               Just (Segment grid) → head grid
               _                   → [SpotNULL]
        e' = case e of
               Just (Segment grid) → map head grid
               _                   → [SpotNULL]
        w' = case w of
               Just (Segment grid) → map last grid
               _                   → [SpotNULL]

-- finds specified seg in zones
indexZSeg ∷ (Int,Int) → (Int,Int) → [Zone] → Segment
indexZSeg _    _   []     = SegmentNULL
indexZSeg zind ind (z:zs)
  | (zoneIndex z) ≡ zind  = indexSeg ind $ zoneSegs z
  | otherwise             = indexZSeg zind ind zs
indexSeg ∷ (Int,Int) → [[Segment]] → Segment
indexSeg (x,y) segs = (segs !! y) !! x

---- adds edges for nothings
--fixEdges ∷ ([α],[α],[α],[α]) → [[Cards α]] → [[(α,α,α,α)]]
--fixEdges _     []   = []
--fixEdges edges grid = map (fixRowEdges edges) (zip yinds grid)
--  where yinds = take (length grid) [0..]
--fixRowEdges ∷ ([α],[α],[α],[α]) → (Int,[Cards α]) → [(α,α,α,α)] 
--fixRowEdges edges (j,rows) = map (fixSpotEdges j edges) (zip xinds rows)
--  where xinds = take (length rows) [0..]
--fixSpotEdges ∷ Int → ([α],[α],[α],[α]) → (Int, Cards α) → (α,α,α,α)
--fixSpotEdges j (ne,se,ee,we) (i,(Cards (n,s,e,w))) = (n',s',e',w')
--  where n' = case n of
--               Nothing → ne !! i
--               Just n0 → n0
--        s' = case s of
--               Nothing → se !! i
--               Just s0 → s0
--        e' = case e of
--               Nothing → ee !! j
--               Just e0 → e0
--        w' = case w of
--               Nothing → we !! j
--               Just w0 → w0

-- sets buffs for the map
-- TODO: distribute across the buffers
setMapBuffs ∷ Int → (Float,Float) → WorldParams → WorldData → [Dyns] → [Dyns]
setMapBuffs nDefTex (cx,cy) wp wd oldBuff = calcMapBuffs 1 nDefTex wp wd (evalScreenCursor segSize (-cx/64.0,-cy/64)) oldBuff
  where segSize = wpSSize wp
calcMapBuffs ∷ Int → Int → WorldParams → WorldData → [(Int,Int)] → [Dyns] → [Dyns]
calcMapBuffs _ _       _  _  []       buff = buff
calcMapBuffs n nDefTex wp wd (sc:scs) buff = calcMapBuffs (n + 1) nDefTex wp wd scs dyns
  where dyns = setTileBuff n (calcMapBuff nDefTex (sh*sw) wp wd sc) buff
        (sw,sh) = wpSSize wp
calcMapBuff ∷ Int → Int → WorldParams → WorldData → (Int,Int) → Dyns
calcMapBuff nDefTex size wp wd curs = Dyns $ res ⧺ (take (size - (length res)) (repeat (DynData 0 (0,0) (1,1) (0,0))))
  where res     = calcMap nDefTex wp wd curs

calcMap ∷ Int → WorldParams → WorldData → (Int,Int) → [DynData]
calcMap nDefTex wp wd curs = [testtile]
  where testtile = DynData (nDefTex+1) (0,0) (w,h) (1,0)
        (zw,zh)   = (fromIntegral zw', fromIntegral zh')
        (zw',zh') = wpZSize wp
        (w,h)     = (fromIntegral w', fromIntegral h')
        (w',h')   = wpSSize wp

