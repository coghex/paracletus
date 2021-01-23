module Epiklesis.Map where
-- display of a global map when zoomed out
import Prelude()
import UPrelude
import Epiklesis.Data

indexWorldZones ∷ (Int,Int) → WorldParams → WorldData → Maybe Spot
indexWorldZones (i,j) wp wd = indexWorldZone (i,j) wp $ wdZones wd
indexWorldZone ∷ (Int,Int) → WorldParams → [Zone] → Maybe Spot
indexWorldZone _     _  []     = Nothing
indexWorldZone (i,j) wp (z:zs)
  | (zoneIndex z) ≡ (zi,zj) = indexWorldSegs (si,sj) wp $ zoneSegs z
  | otherwise               = indexWorldZone (i,j) wp zs
  where (zi,zj) = (i `div` zwidth, j `div` zheight)
        (si,sj) = (i `mod` zwidth, j `mod` zheight)
        (zwidth,zheight) = (zw*sw,zh*sh)
        (zw,zh) = wpZSize wp
        (sw,sh) = wpSSize wp
indexWorldSegs ∷ (Int,Int) → WorldParams → [[Segment]] → Maybe Spot
indexWorldSegs (i,j) wp segs = indexWorldSeg (gi,gj) $ (segs !! sj) !! si
  where (si,sj) = (i `div` swidth, j `div` sheight)
        (gi,gj) = (i `mod` swidth, j `mod` sheight)
        (swidth,sheight) = wpSSize wp
indexWorldSeg ∷ (Int,Int) → Segment → Maybe Spot
indexWorldSeg _     SegmentNULL    = Nothing
indexWorldSeg (i,j) (Segment grid) = Just $ (grid !! (j + 2)) !! (i + 2)

-- takes a grid with indices and gets rid of them
stripGrid ∷ [(α,[(α,β)])] → [[β]]
stripGrid ((_,b):ys) = (stripRow b) : stripGrid ys
stripGrid _          = [[]]
stripRow ∷ [(α,β)] → [β]
stripRow ((_,b):ys) = b : stripRow ys
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
cardRow (g:gs) xns             (Just [])       xw = cardRow (g:gs) xns Nothing xw
cardRow (g:gs) (Just [])       xss             xw = cardRow (g:gs) Nothing xss xw
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

zoneCardInds ∷ [Cards Zone] → String
zoneCardInds [] = ""
zoneCardInds ((Cards (n,s,e,w)):cards) = n' ⧺ s' ⧺ e' ⧺ w' ⧺ "\n" ⧺ zoneCardInds cards
  where n' = case n of
               Nothing → "NULL, "
               Just n0 → show (zoneIndex n0) ⧺ ", "
        s' = case s of
               Nothing → "NULL, "
               Just s0 → show (zoneIndex s0) ⧺ ", "
        e' = case e of
               Nothing → "NULL, "
               Just e0 → show (zoneIndex e0) ⧺ ", "
        w' = case w of
               Nothing → "NULL, "
               Just w0 → show (zoneIndex w0) ⧺ ", "

-- finds cards for zones
zoneCards ∷ [Zone] → [Cards Zone]
zoneCards zones = zoneCardsF zones zones
zoneCardsF ∷ [Zone] → [Zone] → [Cards Zone]
zoneCardsF _  []     = []
zoneCardsF z0 (z:zs) = [cards] ⧺ zoneCardsF z0 zs
  where cards = zoneCardsR (zoneIndex z) z0 $ Cards (Nothing,Nothing,Nothing,Nothing)
zoneCardsR ∷ (Int,Int) → [Zone] → Cards Zone → Cards Zone
zoneCardsR _     []                    cards = cards
zoneCardsR (i,j) ((Zone (zi,zj) g):zs) (Cards (n,s,e,w))
  | (i - 1) ≡ zi ∧ j ≡ zj = zoneCardsR (i,j) zs $ Cards (n,s,e,(Just (Zone (zi,zj) g)))
  | (i + 1) ≡ zi ∧ j ≡ zj = zoneCardsR (i,j) zs $ Cards (n,s,(Just (Zone (zi,zj) g)),w)
  | (j - 1) ≡ zj ∧ i ≡ zi = zoneCardsR (i,j) zs $ Cards (n,(Just (Zone (zi,zj) g)),e,w)
  | (j + 1) ≡ zj ∧ i ≡ zi = zoneCardsR (i,j) zs $ Cards ((Just (Zone (zi,zj) g)),s,e,w)
  | otherwise    = zoneCardsR (i,j) zs $ Cards (n,s,e,w)

-- adds in the edges from nearby zones
edgeCards ∷ (Int,Int) → Cards [Segment] → Cards Segment → Cards Segment
edgeCards (i,j) (Cards (en,es,ee,ew)) (Cards (n,s,e,w)) = Cards (n',s',e',w')
  where n' = case n of
               Nothing → case en of
                           Nothing → Nothing
                           Just g0 → Just $ g0 !! j
               Just n0 → Just n0
        s' = case s of
               Nothing → case es of
                           Nothing → Nothing
                           Just g0 → Just $ g0 !! j
               Just s0 → Just s0
        e' = case e of
               Nothing → case ee of
                           Nothing → Nothing
                           Just g0 → Just $ g0 !! i
               Just e0 → Just e0
        w' = case w of
               Nothing → case ew of
                           Nothing → Nothing
                           Just g0 → Just $ g0 !! i
               Just w0 → Just w0

-- returns edge segments from nearby zone
zoneEdges ∷ Cards Zone → Cards [Segment]
zoneEdges (Cards (n,s,e,w)) = Cards (n',s',e',w')
  where n' = case n of
               Nothing → Nothing
               Just n0 → Just $ last (zoneSegs n0)
        s' = case s of
               Nothing → Nothing
               Just s0 → Just $ head (zoneSegs s0)
        e' = case e of
               Nothing → Nothing
               Just e0 → Just $ map head (zoneSegs e0)
        w' = case w of
               Nothing → Nothing
               Just w0 → Just $ map last (zoneSegs w0)

segEdges ∷ Cards Segment → Cards [Spot]
segEdges (Cards (n,s,e,w)) = Cards (n',s',e',w')
  where n' = case n of
               Just (Segment grid) → Just $ last grid
               _                   → Nothing
        s' = case s of
               Just (Segment grid) → Just $ head grid
               _                   → Nothing
        e' = case e of
               Just (Segment grid) → Just $ map head grid
               _                   → Nothing
        w' = case w of
               Just (Segment grid) → Just $ map last grid
               _                   → Nothing

-- finds specified seg in zones
indexZSeg ∷ (Int,Int) → (Int,Int) → [Zone] → Segment
indexZSeg _    _   []     = SegmentNULL
indexZSeg zind ind (z:zs)
  | (zoneIndex z) ≡ zind  = indexSeg ind $ zoneSegs z
  | otherwise             = indexZSeg zind ind zs
indexSeg ∷ (Int,Int) → [[Segment]] → Segment
indexSeg (x,y) segs = (segs !! y) !! x

-- finds specific zone
indexZone ∷ (Int,Int) → (Int,Int) → [Zone] → Zone
indexZone (w,h) ind []     = Zone ind $ take h $ repeat $ take w $ repeat $ SegmentNULL
indexZone size  ind (z:zs)
  | (zoneIndex z) ≡ ind = z
  | otherwise           = indexZone size ind zs

fixCurs ∷ WorldParams → (Int,Int) → ((Int,Int),(Int,Int))
fixCurs wp (i,j) = ((zi,zj),(i',j'))
  where zi      = ((1 + i) `div` zw)
        zj      = ((1 + j) `div` zh)
        i'      = (1 + i + zw) `mod` zw
        j'      = (1 + j + zh) `mod` zh
        (zw,zh) = wpZSize wp

-- formula for an ellipse
seedDistance ∷ Int → Int → Int → Int → Int → Int → Int
seedDistance x1 y1 x2 y2 x3 y3 = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2
