module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Rand
import Epiklesis.Map
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Buff

-- world parameters set from argV
genWorldParams ∷ UserWorldParams → WorldParams → WorldParams
genWorldParams uwp wp = wp { wpRands = rands
                           , wpConts = conts }
  where rands   = genRands sg0 sg1 ncont w h
        conts   = genConts sg0 sg1 ncont
        sg0     = (wpStdGs wp) !! 0
        sg1     = (wpStdGs wp) !! 1
        (w,h)   = ((fst (wpZSize wp))*(fst (wpSSize wp))*w', (snd (wpZSize wp))*(snd (wpSSize wp))*h')
        (w',h') = (uwpWidth uwp, uwpHeight uwp)
        ncont   = uwpNConts uwp

-- world gets loaded constantly, for many reasons
loadWorld ∷ DrawState → DrawState
loadWorld ds = case (currentWin ds) of
  Nothing  → ds
  Just win → ds { dsWins = replaceWin win' (dsWins ds)
                --, dsStatus = status
                , dsBuff = buffer }
    where buffer = case (findWorldData win') of
                     Nothing      → dsBuff ds
                     Just (wp,wd)
                       | (cz < -0.3)  → setTileBuffs (dsNDefTex ds) (cx,cy) wp wd (dsBuff ds)
                       | otherwise → setMapBuffs (dsNDefTex ds) (cx,cy) wp wd (dsBuff ds)
                       where (cx,cy,_) = winCursor win
          (cz,win') = case (findWorldData win) of
            Nothing      → (-1, win)
            Just (wp,wd) → (cz, win { winElems = replaceWorldWinElem wd' (winElems win) })
              where wd'        = wd { wdZones = zones1 }
                    zones1     = setTileBorder wp zones0 (cx,cy)
                    zones0     = replaceZones newSegs0 zoneSize (wdZones wd)
                    newSegs0   = setTileData wp wd (cx,cy)--fixSegs wpGen $ genSegs wpGen $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
                    zoneSize   = wpZSize wp
                    (cx,cy,cz) = winCursor win

-- proivides world data as dyndata,
-- generates conts and tiles
setTileData ∷ WorldParams → WorldData → (Float,Float) → [((Int,Int),((Int,Int),Segment))]
setTileData wp wd (cx,cy) = genTileData wp wd fixedcs
  where fixedcs = map (fixCurs wp) scs
        scs     = evalScreenCursor segSize (-cx/64.0,-cy/64.0)
        segSize = wpSSize wp
genTileData ∷ WorldParams → WorldData → [((Int,Int),(Int,Int))] → [((Int,Int),((Int,Int),Segment))]
genTileData _  _  []                = []
genTileData wp wd ((zind,ind):inds) = [(zind,(ind,seg))] ⧺ genTileData wp wd inds
  where seg = genSegData wp wd zind ind
genSegData ∷ WorldParams → WorldData → (Int,Int) → (Int,Int) → Segment
genSegData wp wd zind ind = case seg of
  Segment grid → Segment grid
  SegmentNULL  → Segment $ stripGrid $ seg1
  where seg1    = seedConts ind' conts rands zeroSeg
        seg     = indexSeg ind $ zoneSegs zone
        zone    = indexZone (zw,zh) zind zones
        zones   = wdZones wd
        rands   = wpRands wp
        conts   = wpConts wp
        (sw,sh) = wpSSize wp
        (zw,zh) = wpZSize wp
        zeroSeg = take sh (zip [0..] (repeat (take sw (zip [0..] (repeat (Spot 1 0))))))
        ind'    = ((((fst ind)*sw) + ((fst zind)*zw*sw)), (((snd ind)*sh) + ((snd zind)*zh*sh)))

seedConts ∷ (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedConts _   []     _      grid = grid
seedConts ind (c:cs) (r:rs) grid = seedConts ind cs rs spots
  where spots = seedSpots ind c r grid
seedSpots ∷ (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedSpots _   _ _ []             = []
seedSpots ind c r ((j,row):grid) = [(j,(rowSpots j ind c r row))] ⧺ seedSpots ind c r grid
rowSpots ∷ Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,Spot)] → [(Int,Spot)]
rowSpots _ _   _ _ []             = []
rowSpots j ind c r ((i,spot):row) = [(i,spotSpots i j ind c r spot)] ⧺ rowSpots j ind c r row
spotSpots ∷ Int → Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → Spot → Spot
spotSpots i j (zi,zj) (rand,cont) ((w,x),(y,z)) (Spot c t)
  | seedDistance i' j' w x y z < (rand*maxS) = Spot c' t'
  | otherwise                                = Spot c t
  where c'   = cont
        t'   = 0
        i'   = i + zi
        j'   = j + zj
        maxS = 100000

-- adds in borders where possible
setTileBorder ∷ WorldParams → [Zone] → (Float,Float) → [Zone]
setTileBorder wp zones (cx,cy) = calcBorder zones $ map (fixCurs wp) $ take 9 $ evalScreenCursor (wpSSize wp) (-cx/64.0,-cy/64.0)
calcBorder ∷ [Zone] → [((Int,Int),(Int,Int))] → [Zone]
calcBorder []     _    = []
calcBorder (z:zs) inds = [calcZoneBorder z inds] ⧺ calcBorder zs inds
calcZoneBorder ∷ Zone → [((Int,Int),(Int,Int))] → Zone
calcZoneBorder zone []                = zone
calcZoneBorder zone ((zind,ind):inds)
  | (zoneIndex zone)≡zind = calcZoneBorder zone' inds
  | otherwise = calcZoneBorder zone inds
  where zone' = Zone zind $ calcSegsBorder (zoneSegs zone) ind

calcSegsBorder ∷ [[Segment]] → (Int,Int) → [[Segment]]
calcSegsBorder segs ind = replaceSeg ind seg1 segs
  where seg1  = calcSegBorder seg0 cards
        seg0  = (segs !! (snd ind)) !! (fst ind)
        cards = ((cardinals segs) !! (snd ind)) !! (fst ind)
calcSegBorder ∷ Segment → Cards Segment → Segment
calcSegBorder SegmentNULL    _     = SegmentNULL
calcSegBorder (Segment grid) cards = Segment grid'
  where grid'  = calcGridBorder grid scards
        scards = cardinals grid
calcGridBorder ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridBorder []         _            = []
calcGridBorder (row:grid) (crow:cgrid) = [row'] ⧺ calcGridBorder grid cgrid
  where row' = calcRowBorder row crow
calcRowBorder ∷ [Spot] → [Cards Spot] → [Spot]
calcRowBorder []         _            = []
calcRowBorder _          []           = []
calcRowBorder (spot:row) (cards:crow) = [spot'] ⧺ calcRowBorder row crow
  where spot' = calcSpotBorder spot cards
calcSpotBorder ∷ Spot → Cards Spot → Spot
calcSpotBorder (Spot c t) (Cards (n,s,e,w)) = Spot c t'
  where t' = if      (n' ∧ s' ∧ e' ∧ w') then 2
             else if (n' ∧ s' ∧ e'     ) then 2
             else if (n' ∧ s' ∧      w') then 2
             else if (n' ∧      e' ∧ w') then 2
             else if (     s' ∧ e' ∧ w') then 2
             else if (n' ∧ s'          ) then 2
             else if (n' ∧      e'     ) then 2
             else if (n' ∧           w') then 2
             else if (     s' ∧      w') then 2
             else if (     s' ∧ e'     ) then 2
             else if (          e' ∧ w') then 2
             else if (n'               ) then 2
             else if (     s'          ) then 2
             else if (          e'     ) then 2
             else if (               w') then 2
             else t
        n' = case n of
               Nothing → False
               Just (Spot c0 t0)
                 | (c0 ≢ c)  → True
                 | otherwise → False
        s' = case s of
               Nothing → False
               Just (Spot c0 t0)
                 | (c0 ≢ c)  → True
                 | otherwise → False
        e' = case e of
               Nothing → False
               Just (Spot c0 t0)
                 | (c0 ≢ c)  → True
                 | otherwise → False
        w' = case w of
               Nothing → False
               Just (Spot c0 t0)
                 | (c0 ≢ c)  → True
                 | otherwise → False

-- proivides world data as dyndata,
-- loads from window object
setTileBuffs ∷ Int → (Float,Float) → WorldParams → WorldData → [Dyns] → [Dyns]
setTileBuffs nDefTex (cx,cy) wp wd oldBuff = calcWorldBuffs 1 nDefTex wp wd (take 9 (evalScreenCursor segSize (-cx/64.0,-cy/64.0))) oldBuff
  where segSize   = wpSSize wp
calcWorldBuffs ∷ Int → Int → WorldParams → WorldData → [(Int,Int)] → [Dyns] → [Dyns]
calcWorldBuffs _ _       _  _  []       buff = buff
calcWorldBuffs n nDefTex wp wd (sc:scs) buff = calcWorldBuffs (n + 1) nDefTex wp wd scs dyns
  where dyns = setTileBuff n (calcWorldBuff nDefTex (sh*sw) wp wd sc) buff
        (sw,sh) = wpSSize wp

calcWorldBuff ∷ Int → Int → WorldParams → WorldData → (Int,Int) → Dyns
calcWorldBuff nDefTex size wp wd curs = Dyns $ res ⧺ (take (size - (length res)) (repeat (DynData 0 (0,0) (1,1) (0,0))))
  where res = calcSpots nDefTex wp wd curs

fixSegs ∷ WorldParams → [((Int,Int),Segment)] → [((Int,Int),((Int,Int),Segment))]
fixSegs _ [] = []
fixSegs wp (((i,j),seg):segs) = [((zi,zj),((i',j'),seg))] ⧺ fixSegs wp segs
  where zi      = ((1 + i) `div` zw)
        zj      = ((1 + j) `div` zh)
        i'      = (1 + i + zw) `mod` zw
        j'      = (1 + j + zh) `mod` zh
        (zw,zh) = wpZSize wp

-- pretty printers
printWorldParams ∷ String → Window → String
printWorldParams param win = printElemWPs param (winElems win)
printElemWPs ∷ String → [WinElem] → String
printElemWPs _     []       = "no world defined"
printElemWPs "sSize" ((WinElemWorld wp _ _):_) = "seg size: " ⧺ (show (wpSSize wp))
printElemWPs "zSize" ((WinElemWorld wp _ _):_) = "zone size: " ⧺ (show (wpZSize wp))
printElemWPs "rands" ((WinElemWorld wp _ _):_) = "rands: " ⧺ (show (wpRands wp))
printElemWPs "conts" ((WinElemWorld wp _ _):_) = "conts: " ⧺ (show (wpConts wp))
printElemWPs param   ((WinElemWorld wp _ _):_) = "no world parameter " ⧺ param ⧺ " present"
printElemWPs param (wp:wps) = printElemWPs param wps

printSegs ∷ [((Int,Int),((Int,Int),Segment))] → String
printSegs [] = ""
printSegs ((zoneInd,(ind,_)):segs) = str ⧺ printSegs segs
  where str = "(" ⧺ (show zoneInd) ⧺ ", " ⧺ (show ind) ⧺ "), "

replaceZones ∷ [((Int,Int),((Int,Int),Segment))] → (Int,Int) → [Zone] → [Zone]
replaceZones []     _        zs = zs
replaceZones (s:ss) zoneSize zs = replaceZones ss zoneSize $ replaceSegs False s zoneSize zs
replaceSegs ∷ Bool → ((Int,Int),((Int,Int),Segment)) → (Int,Int) → [Zone] → [Zone]
replaceSegs True  _                   _        []     = []
replaceSegs False (zoneInd,(ind,seg)) zoneSize []     = replaceZones [(zoneInd,(ind,seg))] zoneSize $ [Zone zoneInd (initSegs)]
  where (w,h) = zoneSize
        initSegs = take h $ repeat $ take w $ repeat $ SegmentNULL
replaceSegs bool  (zoneInd,(ind,seg)) zoneSize ((Zone zind segs):zs)
  | zind ≡ zoneInd = [Zone zind (replaceSeg ind seg segs)] ⧺ replaceSegs True (zoneInd,(ind,seg)) zoneSize zs
  | otherwise      = [Zone zind segs] ⧺ replaceSegs bool (zoneInd,(ind,seg)) zoneSize zs

replaceSeg ∷ (Int,Int) → Segment → [[Segment]] → [[Segment]]
replaceSeg ind seg segs = map (findAndReplaceSegmentRow ind seg) (zip yinds segs)
  where yinds = take (length segs) [0..]
findAndReplaceSegmentRow ∷ (Int,Int) → Segment → (Int,[Segment]) → [Segment]
findAndReplaceSegmentRow ind seg (j,segs) = map (findAndReplaceSegmentSpot ind seg j) (zip xinds segs)
  where xinds = take (length segs) [0..]
findAndReplaceSegmentSpot ∷ (Int,Int) → Segment → Int → (Int,Segment) → Segment
findAndReplaceSegmentSpot ind seg0 j (i,seg)
  | (i,j) ≡ ind = seg0
  | otherwise   = seg

replaceWorldWinElem ∷ (WorldData) → [WinElem] → [WinElem]
replaceWorldWinElem _   [] = []
replaceWorldWinElem wd0 ((WinElemWorld wp _  dp):wes) = [WinElemWorld wp wd0 dp] ⧺ replaceWorldWinElem wd0 wes
replaceWorldWinElem wd0 (we:wes) = [we] ⧺ replaceWorldWinElem wd0 wes

findWorldData ∷ Window → Maybe (WorldParams,WorldData)
findWorldData win = findWorldDataElems (winElems win)
findWorldDataElems ∷ [WinElem] → Maybe (WorldParams,WorldData)
findWorldDataElems [] = Nothing
findWorldDataElems ((WinElemWorld wp wd dp):wes) = Just (wp,wd)
findWorldDataElems (_:wes) = findWorldDataElems wes

findWorldDataM ∷ Maybe Window → Maybe (WorldParams,WorldData)
findWorldDataM Nothing    = Nothing
findWorldDataM (Just win) = findWorldDataElems (winElems win)

seedDistance ∷ Int → Int → Int → Int → Int → Int → Int
seedDistance x1 y1 x2 y2 x3 y3 = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

-- world generation
calcSpots ∷ Int → WorldParams → WorldData → (Int,Int) → [DynData]
calcSpots nDefTex wp wd curs = calcSeg nDefTex size zsize c zs
  where c     = fixCurs wp curs
        zs    = wdZones wd
        size  = wpSSize wp
        zsize = wpZSize wp
fixCurs ∷ WorldParams → (Int,Int) → ((Int,Int),(Int,Int))
fixCurs wp (i,j) = ((zi,zj),(i',j'))
  where zi      = ((1 + i) `div` zw)
        zj      = ((1 + j) `div` zh)
        i'      = (1 + i + zw) `mod` zw
        j'      = (1 + j + zh) `mod` zh
        (zw,zh) = wpZSize wp
calcSeg ∷ Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [Zone] → [DynData]
calcSeg _       _    _       _                []     = []
calcSeg nDefTex size (zw,zh) (zoneInd,segInd) (z:zs)
  | (zoneIndex z ≡ zoneInd) = dd ⧺ calcSeg nDefTex size (zw,zh) (zoneInd,segInd) zs
  | otherwise               = calcSeg nDefTex size (zw,zh) (zoneInd,segInd) zs
  where dd = calcZone nDefTex zoneSize size segInd (zoneSegs z)
        zoneSize = (sw*zw*(fst zoneInd),sh*zh*(snd zoneInd))
        (sw,sh)  = size
calcZone ∷ Int → (Int,Int) → (Int,Int) → (Int,Int) → [[Segment]] → [DynData]
calcZone nDefTex (zw,zh) (w,h) (i,j) segs = calcSpot (zw+w*i,zh+h*j) nDefTex seg cards
  where seg   = (segs !! j) !! i
        cards = ((cardinals segs) !! j) !! i
calcSpot ∷ (Int,Int) → Int → Segment → Cards Segment → [DynData]
calcSpot _   _       SegmentNULL    cards = []
calcSpot ind nDefTex (Segment grid) cards = flatten $ map (calcGridRow ind nDefTex) (zip yinds grid)
  where yinds = take (length grid) [0..]
calcGridRow ∷ (Int,Int) → Int → (Int,[Spot]) → [DynData]
calcGridRow ind nDefTex (j,spots) = flatten $ map (calcGrid ind j nDefTex) (zip xinds spots)
  where xinds = take (length spots) [0..]
calcGrid ∷ (Int,Int) → Int → Int → (Int,Spot) → [DynData]
calcGrid (cx,cy) y nDefTex (x,(Spot c t)) = [dd]
  where dd = DynData c' (2*x',2*y') (1,1) (ix,iy)
        x' = (fromIntegral cx) + (fromIntegral x)
        y' = (fromIntegral cy) + (fromIntegral y)
        ix = t `mod` 3
        iy = t `div` 3
        c' = c + nDefTex
