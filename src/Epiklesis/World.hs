module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Buff

loadWorld ∷ DrawState → DrawState
loadWorld ds = case (currentWin ds) of
  Nothing  → ds
  Just win → ds { dsWins = replaceWin win' (dsWins ds)
                --, dsStatus = status
                , dsBuff = buffer }
    where buffer = case (findWorldData win') of
                     Nothing      → dsBuff ds
                     Just (wp,wd) → setTileBuff 1 dyns (dsBuff ds)
                       where dyns = calcWorldBuff (dsNDefTex ds) 128 wp wd $ head $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
                             segSize   = wpSSize wp
                             (cx,cy,_) = winCursor win
          status = case (findWorldData win') of
                     Nothing      → DSSNULL
                     Just (wp,wd) → DSSLogDebug $ show $ fixCurs wp $ head $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
                       where segSize   = wpSSize wp
                             (cx,cy,_) = winCursor win
          win' = case (findWorldData win) of
                   Nothing      → win
                   Just (wp,wd) → win { winElems = replaceWorldWinElem wd' (winElems win) }
                     where zoneInd   = (0,0)
                           wd'       = wd { wdZones = replaceZones newSegs zoneSize (wdZones wd) }
                           newSegs   = fixSegs wpGen $ genSegs wpGen $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
                           segSize   = wpSSize wp
                           zoneSize  = wpZSize wp
                           wpGen     = wp
                           (cx,cy,_) = winCursor win

-- calculates dyns of the world tiles
--calcWorldDSBuff ∷ WorldParams → Dyns → Dyns
--calcWorldDSBuff wp (Dyns dyns)
--  | (length dyns) ≢ (w*h) = Dyns $ take (w*h) $ repeat $ DynData 0 (0,0) (1,1) (0,0)
--  | otherwise             = Dyns dyns
--  where (w,h) = wpSSize wp

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

-- returns the list of indecies
-- of segments to generate
evalScreenCursor ∷ (Int,Int) → (Float,Float) → [(Int,Int)]
evalScreenCursor (w,h) (cx,cy) = [pos,posn,pose,poss,posw,posnw,posne,posse,possw]
  where pos   = (x,y)
        posn  = (x,y + 1)
        poss  = (x,y - 1)
        posw  = (x - 1,y)
        pose  = (x + 1,y)
        posnw = (x - 1,y - 1)
        posne = (x + 1,y - 1)
        possw = (x - 1,y + 1)
        posse = (x + 1,y + 1)
        x     = (-1) + (floor $ cx / w')
        y     = (-1) + (floor $ cy / h')
        w'    = fromIntegral w
        h'    = fromIntegral h

-- generates the segments that are
-- required by evalScreenCursor
genSegs ∷ WorldParams → [(Int,Int)] → [((Int,Int),Segment)]
genSegs _  []             = []
genSegs wp (pos:poss) = [(pos,seg)] ⧺ (genSegs wp poss)
  where seg = Segment $ seedSeg wp pos
-- generates tile list for single segment
seedSeg ∷ WorldParams → (Int,Int) → [[Spot]]
seedSeg wp pos = seedConts (sw,sh) pos conts rands zeroSeg
  where rands   = wpRands wp
        conts   = wpConts wp
        zeroSeg = take sh (zip [0..] (repeat (take sw (zip [0..] (repeat (Spot 2 2))))))
        (sw,sh) = wpSSize wp
seedConts ∷ (Int,Int) → (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [(Int,[(Int,Spot)])] → [[Spot]]
seedConts _    _   _      []     seg = flattenSeg seg
seedConts _    _   []     _      seg = flattenSeg seg
seedConts size pos (c:cs) (r:rs) seg = seedConts size pos cs rs seg'
  where seg' = seedCont size pos c r seg
flattenSeg ∷ [(Int,[(Int,Spot)])] → [[Spot]]
flattenSeg [] = []
flattenSeg ((_,row):gs) = [flattenRow row] ⧺ flattenSeg gs
flattenRow ∷ [(Int,Spot)] → [Spot]
flattenRow [] = []
flattenRow ((_,g):gs) = [g] ⧺ flattenRow gs
seedCont ∷ (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedCont size pos conts rands seg = map (seedTileRow size pos conts rands) seg
seedTileRow ∷ (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → (Int,[(Int,Spot)]) → (Int,[(Int,Spot)])
seedTileRow size pos conts rands (j,row) = (j,map (seedTile size pos conts rands j) row)
seedTile ∷ (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → Int → (Int,Spot) → (Int,Spot)
seedTile (width,height) pos (_,s) ((w,x),(y,z)) j (i,t)
  | seedDistance i' j' w x y z < (s*maxsize) = (i,t')
  | otherwise                                = (i,t)
  where t'      = Spot 3 1
        i'      = i + ((fst pos)*width)
        j'      = j + ((snd pos)*height)
        maxsize = (max width height)*(max width height)

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
calcZone nDefTex (zw,zh) (w,h) (i,j) segs = calcSpot (zw+w*i,zh+h*j) nDefTex $ (segs !! j) !! i
calcSpot ∷ (Int,Int) → Int → Segment → [DynData]
calcSpot _   _       SegmentNULL    = []
calcSpot ind nDefTex (Segment grid) = flatten $ map (calcGridRow ind nDefTex) (zip yinds grid)
  where yinds = take (length grid) [0..]
calcGridRow ∷ (Int,Int) → Int → (Int,[Spot]) → [DynData]
calcGridRow ind nDefTex (j,spots) = flatten $ map (calcGrid ind j nDefTex) (zip xinds spots)
  where xinds = take (length spots) [0..]
calcGrid ∷ (Int,Int) → Int → Int → (Int,Spot) → [DynData]
calcGrid (cx,cy) y nDefTex (x,(Spot t c)) = [dd]
  where dd = DynData c' (2*x',2*y') (1,1) (ix,iy)
        x' = (fromIntegral cx) + (fromIntegral x)
        y' = (fromIntegral cy) + (fromIntegral y)
        ix = t `mod` 3
        iy = t `div` 3
        c' = c + nDefTex
