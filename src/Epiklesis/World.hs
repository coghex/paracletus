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
                , dsBuff = buffer }
    where buffer = case (findWorldData win') of
                     Nothing      → dsBuff ds
                     Just (wp,wd) → setTileBuff 1 dyns (dsBuff ds)
                       where dyns = calcWorldBuff (dsNDefTex ds) wp wd
          win' = case (findWorldData win) of
                   Nothing      → win
                   Just (wp,wd) → win { winElems = replaceWorldWinElem wd' (winElems win) }
                     where zoneInd   = (0,0)
                           wd'       = wd { wdZones = replaceZones newSegs zoneSize (wdZones wd) }
                           newSegs   = fixSegs wpGen $ genSegs wpGen $ evalScreenCursor segSize (-0.05*cx,-0.05*cy)
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

calcWorldBuff ∷ Int → WorldParams → WorldData → Dyns
calcWorldBuff nDefTex wp wd = Dyns $ calcSpots nDefTex wp wd

printSegs ∷ [((Int,Int),((Int,Int),Segment))] → String
printSegs [] = ""
printSegs ((zoneInd,(ind,_)):segs) = str ⧺ printSegs segs
  where str = "(" ⧺ (show zoneInd) ⧺ ", " ⧺ (show ind) ⧺ "), "

fixSegs ∷ WorldParams → [((Int,Int),Segment)] → [((Int,Int),((Int,Int),Segment))]
fixSegs _ [] = []
fixSegs wp (((i,j),seg):segs) = [((zi,zj),((i',j'),seg))] ⧺ fixSegs wp segs
  where zi      = -1 + ((i + zw) `div` zw)
        zj      = -1 + ((j + zh) `div` zh)
        i'      = (i + zw) `mod` zw
        j'      = (j + zh) `mod` zh
        (zw,zh) = wpZSize wp

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

--replaceZones ∷ [((Int,Int),((Int,Int),Segment))] → (Int,Int) → (Int,Int) → [Zone] → [Zone]
--replaceZones _    _       _        []     = []
--replaceZones segs zoneInd zoneSize (z:zs)
--  | (zoneIndex z) ≡ zoneInd = [replaceSegs segs zoneSize z] ⧺ replaceZones segs zoneInd zoneSize zs
--  | otherwise               = [z] ⧺ replaceZones segs zoneInd zoneSize zs
--
--replaceSegs ∷ [((Int,Int),((Int,Int),Segment))] → (Int,Int) → Zone → Zone
--replaceSegs []                _        z                   = z
--replaceSegs ((_,(segInd,seg)):ss) zoneSize (Zone zoneInd segs) = replaceSegs ss zoneSize $ Zone zoneInd $ segs'
--  where segs' = replaceSeg zoneSize segInd seg segs
--replaceSeg ∷ (Int,Int) → (Int,Int) → Segment → [[Segment]] → [[Segment]]
--replaceSeg zoneSize segInd newSeg segs = map (findAndReplaceSegmentRow zoneSize segInd newSeg) (zip yinds segs)
--  where yinds = take (fst zoneSize) [0..]
--findAndReplaceSegmentRow ∷ (Int,Int) → (Int,Int) → Segment → (Int,[Segment]) → [Segment]
--findAndReplaceSegmentRow zoneSize segInd newSeg (j,segs) = map (findAndReplaceSegmentSpot segInd newSeg j) (zip xinds segs)
--  where xinds = take (snd zoneSize) [0..]
--findAndReplaceSegmentSpot ∷ (Int,Int) → Segment → Int → (Int,Segment) → Segment
--findAndReplaceSegmentSpot segInd newSeg j (i,seg)
--  | (i,j) ≡ segInd = newSeg
--  | otherwise      = seg

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
calcSpots ∷ Int → WorldParams → WorldData → [DynData]
calcSpots nDefTex wp wd = calcWorldTiles nDefTex wp cam zs
  where cam = wdCam   wd
        zs  = wdZones wd

calcWorldTiles ∷ Int → WorldParams → (Float,Float) → [Zone] → [DynData]
calcWorldTiles _       _  _   []                 = []
calcWorldTiles nDefTex wp cam ((Zone ind segs):zs) = z' ⧺ calcWorldTiles nDefTex wp cam zs
  where z'    = flatten $ map (calcZoneRows nDefTex wp cam ind) (zip yinds segs)
        yinds = take (fst segS) [0..]
        segS  = wpZSize wp

calcZoneRows ∷ Int → WorldParams → (Float,Float) → (Int,Int) → (Integer,[Segment]) → [DynData]
calcZoneRows nDefTex wp cam ind (j,segs) = flatten $ map (calcZoneSpot nDefTex j' wp cam ind) (zip xinds segs)
  where xinds = take (snd segS) [0..]
        segS  = wpZSize wp
        j'    = fromIntegral j

calcZoneSpot ∷ Int → Int → WorldParams → (Float,Float) → (Int,Int) → (Integer,Segment) → [DynData]
calcZoneSpot nDefTex j wp cam ind (i,seg) = calcSegTiles nDefTex (i',j) wp roundCam ind seg
  where roundCam = ((round (fst cam)),(round (snd cam)))
        i' = fromIntegral i

calcSegTiles ∷ Int → (Int,Int) → WorldParams → (Int,Int) → (Int,Int) → Segment → [DynData]
calcSegTiles _       _     _  _   _   (SegmentNULL)  = []
calcSegTiles nDefTex (i,j) wp cam ind (Segment grid) = flatten $ calcSegRow nDefTex cam (x,y) grid
  where (x,y)   = (sw*(i + ((zw - 1)*(fst ind))),sh*(j + (zh*(snd ind))))
        (sw,sh) = wpSSize wp
        (zw,zh) = wpZSize wp

calcSegRow ∷ Int → (Int,Int) → (Int,Int) → [[Spot]] → [[DynData]]
calcSegRow _       _       _     [[]]         = [[]]
calcSegRow _       _       _     []           = []
calcSegRow nDefTex (cx,cy) (x,y) (grow:grows) = [rowTiles] ⧺ calcSegRow nDefTex (cx,cy) (x,(y+1)) grows
  where rowTiles = calcSegSpot nDefTex (cx,cy) (x,y) grow

calcSegSpot ∷ Int → (Int,Int) → (Int,Int) → [Spot] → [DynData]
calcSegSpot _       _       _     [] = []
calcSegSpot nDefTex (cx,cy) (x,y) ((Spot t c):gspots) = [dd] ⧺ calcSegSpot nDefTex (cx,cy) ((x + 1),y) gspots
  --where tile = MTile (x',y') (1,1) (ix,iy) (3,15) c'
  where dd = DynData c' (2*x',2*y') (1,1) (ix,iy)
        x' = fromIntegral x - 1
        y' = fromIntegral y - 1
        ix = t `mod` 3
        iy = t `div` 3
        c' = c + nDefTex
