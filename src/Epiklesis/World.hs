module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data

loadWorld ∷ DrawState → DrawState
loadWorld ds = case (currentWin ds) of
  Nothing  → ds
  Just win → ds { dsWins = replaceWin win' (dsWins ds) }
    where win' = case (findWorldData win) of
                   Nothing      → win
                   Just (wp,wd) → win { winElems = replaceWorldWinElem wd' (winElems win) }
                     where zoneInd   = (0,0)
                           wd'       = wd { wdZones = replaceZones newSegs zoneInd zoneSize (wdZones wd) }
                           newSegs   = genSegs wpGen $ evalScreenCursor segSize cam
                           segSize   = wpSSize wp
                           zoneSize  = wpZSize wp
                           wpGen     = wp
                           (cx,cy,_) = winCursor win
                           cam       = (cx,cy)

replaceZones ∷ [((Int,Int),Segment)] → (Int,Int) → (Int,Int) → [Zone] → [Zone]
replaceZones _    _       _        []     = []
replaceZones segs zoneInd zoneSize (z:zs)
  | (zoneIndex z) ≡ zoneInd = [replaceSegs segs zoneSize z] ⧺ replaceZones segs zoneInd zoneSize zs
  | otherwise               = [z] ⧺ replaceZones segs zoneInd zoneSize zs

replaceSegs ∷ [((Int,Int),Segment)] → (Int,Int) → Zone → Zone
replaceSegs []                _        z                   = z
replaceSegs ((segInd,seg):ss) zoneSize (Zone zoneInd segs) = replaceSegs ss zoneSize $ Zone zoneInd $ segs'
  where segs' = replaceSeg zoneSize segInd seg segs
replaceSeg ∷ (Int,Int) → (Int,Int) → Segment → [[Segment]] → [[Segment]]
replaceSeg zoneSize segInd newSeg segs = map (findAndReplaceSegmentRow zoneSize segInd newSeg) (zip yinds segs)
  where yinds = take (fst zoneSize) [0..]
findAndReplaceSegmentRow ∷ (Int,Int) → (Int,Int) → Segment → (Int,[Segment]) → [Segment]
findAndReplaceSegmentRow zoneSize segInd newSeg (j,segs) = map (findAndReplaceSegmentSpot segInd newSeg j) (zip xinds segs)
  where xinds = take (snd zoneSize) [0..]
findAndReplaceSegmentSpot ∷ (Int,Int) → Segment → Int → (Int,Segment) → Segment
findAndReplaceSegmentSpot segInd newSeg j (i,seg)
  | (i,j) ≡ segInd = newSeg
  | otherwise      = seg

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
evalScreenCursor (w,h) (cx,cy) = [pos]
  where pos = (x,y)
        x   = floor $ cx / w'
        y   = floor $ cy / h'
        w'  = fromIntegral w
        h'  = fromIntegral h

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
calcSegSpot _       _       _     [] = []
calcSegSpot nDefTex (cx,cy) (x,y) ((Spot t c):gspots) = [tile] ⧺ calcSegSpot nDefTex (cx,cy) ((x + 1),y) gspots
  where tile = MTile (x',y') (1,1) (ix,iy) (3,15) c'
        x' = fromIntegral x - 1
        y' = fromIntegral y - 1
        ix = t `mod` 3
        iy = t `div` 3
        c' = c + nDefTex

