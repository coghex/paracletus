module Epiklesis.World where
import Prelude()
import UPrelude
import Epiklesis.Data
    ( WinElem(..), WorldParams(..), WorldData(..)
    , Zone(..), Segment(..), Spot(..), Window(..) )
import Epiklesis.Map
import Epiklesis.Window
import Paracletus.Buff ( setTileBuff )
import Paracletus.Data
    ( Dyns(..), DynData(..), DrawState(..) )

-- returns the first world in the winelems
findWorld ∷ [WinElem] → Maybe (WorldParams, WorldData)
findWorld []                           = Nothing
findWorld ((WinElemWorld wp wd _):wes) = Just (wp,wd)
findWorld (we:wes)                     = findWorld wes

-- replaces the data of the first world in winelems
replaceWorldData ∷ [WinElem] → WorldData → [WinElem]
replaceWorldData []                              _  = []
replaceWorldData ((WinElemWorld wp _ dir):elems) wd = [WinElemWorld wp wd dir] ⧺ elems
replaceWorldData (elem:elems)                    wd = [elem] ⧺ replaceWorldData elems wd

-- prints the first world found nicely
printWorld ∷ Maybe Window → String
printWorld Nothing  = "no window found"
printWorld (Just w) = case (findWorld (winElems w)) of
  Nothing      → "no world found"
  Just (_, wd) → printWorldData wd
printWorldData ∷ WorldData → String
printWorldData wd = show $ wdZones wd
-- just prints the camera
printCam ∷ Maybe Window → String
printCam Nothing  = "no window found"
printCam (Just w) = case (findWorld (winElems w)) of
  Nothing → "no world found"
  Just (wp, wd) → (show (wdCam wd)) ⧺ (printCams fixedCam)
    where fixedCam = map (fixCurs wp) $ take 9 (evalScreenCursor (wpSSize wp) (-cx,-cy))
          (cx,cy)  = wdCam wd
printCams ∷ [((Int,Int),(Int,Int))] → String
printCams []     = "."
printCams (c:cs) = ", " ⧺ (show c) ⧺ printCams cs

-- generates the world data
genWorldData ∷ WorldParams → WorldData → WorldData
genWorldData wp wd = wd { wdZones = zones0 }
  where zones0    = replaceZones newSegs0 zoneSize (wdZones wd)
        newSegs0  = genWorld wp wd
        zoneSize  = wpZSize wp

-- generates zones and segments for a given cursor
genWorld ∷ WorldParams → WorldData → [((Int,Int),((Int,Int),Segment))]
genWorld wp wd = genSegs zSize sSize conts rands cam
  where cam     = map (fixCurs wp) $ take 9 (evalScreenCursor sSize (-cx,-cy))
        sSize   = wpSSize wp
        zSize   = wpZSize wp
        (cx,cy) = wdCam wd
        conts  = wpConts wp
        rands  = wpRands wp
genSegs ∷ (Int,Int) → (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [((Int,Int),(Int,Int))] → [((Int,Int),((Int,Int),Segment))]
genSegs _     _    _     _     []         = []
genSegs zsize size conts rands ((z,s):cs) = segind ⧺ genSegs zsize size conts rands cs
  where segind = [(z,(s,seg))]
        seg    = Segment $ genGrid zsize z size s conts rands
genGrid ∷ (Int,Int) → (Int,Int) → (Int,Int) → (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [[Spot]]
genGrid (zw,zh) (zi,zj) (w,h) (i,j) conts rands = stripGrid2 seg1
  where seg1    = seedConts ind' conts rands zeroSeg
        ind'    = ((i*w) + (zi*zw*zw), (j*h) + (zj*zh*h))
        zeroSeg = take (h+4) (zip [-2..] (repeat (take (w+4) (zip [-2..] (repeat (Spot 1 0 Nothing 0.0))))))
seedConts ∷ (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedConts _   []     _      grid = grid
seedConts _   _      []     grid = grid
seedConts ind (c:cs) (r:rs) grid = seedConts ind cs rs spots
  where spots = seedSpots ind c r grid
seedSpots ∷ (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedSpots _   _ _ []             = []
seedSpots ind c r ((j,row):grid) = [(j,(rowSpots j ind c r row))] ⧺ seedSpots ind c r grid
rowSpots ∷ Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,Spot)] → [(Int,Spot)]
rowSpots _ _   _ _ []             = []
rowSpots j ind c r ((i,spot):row) = [(i,spotSpots i j ind c r spot)] ⧺ rowSpots j ind c r row
spotSpots ∷ Int → Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → Spot → Spot
spotSpots i j (zi,zj) (rand,cont) ((w,x),(y,z)) (Spot c t b e)
  | dist < (rand*100000) = Spot c' t' b e
  | otherwise            = Spot c  t  b e0
  where c'   = if e < 0 then 1 else cont
        t'   = 0
        i'   = i + zi
        j'   = j + zj
        dist = seedDistance i' j' w x y z
        e0   = e
-- generates the world dyns and plugs it in to the buffer
genWorldBuff ∷ [Dyns] → Int → Int → WorldParams → WorldData → [Dyns]
genWorldBuff buff b nDefTex wp wd = setTileBuff b dyns buff
  where dyns = Dyns $ genWorldDyns nDefTex 1000 curs wp wd d0
        curs = take 9 (evalScreenCursor sSiz (-cx, -cy))
        d0   = take 1000 $ repeat $ DynData 0 (0,0) (1,1) (0,0)
        sSiz = wpSSize wp
        (cx,cy) = wdCam wd
genWorldDyns ∷ Int → Int → [(Int,Int)] → WorldParams → WorldData → [DynData] → [DynData]
genWorldDyns nDefTex size curs wp wd d0 = take size $ genCursDyns 0 nDefTex size fcs wp wd d0
  where fcs   = map (fixCurs wp) curs
-- generates tiles for each cursor point
genCursDyns ∷ Int → Int → Int → [((Int,Int),(Int,Int))] → WorldParams → WorldData → [DynData] → [DynData]
genCursDyns _ _       _    []     _  _  d0 = d0
genCursDyns n nDefTex size (c:cs) wp wd d0 = genCursDyns n' nDefTex size cs wp wd d1
  where (d1,n') = genCursDynsF n nDefTex size c wp wd d0
genCursDynsF ∷ Int → Int → Int → ((Int,Int),(Int,Int)) → WorldParams → WorldData → [DynData] → ([DynData],Int)
genCursDynsF n nDefTex size ((zi,zj),(i,j)) wp wd d
  | n > size  = (d, n)
  | otherwise = (d',(n + (sw*sh)))
    where d'        = initlist ⧺ newvals ⧺ taillist
          initlist  = take n d
          taillist  = take (size - n - (length newvals)) $ repeat $ DynData 0 (0,0) (1,1) (0,0)
          newvals   = segToDyns nDefTex (sw'*i'+(zj'*sw'*zw'),sh'*j'+(zi'*sh'*zh')) seg'
          (i',j')   = (fromIntegral i,  fromIntegral j)
          (zi',zj') = (fromIntegral zi, fromIntegral zj)
          (sw',sh') = (fromIntegral (fst (wpSSize wp)), fromIntegral (snd (wpSSize wp)))
          (sw,sh)   = (fst (wpSSize wp), snd (wpSSize wp))
          (zw',zh') = (fromIntegral (fst (wpZSize wp)), fromIntegral (snd (wpZSize wp)))
          seg       = SegmentNULL--indexZone (zi,zj) (i,j) (wdZones wd)
          seg'      = initSeg seg (wpSSize wp)
segToDyns ∷ Int → (Float,Float) → Segment → [DynData]
segToDyns _       _   SegmentNULL = []
segToDyns nDefTex ind (Segment g) = segToDynsF nDefTex ind g'
  where g' = trimFat g
segToDynsF ∷ Int → (Float,Float) → [[Spot]] → [DynData]
segToDynsF _       _     []         = []
segToDynsF _       _     [[]]       = []
segToDynsF nDefTex (i,j) (row:grid) = dd ⧺ segToDynsF nDefTex (i,j+1) grid
  where dd = rowToDyns nDefTex (i,j) row
rowToDyns ∷ Int → (Float,Float) → [Spot] → [DynData]
rowToDyns _       _     []         = []
rowToDyns nDefTex (i,j) (spot:row) = dd ⧺ rowToDyns nDefTex (i+1,j) row
  where dd = [DynData (nDefTex + 3) (2*i,2*j) (1,1) (1,1)]
        c  = spotCont spot

-- creates a new empty segment when they havent been created yet
initSeg ∷ Segment → (Int,Int) → Segment
initSeg (Segment g) _     = Segment g
initSeg SegmentNULL (w,h) = Segment $ take (h+4) $ repeat $ take (w+4) $ repeat $ Spot 1 1 Nothing 0

-- turns a cursor point into a zone and segment index
fixCurs ∷ WorldParams → (Int,Int) → ((Int,Int),(Int,Int))
fixCurs wp (i,j) = ((zi,zj),(i',j'))
  where zi      = (1 + i) `div` zw
        zj      = (1 + j) `div` zh
        i'      = (1 + i + zw) `mod` zw
        j'      = (1 + j + zh) `mod` zh
        (zw,zh) = wpZSize wp

-- returns the list of indecies
-- of world segments to generate
evalScreenCursor ∷ (Int,Int) → (Float,Float) → [(Int,Int)]
evalScreenCursor (w,h) (cx,cy) = [pos,posn,pose,poss,posw,posnw,posne,posse,possw,posnww,posnnw,posnn,posnne,posnee,posene,posee,posese,possee,possse,posss,posssw,possww,poswsw,posww,poswnw]
  where pos    = (x,    y)
        posn   = (x,    y + 1)
        poss   = (x,    y - 1)
        posw   = (x - 1,y)
        pose   = (x + 1,y)
        posnw  = (x - 1,y - 1)
        posne  = (x + 1,y - 1)
        possw  = (x - 1,y + 1)
        posse  = (x + 1,y + 1)
        posnww = (x - 2,y - 2)
        posnnw = (x - 1,y - 2)
        posnn  = (x,    y - 2)
        posnne = (x + 1,y - 2)
        posnee = (x + 2,y - 2)
        posene = (x + 2,y - 1)
        posee  = (x + 2,y)
        posese = (x + 2,y + 1)
        possee = (x + 2,y + 2)
        possse = (x + 1,y + 2)
        posss  = (x,    y + 2)
        posssw = (x - 1,y + 2)
        possww = (x - 2,y + 2)
        poswsw = (x - 2,y + 1)
        posww  = (x - 2,y)
        poswnw = (x - 2,y + 1)
        -- an extra shift centers it since
        -- vulkan is indexed to side of screen
        x      = (floor $ (cx / (64.0*w')))
        y      = (floor $ (cy / (64.0*h')))
        w'     = fromIntegral w
        h'     = fromIntegral h

-- replaces a segment in a collection of zones
replaceZones ∷ [((Int,Int),((Int,Int),Segment))] → (Int,Int) → [Zone] → [Zone]
replaceZones []     _        zs = zs
replaceZones (s:ss) zoneSize zs = replaceZones ss zoneSize $ replaceSegs False s zoneSize zs
replaceSegs ∷ Bool → ((Int,Int),((Int,Int),Segment)) → (Int,Int) → [Zone] → [Zone]
replaceSegs True  _                   _        [] = []
replaceSegs False (zoneInd,(ind,seg)) zoneSize [] = replaceZones [(zoneInd,(ind,seg))] zoneSize $ [Zone zoneInd (initSegs)]
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
