module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Numeric.Noise.Perlin ( Perlin )
import Epiklesis.Border ( calcCornerBuff, setTileBorder )
import Epiklesis.Data
import Epiklesis.Elev ( setElevBuff )
import Epiklesis.Rand ( genConts, genRands )
import Epiklesis.Map
    ( fixCurs, indexSeg, indexZone, seedDistance, stripGrid )
import Epiklesis.Noise ( getNoise )
import Epiklesis.Window
    ( replaceWin,
      currentWin,
      replaceZones,
      replaceWorldWinElem,
      findWorldData,
      evalScreenCursor )
import Paracletus.Data ( DynData(DynData), Dyns(..) )
import Paracletus.Buff ( setTileBuff )
import Paracletus.Text ( calcTextBuff )

-- world parameters set from argV
genWorldParams ∷ UserWorldParams → WorldParams → WorldParams
genWorldParams uwp wp = wp { wpRands = rands
                           , wpConts = conts
                           , wpSize  = (w,h) }
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
                     Just (wp,wd) → case (winScreen win') of
                       WinScreenElev → setElevBuff (dsNDefTex ds) (cx,cy) wp wd (dsBuff ds)
                         where (cx,cy,_) = winCursor win
                       WinScreenNULL → setTileBuffs (dsNDefTex ds) (cx,cy) wp wd (dsBuff ds)
                         where (cx,cy,_) = winCursor win
          (win') = case (findWorldData win) of
            Nothing      → (win)
            Just (wp,wd) → (win { winElems = replaceWorldWinElem wd' (winElems win) })
              where wd'        = wd { wdZones = zones1 }
                    zones1     = setTileBorder wp zones0 (cx,cy)
                    zones0     = replaceZones newSegs0 zoneSize (wdZones wd)
                    newSegs0   = setTileData wp wd (cx,cy)--fixSegs wpGen $ genSegs wpGen $ evalScreenCursor segSize (-cx/64.0,-cy/64.0)
                    zoneSize   = wpZSize wp
                    (cx,cy,_) = winCursor win

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
  SegmentNULL  → Segment (stripGrid seg1)
  where seg1     = seedConts p ind' maxp conts rands zeroSeg
        seg      = indexSeg ind $ zoneSegs zone
        zone     = indexZone (zw,zh) zind zones
        zones    = wdZones wd
        rands    = wpRands wp
        conts    = wpConts wp
        (sw,sh)  = wpSSize wp
        (zw,zh)  = wpZSize wp
        maxp     = wpSize  wp
        zeroSeg  = take (sh+4) (zip [-2..] (repeat (take (sw+4) (zip [-2..] (repeat (Spot 1 0 Nothing 0.0))))))
        ind'     = ((((fst ind)*sw) + ((fst zind)*zw*sw)), (((snd ind)*sh) + ((snd zind)*zh*sh)))
        p        = wpPerl wp

seedConts ∷ Perlin → (Int,Int) → (Int,Int) → [(Int,Int)] → [((Int,Int),(Int,Int))] → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedConts _ _   _    []     _      grid = grid
seedConts _ _   _    _      []     grid = grid
seedConts p ind maxp (c:cs) (r:rs) grid = seedConts p ind maxp cs rs spots
  where spots = seedSpots p ind maxp c r grid
seedSpots ∷ Perlin → (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,[(Int,Spot)])] → [(Int,[(Int,Spot)])]
seedSpots _ _   _    _ _ []             = []
seedSpots p ind maxp c r ((j,row):grid) = [(j,(rowSpots p j ind maxp c r row))] ⧺ seedSpots p ind maxp c r grid
rowSpots ∷ Perlin → Int → (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [(Int,Spot)] → [(Int,Spot)]
rowSpots _ _ _   _    _ _ []             = []
rowSpots p j ind maxp c r ((i,spot):row) = [(i,spotSpots p i j ind maxp c r spot)] ⧺ rowSpots p j ind maxp c r row
spotSpots ∷ Perlin → Int → Int → (Int,Int) → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → Spot → Spot
spotSpots p i j (zi,zj) (maxw,maxh) (rand,cont) ((w,x),(y,z)) (Spot c t b e)
  | dist < (rand*100000) = Spot c' t' b e'
  | otherwise            = Spot c  t  b e0
  where c'   = if e' < 0 then 1 else cont
        t'   = 0
        i'   = i + zi
        j'   = j + zj
        dist = seedDistance i' j' w x y z
        e0   = e + (10000000*((getNoise (maxw + i') (maxh + j') p)) / (100000 + (fromIntegral dist)))
        e'   = e0 + 1

-- proivides world data as dyndata,
-- loads from window object
setTileBuffs ∷ Int → (Float,Float) → WorldParams → WorldData → [Dyns] → [Dyns]
setTileBuffs nDefTex (cx,cy) wp wd dyns0 = dyns3
  where dyns1   = calcWorldBuffs 3 nDefTex wp wd curs dyns0
        curs    = take 9 (evalScreenCursor segSize (-cx/64.0,-cy/64.0))
        segSize = wpSSize wp
        dyns2   = setTileBuff 1 (calcAuxBuff nDefTex wp wd curs) dyns1
        dyns3   = setTileBuff 2 (calcTextBuff wp wd) dyns2

calcAuxBuff ∷ Int → WorldParams → WorldData → [(Int,Int)] → Dyns
calcAuxBuff nDefTex wp wd curs = Dyns $ res ⧺ (take (size - (length res)) (repeat (DynData 0 (0,0) (1,1) (0,0))))
  where res  = reverse $ case (wdSelect wd) of
                 Nothing  → calcCornerBuff nDefTex wp wd curs
                 Just sel → [DynData 115 (fromIntegral (2*(fst sel)), fromIntegral (2*(snd sel))) (1,1) (0,0)] ⧺ calcCornerBuff nDefTex wp wd curs
        size = 512

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
printElemWPs param   ((WinElemWorld _ _ _):_) = "no world parameter " ⧺ param ⧺ " present"
printElemWPs param (_:wps) = printElemWPs param wps

printSegs ∷ [((Int,Int),((Int,Int),Segment))] → String
printSegs [] = ""
printSegs ((zoneInd,(ind,_)):segs) = str ⧺ printSegs segs
  where str = "(" ⧺ (show zoneInd) ⧺ ", " ⧺ (show ind) ⧺ "), "

-- world generation
calcSpots ∷ Int → WorldParams → WorldData → (Int,Int) → [DynData]
calcSpots nDefTex wp wd curs = calcSeg nDefTex size zsize c zs
  where c     = fixCurs wp curs
        zs    = wdZones wd
        size  = wpSSize wp
        zsize = wpZSize wp
calcSeg ∷ Int → (Int,Int) → (Int,Int) → ((Int,Int),(Int,Int)) → [Zone] → [DynData]
calcSeg _       _    _       _                []     = []
calcSeg nDefTex size (zw,zh) (zoneInd,segInd) (z:zs)
  | (zoneIndex z ≡ zoneInd) = dd ⧺ calcSeg nDefTex size (zw,zh) (zoneInd,segInd) zs
  | otherwise               = calcSeg nDefTex size (zw,zh) (zoneInd,segInd) zs
  where dd = calcZone nDefTex zoneSize size segInd (zoneSegs z)
        zoneSize = (sw*zw*(fst zoneInd),sh*zh*(snd zoneInd))
        (sw,sh)  = size
calcZone ∷ Int → (Int,Int) → (Int,Int) → (Int,Int) → [[Segment]] → [DynData]
calcZone nDefTex (zw,zh) (w,h) (i,j) segs = calcSpot (zw+w*i,zh+h*j) nDefTex seg
  where seg   = (segs !! j) !! i
calcSpot ∷ (Int,Int) → Int → Segment → [DynData]
calcSpot _   _       SegmentNULL    = []
calcSpot ind nDefTex (Segment grid) = flatten $ map (calcGridRow ind nDefTex) (zip yinds grid')
  where yinds = take (length grid') [0..]
        grid' = map (init ∘ init) $ map (tail ∘ tail) $ (init ∘ init) $ (tail ∘ tail) grid
calcGridRow ∷ (Int,Int) → Int → (Int,[Spot]) → [DynData]
calcGridRow ind nDefTex (j,spots) = flatten $ map (calcGrid ind j nDefTex) (zip xinds spots)
  where xinds = take (length spots) [0..]
calcGrid ∷ (Int,Int) → Int → Int → (Int,Spot) → [DynData]
calcGrid (cx,cy) y nDefTex (x,(Spot c t _ _)) = [dd]
  where dd = DynData c' (2*x',2*y') (1 + (1/14),1 + (1/14)) (ix,iy)
        x' = (fromIntegral cx) + (fromIntegral x)
        y' = (fromIntegral cy) + (fromIntegral y)
        ix = t `mod` 3
        iy = t `div` 3
        c' = c + 1 + nDefTex
