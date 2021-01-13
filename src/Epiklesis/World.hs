module Epiklesis.World where
-- world related functions exist
import Prelude()
import UPrelude
import Epiklesis.Border
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
                     Just (wp,wd) → case (winScreen win') of
                       WinScreenElev → (dsBuff ds)
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
  where seg1     = seedConts ind' conts rands zeroSeg
        seg      = indexSeg ind $ zoneSegs zone
        zone     = indexZone (zw,zh) zind zones
        zones    = wdZones wd
        rands    = wpRands wp
        conts    = wpConts wp
        (sw,sh)  = wpSSize wp
        (zw,zh)  = wpZSize wp
        zeroSeg  = take (sh+2) (zip [-1..] (repeat (take (sw+2) (zip [-1..] (repeat (Spot 1 0 Nothing))))))
        ind'     = ((((fst ind)*sw) + ((fst zind)*zw*sw)), (((snd ind)*sh) + ((snd zind)*zh*sh)))

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
spotSpots i j (zi,zj) (rand,cont) ((w,x),(y,z)) (Spot c t b)
  | seedDistance i' j' w x y z < (rand*maxS) = Spot c' t' b
  | otherwise                                = Spot c t b
  where c'   = cont
        t'   = 0
        i'   = i + zi
        j'   = j + zj
        maxS = 100000

-- proivides world data as dyndata,
-- loads from window object
setTileBuffs ∷ Int → (Float,Float) → WorldParams → WorldData → [Dyns] → [Dyns]
setTileBuffs nDefTex (cx,cy) wp wd dyns0 = dyns2
  where dyns1   = calcWorldBuffs 2 nDefTex wp wd curs dyns0
        curs    = take 9 (evalScreenCursor segSize (-cx/64.0,-cy/64.0))
        segSize = wpSSize wp
        dyns2   = setTileBuff 1 (calcAuxBuff nDefTex wp wd curs) dyns1

calcAuxBuff ∷ Int → WorldParams → WorldData → [(Int,Int)] → Dyns
calcAuxBuff nDefTex wp wd curs = Dyns $ res ⧺ (take (size - (length res)) (repeat (DynData 0 (0,0) (1,1) (0,0))))
  where res  = calcCornerBuff nDefTex wp wd curs
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
        grid' = map init $ map tail $ init $ tail grid
calcGridRow ∷ (Int,Int) → Int → (Int,[Spot]) → [DynData]
calcGridRow ind nDefTex (j,spots) = flatten $ map (calcGrid ind j nDefTex) (zip xinds spots)
  where xinds = take (length spots) [0..]
calcGrid ∷ (Int,Int) → Int → Int → (Int,Spot) → [DynData]
calcGrid (cx,cy) y nDefTex (x,(Spot c t _)) = [dd]
  where dd = DynData c' (2*x',2*y') (1,1) (ix,iy)
        x' = (fromIntegral cx) + (fromIntegral x)
        y' = (fromIntegral cy) + (fromIntegral y)
        ix = t `mod` 3
        iy = t `div` 3
        c' = c + 1 + nDefTex
