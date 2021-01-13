module Epiklesis.Window where
-- functions to help manipulate
-- windows can be found
import Prelude()
import UPrelude
import Epiklesis.Data

-- replaces specific window in windows
replaceWin ∷ Window → [Window] → [Window]
replaceWin _   []     = []
replaceWin win (w:ws)
  | winTitle win ≡ winTitle w = [win] ⧺ replaceWin win ws
  | otherwise                 = [w] ⧺ replaceWin win ws

-- wins searched by name
findWin ∷ String → [Window] → Maybe Window
findWin _    [] = Nothing
findWin name (win:wins)
  | winTitle win ≡ name = Just win
  | otherwise = findWin name wins

-- index searched by name
findWinI ∷ String → [Window] → Maybe Int
findWinI name wins = findWinIFunc 0 name wins
findWinIFunc ∷ Int → String → [Window] → Maybe Int
findWinIFunc _ _    []         = Nothing
findWinIFunc n name (win:wins)
  | winTitle win ≡ name = Just n
  | otherwise = findWinIFunc (n + 1) name wins

-- its ok to have !! here since
-- currwin and wins are created
-- together, the outcome is known
currentWin ∷ DrawState → Maybe Window
currentWin ds
  | dsWinI ds < 0 = Nothing
  | otherwise     = Just $ (dsWins ds) !! (dsWinI ds)

-- add bit to window pane
loadNewBit ∷ String → [WinElem] → PaneBit → [WinElem]
loadNewBit _    []       _   = []
loadNewBit pane (we:wes) bit = case we of
  WinElemPane pos name bits
    | name ≡ pane → [WinElemPane pos name bits'] ⧺ loadNewBit pane wes bit
    | otherwise   → [WinElemPane pos name bits]  ⧺ loadNewBit pane wes bit
    where bits' = bits ⧺ [((length bits),bit)]
  we0                       → [we0] ⧺ loadNewBit pane wes bit

-- returns requred extra textures
calcWinModTexs ∷ Window → [String]
calcWinModTexs win = calcWinElemModTexs $ winElems win
calcWinElemModTexs ∷ [WinElem] → [String]
calcWinElemModTexs [] = []
calcWinElemModTexs ((WinElemWorld _ _ dps):wes) = dps ⧺ calcWinElemModTexs wes
calcWinElemModTexs (_:wes) = calcWinElemModTexs wes

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
findWorldDataElems ((WinElemWorld wp wd _):_) = Just (wp,wd)
findWorldDataElems (_:wes) = findWorldDataElems wes

findWorldDataM ∷ Maybe Window → Maybe (WorldParams,WorldData)
findWorldDataM Nothing    = Nothing
findWorldDataM (Just win) = findWorldDataElems (winElems win)


-- returns the list of indecies
-- of segments to generate
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
        x      = (-1) + (floor $ cx / w')
        y      = (-1) + (floor $ cy / h')
        w'     = fromIntegral w
        h'     = fromIntegral h

