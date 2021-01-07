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
  we                        → [we] ⧺ loadNewBit pane wes bit

-- returns requred extra textures
calcWinModTexs ∷ Window → [String]
calcWinModTexs win = calcWinElemModTexs $ winElems win
calcWinElemModTexs ∷ [WinElem] → [String]
calcWinElemModTexs [] = []
calcWinElemModTexs ((WinElemWorld _ _ dps):wes) = dps ⧺ calcWinElemModTexs wes
calcWinElemModTexs (we:wes) = calcWinElemModTexs wes

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

