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

-- TODO: set argVs here
changeWin ∷ Int → DrawState → DrawState
changeWin n ds = ds { dsWinI   = n
                    , dsLastI  = n'
                    , dsStatus = DSSLoadWorld }
  where n' = dsWinI ds

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
