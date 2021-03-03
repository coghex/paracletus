module Epiklesis.Window where
-- functions to help manipulate
-- windows can be found
import Prelude()
import UPrelude
import Epiklesis.Data
    ( WinElem(..), Window(..) )

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

-- switches head win
switchWin ∷ String → [Window] → [Window]
switchWin _    []         = []
switchWin name (win:wins)
  | winTitle win ≡ name = win : switchWin name wins
  | otherwise           = switchWin name wins ⧺ [win]

-- returns requred extra textures
calcWinModTexs ∷ Window → [String]
calcWinModTexs win = calcWinElemModTexs $ winElems win
calcWinElemModTexs ∷ [WinElem] → [String]
calcWinElemModTexs [] = []
--calcWinElemModTexs ((WinElemWorld _ _ dps):wes) = dps ⧺ calcWinElemModTexs wes
calcWinElemModTexs (_:wes) = calcWinElemModTexs wes

currentWin ∷ [Window] → Maybe Window
currentWin wins
  | (length wins) ≤ 0 = Nothing
  | otherwise         = Just $ head wins
