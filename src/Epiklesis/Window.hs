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

changeWin ∷ Int → DrawState → DrawState
changeWin n ds = ds { dsWinI   = n
                    , dsLastI  = n'
                    , dsWins   = setArgV n n' (dsWins ds)
                    , dsStatus = DSSLoadVerts }
  where n' = dsWinI ds

setArgV ∷ Int → Int → [Window] → [Window]
setArgV cw lw wins = replaceWin win wins
  where win = (wins !! cw) { winArgV = calcArgV (wins !! lw) }
calcArgV ∷ Window → WinArgV
calcArgV win = calcElemArgV $ winElems win
calcElemArgV ∷ [WinElem] → WinArgV
calcElemArgV []       = WinArgNULL
calcElemArgV (we:wes) = case we of
  WinElemPane _ n b
    | n ≡ "wparams" → calcMenuArgV b
    | otherwise     → WinArgNULL
  _                 → calcElemArgV wes

calcMenuArgV ∷ [(Int,PaneBit)] → WinArgV
calcMenuArgV mbs = case a1 of
  Nothing  → WinArgNULL
  Just a1' → case a2 of
    Nothing  → WinArgNULL
    Just a2' → case a3 of
      Nothing  → WinArgNULL
      Just a3' → case a4 of
        Nothing  → WinArgNULL
        Just a4' → case a5 of
          Nothing  → WinArgNULL
          Just a5' → WinArgUWP $ UserWorldParams a1' a2' a3' a4' a5'
  where a1 = calcUWP $ mbs !! 0
        a2 = calcUWP $ mbs !! 1
        a3 = calcUWP $ mbs !! 2
        a4 = calcUWP $ mbs !! 3
        a5 = calcUWP $ mbs !! 4
calcUWP ∷ (Int,PaneBit) → Maybe Int
calcUWP (_,mb) = case mb of
  PaneBitSlider _ _ _ v → v
  _                     → Nothing

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
