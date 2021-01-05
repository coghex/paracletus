module Epiklesis.ArgV where
-- arguments are passed between windows
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Epiklesis.World

-- changing windows triggers argVs
changeWin ∷ Int → DrawState → DrawState
changeWin n ds = ds { dsWinI   = n
                    , dsLastI  = n'
                    , dsWins   = setArgV n n' (dsWins ds)
                    , dsStatus = DSSLoadVerts }
  where n' = dsWinI ds

setArgV ∷ Int → Int → [Window] → [Window]
setArgV cw lw wins = replaceWin win wins
  where win = (wins !! cw) { winArgV = argv
                           , winElems = argVElems argv (winElems (wins !! cw)) }
        argv = calcArgV (wins !! lw)
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
  where a1 = mbs' !! 0
        a2 = mbs' !! 1
        a3 = mbs' !! 2
        a4 = mbs' !! 3
        a5 = mbs' !! 4
        mbs' = filter (≢Nothing) $ map calcUWP mbs
calcUWP ∷ (Int,PaneBit) → Maybe Int
calcUWP (_,mb) = case mb of
  PaneBitSlider _ _ _ v → v
  _                     → Nothing

-- apply any argvs from the last window
argVElems ∷ WinArgV → [WinElem] → [WinElem]
argVElems _    []     = []
argVElems argv ((WinElemWorld wp wd dp):es) = case argv of
  WinArgUWP uwp → [WinElemWorld wp' wd dp] ⧺ argVElems argv es
    where wp' = genWorldParams uwp wp
  WinArgNULL    → [WinElemWorld wp  wd dp] ⧺ argVElems argv es
argVElems argv (e:es) = [e] ⧺ argVElems argv es


