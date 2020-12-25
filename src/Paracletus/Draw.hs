module Paracletus.Draw where
-- converts epiklesis state to draw state
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Elem

loadTiles ∷ DrawState → [Tile]
loadTiles ds = case (currentWin ds) of
  Just win → loadWindow win
  Nothing  → []

loadWindow ∷ Window → [Tile]
loadWindow win = loadWinElems $ winElems win

loadWinElems ∷ [WinElem] → [Tile]
loadWinElems []       = []
loadWinElems (we:wes) = loadWinElem we ⧺ loadWinElems wes
loadWinElem ∷ WinElem → [Tile]
loadWinElem (WinElemText pos True  str) = (calcTextBox size posOffset s) ⧺ calcText size (fst pos) pos str
  where s = calcTextBoxSize size str
        posOffset = ((fst pos) - 0.5, (snd pos) + 0.5)
        size = TextSize30px
loadWinElem (WinElemText pos False str) = calcText TextSize30px (fst pos) pos str
loadWinElem (WinElemLink _ _ _)         = []
loadWinElem (WinElemNULL)               = []
