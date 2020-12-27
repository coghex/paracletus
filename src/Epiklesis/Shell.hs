module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Elem (calcTextBox)

-- empty shell
initShell ∷ Shell
initShell = Shell "$> " False Nothing 1 False "" "" "" (-1) []

openShell ∷ Shell → Shell
openShell sh = sh { shOpen = True }

closeShell ∷ Shell → Shell
closeShell sh = sh { shOpen = False }

loadShell ∷ Shell → [Tile]
loadShell sh
  | shOpen sh = tiles
  | otherwise = []
  where tiles      = cursorTile ⧺ textBox
        textBox    = calcTextBox TextSize30px (-8.0, 4.5) (32,18)
        cursorTile = [GTile (0,0) (0.1,0.5) (0,0) (1,1) 112]
