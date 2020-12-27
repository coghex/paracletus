module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Elem (calcTextBox, calcText)

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
  where tiles      = textBox ⧺ text ⧺ cursorTile
        textBox    = calcTextBox TextSize30px (-8.0, 4.5) (32,18)
        text       = calcText TextSize30px (-7) (-7,4) $ genShellStr sh
        cursorTile = [GTile (-7 + cursPos,4) (0.1,0.5) (0,0) (1,1) 112]
        cursPos   = findCursPos (shInpStr sh) (shCursor sh)

genShellStr ∷ Shell → String
genShellStr sh
  | (height > 8) = shortret
  | otherwise    = retstring
  where prompt    = shPrompt sh
        strsin    = shInpStr sh
        strsout   = shOutStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt ⧺ strsin
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs

-- TODO: rewrite for new ttf fonts
findCursPos ∷ String → Int → Double
findCursPos str curs = foldl (+) 0.9 $ map fontOffset str'
  where str' = take curs str

