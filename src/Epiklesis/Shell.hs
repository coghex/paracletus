module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Paracletus.Data ( DrawState(..), DSStatus(..) )
import Epiklesis.Data ( Window(..), WinElem(..), Shell(..) )
import Epiklesis.Window ( replaceWin, currentWin )

-- sets all shell elements to their opposite open status
toggleShell ∷ DrawState → DrawState
toggleShell ds = case (currentWin (dsWins ds)) of
  Nothing → ds
  Just w  → ds { dsWins   = replaceWin win (dsWins ds)
               , dsStatus = DSSLoadDyns }
    where win = toggleShellElem w
toggleShellElem ∷ Window → Window
toggleShellElem w = w { winElems = toggleShElem (winElems w) }
toggleShElem ∷ [WinElem] → [WinElem]
toggleShElem []       = []
toggleShElem (we:wes) = [we'] ⧺ toggleShElem wes
  where we' = case we of
          WinElemShell sh bl op → WinElemShell sh bl $ not op
          we0                   → we0

-- returns data of first shell found
findShell ∷ [WinElem] → Maybe (Shell,Bool,Bool)
findShell []                            = Nothing
findShell ((WinElemShell sh bl op):wes) = Just (sh,bl,op)
findShell (_:wes)                       = findShell wes

-- generates string for the shell
genShellStr ∷ Shell → String
genShellStr sh
  | (height > 8) = shortret
  | otherwise    = retstring
  where prompt    = shPrompt sh
        strsout   = shOutStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs
