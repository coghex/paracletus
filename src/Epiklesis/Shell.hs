module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Artos.Data ( ShellCmd(..) )
import Paracletus.Data ( DrawState(..), DSStatus(..) )
import Epiklesis.Data ( Window(..), WinElem(..), Shell(..) )
import Epiklesis.Window ( replaceWin, currentWin )

-- sends command to the first shell
commandShell ∷ ShellCmd → [WinElem] → [WinElem]
commandShell _     [] = []
commandShell shCmd (we:wes) = [we'] ⧺ commandShell shCmd wes
  where we' = case we of
          WinElemShell sh bl op → commandShellF shCmd sh bl op
          we0                   → we0
commandShellF ∷ ShellCmd → Shell → Bool → Bool → WinElem
commandShellF ShellCmdToggle       sh bl op = WinElemShell sh  bl    $ not op
commandShellF (ShellCmdString str) sh bl op = WinElemShell sh' False op
  where sh' = stringShell str sh
commandShellF ShellCmdDelete       sh bl op = WinElemShell sh' bl    op
  where sh' = delShell sh
commandShellF ShellCmdNULL         sh bl op = WinElemShell sh  bl    op

-- sends string to shell
stringShell ∷ String → Shell → Shell
stringShell str sh = sh { shTabbed = Nothing
                        , shInpStr = newStr
                        , shCursor = (shCursor sh) + (length str) }
  where newStr = (take (shCursor sh) (shInpStr sh)) ⧺ str ⧺ (drop (shCursor sh) (shInpStr sh))

-- deletes character in shell
delShell ∷ Shell → Shell
delShell sh = sh { shInpStr = newStr
                 , shCursor = max 0 ((shCursor sh) - 1) }
  where newStr = initS (take (shCursor sh) (shInpStr sh)) ⧺ (drop (shCursor sh) (shInpStr sh))
        initS ""  = ""
        initS str = init str

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
        strsin    = shInpStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt ⧺ strsin
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs
