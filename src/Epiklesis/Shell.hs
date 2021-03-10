module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Artos.Data ( ShellCmd(..), ShellCard(..) )
import Paracletus.Data ( DrawState(..), DSStatus(..) )
import Epiklesis.Data ( Window(..), WinElem(..), Shell(..) )
import Epiklesis.Window ( replaceWin, currentWin )
import Paracletus.Oblatum.Font ( TTFData(..), indexTTF )

-- sends command to the first shell
commandShell ∷ ShellCmd → [WinElem] → [WinElem]
commandShell _     [] = []
commandShell shCmd (we:wes) = [we'] ⧺ commandShell shCmd wes
  where we' = case we of
          WinElemShell sh bl op → commandShellF shCmd sh bl op
          we0                   → we0
commandShellF ∷ ShellCmd → Shell → Bool → Bool → WinElem
commandShellF ShellCmdToggle          sh bl op = WinElemShell sh  bl    $ not op
commandShellF (ShellCmdString str)    sh bl op = WinElemShell sh' False op
  where sh' = stringShell str sh
commandShellF ShellCmdDelete          sh bl op = WinElemShell sh' bl    op
  where sh' = delShell sh
commandShellF (ShellCmdDirection dir) sh bl op = WinElemShell sh' bl    op
  where sh' = directionShell dir sh
commandShellF ShellCmdNULL            sh bl op = WinElemShell sh  bl    op

-- sends directional key to shell
directionShell ∷ ShellCard → Shell → Shell
directionShell ShellUp    sh = upShell   sh
directionShell ShellDown  sh = downShell sh
directionShell ShellLeft  sh = cursorShell (-1) sh
directionShell ShellRight sh = cursorShell 1    sh

-- cycles through shell history
upShell ∷ Shell → Shell
upShell sh
  | shHist sh ≡ [] = sh
  | otherwise      = sh { shInpStr = (shHist sh) !! (incShHist `mod` (length (shHist sh)))
                        , shHistI  = incShHist }
  where incShHist = if (shHistI sh) ≥ (length (shHist sh)) then 0 else (shHistI sh) + 1
downShell ∷ Shell → Shell
downShell sh
  | shHist sh ≡ [] = sh
  | shHistI sh ≥ 0 = sh { shInpStr = (shHist sh) !! ((shHistI sh) `mod` (length (shHist sh)))
                        , shHistI  = max (-1) ((shHistI sh) - 1) }
  | otherwise      = sh { shInpStr = "" }
-- move shell cursor
cursorShell ∷ Int → Shell → Shell
cursorShell n sh = sh { shCursor = n' }
  where n' = max 0 $ min (length (shInpStr sh)) $ (shCursor sh) + n

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

-- finds x position for shell cursor
findCursPos ∷ String → Float
findCursPos []         = 1.6
findCursPos (' ':str) = 0.5 + findCursPos str
findCursPos (ch:str)  = chX' + findCursPos str
  where TTFData _ _ _ chX _ = indexTTF ch
        chX'                = realToFrac chX
