module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import qualified Data.ByteString.Char8 as BL
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import qualified Foreign.Lua as Lua
import Anamnesis.Data ( Env(..) )
import Artos.Data ( ShellCmd(..), ShellCard(..), ShellControl(..) )
import Paracletus.Data ( DrawState(..), DrawStateP(..), DSStatus(..) )
import Epiklesis.Data ( Window(..), WinElem(..), Shell(..) )
import Epiklesis.ShCmd ( loadShCmds )
import Epiklesis.Window ( replaceWin, currentWin )
import Paracletus.Oblatum.Font ( TTFData(..), indexTTF )

-- sends command to the first shell
commandShell ∷ ShellCmd → DrawStateP → [WinElem] → [WinElem]
commandShell _     dsp []       = []
commandShell shCmd dsp (we:wes) = [we'] ⧺ commandShell shCmd dsp wes
  where we' = case we of
          WinElemShell sh bl op → commandShellF shCmd dsp sh bl op
          we0                   → we0
commandShellF ∷ ShellCmd → DrawStateP → Shell → Bool → Bool → WinElem
commandShellF ShellCmdToggle          _   sh bl op = WinElemShell sh  bl    $ not op
commandShellF (ShellCmdString str)    _   sh bl op = WinElemShell sh' False op
  where sh' = stringShell str sh
commandShellF ShellCmdDelete          _   sh bl op = WinElemShell sh' bl    op
  where sh' = delShell sh
commandShellF (ShellCmdDirection dir) _   sh bl op = WinElemShell sh' bl    op
  where sh' = directionShell dir sh
commandShellF ShellCmdTab             _   sh bl op = WinElemShell sh' bl    op
  where sh' = tabShell sh
commandShellF (ShellCmdControl key)   _   sh bl op = WinElemShell sh' bl    op
  where sh' = controlSh key sh
commandShellF (ShellCmdEcho str)      dsp sh bl op = WinElemShell sh' bl    op
  where sh' = echoShell str dsp sh
commandShellF ShellCmdExec            _   sh bl op = WinElemShell sh  bl    op
commandShellF ShellCmdNULL            _   sh bl op = WinElemShell sh  bl    op

-- returns draw state values from within the shell
echoShell ∷ String → DrawStateP → Shell → Shell
echoShell "fps" dsp sh = sh { shRet = show $ dspFPS dsp }
echoShell str   dsp sh = sh { shRet = "unknown variable " ⧺ str }

-- proccesing of shell control keys
controlSh ∷ ShellControl → Shell → Shell
controlSh ShCtlC sh = sh'
  where sh' = sh { shTabbed = Nothing
                 , shCursor = 0
                 , shInpStr = ""
                 , shCache  = ""
                 , shHistI  = -1
                 , shOutStr = retstring }
        retstring = (shOutStr sh) ⧺ (shPrompt sh) ⧺ (shInpStr sh) ⧺ "\n"
controlSh ShCtlA sh = sh { shCursor = 0 }
controlSh ShCtlE sh = sh { shCursor = length (shInpStr sh) }
controlSh key    sh = sh

-- tabs through shell commands
tabShell ∷ Shell → Shell
tabShell sh
  | shTabbed sh ≡ Nothing =
      sh { shCache  = shInpStr sh
         , shInpStr = newStr0
         , shTabbed = Just 0
         , shCursor = length newStr0 }
  | otherwise             =
      sh { shTabbed = Just incSh
         , shInpStr = newStr1
         , shCursor = length newStr1 }
    where incSh   = incShTabbed $ shTabbed sh
          newStr0 = tabCommand 0     (shInpStr sh) cmds
          newStr1 = tabCommand incSh (shCache sh) cmds
          cmds    = ["newWindow", "newText", "newMenu", "newMenuBit", "newLink", "newWorld", "switchWindow", "switchScreen", "setBackground", "luaModule", "newDynObj", "resizeWindow", "toggleFPS", "echo", "recreate", "reload"]
incShTabbed ∷ Maybe Int → Int
incShTabbed Nothing  = 0
incShTabbed (Just n) = (n + 1)
tabCommand ∷ Int → String → [String] → String
tabCommand n inpStr cmds
  | matchedStrings ≡ [] = inpStr
  | otherwise           = matchedStrings !! (n `mod` (length matchedStrings))
  where matchedStrings = filter (isPrefixOf inpStr) cmds

-- evaluates lua commands in IO
evalShell ∷ Env → Shell → IO Shell
evalShell env sh = do
  let ls = envLuaSt env
  if shLoaded sh then return () else loadShCmds env
  (ret,outbuff) ← execShell ls (shInpStr sh)
  let retstring = if (length (shOutStr sh) ≡ 0)
        then case (outbuff) of
          "nil" → (shOutStr sh) ⧺ (shPrompt sh) ⧺ (shInpStr sh) ⧺ "\n" ⧺ (show ret) ⧺ "\n"
          _     → (shOutStr sh) ⧺ (shPrompt sh) ⧺ (shInpStr sh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"
        else case (outbuff) of
          "nil" → (init (shOutStr sh)) ⧺ "> " ⧺ (shRet sh) ⧺ "\n" ⧺ (shPrompt sh) ⧺ (shInpStr sh) ⧺ "\n" ⧺ (show ret) ⧺ "\n"
          _     → (init (shOutStr sh)) ⧺ "> " ⧺ (shRet sh) ⧺ "\n" ⧺ (shPrompt sh) ⧺ (shInpStr sh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"
      sh' = sh { shInpStr = ""
               , shOutStr = retstring
               , shTabbed = Nothing
               , shRet    = ""
               , shLoaded = True
               , shHistI  = -1
               , shHist   = ([shInpStr sh] ⧺ (shHist sh))
               , shCursor = 0 }
  return sh'

execShell ∷ Lua.State → String → IO (Lua.Status,String)
execShell ls str = do
  luaerror ← Lua.runWith ls $ Lua.loadstring $ BL.pack str
  _   ← Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret ← Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (luaerror,(BL.unpack ret))

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

-- replaces first shell with new shell
replaceShell ∷ Shell → [WinElem] → [WinElem]
replaceShell _  []       = []
replaceShell sh (we:wes) = [we'] ⧺ replaceShell sh wes
  where we' = case we of
          WinElemShell _ bl op → WinElemShell sh bl op
          we0                  → we0

-- generates string for the shell
genShellStr ∷ Shell → String
genShellStr sh
  | (height > 8) = shortret
  | otherwise    = retstring
  where prompt    = shPrompt sh
        strsout   = genShellOut (shOutStr sh) (shRet sh)
        strsin    = shInpStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt ⧺ strsin
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs

genShellOut ∷ String → String → String
genShellOut out ""  = out
genShellOut out ret = (init out) ⧺ "> " ⧺ ret ⧺ "\n"

-- finds x position for shell cursor
findCursPos ∷ String → Float
findCursPos []         = 1.6
findCursPos (' ':str) = 0.5 + findCursPos str
findCursPos (ch:str)  = chX' + findCursPos str
  where TTFData _ _ _ chX _ = indexTTF ch
        chX'                = realToFrac chX
