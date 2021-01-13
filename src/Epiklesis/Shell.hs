module Epiklesis.Shell where
-- a basic shell for executing
-- lua commands is defined
import Prelude()
import UPrelude
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.Elev (elevAt)
import Epiklesis.ShCmd
import Epiklesis.World (printWorldParams)
import Epiklesis.Window (currentWin,findWorldData)
import Paracletus.Buff (clearBuff)
import Paracletus.Data
import Paracletus.Elem (calcTextBox, calcText)
import Paracletus.Oblatum.Font
import qualified Data.ByteString.Char8 as BL
import qualified Foreign.Lua as Lua

-- empty shell
initShell ∷ Shell
initShell = Shell "$> " False Nothing 1 False "" "" "" "" (-1) []

openShell ∷ Shell → Shell
openShell sh = sh { shOpen = True }

closeShell ∷ Shell → Shell
closeShell sh = sh { shOpen = False }

-- shell commands from loadcmdshell
evalShCmds ∷ Env → ShellCmd → DrawState → DrawState
evalShCmds env shCmd ds = case shCmd of
    ShellCmdTab → ds { dsShell = tabShell (dsShell ds) $ dsCmds ds
                     , dsStatus = DSSLoadDyns }
    ShellCmdDelete → ds { dsShell = delShell (dsShell ds)
                        , dsStatus = DSSLoadDyns }
    ShellCmdExec → ds { dsStatus = DSSEvalShell }--evalShell env ds
    -- enumerates all possible return
    -- values from echo commands
    ShellCmdRet "cam" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → show $ winCursor w
    ShellCmdRet "elev" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → case (findWorldData w) of
                      Nothing      → "no world"
                      Just (wp,wd) → show $ elevAt (winCursor w) wp wd
    ShellCmdRet "argv" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → show $ winArgV w
    ShellCmdRet "sSize" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → printWorldParams "sSize" w
    ShellCmdRet "zSize" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → printWorldParams "zSize" w
    ShellCmdRet "rands" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → printWorldParams "rands" w
    ShellCmdRet "conts" → ds { dsShell = (dsShell ds) { shRet = str }}
      where str = case (currentWin ds) of
                    Nothing → "no window"
                    Just w  → printWorldParams "conts" w
    ShellCmdRet str → ds { dsShell = (dsShell ds) { shRet = "value '" ⧺ str ⧺ "' not known" }}
    ShellCmdString ch → ds { dsShell  = stringShell ch (dsShell ds)
                           , dsStatus = DSSLoadDyns }
    ShellCmdCursor n → ds { dsShell = (dsShell ds) { shCursor = newN }
                          , dsStatus = DSSLoadDyns }
      where newN = max 0 $ min (length (shInpStr sh)) $ (shCursor sh) + n
            sh   = dsShell ds
    ShellCmdOpen  → ds { dsShell = openShell (dsShell ds)
                       , dsStatus = DSSLoadCap True }
    ShellCmdClose → ds { dsShell = closeShell (dsShell ds)
                       , dsStatus = DSSLoadCap False }
    ShellCmdControl key → ds { dsShell = controlShell (dsShell ds) key
                             , dsStatus = controlShStatus key }
    ShellCmdUp    → ds { dsShell = upShell (dsShell ds)
                       , dsStatus = DSSLoadDyns }
    ShellCmdDown  → ds { dsShell = downShell (dsShell ds)
                       , dsStatus = DSSLoadDyns }
    ShellCmdNULL  → ds

-- different keys require different returns
controlShStatus ∷ ShellControl → DSStatus
controlShStatus ShCtlC = DSSLoadVerts
controlShStatus ShCtlA = DSSLoadDyns
controlShStatus ShCtlE = DSSLoadDyns
controlShStatus _      = DSSNULL

controlShell ∷ Shell → ShellControl → Shell
controlShell sh ShCtlC =
  sh { shTabbed = Nothing
     , shCursor = 0
     , shInpStr = ""
     , shCache  = ""
     , shHistI  = -1
     , shOutStr = retstring }
  where retstring = (shOutStr sh) ⧺ (shPrompt sh) ⧺ shInpStr sh ⧺ "\n"
controlShell sh ShCtlA =
  sh { shCursor = 0 }
controlShell sh ShCtlE =
  sh { shCursor = (length (shInpStr sh)) }
controlShell sh _      = sh

loadShell ∷ Shell → [Tile]
loadShell sh
  | shOpen sh = tiles
  | otherwise = []
  where tiles      = textBox ⧺ text ⧺ retText ⧺ cursorTile
        textBox    = calcTextBox TextSize30px (-8.0, 4.5) (32,18)
        text       = calcText TextSize30px (-7) (-7,4) $ genShellStr sh
        retText    = calcText TextSize30px (-6) (-6.1,6 - i) $ shRet sh
        i          = fromIntegral $ length $ splitOn "\n" $ shOutStr sh
        cursorTile = [DTile (DMShCursor) (-7,4) (0.05,0.5) (0,0) (1,1) 112]

genShellStr ∷ Shell → String
genShellStr sh
  | (height > 8) = shortret
  | otherwise    = retstring
  where prompt    = shPrompt sh
        strsin    = shInpStr sh
        strsout   = shOutStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt-- ⧺ strsin
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs

-- TODO: rewrite for new ttf fonts
findCursPos ∷ String → Double
findCursPos []        = 1.6
findCursPos (' ':str) = 0.5 + findCursPos str
findCursPos (ch:str)  = chX' + findCursPos str
  where TTFData _ _ _ chX _ = indexTTF TextSize30px ch
        chX' = chX

-- send string to shell
stringShell ∷ String → Shell → Shell
stringShell str sh = sh { shTabbed = Nothing
                        , shInpStr = newStr
                        , shCursor = (shCursor sh) + (length str)
                        , shCBlink = False }
  where newStr = (take (shCursor sh) (shInpStr sh)) ⧺ str ⧺ (drop (shCursor sh) (shInpStr sh))
-- delete character
delShell ∷ Shell → Shell
delShell sh = sh { shInpStr = newStr
                 , shCursor = max 0 ((shCursor sh) - 1) }
  where newStr = initS (take (shCursor sh) (shInpStr sh)) ⧺ (drop (shCursor sh) (shInpStr sh))
        initS ""  = ""
        initS str = init str

evalShell ∷ Env → DrawState → IO DrawState
evalShell env ds = do
  let oldSh = dsShell ds
  loadShCmds env
  (ret,outbuff) ← execShell (envLuaSt env) (shInpStr oldSh)
  let retstring = if (length (shOutStr oldSh) ≡ 0)
          then case (outbuff) of
            "nil" → (shOutStr oldSh) ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ "\n"
            _     → (shOutStr oldSh) ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"
          else case (outbuff) of
            "nil" → (init (shOutStr oldSh)) ⧺ " " ⧺ (shRet oldSh) ⧺ "\n" ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ "\n"
            _     → (init (shOutStr oldSh)) ⧺ " " ⧺ (shRet oldSh) ⧺ "\n" ⧺ (shPrompt oldSh) ⧺ (shInpStr oldSh) ⧺ "\n" ⧺ (show ret) ⧺ " > " ⧺ outbuff ⧺ "\n"


      newSh    = oldSh { shInpStr = ""
                       , shOutStr = retstring
                       , shTabbed = Nothing
                       , shHistI  = -1
                       , shRet    = ""
                       , shHist   = ([shInpStr oldSh] ⧺ shHist oldSh)
                       , shCursor = 0 }
  return $ ds { dsShell  = newSh
              , dsBuff   = clearBuff (dsBuff ds) 0
              , dsStatus = DSSLoadVerts }

execShell ∷ Lua.State → String → IO (Lua.Status,String)
execShell ls str = do
  luaerror ← Lua.runWith ls $ Lua.loadstring $ BL.pack str
  _   ← Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret ← Lua.runWith ls $ Lua.tostring' $ Lua.nthFromBottom (-1)
  Lua.runWith ls $ Lua.pop $ Lua.nthFromBottom (-1)
  return $ (luaerror,(BL.unpack ret))

-- cycles through commands with tab
tabShell ∷ Shell → [String] → Shell
tabShell sh cmds
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
          newStr0 = tabCommand 0 (shInpStr sh) cmds
          newStr1 = tabCommand incSh (shCache sh) cmds

incShTabbed ∷ Maybe Int → Int
incShTabbed Nothing  = 0
incShTabbed (Just n) = (n+1)

tabCommand ∷ Int → String → [String] → String
tabCommand n inpStr cmds
  | matchedStrings ≡ [] = inpStr
  | otherwise           = matchedStrings !! (n `mod` (length matchedStrings))
  where matchedStrings = filter (isPrefixOf inpStr) cmds

-- cycles through the shell history
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

