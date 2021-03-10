{-# LANGUAGE Strict #-}
module Paracletus.Load
  ( loadParacletus )
where
-- we define the thread that helps
-- recreate the swapchain
import Prelude()
import UPrelude
import Anamnesis.Data ( Env(..) )
import Artos.Data
import Artos.Var ( atomically )
import Artos.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Epiklesis.Data
    ( Window(..), WinElem(..)
    , PaneBit(..), LinkAction(..) )
import Epiklesis.Elem ( loadNewBit, findBitPos )
import Epiklesis.Shell ( toggleShell )
import Epiklesis.Window
    ( switchWin, findWin, replaceWin
    , calcWinModTexs, currentWin
    , printWinElems )
import Paracletus.Buff
    ( loadDyns, setTileBuff, genDynBuffs
    , clearBuff, moveSlider, printBuff )
import Paracletus.Data
    ( GraphicsLayer(..), Verts(..), FPS(..)
    , DrawState(..), DSStatus(..), LoadResult(..) )
import Paracletus.Draw ( loadTiles )
import Paracletus.Vulkan.Calc ( calcVertices )
import Paracletus.Oblatum.Mouse ( linkTest )
import Control.Concurrent ( threadDelay )
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

-- a different runs for each graphics layer
loadParacletus ∷ Env → GraphicsLayer → IO ()
loadParacletus env Vulkan   = loadParacVulkan env
loadParacletus env OpenGL   = atomically $ writeQueue ec $ EventLogDebug "not yet implemented"
  where ec = envEventQ env
loadParacletus env OpenGLES = atomically $ writeQueue ec $ EventLogDebug "not yet implemented"
  where ec = envEventQ env
loadParacletus env _        = atomically $ writeQueue ec $ EventLogDebug "dont know graphics layer"
  where ec = envEventQ env

-- loop provides calculations for the main thread
-- so loading new objects doesnt stutter the window
loadParacVulkan ∷ Env → IO ()
loadParacVulkan env = do
  runLoadLoop env initDS TStop
  where initDS = DrawState DSSNULL [] [] (FPS 60.0 60 True) 0 []

-- load loop runs with a delay so that
-- it can sleep (ghc threads run like that)
runLoadLoop ∷ Env → DrawState → TState → IO ()
runLoadLoop env ds TStop = do
  --loop starts almost immediately
  let timerChan = envLoadCh env
  tsnew ← atomically $ readChan timerChan
  runLoadLoop env ds tsnew
runLoadLoop env ds TStart = do
  start ← getCurrentTime
  let timerChan = envLoadCh env
  timerstate ← atomically $ tryReadChan timerChan
  tsnew ← case (timerstate) of
    Nothing → return TStart
    Just x  → return x
  ds' ← processCommands env ds
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runLoadLoop env ds' tsnew
-- pause not needed for this timer
runLoadLoop _   _ TPause = return ()
runLoadLoop _   _ TNULL  = return ()

-- command queue processed every tick,
-- logging any errors of all commands
processCommands ∷ Env → DrawState → IO (DrawState)
processCommands env ds = do
    mcmd ← atomically $ tryReadQueue $ envLoadQ env
    case mcmd of
      Just cmd → do
        ret ← processCommand env ds cmd
        case ret of
          -- if command success keep processing commands
          ResSuccess       → processCommands env ds
          ResDrawState ds' → case (dsStatus ds') of
            -- if dsStatus null, update draw state
            DSSNULL        → processCommands env ds'
            DSSExit        → do
              let eventQ = envEventQ env
              atomically $ writeQueue eventQ $ EventExit
              return ds'
            DSSLoadInput link → do
              atomically $ writeQueue (envEventQ env) $ EventInput link
              atomically $ writeQueue (envLoadQ  env) $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadDyns    → do
              atomically $ writeQueue (envLoadQ  env) $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadVerts   → do
              atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadCap cap → do
              atomically $ writeQueue (envEventQ env) $ EventCap True
              atomically $ writeQueue (envLoadQ  env) $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSRecreate    → do
              atomically $ writeQueue (envEventQ env) $ EventRecreate
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLogDebug str → do
              let eventQ = envEventQ env
              atomically $ writeQueue eventQ $ EventLogDebug str
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
          -- errors wont stop command proccessing
          ResError str      → do
            atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "load command error: " ⧺ str
            processCommands env ds
          -- null result will stop processing
          ResNULL           → do
            atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "load null command"
            return ds
      Nothing → return ds

-- commands individually processed
processCommand ∷ Env → DrawState → LoadCmd → IO LoadResult
processCommand env ds cmd = case cmd of
  LoadCmdPrint  arg → do
    let ret = case arg of
                PrintCam      → "no cam defined"
                PrintBuff     → printBuff $ dsBuff ds
                PrintWinElems → printWinElems $ currentWin $ dsWins ds
                PrintNULL     → "no arg " ⧺ (show arg) ⧺ " known"
    atomically $ writeQueue (envEventQ env) $ EventLogDebug $ show ret
    return $ ResSuccess
  LoadCmdSetFPS fps → do
    let ds'    = ds { dsFPS = fps }
        dyns   = loadDyns ds'
    atomically $ writeQueue (envEventQ env) $ EventDyns $ dyns
    return $ ResDrawState ds'
  LoadCmdSetNDefTex nDefTex → do
    return $ ResDrawState ds'
    where ds' = ds { dsNDefTex = nDefTex }
  LoadCmdLink pos → return $ ResDrawState ds'
    where ds' = linkTest pos ds
  LoadCmdVerts → do
    let newVerts = Verts $ calcVertices $ loadTiles ds
        ds'      = ds { dsTiles = loadTiles ds }
        dyns     = loadDyns ds'
    atomically $ writeQueue (envEventQ env) $ EventVerts newVerts
    atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
    return $ ResDrawState ds'
  LoadCmdInitBuff tiles dyns → return $ ResDrawState $ ds { dsTiles = tiles
                                                          , dsBuff  = dyns }
  LoadCmdDyns → do
    let newDyns = loadDyns ds'
        ds'     = ds { dsBuff = genDynBuffs ds }
    atomically $ writeQueue (envEventQ env) $ EventDyns newDyns
    return $ ResDrawState ds'
  LoadCmdBuff n dyns → return $ ResDrawState $ ds { dsBuff = setTileBuff n dyns (dsBuff ds) }
  LoadCmdClear → do
    let ds' = ds { dsBuff = clearBuff (dsBuff ds) 0 }
    atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
    return $ ResDrawState ds'
  LoadCmdNewWin win → return $ ResDrawState ds'
    where ds' = ds { dsWins = win:(dsWins ds) }
  LoadCmdSwitchWin win → do
    let ds' = ds { dsWins = switchWin win (dsWins ds) }
    atomically $ writeQueue (envEventQ env) $ EventModTexs $ calcWinModTexs $ head $ dsWins ds'
    return $ ResDrawState ds'
  LoadCmdNewElem win elem → do
    let wins = dsWins ds
    case (findWin win wins) of
      Nothing  → return $ ResError $ "no window " ⧺ win ⧺ " yet present"
      Just win → do
        let ds'  = ds  { dsWins   = replaceWin win' wins }
            win' = win { winElems = els }
            els  = elem:(winElems win)
        return $ ResDrawState ds'
  LoadCmdNewBit name pane bit → do
    let wins = dsWins ds
    case (findWin name wins) of
      Nothing  → return $ ResError $ "no window " ⧺ name ⧺ " yet present"
      Just win → do
        let ds'        = ds { dsWins = replaceWin win' wins }
            win'       = win { winElems = elems }
            elems      = loadNewBit pane (winElems win) bit
            box        = (2.0,1.0)
            pos'       = ((fst pos) + 6.5, (snd pos) + 0.5)
            (bitL,pos) = findBitPos pane elems
        case bit of
          PaneBitSlider _ _ _ _ → do
            atomically $ writeQueue (envLoadQ env) $ LoadCmdNewElem name $ WinElemLink pos' box $ LinkSlider $ bitL
            atomically $ writeQueue (envEventQ env) $ EventNewInput $ LinkSlider $ bitL
          _ → return ()
        return $ ResDrawState ds'
  LoadCmdInput inp → case inp of
    LCISlider x n → case (currentWin (dsWins ds)) of
      Nothing → return $ ResError "no window"
      Just w  → do
        let dyns = loadDyns ds'
            ds'  = ds { dsWins = replaceWin win (dsWins ds) }
            win  = moveSlider x n w
        atomically $ writeQueue (envEventQ env) $ EventDyns dyns
        return $ ResDrawState ds'
    LCIShell → do
      let ds' = toggleShell ds
      atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
      return $ ResDrawState ds'
    LCINULL → return $ ResError $ "null load input command"
  LoadCmdNULL       → return ResNULL
