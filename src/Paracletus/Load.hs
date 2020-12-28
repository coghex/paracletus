{-# LANGUAGE Strict #-}
module Paracletus.Load
  ( loadParacletus )
where
-- we define the thread that helps
-- recreate the swapchain
import Prelude()
import UPrelude
import Data.List (sort)
import Artos.Data
import Artos.Var
import Artos.Queue
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.Shell
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Draw
import Paracletus.Dyn
import Paracletus.Elem
import Paracletus.Vulkan.Calc
import Paracletus.Oblatum.Mouse (linkTest)
import Control.Concurrent (threadDelay)
import Data.Time.Clock

loadParacletus ∷ Env -> GraphicsLayer → IO ()
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
  where initDS    = DrawState DSSNULL initShell [] (-1) (-1) [] $ FPS 30.0 30 False

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
          ResSuccess → processCommands env ds
          ResDrawState ds' → case (dsStatus ds') of
            DSSNULL → processCommands env ds'
            DSSExit → do
              let eventQ = envEventQ env
              atomically $ writeQueue eventQ $ EventExit
              return ds'
            DSSLoadInput link → do
              let eventQ = envEventQ env
                  loadQ  = envLoadQ  env
              atomically $ writeQueue eventQ $ EventInput link
              atomically $ writeQueue loadQ $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadVerts → do
              atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadDyns → do
              atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLoadCap cap → do
              atomically $ writeQueue (envEventQ env) $ EventCap cap
              atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
            DSSLogDebug str → do
              let eventQ = envEventQ env
              atomically $ writeQueue eventQ $ EventLogDebug str
              processCommands env ds''
                where ds'' = ds' { dsStatus = DSSNULL }
          ResError str → do
            atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "load command error: " ⧺ str
            processCommands env ds
          ResNULL → do
            atomically $ writeQueue (envEventQ env) $ EventLogDebug $ "load null command"
            return ds
      Nothing → return ds

data LoadResult = ResSuccess | ResError String | ResDrawState DrawState | ResNULL deriving (Show, Eq)

processCommand ∷ Env → DrawState → LoadCmd → IO LoadResult
processCommand env ds cmd = case cmd of
          LoadCmdToggleFPS → do
            let eventQ = envEventQ env
                ds'    = ds { dsFPS = toggleFPS (dsFPS ds) }
                toggleFPS ∷ FPS → FPS
                toggleFPS (FPS a b c) = FPS a b (not c)
            atomically $ writeQueue eventQ $ EventToggleFPS
            return $ ResDrawState ds'
          LoadCmdMoveSlider x n → case (currentWin ds) of
            Nothing → return $ ResError "no window"
            Just w  → do
              let eventQ = envEventQ env
                  dyns   = loadDyns ds'
                  ds'    = ds { dsWins = replaceWin win (dsWins ds) }
                  win    = moveSlider x n w
              atomically $ writeQueue eventQ $ EventDyns $ Dyns dyns
              return $ ResDrawState ds'
          LoadCmdSetFPS fps → do
            let eventQ = envEventQ env
                dyns   = loadDyns ds'
                ds'    = ds { dsFPS = fps }
            atomically $ writeQueue eventQ $ EventDyns $ Dyns dyns
            return $ ResDrawState ds'
          LoadCmdShell shCmd → case shCmd of
            ShellCmdTab → return $ ResNULL
            ShellCmdDelete → return $ ResDrawState ds'
              where ds' = ds { dsShell = delShell (dsShell ds)
                             , dsStatus = DSSLoadVerts }
            ShellCmdExec → do
              ds' ← evalShell env ds
              return $ ResDrawState ds'
            ShellCmdString ch → do
              return $ ResDrawState ds'
              where ds' = ds { dsShell = stringShell ch (dsShell ds)
                             , dsStatus = DSSLoadVerts }
            ShellCmdCursor n → do
              return $ ResDrawState ds'
              where ds'  = ds { dsShell = (dsShell ds) { shCursor = newN }
                             , dsStatus = DSSLoadDyns }
                    newN = max 0 $ min (length (shInpStr sh)) $ (shCursor sh) + n
                    sh   = dsShell ds
            ShellCmdOpen  → return $ ResDrawState ds'
              where ds' = ds { dsShell = openShell (dsShell ds)
                             , dsStatus = DSSLoadCap True }
            ShellCmdClose → return $ ResDrawState ds'
              where ds' = ds { dsShell = closeShell (dsShell ds)
                             , dsStatus = DSSLoadCap False }
            ShellCmdNULL  → return $ ResNULL
          LoadCmdNewWin win → return $ ResDrawState ds'
            where ds' = ds { dsWins = win:(dsWins ds) }
          LoadCmdSwitchWin win → case (findWinI win (dsWins ds)) of
            Just n  → return $ ResDrawState $ changeWin n ds
            Nothing → return $ ResError $ "window " ⧺ win ⧺ " not found"
          LoadCmdLink pos → return $ ResDrawState ds'
            where ds' = linkTest pos ds
          LoadCmdDyns → do
            let eventQ = envEventQ env
                newVerts = VertsDF $ calcVertices $ loadTiles ds
                dyns   = loadDyns ds
            atomically $ writeQueue eventQ $ EventDyns $ Dyns dyns
            return ResSuccess
          LoadCmdVerts → do
            let newVerts = VertsDF $ calcVertices $ loadTiles ds
                ds'      = ds { dsTiles = loadTiles ds }
                dyns   = loadDyns ds'
            atomically $ writeQueue (envEventQ env) $ EventVerts newVerts
            atomically $ writeQueue (envEventQ env) $ EventDyns $ Dyns dyns
            return $ ResDrawState ds'
          LoadCmdNewElem name elem → do
            let wins = dsWins ds
            case (findWin name wins) of
              Nothing  → return $ ResError $ "no window " ⧺ name ⧺ " yet present"
              Just win → do
                let ds'   = ds { dsWins = replaceWin win' wins }
                    win'  = win { winElems = elems }
                    elems = elem:(winElems win)
                return $ ResDrawState ds'
          LoadCmdNewBit name pane bit → do
            let wins = dsWins ds
            case (findWin name wins) of
              Nothing  → return $ ResError $ "no window " ⧺ name ⧺ " yet present"
              Just win → do
                let ds'    = ds { dsWins = replaceWin win' wins }
                    win'   = win { winElems = elems }
                    elems  = loadNewBit pane (winElems win) bit
                    box    = (2.0,1.0)
                    loadQ  = envLoadQ env
                    eventQ = envEventQ env
                    pos'   = ((fst pos) + 6.5,(snd pos) + 0.5)
                    (bitL,pos) = findBitPos pane elems
                case bit of
                  PaneBitSlider _ _ _ _ → do
                    atomically $ writeQueue loadQ $ LoadCmdNewElem name $ WinElemLink pos' box $ LinkSlider $ bitL
                    atomically $ writeQueue eventQ $ EventNewInput $ LinkSlider $ bitL
                  _ → return ()
                return $ ResDrawState ds'
          LoadCmdNULL → return ResNULL
