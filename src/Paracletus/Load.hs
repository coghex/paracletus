{-# LANGUAGE Strict #-}
module Paracletus.Load
  ( loadParacletus )
where
-- we define the thread that helps
-- recreate the swapchain
import Prelude()
import UPrelude
import Artos.Data
import Artos.Var
import Artos.Queue
import Anamnesis.Data
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Draw
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
  where initDS = DrawState DSSNULL [] (-1) (-1) []

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
          LoadCmdNewWin win → return $ ResDrawState ds'
            where ds' = ds { dsWins = win:(dsWins ds) }
          LoadCmdSwitchWin win → case (findWinI win (dsWins ds)) of
            Just n  → return $ ResDrawState $ changeWin n ds
            Nothing → return $ ResError $ "window " ⧺ win ⧺ " not found"
          LoadCmdLink pos → do
            let ds'    = linkTest pos ds
                eventQ = envEventQ env
            atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts
            return $ ResDrawState ds'
          LoadCmdVerts → do
            let newVerts = VertsDF $ calcVertices $ loadTiles ds
            atomically $ writeQueue (envEventQ env) $ EventVerts newVerts
            return ResSuccess
          LoadCmdNewElem name elem → do
            let wins = dsWins ds
            case (findWin name wins) of
              Nothing  → return $ ResError "no window yet present"
              Just win → do
                let ds'   = ds { dsWins = replaceWin win' wins }
                    win'  = win { winElems = elems }
                    elems = elem:(winElems win)
                return $ ResDrawState ds'
          LoadCmdNULL → return ResNULL
