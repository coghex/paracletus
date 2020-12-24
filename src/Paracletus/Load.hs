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
import Paracletus.Data
import Paracletus.Vulkan.Calc
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
  where initDS = DrawState [] []

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
          ResDrawState ds' → processCommands env ds'
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
          LoadCmdVerts → do
            let newVerts = if ((dsTiles ds) ≡ []) then VertsNULL else VertsDF $ calcVertices $ dsTiles ds
            atomically $ writeQueue (envEventQ env) $ EventVerts newVerts
            return ResSuccess
          LoadCmdNULL → return ResNULL
