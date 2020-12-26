{-# LANGUAGE Strict #-}
module Anamnesis.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify,gets)
import Data.Time.Clock.System
import System.Exit (exitWith, ExitCode(..))
import qualified Paracletus.Oblatum.GLFW as GLFW
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Artos.Data
import Artos.Except
import Artos.Queue
import Artos.Var
import Paracletus.Data
import Paracletus.Oblatum.Event
import Paracletus.Oblatum.Mouse

-- reads event channel, then
-- executes events in order
processEvents ∷ Anamnesis ε σ ()
processEvents = do
  env ← ask
  event ← liftIO $ atomically $ tryReadQueue $ envEventQ env
  case event of
    Just e → do
      processEvent e
      processEvents
    Nothing → return ()
-- case statement on each event
processEvent ∷ Event → Anamnesis ε σ ()
processEvent event = case event of
  (EventError err str) → do
    st ← get
    _  ← logExcept err ExParacletus str
    case (stWindow st) of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → logWarn $ "no glfw window to close"
  (EventLogDebug str) → logDebug str
  (EventExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO $ exitWith ExitSuccess
  (EventKey win k _ ks mk) → evalKey win k ks mk
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventVerts verts) → modify $ \s → s { stVerts = verts
                                       , stReload = RSReload }
  (EventRecreate) → modify $ \s → s { stReload = RSRecreate }
  (EventReload) → modify $ \s → s { stReload = RSReload }
  (EventToggleFPS) → do
    fps ← gets stFPS
    modify $ \s → s { stFPS = toggleFPS fps }
    where toggleFPS ∷ FPS → FPS
          toggleFPS (FPS a b c) = FPS a b (not c)
