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
  (EventPrint PrintCam) → do
    st ← get
    logDebug $ "> " ⧺ (show (stCam st))
  (EventExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO $ exitWith ExitSuccess
  (EventKey win k _ ks mk) → evalKey win k ks mk
  (EventMoveCam (x,y,z)) → modify $ \s → s { stCam = (x,y,z) }
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventDyns dyns) → modify $ \s → s { stDynData = dyns }
  (EventVerts verts) → do
    env ← ask
    stRel ← gets stReload
    case stRel of
      RSRecreate → modify $ \s → s { stVerts = verts }
      _          → modify $ \s → s { stVerts = verts
                                   , stReload = RSReload }
    --liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdWorld
  (EventNewInput link) → do
    st ← get
    let oldIS = stInput st
        newIS = addLink link oldIS
    modify $ \s → s { stInput = newIS }
  (EventInput link) → do
    st ← get
    let oldIS = stInput st
        newIS = toggleLink link oldIS
    modify $ \s → s { stInput = newIS }
  (EventCap cap) → do
    st ← get
    let oldIS = stInput st
        newIS = oldIS { inpCap = cap }
    modify $ \s → s { stInput = newIS }
  (EventModTexs modTexs) → modify $ \s → s { stModTexs = modTexs
                                           , stReload = RSRecreate }
  (EventRecreate) → modify $ \s → s { stReload = RSRecreate }
  (EventReload) → do
    stRel ← gets stReload
    logDebug $ show stRel
    case stRel of
      RSRecreate → return ()
      _          → modify $ \s → s { stReload = RSReload }
  (EventToggleFPS) → do
    fps ← gets stFPS
    env ← ask
    let loadQ = envLoadQ env
    modify $ \s → s { stFPS    = toggleFPS fps }
    liftIO $ atomically $ writeQueue loadQ $ LoadCmdVerts
    where toggleFPS ∷ FPS → FPS
          toggleFPS (FPS a b c) = FPS a b (not c)
