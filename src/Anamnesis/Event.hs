{-# LANGUAGE Strict #-}
module Anamnesis.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify,gets)
import System.Exit (exitWith, ExitCode(..))
import qualified Paracletus.Oblatum.GLFW as GLFW
import Anamnesis
    ( MonadIO(liftIO), MonadReader(ask), MonadState(get), Anamnesis )
import Anamnesis.Data
    ( Env(envLoadQ, envEventQ),
      ReloadState(RSReload, RSRecreate),
      State(stFPS, stModTexs, stVerts, stReload, stAuxData, stCamData,
            stDynData, stInput, stCam, stWindow) )
import Anamnesis.Util ( logDebug, logExcept, logInfo, logWarn )
import Artos.Data
    ( Event(..), LoadCmd(LoadCmdVerts, LoadCmdWorld) )
import Artos.Except ( ExType(ExParacletus) )
import Artos.Queue ( tryReadQueue, writeQueue )
import Artos.Var ( atomically )
import Paracletus.Data
    ( FPS(..),
      InputState(inpCap, accelCap),
      PrintArg(PrintNULL, PrintCam) )
import Paracletus.Oblatum.Event ( evalKey )
import Paracletus.Oblatum.Mouse
    ( addLink, evalMouse, evalScroll, toggleLink )

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
  (EventPrint arg) → do
    st ← get
    case arg of 
      PrintCam  → logInfo $ "> " ⧺ (show (stCam st))
      PrintNULL → logInfo $ "print null command"
  (EventExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO $ exitWith ExitSuccess
  (EventKey win k _ ks mk) → evalKey win k ks mk
  (EventMoveCam (x,y,z)) → modify $ \s → s { stCam = (x,y,z) }
  (EventAccel) → do
    env ← ask
    modify $ \s → s { stInput = (stInput s) { accelCap = False } }
    liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdWorld
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventScroll win x y) → evalScroll win x y
  (EventDyns (dyns,cams,aux)) → modify $ \s → s { stDynData = dyns
                                            , stCamData = cams
                                            , stAuxData = aux }
  (EventVerts verts) → do
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
