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
    ( Env(..),
      ReloadState(..),
      State(..) )
import Anamnesis.Util ( logDebug, logExcept, logInfo, logWarn )
import Artos.Data
import Artos.Except ( ExType(ExParacletus) )
import Artos.Queue ( tryReadQueue, writeQueue )
import Artos.Var ( atomically, modifyTVar', modifyTVar )
import Paracletus.Buff ( textDyns, clearDDs )
import Paracletus.Oblatum.Event ( evalKey )
import Paracletus.Oblatum.Mouse ( evalMouse, evalScroll )

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
      PrintCam  → logInfo $ "no cam defined" -- "> " ⧺ (show (stCam st))
      PrintNULL → logInfo $ "print null command"
  (EventExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO $ exitWith ExitSuccess
  (EventKey win k _ ks mk) → evalKey win k ks mk
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventScroll win x y) → evalScroll win x y
  (EventModTexs modTexs) → modify $ \s → s { stModTexs = modTexs
                                           , stReload = RSRecreate }
  (EventDyns dyns) → modify $ \s → s { stDyns = dyns }
  (EventVerts verts) → do
    stRel ← gets stReload
    env ← ask
    case stRel of
      RSRecreate  → liftIO . atomically $ modifyTVar' (envVerts env) $ \_ → Just verts
      _           → do
          liftIO . atomically $ modifyTVar' (envVerts env) $ \_ → Just verts
          modify $ \s → s { stReload = RSReload }
  (EventRecreate) → do
    env ← ask
    st  ← get
    liftIO . atomically $ writeQueue (envLoadQ env) $ LoadCmdClear
    modify $ \s → s { stReload = RSRecreate
                    , stDyns   = clearDDs (stDyns st) }
  (EventReload) → do
    stRel ← gets stReload
    logDebug $ show stRel
    case stRel of
      RSRecreate → return ()
      _          → modify $ \s → s { stReload = RSReload }
