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
    ( Env(..), ReloadState(..), ISKeys(..)
    , State(..), InputState(..), Camera(..) )
import Anamnesis.Util ( logDebug, logExcept, logInfo, logWarn )
import Artos.Data
import Artos.Except ( ExType(ExParacletus) )
import Artos.Queue ( tryReadQueue, writeQueue )
import Artos.Var ( atomically, modifyTVar', modifyTVar, readTVar )
import Paracletus.Buff ( textDyns, clearDDs )
import Paracletus.Oblatum.Event ( evalKey, keyInputState )
import Paracletus.Oblatum.Mouse
    ( evalMouse, evalScroll
    , addLink, toggleLink )

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
  (EventLogInfo  str) → logInfo  str
  (EventPrint arg) → do
    env ← ask
    st  ← get
    case arg of 
      PrintCam  → do
        cam ← liftIO . atomically $ readTVar (envCamVar env)
        logInfo $ "> " ⧺ (show cam)
      PrintNULL → logInfo $ "print null command"
  (EventExit) → do
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO $ exitWith ExitSuccess
  (EventKey win k _ ks mk) → evalKey win k ks mk
  (EventMouseButton win mb mbs mk) → evalMouse win mb mbs mk
  (EventScroll win x y) → evalScroll win x y
  (EventModTexs modTexs) → modify $ \s → s { stModTexs = modTexs }
  (EventLoad perc) → do
    env ← ask
    liftIO . atomically $ modifyTVar' (envVerts env) $ const Nothing
    liftIO . atomically $ writeQueue (envLoadQ env) LoadCmdLoadWin
    modify $ \s → s { stReload = RSReload
                    , stDyns = textDyns 64 (-3,-8) ("Loading... " ⧺ (show perc) ⧺ "%") }
  (EventDyns dyns) → modify $ \s → s { stDyns = dyns }
  (EventVerts verts) → do
    stRel ← gets stReload
    env ← ask
    case stRel of
      RSRecreate  → liftIO . atomically $ modifyTVar' (envVerts env) $ \_ → Just verts
      _           → do
          liftIO . atomically $ modifyTVar' (envVerts env) $ \_ → Just verts
          modify $ \s → s { stReload = RSRecreate }
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
  (EventKeyInput key ks) → do
    st ← get
    let oldIS = stInput st
        newIS = keyInputState oldIS key ks
    modify $ \s → s { stInput = newIS }
  (EventCap cap) → do
    st ← get
    let oldIS = stInput st
        newIS = oldIS { inpCap = cap }
    modify $ \s → s { stInput = newIS }
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
  (EventCam camaction) → case camaction of
    CASet cam → do
      env ← ask
      liftIO . atomically $ modifyTVar' (envCamVar env) $ const $ Camera cam (0,0)
    CAMove move → do
      env ← ask
      liftIO . atomically $ modifyTVar' (envCamVar env) $ \cam → moveCam cam move
        where moveCam (Camera (cx,cy,cz) _) (x,y,z) = Camera (cx+x,cy+y,cz+z) (0,0)
    CAAccel (x,y) → do
      env ← ask
      st ← get
      let is = stInput st
          ks = keySt is
      if (((abs x) ≤ 0.0) ∧ ((abs y) ≤ 0.0)) then do
        liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
        modify $ \s → s { stInput = is { accelCap = False
                                       , keySt = ks { keyAccel = (0.0,0.0) } } }
      else do
        (Camera oldcam (ox,oy)) ← liftIO . atomically $ readTVar (envCamVar env)
        -- checks if we have drifted too far
        -- and reloads the dyns, thus the world
        if ((abs ox) > 100) then do
          liftIO . atomically $ modifyTVar' (envCamVar env) $
            \(Camera cam (_,my)) → Camera cam (0,my)
          liftIO . atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
        else return ()
        if ((abs oy) > 100) then do
          liftIO . atomically $ modifyTVar' (envCamVar env) $
            \(Camera cam (mx,_)) → Camera cam (mx,0)
          liftIO . atomically $ writeQueue (envLoadQ env) $ LoadCmdDyns
        else return ()
        -- also sets the world cam to the old cam
        liftIO . atomically $ writeQueue (envLoadQ env) $ LoadCmdCam oldcam
        modify $ \s → s { stInput = is { keySt = ks { keyAccel = (x,y) } } }
        liftIO . atomically $ modifyTVar' (envCamVar env)
          $ \(Camera (cx,cy,cz) (mx,my)) → Camera (cx+x,cy+y,cz) (mx+x,my+y)
    CANULL → return ()
