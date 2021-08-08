{-# LANGUAGE StrictData #-}
module Anamnesis.Init
  ( runAnamnesis ) where
-- initialization of env and state
import Prelude()
import UPrelude
import qualified Control.Monad.Logger as Logger
import Data.Time.Clock.System ( getSystemTime )
import qualified Foreign.Lua as Lua
import Anamnesis ( Anamnesis(unAnamnate) )
import Anamnesis.Data
import Artos.Except
import Artos.Queue
import Artos.Var
import Epiklesis.Settings ( initSettings )
import Paracletus.Data ( Dyns(..), FPS(..) )

-- then entire monad is unraveled here, after init functions
runAnamnesis ∷ (Either AExcept α → IO σ) → Anamnesis ε σ α → IO σ
runAnamnesis c p = do
  (envchan,env) ← initEnv
  st            ← initState env
  unAnamnate p envchan st c

-- read only env best for channels, queues, and pointers
initEnv ∷ IO ((TVar Env), Env)
initEnv = do
  -- event queue handles events from main thread
  newQ1 ← newQueue
  -- load queue handles events in a seperate thread
  newQ2 ← newCmdQueue
  -- load channel controls the loading thread
  newC1 ← newTChan
  -- lua state is just a reference
  newLS ← Lua.newstate
  -- lua channel controls the lua interpreter thread
  newC2 ← newTChan
  -- the vert tvar holds verticies and dyns
  newVs ← atomically $ newTVar Nothing
  --newDs ← atomically $ newTVar $ Dyns []
  -- cam tvar so we are not constantly updating state for no reason
  newCam ← atomically $ newTVar (Camera (0,0,(-1)) (0,0))
  let env = Env { envEventQ = newQ1
                , envLoadQ  = newQ2
                , envLoadCh = newC1
                , envLuaCh  = newC2
                , envLuaSt  = newLS
                , envCamVar = newCam
                , envVerts  = newVs }
  -- env accessed transactionally
  envChan ← atomically $ newTVar env
  -- we return both so that initState doesnt need to
  -- load the TVar
  return (envChan, env)
-- read/write state best for mutable data, dont add too much...
initState ∷ Env → IO (TVar State)
initState env = do
  -- the status handles errors
  let ref = AExcept (Just AnamnSuccess) ExAnamnesis ""
  -- the input state holds all keys and mouse
      is  = initInputState
  -- the logger provides levels of warnings/info
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- the system time marks the start of execution
  st ← getSystemTime
  -- default settings
  settings ← initSettings (envLuaSt env) "mod/base/config.lua"
  -- state is accessed transactionally
  atomically $ newTVar State { stStatus   = ref
                             , stLogFunc  = lf
                             , stWindow   = Nothing
                             , stReload   = RSNULL
                             , stSettings = settings
                             , stInput    = is
                             , stStartT   = st
                             , stTick     = Nothing
                             , stFPS      = FPS 60.0 60 True
                             , stNDefTex  = 0
                             , stModTexs  = []
                             , stDyns     = Dyns [] }

initInputState ∷ InputState
initInputState = InputState { mouse1   = Nothing
                            , mouse2   = Nothing
                            , mouse3   = Nothing
                            , isElems  = []
                            , inpCap   = False
                            , accelCap = False
                            , keySt    = initKS }
    where initKS = ISKeys   { keyUp    = False
                            , keyLeft  = False
                            , keyDown  = False
                            , keyRight = False
                            , keyAccel = (0,0) }
