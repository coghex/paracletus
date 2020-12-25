{-# LANGUAGE StrictData #-}
module Anamnesis.Init
  ( runAnamnesis ) where
-- init functions can be found
import Prelude()
import UPrelude
import qualified Control.Monad.Logger as Logger
import Data.Time.Clock.System
import qualified Foreign.Lua as Lua
import Anamnesis
import Anamnesis.Data
import Epiklesis.Settings
import Artos.Except
import Artos.Queue
import Artos.Var
import Paracletus.Data

runAnamnesis ∷ (Either AExcept α → IO σ) → Anamnesis ε σ α → IO σ
runAnamnesis c p = do
  (envchan,env) ← initEnv
  st            ← initState env
  unAnamnate p envchan st c
initEnv ∷ IO ((TVar Env),Env)
initEnv = do
  newQ1 ← newQueue
  newQ2 ← newCmdQueue
  newC1 ← newTChan
  newLS ← Lua.newstate
  let env = Env { envEventQ = newQ1
                , envLoadQ  = newQ2
                , envLoadCh = newC1
                , envLuaSt  = newLS }
  envChan ← atomically $ newTVar env
  return (envChan,env)
initState ∷ Env → IO (TVar State)
initState env = do
  let ref = AExcept (Just AnamnSuccess) ExAnamnesis ""
      is  = initInputState
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  st ← getSystemTime
  settings ← initSettings (envLuaSt env) "mod/base/config.lua"
  atomically $ newTVar State { stStatus   = ref
                             , stLogFunc  = lf
                             , stWindow   = Nothing
                             , stReload   = RSNULL
                             , stVerts    = VertsNULL
                             , stSettings = settings
                             , stNDefTex  = 0
                             , stInput    = is
                             , stStartT   = st
                             , stTick     = Nothing
                             , stFPS      = FPS 30.0 30 }

initInputState ∷ InputState
initInputState = InputState { mouse1      = False
                            , mouse1Cache = (0.0,0.0)
                            , mouse2      = False
                            , mouse2Cache = (0.0,0.0)
                            , mouse3      = False
                            , mouse3Cache = (0.0,0.0)
                            , isElems     = []
                            , inpCap      = False
                            , keyUp       = False
                            , keyLeft     = False
                            , keyDown     = False
                            , keyRight    = False
                            , keyAccel    = (0.0,0.0) }
