{-# LANGUAGE StrictData #-}
module Anamnesis.Init
  ( runAnamnesis ) where
-- init functions can be found
import Prelude()
import UPrelude
import qualified Control.Monad.Logger as Logger
import Anamnesis
import Anamnesis.Data
import Artos.Except
import Artos.Queue
import Artos.Var

runAnamnesis ∷ (Either AExcept α → IO σ) → Anamnesis ε σ α → IO σ
runAnamnesis c p = do
  env ← initEnv
  st  ← initState
  unAnamnate p env st c
initEnv ∷ IO (TVar Env)
initEnv = do
  newQ1 ← newQueue
  atomically $ newTVar Env { envEventQ  = newQ1 }
initState ∷ IO (TVar State)
initState = do
  let ref = AExcept (Just AnamnSuccess) ExAnamnesis ""
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  atomically $ newTVar State { stStatus    = ref
                             , stLogFunc   = lf }
