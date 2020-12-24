{-# LANGUAGE Strict #-}
module Anamnesis.Data where
-- data for continuation monad
import Data.Time.Clock.System
import qualified Control.Monad.Logger as Logger
import Artos.Data
import Artos.Except
import Artos.Queue
import Paracletus.Data
import qualified Paracletus.Oblatum.GLFW as GLFW

-- possible results of anamnesis
-- specific utility actions
data AnamnResult = AnamnSuccess | AnamnError deriving (Show, Eq)
-- glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- env should only hold pointers/references
data Env = Env { envEventQ ∷ Queue Event
               , envLoadQ  ∷ Queue LoadCmd
               , envLoadCh ∷ TChan TState }
-- state holds mutable data, and the
-- current status of the whole App
data State = State { stStatus  ∷ AExcept
                   , stLogFunc ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO ()
                   , stWindow  ∷ !(Maybe GLFW.Window)
                   , stReload  ∷ !ReloadState
                   , stVerts   ∷ !Verts
                   , stNDefTex ∷ !Int
                   , stInput   ∷ !InputState
                   , stStartT  ∷ !SystemTime
                   , stTick    ∷ !(Maybe Double)
                   , stFPS     ∷ !FPS
                   }
-- fps defined as actual and desired
data FPS = FPS Double Int
-- defines if we want to reload everything and how
data ReloadState = RSReload | RSRecreate | RSNULL deriving (Show, Eq)
