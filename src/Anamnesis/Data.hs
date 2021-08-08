{-# LANGUAGE Strict #-}
module Anamnesis.Data where
-- data for continuation monad
import Data.Time.Clock.System
import qualified Control.Monad.Logger as Logger
import qualified Foreign.Lua as Lua
import Artos.Data ( Event, LoadCmd, TState  )
import Artos.Except ( AExcept )
import Artos.Queue ( Queue, TChan )
import Artos.Var ( TVar )
import Paracletus.Data ( Verts, Dyns, FPS )
import qualified Paracletus.Oblatum.GLFW as GLFW

-- possible results of anamnesis
-- specific utility actions
data AnamnResult = AnamnSuccess | AnamnError deriving (Show, Eq)
-- glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- env should only hold pointers/references
data Env = Env { envEventQ ∷ Queue Event
               , envLoadQ  ∷ Queue LoadCmd
               , envLoadCh ∷ TChan TState
               , envLuaCh  ∷ TChan TState
               , envLuaSt  ∷ Lua.State
               , envCamVar ∷ TVar Camera--(Double,Double,Double)
               , envVerts  ∷ TVar (Maybe Verts) }
-- state holds mutable data, and the
-- current status of the whole App
data State = State { stStatus   ∷ AExcept
                   , stLogFunc  ∷ Logger.Loc → Logger.LogSource → Logger.LogLevel → Logger.LogStr → IO ()
                   , stWindow   ∷ !(Maybe GLFW.Window)
                   , stReload   ∷ !ReloadState
                   , stSettings ∷ !Settings
                   , stInput    ∷ !InputState
                   , stStartT   ∷ !SystemTime
                   , stTick     ∷ !(Maybe Double)
                   , stFPS      ∷ !FPS
                   , stNDefTex  ∷ !Int
                   , stModTexs  ∷ ![String]
                   , stDyns     ∷ !Dyns }
-- defines if we want to reload everything and how
data ReloadState = RSReload | RSRecreate | RSNULL deriving (Show, Eq)

-- defines some user alterable settings
data Settings = Settings { sKeyLayout ∷ GLFW.KeyLayout }

data Camera = Camera { cam ∷ (Double,Double,Double)
                     , mov ∷ (Double,Double) } deriving (Show, Eq)

-- input data
data InputState = InputState { mouse1   ∷ Maybe (Float,Float)
                             , mouse2   ∷ Maybe (Float,Float)
                             , mouse3   ∷ Maybe (Float,Float)
                             , isElems  ∷ [InputElem]
                             , inpCap   ∷ Bool
                             , accelCap ∷ Bool
                             , keySt    ∷ ISKeys
                             } deriving (Show, Eq)

-- input state related to various winelems
data InputElem = IESlider Bool Int | IESelect Bool Int | IENULL deriving (Show, Eq)

-- certain keys state
data ISKeys = ISKeys { keyUp    ∷ Bool
                     , keyLeft  ∷ Bool
                     , keyDown  ∷ Bool
                     , keyRight ∷ Bool
                     , keyAccel ∷ (Double,Double)
                     } deriving (Show, Eq)

data Cardinal = North | South | West | East | NorthWest | NorthEast | SouthWest | SouthEast | CardNULL deriving (Show, Eq)
