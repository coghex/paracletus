{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
    ( MonadIO(liftIO), MonadReader(ask), MonadState(get), Anamnesis )
import Anamnesis.Data
    ( Env(..), Settings(sKeyLayout), State(..),
      Cardinal(..),
      ISKeys(keyRight, keyDown, keyLeft, keyUp),
      InputState(..) )
import Anamnesis.Util ( logDebug, logInfo )
import Artos.Data ( LoadCmd(..), Event(..) )
import Artos.Queue ( writeQueue )
import Artos.Var ( atomically )
import qualified Paracletus.Oblatum.GLFW as GLFW

evalKey ∷ GLFW.Window → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalKey window k ks mk = do
  env ← ask
  st  ← get
  let keyLayout = sKeyLayout $ stSettings st
      cap       = inpCap $ stInput st
  -- glfw is parent thread, so this
  -- will close everything
  when (GLFW.keyCheck False keyLayout k "ESC") $ liftIO $ GLFW.setWindowShouldClose window True
  when (ks ≡ GLFW.KeyState'Pressed) $ do
    when (GLFW.keyCheck False keyLayout k "L") $ liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdVerts

-- mouse bools move cam acceleration each frame
moveCamWithKeys ∷ Anamnesis ε σ ()
moveCamWithKeys = do
  env ← ask
  is  ← gets stInput
  let loadQ    = envLoadQ env 
  return ()--liftIO $ atomically $ writeQueue loadQ $ LoadCmdMoveCam (keySt is)
  --st  ← get
  --let oldIS    = stInput st
  --    newIS    = oldIS { keySt = (keySt oldIS) { keyAccel = newaccel } }
  --    newaccel = decell $ accelIS dir (keyAccel (keySt oldIS))
  --    dir      = case (findDir (keySt oldIS)) of
  --                 Just d  → d
  --                 Nothing → CardNULL
  --let loadQ    = envLoadQ env 
  --liftIO $ atomically $ writeQueue loadQ $ LoadCmdMoveCam $ addZ newaccel (-1.0)
  --modify' $ \s → s { stInput = newIS }

calcCam ∷ (Float,Float) → (Float,Float,Float) → (Float,Float,Float)
calcCam (x,y) (cx,cy,cz) = (cx+x,cy+y,cz)

-- accelerate the inputstate
accelIS ∷ Cardinal → (Float,Float) → (Float,Float)
accelIS North (x,y) = (x, 1.1*(y - 0.1))
accelIS West  (x,y) = (1.1*(x + 0.1), y)
accelIS South (x,y) = (x, 1.1*(y + 0.1))
accelIS East  (x,y) = (1.1*(x - 0.1), y)
accelIS NorthWest (x,y) = (1.1*(x + 0.1), 1.1*(y - 0.1))
accelIS NorthEast (x,y) = (1.1*(x - 0.1), 1.1*(y - 0.1))
accelIS SouthWest (x,y) = (1.1*(x + 0.1), 1.1*(y + 0.1))
accelIS SouthEast (x,y) = (1.1*(x - 0.1), 1.1*(y + 0.1))
accelIS CardNULL (x,y) = (x,y)

decell ∷ (Float,Float) → (Float,Float)
decell (x,y)
  | ((abs x) < 0.01) ∧ ((abs y) < 0.01) = (0.0,0.0)
  | ((abs x) < 0.01) = (0.0,(y / 1.1))
  | ((abs y) < 0.01) = ((x / 1.1),0.0)
  | otherwise = ((x / 1.1),(y / 1.1))

-- many keys can be held at once,
-- we define bahavior of all
-- combinations here
findDir ∷ ISKeys → Maybe Cardinal
findDir is = if      (keyUp    is) ∧ (keyLeft  is) ∧ (keyRight is) ∧ (keyDown  is) then Nothing
             else if (keyUp    is) ∧ (keyLeft  is) ∧ (keyRight is) then Just North
             else if (keyUp    is) ∧ (keyLeft  is) ∧ (keyDown  is) then Just West
             else if (keyUp    is) ∧ (keyRight is) ∧ (keyDown  is) then Just East
             else if (keyUp    is) ∧ (keyLeft  is) then Just NorthWest
             else if (keyUp    is) ∧ (keyRight is) then Just NorthEast
             else if (keyUp    is) ∧ (keyDown  is) then Nothing
             else if (keyUp    is) then Just North
             else if (keyDown  is) ∧ (keyLeft  is) ∧ (keyRight is) then Just South
             else if (keyDown  is) ∧ (keyLeft  is) then Just SouthWest
             else if (keyDown  is) ∧ (keyRight is) then Just SouthEast
             else if (keyDown  is) then Just South
             else if (keyLeft  is) ∧ (keyRight is) then Nothing
             else if (keyLeft  is) then Just West
             else if (keyRight is) then Just East
             else Nothing

