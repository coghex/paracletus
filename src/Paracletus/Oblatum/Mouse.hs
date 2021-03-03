{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
    ( MonadIO(liftIO), MonadReader(ask), MonadState(get), Anamnesis )
import Anamnesis.Data
    ( Env(envLoadQ),
      State(stWindow, stInput),
      InputState(..) )
import Artos.Data
import Artos.Queue ( writeQueue )
import Artos.Var ( atomically )
import qualified Paracletus.Oblatum.GLFW as GLFW
      
-- TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

evalScroll ∷ GLFW.Window → Double → Double → Anamnesis ε σ ()
evalScroll _ _ y = return () --do
--  env ← ask
--  let loadQ = envLoadQ env
--  liftIO $ atomically $ writeQueue loadQ $ LoadCmdScroll y

evalMouse ∷ GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalMouse win mb mbs mk = do
  when ((mb ≡ GLFW.mousebutt1) ∧ (not (GLFW.modifierKeysControl mk))) $ do
    if (mbs ≡ GLFW.MouseButtonState'Pressed) then do
      pos'  ← liftIO $ GLFW.getCursorPos win
      env   ← ask
      oldIS ← gets stInput
      let pos   = convertPixels pos'
          loadQ = envLoadQ env
          newIS = oldIS { mouse1 = Just (realToFrac (fst pos'), realToFrac (snd pos')) }
      modify' $ \s → s { stInput = newIS }
      --liftIO $ atomically $ writeQueue loadQ $ LoadCmdLink pos
    else if (mbs ≡ GLFW.MouseButtonState'Released) then do
      oldIS ← gets stInput
      let newIS = oldIS { mouse1 = Nothing }
      modify' $ \s → s { stInput = newIS }
    else return ()
  when ((mb ≡ GLFW.mousebutt3) ∨ ((mb ≡ GLFW.mousebutt1) ∧ (GLFW.modifierKeysControl mk))) $ do
    oldIS ← gets stInput
    case (mouse3 oldIS) of
      Nothing → if (mbs ≡ GLFW.MouseButtonState'Pressed) then do
          pos' ← liftIO $ GLFW.getCursorPos win
          let pos = ((realToFrac (fst pos')),(realToFrac (snd pos')))
              newIS = oldIS { mouse3 = Just pos }
          modify' $ \s → s { stInput = newIS }
        else return ()
      Just _  → if ((mbs ≡ GLFW.MouseButtonState'Released) ≡ True) then do
          let newIS = oldIS { mouse3 = Nothing }
          modify' $ \s → s { stInput = newIS }
        else return ()
