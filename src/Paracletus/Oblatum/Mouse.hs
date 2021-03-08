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
import Epiklesis.Data ( Window(..), WinElem(..), LinkAction(..) )
import Epiklesis.Window ( currentWin, switchWin )
import Paracletus.Data ( DrawState(..), DSStatus(..) )
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
      liftIO $ atomically $ writeQueue loadQ $ LoadCmdLink pos
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

-- actual link test called from load thread
linkTest ∷ (Double,Double) → DrawState → DrawState
linkTest pos ds = case (currentWin (dsWins ds)) of
  Nothing  → ds
  Just win → linkTestFunc pos elems ds
    where elems = winElems win
linkTestFunc ∷ (Double,Double) → [WinElem] → DrawState → DrawState
linkTestFunc pos []       ds = linklessMouseTest pos ds
linkTestFunc pos (el:els) ds = case el of
  WinElemLink lpos lbox lact → case (testLink pos lpos lbox) of
    True  → evalLink pos lact ds
    False → linkTestFunc pos els ds
  _ → linkTestFunc pos els ds
testLink ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
testLink (x1,y1) (x2,y2) (w,h)
  | ((abs(x1-x2)) < w) ∧ ((abs(y1-y2)) < h) = True
  | otherwise = False
-- if no link was found, execute screen context command
linklessMouseTest ∷ (Double,Double) → DrawState → DrawState
linklessMouseTest pos ds = ds
-- various link actions defined here
evalLink ∷ (Double,Double) → LinkAction → DrawState → DrawState
evalLink _     (LinkExit)      ds = ds { dsStatus = DSSExit }
evalLink _     (LinkLink name) ds = ds { dsWins = switchWin name $ dsWins ds
                                       , dsStatus = DSSLoadVerts }
evalLink _     _               ds = ds
