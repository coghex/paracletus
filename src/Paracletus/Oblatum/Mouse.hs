{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import qualified Paracletus.Oblatum.GLFW as GLFW
      
-- TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

evalMouse ∷ GLFW.Window → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → Anamnesis ε σ ()
evalMouse win mb mbs _ = do
  when (mb ≡ GLFW.mousebutt1) $ do
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

linkTest ∷ (Double,Double) → DrawState → DrawState
linkTest pos ds = case (currentWin ds) of
  Nothing  → ds
  Just win → linkTestFunc pos elems ds
    where elems = winElems win
linkTestFunc ∷ (Double,Double) → [WinElem] → DrawState → DrawState
linkTestFunc _   []           ds = ds
linkTestFunc pos (elem:elems) ds = case (elem) of
  WinElemLink lpos lbox lact → case (testLink pos lpos lbox) of
    True  → evalLink lact ds
    False → linkTestFunc pos elems ds
  _                          → linkTestFunc pos elems ds
testLink ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
testLink (x1,y1) (x2,y2) (w,h)
  | ((abs(x1 - x2)) < w) ∧ ((abs(y1 - y2)) < h) = True
  | otherwise = False

-- various link actions defined here
evalLink ∷ LinkAction → DrawState → DrawState
evalLink (LinkExit)      ds = ds { dsStatus = DSSExit }
evalLink (LinkBack)      ds = ds { dsWinI  = dsLastI ds
                                 , dsLastI = dsWinI  ds }
evalLink (LinkSlider n)  ds = findSlider n ds
evalLink (LinkLink name) ds = case (findWinI name (dsWins ds)) of
  Nothing → ds
  Just wi → changeWin wi ds
evalLink _               ds = ds
