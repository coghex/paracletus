{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.ArgV
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Dyn
import qualified Paracletus.Oblatum.GLFW as GLFW
      
-- TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

evalScroll ∷ GLFW.Window → Double → Double → Anamnesis ε σ ()
evalScroll _ _ y = do
  env ← ask
  let loadQ = envLoadQ env
  liftIO $ atomically $ writeQueue loadQ $ LoadCmdScroll y

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
      let newIS = oldIS { mouse1 = Nothing
                        , isElems = falseInputElems (isElems oldIS) }
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

linkTest ∷ (Double,Double) → DrawState → DrawState
linkTest pos ds = case (currentWin ds) of
  Nothing  → ds
  Just win → linkTestFunc pos elems ds
    where elems = winElems win
linkTestFunc ∷ (Double,Double) → [WinElem] → DrawState → DrawState
linkTestFunc _   []           ds = ds
linkTestFunc pos (el:els) ds = case (el) of
  WinElemLink lpos lbox lact → case (testLink pos lpos lbox) of
    True  → evalLink pos lact ds
    False → linkTestFunc pos els ds
  _                          → linkTestFunc pos els ds
testLink ∷ (Double,Double) → (Double,Double) → (Double,Double) → Bool
testLink (x1,y1) (x2,y2) (w,h)
  | ((abs(x1 - x2)) < w) ∧ ((abs(y1 - y2)) < h) = True
  | otherwise = False

-- various link actions defined here
evalLink ∷ (Double,Double) → LinkAction → DrawState → DrawState
evalLink _     (LinkExit)      ds = ds { dsStatus = DSSExit }
evalLink _     (LinkBack)      ds = ds { dsWinI  = dsLastI ds
                                       , dsLastI = dsWinI  ds
                                       , dsStatus = DSSLoadVerts }
evalLink (x,_) (LinkSlider n)  ds = case (currentWin ds) of
  Nothing → ds
  Just w  → ds { dsWins = replaceWin win (dsWins ds)
               , dsStatus = DSSLoadInput (LinkSlider n)}
    where win = moveSlider x n w
evalLink _     (LinkLink name) ds = case (findWinI name (dsWins ds)) of
  Nothing → ds
  Just wi → changeWin wi ds
evalLink _     _               ds = ds

toggleLink ∷ LinkAction → InputState → InputState
toggleLink link is = is { isElems = toggleLinkAction link (isElems is) }
toggleLinkAction ∷ LinkAction → [InputElem] → [InputElem]
toggleLinkAction _    []       = []
toggleLinkAction link (ie:ies) = [ie'] ⧺ toggleLinkAction link ies
  where ie' = case ie of
                IESlider _ _ → toggleAction link ie
                _            → ie

toggleAction ∷ LinkAction → InputElem → InputElem
toggleAction (LinkSlider n) (IESlider b m)
  | n ≡ m     = IESlider (not b) m
  | otherwise = IESlider b       m
toggleAction _ ie = ie

addLink ∷ LinkAction → InputState → InputState
addLink (LinkSlider n) is = is { isElems = (isElems is) ⧺ [IESlider False n] }
addLink _ is              = is

falseInputElems ∷ [InputElem] → [InputElem]
falseInputElems []       = []
falseInputElems (ie:ies) = [ie'] ⧺ falseInputElems ies
  where ie' = case ie of
                IESlider _ a → IESlider False a
                IESelect b a → IESelect b a
                IENULL       → IENULL

moveSliderWithMouse ∷ InputState → Anamnesis ε σ ()
moveSliderWithMouse is = do
  env ← ask
  st ← get
  case (stWindow st) of
    Nothing → return ()
    Just w  → do
      pos ← liftIO $ GLFW.getCursorPos w
      let (x,_) = convertPixels pos
      let loadQ = envLoadQ env
      liftIO $ atomically $ writeQueue loadQ $ LoadCmdMoveSlider x $ sliderPressed is

moveCamWithMouse ∷ (Float,Float) → Anamnesis ε σ ()
moveCamWithMouse oldPos = do
  env ← ask
  st ← get
  case (stWindow st) of
    Nothing → return ()
    Just w  → do
      pos ← liftIO $ GLFW.getCursorPos w
      modify' $ \s → s { stInput = (stInput st) { mouse3 = Just (realToFrac (fst pos), realToFrac (snd pos)) } }
      let loadQ = envLoadQ env
      liftIO $ atomically $ writeQueue loadQ $ LoadCmdMouseCam pos oldPos


sliderPressed ∷ InputState → Int
sliderPressed is = sliderPressedElem (isElems is)
sliderPressedElem ∷ [InputElem] → Int
sliderPressedElem [] = (-1)
sliderPressedElem ((IESlider True n):_) = n
sliderPressedElem (_:ies) = sliderPressedElem ies
