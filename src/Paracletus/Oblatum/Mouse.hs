{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
    ( MonadIO(liftIO), MonadReader(ask)
    , MonadState(get), Anamnesis )
import Anamnesis.Data
    ( Env(envLoadQ, envCamVar)
    , State(stWindow, stInput)
    , InputState(..), InputElem(..)
    , Camera(..) )
import Anamnesis.Util ( logDebug )
import Artos.Data
import Artos.Queue ( writeQueue )
import Artos.Var ( atomically, readTVar, modifyTVar' )
import Epiklesis.Data ( Window(..), WinElem(..), LinkAction(..) )
import Epiklesis.Window ( currentWin, switchWin, backWin, replaceWin )
import Paracletus.Buff ( moveSlider )
import Paracletus.Data ( DrawState(..), DSStatus(..) )
import qualified Paracletus.Oblatum.GLFW as GLFW
      
-- TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = ((x - (1280.0 / 2.0)) / 64.0)
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

evalScroll ∷ GLFW.Window → Double → Double → Anamnesis ε σ ()
evalScroll _ _ y = do
  env ← ask
  liftIO . atomically $ modifyTVar' (envCamVar env) $ \(Camera (cx,cy,cz) mov) → Camera (cx,cy,(min -0.1 $ max -10 $ cz - (0.1*(realToFrac y)))) mov

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
evalLink _     (LinkBack)      ds = ds { dsWins = backWin $ dsWins ds
                                       , dsStatus = DSSLoadVerts }
evalLink (x,_) (LinkSlider n)  ds = case (currentWin (dsWins ds)) of
  Nothing → ds
  Just w  → ds { dsWins   = replaceWin win (dsWins ds)
               , dsStatus = DSSLoadInput (LinkSlider n) }
    where win = moveSlider x n w
evalLink _     (LinkLink name) ds = ds { dsWins = switchWin name $ dsWins ds
                                       , dsStatus = DSSSwitchWin name }
evalLink _     _               ds = ds

-- adds a slider to the input state
addLink ∷ LinkAction → InputState → InputState
addLink (LinkSlider n) is = is { isElems = (isElems is) ⧺ [IESlider False n] }
addLink _ is              = is

-- evaluates mouse click on link
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

-- evaluates constant mouse click
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
      liftIO $ atomically $ writeQueue loadQ $ LoadCmdInput $ LCISlider x $ sliderPressed is

sliderPressed ∷ InputState → Int
sliderPressed is = sliderPressedElem (isElems is)
sliderPressedElem ∷ [InputElem] → Int
sliderPressedElem [] = (-1)
sliderPressedElem ((IESlider True n):_) = n
sliderPressedElem (_:ies) = sliderPressedElem ies

-- reset input elems on mouse unclick
falseInputElems ∷ [InputElem] → [InputElem]
falseInputElems []       = []
falseInputElems (ie:ies) = [ie'] ⧺ falseInputElems ies
  where ie' = case ie of
                IESlider _ a → IESlider False a
                IESelect b a → IESelect b a
                IENULL       → IENULL

