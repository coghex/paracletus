{-# LANGUAGE Strict #-}
module Paracletus.Oblatum where
-- the input side of GLFW is handled,
-- loops and control functions are defined
import Prelude()
import UPrelude
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, forever)
import Control.Monad.State.Class (gets, modify)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util
import Artos.Except
import Artos.Var
import Paracletus.Data
import Paracletus.Oblatum.Callback
import Paracletus.Oblatum.Event
import Paracletus.Oblatum.GLFW (WindowHint(..),ClientAPI(..))
import Paracletus.Oblatum.Mouse
import qualified Paracletus.Oblatum.GLFW as GLFW

-- setting of glfw callbacks and hints
initGLFWWindow ∷ Int → Int → String → TVar Bool → Anamnesis ε σ GLFW.Window
initGLFWWindow w h n windowSizeChanged = do
  env ← ask
  let eventQ = envEventQ env
  allocResource
    (\() → liftIO GLFW.terminate ≫ logInfo "terminated glfw")
    (liftIO GLFW.init ⌦ flip unless
      (logExcept GLFWError ExParacletus "failed to init glfw") )
  -- this one we set before we create the window
  liftIO $ GLFW.setErrorCallback $ Just $ errorCallback eventQ
  liftIO GLFW.getVersionString ⌦ mapM_ (logInfo ∘ ("glfw version: " ⧺))
  liftIO GLFW.vulkanSupported ⌦ flip unless
    (logExcept GLFWError ExParacletus "glfw does not support vulkan")
  liftIO ∘ GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  liftIO ∘ GLFW.windowHint $ WindowHint'Resizable True
  allocResource
    ( \window → do
        liftIO (GLFW.destroyWindow window)
        logDebug "closed glfw window"
    ) $ do
    mw ← liftIO $ GLFW.createWindow w h n Nothing Nothing
    case mw of
      Nothing → logExcept GLFWError ExParacletus "failed to init glfw"
      Just window → do
        logDebug "initialized glfw window"
        liftIO $ GLFW.setKeyCallback window $ Just $ keyCallback eventQ
        liftIO $ GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback eventQ
        liftIO $ GLFW.setWindowSizeCallback window $
          Just (\_ _ _ → atomically $ writeTVar windowSizeChanged True)
        return window

loadLoop ∷ GLFW.Window → Anamnesis' ε LoopControl → Anamnesis ε σ Bool
loadLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ ContinueLoop then go else return False
          else return True

glfwMainLoop ∷ GLFW.Window → Anamnesis' ε LoopControl → Anamnesis ε σ Bool
glfwMainLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            newtick ← liftIO getCurTick
            status ← locally action
            reload ← gets stReload
            case reload of
              RSNULL → do
                -- TODO: PID loop
                FPS fps dfps disp ← gets stFPS
                let deltafps = 0.1
                liftIO $ whileM_ ((\cur → (cur - (newtick)) < (1.0/fps)) <$> getCurTick) (liftIO (threadDelay 1000))
                if (dfps > 30) then modify $ \s → s { stFPS = FPS (fps-deltafps) dfps disp }
                else if (dfps < 30) then modify $ \s → s { stFPS = FPS (min 200.0 (fps+deltafps)) dfps disp }
                else modify $ \s → s { stFPS = FPS fps dfps disp }
              _ → return ()
            if status ≡ ContinueLoop then go else return False
            else return True


-- loop for the monad
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ p f = do
  x <- p
  when x $ do f >> whileM_ p f

-- gets time in ms
getCurTick :: IO Double
getCurTick = do
  tickUCT <- getCurrentTime
  return (fromIntegral (round $ utctDayTime tickUCT * 1000000 :: Integer) / 1000000.0 :: Double)

drawLoop ∷ GLFW.Window → Anamnesis' ε Bool → Anamnesis ε σ Bool
drawLoop w action = go
  where go = do
          should ← liftIO $ GLFW.windowShouldClose w
          if not should then do
            status ← locally action
            if status ≡ True then go else return False
          else return True

-- runs glfw in the main thread
-- waiting for events every second
glfwWaitEventsMeanwhile ∷ Anamnesis' ε () → Anamnesis ε σ ()
glfwWaitEventsMeanwhile action = occupyThreadAndFork (liftIO $ forever $ GLFW.waitEventsTimeout 1.0) action

-- glfw will wait when minimized
-- so as not to steal input
glfwWaitMinimized ∷ GLFW.Window → Anamnesis ε σ ()
glfwWaitMinimized win = liftIO go where
  go = do
    (x,y) ← GLFW.getFramebufferSize win
    GLFW.waitEvents
    when (x ≡ 0 ∧ y ≡ 0) go

-- run every frame for handling input
processInput ∷ Anamnesis ε σ ()
processInput = do
  st ← get
  let is = stInput st
      ks = keySt   is
  if ((keyUp ks) ∨ (keyLeft ks) ∨ (keyDown ks) ∨ (keyRight ks) ∨ (((abs (fst (keyAccel ks))) > 0.0) ∨ (abs (snd (keyAccel ks)) > 0.0))) then moveCamWithKeys
  else return ()
  case (mouse1 is) of
    Just _  → if ((sliderPressed is) > 0) then do
                moveSliderWithMouse is
              else return ()
    Nothing → return ()
