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
      Cardinal(..), ISKeys(..), InputState(..) )
import Anamnesis.Util ( logDebug, logInfo )
import Artos.Data
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
  when (GLFW.keyCheck cap keyLayout k "SH") $ if (ks ≡ GLFW.KeyState'Pressed) then do
      liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell ShellCmdToggle
      liftIO $ atomically $ writeQueue (envEventQ env) $ EventCap True
    else return ()
  when cap $ do
    if (ks ≡ GLFW.KeyState'Pressed) then do
      if (GLFW.keyCheck False keyLayout k "SH")
      then do
        liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell ShellCmdToggle
        liftIO $ atomically $ writeQueue (envEventQ env) $ EventCap False
      else if (GLFW.keyCheck False keyLayout k "DEL") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell ShellCmdDelete
      else if (GLFW.keyCheck False keyLayout k "SPC") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdString " "
      else if (GLFW.keyCheck False keyLayout k "RET") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdExec
      else if (GLFW.keyCheck False keyLayout k "TAB") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdTab
      else if (GLFW.keyCheck False keyLayout k "UPA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdDirection ShellUp
      else if (GLFW.keyCheck False keyLayout k "DNA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdDirection ShellDown
      else if (GLFW.keyCheck False keyLayout k "RTA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdDirection ShellRight
      else if (GLFW.keyCheck False keyLayout k "LFA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdDirection ShellLeft
      else if (GLFW.modifierKeysControl mk) then
        if (GLFW.keyCheck False keyLayout k "C") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdControl ShCtlC
        else if (GLFW.keyCheck False keyLayout k "A") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdControl ShCtlA
        else if (GLFW.keyCheck False keyLayout k "E") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdControl ShCtlE
        else return ()
      else do
        ch ← liftIO $ GLFW.calcInpKey k mk
        liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ LCIShell $ ShellCmdString ch
    else return ()
  when ((ks ≡ GLFW.KeyState'Pressed) && (GLFW.keyCheck cap keyLayout k "UPA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "UPA" True
  when ((ks ≡ GLFW.KeyState'Released) && (GLFW.keyCheck cap keyLayout k "UPA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "UPA" False
  when ((ks ≡ GLFW.KeyState'Pressed) && (GLFW.keyCheck cap keyLayout k "DNA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "DNA" True
  when ((ks ≡ GLFW.KeyState'Released) && (GLFW.keyCheck cap keyLayout k "DNA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "DNA" False
  when ((ks ≡ GLFW.KeyState'Pressed) && (GLFW.keyCheck cap keyLayout k "RTA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "RTA" True
  when ((ks ≡ GLFW.KeyState'Released) && (GLFW.keyCheck cap keyLayout k "RTA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "RTA" False
  when ((ks ≡ GLFW.KeyState'Pressed) && (GLFW.keyCheck cap keyLayout k "LFA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "LFA" True
  when ((ks ≡ GLFW.KeyState'Released) && (GLFW.keyCheck cap keyLayout k "LFA")) $ liftIO $ atomically $ writeQueue (envEventQ env) $ EventKeyInput "LFA" False

keyInputState ∷ InputState → String → Bool → InputState
keyInputState inputstate "UPA" ks = inputstate { keySt = newKS, accelCap = newAC }
  where newKS = (keySt inputstate) { keyUp = ks }
        newAC = (accelCap inputstate) ∨ ks
keyInputState inputstate "DNA" ks = inputstate { keySt = newKS, accelCap = newAC }
  where newKS = (keySt inputstate) { keyDown = ks }
        newAC = (accelCap inputstate) ∨ ks
keyInputState inputstate "RTA" ks = inputstate { keySt = newKS, accelCap = newAC }
  where newKS = (keySt inputstate) { keyRight = ks }
        newAC = (accelCap inputstate) ∨ ks
keyInputState inputstate "LFA" ks = inputstate { keySt = newKS, accelCap = newAC }
  where newKS = (keySt inputstate) { keyLeft = ks }
        newAC = (accelCap inputstate) ∨ ks
keyInputState inputstate _     _  = inputstate

-- mouse bools move cam acceleration each frame
moveCamWithKeys ∷ Anamnesis ε σ ()
moveCamWithKeys = do
  env ← ask
  is  ← gets stInput
  let eventQ = envEventQ env 
      ks     = keySt is
      addvec (a,b,c) (d,e,f) = (a+d,b+e,c+f)
      newAccel = decell $ accelIS dir (keyAccel ks)
      dir      = case findDir ks of
                   Just d  → d
                   Nothing → CardNULL
  liftIO $ atomically $ writeQueue eventQ $ EventCam $ CAAccel newAccel
  --liftIO $ atomically $ writeQueue loadQ $ LoadCmdMoveCam (keySt is)
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

calcCam ∷ (Double,Double) → (Double,Double,Double) → (Double,Double,Double)
calcCam (x,y) (cx,cy,cz) = (cx+x,cy+y,cz)

-- accelerate the inputstate
accelIS ∷ Cardinal → (Double,Double) → (Double,Double)
accelIS North (x,y) = (x, 1.1*(y - 0.1))
accelIS West  (x,y) = (1.1*(x + 0.1), y)
accelIS South (x,y) = (x, 1.1*(y + 0.1))
accelIS East  (x,y) = (1.1*(x - 0.1), y)
accelIS NorthWest (x,y) = (1.1*(x + 0.1), 1.1*(y - 0.1))
accelIS NorthEast (x,y) = (1.1*(x - 0.1), 1.1*(y - 0.1))
accelIS SouthWest (x,y) = (1.1*(x + 0.1), 1.1*(y + 0.1))
accelIS SouthEast (x,y) = (1.1*(x - 0.1), 1.1*(y + 0.1))
accelIS CardNULL (x,y) = (x,y)

decell ∷ (Double,Double) → (Double,Double)
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

