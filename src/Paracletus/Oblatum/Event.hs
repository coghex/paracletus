{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Monad.State.Class (modify',gets)
import Anamnesis
import Anamnesis.Data
import Paracletus.Data
import Artos.Data
import Artos.Queue
import Artos.Var
import Epiklesis.Data
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
  when (GLFW.keyCheck cap keyLayout k "SH") $ if (ks ≡ GLFW.KeyState'Pressed) then
    liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell ShellCmdOpen
    else return ()
  when (GLFW.keyCheck cap keyLayout k "H") $ do
    liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdWorld
  when (GLFW.keyCheck cap keyLayout k "UP") $ do
    let oldIS = stInput st
    if (keyUp (keySt oldIS)) then
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keySt = (keySt oldIS) { keyUp = False } }
        modify' $ \s → s { stInput = newIS }
      else return ()
    else if (ks ≡ GLFW.KeyState'Pressed) then do
      let newIS = oldIS { accelCap = True
                        , keySt = (keySt oldIS) { keyUp = True } }
      modify' $ \s → s { stInput = newIS }
    else return ()
  when (GLFW.keyCheck cap keyLayout k "LFT") $ do
    let oldIS = stInput st
    if (keyLeft (keySt oldIS)) then
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keySt = (keySt oldIS) { keyLeft = False } }
        modify' $ \s → s { stInput = newIS }
      else return ()
    else if (ks ≡ GLFW.KeyState'Pressed) then do
      let newIS = oldIS { accelCap = True
                        , keySt = (keySt oldIS) { keyLeft = True } }
      modify' $ \s → s { stInput = newIS }
    else return ()
  when (GLFW.keyCheck cap keyLayout k "DWN") $ do
    let oldIS = stInput st
    if (keyDown (keySt oldIS)) then
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keySt = (keySt oldIS) { keyDown = False } }
        modify' $ \s → s { stInput = newIS }
      else return ()
    else if (ks ≡ GLFW.KeyState'Pressed) then do
      let newIS = oldIS { accelCap = True
                        , keySt = (keySt oldIS) { keyDown = True } }
      modify' $ \s → s { stInput = newIS }
    else return ()
  when (GLFW.keyCheck cap keyLayout k "RGT") $ do
    let oldIS = stInput st
    if (keyRight (keySt oldIS)) then
      if (ks ≡ GLFW.KeyState'Released) then do
        let newIS = oldIS { keySt = (keySt oldIS) { keyRight = False } }
        modify' $ \s → s { stInput = newIS }
      else return ()
    else if (ks ≡ GLFW.KeyState'Pressed) then do
      let newIS = oldIS { accelCap = True
                        , keySt = (keySt oldIS) { keyRight = True } }
      modify' $ \s → s { stInput = newIS }
    else return ()
  when cap $ if (ks ≡ GLFW.KeyState'Pressed) then do
      if (GLFW.keyCheck False keyLayout k "SH") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell ShellCmdClose
      else if (GLFW.keyCheck False keyLayout k "DEL") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdDelete
      else if (GLFW.keyCheck False keyLayout k "TAB") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdTab
      else if (GLFW.keyCheck False keyLayout k "SPC") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdString " "
      else if (GLFW.keyCheck False keyLayout k "RET") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdExec
      else if (GLFW.keyCheck False keyLayout k "UPA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdUp
      else if (GLFW.keyCheck False keyLayout k "DNA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdDown
      else if (GLFW.keyCheck False keyLayout k "RTA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdCursor 1
      else if (GLFW.keyCheck False keyLayout k "LFA") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdCursor (-1)
      else if (GLFW.modifierKeysControl mk) then
        if (GLFW.keyCheck False keyLayout k "C") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdControl ShCtlC
        else if (GLFW.keyCheck False keyLayout k "A") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdControl ShCtlA
        else if (GLFW.keyCheck False keyLayout k "E") then liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdControl ShCtlE
        else return ()
      else do
          ch ← liftIO $ GLFW.calcInpKey k mk
          liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdString ch
    else return ()

-- mouse bools move cam acceleration each frame
moveCamWithKeys ∷ Anamnesis ε σ ()
moveCamWithKeys = do
  env ← ask
  is  ← gets stInput
  let loadQ    = envLoadQ env 
  liftIO $ atomically $ writeQueue loadQ $ LoadCmdMoveCam (keySt is)
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

