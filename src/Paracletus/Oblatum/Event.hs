{-# LANGUAGE Strict #-}
module Paracletus.Oblatum.Event where
-- key input is handled
import Prelude()
import UPrelude
import Control.Monad (when)
import Anamnesis
import Anamnesis.Data
import Anamnesis.Util (logDebug)
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
        else return ()
      else do
          ch ← liftIO $ GLFW.calcInpKey k mk
          liftIO $ atomically $ writeQueue (envLoadQ env) $ LoadCmdShell $ ShellCmdString ch
    else return ()
