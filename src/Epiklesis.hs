module Epiklesis
  ( loadEpiklesis ) where
-- an interface to lua is defined
import Prelude()
import UPrelude
import Data.List (sort)
import Data.Time.Clock ( diffUTCTime, getCurrentTime )
import qualified Foreign.Lua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Anamnesis.Data
import Artos.Data
import Artos.Thread ( threadDelay )
import Artos.Queue ( readChan, tryReadChan, writeQueue )
import Artos.Var ( atomically )
import Epiklesis.Command

loadEpiklesis ∷ Env → IO ()
loadEpiklesis env = do
  modFiles ← findModFiles "mod/game/"
  if (modFiles ≡ []) then do
    let eventQ = envEventQ env
    atomically $ writeQueue eventQ $ EventLogDebug $ "no files in mod/game/"
    return ()
  else do
    let ls = envLuaSt env
    _ ← Lua.runWith ls $ do
      Lua.registerHaskellFunction "rawExit" (hsExit env)
      Lua.registerHaskellFunction "logDebug" (hsLogDebug env)
      Lua.registerHaskellFunction "rawNewWindow" (hsNewWindow env)
      Lua.registerHaskellFunction "rawNewText" (hsNewText env)
      Lua.registerHaskellFunction "rawNewLink" (hsNewLink env)
      Lua.registerHaskellFunction "rawSwitchWindow" (hsSwitchWindow env)
      Lua.openlibs
      _ ← Lua.dofile $ "mod/base/game.lua"
      ret ← Lua.callFunc "initParacletus" modFiles
      return (ret∷Int)
    --let eventQ = envEventQ env
    let loadQ  = envLoadQ  env
    atomically $ writeQueue (envEventQ env) $ EventRecreate
    atomically $ writeQueue loadQ  $ LoadCmdVerts
    epiklesisLoop TStart env modFiles

-- the loop runs lua commands every game tick
epiklesisLoop ∷ TState → Env → String → IO ()
epiklesisLoop TPause env modFiles = do
  let timerChan = envLuaCh env
  tsNew ← atomically $ readChan timerChan
  epiklesisLoop tsNew env modFiles
epiklesisLoop TStart env modFiles = do
  let timerChan = envLuaCh env
  start ← getCurrentTime
  tsMby ← (atomically $ tryReadChan timerChan)
  let tsNew = case (tsMby) of
                Nothing → TStart
                Just x  → x
  let ls = envLuaSt env 
  _ ← Lua.runWith ls $ do
    Lua.openlibs
    _ ← Lua.dofile $ "mod/base/game.lua"
    ret ← Lua.callFunc "runParacletus" modFiles
    return (ret∷Int)
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = n*1000 - usecs
      n     = 1000
  if delay > 0 then threadDelay delay else return ()
  epiklesisLoop tsNew env modFiles
epiklesisLoop TStop _   _        = return ()
epiklesisLoop TNULL _   _        = return ()

findModFiles ∷ String → IO (String)
findModFiles path = do
  paths ← getDirectoryContents "mod/game/"
  return $ collapsePaths $ map (combine path) $ sort $ filter filterOutPathJunk paths
  where filterOutPathJunk ∷ FilePath → Bool
        filterOutPathJunk "."  = False
        filterOutPathJunk ".." = False
        filterOutPathJunk _    = True
        collapsePaths ∷ [String] → String
        collapsePaths [] = ""
        collapsePaths [str]      = str
        collapsePaths (str:strs) = str ⧺ ";" ⧺ collapsePaths strs
