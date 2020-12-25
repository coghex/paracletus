module Epiklesis
  ( loadEpiklesis ) where
-- an interface to lua is defined
import Prelude()
import UPrelude
import Data.List (sort)
import qualified Foreign.Lua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Anamnesis.Data
import Artos.Data
import Artos.Queue
import Artos.Var
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
      Lua.registerHaskellFunction "logDebug" (hsLogDebug env)
      Lua.registerHaskellFunction "rawExit" (hsExit env)
      Lua.registerHaskellFunction "rawNewWindow" (hsNewWindow env)
      Lua.registerHaskellFunction "rawNewText" (hsNewText env)
      Lua.registerHaskellFunction "rawNewLink" (hsNewLink env)
      Lua.registerHaskellFunction "rawCreateWorld" (hsCreateWorld env)
      Lua.registerHaskellFunction "rawSwitchWindow" (hsSwitchWindow env)
      Lua.openlibs
      _ ← Lua.dofile $ "mod/base/game.lua"
      ret ← Lua.callFunc "initParacletus" modFiles
      return (ret∷Int)
    let eventQ = envEventQ env
        loadQ  = envLoadQ  env
    atomically $ writeQueue eventQ $ EventRecreate
    atomically $ writeQueue loadQ  $ LoadCmdVerts
    return ()

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
