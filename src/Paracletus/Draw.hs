module Paracletus.Draw where
-- converts epiklesis state to draw state
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Shell (loadShell)
import Epiklesis.Window
import Paracletus.Buff
import Paracletus.Data
import Paracletus.Elem
import Paracletus.Oblatum.Font

loadTiles ∷ DrawState → [Tile]
loadTiles ds = winTiles ⧺ shTiles ⧺ fpsTiles ⧺ buffTiles
  where winTiles  = case (currentWin ds) of
                      Just win → loadWindow nDefTex win
                      Nothing  → []
        fpsTiles  = case (dsFPS ds) of
                      FPS _ _ True  → genFPSTiles
                      FPS _ _ False → []
        shTiles   = loadShell (dsShell ds)
        buffTiles = loadTileBuff
        nDefTex   = dsNDefTex ds

genFPSTiles ∷ [Tile]
genFPSTiles = [tile1,tile2,tile3,tile4]
  where tile1 = DTile (DMFPS 3) (6.0,4.0) (chW',chH') (0,0) (1,1) chIndex
        tile2 = DTile (DMFPS 2) (6.8,4.0) (chW',chH') (0,0) (1,1) chIndex
        tile3 = DTile (DMFPS 1) (7.6,4.0) (chW',chH') (0,0) (1,1) chIndex
        tile4 = DTile (DMFPS 0) (8.4,4.0) (chW',chH') (0,0) (1,1) chIndex
        TTFData chIndex chW chH chX chY = indexTTF TextSize30px '0'
        chW'  = chW
        chH'  = chH
