module Paracletus.Draw where
-- converts epiklesis state to draw state
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Data
import Paracletus.Elem
import Paracletus.Oblatum.Font

loadTiles ∷ DrawState → [Tile]
loadTiles ds = fpsTiles ⧺ winTiles
  where winTiles = case (currentWin ds) of
                     Just win → loadWindow win
                     Nothing  → []
        fpsTiles = case (dsFPS ds) of
                     FPS _ _ True  → genFPSTiles
                     FPS _ _ False → []

genFPSTiles ∷ [Tile]
genFPSTiles = [tile1,tile2,tile3,tile4]
  where tile1 = DTile (DMFPS 0) (3.0,2.0) (chW',chH') (0,0) (1,1) chIndex
        tile2 = DTile (DMFPS 1) (3.4,2.0) (chW',chH') (0,0) (1,1) chIndex
        tile3 = DTile (DMFPS 2) (3.8,2.0) (chW',chH') (0,0) (1,1) chIndex
        tile4 = DTile (DMFPS 3) (4.2,2.0) (chW',chH') (0,0) (1,1) chIndex
        TTFData chIndex chW chH chX chY = indexTTF TextSize30px '0'
        chW'  = 0.5*chW
        chH'  = 0.5*chH
