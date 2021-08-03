module Paracletus.Draw where
-- converts epiklesis state to draw state
import Prelude()
import UPrelude
import Paracletus.Data
    ( Tile(..), DrawState(..)
    , FPS(..), DynMap(..) )
import Paracletus.Oblatum.Font ( TTFData(..), indexTTF )
import Epiklesis.Elem ( loadWindow )
import Epiklesis.Window ( currentWin )

-- loads tiles from drawstate
loadTiles ∷ DrawState → [Tile]
loadTiles ds = winTiles ⧺ buffTiles ⧺ fpsTiles
  where fpsTiles  = case (dsFPS ds) of
                     FPS _ _ True  → genFPSTiles
                     FPS _ _ False → []
        winTiles  = case (currentWin (dsWins ds)) of
                     Just win → loadWindow nDefTex win
                     Nothing  → []
        buffTiles = loadTileBuff
        nDefTex   = dsNDefTex ds
genFPSTiles ∷ [Tile]
genFPSTiles = [tile1,tile2,tile3,tile4]
  where tile1 = DTile (DMFPS 3) (6.0,4.0) (chW',chH') (0,0) (1,1) False chIndex
        tile2 = DTile (DMFPS 2) (6.8,4.0) (chW',chH') (0,0) (1,1) False chIndex
        tile3 = DTile (DMFPS 1) (7.6,4.0) (chW',chH') (0,0) (1,1) False chIndex
        tile4 = DTile (DMFPS 0) (8.4,4.0) (chW',chH') (0,0) (1,1) False chIndex
        TTFData chIndex chW chH _   _   = indexTTF '0'
        chW'  = chW
        chH'  = chH

loadTileBuff ∷ [Tile]
loadTileBuff = makeTileBuff 0 64

makeTileBuff ∷ Int → Int → [Tile]
makeTileBuff b n
  | (n ≡ 0)   = []
  | otherwise = makeTileBuff b (n - 1) ⧺ [tile]
  where tile = DTile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) (1,1) False 0
