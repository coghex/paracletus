module Epiklesis.World where
import Prelude()
import UPrelude
import Epiklesis.Data
    ( WinElem(..), WorldParams(..)
    , WorldData(..) )
import Paracletus.Buff ( setTileBuff )
import Paracletus.Data
    ( Dyns(..), DynData(..) )

-- returns the first world in the winelems
findWorld ∷ [WinElem] → Maybe (WorldParams, WorldData)
findWorld []                           = Nothing
findWorld ((WinElemWorld wp wd _):wes) = Just (wp,wd)
findWorld (we:wes)                     = findWorld wes

-- generates the world dyns and plugs it in to the buffer
genWorldBuff ∷ [Dyns] → Int → Int → WorldParams → WorldData → [Dyns]
genWorldBuff buff b nDefTex wp wd = setTileBuff b dyns buff
  where dyns = Dyns $ genWorldDyns nDefTex wp wd
genWorldDyns ∷ Int → WorldParams → WorldData → [DynData]
genWorldDyns nDefTex wp wd = take 250 $ repeat (DynData (nDefTex + 1) (0,0) (1,1) (0,0))
