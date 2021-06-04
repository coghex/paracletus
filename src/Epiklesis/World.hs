module Epiklesis.World where
import Prelude()
import UPrelude
import Epiklesis.Data
    ( WinElem(..), WorldParams(..), WorldData(..)
    , Zone(..), Segment(..), Spot(..), Window(..) )
import Epiklesis.Window
import Paracletus.Buff ( setTileBuff )
import Paracletus.Data
    ( Dyns(..), DynData(..), DrawState(..) )

-- returns the first world in the winelems
findWorld ∷ [WinElem] → Maybe (WorldParams, WorldData)
findWorld []                           = Nothing
findWorld ((WinElemWorld wp wd _):wes) = Just (wp,wd)
findWorld (we:wes)                     = findWorld wes

-- prints the first world found nicely
printWorld ∷ Maybe Window → String
printWorld Nothing  = "no window found"
printWorld (Just w) = case (findWorld (winElems w)) of
  Nothing      → "no world found"
  Just (_ ,wd) → printWorldData wd
printWorldData ∷ WorldData → String
printWorldData wd = show $ wdZones wd

-- generates the world dyns and plugs it in to the buffer
genWorldBuff ∷ [Dyns] → Int → Int → WorldParams → WorldData → [Dyns]
genWorldBuff buff b nDefTex wp wd = setTileBuff b dyns buff
  where dyns = Dyns $ genWorldDyns nDefTex 256 curs wp wd d0
        curs = take 9 (evalScreenCursor sSiz (-cx/64.0, -cy/64.0))
        d0   = take 256 $ repeat $ DynData 0 (0,0) (1,1) (0,0)
        sSiz = wpSSize wp
        (cx,cy) = wdCam wd
genWorldDyns ∷ Int → Int → [(Int,Int)] → WorldParams → WorldData → [DynData] → [DynData]
genWorldDyns nDefTex size curs wp wd d0 = dyns1
  where dyns1 = til1 ⧺ til2 ⧺ (tail (tail d0))
        til1  = [DynData (nDefTex + 3) (2,0) (1,1) (1,1)]
        til2  = [DynData (nDefTex + 3) (4,0) (1,1) (2,2)]

-- returns the list of indecies
-- of world segments to generate
evalScreenCursor ∷ (Int,Int) → (Float,Float) → [(Int,Int)]
evalScreenCursor (w,h) (cx,cy) = [pos,posn,pose,poss,posw,posnw,posne,posse,possw,posnww,posnnw,posnn,posnne,posnee,posene,posee,posese,possee,possse,posss,posssw,possww,poswsw,posww,poswnw]
  where pos    = (x,    y)
        posn   = (x,    y + 1)
        poss   = (x,    y - 1)
        posw   = (x - 1,y)
        pose   = (x + 1,y)
        posnw  = (x - 1,y - 1)
        posne  = (x + 1,y - 1)
        possw  = (x - 1,y + 1)
        posse  = (x + 1,y + 1)
        posnww = (x - 2,y - 2)
        posnnw = (x - 1,y - 2)
        posnn  = (x,    y - 2)
        posnne = (x + 1,y - 2)
        posnee = (x + 2,y - 2)
        posene = (x + 2,y - 1)
        posee  = (x + 2,y)
        posese = (x + 2,y + 1)
        possee = (x + 2,y + 2)
        possse = (x + 1,y + 2)
        posss  = (x,    y + 2)
        posssw = (x - 1,y + 2)
        possww = (x - 2,y + 2)
        poswsw = (x - 2,y + 1)
        posww  = (x - 2,y)
        poswnw = (x - 2,y + 1)
        x      = (-1) + (floor $ cx / w')
        y      = (-1) + (floor $ cy / h')
        w'     = fromIntegral w
        h'     = fromIntegral h
