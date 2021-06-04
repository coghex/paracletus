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
        curs = take 9 (evalScreenCursor sSiz (-cx, -cy))
        d0   = take 256 $ repeat $ DynData 0 (0,0) (1,1) (0,0)
        sSiz = wpSSize wp
        (cx,cy) = wdCam wd
genWorldDyns ∷ Int → Int → [(Int,Int)] → WorldParams → WorldData → [DynData] → [DynData]
genWorldDyns nDefTex size curs wp wd d0 = genCursDyns 0 nDefTex size fcs wp wd d0
  where fcs   = map (fixCurs wp) curs
-- generates tiles for each cursor point
genCursDyns ∷ Int → Int → Int → [((Int,Int),(Int,Int))] → WorldParams → WorldData → [DynData] → [DynData]
genCursDyns _ _       _    []     _  _  d0 = d0
genCursDyns n nDefTex size (c:cs) wp wd d0 = genCursDyns n' nDefTex size cs wp wd d1
  where (d1,n') = genCursDynsF n nDefTex size c wp wd d0
genCursDynsF ∷ Int → Int → Int → ((Int,Int),(Int,Int)) → WorldParams → WorldData → [DynData] → ([DynData],Int)
genCursDynsF n nDefTex size ((zi,zj),(i,j)) wp wd d
  | n > size  = (d, n)
  | otherwise = (d',(n + 1))
    where d'        = initlist ⧺ newvals ⧺ taillist
          initlist  = take n d
          taillist  = take (size - n - (length newvals)) $ repeat $ DynData 0 (0,0) (1,1) (0,0)
          newvals   = tile1
          tile1     = [DynData (nDefTex + 3) (2*i'+(2*zj'*zw'),2*j'+(2*zi'*zh')) (1,1) (1,1)]
          (i',j')   = (fromIntegral i,  fromIntegral j)
          (zi',zj') = (fromIntegral zi, fromIntegral zj)
          (zw',zh') = (fromIntegral (fst (wpZSize wp)), fromIntegral (snd (wpZSize wp)))

fixCurs ∷ WorldParams → (Int,Int) → ((Int,Int),(Int,Int))
fixCurs wp (i,j) = ((zi,zj),(i',j'))
  where zi      = (i `div` zw)
        zj      = (j `div` zh)
        i'      = ((i + zw) `mod` zw)
        j'      = ((j + zh) `mod` zh)
        (zw,zh) = wpZSize wp

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
