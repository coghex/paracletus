module Paracletus.Elem where
-- functions to design ui elements
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Font

-- figure out what size the textbox should be
calcTextBoxSize ∷ TextSize → String → (Double,Double)
calcTextBoxSize size str = (max 1 (calcTBWidth size str),fromIntegral (length (splitOn ['\n'] str)))
calcTBWidth ∷ TextSize → String → Double
calcTBWidth _    []        = 1.0
calcTBWidth size (' ':str) = 0.25 + calcTBWidth size str
calcTBWidth size (ch:str)  = chWidth + calcTBWidth size str
  where chWidth = (chX $ indexTTF size ch)
-- create a textbox of arbitrary size
calcTextBox ∷ TextSize → (Double,Double) → (Double,Double) → [Tile]
calcTextBox _    (x,y) (sx,sy) = [middleTile,rightTile,leftTile,topTile,bottomTile,topLeftTile,topRightTile,botLeftTile,botRightTile]
  where topLeftTile  = GTile { tPos   = (x,y)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 6 }
        topRightTile = GTile { tPos   = (x + (0.5*sx) + 0.5,y)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 5 }
        botLeftTile  = GTile { tPos   = (x,y - (0.5*sy) - 0.5)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 9 }
        botRightTile = GTile { tPos   = (x + (0.5*sx) + 0.5, y - (0.5*sy) - 0.5)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 8 }
        topTile      = GTile { tPos   = (x + (0.25*sx) + 0.25,y)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = ((0.5*sx),0.5)
                             , tT     = 4 }
        leftTile     = GTile { tPos   = (x,y - (0.25*sy) - 0.25)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,(0.5*sy))
                             , tT     = 10 }
        rightTile    = GTile { tPos   = (x + (0.5*sx) + 0.5,y - (0.25*sy) - 0.25)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,(0.5*sy))
                             , tT     = 3 }
        bottomTile   = GTile { tPos   = (x + (0.25*sx) + 0.25,y - (0.5*sy) - 0.5)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = ((0.5*sx),0.5)
                             , tT     = 7 }
        middleTile   = GTile { tPos   = (x + (0.25*sx) + 0.25,y - (0.25*sy) - 0.25)
                             , tScale = (0.5*sx,0.5*sy)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tT     = 2 }

-- functions to convert winelems to gtiles
calcText ∷ TextSize → Double → (Double,Double) → String → [Tile]
calcText _        _  _     []         = []
calcText size x0 (_,y) ('\n':str) = calcText size x0 (x0,(y - 1)) str
calcText size x0 (x,y) (' ':str)  = calcText size x0 (x + 0.25,y) str
calcText size x0 (x,y) (ch:str)   = [textTile] ⧺ calcText size x0 (x + chX',y) str
  where textTile = GTile (x+(chX'/2.0),y+chY') (chW',chH') (0,0) (1,1) chIndex
        TTFData chIndex chW chH chX chY = indexTTF size ch
        chW' = case size of
                 TextSize16px → 0.25*chW
                 TextSize30px → 0.5*chW
        chH' = case size of
                 TextSize16px → 0.25*chH
                 TextSize30px → 0.5*chH
        chX' = case size of
                 TextSize16px → 0.25*chX
                 TextSize30px → 0.5*chX
        chY' = case size of
                 TextSize16px → 0.25*chY
                 TextSize30px → 0.5*chY
