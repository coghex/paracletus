module Epiklesis.Elem where
-- functions to design ui elements
import Prelude()
import UPrelude
import Data.List.Split ( splitOn )
import Epiklesis.Data
import Paracletus.Data ( Tile(..) )
import Paracletus.Buff
import Paracletus.Oblatum.Font ( indexTTF, TTFData(..) )

-- finds tiles from a window
loadWindow ∷ Int → Window → [Tile]
loadWindow nDefTex win = loadWinElems nDefTex $ winElems win

loadWinElems ∷ Int → [WinElem] → [Tile]
loadWinElems _       []       = []
loadWinElems nDefTex (we:wes) = loadWinElem nDefTex we ⧺ loadWinElems nDefTex wes
loadWinElem ∷ Int → WinElem → [Tile]
loadWinElem _ (WinElemText pos True  str) = (calcTextBox posOffset s) ⧺ calcText (fst pos) pos str
  where s = calcTextBoxSize str
        posOffset = ((fst pos) - 0.5, (snd pos) + 0.5)
loadWinElem _ (WinElemText pos False str) = calcText (fst pos) pos str
loadWinElem _ (WinElemNULL)               = []

-- figure out what size the textbox should be
calcTextBoxSize ∷ String → (Double,Double)
calcTextBoxSize str = (max 1 (calcTBWidth str),fromIntegral (length (splitOn ['\n'] str)))
calcTBWidth ∷ String → Double
calcTBWidth []        = 1.0
calcTBWidth (' ':str) = 0.25 + calcTBWidth str
calcTBWidth (ch:str)  = chWidth + calcTBWidth str
  where chWidth = (chX $ indexTTF ch)
-- create a textbox of arbitrary size
calcTextBox ∷ (Double,Double) → (Double,Double) → [Tile]
calcTextBox (x,y) (sx,sy) = [middleTile,rightTile,leftTile,topTile,bottomTile,topLeftTile,topRightTile,botLeftTile,botRightTile]
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
calcText ∷ Double → (Double,Double) → String → [Tile]
calcText _  _     []         = []
calcText x0 (_,y) ('\n':str) = calcText x0 (x0,(y - 1)) str
calcText x0 (x,y) (' ':str)  = calcText x0 (x + 0.25,y) str
calcText x0 (x,y) (ch:str)   = [textTile] ⧺ calcText x0 (x + chX',y) str
  where textTile = GTile (x+(chX'/2.0),y+chY') (chW',chH') (0,0) (1,1) chIndex
        TTFData chIndex chW chH chX chY = indexTTF ch
        chW' = 0.25*chW
        chH' = 0.25*chH
        chX' = 0.25*chX
        chY' = 0.25*chY
