module Paracletus.Elem where
-- functions to design ui elements
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Epiklesis.Data
import Paracletus.Data
import Paracletus.Oblatum.Font

-- finds tiles from a window
loadWindow ∷ Window → [Tile]
loadWindow win = loadWinElems $ winElems win

loadWinElems ∷ [WinElem] → [Tile]
loadWinElems []       = []
loadWinElems (we:wes) = loadWinElem we ⧺ loadWinElems wes
loadWinElem ∷ WinElem → [Tile]
loadWinElem (WinElemText pos True  str) = (calcTextBox size posOffset s) ⧺ calcText size (fst pos) pos str
  where s = calcTextBoxSize size str
        posOffset = ((fst pos) - 0.5, (snd pos) + 0.5)
        size = TextSize30px
loadWinElem (WinElemText pos False str) = calcText TextSize30px (fst pos) pos str
loadWinElem (WinElemPane pos _ bits)    = (calcTextBox size posOffset s) ⧺ calcPaneTiles pos bits
  where s = calcPaneBoxSize size bits
        posOffset = ((fst pos) - 0.5, (snd pos) + 0.5)
        size = TextSize30px
loadWinElem (WinElemLink _ _ _)         = []
loadWinElem (WinElemNULL)               = []

-- finds tiles for window pane
calcPaneTiles ∷ (Double,Double) → [(Int,PaneBit)] → [Tile]
calcPaneTiles _   []                    = []
calcPaneTiles pos ((i,PaneBitSlider text mn mx vl):pbs) = (calcText TextSize30px (fst pos) pos' text) ⧺ calcPaneSlider i pos' mn mx vl ⧺ calcPaneTiles pos pbs
  where pos' = ((fst pos) + 1.0,(snd pos) - (fromIntegral i))
calcPaneTiles pos ((i,PaneBitText text):pbs) = (calcText TextSize30px (fst pos) pos' text) ⧺ calcPaneTiles pos pbs
  where pos' = ((fst pos) + 0.5,(snd pos) + (fromIntegral i))
calcPaneTiles pos ((i,PaneBitNULL):pbs) = [] ⧺ calcPaneTiles pos pbs

-- figure out what size the pane box should be
calcPaneBoxSize ∷ TextSize → [(Int,PaneBit)] → (Double,Double)
calcPaneBoxSize _    []  = (24,1)
calcPaneBoxSize size pbs = (24,2.0*fromIntegral(length pbs))

-- create a slider of arbitrary bounds
calcPaneSlider ∷ Int → (Double,Double) → Int → Int → Int → [Tile]
calcPaneSlider n pos mn mx val = sliderTile ⧺ barTiles ⧺ minTiles ⧺ maxTiles ⧺ valTiles
  where sliderTile = [DTile (DMSlider n) sliderPos (0.1,0.5) (0,0) (1,1) 112]
  --where sliderTile = [GTile sliderPos (0.1,0.5) (0,0) (1,1) 112]
        sliderPos  = ((fst pos) + calcSliderPos mn mx val, (snd pos))
        barTiles   = calcText size 0 posBar "<-------->"
        posBar     = ((fst pos) + 4.0, (snd pos))
        minTiles   = calcText size 0 posMin $ show mn
        posMin     = ((fst pos) + 3.0, (snd pos))
        maxTiles   = calcText size 0 posMax $ show mx
        posMax     = ((fst pos) + 7.5, (snd pos))
        valTiles   = calcText size 0 posVal $ show val
        posVal     = ((fst pos) + 9.5, (snd pos))
        size       = TextSize30px

-- position offset of slider
calcSliderPos ∷ Int → Int → Int → Double
calcSliderPos mn mx val = 4.0 + 3.0*val'/(mx' - mn')
  where mn'  = fromIntegral mn
        mx'  = fromIntegral mx
        val' = fromIntegral val

-- finds offset of generic bit just added
findBitPos ∷ String → [WinElem] → (Int,(Double,Double))
findBitPos _    []       = (0,(0.0,0.0))
findBitPos pane ((WinElemPane pos name bits):wes)
  | pane ≡ name = (length bits,((fst pos), (snd pos) - (fromIntegral(length bits))))
  | otherwise   = findBitPos pane wes
findBitPos pane (we:wes) = findBitPos pane wes

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
