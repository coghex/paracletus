module Epiklesis.Elem where
-- functions to design ui elements
import Prelude()
import UPrelude
import Data.List.Split ( splitOn )
import Epiklesis.Data
--import Epiklesis.Shell ( loadShell )
import Paracletus.Data ( Tile(..), DynMap(..) )
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
loadWinElem _ (WinElemPane pos _ bits)    = (calcTextBox posOffset s) ⧺ calcPaneTiles pos bits
  where s         = calcPaneBoxSize bits
        posOffset = ((fst pos) - 0.5, (snd pos) + 0.5)
loadWinElem _ (WinElemWorld wp _ _)       = loadWorldBuff wp
loadWinElem _ (WinElemShell sh bl open)   = loadShell
loadWinElem _ (WinElemLink _ _ _)         = []
loadWinElem _ (WinElemNULL)               = []

-- finds tiles for a window pane
calcPaneTiles ∷ (Double,Double) → [(Int,PaneBit)] → [Tile]
calcPaneTiles _   []                         = []
calcPaneTiles pos ((i,PaneBitText text):pbs) = (calcText (fst pos) pos' text) ⧺ calcPaneTiles pos pbs
  where pos' = ((fst pos) + 0.5,(snd pos) + (fromIntegral i))
calcPaneTiles pos ((i,PaneBitSlider text mn mx (Just vl)):pbs) = (calcText (fst pos) pos' text) ⧺ calcPaneSlider i pos' mn mx vl ⧺ calcPaneTiles pos pbs
  where pos' = ((fst pos) + 1.0,(snd pos) - (fromIntegral i))
calcPaneTiles pos ((i,PaneBitSlider text _  _  Nothing):pbs)   = (calcText (fst pos) pos' text) ⧺ calcPaneTiles pos pbs
  where pos' = ((fst pos) + 1.0,(snd pos) - (fromIntegral i))
calcPaneTiles pos ((_,PaneBitNULL):pbs) = [] ⧺ calcPaneTiles pos pbs

-- figure out what size the pane box should be
calcPaneBoxSize ∷ [(Int,PaneBit)] → (Double,Double)
calcPaneBoxSize []  = (24,1)
calcPaneBoxSize pbs = (24,2.0*fromIntegral(length pbs))

-- create a slider of arbitrary bounds
calcPaneSlider ∷ Int → (Double,Double) → Int → Int → Int → [Tile]
calcPaneSlider n pos mn mx _ = sliderTile ⧺ barTiles ⧺ minTiles ⧺ maxTiles ⧺ valTiles
  where sliderTile = [DTile (DMSlider n) sliderPos (0.1,0.5) (0,0) (1,1) False 93]
        sliderPos  = ((fst pos) + 4.0, (snd pos))
        barTiles   = calcText 0 posBar "<-------->"
        posBar     = ((fst pos) + 4.0, (snd pos))
        minTiles   = calcText 0 posMin $ show mn
        posMin     = ((fst pos) + 3.0, (snd pos))
        maxTiles   = calcText 0 posMax $ show mx
        posMax     = ((fst pos) + 7.5, (snd pos))
        valTiles   = calcDText n 3 0 posVal "0000"
        posVal     = ((fst pos) + 9.5, (snd pos))

-- add bit to window pane
loadNewBit ∷ String → [WinElem] → PaneBit → [WinElem]
loadNewBit _    []       _   = []
loadNewBit pane (we:wes) bit = case we of
  WinElemPane pos name bits
    | name ≡ pane → [WinElemPane pos name bits'] ⧺ loadNewBit pane wes bit
    | otherwise   → [WinElemPane pos name bits]  ⧺ loadNewBit pane wes bit
    where bits' = bits ⧺ [((length bits),bit)]
  we0                       → [we0] ⧺ loadNewBit pane wes bit

-- finds offset of generic bit just added
findBitPos ∷ String → [WinElem] → (Int,(Double,Double))
findBitPos _    []       = (0,(0.0,0.0))
findBitPos pane ((WinElemPane pos name bits):wes)
  | pane ≡ name = (length bits - 1,((fst pos), (snd pos) - (fromIntegral(length bits))))
  | otherwise   = findBitPos pane wes
findBitPos pane (_:wes) = findBitPos pane wes

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
                             , tT     = 100 }
        topRightTile = GTile { tPos   = (x + (0.5*sx) + 0.5,y)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 99 }
        botLeftTile  = GTile { tPos   = (x,y - (0.5*sy) - 0.5)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 103 }
        botRightTile = GTile { tPos   = (x + (0.5*sx) + 0.5, y - (0.5*sy) - 0.5)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,0.5)
                             , tT     = 102 }
        topTile      = GTile { tPos   = (x + (0.25*sx) + 0.25,y)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = ((0.5*sx),0.5)
                             , tT     = 98 }
        leftTile     = GTile { tPos   = (x,y - (0.25*sy) - 0.25)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,(0.5*sy))
                             , tT     = 104 }
        rightTile    = GTile { tPos   = (x + (0.5*sx) + 0.5,y - (0.25*sy) - 0.25)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = (0.5,(0.5*sy))
                             , tT     = 97 }
        bottomTile   = GTile { tPos   = (x + (0.25*sx) + 0.25,y - (0.5*sy) - 0.5)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tScale = ((0.5*sx),0.5)
                             , tT     = 101 }
        middleTile   = GTile { tPos   = (x + (0.25*sx) + 0.25,y - (0.25*sy) - 0.25)
                        , tScale = (0.5*sx,0.5*sy)
                             , tInd   = (0,0)
                             , tSize  = (1,1)
                             , tT     = 96 }

-- functions to convert winelems to gtiles
calcText ∷ Double → (Double,Double) → String → [Tile]
calcText _  _     []         = []
calcText x0 (_,y) ('\n':str) = calcText x0 (x0,(y - 1)) str
calcText x0 (x,y) (' ':str)  = calcText x0 (x + 0.25,y) str
calcText x0 (x,y) (ch:str)   = [textTile] ⧺ calcText x0 (x + chX',y) str
  where textTile = GTile (x+(chX'/2.0),y+chY') (chW',chH') (0,0) (1,1) chIndex
        TTFData chIndex chW chH chX chY = indexTTF ch
        chW' = 0.5*chW
        chH' = 0.5*chH
        chX' = 0.5*chX
        chY' = 0.5*chY
calcDText ∷ Int → Int → Double → (Double,Double) → String → [Tile]
calcDText _ _   _  _     []         = []
calcDText n len x0 (_,y) ('\n':str) = calcDText n len x0 (x0,(y - 1)) str
calcDText n len x0 (x,y) (' ':str)  = calcDText n len x0 (x + 0.25,y) str
calcDText n len x0 (x,y) (ch:str)   = [textTile] ⧺ calcDText n (len - 1) x0 (x + chX',y) str
  where textTile = DTile (DMSliderVal n len) (x+(chX'/2.0),y+chY') (chW',chH') (0,0) (1,1) False chIndex
        TTFData chIndex chW chH chX chY = indexTTF ch
        chW' = 0.5*chW
        chH' = 0.5*chH
        chX' = 0.5*chX
        chY' = 0.5*chY
