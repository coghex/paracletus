module Epiklesis.Data where
-- data for generating verticies from lua
import Paracletus.Data ( Dyns, FPS )
import System.Random ( StdGen )

-- draw state is kept in a seperate thread
-- and calculated into verticies
data DrawState = DrawState { dsStatus  ∷ DSStatus
                           , dsShell   ∷ Shell
                           , dsCmds    ∷ [String]
                           , dsWins    ∷ [Window]
                           , dsWinI    ∷ Int
                           , dsLastI   ∷ Int
                           , dsTiles   ∷ [Tile]
                           , dsFPS     ∷ FPS
                           , dsBuff    ∷ [Dyns]
                           , dsNDefTex ∷ Int
                           }

-- status of the loading thread, allowing
-- us to return results of deeply nested
-- pure functions
data DSStatus = DSSLogDebug String
              | DSSLoadWorld
              | DSSLoadDyns
              | DSSLoadVerts
              | DSSLoadInput LinkAction
              | DSSLoadCap Bool
              | DSSEvalShell
              | DSSRecreate
              | DSSExit
              | DSSNULL deriving (Show, Eq)

-- gtiles represent abstact tiles
data Tile = GTile { tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int }
-- dtiles represent dynamic tiles,
-- ones that can change texture
          | DTile { tDyn   ∷ DynMap
                  , tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int }
-- mtiles and dmtiles can move with the cam
          | MTile { tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int }
          | DMTile { tDyn  ∷ DynMap
                  , tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  }
          | ATile { tDyn  ∷ DynMap
                  , tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  } deriving (Show, Eq)

-- function of the particular dyn tile
data DynMap = DMFPS Int
            | DMSlider Int
            | DMSliderVal Int Int
            | DMShCursor
            | DMBuff Int Int
            | DMNULL deriving (Show, Eq)

-- user defined parameters
data UserWorldParams = UserWorldParams
 { uwpNConts ∷ Int
 , uwpNSpots ∷ Int
 , uwpSeed   ∷ Int
 , uwpWidth  ∷ Int
 , uwpHeight ∷ Int
 } deriving (Show, Eq)

-- abstract idea of what a window can be
data Window = Window { winTitle  ∷ String
                     , winType   ∷ WinType
                     , winArgV   ∷ WinArgV
                     , winCursor ∷ (Float,Float,Float)
                     , winAccel  ∷ (Float,Float)
                     , winElems  ∷ [WinElem]
                     , winScreen ∷ WinScreen
                     }

-- types define some behavior
data WinType = WinTypeMenu | WinTypeGame | WinTypeNULL deriving (Show, Eq)

-- screens define behavior on a per type basis
data WinScreen = WinScreenElev | WinScreenNULL

-- possible arguments from previous window
data WinArgV = WinArgUWP UserWorldParams | WinArgNULL deriving (Show, Eq)

-- possible elements of a window
data WinElem
  = WinElemText { textPos ∷ (Double,Double)
                , textBox ∷ Bool
                , textStr ∷ String }
  | WinElemLink { linkPos ∷ (Double,Double)
                , linkBox ∷ (Double,Double)
                , linkAct ∷ LinkAction }
  | WinElemPane { panePos  ∷ (Double,Double)
                , paneName ∷ String
                , paneBits ∷ [(Int,PaneBit)] }
  | WinElemWorld { wParams ∷ WorldParams
                 , wData   ∷ WorldData
                 , wDir    ∷ [String] }
  | WinElemNULL


-- world parameters help generate world
data WorldParams = WorldParams { wpSSize ∷ (Int,Int)
                               , wpZSize ∷ (Int,Int)
                               , wpRands ∷ [((Int,Int),(Int,Int))]
                               , wpConts ∷ [(Int,Int)]
                               , wpStdGs ∷ [StdGen]
                               }
-- data stored with each world
data WorldData = WorldData { wdCam   ∷ (Float,Float)
                           , wdZones ∷ [Zone]
                           } deriving (Show, Eq)
-- zones are infinite, but slow
data Zone = Zone { zoneIndex ∷ (Int,Int)
                 , zoneSegs  ∷ [[Segment]]
                 } deriving (Show, Eq)
-- segments are finite, thus faster
data Segment = SegmentNULL
             | Segment { segGrid ∷ [[Spot]]
                       } deriving (Show, Eq)

-- spots can be of a certain image,
-- with a certain atlas subtile
data Spot = Spot { spotCont ∷ Int
                 , spotTile ∷ Int
                 , spotEdge ∷ Maybe (Bool,Bool,Bool,Bool)
                 , spotElev ∷ Double
                 } deriving (Show, Eq)

-- cardinals of a spot
data Cards α = Cards (Maybe α, Maybe α, Maybe α, Maybe α) deriving (Show,Eq)

-- possible bits in a pane
data PaneBit
  = PaneBitText { bptText ∷ String }
  | PaneBitSlider { pbsText ∷ String
                  , pbsMin  ∷ Int
                  , pbsMax  ∷ Int
                  , pbsVal  ∷ Maybe Int }
  | PaneBitNULL deriving (Show, Eq)

-- possible actions when links are clicked
data LinkAction = LinkExit
                | LinkBack
                | LinkLink String
                | LinkSelect Int String
                | LinkSlider Int
                | LinkNULL deriving (Show, Eq)

-- possible actions to load the shell
data ShellCmd = ShellCmdOpen
              | ShellCmdClose
              | ShellCmdControl ShellControl
              | ShellCmdRet String
              | ShellCmdString String
              | ShellCmdExec
              | ShellCmdDelete
              | ShellCmdTab
              | ShellCmdUp
              | ShellCmdDown
              | ShellCmdCursor Int
              | ShellCmdNULL deriving (Show, Eq)

-- possible shell control keys
data ShellControl = ShCtlC | ShCtlA | ShCtlE | ShCtlR | ShCtlG | ShCtlO | ShCtlL | ShCtlU | ShCtlW | ShCtlNULL deriving (Show, Eq)

-- lua shell executes commands in global state
data Shell = Shell { shPrompt ∷ String
                   , shOpen   ∷ Bool
                   , shTabbed ∷ Maybe Int
                   , shCursor ∷ Int
                   , shCBlink ∷ Bool
                   , shInpStr ∷ String
                   , shCache  ∷ String
                   , shOutStr ∷ String
                   , shRet    ∷ String
                   , shHistI  ∷ Int
                   , shHist   ∷ [String] } deriving (Show, Eq)

