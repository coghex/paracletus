module Epiklesis.Data where
-- data for generating verticies from lua

-- abstract idea of what a window can be
data Window = Window { winTitle  ∷ String
                     , winType   ∷ WinType
                     , winArgV   ∷ WinArgV
                     , winBuffs  ∷ [Int]
                     , winElems  ∷ [WinElem] }

-- types define some behavior
data WinType = WinTypeMenu | WinTypeGame | WinTypeNULL deriving (Show, Eq)

-- possible arguments from previous window
data WinArgV = WinArgUWP UserWorldParams | WinArgNULL deriving (Show, Eq)

-- user defined parameters
data UserWorldParams = UserWorldParams
 { uwpNConts ∷ Int
 , uwpNSpots ∷ Int
 , uwpSeed   ∷ Int
 , uwpWidth  ∷ Int
 , uwpHeight ∷ Int
 } deriving (Show, Eq)

-- possible elements of a window
data WinElem
  = WinElemText { textPos  ∷ (Double,Double)
                , textBox  ∷ Bool
                , textStr  ∷ String }
  | WinElemLink { linkPos  ∷ (Double,Double)
                , linkBox  ∷ (Double,Double)
                , linkAct  ∷ LinkAction }
  | WinElemPane { panePos  ∷ (Double,Double)
                , paneName ∷ String
                , paneBits ∷ [(Int,PaneBit)] }
  | WinElemShell { shData  ∷ Shell
                 , shBlink ∷ Bool
                 , shOpen  ∷ Bool }
  | WinElemWorld { wParams ∷ WorldParams
                 , wData   ∷ WorldData
                 , wDir    ∷ [String] }
  | WinElemNULL

-- possible actions when links are clicked
data LinkAction = LinkExit
                | LinkBack
                | LinkLink String
                | LinkSlider Int
                | LinkNULL deriving (Show, Eq)

-- possible bits in a pane
data PaneBit = PaneBitText { pbtText ∷ String }
             | PaneBitSlider { pbsText ∷ String
                             , pbsMin  ∷ Int
                             , pbsMax  ∷ Int
                             , pbsVal  ∷ Maybe Int }
             | PaneBitNULL deriving (Show, Eq)

-- lua shell executes commands in global state
data Shell = Shell { shPrompt ∷ String
                   , shTabbed ∷ Maybe Int
                   , shCursor ∷ Int
                   , shInpStr ∷ String
                   , shCache  ∷ String
                   , shOutStr ∷ String
                   , shRet    ∷ String
                   , shHistI  ∷ Int
                   , shHist   ∷ [String] } deriving (Show, Eq)

-- world parameters help generate world
data WorldParams = WorldParams { wpSSize ∷ (Int,Int)
                               , wpZSize ∷ (Int,Int)
                               , wpSize  ∷ (Int,Int)
                               , wpRands ∷ [((Int,Int),(Int,Int))]
                               , wpConts ∷ [(Int,Int)]
                               } deriving (Show, Eq)
-- data stored with each world
data WorldData = WorldData { wdCam    ∷ (Float,Float)
                           , wdZones  ∷ [Zone]
                           , wdSelect ∷ Maybe (Int,Int)
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

