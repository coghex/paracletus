module Epiklesis.Data where
-- data for generating verticies from lua
import Paracletus.Data

-- draw state is kept in a seperate thread
-- and calculated into verticies
data DrawState = DrawState { dsStatus ∷ DSStatus
                           , dsWins   ∷ [Window]
                           , dsWinI   ∷ Int
                           , dsLastI  ∷ Int
                           , dsTiles  ∷ [Tile]
                           , dsFPS    ∷ FPS
                           } deriving (Show, Eq)

-- status of the loading thread, allowing
-- us to return results of deeply nested
-- pure functions
data DSStatus = DSSLogDebug String | DSSExit | DSSNULL deriving (Show, Eq)

-- gtiles represent abstact tiles
data Tile = GTile { tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  }
-- dtiles represent dynamic tiles,
-- ones that can move and change texture
          | DTile { tDyn   ∷ DynMap
                  , tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  } deriving (Show, Eq)
instance Ord Tile where
  compare (GTile _ _ _ _ _) (GTile _ _ _ _ _) = EQ
  compare (GTile _ _ _ _ _) _ = LT
  compare _ (GTile _ _ _ _ _) = GT
  compare (DTile (DMNULL) _ _ _ _ _) _ = LT
  compare _ (DTile (DMNULL) _ _ _ _ _) = GT
  compare (DTile (DMFPS nl) _ _ _ _ _) (DTile (DMFPS nr) _ _ _ _ _) = compare nl nr
  compare (DTile (DMSlider nl) _ _ _ _ _) (DTile (DMSlider nr) _ _ _ _ _) = compare nl nr
  compare (DTile (DMFPS _) _ _ _ _ _) (DTile (DMSlider _) _ _ _ _ _) = GT
  compare (DTile (DMSlider _) _ _ _ _ _) (DTile (DMFPS _) _ _ _ _ _) = LT


-- function of the particular dyn tile
data DynMap = DMFPS Int
            | DMSlider Int
            | DMNULL deriving (Show, Eq)

-- abstract idea of what a window can be
data Window = Window { winTitle  ∷ String
                     , winType   ∷ WinType
                     , winArgV   ∷ WinArgV
                     , winCursor ∷ (Float,Float,Float)
                     , winElems  ∷ [WinElem]
                     } deriving (Show, Eq)

-- types define some behavior
data WinType = WinTypeMenu | WinTypeGame | WinTypeNULL deriving (Show, Eq)

-- possible arguments from previous window
data WinArgV = WinArgNULL deriving (Show, Eq)

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
  | WinElemNULL deriving (Show, Eq)

-- possible bits in a pane
data PaneBit
  = PaneBitText { bptText ∷ String }
  | PaneBitSlider { pbsText ∷ String
                  , pbsMin  ∷ Int
                  , pbsMax  ∷ Int
                  , pbsVal  ∷ Int }
  | PaneBitNULL deriving (Show, Eq)

-- possible actions when links are clicked
data LinkAction = LinkExit | LinkBack | LinkLink String | LinkSelect Int String | LinkSlider Int | LinkNULL deriving (Show, Eq)

