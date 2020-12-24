module Epiklesis.Data where
-- data for generating verticies from lua

-- draw state is kept in a seperate thread
-- and calculated into verticies
data DrawState = DrawState { dsWins  ∷ [Window]
                           , dsTiles ∷ [GTile]
                           } deriving (Show, Eq)
-- gtiles represent abstact tiles
data GTile = GTile deriving (Show, Eq)

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
data WinElem = WinElemText { textPos ∷ (Double,Double)
                           , textBox ∷ Bool
                           , textStr ∷ String }
             | WinElemNULL deriving (Show, Eq)
