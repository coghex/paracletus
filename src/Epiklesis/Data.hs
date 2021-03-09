module Epiklesis.Data where
-- data for generating verticies from lua

-- abstract idea of what a window can be
data Window = Window { winTitle  ∷ String
                     , winType   ∷ WinType
                     , winArgV   ∷ WinArgV
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
  = WinElemText { textPos ∷ (Double,Double)
                , textBox ∷ Bool
                , textStr ∷ String }
  | WinElemLink { linkPos ∷ (Double,Double)
                , linkBox ∷ (Double,Double)
                , linkAct ∷ LinkAction }
  | WinElemNULL

-- possible actions when links are clicked
data LinkAction = LinkExit
                | LinkBack
                | LinkLink String
                | LinkNULL deriving (Show, Eq)
