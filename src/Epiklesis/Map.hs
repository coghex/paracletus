module Epiklesis.Map where
-- some functions for handling 2d lists are found
import Prelude()
import UPrelude
import Data.List.Split ( chunksOf )

-- formula for an ellipse
seedDistance ∷ Int → Int → Int → Int → Int → Int → Int
seedDistance x1 y1 x2 y2 x3 y3 = do
  let p1 = (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
      p2 = (((x1-x3)*(x1-x3))+((y1-y3)*(y1-y3)))
  p1*p2

-- these utility functions attach indecies to 2d lists
expandGrid ∷ [α] → (Int,Int) → [([(α,Int)],Int)]
expandGrid m (w,h) = zip (map (workRows w) (chunksOf w m)) [0..h]
workRows ∷ Int → [α] → [(α,Int)]
workRows w l = zip l [0..w]
flattenGrid ∷ [[α]] → [α]
flattenGrid xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []
-- takes a grid with indices and gets rid of them
stripGrid ∷ [([(α,Int)],Int)] → [[α]]
stripGrid ((a,_):ys) = stripRow a : stripGrid ys
stripGrid _          = [[]]
stripRow ∷ [(α,Int)] → [α]
stripRow ((a,_):ys) = a : stripRow ys
stripRow _          = []
-- version with different argument structure
stripGrid2 ∷ [(Int,[(Int,α)])] → [[α]]
stripGrid2 ((_,a):ys) = stripRow2 a : stripGrid2 ys
stripGrid2 _          = [[]]
stripRow2 ∷ [(Int,α)] → [α]
stripRow2 ((_,a):ys) = a : stripRow2 ys
stripRow2 _          = []

-- trims the 2 outside rows and columns surrounding a grid
trimFat ∷ [[α]] → [[α]]
trimFat grid = map trimRows gthin
  where gthin = trimRows grid
trimRows ∷ [α] → [α]
trimRows row = tail $ tail $ init $ init row
