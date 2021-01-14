module Epiklesis.Zazz where
-- a collection of land generation functions exist
import Prelude()
import UPrelude
import Epiklesis.Data ( Cards(..), Spot(Spot) )

calcGridZazz ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridZazz []         _            = []
calcGridZazz _          []           = []
calcGridZazz (row:grid) (crow:cgrid) = [row'] ⧺ calcGridZazz grid cgrid
  where row' = calcRowZazz row crow
calcRowZazz ∷ [Spot] → [Cards Spot] → [Spot]
calcRowZazz []         _            = []
calcRowZazz _          []           = []
calcRowZazz (spot:row) (cards:crow) = [spot'] ⧺ calcRowZazz row crow
  where spot' = calcSpotZazz spot cards
calcSpotZazz ∷ Spot → Cards Spot → Spot
calcSpotZazz (Spot c t b) (Cards (_,_,_,_)) = Spot c t b

