module Epiklesis.Zazz where
-- a collection of land generation functions exist
import Prelude()
import UPrelude
import Epiklesis.Data ( Cards(..), Spot(..) )

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
calcSpotZazz (Spot 1 t b el) (Cards (n,s,e,w)) = Spot 1 t b el
calcSpotZazz (Spot c t b el) (Cards (n,s,e,w)) = Spot c t' b el
  where t' = case n of
               Nothing → t
               Just n0 → case e of
                 Nothing → t
                 Just e0 → case s of
                   Nothing → t
                   Just s0 → case w of
                     Nothing → t
                     Just w0 → calcElevZazz (Spot c t b el) n0 s0 e0 w0
calcElevZazz ∷ Spot → Spot → Spot → Spot → Spot → Int
calcElevZazz (Spot c t b elev) n s e w
  | nBig ∧ sBig ∧ eBig ∧ wBig = 13
  |        sBig ∧ eBig ∧ wBig = 23
  | nBig ∧        eBig ∧ wBig = 22
  | nBig ∧ sBig ∧        wBig = 18
  | nBig ∧ sBig ∧ eBig        = 20
  | nBig ∧ sBig               = 19
  | nBig ∧        eBig        = 11
  | nBig ∧               wBig = 9
  |        sBig ∧ eBig        = 17
  |        sBig ∧        wBig = 15
  |               eBig ∧ wBig = 21
  | nBig                      = 10
  |        sBig               = 16
  |               eBig        = 14
  |                      wBig = 12
  | otherwise                 = t
  where sBig = ((spotElev n) < elev - 12) ∨ (spotCont n ≢ c)
        nBig = ((spotElev s) < elev - 12) ∨ (spotCont s ≢ c)
        eBig = ((spotElev e) < elev - 12) ∨ (spotCont e ≢ c)
        wBig = ((spotElev w) < elev - 12) ∨ (spotCont w ≢ c)

calcGridCliff ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridCliff []         _            = []
calcGridCliff _          []           = []
calcGridCliff (row:grid) (crow:cgrid) = [row'] ⧺ calcGridCliff grid cgrid
  where row' = calcRowCliff row crow
calcRowCliff ∷ [Spot] → [Cards Spot] → [Spot]
calcRowCliff []         _            = []
calcRowCliff _          []           = []
calcRowCliff (spot:row) (cards:crow) = [spot'] ⧺ calcRowCliff row crow
  where spot' = calcSpotCliff spot cards
calcSpotCliff ∷ Spot → Cards Spot → Spot
calcSpotCliff (Spot 1 t b el) (Cards (n,s,e,w)) = Spot 1 t b el
calcSpotCliff (Spot c t b el) (Cards (n,s,e,w)) = Spot c t' b el
  where t' = case n of
               Nothing → t
               Just n0 → case e of
                 Nothing → t
                 Just e0 → case s of
                   Nothing → t
                   Just s0 → case w of
                     Nothing → t
                     Just w0 → calcElevCliff (Spot c t b el) n0 s0 e0 w0
calcElevCliff ∷ Spot → Spot → Spot → Spot → Spot → Int
calcElevCliff (Spot c t b elev) n s e w
  | nCliff          = 42
  | otherwise       = t
  where nCliff = (spotCont s ≡ c) ∧ cliffable (spotTile s)

cliffable ∷ Int → Bool
cliffable 13 = True
cliffable 15 = True
cliffable 16 = True
cliffable 17 = True
cliffable 18 = True
cliffable 19 = True
cliffable 20 = True
cliffable 23 = True
cliffable _  = False

calcGridExtra ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridExtra []         _            = []
calcGridExtra _          []           = []
calcGridExtra (row:grid) (crow:cgrid) = [row'] ⧺ calcGridExtra grid cgrid
  where row' = calcRowExtra row crow
calcRowExtra ∷ [Spot] → [Cards Spot] → [Spot]
calcRowExtra []         _            = []
calcRowExtra _          []           = []
calcRowExtra (spot:row) (cards:crow) = [spot'] ⧺ calcRowExtra row crow
  where spot' = calcSpotExtra spot cards
calcSpotExtra ∷ Spot → Cards Spot → Spot
calcSpotExtra (Spot c t b el) (Cards (n,s,e,w)) = Spot c t' b el
  where t' = case n of
               Nothing → t
               Just n0 → case e of
                 Nothing → t
                 Just e0 → case s of
                   Nothing → t
                   Just s0 → case w of
                     Nothing → t
                     Just w0 → calcElevExtra (Spot c t b el) n0 s0 e0 w0
calcElevExtra ∷ Spot → Spot → Spot → Spot → Spot → Int
calcElevExtra (Spot c 42 b elev) n s e w
  | eCliff ∧ wCliff ∧ sCliff = 43
  | eCliff ∧ wCliff          = 44
  | eCliff ∧          sCliff = 41
  |          wCliff ∧ sCliff = 39
  | eCliff                   = 47
  |          wCliff          = 45
  |                   sCliff = 40
  | otherwise                = 42
  where eCliff = cliffed (spotTile e)
        wCliff = cliffed (spotTile w)
        sCliff = cliffed (spotTile n)
calcElevExtra (Spot c t  b elev) n s e w = t

cliffed ∷ Int → Bool
cliffed 39 = False
cliffed 40 = False
cliffed 41 = False
cliffed 42 = False
cliffed 43 = False
cliffed 44 = False
cliffed 45 = False
cliffed 46 = False
cliffed _  = True
