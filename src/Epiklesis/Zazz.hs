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
calcElevCliff (Spot c 0 b elev) n s e w
  | lbPlat ∧ rbPlat ∧ ltPlat ∧ rtPlat = 19
  | lbPlat ∧ rbPlat = 16
  | ltPlat ∧ rtPlat = 10
  | brPlat ∧ blPlat ∧ trPlat ∧ tlPlat = 22
  | brPlat ∧ trPlat = 14
  | blPlat ∧ tlPlat = 12
  | otherwise       = 0
  where rbPlat = rbplatauable $ spotTile e
        lbPlat = lbplatauable $ spotTile w
        rtPlat = rtplatauable $ spotTile e
        ltPlat = ltplatauable $ spotTile w
        brPlat = brplatauable $ spotTile s
        blPlat = blplatauable $ spotTile s
        trPlat = trplatauable $ spotTile n
        tlPlat = tlplatauable $ spotTile n
calcElevCliff (Spot c t b elev) n s e w
  | nCliff          = 42
  | otherwise       = t
  where nCliff = (spotCont s ≡ c) ∧ cliffable (spotTile s)

-- platauable lb means left bottom, i.e. tile to the left has
-- cliff on bottom, rt means right top, etc...
brplatauable ∷ Int → Bool
brplatauable  9 = True
brplatauable 10 = True
brplatauable 11 = True
brplatauable 13 = True
brplatauable 14 = True
brplatauable 17 = True
brplatauable 18 = True
brplatauable 19 = True
brplatauable 20 = True
brplatauable 21 = True
brplatauable 22 = True
brplatauable 23 = True
brplatauable 39 = True
brplatauable 40 = True
brplatauable 41 = True
brplatauable 42 = True
brplatauable 43 = True
brplatauable 44 = True
brplatauable 45 = True
brplatauable 46 = True
brplatauable 47 = True
brplatauable _  = False

blplatauable ∷ Int → Bool
blplatauable  9 = True
blplatauable 10 = True
blplatauable 11 = True
blplatauable 12 = True
blplatauable 13 = True
blplatauable 15 = True
blplatauable 18 = True
blplatauable 19 = True
blplatauable 20 = True
blplatauable 21 = True
blplatauable 22 = True
blplatauable 23 = True
blplatauable 39 = True
blplatauable 40 = True
blplatauable 41 = True
blplatauable 42 = True
blplatauable 43 = True
blplatauable 44 = True
blplatauable 45 = True
blplatauable 46 = True
blplatauable 47 = True
blplatauable _  = False

trplatauable ∷ Int → Bool
trplatauable  9 = True
trplatauable 10 = True
trplatauable 11 = True
trplatauable 13 = True
trplatauable 14 = True
trplatauable 17 = True
trplatauable 18 = True
trplatauable 19 = True
trplatauable 20 = True
trplatauable 21 = True
trplatauable 22 = True
trplatauable 23 = True
trplatauable 39 = True
trplatauable 40 = True
trplatauable 41 = True
trplatauable 42 = True
trplatauable 43 = True
trplatauable 44 = True
trplatauable 45 = True
trplatauable 46 = True
trplatauable 47 = True
trplatauable _  = False

tlplatauable ∷ Int → Bool
tlplatauable  9 = True
tlplatauable 10 = True
tlplatauable 11 = True
tlplatauable 12 = True
tlplatauable 13 = True
tlplatauable 15 = True
tlplatauable 18 = True
tlplatauable 19 = True
tlplatauable 20 = True
tlplatauable 21 = True
tlplatauable 22 = True
tlplatauable 23 = True
tlplatauable 39 = True
tlplatauable 40 = True
tlplatauable 41 = True
tlplatauable 42 = True
tlplatauable 43 = True
tlplatauable 44 = True
tlplatauable 45 = True
tlplatauable 46 = True
tlplatauable 47 = True
tlplatauable _  = False

lbplatauable ∷ Int → Bool
lbplatauable 11 = True
lbplatauable 13 = True
lbplatauable 14 = True
lbplatauable 15 = True
lbplatauable 16 = True
lbplatauable 17 = True
lbplatauable 18 = True
lbplatauable 19 = True
lbplatauable 20 = True
lbplatauable 21 = True
lbplatauable 22 = True
lbplatauable 23 = True
lbplatauable 39 = True
lbplatauable 40 = True
lbplatauable 41 = True
lbplatauable 42 = True
lbplatauable 43 = True
lbplatauable 44 = True
lbplatauable 45 = True
lbplatauable 46 = True
lbplatauable 47 = True
lbplatauable _  = False

rbplatauable ∷ Int → Bool
rbplatauable  9 = True
rbplatauable 12 = True
rbplatauable 13 = True
rbplatauable 15 = True
rbplatauable 16 = True
rbplatauable 17 = True
rbplatauable 18 = True
rbplatauable 19 = True
rbplatauable 20 = True
rbplatauable 21 = True
rbplatauable 22 = True
rbplatauable 23 = True
rbplatauable 39 = True
rbplatauable 40 = True
rbplatauable 41 = True
rbplatauable 42 = True
rbplatauable 43 = True
rbplatauable 44 = True
rbplatauable 45 = True
rbplatauable 46 = True
rbplatauable 47 = True
rbplatauable _  = False

ltplatauable ∷ Int → Bool
ltplatauable  9 = True
ltplatauable 10 = True
ltplatauable 11 = True
ltplatauable 13 = True
ltplatauable 14 = True
ltplatauable 17 = True
ltplatauable 18 = True
ltplatauable 19 = True
ltplatauable 20 = True
ltplatauable 21 = True
ltplatauable 22 = True
ltplatauable 23 = True
ltplatauable 39 = True
ltplatauable 40 = True
ltplatauable 41 = True
ltplatauable 42 = True
ltplatauable 43 = True
ltplatauable 44 = True
ltplatauable 45 = True
ltplatauable 46 = True
ltplatauable 47 = True
ltplatauable _  = False

rtplatauable ∷ Int → Bool
rtplatauable  9 = True
rtplatauable 10 = True
rtplatauable 11 = True
rtplatauable 12 = True
rtplatauable 13 = True
rtplatauable 14 = True
rtplatauable 15 = True
rtplatauable 17 = True
rtplatauable 18 = True
rtplatauable 19 = True
rtplatauable 20 = True
rtplatauable 21 = True
rtplatauable 22 = True
rtplatauable 23 = True
rtplatauable 39 = True
rtplatauable 40 = True
rtplatauable 41 = True
rtplatauable 42 = True
rtplatauable 43 = True
rtplatauable 44 = True
rtplatauable 45 = True
rtplatauable 46 = True
rtplatauable 47 = True
rtplatauable _  = False

-- whether or not cliffs should go here
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
        sCliff = cliffed (spotTile n) ∧ contB
        contB  = (spotCont n) ≡ c
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

-- fills in all the empty gaps
calcGridFluff ∷ [[Spot]] → [[Cards Spot]] → [[Spot]]
calcGridFluff []         _            = []
calcGridFluff _          []           = []
calcGridFluff (row:grid) (crow:cgrid) = [row'] ⧺ calcGridFluff grid cgrid
  where row' = calcRowFluff row crow
calcRowFluff ∷ [Spot] → [Cards Spot] → [Spot]
calcRowFluff []         _            = []
calcRowFluff _          []           = []
calcRowFluff (spot:row) (cards:crow) = [spot'] ⧺ calcRowFluff row crow
  where spot' = calcSpotFluff spot cards
calcSpotFluff ∷ Spot → Cards Spot → Spot
calcSpotFluff (Spot c 0 b el) (Cards (n,s,e,w)) = Spot c t' b el
  where t' = 1
calcSpotFluff (Spot c t b el) (Cards (n,s,e,w)) = Spot c t b el
