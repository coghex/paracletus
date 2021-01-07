module Epiklesis.Map where
-- display of a global map when zoomed out
import Prelude()
import UPrelude
import Epiklesis.Data
import Epiklesis.Window
import Paracletus.Buff
import Paracletus.Data

-- returns in (n,s,e,w) format surrounding elements
cardinals ∷ Int → [[α]] → Maybe [α] → [[(Maybe α,Maybe α,Maybe α,Maybe α)]]
cardinals _ []     _  = []
cardinals w (g:gs) xn = [cardRow g xn xs Nothing] ++ (cardinals w gs (Just g))
  where xs = case gs of
               []  → Nothing
               gs0 → Just $ head gs0

cardRow ∷ [α] → Maybe [α] → Maybe [α] → Maybe α → [(Maybe α,Maybe α,Maybe α,Maybe α)]
cardRow []     _               _               _       = []
cardRow (g:gs) (Nothing)       (Nothing)       xw = [(Nothing,Nothing,xe,xw)] ++ (cardRow gs Nothing Nothing $ Just g)
  where xe = case gs of
               []  → Nothing
               xe0 → Just $ head xe0
cardRow (g:gs) (Just (xn:xns')) (Nothing)       xw = [(Just xn,Nothing,xe,xw)] ++ (cardRow gs xns Nothing $ Just g)
  where xe  = case gs of
                []  → Nothing
                xe0 → Just $ head xe0
        xns = case xns' of
                []  → Nothing
                xn0 → Just xn0
cardRow (g:gs) (Nothing)       (Just (xs:xss')) xw = [(Nothing,Just xs,xe,xw)] ++ (cardRow gs Nothing xss $ Just g)
  where xe  = case gs of
                []  → Nothing
                xe0 → Just $ head xe0
        xss = case xss' of
                []  → Nothing
                xs0 → Just xs0
cardRow (g:gs) (Just (xn:xns')) (Just (xs:xss')) xw = [(Just xn,Just xs,xe,xw)] ++ (cardRow gs xns xss $ Just g)
  where xe  = case gs of
                []  → Nothing
                xe0 → Just $ head xe0
        xns = case xns' of
                []  → Nothing
                xn0 → Just xn0
        xss = case xss' of
                []  → Nothing
                xs0 → Just xs0

setMapBuffs ∷ Int → (Float,Float) → WorldParams → WorldData → [Dyns] → [Dyns]
setMapBuffs nDefTex (cx,cy) wp wd oldBuff = calcMapBuffs 1 nDefTex wp wd (evalScreenCursor segSize (-cx/64.0,-cy/64)) oldBuff
  where segSize = wpSSize wp
calcMapBuffs ∷ Int → Int → WorldParams → WorldData → [(Int,Int)] → [Dyns] → [Dyns]
calcMapBuffs _ _       _  _  []       buff = buff
calcMapBuffs n nDefTex wp wd (sc:scs) buff = calcMapBuffs (n + 1) nDefTex wp wd scs dyns
  where dyns = setTileBuff n (calcMapBuff nDefTex (sh*sw) wp wd sc) buff
        (sw,sh) = wpSSize wp
calcMapBuff ∷ Int → Int → WorldParams → WorldData → (Int,Int) → Dyns
calcMapBuff nDefTex size wp wd curs = Dyns $ res ⧺ (take (size - (length res)) (repeat (DynData 0 (0,0) (1,1) (0,0))))
  where res     = calcMap nDefTex wp wd curs

calcMap ∷ Int → WorldParams → WorldData → (Int,Int) → [DynData]
calcMap nDefTex wp wd curs = [testtile]
  where testtile = DynData (nDefTex+1) (0,0) (w,h) (1,0)
        (w,h)   = (fromIntegral w', fromIntegral h')
        (w',h') = wpSSize wp

