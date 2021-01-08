module Epiklesis.Rand where
-- some helper functions to generate random lists
--import Prelude()
--import UPrelude
import System.Random

-- rands help generate random ellipses
genRands ∷ StdGen → StdGen → Int → Int → Int → [((Int,Int),(Int,Int))]
genRands sg0 sg1 n w h = buildList2 (xl,yl)
  where xl  = buildList2 (xxl,xyl)
        yl  = buildList2 (yxl,yyl)
        xxl = randomList (-w',w') n sg0
        xyl = randomList (-h',h') n sg0
        yxl = randomList (-w',w') n sg1
        yyl = randomList (-h',h') n sg1
        w'  = w `div` 2
        h'  = h `div` 2
-- conts are iterations of ellipses generation
genConts ∷ StdGen → StdGen → Int → [(Int,Int)]
genConts sg0 sg1 n = buildList2 (xl,yl)
  where xl = randomList (4,20) n sg0
        yl = randomList (1,4) n sg1

randomList ∷ (Random α) ⇒ (α,α) → Int → StdGen → [α]
randomList bnds n = do
  take n . randomRs bnds

buildList2 ∷ ([α],[α]) → [(α,α)]
buildList2 (_,[]) = []
buildList2 ([],_) = []
buildList2 (a:as,b:bs) = [(a,b)] ++ buildList2 (as,bs)

