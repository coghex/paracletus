{-# LANGUAGE Strict #-}
{-# LANGUAGE KindSignatures #-}
module Paracletus.Vulkan.Calc
--  ( calcVertices )
where
-- translations from lua state to draw
-- state are defined, should be run
-- outside of parent thread
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0 ( Word32 )
import Numeric.DataFrame
import Paracletus.Data ( Tile(..) )
import Paracletus.Vulkan.Atlas ( indexAtlas )
import Paracletus.Vulkan.Vertex
    ( atLeastThree, Vertex(Vertex, move, texCoord, pos) )

-- determines dataframes from drawstate
calcVertices ∷ [Tile] →  (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices ts = (vertices ts, indices ts)

vertsqs ∷ [DataFrame Vertex ('[] ∷ [Nat])]
vertsqs = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1) (vec3 0 1 0.1) (vec3 0 0 0)
          , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1) (vec3 1 1 0.1) (vec3 0 0 0)
          , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1) (vec3 1 0 0.1) (vec3 0 0 0)
          , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1) (vec3 0 0 0.1) (vec3 0 0 0) ]
-- combines all Tiles into a dataframe
vertices ∷ [Tile] → DataFrame Vertex '[XN 0]
vertices ts = fromList $ combineVertices (1∷Int) ts
  where combineVertices _    [] = []
        combineVertices nDyn ((GTile (x',y') (xscale',yscale') (ax',ay') (sx,sy) t'):tts) = (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices nDyn tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
        combineVertices nDyn ((DTile _ (x',y') (xscale',yscale') (ax',ay') (sx,sy) moves t'):tts) = withMove (+ vec3 1 dyn moves') (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices (nDyn + 1) tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          dyn = fromIntegral nDyn
          moves' = if moves then 1 else 0
          withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withMove f = map (\(S v) → S v { move = f $ move v })

indices ∷ [Tile] → DataFrame Word32 '[XN 3]
indices tiles = atLeastThree $ fromList $ (combineIndices tiles)
combineIndices ∷ ∀ a. (Num a) ⇒ [Tile] → [a]
combineIndices []           = []
combineIndices (_:tiles) = oneRectIndices ⧺ (map (+4) (combineIndices tiles))
  where oneRectIndices = [0,3,2,2,1,0]

