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
    ( Nat,
      DataFrame(S),
      XN,
      fromList,
      HomTransform4(toHomPoint, fromHom),
      Vector3(vec3),
      Vector4(vec4) )
import Epiklesis.Data ( Tile(ATile, DMTile, DTile, MTile, GTile) )
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
vertices ts = fromList $ combineVertices (1∷Int) (1∷Int) (1∷Int) ts
  where combineVertices _    _    _     [] = []
        combineVertices nAux nCam nTile ((GTile (x',y') (xscale',yscale') (ax',ay') (sx,sy) t'):tts) = (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices nAux nCam nTile tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
        combineVertices nAux nCam nTile ((MTile (x',y') (xscale',yscale') (ax',ay') (sx,sy) t'):tts) = withMove (+ vec3 0 0 1) (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices nAux nCam nTile tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withMove f = map (\(S v) → S v { move = f $ move v })
        combineVertices nAux nCam nTile ((DTile _ (x',y') (xscale',yscale') (ax',ay') (sx,sy) t'):tts) = withMove (+ vec3 1 dyn 0) (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices nAux nCam (nTile+1) tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          dyn = fromIntegral nTile
          withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withMove f = map (\(S v) → S v { move = f $ move v })
        combineVertices nAux nCam nTile ((DMTile _ (x',y') (xscale',yscale') (ax',ay') (sx,sy) t'):tts) = withMove (+ vec3 4 dyn 1) (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices nAux (nCam+1) nTile tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          dyn = fromIntegral nCam
          withPos f = map (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withMove f = map (\(S v) → S v { move = f $ move v })
        combineVertices nAux nCam nTile ((ATile _ (x',y') (xscale',yscale') (ax',ay') (sx,sy) t'):tts) = withMove (+ vec3 10 dyn 1) (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0) (withScale (* vec3 xscale yscale 1) vertsqs)))) ⧺ combineVertices (nAux+1) nCam nTile tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          dyn = fromIntegral nAux
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

