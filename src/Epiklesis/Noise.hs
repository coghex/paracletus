module Epiklesis.Noise where
-- perlin noise is initialized and calculated

import Numeric.Noise.Perlin

makePerlin :: Int -> Int -> Double -> Double -> Perlin
makePerlin seed octaves scale persistance = perlin seed octaves scale persistance

getNoise :: Int -> Int -> Perlin -> Double
getNoise x y p = noiseValue p (xf, yf, 0.0)
  where xf = fromIntegral x
        yf = fromIntegral y
