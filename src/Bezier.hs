-- From https://github.com/hrldcpr/Bezier.hs/blob/master/Bezier.hs
module Bezier where

import           Control.Monad (zipWithM)
import           Linear.Metric (distance)
import           Linear.V2

type Point = [Float]  -- a multi-dimensional coordinate
type Parametric n a = n -> a  -- a value that varies over time

-- linear interpolation between two numbers, from t=0 to t=1
line1d :: Num n => n -> n -> Parametric n n
line1d a b t = (1 - t) * a + t * b

-- line between two points is linear interpolation on each dimension
line :: Num n => [n] -> [n] -> Parametric n [n]
line p q = zipWithM line1d p q

-- bezier of one point is fixed at that point, and bezier of n points is the
-- line between bezier of first n-1 points and bezier of last n-1 points
bezier' :: Num n => [[n]] -> Parametric n [n]
bezier' [p] = return p
bezier' ps  = do p <- bezier' (init ps)
                 q <- bezier' (tail ps)
                 line p q

bezier :: [V2 Float] -> Float -> V2 Float
bezier = (fromComponents .) . bezier' . fmap getComponents

getComponents :: V2 Float -> [Float]
getComponents (V2 x y) = [x, y]

fromComponents :: [Float] -> V2 Float
fromComponents [x, y] = V2 x y
fromComponents _      = undefined

bezierLength :: [V2 Float] -> Float
bezierLength v3s = sum $ fmap (uncurry distance) $ zip samples (drop 1 samples)
  where
    curve = bezier v3s
    samples = fmap (curve $) . take 101 $ iterate (+0.01) 0
