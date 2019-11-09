module RayTracer.Tuple
(   module Linear.Metric
    , module Linear.V4
    , module Linear.V3
    , module Linear.Vector
    , module Linear.Epsilon
    , module Control.Lens.Getter
    , module RayTracer.Tuple)
 where

import Linear.Metric
import Linear.V4 hiding (vector, point)
import Linear.V3 hiding (cross)
import qualified Linear.V4 as V4
import qualified Linear.V3 as V3
import Linear.Vector
import Linear.Epsilon

import Control.Lens.Getter

type Tuple = V4 Double

tuple :: Double -> Double -> Double -> Double -> Tuple
tuple = V4

isPoint :: Tuple -> Bool
isPoint (V4 _ _ _ w) = w == 1 

isVector :: Tuple -> Bool
isVector (V4 _ _ _ w) = w == 0

type Point = Tuple

point :: Double -> Double -> Double -> Point
point x y z = V4 x y z 1

type Vector = Tuple

vector :: Double -> Double -> Double -> Vector
vector x y z = V4 x y z 0 

cross :: Vector -> Vector -> Vector
cross a b = V4.vector $ (a ^. _xyz) `V3.cross` (b ^. _xyz)

type Color = V3 Double

color :: Double -> Double -> Double -> Color
color = V3

hadamard :: (Additive f, Num a) => f a -> f a -> f a
hadamard = liftU2 (*)
