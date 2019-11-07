module RayTracer.Tuple
(
    module Linear.Metric,
    module Linear.V4,
    module Linear.Vector,
    module Control.Lens.Getter, 
    module RayTracer.Tuple)
 where

import Linear.Metric
import Linear.V4 (V4(..), _x, _y, _z, _w, _xyz)
import qualified Linear.V4 as V4
import qualified Linear.V3 as V3
import Linear.Vector

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
