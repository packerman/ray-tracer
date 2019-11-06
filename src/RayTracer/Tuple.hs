module RayTracer.Tuple
(
    module Linear.V4,
    module Linear.Vector,
    module Control.Lens.Getter, 
    module RayTracer.Tuple)
 where

import Linear.V4 (V4(..), _x, _y, _z, _w)
import Linear.Vector

import Control.Lens.Getter

type Tuple = V4 Double

tuple :: Double -> Double -> Double -> Double -> Tuple
tuple = V4

isPoint :: V4 Double -> Bool
isPoint (V4 _ _ _ w) = w == 1 

isVector :: V4 Double -> Bool
isVector (V4 _ _ _ w) = w == 0

type Point = V4 Double

point :: Double -> Double -> Double -> Point
point x y z = V4 x y z 1

type Vector = V4 Double

vector :: Double -> Double -> Double -> Vector
vector x y z = V4 x y z 0 
