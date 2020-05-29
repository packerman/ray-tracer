module RayTracer.Ray
(   ray
    , position
    , Ray(origin, direction)
    , transform
)
where

import RayTracer.Tuple
import RayTracer.Matrix

data Ray = Ray
    {   origin :: Point
    ,   direction :: Vector
    }
    deriving (Eq, Show)

ray :: Point -> Vector -> Ray
ray = Ray

position :: Ray -> Double -> Point
position (Ray origin direction) t = origin ^+^ direction ^* t

transform :: Ray -> Matrix -> Ray
transform (Ray origin direction) matrix = Ray (matrix !* origin) (matrix !* direction)
