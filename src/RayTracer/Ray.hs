module RayTracer.Ray
(   ray
    , position
    , Ray(origin, direction)
    , transform
)
where

import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Types

ray :: Point -> Vector -> Ray
ray = Ray

position :: Ray -> Double -> Point
position (Ray origin direction) t = origin ^+^ direction ^* t

transform :: Ray -> Matrix -> Ray
transform (Ray origin direction) matrix = Ray (matrix !* origin) (matrix !* direction)
