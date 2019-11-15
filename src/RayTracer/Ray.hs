module RayTracer.Ray 
(   ray
    , position
    , Ray(origin, direction)
)
where

import RayTracer.Tuple

data Ray = Ray
    {   origin :: Point
    ,   direction :: Vector
    }

ray :: Point -> Vector -> Ray
ray = Ray

position :: Ray -> Double -> Point
position (Ray origin direction) t = origin ^+^ direction ^* t
