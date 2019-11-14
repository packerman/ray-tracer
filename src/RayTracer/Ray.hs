module RayTracer.Ray 
(   ray
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
