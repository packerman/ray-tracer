module RayTracer.Sphere where

import RayTracer.Ray
import RayTracer.Tuple
import RayTracer.Matrix

data Intersection = Intersection
    {   t :: Double
    ,   object :: Sphere
    }
    deriving (Eq, Show)

data Sphere = Sphere
    {   transformation :: Matrix
    }
    deriving (Eq, Show)

sphere = Sphere identity

transform :: Sphere -> Matrix -> Sphere
transform (Sphere t) m = Sphere (m !*! t)

intersect :: Sphere -> Ray -> [Intersection]
intersect sphere ray = let sphereToRay = origin ray ^-^ point 0 0 0
                           a = direction ray `dot` direction ray
                           b = 2 * (direction ray) `dot` sphereToRay
                           c = sphereToRay `dot` sphereToRay - 1
                           discriminant = b * b - 4 * a * c
                        in if discriminant < 0 then []
                            else let t1 = (-b - sqrt discriminant) / (2*a)
                                     t2 = (-b + sqrt discriminant) / (2*a)
                                    in  [Intersection t1 sphere, Intersection t2 sphere]
