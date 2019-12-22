module RayTracer.Sphere where

import RayTracer.Ray
import RayTracer.Tuple

data Sphere = Sphere

intersect :: Sphere -> Ray -> [Double]
intersect sphere ray = let sphereToRay = origin ray ^-^ point 0 0 0
                           a = direction ray `dot` direction ray
                           b = 2 * (direction ray) `dot` sphereToRay
                           c = sphereToRay `dot` sphereToRay - 1
                           discriminant = b * b - 4 * a * c
                        in if discriminant < 0 then []
                            else let t1 = (-b - sqrt discriminant) / (2*a)
                                     t2 = (-b + sqrt discriminant) / (2*a)
                                    in  [t1, t2]
