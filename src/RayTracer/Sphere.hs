module RayTracer.Sphere where

import RayTracer.Ray
import RayTracer.Tuple

intersect :: Ray -> [Double]
intersect ray = let sphereToRay = origin ray ^-^ point 0 0 0
                    a = dot (direction ray) (direction ray)
                    b = 2 * dot (direction ray) sphereToRay
                    c = (dot sphereToRay sphereToRay) - 1
                    discriminant = b * b - 4 * a * c
                in if discriminant < 0 then []
                    else let t1 = (-b - sqrt discriminant) / (2*a)
                             t2 = (-b + sqrt discriminant) / (2*a)
                         in  [t1, t2]
