module RayTracer.Sphere where

import RayTracer.Ray as Ray
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Types
import RayTracer.Material

sphere = Sphere identity defaultMaterial

transform :: Sphere -> Matrix -> Sphere
transform (Sphere t m) matrix = Sphere (matrix !*! t) m

intersect :: Sphere -> Ray -> [Intersection]
intersect sphere ray = let  ray2 = Ray.transform ray $ inverse $ transformation sphere
                            sphereToRay = origin ray2 ^-^ point 0 0 0
                            a = direction ray2 `dot` direction ray2
                            b = 2 * (direction ray2) `dot` sphereToRay
                            c = sphereToRay `dot` sphereToRay - 1
                            discriminant = b * b - 4 * a * c
                        in if discriminant < 0 then []
                            else let t1 = (-b - sqrt discriminant) / (2*a)
                                     t2 = (-b + sqrt discriminant) / (2*a)
                                    in  [Intersection t1 sphere, Intersection t2 sphere]

normalAt :: Sphere -> Point -> Vector
normalAt sphere worldPoint = let objectPoint = inverse (transformation sphere) !* worldPoint
                                 objectNormal = objectPoint - point 0 0 0
                                 worldNormal = normalTransform (transformation sphere) objectNormal
                             in normalize worldNormal
