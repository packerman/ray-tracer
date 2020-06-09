import Data.Functor
import Data.Maybe

import RayTracer.Tuple as Tuple
import RayTracer.Canvas
import RayTracer.Ray as Ray
import RayTracer.Sphere
import RayTracer.Intersection
import RayTracer.Material
import RayTracer.Light
import RayTracer.Types as Types

rayOrigin = point 0 0 (-5)

wallZ = 10

wallSize = 7 :: Double

canvasPixels = 100  :: Int

pixelSize = wallSize / fromIntegral canvasPixels

half = wallSize / 2

black = Tuple.color 0 0 0

shape = sphere { material = defaultMaterial { Types.color = Tuple.color 1 0.2 1 } }

light = pointLight (point (-10) 10 (-10)) (Tuple.color 1 1 1)

canvas = createCanvas canvasPixels canvasPixels
            ([0..canvasPixels-1] >>= (\y ->
                let worldY = half - pixelSize * fromIntegral y
                in [0..canvasPixels-1] <&> (\x ->
                    let worldX = - half + pixelSize * fromIntegral x
                        position = point worldX worldY wallZ
                        r = ray rayOrigin $ normalize $ position ^-^ rayOrigin
                        xs = intersect shape r
                    in (x, y, maybe black (\hit -> let point = Ray.position r (time hit)
                                                       normal = normalAt (object hit) point
                                                       eye = - (direction r)
                                                    in lighting (material $ object $ hit) light point eye normal)
                                    (hit xs)))))

main = saveCanvas "sphere.png" canvas
