import Data.Functor
import Data.Maybe

import RayTracer.Tuple
import RayTracer.Canvas
import RayTracer.Ray
import RayTracer.Sphere
import RayTracer.Intersection
import RayTracer.Class

rayOrigin = point 0 0 (-5)

wallZ = 10

wallSize = 7 :: Double

canvasPixels = 100  :: Int

pixelSize = wallSize / fromIntegral canvasPixels

half = wallSize / 2

red = color 1 0 0
black = color 0 0 0

shape = sphere

canvas = createCanvas canvasPixels canvasPixels
            ([0..canvasPixels-1] >>= (\y ->
                let worldY = half - pixelSize * fromIntegral y
                in [0..canvasPixels-1] <&> (\x ->
                    let worldX = - half + pixelSize * fromIntegral x
                        position = point worldX worldY wallZ
                        r = ray rayOrigin $ normalize $ position ^-^ rayOrigin
                        xs = intersect sphere r
                    in (x, y, if (isJust $ hit xs) then red else black))))

main = saveCanvas "silhouette.png" canvas
