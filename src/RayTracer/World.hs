module RayTracer.World where

import RayTracer.Sphere
import RayTracer.Light
import RayTracer.Material
import RayTracer.Transformation
import RayTracer.Tuple as Tuple
import RayTracer.Types as Types

data World = World
    {   light :: Maybe Light
    ,   objects :: [Sphere]
    }

defaultWorld :: World
defaultWorld = let l = pointLight (point (-10) 10 (-10)) (Tuple.color 1 1 1)
                   m = defaultMaterial { Types.color = Tuple.color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2 }
                in World (Just l) [
                    sphere { material = m },
                    sphere { transformation = scaling 0.5 0.5 0.5 }
                ]
