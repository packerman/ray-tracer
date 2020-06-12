{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module RayTracer.World where

import Data.Maybe

import RayTracer.Sphere
import RayTracer.Light
import RayTracer.Material
import RayTracer.Transformation
import RayTracer.Intersection as I
import RayTracer.Tuple as TU
import RayTracer.Ray
import RayTracer.Types as TY
import RayTracer.Class

data World = World
    {   light :: Maybe Light
    ,   objects :: [Sphere]
    }

defaultWorld :: World
defaultWorld = let l = pointLight (TU.point (-10) 10 (-10)) (TU.color 1 1 1)
                   m = defaultMaterial { TY.color = TU.color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2 }
                in World (Just l) [
                    sphere { material = m },
                    sphere { transformation = scaling 0.5 0.5 0.5 }
                ]

instance Intersectable World where
    intersect World{objects} ray = intersections $ (`intersect` ray) =<< objects

shadeHit :: World -> Precomputed -> Color
shadeHit world comps = maybe (TU.color 0 0 0) (\light ->
        lighting (material $ I.object $ comps) light (I.point comps) (eyeVector comps) (normalVector comps)
    ) (light world)

colorAt :: World -> Ray -> Color
colorAt world ray = let xs = intersect world ray
                        hit = I.hit xs
                    in maybe (TU.color 0 0 0) (\hit -> shadeHit world (prepareComputations hit ray)) hit
