module RayTracer.Intersection where

import Data.List
import Data.Function

import RayTracer.Sphere
import RayTracer.Types

intersection = Intersection

intersections :: [Intersection] -> [Intersection]
intersections = sortBy (on compare time)

hit :: [Intersection] -> Maybe Intersection
hit = find ((>= 0) . time)
