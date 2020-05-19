module RayTracer.Intersection where

import Data.List
import Data.Function

import RayTracer.Sphere

intersection = Intersection

intersections :: [Intersection] -> [Intersection]
intersections = sortBy (on compare t)

hit :: [Intersection] -> Maybe Intersection
hit = find ((>= 0) . t)
