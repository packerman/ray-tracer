module RayTracer.Intersection where

import Data.List
import Data.Function

import RayTracer.Types

intersection = Intersection

instance Ord Intersection where
    compare = on compare time

intersections :: [Intersection] -> [Intersection]
intersections = sort

hit :: [Intersection] -> Maybe Intersection
hit = find ((>= 0) . time)
