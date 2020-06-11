module RayTracer.Intersection where

import Data.List
import Data.Function

import RayTracer.Types

intersection = Intersection

instance Ord Intersection where
    compare i1 i2 = compare (time i1) (time i2)

intersections :: [Intersection] -> [Intersection]
intersections = sort

hit :: [Intersection] -> Maybe Intersection
hit = find ((>= 0) . time)
