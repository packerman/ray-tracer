module RayTracer.Intersection where

import Data.List

import RayTracer.Sphere

intersection = Intersection

intersections :: [Intersection] -> [Intersection]
intersections = sortBy (\i1 i2 -> compare (t i1) (t i2))

hit :: [Intersection] -> Maybe Intersection
hit = find (\i -> t i >= 0)
