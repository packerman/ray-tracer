module RayTracer.Class where

import RayTracer.Types

class Intersectable a where
    intersect :: a -> Ray -> [Intersection]
