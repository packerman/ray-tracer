module RayTracer.Types where

import RayTracer.Matrix

data Intersection = Intersection
    {   t :: Double
    ,   object :: Sphere
    }
    deriving (Eq, Show)

data Sphere = Sphere
    {   transformation :: Matrix
    }
    deriving (Eq, Show)
