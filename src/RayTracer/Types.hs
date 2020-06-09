module RayTracer.Types where

import RayTracer.Tuple
import RayTracer.Matrix

data Intersection = Intersection
    {   time :: Double
    ,   object :: Sphere
    }
    deriving (Eq, Show)

data Material = Material
    {   color :: Color
    ,   ambient :: Double
    ,   diffuse :: Double
    ,   specular :: Double
    ,   shininess :: Double
    }
    deriving (Eq, Show)

data Sphere = Sphere
    {   transformation :: Matrix
    ,   material :: Material
    }
    deriving (Eq, Show)

data Light = Light
    {   intensity :: Color
    ,   position :: Point
    }
