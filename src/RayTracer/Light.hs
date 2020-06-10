module RayTracer.Light where

import RayTracer.Tuple

data Light = Light
    {   intensity :: Color
    ,   position :: Point
    } deriving (Eq, Show)

pointLight = flip Light
