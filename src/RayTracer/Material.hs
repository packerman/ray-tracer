module RayTracer.Material where

import RayTracer.Tuple as Tuple
import RayTracer.Types

defaultMaterial :: Material
defaultMaterial = Material (Tuple.color 1 1 1) 0.1 0.9 0.9 200
