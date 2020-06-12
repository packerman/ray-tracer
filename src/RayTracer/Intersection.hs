{-# LANGUAGE DisambiguateRecordFields #-}

module RayTracer.Intersection where

import Data.List
import Data.Function

import RayTracer.Tuple (Point, Vector, dot)
import RayTracer.Ray
import RayTracer.Sphere
import RayTracer.Types (Intersection(..), Sphere, Ray)
import qualified RayTracer.Types as TY

intersection = Intersection

instance Ord Intersection where
    compare = on compare TY.time

intersections :: [Intersection] -> [Intersection]
intersections = sort

hit :: [Intersection] -> Maybe Intersection
hit = find ((>= 0) . TY.time)

data Precomputed = Precomputed
    {   time :: Double
    ,   object :: Sphere
    ,   point :: Point
    ,   inside :: Bool
    ,   eyeVector :: Vector
    ,   normalVector :: Vector
    }

prepareComputations :: Intersection -> Ray -> Precomputed
prepareComputations intersection ray =
    let t = TY.time intersection
        p = position ray t
        o = TY.object intersection
        eye = - (direction ray)
        normal = normalAt o p
        inside = normal `dot` eye < 0
    in Precomputed
        {   time = t
        ,   object = o
        ,   point = p
        ,   inside = inside
        ,   eyeVector = eye
        ,   normalVector = if inside then (- normal) else normal
        }
