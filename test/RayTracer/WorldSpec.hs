module RayTracer.WorldSpec where

import Test.Hspec

import RayTracer.World
import RayTracer.Light
import RayTracer.Sphere
import RayTracer.Material
import RayTracer.Transformation
import RayTracer.Ray
import RayTracer.Tuple as TU
import RayTracer.Intersection (intersection, prepareComputations)
import RayTracer.Types as TY
import RayTracer.Class

import TestUtil

spec :: Spec
spec = do
    describe "World" $ do
        it "has default world" $ do
            let l = pointLight (point (-10) 10 (-10)) (TU.color 1 1 1)
                s1 = sphere { material = defaultMaterial { TY.color = TU.color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2 } }
                s2 = sphere { transformation = scaling 0.5 0.5 0.5 }
                w = defaultWorld
            light w `shouldBe` Just l
            objects w `shouldSatisfy` (s1 `elem`)
            objects w `shouldSatisfy` (s2 `elem`)
        it "intersects with ray" $ do
            let w = defaultWorld
                r = ray (point 0 0 (-5)) (vector 0 0 1)
            time <$> (intersect w r) `shouldBe` [4, 4.5, 5.5, 6]
    describe "shadeHit function" $ do
        it "shades intersection" $ do
            let w = defaultWorld
                r = ray (point 0 0 (-5)) (vector 0 0 1)
                shape = head $ objects w
                i = intersection 4 shape
                comps = prepareComputations i r
            shadeHit w comps `shouldSatisfy` nearBy 1e-4 (TU.color 0.38066 0.47583 0.2855)
        it "shedes intersection from the inside" $ do
            let w = defaultWorld { light = Just $ pointLight (point 0 0.25 0) (TU.color 1 1 1)}
                r = ray (point 0 0 0) (vector 0 0 1)
                shape = objects w !! 1
                i = intersection 0.5 shape
                comps = prepareComputations i r
            shadeHit w comps `shouldSatisfy` nearBy 1e-4 (TU.color 0.90498 0.90498 0.90498)
