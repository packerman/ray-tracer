module RayTracer.SphereSpec where

import Test.Hspec

import RayTracer.Sphere
import RayTracer.Ray (ray)
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Transformation
import RayTracer.Types

spec :: Spec
spec = do
        describe "Ray intersecting a sphere" $ do
            it "may intersect sphere at two points" $ do
                let r = ray (point 0 0 (-5)) (vector 0 0 1)
                    s = sphere
                t <$> intersect s r `shouldBe` [4, 6]
            it "may intersect sphere at tangent" $ do
                let r = ray (point 0 1 (-5)) (vector 0 0 1)
                    s = sphere
                t <$> intersect s r `shouldBe` [5, 5]
            it "may miss a sphere" $ do
                let r = ray (point 0 2 (-5)) (vector 0 0 1)
                    s = sphere
                intersect s r `shouldBe` []
            it "may originate inside a sphere" $ do
                let r = ray (point 0 0 0) (vector 0 0 1)
                    s = sphere
                t <$> intersect s r `shouldBe` [-1, 1]
            it "sphere may be behind a ray" $ do
                let r = ray (point 0 0 5) (vector 0 0 1)
                    s = sphere
                t <$> intersect s r `shouldBe` [-6, -4]
            it "sets the object of the intersection" $ do
                let r = ray (point 0 0 (-5)) (vector 0 0 1)
                    s = sphere
                object <$> intersect s r `shouldBe` [s, s]
        describe "Sphere transformation" $ do
            it "has default value" $ do
                let s = sphere
                transformation s `shouldBe` identity
            it "can be changed" $ do
                let s = sphere
                    t = translation 2 3 4
                (transformation $ transform s t) `shouldBe` t
        describe "Intersecting a transformed sphere with a ray" $ do
            it "intersects a scaled sphere" $ do
                let r = ray (point 0 0 (-5)) (vector 0 0 1)
                    s = sphere
                t <$> intersect (transform sphere (scaling 2 2 2)) r `shouldBe` [3, 7]
            it "intersects a transformed sphere" $ do
                let r = ray (point 0 0 (-5)) (vector 0 0 1)
                    s = sphere
                intersect (transform sphere (translation 5 0 0)) r `shouldBe` []
