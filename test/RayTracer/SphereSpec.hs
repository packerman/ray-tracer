module RayTracer.SphereSpec where

import Test.Hspec

import RayTracer.Sphere
import RayTracer.Ray hiding (transform)
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Transformation

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
