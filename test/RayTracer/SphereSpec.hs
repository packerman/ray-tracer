module RayTracer.SphereSpec where

import Test.Hspec

import RayTracer.Sphere
import RayTracer.Ray
import RayTracer.Tuple

spec :: Spec
spec = do
        describe "Ray intersecting a sphere" $ do
            it "may intersect sphere at two points" $ do
                let r = ray (point 0 0 (-5)) (vector 0 0 1)
                    s = Sphere
                t <$> intersect s r `shouldBe` [4, 6]
            it "may intersect sphere at tangent" $ do
                let r = ray (point 0 1 (-5)) (vector 0 0 1)
                    s = Sphere
                t <$> intersect s r `shouldBe` [5, 5]
            it "may miss a sphere" $ do
                let r = ray (point 0 2 (-5)) (vector 0 0 1)
                    s = Sphere
                intersect s r `shouldBe` []
            it "may originate inside a sphere" $ do
                let r = ray (point 0 0 0) (vector 0 0 1)
                    s = Sphere
                t <$> intersect s r `shouldBe` [-1, 1]
            it "sphere may be behind a ray" $ do
                let r = ray (point 0 0 5) (vector 0 0 1)
                    s = Sphere
                t <$> intersect s r `shouldBe` [-6, -4]
            it "sets the object of the intersection" $ do
                let r = ray (point 0 0 (-5)) (vector 0 0 1)
                    s = Sphere
                object <$> intersect s r `shouldBe` [s, s]
