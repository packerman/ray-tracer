module RayTracer.SphereSpec where

import Test.Hspec

import RayTracer.Sphere
import RayTracer.Ray (ray)
import RayTracer.Tuple
import RayTracer.Matrix
import RayTracer.Transformation
import RayTracer.Material
import RayTracer.Types
import RayTracer.Class

import TestUtil

spec :: Spec
spec = do
    describe "Ray intersecting a sphere" $ do
        it "may intersect sphere at two points" $ do
            let r = ray (point 0 0 (-5)) (vector 0 0 1)
                s = sphere
            time <$> intersect s r `shouldBe` [4, 6]
        it "may intersect sphere at tangent" $ do
            let r = ray (point 0 1 (-5)) (vector 0 0 1)
                s = sphere
            time <$> intersect s r `shouldBe` [5, 5]
        it "may miss a sphere" $ do
            let r = ray (point 0 2 (-5)) (vector 0 0 1)
                s = sphere
            intersect s r `shouldBe` []
        it "may originate inside a sphere" $ do
            let r = ray (point 0 0 0) (vector 0 0 1)
                s = sphere
            time <$> intersect s r `shouldBe` [-1, 1]
        it "sphere may be behind a ray" $ do
            let r = ray (point 0 0 5) (vector 0 0 1)
                s = sphere
            time <$> intersect s r `shouldBe` [-6, -4]
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
            time <$> intersect (transform sphere (scaling 2 2 2)) r `shouldBe` [3, 7]
        it "intersects a transformed sphere" $ do
            let r = ray (point 0 0 (-5)) (vector 0 0 1)
                s = sphere
            intersect (transform sphere (translation 5 0 0)) r `shouldBe` []
    describe "normal on a sphere" $ do
        it "normal on a sphere at a point on the x axis" $ do
            let s = sphere
            normalAt s (point 1 0 0) `shouldBe` vector 1 0 0
        it "normal on a sphere at a point on the y axis" $ do
            let s = sphere
            normalAt s (point 0 1 0) `shouldBe` vector 0 1 0
        it "normal on a sphere at a point on the z axis" $ do
            let s = sphere
            normalAt s (point 0 0 1) `shouldBe` vector 0 0 1
        it "normal on a sphere at a nonaxial point" $ do
            let s = sphere
            normalAt s (point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)) `shouldBe` vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
        it "normal is a normalized vector" $ do
            let s = sphere
                n = normalAt s (point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
            n `shouldBe` normalize n
    describe "normalAt on transformed sphere" $ do
        it "computes the normal on a translated sphere" $ do
            let s = transform sphere $ translation 0 1 0
            normalAt s (point 0 1.70711 (-0.70711)) `shouldSatisfy` nearBy 1e-10 (vector 0 0.70711 (-0.70711))
        it "computes the normal on a transformed sphere" $ do
            let s = transform sphere $ scaling 1 0.5 1 !*! rotationZ pi/5
            normalAt s (point 0 (sqrt 2 / 2) (- sqrt 2 / 2)) `shouldSatisfy` nearBy 1e-10 (vector 0 0.97014 (-0.24254))
    describe "sphere material" $ do
        it "has default material" $ do
            let s = sphere
            material s `shouldBe` defaultMaterial
        it "may assign a material" $ do
            let s = sphere
                m = defaultMaterial { ambient = 1 }
            material (s { material = m}) `shouldBe` m
