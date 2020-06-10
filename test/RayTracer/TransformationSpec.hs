module RayTracer.TransformationSpec where

import Test.Hspec

import RayTracer.Transformation
import RayTracer.Matrix
import RayTracer.Tuple

import TestUtil

spec :: Spec
spec = do
    describe "Translation" $ do
        it "translates point" $ do
            let transform = translation 5 (-3) 2
                p = point (-3) 4 5
            transform !* p `shouldBe` point 2 1 7
        it "inverses translation" $ do
            let transform = translation 5 (-3) 2
                p = point (-3) 4 5
            inverse transform !* p `shouldBe` point (-8) 7 3
        it "does not affect vectors" $ do
            let transform = translation 5 (-3) 2
                v = vector (-3) 4 5
            transform !* v `shouldBe` v
    describe "Scaling" $ do
        it "scales point" $ do
            let transform = scaling 2 3 4
                p = point (-4) 6 8
            transform !* p `shouldBe` point (-8) 18 32
        it "scales vector" $ do
            let transform = scaling 2 3 4
                p = vector (-4) 6 8
            transform !* p `shouldBe` vector (-8) 18 32
        it "inverses scaling" $ do
            let transform = scaling 2 3 4
                v = vector (-4) 6 8
            inverse transform !* v `shouldBe` vector (-2) 2 2
        it "reflects point" $ do
            let transform = scaling (-1) 1 1
                p = point 2 3 4
            transform !* p `shouldBe` point (-2) 3 4
    describe "Rotation" $ do
        it "rotates point around x axis" $ do
            let p = point 0 1 0
                halfQuarter = rotationX $ pi / 4
                fullQuarter = rotationX $ pi / 2
            halfQuarter !* p `shouldSatisfy` near (point 0 (sqrt 2 / 2) (sqrt 2 / 2))
            fullQuarter !* p `shouldSatisfy` near (point 0 0 1)
        it "inverts rotation around x axis" $ do
            let p = point 0 1 0
                halfQuarter = rotationX $ pi / 4
            inverse halfQuarter !* p `shouldSatisfy` near (point 0 (sqrt 2 / 2) (- sqrt 2 / 2))
        it "rotates point around y axis" $ do
            let p = point 0 0 1
                halfQuarter = rotationY $ pi / 4
                fullQuarter = rotationY $ pi / 2
            halfQuarter !* p `shouldSatisfy` near (point (sqrt 2 / 2) 0 (sqrt 2 / 2))
            fullQuarter !* p `shouldSatisfy` near (point 1 0 0)
        it "rotates point around y axis" $ do
            let p = point 0 1 0
                halfQuarter = rotationZ $ pi / 4
                fullQuarter = rotationZ $ pi / 2
            halfQuarter !* p `shouldSatisfy` near (point (- sqrt 2 / 2) (sqrt 2 / 2) 0)
            fullQuarter !* p `shouldSatisfy` near (point (-1) 0 0)
    describe "Shearing" $ do
        it "moves x in proportion to y" $ do
            let transform = shearing 1 0 0 0 0 0
                p = point 2 3 4
            transform !* p `shouldBe` point 5 3 4
        it "moves x in proportion to z" $ do
            let transform = shearing 0 1 0 0 0 0
                p = point 2 3 4
            transform !* p `shouldBe` point 6 3 4
        it "moves y in proportion to x" $ do
            let transform = shearing 0 0 1 0 0 0
                p = point 2 3 4
            transform !* p `shouldBe` point 2 5 4
        it "moves y in proportion to z" $ do
            let transform = shearing 0 0 0 1 0 0
                p = point 2 3 4
            transform !* p `shouldBe` point 2 7 4
        it "moves z in proportion to x" $ do
            let transform = shearing 0 0 0 0 1 0
                p = point 2 3 4
            transform !* p `shouldBe` point 2 3 6
        it "moves z in proportion to y" $ do
            let transform = shearing 0 0 0 0 0 1
                p = point 2 3 4
            transform !* p `shouldBe` point 2 3 7
    describe "Chaining transformations" $ do
        it "applies individual transformations in sequence" $ do
            let p = point 1 0 1
                a = rotationX $ pi / 2
                b = scaling 5 5 5
                c = translation 10 5 7
            let p2 = a !* p
                p3 = b !* p2
                p4 = c !* p3
            p2 `shouldSatisfy` near (point 1 (-1) 0)
            p3 `shouldSatisfy` near (point 5 (-5) 0)
            p4 `shouldSatisfy` near (point 15 0 7)
        it "applies chained transformations in reversed order" $ do
            let p = point 1 0 1
                a = rotationX $ pi / 2
                b = scaling 5 5 5
                c = translation 10 5 7
                t = c !*! b !*! a
            t !* p `shouldSatisfy` near (point 15 0 7)
