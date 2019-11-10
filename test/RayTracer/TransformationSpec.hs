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