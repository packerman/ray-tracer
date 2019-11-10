module RayTracer.TransformationSpec where

import Test.Hspec

import RayTracer.Transformation
import RayTracer.Matrix
import RayTracer.Tuple

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
