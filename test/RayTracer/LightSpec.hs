module RayTracer.LightSpec where

import Test.Hspec

import RayTracer.Light
import RayTracer.Tuple as Tuple
import RayTracer.Types

spec :: Spec
spec = do
    describe "Light" $ do
        it "has a position and intensity" $ do
            let i = Tuple.color 1 1 1
                p = point 0 0 0
                light = pointLight p i
            position light `shouldBe` p
            intensity light `shouldBe` i
