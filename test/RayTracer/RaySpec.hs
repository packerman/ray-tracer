module RayTracer.RaySpec where

import Test.Hspec

import Data.Function

import RayTracer.Ray
import RayTracer.Tuple

spec :: Spec
spec = do
        describe "Ray" $ do
            it "creates ray" $ do
                let o = point 1 2 3
                    d = vector 4 5 6
                let r = ray o d
                (r & origin) `shouldBe` o
                (r & direction) `shouldBe` d

