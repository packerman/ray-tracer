module RayTracer.RaySpec where

import Test.Hspec

import Data.Function

import RayTracer.Ray
import RayTracer.Tuple
import RayTracer.Transformation

spec :: Spec
spec = do
        describe "Ray" $ do
            it "creates ray" $ do
                let o = point 1 2 3
                    d = vector 4 5 6
                let r = ray o d
                (r & origin) `shouldBe` o
                (r & direction) `shouldBe` d
        describe "Position" $ do
            it "computes point from distance" $ do
                let r = ray (point 2 3 4) (vector 1 0 0)
                position r 0 `shouldBe` point 2 3 4
                position r 1 `shouldBe` point 3 3 4
                position r (-1) `shouldBe` point 1 3 4
                position r 2.5 `shouldBe` point 4.5 3 4
        describe "Ray transformations" $ do
            it "translates a ray" $ do
                let r = ray (point 1 2 3) (vector 0 1 0)
                    m = translation 3 4 5
                transform r m `shouldBe` ray (point 4 6 8) (vector 0 1 0)
            it "scales a ray" $ do
                let r = ray (point 1 2 3) (vector 0 1 0)
                    m = scaling 2 3 4
                transform r m `shouldBe` ray (point 2 6 12) (vector 0 3 0)
