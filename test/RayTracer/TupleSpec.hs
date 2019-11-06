module RayTracer.TupleSpec where

import Test.Hspec

import RayTracer.Tuple

spec :: Spec
spec = do
        describe "Tuple basics" $ do
            it "has points and vectors" $ do
                let a = tuple 4.3 (-4.2) 3.1 1.0
                a ^. _x `shouldBe` 4.3
                a ^. _y `shouldBe` (-4.2)
                a ^. _z `shouldBe` 3.1
                a ^. _w `shouldBe` 1.0
                a `shouldSatisfy` isPoint
                a `shouldNotSatisfy` isVector
            it "creates points and vectors" $ do
                point 4 (-4) 3 `shouldBe` tuple 4 (-4) 3 1
                vector 4 (-4) 3 `shouldBe` tuple 4 (-4) 3 0
        describe "Tuple operations" $ do
            it "add tuples" $ do
                let a1 = tuple 3 (-2) 5 1
                let a2 = tuple (-2) 3 1 0
                a1 ^+^ a2 `shouldBe` tuple 1 1 6 1
            it "subtracts points" $ do
                let p1 = point 3 2 1
                let p2 = point 5 6 7
                p1 ^-^ p2 `shouldBe` vector (-2) (-4) (-6)
            it "subtracts vector from a point" $ do
                let p = point 3 2 1
                let v = vector 5 6 7
                p ^-^ v `shouldBe` point (-2) (-4) (-6)
            it "subtracts two vectors" $ do
                let v1 = vector 3 2 1
                let v2 = vector 5 6 7
                v1 ^-^ v2 `shouldBe` vector (-2) (-4) (-6)
            it "subtracts vector from zero" $ do
                let v = vector 1 (-2) 3
                zero ^-^ v `shouldBe` vector (-1) 2 (-3)
            it "negates a tuple" $ do
                let a = tuple 1 (-2) 3 (-4)
                negated a `shouldBe` tuple (-1) 2 (-3) 4
            it "multiplies a tuple by a scalar" $ do
                let a = tuple 1 (-2) 3 (-4)
                a ^* 3.5 `shouldBe` tuple 3.5 (-7) 10.5 (-14)
                a ^* 0.5 `shouldBe` tuple 0.5 (-1) 1.5 (-2)
            it "divides tuple by a scalar" $ do
                let a = tuple 1 (-2) 3 (-4)
                a ^/ 2 `shouldBe` tuple 0.5 (-1) 1.5 (-2)

