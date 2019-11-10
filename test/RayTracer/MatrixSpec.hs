module RayTracer.MatrixSpec where

import Test.Hspec

import RayTracer.Matrix
import RayTracer.Tuple

import TestUtil

spec :: Spec
spec = do
    describe "Matrix basics" $ do
        it "creates matrix" $ do
            let m = matrix 1 2 3 4
                            5.5 6.5 7.5 8.5
                            9 10 11 12
                             13.5 14.5 15.5 16.5
            m ^. _x ^. _x `shouldBe` 1
            m ^. _x ^. _w `shouldBe` 4
            m ^. _y ^. _x `shouldBe` 5.5
            m ^. _y ^. _z `shouldBe` 7.5
            m ^. _z ^. _z `shouldBe` 11
            m ^. _w ^. _x `shouldBe` 13.5
            m ^. _w ^. _z `shouldBe` 15.5
        it "multiplies matrices" $ do
            let a = matrix 1 2 3 4
                            5 6 7 8
                            9 8 7 6
                            5 4 3 2
                b = matrix (-2) 1 2 3
                            3 2 1 (-1)
                            4 3 6 5
                            1 2 7 8
            a !*! b `shouldBe` matrix 20 22 50 48
                                        44 54 114 108
                                        40 58 110 102
                                        16 26 46 42
        it "multiplies matrix by tuple" $ do
            let a = matrix 1 2 3 4
                            2 4 4 2
                            8 6 4 1
                            0 0 0 1
                b = tuple 1 2 3 1
            a !* b `shouldBe` tuple 18 24 33 1
        it "multiplies matrix by identity matrix" $ do
            let a = matrix 0 1 2 4
                            1 2 4 8
                            2 4 8 16
                            4 8 16 32
            a !*! identity `shouldBe` a
        it "transposes matrix" $ do
            let a = matrix 0 9 3 0
                            9 8 0 8
                            1 8 5 3
                            0 0 5 8
            transpose a `shouldBe` matrix 0 9 1 0
                                            9 8 8 0
                                            3 0 5 5
                                            0 8 3 8
        it "transposes identity matrix" $ do
            transpose identity `shouldBe` (identity :: Matrix)
        it "tests invertible matrix" $ do
            let a = matrix 6 4 4 4
                            5 5 7 6
                            4 (-9) 3 (-7)
                            9 1 7 (-6)
            determinant a `shouldBe` (-2120)
            a `shouldSatisfy` isInvertible
        it "tests noninvertible matrix" $ do
            let a = matrix (-4) 2 (-2) (-3)
                            9 6 2 6
                            0 (-5) 1 (-5)
                            0 0 0 0
            determinant a `shouldBe` 0
            a `shouldNotSatisfy` isInvertible
        it "inverts matrix" $ do
            let a = matrix (-5) 2 6 (-8)
                            1 (-5) 1 8
                            7 7 (-6) (-7)
                            1 (-3) 7 4
            inverse a `shouldSatisfy` near (matrix 0.21805 0.45113 0.24060 (-0.04511)
                                                    (-0.80827) (-1.45677) (-0.44361) 0.52068
                                                    (-0.07895) (-0.22368) (-0.05263) 0.19737
                                                    (-0.52256) (-0.81391) (-0.30075) 0.30639)
        it "inverts matrix 2" $ do
            let a = matrix 8 (-5) 9 2
                            7 5 6 1
                            (-6) 0 9 6
                            (-3) 0 (-9) (-4)
            inverse a `shouldSatisfy` near (matrix (-0.15385) (-0.15385) (-0.28205) (-0.53846)
                                                    (-0.07692) 0.12308 0.02564 0.03077
                                                    0.35897 0.35897 0.43590 0.92308
                                                    (-0.69231) (-0.69231) (-0.76923) (-1.92308))
        
 