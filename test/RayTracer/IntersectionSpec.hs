module RayTracer.IntersectionSpec where

import Test.Hspec

import RayTracer.Sphere
import RayTracer.Intersection as I
import RayTracer.Tuple as TP
import RayTracer.Types as TY
import RayTracer.Ray

spec :: Spec
spec = do
    describe "Function hit" $ do
        it "finds hit when all intersections have positive t" $ do
            let s = sphere
                i1 = intersection 1 s
                i2 = intersection 2 s
                xs = intersections [i2, i1]
            hit xs `shouldBe` Just i1
        it "finds hit when some intersections have negative t" $ do
            let s = sphere
                i1 = intersection (-1) s
                i2 = intersection 1 s
                xs = intersections [i2, i1]
            hit xs `shouldBe` Just i2
        it "doesn't find a hit when all intersections have negative t" $ do
            let s = sphere
                i1 = intersection (-2) s
                i2 = intersection (-1) s
                xs = intersections [i2, i1]
            hit xs `shouldBe` Nothing
        it "finds a hit which is always the lowest nonnegative intersection" $ do
            let s = sphere
                i1 = intersection 5 s
                i2 = intersection 7 s
                i3 = intersection (-3) s
                i4 = intersection 2 s
                xs = intersections [i1, i2, i3, i4]
            hit xs `shouldBe` Just i4
    describe "Function prepareComputations" $ do
        it "precomputes the state of an intersection" $ do
            let r = ray (TP.point 0 0 (-5)) (vector 0 0 1)
                shape = sphere
                i = intersection 4 shape
                comps = prepareComputations i r
            I.time comps `shouldBe` TY.time i
            I.object comps `shouldBe` TY.object i
            I.point comps `shouldBe` TP.point 0 0 (-1)
            eyeVector comps `shouldBe` vector 0 0 (-1)
            normalVector comps `shouldBe` vector 0 0 (-1)
