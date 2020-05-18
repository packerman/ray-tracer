module RayTracer.IntersectionSpec where

import Test.Hspec

import RayTracer.Sphere
import RayTracer.Intersection

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
