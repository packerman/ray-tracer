module RayTracer.CanvasSpec where

import Test.Hspec

import RayTracer.Canvas
import RayTracer.Tuple

spec :: Spec
spec = do
    describe "Canvas" $ do
        it "creates canvas" $ do
            let red = color 1 0 0
                c = createCanvas 10 20 [(2, 3, red)]
            pixelAt c 3 2 `shouldBe` PixelRGB8 0 0 0
            pixelAt c 2 3 `shouldBe` PixelRGB8 255 0 0
        it "generates canvas" $ do
            let cl = color 1 0.8 0.6
                cs = generateCanvas 10 20 (\_ -> const cl)
            pixelAt cs 2 3 `shouldBe` PixelRGB8 255 204 153
