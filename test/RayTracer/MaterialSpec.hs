module RayTracer.MaterialSpec where

import Test.Hspec

import RayTracer.Types as Types
import RayTracer.Material
import RayTracer.Tuple as Tuple
import RayTracer.Light

import TestUtil

spec :: Spec
spec = do
    describe "Material" $ do
        it "has defaults" $ do
           let m = defaultMaterial
           Types.color m `shouldBe` Tuple.color 1 1 1
           ambient m `shouldBe` 0.1
           diffuse m `shouldBe` 0.9
           specular m `shouldBe`0.9
           shininess m `shouldBe` 200
    describe "Lighting function" $ do
        let m = defaultMaterial
            position = point 0 0 0
        it "lights with the eye between the light and the surface" $ do
            let eye = vector 0 0 (-1)
                normal = vector 0 0 (-1)
                light = pointLight (point 0 0 (-10)) (Tuple.color 1 1 1)
            lighting m light position eye normal `shouldBe` Tuple.color 1.9 1.9 1.9
        it "lights with the eye between light and surface, eye offset 45 degrees" $ do
            let eye = vector 0 (sqrt 2 / 2) (- sqrt 2 / 2)
                normal = vector 0 0 (-1)
                light = pointLight (point 0 0 (-10)) (Tuple.color 1 1 1)
            lighting m light position eye normal `shouldBe` Tuple.color 1 1 1
        it "lights with the eye opposite surface, light offset 45 degrees" $ do
            let eye = vector 0 0 (-1)
                normal = vector 0 0 (-1)
                light = pointLight (point 0 10 (-10)) (Tuple.color 1 1 1)
            lighting m light position eye normal `shouldSatisfy` nearBy 1e-10 (Tuple.color 0.7364 0.7364 0.7364)
        it "lights with eye in the path of the reflection vector" $ do
            let eye = vector 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
                normal = vector 0 0 (-1)
                light = pointLight (point 0 10 (-10)) (Tuple.color 1 1 1)
            lighting m light position eye normal `shouldSatisfy` nearBy 1e-10 (Tuple.color 1.6364 1.6364 1.6364)
        it "lights with the light behind the surface" $ do
            let eye = vector 0 0 (-1)
                normal = vector 0 0 (-1)
                light = pointLight (point 0 0 10) (Tuple.color 1 1 1)
            lighting m light position eye normal `shouldBe` Tuple.color 0.1 0.1 0.1
