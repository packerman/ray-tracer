module RayTracer.MaterialSpec where

import Test.Hspec

import RayTracer.Types
import RayTracer.Material
import qualified RayTracer.Tuple as Tuple

spec :: Spec
spec = do
    describe "Material" $ do
        it "has defaults" $ do
           let m = defaultMaterial
           color m `shouldBe` Tuple.color 1 1 1
           ambient m `shouldBe` 0.1
           diffuse m `shouldBe` 0.9
           specular m `shouldBe`0.9
           shininess m `shouldBe` 200
