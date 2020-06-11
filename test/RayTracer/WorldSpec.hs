module RayTracer.WorldSpec where

import Test.Hspec

import RayTracer.World
import RayTracer.Light
import RayTracer.Sphere
import RayTracer.Material
import RayTracer.Transformation
import RayTracer.Ray
import RayTracer.Tuple as Tuple
import RayTracer.Types as Types
import RayTracer.Class

spec :: Spec
spec = do
    describe "World" $ do
        it "has default world" $ do
            let l = pointLight (point (-10) 10 (-10)) (Tuple.color 1 1 1)
                s1 = sphere { material = defaultMaterial { Types.color = Tuple.color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2 } }
                s2 = sphere { transformation = scaling 0.5 0.5 0.5 }
                w = defaultWorld
            light w `shouldBe` Just l
            objects w `shouldSatisfy` (s1 `elem`)
            objects w `shouldSatisfy` (s2 `elem`)
        it "intersects with ray" $ do
            let w = defaultWorld
                r = ray (point 0 0 (-5)) (vector 0 0 1)
            time <$> (intersect w r) `shouldBe` [4, 4.5, 5.5, 6]
