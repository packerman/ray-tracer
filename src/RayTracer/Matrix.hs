module RayTracer.Matrix
(   module Linear.Matrix
    , module Linear.V4
    , module Control.Lens.Getter
    , module RayTracer.Matrix
) where

import Linear.Matrix hiding (translation)
import Linear.V4 hiding (point, vector)
import qualified Linear.V4 as V4 (vector)
import Linear.Epsilon
import Control.Lens.Getter

import RayTracer.Tuple

type Matrix = M44 Double

matrix :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Matrix
matrix m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 =
    V4 (V4 m00 m01 m02 m03)
        (V4 m10 m11 m12 m13)
        (V4 m20 m21 m22 m23)
        (V4 m30 m31 m32 m33)

determinant :: Matrix -> Double
determinant = det44

isInvertible :: Matrix -> Bool
isInvertible = not . nearZero . det44

inverse :: Matrix -> Matrix
inverse = inv44

submatrix33 :: Matrix -> M33 Double
submatrix33 m = (m ^. _xyz) ^.column _xyz

normalTransform :: Matrix -> Vector -> Vector
normalTransform matrix normal = V4.vector $ (transpose $ inv33 $ submatrix33 matrix) !* (normal ^. _xyz)
