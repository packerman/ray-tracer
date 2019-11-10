module RayTracer.Transformation where

import RayTracer.Matrix

translation :: Double -> Double -> Double -> Matrix
translation x y z = matrix 1 0 0 x
                            0 1 0 y
                            0 0 1 z
                            0 0 0 1

scaling :: Double -> Double -> Double -> Matrix
scaling x y z = matrix x 0 0 0
                        0 y 0 0
                        0 0 z 0
                        0 0 0 1
