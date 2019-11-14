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

rotationX :: Double -> Matrix
rotationX r = matrix 1 0 0 0
                    0 (cos r) (- sin r) 0
                    0 (sin r) (cos r) 0
                    0 0 0 1

rotationY :: Double -> Matrix
rotationY r = matrix (cos r) 0 (sin r) 0
                    0 1 0 0
                    (-sin r) 0 (cos r) 0
                    0 0 0 1

rotationZ :: Double -> Matrix
rotationZ r = matrix (cos r) (- sin r) 0 0
                    (sin r) (cos r) 0 0
                    0 0 1 0
                    0 0 0 1

shearing :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
shearing xy xz yx yz zx zy = matrix 1 xy xz 0
                                    yx 1 yz 0
                                    zx zy 1 0
                                    0 0 0 1
