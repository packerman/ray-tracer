module TestUtil where

import Linear.Metric
import Linear.Vector
import Linear.Epsilon

nearBy :: (Metric f, Num a, Ord a) => a -> f a -> f a -> Bool
nearBy e v1 v2 = qd v1 v2 < e
    
near :: (Additive f, Num a, Epsilon (f a)) => f a -> f a -> Bool
near a1 a2 = nearZero $ a1 ^-^ a2
