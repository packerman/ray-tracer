module RayTracer.Canvas 
(   module Codec.Picture
    , createCanvas
    , saveCanvas
)
where

import Control.Monad.ST

import Codec.Picture
import Codec.Picture.Types

import RayTracer.Tuple

type Canvas = Image PixelRGB8

createCanvas :: Int -> Int -> [(Int, Int, Color)] -> Canvas
createCanvas width height pixels = runST $ do
    img <- createMutableImage width height black
    mapM_ (\(x, y, c) -> writePixel img x y (toPixel c)) pixels
    unsafeFreezeImage img
    where black = PixelRGB8 0 0 0

toPixel :: Color -> PixelRGB8
toPixel (V3 r g b) = PixelRGB8 (f r) (f g) (f b)
    where f = round . (255*) . clamp 0 1

clamp :: (Ord a) => a -> a -> a -> a
clamp a b = min b . max a

saveCanvas :: FilePath -> Canvas -> IO ()
saveCanvas filePath = savePngImage filePath . ImageRGB8
