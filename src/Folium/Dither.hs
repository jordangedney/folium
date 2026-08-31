-- | Grayscale conversion and ordered (Bayer) dithering, so gradients
-- and shading survive e-ink's limited gray levels without banding.
module Folium.Dither
  ( toEInk
  ) where

import Codec.Picture

bayer8 :: [[Int]]
bayer8 =
  [ [ 0, 32,  8, 40,  2, 34, 10, 42]
  , [48, 16, 56, 24, 50, 18, 58, 26]
  , [12, 44,  4, 36, 14, 46,  6, 38]
  , [60, 28, 52, 20, 62, 30, 54, 22]
  , [ 3, 35, 11, 43,  1, 33,  9, 41]
  , [51, 19, 59, 27, 49, 17, 57, 25]
  , [15, 47,  7, 39, 13, 45,  5, 37]
  , [63, 31, 55, 23, 61, 29, 53, 21]
  ]

-- | Convert to 8-bit grayscale quantized to @levels@ gray levels
-- (16 suits most e-ink panels; 2 gives pure black and white) with
-- optional ordered dithering.
toEInk :: Int -> Bool -> Image PixelRGBA8 -> Image Pixel8
toEInk levels dither img = generateImage px (imageWidth img) (imageHeight img)
  where
    lv = max 2 (min 256 levels)
    step = 255 / fromIntegral (lv - 1) :: Double
    px x y =
      let PixelRGBA8 r g b _ = pixelAt img x y
          l = 0.299 * fromIntegral r + 0.587 * fromIntegral g
                + 0.114 * fromIntegral b :: Double
          thr = if dither
                  then (fromIntegral (bayer8 !! (y `mod` 8) !! (x `mod` 8))
                          / 64 - 0.5) * step
                  else 0
          q = fromIntegral (round ((l + thr) / step) :: Int) * step
      in fromIntegral (max 0 (min 255 (round q :: Int)))
