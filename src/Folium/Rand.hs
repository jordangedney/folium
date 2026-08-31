-- | Tiny deterministic PRNG (xorshift64*) so that a given description
-- always yields the same individual leaf, without external dependencies.
module Folium.Rand
  ( Rng
  , mkRng
  , hashStr
  , nextD
  , rangeD
  , doubles
  , splitRng
  ) where

import Data.Bits (shiftR, shiftL, xor)
import Data.Char (ord)
import Data.List (foldl')
import Data.Word (Word64)

newtype Rng = Rng Word64

mkRng :: Word64 -> Rng
mkRng 0 = Rng 0x9E3779B97F4A7C15
mkRng w = Rng w

-- | FNV-1a hash of a string, for seeding from the description text.
hashStr :: String -> Word64
hashStr = foldl' step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3

step64 :: Word64 -> Word64
step64 x0 =
  let x1 = x0 `xor` (x0 `shiftR` 12)
      x2 = x1 `xor` (x1 `shiftL` 25)
      x3 = x2 `xor` (x2 `shiftR` 27)
  in x3

-- | Uniform Double in [0,1).
nextD :: Rng -> (Double, Rng)
nextD (Rng s) =
  let s' = step64 s
      v  = s' * 0x2545F4914F6CDD1D
      d  = fromIntegral (v `shiftR` 11) / 9007199254740992.0
  in (d, Rng s')

-- | Uniform Double in [lo,hi).
rangeD :: Double -> Double -> Rng -> (Double, Rng)
rangeD lo hi g = let (d, g') = nextD g in (lo + d * (hi - lo), g')

-- | Infinite stream of uniform [0,1) doubles.
doubles :: Rng -> [Double]
doubles g = let (d, g') = nextD g in d : doubles g'

-- | Derive an independent generator (for a named sub-purpose).
splitRng :: Word64 -> Rng -> Rng
splitRng salt (Rng s) = mkRng (step64 (s `xor` (salt * 0x9E3779B97F4A7C15)))
