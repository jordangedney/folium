-- | Word-wrapping using real font metrics.
module Folium.Wrap
  ( textWidth
  , wrapText
  ) where

import Graphics.Text.TrueType (Font, PointSize(..), stringBoundingBox, _xMax, _xMin)

dpi :: Int
dpi = 96

-- | Width in pixels of a string at the given pixel height.
textWidth :: Font -> Float -> String -> Float
textWidth _ _ "" = 0
textWidth f px s =
  let bb = stringBoundingBox f (fromIntegral dpi) (PointSize (px * 0.75)) s
  in _xMax bb - _xMin bb

-- | Greedy wrap of prose to a maximum pixel width.  Blank lines in the
-- input separate paragraphs.
wrapText :: Font -> Float -> Float -> String -> [String]
wrapText f px maxW txt =
  concatMap para (paragraphs txt)
  where
    para "" = [""]
    para p  = go (words p)
    go [] = []
    go (w : ws) = line w ws
    line acc [] = [acc]
    line acc (w : ws)
      | textWidth f px (acc ++ " " ++ w) <= maxW = line (acc ++ " " ++ w) ws
      | otherwise = acc : line w ws

paragraphs :: String -> [String]
paragraphs = foldr step [""] . lines
  where
    step l (p : ps)
      | null (dropWhile (== ' ') l) = "" : p : ps
      | null p    = l : ps
      | otherwise = (l ++ " " ++ p) : ps
    step _ [] = [""]
