-- | Composes the wallpaper: the leaf as a framed herbarium plate on
-- the left, the written description typeset on the right.
module Folium.Page
  ( Fonts(..)
  , renderPage
  ) where

import Codec.Picture (Image, PixelRGBA8(..))
import Data.Word (Word64)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (Font)
import Numeric (showHex)

import Folium.Leaf
import Folium.Rand
import Folium.Spec
import Folium.Wrap

data Fonts = Fonts
  { fReg  :: Font
  , fBold :: Font
  , fItal :: Font
  }

gray :: Int -> PixelRGBA8
gray v = let b = fromIntegral (max 0 (min 255 v)) in PixelRGBA8 b b b 255

grayA :: Int -> Int -> PixelRGBA8
grayA v a = let b = fromIntegral (max 0 (min 255 v))
            in PixelRGBA8 b b b (fromIntegral (max 0 (min 255 a)))

v2 :: Double -> Double -> Point
v2 x y = V2 (realToFrac x) (realToFrac y)

renderPage :: Fonts -> Int -> Int -> LeafSpec -> LeafGeom -> String -> String
           -> Word64 -> Rng -> Image PixelRGBA8
renderPage fonts wI hI sp geom desc binom seed rng =
  renderDrawing wI hI (gray 255) $ do
    drawFrame fonts wI hI sp geom binom rng
    drawColumn fonts wI hI sp desc binom seed

-- The framed plate -----------------------------------------------------------

drawFrame :: Fonts -> Int -> Int -> LeafSpec -> LeafGeom -> String -> Rng
          -> Drawing PixelRGBA8 ()
drawFrame fonts wI hI sp geom binom rng = do
  let w = fromIntegral wI :: Double
      h = fromIntegral hI :: Double
      u = h / 1404
      fh = 0.86 * h
      fw = min (0.44 * w) (0.72 * fh)
      cx = 0.26 * w
      (fx, fy) = (cx - fw / 2, (h - fh) / 2)
      rect x y rw rh col =
        withTexture (uniformTexture col) $ fill $
          rectangle (v2 x y) (realToFrac rw) (realToFrac rh)
      -- mat / artwork window geometry
      molding = 14 * u
      bevel = 5 * u
      matPad = 0.105 * fw
      artX = fx + molding + bevel + matPad
      artY = fy + molding + bevel + matPad
      artW = fw - 2 * (molding + bevel + matPad)
      artH = fh - 2 * (molding + bevel + matPad) - 0.09 * fh -- extra mat below
      labelY = artY + artH + (fy + fh - molding - bevel - (artY + artH)) / 2

  -- soft drop shadow
  rect (fx + 10 * u) (fy + 14 * u) fw fh (grayA 0 26)
  rect (fx + 5 * u) (fy + 8 * u) fw fh (grayA 0 30)
  -- molding, bevel line, mat
  rect fx fy fw fh (gray 45)
  rect (fx + molding) (fy + molding) (fw - 2 * molding) (fh - 2 * molding) (gray 150)
  rect (fx + molding + bevel) (fy + molding + bevel)
       (fw - 2 * (molding + bevel)) (fh - 2 * (molding + bevel)) (gray 250)
  -- hairline around the artwork window, then the window itself
  withTexture (uniformTexture (gray 120)) $
    stroke (realToFrac (2 * u)) JoinRound (CapRound, CapRound) $
      rectangle (v2 (artX - 6 * u) (artY - 6 * u))
                (realToFrac (artW + 12 * u)) (realToFrac (artH + 12 * u))
  rect artX artY artW artH (gray 252)

  drawLeaf (artX, artY, artW, artH) u sp geom rng

  -- plate label on the mat
  let lpx = 26 * u
      lw = textWidth (fItal fonts) (realToFrac lpx) binom
  drawText (fItal fonts) lpx (gray 70)
           (cx - realToFrac lw / 2) labelY binom

-- Leaf rendering inside the artwork window -----------------------------------

drawLeaf :: (Double, Double, Double, Double) -> Double -> LeafSpec -> LeafGeom
         -> Rng -> Drawing PixelRGBA8 ()
drawLeaf (ax, ay, aw, ah) u sp geom rng = do
  let (theta, _) = rangeD (-0.07) 0.07 rng
      rot (x, y) =
        let (cxr, cyr) = (0.0, 0.5)
            (dx, dy) = (x - cxr, y - cyr)
        in (cxr + dx * cos theta - dy * sin theta,
            cyr + dx * sin theta + dy * cos theta)
      allPts = map rot $
        lgOutline geom ++ lgPetiole geom
          ++ concatMap snd (lgVeins geom)
      xs = map fst allPts
      ys = map snd allPts
      (mnx, mxx) = (minimum xs, maximum xs)
      (mny, mxy) = (minimum ys, maximum ys)
      pad = 0.07 * min aw ah
      s = min ((aw - 2 * pad) / max 1e-6 (mxx - mnx))
              ((ah - 2 * pad) / max 1e-6 (mxy - mny))
      (bcx, bcy) = ((mnx + mxx) / 2, (mny + mxy) / 2)
      (acx, acy) = (ax + aw / 2, ay + ah / 2)
      mp p = let (x, y) = rot p
             in v2 (acx + (x - bcx) * s) (acy - (y - bcy) * s)
      sw d = realToFrac (max (0.9 * u) (d * s))

      sf = spSurfaces sp
      fillHi = if SfGlaucous `elem` sf then 232 else 218
      fillLo | SfGlaucous `elem` sf = 214
             | SfCoriaceous `elem` sf = 188
             | SfMembranous `elem` sf = 226
             | otherwise = 197
      outlineW | SfCoriaceous `elem` sf = 3.4 * u
               | otherwise = 2.2 * u
      outlinePts = map mp (lgOutline geom)
      strokeLine wd col pts =
        withTexture (uniformTexture col) $
          stroke wd JoinRound (CapRound, CapRound) $ polyline pts

  -- petiole first (runs under the blade)
  case lgPetiole geom of
    [] -> pure ()
    ps -> strokeLine (sw (lgPetioleW geom)) (gray 75) (map mp ps)

  -- blade fill with a soft diagonal gradient
  withTexture (linearGradientTexture
                 [(0, gray fillHi), (1, gray fillLo)]
                 (v2 ax ay) (v2 (ax + aw) (ay + ah))) $
    fill $ polygon outlinePts

  -- interior details, clipped to the blade
  withClipping (fill $ polygon outlinePts) $ do
    case lgMidrib geom of
      [] -> pure ()
      mr -> withTexture (uniformTexture (gray 78)) $ fill $ polygon (map mp mr)
    mapM_ (\(wd, ps) -> case ps of
              (_ : _ : _) -> strokeLine (sw wd) (gray 92) (map mp ps)
              _ -> pure ())
          (lgVeins geom)
    mapM_ (\(wd, ps) -> case ps of
              (_ : _ : _) -> strokeLine (sw wd) (gray 150) (map mp ps)
              _ -> pure ())
          (lgVeinlets geom)
    mapM_ (\(p, r) ->
             withTexture (uniformTexture (grayA 60 26)) $
               fill $ circle (mp p) (sw (2 * r)))
          (lgDotsDark geom)
    mapM_ (\(p, r) ->
             withTexture (uniformTexture (grayA 255 34)) $
               fill $ circle (mp p) (sw (2 * r)))
          (lgDotsLight geom)
    mapM_ (\(p1, p2) ->
             strokeLine (realToFrac u) (grayA 255 90) [mp p1, mp p2])
          [ hp | hp <- lgHairs geom, insideHair hp ]
    if SfGlossy `elem` sf
      then withTexture (uniformTexture (grayA 255 48)) $ fill $ polygon
             (map mp [(-0.6, 0.52), (0.6, 0.68), (0.6, 0.80), (-0.6, 0.64)])
      else pure ()

  -- blade outline
  withTexture (uniformTexture (gray 40)) $
    stroke (realToFrac outlineW) JoinRound (CapRound, CapRound) $
      polygon outlinePts

  -- marginal cilia stick out past the blade, so draw unclipped
  mapM_ (\(p1, p2) ->
           strokeLine (realToFrac u) (gray 100) [mp p1, mp p2])
        [ hp | hp <- lgHairs geom, not (insideHair hp) ]
  where
    -- surface hairs point up-blade; cilia point outward past the margin
    insideHair ((x1, _), (x2, _)) = abs (x2 - x1) < 0.014

-- The text column ------------------------------------------------------------

drawText :: Font -> Double -> PixelRGBA8 -> Double -> Double -> String
         -> Drawing PixelRGBA8 ()
drawText f px col x y s =
  withTexture (uniformTexture col) $
    printTextAt f (PointSize (realToFrac (px * 0.75))) (v2 x y) s

drawColumn :: Fonts -> Int -> Int -> LeafSpec -> String -> String -> Word64
           -> Drawing PixelRGBA8 ()
drawColumn fonts wI hI sp desc binom seed = do
  let w = fromIntegral wI :: Double
      h = fromIntegral hI :: Double
      u = h / 1404
      x0 = 0.525 * w
      x1 = w - 0.055 * w
      colW = x1 - x0

      -- heading, shrunk to fit if needed
      fitPx p = if realToFrac (textWidth (fItal fonts) (realToFrac p) binom) <= colW
                  then p else fitPx (p * 0.94)
      hpx = fitPx (56 * u)
      hy = 0.145 * h
      bodyPx = 34 * u
      leading = bodyPx * 1.58
      bodyLines = wrapText (fReg fonts) (realToFrac bodyPx) (realToFrac colW) desc
      bodyY0 = hy + 0.075 * h

      hline y wd col =
        withTexture (uniformTexture col) $
          stroke (realToFrac wd) JoinRound (CapRound, CapRound) $
            line (v2 x0 y) (v2 x1 y)

  drawText (fItal fonts) hpx (gray 25) x0 hy binom
  hline (hy + 0.021 * h) (3 * u) (gray 60)

  sequence_
    [ if null l
        then pure ()
        else drawText (fReg fonts) bodyPx (gray 30) x0 y l
    | (i, l) <- zip [0 :: Int ..] bodyLines
    , let y = bodyY0 + fromIntegral i * leading ]

  -- effective-trait legend
  let legendY0 = bodyY0 + fromIntegral (length bodyLines) * leading + 0.045 * h
      rowH = 0.034 * h
      labPx = 20 * u
      valPx = 26 * u
      surfTxt = case spSurfaces sp of
                  [] -> "not stated"
                  ss -> commas (map surfaceName ss)
      commas = foldr1 (\a b -> a ++ ", " ++ b)
      rows =
        [ ("LAMINA",    shapeName (spShape sp))
        , ("APEX",      apexName (spApex sp))
        , ("BASE",      baseName (spBase sp)
                          ++ (if spOblique sp then ", oblique" else ""))
        , ("MARGIN",    marginName (spMargin sp))
        , ("VENATION",  venName (spVen sp)
                          ++ (if spReticulate sp then ", reticulate" else ""))
        , ("SURFACE",   surfTxt)
        , ("INSERTION", if spPetiolate sp then "petiolate" else "sessile")
        ]
  hline (legendY0 - 0.022 * h) (1.5 * u) (gray 140)
  sequence_
    [ do drawText (fBold fonts) labPx (gray 110) x0 y lab
         drawText (fReg fonts) valPx (gray 40) (x0 + 0.13 * w) y val
    | (i, (lab, val)) <- zip [0 :: Int ..] rows
    , let y = legendY0 + fromIntegral i * rowH + 0.012 * h ]

  -- footer
  let foot = "folium \183 seed 0x" ++ showHex seed ""
      fpx = 17 * u
      fw' = realToFrac (textWidth (fReg fonts) (realToFrac fpx) foot)
  drawText (fReg fonts) fpx (gray 165) (x1 - fw') (h - 0.028 * h) foot
