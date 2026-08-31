-- | Procedural leaf geometry.  Everything here works in "leaf space":
-- the blade runs from the base at (0,0) to the apex at (0,1), +x to the
-- right (as seen from above), apex pointing up.  The renderer flips y.
module Folium.Leaf
  ( LeafGeom(..)
  , P
  , buildLeaf
  ) where

import Folium.Rand
import Folium.Spec

type P = (Double, Double)

data LeafGeom = LeafGeom
  { lgOutline   :: [P]              -- ^ closed blade polygon
  , lgPetiole   :: [P]              -- ^ petiole polyline (empty if sessile)
  , lgPetioleW  :: Double
  , lgMidrib    :: [P]              -- ^ tapered midrib polygon (empty for some venations)
  , lgVeins     :: [(Double, [P])]  -- ^ (stroke width, polyline)
  , lgVeinlets  :: [(Double, [P])]  -- ^ fainter cross-venules
  , lgHairs     :: [(P, P)]         -- ^ marginal cilia / surface hairs
  , lgDotsDark  :: [(P, Double)]    -- ^ (center, radius) texture stipple
  , lgDotsLight :: [(P, Double)]
  }

-- Shape profiles -------------------------------------------------------------
-- Half-width of the smooth blade (before margin teeth and end caps) at
-- height t in [0,1]: w(t) = w0 * (t^a (1-t)^b)^p, normalized to peak w0.

profileParams :: Shape -> (Double, Double, Double, Double) -- (w0, a, b, p)
profileParams s = case s of
  Ovate        -> (0.335, 0.90, 1.70, 0.50)
  Obovate      -> (0.335, 1.70, 0.90, 0.50)
  Elliptic     -> (0.300, 1.00, 1.00, 0.50)
  Lanceolate   -> (0.165, 0.80, 2.20, 0.50)
  Oblanceolate -> (0.165, 2.20, 0.80, 0.50)
  Linear       -> (0.055, 0.50, 0.50, 0.28)
  Oblong       -> (0.230, 0.45, 0.45, 0.33)
  Orbicular    -> (0.480, 1.00, 1.00, 0.50)
  Spatulate    -> (0.270, 2.60, 0.80, 0.60)
  Deltoid      -> (0.360, 0.16, 1.15, 0.85)
  Reniform     -> (0.620, 0.85, 0.85, 0.50)
  Cordiform    -> (0.400, 0.52, 1.60, 0.58)
  Flabellate   -> (0.420, 1.80, 0.34, 0.72)
  Rhombic      -> (0.330, 1, 1, 1)  -- special-cased below

smoothHW :: Shape -> Double -> Double
smoothHW Rhombic t =
  let (w0, _, _, _) = profileParams Rhombic
      tp = 0.48
      v  = if t <= tp then t / tp else (1 - t) / (1 - tp)
  in w0 * max 0 v ** 1.08
smoothHW s t
  | t <= 0 || t >= 1 = 0
  | otherwise =
      let (w0, a, b, p) = profileParams s
          tp   = a / (a + b)
          peak = (tp ** a * (1 - tp) ** b) ** p
      in w0 * (t ** a * (1 - t) ** b) ** p / peak

-- End caps -------------------------------------------------------------------
-- Multipliers applied to the smooth profile near the apex/base; the
-- window length and curve of the multiplier give each named form.

apexWin :: ApexForm -> Double
apexWin a = case a of
  AxAcuminate -> 0.30; AxAcute -> 0.16; AxObtuse -> 0.09
  AxRounded -> 0.15; AxTruncate -> 0.015
  AxEmarginate -> 0.26; AxRetuse -> 0.20
  AxMucronate -> 0.15; AxCuspidate -> 0.24

apexMul :: ApexForm -> Double -> Double  -- s in [0,1], 0 = cap start, 1 = tip
apexMul a s = case a of
  AxAcuminate  -> (1 - s) ** 2.2
  AxAcute      -> 1 - s
  AxObtuse     -> 1 - s
  AxRounded    -> circ s
  AxTruncate   -> 1
  AxEmarginate -> circ s
  AxRetuse     -> circ s
  AxMucronate  -> circ s
  AxCuspidate  -> (1 - s) ** 1.7
  where circ v = sqrt (max 0 (1 - v * v))

-- Where the side sampling stops (before any inserted apex feature).
apexTEnd :: ApexForm -> Double
apexTEnd a = case a of
  AxEmarginate -> 0.955
  AxRetuse     -> 0.965
  AxMucronate  -> 0.985
  AxCuspidate  -> 0.975
  AxTruncate   -> 0.999
  _            -> 0.9985

baseWin :: BaseForm -> Double
baseWin b = case b of
  BsCuneate -> 0.28; BsAttenuate -> 0.40; BsObtuse -> 0.14
  BsRounded -> 0.17; BsTruncate -> 0.02
  BsCordate -> 0.10; BsSagittate -> 0.08; BsHastate -> 0.08
  BsAuriculate -> 0.09

baseMul :: BaseForm -> Double -> Double  -- s in [0,1], 1 = bottom of blade
baseMul b s = case b of
  BsCuneate    -> 1 - s
  BsAttenuate  -> (1 - s) ** 2.0
  BsObtuse     -> 1 - s * 0.9
  BsRounded    -> sqrt (max 0 (1 - s * s))
  BsTruncate   -> 1
  _            -> sqrt (max 0 (1 - s * s * 0.6)) -- lobed bases: blend into lobe

lobedBase :: BaseForm -> Bool
lobedBase b = b `elem` [BsCordate, BsSagittate, BsHastate, BsAuriculate]

-- Combined half-width, all caps applied.
hwAt :: LeafSpec -> Double -> Double
hwAt sp t =
  let wA = apexWin (spApex sp)
      wB = baseWin (spBase sp)
      mA = if t > 1 - wA then apexMul (spApex sp) ((t - (1 - wA)) / wA) else 1
      mB = if t < wB then baseMul (spBase sp) (1 - t / wB) else 1
  in smoothHW (spShape sp) t * mA * mB

-- Margin ---------------------------------------------------------------------
-- Wavelength (fraction of side arc length), amplitude, and wave shape.

marginParams :: MarginForm -> (Double, Double)
marginParams m = case m of
  MgEntire      -> (1, 0)
  MgSerrate     -> (0.052, 0.0125)
  MgSerrulate   -> (0.020, 0.0050)
  MgBiserrate   -> (0.060, 0.0135)
  MgDentate     -> (0.058, 0.0125)
  MgDenticulate -> (0.022, 0.0050)
  MgCrenate     -> (0.062, 0.0105)
  MgCrenulate   -> (0.024, 0.0045)
  MgSinuate     -> (0.180, 0.0220)
  MgUndulate    -> (0.095, 0.0120)
  MgLobate      -> (0.230, 1)      -- relative, handled below
  MgPinnatifid  -> (0.190, 1)
  MgSpinose     -> (0.062, 0.0300)

frac' :: Double -> Double
frac' x = x - fromIntegral (floor x :: Int)

-- Wave value: mostly outward [0,1]; sinuate/undulate swing both ways.
marginWave :: MarginForm -> Double -> Double
marginWave m u = case m of
  MgEntire      -> 0
  MgSerrate     -> saw u
  MgSerrulate   -> saw u
  MgBiserrate   -> 0.68 * saw u + 0.32 * saw (u * 2.6)
  MgDentate     -> tri u
  MgDenticulate -> tri u
  MgCrenate     -> bump u
  MgCrenulate   -> bump u
  MgSinuate     -> sin (2 * pi * u)
  MgUndulate    -> 0.8 * sin (2 * pi * u)
  MgLobate      -> negate (bump u)          -- sinuses cut inward
  MgPinnatifid  -> negate (bump u ** 0.62)
  MgSpinose     -> spike u + 0.25 * tri u
  where
    saw v = let f = frac' v in if f < 0.78 then f / 0.78 else (1 - f) / 0.22
    tri v = let f = frac' v in 1 - abs (2 * f - 1)
    bump v = 0.5 * (1 - cos (2 * pi * v))
    spike v = max 0 (cos (2 * pi * v)) ** 14

-- Sinus depth for lobed margins is relative to the local half-width.
marginRelative :: MarginForm -> Maybe Double
marginRelative MgLobate     = Just 0.30
marginRelative MgPinnatifid = Just 0.66
marginRelative _            = Nothing

-- Side construction ----------------------------------------------------------

sampleCount :: Int
sampleCount = 640

-- One side of the blade, base to apex, teeth applied.  side = 1 (right)
-- or -1 (left).
sidePoints :: LeafSpec -> Rng -> Double -> [P]
sidePoints sp rng side =
  let t0 = if lobedBase (spBase sp) then baseWin (spBase sp) + 0.02 else 0.0015
      t1 = apexTEnd (spApex sp)
      n  = sampleCount
      asym = if spOblique sp && side < 0 then 0.80 else 1.0
      ts = [ t0 + (t1 - t0) * fromIntegral i / fromIntegral (n - 1)
           | i <- [0 .. n - 1] ]
      smooth = [ (side * asym * hwAt sp t, t) | t <- ts ]
      -- cumulative arc length for tooth phase
      dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
      arcs = scanl (+) 0 (zipWith dist smooth (drop 1 smooth))
      total = last arcs
      (phase, _) = nextD rng
      (lam, ampAbs) = marginParams (spMargin sp)
      (w0, _, _, _) = profileParams (spShape sp)
      toothAt s t =
        let u = s / (lam * total) + phase * 3
            wv = marginWave (spMargin sp) u
            endFade = case marginRelative (spMargin sp) of
                        Just _  -> 1
                        Nothing -> min ((t - t0) / 0.035) ((t1 - t) / 0.035)
            taper = minimum [ 1, hwAt sp t / (0.38 * w0), endFade ]
            fanScale = if spShape sp == Flabellate then 0.5 else 1
            amp = case marginRelative (spMargin sp) of
                    Just rel -> rel * fanScale * hwAt sp t
                    Nothing  -> ampAbs * min 1 (w0 / 0.28)
        in wv * amp * taper
      pts = zip3 smooth arcs ts
      displaced =
        [ displace i p (toothAt s t)
        | (i, ((_, _), s, t)) <- zip [0 ..] pts
        , let p = smooth !! i ]
      displace i (x, y) d =
        let (px, py) = smooth !! max 0 (i - 1)
            (nx', ny') = smooth !! min (n - 1) (i + 1)
            (tx, ty) = (nx' - px, ny' - py)
            len = max 1e-9 (sqrt (tx * tx + ty * ty))
            -- outward normal for a side traversed base->apex
            (ox, oy) = (side * ty / len, side * (-tx) / len)
        in (x + d * ox, y + d * oy)
  in displaced

-- Basal lobes for cordate / sagittate / hastate / auriculate bases.
baseLobe :: LeafSpec -> Double -> [P]
baseLobe sp side =
  let (w0, _, _, _) = profileParams (spShape sp)
      t0 = baseWin (spBase sp) + 0.02
      join = (side * hwAt sp t0, t0)
      notch = (0, 0.025)
      smoothJoin (ex, ey) =
        drop 1 (bez3 (ex, ey) (ex + side * 0.02, ey + 0.07) join 6)
      arcPts cx cy r a0 a1 k =
        [ (side * (cx + r * cos th), cy + r * sin th)
        | i <- [0 .. k]
        , let th = a0 + (a1 - a0) * fromIntegral i / fromIntegral k ]
  in case spBase sp of
       -- arcs sweep from the notch, down under the lobe, back up and
       -- outward to meet the blade profile
       BsCordate ->
         let r = min 0.13 (0.44 * w0)
             (cx, cy) = (min 0.15 (0.46 * w0), 0.04 + 0.25 * r)
             arc = arcPts cx cy r (pi * 1.02) (pi * 1.86) (26 :: Int)
         in notch : arc ++ smoothJoin (last arc)
       BsAuriculate ->
         let r = min 0.07 (0.20 * w0)
             (cx, cy) = (min 0.08 (0.22 * w0), 0.01)
             arc = arcPts cx cy r (pi * 1.02) (pi * 1.80) (18 :: Int)
         in notch : arc ++ smoothJoin (last arc)
       BsSagittate ->
         [ notch, (side * 0.34 * w0, -0.16), join ]
       BsHastate ->
         [ notch, (side * 1.02 * w0, -0.05), join ]
       _ -> []

-- Inserted apex features -----------------------------------------------------

apexInsert :: LeafSpec -> [P]
apexInsert sp = case spApex sp of
  AxEmarginate -> [(0, apexTEnd AxEmarginate - 0.060)]
  AxRetuse     -> [(0, apexTEnd AxRetuse - 0.024)]
  AxMucronate  -> [(0.006, 0.99), (0, 1.035), (-0.006, 0.99)]
  AxCuspidate  -> [(0.010, 0.985), (0, 1.06), (-0.010, 0.985)]
  _            -> []

-- Full outline ---------------------------------------------------------------

buildOutline :: LeafSpec -> Rng -> [P]
buildOutline sp rng =
  let right = baseLobe sp 1 ++ sidePoints sp (splitRng 11 rng) 1
      left  = baseLobe sp (-1) ++ sidePoints sp (splitRng 11 rng) (-1)
  in right ++ apexInsert sp ++ reverse left

-- Veins ----------------------------------------------------------------------

bez3 :: P -> P -> P -> Int -> [P]
bez3 (x0, y0) (x1, y1) (x2, y2) n =
  [ let t = fromIntegral i / fromIntegral n
        u = 1 - t
        x = u * u * x0 + 2 * u * t * x1 + t * t * x2
        y = u * u * y0 + 2 * u * t * y1 + t * t * y2
    in (x, y)
  | i <- [0 .. n] ]

midribPoly :: LeafSpec -> [P]
midribPoly sp
  | spVen sp == VnParallel = []
  | otherwise =
      let top = case spApex sp of
                  AxEmarginate -> 0.90
                  AxRetuse     -> 0.93
                  _            -> 0.975
      in [(-0.0075, 0.015), (0.0075, 0.015), (0.0011, top), (-0.0011, top)]

pinnateVeins :: LeafSpec -> Rng -> ([(Double, [P])], [(Double, [P])])
pinnateVeins sp rng =
  let rs = doubles rng
      nv = 9
      tops = [ 0.075 + 0.68 * fromIntegral i / fromIntegral (nv - 1)
                 + 0.015 * (rs !! i - 0.5)
             | i <- [0 .. nv - 1] ]
      vein side t =
        let te = min 0.86 (t + 0.13 + 0.02 * (rs !! (round (t * 100)) - 0.5))
            end = (side * 0.86 * hwAt sp te, te)
            ctrl = (side * 0.46 * hwAt sp ((t + te) / 2), t + 0.012)
        in (0.0038, bez3 (0, t) ctrl end 22)
      veins = [ vein side t | t <- tops, side <- [1, -1]
              , hwAt sp t > 0.05 ]
      links
        | spReticulate sp =
            [ (0.0016, [a !! k, b !! (k + 1)])
            | ((_, a), (_, b)) <- zip veins (drop 2 veins)
            , k <- [5, 10, 15] ]
        | otherwise = []
  in (veins, links)

palmateVeins :: LeafSpec -> ([(Double, [P])], [(Double, [P])])
palmateVeins sp =
  let prim k =
        let s = fromIntegral (signum k) :: Double
            fr = fromIntegral (abs k) / 3
            te = 0.86 - 0.20 * fromIntegral (abs k)
            end = (s * (fr ** 0.72) * 0.86 * hwAt sp te, te)
            ctrl = (fst end * 0.38, te * 0.42)
        in (0.0048 - 0.0007 * fromIntegral (abs k), bez3 (0, 0.03) ctrl end 24)
      veins = [ prim k | k <- [-3, -2, -1, 1, 2, 3 :: Int] ]
      links | spReticulate sp =
                [ (0.0016, [pa, pb])
                | ((_, a), (_, b)) <- zip veins (drop 1 veins)
                , let pa = a !! 16, let pb = b !! 16 ]
            | otherwise = []
  in (veins, links)

pathVeins :: LeafSpec -> [Double] -> Double -> ([(Double, [P])], [(Double, [P])])
pathVeins sp fracs wdt =
  let path f = [ (f * 0.88 * hwAt sp t, t)
               | i <- [0 .. 60 :: Int]
               , let t = 0.02 + 0.95 * fromIntegral i / 60 ]
      veins = [ (wdt, path f) | f <- fracs ]
  in (veins, [])

dichotomousVeins :: LeafSpec -> Rng -> ([(Double, [P])], [(Double, [P])])
dichotomousVeins sp rng =
  let go _ _ _ 0 _ = []
      go (x, y) ang len depth g =
        let (j1, g1) = rangeD (-0.06) 0.06 g
            (j2, g2) = rangeD (-0.06) 0.06 g1
            end = (x + len * sin ang, y + len * cos ang)
            inside (px, py) = py < 0.96 && py > 0 && abs px < 0.95 * hwAt sp py
            spread = 0.30 - 0.02 * fromIntegral depth
            gl = splitRng (fromIntegral depth * 2 + 1) g2
            gr = splitRng (fromIntegral depth * 2 + 7) g2
        in if not (inside end) then []
           else (0.0020, [(x, y), end])
                : go end (ang - spread + j1) (len * 0.86) (depth - 1) gl
               ++ go end (ang + spread + j2) (len * 0.86) (depth - 1) gr
  in (go (0, 0.03) 0 0.19 (7 :: Int) rng, [])

buildVeins :: LeafSpec -> Rng -> ([(Double, [P])], [(Double, [P])])
buildVeins sp rng = case spVen sp of
  VnPinnate     -> pinnateVeins sp rng
  VnPalmate     -> palmateVeins sp
  VnParallel    -> pathVeins sp [ fromIntegral k / 4.5 | k <- [-4 .. 4 :: Int], k /= 0 ] 0.0028
  VnArcuate     -> pathVeins sp [-0.80, -0.42, 0.42, 0.80] 0.0040
  VnDichotomous -> dichotomousVeins sp rng

-- Surface texture ------------------------------------------------------------

interiorPoints :: LeafSpec -> Rng -> Int -> [P]
interiorPoints sp rng n = go (doubles rng) n
  where
    go _ 0 = []
    go (a : b : rest) k =
      let t = 0.04 + 0.92 * a
          x = (2 * b - 1) * 0.92 * hwAt sp t
      in if abs x < 0.92 * hwAt sp t && hwAt sp t > 0.02
           then (x, t) : go rest (k - 1)
           else go rest k
    go _ _ = []

buildTexture :: LeafSpec -> Rng
             -> ([(P, P)], [(P, Double)], [(P, Double)])
buildTexture sp rng =
  let sf = spSurfaces sp
      rs = doubles (splitRng 31 rng)
      dark
        | SfRugose `elem` sf =
            [ (p, 0.002 + 0.004 * r)
            | (p, r) <- zip (interiorPoints sp (splitRng 32 rng) 320) rs ]
        | otherwise = []
      darkDots
        | SfPunctate `elem` sf || SfScabrous `elem` sf =
            [ (p, 0.0018 + 0.002 * r)
            | (p, r) <- zip (interiorPoints sp (splitRng 33 rng) 150) (drop 5 rs) ]
        | otherwise = []
      light
        | SfRugose `elem` sf =
            [ (p, 0.002 + 0.004 * r)
            | (p, r) <- zip (interiorPoints sp (splitRng 34 rng) 320) (drop 9 rs) ]
        | otherwise = []
      surfHairs
        | SfPubescent `elem` sf =
            [ ((x, y), (x + 0.012 * (a - 0.5), y + 0.018))
            | ((x, y), a) <- zip (interiorPoints sp (splitRng 35 rng) 420) (drop 3 rs) ]
        | otherwise = []
      cilia
        | SfCiliate `elem` sf =
            let outline = buildOutline sp (splitRng 11 rng)
                m = length outline
                every = max 1 (m `div` 110)
                pick = [ (outline !! i, outline !! min (m - 1) (i + 2))
                       | i <- [4, 4 + every .. m - 5] ]
            in [ ((x, y), (x + hx * 0.016, y + hy * 0.016))
               | ((x, y), (x2, y2)) <- pick
               , let (dx, dy) = (x2 - x, y2 - y)
               , let l = max 1e-9 (sqrt (dx * dx + dy * dy))
               , let (hx, hy) = if x >= 0 then (dy / l, -dx / l) else (-dy / l, dx / l) ]
        | otherwise = []
  in (surfHairs ++ cilia, dark ++ darkDots, light)

-- Petiole --------------------------------------------------------------------

buildPetiole :: LeafSpec -> Rng -> ([P], Double)
buildPetiole sp rng
  | not (spPetiolate sp) = ([], 0)
  | otherwise =
      let (bendR, _) = rangeD (-0.045) 0.045 rng
          y0 = if lobedBase (spBase sp) then 0.02 else 0.005
          petLen = 0.15
      in (bez3 (0, y0) (bendR, -petLen * 0.5) (bendR * 1.7, -petLen) 16, 0.013)

-- Assembly -------------------------------------------------------------------

buildLeaf :: LeafSpec -> Rng -> LeafGeom
buildLeaf sp rng =
  let outline = buildOutline sp (splitRng 1 rng)
      (veins, links) = buildVeins sp (splitRng 2 rng)
      (hairs, dark, light) = buildTexture sp (splitRng 3 rng)
      (pet, petW) = buildPetiole sp (splitRng 4 rng)
      clipV (w, ps) = (w, filter insideish ps)
      insideish (x, y) = y > (-0.2) && y < 1.05 && abs x < 1.2
  in LeafGeom
       { lgOutline   = outline
       , lgPetiole   = pet
       , lgPetioleW  = petW
       , lgMidrib    = midribPoly sp
       , lgVeins     = map clipV veins
       , lgVeinlets  = links
       , lgHairs     = hairs
       , lgDotsDark  = dark
       , lgDotsLight = light
       }
