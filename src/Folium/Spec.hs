-- | Core data types describing a leaf in the vocabulary of
-- leaf morphology (see the Wikipedia glossary of leaf morphology).
module Folium.Spec where

-- | Overall outline of the lamina (blade).
data Shape
  = Ovate        -- ^ egg-shaped, widest below the middle
  | Obovate      -- ^ inversely ovate, widest above the middle
  | Elliptic     -- ^ widest at the middle
  | Lanceolate   -- ^ long, narrow, widest below the middle
  | Oblanceolate -- ^ inversely lanceolate
  | Linear       -- ^ very narrow, sides nearly parallel
  | Oblong       -- ^ sides roughly parallel, rounded ends
  | Orbicular    -- ^ approximately circular
  | Rhombic      -- ^ diamond-shaped
  | Spatulate    -- ^ spoon-shaped, narrow base widening to a rounded top
  | Deltoid      -- ^ triangular, widest near the base
  | Reniform     -- ^ kidney-shaped, wider than long
  | Cordiform    -- ^ heart-shaped
  | Flabellate   -- ^ fan-shaped
  deriving (Show, Eq)

-- | Form of the apex (tip).
data ApexForm
  = AxAcuminate  -- ^ tapering to a long, drawn-out point (drip tip)
  | AxAcute      -- ^ ending in a sharp point, angle < 90 degrees
  | AxObtuse     -- ^ blunt, angle > 90 degrees
  | AxRounded
  | AxTruncate   -- ^ cut straight across
  | AxEmarginate -- ^ with a distinct notch
  | AxRetuse     -- ^ with a shallow notch
  | AxMucronate  -- ^ rounded but abruptly tipped with a small point (mucro)
  | AxCuspidate  -- ^ with a sharp, elongated, rigid tip
  deriving (Show, Eq)

-- | Form of the base.
data BaseForm
  = BsCuneate    -- ^ wedge-shaped, straight sides
  | BsAttenuate  -- ^ tapering gradually, concave sides
  | BsObtuse
  | BsRounded
  | BsTruncate
  | BsCordate    -- ^ heart-shaped, notched with rounded lobes
  | BsSagittate  -- ^ arrowhead, lobes pointing downward
  | BsHastate    -- ^ spearhead, lobes pointing outward
  | BsAuriculate -- ^ with small ear-like lobes
  deriving (Show, Eq)

-- | Margin (edge) of the blade.
data MarginForm
  = MgEntire      -- ^ smooth, without teeth
  | MgSerrate     -- ^ saw-toothed, teeth pointing forward
  | MgSerrulate   -- ^ finely serrate
  | MgBiserrate   -- ^ doubly serrate
  | MgDentate     -- ^ toothed, teeth pointing outward
  | MgDenticulate -- ^ finely dentate
  | MgCrenate     -- ^ with rounded teeth (scalloped)
  | MgCrenulate   -- ^ finely crenate
  | MgSinuate     -- ^ with deep, smooth waves
  | MgUndulate    -- ^ gently wavy
  | MgLobate      -- ^ with rounded lobes
  | MgPinnatifid  -- ^ deeply cleft toward the midrib
  | MgSpinose     -- ^ with stiff spine-tipped teeth
  deriving (Show, Eq)

-- | Venation pattern.
data VenForm
  = VnPinnate     -- ^ one midrib with lateral secondaries (feather-veined)
  | VnPalmate     -- ^ several primary veins radiating from the base
  | VnParallel    -- ^ veins running side by side, base to apex
  | VnArcuate     -- ^ veins arching from the base toward the apex
  | VnDichotomous -- ^ veins forking repeatedly in pairs (as in Ginkgo)
  deriving (Show, Eq)

-- | Surface / texture qualities of the lamina.
data Surface
  = SfGlabrous   -- ^ smooth, hairless
  | SfGlossy     -- ^ lustrous, shining
  | SfGlaucous   -- ^ with a pale waxy bloom
  | SfPubescent  -- ^ covered in fine soft hairs
  | SfRugose     -- ^ wrinkled, with sunken veinlets
  | SfPunctate   -- ^ dotted with glands or pits
  | SfScabrous   -- ^ rough to the touch
  | SfCoriaceous -- ^ leathery, thick
  | SfMembranous -- ^ thin, translucent
  | SfCiliate    -- ^ margin fringed with fine hairs
  deriving (Show, Eq)

data LeafSpec = LeafSpec
  { spShape      :: Shape
  , spApex       :: ApexForm
  , spBase       :: BaseForm
  , spMargin     :: MarginForm
  , spVen        :: VenForm
  , spReticulate :: Bool       -- ^ fine net of cross-venules between secondaries
  , spSurfaces   :: [Surface]
  , spPetiolate  :: Bool       -- ^ stalked ('True') or sessile ('False')
  , spOblique    :: Bool       -- ^ asymmetric blade
  , spTerms      :: [(String, String)] -- ^ (category, matched term), in order found
  } deriving (Show)

defaultSpec :: LeafSpec
defaultSpec = LeafSpec
  { spShape      = Elliptic
  , spApex       = AxAcute
  , spBase       = BsCuneate
  , spMargin     = MgEntire
  , spVen        = VnPinnate
  , spReticulate = False
  , spSurfaces   = []
  , spPetiolate  = True
  , spOblique    = False
  , spTerms      = []
  }

-- Human-readable names for the effective-trait legend ------------------------

shapeName :: Shape -> String
shapeName s = case s of
  Ovate -> "ovate"; Obovate -> "obovate"; Elliptic -> "elliptic"
  Lanceolate -> "lanceolate"; Oblanceolate -> "oblanceolate"; Linear -> "linear"
  Oblong -> "oblong"; Orbicular -> "orbicular"; Rhombic -> "rhombic"
  Spatulate -> "spatulate"; Deltoid -> "deltoid"; Reniform -> "reniform"
  Cordiform -> "cordate"; Flabellate -> "flabellate"

apexName :: ApexForm -> String
apexName a = case a of
  AxAcuminate -> "acuminate"; AxAcute -> "acute"; AxObtuse -> "obtuse"
  AxRounded -> "rounded"; AxTruncate -> "truncate"; AxEmarginate -> "emarginate"
  AxRetuse -> "retuse"; AxMucronate -> "mucronate"; AxCuspidate -> "cuspidate"

baseName :: BaseForm -> String
baseName b = case b of
  BsCuneate -> "cuneate"; BsAttenuate -> "attenuate"; BsObtuse -> "obtuse"
  BsRounded -> "rounded"; BsTruncate -> "truncate"; BsCordate -> "cordate"
  BsSagittate -> "sagittate"; BsHastate -> "hastate"; BsAuriculate -> "auriculate"

marginName :: MarginForm -> String
marginName m = case m of
  MgEntire -> "entire"; MgSerrate -> "serrate"; MgSerrulate -> "serrulate"
  MgBiserrate -> "doubly serrate"; MgDentate -> "dentate"
  MgDenticulate -> "denticulate"; MgCrenate -> "crenate"
  MgCrenulate -> "crenulate"; MgSinuate -> "sinuate"; MgUndulate -> "undulate"
  MgLobate -> "lobate"; MgPinnatifid -> "pinnatifid"; MgSpinose -> "spinose"

venName :: VenForm -> String
venName v = case v of
  VnPinnate -> "pinnate"; VnPalmate -> "palmate"; VnParallel -> "parallel"
  VnArcuate -> "arcuate"; VnDichotomous -> "dichotomous"

surfaceName :: Surface -> String
surfaceName s = case s of
  SfGlabrous -> "glabrous"; SfGlossy -> "lustrous"; SfGlaucous -> "glaucous"
  SfPubescent -> "pubescent"; SfRugose -> "rugose"; SfPunctate -> "punctate"
  SfScabrous -> "scabrous"; SfCoriaceous -> "coriaceous"
  SfMembranous -> "membranous"; SfCiliate -> "ciliate"
