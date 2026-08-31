-- | Recognizes technical terms of leaf morphology in free prose and
-- folds them into a 'LeafSpec'.  Vocabulary follows the Wikipedia
-- "Glossary of leaf morphology".
module Folium.Glossary
  ( parseDescription
  , binomialFor
  , glossaryListing
  ) where

import Data.Char (isAlpha, toLower)
import Data.List (find)
import Data.Word (Word64)

import Folium.Spec

-- Parser state: the spec plus flags recording which categories were
-- set explicitly (so shape-implied soft defaults never override them).
data PB = PB
  { pbSpec  :: LeafSpec
  , pbApexE :: Bool
  , pbBaseE :: Bool
  , pbVenE  :: Bool
  }

type Toks = [String]
type Ctx  = (Toks, Int)   -- all tokens + index of the matched token

data Entry = Entry
  { eCat   :: String
  , eApply :: Ctx -> PB -> PB
  }

-- | Parse a description into a leaf specification.
parseDescription :: String -> LeafSpec
parseDescription txt =
  let toks = tokenize txt
      pb0  = PB defaultSpec False False False
      pb'  = walk toks 0 pb0
  in (pbSpec pb') { spTerms = reverse (spTerms (pbSpec pb')) }
  where
    walk toks i pb
      | i >= length toks = pb
      | otherwise =
          case bigramAt toks i of
            Just (term, e) -> walk toks (i + 2) (apply term e (toks, i) pb)
            Nothing ->
              case lookup (toks !! i) unigrams of
                Just e  -> walk toks (i + 1) (apply (toks !! i) e (toks, i) pb)
                Nothing -> walk toks (i + 1) pb
    apply term e ctx pb =
      let pb' = eApply e ctx pb
          sp  = pbSpec pb'
      in pb' { pbSpec = sp { spTerms = (eCat e, term) : spTerms sp } }

tokenize :: String -> Toks
tokenize = words . map clean . map toLower
  where clean c = if isAlpha c then c else ' '

bigramAt :: Toks -> Int -> Maybe (String, Entry)
bigramAt toks i
  | i + 1 >= length toks = Nothing
  | otherwise =
      let pair = toks !! i ++ " " ++ toks !! (i + 1)
      in fmap ((,) pair . snd) (find ((== pair) . fst) bigrams)

-- Context helpers ----------------------------------------------------------

-- Is one of these words within two tokens of the match?
near :: [String] -> Ctx -> Bool
near ws (toks, i) =
  any (`elem` ws) (take 5 (drop (max 0 (i - 2)) toks))

baseWords, apexWords :: [String]
baseWords = ["base", "basally", "basal", "below"]
apexWords = ["apex", "apically", "apical", "tip"]

nearBase :: Ctx -> Bool
nearBase = near baseWords

-- For words that can describe either end: which qualifier is nearest?
-- A preceding qualifier ("the apex obtuse") beats a following one at the
-- same distance ("obtuse, the base ...").
data End = AtApex | AtBase | Unqualified

nearestEnd :: Ctx -> End
nearestEnd (toks, i) =
  let window = [ (j, toks !! j) | j <- [max 0 (i - 2) .. min (length toks - 1) (i + 2)]
                              , j /= i ]
      score j = abs (j - i) * 2 + (if j > i then 1 else 0)
      cands = [ (score j, e) | (j, w) <- window
                             , Just e <- [end w] ]
      end w | w `elem` apexWords = Just AtApex
            | w `elem` baseWords = Just AtBase
            | otherwise = Nothing
  in case cands of
       [] -> Unqualified
       _  -> snd (minimum' cands)
  where
    minimum' = foldr1 (\a b -> if fst a <= fst b then a else b)

-- Spec updaters -------------------------------------------------------------

setApex :: ApexForm -> PB -> PB
setApex a pb = pb { pbSpec = (pbSpec pb) { spApex = a }, pbApexE = True }

setBase :: BaseForm -> PB -> PB
setBase b pb = pb { pbSpec = (pbSpec pb) { spBase = b }, pbBaseE = True }

setMargin :: MarginForm -> PB -> PB
setMargin m pb = pb { pbSpec = (pbSpec pb) { spMargin = m } }

setVen :: VenForm -> PB -> PB
setVen v pb = pb { pbSpec = (pbSpec pb) { spVen = v }, pbVenE = True }

addSurface :: Surface -> PB -> PB
addSurface s pb =
  let sp = pbSpec pb
  in pb { pbSpec = sp { spSurfaces = spSurfaces sp
                          ++ [s | s `notElem` spSurfaces sp] } }

-- Shape entries may install soft defaults for apex/base/venation that
-- suit the shape, without marking those categories as explicit.
setShape :: Shape -> Maybe ApexForm -> Maybe BaseForm -> Maybe VenForm -> PB -> PB
setShape sh mA mB mV pb =
  let sp0 = pbSpec pb
      sp1 = sp0 { spShape = sh }
      sp2 = maybe sp1 (\a -> if pbApexE pb then sp1 else sp1 { spApex = a }) mA
      sp3 = maybe sp2 (\b -> if pbBaseE pb then sp2 else sp2 { spBase = b }) mB
      sp4 = maybe sp3 (\v -> if pbVenE pb then sp3 else sp3 { spVen = v }) mV
  in pb { pbSpec = sp4 }

-- The vocabulary ------------------------------------------------------------

shapeE :: Shape -> Maybe ApexForm -> Maybe BaseForm -> Maybe VenForm -> Entry
shapeE sh a b v = Entry "blade" (\_ -> setShape sh a b v)

apexE :: ApexForm -> Entry
apexE a = Entry "apex" (\_ -> setApex a)

baseE :: BaseForm -> Entry
baseE b = Entry "base" (\_ -> setBase b)

-- Terms valid for either end: decided by nearby words, with a default.
endE :: ApexForm -> BaseForm -> Bool -> Entry
endE a b apexByDefault = Entry "apex/base" go
  where
    go ctx = case nearestEnd ctx of
      AtBase -> setBase b
      AtApex -> setApex a
      Unqualified
        | near ["lobes", "lobe", "teeth", "tooth", "sinuses"] ctx -> id
        | apexByDefault -> setApex a
        | otherwise -> setBase b

marginE :: MarginForm -> Entry
marginE m = Entry "margin" (\_ -> setMargin m)

venE :: VenForm -> Entry
venE v = Entry "venation" (\_ -> setVen v)

surfE :: Surface -> Entry
surfE s = Entry "surface" (\_ -> addSurface s)

bigrams :: [(String, Entry)]
bigrams =
  [ ("doubly serrate", marginE MgBiserrate)
  , ("cross venulate", Entry "venation" (\_ pb ->
      pb { pbSpec = (pbSpec pb) { spReticulate = True } }))
  ]

unigrams :: [(String, Entry)]
unigrams =
  -- Blade shape
  [ ("ovate",        shapeE Ovate Nothing Nothing Nothing)
  , ("obovate",      shapeE Obovate Nothing Nothing Nothing)
  , ("elliptic",     shapeE Elliptic Nothing Nothing Nothing)
  , ("elliptical",   shapeE Elliptic Nothing Nothing Nothing)
  , ("lanceolate",   shapeE Lanceolate Nothing Nothing Nothing)
  , ("oblanceolate", shapeE Oblanceolate Nothing Nothing Nothing)
  , ("linear",       shapeE Linear (Just AxAcute) (Just BsAttenuate) (Just VnParallel))
  , ("ensiform",     shapeE Linear (Just AxAcute) (Just BsAttenuate) (Just VnParallel))
  , ("oblong",       shapeE Oblong (Just AxRounded) (Just BsRounded) Nothing)
  , ("orbicular",    shapeE Orbicular (Just AxRounded) (Just BsRounded) Nothing)
  , ("rotund",       shapeE Orbicular (Just AxRounded) (Just BsRounded) Nothing)
  , ("suborbicular", shapeE Orbicular (Just AxRounded) (Just BsRounded) Nothing)
  , ("rhombic",      shapeE Rhombic Nothing Nothing Nothing)
  , ("rhomboid",     shapeE Rhombic Nothing Nothing Nothing)
  , ("spatulate",    shapeE Spatulate (Just AxRounded) (Just BsAttenuate) Nothing)
  , ("spathulate",   shapeE Spatulate (Just AxRounded) (Just BsAttenuate) Nothing)
  , ("deltoid",      shapeE Deltoid (Just AxAcute) (Just BsTruncate) Nothing)
  , ("deltate",      shapeE Deltoid (Just AxAcute) (Just BsTruncate) Nothing)
  , ("reniform",     shapeE Reniform (Just AxRounded) (Just BsCordate) (Just VnPalmate))
  , ("cordiform",    shapeE Cordiform (Just AxAcuminate) (Just BsCordate) (Just VnPalmate))
  , ("flabellate",   shapeE Flabellate (Just AxRounded) (Just BsCuneate) (Just VnDichotomous))

  -- Apex-only terms
  , ("acuminate", apexE AxAcuminate)
  , ("emarginate", apexE AxEmarginate)
  , ("retuse",    apexE AxRetuse)
  , ("mucronate", apexE AxMucronate)
  , ("apiculate", apexE AxMucronate)
  , ("cuspidate", apexE AxCuspidate)

  -- Base-only terms
  , ("cuneate",   baseE BsCuneate)
  , ("attenuate", baseE BsAttenuate)
  , ("sagittate", baseE BsSagittate)
  , ("hastate",   baseE BsHastate)
  , ("auriculate", baseE BsAuriculate)
  , ("oblique",   Entry "base" (\_ pb ->
      pb { pbSpec = (pbSpec pb) { spOblique = True } }))
  -- "cordate": shape if used alone, base form if near "base"
  , ("cordate", Entry "base/blade" (\ctx pb ->
      if nearBase ctx
        then setBase BsCordate pb
        else setShape Cordiform (Just AxAcuminate) Nothing (Just VnPalmate)
               (setBase BsCordate pb)))

  -- Either-end terms, resolved by context
  , ("acute",    endE AxAcute BsCuneate True)
  , ("obtuse",   endE AxObtuse BsObtuse True)
  , ("rounded",  endE AxRounded BsRounded True)
  , ("truncate", endE AxTruncate BsTruncate True)

  -- Margin
  , ("entire",      marginE MgEntire)
  , ("serrate",     marginE MgSerrate)
  , ("serrulate",   marginE MgSerrulate)
  , ("biserrate",   marginE MgBiserrate)
  , ("dentate",     marginE MgDentate)
  , ("denticulate", marginE MgDenticulate)
  , ("crenate",     marginE MgCrenate)
  , ("crenulate",   marginE MgCrenulate)
  , ("sinuate",     marginE MgSinuate)
  , ("undulate",    marginE MgUndulate)
  , ("wavy",        marginE MgUndulate)
  , ("lobed",       marginE MgLobate)
  , ("lobate",      marginE MgLobate)
  , ("pinnatifid",  marginE MgPinnatifid)
  , ("cleft",       marginE MgPinnatifid)
  , ("spinose",     marginE MgSpinose)
  , ("spiny",       marginE MgSpinose)
  , ("ciliate",     surfE SfCiliate)

  -- Venation
  , ("pinnate",          venE VnPinnate)
  , ("penninerved",      venE VnPinnate)
  , ("craspedodromous",  venE VnPinnate)
  , ("brochidodromous",  venE VnPinnate)
  , ("palmate",          venE VnPalmate)
  , ("palminerved",      venE VnPalmate)
  , ("actinodromous",    venE VnPalmate)
  , ("parallelodromous", venE VnParallel)
  , ("parallel",         venE VnParallel)
  , ("arcuate",          venE VnArcuate)
  , ("acrodromous",      venE VnArcuate)
  , ("campylodromous",   venE VnArcuate)
  , ("dichotomous",      venE VnDichotomous)
  , ("reticulate", Entry "venation" (\_ pb ->
      pb { pbSpec = (pbSpec pb) { spReticulate = True } }))

  -- Surface / texture
  , ("glabrous",   surfE SfGlabrous)
  , ("glossy",     surfE SfGlossy)
  , ("lustrous",   surfE SfGlossy)
  , ("nitid",      surfE SfGlossy)
  , ("shining",    surfE SfGlossy)
  , ("glaucous",   surfE SfGlaucous)
  , ("pruinose",   surfE SfGlaucous)
  , ("pubescent",  surfE SfPubescent)
  , ("puberulent", surfE SfPubescent)
  , ("tomentose",  surfE SfPubescent)
  , ("villous",    surfE SfPubescent)
  , ("hirsute",    surfE SfPubescent)
  , ("pilose",     surfE SfPubescent)
  , ("rugose",     surfE SfRugose)
  , ("bullate",    surfE SfRugose)
  , ("punctate",   surfE SfPunctate)
  , ("scabrous",   surfE SfScabrous)
  , ("coriaceous", surfE SfCoriaceous)
  , ("leathery",   surfE SfCoriaceous)
  , ("membranous", surfE SfMembranous)
  , ("chartaceous", surfE SfMembranous)

  -- Attachment
  , ("petiolate",  Entry "attachment" (\_ pb ->
      pb { pbSpec = (pbSpec pb) { spPetiolate = True } }))
  , ("sessile",    Entry "attachment" (\_ pb ->
      pb { pbSpec = (pbSpec pb) { spPetiolate = False } }))
  , ("subsessile", Entry "attachment" (\_ pb ->
      pb { pbSpec = (pbSpec pb) { spPetiolate = False } }))
  ]

-- | A printable listing of the recognized vocabulary.
glossaryListing :: String
glossaryListing = unlines $
  [ "Recognized terms (category: terms):"
  , ""
  , "  blade      " ++ row ["ovate","obovate","elliptic","lanceolate","oblanceolate"]
  , "             " ++ row ["linear","ensiform","oblong","orbicular","rotund","rhombic"]
  , "             " ++ row ["spatulate","deltoid","reniform","cordate","cordiform","flabellate"]
  , "  apex       " ++ row ["acuminate","acute","obtuse","rounded","truncate"]
  , "             " ++ row ["emarginate","retuse","mucronate","apiculate","cuspidate"]
  , "  base       " ++ row ["cuneate","attenuate","rounded","truncate","cordate"]
  , "             " ++ row ["sagittate","hastate","auriculate","oblique"]
  , "  margin     " ++ row ["entire","serrate","serrulate","doubly serrate","dentate"]
  , "             " ++ row ["denticulate","crenate","crenulate","sinuate","undulate"]
  , "             " ++ row ["lobed","pinnatifid","spinose","ciliate"]
  , "  venation   " ++ row ["pinnate","craspedodromous","brochidodromous","palmate"]
  , "             " ++ row ["actinodromous","parallel","arcuate","acrodromous"]
  , "             " ++ row ["dichotomous","reticulate","cross-venulate"]
  , "  surface    " ++ row ["glabrous","lustrous","glaucous","pubescent","tomentose"]
  , "             " ++ row ["villous","rugose","bullate","punctate","scabrous"]
  , "             " ++ row ["coriaceous","membranous","chartaceous"]
  , "  attachment " ++ row ["petiolate","sessile"]
  ]
  where row = foldr1 (\a b -> a ++ ", " ++ b)

-- | An invented, ink-flavoured pseudo-Latin binomial for the plate label.
binomialFor :: LeafSpec -> Word64 -> String
binomialFor sp seed =
  let genera = [ "Atramentophyllum"  -- ink-leaf
               , "Chartophyllum"     -- paper-leaf
               , "Cinereophyllum"    -- ash-gray-leaf
               , "Plumbophyllum"     -- lead-gray-leaf
               , "Umbriphyllum"      -- shadow-leaf
               ]
      genus = genera !! fromIntegral (seed `mod` fromIntegral (length genera))
      epithet = case spShape sp of
        Ovate -> "ovatum"; Obovate -> "obovatum"; Elliptic -> "ellipticum"
        Lanceolate -> "lanceolatum"; Oblanceolate -> "oblanceolatum"
        Linear -> "lineare"; Oblong -> "oblongum"; Orbicular -> "orbiculare"
        Rhombic -> "rhombeum"; Spatulate -> "spathulatum"
        Deltoid -> "deltoideum"; Reniform -> "reniforme"
        Cordiform -> "cordatum"; Flabellate -> "flabellatum"
  in genus ++ " " ++ epithet
