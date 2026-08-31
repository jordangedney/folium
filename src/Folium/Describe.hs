-- | Generates a random botanical description in proper glossary
-- vocabulary, for @--random@ mode.
module Folium.Describe
  ( randomDescription
  ) where

import Folium.Rand

pick :: [a] -> Rng -> (a, Rng)
pick xs g =
  let (d, g') = nextD g
  in (xs !! min (length xs - 1) (floor (d * fromIntegral (length xs))), g')

randomDescription :: Rng -> String
randomDescription g0 =
  let (attach, g1) = pick ["A petiolate leaf", "A sessile leaf", "A petiolate leaf"] g0
      (shape, g2) = pick
        [ "ovate", "obovate", "elliptic", "lanceolate", "oblanceolate"
        , "oblong", "orbicular", "rhombic", "spatulate", "deltoid"
        , "cordate", "reniform", "flabellate", "linear" ] g1
      (margin, g3) = pick
        [ "entire", "finely serrulate", "coarsely serrate", "doubly serrate"
        , "dentate", "denticulate", "crenate", "crenulate", "sinuate"
        , "undulate", "shallowly lobed", "deeply pinnatifid", "spinose" ] g2
      (apex, g4) = pick
        [ "acuminate", "acute", "obtuse", "rounded", "emarginate"
        , "retuse", "mucronate", "cuspidate" ] g3
      (base, g5) = pick
        [ "cuneate", "attenuate", "rounded", "truncate", "cordate"
        , "sagittate", "hastate", "auriculate", "oblique" ] g4
      (ven, g6) = pick
        [ "pinnate", "palmate", "arcuate", "parallel", "dichotomous"
        , "pinnate and finely reticulate"
        , "craspedodromous", "palmate and cross-venulate" ] g5
      (s1, g7) = pick
        [ "glabrous", "lustrous", "glaucous", "pubescent", "tomentose"
        , "rugose", "punctate", "scabrous" ] g6
      (s2, g8) = pick
        [ "coriaceous", "membranous", "chartaceous", "somewhat coriaceous" ] g7
      (cilia, _) = pick
        [ "", "", "", "; the margin minutely ciliate" ] g8
      shapeClause
        | shape `elem` ["cordate", "reniform"] = "the blade " ++ shape
        | otherwise = "the blade " ++ shape
  in attach ++ ", " ++ shapeClause
       ++ ", the margin " ++ margin
       ++ ", the apex " ++ apex
       ++ " and the base " ++ base
       ++ ". Venation " ++ ven
       ++ "; the lamina " ++ s1 ++ " and " ++ s2 ++ cilia ++ "."
