-- | folium: e-ink leaf wallpapers from botanical descriptions.
module Main (main) where

import Codec.Picture (writePng)
import Control.Monad (filterM, forM_)
import Data.Char (toLower)
import Data.List (isInfixOf, isSuffixOf)
import Data.Word (Word64)
import Graphics.Text.TrueType (Font, loadFontFile)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Folium.Describe (randomDescription)
import Folium.Dither (toEInk)
import Folium.Glossary (binomialFor, glossaryListing, parseDescription)
import Folium.Leaf (buildLeaf)
import Folium.Page (Fonts(..), renderPage)
import Folium.Rand (hashStr, mkRng, splitRng)
import Folium.Spec

data Cfg = Cfg
  { cOut    :: FilePath
  , cWidth  :: Int
  , cHeight :: Int
  , cSeed   :: Maybe Word64
  , cLevels :: Int
  , cDither :: Bool
  , cFont   :: Maybe FilePath
  , cSource :: Source
  }

data Source = FromArgs String | FromFile FilePath | RandomDesc

defCfg :: Cfg
defCfg = Cfg "leaf.png" 1872 1404 Nothing 16 True Nothing RandomDesc

usage :: String
usage = unlines
  [ "folium - e-ink leaf wallpapers generated from botanical descriptions"
  , ""
  , "usage: folium [options] [DESCRIPTION...]"
  , ""
  , "  The description is scanned for technical terms of leaf morphology"
  , "  (ovate, serrulate, acuminate, cordate, craspedodromous, ...) and the"
  , "  leaf they describe is drawn as a framed plate beside the text."
  , "  With no description, an individual is invented (--random)."
  , ""
  , "options:"
  , "  -o, --out FILE      output PNG (default leaf.png)"
  , "  -W, --width N       wallpaper width in px (default 1872)"
  , "  -H, --height N      wallpaper height in px (default 1404)"
  , "      --seed N        override the seed (default: hash of description)"
  , "      --levels N      gray levels for e-ink (default 16)"
  , "      --bw            1-bit output (same as --levels 2)"
  , "      --no-dither     disable ordered dithering"
  , "      --file PATH     read the description from a file"
  , "      --random        invent a random description"
  , "      --font PATH     serif TTF to use (bold/italic siblings auto-found)"
  , "      --list-terms    print the recognized vocabulary and exit"
  , "  -h, --help          this text"
  ]

parseArgs :: [String] -> Either String Cfg
parseArgs = go defCfg []
  where
    go c ws [] = Right $ case (ws, cSource c) of
      ([], _)   -> c
      (_, FromFile _) -> c
      _ -> c { cSource = FromArgs (unwords (reverse ws)) }
    go c ws (a : rest) = case a of
      "-o"          -> arg rest (\v r -> go c { cOut = v } ws r)
      "--out"       -> arg rest (\v r -> go c { cOut = v } ws r)
      "-W"          -> num rest (\v r -> go c { cWidth = v } ws r)
      "--width"     -> num rest (\v r -> go c { cWidth = v } ws r)
      "-H"          -> num rest (\v r -> go c { cHeight = v } ws r)
      "--height"    -> num rest (\v r -> go c { cHeight = v } ws r)
      "--seed"      -> num rest (\v r -> go c { cSeed = Just (fromIntegral (v :: Integer)) } ws r)
      "--levels"    -> num rest (\v r -> go c { cLevels = v } ws r)
      "--bw"        -> go c { cLevels = 2 } ws rest
      "--no-dither" -> go c { cDither = False } ws rest
      "--file"      -> arg rest (\v r -> go c { cSource = FromFile v } ws r)
      "--random"    -> go c { cSource = RandomDesc } ws rest
      "--font"      -> arg rest (\v r -> go c { cFont = Just v } ws r)
      _ | a `elem` ["-h", "--help"] -> Left usage
        | a == "--list-terms" -> Left glossaryListing
        | take 1 a == "-" -> Left ("unknown option: " ++ a ++ "\n\n" ++ usage)
        | otherwise -> go c (a : ws) rest
    arg (v : r) k = k v r
    arg [] _ = Left "missing argument\n"
    num (v : r) k = case reads v of
      [(n, "")] -> k n r
      _ -> Left ("not a number: " ++ v ++ "\n")
    num [] _ = Left "missing argument\n"

-- Font discovery -------------------------------------------------------------

findTTFs :: FilePath -> IO [FilePath]
findTTFs dir = do
  ok <- doesDirectoryExist dir
  if not ok then pure [] else do
    entries <- listDirectory dir
    let paths = map (dir </>) entries
    dirs <- filterM doesDirectoryExist paths
    let ttfs = [ p | p <- paths, ".ttf" `isSuffixOf` map toLower p ]
    subs <- mapM findTTFs dirs
    pure (ttfs ++ concat subs)

loadFontOr :: String -> FilePath -> IO Font
loadFontOr what p = do
  r <- loadFontFile p
  case r of
    Right f -> pure f
    Left e -> do
      hPutStrLn stderr ("failed to load " ++ what ++ " font " ++ p ++ ": " ++ e)
      exitFailure

pickFonts :: Maybe FilePath -> IO Fonts
pickFonts (Just p) = do
  reg <- loadFontOr "regular" p
  let sibling tag = do
        let cands = [ swapTag p tag ]
        found <- filterM doesFileExist cands
        case found of
          (f : _) -> loadFontOr tag f
          [] -> pure reg
      swapTag path tag =
        foldr (\t acc -> if t `isInfixOf` acc
                           then replaceOnce t tag acc else acc)
              path ["Regular"]
        where replaceOnce old new s = case breakOn old s of
                Just (a, b) -> a ++ new ++ b
                Nothing -> s
  b <- sibling "Bold"
  i <- sibling "Italic"
  pure (Fonts reg b i)
pickFonts Nothing = do
  ttfs <- findTTFs "/usr/share/fonts"
  let firstWith pat alt = case [ p | p <- ttfs, pat `isInfixOf` p ] of
        (p : _) -> Just p
        [] -> alt
      mReg = firstWith "Serif-Regular" (firstWith "Regular" Nothing)
  case mReg of
    Nothing -> do
      hPutStrLn stderr "no TTF fonts found under /usr/share/fonts; use --font PATH"
      exitFailure
    Just regP -> do
      reg <- loadFontOr "regular" regP
      let pick pat = case [ p | p <- ttfs, pat `isInfixOf` p ] of
            (p : _) -> loadFontOr pat p
            [] -> pure reg
      b <- pick "Serif-Bold"
      i <- pick "Serif-Italic"
      pure (Fonts reg b i)

breakOn :: String -> String -> Maybe (String, String)
breakOn pat s = go "" s
  where
    go _ [] = Nothing
    go acc rest@(c : cs)
      | pat `isPrefix` rest = Just (reverse acc, drop (length pat) rest)
      | otherwise = go (c : acc) cs
    isPrefix p q = take (length p) q == p

-- Main -----------------------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  cfg <- case parseArgs argv of
    Left msg -> putStr msg >> exitSuccess >> pure defCfg
    Right c -> pure c

  desc <- case cSource cfg of
    FromArgs s -> pure s
    FromFile p -> readFile p
    RandomDesc -> do
      g <- case cSeed cfg of
             Just s -> pure (mkRng s)
             Nothing -> do
               -- no seed and no text given: vary with the clock
               t <- fmap show getPicoTime
               pure (mkRng (hashStr t))
      pure (randomDescription g)

  let spec = parseDescription desc
      seed = maybe (hashStr desc) id (cSeed cfg)
      rng = mkRng seed
      binom = binomialFor spec seed
      geom = buildLeaf spec (splitRng 100 rng)

  fonts <- pickFonts (cFont cfg)
  let img = renderPage fonts (cWidth cfg) (cHeight cfg)
              spec geom desc binom seed (splitRng 200 rng)
      final = toEInk (cLevels cfg) (cDither cfg) img
  writePng (cOut cfg) final

  putStrLn ("description: " ++ desc)
  putStrLn ""
  case spTerms spec of
    [] -> putStrLn "no glossary terms recognized; drew the default leaf (try --list-terms)"
    ts -> do
      putStrLn "recognized terms:"
      forM_ ts $ \(cat, term) ->
        putStrLn ("  " ++ pad 12 cat ++ term)
  putStrLn ""
  putStrLn "effective traits:"
  forM_ [ ("lamina", shapeName (spShape spec))
        , ("apex", apexName (spApex spec))
        , ("base", baseName (spBase spec) ++ (if spOblique spec then ", oblique" else ""))
        , ("margin", marginName (spMargin spec))
        , ("venation", venName (spVen spec) ++ (if spReticulate spec then ", reticulate" else ""))
        , ("surface", case spSurfaces spec of
                        [] -> "not stated"
                        ss -> foldr1 (\a b -> a ++ ", " ++ b) (map surfaceName ss))
        , ("insertion", if spPetiolate spec then "petiolate" else "sessile") ] $
    \(k, v) -> putStrLn ("  " ++ pad 12 k ++ v)
  putStrLn ""
  putStrLn ("plate:  " ++ binom)
  putStrLn ("seed:   0x" ++ showHexW seed)
  putStrLn ("output: " ++ cOut cfg ++ "  ("
              ++ show (cWidth cfg) ++ "x" ++ show (cHeight cfg) ++ ", "
              ++ show (cLevels cfg) ++ " gray levels"
              ++ (if cDither cfg then ", dithered" else "") ++ ")")
  where
    pad n s = s ++ replicate (max 1 (n - length s)) ' '
    showHexW w = let s = go w "" in if null s then "0" else s
      where go 0 acc = acc
            go v acc = go (v `div` 16) (hexDigit (fromIntegral (v `mod` 16)) : acc)
            hexDigit d = "0123456789abcdef" !! d

getPicoTime :: IO Integer
getPicoTime = do
  -- crude wall-clock entropy without extra dependencies
  s <- readFile "/proc/uptime"
  pure (round (read (takeWhile (/= ' ') s) * 1e6 :: Double))
