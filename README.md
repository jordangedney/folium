# folium

E-ink wallpapers generated from botanical descriptions, in Haskell.

Write a description of a leaf using the technical vocabulary of
[leaf morphology](https://en.wikipedia.org/wiki/Glossary_of_leaf_morphology)
— *ovate, serrulate, acuminate, cordate, craspedodromous, glaucous…* —
and `folium` draws that leaf procedurally, mounts it as a framed
herbarium plate on the left, and typesets your description on the right.
Output is a grayscale PNG quantized and ordered-dithered for e-ink panels.

```
$ folium -o leaf.png "A petiolate leaf, the blade broadly ovate, the margin \
    doubly serrate, the apex acuminate and the base cordate; venation pinnate \
    and finely reticulate, the lamina rugose and somewhat coriaceous."
```

Every description yields the same individual (the seed is a hash of the
text), so a wallpaper is reproducible; pass `--seed` to get a different
individual of the same species.

## Building

Needs GHC and cabal (tested with GHC 9.12). All dependencies are pure
Haskell (`Rasterific`, `FontyFruity`, `JuicyPixels`).

```
cabal build
cabal install          # optional: puts `folium` on your PATH
```

Text is set in a serif TTF found under `/usr/share/fonts` (Liberation
Serif, DejaVu Serif, …). Point `--font` at any `.ttf` to override.

## Usage

```
folium [options] [DESCRIPTION...]

  -o, --out FILE      output PNG (default leaf.png)
  -W, --width N       wallpaper width in px (default 1872)
  -H, --height N      wallpaper height in px (default 1404)
      --seed N        override the seed (default: hash of description)
      --levels N      gray levels for e-ink (default 16)
      --bw            1-bit output (same as --levels 2)
      --no-dither     disable ordered dithering
      --file PATH     read the description from a file
      --random        invent a random description
      --font PATH     serif TTF to use (bold/italic siblings auto-found)
      --list-terms    print the recognized vocabulary and exit
```

With no description at all, `folium` invents one (`--random`) — handy for
a cron job that gives your reader a new specimen each morning.

The default size is 1872×1404 (reMarkable 2, landscape). Other panels:
Kindle Paperwhite `-W 1236 -H 1648`, Kobo Clara `-W 1072 -H 1448`,
a 7.5" Waveshare `-W 800 -H 480`. Portrait sizes work too; the frame and
column scale with the page.

## Vocabulary

Run `folium --list-terms` for the full list. Categories:

| category   | examples |
|------------|----------|
| blade      | ovate, obovate, elliptic, lanceolate, oblanceolate, linear, oblong, orbicular, rhombic, spatulate, deltoid, reniform, cordate, flabellate |
| apex       | acuminate, acute, obtuse, rounded, truncate, emarginate, retuse, mucronate, cuspidate |
| base       | cuneate, attenuate, rounded, truncate, cordate, sagittate, hastate, auriculate, oblique |
| margin     | entire, serrate, serrulate, doubly serrate, dentate, denticulate, crenate, crenulate, sinuate, undulate, lobed, pinnatifid, spinose, ciliate |
| venation   | pinnate, craspedodromous, palmate, actinodromous, parallel, arcuate, acrodromous, dichotomous, reticulate, cross-venulate |
| surface    | glabrous, lustrous, glaucous, pubescent, tomentose, rugose, bullate, punctate, scabrous, coriaceous, membranous |
| attachment | petiolate, sessile |

Words that can describe either end (*rounded, obtuse, truncate, acute,
cordate*) are assigned by context: "the base rounded" vs "apex rounded".
Unrecognized words are simply ignored, so you can write freely around the
terms. Each shape also carries sensible defaults (a *linear* leaf gets
parallel venation, a *reniform* one a cordate base) that any explicit term
overrides.

## How it works

* `Folium.Glossary` — scans the prose for glossary terms and folds them
  into a `LeafSpec`.
* `Folium.Leaf` — builds the geometry: a half-width profile per blade
  shape, multiplied by apex/base "cap" curves, then displaced along the
  outline normal by the margin's tooth wave (sawtooth for serrate,
  triangle for dentate, scallops for crenate, deep inward sinuses for
  lobed/pinnatifid…). Basal lobes, apical notches and mucros are inserted
  as explicit points. Veins are Bézier secondaries, fans, parallels or a
  recursive dichotomy, clipped to the blade.
* `Folium.Page` — Rasterific composition: shadowed frame, bevel, mat,
  plate label, then the wrapped description, an effective-trait legend
  and a footer with the seed.
* `Folium.Dither` — luminance → N gray levels with 8×8 Bayer dithering.

`samples/batch.sh` renders one wallpaper per shape/margin/base variant
for eyeballing changes.
