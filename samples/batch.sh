#!/bin/sh
# renders one small wallpaper per description for a contact sheet
cd "$(dirname "$0")/.."
i=0
while IFS= read -r d; do
  i=$((i+1))
  cabal run -v0 folium -- -W 936 -H 702 --no-dither -o "samples/v$i.png" "$d" >/dev/null
done <<'DESC'
Leaf ovate, margin serrate, apex acuminate, base rounded, venation pinnate, glabrous.
Leaf obovate, margin crenate, apex emarginate, base attenuate, venation pinnate, glaucous.
Leaf elliptic, margin entire, apex mucronate, base cuneate, venation arcuate, lustrous and coriaceous.
Leaf lanceolate, margin serrulate, apex acuminate, base attenuate, venation pinnate, pubescent.
Leaf oblanceolate, margin dentate, apex obtuse, base attenuate, venation pinnate, scabrous.
Leaf linear, margin entire, apex acute, base truncate, venation parallel, glabrous.
Leaf oblong, margin undulate, apex rounded, base rounded, venation pinnate, punctate.
Leaf orbicular, margin crenulate, apex retuse, base cordate, venation palmate, rugose.
Leaf rhombic, margin denticulate, apex acute, base cuneate, venation pinnate, membranous.
Leaf spatulate, margin entire, apex rounded, base attenuate, venation pinnate, ciliate.
Leaf deltoid, margin sinuate, apex acute, base truncate, venation palmate and reticulate.
Leaf reniform, margin entire, apex rounded, base cordate, venation palmate, glabrous.
Leaf cordate, margin doubly serrate, apex cuspidate, base cordate, venation palmate, tomentose.
Leaf flabellate, margin lobed, apex truncate, base cuneate, venation dichotomous, glaucous.
Leaf ovate, margin pinnatifid, apex acute, base sagittate, venation pinnate, glabrous.
Leaf ovate, margin spinose, apex cuspidate, base hastate, venation pinnate, coriaceous.
Leaf elliptic, margin lobed, apex obtuse, base auriculate, venation pinnate and cross-venulate, sessile.
Leaf ovate, margin entire, apex acute, base oblique and rounded, venation pinnate, bullate.
DESC
magick montage samples/v*.png -tile 6x3 -geometry +4+4 -background '#888' samples/sheet.png
