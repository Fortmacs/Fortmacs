#!/bin/sh
# generate.sh: writes icon.svg (the end-program organization's profile picture) and banner.svg
# (the logo at the top of its profile README) next to this script.
#
# "end" (pixel letters, 12-unit cells, two empty cells between letters) in blue with three
# offset multiply trails (green, sand, rust), cut by scan lines 6 units thick at the top of
# each 12-unit pixel row; "program" (8-unit cells) beneath it in black, left-aligned.
# The PNGs are rendered from the SVGs.

set -e
cd "$(dirname "$0")"

# cells ROWS X0 Y0 SIZE: one square subpath per "X" of the "|"-separated rows.
letters=$(awk '
function cells(rows, n, x0, y0, s,   i, j, out) {
  out = ""
  for (i = 1; i <= n; i++) for (j = 1; j <= length(rows[i]); j++)
    if (substr(rows[i], j, 1) == "X") out = out sprintf("M%g %gh%gv%gh-%gz", x0 + s*(j-1), y0 + s*(i-1), s, s, s)
  return out
}
function word(w, s, gap,   k, c, n, R, x, out) {
  x = 0; out = ""
  for (k = 1; k <= length(w); k++) {
    c = substr(w, k, 1); n = split(g[c], R, "|")
    out = out cells(R, n, x, s*a[c], s)
    x += s*length(R[1]) + s*gap
  }
  return out
}
BEGIN {
  g["e"] = ".XXX|X..X|X..X|XXXX|X...|X...|.XXX"; a["e"] = 0
  g["n"] = "XXX.|X..X|X..X|X..X|X..X|X..X|X..X"; a["n"] = 0
  g["d"] = "...X|...X|.XXX|X..X|X..X|X..X|X..X|X..X|.XXX"; a["d"] = -2
  g["p"] = "XXX.|X..X|X..X|X..X|X..X|X..X|XXX.|X...|X..."; a["p"] = 0
  g["r"] = "X.XX|XX..|XX..|X...|X...|X...|X..."; a["r"] = 0
  g["o"] = ".XX.|X..X|X..X|X..X|X..X|X..X|.XX."; a["o"] = 0
  g["g"] = ".XXX|X..X|X..X|X..X|X..X|X..X|.XXX|...X|XXX."; a["g"] = 0
  g["a"] = ".XX.|...X|...X|.XXX|X..X|X..X|.XXX"; a["a"] = 0
  g["m"] = "XX.X.|X.X.X|X.X.X|X.X.X|X.X.X|X.X.X|X.X.X"; a["m"] = 0
  print word("end", 12, 2); print word("program", 8, 1)
}')
end=$(echo "$letters" | sed -n 1p)
program=$(echo "$letters" | sed -n 2p)

# notch K: the squares a trail shifted by 4K misses at inside corners of "end", so the trail
# covers them (for each empty cell whose upper and left neighbours are filled and whose
# upper-left neighbour is empty).
notch() {
  echo "$end" | awk -v size=12 -v k="$1" '{
    n = split($0, parts, "M")
    for (i = 2; i <= n; i++) { split(parts[i], xy, /[ h]/); f[xy[1]+0, xy[2]+0] = 1 }
    out = ""; d = 4*k
    for (key in f) {
      split(key, q, SUBSEP); cx = q[1]; cy = q[2] + size
      if (!((cx, cy) in f) && ((cx - size, cy) in f) && !((cx - size, cy - size) in f))
        out = out sprintf("M%g %gh%gv%gh-%gz", cx - d, cy - d, d, d, d)
    }
    print out
  }'
}
trail1=$(notch 1)
trail2=$(notch 2)
trail3=$(notch 3)

# Scan lines: 6 units at the top of each 12-unit pixel row of "end" (rows start at y = -24).
lines=$(awk 'BEGIN { for (y = -24; y <= 96; y += 12)
  printf "<rect x=\"-500\" y=\"%d\" width=\"2000\" height=\"6\" fill=\"#000\"/>", y }')

# The words, centred on (0, 0): "program" is 280 wide, the block runs from y = -24 to 184.
words="<g transform=\"translate(-140 -80)\"><defs><mask id=\"scan-lines\" maskUnits=\"userSpaceOnUse\" x=\"-500\" y=\"-500\" width=\"2000\" height=\"2000\"><rect x=\"-500\" y=\"-500\" width=\"2000\" height=\"2000\" fill=\"#fff\"/>$lines</mask></defs><g mask=\"url(#scan-lines)\"><path fill=\"#976057\" fill-opacity=\"0.6\" style=\"mix-blend-mode:multiply\" transform=\"translate(12 12)\" d=\"$end$trail3\"/><path fill=\"#B39574\" fill-opacity=\"0.8\" style=\"mix-blend-mode:multiply\" transform=\"translate(8 8)\" d=\"$end$trail2\"/><path fill=\"#608A83\" style=\"mix-blend-mode:multiply\" transform=\"translate(4 4)\" d=\"$end$trail1\"/><path fill=\"#578497\" d=\"$end\"/></g><path fill=\"#1A1A1A\" transform=\"translate(0 112)\" d=\"$program\"/></g>"

cat > icon.svg <<SVG
<svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" viewBox="-4.5 11.5 133 133" shape-rendering="crispEdges" role="img" aria-label="end program"><title>end program</title><rect x="-4.5" y="11.5" width="133" height="133" fill="#578497"/><rect x="-3" y="13" width="130" height="130" fill="#D6D6D3"/><g transform="translate(62 78) scale(0.42)">$words</g></svg>
SVG

cat > banner.svg <<SVG
<svg xmlns="http://www.w3.org/2000/svg" width="1280" height="640" viewBox="0 0 1280 640" shape-rendering="crispEdges" role="img" aria-label="end program"><title>end program</title><rect width="1280" height="640" fill="#578497"/><rect x="6" y="6" width="1268" height="628" fill="#D6D6D3"/><g transform="translate(634 314) scale(2)">$words</g></svg>
SVG
