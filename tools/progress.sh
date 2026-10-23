#!/bin/sh
# progress.sh -- rough measure of how much of GNU Emacs 31.1 is ported.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs.
#
# Fortmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fortmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

# Units of work:
#   - C: function definitions in upstream src/*.[ch] (GNU style: name at
#     column 0 followed by " (", or a DEFUN), excluding files for other
#     platforms.  A function counts as done when a "file.c:name" citation
#     for it appears in our sources ("Port of buffer.c:Fset_buffer").
#   - Lisp: top-level definitions (defun, defmacro, cl-defun,
#     define-minor-mode, define-derived-mode) in upstream lisp/, counted
#     done when cited as "path.el:name" with the path under lisp/
#     ("simple.el:newline", "term/xterm.el:terminal-init-xterm").
#   - Packages: the packages in GNU ELPA, NonGNU ELPA and MELPA, as
#     counted in each archive's archive-contents and recorded in
#     tools/package-totals; a package counts as done when listed in
#     tools/ported-packages.
# It's an estimate: functions differ wildly in size, and macros and
# variables are not counted.
#
# Usage: tools/progress.sh [--readme | --update-readme | --fetch-packages]
#   (no option)       print the counts
#   --readme          print the README's progress block (Markdown)
#   --update-readme   replace that block in README.md (and redraw its bars,
#                     docs/progress/*.svg)
#   --fetch-packages  recount the archives' packages into
#                     tools/package-totals (needs curl and network access)
# Needs upstream/emacs (see fetch-upstream.sh) except for --fetch-packages.

set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
up="$root/upstream/emacs"
mode=${1:-}
case "$mode" in
    ""|--readme|--update-readme|--fetch-packages) ;;
    *) echo "usage: tools/progress.sh [--readme | --update-readme | --fetch-packages]" >&2; exit 2 ;;
esac

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Archives: key, name, archive-contents URL.
archives="gnu-elpa|GNU ELPA|https://elpa.gnu.org/packages/archive-contents
nongnu-elpa|NonGNU ELPA|https://elpa.nongnu.org/nongnu/archive-contents
melpa|MELPA|https://melpa.org/packages/archive-contents"

# The number of packages in an archive-contents file: the entries
# (NAME . [...]) of its top-level list, i.e. the lists opened at depth 1
# outside strings.
count_packages() {
    awk 'BEGIN { RS = "\001" }
         {
             n = length($0); depth = 0; instr = 0; count = 0
             for (i = 1; i <= n; i++) {
                 c = substr($0, i, 1)
                 if (instr) {
                     if (c == "\\") i++
                     else if (c == "\"") instr = 0
                 } else if (c == "\"") instr = 1
                 else if (c == "(") { if (depth == 1) count++; depth++ }
                 else if (c == ")") depth--
             }
             print count
         }' "$1"
}

if [ "$mode" = --fetch-packages ]; then
    out="$root/tools/package-totals"
    {
        echo "# Packages per archive, from tools/progress.sh --fetch-packages."
        echo "# fetched $(date -u +%Y-%m-%d)"
        printf '%s\n' "$archives" | while IFS='|' read -r key name url; do
            curl -sSLf --retry 3 -o "$tmp/$key" "$url"
            echo "$key $(count_packages "$tmp/$key")"
        done
    } > "$tmp/totals"
    mv "$tmp/totals" "$out"
    cat "$out"
    exit 0
fi

[ -d "$up/src" ] || { echo "run tools/fetch-upstream.sh first" >&2; exit 1; }

# Every upstream C function, as file:name.
for f in "$up"/src/*.c "$up"/src/*.h; do
    b=$(basename "$f")
    case "$b" in
        w32*|ns*|mac*|msdos*|dosfns*|android*|haiku*) continue ;;
    esac
    sed -n -e "s/^DEFUN (\"[^\"]*\", *\([A-Za-z_][A-Za-z0-9_]*\),.*/$b:\1/p" \
           -e "s/^\([A-Za-z_][A-Za-z0-9_]*\) (.*/$b:\1/p" "$f"
done | grep -v ':DEFUN$' | sort -u > "$tmp/all"

# Functions our sources cite.
grep -ohE '\b[a-z0-9_-]+\.[ch]:[A-Za-z_][A-Za-z0-9_]*' \
    "$root"/src/*.f90 "$root"/src/sys/*.f90 | sort -u > "$tmp/cited"
comm -12 "$tmp/all" "$tmp/cited" > "$tmp/done"

c_total=$(wc -l < "$tmp/all")
c_done=$(wc -l < "$tmp/done")
# Every upstream Lisp definition, as path-under-lisp/:name.
(cd "$up/lisp" && find . -name '*.el' | sed 's|^\./||' | sort | while read -r f; do
    sed -nE "s/^\((defun|cl-defun|defmacro|define-minor-mode|define-derived-mode) ([^ ()]+).*/$(printf '%s' "$f" | sed 's|[/&]|\\&|g'):\2/p" "$f"
done) | sort -u > "$tmp/lall"

# Lisp definitions our sources cite ("simple.el:newline",
# "term/xterm.el:terminal-init-xterm").
grep -ohE '\b[a-z0-9/_-]+\.el:[A-Za-z0-9*+/<>=!?-]*[A-Za-z0-9*+<>=!?]' \
    "$root"/src/*.f90 "$root"/src/sys/*.f90 | sort -u > "$tmp/lcited"
comm -12 "$tmp/lall" "$tmp/lcited" > "$tmp/ldone"

l_total=$(wc -l < "$tmp/lall")
l_done=$(wc -l < "$tmp/ldone")

total=$((c_total + l_total))
done=$((c_done + l_done))

# Packages: totals per archive, and the ported ones ("ARCHIVE NAME" lines).
pkg_total() { awk -v k="$1" '$1 == k { print $2 }' "$root/tools/package-totals"; }
pkg_done() { grep -c "^$1 " "$root/tools/ported-packages" || true; }
gnu_total=$(pkg_total gnu-elpa); gnu_done=$(pkg_done gnu-elpa)
nongnu_total=$(pkg_total nongnu-elpa); nongnu_done=$(pkg_done nongnu-elpa)
melpa_total=$(pkg_total melpa); melpa_done=$(pkg_done melpa)
p_total=$((gnu_total + nongnu_total + melpa_total))
p_done=$((gnu_done + nongnu_done + melpa_done))

# Each bar is an SVG image in docs/progress/ (GitHub strips colors from
# HTML in Markdown, so the bars can't be text): a track with a line every
# 10%, filled from the bottom; a share too small to see still shows a
# sliver.  Arguments: NAME COLOR DONE TOTAL.
bar_svg() {
    awk -v name="$1" -v color="$2" -v d="$3" -v t="$4" '
        BEGIN {
            f = (t > 0) ? d / t : 0
            h = 116 * f
            if (d > 0 && h < 2) h = 2
            printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"28\" height=\"120\" viewBox=\"0 0 28 120\">\n"
            printf "<rect x=\"0.5\" y=\"0.5\" width=\"27\" height=\"119\" rx=\"3\" fill=\"none\" stroke=\"#8c959f\" stroke-opacity=\"0.6\"/>\n"
            for (k = 1; k <= 9; k++)
                printf "<line x1=\"1\" x2=\"27\" y1=\"%.1f\" y2=\"%.1f\" stroke=\"#8c959f\" stroke-opacity=\"0.25\"/>\n", 2 + 11.6 * k, 2 + 11.6 * k
            if (h > 0)
                printf "<rect x=\"2\" y=\"%.2f\" width=\"24\" height=\"%.2f\" rx=\"2\" fill=\"%s\"/>\n", 118 - h, h, color
            printf "</svg>\n"
        }' > "$root/docs/progress/$1.svg"
}

# "DONE<br>of TOTAL" with thousands separators, and the share rounded to
# a hundredth of a percent ("≆" unless exact; never 0 or 100 unless it
# is).
# Arguments: DONE TOTAL WHAT (count or share).
figure() {
    awk -v d="$1" -v t="$2" -v what="$3" '
        function commas(n,   s) {
            s = sprintf("%d", n)
            while (s ~ /[0-9][0-9][0-9][0-9]/) sub(/[0-9][0-9][0-9]($|,)/, ",&", s)
            return s
        }
        BEGIN {
            if (what == "count") { printf "%s<br>of %s", commas(d), commas(t); exit }
            p = (t > 0) ? 100 * d / t : 0
            r = int(p * 100 + 0.5) / 100
            if (d > 0 && r == 0) r = 0.01
            if (d < t && r == 100) r = 99.99
            printf "%s%.2f%%", (r == p) ? "" : "≆", r
        }'
}

# The progress table: columns of NAME LABEL COLOR DONE TOTAL, with
# grouped headings.
readme_block() {
    mkdir -p "$root/docs/progress"
    cols="total|Total|#063D5F|$((done + p_done))|$((total + p_total))
emacs|All|#1D638B|$done|$total
c|C|#4690B9|$c_done|$c_total
elisp|Elisp|#A6C6D9|$l_done|$l_total
packages|All|#1D638B|$p_done|$p_total
gnu-elpa|GNU ELPA|#4690B9|$gnu_done|$gnu_total
nongnu-elpa|NonGNU ELPA|#A6C6D9|$nongnu_done|$nongnu_total
melpa|MELPA|#4690B9|$melpa_done|$melpa_total"
    bars="" counts="" shares=""
    while IFS='|' read -r name label color d t; do
        bar_svg "$name" "$color" "$d" "$t"
        bars="$bars<td align=\"center\"><img src=\"docs/progress/$name.svg\" width=\"28\" height=\"120\" alt=\"$label: $(figure "$d" "$t" share)\"></td>"
        counts="$counts<td align=\"center\">$(figure "$d" "$t" count)</td>"
        shares="$shares<td align=\"center\">$(figure "$d" "$t" share)</td>"
    done <<EOF2
$cols
EOF2
    echo "**Progress:** early porting. Fortmacs runs as a bare-bones terminal editor; most of Emacs is still"
    echo "missing. See [PORTING.md](PORTING.md) for the roadmap and what works so far."
    echo
    echo "<table>"
    echo '<tr><th rowspan="2">Total</th><th colspan="3">Emacs</th><th colspan="4">Packages</th></tr>'
    echo "<tr><th>All</th><th>C</th><th>Elisp</th><th>All</th><th>GNU<br>ELPA</th><th>NonGNU<br>ELPA</th><th>MELPA</th></tr>"
    echo "<tr>$bars</tr>"
    echo "<tr>$counts</tr>"
    echo "<tr>$shares</tr>"
    echo "</table>"
    echo
    echo "Total is Emacs (its C functions and Elisp definitions) and the packages"
    echo "together. A rough estimate from \`tools/progress.sh\`."
}

case "$mode" in
    --readme)
        readme_block
        ;;
    --update-readme)
        readme_block > "$tmp/block"
        awk -v blockfile="$tmp/block" '
            /^<!-- progress:start -->$/ { print; while ((getline l < blockfile) > 0) print l; skip = 1; next }
            /^<!-- progress:end -->$/ { skip = 0 }
            !skip { print }' "$root/README.md" > "$tmp/readme"
        grep -q '^<!-- progress:start -->$' "$tmp/readme" || { echo "README.md has no progress markers" >&2; exit 1; }
        mv "$tmp/readme" "$root/README.md"
        ;;
    *)
        pct=$(awk "BEGIN { printf \"%.2f\", 100 * $done / $total }")
        echo "C functions:      $c_done of $c_total"
        echo "Lisp definitions: $l_done of $l_total"
        echo "Overall:          Done $done of $total (~$pct%)"
        echo "Packages:         $p_done of $p_total (GNU ELPA $gnu_done of $gnu_total," \
             "NonGNU ELPA $nongnu_done of $nongnu_total, MELPA $melpa_done of $melpa_total)"
        ;;
esac
