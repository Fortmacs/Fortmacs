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
#   --update-readme   replace that block in README.md (and redraw its pies,
#                     docs/progress/pie.svg and pie-dark.svg)
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

# The pie is an SVG image, docs/progress/NAME.svg (GitHub strips colors
# from HTML in Markdown): the whole pie is everything to port, each colored
# slice the ported part of one kind, to scale (a share too small to see
# still shows a sliver), and the black slice what is still to port.
# A legend on the right gives each kind's share and count.  The colors are
# the logo's: purple and blue first, then red, tan and gray.  The labels
# are TEXT and the lines between slices EDGE: gray and cream for GitHub's
# light theme, cream and cream for its dark one (outlining the pie).
# Arguments: NAME TEXT EDGE and lines of LABEL|COLOR|DONE|TOTAL.
pie_svg() {
    printf '%s\n' "$4" | awk -F'|' -v text="$2" -v edge="$3" '
        function commas(n,   s) {
            s = sprintf("%d", n)
            while (s ~ /[0-9][0-9][0-9][0-9]/) sub(/[0-9][0-9][0-9]($|,)/, ",&", s)
            return s
        }
        function share(d, t,   p, r) {
            p = (t > 0) ? 100 * d / t : 0
            r = int(p * 100 + 0.5) / 100
            if (d > 0 && r == 0) r = 0.01
            if (d < t && r == 100) r = 99.99
            return sprintf("%s%.2f%%", (r == p) ? "" : "≆", r)
        }
        # A wedge from A to B (fractions of a turn, clockwise from 12 o clock).
        function wedge(a, b, fill,   pi, x1, y1, x2, y2) {
            pi = atan2(0, -1)
            x1 = 90 + 80 * sin(2 * pi * a); y1 = 90 - 80 * cos(2 * pi * a)
            x2 = 90 + 80 * sin(2 * pi * b); y2 = 90 - 80 * cos(2 * pi * b)
            printf "<path d=\"M90 90 L%.2f %.2f A80 80 0 %d 1 %.2f %.2f Z\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>\n", \
                x1, y1, (b - a > 0.5) ? 1 : 0, x2, y2, fill, edge
        }
        { n++; label[n] = $1; color[n] = $2; d[n] = $3; t[n] = $4; done += $3; total += $4 }
        END {
            rest = "#1A1A1A"
            printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"520\" height=\"180\" viewBox=\"0 0 520 180\" font-family=\"-apple-system, Segoe UI, Helvetica, Arial, sans-serif\" font-size=\"13\">\n"
            at = 0
            for (i = 1; i <= n; i++) {
                if (d[i] == 0) continue
                f = d[i] / total
                if (f < 0.004) f = 0.004
                wedge(at, at + f, color[i])
                at += f
            }
            if (at < 1) wedge(at, 1, rest)
            label[n + 1] = "Not yet ported"; color[n + 1] = rest; d[n + 1] = total - done; t[n + 1] = total
            for (i = 1; i <= n + 1; i++) {
                y = 26 + (i - 1) * 26
                printf "<text x=\"262\" y=\"%d\" fill=\"%s\" text-anchor=\"end\">%s</text>\n", y, text, share(d[i], t[i])
                printf "<rect x=\"272\" y=\"%d\" width=\"14\" height=\"14\" rx=\"2\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>\n", y - 11, color[i], edge
                printf "<text x=\"294\" y=\"%d\" fill=\"%s\">%s</text>\n", y, text, label[i]
                printf "<text x=\"516\" y=\"%d\" fill=\"%s\" text-anchor=\"end\">%s of %s</text>\n", y, text, commas(d[i]), commas(t[i])
            }
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

# The progress block: a status line and the pie.
readme_block() {
    mkdir -p "$root/docs/progress"
    parts="C|#557D59|$c_done|$c_total
Elisp|#346A74|$l_done|$l_total
GNU ELPA|#AB5043|$gnu_done|$gnu_total
NonGNU ELPA|#BE925E|$nongnu_done|$nongnu_total
MELPA|#D6D6D3|$melpa_done|$melpa_total"
    pie_svg pie "#1A1A1A" "#1A1A1A" "$parts"
    pie_svg pie-dark "#D6D6D3" "#D6D6D3" "$parts"
    echo "**Progress:** early porting. Fortmacs runs as a bare-bones terminal editor; most of Emacs is still"
    echo "missing. See [PORTING.md](PORTING.md) for the roadmap and what works so far."
    echo
    echo "<picture>"
    echo "<source media=\"(prefers-color-scheme: dark)\" srcset=\"docs/progress/pie-dark.svg\">"
    echo "<img src=\"docs/progress/pie.svg\" width=\"520\" height=\"180\" alt=\"Ported: $(figure "$((done + p_done))" "$((total + p_total))" share) of Emacs's C functions and Elisp definitions and the packages in GNU ELPA, NonGNU ELPA and MELPA\">"
    echo "</picture>"
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
