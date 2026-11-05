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
#   - Lines: the upstream source lines of the definitions counted done
#     (a C function from its header to its closing brace at column 0, a
#     Lisp definition to the next top-level form), against every line of
#     upstream src/*.[ch] (the same files) and lisp/**/*.el plus
#     the packages' lines, estimated as their count times the
#     `lines-per-package' of tools/package-totals (the mean of a sample of
#     MELPA packages), in one sum.
# It's an estimate: functions differ wildly in size, and macros and
# variables are not counted; the line counts include the definitions'
# docstrings and nothing between definitions.
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
    "$root"/src/*.f90 "$root"/src/sys/*.f90 | sort -u > "$tmp/lcited0"
# A citation by the bare file name ("project.el:project-current", as
# "Port of FILE:NAME" headers write it) counts for the file under lisp/
# with that base name (progmodes/project.el).  Files under cedet/ and
# obsolete/ that share a base name with another file are not meant
# (cedet/semantic/symref/grep.el beside progmodes/grep.el); a base name
# still ambiguous after that counts for none.
awk -F: 'NR == FNR {
        p = $1; b = p; sub(/.*\//, "", b)
        side = (p ~ /^(cedet|obsolete)\//)
        if (!(b in path) || (pside[b] && !side)) { path[b] = p; pside[b] = side; dup[b] = 0 }
        else if (path[b] != p && side == pside[b]) dup[b] = 1
        next
    }
    {
        f = $1; n = substr($0, length(f) + 2)
        if (index(f, "/") == 0 && (f in path) && !dup[f]) print path[f] ":" n
        else print
    }' "$tmp/lall" "$tmp/lcited0" | sort -u > "$tmp/lcited"
comm -12 "$tmp/lall" "$tmp/lcited" > "$tmp/ldone"

l_total=$(wc -l < "$tmp/lall")
l_done=$(wc -l < "$tmp/ldone")

total=$((c_total + l_total))
done=$((c_done + l_done))

# The upstream lines of the ported definitions (their whole text), and all
# the lines of the files they come from.
cut -d: -f1 "$tmp/done" | sort -u | while read -r b; do
    names=$(grep "^$b:" "$tmp/done" | cut -d: -f2 | tr '\n' ' ')
    awk -v names=" $names " '
        !infn && (match($0, /^DEFUN \("[^"]*", *[A-Za-z_][A-Za-z0-9_]*,/) || match($0, /^[A-Za-z_][A-Za-z0-9_]* \(/)) {
            n = $0
            if (n ~ /^DEFUN/) { sub(/^DEFUN \("[^"]*", */, "", n); sub(/,.*/, "", n) } else { sub(/ \(.*/, "", n) }
            if (index(names, " " n " ")) { infn = 1; start = NR }
        }
        infn && /^}/ { lines += NR - start + 1; infn = 0 }
        END { print lines + 0 }' "$up/src/$b"
done | awk '{ s += $1 } END { print s + 0 }' > "$tmp/c_lines"
cut -d: -f1 "$tmp/ldone" | sort -u | while read -r f; do
    names=$(grep "^$f:" "$tmp/ldone" | cut -d: -f2- | tr '\n' ' ')
    awk -v names=" $names " '
        /^\(/ {
            if (indef) { lines += NR - start; indef = 0 }
            if (match($0, /^\((defun|cl-defun|defmacro|define-minor-mode|define-derived-mode) [^ ()]+/)) {
                n = $0; sub(/^\([a-z-]+ /, "", n); sub(/[ ()].*/, "", n)
                if (index(names, " " n " ")) { indef = 1; start = NR }
            }
        }
        END { if (indef) lines += NR - start + 1; print lines + 0 }' "$up/lisp/$f"
done | awk '{ s += $1 } END { print s + 0 }' > "$tmp/l_lines"
lines_done=$(( $(cat "$tmp/c_lines") + $(cat "$tmp/l_lines") ))
c_all_lines=$(for f in "$up"/src/*.c "$up"/src/*.h; do
    case "$(basename "$f")" in w32*|ns*|mac*|msdos*|dosfns*|android*|haiku*) continue ;; esac
    cat "$f"
done | wc -l)
l_all_lines=$(find "$up/lisp" -name '*.el' -exec cat {} + | wc -l)
emacs_lines=$((c_all_lines + l_all_lines))

# Packages: totals per archive, and the ported ones ("ARCHIVE NAME" lines).
pkg_total() { awk -v k="$1" '$1 == k { print $2 }' "$root/tools/package-totals"; }
pkg_done() { grep -c "^$1 " "$root/tools/ported-packages" || true; }
gnu_total=$(pkg_total gnu-elpa); gnu_done=$(pkg_done gnu-elpa)
nongnu_total=$(pkg_total nongnu-elpa); nongnu_done=$(pkg_done nongnu-elpa)
melpa_total=$(pkg_total melpa); melpa_done=$(pkg_done melpa)
p_total=$((gnu_total + nongnu_total + melpa_total))
lines_per_package=$(awk '$1 == "lines-per-package" { print $2; exit }' "$root/tools/package-totals")
lines_total=$((emacs_lines + lines_per_package * p_total))
p_done=$((gnu_done + nongnu_done + melpa_done))

# The pie is an SVG image, docs/progress/NAME.svg (GitHub strips colors
# from HTML in Markdown): the whole pie is everything to port, each colored
# slice the ported part of one kind, to scale (a share too small to see
# still shows a sliver), and the black slice what is still to port; below
# the pie, "Not yet ported" with that slice's share and count.  Beside the
# pie, one legend column per group, each under its bold header
# ("Internal:": the C functions and Elisp definitions, on the left;
# "Packages:": the three archives, on the right), with each kind's name,
# share (right-aligned on one column) and count (left-aligned beside it).
# Below a rule each column ends with its own summary: "Complete" in bold
# with the group's share and count, then the lines translated (in the
# counts column, with no share: Emacs's own lines counted exactly, the
# packages' estimated, so "≈").  The colors are the logo's: purple and
# blue first, then red, tan and gray.  The labels are TEXT and the lines
# between slices EDGE: gray and cream for GitHub's light theme, cream and
# cream for its dark one (outlining the pie).  INNER is the color of the
# lines between slices (the same thin dark line in both pies, inside their
# outline).
# Arguments: NAME TEXT EDGE INNER and lines of LABEL|COLOR|DONE|TOTAL (a
# line with only a LABEL is a group header; the first group is Emacs's
# own, the second the packages).
pie_svg() {
    printf '%s\n' "$5" | awk -F'|' -v text="$2" -v edge="$3" -v inner="$4" \
        -v internal_lines_done="$lines_done" -v internal_lines_total="$emacs_lines" \
        -v package_lines_total="$((lines_per_package * p_total))" '
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
            return sprintf("%s%.2f%%", (r == p) ? "" : "≈", r)
        }
        # A wedge from A to B (fractions of a turn, clockwise from 12 o clock).
        function wedge(a, b, fill,   pi, x1, y1, x2, y2) {
            pi = atan2(0, -1)
            x1 = 90 + 80 * sin(2 * pi * a); y1 = 90 - 80 * cos(2 * pi * a)
            x2 = 90 + 80 * sin(2 * pi * b); y2 = 90 - 80 * cos(2 * pi * b)
            printf "<path d=\"M90 90 L%.2f %.2f A80 80 0 %d 1 %.2f %.2f Z\" fill=\"%s\" stroke=\"%s\" stroke-width=\"0.35\"/>\n", \
                x1, y1, (b - a > 0.5) ? 1 : 0, x2, y2, fill, inner
        }
        # A text at X, Y: ANCHOR (start or end), WEIGHT (bold or not), STRING.
        function put(x, y, anchor, weight, s) {
            printf "<text x=\"%d\" y=\"%d\" fill=\"%s\" text-anchor=\"%s\"%s>%s</text>\n", \
                x, y, text, anchor, weight ? " font-weight=\"600\"" : "", s
        }
        # One legend row in the column whose labels end at X.
        function row(x, y, name, fill, d, t) {
            put(x, y, "end", 0, name)
            printf "<rect x=\"%d\" y=\"%d\" width=\"14\" height=\"14\" rx=\"2\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>\n", \
                x + 10, y - 11, fill, edge
            put(x + 90, y, "end", 0, share(d, t))
            put(x + 102, y, "start", 0, commas(d) " of " commas(t))
        }
        # How wide S is in pixels at 13px: the advance widths of the
        # sans-serif font GitHub falls back to (measured in Chromium),
        # with bold (BOLD) about 2.5% wider; "≈" counts as "=".  Other
        # fonts differ by a pixel or two over a label.
        function set_width(chars, w,   i) {
            for (i = 1; i <= length(chars); i++) char_width[substr(chars, i, 1)] = w
        }
        function width(s, bold,   i, c, w) {
            if (!("a" in char_width)) {
                set_width("ijl", 3.1); set_width("I", 3.5); set_width(" !", 3.7); set_width(",.:", 3.8)
                set_width("'\'';", 3.9); set_width("t", 4.5); set_width("()/[]", 4.7); set_width("fr", 5.1)
                set_width("1", 5.3); set_width("-", 5.4); set_width("Tc", 6.8); set_width("s", 6.9)
                set_width("e", 7.0); set_width("kx", 7.1); set_width("oz", 7.2); set_width("Lavy", 7.3)
                set_width("7J", 7.4); set_width("=Fbdghnpqu", 7.7); set_width("E", 7.8); set_width("25", 7.9)
                set_width("+3", 8.0); set_width("689", 8.1); set_width("0Z", 8.2); set_width("PS", 8.3)
                set_width("4R", 8.4); set_width("B", 8.5); set_width("K", 8.7); set_width("Y", 8.8)
                set_width("X", 8.9); set_width("AV", 9.0); set_width("D", 9.4); set_width("C", 9.5)
                set_width("GHU", 9.7); set_width("N", 9.8); set_width("OQ", 9.9); set_width("w", 10.6)
                set_width("m", 11.4); set_width("M", 11.7); set_width("%W", 12.8)
            }
            gsub(/≈/, "=", s)
            w = 0
            for (i = 1; i <= length(s); i++) {
                c = substr(s, i, 1)
                w += (c in char_width) ? char_width[c] : 7.7
            }
            return bold ? w * 1.025 : w
        }
        function widest(a, b) { return (a > b) ? a : b }
        NF == 1 || $2 == "" { g++; header[g] = $1; next }
        {
            k[g]++; label[g, k[g]] = $1; color[g, k[g]] = $2
            d[g, k[g]] = $3 + 0; t[g, k[g]] = $4 + 0
            gd[g] += $3; gt[g] += $4; done += $3; total += $4
            if (k[g] > rows) rows = k[g]
        }
        END {
            rest = "#D6D6D3"
            ld[1] = internal_lines_done; lt[1] = commas(internal_lines_total)
            ld[2] = 0; lt[2] = "≈" commas(package_lines_total)
            # Each column is as wide as its widest label and count: it
            # starts where its widest label starts and ends where its widest
            # count ends, and its rule spans exactly that.
            for (j = 1; j <= g; j++) {
                lw = widest(widest(width(header[j], 1), width("Complete", 1)), width("Lines translated", 0))
                cw = widest(width(commas(gd[j]) " of " commas(gt[j]), 0), width(commas(ld[j]) " of " lt[j], 0))
                for (i = 1; i <= k[j]; i++) {
                    lw = widest(lw, width(label[j, i], 0))
                    cw = widest(cw, width(commas(d[j, i]) " of " commas(t[j, i]), 0))
                }
                start[j] = (j == 1) ? 200 : finish[j - 1] + 44
                xs[j] = int(start[j] + lw + 0.5)
                finish[j] = int(xs[j] + 102 + cw + 0.5)
            }
            svg_width = finish[g] + 8
            rule = 26 + rows * 26 + 14
            height = rule + 22 + 26 + 18
            if (height < 236) height = 236
            printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" font-family=\"-apple-system, Segoe UI, Helvetica, Arial, sans-serif\" font-size=\"13\">\n", svg_width, height, svg_width, height
            at = 0
            for (j = 1; j <= g; j++) for (i = 1; i <= k[j]; i++) {
                if (d[j, i] == 0) continue
                f = d[j, i] / total
                if (f < 0.004) f = 0.004
                wedge(at, at + f, color[j, i])
                at += f
            }
            if (at < 1) wedge(at, 1, rest)
            printf "<circle cx=\"90\" cy=\"90\" r=\"80\" fill=\"none\" stroke=\"%s\" stroke-width=\"1\"/>\n", edge
            # Below the pie: what is still to port.
            printf "<rect x=\"36\" y=\"%d\" width=\"14\" height=\"14\" rx=\"2\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>\n", 200 - 11, rest, edge
            put(58, 200, "start", 0, "Not yet ported")
            put(90, 222, "middle", 0, share(total - done, total) "  " commas(total - done) " of " commas(total))
            for (j = 1; j <= g; j++) {
                x = xs[j]
                put(x, 26, "end", 1, header[j])
                for (i = 1; i <= k[j]; i++) row(x, 26 + i * 26, label[j, i], color[j, i], d[j, i], t[j, i])
                printf "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1\"/>\n", start[j], rule, finish[j], rule, text
                y = rule + 22
                put(x, y, "end", 1, "Complete")
                put(x + 90, y, "end", 1, share(gd[j], gt[j]))
                put(x + 102, y, "start", 0, commas(gd[j]) " of " commas(gt[j]))
                y += 26
                put(x, y, "end", 0, "Lines translated")
                put(x + 102, y, "start", 0, commas(ld[j]) " of " lt[j])
            }
            printf "</svg>\n"
        }' > "$root/docs/progress/$1.svg"
}

# "DONE<br>of TOTAL" with thousands separators, and the share rounded to
# a hundredth of a percent ("≈" unless exact; never 0 or 100 unless it
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
            printf "%s%.2f%%", (r == p) ? "" : "≈", r
        }'
}

# The progress block: a status line and the pie.
readme_block() {
    mkdir -p "$root/docs/progress"
    parts="Internal:|||
C functions|#608A83|$c_done|$c_total
Elisp definitions|#578497|$l_done|$l_total
Packages:|||
Non/GNU ELPA|#976057|$((gnu_done + nongnu_done))|$((gnu_total + nongnu_total))
MELPA|#B39574|$melpa_done|$melpa_total"
    pie_svg pie "#1A1A1A" "#1A1A1A" "#1A1A1A" "$parts"
    pie_svg pie-dark "#D6D6D3" "#D6D6D3" "#1A1A1A" "$parts"
    svg_size=$(sed -n "s/.*<svg[^>]* width=\"\([0-9]*\)\" height=\"\([0-9]*\)\".*/width=\"\1\" height=\"\2\"/p" "$root/docs/progress/pie.svg")
    echo "**Progress:** early porting. Fortmacs runs as a bare-bones terminal editor; most of Emacs is still"
    echo "missing. See [PORTING.md](PORTING.md) for the roadmap and what works so far."
    echo
    echo "<picture>"
    echo "<source media=\"(prefers-color-scheme: dark)\" srcset=\"docs/progress/pie-dark.svg\">"
    echo "<img src=\"docs/progress/pie.svg\" $svg_size alt=\"Ported: $(figure "$((done + p_done))" "$((total + p_total))" share) of Emacs's C functions and Elisp definitions and the packages in GNU ELPA, NonGNU ELPA and MELPA; about $(figure "$lines_done" 1 count | sed 's/<br>of 1$//') of the about $(figure "$lines_total" 1 count | sed 's/<br>of 1$//') lines of Emacs and its packages translated\">"
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
        echo "Lines:            $lines_done of about $lines_total translated (Emacs and its packages)"
        echo "Packages:         $p_done of $p_total (GNU ELPA $gnu_done of $gnu_total," \
             "NonGNU ELPA $nongnu_done of $nongnu_total, MELPA $melpa_done of $melpa_total)"
        ;;
esac
