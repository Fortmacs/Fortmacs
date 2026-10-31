# spelling.awk -- report British spellings (tools/american-spelling) in
# text, docstrings and comments.
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

# Usage: awk -f tools/spelling.awk tools/american-spelling FILE...
#
# Prints FILE:LINE: spelling: BRITISH -> AMERICAN for each word of the
# list in FILE outside the kept texts, and exits with status 1 if there
# is any.  Run by tools/check.sh.

BEGIN { FS = "\t" }

NR == FNR {
    if ($0 ~ /^#/ || $0 ~ /^[ \t]*$/) next
    if ($1 == "keep" && $2 ~ /^".+"$/) {
        keep[++nkeep] = substr($2, 2, length($2) - 2)
    } else if (NF == 2) {
        word[tolower($1)] = $2
        cand = cand (cand == "" ? "" : "|") tolower($1)
    } else {
        printf "%s:%d: not an entry: %s\n", FILENAME, FNR, $0 >"/dev/stderr"
        bad = 1
        exit 2
    }
    next
}

tolower($0) ~ cand {
    line = $0
    for (k = 1; k <= nkeep; k++) {
        while ((p = index(line, keep[k])) > 0)
            line = substr(line, 1, p - 1) substr(line, p + length(keep[k]))
    }
    rest = line
    while (match(rest, /[A-Za-z]+/)) {
        w = tolower(substr(rest, RSTART, RLENGTH))
        rest = substr(rest, RSTART + RLENGTH)
        if (w in word) {
            printf "%s:%d: spelling: %s -> %s\n", FILENAME, FNR, w, word[w]
            nfindings++
        }
    }
}

END {
    if (bad) exit 2
    exit nfindings > 0
}
