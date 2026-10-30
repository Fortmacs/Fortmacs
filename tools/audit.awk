# audit.awk -- check the project rules of CLAUDE.md and PORTING.md that
# no other tool checks, in Fortran source files.
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

# Usage: awk -f tools/audit.awk FILE.f90...
#
# Prints FILE:LINE: RULE: what is wrong, for each finding, and exits
# with status 1 if there is any.  The rules (PORTING.md, "Audit"):
#
#   header      the header of PORTING.md's template: "! FILE -- summary."
#               (FILE the file's name), for a ported file a provenance
#               line ("Ported from GNU Emacs 31.1 ...", for a test
#               "Ports ..."), "!", the upstream copyright lines (only
#               for a ported file), "! Copyright (C) 2026 The Fortmacs
#               authors", "!", "! This file is part of Fortmacs.", "!",
#               the GPLv3-or-later notice, then a blank line.

function finding(line, rule, msg) {
    printf "%s:%d: %s: %s\n", file, line, rule, msg
    nfindings++
}

# The GPLv3-or-later notice, line by line, after "! This file is part
# of Fortmacs." and "!".
BEGIN {
    n = 0
    gpl[++n] = "! Fortmacs is free software: you can redistribute it and/or modify"
    gpl[++n] = "! it under the terms of the GNU General Public License as published by"
    gpl[++n] = "! the Free Software Foundation, either version 3 of the License, or"
    gpl[++n] = "! (at your option) any later version."
    gpl[++n] = "!"
    gpl[++n] = "! Fortmacs is distributed in the hope that it will be useful,"
    gpl[++n] = "! but WITHOUT ANY WARRANTY; without even the implied warranty of"
    gpl[++n] = "! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    gpl[++n] = "! GNU General Public License for more details."
    gpl[++n] = "!"
    gpl[++n] = "! You should have received a copy of the GNU General Public License"
    gpl[++n] = "! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>."
    ngpl = n
}

FNR == 1 {
    if (NR > 1) check_header()
    nhead = 0
    file = FILENAME
    name = file
    sub(/.*\//, "", name)
}

# The header: the lines up to the first blank or non-comment line.
FNR == nhead + 1 && /^!/ && !/^!>/ { head[++nhead] = $0 }

END {
    if (NR > 0) check_header()
    exit nfindings > 0
}

# Check the header lines head[1..nhead] of the file just read.
function check_header(    i, ported, upstream, j) {
    i = 1
    if (nhead == 0 || index(head[1], "! " name " -- ") != 1) {
        finding(1, "header", "the first line is not \"! " name " -- summary.\"")
        return
    }
    # The summary, which may wrap, ends with a full stop.
    while (i < nhead && head[i] !~ /\.$/) i++
    i++
    ported = 0
    if (head[i] ~ /^! (Ported from GNU Emacs 31\.1 |Ports )/) {
        ported = 1
        while (i < nhead && head[i] != "!") i++
    } else if (head[i] != "!") {
        finding(i, "header", "expected a provenance line (\"Ported from GNU Emacs 31.1 ...\" or \"Ports ...\") or \"!\": " head[i])
        return
    }
    if (head[i] != "!") {
        finding(i, "header", "expected \"!\" after the provenance: " head[i])
        return
    }
    i++
    # The upstream copyright lines (with their continuation lines).
    upstream = 0
    while (i <= nhead && head[i] != "! Copyright (C) 2026 The Fortmacs authors" && head[i] != "!") {
        if (head[i] ~ /^! Copyright \(C\) /) upstream = 1
        i++
    }
    if (head[i] != "! Copyright (C) 2026 The Fortmacs authors") {
        finding(i, "header", "expected \"! Copyright (C) 2026 The Fortmacs authors\": " head[i])
        return
    }
    if (ported && !upstream) finding(i, "header", "a ported file without the upstream copyright notice")
    if (!ported && upstream) finding(i, "header", "an upstream copyright notice without a provenance line")
    i++
    if (head[i] != "!" || head[i + 1] != "! This file is part of Fortmacs." || head[i + 2] != "!") {
        finding(i, "header", "expected \"!\", \"! This file is part of Fortmacs.\", \"!\"")
        return
    }
    i += 3
    for (j = 1; j <= ngpl; j++) {
        if (head[i] != gpl[j]) {
            finding(i, "header", "the GPLv3-or-later notice differs: " head[i])
            return
        }
        i++
    }
    if (i <= nhead) finding(i, "header", "the header goes on after the notice: " head[i])
}
