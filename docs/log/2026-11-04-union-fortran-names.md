# 2026-11-04: union-fortran-names

Every PR that adds renames appends them at the end of tools/fortran-names,
so two open PRs always conflicted there although their entries are
independent. .gitattributes now marks the file `merge=union`: git keeps both
sides' lines in such a conflict. A duplicate entry that results (both
branches adding the same line) is now an error in tools/check.sh.
