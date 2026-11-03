# 2026-11-03: progress-two-columns

The progress chart (tools/progress.sh), as the owner reviewed it in renders:
two legend columns beside the pie, Internal (C functions, Elisp
definitions) on the left and Packages on the right, each with its own rule
and, below it, a bold "Complete" with the group's share and count and the
group's lines translated (Emacs's own counted exactly, the packages'
estimated). GNU ELPA and NonGNU ELPA are one row, "Non/GNU ELPA"; MELPA
takes the tan, and the part still to port is the white slice, listed under
the pie as "Not yet ported". Each column is as wide as its widest label
and count (text widths from measured glyph advances), so its rule starts
at its first character and ends at its last; the README's image size
follows the SVG's. Both pies have the same thin dark lines between slices.
