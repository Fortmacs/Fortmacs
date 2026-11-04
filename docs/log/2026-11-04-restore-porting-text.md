# 2026-11-04: restore-porting-text

- #269 (status files) carried a PORTING.md older than main's: merging it
  dropped the hazards `continuation-limit` and `truncated-literal` (from
  #264) and the R6-45 text of `lisp-slips` (the intrinsics checks
  char-loop, blank-skip, constructor-wrapper, duplicate-helper and
  "Lisp functions by hand"). tools/hazards.awk and tools/intrinsics.awk
  were not affected; only the text was lost.
- This branch puts back the two hazards, word for word as on main before
  #269 (16403756). The R6-45 text comes back with #274, which rewrites
  those paragraphs for the version operators.
- Checked: the rest of #269's PORTING.md diff is the status table and the
  intended "status file" wording.
