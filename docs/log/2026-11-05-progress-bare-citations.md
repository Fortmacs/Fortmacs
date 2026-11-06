# 2026-11-05: progress-bare-citations

- Owner: the progress bar had not moved for days. tools/progress.sh counted a Lisp definition as ported only when cited with its path under lisp/ ("progmodes/project.el:project-current"), but "Port of FILE:NAME" headers cite the bare file name ("project.el:project-current"), so everything ported from lisp/'s subdirectories (progmodes, vc, textmodes, mail, calendar, international, emacs-lisp, ...) went uncounted: project.el, Ediff, flyspell, xref, etags, smerge and more.
- progress.sh now resolves a bare file name to its path under lisp/; files under cedet/ and obsolete/ that share a base name with another file are not meant (cedet/semantic/symref/grep.el beside progmodes/grep.el), and a name still ambiguous counts for none.
- Before: Lisp definitions 2135 of 48317, lines 158,860, overall ~6.66%. After: 4101, 211,306, ~10.12%. The C count is unchanged (C citations already use bare names, as src/ has no subdirectories).
- The Progress workflow updates the README block and pie charts on main after the merge.
- Owner: the progress block shows the time on the project, from the first commit to the last (years, months, weeks, days, hours, minutes; units not reached yet, or zero, are left out). The Progress workflow's checkout fetches the whole history for it. Now: 2 weeks, 1 day, 12 hours, 33 minutes.
