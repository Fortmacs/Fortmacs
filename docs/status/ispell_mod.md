# ispell_mod

- Module: ispell_mod
- Upstream: textmodes/ispell.el
- State: partial

## Ported

`ispell-word` (M-$; region, CONTINUE, check-only, quiet),
`ispell-region`,
`ispell-buffer`,
`ispell`,
`ispell-continue`,
`ispell-comments-and-strings`,
`ispell-comment-or-string-at-point`,
`ispell-change-dictionary` (local and global, completion over `ispell-valid-dictionary-list`),
`ispell-kill-ispell`,
`ispell-pdict-save`,
`ispell-check-version` (also as a command);
the process (`ispell-start-process`, `ispell-init-process`, `ispell-send-string`, `ispell-accept-output`, `ispell-filter`, `ispell-parse-output` as `ispell_output_type`, `ispell--run-on-word`, `ispell-correct-p`);
`ispell-get-word`;
`ispell-command-loop` with the *Choices* buffer shown above the selected window (`ispell-display-buffer`, `ispell-show-choices`) and all its keys (digits, SPC, i, a, A, r, R, ?, x, X, q, l, u, m, C-l, C-r, C-u abbrev saving, C-z),
`ispell-help` (minibuffer and buffer),
`ispell-lookup-words` (look or grep);
region skipping (`ispell-skip-region-alist` as `ispell_skip_regions`, the TeX and HTML ones, tib, comments, the file name and URL regexp);
buffer-local definitions (`ispell-accept-buffer-local-defs`, `ispell-buffer-local-dict`, `-words`, `-parsing`, `ispell-add-per-file-word-list`, session local words);
the dictionaries (`ispell-dictionary-base-alist` as `ispell_base_dictionaries`, `ispell-set-spellchecker-params`, the decoded character sets, Aspell's `ispell-find-aspell-dictionaries`, `ispell-aspell-find-dictionary`, `ispell-aspell-add-aliases`);
`ispell-program-name` detection;
highlighting with an overlay

## Deferred

Hunspell's and Enchant's dictionary discovery,
`ispell-base-dicts-override-alist`,
the synchronous mode,
`ispell-buffer-with-debug`,
`ispell-complete-word` (+ interior-frag),
`ispell-completion-at-point`,
`ispell-minor-mode`,
`ispell-message` and the mail skipping,
the menu,
framepop,
highlighting without colors and lazy highlighting,
the *Choices* mode line (no `mode-line-format`),
electric help,
horizontal scrolling (`truncate-lines`)

## Tests

test_ispell_mod (upstream's fake speller)
