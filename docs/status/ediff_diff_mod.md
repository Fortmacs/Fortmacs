# ediff_diff_mod

- Module: ediff_diff_mod
- Upstream: vc/ediff-diff.el
- State: partial

## Ported

`ediff-diff-program`,
`ediff-diff3-program`,
`ediff-diff-options` (the actual options computed when the first session starts),
`ediff-ignore-case-option`,
`-option3`,
`ediff-diff3-options`,
`ediff-diff3-ok-lines-regexp`,
`ediff-actual-diff-options`,
`-diff3-options`,
`ediff-match-diff3-line`,
`ediff-diff-ok-lines-regexp`,
`ediff-match-diff-line`,
`ediff-setup-diff-regions`,
`ediff-setup-diff-regions3` (3-way comparison and merging with an ancestor, the ancestor's file in the middle),
`ediff-make-diff2-buffer`,
`ediff-prepare-error-list`,
`ediff-extract-diffs` (buffer C of 2-way merges by `ediff-default-variant`),
`ediff-extract-diffs3` (merging with an ancestor: prefer-A, prefer-B and the default variant),
`ediff-get-diff3-group`,
`ediff-convert-diffs-to-overlays` (`ediff-state-of-merge`),
`ediff-set-diff-overlays-in-one-buffer` (the state "=diff(X)" of 3-way jobs),
`ediff-exec-process` (LC_MESSAGES=C, the coding system for reading, `--binary`, "/:" unquoted),
`ediff-set-actual-diff-options`,
`ediff-toggle-ignore-case` (#c),
`ediff-custom-diff-program`,
`ediff-custom-diff-options`

## Deferred

fine differences (`ediff-setup-fine-diff-regions`, `ediff-make-fine-diffs`, `ediff-install-fine-diff-if-necessary`, `ediff-set-fine-diff-properties(-in-one-buffer)`, `ediff-set-fine-overlays-*`, `ediff-convert-fine-diffs-to-overlays`, `ediff-auto-refine`, `-limit`, `ediff-ignore-similar-regions`),
the word functions (`ediff-forward-word`, `ediff-wordify`, `ediff-copy-to-buffer`, `ediff-goto-word`, `ediff-whitespace`, `ediff-word-1` to `-4`),
word mode and the narrow bounds of region jobs,
`ediff-same-file-contents`,
`ediff-same-contents` and `ediff-cmp-program`/`-options` (directories),
`ediff-shell`,
the asynchronous custom diff (`ediff-process-filter`, `ediff-process-sentinel`),
decoding the error output

## Tests

test_ediff_diff_mod (ports ediff-diff-tests: both tests, with echo printing the arguments; own: the regions of diff and diff3)
