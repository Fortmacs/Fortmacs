# diff_mod

- Module: diff_mod
- Upstream: vc/diff.el
- State: done

## Ported

`diff` (M-x diff: new file, then old, defaults from the visited file and its newest backup, C-u for switches),
`diff-no-select` (into *Diff* or a given buffer; asynchronous through make-process, or with NO-ASYNC through call-process; the command line; "Diff finished" with `(no differences)`/`(diff error)`; `revert-buffer` runs it again),
`diff-sentinel`,
`diff-switches`,
`diff-command`,
`diff-use-labels` (probing --label),
`diff-entire-buffers`,
`diff-file-local-copy` (buffers through temporary files),
`diff-backup`,
`diff-latest-backup-file`,
`diff-buffer-with-file`,
`diff-buffers` (with `other-buffer` VISIBLE-OK)

## Deferred

remote files,
`with-file-modes`,
the buffer coding system for temporary files,
`diff-switches` as a list,
`diff-setup-whitespace`

## Tests

test_diff_mode_mod (own: M-x diff of two files, Dired's =)
