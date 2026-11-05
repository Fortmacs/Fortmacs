# directory_editor_mod

- Module: directory_editor_mod
- Upstream: dired.c
- State: partial

## Ported

`file-name-completion`,
`file-name-all-completions` (with `completion-ignored-extensions`),
`directory-files` (FULL, NOSORT in reverse read order, COUNT a natural number; sorted as `string-lessp`);
sysdep: `fortmacs_directory_entries` (opendir/readdir);
`system-users`,
`system-groups` (from the passwd and group databases),
the owner and group names of `file-attributes`

## Deferred

MATCH (needs regexps),
`directory-files-and-attributes`,
`completion-regexp-list` in file name completion

## Tests

test_directory_editor_mod (ports dired-test-directory-files without MATCH)
