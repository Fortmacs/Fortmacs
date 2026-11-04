# executable_mod

- Module: executable_mod
- Upstream: progmodes/executable.el (+ files.el `executable-find`)
- State: partial

## Ported

`executable-find`,
`executable-set-magic` (the command and the function; `executable-insert`, `executable-query`, `executable-magicless-file-regexp`, `executable-prefix`, `executable-prefix-env`),
`executable-chmod` and `executable-make-buffer-file-executable-if-script-p` (for `after-save-hook`),
`executable-chmod-function`,
`executable-self-display` (obsolete),
`executable-font-lock-keywords`

## Deferred

`executable-interpret` (it needs `executable-error-regexp-alist`'s rule specifications in `compilation-error-regexp-alist`),
`executable-command-find-posix-p` (`data-directory`),
remote `executable-find`

## Tests

test_sh_script_mod (upstream has no executable tests)
