# save_history_mod

- Module: save_history_mod
- Upstream: savehist.el
- State: partial

## Ported

`savehist-mode` (global; loads the file the first time, keeping and merging the current histories with `savehist--merge`; turned off again if the file can't be read),
`savehist-save` (the minibuffer histories with `savehist-minibuffer-history-variables`, empty ones left out, elements that can't be written left out, `savehist-additional-variables` with MAX-SIZE as `savehist_variable_type` records, `savehist-ignored-variables`, `savehist-save-hook`, reloading a file another session changed, no write when autosaving unchanged text, the directory made, `savehist-file-modes`),
`savehist-autosave` on a timer (`savehist-autosave-interval`) and `kill-emacs-hook`,
`savehist-minibuffer-hook` on `minibuffer-setup-hook`,
`savehist-printable`,
`savehist-install`,
`savehist-uninstall`,
`savehist-coding-system`,
`savehist-loaded`;
keyboard_mod's `kill-emacs-hook` (emacs.c) run when Fortmacs exits,
minibuffer_reading_mod's `minibuffer-setup-hook`

## Deferred

the text properties of saved strings,
the warning when the file can't be written (warnings.el),
the obsolete `savehist-trim-history`

## Tests

test_save_history_mod (ports savehist-tests.el)
