# view_mod

- Module: view_mod
- Upstream: view.el
- State: partial

## Ported

`view-mode` (the minor mode, " View" in the mode line, `view-mode-map`, `view-mode-hook`, leaving on a major mode change),
`view-mode-enter` (EXIT-ACTION),
`view-mode-exit`,
`view-file`,
`view-file-other-window`,
`view-buffer`,
`view-buffer-other-window`,
`kill-buffer-if-not-modified`,
the View- commands (named view_*: `View-quit` q, `View-quit-all` Q, `View-kill-and-leave` C, `View-leave` c, `View-exit` e, `View-exit-and-edit` E, `View-scroll-page-forward` SPC, `-backward` DEL and S-SPC, `-set-page-size` z and w, `View-scroll-half-page-forward` d, `-backward` u, `View-scroll-line-forward` RET and y, `-backward`, `View-goto-percent` %, `View-goto-line` g, `View-back-to-mark` x, `View-scroll-to-buffer-end`, `View-beginning-of-buffer` <, `View-end-of-buffer` >, `View-search-regexp-forward` / and s, `-backward` \ and r, `View-search-last-regexp-forward` n, `-backward` p, the ! and @ prefixes, `View-revert-buffer-scroll-page-forward` excepted),
`view-read-only`,
`view-scroll-auto-exit`,
`view-exits-all-viewing-windows`,
`view-inhibit-help-message`,
`view-no-disable-on-exit`,
the entry message;
`view-file` on a directory and on a file with a special mode

## Deferred

match highlighting (overlays),
F and `view-try-extend-at-buffer-end` (reverting),
H,
h and ? (describe-mode),
the frame commands,
QUIT-RESTORE,
`view-mode-disable`

## Tests

test_view_mod (own: upstream has no view-tests.el)
