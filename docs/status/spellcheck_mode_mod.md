# spellcheck_mode_mod

- Module: spellcheck_mode_mod
- Upstream: textmodes/flyspell.el
- State: partial

## Ported

`flyspell-mode` (its keymap: M-TAB and C-. `auto_correct_flyspell_word`, C-; `auto_correct_previous_flyspell_word`, C-, `goto_next_flyspell_error`; the lighter " Fly/" and the dictionary's first two characters; the welcome message),
`flyspell-prog-mode` (`flyspell-generic-progmode-verify` as `is_flyspell_prog_text`, over `flyspell_prog_text_faces`),
`flyspell-word`,
`flyspell-region`,
`flyspell-buffer`,
`flyspell-goto-next-error`,
`flyspell-auto-correct-word`,
`flyspell-auto-correct-previous-word` (with `flyspell-auto-correct-previous-hook`),
`flyspell-check-previous-highlighted-word`,
`flyspell-delay-command`,
`flyspell-deplacement-command`,
`flyspell-pre-command-hook`,
`flyspell-post-command-hook`,
`flyspell-check-changes`;
checking after commands (`flyspell--mode-on`, `flyspell--mode-off`, `flyspell-check-pre-word-p`, `flyspell-check-word-p` with `sit-for` or the idle timer of `flyspell-delay-use-timer`, `flyspell-check-changed-word-p`, `flyspell-after-change-function`, the delayed and deplacement commands as the `flyspell_delayed` and `flyspell_deplacement` components of `command_type`);
the word cache, repeated words and their exceptions, duplicate misspellings (`flyspell-word-search-backward`, `-forward`), `flyspell-get-word`, `flyspell-tex-command-p`, `flyspell-math-tex-command-p` (AUCTeX's `texmathp` is never defined);
small regions (`flyspell-small-region`) and large ones through the speller's `list`/`-l` run (`flyspell-large-region`, `flyspell-external-point-words`, `flyspell-process-localwords`, `flyspell-check-region-doublons`);
the highlighting overlays (`make-flyspell-overlay`, `flyspell-highlight-incorrect-region`, `flyspell-highlight-duplicate-region`, `flyspell-unhighlight-at`, `flyspell-delete-region-overlays`, `flyspell-delete-all-overlays`, `flyspell-overlay-p`, `flyspell-properties-at-p`) and the faces `flyspell-incorrect` and `flyspell-duplicate`;
`flyspell-display-next-corrections`,
`flyspell-adjust-cursor-point`,
`flyspell-sort` and `flyspell-sort-corrections-alphabetically`,
`flyspell-notify-misspell`,
`flyspell-accept-buffer-local-defs` and the dash dictionaries,
`flyspell-kill-ispell-hook`;
the mode tests of Mail and SGML/HTML modes (`mail-mode-flyspell-verify`, `sgml-mode-flyspell-verify`) as the `flyspell_mode_test_function` of `major_mode_type` (upstream's `flyspell-mode-predicate` property);
the options, with `flyspell-large-region` as `flyspell_large_region_variable` (`%characters`, R6-39) and `flyspell-duplicate-distance` with `FLYSPELL_DUPLICATE_DISTANCE_UNLIMITED`

## Deferred

`flyspell-correct-word-before-point` (C-c $, unbound until then) and `flyspell-do-correct`, `flyspell-emacs-popup`: upstream shows the corrections with `x-popup-menu`, on a terminal a TTY menu (menu.c `tty_menu_show`), which is not ported;
`flyspell-correct-word`, `flyspell-mouse-map`, `flyspell-context-menu` and the mouse lighter map (no mouse);
`flyspell-incorrect-hook`, `flyspell-maybe-correct-transposition`, `flyspell-maybe-correct-doubling` (a hook kind taking the word's bounds and the speller's answer);
`flyspell-abbrev-p`, `flyspell-use-global-abbrev-table-p`, `flyspell-abbrev-table`, `flyspell-define-abbrev`, `flyspell-already-abbrevp`, `flyspell-change-abbrev` (changing an abbrev's expansion in place);
`flyspell-hack-local-variables-hook` (`hack-local-variables-hook` is not ported);
the `flyspell-incorrect` face of the misspelled word in the "Corrections:" message (messages carry no faces);
the coding system of the text sent to the speller in a large region (sent as the buffer holds it, UTF-8);
the mode tests of TeX, Texinfo, message and nXML modes (not ported);
`flyspell-use-meta-tab`'s `:set` (the keymap is made at startup, after the init file);
`flyspell-minibuffer-p` (unused upstream), the debug functions `flyspell-debug-signal-*` (called nowhere upstream), the obsolete `flyspell-buffers`, `flyspell-mode-on`, `flyspell-mode-off`, `turn-on-flyspell` and `turn-off-flyspell`

## Tests

test_spellcheck_mode_mod (own tests over a fake speller, test/flyspell-resources/fake-speller.sh: checking a word, repeated words, exceptions, duplicate misspellings, messages, small and large regions, local words, C-, , M-TAB cycling, the mode, the post-command hook, the lighter, the programming-mode test)
