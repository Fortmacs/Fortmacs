# simple_mod

- Module: simple_mod, simple_view_mod (submodule)
- Upstream: simple.el
- State: partial

## Ported

`compose-mail` (C-x m) and `compose-mail-other-window` (C-x 4 m) with `mail-user-agent` (in the submodule simple_mail_mod),
words (`backward-word` M-b, `kill-word` M-d, `backward-kill-word` M-DEL, `mark-word` M-@ with extending),
`execute-extended-command` (M-x, with read-extended-command and the key binding suggestion),
undo (`undo`, `undo-only`, `undo-redo`, `undo-more`, `undo-start`, `primitive-undo`, `undo-equiv-table`, `pending-undo-list`, `undo-no-redo`),
the mark (`mark`, `set-mark`, `push-mark` with the buffer's mark ring, `pop-mark`, `activate-mark`, `deactivate-mark`, `region-active-p`, `use-region-p`, `set-mark-command`, `push-mark-command`, `pop-to-mark-command`, `exchange-point-and-mark`, `mark-whole-buffer`),
the kill ring (`kill-new`, `kill-append`, `current-kill`, `kill-region` with read-only copying, `copy-region-as-kill`, `kill-ring-save`, `kill-line`, `kill-whole-line`, `forward-visible-line`, `paste`, `yank-pop`, `rotate-yank-pointer`, subr.el `insert-for-yank`),
`delete-active-region` and CUT in the deletion commands,
keyboard-quit deactivating the mark,
calling `kmacro-keyboard-quit` and ending a macro definition,
`newline`,
`open-line`,
`delete-backward-char`,
`delete-forward-char`,
`keyboard-quit`,
`quoted-insert`,
`universal-argument` (+ `-more`, `-map`, `--mode`),
`digit-argument`,
`negative-argument`,
`prefix-command-preserve-state`,
`next-line`,
`previous-line`,
`line-move` (`line-move-visual` by screen lines with temporary-goal-column, `line-move-1` by buffer lines),
`line-move-finish`,
`goal-column`,
`line-move-visual`;
minibuffer history (`goto-history-element`, `previous-history-element`, `next-history-element`, one default);
transposition (`transpose-subr`, `transpose-subr-1`, `transpose-chars` C-t, `transpose-words` M-t, `transpose-lines` C-x C-t),
whitespace (`delete-horizontal-space` M-\, `delete-all-space`, `just-one-space`, `cycle-spacing` M-SPC with `cycle-spacing-actions`, `fixup-whitespace`, `delete-blank-lines` C-x C-o, `delete-indentation`/`join-line` M-^),
`fill-prefix`;
`execute-extended-command` honors disabled commands;
`back-to-indentation` (M-m),
`beginning-of-buffer` (M-<, C-<home>),
`end-of-buffer` (M->, C-<end>),
`count-lines`,
fns.c `line-number-at-pos` (here, as fns_mod does not see buffer text);
in the submodule simple_fill_mod: `do-auto-fill`,
`internal-auto-fill`,
`auto-fill-mode` (a command; minor modes come next),
`turn-on-auto-fill`,
`turn-off-auto-fill`,
`normal-auto-fill-function` (buffer-local),
`comment-line-break-function`,
`auto-fill-inhibit-regexp`,
`inhibit-auto-fill`,
`default-indent-new-line` (M-j, C-M-j),
`set-fill-column` (C-x f);
in the submodule simple_modes_mod: `fundamental-mode`,
`special-mode` with special-mode-map,
the minor modes `transient-mark-mode` and `indent-tabs-mode`;
`auto-fill-mode` is a buffer-local minor mode (easy_mmode_mod);
`read-only-mode` (C-x C-q; a minor mode on `buffer-read-only`, entering and leaving View mode under `view-read-only`; `read-only-mode-hook`),
`what-line` (in simple_view_mod, with its message returned as `what_line_text`),
`messages-buffer-mode` and `messages-buffer` (simple_modes_mod);
invisible text in line motion: `line-move-ignore-invisible` in `line-move-1`,
`line-move-to-column`,
`line-move-finish`,
`move-end-of-line`,
`move-beginning-of-line`,
`forward-visible-line`,
`region-extract-function` and `region-insert-function` (procedure pointers with REGION_EXTRACT_ methods; used by `kill-region`, `copy-region-as-kill`, the deleting commands with an active region and `copy-to-register`),
`region-bounds`,
`region-noncontiguous-p`,
`deactivate-mark-hook` (also run when the command loop deactivates the mark),
a kill ring of strings with text properties (`kill-new`, `current-kill`, `kill-append` for text or propertized strings),
`insert-for-yank` with `yank-handler` properties (FUNCTION, PARAM; `define_paste_handler_function` names FUNCTIONs),
`yank-undo-function` in `yank-pop`

## Deferred

`compose-mail-other-frame` (C-x 5 m, frames),
`message-user-agent` (the default of `mail-user-agent`, null until message.el, step 52c) with `compose-mail-user-agent-warnings`,
the OTHER-HEADERS,
YANK-ACTION,
SEND-ACTIONS and RETURN-ACTION of `compose-mail` (for Rmail and Reporter),
`mail-encode-mml`,
`read-mail-command`,
`transpose-sexps` (needs expressions),
function actions in `cycle-spacing-actions`,
`atomic-change-group` around transpositions and cycle-spacing,
`delete-trailing-whitespace`,
undo in a region (`undo-make-selective-list`, `undo-in-region`),
the global mark ring,
shift-selection,
temporary Transient Mark mode (`(only . OLD)`, `lambda`),
`yank-from-kill-ring`/`read-from-kill-ring` (M-y after other commands is an error, as before Emacs 28: needs completion metadata, history positions and display properties),
interprogram (clipboard) cut and paste,
paste handlers,
electric indent,
hard newlines,
prefix echo,
the timer of the M-x key binding suggestion (shown at once, only when the echo area is empty, until the next command),
overwrite mode in `quoted-insert`,
`move-beginning/end-of-line`,
`set-goal-column`,
`next-line-add-newlines`,
`track-eol`,
vscroll,
invisible text in line motion,
the M-x shorter-name suggestion,
`read-extended-command-predicate`,
M-X;
inserting a string's text properties into a buffer (and so `yank-excluded-properties`, `yank-handled-properties`),
a paste handler's UNDO and COMMAND,
`yank-transform-functions`

## Tests

test_bindings_mod ,
test_simple_mod (ports simple-tests: kill-whole-line-after-other-kill, -buffer-boundaries, -line-boundaries, simple-delete-indentation-no-region, -inactive-region, -blank-line, -boundaries, -region, -prefix; own tests for the mark, the kill ring, transposition and whitespace) ,
test_undo_mod,
test_derived_mod,
test_easy_mmode_mod
