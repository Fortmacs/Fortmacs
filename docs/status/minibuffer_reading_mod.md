# minibuffer_reading_mod

- Module: minibuffer_reading_mod
- Upstream: minibuf.c
- State: partial

## Ported

`read-hide-char`,
HIST t (no history) for `read-from-minibuffer`;
the completion primitives `try-completion`,
`all-completions`,
`test-completion` (lists, tables of (string, value) entries, the obarray, completion functions; `completion-ignore-case`),
the buffer of minibuffer-selected-window,
`read-minibuffer-restore-windows` (window configuration restored on exit),
`minibuffer-scroll-window` set on entry,
`read_minibuf` (prompt as a read-only field with minibuffer-prompt-properties (read-only t), minibuffer window selected, recursive edit, history and history position, a list of defaults, initial input with a position, local variables killed as set_minibuffer_mode does without minibuffer-mode, canceling the active minibuffer from another window),
`read-from-minibuffer`,
`read-string`,
`get_minibuffer`,
`is_minibuffer`,
`minibuffer-prompt-end`,
`minibuffer-contents-no-properties`,
`abort-minibuffers` (asking before aborting outer levels),
`minibuffer-innermost-command-loop-p`,
`innermost-minibuffer-p`,
`minibuffer-local-map`,
`enable-recursive-minibuffers`,
`history-add-new-input`,
`completion-ignore-case` (a setting),
minibuffer-with-setup-hook (one function);
`read-from-minibuffer` binds `inhibit-read-only` (restored on exit);
the completion tables and the completion-at-point data are in minibuffer_reading_tables_mod (re-exported),
with a table's completion category;
`complete-with-action`,
`completion-table-subvert`,
`completion-table-with-predicate`;
`completion-table-with-quoting` (its table function with the completion--twq-try and completion--twq-all requoting; no metadata);
`minibuffer-depth`,
`minibuffer-prompt` (`minibuf_prompt` per level),
`minibuffer-exit-hook` (`execute_exit_minibuf_hook`, run safely with the minibuffer current as it exits),
`completion-regexp-list` (`match_regular_expressions`, in try-completion, all-completions and test-completion),
`assoc-string`,
`is_minibuffer`'s LIVE (`is_live_minibuffer`, `is_minibuffer_at_depth`, `nth_minibuffer`),
`inhibit-interaction` (`barf_if_interaction_inhibited`, in keyboard_mod),
`read-variable` (simple_mod: completion over the user options),
`set_initial_minibuffer_mode` and `set_minibuffer_mode` (local variables killed, as when minibuffer-mode is not defined),
`init_minibuf_once`,
`read_minibuf_unwind`,
`minibuffer_unwind` and `choose_minibuf_frame` (one frame);
`minibuf_level` lives in keyboard_mod (re-exported)

## Deferred

hash tables and other obarrays as collections,
`read-minibuffer`/`eval-minibuffer`,
minibuffer-only frames,
input methods,
minibuffer-mode,
the minibuffer-prompt face ;
after step 42c: `minibuffer-contents` with text properties (buffer-substring copies them since step 42f),
several frames (`set-minibuffer-window`, `minibuf_follows_frame`, `minibuf_moves_frame_when_opened`, `minibuf_stays_put`, `move_minibuffers_onto_frame`, `zip_minibuffer_stacks`, `minibuffer_ent_greater`: one frame until make-frame and the GUI);
Not applicable: `read-function` (compiled out upstream: `#ifdef NOTDEF`),
`string_to_object` (the Lisp reader: read-minibuffer, eval-minibuffer),
`read_minibuf_noninteractive` (no batch mode),
`init_minibuf_once_for_pdumper` (the dumper),
`syms_of_minibuf` (symbol definitions),
the scanner's artifacts `Copyright`,
`Return`;
waits for the GUI: minibuffer-only frames

## Tests

test_minibuffer_reading_mod (ports the minibuf-tests.el completion tests for lists and tables, test-try-completion-ignore-case);
ports the minibuf-tests.el completion-regexp tests for lists and tables and test-inhibit-interaction;
own: minibuf-depth-and-prompt,
assoc-string,
read-variable
