# Porting Fortmacs

This file is the single source of truth for what has been ported, what is
next, and how each port is done. Update it in the same PR as the code.

Reference source: GNU Emacs tag `emacs-31.1`
(commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`). Fetch it with
`tools/fetch-upstream.sh`; it lands in the git-ignored `upstream/emacs/`.

## Roadmap

### Phase 0 — Scaffold
fpm layout, `src/sys/`, CI on x86-64 and aarch64, upstream fetch script,
this file.

### Phase 1 — Data structures
1. `error_fm`, `handle_fm` — status/error type and the generational handle
   registry. Foundations with no single upstream file.
2. `character_fm` — UTF-8 encoding and char↔byte conversion
   (`character.c`, `character.h`).
3. `buffer_fm` + `insdel_fm` — gap buffer, insert/delete, create/kill,
   current buffer (`buffer.c`, `buffer.h`, `insdel.c`); plus the
   char↔byte conversion half of `marker_fm` (`marker.c`), which insdel needs.
4. `marker_fm` — marker objects and their adjustment on insert/delete
   (`marker.c`); `kill-buffer`/`erase-buffer` completed in a submodule of
   buffer_fm (which can use marker_fm/insdel_fm without a module cycle).
   Narrowing and `goto-char` come with `editfns_fm` (`editfns.c`, done).
5. Value model — symbols and the minimal value type that text properties,
   keymaps and buffer-local variables need (see Open questions).
6. `intervals_fm` + `textprop_fm` — interval tree and text properties
   (`intervals.c`, `textprop.c`).

### Phase 2 — Command loop and keymaps
1. `src/sys/` termios raw mode — done.
2. `keymap_fm` (`keymap.c`) — done (core; see the status table).
3. `keyboard_fm` (`keyboard.c`) + `callint_fm` (`callint.c`) — event
   reading, `read-key-sequence`, command registry, command loop — done
   (core). Still needed for a usable loop: xterm escape-sequence maps
   (`term/xterm.el` input-decode-map) and echo-area messages.
4. Typing works: `self-insert-command` (`cmds_fm`), basic simple.el
   commands (`simple_fm`), the default global bindings (`bindings_fm`),
   xterm escape sequences (`term_xterm_fm`) — done (core).
5. `app/main.f90` editor loop on a raw tty — needs Phase 3 output to be
   useful.

### Phase 3 — Terminal display
1. `terminfo_fm` — terminfo database reader and `tparm` (pure Fortran) — done.
2. Minimal `term.c` / `cm.c` (cursor motion, clearing, insert/delete
   lines, colors and attributes) on top of it — done.
3. Minimal `dispnew.c` (frame matrices, row update) — done; `xdisp.c` /
   `window.c` subset (one window, mode line, echo area) — done. The
   `fortmacs` binary now runs as an editor on a terminal.
4. File visiting and saving (`fileio.c` subset, `find-file`,
   `save-buffer`), in two steps:
   - a. Files without the minibuffer: visiting files named on the command
     line (`+LINE` too), `C-x C-s` with `FILE~` backups, `C-x s`, and
     `C-x C-c` asking about unsaved buffers — done. Until 4b, questions
     upstream asks with `yes-or-no-p` use `y-or-n-p`, which reads a
     single key (upstream's `y-or-n-p-use-read-key` path).
   - b. A minimal minibuffer (`read-from-minibuffer`, `read-file-name`
     without completion): `C-x C-f`, `C-x C-w`, saving a buffer with no
     file, `yes-or-no-p`, history — done. File name completion (TAB) comes
     with completion in general.
5. Moving between lines: `indent.c` columns and `vertical-motion`,
   `next-line`/`previous-line` (by screen lines, keeping the goal column)
   — done.
**Milestone: open a file, edit it, save it — reached with 4a.**

### Phase 4 — Config API
Fortran-native `init.f90` API, `fortmacs --recompile`.

1. The API: module `fortmacs` (settings, commands, keys) and the
   `user_config` module's `configure`, run at startup (`-q` skips it);
   `keymap-global-set`, `key-valid-p`. — done
2. `fortmacs --recompile`: build `~/.config/fortmacs/init.f90` with
   gfortran against the installed library (`callproc_fm` runs the
   compiler) into `~/.local/state/fortmacs`, scratch in
   `~/.cache/fortmacs`; the installed binary runs the built one. — done

### Phase 5 — Generations, then features
1. Numbered generations of the configuration in
   `~/.local/state/fortmacs/generations` (each with its binary and the
   init.f90 it was built from, `current` a symbolic link), `--list-generations`,
   `--rollback`, `--switch-generation N`. — done
2. Features, in this order unless the user asks otherwise:
   1. The mark, the region and the kill ring (C-SPC, C-w, M-w, C-k, C-y,
      M-y, region highlighting). — done
   2. Undo (C-/, C-_, C-x u, C-?). — done
   3. Scrolling and recentering (C-v, M-v, C-l). — done
   4. Incremental search (C-s, C-r). — done
   5. M-x and minibuffer completion.
   6. Built-in help (C-h f, C-h k, C-h v) with the docstrings compiled in.
   7. Word and paragraph motion (syntax tables).
   8. Buffers and windows (C-x b, C-x k, C-x 2, C-x 3, C-x o).

## Per-module workflow (definition of done)

1. **Scope.** List the upstream functions being ported and the ones
   deferred (with reason) in the module's status row below.
2. **Tests first.** Port the matching `test/src/*-tests.el` cases to
   test-drive (`test/test_<name>_fm.f90`) where they exist; add our own for
   uncovered behaviour. Name each ported test after its ERT test.
3. **Implement.** One module per upstream file, named after it with an `_fm` suffix
   (buffer.c → `buffer_fm`). Cite the
   upstream file/function in a comment on every ported procedure, e.g.
   `!> Port of insdel.c:insert_1_both.`
4. **Headers.** Ported files carry the upstream FSF notice plus ours
   (template below); new files carry ours only.
5. **Gate.** `tools/check.sh` passes (fprettify clean, debug profile
   `-std=f2018 -Wall -Wextra -fcheck=all -g`), and CI is green on both
   architectures.
6. **Record.** Update the status table, one module per PR, the session
   log, and the README's progress block (`tools/progress.sh
   --update-readme`).
7. **Merge.** The user merges. Claude polls the open PR
   (`gh api repos/vermi-culous/Fortmacs/pulls/N --jq .merged`) and, once it
   is merged, starts the next step on a fresh branch from main; work done
   while waiting is rebased onto main before its PR is opened.

### Header for ported files

```fortran
! <name>_fm.f90 -- <one line>.
! Ported from GNU Emacs 31.1 src/<file>.c.
!
! Copyright (C) <upstream years> Free Software Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: ... (GPLv3-or-later text, as in version_fm.f90)
```

Copy the FSF year range exactly from the upstream file.

## Status

| Module | Upstream | Ported | Deferred | Tests | State |
|---|---|---|---|---|---|
| version_fm | — | — | — | test_version_fm | done |
| error_fm | data.c `syms_of_data` error table (subset), fileio.c `file-error`, print.c `print_error_message` layout | status type, codes (incl. `void-function`, `file-missing`, `file-already-exists`, `mark-inactive`, `search-failed`), file-error messages, `condition_matches`, `format_error` | remaining error symbols (added as modules need them) | test_error_fm | done |
| handle_fm | — (replaces GC'd pointers) | slot table, generational handles | — | test_handle_fm | done |
| buffer_ops_fm (submodule of buffer_fm) | buffer.c | `kill-buffer` (unchains markers, clears the position cache), `erase-buffer` (with inline `widen`) | hooks, windows, processes, overlays, labeled restrictions | test_buffer_fm, test_marker_fm | done |
| buffer_fm | buffer.c, buffer.h (+ intervals.c `temp_set_point_both`, lisp.h `modiff_incr`) | BVAR `undo_list` (an array of entries, newest last) and the undo-equiv-table per buffer, BVAR `mark`, `mark-active` and the mark ring, `transient-mark-mode`, `deactivate-mark` (the variable, as `vdeactivate_mark`), struct buffer_text / struct buffer (subset), registry, `get-buffer`, `get-buffer-create`, `generate-new-buffer-name`, `buffer-name`, `buffer-last-name`, `rename-buffer`, `buffer-list`, `set-buffer`, `kill-buffer` (core), `buffer-modified-p`, `restore-buffer-modified-p`, modified ticks, `barf-if-buffer-read-only`, BUF_BYTE_ADDRESS / FETCH_BYTE / FETCH_CHAR / next/prev char len, BUF_COMPUTE_UNCHANGED, `get-file-buffer` (given an expanded name), BVAR filename/directory, `buffer-backed-up`, visited modtime, new buffers inherit `default-directory` | indirect buffers, buffer-local vars, overlays, files/locking, hooks, `autosaved` state, `inhibit-read-only`, `erase-buffer`, marker/window parts of `kill-buffer`, `other-buffer` ordering | test_buffer_fm (ports bug27966 and the file-independent parts of test-buffer-modifications, test-restore-buffer-modified-p, test-buffer-chars-modified-ticks) | done |
| marker_fm | marker.c (+ alloc.c `Fmake_marker`) | conversion with cache and marker hints, `make-marker`, `marker-buffer`, `marker-position`, `marker_byte_position`, `marker-last-position`, `set-marker` (integer and marker positions), `set_marker_restricted(_both)`, `set_marker_both`, `detach_marker`, `unchain_marker`, `copy-marker`, insertion types, `count_markers`; `free_marker` (ours: no GC) | `build_marker` after long scans (would leak without GC), `need_adjustment` | test_marker_fm (marker-tests.el needs windows; its Bug#24368 point is ported as set-marker-from-other-buffer) | done |
| insdel_fm | insdel.c | undo recording (`record_insert`, `record_delete`, `record_first_change` in `modify_text`), `prepare_to_modify_buffer` sets `deactivate-mark`, gap motion, `make_gap` (larger/smaller), `insert`, `insert_char`, `insert_before_markers`, `insert_1_both`, `adjust_point`, `adjust_markers_for_insert/delete`, `del_range`, `del_range_1/_byte/_both/_2`, `modify_text`, `prepare_to_modify_buffer` (read-only check) | intervals, overlays, change hooks, compositions, quit during gap motion, `copy_text`, `insert_from_string/buffer`, `replace_range` | test_insdel_fm (own; no upstream insdel-tests.el) | partial |
| editfns_fm | editfns.c (+ buffer.c `validate_region`, intervals.c `set_point_from_marker`) | `mark-marker` (made on first use), `region-beginning`, `region-end` (`region_limit`, `mark-inactive`), `point`, `point-min/max`, `pos-bol`, `pos-eol`, `line-beginning/end-position` (constrained to fields), fields (`find_field`, `field-beginning`, `field-end`, `field-string-no-properties`, `delete-field`, `constrain-to-field`, `get-pos-property`; text properties only), `inhibit-field-text-motion`, `buffer-size`, `gap-position/size`, `goto-char` (integer and marker), `point(-min/-max)-marker`, `position-bytes`, `byte-to-position`, `following-char`, `preceding-char`, `bobp`, `eobp`, `bolp`, `eolp`, `char-after/before`, `insert-char` (count), `narrow-to-region`, `widen`, `buffer-substring`, `buffer-string`, `delete-region`, `delete-and-extract-region` | labeled restrictions, save-excursion/-restriction, `format`/`message`, overlays in fields, INHIBIT-CAPTURE-PROPERTY, `transpose-regions`, `compare-buffer-substrings`, `subst-char-in-region`, `insert-buffer-substring`, text properties in substrings | test_editfns_fm (ports find-fields), test_cmds_fm (own; most of editfns-tests.el needs format/undo/transpose) | partial |
| search_fm | search.c | `find_newline` (both directions, across the gap), `scan_newline`, `scan_newline_from_point`, `find_newline_no_quit`, `find_before_next_newline`; `search-forward`, `search-backward` (`search_command`, `search_buffer`, `simple_search`, through the case canon table with `case-fold-search`), match data for group 0 (`match-beginning`, `match-end`) | newline cache (region-cache.c), Boyer-Moore, regexp search, `match-data`/`set-match-data`, `replace-match`, buffer-local `case-fold-search`, `translation-table-for-input` | test_cmds_fm, test_search_fm (own; search-tests.el covers replace-match) | partial |
| cmds_fm | cmds.c | `undo-auto-amalgamate` in `self-insert-command` and `delete-char`, `forward-char`, `backward-char` (`move_point`), `forward-line` (incl. excessive counts), `beginning-of-line`, `end-of-line`, `delete-char`, `self-insert-command` (insertion only), command registration (`syms_of_cmds`) | overwrite mode, abbrevs, auto-fill, `post-self-insert-hook`, kill-ring `delete-char`, intangible text | test_cmds_fm (ports forward-line-with-bignum from cmds-tests.el), test_bindings_fm | partial |
| casetab_fm, uni_case_fm (generated) | casetab.c, buffer.h, characters.el (case pairs), admin/unidata/UnicodeData.txt | the standard case table: `downcase`, `upcase`, `uppercasep`, `lowercasep`, the canonicalize table (`set_canon`); ASCII as in `init_casetab_once`, the rest from the Unicode simple case mappings as characters.el sets them (`tools/gen-unicase.sh`) | buffer-local case tables, `set-case-table`, the equivalence table, `case-table-p`, special casing (ß → SS) | test_casetab_fm (ports casefiddle-tests-case-table, the upcase/downcase part of casefiddle-tests-char-casing) | partial |
| isearch_fm | isearch.el | `isearch-forward`, `isearch-backward`, `isearch-mode`, `isearch-done`, `isearch-update`, `isearch-message` (prefix and suffix), `isearch-search`, `isearch-search-string`, `isearch-search-and-update`, `isearch-repeat` (+ `-forward`, `-backward`, wrapping), `isearch-exit` (nonincremental search on an empty string), `isearch-cancel`, `isearch-abort`, `isearch-delete-char`, `isearch-del-char`, `isearch-printing-char`, `isearch-quote-char`, `isearch-yank-char`, `isearch-yank-line`, `isearch-yank-string`, `isearch-yank-kill`, `isearch-yank-pop-only`, the mark where the search started, `isearch-beginning-of-buffer`, `isearch-end-of-buffer`, `isearch-toggle-case-fold`, `isearch-edit-string` (with-isearch-suspended), the search ring (`isearch-update-ring`, `isearch-ring-advance`/`-retreat`), `isearch-pre-command-hook` (prefix arguments allowed), `isearch-no-upper-case-p`, `search-upper-case`, `search-exit-option` (t or nil), `isearch-mode-map`; C-s, C-r | regexp, word, symbol and char-fold search, lax whitespace, highlighting and lazy highlighting, C-w/C-M-w (syntax), reading a kill in the minibuffer (C-u M-y), invisible text, scrolling and motion during search, query-replace/occur, help map, input methods, completion, minibuffer-local-isearch-map | test_isearch_fm (ports isearch--test-done; own tests through the command loop) | partial |
| intervals_fm | intervals.c, intervals.h | intervals as a sorted run array: `create_root_interval`, `find_interval`, `split_interval_right/left`, `intervals_equal`, `offset_intervals` (insertion/deletion), `textget` (with `category`), TMEM | balanced tree (only if profiling asks), stickiness/`graft_intervals_into_buffer` (insert-and-inherit), string intervals, `char-property-alias-alist`, `default-text-properties` | test_textprop_fm | partial |
| textprop_fm | textprop.c | `text-properties-at`, `get-text-property`, `put-text-property`, `add-text-properties`, `set-text-properties`, `remove-text-properties`, `remove-list-of-text-properties`, `next/previous(-single)-property-change`, `text-property-any`, `text-property-not-all`, `verify_interval_modification` (read-only, inhibit-read-only, stickiness on insertion), `modify_text_properties`, `text_property_stickiness`, `next/previous-single-char-property-change` (without overlays) | string properties, overlays (`get-char-property`), undo records, modification hooks, LIMIT t, `add-face-text-property` | test_textprop_fm (ports textprop-interval-immutability) | partial |
| sys_linux_fm (src/sys) | — (glibc/kernel headers) | struct termios / winsize / pollfd, termios flags and c_cc indices, ioctl/open/signal/errno constants, bindings for tcgetattr, tcsetattr, read, write, close, isatty, poll, ioctl, signal, raise, posix_openpt, grantpt, unlockpt, ptsname, open, errno; file calls (creat, access, rename, unlink, chmod, mkdir, rmdir, statx, getcwd, setenv, unsetenv), user ids and password entries (getuid, geteuid, getegid, getpwnam, getpwuid), strerror | — | test_sysdep_fm (struct sizes checked on both arches in CI) | done |
| sysdep_fm (src/sys) | sysdep.c (+ dispnew.c SIGWINCH flag, process.c `allocate_pty`) | `emacs_get_tty`, `emacs_set_tty`, the termios part of `init_sys_modes` / `reset_sys_modes` (raw-mode transform as pure `tty_raw_settings`), `get_tty_size`, `set_window_size`, `emacs_read`, `emacs_full_write`, `emacs_write`, `input_available` (poll), SIGWINCH and SIGINT flags, `open_pty`, file wrappers returning errno (`emacs_open_read`, `emacs_creat`, access, rename, chmod, mkdir, stat via statx, setenv), `emacs_get_current_dir_name` (prefers a matching $PWD), password-entry home directories, `emacs_strerror` | job control (`narrow_foreground_group`, SIGTSTP/SIGCONT), F_SETOWN/SIGIO, keypad/meta terminfo setup, process groups | test_sysdep_fm (runs on a pty, so CI needs no terminal) | done |
| character_fm | character.c, character.h | encoding table, `char_string`, `multibyte_length`, `string_char_and_length`, `raw_prev_char_len`, raw-byte (eight-bit) conversions, `multibyte_chars_in_text`, `parse_str_as_multibyte`, `str_as_multibyte`, `count_size_as_multibyte`, `str_to_multibyte`, `str_as_unibyte`, `char_resolve_modifier_mask`, small predicates | `chars_in_text` (needs current buffer → buffer_fm); `char_width`/`string-width` and `alphabeticp` etc. (need char-tables → display phase); `translate_char`; Lisp-level DEFUNs | test_character_fm (own; character-tests.el only tests `string-width`, deferred) | done |
| lisp_fm, lread_fm, data_fm, fns_fm | lisp.h, alloc.c, lread.c (obarray), data.c, fns.c | `value_type` (nil, fixnums, symbols, strings, proper lists, buffer/marker/keymap refs), constructors and predicates, obarray (`intern`, `intern-soft`), `eq`, `car`/`cdr`, symbol values, `equal`, `length`, `nth`, `memq`, `member`, `string-lessp`, plists, `get`/`put`, fns.c `yes-or-no-p` (submodule fns_ask_fm; ASCII downcase) | dotted pairs, floats, bignums, vectors, hash tables, function cells | test_values_fm (ports fns-tests length and plist cases, binding-test-set-constant) | partial |
| keymap_fm | keymap.c (+ keymap.el `key-parse`, `key-valid-p`, `keymap-set`, `keymap-global-set`, keyboard.c `reorder_modifiers` subset) | registry with explicit lifetimes, `make-keymap`, `make-sparse-keymap`, `keymapp`, `keymap-parent`, `set-keymap-parent` (cycle check), `copy-keymap`, `access_keymap` (meta→ESC, t defaults, parent and composite prefix maps), `define-key` (with REMOVE, `define_as_prefix`), character ranges (the char-table part of full keymaps), overriding-terminal-local-map, `lookup-key` (lists of keymaps, too-long), `current-global-map`, `use-global-map`, `current-local-map`, `use-local-map`, `current-active-maps`, `key-binding` (local + global), keyboard.c `parse_modifiers`/`apply_modifiers`/`reorder_modifiers` (symbols), `kbd`, `key-description`, `single-key-description`, `push_key_description`, `key-valid-p`, `keymap-set` (a character definition names a command), `keymap-global-set` | `keymap-local-set` and the other keymap.el functions, char-tables/vectors (full keymap is a flag), menu items, `[remap CMD]`, symbol function cells as prefix maps, event ranges, minor-mode / text-property / overriding maps, `where-is-internal`, `describe-*`, `text-char-description`, `map-keymap` | test_keymap_fm (ports keymap-tests: make-keymap, keymapp, keymap-parent, copy-keymap, define-key/undefined, /keyboard-macro, /keymap, lookup-key, /list-of-keymaps, /too-long, removal-inherit, unset-test-remove-and-inheritance, keymap--key-description, keymap-set-consistency (the `keymap-set` part); subr-tests: subr-test-key-valid-p) | partial |
| callint_fm | callint.c (+ eval.c `commandp`) | `mark-even-if-inactive`, command registry (Fortran procedures under symbols, standing in for function cells), `call-interactively`, `commandp`, `prefix-numeric-value`, `current-prefix-arg`, `prefix-arg` | interactive specs (commands read their own arguments), `command-history`, `funcall-interactively` | test_keyboard_fm | partial |
| keyboard_fm | keyboard.c (+ simple.el `command-execute`/`undefined`, macros.c `execute-kbd-macro` simplified) | undo boundaries before each command (`undo-auto--add-boundary`, `point_before_last_command_or_undo`), deactivating the mark after a command that set `deactivate-mark`, event queue, `tty_read_avail_input` (meta modes 0/1/2, UTF-8 keyboard decoding), `unread-command-events`, `discard-input`, `input-pending-p`, `read_char` core, `read_key_sequence` (input-decode-map, local-function-key-map, key-translation-map, upper-case and shift fallbacks), `keyremap_step`, `test_undefined`, `command_loop_1` (prefix arg, keyboard macros, error reporting), subr.el `set-transient-map` (one key sequence, pushed over the previous overriding map as internal-push-keymap does), redisplay before waiting for input (a hook, to keep keyboard_fm below xdisp_fm), `recursive_edit_1` and the `exit` catch (`exit-recursive-edit`, `abort-recursive-edit`), pre- and post-command hooks (one function each; the pre-command hook may change `this-command`), C-g from SIGINT read as the quit character, subr.el `sit-for` | echoing of prefix keys, timers and idle hooks, mouse/frame events, `recursive-edit` command, macro recording, general hooks, help-char, case tables (fallback is ASCII), quit-while-computing, translation functions in keyremaps | test_keyboard_fm (own; keyboard-tests.el and callint-tests.el need Lisp) | partial |
| simple_fm | simple.el | undo (`undo`, `undo-only`, `undo-redo`, `undo-more`, `undo-start`, `primitive-undo`, `undo-equiv-table`, `pending-undo-list`, `undo-no-redo`), the mark (`mark`, `set-mark`, `push-mark` with the buffer's mark ring, `pop-mark`, `activate-mark`, `deactivate-mark`, `region-active-p`, `use-region-p`, `set-mark-command`, `push-mark-command`, `pop-to-mark-command`, `exchange-point-and-mark`, `mark-whole-buffer`), the kill ring (`kill-new`, `kill-append`, `current-kill`, `kill-region` with read-only copying, `copy-region-as-kill`, `kill-ring-save`, `kill-line`, `kill-whole-line`, `forward-visible-line`, `yank`, `yank-pop`, `rotate-yank-pointer`, subr.el `insert-for-yank`), `delete-active-region` and KILLFLAG in the deletion commands, keyboard-quit deactivating the mark, `newline`, `open-line`, `delete-backward-char`, `delete-forward-char`, `keyboard-quit`, `quoted-insert`, `universal-argument` (+ `-more`, `-map`, `--mode`), `digit-argument`, `negative-argument`, `prefix-command-preserve-state`, `next-line`, `previous-line`, `line-move` (`line-move-visual` by screen lines with temporary-goal-column, `line-move-1` by buffer lines), `line-move-finish`, `goal-column`, `line-move-visual`; minibuffer history (`goto-history-element`, `previous-history-element`, `next-history-element`, one default) | undo in a region, the global mark ring, shift-selection, `yank-from-kill-ring` (M-y after other commands is an error, as before Emacs 28), interprogram (clipboard) cut and paste, `indicate-copied-region`, yank handlers, electric indent, auto-fill, hard newlines, prefix echo, octal `read-quoted-char`, `move-beginning/end-of-line`, `set-goal-column`, `next-line-add-newlines`, `track-eol`, vscroll, invisible text in line motion | test_bindings_fm , test_simple_fm (ports simple-tests: kill-whole-line-after-other-kill, -buffer-boundaries, -line-boundaries; own tests for the mark and the kill ring) , test_undo_fm | partial |
| bindings_fm | bindings.el, subr.el (`global-map`, `esc-map`, `ctl-x-map`), international/mule-conf.el (self-insert range) | C-_, C-/, C-x u, C-?, C-M-_, C-SPC, C-@, C-x C-x, C-x h, C-w, M-w, C-k, C-y, M-y, <C-S-backspace>, the default keymaps with every binding whose command is ported (incl. minibuffer-local-map history keys), `function-key-map` (backspace/delete → DEL), `right-char`, `left-char`; C-n, C-p, <down>, <up> | bindings to unported commands, bidi-aware left/right, menus | test_bindings_fm | partial |
| undo_fm | undo.c (+ simple.el undo-auto) | `record_point`, `record_insert` (merging consecutive insertions), `record_delete`, `record_first_change`, `undo-boundary`; simple.el's `undo-auto--undoable-change`, `undo-auto--boundaries`, `undo-auto--ensure-boundary`, `undo-auto--add-boundary`, `undo-auto-amalgamate` (`amalgamating-undo-limit`), here because the C side calls them | marker adjustments, property changes, `record_change`, the boundary timer, `truncate_undo_list` (undo-limit), `buffer-undo-list` as a Lisp value, `buffer-enable-undo`/`buffer-disable-undo` | test_undo_fm (ports undo-tests: undo-test0, undo-test2, undo-test-buffer-modified, undo-test-file-modified; simple-tests: simple-tests--undo, undo-point-in-wrong-place (first case); own: amalgamation) | partial |
| term_xterm_fm | term/xterm.el | `xterm-rxvt-function-map`, `xterm-function-map` (cursor, editing, F1–F12, keypad, all modifier forms, modifyOtherKeys/formatOtherKeys), `terminal-init-xterm` (input-decode-map part) | bracketed paste, focus events, terminal queries, colors, OSC 52, rxvt, `xterm-alternatives-map` | test_term_xterm_fm (own) | partial |
| terminfo_fm | terminfo.c (`tparam`), plus the libtinfo interface Emacs uses, reimplemented | compiled entry parser (16- and 32-bit numbers, extended capabilities), ncurses search path, `tgetent`, `tgetstr`/`tgetnum`/`tgetflag` (termcap codes), `tigetstr`/`tigetnum`/`tigetflag`, `tparm` (full %-language and printf formats), `tgoto`, padding removal, built-in xterm-256color | string parameters (%s), padding delays, the termcap database | test_terminfo_fm (own; checked against infocmp) | done |
| dispextern_fm | dispextern.h | `glyph_type` (character, face id, padding), `face_type` (terminal attributes and colors), face color constants, basic face ids (default, mode line), `face_tty_specified_color` | everything graphical; glyph rows and matrices (with redisplay) | test_term_fm | partial |
| termchar_fm | termchar.h, cm.h (`struct cm`) | `tty_display_info` (capabilities, modes, cursor-motion state), the output buffer (`tty_output`, `tty_flush`) | termscript, mouse highlight, terminal parameters | test_term_fm | partial |
| cm_fm | cm.c, cm.h macros | `cmcostinit`, `calccost` (incl. tabs), `cmgoto`, `cmcheckmagic`, `Wcm_clear`, `Wcm_init`, `cmat`, `cmplus`, `losecursor` | — (where upstream aborts in `cmcheckmagic`, the cursor is forgotten) | test_term_fm | done |
| term_fm | term.c | `init_tty` (capabilities, colors incl. 24-bit, size, raw mode), `delete_tty`, `term_get_fkeys` (terminfo keys into input-decode-map), `calculate_costs` (motion part), bell, terminal modes, scroll region, insert/standout modes, cursor show/hide, `cursor_to`, clearing, `write_glyphs` (incl. the bottom-right cell), `insert_glyphs`, `delete_glyphs`, `ins_del_lines`, `turn_on_face`/`turn_off_face`, `encode_terminal_code` (UTF-8) | glyph production (with redisplay), menus, mouse/GPM, colour-mode switching, char/line ins-del cost tables, tty-mode strings, DOS/Windows/Android | test_term_fm (own; byte-exact xterm output, a pty) | partial |
| frame_fm | frame.h | `frame_type` (terminal, current and desired matrices, face table, cursor, root and minibuffer windows, which is selected), `selected_frame` (frame.c) | window splitting, child frames, frame parameters | test_dispnew_fm, test_xdisp_fm | partial |
| dispnew_fm | dispnew.c | frame matrices (`adjust_frame_glyphs`, clear current/desired), `redraw_frame`, `write_row` (prefix/suffix matching, character insertion and deletion by cost), `write_matrix` (last row first), `make_matrix_current`, `tty_set_cursor`, `update_frame` (terminal path), `ding`; term.c `calculate_ins_del_char_costs` | line scrolling (scroll.c), window matrices, pausing for input, size changes, child frames, menus | test_dispnew_fm (own; byte-exact xterm output) | partial |
| window_fm, window_scroll_fm (submodule) | window.c, window.h, window.el (scrolling commands) | `struct window` (subset: buffer, start and point markers, geometry, cursor, hscroll), `set-window-buffer`, `window-start`, `set-window-start` (incl. NOFORCE), `window-point`, `set-window-point`, `window-body-height` (lines), freeing a window's markers; `scroll-up`, `scroll-down` (`window_scroll_line_based`, `scroll_command`), `recenter` (`recenter-redisplay` `tty`), `move-to-window-line`, `next-screen-context-lines`; window.el `scroll-up-command`, `scroll-down-command` (with `scroll-error-top-bottom` and the goal column), `recenter-top-bottom`, `move-to-window-line-top-bottom`; C-v, M-v, <next>, <prior>, C-l, M-r | window trees and splitting, `selected-window` as an object, `scroll-margin`, `scroll-preserve-screen-position`, pixel-based scrolling, customizing `recenter-positions`, other-window scrolling, window parameters, dedication, `window-end` | test_window_fm (own; upstream has no ERT tests for scrolling), test_xdisp_fm | partial |
| xdisp_fm | xdisp.c (+ term.c `produce_glyphs` terminal part, xfaces.c `realize_basic_faces` subset) | the region face (Transient Mark mode, selected window, `:extend`; inverse video until tty colors are ported), `redisplay` (one window, recentering when point leaves the window), `redisplay_window` core, glyph production for terminals (TAB, `^X` and `\ooo` control/raw bytes, wide characters, `\` continuation), `display_mode_line` (fixed default format), echo area (`message`, `cursor-in-echo-area`), the active minibuffer on one line with auto-hscroll and `$` marks, line counting for `L`, `glyph_columns` (display width at a column) | mode-line-format interpretation, faces from text properties, truncation, horizontal scrolling of other windows, multi-line minibuffers (resize-mini-windows), the minibuffer-prompt face, bidi, `scroll-margin`/`scroll-conservatively`, display properties, overlays, fringes, header/tab lines | test_xdisp_fm (own; xdisp-tests.el needs windows and Lisp) | partial |
| files_fm | files.el (+ startup.el `command-line-1` file arguments are in emacs_fm) | `make-directory` (PARENTS; `files--ensure-directory`), `find-file-noselect` (with `find-file-noselect-1`, `after-find-file` messages and read-only), `find-file` (for programs), `create-file-buffer`, `abbreviate-file-name`, `backup-buffer` (rename or copy to `FILE~`, keeping modes), `make-backup-file-name`, `normal-backup-enable-predicate`, `file-ownership-preserved-p`, `save-buffer` (prefix args), `basic-save-buffer` (modtime check, write-protected files), `save-some-buffers`, `save-buffers-kill-emacs`, `save-buffers-kill-terminal`, `set-visited-file-name`, `write-file`, `find-file` and `write-file` commands (via submodule files_read_fm); `C-x C-f`, `C-x C-w`, `C-x C-s`, `C-x s`, `C-x C-c` | file name completion, wildcards and dired, creating missing directories, auto-save, file locks, major modes and local variables, hooks, uniquify, numbered backups and `backup-directory-alist`, `file-precious-flag`, symlink chasing (a symlink is backed up by copying), revert, truenames | test_files_fm (ports files-tests-make-directory; otherwise own: files-tests.el needs file name handlers, the minibuffer and hooks) | partial |
| fileio_fm | fileio.c (+ dired.c `file-attributes` subset, editfns.c `user-uid`/`group-gid`) | `make-symbolic-link` (OK-IF-ALREADY-EXISTS; `file-already-exists`), `file-executable-p`, `file-newer-than-file-p`, `file-name-directory`, `file-name-nondirectory`, `file-name-as-directory`, `directory-file-name`, `directory-name-p`, `file-name-absolute-p`, `expand-file-name` (`~`, `~USER`, relative default directories, null-byte check), `get_homedir`, `user_homedir`, `file-exists-p`, `file-readable-p`, `file-writable-p`, `file-directory-p`, `file_accessible_directory_p`, `file-regular-p`, `file-symlink-p` (as a truth value), `file-modes`, `set-file-modes`, `rename-file`, `copy-file` (modes only), `make-directory-internal`, `delete-directory-internal`, `delete-file-internal`, `insert-file-contents` (with VISIT; UTF-8 with raw bytes), `write-region` (whole buffer or region, VISIT), `verify-visited-file-modtime`, `set-visited-file-modtime`, `report_file_errno`, `substitute-in-file-name` (with env.el `substitute-env-in-file-name`) | file name handlers, coding systems and EOL conversion, BEG/END/REPLACE, APPEND/MUSTBENEW/LOCKNAME, fsync, `file-truename`, `make-temp-file`, ACLs and SELinux, `file-name-concat` | test_fileio_fm (ports directory-file-name, file-name-as-directory, relative-HOME, relative-default-directory, expand-file-name-null-bytes, file-name-absolute-p) | partial |
| subr_fm | subr.el | `add-to-history` MAXELT and KEEP-ALL, `read-key` (single events), `y-or-n-p` (the `y-or-n-p-use-read-key` path), the `query-replace-map` answers (replace.el), `add-to-history`, `global-set-key` | the minibuffer path of `y-or-n-p` (the read-key one is kept), help-form, scrolling answers, `ding` | test_files_fm, test_keymap_fm | partial |
| map_ynp_fm | emacs-lisp/map-ynp.el | `map-y-or-n-p` (read-key path: y/SPC, n/DEL, !, ., q/RET, C-g) with Fortran prompter and actor procedures | help (C-h), ACTION-ALIST, the minibuffer path, dialog boxes | test_files_fm | partial |
| minibuf_fm | minibuf.c | `read_minibuf` (prompt as a read-only field, minibuffer window selected, recursive edit, history, default), `read-from-minibuffer`, `read-string`, `get_minibuffer`, `minibufferp`, `minibuffer-prompt-end`, `minibuffer-contents-no-properties`, `abort-minibuffers`, `minibuffer-local-map`, `enable-recursive-minibuffers`, `history-add-new-input`; the prompt's cursor-intangible as a post-command hook | completion (`try-completion`, `all-completions`, `completing-read`), `read-minibuffer`/`eval-minibuffer`, minibuffer-only frames, input methods, minibuffer-setup/exit hooks, window configurations | test_minibuf_fm (own; minibuf-tests.el tests completion) | partial |
| minibuffer_fm | minibuffer.el | `exit-minibuffer`, `delete-minibuffer-contents`, `read-file-name` (read-file-name-default: abbreviated directory as input, default file, `substitute-in-file-name`), `insert-default-directory`, minibuffer-local-map's C-g/C-j/RET | completion (TAB, *Completions*, styles), `read-file-name` MUSTMATCH/PREDICATE, file name shadowing | test_minibuf_fm | partial |
| env_fm | env.el | `getenv` (process environment), `substitute-env-vars` (WHEN-UNDEFINED nil or t) | `setenv`, `process-environment`, frames' environments | test_fileio_fm | partial |
| indent_fm | indent.c | `current-column`, `move-to-column` (no FORCE), `vertical-motion` (LINES or (COLS . LINES), in the selected window, continuing lines as redisplay does), point's screen column (as posn-at-point gives it) | invisible text, display properties, selective display, truncated and horizontally scrolled lines, `indent-to`, `current-indentation`, `compute_motion` | test_indent_fm (own; indent-tests.el needs invisible text) | partial |
| emacs_fm | emacs.c `main` (+ startup.el `command-line` terminal part, `-q`/`-Q` and the init file and `command-line-1` file arguments, faces.el `tty-run-terminal-initialization`, buffer.c `init_buffer` directory) | `emacs_main`: bindings, `*scratch*` (in the starting directory), terminal setup (incl. xterm init), windows and faces, raw mode, visiting command-line files (`+LINE`, `--`, `-file`/`-find-file`/`-visit`), the command loop, restoring the terminal; `command_line` (`-q`, `--no-init-file`, `-Q`), `load_init_file` (the user's `configure`, errors shown in the echo area, `init-file-had-error`); `run`, the program (`--version`, `--help`, tty checks) | other options (`--debug-init`, `-u`, `--init-directory`), `+LINE:COLUMN` columns (need indent.c), splitting windows for two files, daemon, dumping, signals | test_xdisp_fm (runs the editor on a pty), test_files_fm, test_emacs_fm (options and the init file, on a pty) | partial |
| fortmacs | (none: the config API) | the module an init.f90 uses: re-exports of the status type, values, `defcommand`, `keymap-global-set`, `global-set-key`, editing and file functions, and the settings (`tab-width`, `make-backup-files`, `line-move-visual`, `goal-column`, `history-length`, `enable-recursive-minibuffers`, `history-add-new-input`, `insert-default-directory`, `inhibit-field-text-motion`, `visible-bell`), which now carry their upstream docstrings; app/main.f90's default `configure` is written against it (which also keeps fpm from pruning it out of an install) | hooks (`add-hook`), buffer-local settings | test_emacs_fm (a `configure` written against it) | partial |
| config_fm | (none: building the configuration) | XDG directories for init.f90 (`~/.config/fortmacs`), generations (`~/.local/state/fortmacs/generations/N`, `current`) and scratch (`~/.cache/fortmacs`); `install_prefix` (PREFIX/bin/fortmacs next to PREFIX/lib/libfortmacs.a), `driver_source` (the user build's main program), `recompile` (gfortran via `call_process`, errors shown, the last good build kept) into a new generation; `generation_numbers`, `current_generation`, `generation_of`, `switch_generation` (a new link renamed over `current`), `rollback`, `list_generations` (as Guix lists them); emacs_fm's `run` handles `--recompile`, runs the user's build from an installed fortmacs (unless the installation is newer) and reminds when init.f90 changed; `--list-generations`, `--rollback`, `--switch-generation N` ("switched from generation 3 to 2") | `--delete-generations`; a generation for Fortmacs itself | test_config_fm; tools/test-recompile.sh (installs into a scratch prefix, builds docs/init.f90 twice, lists, rolls back and switches, and checks a failed build makes no generation; run by tools/check.sh) | partial |
| dired_fm | dired.c | `directory-files` (FULL, NOSORT, COUNT; sorted as `string-lessp`); sysdep: `emacs_directory_entries` (opendir/readdir) | MATCH (needs regexps), `directory-files-and-attributes`, file name completion | test_dired_fm (ports dired-test-directory-files without MATCH) | partial |
| callproc_fm | callproc.c | `call-process` (INFILE, DESTINATION t, nil, a buffer or a buffer name; ARGS; the current buffer's directory; exit status or signal description), `call-process-region` (whole buffer or a region, DELETE; via a temporary file, `create_temp_file`), the PATH search of lread.c `openp`; sysdep: `emacs_spawn` (fork and execv), `wait_for_termination`, `emacs_pipe`, `emacs_mkostemp`, readlink and execv | DESTINATION 0 and `(:file FILE)`, separate standard error, DISPLAY, quitting, coding systems, `exec-path`, `process-environment`, `process-file`, `shell-command` | test_callproc_fm (ports callproc-tests: call-process-region-entire-buffer-with-delete; own tests for the rest) | partial |

## Style

- The [fortran-lang style guide](https://fortran-lang.org/learn/best_practices/style_guide/):
  lowercase keywords and names, words separated by underscores, short but
  unambiguous names. Like the [stdlib guide](https://stdlib.fortran-lang.org/page/contributing/StyleGuide.html):
  `intent` on every dummy, named `end` statements, one module per file,
  FORD `!>` comments on public entities, 4-space indentation; but 132
  columns (stdlib's hard limit) rather than 80.
- Blank lines: neither guide above sets a rule, so they follow the
  [Bristol RSE Fortran guidelines](https://bristolcompositesinstitute.github.io/RSE-Guide/abaqus-user-subroutines/fortran-style-guidelines.html)
  ("Separate related blocks of code with 1 blank line"), but never more
  than one blank line, and none at the start or end of a body.
  `tools/blank-lines.awk` makes that mechanical: one blank line around
  every multi-line construct (if, do, select, block, associate, where;
  type and interface definitions), after the declarations, between
  procedures and around `contains`; none after a procedure or construct
  opens, before `else` or `case`, or before any `end`. Similar statements
  (the declarations, a run of calls) stay together; other blank lines
  between runs of statements are the author's.
  Comments directly above a statement move with it.
- Enforced by `tools/check.sh` (and so CI): fprettify for layout
  (`.fprettify.rc`),
  `tools/blank-lines.awk` for blank lines, Fortitude 0.9.2 with its default rules for the rest
  (`[extra.fortitude.check]` in fpm.toml). Notable rules: double-quoted
  strings, `use ..., only:` everywhere, `implicit none` in interface
  bodies, a `case default` in every `select case`, no misleading one-line
  `if` continuations. Justified exceptions are marked in the source with
  `! allow(rule-name)` and a comment saying why.

## Conventions learned while porting

- Bytes are default-kind `character` elements: build them with `char(i)` and
  read them with `ichar(c, kind=int32)`. Not `achar`, whose result outside
  0..127 is processor-dependent (gfortran warns).
- Upstream's unchecked fast paths (`check = false`, no end pointer) are not
  ported; everything bound-checks against `len(str)`.
- Functions that must return two values become `pure subroutine`s (a pure
  function may not have `intent(out)` arguments).
- fpm tree-shakes the library: a module is only compiled once something
  (a test or the app) uses it, so register a module's test suite at once.
- Procedures in a submodule can't call *private* parent procedures that
  the parent itself never uses: gfortran drops them (link error). Put such
  helpers in the submodule.
- Tests of the src/sys layer may use ISO_C_BINDING and sys_linux_fm
  directly; nothing else may. Terminal tests run on a pty (`open_pty`).
- If fpm reports "Project is up to date" after a source change, the build
  cache is stale: delete `build/gfortran_*`. CI always builds clean.
- Never pass the same variable to two dummies when one is `intent(out)`
  (e.g. `call split(iv, k, len, k)`); use a temporary.
- Never put a function result inside an array constructor of a type with
  allocatable components (`[f(x)]`): gfortran leaks it. Use `list(...)`
  or variables. No recursive allocatable types (gfortran copies them
  shallowly). `tools/check.sh --valgrind` (run in CI) catches leaks.
- Objects are freed explicitly: every `make_marker` needs a `free_marker`.
- Registry slots are reused after kill; any cache keyed by slot must also
  check `buffers(i)%serial` (see the regression test in test_insdel_fm).
- Text uses Emacs's internal encoding (UTF-8 extended to 22-bit codes, raw
  bytes as C0/C1 two-byte forms), so files with invalid UTF-8 round-trip.

- Keymaps are registry objects too. Prefix maps made by `define_key` and
  sub-keymap copies are owned by the map they were made for, so one
  `free_keymap` frees the whole tree.

## Design decisions

- **Pure Fortran (2026-10-20).** Besides libc system calls bound in
  src/sys, Fortmacs links no C libraries. Where upstream uses one (ncurses
  for terminfo, and later things like GnuTLS, libxml2 or image
  libraries), the needed part is reimplemented in Fortran or the feature
  waits.

- **Threads (2026-10-20).** One main thread owns all editor state
  (buffers, markers, symbols, keymaps, windows), as in upstream. Background
  work (file I/O, search over large files, subprocess output, syntax
  analysis) will run on POSIX worker threads bound from src/sys (libc
  `pthread_*` via ISO_C_BINDING, built with `-pthread`). Workers only see
  copied data and report back through a queue that the command loop polls,
  so core modules need no locks. Worker code must be reentrant (no `save`d
  state). No OpenMP or coarrays. Not implemented yet: lands with the
  command loop (`keyboard_fm`) or the first feature that needs it.

## Open questions

Decisions the porting work is waiting on, or defaults taken provisionally.
Each lists the default being used meanwhile.

1. **Merging.** *Decided:* one roadmap step per PR, based on main (never
   stacked); the user reviews and merges.
2. **Value model.** *Decided (2026-10-20):* no Lisp; values are a plain
   Fortran `value_type` with value semantics (lisp_fm), interned symbols
   by id (lread_fm), data.c/fns.c operations in data_fm/fns_fm. Values are
   stored as flat preorder atom arrays because gfortran 13/14 copy
   recursive allocatable components shallowly (double free).
3. **Error-code naming.** Error codes are `FM_<SYMBOL>` parameters, except
   the root `error` condition, which is `FM_GENERIC_ERROR` because Fortran
   names are case-insensitive and `FM_ERROR` collides with module
   `error_fm`. *Default: keep as is.*
4. **Buffer internals visibility.** `buffer_fm` exposes `buffers(:)` and
   the current slot `cur` (read-only outside the module) so insdel_fm and
   marker_fm can work like upstream's buffer.h macros. *Default: keep;
   higher layers use the procedures only.*
5. **Terminal capabilities.** *Decided (2026-10-20): (b), pure Fortran.*
   Upstream drives terminals through ncurses' libtinfo; Fortmacs reads
   the compiled terminfo files itself and evaluates `tparm` strings
   (`terminfo_fm`), with a built-in xterm-256color entry as fallback.
6. **Upstream LLM policy (FYI).** The emacs-31.1 tree contains an
   `AGENTS.md` stating that GNU Emacs does not accept LLM-generated
   contributions. That governs patches sent to Emacs, not this separate
   GPL project, but it means Fortmacs code (largely Claude-written) should
   never be offered upstream. *Default: read upstream as reference only;
   never propose changes to it.*

## Session log

Resume point for the next working session, newest first.

- 2026-10-22 — Guix: `fortmacs-manifest.scm` builds fpm 0.13.0 (from its single-file release) and Fortmacs from the checkout (offline; test-drive as a Guix origin; tests run; the library and modules installed for `--recompile`); the weekly Guix workflow builds it. On `guix-manifest`.
- 2026-10-22 — Phase 5.2, feature 4: incremental search (`isearch_fm`; literal `search-forward`/`search-backward` and match data in `search_fm`; the standard case table in `casetab_fm` with `uni_case_fm` generated from UnicodeData.txt by `tools/gen-unicase.sh`; the `search-failed` error; `ding`; a pre-command hook; transient maps now pushed over the previous overriding map), on `phase5-isearch` (based on main after PR #33; PRs #34–#36 are open). Next: M-x and minibuffer completion.
- 2026-10-22 — Phase 5.2, feature 3: scrolling and recentering (`window_scroll_fm`, a submodule of window_fm: `scroll-up`/`scroll-down` line based, `recenter`, `move-to-window-line`, and window.el's `scroll-up-command`, `scroll-down-command`, `recenter-top-bottom`, `move-to-window-line-top-bottom`; C-v, M-v, <next>, <prior>, C-l, M-r), on `phase5-scroll` (based on main after PR #33; PRs #34 and #35 are open, so the feature list here marks them by number). Next: incremental search.
- 2026-10-22 — README: the side-by-side syntax link points at `docs/syntax.html` in the repository, since GitHub Pages is not enabled while the repository is private (the Pages URL gave a 404). On `docs-syntax-link`.
- 2026-10-22 — Phase 5.2.2: undo (`undo_fm` for undo.c and the undo-auto boundaries; `undo`, `undo-only`, `undo-redo` in simple_fm; recording in insdel, boundaries in the command loop, amalgamation in cmds), on `phase5-undo` (based on main after PR #33, alongside PR #34, which it does not include). Next: scrolling and recentering.
- 2026-10-22 — Phase 5.2.1: the mark, the region and the kill ring (simple_fm; `mark-marker`, `region-beginning/end` in editfns_fm; Transient Mark mode with deactivation after changes and C-g; region highlighting; `mark-inactive`), on `phase5-killing` (based on main after PR #33). The user is away and asked for continuous work without waiting on merges, so the following feature PRs are each based on main as it stands and may need main merged in. Next: undo.
- 2026-10-22 — Style: one blank line at most (none between a procedure's start or end and its body, one between procedures), as the user asked; `check.sh` no longer needs a copy for fprettify. On `style-single-blank` (based on main after PR #32). Next: features, in whatever order the user asks for.
- 2026-10-22 — Phase 5.1: generations of the configuration (`config_fm`: numbered builds with their init.f90, `current` link, `--list-generations`, `--rollback`, `--switch-generation N`; `dired_fm` `directory-files`; `make-symbolic-link` and the `file-already-exists` error; opendir/readdir, symlink and localtime_r bindings), on `phase5-generations` (based on main after PR #31). Next: features, in whatever order the user asks for.
- 2026-10-22 — Style: blank lines after the Bristol RSE Fortran guidelines (`tools/blank-lines.awk`, checked and fixed by `tools/check.sh`), applied to all code; the README example too, with a link to `docs/syntax.html` (the two versions side by side, each part lighting up its counterpart; served by GitHub Pages from docs/); the repository is now vermi-culous/Fortmacs. On `style-blank-lines-bristol` (based on main after PR #30). Next: Phase 5, generations and `--rollback`.
- 2026-10-22 — Phase 4.2: `fortmacs --recompile` (`config_fm`, `callproc_fm`; `make-directory` with parents, `file-executable-p`, `file-newer-than-file-p`; the library and module files are installed, `tools/test-recompile.sh` checks a real build), README "Configuring" section and "similar to Guix" wording, on `phase4-recompile` (based on main after PR #29). Phase 4 is complete. Next: Phase 5, generations and `--rollback`; a blank-line style question is open with the user.
- 2026-10-21 — Phase 4.1: the config API (module `fortmacs`; `user_config`'s `configure` runs at startup, `-q`/`-Q` skip it, errors go to the echo area; `keymap-set`, `keymap-global-set`, `key-valid-p`, `global-set-key`; `run` in emacs_fm is the program, app/main.f90 a thin driver), on `phase4-config` (based on main after PR #28). Next: Phase 4.2, `fortmacs --recompile` (callproc_fm, installed library).
- 2026-10-21 — Naming: modules and files take an `_fm` suffix instead of an `fm_` prefix (buffer.c → `buffer_fm`, `test_buffer_fm`); no house blank-line rule (no guide sets one); on `rename-fm-suffix` (based on main after PR #27). Next: Phase 4, the config API.
- 2026-10-21 — Phase 3.5: moving between lines (`indent_fm`; `next-line`/`previous-line` in simple_fm, by screen lines with the goal column; C-n, C-p, <down>, <up>), on `phase3-lines` (based on main after PR #26). Phase 3 is complete. Next by the roadmap: Phase 4, the Fortran config API (`init.f90`, `fortmacs --recompile`).
- 2026-10-21 — README: progress as a table of vertical SVG bars (`docs/progress/*.svg`, drawn by `tools/progress.sh --update-readme`; Total = Emacs + packages, Emacs with C and Elisp, Packages with each archive), a "Real parallel threads" section, the syntax point last with a deeper example, "Why not?" removed. On `docs-progress-table`.
- 2026-10-21 — Phase 3.4b: the minibuffer (`minibuf_fm`, `minibuffer_fm`, `env_fm`; recursive edit, fields, `yes-or-no-p`, history, `C-x C-f`, `C-x C-w`; C-g on the terminal no longer kills the editor — SIGINT now reads as C-g), on `phase3-minibuf` (based on main after PR #24). Next: line motion (`next-line`/`previous-line`, which need indent.c columns), then scroll commands; completion later.
- 2026-10-21 — Guix CI fixed: the installer moved to guix.gnu.org/install.sh, pulls come from Codeberg, and the build shell gets nss-certs and git (curl and fpm need TLS). A run on the branch passed. On `ci-guix-main`.
- 2026-10-21 — README: progress shown as bars, now including the packages in GNU ELPA, NonGNU ELPA and MELPA (`tools/progress.sh --update-readme`; totals in `tools/package-totals`, recounted by `--fetch-packages` and the weekly Package totals workflow; ported packages listed in `tools/ported-packages`); syntax and "why not" in Why Fortran; no pip instructions. On `docs-progress`. Next: Phase 3.4b (minibuffer), WIP on `phase3-minibuf`.
- 2026-10-21 — Phase 3.4a: visiting and saving files (`fileio_fm`, `files_fm`, `subr_fm`, `map_ynp_fm`; command-line files in `emacs_fm`; file bindings in src/sys; `file-missing`), on `phase3-files` (based on main after PR #21). Milestone reached: `fortmacs FILE`, edit, `C-x C-s`. Next: Phase 3.4b, a minimal minibuffer (`C-x C-f`, `yes-or-no-p`).
- 2026-10-21 — Phase 3.3b: redisplay and a runnable editor (`window_fm`, `xdisp_fm`, `files_fm`, `emacs_fm`, redisplay hook in keyboard_fm, `emacs_isatty`), on `phase3-redisplay` (based on main after PR #20). Running `fortmacs` in a terminal edits `*scratch*` with a mode line and echo area; `C-x C-c` exits. Next: Phase 3.4, file visiting and saving; then `next-line` (indent.c columns) and scroll.c line scrolling.
- 2026-10-21 — Style and CI: Fortitude lint gate (all code fixed), 4-space indentation (stdlib), CI on Fedora (x86-64, aarch64) and Arch (x86-64) containers, a weekly Guix job (`.github/workflows/guix.yml`, also runnable by hand; first run unverified), on `style-ci`. Proposed instruction changes (pure Fortran incl. X11/fonts, terminal first then X11, distros, style, built-in docs) are awaiting the user's confirmation.
- 2026-10-21 — Frame updates (`frame_fm`, `dispnew_fm`, ins/del character costs) on `phase3-dispnew` (based on main after PR #18). WIP kept aside: `window_fm` (window.c subset) for the next step. Next: `xdisp.c`/`window.c` subset; a style/CI PR (Fortitude lint gate, Fedora and Arch containers).
- 2026-10-20 — Phase 3.2 terminal output (`dispextern_fm`, `termchar_fm`, `cm_fm`, `term_fm`), `tabs_safe_p` in sysdep_fm, PR-polling workflow recorded; on `phase3-term` (based on main after PR #17). Next: Phase 3.3, the redisplay subset (glyph matrices, one window, mode line, echo area).
- 2026-10-20 — Open question 5 decided (pure-Fortran terminfo); `terminfo_fm` on `phase3-terminfo` (based on main after PRs #15, #16). Pure-Fortran rule recorded under design decisions. Next: Phase 3.2, `term.c`/`cm.c` output on top of it.
- 2026-10-20 — Typing: `self-insert-command`, `simple_fm`, `bindings_fm`, `term_xterm_fm`, keymap character ranges and transient maps; progress.sh now counts cited Lisp definitions; on `phase2-bindings` (based on main after PR #14). Next: the editor binary needs Phase 3 output, which waits on open question 5; meanwhile `move-beginning/end-of-line` and `next-line` (needs indent.c columns), or the mark/region.
- 2026-10-20 — Command loop (`keyboard_fm`, `callint_fm`), `void-function` error, `(error MSG)` formats as MSG, on `phase2-keyboard` (based on main after PR #13). Next: xterm input-decode-map (port of `term/xterm.el` key maps) and `self-insert-command` + default global bindings (`bindings.el` subset), then the `app/main.f90` loop on a raw tty. Terminal output still waits on open question 5.
- 2026-10-20 — Phase 2 keymaps (`keymap_fm`), buffer-local keymap slot, threading decision recorded, on `phase2-keymap` (based on main after PR #12). Next: `keyboard_fm` subset (command loop: read events from the tty, look them up with `key_binding`, run Fortran command procedures), then `self-insert-command`. Terminal output still waits on open question 5.
- 2026-10-20 — Text properties (`intervals_fm`, `textprop_fm`), read-only text in insdel, `inhibit-read-only` in barf, on `phase1-textprop` (PR #12). Next: strings with properties / buffer-substring with properties, or Phase 2 keymaps.
- 2026-10-20 — Value model (lisp_fm, lread_fm, data_fm, fns_fm), valgrind CI gate, progress script, on `phase1-values` (based on main). Open: spurious gfortran `-Wmaybe-uninitialized` warnings in lisp_fm (no failures). Next: intervals/text properties.

- 2026-10-20 — `search_fm` newline scanning, line positions, `cmds_fm` motion/deletion on `phase1-search-cmds`, stacked on PR #7. Work that doesn't need an answer to open questions 2 (value model) or 5 (terminal capabilities) is now done; keymaps, the command loop, text properties and terminal output all wait on them.
- 2026-10-20 — `editfns_fm` subset on `phase1-editfns` (PR #7).
- 2026-10-20 — Phase 2 groundwork: src/sys (`sys_linux_fm`, `sysdep_fm`) on `phase2-sys` (PR #6), stacked on PR #5. Phase 1 step 5 waits on open question 2. Next without it: keyboard input decoding (`keyboard_fm` subset: reading bytes into events, terminal escape sequences come with `term.c`), or `editfns_fm` (point, narrowing, `buffer-substring`).
- 2026-10-20 — Phase 1 step 4 (marker objects, marker adjustment, `kill-buffer`/`erase-buffer` submodule) on `phase1-markers` (PR #5).
- 2026-10-20 — Phase 1 step 3 (`buffer_fm`, `insdel_fm`, conversion half of `marker_fm`) on `phase1-buffer` (PR #4).
- 2026-10-20 — Phase 1 step 2 (`character_fm`) on `phase1-character` (PR #3).
- 2026-10-20 — Phase 1 step 1 (`error_fm`, `handle_fm`) on `phase1-error-handle` (PR #2), stacked on PR #1.
- 2026-10-20 — Phase 0 scaffold on branch `phase0-scaffold` (PR #1, CI green).
