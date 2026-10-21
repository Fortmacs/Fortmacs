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
1. `fm_error`, `fm_handle` — status/error type and the generational handle
   registry. Foundations with no single upstream file.
2. `fm_character` — UTF-8 encoding and char↔byte conversion
   (`character.c`, `character.h`).
3. `fm_buffer` + `fm_insdel` — gap buffer, insert/delete, create/kill,
   current buffer (`buffer.c`, `buffer.h`, `insdel.c`); plus the
   char↔byte conversion half of `fm_marker` (`marker.c`), which insdel needs.
4. `fm_marker` — marker objects and their adjustment on insert/delete
   (`marker.c`); `kill-buffer`/`erase-buffer` completed in a submodule of
   fm_buffer (which can use fm_marker/fm_insdel without a module cycle).
   Narrowing and `goto-char` come with `fm_editfns` (`editfns.c`, done).
5. Value model — symbols and the minimal value type that text properties,
   keymaps and buffer-local variables need (see Open questions).
6. `fm_intervals` + `fm_textprop` — interval tree and text properties
   (`intervals.c`, `textprop.c`).

### Phase 2 — Command loop and keymaps
1. `src/sys/` termios raw mode — done.
2. `fm_keymap` (`keymap.c`) — done (core; see the status table).
3. `fm_keyboard` (`keyboard.c`) + `fm_callint` (`callint.c`) — event
   reading, `read-key-sequence`, command registry, command loop — done
   (core). Still needed for a usable loop: xterm escape-sequence maps
   (`term/xterm.el` input-decode-map) and echo-area messages.
4. Typing works: `self-insert-command` (`fm_cmds`), basic simple.el
   commands (`fm_simple`), the default global bindings (`fm_bindings`),
   xterm escape sequences (`fm_term_xterm`) — done (core).
5. `app/main.f90` editor loop on a raw tty — needs Phase 3 output to be
   useful.

### Phase 3 — Terminal display
1. `fm_terminfo` — terminfo database reader and `tparm` (pure Fortran) — done.
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

### Phase 5 — Generations, then features
Lightweight numbered generations, `--rollback`, `--list-generations`;
then features in whatever order the user asks for.

## Per-module workflow (definition of done)

1. **Scope.** List the upstream functions being ported and the ones
   deferred (with reason) in the module's status row below.
2. **Tests first.** Port the matching `test/src/*-tests.el` cases to
   test-drive (`test/test_fm_<name>.f90`) where they exist; add our own for
   uncovered behaviour. Name each ported test after its ERT test.
3. **Implement.** One module per upstream C file, `fm_` prefix. Cite the
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
   (`gh api repos/vermi-culous/fortmacs/pulls/N --jq .merged`) and, once it
   is merged, starts the next step on a fresh branch from main; work done
   while waiting is rebased onto main before its PR is opened.

### Header for ported files

```fortran
! fm_<name>.f90 -- <one line>.
! Ported from GNU Emacs 31.1 src/<file>.c.
!
! Copyright (C) <upstream years> Free Software Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: ... (GPLv3-or-later text, as in fm_version.f90)
```

Copy the FSF year range exactly from the upstream file.

## Status

| Module | Upstream | Ported | Deferred | Tests | State |
|---|---|---|---|---|---|
| fm_version | — | — | — | test_fm_version | done |
| fm_error | data.c `syms_of_data` error table (subset), fileio.c `file-error`, print.c `print_error_message` layout | status type, codes (incl. `void-function`, `file-missing`), file-error messages, `condition_matches`, `format_error` | remaining error symbols (added as modules need them) | test_fm_error | done |
| fm_handle | — (replaces GC'd pointers) | slot table, generational handles | — | test_fm_handle | done |
| fm_buffer_ops (submodule of fm_buffer) | buffer.c | `kill-buffer` (unchains markers, clears the position cache), `erase-buffer` (with inline `widen`) | hooks, windows, processes, overlays, labeled restrictions | test_fm_buffer, test_fm_marker | done |
| fm_buffer | buffer.c, buffer.h (+ intervals.c `temp_set_point_both`, lisp.h `modiff_incr`) | struct buffer_text / struct buffer (subset), registry, `get-buffer`, `get-buffer-create`, `generate-new-buffer-name`, `buffer-name`, `buffer-last-name`, `rename-buffer`, `buffer-list`, `set-buffer`, `kill-buffer` (core), `buffer-modified-p`, `restore-buffer-modified-p`, modified ticks, `barf-if-buffer-read-only`, BUF_BYTE_ADDRESS / FETCH_BYTE / FETCH_CHAR / next/prev char len, BUF_COMPUTE_UNCHANGED, `get-file-buffer` (given an expanded name), BVAR filename/directory, `buffer-backed-up`, visited modtime, new buffers inherit `default-directory` | indirect buffers, buffer-local vars, overlays, undo, files/locking, hooks, `autosaved` state, `inhibit-read-only`, `erase-buffer`, marker/window parts of `kill-buffer`, `other-buffer` ordering | test_fm_buffer (ports bug27966 and the file-independent parts of test-buffer-modifications, test-restore-buffer-modified-p, test-buffer-chars-modified-ticks) | done |
| fm_marker | marker.c (+ alloc.c `Fmake_marker`) | conversion with cache and marker hints, `make-marker`, `marker-buffer`, `marker-position`, `marker_byte_position`, `marker-last-position`, `set-marker` (integer and marker positions), `set_marker_restricted(_both)`, `set_marker_both`, `detach_marker`, `unchain_marker`, `copy-marker`, insertion types, `count_markers`; `free_marker` (ours: no GC) | `build_marker` after long scans (would leak without GC), `need_adjustment` | test_fm_marker (marker-tests.el needs windows; its Bug#24368 point is ported as set-marker-from-other-buffer) | done |
| fm_insdel | insdel.c | gap motion, `make_gap` (larger/smaller), `insert`, `insert_char`, `insert_before_markers`, `insert_1_both`, `adjust_point`, `adjust_markers_for_insert/delete`, `del_range`, `del_range_1/_byte/_both/_2`, `modify_text`, `prepare_to_modify_buffer` (read-only check) | intervals, overlays, undo, change hooks, compositions, quit during gap motion, `copy_text`, `insert_from_string/buffer`, `replace_range` | test_fm_insdel (own; no upstream insdel-tests.el) | partial |
| fm_editfns | editfns.c (+ buffer.c `validate_region`, intervals.c `set_point_from_marker`) | `point`, `point-min/max`, `pos-bol`, `pos-eol`, `line-beginning/end-position` (constrained to fields), fields (`find_field`, `field-beginning`, `field-end`, `field-string-no-properties`, `delete-field`, `constrain-to-field`, `get-pos-property`; text properties only), `inhibit-field-text-motion`, `buffer-size`, `gap-position/size`, `goto-char` (integer and marker), `point(-min/-max)-marker`, `position-bytes`, `byte-to-position`, `following-char`, `preceding-char`, `bobp`, `eobp`, `bolp`, `eolp`, `char-after/before`, `insert-char` (count), `narrow-to-region`, `widen`, `buffer-substring`, `buffer-string`, `delete-region`, `delete-and-extract-region` | labeled restrictions, save-excursion/-restriction, `format`/`message`, overlays in fields, INHIBIT-CAPTURE-PROPERTY, `transpose-regions`, `compare-buffer-substrings`, `subst-char-in-region`, `insert-buffer-substring`, text properties in substrings | test_fm_editfns (ports find-fields), test_fm_cmds (own; most of editfns-tests.el needs format/undo/transpose) | partial |
| fm_search | search.c | `find_newline` (both directions, across the gap), `scan_newline`, `scan_newline_from_point`, `find_newline_no_quit`, `find_before_next_newline` | newline cache (region-cache.c), string and regexp search, match data, `replace-match` | test_fm_cmds | partial |
| fm_cmds | cmds.c | `forward-char`, `backward-char` (`move_point`), `forward-line` (incl. excessive counts), `beginning-of-line`, `end-of-line`, `delete-char`, `self-insert-command` (insertion only), command registration (`syms_of_cmds`) | overwrite mode, abbrevs, auto-fill, `post-self-insert-hook`, kill-ring `delete-char`, undo amalgamation, intangible text | test_fm_cmds (ports forward-line-with-bignum from cmds-tests.el), test_fm_bindings | partial |
| fm_intervals | intervals.c, intervals.h | intervals as a sorted run array: `create_root_interval`, `find_interval`, `split_interval_right/left`, `intervals_equal`, `offset_intervals` (insertion/deletion), `textget` (with `category`), TMEM | balanced tree (only if profiling asks), stickiness/`graft_intervals_into_buffer` (insert-and-inherit), string intervals, `char-property-alias-alist`, `default-text-properties` | test_fm_textprop | partial |
| fm_textprop | textprop.c | `text-properties-at`, `get-text-property`, `put-text-property`, `add-text-properties`, `set-text-properties`, `remove-text-properties`, `remove-list-of-text-properties`, `next/previous(-single)-property-change`, `text-property-any`, `text-property-not-all`, `verify_interval_modification` (read-only, inhibit-read-only, stickiness on insertion), `modify_text_properties`, `text_property_stickiness`, `next/previous-single-char-property-change` (without overlays) | string properties, overlays (`get-char-property`), undo records, modification hooks, LIMIT t, `add-face-text-property` | test_fm_textprop (ports textprop-interval-immutability) | partial |
| fm_sys_linux (src/sys) | — (glibc/kernel headers) | struct termios / winsize / pollfd, termios flags and c_cc indices, ioctl/open/signal/errno constants, bindings for tcgetattr, tcsetattr, read, write, close, isatty, poll, ioctl, signal, raise, posix_openpt, grantpt, unlockpt, ptsname, open, errno; file calls (creat, access, rename, unlink, chmod, mkdir, rmdir, statx, getcwd, setenv, unsetenv), user ids and password entries (getuid, geteuid, getegid, getpwnam, getpwuid), strerror | — | test_fm_sysdep (struct sizes checked on both arches in CI) | done |
| fm_sysdep (src/sys) | sysdep.c (+ dispnew.c SIGWINCH flag, process.c `allocate_pty`) | `emacs_get_tty`, `emacs_set_tty`, the termios part of `init_sys_modes` / `reset_sys_modes` (raw-mode transform as pure `tty_raw_settings`), `get_tty_size`, `set_window_size`, `emacs_read`, `emacs_full_write`, `emacs_write`, `input_available` (poll), SIGWINCH and SIGINT flags, `open_pty`, file wrappers returning errno (`emacs_open_read`, `emacs_creat`, access, rename, chmod, mkdir, stat via statx, setenv), `emacs_get_current_dir_name` (prefers a matching $PWD), password-entry home directories, `emacs_strerror` | job control (`narrow_foreground_group`, SIGTSTP/SIGCONT), F_SETOWN/SIGIO, keypad/meta terminfo setup, process groups | test_fm_sysdep (runs on a pty, so CI needs no terminal) | done |
| fm_character | character.c, character.h | encoding table, `char_string`, `multibyte_length`, `string_char_and_length`, `raw_prev_char_len`, raw-byte (eight-bit) conversions, `multibyte_chars_in_text`, `parse_str_as_multibyte`, `str_as_multibyte`, `count_size_as_multibyte`, `str_to_multibyte`, `str_as_unibyte`, `char_resolve_modifier_mask`, small predicates | `chars_in_text` (needs current buffer → fm_buffer); `char_width`/`string-width` and `alphabeticp` etc. (need char-tables → display phase); `translate_char`; Lisp-level DEFUNs | test_fm_character (own; character-tests.el only tests `string-width`, deferred) | done |
| fm_lisp, fm_lread, fm_data, fm_fns | lisp.h, alloc.c, lread.c (obarray), data.c, fns.c | `value_type` (nil, fixnums, symbols, strings, proper lists, buffer/marker/keymap refs), constructors and predicates, obarray (`intern`, `intern-soft`), `eq`, `car`/`cdr`, symbol values, `equal`, `length`, `nth`, `memq`, `member`, `string-lessp`, plists, `get`/`put`, fns.c `yes-or-no-p` (submodule fm_fns_ask; ASCII downcase) | dotted pairs, floats, bignums, vectors, hash tables, function cells | test_fm_values (ports fns-tests length and plist cases, binding-test-set-constant) | partial |
| fm_keymap | keymap.c (+ keymap.el `key-parse`, keyboard.c `reorder_modifiers` subset) | registry with explicit lifetimes, `make-keymap`, `make-sparse-keymap`, `keymapp`, `keymap-parent`, `set-keymap-parent` (cycle check), `copy-keymap`, `access_keymap` (meta→ESC, t defaults, parent and composite prefix maps), `define-key` (with REMOVE, `define_as_prefix`), character ranges (the char-table part of full keymaps), overriding-terminal-local-map, `lookup-key` (lists of keymaps, too-long), `current-global-map`, `use-global-map`, `current-local-map`, `use-local-map`, `current-active-maps`, `key-binding` (local + global), keyboard.c `parse_modifiers`/`apply_modifiers`/`reorder_modifiers` (symbols), `kbd`, `key-description`, `single-key-description`, `push_key_description` | char-tables/vectors (full keymap is a flag), menu items, `[remap CMD]`, symbol function cells as prefix maps, event ranges, minor-mode / text-property / overriding maps, `where-is-internal`, `describe-*`, `text-char-description`, `map-keymap` | test_fm_keymap (ports keymap-tests: make-keymap, keymapp, keymap-parent, copy-keymap, define-key/undefined, /keyboard-macro, /keymap, lookup-key, /list-of-keymaps, /too-long, removal-inherit, unset-test-remove-and-inheritance, keymap--key-description) | partial |
| fm_callint | callint.c (+ eval.c `commandp`) | command registry (Fortran procedures under symbols, standing in for function cells), `call-interactively`, `commandp`, `prefix-numeric-value`, `current-prefix-arg`, `prefix-arg` | interactive specs (commands read their own arguments), `command-history`, `funcall-interactively` | test_fm_keyboard | partial |
| fm_keyboard | keyboard.c (+ simple.el `command-execute`/`undefined`, macros.c `execute-kbd-macro` simplified) | event queue, `tty_read_avail_input` (meta modes 0/1/2, UTF-8 keyboard decoding), `unread-command-events`, `discard-input`, `input-pending-p`, `read_char` core, `read_key_sequence` (input-decode-map, local-function-key-map, key-translation-map, upper-case and shift fallbacks), `keyremap_step`, `test_undefined`, `command_loop_1` (prefix arg, keyboard macros, error reporting), subr.el `set-transient-map` (one key sequence), redisplay before waiting for input (a hook, to keep fm_keyboard below fm_xdisp), `recursive_edit_1` and the `exit` catch (`exit-recursive-edit`, `abort-recursive-edit`), a post-command hook (one function), C-g from SIGINT read as the quit character, subr.el `sit-for` | echoing of prefix keys, timers and idle hooks, mouse/frame events, `recursive-edit` command, macro recording, pre-command-hook and general hooks, help-char, case tables (fallback is ASCII), quit-while-computing, translation functions in keyremaps | test_fm_keyboard (own; keyboard-tests.el and callint-tests.el need Lisp) | partial |
| fm_simple | simple.el | `newline`, `open-line`, `delete-backward-char`, `delete-forward-char`, `keyboard-quit`, `quoted-insert`, `universal-argument` (+ `-more`, `-map`, `--mode`), `digit-argument`, `negative-argument`, `prefix-command-preserve-state`, `next-line`, `previous-line`, `line-move` (`line-move-visual` by screen lines with temporary-goal-column, `line-move-1` by buffer lines), `line-move-finish`, `goal-column`, `line-move-visual`; minibuffer history (`goto-history-element`, `previous-history-element`, `next-history-element`, one default) | mark/region, undo, kill ring, electric indent, auto-fill, hard newlines, prefix echo, octal `read-quoted-char`, `move-beginning/end-of-line`, `set-goal-column`, `next-line-add-newlines`, `track-eol`, vscroll, invisible text in line motion | test_fm_bindings | partial |
| fm_bindings | bindings.el, subr.el (`global-map`, `esc-map`, `ctl-x-map`), international/mule-conf.el (self-insert range) | the default keymaps with every binding whose command is ported (incl. minibuffer-local-map history keys), `function-key-map` (backspace/delete → DEL), `right-char`, `left-char`; C-n, C-p, <down>, <up> | bindings to unported commands, bidi-aware left/right, menus | test_fm_bindings | partial |
| fm_term_xterm | term/xterm.el | `xterm-rxvt-function-map`, `xterm-function-map` (cursor, editing, F1–F12, keypad, all modifier forms, modifyOtherKeys/formatOtherKeys), `terminal-init-xterm` (input-decode-map part) | bracketed paste, focus events, terminal queries, colors, OSC 52, rxvt, `xterm-alternatives-map` | test_fm_term_xterm (own) | partial |
| fm_terminfo | terminfo.c (`tparam`), plus the libtinfo interface Emacs uses, reimplemented | compiled entry parser (16- and 32-bit numbers, extended capabilities), ncurses search path, `tgetent`, `tgetstr`/`tgetnum`/`tgetflag` (termcap codes), `tigetstr`/`tigetnum`/`tigetflag`, `tparm` (full %-language and printf formats), `tgoto`, padding removal, built-in xterm-256color | string parameters (%s), padding delays, the termcap database | test_fm_terminfo (own; checked against infocmp) | done |
| fm_dispextern | dispextern.h | `glyph_type` (character, face id, padding), `face_type` (terminal attributes and colors), face color constants, basic face ids (default, mode line), `face_tty_specified_color` | everything graphical; glyph rows and matrices (with redisplay) | test_fm_term | partial |
| fm_termchar | termchar.h, cm.h (`struct cm`) | `tty_display_info` (capabilities, modes, cursor-motion state), the output buffer (`tty_output`, `tty_flush`) | termscript, mouse highlight, terminal parameters | test_fm_term | partial |
| fm_cm | cm.c, cm.h macros | `cmcostinit`, `calccost` (incl. tabs), `cmgoto`, `cmcheckmagic`, `Wcm_clear`, `Wcm_init`, `cmat`, `cmplus`, `losecursor` | — (where upstream aborts in `cmcheckmagic`, the cursor is forgotten) | test_fm_term | done |
| fm_term | term.c | `init_tty` (capabilities, colors incl. 24-bit, size, raw mode), `delete_tty`, `term_get_fkeys` (terminfo keys into input-decode-map), `calculate_costs` (motion part), bell, terminal modes, scroll region, insert/standout modes, cursor show/hide, `cursor_to`, clearing, `write_glyphs` (incl. the bottom-right cell), `insert_glyphs`, `delete_glyphs`, `ins_del_lines`, `turn_on_face`/`turn_off_face`, `encode_terminal_code` (UTF-8) | glyph production (with redisplay), menus, mouse/GPM, colour-mode switching, char/line ins-del cost tables, tty-mode strings, DOS/Windows/Android | test_fm_term (own; byte-exact xterm output, a pty) | partial |
| fm_frame | frame.h | `frame_type` (terminal, current and desired matrices, face table, cursor, root and minibuffer windows, which is selected), `selected_frame` (frame.c) | window splitting, child frames, frame parameters | test_fm_dispnew, test_fm_xdisp | partial |
| fm_dispnew | dispnew.c | frame matrices (`adjust_frame_glyphs`, clear current/desired), `redraw_frame`, `write_row` (prefix/suffix matching, character insertion and deletion by cost), `write_matrix` (last row first), `make_matrix_current`, `tty_set_cursor`, `update_frame` (terminal path); term.c `calculate_ins_del_char_costs` | line scrolling (scroll.c), window matrices, pausing for input, size changes, child frames, menus | test_fm_dispnew (own; byte-exact xterm output) | partial |
| fm_window | window.c, window.h | `struct window` (subset: buffer, start and point markers, geometry, cursor, hscroll), `set-window-buffer`, `window-start`, `set-window-start` (incl. NOFORCE), `window-point`, `set-window-point`, `window-body-height` (lines), freeing a window's markers | window trees and splitting, `selected-window`, scrolling commands, window parameters, dedication, `window-end` | test_fm_xdisp | partial |
| fm_xdisp | xdisp.c (+ term.c `produce_glyphs` terminal part, xfaces.c `realize_basic_faces` subset) | `redisplay` (one window, recentering when point leaves the window), `redisplay_window` core, glyph production for terminals (TAB, `^X` and `\ooo` control/raw bytes, wide characters, `\` continuation), `display_mode_line` (fixed default format), echo area (`message`, `cursor-in-echo-area`), the active minibuffer on one line with auto-hscroll and `$` marks, line counting for `L`, `glyph_columns` (display width at a column) | mode-line-format interpretation, faces from text properties, truncation, horizontal scrolling of other windows, multi-line minibuffers (resize-mini-windows), the minibuffer-prompt face, bidi, `scroll-margin`/`scroll-conservatively`, display properties, overlays, fringes, header/tab lines | test_fm_xdisp (own; xdisp-tests.el needs windows and Lisp) | partial |
| fm_files | files.el (+ startup.el `command-line-1` file arguments are in fm_emacs) | `find-file-noselect` (with `find-file-noselect-1`, `after-find-file` messages and read-only), `find-file` (for programs), `create-file-buffer`, `abbreviate-file-name`, `backup-buffer` (rename or copy to `FILE~`, keeping modes), `make-backup-file-name`, `normal-backup-enable-predicate`, `file-ownership-preserved-p`, `save-buffer` (prefix args), `basic-save-buffer` (modtime check, write-protected files), `save-some-buffers`, `save-buffers-kill-emacs`, `save-buffers-kill-terminal`, `set-visited-file-name`, `write-file`, `find-file` and `write-file` commands (via submodule fm_files_read); `C-x C-f`, `C-x C-w`, `C-x C-s`, `C-x s`, `C-x C-c` | file name completion, wildcards and dired, creating missing directories, auto-save, file locks, major modes and local variables, hooks, uniquify, numbered backups and `backup-directory-alist`, `file-precious-flag`, symlink chasing (a symlink is backed up by copying), revert, truenames | test_fm_files (own; files-tests.el needs file name handlers, the minibuffer and hooks) | partial |
| fm_fileio | fileio.c (+ dired.c `file-attributes` subset, editfns.c `user-uid`/`group-gid`) | `file-name-directory`, `file-name-nondirectory`, `file-name-as-directory`, `directory-file-name`, `directory-name-p`, `file-name-absolute-p`, `expand-file-name` (`~`, `~USER`, relative default directories, null-byte check), `get_homedir`, `user_homedir`, `file-exists-p`, `file-readable-p`, `file-writable-p`, `file-directory-p`, `file_accessible_directory_p`, `file-regular-p`, `file-symlink-p` (as a truth value), `file-modes`, `set-file-modes`, `rename-file`, `copy-file` (modes only), `make-directory-internal`, `delete-directory-internal`, `delete-file-internal`, `insert-file-contents` (with VISIT; UTF-8 with raw bytes), `write-region` (whole buffer or region, VISIT), `verify-visited-file-modtime`, `set-visited-file-modtime`, `report_file_errno`, `substitute-in-file-name` (with env.el `substitute-env-in-file-name`) | file name handlers, coding systems and EOL conversion, BEG/END/REPLACE, APPEND/MUSTBENEW/LOCKNAME, fsync, `file-truename`, `file-newer-than-file-p`, `make-temp-file`, ACLs and SELinux, `file-name-concat` | test_fm_fileio (ports directory-file-name, file-name-as-directory, relative-HOME, relative-default-directory, expand-file-name-null-bytes, file-name-absolute-p) | partial |
| fm_subr | subr.el | `read-key` (single events), `y-or-n-p` (the `y-or-n-p-use-read-key` path), the `query-replace-map` answers (replace.el), `add-to-history` | the minibuffer path of `y-or-n-p` (the read-key one is kept), help-form, scrolling answers, `ding` | test_fm_files | partial |
| fm_map_ynp | emacs-lisp/map-ynp.el | `map-y-or-n-p` (read-key path: y/SPC, n/DEL, !, ., q/RET, C-g) with Fortran prompter and actor procedures | help (C-h), ACTION-ALIST, the minibuffer path, dialog boxes | test_fm_files | partial |
| fm_minibuf | minibuf.c | `read_minibuf` (prompt as a read-only field, minibuffer window selected, recursive edit, history, default), `read-from-minibuffer`, `read-string`, `get_minibuffer`, `minibufferp`, `minibuffer-prompt-end`, `minibuffer-contents-no-properties`, `abort-minibuffers`, `minibuffer-local-map`, `enable-recursive-minibuffers`, `history-add-new-input`; the prompt's cursor-intangible as a post-command hook | completion (`try-completion`, `all-completions`, `completing-read`), `read-minibuffer`/`eval-minibuffer`, minibuffer-only frames, input methods, minibuffer-setup/exit hooks, window configurations | test_fm_minibuf (own; minibuf-tests.el tests completion) | partial |
| fm_minibuffer | minibuffer.el | `exit-minibuffer`, `delete-minibuffer-contents`, `read-file-name` (read-file-name-default: abbreviated directory as input, default file, `substitute-in-file-name`), `insert-default-directory`, minibuffer-local-map's C-g/C-j/RET | completion (TAB, *Completions*, styles), `read-file-name` MUSTMATCH/PREDICATE, file name shadowing | test_fm_minibuf | partial |
| fm_env | env.el | `getenv` (process environment), `substitute-env-vars` (WHEN-UNDEFINED nil or t) | `setenv`, `process-environment`, frames' environments | test_fm_fileio | partial |
| fm_indent | indent.c | `current-column`, `move-to-column` (no FORCE), `vertical-motion` (LINES or (COLS . LINES), in the selected window, continuing lines as redisplay does), point's screen column (as posn-at-point gives it) | invisible text, display properties, selective display, truncated and horizontally scrolled lines, `indent-to`, `current-indentation`, `compute_motion` | test_fm_indent (own; indent-tests.el needs invisible text) | partial |
| fm_emacs | emacs.c `main` (+ startup.el `command-line` terminal part and `command-line-1` file arguments, faces.el `tty-run-terminal-initialization`, buffer.c `init_buffer` directory) | `emacs_main`: bindings, `*scratch*` (in the starting directory), terminal setup (incl. xterm init), windows and faces, raw mode, visiting command-line files (`+LINE`, `--`, `-file`/`-find-file`/`-visit`), the command loop, restoring the terminal | other options, `+LINE:COLUMN` columns (need indent.c), splitting windows for two files, init files (the config API), daemon, dumping, signals | test_fm_xdisp (runs the editor on a pty), test_fm_files | partial |

## Style

- The [fortran-lang style guide](https://fortran-lang.org/learn/best_practices/style_guide/):
  lowercase keywords and names, words separated by underscores, short but
  unambiguous names. Like the [stdlib guide](https://stdlib.fortran-lang.org/page/contributing/StyleGuide.html):
  `intent` on every dummy, named `end` statements, one module per file,
  FORD `!>` comments on public entities, 4-space indentation; but 132
  columns (stdlib's hard limit) rather than 80.
- Enforced by `tools/check.sh` (and so CI): fprettify for layout
  (`.fprettify.rc`), Fortitude 0.9.2 with its default rules for the rest
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
- Tests of the src/sys layer may use ISO_C_BINDING and fm_sys_linux
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
  check `buffers(i)%serial` (see the regression test in test_fm_insdel).
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
  command loop (`fm_keyboard`) or the first feature that needs it.

## Open questions

Decisions the porting work is waiting on, or defaults taken provisionally.
Each lists the default being used meanwhile.

1. **Merging.** *Decided:* one roadmap step per PR, based on main (never
   stacked); the user reviews and merges.
2. **Value model.** *Decided (2026-10-20):* no Lisp; values are a plain
   Fortran `value_type` with value semantics (fm_lisp), interned symbols
   by id (fm_lread), data.c/fns.c operations in fm_data/fm_fns. Values are
   stored as flat preorder atom arrays because gfortran 13/14 copy
   recursive allocatable components shallowly (double free).
3. **Error-code naming.** Error codes are `FM_<SYMBOL>` parameters, except
   the root `error` condition, which is `FM_GENERIC_ERROR` because Fortran
   names are case-insensitive and `FM_ERROR` collides with module
   `fm_error`. *Default: keep as is.*
4. **Buffer internals visibility.** `fm_buffer` exposes `buffers(:)` and
   the current slot `cur` (read-only outside the module) so fm_insdel and
   fm_marker can work like upstream's buffer.h macros. *Default: keep;
   higher layers use the procedures only.*
5. **Terminal capabilities.** *Decided (2026-10-20): (b), pure Fortran.*
   Upstream drives terminals through ncurses' libtinfo; Fortmacs reads
   the compiled terminfo files itself and evaluates `tparm` strings
   (`fm_terminfo`), with a built-in xterm-256color entry as fallback.
6. **Upstream LLM policy (FYI).** The emacs-31.1 tree contains an
   `AGENTS.md` stating that GNU Emacs does not accept LLM-generated
   contributions. That governs patches sent to Emacs, not this separate
   GPL project, but it means Fortmacs code (largely Claude-written) should
   never be offered upstream. *Default: read upstream as reference only;
   never propose changes to it.*

## Session log

Resume point for the next working session, newest first.

- 2026-10-21 — Phase 3.5: moving between lines (`fm_indent`; `next-line`/`previous-line` in fm_simple, by screen lines with the goal column; C-n, C-p, <down>, <up>), on `phase3-lines` (based on main after PR #26). Phase 3 is complete. Next by the roadmap: Phase 4, the Fortran config API (`init.f90`, `fortmacs --recompile`).
- 2026-10-21 — README: progress as a table of vertical SVG bars (`docs/progress/*.svg`, drawn by `tools/progress.sh --update-readme`; Total = Emacs + packages, Emacs with C and Elisp, Packages with each archive), a "Real parallel threads" section, the syntax point last with a deeper example, "Why not?" removed. On `docs-progress-table`.
- 2026-10-21 — Phase 3.4b: the minibuffer (`fm_minibuf`, `fm_minibuffer`, `fm_env`; recursive edit, fields, `yes-or-no-p`, history, `C-x C-f`, `C-x C-w`; C-g on the terminal no longer kills the editor — SIGINT now reads as C-g), on `phase3-minibuf` (based on main after PR #24). Next: line motion (`next-line`/`previous-line`, which need indent.c columns), then scroll commands; completion later.
- 2026-10-21 — Guix CI fixed: the installer moved to guix.gnu.org/install.sh, pulls come from Codeberg, and the build shell gets nss-certs and git (curl and fpm need TLS). A run on the branch passed. On `ci-guix-main`.
- 2026-10-21 — README: progress shown as bars, now including the packages in GNU ELPA, NonGNU ELPA and MELPA (`tools/progress.sh --update-readme`; totals in `tools/package-totals`, recounted by `--fetch-packages` and the weekly Package totals workflow; ported packages listed in `tools/ported-packages`); syntax and "why not" in Why Fortran; no pip instructions. On `docs-progress`. Next: Phase 3.4b (minibuffer), WIP on `phase3-minibuf`.
- 2026-10-21 — Phase 3.4a: visiting and saving files (`fm_fileio`, `fm_files`, `fm_subr`, `fm_map_ynp`; command-line files in `fm_emacs`; file bindings in src/sys; `file-missing`), on `phase3-files` (based on main after PR #21). Milestone reached: `fortmacs FILE`, edit, `C-x C-s`. Next: Phase 3.4b, a minimal minibuffer (`C-x C-f`, `yes-or-no-p`).
- 2026-10-21 — Phase 3.3b: redisplay and a runnable editor (`fm_window`, `fm_xdisp`, `fm_files`, `fm_emacs`, redisplay hook in fm_keyboard, `emacs_isatty`), on `phase3-redisplay` (based on main after PR #20). Running `fortmacs` in a terminal edits `*scratch*` with a mode line and echo area; `C-x C-c` exits. Next: Phase 3.4, file visiting and saving; then `next-line` (indent.c columns) and scroll.c line scrolling.
- 2026-10-21 — Style and CI: Fortitude lint gate (all code fixed), 4-space indentation (stdlib), CI on Fedora (x86-64, aarch64) and Arch (x86-64) containers, a weekly Guix job (`.github/workflows/guix.yml`, also runnable by hand; first run unverified), on `style-ci`. Proposed instruction changes (pure Fortran incl. X11/fonts, terminal first then X11, distros, style, built-in docs) are awaiting the user's confirmation.
- 2026-10-21 — Frame updates (`fm_frame`, `fm_dispnew`, ins/del character costs) on `phase3-dispnew` (based on main after PR #18). WIP kept aside: `fm_window` (window.c subset) for the next step. Next: `xdisp.c`/`window.c` subset; a style/CI PR (Fortitude lint gate, Fedora and Arch containers).
- 2026-10-20 — Phase 3.2 terminal output (`fm_dispextern`, `fm_termchar`, `fm_cm`, `fm_term`), `tabs_safe_p` in fm_sysdep, PR-polling workflow recorded; on `phase3-term` (based on main after PR #17). Next: Phase 3.3, the redisplay subset (glyph matrices, one window, mode line, echo area).
- 2026-10-20 — Open question 5 decided (pure-Fortran terminfo); `fm_terminfo` on `phase3-terminfo` (based on main after PRs #15, #16). Pure-Fortran rule recorded under design decisions. Next: Phase 3.2, `term.c`/`cm.c` output on top of it.
- 2026-10-20 — Typing: `self-insert-command`, `fm_simple`, `fm_bindings`, `fm_term_xterm`, keymap character ranges and transient maps; progress.sh now counts cited Lisp definitions; on `phase2-bindings` (based on main after PR #14). Next: the editor binary needs Phase 3 output, which waits on open question 5; meanwhile `move-beginning/end-of-line` and `next-line` (needs indent.c columns), or the mark/region.
- 2026-10-20 — Command loop (`fm_keyboard`, `fm_callint`), `void-function` error, `(error MSG)` formats as MSG, on `phase2-keyboard` (based on main after PR #13). Next: xterm input-decode-map (port of `term/xterm.el` key maps) and `self-insert-command` + default global bindings (`bindings.el` subset), then the `app/main.f90` loop on a raw tty. Terminal output still waits on open question 5.
- 2026-10-20 — Phase 2 keymaps (`fm_keymap`), buffer-local keymap slot, threading decision recorded, on `phase2-keymap` (based on main after PR #12). Next: `fm_keyboard` subset (command loop: read events from the tty, look them up with `key_binding`, run Fortran command procedures), then `self-insert-command`. Terminal output still waits on open question 5.
- 2026-10-20 — Text properties (`fm_intervals`, `fm_textprop`), read-only text in insdel, `inhibit-read-only` in barf, on `phase1-textprop` (PR #12). Next: strings with properties / buffer-substring with properties, or Phase 2 keymaps.
- 2026-10-20 — Value model (fm_lisp, fm_lread, fm_data, fm_fns), valgrind CI gate, progress script, on `phase1-values` (based on main). Open: spurious gfortran `-Wmaybe-uninitialized` warnings in fm_lisp (no failures). Next: intervals/text properties.

- 2026-10-20 — `fm_search` newline scanning, line positions, `fm_cmds` motion/deletion on `phase1-search-cmds`, stacked on PR #7. Work that doesn't need an answer to open questions 2 (value model) or 5 (terminal capabilities) is now done; keymaps, the command loop, text properties and terminal output all wait on them.
- 2026-10-20 — `fm_editfns` subset on `phase1-editfns` (PR #7).
- 2026-10-20 — Phase 2 groundwork: src/sys (`fm_sys_linux`, `fm_sysdep`) on `phase2-sys` (PR #6), stacked on PR #5. Phase 1 step 5 waits on open question 2. Next without it: keyboard input decoding (`fm_keyboard` subset: reading bytes into events, terminal escape sequences come with `term.c`), or `fm_editfns` (point, narrowing, `buffer-substring`).
- 2026-10-20 — Phase 1 step 4 (marker objects, marker adjustment, `kill-buffer`/`erase-buffer` submodule) on `phase1-markers` (PR #5).
- 2026-10-20 — Phase 1 step 3 (`fm_buffer`, `fm_insdel`, conversion half of `fm_marker`) on `phase1-buffer` (PR #4).
- 2026-10-20 — Phase 1 step 2 (`fm_character`) on `phase1-character` (PR #3).
- 2026-10-20 — Phase 1 step 1 (`fm_error`, `fm_handle`) on `phase1-error-handle` (PR #2), stacked on PR #1.
- 2026-10-20 — Phase 0 scaffold on branch `phase0-scaffold` (PR #1, CI green).
