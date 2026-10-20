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
4. `fm_cmds` (`cmds.c`) — `self-insert-command` and friends (motion and
   deletion done in Phase 1).

### Phase 3 — Terminal display
Minimal `term.c` / `dispnew.c` / `xdisp.c` subset.
**Milestone: open a file, edit it, save it.**

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
6. **Record.** Update the status table, one module per PR.

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
| fm_error | data.c `syms_of_data` error table (subset), fileio.c `file-error`, print.c `print_error_message` layout | status type, codes (incl. `void-function`), `condition_matches`, `format_error` | remaining error symbols (added as modules need them) | test_fm_error | done |
| fm_handle | — (replaces GC'd pointers) | slot table, generational handles | — | test_fm_handle | done |
| fm_buffer_ops (submodule of fm_buffer) | buffer.c | `kill-buffer` (unchains markers, clears the position cache), `erase-buffer` (with inline `widen`) | hooks, windows, processes, overlays, labeled restrictions | test_fm_buffer, test_fm_marker | done |
| fm_buffer | buffer.c, buffer.h (+ intervals.c `temp_set_point_both`, lisp.h `modiff_incr`) | struct buffer_text / struct buffer (subset), registry, `get-buffer`, `get-buffer-create`, `generate-new-buffer-name`, `buffer-name`, `buffer-last-name`, `rename-buffer`, `buffer-list`, `set-buffer`, `kill-buffer` (core), `buffer-modified-p`, `restore-buffer-modified-p`, modified ticks, `barf-if-buffer-read-only`, BUF_BYTE_ADDRESS / FETCH_BYTE / FETCH_CHAR / next/prev char len, BUF_COMPUTE_UNCHANGED | indirect buffers, buffer-local vars, overlays, undo, files/locking, hooks, `autosaved` state, `inhibit-read-only`, `erase-buffer`, marker/window parts of `kill-buffer`, `other-buffer` ordering | test_fm_buffer (ports bug27966 and the file-independent parts of test-buffer-modifications, test-restore-buffer-modified-p, test-buffer-chars-modified-ticks) | done |
| fm_marker | marker.c (+ alloc.c `Fmake_marker`) | conversion with cache and marker hints, `make-marker`, `marker-buffer`, `marker-position`, `marker_byte_position`, `marker-last-position`, `set-marker` (integer and marker positions), `set_marker_restricted(_both)`, `set_marker_both`, `detach_marker`, `unchain_marker`, `copy-marker`, insertion types, `count_markers`; `free_marker` (ours: no GC) | `build_marker` after long scans (would leak without GC), `need_adjustment` | test_fm_marker (marker-tests.el needs windows; its Bug#24368 point is ported as set-marker-from-other-buffer) | done |
| fm_insdel | insdel.c | gap motion, `make_gap` (larger/smaller), `insert`, `insert_char`, `insert_before_markers`, `insert_1_both`, `adjust_point`, `adjust_markers_for_insert/delete`, `del_range`, `del_range_1/_byte/_both/_2`, `modify_text`, `prepare_to_modify_buffer` (read-only check) | intervals, overlays, undo, change hooks, compositions, quit during gap motion, `copy_text`, `insert_from_string/buffer`, `replace_range` | test_fm_insdel (own; no upstream insdel-tests.el) | partial |
| fm_editfns | editfns.c (+ buffer.c `validate_region`, intervals.c `set_point_from_marker`) | `point`, `point-min/max`, `pos-bol`, `pos-eol`, `line-beginning/end-position` (no fields), `buffer-size`, `gap-position/size`, `goto-char` (integer and marker), `point(-min/-max)-marker`, `position-bytes`, `byte-to-position`, `following-char`, `preceding-char`, `bobp`, `eobp`, `bolp`, `eolp`, `char-after/before`, `insert-char` (count), `narrow-to-region`, `widen`, `buffer-substring`, `buffer-string`, `delete-region`, `delete-and-extract-region` | labeled restrictions, save-excursion/-restriction, `format`/`message`, fields, `transpose-regions`, `compare-buffer-substrings`, `subst-char-in-region`, `insert-buffer-substring`, fields (so `line-beginning/end-position` equal `pos-bol/eol` for now), text properties in substrings | test_fm_editfns, test_fm_cmds (own; editfns-tests.el needs format/props/undo/transpose) | partial |
| fm_search | search.c | `find_newline` (both directions, across the gap), `scan_newline`, `scan_newline_from_point`, `find_newline_no_quit`, `find_before_next_newline` | newline cache (region-cache.c), string and regexp search, match data, `replace-match` | test_fm_cmds | partial |
| fm_cmds | cmds.c | `forward-char`, `backward-char` (`move_point`), `forward-line` (incl. excessive counts), `beginning-of-line`, `end-of-line`, `delete-char` | `self-insert-command` (needs the command loop), kill-ring `delete-char`, undo amalgamation, intangible text | test_fm_cmds (ports forward-line-with-bignum from cmds-tests.el) | partial |
| fm_intervals | intervals.c, intervals.h | intervals as a sorted run array: `create_root_interval`, `find_interval`, `split_interval_right/left`, `intervals_equal`, `offset_intervals` (insertion/deletion), `textget` (with `category`), TMEM | balanced tree (only if profiling asks), stickiness/`graft_intervals_into_buffer` (insert-and-inherit), string intervals, `char-property-alias-alist`, `default-text-properties` | test_fm_textprop | partial |
| fm_textprop | textprop.c | `text-properties-at`, `get-text-property`, `put-text-property`, `add-text-properties`, `set-text-properties`, `remove-text-properties`, `remove-list-of-text-properties`, `next/previous(-single)-property-change`, `text-property-any`, `text-property-not-all`, `verify_interval_modification` (read-only, inhibit-read-only, stickiness on insertion), `modify_text_properties` | string properties, overlays (`get-char-property`), undo records, modification hooks, LIMIT t, `add-face-text-property` | test_fm_textprop (ports textprop-interval-immutability) | partial |
| fm_sys_linux (src/sys) | — (glibc/kernel headers) | struct termios / winsize / pollfd, termios flags and c_cc indices, ioctl/open/signal/errno constants, bindings for tcgetattr, tcsetattr, read, write, close, isatty, poll, ioctl, signal, raise, posix_openpt, grantpt, unlockpt, ptsname, open, errno | — | test_fm_sysdep (struct sizes checked on both arches in CI) | done |
| fm_sysdep (src/sys) | sysdep.c (+ dispnew.c SIGWINCH flag, process.c `allocate_pty`) | `emacs_get_tty`, `emacs_set_tty`, the termios part of `init_sys_modes` / `reset_sys_modes` (raw-mode transform as pure `tty_raw_settings`), `get_tty_size`, `set_window_size`, `emacs_read`, `emacs_full_write`, `emacs_write`, `input_available` (poll), SIGWINCH flag, `open_pty` | job control (`narrow_foreground_group`, SIGTSTP/SIGCONT), F_SETOWN/SIGIO, keypad/meta terminfo setup, process groups | test_fm_sysdep (runs on a pty, so CI needs no terminal) | done |
| fm_character | character.c, character.h | encoding table, `char_string`, `multibyte_length`, `string_char_and_length`, `raw_prev_char_len`, raw-byte (eight-bit) conversions, `multibyte_chars_in_text`, `parse_str_as_multibyte`, `str_as_multibyte`, `count_size_as_multibyte`, `str_to_multibyte`, `str_as_unibyte`, `char_resolve_modifier_mask`, small predicates | `chars_in_text` (needs current buffer → fm_buffer); `char_width`/`string-width` and `alphabeticp` etc. (need char-tables → display phase); `translate_char`; Lisp-level DEFUNs | test_fm_character (own; character-tests.el only tests `string-width`, deferred) | done |
| fm_lisp, fm_lread, fm_data, fm_fns | lisp.h, alloc.c, lread.c (obarray), data.c, fns.c | `value_type` (nil, fixnums, symbols, strings, proper lists, buffer/marker/keymap refs), constructors and predicates, obarray (`intern`, `intern-soft`), `eq`, `car`/`cdr`, symbol values, `equal`, `length`, `nth`, `memq`, `member`, `string-lessp`, plists, `get`/`put` | dotted pairs, floats, bignums, vectors, hash tables, function cells | test_fm_values (ports fns-tests length and plist cases, binding-test-set-constant) | partial |
| fm_keymap | keymap.c (+ keymap.el `key-parse`, keyboard.c `reorder_modifiers` subset) | registry with explicit lifetimes, `make-keymap`, `make-sparse-keymap`, `keymapp`, `keymap-parent`, `set-keymap-parent` (cycle check), `copy-keymap`, `access_keymap` (meta→ESC, t defaults, parent and composite prefix maps), `define-key` (with REMOVE, `define_as_prefix`), `lookup-key` (lists of keymaps, too-long), `current-global-map`, `use-global-map`, `current-local-map`, `use-local-map`, `current-active-maps`, `key-binding` (local + global), keyboard.c `parse_modifiers`/`apply_modifiers`/`reorder_modifiers` (symbols), `kbd`, `key-description`, `single-key-description`, `push_key_description` | char-tables/vectors (full keymap is a flag), menu items, `[remap CMD]`, symbol function cells as prefix maps, event ranges, minor-mode / text-property / overriding maps, `where-is-internal`, `describe-*`, `text-char-description`, `map-keymap` | test_fm_keymap (ports keymap-tests: make-keymap, keymapp, keymap-parent, copy-keymap, define-key/undefined, /keyboard-macro, /keymap, lookup-key, /list-of-keymaps, /too-long, removal-inherit, unset-test-remove-and-inheritance, keymap--key-description) | partial |
| fm_callint | callint.c (+ eval.c `commandp`) | command registry (Fortran procedures under symbols, standing in for function cells), `call-interactively`, `commandp`, `prefix-numeric-value`, `current-prefix-arg`, `prefix-arg` | interactive specs (commands read their own arguments), `command-history`, `funcall-interactively` | test_fm_keyboard | partial |
| fm_keyboard | keyboard.c (+ simple.el `command-execute`/`undefined`, macros.c `execute-kbd-macro` simplified) | event queue, `tty_read_avail_input` (meta modes 0/1/2, UTF-8 keyboard decoding), `unread-command-events`, `discard-input`, `input-pending-p`, `read_char` core, `read_key_sequence` (input-decode-map, local-function-key-map, key-translation-map, upper-case and shift fallbacks), `keyremap_step`, `test_undefined`, `command_loop_1` (prefix arg, keyboard macros, error reporting) | echo area/echoing, timers and idle hooks, redisplay, mouse/frame events, recursive edit, macro recording, pre/post-command hooks, help-char, case tables (fallback is ASCII), quit-while-computing, translation functions in keyremaps | test_fm_keyboard (own; keyboard-tests.el and callint-tests.el need Lisp) | partial |

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
5. **Terminal capabilities (blocks Phase 3 output).** Upstream drives
   terminals through terminfo (term.c with tparam.c/terminfo.c over
   ncurses' libtinfo). Options: (a) bind libtinfo's `tgetent`/`tgetstr`/
   `tparm` from src/sys -- closest to upstream, adds a C library
   dependency; (b) parse compiled terminfo files in pure Fortran;
   (c) hardcode ANSI/xterm sequences (simplest; most modern terminals).
   *No default taken: Phase 3 output waits for an answer.*
6. **Upstream LLM policy (FYI).** The emacs-31.1 tree contains an
   `AGENTS.md` stating that GNU Emacs does not accept LLM-generated
   contributions. That governs patches sent to Emacs, not this separate
   GPL project, but it means Fortmacs code (largely Claude-written) should
   never be offered upstream. *Default: read upstream as reference only;
   never propose changes to it.*

## Session log

Resume point for the next working session, newest first.

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
