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
1. `error_mod`, `handle_mod` — status/error type and the generational handle
   registry. Foundations with no single upstream file.
2. `character_mod` — UTF-8 encoding and char↔byte conversion
   (`character.c`, `character.h`).
3. `buffer_mod` + `insert_delete_mod` — gap buffer, insert/delete, create/kill,
   current buffer (`buffer.c`, `buffer.h`, `insdel.c`); plus the
   char↔byte conversion half of `marker_mod` (`marker.c`), which insdel needs.
4. `marker_mod` — marker objects and their adjustment on insert/delete
   (`marker.c`); `kill-buffer`/`erase-buffer` completed in a submodule of
   buffer_mod (which can use marker_mod/insert_delete_mod without a module cycle).
   Narrowing and `goto-char` come with `editing_functions_mod` (`editfns.c`, done).
5. Value model — symbols and the minimal value type that text properties,
   keymaps and buffer-local variables need (see Open questions).
6. `intervals_mod` + `text_properties_mod` — interval tree and text properties
   (`intervals.c`, `textprop.c`).

### Phase 2 — Command loop and keymaps
1. `src/sys/` termios raw mode — done.
2. `keymap_mod` (`keymap.c`) — done (core; see docs/status/keymap_mod.md).
3. `keyboard_mod` (`keyboard.c`) + `execute_interactively_mod` (`callint.c`) — event
   reading, `read-key-sequence`, command registry, command loop — done
   (core). Still needed for a usable loop: xterm escape-sequence maps
   (`term/xterm.el` input-decode-map) and echo-area messages.
4. Typing works: `self-insert-command` (`editing_commands_mod`), basic simple.el
   commands (`simple_mod`), the default global bindings (`bindings_mod`),
   xterm escape sequences (`terminal_xterm_mod`) — done (core).
5. `app/main.f90` editor loop on a raw tty — needs Phase 3 output to be
   useful.

### Phase 3 — Terminal display
1. `terminfo_mod` — terminfo database reader and `tparm` (pure Fortran) — done.
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
   `user_configuration` module's `configure`, run at startup (`-q` skips it);
   `keymap-global-set`, `key-valid-p`. — done
2. `fortmacs --recompile`: build `~/.config/fortmacs/init.f90` with
   gfortran against the installed library (`execute_process_mod` runs the
   compiler) into `~/.local/state/fortmacs`, scratch in
   `~/.cache/fortmacs`; the installed binary runs the built one. — done

### Phase 5 — Generations, then features
1. Numbered generations of the configuration in
   `~/.local/state/fortmacs/generations` (each with its binary and the
   init.f90 it was built from, `current` a symbolic link), `--list-generations`,
   `--rollback`, `--switch-generation N`. — done
2. Features, in this order unless the user asks otherwise:
   1. The mark, the region and the clipboard history (C-SPC, C-w, M-w, C-k, C-y,
      M-y, region highlighting). — done
   2. Undo (C-/, C-_, C-x u, C-?). — done
   3. Scrolling and recentering (C-v, M-v, C-l). — done
   4. Incremental search (C-s, C-r). — done
   5. M-x and minibuffer completion. — done
   6. Built-in help (C-h f, C-h k, C-h v) with the docstrings compiled in. — done
   7. Word and paragraph motion (syntax tables). — done
   8. Buffers and windows (C-x b, C-x k, C-x 2, C-x 3, C-x o). — done
   9. Regular expressions (regex-emacs.c; `re-search-forward`,
      `re-search-backward`, `looking-at`, `string-match`, the match data,
      C-M-s and C-M-r; paragraph-start and -separate as regexps). — done
   10. Query replace (M-%, C-M-%, `replace-string`, `replace-regexp`). — done
   11. Case, transposition and whitespace (M-u, M-l, M-c, C-x C-u, C-x C-l,
       C-t, M-t, C-x C-t, M-SPC, M-\, C-o, C-x C-o, M-^). — done
   12. Indentation and filling (TAB, `indent-rigidly`, M-q, `auto-fill-mode`,
       `fill-column`). — done
   13. Major and minor modes (buffer-local variables, mode hooks,
       `fundamental-mode`, `text-mode`, `prog-mode`). — done
   14. Text properties and faces in redisplay (font-lock core), in two
       steps:
       - a. Faces: faces.el's faces configured as Fortran objects, terminal
         colors, faces realized for the terminal, text properties' faces,
         the region, mode lines and the minibuffer prompt in redisplay —
         done.
       - b. Font-lock core: font-lock.el's faces, `font-lock-mode` and
         `global-font-lock-mode`, syntactic and keyword fontification. — done.
   15. Rectangles and registers (C-x r). — done
   16. Keyboard macros (C-x (, C-x ), C-x e). — done
   17. Dired. — done (dired.el; dired-aux.el's file operations and view.el
       came with step 21, with shell commands, printing, diffs and searching Deferred)
   18. Processes on worker threads (M-!, M-|, `compile`). — done
   19. Fake GUI output device for tests (redisplay device independence). — done
   20. Comint: M-x shell, asynchronous shell commands, compilation errors and
       `next-error`. — done (see command_interpreter_mod, shell_mod, compile_mod and
       simple_next_error_mod for what is Deferred).
   21. Dired file operations (dired-aux.el) and view-mode. — done (core; `!`, `&`, diffs and Wdired are Deferred)
   22. More help commands and `set-variable` (C-h m, C-h b, C-h e, C-h l,
       C-h o, C-h x, C-h a, C-h d, C-h u, C-h C-h; *Messages*; M-x
       set-variable with Fortran literals). — done (core; Info, Outline in
       *Help* and help-mode navigation are Deferred; see help_mod,
       help_functions_mod, apropos_mod and simple_set_variable_mod).
   23. Parenthesis matching and electric modes (Show Paren mode, blinking
       parens, Electric Indent mode, Electric Pair mode; timers and
       overlays for them). — done (core: Electric Layout and Electric Quote
       modes are Deferred; see parenthesis_mod, electric_mod and electric_pair_mod).
   24. Abbrevs and dabbrev (abbrev tables, Abbrev mode, C-x a, C-x ', M-',
       M-/, saving abbrevs into init.f90). — done (core: see abbreviation_mod and
       dynamic_abbreviation_mod for what is Deferred; hippie-exp.el is Deferred).
   25. Faces in the config API: `faces%`, `face_underline`, `face_box` (the
       design decision of 2026-10-24). — done (a number as an underline's
       `:position` is Deferred with the GUI).
   26. Customize (custom.el, wid-edit.el, cus-edit.el: about 13,000 lines
       upstream), in three PRs: 26a each user option's custom type (from its
       defcustom) with `customize-set-variable`, `customize-save-variable`
       and `custom-save-all` writing Fortran assignments into init.f90;
       26b the widget library (wid-edit.el core); 26c the Customize
       buffers (`customize`, `customize-variable`, `customize-face` saving
       `faces%` assignments, groups). See open question 8.
   27. Coding systems (coding.c): decoding and encoding for files and
       processes, `buffer-file-coding-system`. — done (core: UTF-8,
       UTF-16, Latin-1, ASCII and raw text with their line ends; see
       coding_mod and multilingual_environment_mod for what is Deferred).
   28. Completion: more minibuffer completion styles and
       `completion-at-point` (then `dabbrev-completion`, Comint and shell
       completion). — done (the styles, categories, `completion-at-point`
       and `completion-in-region-mode`, `dabbrev-completion`; Comint and
       shell completion moved to step 38).
   29. Display properties in redisplay: `invisible`, `display`,
       `before-string` and `after-string` on overlays and text. — done
       (string and space display specs; the other specs are Deferred).
   30. Major modes: Outline mode, f90-mode, sh-script, conf-mode (first
       newcomment.el, the commenting commands they all use).
   31. diff.el, Diff mode and VC with vc-git (Dired's `=`, VC renames).
       About 20,000 lines upstream, so three PRs: 31a diff.el and
       diff-mode.el (`diff`, `diff-buffer-with-file`, Diff mode's hunk and
       file motion, `diff-apply-hunk`, `diff-goto-source`, Dired's `=`);
       31b vc-hooks.el, vc-dispatcher.el, vc.el and vc-git.el (the mode
       line, `vc-next-action` C-x v v, `vc-diff`, `vc-revert`,
       `vc-rename-file`);
       31c log-edit.el (Log Edit mode, committing: `vc-checkin`,
       `vc-git-checkin`); 31d log-view.el (`vc-print-log` C-x v l,
       `vc-print-root-log` C-x v L, Log View mode); 31e vc-dir.el
       (C-x v d). — done.
   32. Auto-revert, with file notifications through inotify in src/sys.
       — done (local files; Dired's `buffer-stale-function` and remote
       files are Deferred).
   33. Small everyday packages, six PRs: 33a repeat.el (`repeat` C-x z,
       `repeat-mode` and the repeat maps of ported commands); 33b
       `rectangle-mark-mode`; 33c savehist and saveplace (saved state in
       namelist syntax); 33d recentf; 33e bookmarks (bookmark.el without
       the bookmark list); 33f header lines, tabulated-list.el and the
       bookmark list (C-x r l). — done.
   34. The Info reader (C-h i and the manual commands of help), as six
       PRs: 34a the reader (info.el: finding and reading Info files,
       tag tables and split files, the directory node, Info mode and
       its node commands, C-h i, C-h r) — done; 34b the display of
       nodes (`Info-fontify-node`, the header line and breadcrumbs,
       the faces) with the table of contents (T) and history (L)
       nodes — done; 34c searching (`Info-search`, s, S), the indexes
       (`Info-index` i, `Info-index-next` ,, `Info-virtual-index` I,
       `info-apropos`) and the help commands that use them (C-h F,
       C-h K) — done; 34d the manuals: an awk converter in tools/
       from the Texinfo sources of the upstream manuals to Info files
       installed with Fortmacs (the Emacs manual as the Info file
       `fortmacs`, R6-32), with "Deferred in Fortmacs" notes (see
       the design decision "The manuals"); 34e Isearch across nodes — done
       (isearch.el's `isearch-search-fun-function`,
       `isearch-wrap-function`, `isearch-push-state-function` and
       `isearch-filter-predicate`); 34f info-look.el (C-h S,
       `info-lookup-symbol`) and `info-display-manual` (C-h R) — done.
   35. Grep (grep.el: `grep`, `lgrep`, `rgrep`, `grep-find`, Grep mode on
       compile_mod and next-error). — done (Grep-Edit mode came with step 37).
   36. Buffer lists: `list-buffers` (C-x C-b, buff-menu.el) and Ibuffer, as
       three PRs: 36a the Buffer Menu — done; 36b Ibuffer (ibuffer.el,
       ibuf-macs.el and ibuf-ext.el but the filters) — done; 36c Ibuffer's
       filters and filter groups (ibuf-ext.el, with crm.el) — done.
   37. Occur (`occur`, `multi-occur`, Occur mode and its edit mode).
       — done (with the keep-lines family and Grep-Edit mode).
   38. Comint and shell, the rest: history expansion and M-r,
       completion (`comint-completion-at-point`, file names through
       `completion-table-with-quoting`, the `:exit-function` property,
       shell's completion functions; moved here from step 28), password
       prompts, `ansi-color`, C-c C-z,
       `list-processes` and the process menu.  As six PRs: 38a history
       expansion (`comint-replace-by-expanded-history`, C-c .), Isearch
       in the input history (M-r), C-c C-z and
       `comint-scroll-to-bottom-on-input` — done; 38b password prompts
       (`comint-watch-for-password-prompt`, `comint-send-invisible`,
       auth-source.el's `read-passwd` and `read-passwd-mode`) — done; 38c
       `ansi-color` — done; 38d `list-processes` and the process
       menu — done; 38e completion (minibuffer.el's
       `completion-table-with-quoting` and `:exit-function`, comint's
       file name completion and input ring completion) — done; 38f
       pcomplete.el and shell's completion functions — done.
   39. The command loop's wrapper commands and C-x RET c
       (`universal-coding-system-argument`), with the process, terminal
       and keyboard coding system commands (C-x RET p, t, k), as two PRs:
       39a C-x RET c (the command loop's `this_command_wrapper`,
       `prefix-command-preserve-state-hook`) and C-x RET p — done; 39b
       the terminal and keyboard coding systems (C-x RET t, k: encoding
       terminal output and decoding keyboard input) — done.

   40. More major modes, one PR each, after the basic editing commands
       found missing: 40a `makefile-mode` (with its variants) — done;
       40b the basic editing commands (lisp.el's subprogram and pair commands
       C-M-a, C-M-e, C-M-h, C-x n d, M-(; the narrowing commands C-x n n,
       w, g, p; M-g g, c, TAB; M-=; page.el's C-x C-p, C-x l) — done;
       40c `sh-mode` beyond step 30 (completion, the inferior shell,
       styles) — done; 40d skeleton.el with sh-mode's statements and
       smie-config — done (HTML mode's completion at point deferred); 40e `python-mode` (core) — done; 40f `css-mode` and
       `scss-mode` — done; 40g `js-mode` — done (filling and JSX deferred);
       40h `sgml-mode`/`html-mode` (core) — done (its skeleton parts
       came with 40d).
   41. Charsets and the other coding systems: charset maps (ISO-8859-x,
       Windows code pages, KOI8), ISO-2022, Shift-JIS, Big5, emacs-mule,
       language environments (C-x RET l) and input methods (`C-\`).  As
       eight PRs: 41a charsets (charset.c, mule-conf.el's charsets, the
       one-byte maps) — done; 41b the charset coding systems (coding.c's
       charset type, the one-byte coding systems of language/*.el,
       `define-coding-system`) — done; 41c the CJK maps with Shift-JIS
       and Big5 — done; 41d ISO-2022 (with EUC, the iso-2022-jp family
       and compound text) — done; 41e emacs-mule — done; 41f language environments
       (C-x RET l, `describe-language-environment`) — done; 41g
       `set-locale-environment` (at startup too) — done; 41h mule-diag.el's
       charset commands (`list-character-sets`, `describe-character-set`,
       `list-charset-chars`) — done; 41i the input method framework
       (`input-method-function` in the keyboard, `register-input-method`,
       C-\, C-x RET C-\, C-x \, C-h I) — done; 41j Quail and its input
       methods.
   42. Terminal core C: the C files the terminal editor rests on,
       finished file by file to everything but what is Not applicable
       (the Lisp engine: allocation and GC, the reader and printer,
       bytecode, the dumper, modules) or waits for the GUI, each listed
       in its module's status file.  In dependency order: 42a buffer.c,
       insdel.c, marker.c, intervals.c, textprop.c, itree.c; 42b
       search.c, syntax.c, casefiddle.c, casetab.c, chartab.c,
       category.c (done); 42c keyboard.c, keymap.c, minibuf.c, callint.c,
       cmds.c (done); 42d window.c, dispnew.c, term.c, the terminal parts of
       xdisp.c, bidi.c (done); 42e fileio.c, process.c, callproc.c, sysdep.c,
       timefns.c, floatfns.c; 42f the parts of fns.c, editfns.c and
       data.c that are not the Lisp engine, with the Gnulib digests
       fns.c uses (lib/md5.c, sha1.c, sha256.c, sha512.c and sha3.c as
       md5_mod, sha1_mod, sha256_mod, sha512_mod and sha3_mod; chunk
       5).  Before it: the citation
       burn-down of tools/audit-baseline (so the remaining functions are
       known exactly) — done, then the rest of steps 40 and 41 (skeletons,
       input methods).  External libraries (treesit.c, json.c, gnutls.c,
       sqlite.c, dbusbind.c) are decided separately: only libc, FreeType
       and HarfBuzz may be bound.
   43. Spelling: `ispell` and `flyspell` over an external speller process.
       43a ispell.el (M-$, `ispell-region`, `ispell-buffer`, the choices
       buffer, personal and per-file dictionaries, Aspell discovery) —
       done; 43b flyspell.el (`flyspell-mode`, `flyspell-prog-mode`, the
       highlighting, `flyspell-region` with large regions through the
       speller's list run, M-TAB auto-correction) — in progress (the
       correction menu of C-c $ waits for TTY menus).
   44. Terminal frames: several frames on one terminal (C-x 5), and
       `tab-bar-mode`/`tab-line-mode`: 44a terminal frames and C-x 5 —
       done; 44b tab-bar.el on terminal frames (the tab bar line, tabs,
       C-x t, tab history; see docs/status/tab_bar_mod.md) — done; 44c
       tab-line.el on terminal frames (the tab line of each window,
       Global Tab-Line mode, switching, moving and closing tabs, grouping;
       see docs/status/tab_line_mod.md) — done.
   45. Windows, the rest: `winner-mode`, `windmove`, `follow-mode`,
       `window-divider` and side windows.
   46. Calendar and diary, in parts: 46a the calendar core (calendar.el
       and cal-move.el: M-x calendar, Calendar mode, movement and
       scrolling) — done; 46b the holidays (holidays.el, with the
       Julian and Islamic conversions of cal-julian.el and cal-islam.el;
       docs/status/holidays_mod.md) — partial; 46c the diary core
       (diary-lib.el: M-x diary, the calendar's d, D, s, m and i keys,
       the date forms, the fancy and simple displays, the expression
       entries diary-date, diary-block, diary-float, diary-anniversary,
       diary-cyclic, diary-offset and diary-remind read by a fixed
       reader; docs/status/diary_library_mod.md) — partial; then the
       other calendars (cal-iso.el, the rest of cal-julian.el and the
       others, with their diary entries), lunar.el and solar.el (and the
       holidays that need them).
   47. VC, the rest: `vc-annotate` (C-x v g), branches and tags, pull
       and push, incoming and outgoing logs, and the vc-hg and vc-svn
       backends (vc-svn: done).
   48. Smerge mode (conflict markers, `vc-git-resolve-conflicts`) and
       Ediff, as PRs: 48a Smerge mode (smerge-mode.el, vc-git.el's
       conflict hooks) — done (core: refinement, Ediff and the menus are
       Deferred; see simple_merge_mode_mod); 48b Ediff core: comparing two and
       three files or buffers on one terminal frame (ediff.el,
       ediff-util.el, ediff-diff.el, ediff-init.el, ediff-wind.el,
       ediff-help.el; see docs/status/fortmacs_diff_mod.md) — done; 48c merging
       files and buffers (ediff-merg.el, `ediff-merge-files`,
       `ediff-merge-buffers` and their `-with-ancestor` forms; see
       docs/status/fortmacs_diff_merging_mod.md) — done; 48d session groups
       (ediff-mult.el, `ediff-directories`, `ediff-directories3`,
       `ediff-merge-directories` and `-with-ancestor`, the registry; see
       docs/status/fortmacs_diff_multiple_mod.md) — done; then revisions
       (ediff-vers.el) and patches (ediff-ptch.el).
   49. Projects and cross-references: project.el (C-x p), xref.el and
       etags.el (M-., M-?, M-,) — 49a partial (docs/status/cross_reference_mod.md and
       fortmacs_tags_mod.md), 49b partial (docs/status/project_mod.md), Imenu and
       `which-function-mode`.
   50. Change logs, the rest: `add-change-log-entry` (C-x 4 a),
       `log-edit-generate-changelog-from-diff`, `diff-add-log-current-defuns`
       — done (Change Log mode's filling, indentation and goto-source,
       merging, and the comment cycling deferred; see docs/status/add_log_mod.md).
   51. Buttons (button.el: `insert-text-button`, `forward-button` TAB,
       `push-button` RET), then what waits on them: Help's cross-references
       and the help-xref stack (l, r), and the buttons at the end of VC logs.
       — partial (button.el, Help's cross-references and stack; mouse
       events, the other reference kinds and the VC log buttons are
       Deferred; see button_mod and help_mode_mod)
   52. Mail: composing and sending (`compose-mail` C-x m, sendmail.el,
       message.el, smtpmail.el over a socket in src/sys, mail-utils.el,
       mailabbrev.el), then Rmail (rmail.el, rmailsum.el, rmailout.el,
       rmailedit.el, rmailkwd.el, rmailmm.el; mbox files and movemail).
       In progress: 52a mail-utils.el, mailabbrev.el, sendmail.el (Mail
       mode, `mail`, sending through `sendmail-program`, Fcc) and
       `compose-mail` with `sendmail-user-agent` — done; 52b smtpmail.el
       (SMTP over a TCP socket in src/sys, AUTH PLAIN, LOGIN, CRAM-MD5 and
       XOAUTH2, the queue; TLS, STARTTLS and auth-source deferred; see
       docs/status/smtpmail_mod.md) — partial, and mail-extr.el (the
       address and full name of a header address, for the envelope
       address; docs/status/mail_extract_mod.md) — done;
       52c message.el (`message-user-agent`, the default of
       `mail-user-agent`: Message mode, composing with C-x m, sending
       with sendmail or SMTP, the generated headers and Fcc; MML, RFC 2047,
       news and replies deferred; see docs/status/message_mod.md) —
       partial; 52d the Rmail core: reading mbox files and
       new mail moved from local inboxes with lock files (rmail.el,
       movemail.c in Fortran), Rmail mode with the summary, output,
       editing and labels (rmailsum.el, rmailout.el, rmailedit.el,
       rmailkwd.el) — partial (MIME display (rmailmm.el), RFC 2047
       decoding, replying and forwarding, Babyl files, remote mailboxes
       and sorting deferred; see docs/status/rmail_mod.md).
   53. The X11 GUI (the protocol over a Unix socket in Fortran, FreeType and
       HarfBuzz fonts), then Wayland.

The GUI (X11 protocol over a Unix socket from Fortran, our own font
rendering, then Wayland) comes after the bulk of Emacs is ported; its
fonts use FreeType and HarfBuzz through ISO_C_BINDING in src/sys (see
Design decisions).

## Per-module workflow (definition of done)

1. **Scope.** List the upstream functions being ported and the ones
   deferred (with reason) in the module's status file, docs/status/MODULE.md ("Status").
2. **Tests first.** Port the matching `test/src/*-tests.el` cases to
   test-drive (`test/test_<name>_mod.f90`) where they exist; add our own for
   uncovered behavior. Name each ported test after its ERT test.
3. **Implement.** One module per upstream file, named after it with an `_mod` suffix
   (buffer.c → `buffer_mod`) and spelled out (cal-move.el →
   `calendar_movement_mod`; see "Module names" below). Cite the
   upstream file/function in a comment on every ported procedure, e.g.
   `!> Port of insdel.c:insert_1_both.` Run `tools/fortran-names.sh` on
   the new files (see "Fortran names for Lisp terms").
4. **Headers.** Ported files carry the upstream FSF notice plus ours
   (template below); new files carry ours only.
5. **Gate.** `tools/check.sh` passes (fprettify clean, debug profile
   `-std=f2018 -Wall -Wextra -fcheck=all -g`), and CI is green on both
   architectures.
6. **Record.** Update the module's status file (docs/status/MODULE.md), one module per PR, and add the
   session log entry (`docs/log/DATE-BRANCH.md`).  The README progress
   block is not touched: the Progress workflow updates it on main after
   each merge.
7. **Merge.** The owner merges, or clears a PR by number for Claude to
   merge once CI is green. Claude polls the open PR
   (`gh api repos/Fortmacs/Fortmacs/pulls/N --jq .merged`) and, once it
   is merged, starts the next step on a fresh branch from main; work done
   while waiting is rebased onto main before its PR is opened.

### Header for ported files

```fortran
! <name>_mod.f90 -- <one line>.
! Ported from GNU Emacs 31.1 src/<file>.c.
!
! Copyright (C) <upstream years> Free Software Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: ... (GPLv3-or-later text, as in version_mod.f90)
```

Copy the FSF year range exactly from the upstream file.

The second line, the provenance, takes one of these forms (it may wrap,
and ends with a full stop):

- `! Ported from GNU Emacs 31.1 <files>.` for a ported source, with
  "(in part)" or "(in part: ...)" when it ports only some of them;
- `! Ports GNU Emacs 31.1 <test files>.` for a test that ports all of
  them, `! Ports GNU Emacs 31.1 <test files> in part; <why the rest is
  left out>.` for one that ports some, `! Own tests; <why>.` when there
  is nothing upstream to port;
- `! Generated by tools/<script>.sh from <source>; do not edit.` for a
  generated file;
- nothing (the `!` line follows the summary) for a new file.

The upstream copyright lines come with "Ported from" and "Ports", and
with "Generated by" from GNU Emacs data, and only then.  docs/init.f90,
the template users copy into their configuration, is the one file with
no header beyond its first lines.

### Audit

`tools/audit.awk`, run by `tools/check.sh` (so on every PR), checks the
rules no other tool checks (chosen with the user, 2026-10-30):

- **header**: the template above, line by line, with the GPLv3-or-later
  notice of version_mod.f90 and a blank line after it;
- **name**: a file holds the module, submodule or program of its name
  (app/main.f90, test/main.f90 and docs/init.f90 excepted);
- **suffix**: a module or submodule is named with the `_mod` suffix
  (`buffer_mod` in buffer_mod.f90, `test_buffer_mod`), except the API
  module `fortmacs`; programs and docs/init.f90's `user_configuration` take
  none (tools/test-audit.sh checks the rule on tools/audit-test);
- **doc**: every public entity a module declares has a `!>` comment
  right above its declaration (a group comment covers only the
  declaration below it; a separate module procedure is documented at its
  interface, its submodule body bare);
- **port**: in a ported source, every procedure of the module (not the
  ones contained in procedures) says "Port of <file>:<function>" or "Not
  a port" in the comment above it, at the interface for a separate
  module procedure; commands' `execute_` procedures (the targets of
  the file's `commands%NAME%execute =>`), which only read a command's
  arguments, are exempt, and so are the procedures of an
  abstract interface and the `module procedure` list of a generic one.
  `tools/audit-baseline` listed the FILE:PROCEDURE pairs that predated
  the rule; it is empty since the burn-down of 2026-10-30 and takes no
  new entries (the audit reports an entry no longer needed).

Two checks from the audit of 2026-11-02 ratchet what the rules above
cannot see in one file: `tools/lisp-data.awk` counts the places that reach
a compile-time thing through a name string against
`tools/lisp-data-baseline`, which only shrinks (`tools/lisp-data.sh
--update` after removing some; tools/test-lisp-data.sh tests it on
tools/lisp-data-test), and `tools/unported-commands.sh` writes
`tools/unported-commands`, the interactive subprograms of every cited Elisp file
that are not commands yet, which the Progress workflow regenerates and
commits on main after each merge (see "Known debt" and "Status"); branches
do not edit it.

## Status

Each module's status is a file of its own, `docs/status/MODULE.md`
(named for the first module of the row it was in the table this section
held until 2026-11-04), so two pull requests conflict over the status
only when they change the same module.  A file has the module, its
upstream files and its state (done or partial) as bullets, then three
sections: what is ported, what is Deferred and the tests.  In a section,
each top-level item of the list is on a line of its own (a line break
after a comma or semicolon outside parentheses and code), so a change
shows as the lines it touches; a reader joins the lines with spaces:

```markdown
# undo_mod

- Module: undo_mod
- Upstream: undo.c (+ simple.el undo-auto)
- State: partial

## Ported

`record_point`,
`record_insert` (merging consecutive insertions),
...

## Deferred

marker adjustments,
...

## Tests

test_undo_mod (ports undo-tests: ...)
```

The Deferred section is the one place that lists what a module does not
port yet (CLAUDE.md); file headers and docstrings point at it.  A new
module gets a new file; a module that spans several files (a module and
its submodules) keeps one file, named for the first, with the others in
its Module bullet.  `tools/status.sh --check` (run by tools/check.sh)
checks the format (the state is done or partial), `--index` prints the
index of the modules (module, upstream, state) and `--rows` prints the
files back as the old table's rows (to check a round trip with
`--from-row`).

The index is [docs/progress/status-index.md](docs/progress/status-index.md),
generated on main: the Progress workflow writes it with `tools/status.sh
--index` after each merge, as it does the README's progress block and pie
charts.  Pull requests never edit it (kept by hand it would be the shared
line every pull request edits again); a branch's changes show in it once
merged.

The same goes for the other tables most pull requests added to, which
conflicted almost every time (GitHub's mergeability check ignores
`merge=union` in .gitattributes).  tools/fortran-names.d and
tools/user-options.d are directories: `0-base` holds the entries made
before 2026-11-04, and each pull request adds its own in a file named
like its session log, `DATE-BRANCH` (`2026-11-04-step49b-project`),
never editing another's except to fix an entry; their readers take the
files in `LC_ALL=C` sorted order, so `0-base` comes first.
tools/unported-commands is generated on main by the Progress workflow,
as the index is.  A branch written before this moves its additions after
merging main with `tools/split-table-additions.sh` (lines git carried
over into a `0-base` go to the branch's own files, and its edits of
tools/unported-commands are dropped).

A branch written before the status files, which edited a row of the
table: merge main, resolve the conflict in this section by taking main's
side, then move the branch's row changes into the files with
`tools/status.sh --move-rows OLD NEW`, where OLD is the branch's merge
base with main before the merge and NEW its last commit before the merge
(a three-way merge per changed row; conflicts are left marked).

### Not applicable: run-time Lisp

Fortmacs has no Lisp interpreter: the configuration is Fortran compiled
into the binary (`init.f90`, `fortmacs --recompile`), so the facilities
that evaluate Lisp in a running session are not applicable and are not
ported. Their keys are unbound, as test_bindings_mod checks:

- `eval-expression` (M-:), `eval-last-sexp` (C-x C-e), `eval-defun`
  (C-M-x), `eval-print-last-sexp`, `eval-buffer`, `eval-region`,
  `load-file`, `load-library`, `ielm`;
- `lisp-interaction-mode` and the evaluation commands of
  `emacs-lisp-mode` (`*scratch*` starts empty, in Fundamental mode:
  `initial-scratch-message` and `initial-major-mode` have no Fortran
  counterpart);
- `\,` (an expression to evaluate) in `query-replace-regexp`
  replacements, and `-l`/`--load`, `--eval` and `-f`/`--funcall` on
  the command line.

floatfns.c (step 42e) is Not applicable as a file: its functions are Lisp's
arithmetic on floats, and Fortran has each as an intrinsic, which ported
code uses directly (Intrinsics first): `sin`, `cos`, `tan`, `asin`,
`acos`, `atan` (`atan2` for two arguments), `exp`, `log` (`log(x)/log(b)`
for a base; `log10`), `sqrt`, `abs`, `expt` (`**`), `float` (`real`; `float-time` is `real` extended for a time value, time_functions_mod, R6-14),
`isnan` (`ieee_is_nan`), `copysign` (`ieee_copy_sign`), `frexp`
(`fraction`, `exponent`), `ldexp` (`scale`), `logb` (`exponent(x) - 1`),
`ceiling`, `floor`, `truncate` (`int`), `fceiling`/`ffloor`/`ftruncate`
(`real` of those, `aint`), and `round`/`fround` (`ieee_rint`: ties go to
the even integer, see Hazards).  Its helpers (`extract_float`,
`rounding_driver`, `double_integer_scale`, `rescale_for_division`) serve
Lisp's mixed integer and float numbers.

## Fortran names for Lisp terms

<!-- fortran-names: off -->
Fortmacs is Fortran-first, not Lisp-first: where upstream names a thing
with a Lisp term, Fortmacs uses a Fortran word, in identifiers, file
names, command and variable names (M-x, C-h f/v/k), docstrings, messages,
tests and these documents.  Each term is confirmed by the owner, by an
existing rule (decided and reported), or by the decision panel
(CLAUDE.md); no rename is added otherwise.  The confirmed renames:

| Lisp term | Fortran name | Examples |
|---|---|---|
| defun (a top-level definition) | subprogram | `beginning_of_subprogram`, `mark_subprogram`, `subprogram_prompt_regular_expression`, `open_parenthesis_in_column_0_is_subprogram_start`, `add_log_current_subprogram`, `f90_beginning_of_subprogram`; `f90_subprogram_start` and `f90_subprogram_end` (f90_mod's adapters to `beginning_of_subprogram_function` and `end_of_subprogram_function`, whose plain renames would be the f90.el commands they adapt) |
| defun, in a mode's own helper | the language's term | `python_mark_definition`, `python_navigation_beginning_of_definition` (and `_regexp`), `python_navigation_end_of_definition` (and `_function`), `python_navigation_forward_definition`, `python_navigation_backward_definition`, `python_info_current_definition`, `python_info_looking_at_beginning_of_definition`; `makefile_add_log_target`; `shell_current_function_name`; `log_view_beginning_of_entry`, `log_view_end_of_entry`; `css_current_rule_name`.  Generic commands, hooks and helpers keep subprogram: `search_subprogram_forward`/`_backward`, `nav_*_subprogram`, `beginning_subprogram_indent`, `change_log_read_subprograms`, `log_edit_insert_filled_subprograms`, `diff_add_log_current_subprograms` |
| sexp (a balanced expression) | expression | `forward_expression`, `cut_expression`, `parse_partial_expression`, `scan_expressions`, `parse_expression_ignore_comments`, `halfexpression`, `expression_flag` (and the analogous `execute_`, `test_` and `_1` forms of the renamed names) |
| list, as a parenthesised group (these names only) | group | `forward_group`, `backward_group`, `up_group`, `down_group`, `backward_up_group`, `cut_backward_up_group`, their `*_group_function` variables, `scan_groups`, `simple_minded_indentation_engine_down_group` |
| alist (an association list) | a plural name, no suffix (R2-Q8; first `_table`) | `auto_modes`, `bookmarks`, `file_coding_systems`, `registers`, `save_places`; `compilation_error_regular_expressions` (all known formats) and `compilation_error_regular_expression_names` (the enabled ones); where the plural is taken or unclear, the plural names the records: `minor_mode_lighters`, `coding_system_names`, `display_buffer_rules`; `abbrev_minor_mode_tables`, `syntax_modifications` (font-lock-defaults' SYNTAX-ALIST, not `syntax_table`), `display_action_parameters_type` (display-buffer's action alist, fixed keys as components); in buffer_mod, buffer_operations_mod and process_mod, Vbuffer_alist and Vprocess_alist are `buffer_order`/`buffer_order_count` and `process_order`/`process_order_count` (the modules already have a `table`) |
| module lisp_cmds_mod (lisp.el) | expression_motion_mod | src/expression_motion_mod.f90, `initialize_expression_motion` |
| the modules of lisp.h, eval.c and lread.c | `value_mod` (submodule `value_names_mod`), `hooks_mod`, `names_mod`, each named for what it holds (print.c stays `print_mod`) | src/value_mod.f90, src/hooks_mod.f90, src/names_mod.f90 |
| `syms_of_X` (a module's one-time startup setup) | `initialize_X` (`initialize_X_commands` where upstream has an `init_X` of its own; `init_X` before the word renames of 2026-11-05) | `initialize_simple`, `initialize_files`; `initialize_buffer_commands`, `initialize_display_commands`, `initialize_window_commands`, `initialize_macros_commands` |
| a predicate (`*-p`, `*p`) | `is_*` or `has_*`, a logical function | `is_region_active`, `is_derived_mode`, `is_process_live`, `is_beginning_of_buffer`, `has_overlay_strings`; a verb phrase where it reads so: `file_exists`, `string_matches` (logical, no longer the match position); `set_buffer_modified`, `restore_buffer_modified`; a predicate that only wrapped an intrinsic or a tag test is gone (`frame_window_p` is `associated(f%rif)`) |
| y-or-n-p, yes-or-no-p (prompts) | `read_y_or_n`, `read_yes_or_no` | `read_y_or_n_map`, `read_y_or_n_use_read_key`, `map_read_y_or_n`, `read_yes_or_no_history` |
| `*_NIL`, `*_T` named constants | the meaning, after the option's full name (R2-Q15; a stem several options share is the common start of their names, else each option has its own family: changed by #261, owner, 2026-11-04): `_NO`/`_YES` for plain off/on, `_NONE`, `_ASK`, `_ALWAYS` only beside `_YES`, `_UNLIMITED`, else the meaning itself (R2-Q17) | `COMPILATION_SCROLL_OUTPUT_NO`/`_YES`/`_FIRST_ERROR`, `LOG_EDIT_CONFIRM_YES`, `DIRECTORY_EDITOR_LOCALP_ABSOLUTE`/`_RELATIVE`, `KEYBOARD_MACRO_REPEAT_YES`/`_NO` (upstream's NO-REPEAT read positively; the argument is REPEAT), `CASE_FOLD_NO`/`_YES`/`_FORCED`, one family per setting (`GREP_USE_NULL_DEVICE_*`, `GREP_USE_NULL_FILENAME_SEPARATOR_*`, `GREP_HIGHLIGHT_MATCHES_*`); every family has three or more states upstream, so none became a logical (the full table: tools/fortran-names.d/0-base, "A1"); since R6-38 a setting's on and off states are `.true.` and `.false.` of a `choice_setting_type` (`compilation_scroll_output = .true.`): `_NO`/`_YES` remain only in internal enumerations ("Choice settings" below) |
| nil, NILP (the null value) | `value_type()`; `.not. allocated(v%a)` | the intrinsic itself, no helper; a function result is tested as `vtag(f(x)) == V_NULL`; the value tags are `V_NULL`, `V_INTEGER`, `V_NAME`, `V_STRING`, `V_ARRAY` (were `V_NIL`, `V_INT`, `V_SYMBOL`, `V_STRING`, `V_LIST`) |
| FIXNUMP, STRINGP, CONSP, SYMBOLP, LISTP, BUFFERP, MARKERP, WINDOWP and value_mod's keymap and syntax-table tests | the tag test itself: `vtag(v) == V_INTEGER`, `vtag(v) /= V_STRING`, `(vtag(v) == V_NAME .or. vtag(v) == V_NULL)` | no wrapper; `vtag` gives `V_NULL` for the null value, and is elemental like the wrappers were; window.c's `windowp` (a handle test) is `is_window` |
| the argument list C-h f shows | the call line, then the declaration block | `call insert(string, status)` followed by `character(len=*), intent(in) :: string` and `type(status_type), intent(out) :: status`; a function's result is declared too (`integer(int64) :: point`); tools/gen-docstrings.sh takes the lines from the dummy arguments' declarations (the help entry's `declarations`) |
| funcall, funcall_nil, apply | the procedure pointer called directly | `call find_function(v, fn, status)` resolves a run-time name once, then `call fn%execute(status)`; set_auto_mode resolves the mode with `find_command`; the undefined-key command is keyboard_mod's `undefined_function` pointer, which basic_subprograms_mod sets; grep's and log-view's internal `apply` are `set_defaults` and `toggle_entry` |
| fboundp, keywordp | `associated(find_command(name))`; the name's first character | no wrapper |
| cons (printing a keymap entry), a cons argument | `keymap_entry_text(event, binding)`; newcomment's `universal_argument` | named for what they hold |
| the exact Lisp value of a setting (`setting_type%exact`) | removed (A8) | a logical setting holds `.true.` or `.false.` (any allocated value is `.true.`); an integer or string setting given another value is a `wrong-type-argument` error condition except where upstream's nil means something: fill-column's nil is `FILL_COLUMN_NONE` (`current_fill_column` then gives the largest fixnum, as upstream; its obsolescence warning is Deferred with lwarn; set_fill_column shows and refuses it as upstream does), tab-width's nil is `TAB_WIDTH_DEFAULT` (tabs take the default width, 8, buffer.h:sanitize_tab_width); both names confirmed by the owner; a function setting resolves the name when it is set (an undefined name is `void-function`, "Function is not defined") and keeps the resolved command (`%command`) for the name it reads back as; an unknown require-final-newline value is `FINAL_NEWLINE_ASK` |
| the face an ansi-color face function receives | `type(ansi_color_face_type)` (A5): `faces(:)` of `ansi_color_face_reference_type` (a pointer to a named face; null for an entry of `ansi_color_basic_faces` that is ""), `foreground` and `background` ("" for a color face without the color); empty (`%is_empty()`) means none | no `value_type` in the face function's interface; `ansi_color_face_value` builds the text property value upstream's ansi-color--face-vec-face gives |
| car, cdr, nth, nthcdr, memq, member, length (C2c) | removed: plain arrays with the intrinsics | membership in an array is `any(...)` (`has_handle(list, h)` in handle_mod for an allocatable handle array; `has_name` with `any(names == name)`), a loop named for what it tests where the elements have deferred-length components (`has_word`, `has_string`, `has_file`, `has_coding_system`, `is_name_taken`); a value array's first element is `list_element(v, 1_int64)`; the fns-tests-length port and the car/cdr tests go with the helpers |
| thin wrappers of an intrinsic | the intrinsic | `ichar(str(i:i), kind=int32)` for byte_at, `btest(flags, SYNTAX_FLAG_COMMENT_NESTED)` (named bits of syntax.c's SYNTAX_FLAGS_*), `btest(mask, cat)`, `iand(buf(p + 1), 127)`, `all(set /= c)`, `size(a)` (a local named `size` became `total_size`); test_tabulated_list_mod's `sort_car` stays: sort needs a procedure |
| symbol (an object: a name of a function, variable, ...) | name | `describe_name` (C-h o), `complete_name`, `apropos_next_name`, `apropos_previous_name`, `info_lookup_name`, `abbreviation_name`, `abbreviation_table_names` |
| symbol (the syntax class: word and symbol constituents) | identifier | `forward_identifier`, `beginning_of_identifier`, `bounds_of_identifier_at_point`, `case_identifiers_as_words`, `word_or_identifier`, `occur_identifier_at_mouse`, `REGULAR_EXPRESSION_OPTIMIZATION_IDENTIFIERS` |
| vector (an array) | a plural name, "array" in help | `ansi_color_basic_faces`, `ansi_color_normal_colors`, `ansi_color_bright_colors` |
| the repeat-check-key property | a `choice_type` (R6-38; Q4 of the choice PR): `.true.`, `.false.` or `REPEAT_CHECK_KEY_DEFAULT` | `commands%x%repeat_check_key = .false.` (were `PROPERTY_UNSET`, `_TRUE`, `_NO`, then `REPEAT_CHECK_KEY_DEFAULT`, `_YES`, `_NO`) |
| help-form | help procedure | `help_procedure`, `help_procedure_show`, `minibuffer_help_procedure` |
| prin1-to-string, prin1 of a string | `readable_text`, `quoted_text` | help shows values with `fortran_text` |
| register value | register contents | `register_contents_type` |
| this-command, last-command, real-this-command, real-last-command, last-repeatable-command, current-minibuffer-command (A7) | `type(command_type), pointer`, compared with `associated()` | `associated(last_command, commands%cut_region)`, `this_command => commands%paste`; a key's binding is resolved once with `command_of`; `last_macro_command` where upstream holds a keyboard macro (A7a: a `command_type` whose `macro` handle names the macro's events in keyboard_mod's `command_macros` and whose procedure executes it once, so C-x z repeats it; help shows the events as an array constructor); null for a name that is no command and that nothing compares (undo-start, kill-region-dont-append, mode-exited, yank's t); the two that are compared are typed objects of their modules, `mark_subprogram_back` and `comment_line_backward` (`is_command` .false.); help shows the command's name or null() |
| prefix-arg, current-prefix-arg, last-prefix-arg, a raw prefix argument "P" (A7) | `type(prefix_argument_type)`: `%kind` (`PREFIX_NONE`, `PREFIX_NUMBER`, `PREFIX_RAW` for C-u's, `PREFIX_MINUS`) and `%value` | `current_prefix_argument%kind /= PREFIX_NONE`, `prefix_argument = prefix_argument_type(PREFIX_RAW, 4_int64)`, `call recenter(prefix_argument_type(PREFIX_NUMBER, -1_int64), .false., status)`; `prefix_numeric_value` takes it; help shows `prefix_argument_type(kind=PREFIX_RAW, value=4)` |
| the interactive spec of a command | `is_command` | `commands%directory_editor_mode%is_command = .false.`; tools/gen-docstrings.sh's help-name field (`hname`) and command column (the help entry's `command`) |

Citations keep upstream names, so they still point at the exact upstream
definition: "Port of lisp.el:beginning-of-defun", "Port of
syntax.c:scan_lists", and upstream names quoted as such ("upstream's
`forward-sexp`").

**The tools.**  tools/fortran-names.d is the single table: its files read
in `LC_ALL=C` sorted order, `0-base` (whose header gives the format:
`term`, `word`, `part`, `prefix`, `name`, `file` and `keep` entries)
first, then one file per pull request named like its session log
("Status").  A `term` renames a Lisp term everywhere, prose included; a
`word` (an abbreviation spelled out, 2026-11-05) renames it in code and
in names written in text, never in plain prose, prompts or messages; a
`part` is a `word` that is never a whole one-part name (the intrinsic of
that name: `part len length` renames `old_len`, not `len(x)`); a `prefix`
renames the first part of a name of two or more parts (`cmd_x` is
`execute_x`).
tools/fortran-names.sh applies it, by default to src, test, app, tools,
docs/init.f90, fpm.toml, PORTING.md and README.md, or to the paths given:
it renames files (with `git mv`) and contents, protects citations
(`FILE.el:NAME`, `FILE.c:NAME`, `FILE.h:NAME`), hyphenated and C names
(`forward-sexp`, `Fscan_lists`), the word after "upstream" or
"upstream's", kept text, and lines with the markers the table's header
describes; it fixes "a"/"an" before a
renamed word; an entry's new name that holds another entry's old word is renamed again (0-base's `auto_coding_regexps` is `auto_coding_regular_expressions`), so one run reaches the final names and a second run changes nothing.  An interface named `*_proc` becomes `*_procedure` (after `procedure(` and inside an abstract interface); `proc` elsewhere is a process.  tools/check.sh runs
tools/fortran-names.awk, which fails with FILE:LINE on an old name in
Fortran code or in a `!>` docstring, or on a file of an old name, and
tools/test-fortran-names.sh, which runs the script on the fixture in
tools/fortran-names-test.  tools/gen-docstrings.sh gives the names it
derives from citations their Fortran names too.
The table also holds the names R6-21 gives this program (below): the
`file` entry `emacs_mod` → `fortmacs_mod`, one `name` entry for each
identifier that means this program (`exit_emacs`, `emacs_read`, ...), a
`keep` of the word `emacs` alone, so that the file entry's stem leaves
prose ("GNU Emacs", "Emacs Lisp") to be changed by hand, the `file`
entry `regex_emacs_mod` → `regular_expression_mod` (R6-24), and `name` entries for
the commands and settings R6-25 names for this program.

**Lisp that stays Lisp.**  `keep` entries leave what is genuinely Lisp
as it is: the commands of the Lisp modes and of run-time evaluation
(`eval_last_sexp`, `eval_defun`), Lisp source in string literals and
comments (`"(defun "`), upstream's C macro `DEFUN`, the Lisp
language modes' files once ported (lisp_mode_mod, elisp_mode_mod,
scheme_mod and their tests), and tools that read upstream's Lisp and C
(tools/progress.sh).  Messages and docstrings are renamed; Lisp that code
matches is not.

**This program is Fortmacs (R6-21, 2026-11-03).**  Fortmacs is a new
program in another language, so a name that means this program says
fortmacs, not emacs: the module of emacs.c is `fortmacs_mod`
(`fortmacs_main`), the system-call wrappers ported from sysdep.c are
`fortmacs_read`, `fortmacs_write`, `fortmacs_spawn`, `fortmacs_kill`, ...,
`fortmacs_terminal_type`, `fortmacs_wd`, xfaces' `fortmacs_color_type`, and the
commands, hooks and settings are `exit_fortmacs` (upstream's kill-emacs),
`suspend_fortmacs`, `save_buffers_exit_fortmacs`, `exit_fortmacs_hook`,
`exit_fortmacs_query_functions`, `fortmacs_pid`, `restart_fortmacs`,
`user_fortmacs_directory`; docstrings and messages say "Exit Fortmacs",
"Stop Fortmacs and return to superior process".  Names of external things
keep Emacs: the emacs-mule encoding (`emacs_multilingual_environment_id`, `CATEGORY_EMACS_MULTILINGUAL_ENVIRONMENT`,
"emacs-mule"), the "emacs" charset and utf-8-emacs, Emacs Lisp
(`emacs_lisp_mode`), completion's `emacs21`/`emacs22` styles, INSIDE_EMACS and
~/.emacs_SHELLNAME (what other programs and users see), prose about
upstream ("GNU Emacs 31.1", "an Emacs regexp") and every citation
("Port of emacs.c:main", "Cf. sysdep.c:emacs_open").  Keys and behavior do
not change.  R6-24 (2026-11-03): the module of regex-emacs.c is
`regular_expression_mod` (test_regular_expression_mod), Fortmacs's only regex engine, so the
program's name does not belong in it; `emacs_mule_*`, the coding systems
emacs-mule and utf-8-emacs and Emacs Lisp mode keep emacs, because they
name formats and a language that exist outside this program.  R6-25
(2026-11-03): the help-for-help menu (C-h C-h) says Fortmacs on every
line that means this program: "About Fortmacs", "Extending Fortmacs with
external packages", "Search for Fortmacs packages", "Describe a specific
Fortmacs package", "Start the Fortmacs tutorial", "Fortmacs FAQ",
"Debugging Fortmacs", "Fortmacs copying permission (GNU General Public
License)", "Fortmacs ordering and distribution information", "Fortmacs
TODO"; its Manuals lines say Fortmacs too since R6-32 (below).  The unported commands it names are
`about_fortmacs`, `view_fortmacs_faq`, `view_fortmacs_news`,
`view_fortmacs_problems`, `view_fortmacs_debugging`, `view_fortmacs_todo`
(`describe_gnu_project` stays), and the other upstream names that mean
this program have their entries ahead of their ports:
`fortmacs_startup_hook`, `fortmacs_init_time`, `fortmacs_uptime`,
`fortmacs_build_time` and the command emacs-version,
`commands%fortmacs_version` (version_mod's constant `fortmacs_version`
keeps its name: commands are a separate namespace).  R6-32 (2026-11-03):
the installed manual is Fortmacs's own.  The Emacs manual, converted
(step 34d), is installed as the Info file `fortmacs`, its directory entry
says Fortmacs, and every reference to it is "(fortmacs)Node": C-h r
`info_fortmacs_manual` opens (fortmacs)Top, `info_fortmacs_bug`
(fortmacs)Bugs, C-h F `info_goto_fortmacs_command_node` and C-h K
`info_goto_fortmacs_key_command_node` look in (fortmacs) by default and
through `info_files_for_fortmacs` (upstream's Info-file-list-for-emacs),
the `info_file` of `info`, of those two commands and of
`info_lookup_name`/`info_lookup_file` is "fortmacs", info-look's
Emacs-command help reads (fortmacs)Command Index and (fortmacs)Variable
Index, docstrings say "see Info node `(fortmacs)Auto Revert'", and the
help-for-help Manuals lines say "Show Fortmacs manual", "Show Fortmacs
manual section for command", "Show Fortmacs manual section for a key
sequence".  The other manuals keep their names: the Emacs Lisp reference
(elisp) and the Lisp introduction (eintr), which are not installed (no
run-time Lisp), and the doc/misc manuals (info, efaq, emacs-mime,
emacs-gnutls, ...), installed with the package each documents.
`info_url_table` keeps "emacs": it names upstream's manuals on the web
(Info-url-alist), and Fortmacs's manual has no web copy.

R6-31 (2026-11-03): secure-hash's `iv-auto` OBJECT (the hash of LENGTH
random bytes from getrandom) is its own subroutine,
`call hash_random_bytes(SECURE_HASH_SHA512, 100_int64, hash, status)`,
in utility_functions_mod beside `secure_hash` for strings and buffers.

### Wording

Docstrings, messages, error strings and these documents say in Fortran
what upstream says in Lisp.  Applied (the owner's design review, PR 2):

- A2: a wrong-type error names the expected Fortran declaration: "Wrong
  type argument: expected character(len=*)", "expected integer",
  "expected type(keymap_type)", "expected live type(window_type)",
  "expected integer (a character code)", "expected name", "expected
  array", then ", got VALUE" where upstream shows the value ("Wrong
  type argument: expected character(len=*), got file").  The qualifiers
  (live, valid, case table, ">= 0", "a character code") stay.
- A3: the error table and help say "Variable has no value", "Function
  is not defined", "Attempt to set a named constant" and "Unknown name";
  help says "NAME has no value as a variable" and "You didn't specify a
  function name".
- A4a: nil and non-nil go by the value's type: a logical is `.true.` or
  `.false.`, an optional argument present or absent, an allocatable
  unallocated, a pointer null, a missing name "", a handle the null
  handle, and a setting its named constants (the "(upstream nil, t,
  ...)" asides are gone); a bare t meaning true is `.true.`.  A null
  `value_type` is unallocated ("Return an unallocated value if KEY has
  no binding"), a 0 or -1 sentinel is none, and an absent prefix
  argument is no prefix.
- A4b and B2: a symbol in the Lisp-object sense is a name; the syntax
  class (symbol constituents, the identifier at point) is an
  identifier.  Upstream's own symbols, cited as such, keep the word.
- A4c: an error is "a `X' error condition occurs" (the standard's
  wording), not a signal; POSIX process signals keep the word.
- B8: a condition-case handler is a check of the status, a throw to X a
  return to X with a status, and a catch the caller that checks the
  status.
- A4d: a cons is named components (`decoding` and `encoding`; "a
  replacement with components `function` and `data`"), or, where the
  text only describes upstream data, "two values, X and Y"; the word
  "pair" is never used for data; a FORM that is evaluated is a
  PREDICATE (a logical function)
  that is called; an interactive spec is described ("a command that
  reads the region"); a plist is typed components and constructors for
  faces (`face_underline(color=..., style=..., position=...)`), "text
  properties" for text, and "attributes" for symbols, charsets and
  abbrevs; byte-compiler mentions are gone; help's Lisp samples are
  Fortran (raise_expression: "x = max(a, min(b, c))" with point before
  min gives "x = min(b, c)").
- The rest of the scan: car and cdr are first and rest (or the
  component's name), lambda a procedure, funcall a call, setq an
  assignment (setq-local `set_local`), the obarray the name table,
  "Elisp" and "Lisp" are reworded (Info node names such as
  `(elisp)Basic Completion` and the Lisp language modes stay), and
  "dynamic binding" or "let-bound" in comments is "saved and restored".

**The check.**  tools/fortran-prose lists the wording that is no longer
used, with what to write instead: non-nil, a standalone nil, "is void",
"Symbol's value", "constant symbol", "Wrong type argument: PREDp,",
"signal an error", throw and catch, condition-case, "property list" and
plist, "interactive spec", "byte-compil", cons, "dotted pair", pair
(kept where it means delimiters, Unicode bracket pairs or a
pseudo-terminal pair), car and cdr, lambda, funcall, setq, obarray and
Elisp.  tools/check.sh runs
tools/fortran-prose.awk, which fails with FILE:LINE on one of them in a
`!>` docstring or in a message or error string (the string literals of
a line that calls `set_error`, `message`, `user_error`,
`format_message` or `minibuffer_message`), and
tools/test-fortran-prose.sh on its fixture (tools/fortran-prose-test).
Citations, identifiers (a quoted `name`, a name with an underscore, an
argument in capitals), the tests, the Lisp language modes and the
table's `keep` entries are not checked; the generic value machinery
(value_mod, utility_functions_mod, data_mod, print_mod, names_mod, data_buffer_mod) is kept
until C1 to C5 rewrite it.

### Decided, not yet applied

The owner settled these in the same design review; each waits for the PR
named with it.

**PR 2: wording and names.**

- A2, A3, A4a to A4c and the wording of A4d are applied (see "Wording"
  above).
- A6b: a setting with two states is a `logical` (.true./.false.); one
  with more states is an integer setting with named constants named for
  their meaning, never `_NIL`, `_T`, `_TRUE` or `_FALSE` (checked with
  A1: no family has only two states; tools/intrinsics.awk rejects the old
  forms).  Superseded by R6-38: more states make a choice setting
  ("Choice settings").

**PR 2b: refactors** (branch `fortran-first-refactors`).

- The code half of A4d: a cons becomes named components (for example
  `decoding` and `encoding` of `default_process_coding_system`);
  `ibuffer_always_compile_formats` (the byte compiler) is removed as not
  applicable (done in `audit-fixes`, 2026-11-02).
- A6: settings reached by a name string become typed setting objects;
  saving and restoring a value goes through a local copy.
- A7, the rest: events become an `event_type` (waits for the owner's
  choice of its components; the command state and the prefix argument
  are applied, see "Fortran names for Lisp terms").
- Completion help says "COLLECTION is an array of strings, a table of
  (string, value) entries, or a completion function".
- C2b: a value's type is tested with the language construct on the flat
  tagged `value_type`, as json-fortran does: `select case (v%a(1)%tag)`
  or `if (v%a(1)%tag == V_INTEGER)`; components are read directly
  (`v%a(1)%i`, `v%a(1)%s`) (the tags `V_NULL`, `V_INTEGER`, `V_NAME`,
  `V_STRING` and `V_ARRAY` are applied); no
  `is_integer`, `xfixnum` or `integer_of` helpers.  Why not `select type`:
  see the design decision "Values stay a flat array of tagged nodes".
- C2c, the rest: `cons` (building nested value lists) waits for C3/C4,
  with `memq_p` in text_properties_mod (text properties, C4).
- C3: the name table holds only names that exist at run time;
  compile-time things are reached only through their typed objects;
  `intern` becomes `add_name`, `intern_soft` `find_name`, and
  `symbol_type` `name_entry_type`; plain IF (no Fortran 2023 conditional
  expressions: gfortran 13, -std=f2018).
- C4: `put`, `get` and the `plist_*` procedures are removed:
  `history_length` becomes a component of the history objects, and text
  properties an array of `text_property_type` (name, value) searched with
  `findloc`.
- C5-1: the `defvar_false` histories become typed history objects
  (`type(history_setting_type) :: bookmark_history`, with `%length` per C4).
- C5-2: `setq_local` is removed in favor of the setting's `%set_local()`.
- C5-3: `boundp` and `makunbound` (data_mod's run-time variables, which
  port data-tests.el) become direct components; they go with C3's name
  table.  (`fboundp` and `keywordp` are applied.)
- C2b, the rest: the type-checked readers `xfixnum`, `xsymbol`, `xstring`
  and `xhandle` (each returns a default for another type, so every call
  site is checked) become direct component reads.

Settled by the owner later the same day and applied in PR 2b:

- `TAB_WIDTH_NONE` is `TAB_WIDTH_DEFAULT` (upstream sanitizes tab-width's
  nil to the default width, 8); `FILL_COLUMN_NONE` is confirmed.
- Q3: a function upstream names `*-p` that returns more than a logical
  takes the name of what it returns: `window_dedication` and
  `set_window_dedication`, `invisibility`, `window_part_at`,
  `diff_header_position`; by the same rule (decided by Claude, reported)
  `variation_selector_number`, `preferred_sequence_kind` and
  `numeric_prefix_argument` (the `arg_p` helpers).  Subroutines, the hook and
  six tests drop the suffix (`vc_call_responsible`,
  `shell_indentation_engine_rc_after_special_argument`,
  `executable_make_buffer_file_executable_if_script`, `test_empty`, ...).
- Q4: the `_vector` names whose plurals clash name what the array holds:
  `this_command_key_events`, `execute_keyboard_macro_events`,
  `describe_character_table`, and keymap_mod's local `is_octal_code`.
- Q8: logical user options and public logicals drop upstream's p/-p
  (`makefile_pickup_everything_picks_up_filenames`,
  `makefile_use_curly_braces_for_macros`,
  `select_safe_coding_system_accept_default`,
  `terminal_suppress_bold_inverse_default_colors`); disabled-command-function's
  flag, whose plain name is the subroutine, is
  `disabled_command_function_variable` (CLAUDE.md's `_variable` rule);
  `ibuffer_default_sorting_reversed_p` is not ported yet (its table entry
  waits for it).
- B2, the rest (decided by Claude, reported): `event_name_parse_modifiers`,
  the apropos button type `apropos_name` (its entry's name property
  `apropos_entry_name`), abbrev's `table_name` and `name_in`,
  `f90_equal_names`.
- Q7: one generic constructor, `value_type(...)`: `value_type()` (the
  null value), `value_type(.true.)` (t; `.false.` gives the null value),
  `value_type(42_int64)`, `value_type(name="x")` and
  `value_type(text="abc")`, replacing `t_value`, `make_fixnum`, `symbol` and
  `make_string`.  NAME and TEXT are the optional arguments of one
  specific (two specifics with one character argument each cannot be
  told apart), in the submodule value_names_mod, which uses names_mod's name
  table.  The handle constructors (`make_buffer_reference`, ...) stay.
- Q10: histories are `history_setting_type` objects (a `setting_type` with
  `%items`, `%length` and `%append`), registered with `forward_setting` and
  found by name with `find_history` (the HISTORY argument of the
  minibuffer readers, `add_to_history`, savehist through their values);
  the `defvar_false` histories are typed objects (`bookmark_history`,
  `grep_history`, `search_ring`, ...); `clipboard_history`, which is no history,
  keeps `defvar_false` until C4.
- Q10b: no run-time variables: a variable exists only as a Fortran
  object; the run-time-variable parts of data.c and data-tests.el are
  listed Not applicable (data_mod, in docs/status/value_mod.md).
- Q12: set_variable takes the named constants of an enumerated option
  only (since R6-38 also `.true.`/`.false.`), read with completion over
  them, each annotated with its value (upstream's `:annotation-function`,
  now ported for *Completions*); anything else is a `wrong-type-argument`
  error naming the constants.  The constants carry no docstrings of their
  own, so the annotation is the value, not a one-line meaning.
- `PYTHON_DOCSTRING_NONE` and the logo's blended (multiply) shadows are
  confirmed.

**R5-1 (2026-11-02), not yet applied:** the `_start` names of our own helpers (58 procedures, about 870 word parts) become `_beginning`; a PR of its own.

**PR 3: module suffix and module names (applied).**

- The module suffix `_fm` is `_mod`: `buffer_mod` in buffer_mod.f90,
  tested by test_buffer_mod.f90 (suite `buffer_mod`, `collect_buffer_mod`);
  modules that port nothing take it too (`configuration_mod`, and the test
  helpers' `test_support_mod`, which had no suffix); the API module
  `fortmacs` and the user's `user_configuration` are unchanged.  tools/audit.awk
  checks it (rule `suffix`).
- C1: lisp.h's module is `value_mod` (its submodule `value_names_mod`),
  eval.c's `hooks_mod`, lread.c's `names_mod`; `print_mod` keeps its
  name.  `syms_of_X` is `initialize_X` (`init_X` until 2026-11-05); where
  upstream has an `init_X` of its own (a different startup step: buffer.c,
  dispnew.c, window.c, macros.c), the setup that registers the commands is
  `initialize_X_commands` (`initialize_buffer_commands`, `initialize_display_commands`,
  `initialize_window_commands`, `initialize_macros_commands`).

Already settled and kept as they are: hook and the `_hook` suffix, and
the names of the Lisp language modes.

**A new upstream version** (31.2, 32.0) or a branch written before an
entry: port or merge as usual, run `tools/fortran-names.sh` (on the new
files, or on the whole tree), split any line it reports as pushed past
132 columns (exit status 3), and where it reports a clash (exit status
4: a new name the file already has, such as an imported `forward_sexp`
renamed onto the module's own `forward_expression`; the file is left
unchanged) rename the file's own identifier (`javascript_forward_expression`) and
run it again; then regenerate (tools/gen-*.sh), run
`tools/check.sh --fix` and then `tools/check.sh`, and read the renamed
docstrings: grammar the article fix does not cover ("balanced expression
(sexp)") and "list" in the group sense are adapted by hand.  A new
Lisp-derived term found while porting is confirmed by the owner, by an
existing rule (decided and reported), or by the decision panel
(CLAUDE.md), never renamed otherwise; once confirmed it becomes a line of
tools/fortran-names.d and a row above.
<!-- fortran-names: on -->

## Intrinsics first

Fortmacs must look like Fortran, not Elisp written in Fortran.  The
intrinsics and language constructs come first: `allocated`, `associated`,
`present`, `size`, `len`, `any`, `all`, `findloc`, `pack`, `merge`, array
sections, `select case` and `select type`, `index`, `scan`, `verify`,
`trim`, `adjustl` and the rest.  A helper that only wraps one is not
written: not `is_null(v)` for `.not. allocated(v%a)`, `length(x)` for
`size(x)`, `memq` for `any(x == y)` or `car` for `x(1)`.  Flat data is a
plain array, used with the intrinsics directly.

<!-- fortran-names: off -->
tools/intrinsics.awk, run by tools/check.sh, reports these patterns:

- **wrapper**: a function or subroutine whose executable part is one
  statement assigning one intrinsic (or `.not.` of one) applied to its
  dummy arguments, such as `is_null = .not. allocated(v%a)` or
  `n = size(x, kind=int64)`, or that only tests a value's tag or reads
  its component (`is_integer = v%a(1)%tag == V_INTEGER`,
  `integer_of = v%a(1)%i`);
- **lisp-helper**: a procedure named after a Lisp helper: `nil`, `nilp`,
  `t_value`, `symbol`, `consp`, `cons`, `car`, `cdr`, `car_safe`,
  `cdr_safe`, `nth`, `nthcdr`, `memq`, `member`, `length`, `fixnump`,
  `make_fixnum`, `xfixnum`, `xstring`, `xsymbol`, `xhandle`,
  `is_integer`, `integer_of`, `funcall`, `funcall_noargs`, `apply`,
  `put`, `get`, `plist_get`, `plist_put`, `plist_member`, `intern`,
  `intern_soft`, `setq_local`, `boundp`, `fboundp`, `makunbound`,
  `keywordp`, `defvar_false`, `prin1_to_string`, `prin1_string`,
  `cons_repr`, `cons_arg`;
- **predicate**: a procedure named like a Lisp predicate, ending in `_p`
  or in `p` after a type or position word (`charsetp`, `processp`,
  `bobp`); logical user options named `*_p` upstream are variables, not
  predicates, and drop upstream's p/-p (Q8);
- **constant**: a named constant ending in `_NIL`, `_T`, `_TRUE` or
  `_FALSE`, or in `_DISABLED`, `_NEVER`, `_OFF`, `_ON` or `_NO_LIMIT` (R2-Q16, R2-Q17);
- **vector**: a name ending in `_vector`;
- **interactive**: a component `interactive` of `command_type` (it is
  `is_command`);
- **verb**: a procedure, procedure pointer or type-bound procedure named
  `run`, `run_*` or `call_*` (see "One word per meaning"); the two runs
  of text properties text_properties_mod keeps as nouns (`run_property`,
  `run_position`) are in the baseline.
- **command-state**: the command loop's state declared as a
  `value_type` (A7): `this_command`, `last_command`, `real_this_command`,
  `real_last_command`, `last_repeatable_command` and
  `current_minibuffer_command` are `type(command_type), pointer`;
  `prefix_argument`, `current_prefix_argument` and `last_prefix_argument` are
  `type(prefix_argument_type)`.
- **abbreviation**: a name with the part `proc` or `procs` (R2-Q10);
- **interface**: an abstract interface whose name does not end in
  `_procedure` (R2-Q10, R2-Q19);
- **type**: a derived type whose name does not end in `_type` (R2-Q19);
- **underscores**: a name with a double underscore (R2-Q20);
- **regexp**: a name ending in `_re` or `_regex` (V3a);
- **test**: a name with the part `predicate`, `predicates`, `pred` or
  `preds` (R2-Q10); bidirectional_mod's `NEUTRAL_ON` (the Unicode bidi class ON)
  is in the baseline for the constant rule.
- **write-wrapper**: a function or subroutine whose executable part is a
  `write` into a local buffer followed by `trim` of it (`write (buffer,
  "(i0)") n` then `text = trim(buffer)`), or one `write` into its result
  (R6-44, see "Numbers to text" below); the generated docstrings_mod
  writes an integer setting's value inline (tools/gen-docstrings.sh), its
  `value_text` generic keeps only the other types.
- **number-text**: a reference to a procedure named `number_to_string`,
  `int_text`, `integer_text`, `itoa`, `int_str`, `int2str` or
  `to_string` (R6-44, R6-45).
- **char-loop**: a `do K = 1, min(len(A), len(B))` loop that orders the
  characters `A(K:K)` and `B(K:K)` with `<` or `>` (also through `ichar`
  or `iachar`): string-lessp by hand, whose port is utility_functions_mod's
  `is_string_less` (R6-45, see "Lisp functions by hand" below).
- **blank-skip**: a `do while` whose body is only `if (S(K:K) /= C1
  .and. S(K:K) /= C2 ...) exit` (each C a character literal, constant or
  `achar(N)`) and `K = K + 1` (or `K = K - 1`): it is `verify` (R6-45).
- **constructor-wrapper**: a function whose one executable statement
  assigns a structure constructor of its dummies to the result
  (`v = value_type(text=s)`); a callback that only clears its status
  (`status = status_type()`) names no dummy and is not one (R6-45).
- **duplicate-helper**: a definition of `is_string_less`,
  `is_string_prefix`, `string_to_number`, `int_text`, `string_trim`,
  `version_less`, `version_less_or_equal` (or `version_le`,
  `is_version_le`, `is_version_less`, `is_version_less_or_equal`,
  `version_to_numbers`), `char_text` or `list_delq` outside the module
  that holds the one port (utility_functions_mod, files_directory_mod, data_mod, none, none,
  basic_subprograms_mod: its `version_type` operators, editing_functions_mod's
  `character_to_string`, none: `pack`): a private copy of a Lisp function
  (R6-45).
<!-- fortran-names: on -->

The code of 2026-10-31, after B1 and A1, has 131 such findings, listed
as FILE:NAME:RULE in tools/intrinsics-baseline; PR 2 removes them.  The
check fails on a finding the baseline does not list and on a baseline
line that no longer occurs, so the baseline only shrinks; once it is
empty the check stays, with the empty baseline.

**Numbers to text (R6-44, R6-45).**  Text built from a format fixed in
the source is one internal `write` with edit descriptors, never Emacs's
`format`, a wrapper function, or a concatenation of converted numbers.
data_mod's `number_to_string`, a port of data.c `Fnumber_to_string`
that stood in about 150 places for `write (text, "(i0)") n`, is gone
(`write-not-number-to-string`, 2026-11-04), with the private copies
spellcheck_mod's `integer_text`, buffer_menu_mod's `buffer_id`,
simple_goto_mod's `counted`, compile_mod's `code_string` and
test_syntax_scan_mod's `string`; `number-to-string` is not applicable.
Where a number fed a larger piece of text, the whole piece is one
`write`:

```fortran
! before
s = "Day "//number_to_string(day)//" of "//number_to_string(d%year)//"; "//number_to_string(days_remaining)//" day"
! after
write (buffer, '("Day ", i0, " of ", i0, "; ", i0, " day")') day, d%year, days_remaining
s = trim(buffer)
```

gfortran 13 cannot write into an allocatable deferred-length string, so
the buffer is a fixed-length `character(len=N)` sized for the maximum
(20 digits per int64 plus the literal text and the `a` items' maximum
length; an `a` item of unbounded length sizes the buffer from `len()` of
it, as `character(len=len(string) + 160)`), then `trim`.  `trim` drops a
trailing blank the format meant to keep, so such a blank is appended
after the `trim`, or the variable part that follows it is concatenated
(`trim(count_text)//" "//noun`).  A number written where a local
allocatable string is also part of the message keeps the message's
concatenation: `write (digits, "(i0)") total` and then
`"Found `"//entry//"' ("//trim(digits)//" total)"`.  A format built at
run time is itself written first: `write (format, '("(f0.", i0, ")")')
digits` then `write (buffer, format) x`.  An internal write is allowed in
a pure procedure.

**Lisp functions by hand (R6-45).**  A Lisp function that Fortran has as
an operator or an intrinsic with the same behavior is not rewritten as a
loop, and no Lisp function is copied into the module that needs it.
Standard operators are used wherever they can be, over wrapper
functions (owner 2026-11-04): a comparison of values of a derived type
is a defined `operator(<)` and its kin, as basic_subprograms_mod's `version_type`
(`version(a) <= version(b)` for version<=), never `is_X_less(a, b)`.
A procedure behind an operator is private, the operator its only
spelling (owner 2026-11-04): basic_subprograms_mod exports `operator(<)` and its kin,
not `version_less`; calendar_mod exports `operator(==)`, not
`is_calendar_date_equal`.  C-h f still documents such a procedure under
its help name (its citation when that is a name, else its own name:
`version_less_or_equal`, `calendar_date_equal`) with the upstream
docstring and the operator form as its call line (`v1 <= v2`):
tools/gen-docstrings.sh reads `interface operator(OP)` blocks.
Examples:
string-prefix-p is `index(string, prefix) == 1` (an empty prefix
included), reverse of an array is the section `a(size(a):1:-1)`, delq of
a handle is `pack(list, .not. same_handle(list, h))` (handle_mod's
`same_handle` is elemental, and the one copy; window_mod re-exports it),
memq of a handle is handle_mod's `has_handle`, char-to-string is
editing_functions_mod's port (`call character_to_string(c, text, status)`; coding_mod,
a port of C, calls character_mod's `character_string`, the port of
CHARACTER_STRING), version< and version<= are the operators `<` and `<=`
between basic_subprograms_mod's `version_type` values (`version(a) < version(b)`,
owner 2026-11-04, so "1.0pre2", "1.0-git" and "22.3a" compare as
upstream; a caller that reports upstream's error checks the result's
`status` component), string-trim and the loops
that skip a set of characters are `verify(s, set)` (0 for all blank,
which the caller handles where the loop's end would have been `len(s) +
1`), `verify(s, set, back=.true.)` and `scan`, a string value is its
constructor `value_type(text=s)` where it is used, and string-to-number
on arbitrary text is data_mod's port (`call string_to_number(text, n[,
base])`, int64 or real64), which compile_mod, register_mod, version_control_subversion_mod,
buffer_menu_mod, diff_mode_mod, ibuffer_extensions_mod and directory_editor_auxiliary_mod call
instead of their copies.  string-lessp keeps its port, utility_functions_mod's
`is_string_less` (a generic: values, or character strings since
`lisp-slips`), because the comparison `<` pads the shorter string with
blanks: `"ab" < "ab "` is false, and a string whose next character after
a common prefix is below a blank (a tab, a control character) sorts
before that prefix, where string-lessp sorts the prefix first; the eight
private copies (`is_string_less`, `is_name_less`, `is_less`,
`string_less`) call it.  (`lisp-slips`, 2026-11-04; also gone:
`is_string_prefix`, `string_to_number`, `size_number`, `string_to_octal`,
`len_trim_ws`, `trim_trailing_space`, `reversed`, `delq`, `string_trim`,
`make_string` and three private `same_handle`; then `version_less`,
`version_le`, `is_version_le` (and later their replacements
`is_version_less`, `is_version_less_or_equal`, `version_to_numbers`,
`is_version_numbers_less(_or_equal)`, for the operators), 14 private char-to-string copies
(`char_text`, `character_string`, `char_of`, `char_utf8`, `char_string_of`,
`encode`, `utf8`, `internal_bytes`), buffer_mod's `list_delq` and
`in_list`.)

```fortran
! before
if (.not. is_less(tmp%text, sorted(j)%text)) exit   ! a private char-by-char loop
! after
if (.not. is_string_less(tmp%text, sorted(j)%text)) exit   ! utility_functions_mod's port

! before
k = 1

do while (k <= len(s))
    if (s(k:k) /= " " .and. s(k:k) /= TAB) exit
    k = k + 1
end do
! after
k = verify(s, " "//TAB)
if (k == 0) k = len(s) + 1
```

A character scalar has no reversing section: replace_mod's
`reverse_string` (the bytes of a line, for the common suffix) stays a
loop, as a helper that does what no intrinsic does.

tools/test-intrinsics.sh tests the check on the fixtures in
tools/intrinsics-test.

## One word per meaning

<!-- fortran-names: off -->
One meaning has one word.  Upstream runs commands, hooks and timers with
call, run, funcall and apply, and its internals mix fetch and get, new
and make, push and append; Fortmacs gives each meaning one word, in
identifiers, bindings and the docstrings that describe them (owner's
decisions V1 and V2, 2026-10-31).  Where Emacs gives two words to two
different behaviors, both stay.  The renames are per-name lines of
tools/fortran-names.d (never a `term`, so nouns such as `runs`,
`run_property`, `excluded_run` and `makefile_runtime_macros_list` keep
their word), and tools/intrinsics.awk's verb rule rejects a new
procedure, procedure pointer or binding named `run`, `run_*` or
`call_*`.

**Verb first (R6-17, R6-40).**  Names are verb first: `set_bookmark`
(bookmark-set), `execute_kmacro`, `update_font_lock`; a command without
a verb takes one from what it does (`undo_window_change`); keys and
behavior are unchanged.  The verb-last names in the tables below
(`kmacro_execute_*`, `hook_execute`, `table_destroy`, `history_append`,
`hook_append`, `buffer_order_append`, `process_order_prepend`,
`abbreviation_table_set_local`) keep their form until the verb-first rename
(R6-17).

**Running something: execute (V1).**  Commands, keyboard macros, hooks,
timers and processes are executed, user-visible names included; a
procedure pointer is called directly, with no wrapper that only
dispatches.

| Before | After |
|---|---|
| `call_interactively`, `call_last_kbd_macro` | `execute_interactively`, `execute_last_keyboard_macro` |
| `kmacro_call_macro`, `kmacro_call_ring_2nd` (and `_repeat`), `kmacro_end_or_call_macro`, `kmacro_end_and_call_macro`, `kmacro_call_repeat_key`, `KMACRO_CALL_REPEAT_*` | `keyboard_macro_execute_macro`, `keyboard_macro_execute_ring_2nd`, `keyboard_macro_end_or_execute_macro`, `keyboard_macro_end_and_execute_macro`, `keyboard_macro_execute_repeat_key`, `KMACRO_EXECUTE_REPEAT_*` (until the verb-first rename, R6-17) |
| `run_at_time`, `run_with_timer`, `run_with_idle_timer`, `run_timers` | `execute_at_time`, `execute_with_timer`, `execute_with_idle_timer`, `execute_timers` |
| `call_process`, `call_process_region`, `call_process_shell_command`, `call_shell_region`, `comint_run`, `vc_git_call`, `vc_git_run_command_string` | `execute_process`, `execute_process_region`, `execute_process_shell_command`, `execute_shell_region`, `command_interpreter_execute`, `version_control_git_execute`, `version_control_git_execute_command_string` |
| `run_mode_hooks`, `safe_run_hooks`, `run_buffer_list_update_hook`, `run_exit_minibuf_hook`, `run_undoable_change`, `call_mod_hooks`, `call_overlay_mod_hooks`, `jit_lock_run_functions` | `execute_mode_hooks`, `safe_execute_hooks`, `execute_buffer_list_update_hook`, `execute_exit_minibuf_hook`, `execute_undoable_change`, `execute_mod_hooks`, `execute_overlay_mod_hooks`, `just_in_time_lock_execute_functions` |
| `%run`, `%run_safe` of `command_type`, `major_mode_type`, the hook types, `yank_handler_function_type`, `version_control_action_type` and `version_control_log_operation_type` | `%execute`, `%execute_safe`: `call commands%paste%execute(status)`, `call text_mode_hook%execute(status)` |
| `hook_run`, `change_hook_run`, ... (the bindings' procedures), `run_kill_buffer_hook` (a procedure pointer) | `hook_execute`, `change_hook_execute`, ... (until the verb-first rename, R6-17), `execute_kill_buffer_hook` |
| emacs_mod's `run` (the program), `recentf_auto_cleanup_run`, `completion_at_point_run` | `execute_editor`, `execute_recent_files_auto_cleanup`, `execute_completion_at_point` |
| wrappers that do more than dispatch (a default, a status, a lookup): `call_indent_line_function`, `call_end_of_subprogram_function`, `call_get_old_input`, `call_callback`, `call_entry`, `call_rules`, `call_property_function`, `call_overlay_change_function`, `run_body_function`, `run_detector`, `run_action`, `run_point_hook`, `call_log_edit` | `execute_indent_line_function`, `execute_end_of_subprogram_function`, `execute_get_old_input`, `execute_callback`, `execute_entry`, `execute_rules`, `execute_property_function`, `execute_overlay_change_function`, `execute_body_function`, `execute_detector`, `execute_action`, `execute_point_hook`, `execute_log_edit`; the unused `call_diff_function` is gone |
| `vc_call_root`, `vc_call_responsible`, `is_vc_call_make_version_backups` | `version_control_execute_root`, `version_control_execute_responsible`, `is_version_control_make_version_backups` |
| `execute_charset` (it tests membership) | `match_character_set` |

**Other synonyms (V2).**  Internal helpers; upstream names keep their
words (a name whose citation uses the word stays: `buffer_fetch_character`,
`bidirectional_fetch_character`, `new_process`, `new_marker_at`, `push_mark`,
`append_to_register`, `overlay_put`, `put_text_property`).

| Meaning | Word | Examples |
|---|---|---|
| read a value | get (fetch) | `get_character_before` |
| construct | make_ (new_) | `make_info`, `make_node`, `make_table`, `make_token`, `make_keymap_slot` (keymap_mod's `make_keymap` is upstream's) |
| destroy | destroy_ (release, free_; R2-Q6) | `destroy_dead_window`, `table_destroy` (until the verb-first rename, R6-17), `destroy_marker_if_live`, `destroy_marker_position` |
| forget a buffer-local value | clear_local, clear_X_locals (drop_local, free_local, reset_local, kill_local; R2-Q3) | the setting objects' `%clear_local`, `clear_incremental_search_locals`, data_buffer_mod's `clear_local` |
| a setting's local value | the bindings `%set_local`, `%clear_local` (set_local_X, X_set_local, X_clear_local; panel 14) | `blink_matching_check_function%set_local(check)`, `electric_indent_characters%clear_local()`; `abbreviation_table_set_local` (until the verb-first rename, R6-17) |
| a setting's current value | the binding `%get_current` (current_X, `%current`, X_get, `%get`; R6-17, panel 14) | `fill_paragraph_function%get_current()`, `index_menu_patterns%get_current()` |
| whether a buffer has its own value | the binding `%is_local` (`%local_p`, X_local_p) | `forward_expression_function%is_local()`, `fill_column%is_local()` |
| store | set (put) | `set_current`, `prec2_set`; put-text-property's helpers (`put_property`, `put_syntax`) keep Emacs's word, and output keeps put (`terminal_put_bytes`, display_engine_mod's glyph `put`) |
| grow an array by one | append_X, prepend_X at the front (push, add_, add_X_front; R2-Q4) | `append_string`, `append_record`, `prepend_record`, `append_undo_entry`, `buffer_order_append` (until the verb-first rename, R6-17); a stack's push (terminfo_mod, Info's history) stays; version_control_git_mod's helper that appends many is `append_all`; moving to the front is `move_to_front` |
| draw the whole frame again; update what changed | redraw; redisplay (garbaged, SET_FRAME_GARBAGED, frame_garbaged; R6-9) | the frame flag `needs_redraw` (upstream `f->garbaged`), `redraw_frame`, `redraw_display`; `redisplay`, `redisplay_internal` |
| a pair of steps | X_begin, X_end (finish, save, restore; R2-Q9) | `major_mode_end` and the binding `%end`, `fn_begin`, `fn_end` |
| the one before | previous (prev) | `previous_character`, `previous_entry`, `previous_newline_byte`, and upstream's prev names too since 2026-11-05 (`window_previous_buffers`, `switch_to_previous_buffer`) |
| a conversion | X_to_Y (Y_from_X, X_as_Y) | `color_to_box`, `logical_to_flag`, `buffer_to_subject`, `register_to_number` |
| an interface | _procedure (_iface, _pred, _proc_x, _proc, _predicate; R2-Q10) | `syntactic_face_procedure`, `save_some_buffers_test_procedure`, `after_hook_procedure`, `ibuffer_buffer_test_procedure` |
| a procedure | its name, no _fn or _fun | `end_of_hunk`, `restrict_view`, `mark_state`, `is_marked`, `unmark_entry` |
| a logical function | is_* (check_, test_, verify_) | `is_matching`, `is_exact_completion`, `is_undefined_binding`, `is_visited_file_modtime_current` |

**Kept distinct** (Emacs gives them different behavior, R2-Q11, R2-Q12):
cut and delete (only cut keeps the text in the clipboard history), find,
get, query and read (visit, look up, ask, read input: read_ prompts the
user and waits, get_ looks up without a prompt), forward and next
(forward_ moves through text by a unit, next_ goes to the next item of a
list, C-n to the next screen line), display, describe, message and
print, insert, and init_ (one-time setup at startup).

### The second review (R2, 2026-11-01)

The owner's second review settled the words V1 and V2 left open.  Each
table is before and after; the per-name lines are in tools/fortran-names.d
(scoped by file where the name is local), and tools/intrinsics.awk
rejects the forms a script can see (below).

**Kill split by meaning (R2-Q2, R2-Q3).**  Text is cut, copied and
pasted, and the copies are kept in the clipboard history; the X or
Wayland clipboard is "the clipboard".  A buffer is closed, a process
stopped, Fortmacs exited, a buffer-local value cleared.  Keys are
unchanged (C-w, M-w, C-y, M-y, C-x k, C-x C-c); help and messages follow
("Copied to the clipboard history").

| Before | After |
|---|---|
| `kill_region`, `kill_line`, `kill_word`, `backward_kill_word`, `kill_whole_line`, `kill_sentence`, `backward_kill_sentence`, `kill_paragraph`, `backward_kill_paragraph`, `kill_sexp`/`kill_expression`, `backward_kill_expression`, `kill_rectangle`, `kill_comment`, `comment_kill`, `kill_matching_lines`, `kill_or_copy_matching_lines`, `kill_backward_up_list` | `cut_region`, `cut_line`, `cut_word`, `backward_cut_word`, `cut_whole_line`, `cut_sentence`, `backward_cut_sentence`, `cut_paragraph`, `backward_cut_paragraph`, `cut_expression`, `backward_cut_expression`, `cut_rectangle`, `cut_comment`, `comment_cut`, `cut_matching_lines`, `cut_or_copy_matching_lines`, `cut_backward_up_group` |
| `kill_ring_save`, `copy_region_as_kill`, `copy_rectangle_as_kill`, `dired_copy_filename_as_kill`, `ibuffer_copy_filename_as_kill`, `ibuffer_copy_buffername_as_kill`, `log_view_copy_revision_as_kill`, `diff_kill_ring_save` | `copy_region`, `copy_region_quietly` (the same without showing the copied text), `copy_rectangle`, `directory_editor_copy_filename`, `ibuffer_copy_filename`, `ibuffer_copy_buffername`, `log_view_copy_revision`, `diff_copy_region` |
| `yank`, `yank_pop`, `clipboard_yank`, `yank_rectangle`, `insert_for_yank`, `yank_handler`, `yank_excluded_properties`, `yank_transform_functions`, `yank_undo_function`, `rotate_yank_pointer`, `isearch_yank_kill`, `isearch_yank_char` (and the other `isearch_yank_*`), `bookmark_yank_word`, `ibuffer_yank` | `paste`, `paste_previous`, `paste_clipboard`, `paste_rectangle`, `insert_for_paste`, `paste_handler`, `paste_excluded_properties`, `paste_transform_functions`, `paste_undo_function`, `rotate_paste_pointer`, `incremental_search_paste`, `incremental_search_paste_character`, `bookmark_paste_word`, `ibuffer_paste` |
| `kill_ring`, `kill_ring_max`, `kill_ring_yank_pointer`, `kill_new`, `kill_append`, `current_kill`, `kill_transform_function`, `kill_do_not_save_duplicates`, `kill_read_only_ok`, `killed_rectangle`, `select_enable_clipboard`, delete-char's KILLFLAG | `clipboard_history`, `clipboard_history_maximum`, `clipboard_history_paste_pointer`, `copy_to_clipboard_history`, `append_to_clipboard_entry`, `current_clipboard_entry`, `clipboard_history_transform_function`, `clipboard_history_no_duplicates`, `cut_read_only_ok`, `last_cut_rectangle`, `share_clipboard`, CUT |
| text removed without the clipboard history (delete-region upstream): `bookmark_kill_line`, `diff_kill_junk`; lines removed from a listing ("Remove all marked lines"): `dired_kill_line`, `dired_do_kill_lines`, `dired_kill_subdir`, `ibuffer_do_kill_lines`, `vc_dir_kill_line` | `bookmark_delete_line`, `diff_delete_junk`; `directory_editor_remove_line`, `directory_editor_do_remove_lines`, `directory_editor_remove_subdirectory`, `ibuffer_do_remove_lines`, `version_control_directory_remove_line` |
| `diff_hunk_kill`, `diff_file_kill`, `diff_revert_and_kill_hunk`, `ibuffer_kill_line`, `ibuffer_kill_filter_group`, `ibuffer_filter_group_kill_ring` | `diff_hunk_cut`, `diff_file_cut`, `diff_revert_and_cut_hunk`, `ibuffer_cut_line`, `ibuffer_cut_filter_group`, `ibuffer_cut_filter_groups` |
| `kill_buffer`, `kill_buffer_hook`, `kill_buffer_query_functions`, `kill_this_buffer`, `kill_buffer_if_not_modified`, `log_edit_kill_buffer`, `quit_window_kill_buffer`, `view_kill_and_leave`, `shell_kill_buffer_on_exit`, `dired_kill_when_opening_new_dired_buffer`, `ibuffer_do_kill_on_deletion_marks`, `window_restore_killed_buffer_windows`, switch-to-prev-buffer's BURY-OR-KILL | `close_buffer`, `close_buffer_hook`, `close_buffer_query_functions`, `close_this_buffer`, `close_buffer_if_not_modified`, `log_edit_close_buffer`, `quit_window_close_buffer`, `view_close_and_leave`, `shell_close_buffer_on_exit`, `dired_close_when_opening_new_dired_buffer`, `ibuffer_do_close_on_deletion_marks`, `window_restore_closed_buffer_windows`, BURY-OR-CLOSE (process.c's `kill_buffer_processes` keeps its name) |
| `kill_process`, `kill_compilation`, `kill_grep`, `compilation_always_kill`, `comint_kill_subjob`, `vc_dir_kill_dir_status_process`; `stop_process`, `comint_stop_subjob` (they suspend a process: SIGTSTP) | `stop_process`, `stop_compilation`, `stop_grep`, `compilation_always_stop`, `comint_stop_subjob`, `version_control_directory_stop_directory_status_process`; `suspend_process`, `command_interpreter_suspend_subjob` |
| `kill_emacs`, `kill_emacs_hook`, `kill_emacs_query_functions`, `save_buffers_kill_emacs`, `save_buffers_kill_terminal` | `exit_fortmacs`, `exit_fortmacs_hook`, `exit_fortmacs_query_functions`, `save_buffers_exit_fortmacs`, `save_buffers_exit_terminal` (R6-21: fortmacs, not emacs) |
| `kill_local_variable`, `kill_all_local_variables`, `kill_local`, `kill_X_locals`, `kill_delayed_mode_hooks`, `kill_all_abbrevs`, data_mod's `killers` | `clear_local_variable`, `clear_all_local_variables`, `clear_local`, `clear_X_locals`, `clear_delayed_mode_hooks`, `clear_all_abbreviations`, `locals_clearers` |

R6-43 (2026-11-04): "stop" ends a process and "suspend" pauses it
(SIGTSTP), as the table above says; tools/fortran-names.d now records
the whole chain, so upstream's comint-stop-subjob and stop-process map
to `command_interpreter_suspend_subjob` and `suspend_process` in freshly ported code
and in tools/unported-commands.sh's probe, while the ported
`comint_stop_subjob` (comint-kill-subjob) and `stop_process`
(kill-process) are kept by name in the files that hold them.

**Growing an array (R2-Q4, R2-Q5).**  `append_X` adds at the end,
`prepend_X` at the front; no `_last`, `_front` or `add_`.  The config
API's typed `%add` is `%append` (`call ibuffer_formats%append(...)`; a hook's `%prepend` is add-hook's default place and `%append` APPEND, R3-2,
`call text_mode_hook%prepend(...)`).  A stack's push stays.

| Before | After |
|---|---|
| `add_auto_mode_last`, `add_auto_mode` (front) | `append_auto_mode`, `prepend_auto_mode` |
| `add_arg`, `add_record`, `add_undo_entry`, `buffer_order_add`, `add_local_variables_killer`, `add_save_some_buffers_function`, the bindings' `history_add`, `hook_add`, `item_add_string` | `append_argument`, `append_record`, `append_undo_entry`, `buffer_order_append`, `append_locals_clearer`, `append_save_some_buffers_function`, `history_append`, `hook_append`, `item_append_string` (`buffer_order_append`, `history_append`, `hook_append` and `item_append_string` until the verb-first rename, R6-17) |
| `add_record_front`, `add_shadow_front`, `add_front`, `process_order_add`, `add_in_progress`, `add_defaults` | `prepend_record`, `prepend_shadow`, `prepend`, `process_order_prepend` (until the verb-first rename, R6-17), `prepend_in_progress`, `prepend_defaults` |
| `%add`, `%add_string`, `%add_parent` | `%append`, `%append_string`, `%append_parent` |

**Ending a life; files on disk (R2-Q6, R2-Q13).**  Objects in memory
are made and destroyed; files and directories on disk are created and
deleted.

| Before | After |
|---|---|
| `free_marker`, `free_keymap`, `free_overlay`, `free_process`, `free_timer`, `free_window`, `free_char_table`, `ewoc_free`, `table_free` | `destroy_marker`, `destroy_keymap`, `destroy_overlay`, `destroy_process`, `destroy_timer`, `destroy_window`, `destroy_character_table`, `destroy_fortmacs_widget_for_object_collections`, `table_destroy` (until the verb-first rename, R6-17) (a free slot, a free buffer name and Dired's free space stay adjectives) |
| `make_directory`, `make_temp_file`, `make_symbolic_link`, `make_empty_file`, `make_backup_files` | `create_directory`, `create_temporary_file`, `create_symbolic_link`, `create_empty_file`, `create_backup_files` |
| `create_file_buffer`, `get_buffer_create`, `ewoc_create` | `make_file_buffer`, `get_or_make_buffer`, `make_fortmacs_widget_for_object_collections` |

**Lists of records (R2-Q8, R2-Q14).**  An alist, or an array named
`_list`, has a plural name without a suffix (replacing alist → `_table`);
a `_list` naming a listing (`list_buffers`, a tabulated list) or a
function returning one (`buffer_list`) stays.

| Before | After |
|---|---|
| `auto_mode_alist`, `auto_coding_alist`, `backup_directory_alist`, `bookmark_alist`, `register_alist`, `save_place_alist`, `compilation_error_regexp_alist_alist`, `grep_regexp_alist`, `insert_pair_alist`, `tty_color_alist` | `auto_modes`, `auto_codings`, `backup_directories`, `bookmarks`, `registers`, `save_places`, `compilation_error_regular_expressions`, `grep_regular_expressions`, `insert_pairs`, `terminal_colors` |
| where the plain plural is taken or unclear: `minor_mode_alist`, `coding_system_alist`, `conf_space_keywords_alist`, `font_lock_keywords_alist`, `display_buffer_alist`, `dired_compress_file_alist`, `dired_switches_alist`, display_buffer's action alist | `minor_mode_lighters`, `coding_system_names`, `configuration_space_keywords_by_file`, `font_lock_added_keywords`, `display_buffer_rules`, `directory_editor_compress_file_commands`, `dired_switches_by_directory`, `display_action_parameters_type` |
| `tab_stop_list`, `makefile_runtime_macros_list`, `recentf_list`, `timer_list`, `timer_idle_list`, `buffer_undo_list`, `help_event_list`, `info_directory_list`, `abbrev_table_name_list`, `auto_revert_buffer_list` | `tab_stops`, `makefile_runtime_macros`, `recent_files_files`, `active_timers`, `active_idle_timers`, `buffer_undo_entries`, `help_events`, `info_directories`, `abbreviation_tables`, `auto_revert_mode_buffers` |

**Pairs (R2-Q9).**  Steps around a block are `X_begin`/`X_end`: the
major modes' `%finish` binding is `%end`.

**Procedures and tests (R2-Q10, R2-Q19).**  No "proc" abbreviation: an
interface ends `_procedure` (every abstract interface), and "proc"
meaning a process is spelled process.  "Predicate" goes: a passed
logical procedure is named for its job, `test` (as REDUCE names
OPERATION); a setting holding one is `X_test_function`, or
`X_functions` where upstream's holds several; an array is filtered with
a logical mask (`pack`, `count`, `any` with an elemental `is_*`).

| Before | After |
|---|---|
| `command_proc`, `yank_handler_proc`, `process_filter_proc`, `after_change_function` (an interface), `ynp_actor`, `transpose_mover` | `command_procedure`, `paste_handler_procedure`, `process_filter_procedure`, `after_change_procedure`, `ynp_actor_procedure`, `transpose_mover_procedure` |
| `comint_check_proc`, `proc`, `procs`, `oldproc` | `command_interpreter_check_process`, `process`, `processes`, `old_process` |
| procedures and pointers named `_proc`: javascript_mod's and sgml_mode_mod's `indent_line_proc`, saveplace's `save_place_dired_hook_proc`, font_core_mod's `font_lock_mode_internal_proc`, `is_font_lock_specified_proc` | `indent_line_adapter`, `save_place_directory_editor_hook_adapter`, `font_lock_mode_internal_hook`, `font_lock_specified_test` |
| `minibuffer_completion_predicate`, `fill_nobreak_predicate`, `isearch_filter_predicate`, `electric_pair_inhibit_predicate`, `backup_enable_predicate`, `show_paren_predicate`, `ibuffer_never_show_predicates`, `completion_table_with_predicate`, a PREDICATE argument | `minibuffer_completion_test_function`, `fill_nobreak_functions`, `incremental_search_filter_test_function`, `electric_pair_inhibit_test_function`, `backup_enable_test_function`, `show_parenthesis_test_function`, `ibuffer_never_show_tests`, `completion_table_with_test`, TEST |
| the 27 types without `_type`: `logical_setting`, `integer_setting`, `string_setting`, `history_setting`, `function_setting`, `isearch_state`, `access_state`, `tty_display_info`, ... | `logical_setting_type`, `integer_setting_type`, `string_setting_type`, `history_setting_type`, `function_setting_type`, `incremental_search_state_type`, `access_state_type`, `terminal_display_info_type`, ... |

**Named constants (R2-Q15, R2-Q16, R2-Q17).**  A constant starts with
its option's full name; a value set several options share is defined
once under a shared stem only when the stem is the common start of the
sharing options' names (`DABBREV_*`, `SHELL_PLUS`
for the sh_indent_* choice settings, `DIRED_RECURSIVE_*`,
`ANSI_COLOR_*`); otherwise each option gets its own family with its full
name as prefix (`SGML_QUICK_KEYS_INDENT` and `HTML_QUICK_KEYS_INDENT`,
`REQUIRE_FINAL_NEWLINE_VISIT` and `MODE_REQUIRE_FINAL_NEWLINE_VISIT`), with
only the states that option takes, and code that passes a state from one
option to another maps the constants explicitly (owner, #261,
2026-11-04; changed R2-Q15, which had `FINAL_NEWLINE_*`).  The
vocabulary is `_NO` (off), `_YES` (on), `_NONE` (no item or value
chosen), `_ASK` (prompt each time) and `_ALWAYS` (only where a family
also has `_YES`); "no limit" is
`_UNLIMITED`; distinct behaviors keep their own names (`_FIRST_ERROR`,
`_VISIT_SAVE`, `SHELL_PLUS`).  Options are named positively (R2-Q18b).
Since R6-38 a setting's off and on states are `.false.` and `.true.`, not
`_NO` and `_YES` ("Choice settings"); the table shows the names of that time.
This applies to settings only: internal enumerations such as arguments,
record fields and face attribute states keep `_NO`/`_YES` (`FACE_NO`/`FACE_YES`,
`CASE_FOLD_*`; panel 2026-11-04).

| Before | After |
|---|---|
| `UPPER_CASE_NO`, `UPPER_CASE_NOT_YANKS`, `JUSTIFY_NONE`, `COMPILATION_SCROLL_NO`, `SHOW_PAREN_MIXED`, `SPLIT_LONGEST`, `UNTABIFY_METHOD_HUNGRY`, `DIFF_LABELS_CHECK`, `IBUFFER_HEADER_LINE_TITLE`, `COMINT_MOVE_OTHERS` | `SEARCH_UPPER_CASE_NO`, `SEARCH_UPPER_CASE_NOT_PASTED`, `DEFAULT_JUSTIFICATION_NONE`, `COMPILATION_SCROLL_OUTPUT_NO`, `SHOW_PARENTHESIS_STYLE_MIXED`, `SPLIT_WINDOW_PREFERRED_DIRECTION_LONGEST`, `BACKWARD_DELETE_CHARACTER_UNTABIFY_METHOD_HUNGRY`, `DIFF_USE_LABELS_CHECK`, `IBUFFER_USE_HEADER_LINE_TITLE`, `COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_OTHERS` |
| `DABBREV_OFF`, `DABBREV_ON`, `FACE_OFF`, `FACE_ON`, `FINAL_NEWLINE_NEVER`, `SAVE_ABBREVS_NEVER`, `KMACRO_EXECUTE_REPEAT_DISABLED`, `MESSAGE_LOG_DISABLED`, `IBUFFER_SHOW_LAST_ALWAYS` (no `_YES` beside it), `LOG_EDIT_CONFIRM_ALWAYS`, `VC_DIR_MASS_MARK_ALWAYS` | `DABBREV_NO`, `DABBREV_YES`, `FACE_NO`, `FACE_YES`, `FINAL_NEWLINE_NO`, `SAVE_ABBREVS_NO`, `KMACRO_EXECUTE_REPEAT_KEY_NO`, `MESSAGE_LOG_MAX_NO`, `IBUFFER_ALWAYS_SHOW_LAST_BUFFER_YES`, `LOG_EDIT_CONFIRM_YES`, `VC_DIR_ALLOW_MASS_MARK_CHANGES_YES` |
| `RECENTF_NO_LIMIT`, `SAVE_PLACE_NO_LIMIT`, `MESSAGE_LOG_UNLIMITED` | `RECENT_FILES_MAXIMUM_SAVED_ITEMS_UNLIMITED`, `SAVE_PLACE_LIMIT_UNLIMITED`, `MESSAGE_LOG_MAX_UNLIMITED` (`FILL_COLUMN_NONE` stays: no column set) |
| `shell_command_dont_erase_buffer`: `DONT_ERASE_NO`, `DONT_ERASE_ERASE`, `DONT_ERASE_BEG_LAST_OUT`, `DONT_ERASE_END_LAST_OUT`, `DONT_ERASE_SAVE_POINT` | `shell_command_keep_output`: `SHELL_COMMAND_KEEP_OUTPUT_NO` (the default), `_ERASE_CURRENT` (R3-4; first `_ERASE`), `_BEGINNING_OF_LAST_OUTPUT`, `_END_OF_LAST_OUTPUT`, `_SAVE_POINT` |
| `no_redraw_on_reenter` (step 42e) | `redraw_after_suspend` (.true. by default: the value inverted) |
| shared stems that were not the common start of their options' names (#261): `QUICK_KEYS_INDENT`, `_CLOSE`, `_DEFAULT`; `FINAL_NEWLINE_VISIT`, `_VISIT_SAVE`, `_ASK`; `COMINT_AUTOEXPAND_INPUT`, `_HISTORY` | `SGML_QUICK_KEYS_INDENT`, `_CLOSE` and `HTML_QUICK_KEYS_INDENT`, `_CLOSE`, `_DEFAULT`; `REQUIRE_FINAL_NEWLINE_*` and `MODE_REQUIRE_FINAL_NEWLINE_*`; `COMINT_INPUT_AUTOEXPAND_*` and `SHELL_INPUT_AUTOEXPAND_*` |

**Frame fields (step 42e).**  frame.h's flags are named for what they
say: new_size_p is `new_size_pending`, garbaged (SET_FRAME_GARBAGED) is
`needs_redraw`.

**Words spelled out (R2-Q18a, R2-Q20, V3, V3a; owner, 2026-11-05).**  No
abbreviation in any name, Emacs's own included: commands, variables,
functions, modes, hooks, faces, types, modules and files, and local
variables that stand for a thing (plain loop counters, `i`, `j` and `k`
used only as do variables in their loop, stay: panel 14).  No "beg": a
place is a beginning (BEG, `region_beg`, `line_beg` are BEGINNING,
`region_beginning`, `line_beginning`); "begin" is only the verb of a
pair.  have_ is has_; counts are `X_count` (`n_auto_modes`, `num_errors`
are `auto_mode_count`, `error_count`).  The word list (the entries of
tools/fortran-names.d/2026-11-05-spell-out-words):

- ordinary words: arg argument, attr/attrib attribute, auth
  authentication, bg/fg background/foreground, buf/buff buffer, bytepos
  byte_position, charpos character_position, cmd command, cnt count, col
  column, conf/config configuration, ctl/ctrl control, cur current, cwd
  current_working_directory, decl declaration, defdir default_directory,
  del delete, desc/descr description, dest/dst destination, dict
  dictionary, dir directory, dirline/dirname/dirstack directory_line,
  directory_name, directory_stack, elec electric, elem/elt element, env
  environment, esc escape, expr expression, fns functions, fun/func
  function, hist history, hpos/vpos horizontal_position/vertical_position,
  bpos/mpos buffer_position/marker_position, idx index, incr increment,
  lang language, lim limit, loc location, mid middle, misc miscellaneous,
  msg message, nav navigation, newdir new_directory, nl newline, num
  number, obuf output_buffer, opoint original_point, opt option, orig
  original, param parameter, paren parenthesis, passwd/pw password, pat
  pattern, perm permission, phys physical, pos position, prev previous,
  proc process, prop property, ptr pointer, rect rectangle, ref reference,
  rel relative, repo repository, sec second, sep separator, sig signal,
  spec specification, src source, stats statistics, std standard, str
  string, subdir subdirectory, subst substitute, suf suffix, syn syntax,
  sys system, temp/tmp temporary, tok token, util utility, val value, var
  variable, vec vector, win window, and their plurals;
- the words of Fortran intrinsics, in names only: char character, len
  length, int integer, min/max minimum/maximum, abs absolute, stat status
  (`old_len` is `old_length`, `point_min` `point_minimum`); the intrinsics
  themselves, their keywords and the names of the intrinsic modules keep
  their spelling (`len(x)`, `character(len=8)`, `stat=`, `len_trim`,
  `c_int`, `c_char`, `iso_fortran_env`), and so do the C bindings' names
  of C types (`c_ioctl_int`);
- Emacs's own words: regexp regular_expression (`re_` is
  `regular_expression_`: `re_search_forward` is
  `regular_expression_search_forward`), ppss parser_state, capf
  completion_at_point_function, begv/zv beginning_of_accessible_portion/
  end_of_accessible_portion, bol/eol beginning_of_line/end_of_line,
  bob/eob beginning_of_buffer/end_of_buffer, dwim do_what_i_mean, hex
  hexadecimal, kbd keyboard, sync synchronize, rev revision, ws
  whitespace, sym symbol, nil false; cat is category (the coding and
  completion categories; the cat program is only named in strings);
  flyspell and ispell are one family, spellcheck (`spellcheck_mode`,
  `spellcheck_programming_mode`, `spellcheck_buffer`, `spellcheck_word`;
  panel, 2026-11-05: Ispell keeps the plain names, and a Flyspell command
  that would take one ends in `_highlight`, `spellcheck_word_highlight`,
  `spellcheck_buffer_highlight`, `spellcheck_region_highlight`;
  flyspell-overlay is `spellcheck_highlight_overlay`; Flyspell's other
  names take no infix, `auto_correct_spellcheck_word`); init initialize
  (init_file and its kin initialization_file, js-indent-first-init an
  initializer); "diff" stays;
- package words (owner, 2026-11-05, for every name): vc version_control
  (hg mercurial, svn subversion, cvs concurrent_versions_system), dired
  directory_editor, ediff fortmacs_diff, comint command_interpreter, sh
  shell (sh-mode and the names that would meet shell.el's are
  `shell_script_mode`, ...), xref cross_reference, smie
  simple_minded_indentation_engine, isearch incremental_search, abbrev
  abbreviation, dabbrev dynamic_abbreviation, tty terminal, kmacro
  keyboard_macro, edmacro keyboard_macro_editor, imenu index_menu, bidi
  bidirectional, smerge simple_merge, js javascript, pcomplete
  programmable_completion, recentf recent_files, bmenu bookmark_menu,
  mule multilingual_environment, ewoc
  fortmacs_widget_for_object_collections, savehist save_history, async
  asynchronous, cm cursor_motion, etags fortmacs_tags (the etags program
  and the TAGS format keep theirs), jit just_in_time, charset
  character_set, prog programming, crm completing_read_multiple, rmc
  read_multiple_choice, recc regular_expression_character_class, bnf
  backus_naur_form, bom byte_order_mark, gui graphical_user_interface, rif
  redisplay_interface, subr basic_subprograms (transpose-subr's subr a
  subroutine), nts nonterminals, pcm partial_completion_mode, gmake
  gnu_make, eoh/eop end_of_headers/end_of_paragraph, bmk/bmrk bookmark, io
  input_output, cpp c_preprocessor, el element, defs definitions;
  ibuffer stays (a package name), and so do the module suffix `_mod`,
  POSIX utilities (ls, cd), the Tib program (tib) and cc (a cost, CC Mode,
  the Cc header); a name that would pass Fortran's 63 characters leaves
  out a repeated or least telling word (`dynamic_abbreviation_skip_leading_regular_expression`);
- prefixes and suffixes: a command's procedure is `execute_X` (cmd_X),
  a module's one-time setup `initialize_X` (init_X), an error code
  `X_CODE` (FM_X: `QUIT_CODE`, `ARITHMETIC_ERROR_CODE`); a variable that
  holds a command is `command_X`.

Standards and POSIX names keep their abbreviations: iso, ascii, utf,
ansi, html, css, xml, json, url, mime, rgb, cns, big5, gbk, sjis, cp932,
md5, sha*, f90, sgml, terminfo, smtp; pid, uid, gid, fd, errno, getenv,
mkdir, chmod, ioctl, and the fields and constants of the Linux structures
src/sys hardcodes (`tv_sec`, `tm_min`, `pw_dir`, `ws_row`).  Keys,
mode-line text, prompts and messages keep their words; the word entries
of the table rename text only where a name is written in it (a word with
an underscore, or, in a comment, an upper-case or quoted argument name:
`BUF`, `` `pos' ``), so program text in tests, environment variables and
file names stay (keep entries for the rest).  Citations keep upstream's
names.  No aliases: an old name is gone, not kept for completion, M-x
or help; where an upstream alias would take its command's spelled-out
name (javascript-mode, js-mode's alias), the two are one command.  A
file written for Emacs still gives its mode by upstream's name: modes_mod
(tools/gen-modes.sh) holds a table of the upstream names of the modes
whose Fortran name differs, read from the citations of the modes'
procedures, and files_mode_mod consults it only when NAME_mode is not a
mode (`-*- mode: sh -*-` is `shell_script_mode`, `mode: conf-unix`
`configuration_unix_mode`; panel, 2026-11-05).  Where
two spelled-out names meet, CLAUDE.md's suffix rule applies (`_variable`:
which-func-mode is `which_function_mode_variable` beside the minor mode
`which_function_mode`); otherwise the one that is not the plain Emacs name
gets a name for its role (`incremental_search_delete_end_character` beside
`incremental_search_delete_character`, `command_interpreter_range_arguments` beside
`command_interpreter_arguments`, `winner_current_configuration` beside
`winner_configuration`, the face `which_function_face` beside the
function `which_function`, `shell_indentation_variables` beside
`shell_variables`; panel, 2026-11-05: the hook variable
ispell-kill-ispell-hook is `spellcheck_kill_spellcheck_hook_variable`
beside the function flyspell-kill-ispell-hook).  No double underscore: where the plain name
is taken a suffix names the role (`smie__parent`, `smie__after`,
`shell__unquote_argument`, `rectangle__insert_for_yank` are
`simple_minded_indentation_engine_parent_variable`, `simple_minded_indentation_engine_after_variable`,
`shell_unquote_argument_internal`, `rectangle_insert_for_paste_internal`;
`syntax_propertize__done` is `syntax_propertize_done`).

**Checks.**  tools/intrinsics.awk (PORTING.md, "Intrinsics first")
rejects new forms: the `abbreviation` rule (a part `proc` or `procs`),
`interface` (an abstract interface not ending `_procedure`), `type` (a
derived type not ending `_type`), `underscores` (a double underscore),
`regular_expression` (a name ending `_re` or `_regex`), `test` (a part `predicate`
or `pred`), and the `constant` rule's `_DISABLED`, `_NEVER`, `_OFF`,
`_ON` and `_NO_LIMIT`.
<!-- fortran-names: on -->

### The third and fourth reviews (R3, R4, 2026-11-02)

<!-- fortran-names: off -->
The owner settled the questions steps 41 and 40d raised; the audit of
2026-11-02 applied them (the PR `audit-fixes`).

| Question | Decision |
|---|---|
| stop-process (SIGSTOP) beside kill-process (R3-1) | `suspend_process` suspends (`continue_process` resumes), `stop_process` ends; `command_interpreter_suspend_subjob` (C-c C-z), `comint_stop_subjob` (C-c C-k) |
| add-hook's two places (R3-2) | `%prepend(fn, depth, local)` is add-hook's default (the front, depth 0), `%append(fn, depth, local)` add-hook with APPEND (depth 90, upstream's `t`); `%remove` is remove-hook; every ported `(add-hook ...)` uses the one upstream's arguments mean |
| face attribute states (R3-3) | `FACE_NO`, `FACE_YES`, `FACE_UNSPECIFIED`, `FACE_COLOR` stay |
| shell-command-dont-erase-buffer's fifth value `erase` (R3-4) | `SHELL_COMMAND_KEEP_OUTPUT_ERASE_CURRENT` (erase even the current buffer); `.false.` is the default (R6-38) |
| logical fields named `*_p` upstream (R3-5) | the plain name (`ascii_compatible`, `code_linear`, `supplementary`); `is_`/`has_` only for logical functions |
| reading a table's fields (R3-6) | the table is a public protected array read directly, with `find_X(name)` for the lookup; no field-reading accessor functions |
| acronyms (R3-7) | an encoding's own short name stays (`SJIS`, like `UTF8` and `ISO`) |
| a function with a side effect (R3-8) | a subroutine: coding.c's ONE_MORE_BYTE is `call take_byte(b, ended)`; a function that defines something returns the id through `intent(out)` (the Hazards checklist) |
| set-language-info-alist (R3-9) | `define_language_environment` |
| a variable's module (R3-10) | the module of its upstream file (`system_messages_locale` in fortmacs_mod); a cycle is broken with a submodule, never by moving the variable |
| ported user-visible text (R3-11) | follows the renames (list_character_sets' header says CHARSET-NAME), also when a future upstream version is translated |
| a missing value in help (R3-12) | shown by its type: a logical `.false.`, a string `""`, a pointer or procedure `null()`, an optional argument absent |
| Quail's "library" (R3-13) | module: `quail_package_module_type`, `define_quail_packages_procedure`, `quail_package_modules` |
| skeleton.el's elements and expressions (R4-1, R4-2) | named for what they do: `SKELETON_NEWLINE`, `SKELETON_POINT`, `SKELETON_POINT_OVERRIDE`, `SKELETON_INDENT`, `SKELETON_POSITION`, `SKELETON_IF_MOVED`, `SKELETON_IF_NOT_MOVED`, `SKELETON_STRING`, `SKELETON_RESUME`, `SKELETON_V1`, `SKELETON_V2`, `SKELETON_END`; a quoted expression is `skeleton_action(p)` (run for its effect), another `skeleton_value(p)` (sets `skeleton_state%value`); a quit's data are `SKELETON_QUIT_ITERATION` and `SKELETON_QUIT_RECURSIVE`, and a quit prints "Quit" |
| the pre-filled prompt text (R4-3) | `initial_input` everywhere: `skeleton_state%initial_input` and `%initial_position`, and the readers' argument (`read_from_minibuffer`, `completing_read`, `read_file_name`, crm) |
| skeleton-pair-alist and the others (R4-4) | `skeleton_pairs`, `skeleton_default_pairs`, `shell_skeleton_default_pairs`: "pair" is bracket pairing (Electric Pair); the entries are `skeleton_pair_type(opening=, closing=)` |
| skeleton-further-elements (R4-5) | a buffer-local procedure `skeleton_further_elements(bind)`; the rule: entries of one shape are an array of records, settings of differing types are assigned in a procedure ("Lists in the config API") |
| per-shell skeletons (R4-6) | `skeleton_variants_type`, chosen by `skeleton_filter_function` (sh-mode's `shell_skeleton_feature`) |
| smie-config's rules (R4-7) | `simple_minded_indentation_engine_configuration_rule_type(mode=, method=, token=, normal=, new=, normal_kind=, new_kind=)`; messages and prompts in Fortran form (`SIMPLE_MINDED_INDENTATION_ENGINE_AFTER "then" -> 4`) |
| sgml-tag-alist's end-tag field (R4-8) | `SGML_END_TAG_NONE` (absent), `_DEFAULT` (an explicit nil), `_NO` (t), `_NEWLINES`, `_SKELETON` |
| sgml-quick-keys and html-quick-keys (R4-9; changed by #261) | `.false.`, `.true.` (R6-38) and a family each (owner, #261, 2026-11-04): `SGML_QUICK_KEYS_INDENT`, `_CLOSE`; `HTML_QUICK_KEYS_INDENT`, `_CLOSE` (taken from sgml-quick-keys), with `HTML_QUICK_KEYS_DEFAULT` for "follow sgml-quick-keys"; R4-9 had one shared `QUICK_KEYS_*` set |
| the command and the variable sgml-tag-help (R4-10) | `sgml_tag_help` and `sgml_tag_help_variable` (the `_variable` rule) |
| procedure-valued variables (R4-11) | plain procedure pointers (`sgml_transformation_function => upcase_tag_name`); tools/gen-docstrings.sh documents them (the value is `null()`, the default target's name while it is the target, else "a procedure set by the configuration"); the defcustoms among them are in tools/user-options.d |
| a place in our own helper names (R5-1) | "beginning", as in upstream's names: `_beginning`/`_end`, `line_beginning`, `hunk_beginning`; "start" only as a verb.  The `_start` names (58 procedures) are renamed in a PR of their own |
<!-- fortran-names: on -->

### Known debt (the audit of 2026-11-02)

What a review of the whole tree and of four sampled modules against
upstream measured; the numbers are the baselines to shrink.

- **Compile-time things reached through Lisp data.**  tools/lisp-data-baseline
  counts them per file: `value_type(name="...")` 749 uses, `set(value_type(name=`
  99, `symbol_value(value_type(name=` 50, `intern("` 13, `select case (name)`
  11, in 78 files.  The three places to start, which all four module reviews
  named: a key's binding is a name value that the command loop resolves with
  `find_command(name)` on every keystroke (`command_of`, execute_interactively_mod;
  `keymap_set` and `keymap_global_set` take the command as a string), so a
  typed binding (command pointer, keymap handle, macro, unbound) and
  `execute_interactively` taking `command_type` come first; `inhibit_read_only`
  and the clipboard history are reached through the name table
  (simple_mod); composite keymaps are value lists with the symbol `keymap` as
  a marker and `current_active_maps` returns one.  C3 and C4 ("Decided, not
  yet applied") finish the job: `xfixnum` 223 uses, `xstring` 212, `xsymbol`
  108, `textget` 45, `boundp` 11, `setq_local` 8, `defvar_false` 5, `memq_p` 10.
- **Commands.**  Of the 1725 interactive subprograms in the 104 Elisp files cited
  as ported, 780 (45%) are commands; tools/unported-commands lists the rest
  (945), which the status files named only 243 of.  Procedures that exist
  but are not registered as commands count as unported (`delete_file`,
  `save_buffers_exit_fortmacs`).
- **Tests.**  107 ported files have an upstream -tests.el; 18 are cited by
  no test: add-log, auth-source, doc, edmacro, env, executable, font,
  format-spec, help-fns, help-mode, jit-lock, map-ynp, startup, url-util
  (and alloc, eval, lread, print, which are Lisp machinery).  keymap-tests
  has 6 more tests portable today; subr-tests' replace-*-in-region and
  match-substitute-replacement are not ported.
- **Names.**  About 100 dummies and components still end in `_fn` (V2:
  `region_fn`, `replacement_fn`, `until_success_fn`, ...); the `_start`
  helpers become `_beginning` (R5-1).
- **Behavior at the margins**, found by sampling and not yet fixed: the
  progress reporter never pulses (basic_subprograms_mod), `cut_line` uses
  `line_end_position` where upstream skips invisible text, kill-append's
  replace rule, `-` after digits in universal-argument, "Mark set" while a
  macro runs, no `activate-mark-hook`, `number_input_keys` counted inside
  read_key_sequence, `where-is-preferred-modifier` ignored, `execute_error`
  without `cancel_echoing`, `this-original-command` missing,
  `delete_active_region` a logical though upstream has three states,
  `character_set_mod`'s accessor layer against R3-6, `define_character_set_internal` a
  function with a side effect (R3-8).  Each goes into its module's status file as
  Deferred when it is not fixed in the PR that touches the module next.
<!-- fortran-names: keep -->

## Fortran-first decision guide

<!-- fortran-names: off -->
The renames and checks above cover the Lisp-isms already decided; this
guide is for the next one.  It records how the owner decides, so a
session or agent can work without supervision and still not drift into
Lisp written in Fortran.  Run it before naming anything, wording a
docstring or choosing a data structure for something that comes from Lisp.

### The questions, in order

1. **Is there an intrinsic or a language construct for it?**  Use it
   directly (see "Intrinsics first"): `allocated` for "is it nil",
   `findloc`/`any` for `memq`, array sections for `cdr`, `associated` to
   compare commands, `select case` on a tag for a type test.  No helper
   that only wraps one.
2. **What does the Fortran standard call it?**  Use the standard's own
   word: subprogram (defun), procedure pointer (function value), named
   constant, derived type and component (struct, plist), array (vector,
   list of strings), "an error condition occurs" (signal), STAT/status
   and RETURN (throw/catch), present/absent (optional arguments),
   allocated/unallocated, associated/null.
   Search before proposing, and list what was searched in any question
   to the owner (R6-4): the intrinsic procedures and their dummy-argument
   names (`system_clock`'s `count`/`count_rate`, `date_and_time`'s
   `values`, `execute_command_line`'s `cmdstat`, `random_number`'s
   `harvest`), the standard modules (`iso_fortran_env`,
   `ieee_arithmetic`, `iso_c_binding`), ISO/IEC 1539-2's `varying_string`,
   and fortran-lang stdlib's names (`string_type`).  A term found there is
   the answer, not a question: `time_type`'s fraction of a second is
   `count`/`count_rate`, not upstream's TICKS/HZ (R6-3).
3. **Is there an established Fortran convention?**  Follow it: the `_mod`
   module suffix, `read_*` for prompts (READ is Fortran's input verb),
   names verb first (R6-17, R6-40: `set_bookmark` for bookmark-set,
   `execute_kmacro`, `update_font_lock`; a command without a verb takes
   one from what it does, `undo_window_change`; keys and behavior
   unchanged), `is_*`/`has_*` for logical functions, plural array names without
   `_array`/`_vector`/`_list`/`_table` (R2-Q8, R2-Q14), `_type` on every derived type and `_procedure` on every interface and no other keyword suffix (R2-Q19), "beginning" for a place, in our own helpers too (`line_beginning`,
   `hunk_beginning`; R2-Q18a, R5-1), "start" only as a verb, a
   tagged record with `select case` where `select type` is blocked.
4. **What does Fortmacs already use for the same role?**  Grep first and
   match it (the owner asks this whenever two forms exist).
5. **Does the result read well?**  A mechanical rename that doubles a
   word (`table_table`), collides with an existing name (`syntax_table`,
   `display_table_type`, a module's `table`) or reads as a double
   negative (`KMACRO_NO_REPEAT_NO`) is not applied: propose a better name.
6. **Is it a type question in disguise?**  A setting with two states is a
   `logical` (.true./.false.); more states make a choice setting
   (R6-38) whose `=` takes .true./.false. for its on and off states, which
   mean what the name plainly says (R6-46), and named constants for the
   others, named with the option's full name after their meaning
   (COMPILATION_SCROLL_OUTPUT_FIRST_ERROR; the vocabulary _NONE, _ASK,
   _ALWAYS beside an on state, _UNLIMITED), never
   `_NO`/`_YES`/`_NIL`/`_T`/`_TRUE`/`_FALSE` (internal enumerations such
   as arguments, record fields and face attribute states keep
   `_NO`/`_YES`: `FACE_NO`/`FACE_YES`, `CASE_FOLD_*`; panel 2026-11-04);
   a function value is its
   `%procedure` (R6-37), a string or number value a named component
   (R6-39) ("Choice settings").  A pair is a derived type with named
   components (`decoding`/`encoding`), never a generic "pair".  A value
   held "by name" is resolved once to a pointer.  Compile-time things are
   reached through their typed objects, never by a name string.
7. **Does gfortran allow it?**  Fortmacs targets gfortran only (13 and
   later).  Check CLAUDE.md's hazards; when unsure, write a ten-line
   spike and run it under valgrind (as for recursive allocatable types,
   which gfortran 13.3 still copies shallowly).

### What stays Lisp

Upstream citations ("Port of lisp.el:forward-sexp"), upstream names quoted
as upstream names, the Lisp language modes and their keywords, Lisp source
in test fixtures, Info node names such as `(elisp)Hooks`, and Emacs's own
vocabulary that is not Lisp: buffer, window, frame, keymap, mode, hook,
face, marker, overlay, text property, minibuffer, clipboard history, prefix
argument, keyboard macro, process filter and sentinel.  Keys and mode-line
text are unchanged.

### When the answer is not clear: ask

Cases one existing rule or pattern covers unambiguously (e.g. upstream
nil/t → `.false.`/`.true.` of a choice setting, `_p` → `is_*`) are
decided by Claude and reported, not asked.  The report names each case
and the rule it follows, so the owner can still overrule it.

A Lisp-derived term with no line in tools/fortran-names.d, a case where
rules conflict, or a choice the questions above do not settle exactly,
goes to the decision panel (CLAUDE.md), and what the panel does not
settle to the "Owner questions" issue (#261) for the owner; it is never
invented and never copied from Lisp.  Ask as a multiple-choice question,
one decision at a time:

- state what the thing is and where it occurs (with counts), so the
  question stands alone;
- put the recommended option first, with the reason in Fortran terms
  ("findloc is a standard intrinsic", "the standard calls it ...");
- show real before/after examples from the code for each option;
- never offer an option that contradicts an earlier decision (check this
  section and tools/fortran-names.d first);
- when the owner asks "which is more Fortran?" or for more examples,
  answer that and ask again; do not take the question as a choice.
- answer such a request in the question itself, with the options and
  their previews again, not in text outside it.

Each answer is recorded at once: a line in the pull request's own file
of tools/fortran-names.d (or a keep entry), a row in "Fortran names for
Lisp terms", a rule in CLAUDE.md when it is general, and the check that
enforces it.

### Catching what nobody has decided yet

tools/lisp-candidates.sh, run by tools/check.sh, lists Lisp vocabulary
(tools/lisp-candidates: funcall, lambda, symbol, cons, quote, eval,
progn, interactive, vector, ...) in the lines a branch adds.  It only
warns: many hits are fine (a keyboard macro, a POSIX signal).  Every PR
description lists the candidates it adds with their decision: asked and
confirmed (now in tools/fortran-names.d), or why it is not a Lisp-ism here.

### The review of 2026-10-31, as examples

The decisions that shaped this guide, with the owner's reasoning:

| Question | Decision | Why |
|---|---|---|
| defun | subprogram | the standard's term for the source text of a function or subroutine; "procedure" is the callable entity |
| a mode's own defun helpers | that language's term: `python_navigation_beginning_of_definition`, `makefile_add_log_target`, `shell_current_function_name`, `log_view_beginning_of_entry`, `css_current_rule_name`; generic commands, hooks and helpers that serve every mode keep subprogram (`search_subprogram_forward`, `add_log_current_subprogram`) | Python's reference calls def and class definitions; a Makefile has targets, a log entries, CSS rules |
| sexp | expression | Fortran's primaries are names, constants and parenthesized groups, what C-M-f moves over |
| list (a parenthesized group) | group | "list" means argument and I/O lists in Fortran |
| alist | first `_table`; since R2-Q8 a plural name (`auto_modes`; `compilation_error_regular_expression_names` where it is only names) | an array of records is named for what it holds, as any array; only a real table gets `_table` |
| `*_NIL`/`*_T` constants | meaningful names; two states become a logical | Fortran's own multi-state constants (IOSTAT_END, IEEE_POSITIVE_INF) are named for their meaning |
| isearch-case-fold-search (nil, t, `yes`) | `CASE_FOLD_NO`, `CASE_FOLD_YES`, `CASE_FOLD_FORCED` | `yes` is M-c's forced folding; t is the plain "yes" |
| kmacro-call-macro's NO-REPEAT | `KEYBOARD_MACRO_REPEAT_YES`, `KEYBOARD_MACRO_REPEAT_NO`; the argument is REPEAT | names read positively, never as a double negative (`KMACRO_NO_REPEAT_NO`) |
| constants shared by several settings (GREP_NIL, GREP_T) | one family per setting: `GREP_USE_NULL_DEVICE_*`, `GREP_USE_NULL_FILENAME_SEPARATOR_*`, `GREP_HIGHLIGHT_MATCHES_*` | each setting has its own states (only grep-highlight-matches has `always` and `auto`), so a generic pair would admit values the setting does not have |
| face attribute states | `FACE_ON`, `FACE_OFF` kept; since R2-Q17 `FACE_YES`, `FACE_NO` | one constant vocabulary everywhere (`_NO`, `_YES`, `_NONE`, `_ASK`, `_ALWAYS`) |
| kill (R2-Q2) | by meaning: text is cut, copied and pasted into the clipboard history; a buffer closed, a process stopped, Fortmacs exited, a local value cleared | one word per behavior: Emacs's "kill" means five different things |
| growing an array (R2-Q4) | `append_X` at the end, `prepend_X` at the front; the config API's `%append` | the word says where the entry goes |
| ending a life; files (R2-Q6, R2-Q13) | `make_`/`destroy_` for objects in memory, `create_`/`delete_` for files and directories on disk | Fortran's own pairs: ALLOCATE/DEALLOCATE in memory, OPEN with STATUS="new" and CLOSE with STATUS="delete" on disk |
| keyword suffixes (R2-Q19) | only `_type`, `_procedure` and the module suffix | Fortran has one case-blind namespace, so only a type or an interface would clash with a variable of the same name |
| "proc", "beg", `__` (R2-Q10, R2-Q18a, R2-Q20) | spelled out (`process`, `beginning`), the role as a suffix where a plain name is taken (`simple_minded_indentation_engine_parent_variable`) | names read as words |
| wrong-type messages | "Wrong type argument: expected X, got VALUE", with a qualifier where the bare type loses what was checked: `integer (a character code)`, `integer (>= 0)`, `live type(window_type)`, `name`, `array` | it says what a declaration would say; predicate names (`characterp`) never appear |
| values with no Fortran type | a null `value_type` is "unallocated"; a 0 or -1 sentinel is "none"; an absent prefix argument is "no prefix" | "unallocated" matches the test `.not. allocated(v%a)` |
| pair (a cons) | never said: our types name their components, and upstream data is "two values, X and Y" | there is no pair type in Fortran; no "upstream's cons" aside |
| obarray, symbol table | the name table holds run-time names only (`find_name`, `add_name`); compile-time things are typed objects | names typed by the user are the only names looked up at run time |
| defvar'd history lists | typed history objects (`type(history_setting_type)`, with `%length`) | a history is a record with a known shape |
| setq-local | `%set_local()` of the setting | the buffer-local value is a component operation of the typed setting |
| funcall, apply | the procedure pointer called directly | Fortran calls a procedure pointer like a procedure |
| boundp, fboundp, makunbound | direct components and `associated()` | `associated` is the standard test for a pointer |
| register value | `register_contents_type` | what a register holds |
| interactive (of a command) | `is_command` | a logical component is named for what it states |
| nil / non-nil in help | the word for the value's type (.true., present, unallocated, null, "") | each sentence uses the Fortran word for that type |
| signal / throw | "an error condition occurs"; "a check of the status"; "return to X with a status" | the standard's STAT=/IOSTAT= wording; EXIT only leaves a construct, "handle" is not Fortran |
| nilp, car, memq, fixnump | `.not. allocated(v%a)`, sections, `findloc`, `select case` on the tag | intrinsics and constructs, no wrappers |
| `select type` for values | tagged `select case` (json-fortran style) | gfortran 13.3 shallow-copies recursive allocatable types (spike: changing a copy changed the original, then a segfault) |
| y_or_n_p | read_y_or_n | READ is the input verb, and Emacs's read_string/read_number match |
| `_vector` | plural names | Fortran does not suffix arrays; the rank is in the declaration |
| this_command by name (A7, applied) | `type(command_type), pointer` with `associated()` | `associated(p, t)` is a standard intrinsic for identity |
| a keyboard macro in this-command (A7a, applied) | a `command_type` with a `macro` handle into the registry of macro events `command_macros` (`last_macro_command`) | the command state stays one type; the same object serves keys bound to macros |
| prefix_argument (nil, a number, `-`, (4)) (A7, applied) | `prefix_argument_type`: a kind (`PREFIX_NONE`, `PREFIX_NUMBER`, `PREFIX_RAW`, `PREFIX_MINUS`) and a value | a tagged record with named constants, tested with `select case` |
| put/get | typed components; `findloc` over a `text_property_type` array | CLAUDE.md forbids property bags; findloc is standard (Fortran 2008) |
| C-h f signature | the call line and a declaration block | what one writes in Fortran to call or wrap the procedure |
| `_fm` module suffix | `_mod` | the most common Fortran module suffix; `_f90` would read as the Fortran 90 dialect |
| lisp-name in gen-docstrings | help-name | the value is the name C-h f looks up, not the Lisp name |
| British spelling | American | owner's rule for all text |
| conditional expressions (Fortran 2023, `(c ? a : b)`) | not used: a plain IF | gfortran 13 lacks them and Fortmacs builds with -std=f2018; the IF statement is not obsolete |
| call, run, funcall (running something) | execute: `execute_interactively`, `execute_with_timer`, `%execute`; a procedure pointer called directly | one meaning, one word; EXECUTE is the word Fortran's own EXECUTE_COMMAND_LINE uses |
| synonyms for one meaning (fetch/get, new/make, release/free, push/append/add, prev/previous) | one word each (see "One word per meaning"); Emacs words with different behaviors stay distinct (kill/delete, find/get/query/read, forward/next) | a reader learns one word per meaning; two words promise two behaviors |
<!-- fortran-names: on -->

## Style

- The [fortran-lang style guide](https://fortran-lang.org/learn/best_practices/style_guide/):
  lowercase keywords and names, words separated by underscores, short but
  unambiguous names. Like the [stdlib guide](https://stdlib.fortran-lang.org/page/contributing/StyleGuide.html):
  `intent` on every dummy, named `end` statements, one module per file,
  FORD `!>` comments on public entities, 4-space indentation; but 132
  columns (stdlib's hard limit) rather than 80.
- Blank lines as CLAUDE.md sets them (chosen with the user, 2026-10-29):
  in procedure bodies around block constructs and after the declarations,
  one around `contains`, between procedures and after a section banner
  between procedures, one after the file's header comment (2026-10-30,
  after docs/init.f90 ran its header into `module`); module specification
  parts, interface blocks and type definitions keep theirs.
  `tools/blank-lines.awk` applies them (also to the generated files), and
  `tools/check.sh` fixes or checks them.
- Enforced by `tools/check.sh` (and so CI): fprettify for layout
  (`.fprettify.rc`), Fortitude 0.9.2 with its default rules for the rest
  (`[extra.fortitude.check]` in fpm.toml). Notable rules: double-quoted
  strings, `use ..., only:` everywhere, `implicit none` in interface
  bodies, a `case default` in every `select case`, no misleading one-line
  `if` continuations. Justified exceptions are marked in the source with
  `! allow(rule-name)` and a comment saying why.

## Hazards

The pitfalls of Fortran and gfortran that bit this port.  `tools/hazards.awk`
(run by `tools/check.sh`, so by CI on every PR) reports those a script can
find; a justified exception carries `! hazard-ok(NAME): why` on the
statement or the comment line above it.  Checked:

- `short-circuit`: `.and.` and `.or.` evaluate both sides, so the right
  side must not use what the left side guards (allocated, associated,
  present, a compared len or size, an index compared with a bound): nest
  an `if` instead.
- `implied-save`: an initialized local declaration implies `save`.
- `deferred-result-pointer`: gfortran frees a procedure pointer to a
  function with an allocatable or deferred-length result: make the
  interface a subroutine with an `intent(out)` argument.
- `allocatable-constructor`: a function result or structure constructor
  of a type with allocatable components inside an array constructor
  leaks: use `list(...)`, a variable or element assignment.
- `allocatable-result-inquiry`: `size` (or `lbound`, `ubound`, `shape`)
  of the result of a function or binding returning an allocatable array
  of a type with allocatable components leaks the elements' components
  (valgrind on `size(tabulated_list_entries%get_current())`, #304): a
  getter of such an array is a subroutine with an `intent(out)` argument
  (`call tabulated_list_entries%get_current(entries)`), or the result is
  assigned to a variable first.
- `recursive-allocatable-type`: an allocatable component of its own type
  is copied shallowly.
- `command-component`: any allocatable component of `command_type`, a
  deferred-length string included, or a component of a type with
  allocatable components (such as `value_type`), makes gfortran generate
  copy and finalization code for each of the 1,200 generated commands:
  compiling commands_mod at -O3 took 581 s with the macro events as a
  component, 134 s with the five remaining strings and the one array,
  and 1 s with none (measured on a probe of the real table, 2026-11-02).
  Strings are fixed length (`DISABLED_MESSAGE_MAXIMUM`, `ADVERTISED_BINDING_MAXIMUM`,
  `REPEAT_HINT_MAXIMUM`, `INFO_FILE_MAXIMUM`; "" for none), a list is a fixed
  array (`repeat_continue(REPEAT_CONTINUE_MAXIMUM)`, filled with
  `append_repeat_continue`) or an integer handle into a registry (`macro`,
  into `command_macros`).  tools/test-hazards.sh tests the rule on the
  fixture in tools/hazards-test.
- `implicit-none`: `implicit none (type, external)` everywhere.
- `stop`: errors return a status; `stop` only in a main program.
- `obsolete`: no `common`, `equivalence` or `goto`.
- `continuation-limit`: a statement of more than 255 continuation lines
  (the standard's limit; gfortran only warns): a long array constructor
  is split into parameter arrays `NAME_part_1`, `NAME_part_2`, ... that
  the constructor joins, as the generators do (tools/gen-commands.sh,
  gen-docstrings.sh, gen-tty-colors.sh, gen-unicategory.sh).
- `truncated-literal`: a string longer than the length of its array
  constructor's type specification (`[character(len=N) :: ...]`), which
  gfortran truncates without a word (terminal_xterm_mod's `kp-separator`
  read as `kp-separato`, fortmacs_mod's `--no-blinking-cursor` as
  `--no-blinking-cu`): size the length from the longest string, as the
  generators do.

Checked by hand on every PR's diff (no script can tell):

- The same variable passed to two dummies when one is `intent(out)` (it
  is deallocated on entry).
- An array constructor with no elements assigned to an allocatable
  (`realloc()` with size 0, reported by valgrind): allocate the empty
  array explicitly.
- A guard the checker misses: an index passed to a procedure that
  subscripts with it, a substring or element in the guarded part of a
  nested expression.
- A dummy argument that hides a module entity of the same name in another
  case (`beginning` and `BEGINNING`).
- A type-bound procedure called on a registry entry
  (`fortmacs_diff_sessions(session_index)%quit(.false., status)`) whose body
  can append to that registry, directly or through a hook: SELF then
  points into the freed array.  Find the entry again on entry and work
  through the registry (see "Type-bound procedures on registry objects").
- A component of a registry passed as an `intent(in)` argument to a
  procedure that changes that registry (`call close_buffer(windows(w%index)%buffer,
  status)`: closing the buffer gives the window another buffer, so the
  argument changes under the procedure): pass a local copy.
- Objects with explicit lifetimes (markers, overlays, timers, processes)
  freed on every path, and handles checked for staleness.
- A procedure with a side effect is a subroutine, never a function (R3-8):
  coding.c's ONE_MORE_BYTE is `call take_byte(b, ended)`, not a logical
  function that also advances; a function that defines or registers
  something returns through an `intent(out)` argument instead.
- `intent` on every dummy argument; `recursive` on procedures that can be
  reentered (hooks, timers, the command loop).
- No allocatable component at all in `command_type` or other types
  instantiated hundreds of times in generated files (compile time at
  -O3): fixed lengths, fixed arrays or an integer handle into a registry
  instead (tools/hazards.awk checks `command_type`).
- No type-bound defined assignment on a type that is a component of a
  type built with a structure constructor in a submodule of another
  module: gfortran 13 then fails to link that submodule (undefined
  `_F.DAn`, the component's default initializer).  `choice_type`'s `=`
  is a module generic `assignment(=)` of value_mod for that reason
  (`command_type%repeat_check_key`; spike of 2026-11-04); a setting type
  is never such a component, so its `=` stays type-bound.
- Lisp's `round` and `fround` round a value halfway between two integers
  to the even one (floatfns.c's emacs_rint, rounddiv_q); Fortran's `nint`
  and `anint` round it away from zero.  Port them as `ieee_rint` from
  `ieee_arithmetic` (terminal_colors_mod's 16-bit color scaling does).
- `findloc` or `pack` on a character component section (`findloc(list%name,
  name, dim=1)`, `pack(list%name, keep)`) copies the section into an
  array temporary on every call ("Fortran runtime warning: An array
  temporary was created" under `-fcheck=all`); integer sections and
  elemental comparisons do not.  Search with the comparison,
  `findloc(list%name == name, .true., dim=1)`, and gather selected
  strings with a loop (grep_mod's `filter_list_by_directory`).
- A test that signals a child assumes the signal's default action in the
  child, but a signal the test runner ignores (SIGHUP under `nohup`)
  stays ignored through fork and exec: set it to the default around the
  start of the child (system_dependent_mod's `default_signal`, then
  `restore_signal`), as test_process_mod's `process-kill-buffer` does.
  It failed after 100 s with "finished" (its `sleep 100` ran out) only in
  `nohup` runs, which looked like a flake under load.

A newly learned hazard goes into `tools/hazards.awk` when a script can find
it, otherwise into this checklist.

## Conventions learned while porting

### Choice settings (R6-37, R6-38, R6-39, R6-46)

A user-settable setting with more than two states is a
`choice_setting_type` (data_mod), not an integer with `_NO`/`_YES`
constants (R6-38); internal enumerations that are not settings
(arguments, record fields, face attribute states) keep `_NO`/`_YES`
(`FACE_NO`/`FACE_YES`, `CASE_FOLD_*`; panel 2026-11-04).  Its `=` takes `.true.` and `.false.` for its on and
off states and named constants for its other states only; those start
with the option's full name, or a stem that is the common start of the
names of the options that share it (`DABBREV_*`, `ANSI_COLOR_FILTER`;
otherwise a family per option, `SGML_QUICK_KEYS_*` and `HTML_QUICK_KEYS_*`:
owner, #261, 2026-11-04), and take the values 2, 3, ...
(a named constant's number is never 0, the internal mark of the plain on
and off states).  The object has two state words, as value_mod's
nameless `choice_type` (the type of a command property such as
`repeat_check_key`): `%value`, .true. in the on state and .false.
otherwise, and `%choice`, the named constant of the state.  Code reads
on and off as `x%value`, a named state as `x%choice == X_CONSTANT` and
anything but off (upstream's "non-nil") as `.not. x%is_off()`;
`choice_state(x)` copies the state into a `choice_type` to keep or pass
on, and `x = state` puts it back (saving `x%choice` alone and
assigning it back loses on and off: a plain `.true.` comes back as
`.false.`):

```fortran
!> Values of `compilation_scroll_output' besides .true. and .false.
integer, parameter, public :: COMPILATION_SCROLL_OUTPUT_FIRST_ERROR = 2
type(choice_setting_type), public :: compilation_scroll_output = &
    choice_setting_type(name="compilation_scroll_output")
! init.f90
compilation_scroll_output = .true.
compilation_scroll_output = COMPILATION_SCROLL_OUTPUT_FIRST_ERROR
! Fortran code
scroll = .not. compilation_scroll_output%is_off()
```

- `.true.` and `.false.` mean what the option's name plainly says to
  someone who has never used Emacs, not upstream's t and nil (R6-46).
  When upstream's t carries a special meaning, that state gets a named
  constant and `.true.` is the plain yes.  Docstrings describe the
  Fortran states ("If .true., ..."), never "non-nil".  An option whose
  values would still read obscurely is renamed so that they don't; each
  such rename is an owner decision, listed as an open question with
  before and after, never invented.

  ```fortran
  directory_editor_recursive_deletes = .true.               ! delete recursively without asking (upstream always)
  directory_editor_recursive_deletes = DIRECTORY_EDITOR_RECURSIVE_ASK  ! ask for each directory (upstream t)
  directory_editor_recursive_deletes = DIRECTORY_EDITOR_RECURSIVE_TOP  ! ask for each top directory (upstream top)
  directory_editor_recursive_deletes = .false.              ! delete no non-empty directory (upstream nil)
  ```

  The owner's follow-ups of 2026-11-04: `dynamic_abbreviation_case_distinction =
  .true.` makes case differences count (upstream nil), `.false.` treats
  such expansions as the same (upstream t); compile.el's
  compilation-context-lines is `compilation_context` (`%lines = N`,
  COMPILATION_CONTEXT_TOP for upstream nil, COMPILATION_CONTEXT_NO_SCROLL
  for upstream t); `directory_editor_switches_in_mode_line` takes
  DIRECTORY_EDITOR_SWITCHES_IN_MODE_LINE_SORT_ORDER, _AS_IS, `%characters = N` and
  `%procedure => f`; the sh_indent_* options take `%columns = N` and
  SHELL_PLUS … SHELL_SLASH; disabled-command-function is one choice setting,
  keyboard_mod's `disabled_command_function_variable` (`= .false.` lets
  every command run, `%procedure => handler`; initialize_novice sets novice_mod's
  `disabled_command_function`).

  A setting with no on or off state upstream (`show_parenthesis_style`,
  `default_justification`, `compilation_context`,
  `directory_editor_switches_in_mode_line`, the sh_indent_* options) is a choice
  setting too, whose `=` takes its constants only; a state upstream's doc gives to "any other non-nil
  value" is `.true.` (`directory_editor_movement_style = .true.` is
  `DIRECTORY_EDITOR_MOVEMENT_STYLE_BOUNDED`).
- A setting whose upstream value can also be a function is that same
  object (R6-37): an extension of `choice_setting_type` adds
  `procedure(X_procedure), pointer, nopass :: procedure` and overrides
  `clear_kinds`, which every `=` calls, so `X = .true.` or
  `X = CONSTANT` clears a procedure set before, and `X%procedure =>
  mine` sets one (`grep_save_buffers`, `directory_editor_do_what_i_mean_target`,
  `electric_pair_skip_self`; `buffer_menu_name_width` is the same on an
  `integer_setting_type`: `= 19` or `%procedure => mine`).  Named
  upstream functions become named constants
  (`ELECTRIC_PAIR_SKIP_SELF_HELP_BALANCE`, `DIRECTORY_EDITOR_DO_WHAT_I_MEAN_TARGET_NEXT`).
  An option that is only ever a function stays a plain procedure
  pointer.  The old sibling (`grep_save_buffers_function`,
  `buffer_menu_name_width_function`) became the component: components,
  not prefixed sibling variables.
- A value that can also be a number goes in a named component of the
  object, named for what the number is, and setting that component
  selects that kind (R6-39: `message_log_maximum%lines = 500`,
  `bookmark_save_flag%modifications = 5`, `recent_files_auto_cleanup%seconds
  = 300`, `font_lock_maximum_decoration%level = 1`,
  `keyboard_macro_execute_repeat_key%char = iachar("e")`).  The design, in
  standard Fortran with plain component assignment: the component is
  negative while no number is set (so a declaration can give a default,
  `lines=1000`), every `=` resets it along with the procedure
  (`clear_kinds`), `is_off()` is false while a number or a procedure is
  set, and the code reading the setting takes the procedure first, then
  a number that is set, then `%value` and `%choice`.  A string value
  (R6-39 too) is an allocatable character component, unallocated while
  unset (`mail_signature_variable%text = "..."`, beside
  `mail_signature_variable%procedure`; recentf's time string stays
  Deferred).
- Each choice setting declares the values it accepts (panel
  2026-11-04; precedents A8, the wrong-type error, and Q12, set_variable
  rejecting non-constants), as data on the one `choice_setting_type`:
  `choices`, its named constants, which docstrings_mod's generated
  `initialize_choice_values` fills from the "Values of `x'" parameter statement
  naming it (so a family shared by several settings is split into one
  statement per setting where they accept different constants, as the
  DABBREV_* ones are), and `true_valid`/`false_valid`, set to `.false.`
  in the declaration when `.true.` or `.false.` is not a state
  (`show_parenthesis_style`, `default_justification`,
  `split_window_preferred_direction`, `compilation_context`,
  `directory_editor_switches_in_mode_line` and the sh_indent_* options take
  neither; `directory_editor_free_space`, `directory_editor_mark_region`,
  `backward_delete_character_untabify_method` and
  `disabled_command_function_variable` take no `.true.`).  Any other `=`
  leaves the state as it was and records an error in data_mod's
  `setting_errors`: loading the init file reports each one like an
  error of `configure` ("An error occurred while loading the init file:
  shell_indent_for_fi: 0 is not a value of this option; a number of columns
  is `shell_indent_for_fi%columns = 0'", `initialization_file_had_error`), and
  `set_variable` and a buffer-local value set through its variable return
  it as a `wrong-type-argument` error.  The right assignment comes from the
  extension's `kind_hint` binding (the number or procedure it takes).
  The settings the generator does not see (not in the config API) accept
  any constant, as before.
- The setting objects with other kinds override `other_value`, the
  text C-h v shows for them ("a procedure set by the configuration",
  "%lines = 500"); `clear_kinds` and `other_value` are bindings of the
  base `setting_type`, so an extension of any setting type can add a
  kind.
- C-h v shows `.true.`, `.false.`, the constant's name, or the other
  kind; M-x set-variable takes `.true.`, `.false.` or a constant
  (tools/gen-docstrings.sh writes both from the declarations; it finds a
  choice setting's constants through the "Values of `x'" comment above
  their parameter statement, or the constants that start with the
  setting's name).  Setting a number or a procedure from set-variable is
  Deferred.
- A choice setting forwarded to its variable (`forward_setting`) has
  buffer-local values like the integer settings (nil, t, or the
  constant's integer as the Lisp-side value; `require_final_newline`
  keeps upstream's names `visit`, `visit_save` and `ask`); a value of
  another kind is buffer-local when the extension overrides
  `get_kind_value` and `set_kind_value` (the sh_indent_* settings: an
  integer for `%columns`, a name for a named constant), else one global
  value.
- A record field with several states stays an integer with named
  constants, since a structure constructor has no defined assignment
  (`FONT_LOCK_OVERRIDE_NONE`, `_REPLACE`, `_KEEP`, `_PREPEND`, `_APPEND`
  for upstream's nil, t, keep, prepend, append).
- tools/intrinsics.awk's `yes-no` rule rejects a new named constant
  ending in `_YES` or `_NO`; its baseline lists the internal
  enumerations (arguments, return values, record fields, face attribute
  states) and the settings still waiting for a decision.

- An array grows by assignment, `list = [list, item]` or `list = [item,
  list]` (gfortran reallocates the left side), not with a temporary and
  `move_alloc`: `item` is a variable when its type has allocatable
  components (the `allocatable-constructor` hazard), and `list` is
  allocated first (an unallocated array can't appear in the constructor).
  `move_alloc` stays for the capacity-doubling buffers and for hot loops
  (simple_minded_indentation_engine_mod's cycle search and config-guess traces, compile_mod's line
  nodes).  A module's per-buffer table is searched with
  `findloc(locals%serial, buffers(current)%serial, dim=1)` (with
  `mask=locals%set` where entries carry a flag) and reuses the entries of
  closed buffers with buffer_mod's `buffer_serial_live`; the lookup is a
  function (`local_index()`), finding or making the entry a subroutine
  (`call make_local_entry(k)`, R3-8).  The PR `simplify-a` (2026-11-02)
  applied both.  A search for an element or a component of intrinsic type
  (a fixed-length string included) is `findloc`, `any`, `count` or `where`,
  not a loop; a deferred-length component can't be searched that way and
  keeps its loop.  An integer's decimal text, and any text built from a
  format fixed in the source, is an internal write with edit descriptors
  (`write (text, "(i0)") n`) where it is used ("Intrinsics first",
  "Numbers to text"), never `number_to_string` (data_mod's generic one,
  which `simplify-a2` had made the one copy, is gone since R6-44),
  `format` or a private wrapper (R6-44, R6-45); a string's character
  count utility_functions_mod's `count_characters`, the ASCII lowercase of a string
  character_mod's
  `downcase_ascii` (R6-16): no module keeps its own copy (`simplify-a2`,
  2026-11-03).
- A list of strings of different lengths is an array of string_type_mod's
  `string_type` (its component is `text`); no module declares its own
  one-component wrapper (`process_arg_type`, `vc_string_type`,
  `sys_string_type` and 27 more became it, `simplify-b`, 2026-11-03), and
  no module keeps its own copy of `append_string`/`prepend_string`.  A
  list is built with `call append_string(list, "a", b, "c")` on an
  unallocated array, not `allocate (list(3))` and one assignment per
  element; a list of fixed-length strings is one array constructor
  (`properties = [character(len=32) :: "a", "b"]`).  A wrapper whose one
  component names something more than text stays (recent_files_mod's
  `recent_files_file_type` and coding_mod's `coding_system_list_entry_type`,
  both `%name` and in the config API), and so do types with more
  components.
- Newer valgrind (Fedora's) reports `realloc() with size 0`, which
  gfortran calls when an array constructor with no elements is assigned
  to an allocatable (`list = [a(1:0), b(1:0)]`): allocate the empty array
  explicitly instead (autorevert_mod's `auto_revert_buffer_candidates`).
- GitHub runs no CI for a pull request that conflicts with its base (a
  status file changed on both sides is enough).  Merge the
  current main into a branch right before opening its PR and whenever main
  moves, and check that the run started.
- When a .el and a .c file share a base name, the .c file keeps the plain
  name and the .el module gets a descriptive one, noted in its status file:
  lisp.el is `expression_motion_mod` (lisp.h is `value_mod`), dired.el `directory_editor_mode_mod`
  (dired.c is `directory_editor_mod`).
- `syntax-propertize` runs mode code in the middle of a scan or a regexp
  match (as upstream's UPDATE_SYNTAX_TABLE does), so the scanners, the
  matcher and the search commands are `recursive`, and a compiled pattern
  in use is marked busy in the cache (search.c's freeze_pattern).
- fpm builds only the modules some program or test uses: a new module that
  nothing uses yet is not compiled at all (no errors either), so register
  its `init_` in bindings_mod (or use it) before relying on the build.
- fprettify (0.3.7) crashes splitting a continued line whose string holds
  `&!`; such regexps take the characters from a named constant built with
  `achar(38)` (f90_mod's NOT_CONT).
- A procedure pointer component whose interface is a function with an
  allocatable or deferred-length result is freed by gfortran when the
  derived type is assigned (invalid free of the procedure's address): make
  the interface a subroutine with an `intent(out)` argument
  (`font_lock_face_procedure`, display_engine_mod's `lighter_text_procedure`, minibuffer_reading_tables_mod's
  `completion_unquote_procedure` and `completion_quote_procedure`).
- Fortran names are case-insensitive: a dummy argument named `beginning` hides
  the constant `BEGINNING` in its procedure (a null-pointer read at run time),
  so arguments mirroring upstream's BEG and END are `beginning_byte` and
  `end_byte`.
- A function reference can't be the left side of a component assignment
  (`f(x)%c = v` is invalid, and an `associate` name for a pointer function
  result is not definable), so compile-time objects are named variables.
- `.and.` and `.or.` do not short-circuit: guard a substring or element
  with a nested `if` (or `index(s, "x") == 1`), never `len(s) > 0 .and.
  s(1:1) == ...`; newer gfortran on Fedora and Arch evaluates both sides.
- After an interface or derived-type change fpm may keep stale test objects
  or an old tester binary (bogus segfaults, "Mismatch in components"):
  delete build/*/fortmacs/test_* and build/*/test/tester.
- Bytes are default-kind `character` elements: build them with `char(i)` and
  read them with `ichar(c, kind=int32)`. Not `achar`, whose result outside
  0..127 is processor-dependent (gfortran warns).
- Upstream's unchecked fast paths (`check = false`, no end pointer) are not
  ported; everything bound-checks against `len(string)`.
- Functions that must return two values become `pure subroutine`s (a pure
  function may not have `intent(out)` arguments).
- fpm tree-shakes the library: a module is only compiled once something
  (a test or the app) uses it, so register a module's test suite at once.
- Procedures in a submodule can't call *private* parent procedures that
  the parent itself never uses: gfortran drops them (link error). Put such
  helpers in the submodule.
- Tests of the src/sys layer may use ISO_C_BINDING and system_linux_mod
  directly; nothing else may. Terminal tests run on a pty (`open_pty`).
- If fpm reports "Project is up to date" after a source change, the build
  cache is stale: delete `build/gfortran_*`. CI always builds clean.
- Never pass the same variable to two dummies when one is `intent(out)`
  (e.g. `call split(iv, k, len, k)`); use a temporary.
- Never put a function result or a structure constructor inside an array
  constructor of a type with allocatable components (`[f(x)]`,
  `[t(name="a")]`): gfortran 13 leaks them (checked with valgrind,
  2026-10-28). Use `list(...)`, variables, element-by-element assignment,
  a typed `%append`, or fixed-length components. No recursive allocatable types (gfortran copies them
  shallowly). `tools/check.sh --valgrind` (run in CI) catches leaks: it runs
  each suite alone under valgrind, in parallel; `--valgrind-combined` (CI
  on main) also runs all suites in one process.
- Objects are freed explicitly: every `make_marker` needs a `destroy_marker`.
- Change hooks run user code in the middle of editing primitives, which
  may call them again: the primitives and every procedure on the way to a
  hook (insert, delete, replace, text property changes, case changes) are
  `recursive` (`-fcheck=all` stops on a recursive call to one that is
  not), and none holds an `associate` on `buffers(current)` across a hook
  (the registry may grow).
- Registry slots are reused after kill; any cache keyed by slot must also
  check `buffers(i)%serial` (see the regression test in test_insert_delete_mod).
- Text uses Emacs's internal encoding (UTF-8 extended to 22-bit codes, raw
  bytes as C0/C1 two-byte forms), so files with invalid UTF-8 round-trip.

- Docstrings: the `!>` comment on a command (its `execute_*` procedure, or
  else the procedure citing it), a config-API procedure or a setting starts
  with the upstream docstring, adapted to Fortran names and to what is
  ported, then "Port of FILE:NAME" (a procedure) or "Port of FILE NAME"
  (a variable) and any notes on the port. Help shows the text before
  "Port of". Write keys as upstream does: `\[command]`, `\<map>`,
  `` \`KEY' ``, `` `quote' ``. After changing one, run
  `tools/build.sh` (it regenerates src/docstrings_mod.f90, which is not
  committed, before fpm builds).

- User options: a variable that upstream defines with `defcustom` (or
  lists in lisp/cus-start.el) goes in the pull request's own file of
  `tools/user-options.d` (`DATE-BRANCH`, "Status"), so C-h v
  says "You can customize this variable.", `set_variable` offers it and
  `apropos_user_option` lists it; gen-docstrings.sh generates its
  setter (a variable listed there must be documented).

- Buffer-local procedure settings (panel 14): one whose procedures take
  no arguments but a status is a `function_setting_type` given to
  `forward_setting`, swapped with the buffer like other settings
  (`comment_insert_comment_function`, `normal_auto_fill_function`).
  Another interface gets a setting object of its own, an extension of
  buffer_local_setting_mod's abstract `buffer_local_setting_type` (Fortran
  has no procedure pointer component of a generic interface, so one
  extension per interface, as the `*_choice_setting_type` extensions are
  one per kind).  The parent keeps the serial numbers of the buffers with
  their own value and finds, makes (reusing a closed buffer's entry) and
  clears entries; the extension holds the global value in `procedure`
  (R6-37) and the buffers' own values in `locals`, and binds `set_local`,
  `get_current` and the parent's two deferred hooks, `append_local_entry`
  and `clear_local_entry`:

  ```fortran
  blink_matching_check_function%procedure => mine     ! init.f90: the global value
  call blink_matching_check_function%set_local(check)  ! this buffer's own (absent: null)
  check_now => blink_matching_check_function%get_current()
  if (blink_matching_check_function%is_local()) ...
  call blink_matching_check_function%clear_local()     ! back to the global value
  ```

  A buffer's own value wins even when null (upstream's local nil).  The
  module's `clear_X_locals`, registered with `append_locals_clearer`, calls
  `clear_local` for kill-all-local-variables (not for a permanently local
  one, `revert_buffer_function`).  `get_current` is a subroutine,
  `call x%get_current(f)`, for `incremental_search_search_function_function`
  (gfortran rejects a function returning a pointer to a procedure that
  returns a procedure pointer) and the pcomplete function variables
  (whose callers took it that way), and for an array of a type with
  allocatable components (`tabulated_list_format`, `tabulated_list_entries`:
  the `allocatable-result-inquiry` hazard).  A value of another type is the same
  parent with a component named for it: `electric_indent_characters`
  (`characters`, set with `=`), `skeleton_pairs` and
  `skeleton_default_pairs` (`pairs`, grown with `%append`).  The free
  procedures `X_set_local`, `current_X` and `X_clear_local` this replaced
  are gone.  The per-buffer variables once kept in a module's private
  record behind free setters follow it too: tabulated-list.el's six
  (`tabulated_list_entries%set_local` takes an array or a procedure,
  R6-37; the permanent-local ones are not cleared by `clear_X_locals`),
  comint.el's procedure variables and `command_interpreter_completion_fignore`,
  `local_abbreviation_table` and `syntax_parser_state_table`; one of a
  forwarded kind becomes a forwarded setting (`view_read_only`,
  `call view_read_only%set_local(.true.)`).  What only the module reads
  stays private: comint's markers and input ring in its record,
  tabulated-list--original-order in the entry of `tabulated_list_entries`.

- Shell-dependent settings (sh-script.el's tables read by `sh-feature`)
  are typed feature tables, one per kind of value (`shell_strings_feature_type`,
  `shell_keywords_feature_type`, `shell_string_feature_type`): `add` gives a
  shell its entry (with the shell whose value follows, for sh-append),
  `get` looks the current shell up through `shell_ancestors`, finally
  sh.  An indentation variable holding a number or one of the symbols
  + - ++ -- * / is a choice setting (`shell_indent_choice_setting_type`):
  a number of columns in `%columns` (`shell_indent_for_fi%columns = 0`;
  negative numbers indent left, so "no number" is the most negative
  integer) or the shared named constants SHELL_PLUS … SHELL_SLASH by plain
  assignment (`shell_indent_after_if = SHELL_PLUS`); `set_variable` reads the
  constant names.  Its buffer-local values keep upstream's values (an
  integer, or the name + - ++ -- * /) through the `get_kind_value` and
  `set_kind_value` bindings of `choice_setting_type`, which data_buffer_mod
  calls for a forwarded choice setting.  A style (`shell_style_type`) holds
  `shell_indent_type(columns=N)` or `shell_indent_type(choice=SHELL_PLUS)` per
  variable.

- Keymaps are registry objects too. Prefix maps made by `define_key` and
  sub-keymap copies are owned by the map they were made for, so one
  `destroy_keymap` frees the whole tree.

- A `bind(C)` module variable is a definition, not a reference: binding
  libc's `environ` gave the program its own null `environ`, so child
  processes got only the entries we passed.  system_dependent_mod keeps its own
  copy of the environment instead (read from /proc/self/environ, kept
  current by `system_setenv` and `system_unsetenv`), as upstream keeps
  `process-environment`.

- gfortran 13 aborts (`free(): invalid pointer`) when it deallocates an
  array (`move_alloc` included) whose type has an associated procedure
  pointer component with a function interface returning
  `character(len=:), allocatable`: it frees the procedure as if it were
  an allocatable result.  Subroutine pointers are safe.  `version_control_backends`,
  which has such components, is a fixed-size array with a count.

- `close_buffer` is recursive: a function of `close_buffer_hook` may kill
  other buffers, as `info_close_buffer` kills the tag table buffer.
- An `intent(out)` allocatable is freed on entry, so passing the same
  variable as input and result (`call expand_file_name(f, f, st)`) reads
  freed memory.  Copy the input first.
- gfortran (13) rejects a function returning a pointer to a function
  that itself returns a procedure pointer (`fn => f()`: "is not a
  subroutine").  Such a variable holds a subroutine that sets its
  argument instead, as `incremental_search_search_function_function` does.
- gfortran does not emit a private module procedure that only a submodule
  calls, so such helpers are made public with a comment saying so (they are
  not part of the config API); ibuffer_mod does this for ibuffer_extensions_mod.

### Type-bound procedures on registry objects (panels 13, 14)

An object kept in an allocatable registry and referred to by its integer
index there (an Ediff session in `fortmacs_diff_sessions`, a session
group in `fortmacs_diff_session_groups`) has no handle wrapper: the index
is named for the thing, `session_index`, `group_index` (plain loop
counters stay `i`, `j`, `k`).  A helper that acts on one such object is a
type-bound procedure, called on the registry entry, verb first and
spelled out: `call fortmacs_diff_sessions(session_index)%select_difference(difference,
status)`, `fortmacs_diff_session_groups(group_index)%get_session_at(position)`
(ediff-select-difference, ediff-get-session-number-at-pos).  Commands (the
`execute_` procedures in `commands%`) keep their command names, and so do
the procedures that find or make a registry entry
(`make_fortmacs_diff_session_entry`, `fortmacs_diff_session_index`).

- A binding is declared in the type's module and its procedure lives in
  that module or one of its submodules.  When the procedures need modules
  that use the type's module, they go into submodules of it named for
  their upstream file: fortmacs_diff_settings_mod (ediff-init.el) holds
  `fortmacs_diff_session_type` and has fortmacs_diff_settings_diff_mod,
  `_merging_mod`, `_utilities_mod`, `_window_mod` and `_help_mod` for the
  session procedures of ediff-diff.el, ediff-merg.el, ediff-util.el,
  ediff-wind.el and ediff-help.el; those files' modules keep their
  settings, commands and other procedures.  A procedure that needs a
  private procedure of another submodule stays free and takes the index
  (`set_up_fortmacs_diff_keymap`, which binds the commands of
  fortmacs_diff_utilities_session_mod).
- The registry can grow, and move, while a bound procedure runs (a new
  session, a hook running user code), leaving SELF dangling: see the
  Hazards checklist.  A bound subroutine that can get there looks its
  entry up again on entry by a key that does not move, through the private
  binding `get_index` (`call self%get_index(session_index, status)`: the
  serial number, or the group's buffer), and works through the registry
  from then on.  A lookup that finds nothing (the control or group buffer
  was closed) is a `user_error`, as for a command run outside an Ediff
  buffer, never an index of 0.  A function, which has no side effects, and
  a subroutine that calls nothing that can run user code or make a session
  read SELF.  SELF has `intent(inout), target` in a subroutine,
  so changing the entry through the registry while SELF is associated
  with it is allowed.
- A binding whose name meets a name its submodule imports keeps the
  binding name and renames the procedure: `procedure :: recenter =>
  recenter_session` (window_mod's `recenter`).
- Bindings used outside the type's module and its submodules are public,
  each with its own `!>` line (FORD documents every public entity;
  tools/audit.awk does not look inside derived types); the others are
  `private`, documented by a comment over each group of them.

## Added features

Features that upstream Emacs 31.1 does not have, each an owner decision
(CLAUDE.md: behavior mirrors Emacs except for these).

- **Completion after `%` in f90 buffers** (owner, 2026-11-05): the minor
  mode `fortran_completion_mode` (fortran_completion_mod), off by
  default: M-x, or `call f90_mode_hook%prepend(turn_on_fortran_completion_mode)`
  in init.f90.  While it is on, completion-at-point (M-TAB, C-M-i,
  completion-preview, icomplete) offers, after a designator and `%`
  (`faces%`, `commands%upcase_region%`, `ediff_sessions(k)%`), the
  components and type-bound procedures of the designator's derived type
  and its parent types.  The types come from the current buffer, then
  the project's .f90 files (read only when the first name is not
  declared in the buffer), then the configuration API, in that order:
  fortran_declarations_mod parses the source (type definitions and the
  declarations that use them, with their scopes), each buffer and file
  is parsed again only when it changes, and fortran_completion_table_mod
  is the table of the configuration API's types that
  tools/gen-completion-table.sh writes at build time (from what
  src/fortmacs.f90 gathers; not committed).  The owner asked for the
  parsing on the worker thread; it is synchronous behind the caches for
  now (system_worker_mod's workers only read subprocess output), Deferred
  in docs/status/fortran_completion_mod.md with the rest of what is not
  done.  f90_mode without the mode is unchanged.

## Design decisions

- **Generated at build time (2026-11-04, decided by the owner).**  The
  four modules made from src/ alone, src/commands_mod.f90,
  src/modes_mod.f90, src/face_table_mod.f90 and src/docstrings_mod.f90
  (tools/gen-commands.sh, gen-modes.sh, gen-faces.sh, gen-docstrings.sh),
  are not committed (.gitignore), so they no longer show in diffs or
  conflict between PRs.  `tools/build.sh [FPM-ARGUMENTS]`
  runs the generators, only when an input is newer than
  build/generated.stamp (or a file is missing; `--force` always), then
  fpm with the arguments (`build` when there are none).  tools/check.sh
  generates first (`--fix` always), so CI and the Guix job build from
  generated files; check.sh no longer compares them with a committed copy.
  The generators also work without fprettify (the Guix job has none):
  their output then keeps the generator's indentation, within 132
  columns.  `fortmacs --recompile` needs nothing new: it compiles init.f90
  against the installed library and module files (PREFIX/lib,
  PREFIX/include), which `tools/build.sh install` builds from the
  generated sources.  The tables made from upstream data (the outputs of
  tools/gen-uni*.sh, gen-charsets.sh, gen-charscript.sh and
  gen-tty-colors.sh) stay committed: they need upstream/emacs,
  which a build does not have.

- **Module names (2026-11-01, decided by the owner).**  The module
  suffix is `_mod`, the most common Fortran module suffix (`_f90` would
  read as the Fortran 90 dialect): `buffer_mod` in buffer_mod.f90,
  tested by test_buffer_mod.f90; only the API module `fortmacs` has no
  suffix, and tools/audit.awk checks it.  Modules of Lisp-named upstream
  files are named for what they hold: lisp.h's `value_mod`, eval.c's
  `hooks_mod`, lread.c's `names_mod` (print.c's `print_mod` already
  read so).  A module's one-time startup setup is `initialize_X`,
  upstream's syms_of_X; where upstream has an init_X of its own (another
  startup step), it is `initialize_X_commands`, so a citation of
  upstream's init_X never names a different Fortmacs procedure.
  Module names are spelled out (2026-11-05, decided by the owner):
  they follow the no-abbreviations rule even where upstream's file
  name is abbreviated (cal-move.el → `calendar_movement_mod`, xdisp.c →
  `display_engine_mod`, fns.c → `utility_functions_mod`), and the file
  header keeps citing the upstream file.  tools/module-renames lists the
  renames made then; tools/rename-modules.sh applies them (file names,
  `use` lines, test suites, status files), and a branch begun before
  them runs it after merging main, then tools/check.sh --fix, and
  commits.

- **Fortran names (2026-10-23).** Fortmacs follows Emacs except where being
  Fortran requires otherwise: names use underscores (`find_file`) and
  Fortran words (see the next two entries), there is
  no run-time Lisp (`M-:`, `eval-buffer` and `ielm` are not applicable,
  `*scratch*` is in Fundamental mode), Help shows Fortran values and Fortran
  source files, and saved settings and packages are Fortran compiled in.

- **Fortran-first names for Lisp terms (2026-10-31, decided by the owner).**
  Fortmacs leans Fortran-first, not Lisp-first: Lisp-derived terms get
  Fortran names (subprogram, expression, group, a plural name for an
  alist, and the module expression_motion_mod), each term confirmed by
  the owner, by an existing rule (decided and reported), or by the
  decision panel (CLAUDE.md).
  So that the names never drift and new upstream versions port without
  bringing the old ones back, the renames are data (tools/fortran-names.d)
  applied by a script (tools/fortran-names.sh) and enforced by
  tools/check.sh; citations keep upstream names.  See "Fortran names for
  Lisp terms".

- **One word per meaning (2026-10-31, decided by the owner).**  execute is
  the only verb for running something (commands, keyboard macros, hooks,
  timers, processes), user-visible names included (`execute_with_timer`,
  `commands%paste%execute`); procedure pointers are called directly; and
  other synonyms used for one meaning are merged (fetch into get, new
  into make, release and free into destroy, push and add into
  append/prepend, ...), while
  Emacs words whose behavior differs stay distinct.  The renames are
  per-name lines of tools/fortran-names.d and tools/intrinsics.awk rejects
  new run_ and call_ procedures.  See "One word per meaning".

- **Values stay a flat array of tagged nodes (2026-10-31, decided by the
  owner).**  A dynamically typed value (`value_type`) is tested with
  `select case` on its tag, not with `select type` on a `class(*)` or a
  polymorphic tree.  A spike on gfortran 13.3, the minimum Fortmacs
  supports, showed that recursive allocatable components are still
  copied shallowly by intrinsic assignment (changing a copy changed the
  original), followed by invalid reads and a segmentation fault under
  valgrind; gfortran 13 also rejects a `class(*)` component in a
  structure constructor ("Cannot convert CHARACTER(3) to CLASS(*)").
  Fortmacs targets gfortran only, so values stay a flat array of tagged
  nodes read with the language constructs (C2b).

- **Ibuffer filters as nested function calls (2026-10-29, chosen with the user).**
  A filter specification (`(and (derived-mode . text-mode) (not (starred-name)))`)
  is written as nested Fortran function calls returning an
  `ibuffer_filter_type`: `ibuffer_and(ibuffer_derived_mode("text_mode"),
  ibuffer_not(ibuffer_starred_name()))`; saved filters and filter groups
  are added with `%append`. The tree is kept flattened inside the type (no
  recursive allocatable types), and no constructor goes in an array
  constructor. Ported with the filters (step 36c): the mode filter is
  `ibuffer_major_mode` (`ibuffer_mode` is the major mode), and a test
  filter takes a Fortran function (`ibuffer_test(my_function)`).
  It shows as " [test]" (it has no printable form), and `/ e` is not
  bound (it reads a Lisp form). The sorting mode `filename/process` keeps
  its slash as data (`ibuffer_default_sorting_mode = "filename/process"`);
  its command is `ibuffer_do_sort_by_filename_process`. `/ SPC`
  (`ibuffer_filter_chosen_by_completion`) reproduces upstream's error after
  filtering ("Wrong type argument: sequencep, 47": upstream gives
  `key-description` an event instead of a key sequence).

- **Name clashes (2026-10-29, chosen with the user).** When two upstream
  names become the same Fortran name, the plain Emacs name keeps its name
  unchanged and the other gets a spelled-out suffix saying what it is:
  - a function and a variable with the same name: the variable gets
    `_variable` (`user-full-name` → `user_full_name_variable`);
  - names that differ only in case (Fortran names are case-insensitive):
    the capitalized one, a mode's command, gets `_command`
    (`Buffer-menu-other-window` → `buffer_menu_other_window_command`, the
    Buffer Menu's `o`).

  The renamed entity's docstring cites the upstream name and says why it
  was renamed, and the table below lists every rename. Aliases
  (`LaTeX-mode` of `latex-mode`) collapse to one name. A capitalized name
  without a twin is only lowercased (`Buffer-menu-mark` →
  `buffer_menu_mark`). Upstream C variables that are not Lisp variables
  keep their `V` prefix lowercased (editing_functions_mod's `vuser_login_name`).

  | Upstream | Fortmacs |
  |---|---|
  | variable `user-full-name` (editfns.c) | `user_full_name_variable` |
  | variable `deactivate-mark` (keyboard.c) | `deactivate_mark_variable` |
  | variable `truncate-string-ellipsis` (mule-util.el) | `truncate_string_ellipsis_variable` |
  | variable `grep-find-ignored-files` (grep.el) | `grep_find_ignored_files_variable` |
  | variable `sgml-tag-help` (sgml-mode.el) | `sgml_tag_help_variable` |
  | `Buffer-menu-other-window` (buff-menu.el) | `buffer_menu_other_window_command` |

- **Buffer-local variables (2026-10-23).** As upstream's swapped-in
  bindings: a variable's value (its symbol slot, or the Fortran variable it
  is forwarded to) is always the current buffer's. A buffer keeps its own
  values while it is not current and `set_buffer_internal` swaps them, so
  Fortran code reads settings such as `fill_column` directly and a config
  that assigns one sets the default. data_buffer_mod forwards symbols to
  Fortran settings by a table (upstream's DEFVAR_INT/BOOL/LISP and
  DEFVAR_PER_BUFFER); procedure-pointer settings keep their pointers in the
  swap, so an unnamed procedure survives it. BVAR slots (`major-mode`,
  `mode-name`, `buffer-read-only`) read buffer fields.

- **Idiomatic Fortran objects (2026-10-23).** Nothing in the config API
  looks like Lisp. Things that exist at compile time are named objects of
  derived types, changed by assignment to components and driven by
  type-bound procedures: commands are `commands%NAME` (`command_type`,
  generated src/commands_mod.f90), settings are typed objects
  (`integer_setting_type`, `logical_setting_type`, `string_setting_type`,
  `function_setting_type`; `fill_column = 72`, `call fill_column%set_local(60)`,
  code reads `fill_column%value`), hooks are `hook_type` objects holding
  procedure pointers (`call hook%append(process, depth=90, local=.true.)`;
  hooks whose functions take arguments have a type per argument list,
  such as `after_change_hook_type` for `after_change_functions`, all
  extending `hook_base_type`, which keeps the functions, buffer-local
  values and depths once), and
  modes are `major_mode_type` / `minor_mode_type` objects (internally the
  components of `major_modes` / `minor_modes`, generated src/modes_mod.f90);
  the configuration reaches a mode through objects named after its Emacs
  variables: `text_mode_hook`, `text_mode_map`, `text_mode_syntax_table`.
  Only names that exist
  solely at run time go through a specific subroutine with intrinsic
  arguments (`set_command_disabled(name, .false., status)`). `value_type`
  stays internal: the forwarding table in data_buffer_mod maps variable
  names to the objects, so name-based access (`set-variable`, C-h v, minor-mode maps and
  lighters) sees the same values. API procedures that would need a Lisp
  value (raw prefix arguments, key vectors, completion tables) are not
  exported; their commands are reached as `commands%NAME`.

- **Modes (2026-10-23).** A major mode is a `major_mode_type` whose
  command brackets its body with `%begin`/`%end` (the expansion of
  define-derived-mode done by hand; a parent of fundamental-mode is no
  parent, as in derived.el). A minor mode is a `minor_mode_type` with
  `%enable`, `%disable`, `%toggle` and `%command`; its own state is a
  `logical_setting_type` forwarded by the mode's name (buffer-local unless
  global), or another variable named by `variable` (upstream's
  :variable).

- **Docstrings (2026-10-22).** Help reads the docstrings from a generated
  module, src/docstrings_mod.f90, as upstream reads etc/DOC: the
  `!>` comments stay the single source, and `tools/gen-docstrings.sh`
  (awk) collects them into select-case tables, along with code that reads
  each setting so C-h v can show its value. Functions are the commands and
  the config API, named as their citations name them.

- **Pure Fortran (2026-10-20).** Besides libc system calls bound in
  src/sys, Fortmacs links no C libraries, except FreeType and HarfBuzz
  for the GUI's fonts. Where upstream uses one (ncurses
  for terminfo, and later things like GnuTLS, libxml2 or image
  libraries), the needed part is reimplemented in Fortran or the feature
  waits.

- **Threads (2026-10-20).** One main thread owns all editor state
  (buffers, markers, symbols, keymaps, windows), as in upstream. Background
  work (file I/O, search over large files, subprocess output, syntax
  analysis) runs on POSIX worker threads bound from src/sys (libc
  `pthread_*` via ISO_C_BINDING; glibc 2.34 and later has them in libc
  itself, so no `-pthread`/`-lpthread` is needed for linking, here or in
  `fortmacs --recompile`). Workers only see copied data and report back
  through a queue that the command loop polls, so core modules need no
  locks. Worker code must be reentrant (`recursive` procedures, no `save`d
  state). No OpenMP or coarrays. Implemented for subprocesses
  (2026-10-24): src/sys/system_worker_mod has one thread per child, reading
  its output and reaping it (a pidfd tells when it exits), and a
  mutex-protected queue with a self-pipe; keyboard_mod polls the self-pipe
  together with the terminal while waiting for input and calls
  `process_output_hook`, so filters and sentinels run on the main thread,
  as upstream runs them from wait_reading_process_output.

- **Redisplay and output devices (2026-10-24).** Upstream's redisplay
  is pixel-based and a terminal is the case where everything is one
  "pixel": `struct glyph` has `pixel_width` (1 on a tty, term.c
  `append_glyph`; a wide character is a glyph plus padding glyphs),
  `ascent` and `descent`; `struct glyph_row` has `x`, `y`, `height`,
  `ascent`, `pixel_width`, `visible_height` (tty: 1 high, ascent 0,
  `compute_line_metrics`); a frame has FRAME_COLUMN_WIDTH and
  FRAME_LINE_HEIGHT (1 on a tty) and windows have pixel edges; glyphs
  are produced by `PRODUCE_GLYPHS`, which calls the frame's
  `redisplay_interface` (`gui_produce_glyphs`, sizes from `face->font`)
  or term.c `produce_glyphs`; output goes through terminal hooks
  (`write_glyphs_hook`, `cursor_to_hook`, `clear_end_of_line_hook`, ...)
  on a tty, which updates frame matrices (`update_frame_1`), and through
  FRAME_RIF (`write_glyphs`, `clear_end_of_line`, `draw_glyph_string`,
  `draw_vertical_window_border`, `draw_window_cursor`, `flush_display`)
  on a window system, which updates window matrices (`update_window`).
  Survey of Fortmacs before this step: redisplay counted cells: glyphs
  had no width (a wide character was two cells, a TAB as many spaces),
  rows no height, display_engine_mod's columns doubled as glyph indices and as
  positions, `last_x` was the text width less one cell, the window rows
  went straight into the frame matrix (`put_rows`, the "|" border column
  of `window_text_columns`), the frame held its tty directly and
  update_frame called terminal_output_mod; one font, one line per row everywhere.
  Now: glyphs and rows carry upstream's pixel fields with their tty
  values (all existing tty output is unchanged, its tests untouched),
  frames have FRAME_COLUMN_WIDTH/FRAME_LINE_HEIGHT, a redisplay
  interface (`redisplay_interface_type`, null on a tty, so a tty frame
  still goes through terminal_output_mod as upstream's terminal hooks) and a font
  driver (`font_driver_type`, font_mod; faces get a font id when
  realized on a window-system frame).  display_engine_mod lays out a window-system
  frame in pixels (`display_row_graphical_user_interface`, the pixel half of `layout_window`,
  the graphic mode line, `get_physical_cursor_geometry`,
  `graphical_user_interface_draw_vertical_border`) and draws each window's rows through the
  interface (display_update_mod `update_window_line`); the terminal path keeps
  its frame matrices.  The device is exercised by a test-only fake GUI
  (test/fake_graphical_user_interface_mod.f90: an in-memory pixel grid, a monospaced or
  proportional default font and one twice its size), checked by
  test_fake_graphical_user_interface_mod: the same text as a terminal with a monospaced font,
  wrapping by pixel width, a taller row for a larger font, the graphic
  mode line, side-by-side windows with a drawn border, the cursor box.
  What a window-system frame can't do yet is Deferred in docs/status/display_engine_mod.md
  (recentering and vertical motion in pixels, the active minibuffer,
  face extension, the rest of realize_gui_face, ...); vertical motion
  (indent_mod) still counts cells.
- **GUI fonts (2026-10-24).** The future GUI renders fonts with FreeType
  (rasterizing) and shapes text with HarfBuzz, bound through
  ISO_C_BINDING and isolated in src/sys: the one exception to "no C
  libraries beyond libc system calls".  The window-system protocols
  (X11, later Wayland) stay pure Fortran over a Unix socket.  Nothing of
  this is built yet; the fake device's driver stands in for it.
- **Abbrevs (2026-10-24).** Abbrev tables are `abbreviation_table_type` objects
  named after their Emacs variables (`global_abbreviation_table`,
  `text_mode_abbreviation_table`, ...; a major mode's `define` takes its
  table), with the special properties as components and
  `call TABLE%define("teh", "the")` for define-abbrev.  `abbreviation_mode` is a
  pointer to the minor mode's own state, so a configuration writes
  `abbreviation_mode = .true.` (setq-default).  Saving abbrevs is a
  setting-saving command: `write_abbreviation_file` (and `save_abbreviations` through
  `save_some_buffers_functions`) writes the user abbrevs as `define` calls
  between the lines `! Abbrevs, written by write_abbreviation_file:` and
  `! End of abbrevs.` at the end of `configure` in the init file
  (`abbreviation_file_name`, by default the init file), replacing the block
  written before; the next `--recompile` defines them.  Reading abbrev
  files is not applicable, and `abbreviations_changed` is reset after the init
  file runs, as startup.el does after reading the abbrev file.  Abbrevs
  with a hook or an enable function are not written (a Fortran procedure
  has no name to write; the init file defines them in code).

- **Faces in the config API (2026-10-24).** Faces are reached through a
  generated `faces%` object, as commands are through `commands%`, because
  faces, like commands, are a namespace of their own in Emacs rather than
  variables: `faces%region%background = "blue"`,
  `faces%link%underline = .true.`, `faces%error%underline = "red"`.
  Attributes that are property lists upstream (`:underline` with `:color`,
  `:style` and `:position`; `:box`) are set in one assignment from a
  constructor, because Fortran cannot react to a single component being
  assigned (`%style` alone would leave the attribute unspecified):
  `faces%error%underline = face_underline(color="red", style=FACE_STYLE_WAVE)`,
  `faces%mode_line%box = face_box(line_width=2, color="gray")`. Styles keep
  upstream's names as faces_mod's constants `FACE_STYLE_LINE`,
  `FACE_STYLE_DOUBLE_LINE`, `FACE_STYLE_WAVE`, `FACE_STYLE_DOTS` and
  `FACE_STYLE_DASHES`. On a terminal, a wave is drawn as a curly underline
  where the terminal supports one and the other styles as a plain
  underline, as upstream does. A face whose variable has another name
  (`command_interpreter_highlight_input_face`, as a setting took the name) appears in
  `faces%` under the face's name. Implemented in roadmap step 25
  (tools/gen-faces.sh, src/face_table_mod.f90).

- **Time values (2026-11-02, R6-3).** Lisp's time values (an integer,
  (HIGH LOW USEC PSEC), (TICKS . HZ), a float) are one `time_type`:
  `seconds` (int64, whole seconds since the epoch) plus a fraction of a
  second `count`/`count_rate` (int64, 0 <= count < count_rate), the
  terms of the intrinsic `system_clock`.  Exact for any rate upstream
  produces (picoseconds, thirds of a second), with a range of about 292
  billion years; file and clock times use count_rate = 10**9.  Lisp's
  other shapes stay out of the config API.  Implemented in step 42e
  (time_functions_mod): the products of counts and rates are 128-bit integers
  (selected_int_kind(38)), a real converts to the least power of 2 that
  holds it, and a time zone (upstream's ZONE) is a POSIX TZ string.
- **Lists in the config API (2026-10-28).** What Elisp writes as lists
  is written as plain Fortran, never as Lisp data in Fortran (no cons
  cells, quoting, symbols or name/value pairs):
  - A list of plain values is an array constructor:
    `completion_styles = [character(len=24) :: "basic", "emacs22"]`.
    `add-to-list` is `x = [new, x]` after an `any(x == new)` check.
  - A table whose keys are known at compile time (frame parameters and
    the like) is a derived type with one typed component per key, set by
    assignment: `default_frame%left_fringe = 0`; a function value is a
    procedure pointer set with `=>`.
  - A list of records the user writes out in full is an array of a
    derived type with fixed-length text components, built with keyword
    structure constructors: `[server_type(name="work", port=993), ...]`.
    (Allocatable components leak in an array constructor with gfortran 13;
    see the conventions.)
  - A list that packages or the user add to one entry at a time gets a
    typed procedure: `call text_mode_hook%prepend(...)` (add-hook's
    default, the front) or `%append(...)` (add-hook with APPEND),
    `call prepend_auto_mode(regexp, mode)`.
  - Strings are never wrapped at a call site (R6-5): a procedure that
    takes several strings has optional arguments, as the intrinsics `max`
    and `min` do (`call file_name_concat(name, "/usr", "lib", "x")`); a
    stored list of strings of different lengths keeps its element type
    internal and is reached through procedures with plain literals
    (`call setenv("TERM", "dumb")`, `getenv`).  That element type is one
    shared `string_type` (fortran-lang stdlib's name) with an allocatable
    `text`, in string_type_mod; such a list grows through
    `append_string(list, text, ...)` (one to eight strings, as `max` takes
    several) and `prepend_string(list, text)`, never by assigning element
    after element (`call append_string(command, "sh", "-c", script)`).
  - Entries of one shape are an array of records (`skeleton_pairs`, each a
    `skeleton_pair_type(opening=, closing=)`); settings of differing types
    that a mode binds together are assigned in a procedure
    (`skeleton_further_elements`), never a table that would need a generic
    value type (R4-5).
  - Elisp's sequencing forms need nothing: statements run in order
    (`let*`), a `block` scopes locals, `let` computes from the old values
    into temporaries first, and a `let` that rebinds a global is written
    save, set, run, restore (errors return a status, so the restore runs).
- **Saved state in namelist syntax (2026-10-29).** The files Fortmacs
  keeps between sessions (savehist's history, saveplace's places, later
  recentf and bookmarks) are written in Fortran namelist syntax, not the
  Lisp data upstream writes: a Lisp file only makes sense to a Lisp
  reader, and sharing the files with Emacs is not a goal (they live in
  `~/.local/state/fortmacs/`, with Fortmacs's own names).  What must
  match upstream is the behavior: which values are saved, their limits,
  when files are read and written, where point goes.
  ```
  &savehist
   save_history_minibuffer_history_variables(1:2) = "file_name_history", "extended_command_history",
   file_name_history(1:2) = "~/src/my-notes.txt", "~/notes.txt",
  /
  &saveplace
   place(1)%file = "/home/v/notes.txt",
   place(1)%position = 1234,
  /
  ```
  - Arrays are written with their section (`name(1:n)`), so a
    one-element list reads back as a list; records are arrays of a
    derived type (`place(k)%component`), as in memory.
  - namelist_mod reads and writes the syntax itself: the `nml=` specifier
    needs every object declared at compile time with a fixed length and
    size (histories hold any number of strings of any length; savehist
    saves variables named at run time), and fixed-length strings lose
    trailing blanks.  A line break inside a character constant is kept.
  - Values are strings, integers and logicals (upstream's nil is `.false.`); others
    (symbols, nested lists) are not printable there and are not saved.

- **Header lines as typed segments (2026-10-29).** A buffer's
  `header_line_format` is a `header_line_format_type`: an array of
  `header_line_segment_type` records (TEXT, a FACE, the column ALIGN_TO
  it starts at), built one entry at a time with `%append` and emptied with
  `%clear`, rather than upstream's mode-line-format list of strings with
  text properties.  Chosen with the user over a propertized string (Lisp
  data, kept out of the config API) and a mode-line-format interpreter
  (Lisp lists in Fortran).
  ```fortran
  call header%append("", align_to=x)                  ! (space :align-to X)
  call header%append("Bookmark Name", "bold")         ! a label in a face
  buffers(current)%header_line_format = header
  ```
  - The %-constructs, `:eval` and the other mode-line-format elements
    are Deferred, as for the mode line, which is still built by fixed
    code.
  - A buffer's `mode_line_buffer_identification` (step 34a, for Info's
    "*info* (fortmacs) Node") is a `header_line_format_type` too: its
    segments replace the buffer name padded to 12 columns in the mode
    line, "%b" standing for the buffer name and "%%" for "%".

- **The manuals (2026-10-29, chosen with the user).** The Info manuals
  of Fortmacs (step 34d) are made from upstream's Texinfo sources by an
  awk converter in tools/, not by makeinfo (texi2any is Perl).  Where a
  manual describes a feature not ported yet, its section carries a
  "Deferred in Fortmacs" note.  The notes are generated, not written by
  hand: the converter reads the Deferred sections of the status files
  (docs/status, "Status") at build time, through a mapping from manual sections to
  modules, so they cannot go stale.  Manuals: the Emacs manual and the
  Info manual (info.texi) in 34d, the Emacs manual installed as the Info
  file `fortmacs` (R6-32: its output file, its directory entry, which
  says Fortmacs, and every node reference it writes are (fortmacs), and
  references from other converted manuals to (emacs) become (fortmacs));
  each doc/misc manual in the PR that
  ports its package; the Emacs Lisp reference and the Lisp introduction
  are not applicable (no run-time Lisp).  The converted text uses
  Fortmacs's underscore names (`M-x find_file`, `C-h v fill_column`), to
  match the commands.

- **Char-tables, case tables and categories (2026-10-30, step 42b).**
  A char-table (character_table_mod) is a registry object referenced by handle,
  with chartab.c's four-level trie; its sub char-tables live in a pool
  inside it (no recursive types) and go back to a free list when a
  value replaces them.  Values are integers, `NO_VALUE` standing for
  nil (default value, then parent); an extra slot holds an integer or a
  char-table handle (`character_table_extra_slot` / `character_table_extra_table`).
  The number of extra slots comes from the purpose ("case_table" 3,
  "category_table" 2) or an argument, not from a symbol property.  Case
  tables are char-tables of characters; the buffer keeps four handles
  (null for the standard tables), and before the standard table is
  first asked for, `downcase` and friends read the generated tables,
  which hold the same mappings, so they stay pure.  A category table is
  a char-table whose values number the distinct category sets of the
  table (upstream's hash table in extra slot 1), kept with the category
  docstrings in an info record that extra slot 0 points to.  Syntax
  tables keep their own representation (ASCII array and sorted runs).

- **Echo, this-command-keys and the top level (2026-10-30, step 42c).**
  The echo area is `last_message`; keyboard_mod keeps the kboard's echo
  state (echo string, prompt, immediate echo) and, as read_character does,
  wipes the echo area when it reads an event typed (not one from
  unread-command-events or a keyboard macro), adds the event to
  `this_command_keys` and echoes it.  So tests that simulate typing put
  events in the event queue (`store_event`, `keyboard_store_bytes`), not in
  unread-command-events.  `command_loop_level` counts the recursive
  edits `recursive_edit` (Frecursive_edit) enters, minibuffers being
  counted by `minibuf_level`, which moved to keyboard_mod.  A return to
  top level (upstream's throw to `top-level`) is the status
  `TOP_LEVEL_CODE`: not an error (no check of the status for `error` takes
  it), passed on by each recursive edit until the
  top-level command loop takes it without a message.  The top-level
  `command_loop` skips command_loop_1's initializations (callers run it
  again for more input, as one session); recursive edits do them.

- **Windows, bidi and the display core (2026-10-31, step 42d).**
  window.c's DEFUNs take the window as an optional handle (the selected
  window when absent) and a status for upstream's wrong-type errors;
  no window (upstream's nil) is the null handle.  A deleted window's handle stays a window
  (`is_window`) because handle_mod can tell a handle it gave out
  (`issued`) from one it never did.  With one frame, ALL-FRAMES
  arguments make no difference and are left out; MINIBUF is an
  integer (MINIBUF_ACTIVE, MINIBUF_YES, MINIBUF_NO).  These live
  in the submodule window_info_mod because they need frame_mod.  bidirectional_mod
  ports bidi.c whole over the buffer or a string, its state split as
  upstream's (`bidirectional_copy_it` copies the members before the stack and
  the stack up to its top), with the cache of states as module data and
  the bidi tables generated from the UCD by tools/gen-unibidi.sh; it is
  checked by bidi-find-overridden-directionality's upstream tests, while
  reordering in redisplay waits for display_line to use it.  The
  redisplay entry of display_engine_mod is now `redisplay_internal`, its upstream
  name, leaving `redisplay` to dispnew.c's DEFUN.  The cursor-off flag
  is per window, as upstream (`internal_show_cursor`).

- **Skeletons as typed data (2026-11-02, step 40d).**  Upstream writes a
  skeleton as Lisp data that skeleton.el interprets: strings, the symbols
  `\n _ - > @ & | resume: string v1 v2`, negative numbers, quoted and other
  Lisp expressions, and nested lists as sub-skeletons.  Fortmacs writes it
  as an array of `skeleton_element_type` records, built in an array
  constructor from constructor functions and named constants:
  ```fortran
  [skeleton_prompt("condition: "), SKELETON_INDENT, skeleton_text("if "), SKELETON_STRING, &
   skeleton_text("; then"), SKELETON_NEWLINE, SKELETON_INDENT, SKELETON_POINT, SKELETON_NEWLINE, &
   skeleton_prompt("other condition, %s: "), SKELETON_INDENT, skeleton_text("elif "), SKELETON_STRING, &
   skeleton_text("; then"), SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_END, &
   skeleton_text("else"), SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_RESUME, &
   skeleton_text("fi"), SKELETON_INDENT, SKELETON_NEWLINE]
  ```
  - The record has only fixed-size components (a text of at most
    SKELETON_TEXT_MAXIMUM characters with its length, so trailing blanks are
    kept, counts and procedure pointers), so constructors and function
    results may appear in array constructors (no allocatable component
    to leak) and the element kinds are named constants.
  - Nesting is flat, as in `value_type` (no recursive allocatable types):
    an interactor after the first element opens a sub-skeleton and
    SKELETON_END closes it, as upstream's parentheses do; `&`, `|` and `> \n`
    step over a whole sub-skeleton as over one element.
  - Lisp expressions are procedures: `skeleton_action(p)` for a quoted
    expression (called for its side effects) and `skeleton_value(p)` for
    another (it sets `skeleton_state%value` to one element, inserted
    literally); an interactor that reads is `skeleton_interactor(p)`.  The
    procedure pointers' interfaces return through `skeleton_state`
    because an interface can't name the record that holds the pointer
    (gfortran 13 miscompiles the forward reference).
  - The variables a skeleton sees (string, input, help, v1, v2) are the
    components of `skeleton_state`, saved and restored around each
    insertion and string around each list, as upstream binds them.
  - A mode's own elements are `skeleton_value` elements of its module
    (sh-mode's `<`); `skeleton-further-elements` is reduced to what it
    also does, binding variables, as a procedure called before and after.
  - Shell-dependent skeletons are `skeleton_variants_type` tables of
    named variants that `skeleton_filter_function` chooses from (sh-mode's
    `shell_skeleton_feature`); `shell_modify` replaces top-level elements by
    upstream's indices, so sh-script.el's `sh-modify` forms are kept as
    they are.

## Open questions

Decisions the porting work is waiting on, or defaults taken provisionally.
Each lists the default being used meanwhile.

1. **Merging.** *Decided:* one roadmap step per PR, based on main (never
   stacked); the owner merges, or clears a PR by number for Claude to
   merge once CI is green.
2. **Value model.** *Decided (2026-10-20):* no Lisp; values are a plain
   Fortran `value_type` with value semantics (value_mod), interned symbols
   by id (names_mod), data.c/fns.c operations in data_mod/utility_functions_mod. Values are
   stored as flat preorder atom arrays because gfortran 13/14 copy
   recursive allocatable components shallowly (double free).
3. **Error-code naming.** Error codes are `<SYMBOL>_CODE` parameters
   (`QUIT_CODE`, `ARITHMETIC_ERROR_CODE`; `FM_<SYMBOL>` before the owner's
   word renames of 2026-11-05), except the root `error` condition, which is
   `GENERIC_ERROR_CODE` because Fortran names are case-insensitive and
   `ERROR_CODE` would read as any error's code.
4. **Buffer internals visibility.** `buffer_mod` exposes `buffers(:)` and
   the current slot `current` (read-only outside the module) so insert_delete_mod and
   marker_mod can work like upstream's buffer.h macros. *Default: keep;
   higher layers use the procedures only.*
5. **Terminal capabilities.** *Decided (2026-10-20): (b), pure Fortran.*
   Upstream drives terminals through ncurses' libtinfo; Fortmacs reads
   the compiled terminfo files itself and evaluates `tparm` strings
   (`terminfo_mod`), with a built-in xterm-256color entry as fallback.
6. **Upstream LLM policy (FYI).** The emacs-31.1 tree contains an
   `AGENTS.md` stating that GNU Emacs does not accept LLM-generated
   contributions. That governs patches sent to Emacs, not this separate
   GPL project, but it means Fortmacs code (largely Claude-written) should
   never be offered upstream. *Default: read upstream as reference only;
   never propose changes to it.*

7. **Coding system names.** Coding systems are named by strings holding
   upstream's names, hyphens included (`"utf-8-unix"`, `"iso-latin-1"`),
   not underscored like other Emacs names: the names are data that files
   carry (`-*- coding: utf-8 -*-`), as key names are. In init.f90:
   `buffer_file_coding_system = "utf-8-dos"` (the default is "utf-8-unix"),
   `coding_system_for_read = "latin-1"`, `call prefer_coding_system("utf-8", status)`.
   *Default (2026-10-24): keep upstream's names.*
8. **Custom types (for step 26).** Customize depends on each user
   option's custom `:type`: prompts name it (`[integer] Set fill-column
   to value: `), values are read and checked by its widget (a boolean by
   y-or-n-p, a string or regexp as raw text, an integer or symbol as a
   read object, a `choice` by completing over its tags, then the chosen
   alternative), and the Customize buffers build their widgets from it.
   Fortmacs knows only each option's Fortran type, and the 245 user
   options use about 40 distinct `:type` forms (boolean, regexp, natnum,
   file, hook, `choice` of `const`s with `:tag`s, `repeat`, ...). Where an
   option is enumerated in Fortran, its `choice` constants map to the
   option's named constants; where Fortran encodes an upstream `nil` or
   symbol another way (a negative number, an empty string), the mapping
   is per option. *Default: record each option's type in
   tools/user-options.d as a second column, in upstream's notation with
   underscore names, plus, for each `const`, the Fortran value it maps to;
   gen-docstrings generates the type tree (an internal derived type, not
   part of the config API). `repeat`, `hook` and other types whose values
   are arrays or procedures are Deferred, as for set_variable.*

## Session log

Each PR adds its entry as a file of its own, `docs/log/DATE-BRANCH.md`
(`docs/log/2026-10-30-progress-workflow.md`): one paragraph starting with
the date and a title, as before.  Separate files keep two open PRs from
conflicting over the log.  Entries up to 2026-10-30 were moved there from
this section; entries that named no branch took a number or the first
hyphenated name they cite.
