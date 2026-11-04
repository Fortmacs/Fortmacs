# files_mod

- Module: files_mod, files_dir_mod, files_copy_mod (submodules)
- Upstream: files.el (+ startup.el `command-line-1` file arguments are in fortmacs_mod); dired.el `dired--ls-error-file`/`-buffer`
- State: partial

## Ported

`file-name-unquote` (local names),
`buffer-stale-function` and `buffer-stale--default-function` (`fast` as `BUFFER_STALE_FAST`),
`buffer-auto-revert-by-notification`,
`mounted-file-systems`,
`after-set-visited-file-name-hook`,
`make-directory` (PARENTS; `files--ensure-directory`),
`find-file-noselect` (with `find-file-noselect-1`, `after-find-file` messages and read-only),
`find-file` (for programs),
`create-file-buffer`,
`abbreviate-file-name`,
`backup-buffer` (rename or copy to `FILE~`, keeping modes),
`make-backup-file-name`,
`normal-backup-enable-predicate`,
`file-ownership-preserved-p`,
`save-buffer` (prefix args),
`basic-save-buffer` (modtime check, write-protected files),
`save-some-buffers` (with `save-some-buffers-functions`, also queried by `save-buffers-kill-emacs`),
`save-buffers-kill-emacs`,
`save-buffers-kill-terminal`,
`set-visited-file-name`,
`write-file`,
`find-file` and `write-file` commands (via submodule files_read_mod);
`C-x C-f`,
`C-x C-w`,
`C-x C-s`,
`C-x s`,
`C-x C-c`,
`normal-mode` and `set-auto-mode` (the -*- line, `auto-mode-alist` with the case-folding retry and (REGEXP MODE t) entries (Conf mode's entries, `/config.bat`), `file-name-sans-versions`; submodule files_mode_mod),
run by `after-find-file`,
`file-relative-name` (files_read_mod);
`find-file` and `find-file-noselect` on a directory run `find-directory-functions` (Dired) when `find-file-run-dired`,
`find-file-other-window` (C-x 4 f, C-x 4 C-f),
`backup-file-name-p`,
`auto-save-file-name-p`,
`file-name-extension`,
`file-size-human-readable` and `byte-count-to-string` (`file-size-human-readable-iec`),
`get-free-disk-space`,
`revert-buffer` with `revert-buffer-function` (permanently buffer-local),
`revert-buffer-in-progress-p`,
`revert-buffer-preserve-modes`,
`revert-buffer-restore-functions` (`revert-buffer-restore-read-only`, with `read-only-mode--state` set by C-x C-q and View mode),
`after-find-file` NOMODES;
in the submodule files_dir_mod: `insert-directory` (`insert-directory-program` through `call-process`, wildcards through the shell, standard error into a temporary file and the *ls error* buffer, the //DIRED// and //SUBDIRED// offsets as `dired-filename` properties, //DIRED-OPTIONS//),
`insert-directory-clean`,
`insert-directory-wildcard-in-dir-p`,
`directory-listing-before-filename-regexp`,
`file-truename` (symlink chasing with the cache of seen directories),
`delete-file` (directories are an error),
`delete-directory` (RECURSIVE),
`split-string-and-unquote`,
`shell-quote-wildcard-pattern`,
`string-prefix-p`,
`make-temp-file` (as `create_temp_file`: DIR-FLAG and SUFFIX; an empty, "." or ".." PREFIX names a file in `temporary-file-directory`; TEXT Deferred),
`temporary-file-directory` (cus-start.el: $TMPDIR, $TMP, $TEMP, else /tmp),
`revert-buffer--default` (`revert-without-query`, `before-revert-hook`, `after-revert-hook` as it was before reverting),
`revert-buffer-insert-file-contents--default-function`;
`copy-directory` (KEEP-TIME, PARENTS, COPY-CONTENTS, `copy-directory-create-symlink`),
`file-in-directory-p`,
`file-equal-p`,
`make-empty-file` (PARENTS),
`file-modes-symbolic-to-number` (FROM; u, g, o, a; r, w, x, X, s, t; =, +, -),
`file-modes-number-to-symbolic` (in files_copy_mod);
the rename hooks of fileio.c for directories,
`find-file-read-only` (C-x C-r),
`find-file-read-only-other-window` (C-x 4 r) with `find-file-read-only-1` (in files_read_mod);
`backup-buffer` copies with KEEP-TIME and the owner and group,
as upstream,
`before-save-hook`,
`after-save-hook`,
`write-file-functions` (permanently buffer-local, flushed by `set-visited-file-name`),
`interpreter-mode-alist` with `auto-mode-interpreter-regexp` (the sh, text and make entries),
the Makefile entries of `auto-mode-alist`,
`wildcard-to-regexp`,
`file-name-sans-extension`;
step 44a: `find-file-other-frame` (C-x 5 f, C-x 5 C-f) and `find-file-read-only-other-frame` (C-x 5 r; `find-file--read-only` takes the visiting procedure)

## Deferred

file name completion,
wildcards in `find-file`,
creating missing directories,
auto-save,
file locks,
major modes and local variables,
the other hooks,
uniquify,
numbered backups and `backup-directory-alist`,
`file-precious-flag`,
symlink chasing (a symlink is backed up by copying),
reverting from the auto-save file,
`revert-buffer-with-fine-grain`,
`ls-lisp`,
trash,
file-local and directory-local variables,
the rest of `interpreter-mode-alist`,
`magic-mode-alist`,
`major-mode-remap-alist`,
minor modes on the -*- line

## Tests

test_files_mod (ports files-tests-make-directory, files-tests-revert-buffer, files-tests-make-temp-file-empty-prefix; otherwise own: files-tests.el needs file name handlers, the minibuffer and hooks),
test_derived_mod (ports part of files-tests--bug75961),
test_dired_mode_mod (ports files-tests-insert-directory-wildcard-in-dir-p, files-tests-insert-directory-shows-files);
test_files_copy_mod (ports files-tests-file-modes-symbolic-to-number, files-tests-file-modes-number-to-symbolic, files-tests-copy-directory; own tests of copy-file, rename-file, add-name-to-file, file-in-directory-p, make-empty-file)
