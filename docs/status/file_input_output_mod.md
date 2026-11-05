# file_input_output_mod

- Module: file_input_output_mod
- Upstream: fileio.c (+ dired.c `file-attributes` subset, editfns.c `user-uid`/`group-gid`)
- State: partial

## Ported

`write-region` with APPEND true ("Added to FILE"; for Fcc),
`make-symbolic-link` (OK-IF-ALREADY-EXISTS; `file-already-exists`),
`make-temp-name`,
`make-temp-file-internal` (as `create_temp_file_internal`: a file of mode 600, a directory of mode 700, or only a free name, with SUFFIX),
`set-default-file-modes`,
`unix-sync`,
`access-file`,
`file-executable-p`,
`file-newer-than-file-p`,
`file-name-directory`,
`file-name-nondirectory`,
`file-name-as-directory`,
`directory-file-name`,
`directory-name-p`,
`file-name-absolute-p`,
`expand-file-name` (`~`, `~USER`, relative default directories, null-byte check),
`get_homedir`,
`user_homedir`,
`file-exists-p`,
`file-readable-p`,
`file-writable-p`,
`file-directory-p`,
`is_file_accessible_directory`,
`file-regular-p`,
`file-symlink-p` (as a truth value),
`file-modes`,
`set-file-modes`,
`rename-file`,
`copy-file` (modes only),
`make-directory-internal`,
`delete-directory-internal`,
`delete-file-internal`,
`insert-file-contents` (with VISIT; decoded by the coding system fileio.c chooses: `coding-system-for-read`, `set-auto-coding-function`, `file-coding-system-alist`, else `undecided`; raw-text visits make the buffer unibyte; `last-coding-system-used` and `after-insert-file-set-coding`; the change hooks unless visiting; REPLACE as `if-regular`, keeping the text the buffer and the file share at both ends and the relative place of point and the window points in the rest; BEGINNING and END as byte offsets),
`write-region` (whole buffer or region, VISIT; encoded by `choose_write_coding_system` with `select-safe-coding-system-function`),
`verify-visited-file-modtime`,
`set-visited-file-modtime`,
`report_file_errno`,
`substitute-in-file-name` (with env.el `substitute-env-in-file-name`),
`file-system-info` (statvfs),
`file-attributes` with NOFOLLOW;
`copy-file` (OK-IF-ALREADY-EXISTS with `file-already-exists`, KEEP-TIME with `file-date-error`, PRESERVE-UID-GID, PRESERVE-PERMISSIONS, the umask, "Non-regular file", "Input and output files are the same"),
`rename-file` (RENAME_NOREPLACE, across file systems by copying, symbolic links and directories through the files.el hooks),
`add-name-to-file`,
`set-file-modes` (NOFOLLOW),
`file-modes` (FLAG),
`default-file-modes`,
`set-file-times` (TIMESTAMP, a `time_type`; NOFOLLOW, the current time);
step 42e timefns: `visited-file-modtime` (0 for no recorded time, -1 for a file that did not exist; fileio.c's `buffer_visited_file_modtime` is in buffer_mod, below undo_mod which records it),
`set-visited-file-modtime` with TIME-FLAG (a time value, or the flags 0 and -1, else args-out-of-range),
`verify-visited-file-modtime` comparing time values;
the buffer's `modtime` and the undo list's first-change entries are `time_type`s;
`write-region-annotate-functions` (`write_region_annotate_functions`, a hook of functions taking BEGINNING and END, BEGINNING 0 for the whole buffer: a function that makes another buffer current has that buffer's accessible text written, with its coding system; step 52d, for Rmail)

## Deferred

file name handlers,
the annotations the functions of `write-region-annotate-functions` return, starting over with the functions of a buffer they make current, and `write-region-post-annotation-function`,
the auto-save coding system,
BEGINNING/END,
APPEND as a position,
MUSTBENEW/LOCKNAME,
fsync,
ACLs and SELinux,
`file-name-concat` (a list of strings has no shared Fortran form yet),
the TEXT of `make-temp-file-internal` (its coding system is chosen as for a buffer region),
auto-saving (`do-auto-save`, `recent-auto-save-p`, `set-buffer-auto-saved`, `clear-buffer-auto-save-failure`),
`file-name-case-insensitive-p` (always false on GNU/Linux, where upstream asks pathconf only on Cygwin and macOS),
`next-read-file-uses-dialog-p` (always false without a GUI toolkit);
Not applicable: `set-binary-mode` (batch-mode Lisp streams),
`car-less-than-car` and the cleanup handlers of nonlocal exits (`close_file_unwind` and the like)

## Tests

test_file_input_output_mod (make-temp-file-internal, set-default-file-modes, unix-sync and access-file; ports directory-file-name, file-name-as-directory, relative-HOME, relative-default-directory, expand-file-name-null-bytes, file-name-absolute-p)
