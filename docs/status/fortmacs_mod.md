# fortmacs_mod

- Module: fortmacs_mod
- Upstream: emacs.c `main` (+ startup.el `command-line` terminal part, `-q`/`-Q` and the init file and `command-line-1` file arguments, faces.el `tty-run-terminal-initialization`, buffer.c `initialize_buffer` directory)
- State: partial

## Ported

`fortmacs_main`: bindings,
`*scratch*` (in the starting directory),
terminal setup (incl. xterm init),
windows (the root window shows `*scratch*`, as frame.c:make_frame) and faces,
raw mode,
visiting command-line files (`+LINE`, `--`, `-file`/`-find-file`/`-visit`) and displaying them (the first in the selected window, a second in another window, more as the other window's previous buffers),
the command loop,
restoring the terminal;
`sort_arguments` (the whole standard_arguments table of a GNU/Linux build) and `argmatch` for `--version`,
`-nw` and `--help`;
`command_line` (`-q`, `--no-init-file`, `-Q`, `-no-site-file`, abbreviated long options, --OPTION=VALUE and its errors; the X, debugger, user, init-directory, dump and native-compilation options are Deferred),
`load_init_file` (the user's `configure`, errors shown in the echo area, `init-file-had-error`);
`execute_editor`,
the program (`--version`, `--help`, tty checks)

## Deferred

other options (`--debug-init`, `-u`, `--init-directory`),
`+LINE:COLUMN` columns (need indent.c),
splitting windows for two files,
daemon,
dumping,
signals

## Tests

test_display_engine_mod (runs the editor on a pty),
test_files_mod,
test_fortmacs_mod (options and the init file, on a pty)
