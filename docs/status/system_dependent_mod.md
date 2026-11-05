# system_dependent_mod

- Module: system_dependent_mod (src/sys)
- Upstream: sysdep.c (+ dispnew.c SIGWINCH flag, process.c `allocate_pty`)
- State: done

## Ported

`fortmacs_get_tty`,
`fortmacs_set_tty`,
the termios part of `initialize_system_modes` / `reset_system_modes` (raw-mode transform as pure `tty_raw_settings`),
`get_tty_size`,
`set_window_size`,
`fortmacs_read`,
`fortmacs_full_write`,
`fortmacs_write`,
`input_available` (poll),
SIGWINCH and SIGINT flags,
`open_pty`,
file wrappers returning errno (`fortmacs_open_read`, `fortmacs_creat`, access, rename, chmod, mkdir, stat via statx, setenv),
`fortmacs_get_current_directory_name` (prefers a matching $PWD),
password-entry home directories,
`fortmacs_strerror`,
`child_setup_tty`,
`fortmacs_spawn_async` (a pipe or a pty, a new session, the exec-monitor pipe, extra environment entries over the editor's own copy of its environment, read from /proc/self/environ and kept current by `system_setenv`),
`fortmacs_kill`,
SIGPIPE ignored,
`fortmacs_number_processors`,
`inputs_available` (several descriptors),
`fortmacs_statvfs`;
for fileio.c's file operations: `fortmacs_open_mode`,
`system_link`,
`system_rename_noreplace`,
`system_set_file_times`,
`system_futimens`,
`fortmacs_umask`,
`system_fchmod`,
`system_chmod_at`,
`system_fchown`,
user and group names of ids and the lists of all of them;
access times in `file_status_type`;
step 42e: `system_suspend` (SIGTSTP to the process group),
`stuff_character` (TIOCSTI),
the descriptor part of `reset_system_modes` (tcdrain, O_NONBLOCK cleared; `reset_all_system_modes` and `initialize_all_system_modes`, which need the terminal and the frame, are in terminal_output_mod, above this module),
dispnew.c's `deliver_window_change_signal` (the SIGWINCH handler sets a flag, `window_change_pending`, cleared by `clear_window_change`; the size is read outside the handler) and SIGWINCH ending the wait for input;
for tests: a signal ignored or set to its default and its handling restored (`ignore_signal`, `default_signal`, `restore_signal`),
`fortmacs_getpid`,
`fortmacs_getpgrp`,
`system_setpgid`;
for time_functions_mod (step 42e): `system_localtime` (localtime_r with tm_gmtoff and tm_zone),
`system_mktime`,
`system_set_time_zone` (TZ for the C library, then tzset),
`fortmacs_clock_process_cputime`

## Deferred

job control: the foreground process group (`narrow_foreground_group`, `widen_foreground_group`, `inherited_pgroup`);
the subshell run instead of suspending when `cannot-suspend` is set (`sys_subshell`: GNU/Linux has job control, so upstream runs it only on that request);
F_SETOWN/SIGIO,
keypad/meta terminfo setup,
process groups

## Tests

test_system_dependent_mod (runs on a pty, so CI needs no terminal)
