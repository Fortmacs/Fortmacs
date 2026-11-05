# execute_process_mod

- Module: execute_process_mod, execute_process_execute_mod (submodule)
- Upstream: callproc.c (+ env.el `setenv-internal`)
- State: partial

## Ported

`call-process` (INFILE, DESTINATION t, nil, a buffer or a buffer name; ARGS; standard error to a file of its own (`error_file`, the (REAL-DESTINATION ERROR-FILE) form); the current buffer's directory (`get_current_directory`); exit status or signal description),
`call-process-region` (whole buffer or a region, DELETE; via a temporary file, `create_temp_file`),
the PATH search of lread.c `openp` (`locate_program`),
`shell-file-name` (`init_callproc`, from SHELL);
sysdep: `fortmacs_spawn` (fork and execv; a new session, SIGINT/SIGQUIT/SIGPIPE at their defaults),
`wait_for_termination`,
`fortmacs_pipe`,
`fortmacs_mkostemp`,
readlink and execv;
the current directory expanded as `get_current_directory` does (a `default-directory` under `~`);
the environment: `process-environment` and `initial-environment` (`set_initial_environment`, filled from the editor's environment on first use, the last variable first as upstream conses them),
`getenv-internal` (`getenv_internal`, `getenv_internal_1`; DISPLAY from `initial-environment`; R6-15: `call getenv_internal(variable, value, status, initial)`, an integer status as the intrinsic get_environment_variable's, GETENV_SET, GETENV_NOT_SET or GETENV_REMOVED (a lone name), and the optional logical INITIAL for upstream's ENV `initial-environment`),
`setenv-internal` (env.el; on `process_environment`),
`make_environment_block` with `add_env` (PWD corrected to the directory, DISPLAY from `initial-environment`, the first definition wins, lone names removed),
passed whole to every child of `execute_process` and `make_process` (execve)

## Deferred

DESTINATION 0 and `(:file FILE)`,
an ERROR-FILE of nil,
DISPLAY,
quitting,
coding systems,
`exec-path`,
`process-environment` and `initial-environment` as variables (C-h v, the configuration; reached through `getenv`/`setenv`),
the frame's `display` parameter for DISPLAY (waits for the GUI),
`process-file`

## Tests

test_execute_process_mod (ports callproc-tests: call-process-region-entire-buffer-with-delete; own tests for the rest, including the environment children inherit, the `environment` overrides, `make_environment_block` and `getenv_internal`'s status and INITIAL)
