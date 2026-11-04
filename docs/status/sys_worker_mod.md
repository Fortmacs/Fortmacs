# sys_worker_mod

- Module: sys_worker_mod (src/sys)
- Upstream: — (replaces select and SIGCHLD in process.c)
- State: done

## Ported

one POSIX worker thread per subprocess (pthread_create with all signals blocked, `recursive` procedures only),
reading its output and waiting on a pidfd (pidfd_open; a 50 ms waitpid(WNOHANG) poll where it is missing),
reaping it;
a mutex-protected message queue (output bytes, then the wait status) and a self-pipe the main thread polls (`worker_wake_fd`, `wait_for_worker_messages`, `take_worker_messages`, which joins finished workers);
`on_main_thread`

## Deferred

—

## Tests

test_process_mod
