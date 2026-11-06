# movemail_mod

- Module: movemail_mod
- Upstream: lib-src/movemail.c
- State: partial

## Ported

`main` for a local inbox (`move_mail`, run inside Fortmacs instead of as a program: it gives back what the program writes on its standard error): the lock file INBOX.lock made by linking a temporary file to it (mkostemp, link),
waiting a second between tries and removing a lock older than five minutes,
copying the inbox to a new destination file (O_EXCL, the user's read permission kept by the umask),
fsync,
emptying the inbox unless `-p` (PRESERVE) is given,
`fatal`,
`error`,
`pfatal_with_name` and `pfatal_and_delete` (their messages, "movemail: TEXT")

## Deferred

POP (`popmail`, `pop_retr`, `mbx_write`, `mbx_delimit_begin`, `mbx_delimit_end`, the `po:` inboxes, `-r`),
the system locks (`MAIL_USE_FLOCK` and `MAIL_USE_LOCKF`, flock and lockf) and maillock (`mail_spool_name`, liblockfile's `maillock` and `touchlock`): Fortmacs uses the lock file;
Not applicable: the usage message and option parsing (no command line),
dropping and regaining set-group-ID privileges and the child process that does it (Fortmacs is not setgid),
`xmalloc`

## Tests

test_movemail_mod (own; upstream has no tests of movemail.c: moving and emptying, preserving, a missing inbox, an existing destination, an empty destination name)
