# system_linux_mod

- Module: system_linux_mod (src/sys)
- Upstream: — (glibc/kernel headers)
- State: done

## Ported

struct termios / winsize / pollfd,
termios flags and c_cc indices,
ioctl/open/signal/errno constants,
bindings for tcgetattr,
tcsetattr,
read,
write,
close,
isatty,
poll,
ioctl,
signal,
raise,
posix_openpt,
grantpt,
unlockpt,
ptsname,
open,
errno;
file calls (creat, access, rename, unlink, chmod, mkdir, rmdir, statx, getcwd, setenv, unsetenv),
user ids and password entries (getuid, geteuid, getegid, getpwnam, getpwuid),
strerror,
struct statvfs and statvfs;
`open` with a mode,
`link`,
`renameat2`,
`utimensat`,
`futimens`,
`umask`,
`fchmod`,
`fchmodat`,
`fchown`,
`getgrgid` and the passwd and group enumerations (`setpwent`/`getpwent`/`endpwent`, `setgrent`/`getgrent`/`endgrent`),
O_CREAT/O_EXCL/O_TRUNC,
RENAME_NOREPLACE,
UTIME_NOW/UTIME_OMIT,
EXDEV/EINVAL/ENOTSUP;
step 42e: `tcdrain`,
`getpid`,
`getpgrp`,
`setpgid`,
`ioctl` with a character and TIOCSTI

## Deferred

—

## Tests

test_system_dependent_mod (struct sizes checked on both arches in CI)
