! sysdep_fm.f90 -- terminal modes, tty size and raw I/O for Linux.
! Ported from GNU Emacs 31.1 src/sysdep.c (and the window-size signal
! flag of src/dispnew.c).
!
! Copyright (C) 1985-1988, 1993-1995, 1999-2026 Free Software
! Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> System-dependent terminal handling.  Only Linux is supported, so the
!> DOS_NT and old-BSD branches of sysdep.c are not ported.
!>
!> File descriptors are `int32` (the same kind as C int on both targets),
!> so callers need no ISO_C_BINDING.  Errors from the C library come back
!> as file-error statuses whose detail names the call and errno.
module sysdep_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use, intrinsic :: iso_c_binding, only: c_int, c_long, c_size_t, c_char, c_ptr, c_null_char, &
                                          c_associated, c_f_pointer, c_signed_char, c_funloc, c_funptr, c_loc, c_null_ptr, c_int32_t
    use error_fm, only: status_type, set_error, FM_FILE_ERROR
    use sys_linux_fm, only: c_close, c_grantpt, c_ioctl_winsize, c_isatty, c_open, c_poll, c_posix_openpt, &
                            c_ptsname, c_read, c_signal, sigint, c_string_to_fortran, c_tcgetattr, &
                            c_tcsetattr, c_unlockpt, c_write, cdisable, cs8, echo, eintr, errno, &
                            icanon, icrnl, iexten, ignbrk, inlcr, isig, istrip, ixany, ixon, &
                            o_noctty, o_rdwr, onlcr, parenb, pollfd_type, pollin, sigwinch, tab3, &
                            tabdly, tcsadrain, tcsaflush, termios_type, tiocgwinsz, tiocswinsz, &
                            vdiscard, vintr, vlnext, vmin, vquit, vreprint, vstart, vstop, vsusp, &
                            vtime, vwerase, winsize_type, c_creat, c_access, c_rename, c_unlink, &
                            c_chmod, c_statx, c_getcwd, c_getuid, c_geteuid, c_getegid, &
                            c_getpwnam, c_getpwuid, at_symlink_nofollow, S_IFLNK, c_mkdir, &
                            c_rmdir, c_setenv, c_unsetenv, &
                            c_strerror, statx_type, passwd_type, o_rdonly, o_cloexec, at_fdcwd, &
                            at_statx_sync_as_stat, statx_basic_stats, ENOENT, EACCES, EEXIST, &
                            ENOTDIR, EISDIR, ERANGE, F_OK, R_OK, W_OK, X_OK, S_IFMT, S_IFDIR, &
                            S_IFREG, c_fork, c_dup2, c_chdir, c_exit, c_pipe2, &
                            c_waitpid, c_readlink, c_execv, c_strsignal, c_mkostemp, &
                            O_WRONLY, c_opendir, c_readdir, c_closedir, c_symlink, c_localtime_r, &
                            tm_type
    implicit none(type, external)
    private

    !> Port of systty.h:struct emacs_tty.
    type, public :: emacs_tty_type
        type(termios_type) :: main
    end type emacs_tty_type

    !> The quit character, C-g (keyboard.c:quit_char).
    integer(c_signed_char), parameter, public :: QUIT_CHAR = 7_c_signed_char

    public :: emacs_get_tty, emacs_set_tty, tty_raw_settings, init_sys_modes, &
              reset_sys_modes, get_tty_size, set_tty_size, emacs_read, &
              emacs_full_write, emacs_write, input_available
    public :: install_window_change_handler, window_change_pending
    public :: install_interrupt_handler, interrupt_pending
    public :: open_pty, close_fd, tabs_safe_p, emacs_isatty
    public :: file_stat_type, emacs_open_read, emacs_creat, sys_access, sys_rename, &
              sys_unlink, sys_chmod, sys_stat, sys_mkdir, sys_rmdir, sys_setenv, &
              sys_unsetenv, emacs_get_current_dir_name, &
              passwd_home_by_name, passwd_home_of_user, passwd_name_of_user, &
              emacs_geteuid, emacs_getegid, emacs_strerror
    public :: ENOENT, EACCES, EEXIST, ENOTDIR, EISDIR, F_OK, R_OK, W_OK, X_OK, &
              S_IFMT, S_IFDIR, S_IFREG, S_IFLNK
    public :: sys_string_type, emacs_spawn, wait_for_termination, emacs_pipe, emacs_strsignal, &
              emacs_readlink, emacs_execv, emacs_mkostemp, emacs_directory_entries, sys_symlink, &
              emacs_localtime

    !> A string of any length, for argument vectors.
    type :: sys_string_type
        character(len=:), allocatable :: s
    end type sys_string_type

    !> A C argument vector: the strings, NUL-terminated, in `buf`, and a
    !> null-terminated array of pointers to them.
    type :: c_argv_type
        character(kind=c_char), allocatable :: buf(:)
        type(c_ptr), allocatable :: ptrs(:)
    end type c_argv_type

    !> What fileio.c reads from struct stat: the file's type and permission
    !> bits, link count, owner, size and modification time.
    type :: file_stat_type
        integer(int32) :: mode = 0
        integer(int64) :: nlink = 0
        integer(int32) :: uid = 0, gid = 0
        integer(int64) :: size = 0
        integer(int64) :: mtime_sec = 0
        integer(int32) :: mtime_nsec = 0
        integer(int64) :: dev = 0, ino = 0
    end type file_stat_type

    !> Set (to the signal number) asynchronously by the SIGWINCH handler;
    !> read and cleared by `window_change_pending`.
    integer(c_int), volatile :: winch_flag = 0
    !> Set by the SIGINT handler; read and cleared by `interrupt_pending`.
    integer(c_int), volatile :: sigint_flag = 0

contains

    !> Fill `status` from errno after the C call `what` failed.
    subroutine sys_error(status, what, err)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: what
        !> errno, if already fetched.
        integer(c_int), intent(in), optional :: err
        character(len=12) :: num

        if (present(err)) then
            write (num, "(i0)") err
        else
            write (num, "(i0)") errno()
        end if

        call set_error(status, FM_FILE_ERROR, what//" failed, errno "//trim(num))
    end subroutine sys_error

    !> Read the terminal modes of `fd`.  Port of sysdep.c:emacs_get_tty.
    subroutine emacs_get_tty(fd, settings, status)
        integer(int32), intent(in) :: fd
        type(emacs_tty_type), intent(out) :: settings
        type(status_type), intent(out) :: status

        if (c_tcgetattr(fd, settings%main) /= 0) call sys_error(status, "tcgetattr")
    end subroutine emacs_get_tty

    !> Set the terminal modes of `fd`, discarding pending input if `flushp`.
    !> Reads the settings back and retries (up to 10 times) until they
    !> stick, as POSIX lets tcsetattr succeed partially.
    !> Port of sysdep.c:emacs_set_tty.
    subroutine emacs_set_tty(fd, settings, flushp, status)
        integer(int32), intent(in) :: fd
        type(emacs_tty_type), intent(in) :: settings
        logical, intent(in) :: flushp
        type(status_type), intent(out) :: status
        type(termios_type) :: now
        integer(c_int) :: action
        integer :: i

        action = TCSADRAIN
        if (flushp) action = TCSAFLUSH

        do i = 1, 10
            if (c_tcsetattr(fd, action, settings%main) < 0) then
                if (errno() == EINTR) cycle
                call sys_error(status, "tcsetattr")
                return
            end if

            if (c_tcgetattr(fd, now) /= 0) cycle
            if (now%c_iflag == settings%main%c_iflag .and. &
                now%c_oflag == settings%main%c_oflag .and. &
                now%c_cflag == settings%main%c_cflag .and. &
                now%c_lflag == settings%main%c_lflag .and. &
                all(now%c_cc == settings%main%c_cc)) return
        end do
    end subroutine emacs_set_tty

    !> The modes Emacs runs a terminal in, derived from `old`: no echo, no
    !> line editing, no CR/NL mapping, 8-bit input, reads of at least one
    !> byte, and special characters disabled except C-g for SIGINT/SIGQUIT
    !> on the controlling terminal.  The termios part of
    !> sysdep.c:init_sys_modes, separated so it can be tested.
    pure function tty_raw_settings(old, flow_control, meta_key, controlling) result(tty)
        type(emacs_tty_type), intent(in) :: old
        logical, intent(in) :: flow_control, meta_key, controlling
        type(emacs_tty_type) :: tty

        tty = old

        associate (m => tty%main)
            m%c_iflag = ior(m%c_iflag, IGNBRK)          ! Ignore break condition
            m%c_iflag = iand(m%c_iflag, not(ICRNL))     ! No CR -> NL on input
            m%c_iflag = iand(m%c_iflag, not(INLCR))     ! No NL -> CR on input
            m%c_iflag = iand(m%c_iflag, not(ISTRIP))    ! Keep the 8th bit
            m%c_lflag = iand(m%c_lflag, not(ECHO))      ! No echo
            m%c_lflag = iand(m%c_lflag, not(ICANON))    ! No erase/kill processing
            m%c_lflag = iand(m%c_lflag, not(IEXTEN))    ! No other editing chars
            m%c_lflag = ior(m%c_lflag, ISIG)            ! Enable signals

            if (flow_control) then
                m%c_iflag = ior(m%c_iflag, IXON)
                m%c_iflag = iand(m%c_iflag, not(IXANY))
            else
                m%c_iflag = iand(m%c_iflag, not(IXON))
            end if

            m%c_oflag = iand(m%c_oflag, not(ONLCR))     ! No NL -> CR-NL on output
            m%c_oflag = iand(m%c_oflag, not(TAB3))      ! No tab expansion

            if (meta_key) then
                m%c_cflag = ior(m%c_cflag, CS8)          ! 8-bit input
                m%c_cflag = iand(m%c_cflag, not(PARENB)) ! No parity
            end if

            if (controlling) then
                m%c_cc(VINTR) = QUIT_CHAR                ! C-g gives SIGINT
                m%c_cc(VQUIT) = QUIT_CHAR
            else
                m%c_cc(VINTR) = CDISABLE
                m%c_cc(VQUIT) = CDISABLE
            end if

            m%c_cc(VMIN) = 1_c_signed_char              ! Wait for at least 1 byte
            m%c_cc(VTIME) = 0_c_signed_char             ! however long that takes
            m%c_cc(VSUSP) = CDISABLE                    ! C-z is ordinary input
            m%c_cc(VLNEXT) = CDISABLE
            m%c_cc(VREPRINT) = CDISABLE
            m%c_cc(VWERASE) = CDISABLE
            m%c_cc(VDISCARD) = CDISABLE

            if (flow_control) then
                m%c_cc(VSTART) = 17_c_signed_char        ! C-q
                m%c_cc(VSTOP) = 19_c_signed_char         ! C-s
            else
                m%c_cc(VSTART) = CDISABLE
                m%c_cc(VSTOP) = CDISABLE
            end if
        end associate
    end function tty_raw_settings

    !> Save the modes of `fd` in `old` and switch it to Emacs's modes.
    !> Port of the termios part of sysdep.c:init_sys_modes.
    subroutine init_sys_modes(fd, old, status, flow_control, meta_key, controlling)
        integer(int32), intent(in) :: fd
        type(emacs_tty_type), intent(out) :: old
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: flow_control, meta_key, controlling
        logical :: flow, meta, ctl

        flow = .false.
        meta = .true.
        ctl = .true.
        if (present(flow_control)) flow = flow_control
        if (present(meta_key)) meta = meta_key
        if (present(controlling)) ctl = controlling

        call emacs_get_tty(fd, old, status)
        if (.not. status%ok()) return
        call emacs_set_tty(fd, tty_raw_settings(old, flow, meta, ctl), .false., status)
    end subroutine init_sys_modes

    !> Restore the modes saved by `init_sys_modes`.
    !> Port of the termios part of sysdep.c:reset_sys_modes.
    subroutine reset_sys_modes(fd, old, status)
        integer(int32), intent(in) :: fd
        type(emacs_tty_type), intent(in) :: old
        type(status_type), intent(out) :: status

        call emacs_set_tty(fd, old, .false., status)
    end subroutine reset_sys_modes

    !> Whether `fd` is a terminal.  The isatty check of emacs.c:main.
    logical function emacs_isatty(fd)
        integer(int32), intent(in) :: fd

        emacs_isatty = c_isatty(int(fd, c_int)) == 1
    end function emacs_isatty

    !> Whether the terminal on `fd` passes tabs through (does not expand
    !> them to spaces).  Port of sysdep.c:tabs_safe_p.
    logical function tabs_safe_p(fd)
        integer(int32), intent(in) :: fd
        type(emacs_tty_type) :: settings
        type(status_type) :: status

        call emacs_get_tty(fd, settings, status)
        tabs_safe_p = status%ok() .and. iand(settings%main%c_oflag, TABDLY) /= TAB3
    end function tabs_safe_p

    !> Width and height of the terminal on `fd`.
    !> Port of sysdep.c:get_tty_size.
    subroutine get_tty_size(fd, width, height, status)
        integer(int32), intent(in) :: fd
        integer(int32), intent(out) :: width, height
        type(status_type), intent(out) :: status
        type(winsize_type) :: ws

        width = 0
        height = 0

        if (c_ioctl_winsize(fd, TIOCGWINSZ, ws) < 0) then
            call sys_error(status, "ioctl(TIOCGWINSZ)")
            return
        end if

        width = int(ws%ws_col, int32)
        height = int(ws%ws_row, int32)
    end subroutine get_tty_size

    !> Set the size of the terminal on `fd` (used on pseudo-terminals).
    !> Port of sysdep.c:set_window_size.
    subroutine set_tty_size(fd, width, height, status)
        integer(int32), intent(in) :: fd
        integer(int32), intent(in) :: width, height
        type(status_type), intent(out) :: status
        type(winsize_type) :: ws

        if (c_ioctl_winsize(fd, TIOCGWINSZ, ws) < 0) then
            call sys_error(status, "ioctl(TIOCGWINSZ)")
            return
        end if

        ws%ws_col = int(width, kind(ws%ws_col))
        ws%ws_row = int(height, kind(ws%ws_row))
        if (c_ioctl_winsize(fd, TIOCSWINSZ, ws) < 0) call sys_error(status, "ioctl(TIOCSWINSZ)")
    end subroutine set_tty_size

    !> Read up to len(buf) bytes from `fd` into `buf`, retrying on EINTR.
    !> `nread` is the count (0 at end of file).
    !> Port of sysdep.c:emacs_read (via emacs_intr_read, not interruptible).
    subroutine emacs_read(fd, buf, nread, status, err)
        integer(int32), intent(in) :: fd
        ! A read buffer: its length is the caller's, by design.
        ! allow(assumed-size-character-intent)
        character(len=*), intent(out) :: buf
        integer(int64), intent(out) :: nread
        type(status_type), intent(out) :: status
        !> errno on failure, 0 on success.
        integer(int32), intent(out), optional :: err
        integer(c_long) :: r
        integer(c_int) :: e

        nread = 0
        if (present(err)) err = 0

        do
            r = c_read(fd, buf, int(len(buf), c_size_t))
            if (r >= 0) exit
            e = errno()

            if (e /= EINTR) then
                if (present(err)) err = e
                call sys_error(status, "read", e)
                return
            end if
        end do

        nread = int(r, int64)
    end subroutine emacs_read

    !> Write all of `buf` to `fd`, retrying on EINTR and partial writes.
    !> `written` is the number of bytes written before any error.
    !> Port of sysdep.c:emacs_full_write.
    subroutine emacs_full_write(fd, buf, written, status, err)
        integer(int32), intent(in) :: fd
        character(len=*), intent(in) :: buf
        integer(int64), intent(out) :: written
        type(status_type), intent(out) :: status
        !> errno on failure, 0 on success.
        integer(int32), intent(out), optional :: err
        integer(c_long) :: r
        integer(c_int) :: e
        integer(int64) :: n

        n = len(buf, kind=int64)
        written = 0
        if (present(err)) err = 0

        do while (written < n)
            r = c_write(fd, buf(written + 1:), int(n - written, c_size_t))

            if (r < 0) then
                e = errno()
                if (e == EINTR) cycle
                if (present(err)) err = e
                call sys_error(status, "write", e)
                return
            end if

            written = written + r
        end do
    end subroutine emacs_full_write

    !> Write `buf` to `fd`.  Port of sysdep.c:emacs_write.
    subroutine emacs_write(fd, buf, status)
        integer(int32), intent(in) :: fd
        character(len=*), intent(in) :: buf
        type(status_type), intent(out) :: status
        integer(int64) :: written

        call emacs_full_write(fd, buf, written, status)
    end subroutine emacs_write

    !> True if `fd` has input within `timeout_ms` milliseconds (0: just
    !> check; negative: wait indefinitely).  Not a port: the poll(2) core
    !> of what process.c:wait_reading_process_output does for keyboard input.
    logical function input_available(fd, timeout_ms)
        integer(int32), intent(in) :: fd, timeout_ms
        type(pollfd_type) :: pfd(1)
        integer(c_int) :: r

        pfd(1)%fd = fd
        pfd(1)%events = POLLIN
        pfd(1)%revents = 0

        do
            r = c_poll(pfd, 1_c_long, timeout_ms)
            if (r >= 0 .or. errno() /= EINTR) exit
            ! A C-g (SIGINT) must reach the command loop.
            if (sigint_flag /= 0) exit
        end do

        input_available = r > 0 .and. iand(pfd(1)%revents, POLLIN) /= 0
    end function input_available

    !> SIGWINCH handler: only sets a flag.  The work happens in the
    !> command loop.  Cf. dispnew.c:deliver_window_change_signal.
    subroutine handle_window_change_signal(signum) bind(C)
        integer(c_int), value :: signum

        winch_flag = signum
    end subroutine handle_window_change_signal

    !> Install the SIGWINCH handler.
    subroutine install_window_change_handler()
        type(c_funptr) :: old

        old = c_signal(SIGWINCH, c_funloc(handle_window_change_signal))
    end subroutine install_window_change_handler

    !> SIGINT handler, for the quit character typed on the controlling
    !> terminal: only sets a flag, which the command loop reads.  Cf.
    !> keyboard.c:handle_interrupt_signal.
    subroutine handle_interrupt_signal(signum) bind(C)
        integer(c_int), value :: signum

        sigint_flag = signum
    end subroutine handle_interrupt_signal

    !> Install the SIGINT handler (keyboard.c:init_keyboard).
    subroutine install_interrupt_handler()
        type(c_funptr) :: old

        old = c_signal(SIGINT, c_funloc(handle_interrupt_signal))
    end subroutine install_interrupt_handler

    !> True if the quit character was typed (SIGINT) since the last call.
    logical function interrupt_pending()
        interrupt_pending = sigint_flag /= 0
        sigint_flag = 0
    end function interrupt_pending

    !> True if the terminal was resized since the last call.
    logical function window_change_pending()
        window_change_pending = winch_flag /= 0
        winch_flag = 0
    end function window_change_pending

    !> Open a pseudo-terminal pair: `master` and the matching `slave`.
    !> Port of the Unix98 path of process.c:allocate_pty (plus opening the
    !> slave, as process.c:create_process does).
    subroutine open_pty(master, slave, status)
        integer(int32), intent(out) :: master, slave
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        slave = -1
        master = c_posix_openpt(ior(O_RDWR, O_NOCTTY))

        if (master < 0) then
            call sys_error(status, "posix_openpt")
            return
        end if

        if (c_grantpt(master) /= 0 .or. c_unlockpt(master) /= 0) then
            call sys_error(status, "grantpt/unlockpt")
            call close_fd(master)
            return
        end if

        name = c_string_to_fortran(c_ptsname(master))
        slave = c_open(name//char(0), ior(O_RDWR, O_NOCTTY))

        if (slave < 0) then
            call sys_error(status, "open("//name//")")
            call close_fd(master)
        end if
    end subroutine open_pty

    !> Close `fd`, ignoring errors.  Cf. sysdep.c:emacs_close.
    subroutine close_fd(fd)
        integer(int32), intent(in) :: fd
        integer(c_int) :: r

        r = c_close(fd)
    end subroutine close_fd

    ! ------------------------------------------------------------------
    ! Files.  Each returns 0 or the errno value, so that fileio.c's ports
    ! can build their messages (report_file_errno) without ISO_C_BINDING.
    ! ------------------------------------------------------------------

    !> `name` as a NUL-terminated C string.
    pure function c_path(name) result(p)
        character(len=*), intent(in) :: name
        character(len=len(name) + 1) :: p

        p = name//c_null_char
    end function c_path

    !> Open `file` for reading.  Cf. sysdep.c:emacs_open (O_CLOEXEC added,
    !> retrying on EINTR).
    integer(int32) function emacs_open_read(file, fd) result(err)
        character(len=*), intent(in) :: file
        integer(int32), intent(out) :: fd

        err = 0

        do
            fd = c_open(c_path(file), ior(O_RDONLY, O_CLOEXEC))
            if (fd >= 0) return
            err = errno()
            if (err /= EINTR) return
        end do
    end function emacs_open_read

    !> Create (or truncate) `file` for writing with permission bits `mode`
    !> (before the umask).  Cf. sysdep.c:emacs_open with O_CREAT | O_TRUNC.
    integer(int32) function emacs_creat(file, mode, fd) result(err)
        character(len=*), intent(in) :: file
        integer(int32), intent(in) :: mode
        integer(int32), intent(out) :: fd

        err = 0

        do
            fd = c_creat(c_path(file), mode)
            if (fd >= 0) return
            err = errno()
            if (err /= EINTR) return
        end do
    end function emacs_creat

    !> access(2) with `mode` (F_OK, R_OK, W_OK, X_OK); 0 if allowed.
    integer(int32) function sys_access(file, mode) result(err)
        character(len=*), intent(in) :: file
        integer(int32), intent(in) :: mode

        err = 0
        if (c_access(c_path(file), mode) /= 0) err = errno()
    end function sys_access

    !> rename(2).
    integer(int32) function sys_rename(old, new) result(err)
        character(len=*), intent(in) :: old, new

        err = 0
        if (c_rename(c_path(old), c_path(new)) /= 0) err = errno()
    end function sys_rename

    !> unlink(2).
    integer(int32) function sys_unlink(file) result(err)
        character(len=*), intent(in) :: file

        err = 0
        if (c_unlink(c_path(file)) /= 0) err = errno()
    end function sys_unlink

    !> mkdir(2) with permission bits `mode` (before the umask).
    integer(int32) function sys_mkdir(dir, mode) result(err)
        character(len=*), intent(in) :: dir
        integer(int32), intent(in) :: mode

        err = 0
        if (c_mkdir(c_path(dir), mode) /= 0) err = errno()
    end function sys_mkdir

    !> rmdir(2).
    integer(int32) function sys_rmdir(dir) result(err)
        character(len=*), intent(in) :: dir

        err = 0
        if (c_rmdir(c_path(dir)) /= 0) err = errno()
    end function sys_rmdir

    !> Set environment variable `name` to `value` for this process (and
    !> so for get_environment_variable).
    integer(int32) function sys_setenv(name, value) result(err)
        character(len=*), intent(in) :: name, value

        err = 0
        if (c_setenv(c_path(name), c_path(value), 1) /= 0) err = errno()
    end function sys_setenv

    !> Remove environment variable `name`.
    integer(int32) function sys_unsetenv(name) result(err)
        character(len=*), intent(in) :: name

        err = 0
        if (c_unsetenv(c_path(name)) /= 0) err = errno()
    end function sys_unsetenv

    !> chmod(2).
    integer(int32) function sys_chmod(file, mode) result(err)
        character(len=*), intent(in) :: file
        integer(int32), intent(in) :: mode

        err = 0
        if (c_chmod(c_path(file), mode) /= 0) err = errno()
    end function sys_chmod

    !> Status of `file`, following symbolic links unless `nofollow`.
    !> Cf. emacs_fstatat (statx is used because its layout is the same on
    !> both targets).
    integer(int32) function sys_stat(file, st, nofollow) result(err)
        character(len=*), intent(in) :: file
        type(file_stat_type), intent(out) :: st
        logical, intent(in), optional :: nofollow
        type(statx_type) :: sx
        integer(c_int) :: flags

        err = 0
        flags = AT_STATX_SYNC_AS_STAT

        if (present(nofollow)) then
            if (nofollow) flags = ior(flags, AT_SYMLINK_NOFOLLOW)
        end if

        if (c_statx(AT_FDCWD, c_path(file), flags, STATX_BASIC_STATS, sx) /= 0) then
            err = errno()
            return
        end if

        st%mode = iand(int(sx%stx_mode, int32), int(z'FFFF', int32))
        st%nlink = iand(int(sx%stx_nlink, int64), int(z'FFFFFFFF', int64))
        st%uid = sx%stx_uid
        st%gid = sx%stx_gid
        st%size = sx%stx_size
        st%mtime_sec = sx%stx_mtime%tv_sec
        st%mtime_nsec = sx%stx_mtime%tv_nsec
        st%dev = ior(shiftl(int(sx%stx_dev_major, int64), 32), &
                     iand(int(sx%stx_dev_minor, int64), int(z'FFFFFFFF', int64)))
        st%ino = sx%stx_ino
    end function sys_stat

    !> The current working directory, preferring $PWD when it names the
    !> same directory (so symbolic links in it are kept); "" on failure.
    !> Port of sysdep.c:emacs_get_current_dir_name.
    function emacs_get_current_dir_name() result(dir)
        character(len=:), allocatable :: dir
        character(len=:), allocatable :: pwd
        character(kind=c_char, len=:), allocatable :: buf
        type(file_stat_type) :: dotstat, pwdstat
        type(c_ptr) :: p
        integer :: length, stat, bufsize
        integer(int32) :: e1, e2

        call get_environment_variable("PWD", length=length, status=stat)

        if (stat == 0 .and. length > 0) then
            allocate (character(len=length) :: pwd)
            call get_environment_variable("PWD", pwd)

            if (pwd(1:1) == "/") then
                e1 = sys_stat(pwd, pwdstat)
                e2 = sys_stat(".", dotstat)

                if (e1 == 0 .and. e2 == 0 .and. pwdstat%dev == dotstat%dev .and. &
                    pwdstat%ino == dotstat%ino) then
                    dir = pwd
                    return
                end if
            end if
        end if

        bufsize = 256

        do
            allocate (character(kind=c_char, len=bufsize) :: buf)
            p = c_getcwd(buf, int(bufsize, c_size_t))
            if (c_associated(p)) exit
            deallocate (buf)

            ! ERANGE: the buffer was too small; grow it and retry.
            if (errno() /= ERANGE .or. bufsize > 1048576) then
                dir = ""
                return
            end if

            bufsize = 2*bufsize
        end do

        dir = buf(1:index(buf, c_null_char) - 1)
    end function emacs_get_current_dir_name

    !> Home directory of the user called `name` (getpwnam).  `found` is
    !> false when there is no such user.
    subroutine passwd_home_by_name(name, home, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: home
        logical, intent(out) :: found

        call passwd_home(c_getpwnam(c_path(name)), home, found)
    end subroutine passwd_home_by_name

    !> Home directory of the real user (getpwuid (getuid ())).
    subroutine passwd_home_of_user(home, found)
        character(len=:), allocatable, intent(out) :: home
        logical, intent(out) :: found

        call passwd_home(c_getpwuid(c_getuid()), home, found)
    end subroutine passwd_home_of_user

    !> Login name of the real user (getpwuid (getuid ())), as
    !> editfns.c:Fuser_real_login_name finds it.  `found` is false when the
    !> user has no passwd entry.
    subroutine passwd_name_of_user(name, found)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        type(c_ptr) :: p
        type(passwd_type), pointer :: pw

        p = c_getpwuid(c_getuid())
        found = c_associated(p)
        name = ""
        if (.not. found) return
        call c_f_pointer(p, pw)
        name = c_string_to_fortran(pw%pw_name)
    end subroutine passwd_name_of_user

    subroutine passwd_home(p, home, found)
        type(c_ptr), intent(in) :: p
        character(len=:), allocatable, intent(out) :: home
        logical, intent(out) :: found
        type(passwd_type), pointer :: pw

        found = c_associated(p)
        home = ""
        if (.not. found) return
        call c_f_pointer(p, pw)
        home = c_string_to_fortran(pw%pw_dir)
    end subroutine passwd_home

    !> The effective user id (editfns.c:Fuser_uid).
    integer(int32) function emacs_geteuid()
        emacs_geteuid = c_geteuid()
    end function emacs_geteuid

    !> The effective group id (editfns.c:Fgroup_gid).
    integer(int32) function emacs_getegid()
        emacs_getegid = c_getegid()
    end function emacs_getegid

    !> Text describing errno value `err`.  Cf. sysdep.c:emacs_strerror.
    function emacs_strerror(err) result(text)
        integer(int32), intent(in) :: err
        character(len=:), allocatable :: text

        text = c_string_to_fortran(c_strerror(err))
    end function emacs_strerror

    ! ------------------------------------------------------------------
    ! Processes.  Like the file calls, each returns 0 or the errno value.
    ! ------------------------------------------------------------------

    !> Fill `argv` (which must be a target) from `args`.
    subroutine make_argv(args, argv)
        type(sys_string_type), intent(in) :: args(:)
        type(c_argv_type), intent(out), target :: argv
        integer :: k, j, off, n

        allocate (argv%buf(sum([(len(args(k)%s) + 1, k=1, size(args))])))
        allocate (argv%ptrs(size(args) + 1))
        off = 1

        do k = 1, size(args)
            n = len(args(k)%s)

            do j = 1, n
                argv%buf(off + j - 1) = args(k)%s(j:j)
            end do

            argv%buf(off + n) = c_null_char
            argv%ptrs(k) = c_loc(argv%buf(off))
            off = off + n + 1
        end do

        argv%ptrs(size(args) + 1) = c_null_ptr
    end subroutine make_argv

    !> Start the program `path` (a file name, found by the caller) with
    !> arguments `args` (args(1) is its name), standard input from file
    !> `infile`, standard output and error to `out_fd` (or /dev/null if it
    !> is negative), in directory `dir` (unless empty), with the editor's
    !> environment.  `pid` is the child.  The caller checks that `path`,
    !> `infile` and `dir` can be used first; if the child still fails to
    !> start, it exits with status 127.  Port of callproc.c:emacs_spawn
    !> (the fork path, without a pty).
    integer(int32) function emacs_spawn(path, args, infile, out_fd, dir, pid) result(err)
        character(len=*), intent(in) :: path, infile, dir
        type(sys_string_type), intent(in) :: args(:)
        integer(int32), intent(in) :: out_fd
        integer(int32), intent(out) :: pid
        type(c_argv_type), target :: argv
        character(len=:), allocatable :: c_file, c_infile, c_null, c_dir
        integer(c_int) :: fd, r

        ! Everything the child needs is made before forking: between fork
        ! and exec it only makes system calls.
        call make_argv(args, argv)
        c_file = c_path(path)
        c_infile = c_path(infile)
        c_null = c_path("/dev/null")
        c_dir = c_path(dir)
        err = 0
        pid = c_fork()

        if (pid < 0) then
            err = errno()
            return
        end if

        if (pid /= 0) return

        ! The child.
        fd = c_open(c_infile, ior(O_RDONLY, O_CLOEXEC))
        if (fd < 0) call c_exit(127)
        r = c_dup2(fd, 0)

        if (out_fd >= 0) then
            r = c_dup2(out_fd, 1)
            r = c_dup2(out_fd, 2)
        else
            fd = c_open(c_null, ior(O_WRONLY, O_CLOEXEC))
            if (fd < 0) call c_exit(127)
            r = c_dup2(fd, 1)
            r = c_dup2(fd, 2)
        end if

        if (len(dir) > 0) then
            if (c_chdir(c_dir) /= 0) call c_exit(127)
        end if

        r = c_execv(c_file, argv%ptrs)
        call c_exit(127)
    end function emacs_spawn

    !> Wait for child `pid` to exit.  `code` is its exit status, or -1 if
    !> it was killed by signal `sig` (0 otherwise).  Port of
    !> sysdep.c:wait_for_termination (get_child_status, blocking).
    integer(int32) function wait_for_termination(pid, code, sig) result(err)
        integer(int32), intent(in) :: pid
        integer(int32), intent(out) :: code, sig
        integer(c_int) :: wstatus
        integer(c_int32_t) :: r

        code = -1
        sig = 0
        err = 0

        do
            r = c_waitpid(pid, wstatus, 0)
            if (r == pid) exit
            err = errno()
            if (err /= EINTR) return
            err = 0
        end do

        ! bits/waitstatus.h: WIFEXITED, WEXITSTATUS, WTERMSIG.
        if (iand(wstatus, 127) == 0) then
            code = iand(ishft(wstatus, -8), 255)
        else
            sig = iand(wstatus, 127)
        end if
    end function wait_for_termination

    !> A pipe: `fds(1)` reads what is written to `fds(2)`; both are
    !> close-on-exec.  Port of sysdep.c:emacs_pipe.
    integer(int32) function emacs_pipe(fds) result(err)
        integer(int32), intent(out) :: fds(2)
        integer(c_int) :: cfds(2)

        err = 0
        fds = -1

        if (c_pipe2(cfds, O_CLOEXEC) /= 0) then
            err = errno()
        else
            fds = cfds
        end if
    end function emacs_pipe

    !> The description of signal `sig` ("Killed"), as strsignal gives it.
    function emacs_strsignal(sig) result(text)
        integer(int32), intent(in) :: sig
        character(len=:), allocatable :: text

        text = c_string_to_fortran(c_strsignal(sig))
        if (len(text) == 0) text = "unknown"
    end function emacs_strsignal

    !> The target of symbolic link `path`.  Cf. sysdep.c:emacs_readlinkat.
    integer(int32) function emacs_readlink(path, target) result(err)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: target
        character(kind=c_char), allocatable :: buf(:)
        integer(c_long) :: n
        integer(c_size_t) :: size
        integer :: k

        err = 0
        target = ""
        size = 256

        do
            allocate (buf(size))
            n = c_readlink(c_path(path), buf, size)

            if (n < 0) then
                err = errno()
                return
            end if

            if (n < size) exit
            ! Truncated: try again with more room.
            deallocate (buf)
            size = 2*size
        end do

        target = repeat(" ", int(n))

        do k = 1, int(n)
            target(k:k) = buf(k)
        end do
    end function emacs_readlink

    !> Replace this process with the program `path`, given arguments
    !> `args` (args(1) is its name) and the same environment.  Returns
    !> only on failure.
    integer(int32) function emacs_execv(path, args) result(err)
        character(len=*), intent(in) :: path
        type(sys_string_type), intent(in) :: args(:)
        type(c_argv_type), target :: argv
        integer(c_int) :: r

        call make_argv(args, argv)
        r = c_execv(c_path(path), argv%ptrs)
        err = errno()
    end function emacs_execv

    !> Create a new file from `template`, whose last six characters are
    !> "XXXXXX" (replaced to make the name unique); `fd` is open for
    !> writing, close-on-exec.  Cf. sysdep.c's use of mkostemp.
    integer(int32) function emacs_mkostemp(template, fd) result(err)
        ! The name is filled in, so its length is the caller's.
        ! allow(assumed-size-character-intent)
        character(len=*), intent(inout) :: template
        integer(int32), intent(out) :: fd
        character(len=len(template) + 1) :: buf

        err = 0
        buf = c_path(template)
        fd = c_mkostemp(buf, O_CLOEXEC)

        if (fd < 0) then
            err = errno()
        else
            template = buf(1:len(template))
        end if
    end function emacs_mkostemp

    !> The names of the entries of directory `dir`, "." and ".." included,
    !> in the order the file system gives them.  Cf. dired.c's use of
    !> opendir and readdir.
    integer(int32) function emacs_directory_entries(dir, names) result(err)
        character(len=*), intent(in) :: dir
        type(sys_string_type), allocatable, intent(out) :: names(:)
        ! struct dirent: d_ino (8), d_off (8), d_reclen (2), d_type (1), then
        ! d_name (256).
        integer, parameter :: name_offset = 19, dirent_size = 280
        type(sys_string_type), allocatable :: grown(:)
        character(kind=c_char), pointer :: entry(:)
        type(c_ptr) :: dirp, p
        integer(c_int) :: r
        integer :: n, k, e

        err = 0
        allocate (names(0))
        dirp = c_opendir(c_path(dir))

        if (.not. c_associated(dirp)) then
            err = errno()
            return
        end if

        n = 0

        do
            p = c_readdir(dirp)
            if (.not. c_associated(p)) exit
            call c_f_pointer(p, entry, [dirent_size])
            e = name_offset

            do while (entry(e + 1) /= c_null_char)
                e = e + 1
            end do

            if (n == size(names)) then
                allocate (grown(max(16, 2*n)))
                grown(1:n) = names
                call move_alloc(grown, names)
            end if

            n = n + 1
            names(n)%s = repeat(" ", e - name_offset)

            do k = 1, e - name_offset
                names(n)%s(k:k) = entry(name_offset + k)
            end do
        end do

        r = c_closedir(dirp)
        names = names(1:n)
    end function emacs_directory_entries

    !> symlink(2): make `link` a symbolic link to `target`.
    integer(int32) function sys_symlink(target, link) result(err)
        character(len=*), intent(in) :: target, link

        err = 0
        if (c_symlink(c_path(target), c_path(link)) /= 0) err = errno()
    end function sys_symlink

    !> The local time of `seconds` since the epoch: year, month (1-12),
    !> day, hour, minute and second.
    function emacs_localtime(seconds) result(parts)
        integer(int64), intent(in) :: seconds
        integer :: parts(6)
        type(tm_type) :: tm
        type(c_ptr) :: p

        p = c_localtime_r(seconds, tm)
        parts = [tm%tm_year + 1900, tm%tm_mon + 1, tm%tm_mday, tm%tm_hour, tm%tm_min, tm%tm_sec]
    end function emacs_localtime
end module sysdep_fm
