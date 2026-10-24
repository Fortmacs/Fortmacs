! callproc_fm.f90 -- synchronous subprocesses.
! Ported from GNU Emacs 31.1 src/callproc.c.
!
! Copyright (C) 1985-1988, 1993-1995, 1999-2026 Free Software Foundation,
! Inc.
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

!> Running a program and waiting for it: `call_process' and
!> `call_process_region'.  The program's output (standard output and
!> error together) goes into a buffer or is discarded; the result is its
!> exit status, or a description of the signal that killed it.
!>
!>     call call_process("ls", result, status, destination=t_value(), &
!>                       args=list(make_string("-l")))
!>
!> Standard error can go to a file of its own (DESTINATION's
!> (REAL-DESTINATION ERROR-FILE) form, as `error_file').  Not ported yet:
!> DESTINATION 0 (not waiting), `(:file FILE)', an ERROR-FILE of nil
!> (discarding standard error), DISPLAY,
!> quitting (killing the process on C-g), coding systems (output is
!> decoded as UTF-8), `exec_path' (PATH is searched), `process_environment'
!> (the editor's environment is passed), remote directories, executable
!> suffixes.
module callproc_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, make_fixnum, make_string, fixnump, xfixnum, &
                       stringp, xstring, bufferp, xhandle, vtag, V_SYMBOL, SYM_T, xsymbol, &
                       list_elements
    use data_fm, only: check_string, string_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, BEG, &
                         buffer_live_p
    use character_fm, only: str_as_multibyte
    use insdel_fm, only: insert, del_range
    use editfns_fm, only: point_min, point_max, widen, delete_region, validate_region
    use fileio_fm, only: expand_file_name, file_name_as_directory, file_accessible_directory_p, &
                         file_executable_p, file_directory_p, &
                         report_file_errno, write_region, delete_file_internal, emacs_wd
    use files_fm, only: temporary_file_directory
    use env_fm, only: getenv
    use sysdep_fm, only: sys_string_type, emacs_spawn, wait_for_termination, emacs_pipe, &
                         emacs_strsignal, emacs_open_read, emacs_mkostemp, emacs_read, close_fd, &
                         ENOENT
    implicit none(type, external)
    private

    public :: call_process, call_process_region, get_current_directory, locate_program, init_callproc

    !> File name to load inferior shells from.
    !> Initialized from the SHELL environment variable, or to a system-dependent
    !> default if SHELL is unset.  See Info node `(elisp)Security Considerations'.
    !> Port of callproc.c shell-file-name.
    type(string_setting), public :: shell_file_name = string_setting(name="shell_file_name")

contains

    !> Set `shell_file_name' from SHELL (/bin/sh if unset).  Port of
    !> callproc.c:init_callproc (the part ported).
    subroutine init_callproc()
        character(len=:), allocatable :: sh
        logical :: found
        call getenv("SHELL", sh, found)
        if (.not. found) sh = "/bin/sh"
        shell_file_name = sh
    end subroutine init_callproc

    !> Call PROGRAM synchronously in separate process.
    !> The remaining arguments are optional.
    !>
    !> The program's input comes from file INFILE (absent means
    !> `null_device').  If INFILE is a relative path, it will be looked for
    !> relative to the directory where the process is run (see below).  If
    !> you want to make the input come from a buffer, use
    !> `call_process_region' instead.
    !>
    !> DESTINATION specifies how to handle program's output.
    !> ("Output" here means both standard output and standard error
    !> output.)
    !> If DESTINATION is a buffer or the name of a buffer, or t (which
    !> stands for the current buffer), it means insert output in that buffer
    !> before point.  If DESTINATION is nil (or absent), it means discard
    !> output.
    !>
    !> ARGS, a list of strings, are passed as command arguments to PROGRAM.
    !>
    !> ERROR_FILE, if present, is a file to write the standard error output
    !> to (DESTINATION's (REAL-DESTINATION ERROR-FILE) form; the file is
    !> created or truncated); otherwise standard error goes where standard
    !> output goes.
    !>
    !> If PROGRAM is not an absolute file name, `call_process' will look
    !> for PROGRAM in PATH.
    !>
    !> If executable PROGRAM can't be found as an executable,
    !> `call_process' signals an error.  `call_process' reports errors in
    !> execution of the program only through its return and output.
    !>
    !> It waits for PROGRAM to terminate and RESULT is a numeric exit
    !> status or a signal description string.
    !>
    !> The process runs in `default_directory'.  Port of
    !> callproc.c:Fcall_process and call_process.
    subroutine call_process(program, result, status, infile, destination, args, error_file)
        character(len=*), intent(in) :: program
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: infile, error_file
        type(value_type), intent(in), optional :: destination, args
        type(sys_string_type), allocatable :: argv(:)
        type(value_type), allocatable :: elems(:)
        type(handle_type) :: buffer, old
        character(len=:), allocatable :: dir, input, output, path, errpath
        character(len=65536) :: chunk
        logical :: discard
        integer(int32) :: fds(2), err, fd, pid, code, sig
        integer(int64) :: n, k
        type(status_type) :: rst
        result = nil()
        call get_current_directory(dir, status)
        if (.not. status%ok()) return
        input = "/dev/null"
        if (present(infile)) then
            call expand_file_name(infile, input, status, dir)
            if (.not. status%ok()) return
            err = emacs_open_read(input, fd)
            if (err /= 0) then
                call report_file_errno("Opening process input file", input, err, status)
                return
            end if
            call close_fd(fd)
        end if
        ! The destination: a buffer, or nothing.
        discard = .true.
        if (present(destination)) then
            if (fixnump(destination)) then
                call set_error(status, FM_GENERIC_ERROR, &
                               "call-process with DESTINATION 0 (not waiting) is not supported yet")
                return
            else if (vtag(destination) == V_SYMBOL) then
                if (xsymbol(destination) == SYM_T) then
                    discard = .false.
                    buffer = current_buffer()
                else if (.not. nilp(destination)) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid destination for call_process")
                    return
                end if
            else if (bufferp(destination)) then
                discard = .false.
                buffer = xhandle(destination)
                if (.not. buffer_live_p(buffer)) then
                    call set_error(status, FM_GENERIC_ERROR, "Selecting deleted buffer")
                    return
                end if
            else if (stringp(destination)) then
                discard = .false.
                call get_buffer_create(xstring(destination), buffer, status)
                if (.not. status%ok()) return
            else
                call set_error(status, FM_GENERIC_ERROR, "Invalid destination for call_process")
                return
            end if
        end if
        call locate_program(program, dir, path)
        if (len(path) == 0) then
            call report_file_errno("Searching for program", program, ENOENT, status)
            return
        end if
        ! The argument vector: PROGRAM, then ARGS.
        if (present(args)) then
            call list_elements(args, elems)
        else
            allocate (elems(0))
        end if
        allocate (argv(size(elems) + 1))
        argv(1)%s = program
        do k = 1, size(elems, kind=int64)
            call check_string(elems(k), status)
            if (.not. status%ok()) return
            argv(k + 1)%s = xstring(elems(k))
        end do
        fds = -1
        if (.not. discard) then
            err = emacs_pipe(fds)
            if (err /= 0) then
                call report_file_errno("Creating process pipe", "", err, status)
                return
            end if
        end if
        if (present(error_file)) then
            call expand_file_name(error_file, errpath, status, dir)
            if (.not. status%ok()) then
                if (.not. discard) call close_fd(fds(1))
                if (.not. discard) call close_fd(fds(2))
                return
            end if
            err = emacs_spawn(path, argv, input, fds(2), dir, pid, errpath)
        else
            err = emacs_spawn(path, argv, input, fds(2), dir, pid)
        end if
        if (.not. discard) call close_fd(fds(2))
        if (err /= 0) then
            if (.not. discard) call close_fd(fds(1))
            call report_file_errno("Doing fork", program, err, status)
            return
        end if
        ! Read everything the program writes, then wait for it.
        output = ""
        if (.not. discard) then
            do
                call emacs_read(fds(1), chunk, n, rst)
                if (.not. rst%ok() .or. n <= 0) exit
                output = output//chunk(1:n)
            end do
            call close_fd(fds(1))
        end if
        err = wait_for_termination(pid, code, sig)
        if (len(output) > 0) then
            old = current_buffer()
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call insert_output(output, status)
            call set_buffer(old, rst)
            if (.not. status%ok()) return
        end if
        if (sig /= 0) then
            result = make_string(emacs_strsignal(sig))
        else
            result = make_fixnum(int(code, int64))
        end if
    end subroutine call_process

    !> The directory a subprocess runs in: the current buffer's
    !> `default_directory' (or the directory the editor started in), which
    !> must be accessible.  The name is expanded (`expand_and_dir_to_file':
    !> a `default_directory' of "~/..." works) and kept as a directory
    !> name.  Port of callproc.c:get_current_directory.
    subroutine get_current_directory(dir, status)
        character(len=:), allocatable, intent(out) :: dir
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: curdir
        dir = ""
        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        end if
        if (len(dir) == 0 .and. allocated(emacs_wd)) dir = file_name_as_directory(emacs_wd)
        if (len(dir) == 0) dir = "/"
        curdir = dir
        call expand_file_name(curdir, dir, status)
        if (.not. status%ok()) return
        dir = file_name_as_directory(dir)
        if (.not. file_accessible_directory_p(dir)) then
            call report_file_errno("Setting current directory", curdir, ENOENT, status)
        end if
    end subroutine get_current_directory

    !> The executable file PROGRAM names: itself if it has a slash (relative
    !> to `dir'), else the first one in a directory of PATH (an empty entry
    !> means `dir').  `path' is "" if there is none.  Port of the part of
    !> lread.c:openp that call_process uses (`exec_path' is PATH here).
    subroutine locate_program(program, dir, path)
        character(len=*), intent(in) :: program, dir
        character(len=:), allocatable, intent(out) :: path
        character(len=:), allocatable :: search, entry, candidate
        type(status_type) :: st
        logical :: found
        integer :: b, e
        path = ""
        if (index(program, "/") > 0) then
            call expand_file_name(program, candidate, st, dir)
            if (st%ok() .and. executable_file_p(candidate)) path = candidate
            return
        end if
        call getenv("PATH", search, found)
        if (.not. found) return
        b = 1
        do while (b <= len(search) + 1)
            e = index(search(b:), ":")
            if (e == 0) then
                e = len(search) + 1
            else
                e = b + e - 1
            end if
            entry = search(b:e - 1)
            if (len(entry) == 0) entry = dir
            call expand_file_name(program, candidate, st, file_name_as_directory(entry))
            if (st%ok() .and. executable_file_p(candidate)) then
                path = candidate
                return
            end if
            b = e + 1
        end do
    contains

        logical function executable_file_p(name)
            character(len=*), intent(in) :: name
            executable_file_p = file_executable_p(name) .and. .not. file_directory_p(name)
        end function executable_file_p
    end subroutine locate_program

    !> Insert the program output `bytes' at point, decoded as UTF-8 in a
    !> multibyte buffer.
    subroutine insert_output(bytes, status)
        character(len=*), intent(in) :: bytes
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64) :: nchars
        if (buffers(cur)%multibyte) then
            call str_as_multibyte(bytes, text, nchars)
            call insert(text, status)
        else
            call insert(bytes, status)
        end if
    end subroutine insert_output

    !> Send text from START to END to a synchronous process running
    !> PROGRAM.
    !>
    !> START and END are normally buffer positions specifying the part of
    !> the buffer to send to the process.  If START is absent, that means to
    !> use the entire buffer contents; END is ignored.
    !> The remaining arguments are optional.
    !> Delete the text if DELETE is true.
    !>
    !> Insert output in DESTINATION before point; t means current buffer;
    !> nil (or absent) means discard it.  DESTINATION can be a string which
    !> is the name of a buffer.
    !> ARGS, a list of strings, are passed to PROGRAM at startup as
    !> command-line arguments.
    !>
    !> ERROR_FILE is as for `call_process'.
    !>
    !> If PROGRAM is not an absolute file name, `call_process_region' will
    !> look for PROGRAM in PATH.
    !>
    !> It waits for PROGRAM to terminate and RESULT is a numeric exit
    !> status or a signal description string.  Port of
    !> callproc.c:Fcall_process_region.
    subroutine call_process_region(program, result, status, start, end, delete, destination, args, error_file)
        character(len=*), intent(in) :: program
        character(len=*), intent(in), optional :: error_file
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        logical, intent(in), optional :: delete
        type(value_type), intent(in), optional :: destination, args
        character(len=:), allocatable :: tempfile
        integer(int64) :: s, e
        logical :: whole, empty_input
        type(status_type) :: dst
        result = nil()
        whole = .not. (present(start) .and. present(end))
        if (whole) then
            empty_input = buffers(cur)%text%z == BEG
        else
            s = start
            e = end
            call validate_region(s, e, status)
            if (.not. status%ok()) return
            empty_input = s == e
        end if
        tempfile = ""
        if (.not. empty_input) then
            if (whole) then
                call create_temp_file(tempfile, status)
            else
                call create_temp_file(tempfile, status, s, e)
            end if
            if (.not. status%ok()) return
        end if
        delete_text: if (present(delete)) then
            if (.not. delete) exit delete_text
            if (whole) then
                ! No need to save restrictions since we delete everything
                ! anyway.
                call widen()
                call del_range(BEG, buffers(cur)%text%z, status)
            else
                call delete_region(s, e, status)
            end if
            if (.not. status%ok()) then
                if (len(tempfile) > 0) call delete_file_internal(tempfile, dst)
                return
            end if
        end if delete_text
        if (len(tempfile) > 0) then
            call call_process(program, result, status, tempfile, destination, args, error_file)
            call delete_file_internal(tempfile, dst)
        else
            call call_process(program, result, status, destination=destination, args=args, error_file=error_file)
        end if
    end subroutine call_process_region

    !> Write the text from `start' to `end' (the whole buffer if absent) to
    !> a new temporary file, named `tempfile'.  Port of
    !> callproc.c:create_temp_file.
    subroutine create_temp_file(tempfile, status, start, end)
        character(len=:), allocatable, intent(out) :: tempfile
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        character(len=:), allocatable :: pattern
        integer(int32) :: fd, err
        type(status_type) :: dst
        ! temp-file-name-pattern in temporary-file-directory.
        call expand_file_name("emacsXXXXXX", pattern, status, temporary_file_directory())
        if (.not. status%ok()) return
        tempfile = pattern
        err = emacs_mkostemp(tempfile, fd)
        if (err /= 0) then
            call report_file_errno("Failed to open temporary file using pattern", pattern, err, status)
            return
        end if
        call close_fd(fd)
        call write_region(tempfile, .false., status, start, end)
        if (.not. status%ok()) call delete_file_internal(tempfile, dst)
    end subroutine create_temp_file
end module callproc_fm
