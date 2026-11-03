! callproc_execute_mod.f90 -- starting synchronous subprocesses.
! Ported from GNU Emacs 31.1 src/callproc.c (in part: call-process,
! call-process-region and their helpers).
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

!> `execute_process' and `execute_process_region': the part of
!> callproc_mod that starts programs.  A submodule, as it uses
!> fileio_mod, which reads the environment through env_mod and so sits
!> above callproc_mod.
submodule(callproc_mod) callproc_execute_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type
    use value_mod, only: xstring, xhandle, vtag, V_NAME, SYM_T, xsymbol, list_elements, V_INTEGER, V_STRING, V_BUFFER
    use data_mod, only: check_string
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, BEGINNING, &
                          is_buffer_live
    use character_mod, only: str_as_multibyte
    use insdel_mod, only: insert, del_range
    use editfns_mod, only: widen, delete_region, validate_region
    use fileio_mod, only: expand_file_name, file_name_as_directory, is_file_accessible_directory, &
                          is_file_executable, is_file_directory, &
                          report_file_errno, write_region, delete_file_internal, fortmacs_wd
    use files_mod, only: temporary_file_directory
    use sysdep_mod, only: fortmacs_spawn, wait_for_termination, fortmacs_pipe, fortmacs_strsignal, fortmacs_open_read, &
                          fortmacs_mkostemp, fortmacs_read, close_fd, ENOENT
    implicit none(type, external)

contains

    module procedure execute_process
    type(sys_string_type), allocatable :: argv(:), env(:), overrides(:)
    type(value_type), allocatable :: elems(:)
    type(handle_type) :: buffer, old
    character(len=:), allocatable :: dir, input, output, path, errpath
    character(len=65536) :: chunk
    logical :: discard
    integer(int32) :: fds(2), err, fd, pid, code, sig
    integer(int64) :: n, k
    type(status_type) :: rst

    result = value_type()
    call get_current_directory(dir, status)
    if (.not. status%ok()) return
    input = "/dev/null"

    if (present(infile)) then
        call expand_file_name(infile, input, status, dir)
        if (.not. status%ok()) return
        err = fortmacs_open_read(input, fd)

        if (err /= 0) then
            call report_file_errno("Opening process input file", input, err, status)
            return
        end if

        call close_fd(fd)
    end if

    ! The destination: a buffer, or nothing.
    discard = .true.

    if (present(destination)) then
        if (.not. allocated(destination%a)) then
            ! nil: discard the output.
            discard = .true.
        else if (vtag(destination) == V_INTEGER) then
            call set_error(status, FM_GENERIC_ERROR, &
                           "call-process with DESTINATION 0 (not waiting) is not supported yet")
            return
        else if (vtag(destination) == V_NAME) then
            if (xsymbol(destination) == SYM_T) then
                discard = .false.
                buffer = current_buffer()
            else if (allocated(destination%a)) then
                call set_error(status, FM_GENERIC_ERROR, "Invalid destination for execute_process")
                return
            end if
        else if (vtag(destination) == V_BUFFER) then
            discard = .false.
            buffer = xhandle(destination)

            if (.not. is_buffer_live(buffer)) then
                call set_error(status, FM_GENERIC_ERROR, "Selecting deleted buffer")
                return
            end if
        else if (vtag(destination) == V_STRING) then
            discard = .false.
            call get_or_make_buffer(xstring(destination), buffer, status)
            if (.not. status%ok()) return
        else
            call set_error(status, FM_GENERIC_ERROR, "Invalid destination for execute_process")
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

    if (present(environment)) then
        call list_elements(environment, elems)
        allocate (overrides(size(elems)))

        do k = 1, size(elems, kind=int64)
            call check_string(elems(k), status)
            if (.not. status%ok()) return
            overrides(k)%s = xstring(elems(k))
        end do

        call make_environment_block(dir, env, overrides)
    else
        call make_environment_block(dir, env)
    end if

    fds = -1

    if (.not. discard) then
        err = fortmacs_pipe(fds)

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

        err = fortmacs_spawn(path, argv, input, fds(2), dir, pid, errpath, env)
    else
        err = fortmacs_spawn(path, argv, input, fds(2), dir, pid, env=env)
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
            call fortmacs_read(fds(1), chunk, n, rst)
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
        result = value_type(text=fortmacs_strsignal(sig))
    else
        result = value_type(int(code, int64))
    end if
    end procedure execute_process

    module procedure get_current_directory
    character(len=:), allocatable :: curdir, expanded

    curdir = ""

    if (current /= 0) then
        if (allocated(buffers(current)%directory)) curdir = buffers(current)%directory
    end if

    if (len(curdir) == 0 .and. allocated(fortmacs_wd)) curdir = file_name_as_directory(fortmacs_wd)
    if (len(curdir) == 0) curdir = "/"
    call expand_file_name(curdir, expanded, status)
    if (.not. status%ok()) return
    dir = file_name_as_directory(expanded)

    if (.not. is_file_accessible_directory(dir)) then
        call report_file_errno("Setting current directory", curdir, ENOENT, status)
    end if
    end procedure get_current_directory

    module procedure locate_program
    character(len=:), allocatable :: search, entry, candidate
    type(status_type) :: st
    integer :: stat, b, e

    path = ""

    if (index(program, "/") > 0) then
        call expand_file_name(program, candidate, st, dir)
        if (st%ok() .and. is_executable_file(candidate)) path = candidate
        return
    end if

    call getenv_internal("PATH", search, stat)
    if (stat /= GETENV_SET) return
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

        if (st%ok() .and. is_executable_file(candidate)) then
            path = candidate
            return
        end if

        b = e + 1
    end do

contains

    logical function is_executable_file(name)
        character(len=*), intent(in) :: name

        is_executable_file = is_file_executable(name) .and. .not. is_file_directory(name)
    end function is_executable_file
    end procedure locate_program

    !> Insert the program output `bytes' at point, decoded as UTF-8 in a
    !> multibyte buffer.  Not a port.
    subroutine insert_output(bytes, status)
        character(len=*), intent(in) :: bytes
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64) :: nchars

        if (buffers(current)%multibyte) then
            call str_as_multibyte(bytes, text, nchars)
            call insert(text, status)
        else
            call insert(bytes, status)
        end if
    end subroutine insert_output

    module procedure execute_process_region
    character(len=:), allocatable :: tempfile
    integer(int64) :: s, e
    logical :: whole, empty_input
    type(status_type) :: dst

    result = value_type()
    whole = .not. (present(start) .and. present(end))

    if (whole) then
        empty_input = buffers(current)%text%z == BEGINNING
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
            call del_range(BEGINNING, buffers(current)%text%z, status)
        else
            call delete_region(s, e, status)
        end if

        if (.not. status%ok()) then
            if (len(tempfile) > 0) call delete_file_internal(tempfile, dst)
            return
        end if
    end if delete_text

    if (len(tempfile) > 0) then
        call execute_process(program, result, status, tempfile, destination, args, error_file)
        call delete_file_internal(tempfile, dst)
    else
        call execute_process(program, result, status, destination=destination, args=args, error_file=error_file)
    end if
    end procedure execute_process_region

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
        err = fortmacs_mkostemp(tempfile, fd)

        if (err /= 0) then
            call report_file_errno("Failed to open temporary file using pattern", pattern, err, status)
            return
        end if

        call close_fd(fd)
        call write_region(tempfile, .false., status, start, end)
        if (.not. status%ok()) call delete_file_internal(tempfile, dst)
    end subroutine create_temp_file
end submodule callproc_execute_mod
