! callproc_mod.f90 -- synchronous subprocesses and their environment.
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

!> Running a program and waiting for it: `execute_process' and
!> `execute_process_region'.  The program's output (standard output and
!> error together) goes into a buffer or is discarded; the result is its
!> exit status, or a description of the signal that killed it.
!>
!>     call execute_process("ls", result, status, destination=value_type(.true.), &
!>                       args=list(value_type(text="-l")))
!>
!> The environment subprocesses inherit, `process_environment', is kept
!> here: filled from the editor's environment at startup
!> (`set_initial_environment', which also keeps the startup copy,
!> `initial_environment'), read with `getenv_internal', changed with
!> `setenv_internal' (env_mod's `getenv' and `setenv' use them), and
!> handed to each child by `make_environment_block'.  Both are reached
!> only through these procedures; their element type stays internal.
!>
!> The procedures that start programs are in the submodule
!> callproc_execute_mod, as they use fileio_mod, which reads the
!> environment through env_mod.
!>
!> Standard error can go to a file of its own (DESTINATION's
!> (REAL-DESTINATION ERROR-FILE) form, as `error_file').  Not ported yet:
!> DESTINATION 0 (not waiting), `(:file FILE)', an absent ERROR-FILE
!> (discarding standard error), DISPLAY,
!> quitting (killing the process on C-g), coding systems (output is
!> decoded as UTF-8), `exec_path' (PATH is searched), `process_environment'
!> and `initial_environment' as variables for C-h v and the configuration
!> (they are reached through `getenv' and `setenv'), `getenv_internal''s
!> ENV argument, the frame's `display' parameter for DISPLAY (no GUI
!> frames yet), remote directories, executable suffixes.
module callproc_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use value_mod, only: value_type
    use data_mod, only: string_setting_type
    use sysdep_mod, only: sys_string_type, fortmacs_environment
    implicit none(type, external)
    private

    public :: execute_process, execute_process_region, get_current_directory, locate_program, init_callproc
    public :: getenv_internal, setenv_internal, set_initial_environment, make_environment_block

    !> File name to load inferior shells from.
    !> Initialized from the SHELL environment variable, or to a system-dependent
    !> default if SHELL is unset.  See Info node `(elisp)Security Considerations'.
    !> Port of callproc.c shell-file-name.
    type(string_setting_type), public :: shell_file_name = string_setting_type(name="shell_file_name")

    ! A string of any length: an entry of an environment list.  Not a port.
    type :: string_type
        character(len=:), allocatable :: text
    end type string_type

    ! List of overridden environment variables for subprocesses to inherit.
    ! Each element should be a string of the form ENVVARNAME=VALUE.
    ! To remove an environment variable, an element is only its name,
    ! without "=VALUE".  If multiple entries define the same variable, the
    ! first one always takes precedence.  Unallocated until
    ! `set_initial_environment' fills it.  Port of callproc.c
    ! process-environment.
    type(string_type), allocatable :: process_environment(:)

    ! List of environment variables inherited from the parent process.
    ! Each element should be a string of the form ENVVARNAME=VALUE.  Port
    ! of callproc.c initial-environment.
    type(string_type), allocatable :: initial_environment(:)

    interface
        !> Call PROGRAM synchronously in separate process.
        !> The remaining arguments are optional.
        !>
        !> The program's input comes from file INFILE (absent means
        !> `null_device').  If INFILE is a relative path, it will be looked for
        !> relative to the directory where the process is run (see below).  If
        !> you want to make the input come from a buffer, use
        !> `execute_process_region' instead.
        !>
        !> DESTINATION specifies how to handle program's output.
        !> ("Output" here means both standard output and standard error
        !> output.)
        !> If DESTINATION is a buffer or the name of a buffer, or t (which
        !> stands for the current buffer), it means insert output in that buffer
        !> before point.  If DESTINATION is absent, it means discard
        !> output.
        !>
        !> ARGS, a list of strings, are passed as command arguments to PROGRAM.
        !>
        !> ERROR_FILE, if present, is a file to write the standard error output
        !> to (DESTINATION's (REAL-DESTINATION ERROR-FILE) form; the file is
        !> created or truncated); otherwise standard error goes where standard
        !> output goes.
        !>
        !> If PROGRAM is not an absolute file name, `execute_process' will look
        !> for PROGRAM in PATH.
        !>
        !> If executable PROGRAM can't be found as an executable,
        !> a `file_missing' error condition occurs in `execute_process'.  `execute_process' reports errors in
        !> execution of the program only through its return and output.
        !>
        !> It waits for PROGRAM to terminate and RESULT is a numeric exit
        !> status or a signal description string.
        !>
        !> The process runs in `default_directory'.  Port of
        !> callproc.c:Fcall_process and execute_process.  (In
        !> callproc_execute_mod.)
        module subroutine execute_process(program, result, status, infile, destination, args, error_file, environment)
            implicit none(type, external)
            character(len=*), intent(in) :: program
            type(value_type), intent(out) :: result
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: infile, error_file
            type(value_type), intent(in), optional :: destination, args
            !> "NAME=VALUE" strings put first in the program's environment, as
            !> binding `process_environment' to a list starting with them does
            !> (a "NAME" without "=" removes the variable).
            type(value_type), intent(in), optional :: environment
        end subroutine execute_process

        !> The directory a subprocess runs in: the current buffer's
        !> `default_directory' (or the directory the editor started in),
        !> expanded (it may start with `~'), which must be accessible.  Port of
        !> callproc.c:get_current_directory (as a directory name, with the
        !> trailing slash).  (In callproc_execute_mod.)
        module subroutine get_current_directory(dir, status)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: dir
            type(status_type), intent(out) :: status
        end subroutine get_current_directory

        !> The executable file PROGRAM names: itself if it has a slash (relative
        !> to `dir'), else the first one in a directory of PATH (an empty entry
        !> means `dir').  `path' is "" if there is none.  Port of the part of
        !> lread.c:openp that execute_process uses (`exec_path' is PATH here).
        !> (In callproc_execute_mod.)
        module subroutine locate_program(program, dir, path)
            implicit none(type, external)
            character(len=*), intent(in) :: program, dir
            character(len=:), allocatable, intent(out) :: path
        end subroutine locate_program

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
        !> absent means discard it.  DESTINATION can be a string which
        !> is the name of a buffer.
        !> ARGS, a list of strings, are passed to PROGRAM at startup as
        !> command-line arguments.
        !>
        !> ERROR_FILE is as for `execute_process'.
        !>
        !> If PROGRAM is not an absolute file name, `execute_process_region' will
        !> look for PROGRAM in PATH.
        !>
        !> It waits for PROGRAM to terminate and RESULT is a numeric exit
        !> status or a signal description string.  Port of
        !> callproc.c:Fcall_process_region.  (In callproc_execute_mod.)
        module subroutine execute_process_region(program, result, status, start, end, delete, destination, args, &
                                                 error_file)
            implicit none(type, external)
            character(len=*), intent(in) :: program
            character(len=*), intent(in), optional :: error_file
            type(value_type), intent(out) :: result
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: start, end
            logical, intent(in), optional :: delete
            type(value_type), intent(in), optional :: destination, args
        end subroutine execute_process_region
    end interface

contains

    !> Set `shell_file_name' from SHELL (/bin/sh if unset).  Port of
    !> callproc.c:init_callproc (the part ported).
    subroutine init_callproc()
        character(len=:), allocatable :: sh
        logical :: found

        call getenv_internal("SHELL", sh, found)
        if (.not. found) sh = "/bin/sh"
        shell_file_name = sh
    end subroutine init_callproc

    !> Fill `process_environment' from the editor's environment (the last
    !> variable first, as upstream conses them) and keep a copy of it as
    !> `initial_environment'.  Called at startup, before anything reads
    !> the environment (the first `getenv_internal', `setenv_internal' or
    !> `make_environment_block' does it).  Port of
    !> callproc.c:set_initial_environment.
    subroutine set_initial_environment()
        type(sys_string_type), allocatable :: environ(:)
        integer :: n, k

        environ = fortmacs_environment()
        n = size(environ)
        allocate (process_environment(n))

        do k = 1, n
            process_environment(k)%text = environ(n + 1 - k)%s
        end do

        ! A copy: `initial_environment' stays as it was when
        ! `process_environment' changes.
        initial_environment = process_environment
    end subroutine set_initial_environment

    !> Get the value of environment variable VARIABLE.
    !> `found' is false if VARIABLE is undefined in the environment (and
    !> `value' is then "").  Otherwise, `value' is a string.
    !>
    !> This function searches `process_environment' for VARIABLE.  If it
    !> is not there and VARIABLE is "DISPLAY", it searches
    !> `initial_environment'.  Port of callproc.c:Fgetenv_internal (no
    !> ENV) and getenv_internal.
    subroutine getenv_internal(variable, value, found)
        character(len=*), intent(in) :: variable
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found

        if (.not. allocated(process_environment)) call set_initial_environment()
        ! Try to find VAR in Vprocess_environment first.
        call getenv_internal_1(variable, value, found, process_environment)

        if (found) then
            found = allocated(value)
            if (.not. found) value = ""
            return
        end if

        ! For DISPLAY try to get the value from the initial environment
        ! (no frame has a `display' parameter on a terminal).
        if (variable == "DISPLAY") then
            call getenv_internal_1(variable, value, found, initial_environment)
            if (found) found = allocated(value)
        end if

        if (.not. found) value = ""
    end subroutine getenv_internal

    ! Search ENV for VARIABLE: `found' is true if it has an entry, with
    ! `value' its value, or unallocated for a lone variable name (the
    ! variable is removed).  Port of callproc.c:getenv_internal_1.
    subroutine getenv_internal_1(variable, value, found, env)
        character(len=*), intent(in) :: variable
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        type(string_type), intent(in) :: env(:)
        integer :: k, n

        n = len(variable)
        found = .false.

        do k = 1, size(env)
            if (len(env(k)%text) < n) cycle
            if (env(k)%text(1:n) /= variable) cycle

            if (len(env(k)%text) == n) then
                ! Lone variable names in Vprocess_environment mean that
                ! variable should be removed from the environment.
                found = .true.
                return
            end if

            if (env(k)%text(n + 1:n + 1) == "=") then
                value = env(k)%text(n + 2:)
                found = .true.
                return
            end if
        end do
    end subroutine getenv_internal_1

    !> Set VARIABLE to VALUE in `process_environment', adding an entry
    !> that only names VARIABLE if VALUE is absent and KEEP_EMPTY (the
    !> variable is then removed for subprocesses); without KEEP_EMPTY an
    !> absent VALUE deletes VARIABLE's entry.  An existing entry is changed
    !> in place; a new one goes first.  Port of env.el:setenv-internal (on
    !> `process_environment').
    subroutine setenv_internal(variable, keep_empty, value)
        character(len=*), intent(in) :: variable
        logical, intent(in) :: keep_empty
        character(len=*), intent(in), optional :: value
        type(string_type) :: entry
        integer :: k

        if (.not. allocated(process_environment)) call set_initial_environment()

        ! Try to find existing entry for VARIABLE in ENV.
        do k = 1, size(process_environment)
            if (.not. is_entry_for(process_environment(k)%text, variable)) cycle

            if (present(value)) then
                process_environment(k)%text = variable//"="//value
            else if (keep_empty) then
                process_environment(k)%text = variable
            else if (size(process_environment) == 1) then
                deallocate (process_environment)
                allocate (process_environment(0))
            else
                process_environment = [process_environment(:k - 1), process_environment(k + 1:)]
            end if

            return
        end do

        if (.not. (present(value) .or. keep_empty)) return
        entry%text = variable
        if (present(value)) entry%text = variable//"="//value
        process_environment = [entry, process_environment]
    end subroutine setenv_internal

    ! Whether environment entry ENTRY is for VARIABLE: "VARIABLE=..." or
    ! "VARIABLE" alone (setenv-internal's pattern).  Not a port.
    pure logical function is_entry_for(entry, variable)
        character(len=*), intent(in) :: entry, variable
        integer :: n

        n = len(variable)
        is_entry_for = .false.
        if (len(entry) < n) return
        if (entry(1:n) /= variable) return
        is_entry_for = len(entry) == n

        if (len(entry) > n) then
            is_entry_for = entry(n + 1:n + 1) == "="
        end if
    end function is_entry_for

    !> The environment of a new subprocess running in `current_dir' (a
    !> directory name): PWD set to `current_dir' (without trailing
    !> slashes) if PWD is set, DISPLAY from `initial_environment' unless
    !> `process_environment' has it, then `process_environment' (the first
    !> definition of a variable wins; entries that only name a variable
    !> remove it).  `overrides', "NAME=VALUE" or "NAME" entries, are
    !> consulted first, as when `process_environment' is bound to a list
    !> starting with them.  Port of callproc.c:make_environment_block and
    !> add_env.
    subroutine make_environment_block(current_dir, block, overrides)
        character(len=*), intent(in) :: current_dir
        type(sys_string_type), allocatable, intent(out) :: block(:)
        type(sys_string_type), intent(in), optional :: overrides(:)
        type(sys_string_type), allocatable :: kept(:)
        type(string_type), allocatable :: env(:)
        character(len=:), allocatable :: pwd_var, display, value
        logical :: has_display, found
        integer :: i, k, n

        if (.not. allocated(process_environment)) call set_initial_environment()

        if (present(overrides)) then
            allocate (env(size(overrides) + size(process_environment)))

            do k = 1, size(overrides)
                env(k)%text = overrides(k)%s
            end do

            env(size(overrides) + 1:) = process_environment
        else
            env = process_environment
        end if

        ! Strip trailing slashes for PWD, but leave "/" and "//" alone.
        i = len(current_dir)

        do while (i > 2)
            if (current_dir(i:i) /= "/") exit
            i = i - 1
        end do

        pwd_var = "PWD="//current_dir(1:i)
        has_display = .false.

        do k = 1, size(env)
            ! DISPLAY is specified in process-environment.
            if (is_entry_for(env(k)%text, "DISPLAY")) has_display = .true.
        end do

        display = ""

        if (.not. has_display) then
            ! If still not found, Look for DISPLAY in Vinitial_environment
            ! (no frame has a `display' parameter on a terminal).
            call getenv_internal_1("DISPLAY", value, found, initial_environment)

            if (found .and. allocated(value)) then
                has_display = .true.
                display = value
            end if
        end if

        allocate (block(size(env) + 2))
        n = 0
        ! If we have a PWD envvar, pass one down, but with corrected value.
        call getenv_internal_1("PWD", value, found, env)

        if (found .and. allocated(value)) then
            n = 1
            block(1)%s = pwd_var
        end if

        if (has_display) call add_env("DISPLAY="//display)

        ! Overrides.
        do k = 1, size(env)
            call add_env(env(k)%text)
        end do

        ! Remove variable names without values.
        allocate (kept(count([(index(block(k)%s, "=") > 0, k=1, n)])))
        i = 0

        do k = 1, n
            if (index(block(k)%s, "=") == 0) cycle
            i = i + 1
            call move_alloc(block(k)%s, kept(i)%s)
        end do

        call move_alloc(kept, block)

    contains

        ! Put STRING in the block unless an earlier entry already defines
        ! its variable (when an env var has multiple definitions, we keep
        ! the definition that comes first in process-environment).  A lone
        ! variable name is kept for now, as a placeholder for a variable
        ! that is not to be included in the environment.  Port of
        ! callproc.c:add_env.
        subroutine add_env(string)
            character(len=*), intent(in) :: string
            integer :: e, j

            e = index(string, "=")

            if (e > 0) then
                do j = 1, n
                    if (is_entry_for(block(j)%s, string(1:e - 1))) return
                end do
            end if

            n = n + 1
            block(n)%s = string
        end subroutine add_env
    end subroutine make_environment_block
end module callproc_mod
