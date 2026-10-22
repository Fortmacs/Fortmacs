! config_fm.f90 -- where the configuration lives, and building it.
!
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

!> The user's configuration is a Fortran module, `user_config`, in
!> init.f90.  `fortmacs --recompile` compiles it with gfortran together
!> with a small main program against the installed library
!> (PREFIX/lib/libfortmacs.a and the module files in PREFIX/include) into a
!> binary of its own, and an installed fortmacs runs that binary instead
!> of itself (similar to Guix: the configuration is part of the build).
!>
!> The files follow the XDG base directory specification:
!>
!> - init file: $XDG_CONFIG_HOME/fortmacs/init.f90 (~/.config/fortmacs/)
!> - the built binary: $XDG_STATE_HOME/fortmacs/fortmacs (~/.local/state/fortmacs/)
!> - build scratch: $XDG_CACHE_HOME/fortmacs/ (~/.cache/fortmacs/)
!>
!> Not a port: upstream loads its init file at startup instead.
module config_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_string, make_list, t_value, fixnump, xfixnum, xstring
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, current_buffer
    use editfns_fm, only: buffer_string
    use env_fm, only: getenv
    use fileio_fm, only: file_name_as_directory, file_name_directory, directory_file_name, &
                         file_name_absolute_p, get_homedir, file_exists_p, file_readable_p, &
                         file_executable_p, file_newer_than_file_p, rename_file, report_file_errno, &
                         delete_file_internal
    use files_fm, only: make_directory
    use callproc_fm, only: call_process
    use sysdep_fm, only: emacs_readlink, ENOENT
    implicit none(type, external)
    private

    public :: config_directory, state_directory, cache_directory, init_file_name, user_build_name
    public :: running_executable, install_prefix, driver_source, recompile

    !> The compiler `--recompile` runs.
    character(len=*), parameter :: compiler = "gfortran"

contains

    !> $VARIABLE/fortmacs/ if VARIABLE is set to an absolute directory,
    !> else ~/FALLBACK/fortmacs/ (the XDG base directory rule).
    function xdg_directory(variable, fallback) result(dir)
        character(len=*), intent(in) :: variable, fallback
        character(len=:), allocatable :: dir
        character(len=:), allocatable :: value
        logical :: found

        call getenv(variable, value, found)
        if (found .and. len(value) > 0) then
            if (file_name_absolute_p(value)) then
                dir = file_name_as_directory(value)//"fortmacs/"
                return
            end if
        end if
        dir = file_name_as_directory(get_homedir())//fallback//"/fortmacs/"
    end function xdg_directory

    !> The directory of init.f90.
    function config_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = xdg_directory("XDG_CONFIG_HOME", ".config")
    end function config_directory

    !> The directory of the built binary.
    function state_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = xdg_directory("XDG_STATE_HOME", ".local/state")
    end function state_directory

    !> The directory `--recompile` builds in.
    function cache_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = xdg_directory("XDG_CACHE_HOME", ".cache")
    end function cache_directory

    !> The user's init file.
    function init_file_name() result(name)
        character(len=:), allocatable :: name

        name = config_directory()//"init.f90"
    end function init_file_name

    !> The binary `--recompile` makes.
    function user_build_name() result(name)
        character(len=:), allocatable :: name

        name = state_directory()//"fortmacs"
    end function user_build_name

    !> The file name of the running program ("" if unknown).
    function running_executable() result(name)
        character(len=:), allocatable :: name

        if (emacs_readlink("/proc/self/exe", name) /= 0) name = ""
    end function running_executable

    !> The installation `executable` belongs to: PREFIX when it is
    !> PREFIX/bin/fortmacs and PREFIX/lib/libfortmacs.a exists (`found`).
    subroutine install_prefix(executable, prefix, found)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: found
        character(len=:), allocatable :: bin

        found = .false.
        prefix = ""
        bin = directory_file_name(file_name_directory(executable))
        if (len(bin) < 4) return
        if (bin(len(bin) - 3:) /= "/bin") return
        prefix = file_name_directory(bin)
        found = file_exists_p(prefix//"lib/libfortmacs.a")
    end subroutine install_prefix

    !> The main program of a user build: `run` with the user's
    !> `configure`, knowing the installation it was built against.
    function driver_source(prefix) result(text)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: text
        character(len=*), parameter :: nl = new_line("a")

        text = "! Written by fortmacs --recompile."//nl// &
               "program fortmacs_user"//nl// &
               "    use emacs_fm, only: run"//nl// &
               "    use user_config, only: configure"//nl// &
               "    implicit none(type, external)"//nl// &
               nl// &
               "    integer :: exit_code"//nl// &
               nl// &
               "    call run(configure, exit_code, "//fortran_string(prefix)//")"//nl// &
               "    if (exit_code /= 0) stop exit_code, quiet = .true."//nl// &
               "end program fortmacs_user"//nl
    end function driver_source

    !> `s` as a Fortran string literal, split over continuation lines so
    !> that no line gets too long.
    function fortran_string(s) result(literal)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: literal
        character(len=:), allocatable :: quoted
        integer :: k, n

        quoted = ""
        do k = 1, len(s)
            if (s(k:k) == '"') then
                quoted = quoted//'""'
            else
                quoted = quoted//s(k:k)
            end if
        end do
        literal = '"'
        n = 0
        do k = 1, len(quoted)
            literal = literal//quoted(k:k)
            n = n + 1
            ! Break between characters, never inside a doubled quote.
            if (n >= 60 .and. k < len(quoted) .and. quoted(k:k) /= '"') then
                literal = literal//'" // &'//new_line("a")//'        "'
                n = 0
            end if
        end do
        literal = literal//'"'
    end function fortran_string

    !> Build the init file against the installation at `prefix` into
    !> `user_build_name()`.  `log` gets the compiler's output.  Errors: no
    !> init file, no installed library, or the compilation failed.
    subroutine recompile(prefix, log, status)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable, intent(out) :: log
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: init, cache, built, driver, library
        type(value_type) :: args(14), result
        type(handle_type) :: old, buf
        type(status_type) :: ignore
        integer :: unit, ios

        log = ""
        init = init_file_name()
        if (.not. file_readable_p(init)) then
            call report_file_errno("Opening init file", init, ENOENT, status)
            return
        end if
        library = prefix//"lib/libfortmacs.a"
        if (.not. file_exists_p(library)) then
            call set_error(status, FM_GENERIC_ERROR, "Can't find "//library// &
                           "; install Fortmacs with `fpm install`")
            return
        end if
        cache = cache_directory()
        call make_directory(cache, status, parents=.true.)
        if (.not. status%ok()) return
        call make_directory(state_directory(), status, parents=.true.)
        if (.not. status%ok()) return

        driver = cache//"main.f90"
        open (newunit=unit, file=driver, status="replace", action="write", access="stream", &
              form="unformatted", iostat=ios)
        if (ios == 0) write (unit, iostat=ios) driver_source(prefix)
        if (ios == 0) close (unit, iostat=ios)
        if (ios /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Can't write "//driver)
            return
        end if

        ! gfortran -O2 -J CACHE -I PREFIX/include INIT DRIVER
        !     -L PREFIX/lib -lfortmacs -o BUILT.new
        built = user_build_name()
        args(1) = make_string("-O2")
        args(2) = make_string("-J")
        args(3) = make_string(cache)
        args(4) = make_string("-I")
        args(5) = make_string(prefix//"include")
        args(6) = make_string("-o")
        args(7) = make_string(built//".new")
        args(8) = make_string(init)
        args(9) = make_string(driver)
        args(10) = make_string("-L")
        args(11) = make_string(prefix//"lib")
        args(12) = make_string("-lfortmacs")
        args(13) = make_string("-fdiagnostics-plain-output")
        args(14) = make_string("-fno-diagnostics-color")

        ! The compiler's output goes to a buffer, made in the cache
        ! directory so the compiler runs there.
        old = current_buffer()
        call get_buffer_create(" *fortmacs-recompile*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        buffers(cur)%directory = cache
        call call_process(compiler, result, status, destination=t_value(), args=make_list(args))
        log = buffer_string()
        call kill_buffer(buf, ignore)
        if (old%index /= 0) call set_buffer(old, ignore)
        if (.not. status%ok()) return

        if (.not. fixnump(result)) then
            call set_error(status, FM_GENERIC_ERROR, compiler//" was killed: "//xstring(result))
        else if (xfixnum(result) /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Compiling "//init//" failed")
        end if
        if (.not. status%ok()) then
            if (file_exists_p(built//".new")) call delete_file_internal(built//".new", ignore)
            return
        end if
        call rename_file(built//".new", built, status)
    end subroutine recompile

end module config_fm
