! test_config_fm.f90 -- tests for config_fm: where the configuration
! lives and the parts of --recompile that need no installation (the
! build itself is tested by tools/test-recompile.sh).
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

module test_config_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, get_homedir, write_region, &
                         delete_file_internal, delete_directory_internal
    use files_fm, only: make_directory
    use env_fm, only: getenv
    use sysdep_fm, only: sys_setenv, sys_unsetenv
    use config_fm, only: config_directory, state_directory, cache_directory, init_file_name, &
                         user_build_name, running_executable, install_prefix, driver_source, recompile
    implicit none(type, external)
    private

    public :: collect_config_fm

    character(len=*), parameter :: testdir = "build/test-config"

contains

    subroutine collect_config_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("config-xdg-directories", test_directories), &
                    new_unittest("config-install-prefix", test_install_prefix), &
                    new_unittest("config-driver-source", test_driver_source), &
                    new_unittest("config-recompile-errors", test_recompile_errors) &
                    ]
    end subroutine collect_config_fm


    !> Set (or with `value` absent, unset) an environment variable.
    subroutine setenv(name, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: value
        integer :: err

        if (present(value)) then
            err = sys_setenv(name, value)
        else
            err = sys_unsetenv(name)
        end if
    end subroutine setenv


    !> Restore variable `name` to `old` (unset unless `found`).
    subroutine restore(name, old, found)
        character(len=*), intent(in) :: name, old
        logical, intent(in) :: found

        if (found) then
            call setenv(name, old)
        else
            call setenv(name)
        end if
    end subroutine restore


    subroutine test_directories(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: config, state, cache, home
        logical :: has_config, has_state, has_cache

        call getenv("XDG_CONFIG_HOME", config, has_config)
        call getenv("XDG_STATE_HOME", state, has_state)
        call getenv("XDG_CACHE_HOME", cache, has_cache)
        home = file_name_as_directory(get_homedir())

        body: block
            call setenv("XDG_CONFIG_HOME", "/xdg/config")
            call setenv("XDG_STATE_HOME", "/xdg/state/")
            call setenv("XDG_CACHE_HOME", "/xdg/cache")
            call check(error, config_directory(), "/xdg/config/fortmacs/")
            if (allocated(error)) exit body
            call check(error, state_directory(), "/xdg/state/fortmacs/")
            if (allocated(error)) exit body
            call check(error, cache_directory(), "/xdg/cache/fortmacs/")
            if (allocated(error)) exit body
            call check(error, init_file_name(), "/xdg/config/fortmacs/init.f90")
            if (allocated(error)) exit body
            call check(error, user_build_name(), "/xdg/state/fortmacs/fortmacs")
            if (allocated(error)) exit body
            ! Unset, empty or relative: the defaults under the home directory.
            call setenv("XDG_CONFIG_HOME")
            call setenv("XDG_STATE_HOME", "")
            call setenv("XDG_CACHE_HOME", "relative")
            call check(error, config_directory(), home//".config/fortmacs/")
            if (allocated(error)) exit body
            call check(error, state_directory(), home//".local/state/fortmacs/")
            if (allocated(error)) exit body
            call check(error, cache_directory(), home//".cache/fortmacs/")
        end block body

        call restore("XDG_CONFIG_HOME", config, has_config)
        call restore("XDG_STATE_HOME", state, has_state)
        call restore("XDG_CACHE_HOME", cache, has_cache)
    end subroutine test_directories


    subroutine test_install_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: prefix, root, exe
        logical :: found
        type(status_type) :: st

        exe = running_executable()
        call check(error, index(exe, "/tester", back=.true.) == len(exe) - 6, exe)
        if (allocated(error)) return
        ! The test program is not installed.
        call install_prefix(exe, prefix, found)
        call check(error,.not. found, "the tester is not installed")
        if (allocated(error)) return
        call install_prefix("/opt/fortmacs-test/libexec/fortmacs", prefix, found)
        call check(error,.not. found .and. prefix == "", "not in a bin directory")
        if (allocated(error)) return
        call init_fileio()
        root = file_name_as_directory(emacs_wd)//testdir//"/prefix/"
        call make_directory(root//"lib", st, parents=.true.)
        call install_prefix(root//"bin/fortmacs", prefix, found)
        call check(error,.not. found .and. prefix == root, "no library")

        if (.not. allocated(error)) then
            call write_region(root//"lib/libfortmacs.a", .false., st)
            call install_prefix(root//"bin/fortmacs", prefix, found)
            call check(error, found .and. prefix == root, "installed")
        end if

        call delete_file_internal(root//"lib/libfortmacs.a", st)
        call delete_directory_internal(root//"lib", st)
        call delete_directory_internal(root, st)
    end subroutine test_install_prefix


    subroutine test_driver_source(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text, long
        integer :: b, e

        text = driver_source("/usr/local/")
        call check(error, index(text, "use user_config, only: configure") > 0, "uses user_config")
        if (allocated(error)) return
        call check(error, index(text, 'call run(configure, exit_code, "/usr/local/")') > 0, text)
        if (allocated(error)) return
        ! Quotes are doubled; long prefixes are split over lines that stay
        ! within 132 columns.
        text = driver_source('/a"b/')
        call check(error, index(text, '"/a""b/"') > 0, text)
        if (allocated(error)) return
        long = "/"//repeat("abcdefghi/", 30)
        text = driver_source(long)
        b = 1

        do while (b <= len(text))
            e = index(text(b:), new_line("a"))
            if (e == 0) exit
            call check(error, e - 1 <= 132, "line too long: "//text(b:b + e - 2))
            if (allocated(error)) return
            b = b + e
        end do

        call check(error, index(text, '" // &') > 0, "continued")
    end subroutine test_driver_source


    subroutine test_recompile_errors(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: config, root, log
        logical :: has_config
        type(status_type) :: st

        call init_fileio()
        root = file_name_as_directory(emacs_wd)//testdir//"/xdg/"
        call getenv("XDG_CONFIG_HOME", config, has_config)
        call setenv("XDG_CONFIG_HOME", root)

        body: block
            call recompile("/fortmacs-no-such-prefix/", log, st)
            call check(error, format_error(st), "Opening init file: No such file or directory, "// &
                       root//"fortmacs/init.f90")
            if (allocated(error)) exit body
            call make_directory(root//"fortmacs", st, parents=.true.)
            call write_region(root//"fortmacs/init.f90", .false., st)
            call recompile("/fortmacs-no-such-prefix/", log, st)
            call check(error, format_error(st), "Can't find /fortmacs-no-such-prefix/lib/libfortmacs.a; "// &
                       "install Fortmacs with `fpm install`")
        end block body

        call delete_file_internal(root//"fortmacs/init.f90", st)
        call delete_directory_internal(root//"fortmacs", st)
        call delete_directory_internal(root, st)
        call restore("XDG_CONFIG_HOME", config, has_config)
    end subroutine test_recompile_errors

end module test_config_fm
