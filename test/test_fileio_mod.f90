! test_fileio_mod.f90 -- tests for fileio_mod.
! Ports GNU Emacs 31.1 test/src/fileio-tests.el in part; the others are
! not ported yet.
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

module test_fileio_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, condition_matches, format_error, FM_FILE_ERROR, &
                         FM_FILE_ALREADY_EXISTS, &
                         FM_FILE_MISSING, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type
    use coding_mod, only: last_coding_system_used
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, &
                          is_buffer_modified, current_buffer
    use insdel_mod, only: insert, after_change_functions
    use editfns_mod, only: point, buffer_string, goto_char
    use sysdep_mod, only: sys_setenv, sys_unsetenv, passwd_name_of_user, passwd_home_of_user
    use fileio_mod, only: file_name_directory, file_name_nondirectory, file_name_as_directory, &
                          is_file_executable, is_file_newer_than_file, &
                          create_symbolic_link, is_file_symlink, &
                          directory_file_name, is_directory_name, is_file_name_absolute, &
                          expand_file_name, get_homedir, init_fileio, emacs_wd, file_exists, &
                          is_file_readable, is_file_writable, is_file_directory, file_modes, &
                          set_file_modes, insert_file_contents, write_region, &
                          is_visited_file_modtime_current, create_directory_internal, &
                          delete_directory_internal, delete_file_internal, rename_file, copy_file, &
                          substitute_in_file_name
    implicit none(type, external)
    private

    public :: collect_fileio_mod

    !> Scratch directory for file tests, relative to the working directory.
    character(len=*), parameter :: testdir = "build/test-fileio"

    !> What the change functions recorded.
    character(len=:), allocatable :: trace

contains

    !> Collect all exported unit tests.
    subroutine collect_fileio_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("directory-file-name", test_directory_file_name), &
                    new_unittest("file-name-as-directory", test_file_name_as_directory), &
                    new_unittest("file-name-parts", test_file_name_parts), &
                    new_unittest("file-name-absolute-p", test_is_file_name_absolute), &
                    new_unittest("expand-file-name", test_expand_file_name), &
                    new_unittest("expand-file-name-tilde", test_expand_tilde), &
                    new_unittest("relative-HOME", test_relative_home), &
                    new_unittest("relative-default-directory", test_relative_default_directory), &
                    new_unittest("expand-file-name-null-bytes", test_null_bytes), &
                    new_unittest("write-and-insert", test_write_and_insert), &
                    new_unittest("raw-bytes-detection", test_raw_bytes), &
                    new_unittest("visit-nonexistent", test_visit_nonexistent), &
                    new_unittest("insert-directory-error", test_insert_directory), &
                    new_unittest("visit-non-empty-buffer", test_visit_non_empty), &
                    new_unittest("modtime", test_modtime), &
                    new_unittest("predicates-and-modes", test_tests), &
                    new_unittest("rename-and-copy", test_rename_copy), &
                    new_unittest("substitute-in-file-name", test_substitute), &
                    new_unittest("insert-file-contents-change-hooks", test_insert_change_hooks) &
                    ]
    end subroutine collect_fileio_mod

    !> Make the scratch directory (empty) and a fresh current buffer.
    subroutine setup(buffer)
        type(handle_type), intent(out) :: buffer
        type(status_type) :: st
        integer :: i

        call make_test_buffer(buffer)

        do i = 1, 6
            call delete_file_internal(testdir//"/f"//achar(iachar("0") + i), st)
        end do

        call delete_file_internal(testdir//"/missing", st)
        call delete_directory_internal(testdir//"/sub", st)
        call create_directory_internal(testdir, st)
    end subroutine setup

    !> A fresh current buffer in the working directory.
    subroutine make_test_buffer(buffer)
        type(handle_type), intent(out) :: buffer
        type(status_type) :: st

        call init_fileio()
        call get_or_make_buffer("fileio-test", buffer, st)
        call set_buffer(buffer, st)
        buffers(current)%directory = file_name_as_directory(emacs_wd)
    end subroutine make_test_buffer

    subroutine teardown(buffer)
        type(handle_type), intent(in) :: buffer
        type(status_type) :: st

        call close_buffer(buffer, st)
    end subroutine teardown

    ! fileio-tests--directory-file-name
    subroutine test_directory_file_name(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, directory_file_name("/") == "/")
        if (allocated(error)) return
        call check(error, directory_file_name("//") == "//" .and. len(directory_file_name("//")) == 2)
        if (allocated(error)) return
        call check(error, len(directory_file_name("///")) == 1)
        if (allocated(error)) return
        call check(error, len(directory_file_name("////")) == 1)
        if (allocated(error)) return
        call check(error, directory_file_name("/abc") == "/abc")
        if (allocated(error)) return
        call check(error, directory_file_name("/abc/") == "/abc" .and. len(directory_file_name("/abc/")) == 4)
        if (allocated(error)) return
        call check(error, len(directory_file_name("/abc//")) == 4)
    end subroutine test_directory_file_name

    ! fileio-tests--file-name-as-directory
    subroutine test_file_name_as_directory(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, file_name_as_directory("") == "./")
        if (allocated(error)) return
        call check(error, file_name_as_directory("/") == "/" .and. len(file_name_as_directory("/")) == 1)
        if (allocated(error)) return
        call check(error, len(file_name_as_directory("//")) == 2)
        if (allocated(error)) return
        call check(error, len(file_name_as_directory("///")) == 3)
        if (allocated(error)) return
        call check(error, len(file_name_as_directory("////")) == 4)
        if (allocated(error)) return
        call check(error, file_name_as_directory("/abc") == "/abc/")
        if (allocated(error)) return
        call check(error, file_name_as_directory("/abc/") == "/abc/" .and. len(file_name_as_directory("/abc/")) == 5)
        if (allocated(error)) return
        call check(error, len(file_name_as_directory("/abc//")) == 6)
    end subroutine test_file_name_as_directory

    subroutine test_file_name_parts(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, file_name_directory("/usr/lib/libc.so") == "/usr/lib/")
        if (allocated(error)) return
        call check(error, len(file_name_directory("libc.so")) == 0, "no directory: nil")
        if (allocated(error)) return
        call check(error, file_name_nondirectory("/usr/lib/libc.so") == "libc.so")
        if (allocated(error)) return
        call check(error, len(file_name_nondirectory("/usr/lib/")) == 0)
        if (allocated(error)) return
        call check(error, file_name_nondirectory("libc.so") == "libc.so")
        if (allocated(error)) return
        call check(error, is_directory_name("/a/") .and. .not. is_directory_name("/a") .and. &
                   .not. is_directory_name(""))
    end subroutine test_file_name_parts

    ! fileio-tests--file-name-absolute-p
    subroutine test_is_file_name_absolute(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=6), parameter :: suffixes(7) = [character(len=6) :: &
                                                      "", "/", "//", "/foo", "/foo/", "/foo//", "/foo/b"]
        character(len=:), allocatable :: me
        integer :: i
        logical :: found

        call passwd_name_of_user(me, found)

        do i = 1, size(suffixes)
            if (len_trim(suffixes(i)) > 0) then
                call check(error, is_file_name_absolute(trim(suffixes(i))), trim(suffixes(i)))
                if (allocated(error)) return
            end if

            call check(error, is_file_name_absolute("~"//trim(suffixes(i))), "~"//trim(suffixes(i)))
            if (allocated(error)) return

            if (found) then
                call check(error, is_file_name_absolute("~"//me//trim(suffixes(i))), "~"//me//trim(suffixes(i)))
                if (allocated(error)) return
            end if

            call check(error,.not. is_file_name_absolute("~nosuchuser"//trim(suffixes(i))), &
                       "~nosuchuser"//trim(suffixes(i)))
            if (allocated(error)) return
        end do

        call check(error,.not. is_file_name_absolute("foo") .and. .not. is_file_name_absolute(""))
    end subroutine test_is_file_name_absolute

    !> expand(name, dir) == want.
    subroutine check_expand(error, name, dir, want)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, dir, want
        character(len=:), allocatable :: got
        type(status_type) :: st

        call expand_file_name(name, got, st, dir)
        call check(error, st%ok(), "expand "//name)
        if (allocated(error)) return
        call check(error, got == want .and. len(got) == len(want), &
                   "expand-file-name """//name//""" """//dir//""" gave """//got//""", want """//want//"""")
    end subroutine check_expand

    subroutine test_expand_file_name(error)
        type(error_type), allocatable, intent(out) :: error

        call check_expand(error, "foo", "/x/y/", "/x/y/foo")
        if (allocated(error)) return
        call check_expand(error, "foo", "/x/y", "/x/y/foo")
        if (allocated(error)) return
        call check_expand(error, "/a/./b/../c", "/x/", "/a/c")
        if (allocated(error)) return
        call check_expand(error, "/a/b/.", "/x/", "/a/b")
        if (allocated(error)) return
        call check_expand(error, "/a/b/..", "/x/", "/a")
        if (allocated(error)) return
        call check_expand(error, "/.", "/x/", "/")
        if (allocated(error)) return
        ! (expand-file-name ".." "/") is "/.." (the doc string's example).
        call check_expand(error, "..", "/", "/..")
        if (allocated(error)) return
        call check_expand(error, "../z", "/x/y/", "/x/z")
        if (allocated(error)) return
        call check_expand(error, "//a//b", "/x/", "//a/b")
        if (allocated(error)) return
        call check_expand(error, "///a", "/x/", "/a")
        if (allocated(error)) return
        call check_expand(error, "/already/canonical", "/x/", "/already/canonical")
        if (allocated(error)) return
        call check_expand(error, "", "/x/y/", "/x/y")
        if (allocated(error)) return
        call check_expand(error, ".", "/x/y/", "/x/y")
        if (allocated(error)) return
        call check_expand(error, "/a/", "/x/", "/a/")
        if (allocated(error)) return
        call check_expand(error, "/a/", "/x/", "/a/")
        if (allocated(error)) return
        call relative_default(error)
    end subroutine test_expand_file_name

    !> A relative default directory is itself expanded, against the
    !> buffer's default-directory.
    subroutine relative_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer

        call make_test_buffer(buffer)
        buffers(current)%directory = "/b/"
        call check_expand(error, "foo", "rel", "/b/rel/foo")
        call teardown(buffer)
    end subroutine relative_default

    subroutine test_expand_tilde(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: home
        logical :: had_home
        integer(int32) :: e

        call save_home(home, had_home)
        e = sys_setenv("HOME", "/home/someone")
        call tilde_cases(error)
        call restore_home(home, had_home)
    end subroutine test_expand_tilde

    subroutine tilde_cases(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: me, home
        logical :: found, found_home

        call passwd_name_of_user(me, found)
        call passwd_home_of_user(home, found_home)
        call check_expand(error, "~", "/x/", "/home/someone")
        if (allocated(error)) return
        call check_expand(error, "~/a/../b", "/x/", "/home/someone/b")
        if (allocated(error)) return

        ! ~USER takes USER's home from the passwd database, not $HOME.
        ! A home of "/" gives "//x", a leading "//" being kept (as
        ! upstream; a build sandbox's user may have that home).
        if (found .and. found_home) then
            if (home == "/") then
                call check_expand(error, "~"//me//"/x", "/x/", "//x")
            else
                call check_expand(error, "~"//me//"/x", "/x/", file_name_as_directory(home)//"x")
            end if

            if (allocated(error)) return
        end if

        ! An unknown user leaves "~nosuchuser" as an ordinary name.
        call check_expand(error, "~nosuchuser/x", "/d/", "/d/~nosuchuser/x")
        if (allocated(error)) return
        call check(error, get_homedir() == "/home/someone")
    end subroutine tilde_cases

    subroutine save_home(home, had_home)
        character(len=:), allocatable, intent(out) :: home
        logical, intent(out) :: had_home
        integer :: length, stat

        call get_environment_variable("HOME", length=length, status=stat)
        had_home = stat == 0
        allocate (character(len=max(length, 0)) :: home)
        if (had_home .and. length > 0) call get_environment_variable("HOME", home)
    end subroutine save_home

    subroutine restore_home(home, had_home)
        character(len=*), intent(in) :: home
        logical, intent(in) :: had_home
        integer(int32) :: e

        if (had_home) then
            e = sys_setenv("HOME", home)
        else
            e = sys_unsetenv("HOME")
        end if
    end subroutine restore_home

    ! fileio-tests--relative-HOME
    subroutine test_relative_home(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: a, b, home
        type(status_type) :: st
        integer(int32) :: e
        logical :: had_home

        call init_fileio()
        call save_home(home, had_home)
        e = sys_setenv("HOME", "a/b/c")
        call expand_file_name("~/foo", a, st)
        call expand_file_name("a/b/c/foo", b, st, file_name_as_directory(emacs_wd))
        call restore_home(home, had_home)
        call check(error, a == b .and. len(a) == len(b), a//" vs "//b)
    end subroutine test_relative_home

    ! fileio-tests--relative-default-directory
    subroutine test_relative_default_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        character(len=:), allocatable :: name
        type(status_type) :: st

        call setup(buffer)
        buffers(current)%directory = "some/relative/name"
        call expand_file_name("foo", name, st)
        call check(error, st%ok() .and. is_file_name_absolute(name), name)
        if (allocated(error)) return
        buffers(current)%directory = "~foo"
        call expand_file_name("bar", name, st)
        call check(error, is_file_name_absolute(name) .and. name(1:1) /= "~", name)
        call teardown(buffer)
    end subroutine test_relative_default_directory

    ! fileio-tests--expand-file-name-null-bytes
    subroutine test_null_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        character(len=:), allocatable :: name
        type(status_type) :: st

        call expand_file_name("file"//achar(0)//".txt", name, st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call expand_file_name("file.txt", name, st, "dir"//achar(0))
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call setup(buffer)
        buffers(current)%directory = "dir"//achar(0)
        call expand_file_name("file.txt", name, st)
        call check(error, st%code == FM_WRONG_TYPE_ARGUMENT)
        call teardown(buffer)
    end subroutine test_null_bytes

    subroutine test_write_and_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: msg
        integer(int64) :: inserted
        logical :: modified

        call setup(buffer)
        call insert("héllo"//new_line("a")//"wörld"//new_line("a"), st)
        call write_region(testdir//"/f1", .true., st, message=msg)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, msg == "Wrote "//file_name_as_directory(emacs_wd)//testdir//"/f1", msg)
        if (allocated(error)) return
        call check(error, buffers(current)%filename == file_name_as_directory(emacs_wd)//testdir//"/f1")
        if (allocated(error)) return
        call is_buffer_modified(current_buffer(), modified, st)
        call check(error,.not. modified, "visiting write leaves the buffer unmodified")
        if (allocated(error)) return
        ! Insert it twice into another buffer: point stays before the text.
        call teardown(buffer)
        call make_test_buffer(buffer)
        call insert("<>", st)
        call goto_char(2_int64)
        call insert_file_contents(testdir//"/f1", .false., inserted, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, inserted == 12 .and. point() == 2, "inserted 12 chars, point stays")
        if (allocated(error)) return
        call check(error, buffer_string() == "<héllo"//new_line("a")//"wörld"//new_line("a")//">")
        if (allocated(error)) return
        call check(error,.not. allocated(buffers(current)%filename), "no visit")
        if (allocated(error)) return
        ! A region.
        call write_region(testdir//"/f2", .false., st, 2_int64, 7_int64)
        call teardown(buffer)
        call make_test_buffer(buffer)
        call insert_file_contents(testdir//"/f2", .true., inserted, st)
        call check(error, buffer_string() == "héllo")
        call teardown(buffer)
    end subroutine test_write_and_insert

    !> Bytes that are neither UTF-8 nor Latin-1 visit as emacs-mule.
    subroutine test_raw_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: bytes
        integer(int64) :: inserted
        integer :: u, ios

        call setup(buffer)
        bytes = "a"//achar(255)//achar(195)//achar(195)//"b"//achar(226)//achar(130)//achar(172)
        open (newunit=u, file=testdir//"/f3", access="stream", form="unformatted", status="replace", &
              action="write", iostat=ios)
        call check(error, ios == 0, "open f3")
        if (allocated(error)) return
        write (u) bytes
        close (u)
        call insert_file_contents(testdir//"/f3", .true., inserted, st)
        ! Neither UTF-8 nor Latin-1: emacs-mule, as GNU Emacs detects it,
        ! with seven characters: a, three raw bytes, b, a raw byte and Ź
        ! (latin-iso8859-2 0xAC).
        call check(error, st%ok() .and. inserted == 7 .and. buffers(current)%multibyte, "7 characters")
        if (allocated(error)) return
        call check(error, last_coding_system_used%value == "emacs-mule", "emacs-mule")
        ! (Writing it back asks for a coding system in GNU Emacs, as
        ! select-safe-coding-system does not offer emacs-mule.)
        call teardown(buffer)
    end subroutine test_raw_bytes

    subroutine test_visit_nonexistent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: inserted

        call setup(buffer)
        call insert_file_contents(testdir//"/missing", .true., inserted, st)
        call check(error, st%code == FM_FILE_MISSING .and. condition_matches(st%code, FM_FILE_ERROR), format_error(st))
        if (allocated(error)) return
        call check(error, format_error(st) == "Opening input file: No such file or directory, "// &
                   file_name_as_directory(emacs_wd)//testdir//"/missing", format_error(st))
        if (allocated(error)) return
        call check(error, allocated(buffers(current)%filename), "visit records the file anyway")
        if (allocated(error)) return
        call check(error, is_visited_file_modtime_current(), "nonexistent and unchanged")
        if (allocated(error)) return
        ! Without visit, nothing is recorded.
        call teardown(buffer)
        call make_test_buffer(buffer)
        call insert_file_contents(testdir//"/missing", .false., inserted, st)
        call check(error, st%code == FM_FILE_MISSING .and. .not. allocated(buffers(current)%filename), format_error(st))
        call teardown(buffer)
    end subroutine test_visit_nonexistent

    subroutine test_insert_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: inserted

        call setup(buffer)
        call insert_file_contents(testdir, .false., inserted, st)
        call check(error, st%code == FM_FILE_ERROR, format_error(st))
        if (allocated(error)) return
        call check(error, index(format_error(st), "Read error: Is a directory, ") == 1, format_error(st))
        call teardown(buffer)
    end subroutine test_insert_directory

    subroutine test_visit_non_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: inserted

        call setup(buffer)
        call insert("x", st)
        call insert_file_contents(testdir//"/f1", .true., inserted, st)
        call check(error, format_error(st) == "Cannot do file visiting in a non-empty buffer", format_error(st))
        call teardown(buffer)
    end subroutine test_visit_non_empty

    subroutine test_modtime(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer :: u, ios

        call setup(buffer)
        call insert("one", st)
        call write_region(testdir//"/f5", .true., st)
        call check(error, is_visited_file_modtime_current(), "just written")
        if (allocated(error)) return
        ! Change the file behind the buffer's back (its size differs).
        open (newunit=u, file=testdir//"/f5", status="replace", action="write", iostat=ios)
        call check(error, ios == 0, "open f5")
        if (allocated(error)) return
        write (u, "(a)") "changed elsewhere"
        close (u)
        call check(error,.not. is_visited_file_modtime_current(), "changed on disk")
        call teardown(buffer)
    end subroutine test_modtime

    subroutine test_tests(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup(buffer)
        call insert("p", st)
        call write_region(testdir//"/f6", .false., st)
        call check(error, file_exists(testdir//"/f6") .and. is_file_readable(testdir//"/f6"))
        if (allocated(error)) return
        call check(error, is_file_writable(testdir//"/f6") .and. is_file_writable(testdir//"/new"), "writable")
        if (allocated(error)) return
        call check(error, is_file_directory(testdir) .and. .not. is_file_directory(testdir//"/f6"))
        if (allocated(error)) return
        call check(error,.not. file_exists(testdir//"/missing"))
        if (allocated(error)) return
        call check(error,.not. is_file_writable(testdir//"/nodir/x"), "no such directory")
        if (allocated(error)) return
        call set_file_modes(testdir//"/f6", int(o'640', int32), st)
        call check(error, st%ok() .and. file_modes(testdir//"/f6") == int(o'640', int32))
        if (allocated(error)) return
        call check(error, file_modes(testdir//"/missing") == -1)
        if (allocated(error)) return
        call check(error,.not. is_file_executable(testdir//"/f6"), "not executable")
        if (allocated(error)) return
        call set_file_modes(testdir//"/f6", int(o'750', int32), st)
        call check(error, is_file_executable(testdir//"/f6") .and. is_file_executable(testdir), "executable")
        if (allocated(error)) return
        ! file-newer-than-file-p: a missing file is older than any other.
        call check(error, is_file_newer_than_file(testdir//"/f6", testdir//"/missing") .and. &
                   .not. is_file_newer_than_file(testdir//"/missing", testdir//"/f6") .and. &
                   .not. is_file_newer_than_file(testdir//"/f6", testdir//"/f6"), "file-newer-than-file-p")
        if (allocated(error)) return
        ! make-symbolic-link: once, then again only when allowed.
        call create_symbolic_link("f6", testdir//"/link", st)
        call check(error, st%ok() .and. is_file_symlink(testdir//"/link"), "symbolic link")
        if (allocated(error)) return
        call create_symbolic_link("f6", testdir//"/link", st)
        call check(error, st%code == FM_FILE_ALREADY_EXISTS .and. condition_matches(st%code, FM_FILE_ERROR), &
                   format_error(st))
        if (allocated(error)) return
        call create_symbolic_link("f6", testdir//"/link", st, ok_if_already_exists=.true.)
        call check(error, st%ok(), "replaced")
        if (allocated(error)) return
        call delete_file_internal(testdir//"/link", st)
        call create_directory_internal(testdir//"/sub", st)
        call check(error, st%ok() .and. is_file_directory(testdir//"/sub/"))
        if (allocated(error)) return
        call create_directory_internal(testdir//"/sub", st)
        call check(error, condition_matches(st%code, FM_FILE_ERROR), "already exists")
        if (allocated(error)) return
        call check(error, st%code == FM_FILE_ALREADY_EXISTS .and. index(format_error(st), "File exists: /") == 1 &
                   .and. index(format_error(st), testdir//"/sub") > 0, format_error(st))
        if (allocated(error)) return
        call delete_directory_internal(testdir//"/sub", st)
        call check(error, st%ok() .and. .not. file_exists(testdir//"/sub"))
        call teardown(buffer)
    end subroutine test_tests

    subroutine test_rename_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: inserted

        call setup(buffer)
        call insert("data", st)
        call write_region(testdir//"/f1", .false., st)
        call set_file_modes(testdir//"/f1", int(o'600', int32), st)
        call copy_file(testdir//"/f1", testdir//"/f2", st)
        call check(error, st%ok() .and. file_modes(testdir//"/f2") == int(o'600', int32), "copy keeps modes")
        if (allocated(error)) return
        call rename_file(testdir//"/f2", testdir//"/f3", st)
        call check(error, st%ok() .and. .not. file_exists(testdir//"/f2") .and. file_exists(testdir//"/f3"))
        if (allocated(error)) return
        call teardown(buffer)
        call make_test_buffer(buffer)
        call insert_file_contents(testdir//"/f3", .false., inserted, st)
        call check(error, buffer_string() == "data")
        if (allocated(error)) return
        call rename_file(testdir//"/missing", testdir//"/f4", st)
        call check(error, st%code == FM_FILE_MISSING .and. index(format_error(st), "Renaming: ") == 1, &
                   format_error(st))
        call teardown(buffer)
    end subroutine test_rename_copy

    subroutine test_substitute(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: me
        integer(int32) :: e
        logical :: found

        call passwd_name_of_user(me, found)
        e = sys_setenv("FM_TEST_DIR", "/opt/x")
        e = sys_unsetenv("FM_TEST_UNSET")
        call check(error, substitute_in_file_name("/a/b//etc/hosts") == "/etc/hosts")
        if (allocated(error)) return
        call check(error, substitute_in_file_name("/a/b/~/x") == "~/x")
        if (allocated(error)) return

        if (found) then
            call check(error, substitute_in_file_name("/a/b/~"//me//"/x") == "~"//me//"/x")
            if (allocated(error)) return
        end if

        call check(error, substitute_in_file_name("/a/~nosuchuser/x") == "/a/~nosuchuser/x")
        if (allocated(error)) return
        call check(error, substitute_in_file_name("$FM_TEST_DIR/y") == "/opt/x/y")
        if (allocated(error)) return
        call check(error, substitute_in_file_name("${FM_TEST_DIR}y") == "/opt/xy")
        if (allocated(error)) return
        ! A substitution that makes an absolute name drops what came before.
        call check(error, substitute_in_file_name("/a/$FM_TEST_DIR/y") == "/opt/x/y", &
                   substitute_in_file_name("/a/$FM_TEST_DIR/y"))
        if (allocated(error)) return
        call check(error, substitute_in_file_name("/a/$FM_TEST_UNSET/b") == "/a/$FM_TEST_UNSET/b")
        if (allocated(error)) return
        call check(error, substitute_in_file_name("/cost$$") == "/cost$")
        if (allocated(error)) return
        call check(error, substitute_in_file_name("/plain") == "/plain")
    end subroutine test_substitute

    !> Records "A<beginning>,<end>,<old_len>" (an after-change function).
    subroutine rec_after(beginning, end, old_len, status)
        integer(int64), intent(in) :: beginning, end, old_len
        type(status_type), intent(out) :: status
        character(len=64) :: buffer

        status = status_type()
        write (buffer, "(a, i0, a, i0, a, i0)") " A", beginning, ",", end, ",", old_len
        trace = trace//trim(buffer)
    end subroutine rec_after

    !> Inserting a file runs the after-change functions for the inserted
    !> text; visiting one does not (fileio.c:Finsert_file_contents).
    subroutine test_insert_change_hooks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: inserted

        call setup(buffer)
        call insert("abc", st)
        call write_region(testdir//"/f5", .false., st)
        call teardown(buffer)
        call after_change_functions%append(rec_after)
        call make_test_buffer(buffer)
        trace = ""
        call insert_file_contents(testdir//"/f5", .true., inserted, st)
        call check(error, st%ok() .and. trace == "", "visiting: no change hooks")
        if (allocated(error)) return
        call teardown(buffer)
        call make_test_buffer(buffer)
        call insert("<>", st)
        call goto_char(2_int64)
        trace = ""
        call insert_file_contents(testdir//"/f5", .false., inserted, st)
        call check(error, trace, " A2,5,0", "inserting")
        call teardown(buffer)
        call after_change_functions%remove(rec_after)
    end subroutine test_insert_change_hooks
end module test_fileio_mod
