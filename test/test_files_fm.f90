! test_files_fm.f90 -- tests for files_fm, subr_fm, map_ynp_fm and the
! command-line part of emacs_fm.
! Own tests; files-tests.el exercises these functions through file name
! handlers, the minibuffer and Lisp hooks.
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

!> Own tests: files-tests.el exercises these functions through file name
!> handlers, the minibuffer and Lisp hooks, none of which exist yet.
module test_files_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use bindings_fm, only: init_bindings
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support, only: show_alone
    use error_fm, only: format_message
    use error_fm, only: status_type, format_error, condition_matches, FM_QUIT, FM_USER_ERROR, &
                        FM_FILE_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, &
                         current_buffer, resolve_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, buffer_string, goto_char, point_max
    use keyboard_fm, only: unread_command_events, discard_input, last_message, exit_requested_p
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_exists_p, &
                         file_modes, set_file_modes, write_region, make_directory_internal, &
                         delete_file_internal, delete_directory_internal
    use files_fm, only: find_file_noselect, find_file, create_file_buffer, abbreviate_file_name, &
                        save_buffer, basic_save_buffer, make_backup_file_name, &
                        normal_backup_enable_predicate, save_some_buffers, &
                        save_buffers_kill_emacs, temporary_file_directory, make_directory, require_final_newline, &
                        FINAL_NEWLINE_NEVER, FINAL_NEWLINE_ASK, FINAL_NEWLINE_VISIT, find_file_run_dired
    use subr_fm, only: y_or_n_p
    use map_ynp_fm, only: map_y_or_n_p, YNP_SKIP, YNP_ASK, YNP_ACT
    use emacs_fm, only: command_line_1, arg_type
    use sysdep_fm, only: sys_setenv, sys_unsetenv
    implicit none(type, external)
    private

    public :: collect_files_fm

    character(len=*), parameter :: testdir = "build/test-files"

    !> Objects acted on by the map_y_or_n_p test.
    logical :: acted(5)

contains

    !> Name of live buffer `h`.
    function bname(h) result(name)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: name
        integer(int64) :: i
        type(status_type) :: st

        call resolve_buffer(h, i, st)
        name = "<dead>"
        if (st%ok()) name = buffers(i)%name
    end function bname

    !> Collect all exported unit tests.
    subroutine collect_files_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("abbreviate-file-name", test_abbreviate), &
                    new_unittest("create-file-buffer", test_create_file_buffer), &
                    new_unittest("find-file-existing", test_find_existing), &
                    new_unittest("find-file-new", test_find_new), &
                    new_unittest("find-file-directory", test_find_directory), &
                    new_unittest("find-file-read-only", test_find_read_only), &
                    new_unittest("save-buffer-backup", test_save_backup), &
                    new_unittest("save-buffer-unmodified", test_save_unmodified), &
                    new_unittest("save-buffer-no-file", test_save_no_file), &
                    new_unittest("backup-predicate", test_backup_predicate), &
                    new_unittest("temporary-file-directory", test_temporary_file_directory), &
                    new_unittest("y-or-n-p", test_y_or_n_p), &
                    new_unittest("map-y-or-n-p", test_map_ynp), &
                    new_unittest("save-some-buffers", test_save_some_buffers), &
                    new_unittest("save-buffers-kill-emacs", test_kill_emacs), &
                    new_unittest("command-line-files", test_command_line), &
                    new_unittest("files-tests-make-directory", test_make_directory), &
                    new_unittest("require-final-newline", test_final_newline), &
                    new_unittest("write-file", test_write_file), &
                    new_unittest("files-tests-revert-buffer", test_revert_buffer), &
                    new_unittest("revert-buffer-replace", test_revert_buffer_replace), &
                    new_unittest("revert-buffer-nonexistent", test_revert_nonexistent) &
                    ]
    end subroutine collect_files_fm

    !> Absolute name of `name` in the test directory.
    function path(name) result(p)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: p

        p = file_name_as_directory(emacs_wd)//testdir//"/"//name
    end function path

    !> A clean test directory holding a.txt ("alpha\n") and a scratch
    !> buffer in the working directory.
    subroutine setup()
        type(handle_type) :: buf
        type(status_type) :: st
        character(len=5), parameter :: names(6) = [character(len=5) :: &
                                                   "a.txt", "b.txt", "c.txt", "new", "ro", "x"]
        integer :: i

        call init_bindings()
        call init_fileio()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
        call make_directory_internal(testdir, st)

        do i = 1, size(names)
            call set_file_modes(path(trim(names(i))), int(o'644', int32), st)
            call delete_file_internal(path(trim(names(i))), st)
            call delete_file_internal(path(trim(names(i))//"~"), st)
        end do

        call delete_directory_internal(path("dir"), st)
        call get_buffer_create(" setup", buf, st)
        call set_buffer(buf, st)
        call insert("alpha"//new_line("a"), st)
        call write_region(path("a.txt"), .false., st)
        call kill_buffer(buf, st)
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        call show_alone()
    end subroutine setup

    !> Kill every buffer visiting a file in the test directory.
    subroutine kill_file_buffers()
        character(len=5), parameter :: names(5) = [character(len=5) :: "a.txt", "b.txt", "c.txt", &
                                                                                                                  "new", "ro"]
        type(handle_type) :: buf
        type(status_type) :: st
        integer :: i

        do i = 1, size(names)
            call find_file_noselect(path(trim(names(i))), buf, st)
            if (st%ok()) call kill_buffer(buf, st)
        end do
    end subroutine kill_file_buffers

    !> Read file `file` whole.
    function slurp(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text
        integer :: u, ios, n

        inquire (file=file, size=n)
        allocate (character(len=max(n, 0)) :: text)
        open (newunit=u, file=file, access="stream", form="unformatted", status="old", &
              action="read", iostat=ios)

        if (ios /= 0) then
            text = "<missing>"
            return
        end if

        if (n > 0) read (u, iostat=ios) text
        if (ios /= 0) text = "<unreadable>"
        close (u)
    end function slurp

    !> Queue `keys` as input events.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(keys)))

        do i = 1, len(keys)
            evs(i) = make_fixnum(int(iachar(keys(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_keys

    subroutine test_abbreviate(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: home
        integer(int32) :: e
        integer :: length, stat

        body: block
            call get_environment_variable("HOME", length=length, status=stat)
            allocate (character(len=max(length, 0)) :: home)
            if (stat == 0 .and. length > 0) call get_environment_variable("HOME", home)
            e = sys_setenv("HOME", "/home/u")
            call check(error, abbreviate_file_name("/home/u/x/y") == "~/x/y")
            if (allocated(error)) exit body
            call check(error, abbreviate_file_name("/home/u") == "~")
            if (allocated(error)) exit body
            call check(error, abbreviate_file_name("/home/user2/x") == "/home/user2/x")
            if (allocated(error)) exit body
            e = sys_setenv("HOME", "/")
            call check(error, abbreviate_file_name("/etc") == "/etc", "a home of / is not abbreviated")
        end block body

        if (stat == 0) then
            e = sys_setenv("HOME", home)
        else
            e = sys_unsetenv("HOME")
        end if
    end subroutine test_abbreviate

    subroutine test_create_file_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, b3
        type(status_type) :: st

        call create_file_buffer("/x/notes.txt", b1, st)
        call create_file_buffer("/y/notes.txt", b2, st)
        call create_file_buffer("/z/ spaced", b3, st)
        call check(error, bname(b1) == "notes.txt" .and. bname(b2) == "notes.txt<2>", &
                   bname(b2))
        if (allocated(error)) return
        call check(error, bname(b3) == "| spaced", bname(b3))
        call kill_buffer(b1, st)
        call kill_buffer(b2, st)
        call kill_buffer(b3, st)
    end subroutine test_create_file_buffer

    subroutine test_find_existing(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, again, old, cb
        type(status_type) :: st
        integer(int64) :: i

        call setup()
        old = current_buffer()
        call find_file_noselect(testdir//"/a.txt", buf, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        cb = current_buffer()
        call check(error, cb%index == old%index, "not selected")
        if (allocated(error)) return
        call resolve_buffer(buf, i, st)
        call check(error, buffers(i)%name == "a.txt" .and. buffers(i)%filename == path("a.txt"))
        if (allocated(error)) return
        call check(error, buffers(i)%directory == path(""), buffers(i)%directory)
        if (allocated(error)) return
        call check(error, buffers(i)%pt == 1 .and. .not. buffers(i)%read_only)
        if (allocated(error)) return
        call check(error, buffers(i)%text%save_modiff >= buffers(i)%text%modiff, "unmodified")
        if (allocated(error)) return
        ! Visiting it again finds the same buffer.
        call find_file_noselect(path("a.txt"), again, st)
        call check(error, again%index == buf%index .and. again%generation == buf%generation)
        if (allocated(error)) return
        call find_file(path("a.txt"), st)
        cb = current_buffer()
        call check(error, cb%index == buf%index .and. buffer_string() == "alpha"//new_line("a"))
        call kill_buffer(buf, st)
    end subroutine test_find_existing

    subroutine test_find_new(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: i

        call setup()
        last_message = ""
        call find_file_noselect(path("new"), buf, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, last_message == "(New file)", last_message)
        if (allocated(error)) return
        call resolve_buffer(buf, i, st)
        call check(error, buffers(i)%filename == path("new") .and. buffers(i)%text%z == 1)
        if (allocated(error)) return
        ! Saving an empty new buffer creates the file.
        call set_buffer(buf, st)
        call save_buffer(1_int64, .true., st)
        call check(error, st%ok() .and. file_exists_p(path("new")), format_error(st))
        if (allocated(error)) return
        call check(error, last_message == "Wrote "//path("new"), last_message)
        call kill_buffer(buf, st)
    end subroutine test_find_new

    subroutine test_find_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: i

        call setup()
        ! Without `find_file_run_dired', visiting a directory is an error.
        find_file_run_dired = .false.
        call find_file_noselect(testdir, buf, st)
        find_file_run_dired = .true.
        call check(error, format_error(st) == abbreviate_file_name(path(""))//" is a directory" .or. &
                   format_error(st) == abbreviate_file_name(file_name_as_directory(emacs_wd)//testdir)// &
                   " is a directory", format_error(st))
        if (allocated(error)) return
        ! With it, `find_directory_functions' (Dired) visits it.
        call find_file_noselect(testdir, buf, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call resolve_buffer(buf, i, st)
        call check(error, buffers(i)%major_mode == "dired_mode", "a Dired buffer")
        call kill_buffer(buf, st)
    end subroutine test_find_directory

    subroutine test_find_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: i

        call setup()
        call write_region(path("ro"), .false., st)
        call set_file_modes(path("ro"), int(o'444', int32), st)
        call find_file_noselect(path("ro"), buf, st)
        call resolve_buffer(buf, i, st)
        call check(error, buffers(i)%read_only, "a file without write bits is visited read-only")
        call kill_buffer(buf, st)
        call set_file_modes(path("ro"), int(o'644', int32), st)
    end subroutine test_find_read_only

    subroutine test_save_backup(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        body: block
            call setup()

            ! No backups under /tmp or temporary-file-directory, where a
            ! sandboxed build (Guix, Nix) may run the tests.
            if (.not. normal_backup_enable_predicate(path("a.txt"))) then
                call skip_test(error, "test directory is under /tmp, where no backups are made")
                exit body
            end if

            call set_file_modes(path("a.txt"), int(o'600', int32), st)
            call find_file(path("a.txt"), st)
            call goto_char(point_max())
            call insert("beta"//new_line("a"), st)
            call save_buffer(1_int64, .true., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, slurp(path("a.txt")) == "alpha"//new_line("a")//"beta"//new_line("a"), "saved")
            if (allocated(error)) exit body
            call check(error, slurp(path("a.txt~")) == "alpha"//new_line("a"), "backup holds the old text")
            if (allocated(error)) exit body
            call check(error, file_modes(path("a.txt")) == int(o'600', int32), "modes kept")
            if (allocated(error)) exit body
            call check(error, buffers(cur)%backed_up)
            if (allocated(error)) exit body
            ! The second save doesn't back up again.
            call insert("gamma"//new_line("a"), st)
            call save_buffer(1_int64, .true., st)
            call check(error, slurp(path("a.txt~")) == "alpha"//new_line("a"), "one backup per visit")
            if (allocated(error)) exit body
            ! C-u C-u C-x C-s backs up now.
            call insert("delta"//new_line("a"), st)
            call save_buffer(16_int64, .true., st)
            call check(error, index(slurp(path("a.txt~")), "gamma") > 0, "C-u C-u backs up the previous version")
        end block body

        call kill_file_buffers()
    end subroutine test_save_backup

    subroutine test_save_unmodified(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        body: block
            call setup()
            call find_file(path("a.txt"), st)
            last_message = ""
            call save_buffer(1_int64, .true., st)
            call check(error, st%ok() .and. last_message == "(No changes need to be saved)", last_message)
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(path("a.txt~")), "no backup")
            if (allocated(error)) exit body
            ! A visited file deleted behind Emacs's back is saved again.
            call delete_file_internal(path("a.txt"), st)
            call save_buffer(1_int64, .true., st)
            call check(error, st%ok() .and. slurp(path("a.txt")) == "alpha"//new_line("a"), format_error(st))
        end block body

        call kill_file_buffers()
    end subroutine test_save_unmodified

    subroutine test_save_no_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st

        call setup()
        call get_buffer_create("nofile", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%directory = path("")
        call insert("x", st)
        ! It asks for a file name ("File to save in: "); C-g gives up.
        call type_keys(achar(7))
        call basic_save_buffer(.true., st)
        call check(error, st%code == FM_QUIT, format_error(st))
        if (allocated(error)) return
        ! RET accepts the default, the buffer name in its directory.
        call type_keys(achar(13))
        call basic_save_buffer(.true., st)
        call check(error, st%ok() .and. slurp(path("nofile")) == "x", format_error(st))
        if (allocated(error)) return
        call check(error, buffers(cur)%filename == path("nofile"))
        call kill_buffer(buf, st)
        call delete_file_internal(path("nofile"), st)
    end subroutine test_save_no_file

    subroutine test_backup_predicate(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error,.not. normal_backup_enable_predicate("/tmp/x"))
        if (allocated(error)) return
        call check(error,.not. normal_backup_enable_predicate(temporary_file_directory()//"x"))
        if (allocated(error)) return
        call check(error, normal_backup_enable_predicate("/tmp"), "/tmp itself is not under /tmp")
        if (allocated(error)) return
        call check(error, make_backup_file_name("/a/b.c") == "/a/b.c~")
        if (allocated(error)) return
        call check(error, normal_backup_enable_predicate(path("a.txt")) .eqv. &
                   (index(path("a.txt"), temporary_file_directory()) /= 1))
    end subroutine test_backup_predicate

    subroutine test_temporary_file_directory(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=6), parameter :: names(3) = [character(len=6) :: "TMPDIR", "TMP", "TEMP"]
        character(len=256) :: saved(3)
        integer :: stat(3), i
        integer(int32) :: e

        do i = 1, size(names)
            call get_environment_variable(trim(names(i)), saved(i), status=stat(i))
            e = sys_unsetenv(trim(names(i)))
        end do

        body: block
            call check(error, temporary_file_directory() == "/tmp/")
            if (allocated(error)) exit body
            e = sys_setenv("TEMP", "/c")
            call check(error, temporary_file_directory() == "/c/", "TEMP is the last fallback")
            if (allocated(error)) exit body
            e = sys_setenv("TMP", "/b/")
            call check(error, temporary_file_directory() == "/b/", "TMP comes before TEMP")
            if (allocated(error)) exit body
            e = sys_setenv("TMPDIR", "/a")
            call check(error, temporary_file_directory() == "/a/", "TMPDIR comes first")
            if (allocated(error)) exit body
            e = sys_setenv("TMPDIR", "")
            call check(error, temporary_file_directory() == "./", "an empty TMPDIR is still set")
        end block body

        do i = 1, size(names)
            if (stat(i) == 0) then
                e = sys_setenv(trim(names(i)), trim(saved(i)))
            else
                e = sys_unsetenv(trim(names(i)))
            end if
        end do
    end subroutine test_temporary_file_directory

    subroutine test_y_or_n_p(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: answer

        call discard_input()
        call type_keys("y")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%ok() .and. answer .and. last_message == "Go? (y or n) y", last_message)
        if (allocated(error)) return
        call type_keys(achar(127))
        call y_or_n_p("Go? ", answer, st)
        call check(error, st%ok() .and. .not. answer .and. last_message == "Go? (y or n) n", last_message)
        if (allocated(error)) return
        call type_keys(achar(7))
        call y_or_n_p("Go? ", answer, st)
        call check(error, st%code == FM_QUIT)
        if (allocated(error)) return
        ! End of input reads as quit.
        call y_or_n_p("Go? ", answer, st)
        call check(error, st%code == FM_QUIT)
    end subroutine test_y_or_n_p

    subroutine ask_all(k, how, prompt)
        integer, intent(in) :: k
        integer, intent(out) :: how
        character(len=:), allocatable, intent(out) :: prompt
        character(len=12) :: num

        write (num, "(i0)") k
        how = YNP_ASK
        if (k == 2) how = YNP_SKIP
        prompt = "Item "//trim(num)//"? "
    end subroutine ask_all

    subroutine act(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status

        status = status_type()
        acted(k) = .true.
    end subroutine act

    subroutine test_map_ynp(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer :: actions

        call discard_input()
        ! y on 1, (2 skipped by the prompter), n on 3, ! for 4 and 5
        acted = .false.
        call type_keys("yn!")
        call map_y_or_n_p(5, ask_all, act, actions, st)
        call check(error, st%ok() .and. actions == 3, "three acted on")
        if (allocated(error)) return
        call check(error, all(acted .eqv. [.true., .false., .false., .true., .true.]))
        if (allocated(error)) return
        ! "." acts and stops; q stops without acting.
        acted = .false.
        call type_keys(".")
        call map_y_or_n_p(5, ask_all, act, actions, st)
        call check(error, actions == 1 .and. acted(1) .and. .not. any(acted(2:)))
        if (allocated(error)) return
        acted = .false.
        call type_keys("nq")
        call map_y_or_n_p(5, ask_all, act, actions, st)
        call check(error, actions == 0 .and. .not. any(acted))
        if (allocated(error)) return
        ! The echo area is restored.
        call check(error, last_message == "", last_message)
        if (allocated(error)) return
        ! Other keys say how to get help; the question stays.
        acted = .false.
        call type_keys("xy.")
        call map_y_or_n_p(5, ask_all, act, actions, st)
        call check(error, st%ok() .and. actions == 2 .and. acted(1) .and. acted(3), "x is not an answer")
        if (allocated(error)) return
        call type_keys(achar(7))
        call map_y_or_n_p(5, ask_all, act, actions, st)
        call check(error, st%code == FM_QUIT)
    end subroutine test_map_ynp

    subroutine test_save_some_buffers(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        body: block
            call setup()
            call find_file(path("b.txt"), st)
            call insert("bee", st)
            call find_file(path("c.txt"), st)
            call insert("sea", st)
            ! Answer n for c.txt, y for b.txt (in buffer-list order: c.txt
            ! was selected last).
            call type_keys("ny")
            call save_some_buffers(.false., .true., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            ! Text mode (from the file name) sets require-final-newline.
            call check(error, slurp(path("b.txt")) == "bee"//achar(10) .and. .not. file_exists_p(path("c.txt")))
            if (allocated(error)) exit body
            ! With ARG, save all without asking.
            call save_some_buffers(.true., .true., st)
            call check(error, slurp(path("c.txt")) == "sea"//achar(10))
            if (allocated(error)) exit body
            last_message = ""
            call save_some_buffers(.false., .true., st)
            call check(error, last_message == "(No files need saving)", last_message)
        end block body

        call kill_file_buffers()
    end subroutine test_save_some_buffers

    subroutine test_kill_emacs(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        body: block
            call setup()
            call save_buffers_kill_emacs(.false., st)
            call check(error, st%ok() .and. exit_requested_p(), "nothing to save: exit")
            if (allocated(error)) exit body
            call find_file(path("b.txt"), st)
            call insert("bee", st)
            ! Don't save; then refuse to exit.
            call reset_exit()
            call type_keys("nno"//achar(13))
            call save_buffers_kill_emacs(.false., st)
            call check(error, st%ok() .and. .not. exit_requested_p(), "stayed")
            if (allocated(error)) exit body
            ! Save it: then exit without further questions.
            call type_keys("y")
            call save_buffers_kill_emacs(.false., st)
            call check(error, st%ok() .and. exit_requested_p() .and. slurp(path("b.txt")) == "bee"//achar(10))
        end block body

        call kill_file_buffers()
        call reset_exit()
    end subroutine test_kill_emacs

    !> Clear a pending exit request (command_loop resets it on entry; with
    !> no input it returns at once).
    subroutine reset_exit()
        use keyboard_fm, only: command_loop
        type(status_type) :: st

        call discard_input()
        call command_loop(st)
    end subroutine reset_exit

    subroutine test_command_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(arg_type), allocatable :: args(:)
        type(status_type) :: st

        body: block
            call setup()
            call set_buffer_text_file()
            allocate (args(4))
            args(1)%s = "+2"
            args(2)%s = testdir//"/a.txt"
            args(3)%s = "--"
            args(4)%s = "-dashed"
            call command_line_1(args, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, bname(current_buffer()) == "-dashed", "last file is current")
            if (allocated(error)) exit body
            call find_file(path("a.txt"), st)
            call check(error, point() == 7, "+2 put point on line 2")
            if (allocated(error)) exit body
            deallocate (args)
            allocate (args(2))
            args(1)%s = "+2:4"
            args(2)%s = testdir//"/a.txt"
            call command_line_1(args, st)
            call check(error, point() == 10, "+2:4 put point on column 3 of line 2")
            if (allocated(error)) exit body
            deallocate (args)
            allocate (args(1))
            args(1)%s = "--bogus"
            call command_line_1(args, st)
            call check(error, format_error(st) == format_message("Unknown option `")//"--bogus"//format_message("'"), &
                       format_error(st))
        end block body

        call kill_file_buffers()
        call find_file(path("-dashed"), st)
        if (st%ok()) call kill_buffer(current_buffer(), st)
    end subroutine test_command_line

    !> Make a.txt two lines long.
    subroutine set_buffer_text_file()
        type(handle_type) :: buf
        type(status_type) :: st

        call get_buffer_create(" two", buf, st)
        call set_buffer(buf, st)
        call insert("line1"//new_line("a")//"line2"//new_line("a"), st)
        call write_region(path("a.txt"), .false., st)
        call kill_buffer(buf, st)
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
    end subroutine set_buffer_text_file

    subroutine test_make_directory(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: dir, dirname, file, subdir1, subdir2, a_b
        type(status_type) :: st
        logical :: already

        dir = path("mkdir")
        dirname = dir//"/"
        file = dirname//"file"
        subdir1 = dirname//"subdir1"
        subdir2 = dirname//"subdir2"
        a_b = dirname//"a/b"
        call make_directory_internal(dir, st)
        call write_region(file, .false., st)

        body: block
            call make_directory("/", st)
            call check(error,.not. st%ok(), "/")
            if (allocated(error)) exit body
            call make_directory("/", st, parents=.true., already=already)
            call check(error, st%ok() .and. already, "/ with parents")
            if (allocated(error)) exit body
            call make_directory(dir, st)
            call check(error,.not. st%ok(), "dir")
            if (allocated(error)) exit body
            call make_directory(dir, st, parents=.true., already=already)
            call check(error, st%ok() .and. already, "dir with parents")
            if (allocated(error)) exit body
            call make_directory(dirname, st)
            call check(error,.not. st%ok(), "dirname")
            if (allocated(error)) exit body
            call make_directory(dirname, st, parents=.true., already=already)
            call check(error, st%ok() .and. already, "dirname with parents")
            if (allocated(error)) exit body
            call make_directory(file, st)
            call check(error,.not. st%ok(), "file")
            if (allocated(error)) exit body
            call make_directory(file, st, parents=.true.)
            call check(error,.not. st%ok(), "file with parents")
            if (allocated(error)) exit body
            call make_directory(subdir1, st, already=already)
            call check(error, st%ok() .and. .not. already, "subdir1")
            if (allocated(error)) exit body
            call make_directory(subdir2, st, parents=.true., already=already)
            call check(error, st%ok() .and. .not. already, "subdir2 with parents")
            if (allocated(error)) exit body
            call make_directory(a_b, st)
            call check(error,.not. st%ok(), "a/b")
            if (allocated(error)) exit body
            call make_directory(a_b, st, parents=.true., already=already)
            call check(error, st%ok() .and. .not. already, "a/b with parents")
        end block body

        call delete_directory_internal(a_b, st)
        call delete_directory_internal(dirname//"a", st)
        call delete_directory_internal(subdir1, st)
        call delete_directory_internal(subdir2, st)
        call delete_file_internal(file, st)
        call delete_directory_internal(dir, st)
    end subroutine test_make_directory

    !> basic-save-buffer adds a final newline as `require_final_newline'
    !> says (asking for other values than t and visit-save), visiting adds
    !> one for `visit', and saving into a missing directory asks to create
    !> it.
    subroutine test_final_newline(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        character(len=*), parameter :: LF = achar(10)

        body: block
            call setup()
            call delete_file_internal(path("nl/f"), st)
            call delete_directory_internal(path("nl"), st)
            ! No extension: Fundamental mode, which leaves it nil.
            call find_file(path("x"), st)
            call insert("x", st)
            call save_buffer(1_int64, .true., st)
            call check(error, slurp(path("x")) == "x", "nil: no newline")
            if (allocated(error)) exit body
            require_final_newline = FINAL_NEWLINE_ASK
            call insert("y", st)
            call type_keys("n")
            call save_buffer(1_int64, .true., st)
            call check(error, slurp(path("x")) == "xy", "declined")
            if (allocated(error)) exit body
            call insert("z", st)
            call type_keys("y")
            call save_buffer(1_int64, .true., st)
            call check(error, slurp(path("x")) == "xyz"//LF, "asked")
            if (allocated(error)) exit body
            call check(error, buffer_string() == "xyz"//LF, "added to the buffer")
            if (allocated(error)) exit body
            require_final_newline = FINAL_NEWLINE_NEVER
            call find_file(path("nl/f"), st)
            call insert("f", st)
            call type_keys("y")
            call save_buffer(1_int64, .true., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, slurp(path("nl/f")) == "f", "directory created")
            if (allocated(error)) exit body
            call get_buffer_create(" visit", buf, st)
            call set_buffer(buf, st)
            call insert("v", st)
            call write_region(path("new"), .false., st)
            call kill_buffer(buf, st)
            require_final_newline = FINAL_NEWLINE_VISIT
            call find_file(path("new"), st)
            call check(error, buffer_string() == "v"//LF, "visit")
        end block body

        require_final_newline = FINAL_NEWLINE_NEVER
        call find_file_noselect(path("x"), buf, st)
        if (st%ok()) call kill_buffer(buf, st)
        call find_file_noselect(path("nl/f"), buf, st)
        if (st%ok()) call kill_buffer(buf, st)
        call delete_file_internal(path("nl/f"), st)
        call delete_directory_internal(path("nl"), st)
        call kill_file_buffers()
    end subroutine test_final_newline

    !> write-file: a directory name takes the buffer's name, the major
    !> mode follows the new file name, and an executable file's new copy
    !> is executable.
    subroutine test_write_file(error)
        use files_fm, only: write_file
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf

        body: block
            call setup()
            call get_buffer_create("ro", buf, st)
            call set_buffer(buf, st)
            call insert("text", st)
            call write_file(path(""), .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, slurp(path("ro")) == "text", "file named after the buffer")
            if (allocated(error)) exit body
            call check(error, mode_name_of_current() == "fundamental_mode", "no mode for the name")
            if (allocated(error)) exit body
            call set_file_modes(path("ro"), int(o'755', int32), st)
            call write_file(path("new"), .false., st)
            call check(error, mode_name_of_current() == "fundamental_mode")
            if (allocated(error)) exit body
            call check(error, iand(file_modes(path("new")), int(o'111', int32)) == int(o'111', int32), "executable")
            if (allocated(error)) exit body
            call write_file(path("c.txt"), .false., st)
            call check(error, buffers(cur)%major_mode == "text_mode", "mode from the new name")
        end block body

        call kill_file_buffers()
    end subroutine test_write_file

    !> Write TEXT to FILE from a scratch buffer (another program's edit).
    subroutine put_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, buf
        type(status_type) :: st

        old = current_buffer()
        call get_buffer_create(" put", buf, st)
        call set_buffer(buf, st)
        call insert(text, st)
        call write_region(file, .false., st)
        call kill_buffer(buf, st)
        call set_buffer(old, st)
    end subroutine put_file

    !> Test that revert-buffer is successful.  Port of
    !> files-tests.el:files-tests-revert-buffer.
    subroutine test_revert_buffer(error)
        use files_fm, only: write_file, revert_buffer
        use buffer_fm, only: erase_buffer
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: nl = achar(10)
        character(len=*), parameter :: lao = "The Way that can be told of is not the eternal Way;"//nl// &
                                       "The name that can be named is not the eternal name."//nl// &
                                       "The Nameless is the origin of Heaven and Earth;"//nl// &
                                       "The Named is the mother of all things."//nl// &
                                       "Therefore let there always be non-being,"//nl// &
                                       "  so we may see their subtlety,"//nl// &
                                       "And let there always be being,"//nl// &
                                       "  so we may see their outcome."//nl// &
                                       "The two are the same,"//nl// &
                                       "But after they are produced,"//nl// &
                                       "  they have different names."//nl
        character(len=*), parameter :: tzu = "The Nameless is the origin of Heaven and Earth;"//nl// &
                                       "The named is the mother of all things."//nl// &
                                       nl// &
                                       "Therefore let there always be non-being,"//nl// &
                                       "  so we may see their subtlety,"//nl// &
                                       "And let there always be being,"//nl// &
                                       "  so we may see their outcome."//nl// &
                                       "The two are the same,"//nl// &
                                       "But after they are produced,"//nl// &
                                       "  they have different names."//nl// &
                                       "They both may be called deep and profound."//nl// &
                                       "Deeper and more profound,"//nl// &
                                       "The door of all subtleties!"//nl
        type(status_type) :: st
        type(handle_type) :: buf

        call setup()
        call get_buffer_create("x", buf, st)
        call set_buffer(buf, st)
        call insert(lao, st)
        call write_file(path("x"), .false., st)
        call erase_buffer(st)
        call insert(tzu, st)
        call revert_buffer(st, .true., .true., .true.)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), lao)
        call kill_buffer(buf, st)
    end subroutine test_revert_buffer

    !> Reverting a buffer whose file another program changed keeps point
    !> in the text they share and leaves the buffer unmodified, with its
    !> major mode; a read-only state set with C-x C-q survives.
    subroutine test_revert_buffer_replace(error)
        use files_fm, only: revert_buffer
        use simple_fm, only: read_only_mode
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: nl = achar(10)
        type(status_type) :: st
        type(handle_type) :: buf

        body: block
            call setup()
            call put_file(path("c.txt"), "abc"//nl//"def"//nl)
            call find_file_noselect(path("c.txt"), buf, st)
            call set_buffer(buf, st)
            call goto_char(6_int64)
            call put_file(path("c.txt"), "abc"//nl//"def"//nl//"ghi"//nl)
            call revert_buffer(st, .true., .true.)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), "abc"//nl//"def"//nl//"ghi"//nl)
            if (allocated(error)) exit body
            call check(error, point() == 6_int64, "point kept")
            if (allocated(error)) exit body
            call check(error, buffers(cur)%text%save_modiff >= buffers(cur)%text%modiff, "unmodified")
            if (allocated(error)) exit body
            call check(error, buffers(cur)%major_mode, "text_mode")
            if (allocated(error)) exit body
            call read_only_mode(st)
            call check(error, buffers(cur)%read_only, "C-x C-q")
            if (allocated(error)) exit body
            call put_file(path("c.txt"), "x"//nl)
            call revert_buffer(st, .true., .true.)
            call check(error, buffer_string() == "x"//nl .and. buffers(cur)%read_only, "still read-only")
        end block body

        call kill_file_buffers()
    end subroutine test_revert_buffer_replace

    !> A buffer whose file was never written can't be reverted.
    subroutine test_revert_nonexistent(error)
        use files_fm, only: revert_buffer
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf

        call setup()
        call find_file_noselect(path("new"), buf, st)
        call set_buffer(buf, st)
        call revert_buffer(st, .true., .true.)
        call check(error, format_error(st), "Cannot revert nonexistent file "//path("new"))
        call kill_file_buffers()
    end subroutine test_revert_nonexistent

    !> The current buffer's `major_mode'.
    function mode_name_of_current() result(mode)
        character(len=:), allocatable :: mode

        mode = "fundamental_mode"
        if (allocated(buffers(cur)%major_mode)) mode = buffers(cur)%major_mode
    end function mode_name_of_current
end module test_files_fm
