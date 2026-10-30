! test_diff_mode_fm.f90 -- tests for diff_mode_fm and diff_fm.
! Ports GNU Emacs 31.1 test/lisp/vc/diff-mode-tests.el in part; the
! overlay halves of the font-lock tests need diff refinement and syntax
! highlighting, which are deferred.
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

!> Ports of diff-mode-tests.el (the resource files are in
!> test/diff-mode-resources), and own tests of `diff' and Dired's `='.
!> Not ported: the overlay halves of diff-mode-test-font-lock and
!> diff-mode-test-font-lock-syntax-one-line (`diff_refine' and
!> `diff_font_lock_syntax' are deferred), the second check of
!> diff-mode-test-git-patch/before-first-hunk (it reads the face at the
!> match data Font Lock leaves behind) and
!> diff-mode-test-find-file-name-create (`read_file_name_function').
module test_diff_mode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, consp, nilp, symbolp, xsymbol, symbols, list_elements
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, current_buffer, &
                         get_buffer, get_file_buffer, buffer_live_p, restore_buffer_modified_p
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, point_min, point_max, buffer_string, narrow_to_region, widen
    use search_fm, only: re_search_forward, re_search_backward, match_beginning
    use textprop_fm, only: get_text_property
    use fileio_fm, only: init_fileio, emacs_wd, expand_file_name, file_name_as_directory, file_directory_p, &
                         make_directory_internal, write_region
    use files_fm, only: find_file_noselect, delete_directory
    use keyboard_fm, only: discard_input, kbd_store_bytes, command_loop
    use process_fm, only: accept_process_output, get_buffer_process
    use modes_fm, only: minor_modes
    use font_lock_fm, only: font_lock_ensure
    use bindings_fm, only: init_bindings
    use dired_mode_fm, only: dired, dired_goto_file, dired_buffers_for_dir
    use diff_mode_fm, only: diff_mode, diff_hunk_text, diff_text_type, diff_fixup_modifs, diff_hunk_file_names, &
                            diff_name_type, diff_apply_hunk, diff_hunk_next, diff_beginning_of_hunk, diff_end_of_hunk, &
                            diff_buffer_type, DIFF_BUFFER_PLAIN, DIFF_BUFFER_GIT, DIFF_BUFFER_HG
    use diff_fm, only: diff_no_select, diff_operand_type
    implicit none(type, external)
    private

    public :: collect_diff_mode_fm

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-diff-mode"

    character, parameter :: LF = achar(10), CR = achar(13)

    integer, parameter :: TIMEOUT = 600

    !> diff-mode-tests--git-patch.
    character(len=*), parameter :: git_patch = &
                                   "From 1234567890abcdef1234567890abcdef12345678 Mon Sep 17 00:00:00 2001"//LF// &
                                   "From: Alyssa P. Hacker <alyssa.p.hacker@example.com>"//LF// &
                                   "Date: Sun, 3 Mar 2025 10:30:00 -0400"//LF// &
                                   "Subject: [PATCH] Subtle bug fixes and slight improvements"//LF// &
                                   LF// &
                                   "- This is not a removed line"//LF// &
                                   "+ This is not an added line"//LF// &
                                   LF// &
                                   "---"//LF// &
                                   " src/main.py | 10 +++++-----"//LF// &
                                   " 1 file changed, 5 insertions(+), 5 deletions(-)"//LF// &
                                   LF// &
                                   "diff --git a/src/main.py b/src/main.py"//LF// &
                           "index 9f6c5fe43e47eab441232e54456c5c2b06297b65..7b3f91a8b4ed923c8f43183276e3ab36fe04f6c9 100644"//LF// &
                                   "--- a/src/main.py"//LF// &
                                   "+++ b/src/main.py"//LF// &
                                   "@@ -2,25 +2,24 @@"//LF// &
                                   LF// &
                                   " def main():"//LF// &
                                   "     # Initialize the magic number generator"//LF// &
                                   "-    magic_number = 42"//LF// &
                                   "-    print(""Magic number: "", magic_number)"//LF// &
                                   LF// &
                                   "-    # TODO: Fix the infinite loop"//LF// &
                                   "-    while True:"//LF// &
                                   "-        print(""This loop will never end"")"//LF// &
                                   "+    magic_number = 73  # After reconsidering, 73 seems more appropriate"//LF// &
                                   "+    print(""Updated magic number: "", magic_number)"//LF// &
                                   LF// &
                                   "+    # The infinite loop was probably not the best approach"//LF// &
                                   "+    # while True:"//LF// &
                                   "+    #     print(""This loop will never end."")"//LF// &
                                   LF// &
                                   "     # This part of the code handles other important tasks"//LF// &
                                   "     print(""Processing other tasks..."")"//LF// &
                                   LF// &
                                   "     # Error handling has been updated for clarity"//LF// &
                                   "-    if not fixed_it_yet:"//LF// &
                                   "-        print(""ERROR: Still broken!"")"//LF// &
                                   "+    if not fixed_it_yet:  # This should be fine now"//LF// &
                                   "+        print(""ERROR: No longer an issue."")"//LF// &
                                   LF// &
                                   "     # Exiting the function on a positive note"//LF// &
                                   "-    print(""Goodbye, cruel world!"")"//LF// &
                                   "+    print(""Goodbye, world!"")"//LF// &
                                   LF// &
                                   " if __name__ == ""__main__"":"//LF// &
                                   "     main()"//LF// &
                                   LF// &
                                   "-- "//LF// &
                                   "2.40.0"//LF

contains

    !> Whether `diff_command' and the shell can be run, skipping the test
    !> if not ((skip-unless (executable-find diff-command))).
    logical function have_diff(error)
        use diff_fm, only: diff_command
        use callproc_fm, only: shell_file_name
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: shell, command

        command = "diff"
        if (allocated(diff_command)) command = diff_command
        have_diff = on_path(command)

        if (have_diff) then
            shell = "/bin/sh"
            if (allocated(shell_file_name%value)) shell = shell_file_name%value
            have_diff = on_path(shell)
        end if

        if (.not. have_diff) call skip_test(error, "no "//command//" or shell")
    end function have_diff

    !> Whether PROGRAM is an executable file in a directory of PATH (or
    !> an absolute file name).
    logical function on_path(program) result(found)
        character(len=*), intent(in) :: program
        character(len=:), allocatable :: path, dir
        integer :: n, st, k

        found = .false.

        if (index(program, "/") == 1) then
            inquire (file=program, exist=found)
            return
        end if

        call get_environment_variable("PATH", length=n, status=st)
        if (st /= 0) return
        allocate (character(len=n) :: path)
        call get_environment_variable("PATH", path)

        do while (len(path) > 0)
            k = index(path, ":")
            if (k == 0) k = len(path) + 1
            dir = path(1:k - 1)

            if (k > len(path)) then
                path = ""
            else
                path = path(k + 1:)
            end if

            if (len(dir) == 0) cycle
            inquire (file=dir//"/"//program, exist=found)
            if (found) return
        end do
    end function on_path

    !> Collect all exported unit tests.
    subroutine collect_diff_mode_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("diff-mode-test-ignore-trailing-dashes", test_ignore_trailing_dashes), &
                    new_unittest("diff-mode-test-hunk-text-no-newline", test_hunk_text_no_newline), &
                    new_unittest("diff-mode-test-font-lock", test_font_lock), &
                    new_unittest("diff-mode-test-font-lock-syntax-one-line", test_font_lock_one_line), &
                    new_unittest("test-hunk-file-names", test_hunk_file_names), &
                    new_unittest("diff-mode-test-fixups-added-lines", test_fixups_added_lines), &
                    new_unittest("diff-mode-test-fixups-empty-hunks", test_fixups_empty_hunks), &
                    new_unittest("diff-mode-test-git-patch", test_git_patch), &
                    new_unittest("diff-mode-test-git-patch/before-first-hunk", test_git_patch_before_first_hunk), &
                    new_unittest("diff-mode-test-git-patch/signature", test_git_patch_signature), &
                    new_unittest("diff-mode-test-topmost-addition-undo", test_topmost_addition_undo), &
                    new_unittest("diff-mode-test-bottommost-addition-undo", test_bottommost_addition_undo), &
                    new_unittest("diff-mode-test-setup-buffer-type", test_setup_buffer_type), &
                    new_unittest("diff-files", test_diff_files), &
                    new_unittest("dired-diff", test_dired_diff) &
                    ]
    end subroutine collect_diff_mode_fm

    !> The absolute name of the test directory, as a directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    !> A fresh, empty test directory; *scratch* current and shown alone.
    subroutine setup()
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call discard_input()
        call scratch()
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        call make_directory_internal(tdir(), st)
    end subroutine setup

    !> Remove the test directory.
    subroutine teardown()
        type(status_type) :: st
        type(handle_type), allocatable :: bufs(:)
        integer :: k

        call discard_input()
        bufs = dired_buffers_for_dir(tdir())

        do k = 1, size(bufs)
            call kill_buffer(bufs(k), st)
        end do

        call scratch()
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
    end subroutine teardown

    !> Make *scratch* current, shown alone.
    subroutine scratch()
        type(handle_type) :: buf
        type(status_type) :: st

        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
        call show_alone()
        call set_buffer(buf, st)
    end subroutine scratch

    !> Create the file NAME (relative to the test directory) holding TEXT.
    subroutine make_file(name, text)
        character(len=*), intent(in) :: name, text
        type(handle_type) :: buf, old
        type(status_type) :: st

        old = current_buffer()
        call get_buffer_create(" *diff-test-file*", buf, st)
        call set_buffer(buf, st)
        call erase_buffer(st)
        call insert(text, st)
        call write_region(tdir()//name, .false., st)
        call kill_buffer(buf, st)
        call set_buffer(old, st)
    end subroutine make_file

    !> The text of buffer H.
    function text_of(h) result(s)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: s
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(h, st)
        s = buffer_string()
        call set_buffer(old, st)
    end function text_of

    !> Kill the file buffer H without saving it.
    subroutine discard(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: old
        type(status_type) :: st

        if (.not. buffer_live_p(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        call restore_buffer_modified_p(.false.)
        call set_buffer(old, st)
        call kill_buffer(h, st)
    end subroutine discard

    !> A fresh buffer NAME holding TEXT, current, in DIR (the test
    !> directory if absent), as with-temp-buffer and cd do.
    subroutine temp_buffer(name, text, h, dir)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        character(len=*), intent(in), optional :: dir
        type(status_type) :: st

        call get_buffer_create(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)

        if (present(dir)) then
            buffers(cur)%directory = dir
        else
            buffers(cur)%directory = tdir()
        end if

        call insert(text, st)
    end subroutine temp_buffer

    !> The name of the face at POS ("" if none; the first of a list).
    function face_at(pos) result(name)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: name
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        name = ""
        call get_text_property(pos, sym("face"), v, st)

        if (consp(v)) then
            call list_elements(v, elts)
            v = elts(1)
        end if

        if (nilp(v)) return
        if (symbolp(v)) name = symbols(xsymbol(v))%name
    end function face_at

    !> Whether the face over the 0-based range [FROM, TO) of the
    !> accessible text is FACE throughout.
    logical function face_over(from, to, face) result(ok)
        integer, intent(in) :: from, to
        character(len=*), intent(in) :: face
        integer :: k

        ok = .true.

        do k = from, to - 1
            if (face_at(point_min() + k) /= face) then
                ok = .false.
                return
            end if
        end do
    end function face_over

    !> (re-search-forward RE) then the face at its match beginning.
    function face_after_search(re, backward) result(name)
        character(len=*), intent(in) :: re
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: name
        integer(int64) :: p
        type(status_type) :: st
        logical :: back

        back = .false.
        if (present(backward)) back = backward

        if (back) then
            call re_search_backward(re, p, st)
        else
            call re_search_forward(re, p, st)
        end if

        name = "<not found>"
        if (p == 0 .or. .not. st%ok()) return
        name = face_at(match_beginning(0, st))
    end function face_after_search

    !> Wait until the process of buffer BUF has exited.
    subroutine wait_process(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got

        call system_clock(start, rate)

        do
            proc = get_buffer_process(buf)
            if (proc%index == 0) exit
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > TIMEOUT) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_process

    !> Check to make sure we successfully ignore trailing -- made by
    !> 'git format-patch'.  This is bug #9597.
    subroutine test_ignore_trailing_dashes(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: patch = &
                                       "From 18ed35640be496647e0a02fc155b4ee4a0490eca Mon Sep 17 00:00:00 2001"//LF// &
                                       "From: Dima Kogan <dima@secretsauce.net>"//LF// &
                                       "Date: Mon, 30 Jan 2017 22:24:13 -0800"//LF// &
                                       "Subject: [PATCH] test commit"//LF// &
                                       LF// &
                                       "---"//LF// &
                                       " fil  | 3 ---"//LF// &
                                       " fil2 | 4 ----"//LF// &
                                       " 2 files changed, 7 deletions(-)"//LF// &
                                       LF// &
                                       "diff --git a/fil b/fil"//LF// &
                                       "index 10344f1..2a56245 100644"//LF// &
                                       "--- a/fil"//LF// &
                                       "+++ b/fil"//LF// &
                                       "@@ -2,10 +2,8 @@ Afrocentrism"//LF// &
                                       " Americanisms"//LF// &
                                       " Americanization"//LF// &
                                       " Americanizations"//LF// &
                                       "-Americanized"//LF// &
                                       " Americanizes"//LF// &
                                       " Americanizing"//LF// &
                                       "-Andrianampoinimerina"//LF// &
                                       " Anglicanisms"//LF// &
                                       " Antananarivo"//LF// &
                                       " Apalachicola"//LF// &
                                       "@@ -15,6 +13,5 @@ Aristophanes"//LF// &
                                       " Aristotelian"//LF// &
                                       " Ashurbanipal"//LF// &
                                       " Australopithecus"//LF// &
                                       "-Austronesian"//LF// &
                                       " Bangladeshis"//LF// &
                                       " Barquisimeto"//LF// &
                                       "diff --git a/fil2 b/fil2"//LF// &
                                       "index 8858f0d..86e8ea5 100644"//LF// &
                                       "--- a/fil2"//LF// &
                                       "+++ b/fil2"//LF// &
                                       "@@ -1,20 +1,16 @@"//LF// &
                                       " whippoorwills"//LF// &
                                       " whitewashing"//LF// &
                                       " wholehearted"//LF// &
                                       "-wholeheartedly"//LF// &
                                       " wholesomeness"//LF// &
                                       " wildernesses"//LF// &
                                       " windbreakers"//LF// &
                                       " wisecracking"//LF// &
                                       " withstanding"//LF// &
                                       "-woodcarvings"//LF// &
                                       " woolgathering"//LF// &
                                       " workstations"//LF// &
                                       " worthlessness"//LF// &
                                       " wretchedness"//LF// &
                                       " wristwatches"//LF// &
                                       "-wrongfulness"//LF// &
                                       " wrongheadedly"//LF// &
                                       " wrongheadedness"//LF// &
                                       "-xylophonists"//LF// &
                                       " youthfulness"//LF// &
                                       "--"//LF// &
                                       "2.11.0"//LF// &
                                       LF
        character(len=*), parameter :: fil_before = &
                                       "Afrocentrism"//LF//"Americanisms"//LF//"Americanization"//LF//"Americanizations"//LF// &
                                       "Americanized"//LF//"Americanizes"//LF//"Americanizing"//LF//"Andrianampoinimerina"//LF// &
                                       "Anglicanisms"//LF//"Antananarivo"//LF//"Apalachicola"//LF//"Appalachians"//LF// &
                                       "Argentinians"//LF//"Aristophanes"//LF//"Aristotelian"//LF//"Ashurbanipal"//LF// &
                                       "Australopithecus"//LF//"Austronesian"//LF//"Bangladeshis"//LF//"Barquisimeto"//LF
        character(len=*), parameter :: fil_after = &
                                       "Afrocentrism"//LF//"Americanisms"//LF//"Americanization"//LF//"Americanizations"//LF// &
                                       "Americanizes"//LF//"Americanizing"//LF// &
                                       "Anglicanisms"//LF//"Antananarivo"//LF//"Apalachicola"//LF//"Appalachians"//LF// &
                                       "Argentinians"//LF//"Aristophanes"//LF//"Aristotelian"//LF//"Ashurbanipal"//LF// &
                                       "Australopithecus"//LF//"Bangladeshis"//LF//"Barquisimeto"//LF
        character(len=*), parameter :: fil2_before = &
                                       "whippoorwills"//LF//"whitewashing"//LF//"wholehearted"//LF//"wholeheartedly"//LF// &
                                       "wholesomeness"//LF//"wildernesses"//LF//"windbreakers"//LF//"wisecracking"//LF// &
                                       "withstanding"//LF//"woodcarvings"//LF//"woolgathering"//LF//"workstations"//LF// &
                                       "worthlessness"//LF//"wretchedness"//LF//"wristwatches"//LF//"wrongfulness"//LF// &
                                       "wrongheadedly"//LF//"wrongheadedness"//LF//"xylophonists"//LF//"youthfulness"//LF
        character(len=*), parameter :: fil2_after = &
                                       "whippoorwills"//LF//"whitewashing"//LF//"wholehearted"//LF// &
                                       "wholesomeness"//LF//"wildernesses"//LF//"windbreakers"//LF//"wisecracking"//LF// &
                                       "withstanding"//LF//"woolgathering"//LF//"workstations"//LF// &
                                       "worthlessness"//LF//"wretchedness"//LF//"wristwatches"//LF// &
                                       "wrongheadedly"//LF//"wrongheadedness"//LF//"youthfulness"//LF
        type(handle_type) :: buf, buf2, tmp
        type(status_type) :: st
        integer :: k

        call setup()
        call make_file("fil", fil_before)
        call make_file("fil2", fil2_before)
        call find_file_noselect(tdir()//"fil", buf, st)
        call find_file_noselect(tdir()//"fil2", buf2, st)

        body: block
            call temp_buffer("*diff-test*", patch, tmp)
            call goto_char(point_min())

            do k = 1, 3
                call diff_apply_hunk(st)
                call check(error, st%ok(), "apply hunk: "//format_error(st))
                if (allocated(error)) exit body
            end do

            call check(error, text_of(buf), fil_after)
            if (allocated(error)) exit body
            call check(error, text_of(buf2), fil2_after)
        end block body

        call kill_buffer(tmp, st)
        call discard(buf)
        call discard(buf2)
        call teardown()
    end subroutine test_ignore_trailing_dashes

    !> The source (or with DESTP the destination) text of HUNK.
    function hunk_text(hunk, destp) result(s)
        character(len=*), intent(in) :: hunk
        logical, intent(in) :: destp
        character(len=:), allocatable :: s
        type(diff_text_type) :: res
        type(status_type) :: st

        call diff_hunk_text(hunk, destp, -1_int64, res, st)
        s = "<error>"
        if (st%ok()) s = res%text
    end function hunk_text

    !> Check output of `diff_hunk_text' with no newline at end of file.
    subroutine test_hunk_text_no_newline(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: nonl = "\ No newline at end of file"//LF
        character(len=:), allocatable :: hunk

        ! First check unified change/remove/add cases with newline
        hunk = "@@ -1 +1 @@"//LF//"-foo"//LF//"+bar"//LF
        call pair(hunk, "foo"//LF, "bar"//LF)
        if (allocated(error)) return
        hunk = "@@ -1 +0,0 @@"//LF//"-foo"//LF
        call pair(hunk, "foo"//LF, "")
        if (allocated(error)) return
        hunk = "@@ -0,0 +1 @@"//LF//"+bar"//LF
        call pair(hunk, "", "bar"//LF)
        if (allocated(error)) return
        ! Check unified change/remove cases with no newline in old file
        hunk = "@@ -1 +1 @@"//LF//"-foo"//LF//nonl//"+bar"//LF
        call pair(hunk, "foo", "bar"//LF)
        if (allocated(error)) return
        hunk = "@@ -1 +0,0 @@"//LF//"-foo"//LF//nonl
        call pair(hunk, "foo", "")
        if (allocated(error)) return
        ! Check unified change/add cases with no newline in new file
        hunk = "@@ -1 +1 @@"//LF//"-foo"//LF//"+bar"//LF//nonl
        call pair(hunk, "foo"//LF, "bar")
        if (allocated(error)) return
        hunk = "@@ -0,0 +1 @@"//LF//"+bar"//LF//nonl
        call pair(hunk, "", "bar")
        if (allocated(error)) return
        ! Check unified change case with no newline in both old/new file
        hunk = "@@ -1 +1 @@"//LF//"-foo"//LF//nonl//"+bar"//LF//nonl
        call pair(hunk, "foo", "bar")
        if (allocated(error)) return
        ! Check context-after unified change case with no newline in both
        ! old/new file
        hunk = "@@ -1,2 +1,2 @@"//LF//"-foo"//LF//"+bar"//LF//" baz"//LF//nonl
        call pair(hunk, "foo"//LF//"baz", "bar"//LF//"baz")
        if (allocated(error)) return
        hunk = "@@ -1,2 +1,2 @@"//LF//"-foo"//LF//"-baz"//LF//nonl//"+bar"//LF//"+baz"//LF
        call pair(hunk, "foo"//LF//"baz", "bar"//LF//"baz"//LF)
        if (allocated(error)) return
        hunk = "@@ -1,2 +1,2 @@"//LF//"-foo"//LF//"-baz"//LF//"+bar"//LF//"+baz"//LF//nonl
        call pair(hunk, "foo"//LF//"baz"//LF, "bar"//LF//"baz")

    contains

        subroutine pair(h, old, new)
            character(len=*), intent(in) :: h, old, new

            call check(error, hunk_text(h, .false.), old, "source of "//h)
            if (allocated(error)) return
            call check(error, hunk_text(h, .true.), new, "destination of "//h)
        end subroutine pair
    end subroutine test_hunk_text_no_newline

    !> diff -u OLD NEW of the resource files, synchronously, into *Diff*,
    !> narrowed to the first hunk; DIFF_BEG is its start.
    subroutine resource_diff(old, new, h, diff_beg, status)
        character(len=*), intent(in) :: old, new
        type(handle_type), intent(out) :: h
        integer(int64), intent(out) :: diff_beg
        type(status_type), intent(out) :: status
        type(diff_operand_type) :: o, n
        type(handle_type) :: res, old_buf
        integer(int64) :: e
        character(len=:), allocatable :: dir

        call expand_file_name("test/diff-mode-resources/", dir, status, emacs_wd)
        if (.not. status%ok()) return
        old_buf = current_buffer()
        buffers(cur)%directory = dir
        call get_buffer_create("*Diff*", h, status)
        o%file = old
        n%file = new
        call diff_no_select(o, n, res, status, "-u", .true., h)
        call set_buffer(old_buf, status)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
        if (.not. status%ok()) return
        call set_buffer(h, status)
        call minor_modes%font_lock_mode%enable(status)
        call font_lock_ensure(status)
        call goto_char(point_min())
        call diff_hunk_next(status)
        if (.not. status%ok()) return
        diff_beg = diff_beginning_of_hunk(status)
        if (.not. status%ok()) return
        e = diff_end_of_hunk()
        call narrow_to_region(diff_beg, e, status)
    end subroutine resource_diff

    !> Check font-locking of diff hunks (the text properties; the overlays
    !> of `diff_refine' and `diff_font_lock_syntax' are deferred).
    subroutine test_font_lock(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: diff_beg

        if (.not. have_diff(error)) return
        call init_bindings()
        call init_fileio()
        call scratch()
        call resource_diff("hello_world.c", "hello_emacs.c", h, diff_beg, st)

        body: block
            call check(error, st%ok(), "diff: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), &
                       "@@ -1,6 +1,6 @@"//LF// &
                       " #include <stdio.h>"//LF// &
                       " int main()"//LF// &
                       " {"//LF// &
                       "-  printf(""Hello, World!\n"");"//LF// &
                       "+  printf(""Hello, Emacs!\n"");"//LF// &
                       "   return 0;"//LF// &
                       " }"//LF)
            if (allocated(error)) exit body
            call check(error, face_over(0, 15, "diff_hunk_header") .and. face_at(point_min() + 15) == "" .and. &
                       face_over(16, 51, "diff_context") .and. face_over(51, 52, "diff_indicator_removed") .and. &
                       face_over(52, 81, "diff_removed") .and. face_over(81, 82, "diff_indicator_added") .and. &
                       face_over(82, 111, "diff_added") .and. face_over(111, 127, "diff_context"), "faces")
        end block body

        call set_buffer(h, st)
        call widen()
        call kill_buffer(h, st)
        call scratch()
    end subroutine test_font_lock

    !> Check diff highlighting for one line with no newline at end (the
    !> text properties; `diff_font_lock_syntax' is deferred).
    subroutine test_font_lock_one_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: diff_beg

        if (.not. have_diff(error)) return
        call init_bindings()
        call init_fileio()
        call scratch()
        call resource_diff("hello_world_1.c", "hello_emacs_1.c", h, diff_beg, st)

        body: block
            call check(error, st%ok(), "diff: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), &
                       "@@ -1 +1 @@"//LF// &
                       "-int main() { printf(""Hello, World!\n""); return 0; }"//LF// &
                       "\ No newline at end of file"//LF// &
                       "+int main() { printf(""Hello, Emacs!\n""); return 0; }"//LF// &
                       "\ No newline at end of file"//LF)
            if (allocated(error)) exit body
            call check(error, face_over(0, 11, "diff_hunk_header") .and. face_at(point_min() + 11) == "" .and. &
                       face_over(12, 13, "diff_indicator_removed") .and. face_over(13, 65, "diff_removed") .and. &
                       face_over(65, 93, "diff_context") .and. face_over(93, 94, "diff_indicator_added") .and. &
                       face_over(94, 146, "diff_added") .and. face_over(146, 174, "diff_context"), "faces")
        end block body

        call set_buffer(h, st)
        call widen()
        call kill_buffer(h, st)
        call scratch()
    end subroutine test_font_lock_one_line

    subroutine test_hunk_file_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(diff_name_type), allocatable :: names(:)

        call temp_buffer("*diff-test*", "diff -c /tmp/ange-ftp13518wvE.el /tmp/ange-ftp1351895K.el"//LF, h, "/tmp/")
        call goto_char(point_min())
        call diff_hunk_file_names(names)
        call check(error, size(names) == 2, "two names")
        if (allocated(error)) return
        call check(error, names(1)%s, "/tmp/ange-ftp1351895K.el")
        if (allocated(error)) return
        call check(error, names(2)%s, "/tmp/ange-ftp13518wvE.el")
        if (allocated(error)) return
        call temp_buffer("*diff-test*", "diff -c -L /ftp:slbhao:/home/albinus/src/tramp/lisp/tramp.el -L "// &
                         "/ftp:slbhao:/home/albinus/src/emacs/lisp/net/tramp.el /tmp/ange-ftp13518wvE.el "// &
                         "/tmp/ange-ftp1351895K.el"//LF, h, "/tmp/")
        call goto_char(point_min())
        call diff_hunk_file_names(names)
        call check(error, size(names) == 2, "two names with labels")
        if (allocated(error)) return
        call check(error, names(1)%s, "/tmp/ange-ftp1351895K.el")
        if (allocated(error)) return
        call check(error, names(2)%s, "/tmp/ange-ftp13518wvE.el")
        call kill_buffer(h, st)
    end subroutine test_hunk_file_names

    !> `diff_fixup_modifs' of PATCH gives FIXED.
    subroutine check_fixup(error, patch, fixed)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: patch, fixed
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("*diff-test*", patch, h, "/tmp/")
        call diff_fixup_modifs(point_min(), point_max(), st)
        call check(error, st%ok(), "fixup: "//format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), fixed)
        call kill_buffer(h, st)
    end subroutine check_fixup

    !> Check that `diff_fixup_modifs' works well with hunks with added
    !> lines.
    subroutine test_fixups_added_lines(error)
        type(error_type), allocatable, intent(out) :: error

        call check_fixup(error, "--- file"//LF//"+++ file"//LF//"@@ -0,0 +1,15 @@"//LF// &
                         "+1"//LF//"+2"//LF//"+3"//LF//"+4"//LF, &
                         "--- file"//LF//"+++ file"//LF//"@@ -0,0 +1,4 @@"//LF// &
                         "+1"//LF//"+2"//LF//"+3"//LF//"+4"//LF)
        if (allocated(error)) return
        call check_fixup(error, "--- file"//LF//"+++ file"//LF//"@@ -389,5 +398,6 @@"//LF// &
                         "       while (1)"//LF//"               ;"//LF//"+       # not needed"//LF// &
                         "        # at all"//LF//"        # stop"//LF, &
                         "--- file"//LF//"+++ file"//LF//"@@ -389,4 +398,5 @@"//LF// &
                         "       while (1)"//LF//"               ;"//LF//"+       # not needed"//LF// &
                         "        # at all"//LF//"        # stop"//LF)
    end subroutine test_fixups_added_lines

    !> Check that `diff_fixup_modifs' works well with empty hunks.
    subroutine test_fixups_empty_hunks(error)
        type(error_type), allocatable, intent(out) :: error

        call check_fixup(error, "--- file"//LF//"+++ file"//LF//"@@ -1 +1 @@"//LF//"-1"//LF// &
                         "@@ -10 +10 @@"//LF//"-1"//LF//"+1"//LF// &
                         "--- otherfile"//LF//"+++ otherfile"//LF//"@@ -1 +1 @@"//LF//"+2"//LF// &
                         "@@ -10 +10 @@"//LF//"-1"//LF//"+1"//LF, &
                         "--- file"//LF//"+++ file"//LF//"@@ -1,1 +1,0 @@"//LF//"-1"//LF// &
                         "@@ -10,1 +10,1 @@"//LF//"-1"//LF//"+1"//LF// &
                         "--- otherfile"//LF//"+++ otherfile"//LF//"@@ -1,0 +1,1 @@"//LF//"+2"//LF// &
                         "@@ -10,1 +10,1 @@"//LF//"-1"//LF//"+1"//LF)
    end subroutine test_fixups_empty_hunks

    !> A Diff mode buffer holding `git_patch', fontified.
    subroutine git_patch_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call temp_buffer("*diff-test*", git_patch, h, "/tmp/")
        call diff_mode(st)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
        call goto_char(point_min())
    end subroutine git_patch_buffer

    subroutine test_git_patch(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call git_patch_buffer(h)
        call check(error, face_after_search("magic_number = 42"), "diff_removed")
        if (.not. allocated(error)) call check(error, face_after_search("magic_number = 73"), "diff_added")
        call kill_buffer(h, st)
    end subroutine test_git_patch

    !> The removed-looking line of the preamble is context.  (Upstream's
    !> second check reads the face at the match data that
    !> `font_lock_ensure' leaves behind, not at the added-looking line,
    !> which has no face; it is not ported.)
    subroutine test_git_patch_before_first_hunk(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call git_patch_buffer(h)
        call check(error, face_after_search("This is not a removed line"), "diff_context")
        call kill_buffer(h, st)
    end subroutine test_git_patch_before_first_hunk

    subroutine test_git_patch_signature(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call git_patch_buffer(h)
        call goto_char(point_max())
        call check(error, face_after_search("^-- $", .true.), "diff_context")
        call kill_buffer(h, st)
    end subroutine test_git_patch_signature

    !> Undo the addition of PATCH to the file fruits holding AFTER, giving
    !> BEFORE: by the REVERSE argument, then by the dwim behaviour.
    subroutine check_addition_undo(error, patch, before, after)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: patch, before, after
        type(handle_type) :: buf_after, tmp
        type(status_type) :: st

        call setup()
        call make_file("fruits", after)
        call find_file_noselect(tdir()//"fruits", buf_after, st)

        body: block
            call temp_buffer("*diff-test*", patch, tmp)
            call goto_char(point_min())
            call diff_hunk_next(st)
            ! Undo hunk by true REVERSE argument (C-u C-c C-a)
            call diff_apply_hunk(st, reverse=.true.)
            call check(error, st%ok(), "reverse: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(buf_after), before)
            if (allocated(error)) exit body
            call kill_buffer(tmp, st)
            call set_buffer(buf_after, st)
            call erase_buffer(st)
            call insert(after, st)
            call temp_buffer("*diff-test*", patch, tmp)
            call goto_char(point_min())
            call diff_hunk_next(st)
            ! Undo hunk by dwim behavior
            call kbd_store_bytes("y")
            call diff_apply_hunk(st)
            call check(error, st%ok(), "dwim: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(buf_after), before)
        end block body

        call kill_buffer(tmp, st)
        call discard(buf_after)
        call teardown()
    end subroutine check_addition_undo

    subroutine test_topmost_addition_undo(error)
        type(error_type), allocatable, intent(out) :: error

        call check_addition_undo(error, "diff --git a/fruits b/fruits"//LF//"index 0dcecd3..d0eb2e7 100644"//LF// &
                                 "--- a/fruits"//LF//"+++ b/fruits"//LF//"@@ -1,2 +1,3 @@"//LF// &
                                 "+fruits"//LF//" apple"//LF//" orange"//LF, &
                                 "apple"//LF//"orange"//LF, "fruits"//LF//"apple"//LF//"orange"//LF)
    end subroutine test_topmost_addition_undo

    subroutine test_bottommost_addition_undo(error)
        type(error_type), allocatable, intent(out) :: error

        call check_addition_undo(error, "diff --git a/fruits b/fruits"//LF//"index 0dcecd3..6f210ff 100644"//LF// &
                                 "--- a/fruits"//LF//"+++ b/fruits"//LF//"@@ -1,2 +1,3 @@"//LF// &
                                 " apple"//LF//" orange"//LF//"+plum"//LF, &
                                 "apple"//LF//"orange"//LF, "apple"//LF//"orange"//LF//"plum"//LF)
    end subroutine test_bottommost_addition_undo

    !> Check that `diff_setup_buffer_type' recognizes the diff's origin.
    subroutine test_setup_buffer_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call temp_buffer("*diff-test*", "diff --git a/foo b/foo"//LF//"--- a/foo"//LF//"+++ b/foo"//LF, h, "/tmp/")
        call diff_mode(st)
        call check(error, diff_buffer_type() == DIFF_BUFFER_GIT, "git")
        call kill_buffer(h, st)
        if (allocated(error)) return
        call temp_buffer("*diff-test*", "diff -r 0123456789ab foo"//LF//"--- a/foo"//LF//"+++ b/foo"//LF, h, "/tmp/")
        call diff_mode(st)
        call check(error, diff_buffer_type() == DIFF_BUFFER_HG, "hg")
        call kill_buffer(h, st)
        if (allocated(error)) return
        call temp_buffer("*diff-test*", "--- foo"//LF//"+++ foo"//LF//"@@ -1 +1 @@"//LF//"-a"//LF//"+b"//LF, h, "/tmp/")
        call diff_mode(st)
        call check(error, diff_buffer_type() == DIFF_BUFFER_PLAIN, "plain")
        call kill_buffer(h, st)
    end subroutine test_setup_buffer_type

    !> M-x diff of two files: the command line, the unified diff in Diff
    !> mode and the closing line.
    subroutine test_diff_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (.not. have_diff(error)) return
        call setup()
        call make_file("old.txt", "one"//LF//"two"//LF)
        call make_file("new.txt", "one"//LF//"three"//LF)
        buffers(cur)%directory = tdir()
        call kbd_store_bytes(achar(27)//"xdiff"//CR//"new.txt"//CR//"old.txt"//CR)
        call command_loop(st)
        h = get_buffer("*Diff*")

        body: block
            call check(error, buffer_live_p(h), "*Diff* exists")
            if (allocated(error)) exit body
            call wait_process(h)
            text = text_of(h)
            call check(error, index(text, "diff -u "//tdir()//"old.txt "//tdir()//"new.txt"//LF) == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, LF//"-two"//LF//"+three"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, LF//"Diff finished.  ") > 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(h%index)%major_mode, "diff_mode")
        end block body

        if (buffer_live_p(h)) call kill_buffer(h, st)
        call teardown()
    end subroutine test_diff_files

    !> Dired's = compares the file at point with its backup.
    subroutine test_dired_diff(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: p
        character(len=:), allocatable :: text

        if (.not. have_diff(error)) return
        call setup()
        call make_file("a.txt", "alpha2"//LF)
        call make_file("a.txt~", "alpha"//LF)
        call dired(tdir(), st)
        call show_alone()
        p = dired_goto_file(tdir()//"a.txt", st)
        call kbd_store_bytes("="//CR)
        call command_loop(st)
        h = get_buffer("*Diff*")

        body: block
            call check(error, buffer_live_p(h), "*Diff* exists")
            if (allocated(error)) exit body
            call wait_process(h)
            text = text_of(h)
            call check(error, index(text, "diff -u "//tdir()//"a.txt\~ "//tdir()//"a.txt"//LF) == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, LF//"-alpha"//LF//"+alpha2"//LF) > 0, text)
        end block body

        if (buffer_live_p(h)) call kill_buffer(h, st)
        call teardown()
    end subroutine test_dired_diff
end module test_diff_mode_fm
