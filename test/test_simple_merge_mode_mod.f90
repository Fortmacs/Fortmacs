! test_simple_merge_mode_mod.f90 -- tests for simple_merge_mode_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/smerge-mode-tests.el.
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

!> The port of smerge-mode-tests.el (smerge-mode-test-empty-hunk), and
!> own tests of matching conflicts, the commands, their keys, diffs
!> between versions, automatic resolution (with diff and patch, skipped
!> without them) and VC's Git find-file hook on a conflicted file
!> (skipped without git).
module test_simple_merge_mode_mod
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, &
                          get_buffer, is_buffer_live, generate_new_buffer_name, restore_buffer_modified
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point, point_min, point_max, goto_char, buffer_substring
    use search_mod, only: re_search_forward, match_beginning, match_end, SEARCH_NOERROR
    use file_input_output_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, &
                                     create_directory_internal, write_region
    use files_mod, only: delete_directory, find_file_noselect, executable_find, save_buffer
    use execute_process_mod, only: execute_process
    use keyboard_mod, only: discard_input, kbd_store_bytes, command_loop, last_message
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use paragraphs_mod, only: paragraph_separate
    use bindings_mod, only: init_bindings
    use version_control_hooks_mod, only: vc_clear_context, vc_handled_backends, vc_backend_named, vc_state, vc_file_clearprops, &
                                         VC_CONFLICT
    use version_control_mod, only: vc_create_repo
    use simple_merge_mode_mod, only: match_smerge_conflict, next_smerge_conflict, previous_smerge_conflict, keep_smerge_upper, &
                                     keep_smerge_lower, &
                                     keep_smerge_base, keep_smerge_all, keep_smerge_current, delete_smerge_current, &
                                     combine_smerge_with_next, swap_smerge, start_smerge_session, resolve_smerge, &
                                     diff_smerge_upper_lower, smerge_conflict_style, smerge_repeat_map, smerge_parsep_regexp, &
                                     smerge_auto_leave, SMERGE_CONFLICT_STYLE_DIFF3_A
    implicit none(type, external)
    private

    public :: collect_simple_merge_mode_mod

    character, parameter :: LF = achar(10)

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-smerge/"

    !> A two-way conflict between two lines.
    character(len=*), parameter :: two_way = "a"//LF//"<<<<<<< mine"//LF//"UUU"//LF//"======="//LF//"LLL"//LF// &
                                   ">>>>>>> theirs"//LF//"z"//LF

    !> A three-way conflict (diff3 -A).
    character(len=*), parameter :: three_way = "a"//LF//"<<<<<<< mine"//LF//"UUU"//LF//"||||||| base"//LF// &
                                   "BBB"//LF//"======="//LF//"LLL"//LF//">>>>>>> theirs"//LF//"z"//LF

contains

    !> Collect all exported unit tests.
    subroutine collect_simple_merge_mode_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("smerge-mode-test-empty-hunk", test_empty_hunk), &
                    new_unittest("match-two-way", test_match_two_way), &
                    new_unittest("match-three-way", test_match_three_way), &
                    new_unittest("not-in-conflict", test_not_in_conflict), &
                    new_unittest("keep-upper-lower-base", test_keep), &
                    new_unittest("keep-all", test_keep_all), &
                    new_unittest("next-prev", test_next_prev), &
                    new_unittest("auto-leave", test_auto_leave), &
                    new_unittest("combine-with-next", test_combine), &
                    new_unittest("swap", test_swap), &
                    new_unittest("delete-current", test_delete_current), &
                    new_unittest("keys", test_keys), &
                    new_unittest("start-session", test_start_session), &
                    new_unittest("paragraph-separate", test_paragraph_separate), &
                    new_unittest("diff-upper-lower", test_diff_upper_lower), &
                    new_unittest("resolve-whitespace", test_resolve_whitespace), &
                    new_unittest("resolve-patch", test_resolve_patch), &
                    new_unittest("resolve-refine", test_resolve_refine), &
                    new_unittest("vc-git-conflict", test_vc_git_conflict) &
                    ]
    end subroutine collect_simple_merge_mode_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A fresh buffer holding TEXT, current, with point at the first
    !> occurrence of AT (point-min if absent).
    subroutine temp_buffer(h, text, at)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: at
        type(status_type) :: st
        integer(int64) :: found

        call init_bindings()
        call init_fileio()
        call discard_input()
        call get_or_make_buffer(generate_new_buffer_name(" *smerge-test*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = file_name_as_directory(fortmacs_wd)
        call insert(text, st)
        call goto_char(point_min())

        if (present(at)) then
            call re_search_forward(at, found, st, noerror=SEARCH_NOERROR)
            if (found /= 0) call goto_char(match_beginning(0, st))
        end if
    end subroutine temp_buffer

    !> Close buffer H.
    subroutine done(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        if (is_buffer_live(h)) call close_buffer(h, st)
    end subroutine done

    !> The text of subexpression N of the match data ("" if unmatched).
    function part(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        type(status_type) :: st

        text = ""
        if (match_end(n, st) == 0) return
        call buffer_substring(match_beginning(n, st), match_end(n, st), text, st)
    end function part

    !> Whether PROGRAM is on the path (else the test is skipped).
    logical function has_program(program, error)
        character(len=*), intent(in) :: program
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        call executable_find(program, path)
        has_program = len(path) > 0
        if (.not. has_program) call skip_test(error, "no "//program)
    end function has_program

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    !> Regression test for bug #25555.
    subroutine test_empty_hunk(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, "<<<<<<< one"//LF//"======="//LF//"LLL"//LF//">>>>>>> end"//LF)
        ! (insert "<<<<<<< one\n") (save-excursion (insert ...))
        call goto_char(point_min() + len("<<<<<<< one"//LF))
        call minor_modes%smerge_mode%enable(st)
        call keep_smerge_current(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), "")
        end block body

        call done(h)
    end subroutine test_empty_hunk

    !> The parts of a two-way conflict.
    subroutine test_match_two_way(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way, "UUU")
        call match_smerge_conflict(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, part(1), "UUU"//LF)
            if (allocated(error)) exit body
            call check(error, part(3), "LLL"//LF)
            if (allocated(error)) exit body
            call check(error, part(2), "")
            if (allocated(error)) exit body
            call check(error, part(0), two_way(3:len(two_way) - 2))
        end block body

        call done(h)
    end subroutine test_match_two_way

    !> The parts of a three-way conflict, and its style.
    subroutine test_match_three_way(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, three_way, "LLL")
        call match_smerge_conflict(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, part(1), "UUU"//LF)
            if (allocated(error)) exit body
            call check(error, part(2), "BBB"//LF)
            if (allocated(error)) exit body
            call check(error, part(3), "LLL"//LF)
            if (allocated(error)) exit body
            call check(error, smerge_conflict_style() == SMERGE_CONFLICT_STYLE_DIFF3_A, "diff3-A")
        end block body

        call done(h)
    end subroutine test_match_three_way

    !> Outside a conflict.
    subroutine test_not_in_conflict(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way)
        call match_smerge_conflict(st)
        call check(error, format_error(st), "Point not in conflict region")
        call done(h)
    end subroutine test_not_in_conflict

    !> u, l and b.
    subroutine test_keep(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        smerge_auto_leave = .true.
        call temp_buffer(h, three_way, "UUU")
        call keep_smerge_upper(st)
        call check(error, buffer_string(), "a"//LF//"UUU"//LF//"z"//LF)
        call done(h)
        if (allocated(error)) return
        call temp_buffer(h, three_way, "UUU")
        call keep_smerge_lower(st)
        call check(error, buffer_string(), "a"//LF//"LLL"//LF//"z"//LF)
        call done(h)
        if (allocated(error)) return
        call temp_buffer(h, three_way, "UUU")
        call keep_smerge_base(st)
        call check(error, buffer_string(), "a"//LF//"BBB"//LF//"z"//LF)
        call done(h)
        if (allocated(error)) return
        ! No base in a two-way conflict.
        call temp_buffer(h, two_way, "UUU")
        call keep_smerge_base(st)
        call check(error, format_error(st), "No `base'")
        call done(h)
    end subroutine test_keep

    !> a: all three versions.
    subroutine test_keep_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, three_way, "UUU")
        call keep_smerge_all(st)
        call check(error, buffer_string(), "a"//LF//"UUU"//LF//"BBB"//LF//"LLL"//LF//"z"//LF)
        call done(h)
        if (allocated(error)) return
        call temp_buffer(h, two_way, "UUU")
        call keep_smerge_all(st)
        call check(error, buffer_string(), "a"//LF//"UUU"//LF//"LLL"//LF//"z"//LF)
        call done(h)
    end subroutine test_keep_all

    !> n and p between two conflicts.
    subroutine test_next_prev(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: second

        call temp_buffer(h, two_way//two_way)
        second = len(two_way) + 3

        body: block
            call next_smerge_conflict(st)
            call check(error, point(), 3_int64)
            if (allocated(error)) exit body
            call next_smerge_conflict(st)
            call check(error, point(), second)
            if (allocated(error)) exit body
            ! At the last conflict: to its end (point-max).
            call next_smerge_conflict(st)
            call check(error, point(), point_max())
            if (allocated(error)) exit body
            call next_smerge_conflict(st)
            call check(error, format_error(st), "No next conflict")
            if (allocated(error)) exit body
            call previous_smerge_conflict(st)
            call check(error, point(), second)
            if (allocated(error)) exit body
            call previous_smerge_conflict(st)
            call check(error, point(), 3_int64)
            if (allocated(error)) exit body
            call previous_smerge_conflict(st)
            call check(error, format_error(st), "No previous conflict")
        end block body

        call done(h)
    end subroutine test_next_prev

    !> Smerge mode is turned off once the last conflict is resolved.
    subroutine test_auto_leave(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        smerge_auto_leave = .true.
        call temp_buffer(h, two_way//two_way, "UUU")
        call minor_modes%smerge_mode%enable(st)

        body: block
            call keep_smerge_upper(st)
            call check(error, minor_modes%smerge_mode%enabled(), "still on with a conflict left")
            if (allocated(error)) exit body
            call next_smerge_conflict(st)
            call keep_smerge_lower(st)
            call check(error,.not. minor_modes%smerge_mode%enabled(), "off with no conflict left")
            if (allocated(error)) exit body
            call check(error, buffer_string(), "a"//LF//"UUU"//LF//"z"//LF//"a"//LF//"LLL"//LF//"z"//LF)
        end block body

        call done(h)
    end subroutine test_auto_leave

    !> C: one conflict made of two.
    subroutine test_combine(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way//two_way, "UUU")
        call combine_smerge_with_next(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), "a"//LF//"<<<<<<< mine"//LF//"UUU"//LF//"z"//LF//"a"//LF//"UUU"//LF// &
                       "======="//LF//"LLL"//LF//"z"//LF//"a"//LF//"LLL"//LF//">>>>>>> theirs"//LF//"z"//LF)
        end block body

        call done(h)
    end subroutine test_combine

    !> Upper and lower swapped.
    subroutine test_swap(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way, "UUU")
        call swap_smerge(st)
        call check(error, buffer_string(), "a"//LF//"<<<<<<< mine"//LF//"LLL"//LF//"======="//LF//"UUU"//LF// &
                   ">>>>>>> theirs"//LF//"z"//LF)
        call done(h)
    end subroutine test_swap

    !> Removing the version at point keeps the other one.
    subroutine test_delete_current(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way, "UUU")
        call delete_smerge_current(st)
        call check(error, buffer_string(), "a"//LF//"LLL"//LF//"z"//LF)
        call done(h)
    end subroutine test_delete_current

    !> C-c ^ u in Smerge mode, and the repeat map.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way, "UUU")
        call show_alone()
        call set_buffer(h, st)
        call minor_modes%smerge_mode%enable(st)
        call kbd_store_bytes(achar(3)//"^u")
        call command_loop(st)

        body: block
            call check(error, buffer_string(), "a"//LF//"UUU"//LF//"z"//LF)
            if (allocated(error)) exit body
            call check(error, commands%next_smerge_conflict%repeat_map%index == smerge_repeat_map%index, "repeat map")
        end block body

        call done(h)
    end subroutine test_keys

    !> The session starts at the first conflict, or ends without one.
    subroutine test_start_session(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way)
        call start_smerge_session(st)

        body: block
            call check(error, minor_modes%smerge_mode%enabled(), "on")
            if (allocated(error)) exit body
            call check(error, point(), 3_int64)
        end block body

        call done(h)
        if (allocated(error)) return
        smerge_auto_leave = .true.
        call temp_buffer(h, "no conflict"//LF)
        call start_smerge_session(st)
        call check(error,.not. minor_modes%smerge_mode%enabled(), "off")
        call done(h)
    end subroutine test_start_session

    !> Conflict markers separate paragraphs while the mode is on.
    subroutine test_paragraph_separate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(h, two_way)
        call minor_modes%smerge_mode%enable(st)

        body: block
            call check(error, index(paragraph_separate%value, smerge_parsep_regexp) == 1, "added")
            if (allocated(error)) exit body
            call minor_modes%smerge_mode%disable(st)
            call check(error, index(paragraph_separate%value, smerge_parsep_regexp) == 0, "removed")
        end block body

        call done(h)
    end subroutine test_paragraph_separate

    !> C-c ^ = =: the upper and lower versions in *vc-diff*.
    subroutine test_diff_upper_lower(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, d
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (.not. has_program("diff", error)) return
        call temp_buffer(h, two_way, "UUU")
        call diff_smerge_upper_lower(st)
        d = get_buffer("*vc-diff*")

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, is_buffer_live(d), "*vc-diff*")
            if (allocated(error)) exit body
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, LF//"-UUU"//LF) > 0 .and. index(text, LF//"+LLL"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(current)%major_mode, "diff_mode")
        end block body

        call done(d)
        call done(h)
    end subroutine test_diff_upper_lower

    !> Only whitespace changed in the upper version: keep the lower one.
    subroutine test_resolve_whitespace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_program("diff", error)) return
        call temp_buffer(h, "<<<<<<< mine"//LF//"x  y"//LF//"||||||| base"//LF//"x y"//LF//"======="//LF//"x z"//LF// &
                         ">>>>>>> theirs"//LF, "x  y")
        call resolve_smerge(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), "x z"//LF)
        end block body

        call done(h)
    end subroutine test_resolve_whitespace

    !> Changes to different lines: the base's diff to the upper version
    !> applied to the lower one.
    subroutine test_resolve_patch(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_program("diff", error)) return
        if (.not. has_program("patch", error)) return
        call temp_buffer(h, "<<<<<<< mine"//LF//"X"//LF//"y"//LF//"z"//LF//"||||||| base"//LF//"x"//LF//"y"//LF//"z"//LF// &
                         "======="//LF//"x"//LF//"y"//LF//"Z"//LF//">>>>>>> theirs"//LF, "X")
        call resolve_smerge(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), "X"//LF//"y"//LF//"Z"//LF)
        end block body

        call done(h)
    end subroutine test_resolve_patch

    !> A two-way conflict refined into a smaller one by diff -b.
    subroutine test_resolve_refine(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_program("diff", error)) return
        call temp_buffer(h, "<<<<<<< mine"//LF//"a"//LF//"b"//LF//"c"//LF//"======="//LF//"a"//LF//"B"//LF//"c"//LF// &
                         ">>>>>>> theirs"//LF, "b")
        call resolve_smerge(st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffer_string(), "a"//LF//"<<<<<<< mine"//LF//"b"//LF//"======="//LF//"B"//LF// &
                       ">>>>>>> theirs"//LF//"c"//LF)
        end block body

        call done(h)
    end subroutine test_resolve_refine

    !> Visiting a file Git has a conflict in turns on Smerge mode, and
    !> saving it without markers marks the conflict resolved.
    subroutine test_vc_git_conflict(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, f, vc
        type(status_type) :: st
        character(len=:), allocatable :: file

        if (.not. has_program("git", error)) return
        call init_bindings()
        call init_fileio()
        call vc_clear_context()
        vc_handled_backends = [character(len=8) :: "Git"]
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(file_name_as_directory(fortmacs_wd)//"build", st)
        call create_directory_internal(tdir(), st)
        call temp_buffer(h, "")
        buffers(current)%directory = tdir()
        call vc_create_repo(vc_backend_named("Git"), st)
        file = tdir()//"f.txt"
        call write_file(file, "one"//LF)
        call git("add", "f.txt")
        call git("commit", "-m", "one")
        call git("checkout", "-b", "other")
        call write_file(file, "two"//LF)
        call git("commit", "-a", "-m", "two")
        call git("checkout", "-")
        call write_file(file, "three"//LF)
        call git("commit", "-a", "-m", "three")
        call git("merge", "other")
        call find_file_noselect(file, f, st)

        body: block
            call check(error, st%ok() .and. is_buffer_live(f), "visited: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(f, st)
            call check(error, minor_modes%smerge_mode%enabled(), "Smerge mode on")
            if (allocated(error)) exit body
            call check(error, allocated(last_message), "a message")
            if (allocated(error)) exit body
            call check(error, last_message, "There are unresolved conflicts in "//file)
            if (allocated(error)) exit body
            call keep_smerge_upper(st)
            call check(error, buffer_string(), "three"//LF)
            if (allocated(error)) exit body
            call save_buffer(1_int64, .false., st)
            call check(error, st%ok(), "saved: "//format_error(st))
            if (allocated(error)) exit body
            call vc_file_clearprops(file)
            call check(error, vc_state(file) /= VC_CONFLICT, "resolved")
        end block body

        if (is_buffer_live(f)) then
            call set_buffer(f, st)
            call restore_buffer_modified(.false.)
            call close_buffer(f, st)
        end if

        vc = get_buffer("*vc*")
        if (is_buffer_live(vc)) call close_buffer(vc, st)
        call done(h)
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        vc_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        call vc_clear_context()
    end subroutine test_vc_git_conflict

    !> The test repository directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdir)
    end function tdir

    !> Write TEXT to FILE (write-region ... 'nomessage).
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *smerge-file*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> Run git with ARGS in the test directory; a failure is reported on
    !> standard error (a merge with conflicts fails on purpose).
    subroutine git(a1, a2, a3, a4)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4
        type(value_type) :: args(8)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n

        args(1) = value_type(text="-c")
        args(2) = value_type(text="user.name=t")
        args(3) = value_type(text="-c")
        args(4) = value_type(text="user.email=t@t")
        args(5) = value_type(text=a1)
        n = 5
        if (present(a2)) call append(a2)
        if (present(a3)) call append(a3)
        if (present(a4)) call append(a4)
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *smerge-git*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = tdir()
        call execute_process("git", result, st, destination=value_type(.true.), args=make_list(args(1:n)))

        if (.not. st%ok()) then
            write (error_unit, "(a)") "git "//a1//": "//format_error(st)
        else if (vtag(result) == V_INTEGER) then
            if (xfixnum(result) /= 0 .and. a1 /= "merge") write (error_unit, "(a)") "git "//a1//" failed: "//buffer_string()
        end if

        call close_buffer(h, st)
        call set_buffer(old, st)

    contains

        subroutine append(a)
            character(len=*), intent(in) :: a

            n = n + 1
            args(n) = value_type(text=a)
        end subroutine append
    end subroutine git
end module test_simple_merge_mode_mod
