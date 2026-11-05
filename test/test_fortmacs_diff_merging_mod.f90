! test_fortmacs_diff_merging_mod.f90 -- tests for fortmacs_diff_merging_mod and Ediff's merge jobs.
! Own tests; upstream has no tests of ediff-merg.el or of merging.
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

!> Merging two buffers with Ediff, with and without an ancestor: the
!> merge buffer and its default variant, the states of the regions, the
!> merge keys (a, b, r, +, &, $$, $*), the windows, the help, and saving
!> the merge in a file.
module test_fortmacs_diff_merging_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, &
                          get_buffer, is_buffer_live, restore_buffer_modified
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string
    use keyboard_mod, only: discard_input, kbd_store_bytes, command_loop
    use bindings_mod, only: init_bindings
    use window_mod, only: is_window_live, window_top_line
    use file_input_output_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, &
                                     create_directory_internal, file_exists
    use files_mod, only: delete_directory
    use fortmacs_diff_settings_mod, only: ediff_sessions, ediff_session_index, EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_C, &
                                        EDIFF_BUFFER_TYPE_ANCESTOR, EDIFF_STATE_OF_MERGE_COMBINED, EDIFF_STATE_OF_MERGE_DEFAULT_B, &
                                          EDIFF_STATE_OF_MERGE_PREFER_A, EDIFF_STATE_OF_MERGE_PREFER_B
    use fortmacs_diff_utilities_mod, only: next_ediff_difference, jump_to_ediff_difference, quit_ediff_session, toggle_ediff_help, &
                                           toggle_ediff_show_clashes_only, maybe_save_and_delete_ediff_merge_session
    use fortmacs_diff_merging_mod, only: ediff_default_variant, do_ediff_merge, combine_ediff_diffs, ediff_variant_state_of_merge, &
                                    EDIFF_DEFAULT_VARIANT_DEFAULT_A, EDIFF_DEFAULT_VARIANT_DEFAULT_B, EDIFF_DEFAULT_VARIANT_COMBINED
    use fortmacs_diff_mod, only: ediff_merge_buffers, ediff_merge_buffers_with_ancestor
    use commands_mod, only: commands
    implicit none(type, external)
    private

    public :: collect_fortmacs_diff_merging_mod

    character, parameter :: LF = achar(10)

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-ediff-merg"

contains

    !> Collect all exported unit tests.
    subroutine collect_fortmacs_diff_merging_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ediff-merge-combined", test_merge_combined), &
                    new_unittest("ediff-merge-default-b", test_merge_default_b), &
                    new_unittest("ediff-merge-keys", test_merge_keys), &
                    new_unittest("ediff-re-merge", test_merge_with_new_default), &
                    new_unittest("ediff-merge-help", test_merge_help), &
                    new_unittest("ediff-merge-with-ancestor", test_merge_with_ancestor), &
                    new_unittest("ediff-show-clashes-only", test_show_clashes_only), &
                    new_unittest("ediff-merge-store-file", test_merge_store_file) &
                    ]
    end subroutine collect_fortmacs_diff_merging_mod

    !> Whether PROGRAM is on PATH, skipping the test if not.
    logical function has_program(program, error)
        character(len=*), intent(in) :: program
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path, dir
        integer :: n, st, k

        has_program = .false.
        call get_environment_variable("PATH", length=n, status=st)

        if (st == 0) then
            allocate (character(len=n) :: path)
            call get_environment_variable("PATH", path)

            do while (len(path) > 0 .and. .not. has_program)
                k = index(path, ":")
                if (k == 0) k = len(path) + 1
                dir = path(1:k - 1)

                if (k > len(path)) then
                    path = ""
                else
                    path = path(k + 1:)
                end if

                if (len(dir) > 0) inquire (file=dir//"/"//program, exist=has_program)
            end do
        end if

        if (.not. has_program) call skip_test(error, "no "//program)
    end function has_program

    !> A fresh buffer NAME holding TEXT.
    subroutine temp_buffer(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call set_buffer(old, st)
    end subroutine temp_buffer

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

    !> *scratch* current, shown alone.
    subroutine setup()
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
    end subroutine setup

    !> Merge two buffers that differ in lines 2 and 4 (A has a and b, B
    !> has A and B), without ancestor; A and B are the buffers, the control
    !> buffer is current and K is its session.
    subroutine start_merge(a, b, k, status)
        type(handle_type), intent(out) :: a, b
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: control

        call setup()
        call temp_buffer("merge-a", "1"//LF//"a"//LF//"3"//LF//"b"//LF//"5"//LF, a)
        call temp_buffer("merge-b", "1"//LF//"A"//LF//"3"//LF//"B"//LF//"5"//LF, b)
        call ediff_merge_buffers(a, b, status)
        k = 0
        if (.not. status%ok()) return
        control = get_buffer("*Ediff Control Panel*")
        call set_buffer(control, status)
        k = ediff_session_index(control)
    end subroutine start_merge

    !> Quit the session and close the buffers of the merge (A, B, the
    !> ancestor ANCESTOR if live, and the merge buffer).
    subroutine end_merge(a, b, ancestor)
        type(handle_type), intent(in) :: a, b
        type(handle_type), intent(in), optional :: ancestor
        type(handle_type) :: control
        type(status_type) :: st
        integer :: k

        control = get_buffer("*Ediff Control Panel*")

        if (is_buffer_live(control)) then
            call set_buffer(control, st)
            k = ediff_session_index(control)
            if (k > 0) call quit_ediff_session(k, .false., st)
        end if

        call discard(a)
        call discard(b)
        if (present(ancestor)) call discard(ancestor)
        call discard(get_buffer("*ediff-merge*"))
    end subroutine end_merge

    !> Close buffer H without saving it.
    subroutine discard(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: old
        type(status_type) :: st

        if (.not. is_buffer_live(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        call restore_buffer_modified(.false.)
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine discard

    !> With the default variant combined, the merge buffer holds both
    !> regions of each difference between the delimiters of
    !> `ediff_combination_pattern' (no ancestor part without an ancestor).
    subroutine test_merge_combined(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, merge
        type(status_type) :: st
        integer :: k

        if (.not. has_program("diff", error)) return
        call start_merge(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, associated(ediff_sessions(k)%job_name, commands%ediff_merge_buffers), "job ediff_merge_buffers")
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%merge_job .and. ediff_sessions(k)%three_way_job, "a merge job")
            if (allocated(error)) exit body
            merge = get_buffer("*ediff-merge*")
            call check(error, merge%index, ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_C)%index)
            if (allocated(error)) exit body
            call check(error, text_of(merge), "1"//LF//"<<<<<<< variant A"//LF//"a"//LF//">>>>>>> variant B"//LF//"A"//LF// &
                       "======= end"//LF//"3"//LF//"<<<<<<< variant A"//LF//"b"//LF//">>>>>>> variant B"//LF//"B"//LF// &
                       "======= end"//LF//"5"//LF)
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%merge_states(1)%state_of_merge, EDIFF_STATE_OF_MERGE_COMBINED)
            if (allocated(error)) exit body
            ! Buffers A and B are read-only, the merge buffer is not.
            call check(error, buffers(a%index)%read_only .and. .not. buffers(merge%index)%read_only, "read-only")
            if (allocated(error)) exit body
            ! The merge buffer's window is below the windows of A and B.
            call check(error, is_window_live(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_C)), "window C")
            if (allocated(error)) exit body
            call check(error, window_top_line(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_C), st) > &
                       window_top_line(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_A), st), "window C below window A")
        end block body

        call end_merge(a, b)
    end subroutine test_merge_combined

    !> With the default variant default-B, the merge buffer is a copy of
    !> buffer B.
    subroutine test_merge_default_b(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_program("diff", error)) return
        ediff_default_variant = EDIFF_DEFAULT_VARIANT_DEFAULT_B
        call start_merge(a, b, k, st)
        ediff_default_variant = EDIFF_DEFAULT_VARIANT_COMBINED

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(get_buffer("*ediff-merge*")), text_of(b))
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%merge_states(2)%state_of_merge, EDIFF_STATE_OF_MERGE_DEFAULT_B)
            if (allocated(error)) exit body
            call check(error, ediff_variant_state_of_merge(EDIFF_DEFAULT_VARIANT_DEFAULT_B), EDIFF_STATE_OF_MERGE_DEFAULT_B)
        end block body

        call end_merge(a, b)
    end subroutine test_merge_default_b

    !> The merge keys: n then a copies A's region into the merge buffer, r
    !> restores it, + combines it again.
    subroutine test_merge_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, merge
        type(status_type) :: st
        integer :: k
        character(len=:), allocatable :: merged

        if (.not. has_program("diff", error)) return
        call start_merge(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            merge = get_buffer("*ediff-merge*")
            merged = text_of(merge)
            call kbd_store_bytes("na")
            call command_loop(st)
            call check(error, ediff_sessions(k)%current_difference, 0)
            if (allocated(error)) exit body
            call check(error, index(text_of(merge), "1"//LF//"a"//LF//"3"//LF) == 1, text_of(merge))
            if (allocated(error)) exit body
            call kbd_store_bytes("r")
            call command_loop(st)
            call check(error, text_of(merge), merged)
            if (allocated(error)) exit body
            call kbd_store_bytes("b")
            call command_loop(st)
            call check(error, index(text_of(merge), "1"//LF//"A"//LF//"3"//LF) == 1, text_of(merge))
            if (allocated(error)) exit body
            call combine_ediff_diffs(k, 0, st, batch_invocation=.true.)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(merge), merged)
        end block body

        call end_merge(a, b)
    end subroutine test_merge_keys

    !> Merging again with a new default changes the regions that were not
    !> edited since the last merge.
    subroutine test_merge_with_new_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_program("diff", error)) return
        call start_merge(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            ediff_sessions(k)%default_variant%choice = EDIFF_DEFAULT_VARIANT_DEFAULT_A
            call do_ediff_merge(k, 0, st, remerging=.true.)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(get_buffer("*ediff-merge*")), text_of(a))
        end block body

        call end_merge(a, b)
    end subroutine test_merge_with_new_default

    !> The long help of a merge session lists the merge keys.
    subroutine test_merge_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, control
        type(status_type) :: st
        integer :: k

        if (.not. has_program("diff", error)) return
        call start_merge(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = current_buffer()
            call toggle_ediff_help(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, index(text_of(control), "+ -combine diff regions") > 0, text_of(control))
        end block body

        call end_merge(a, b)
    end subroutine test_merge_help

    !> Merging with an ancestor: a region only A changed is prefer-A and
    !> takes A's text, one only B changed is prefer-B; the ancestor has a
    !> window.
    subroutine test_merge_with_ancestor(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, ancestor, control
        type(status_type) :: st
        integer :: k

        if (.not. has_program("diff3", error)) return
        call setup()
        call temp_buffer("merge-a", "1"//LF//"x"//LF//"3"//LF//"4"//LF, a)
        call temp_buffer("merge-b", "1"//LF//"2"//LF//"3"//LF//"y"//LF, b)
        call temp_buffer("merge-ancestor", "1"//LF//"2"//LF//"3"//LF//"4"//LF, ancestor)
        call ediff_merge_buffers_with_ancestor(a, b, ancestor, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = get_buffer("*Ediff Control Panel*")
            k = ediff_session_index(control)
            call check(error, ediff_sessions(k)%merge_with_ancestor_job, "merging with an ancestor")
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%number_of_differences, 2)
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%merge_states(1)%state_of_merge, EDIFF_STATE_OF_MERGE_PREFER_A)
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%merge_states(2)%state_of_merge, EDIFF_STATE_OF_MERGE_PREFER_B)
            if (allocated(error)) exit body
            call check(error, text_of(get_buffer("*ediff-merge*")), "1"//LF//"x"//LF//"3"//LF//"y"//LF)
            if (allocated(error)) exit body
            call check(error, is_window_live(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_ANCESTOR)), "ancestor window")
        end block body

        call end_merge(a, b, ancestor)
    end subroutine test_merge_with_ancestor

    !> With $$, n skips the regions where only one variant changed: here
    !> both are skipped, so n goes past the last one.
    subroutine test_show_clashes_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, ancestor, control
        type(status_type) :: st
        integer :: k

        if (.not. has_program("diff3", error)) return
        call setup()
        call temp_buffer("merge-a", "1"//LF//"x"//LF//"3"//LF//"4"//LF//"5"//LF//"p"//LF//"7"//LF, a)
        call temp_buffer("merge-b", "1"//LF//"2"//LF//"3"//LF//"y"//LF//"5"//LF//"q"//LF//"7"//LF, b)
        call temp_buffer("merge-ancestor", "1"//LF//"2"//LF//"3"//LF//"4"//LF//"5"//LF//"6"//LF//"7"//LF, ancestor)
        call ediff_merge_buffers_with_ancestor(a, b, ancestor, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = get_buffer("*Ediff Control Panel*")
            call set_buffer(control, st)
            k = ediff_session_index(control)
            call check(error, ediff_sessions(k)%number_of_differences, 3)
            if (allocated(error)) exit body
            call toggle_ediff_show_clashes_only(st)
            call check(error, ediff_sessions(k)%show_clashes_only, "showing clashes only")
            if (allocated(error)) exit body
            ! Regions 1 and 2 are prefer-A and prefer-B; region 3 clashes.
            call next_ediff_difference(st)
            call check(error, ediff_sessions(k)%current_difference, 2)
            if (allocated(error)) exit body
            call jump_to_ediff_difference(0_int64, st)
            call toggle_ediff_show_clashes_only(st)
            call next_ediff_difference(st)
            call check(error, ediff_sessions(k)%current_difference, 0)
        end block body

        call end_merge(a, b, ancestor)
    end subroutine test_show_clashes_only

    !> A merge given a file to store it in visits that file and saves it
    !> there.
    subroutine test_merge_store_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, merge, control
        type(status_type) :: st
        character(len=:), allocatable :: dir
        integer :: k

        if (.not. has_program("diff", error)) return
        call setup()
        call init_fileio()
        dir = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdir)
        if (is_file_directory(dir)) call delete_directory(dir, st, .true.)
        call create_directory_internal(dir, st)
        call temp_buffer("merge-a", "1"//LF//"a"//LF, a)
        call temp_buffer("merge-b", "1"//LF//"b"//LF, b)
        k = 0
        call ediff_merge_buffers(a, b, st, merge_buffer_file=dir//"merged.txt")

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = get_buffer("*Ediff Control Panel*")
            call set_buffer(control, st)
            k = ediff_session_index(control)
            merge = ediff_sessions(k)%buffers(EDIFF_BUFFER_TYPE_C)
            call check(error, buffers(merge%index)%filename, dir//"merged.txt")
            if (allocated(error)) exit body
            call check(error, ediff_sessions(k)%autostore_merges%value, "autostore")
            if (allocated(error)) exit body
            call maybe_save_and_delete_ediff_merge_session(k, .true., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, file_exists(dir//"merged.txt"), "the merge is saved")
            if (allocated(error)) exit body
            call check(error, text_of(merge), "1"//LF//"<<<<<<< variant A"//LF//"a"//LF//">>>>>>> variant B"//LF//"b"//LF// &
                       "======= end"//LF)
        end block body

        ! Quitting would save it again, asking before overwriting it.
        if (k > 0) ediff_sessions(k)%autostore_merges%value = .false.
        call end_merge(a, b)
        call delete_directory(dir, st, .true.)
    end subroutine test_merge_store_file
end module test_fortmacs_diff_merging_mod
