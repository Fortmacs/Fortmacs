! ediff_merg_mod.f90 -- merging with Ediff.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-merg.el (in part: see
! docs/status/ediff_merg_mod.md).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> The merge jobs of Ediff: buffer C, the merge buffer, starts as a copy
!> of variant A (or B), and each difference region of it is then set by
!> the default variant (`ediff_default_variant': A's region, B's, or both
!> combined by `ediff_combination_pattern'), or, when merging with an
!> ancestor, by the variant that changed the region (prefer-A, prefer-B).
!> The control buffer's keys + (combine), & (merge again with a new
!> default) and s (shrink window C) are here; ediff_util_mod's keys n and
!> p skip regions with the tests here when $$ or $* asks for it.
module ediff_merg_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use value_mod, only: value_type, make_list
    use data_mod, only: choice_setting_type
    use hooks_mod, only: hook_type
    use callint_mod, only: prefix_arg_type, current_prefix_arg, PREFIX_NUMBER, PREFIX_MINUS
    use commands_mod, only: commands
    use files_mod, only: normal_mode, write_file_functions
    use minibuf_mod, only: completion_list_table
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use window_mod, only: window_total_height
    use xdisp_mod, only: message
    use ediff_init_mod, only: ediff_sessions, check_ediff_control_buffer, EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, &
                              EDIFF_BUFFER_TYPE_C, EDIFF_BUFFER_TYPE_ANCESTOR, EDIFF_STATE_OF_MERGE_NONE, &
                              EDIFF_STATE_OF_MERGE_DEFAULT_A, EDIFF_STATE_OF_MERGE_DEFAULT_B, EDIFF_STATE_OF_MERGE_COMBINED, &
                              EDIFF_STATE_OF_MERGE_PREFER_A, EDIFF_STATE_OF_MERGE_PREFER_B
    use ediff_util_mod, only: get_ediff_region_contents, copy_ediff_diff, jump_to_ediff_difference, recenter_ediff
    implicit none(type, external)
    private

    public :: ediff_variant_state_of_merge, get_ediff_state_of_merge, is_ediff_merge_region_non_clash, &
              is_ediff_merge_region_non_clash_to_skip, is_ediff_merge_region_changed_from_default_to_skip, &
              get_ediff_combined_region, set_ediff_state_of_all_diffs_in_all_buffers, set_ediff_state_of_diff_in_all_buffers, &
              set_ediff_merge_mode, do_ediff_merge, re_merge_ediff, shrink_ediff_window_c, combine_ediff_diffs, &
              is_ediff_merge_changed_from_default, ediff_default_variant_name, init_ediff_merg

    character, parameter :: LF = achar(10)

    !> Hooks to run before quitting a merge job.
    !> The most common use is to save and delete the merge buffer.  Port of
    !> ediff-merg.el ediff-quit-merge-hook (its default function,
    !> `maybe_save_and_delete_ediff_merge', is added by ediff_util_mod).
    type(hook_type), target, public :: ediff_quit_merge_hook = hook_type(name="ediff_quit_merge_hook")

    !> Values of `ediff_default_variant'.
    integer, parameter, public :: EDIFF_DEFAULT_VARIANT_DEFAULT_A = 2, EDIFF_DEFAULT_VARIANT_DEFAULT_B = 3, &
                                  EDIFF_DEFAULT_VARIANT_COMBINED = 4

    !> The variant to be used as a default for buffer C in merging.
    !> Valid values are EDIFF_DEFAULT_VARIANT_DEFAULT_A,
    !> EDIFF_DEFAULT_VARIANT_DEFAULT_B, and EDIFF_DEFAULT_VARIANT_COMBINED.
    !> Port of ediff-merg.el ediff-default-variant (each session takes its
    !> own value when it starts).
    type(choice_setting_type), public :: ediff_default_variant = &
                                         choice_setting_type(name="ediff_default_variant", true_valid=.false., &
                                                             false_valid=.false., choice=EDIFF_DEFAULT_VARIANT_COMBINED)

    !> One part of `ediff_combination_pattern': the line DELIMITER, then the
    !> difference region of BUFFER_TYPE (EDIFF_BUFFER_TYPE_A, _B or
    !> _ANCESTOR; 0 for the last delimiter, which has no region after it).
    type, public :: ediff_combination_part_type
        character(len=80) :: delimiter = ""
        integer :: buffer_type = 0
    end type ediff_combination_part_type

    !> Pattern to be used for combining difference regions in buffers A
    !> and B.
    !> The value must be an array of the form
    !> [ediff_combination_part_type(STRING1, bufspec1), (STRING2, bufspec2),
    !> (STRING3, bufspec3), (STRING4, 0)]
    !> where bufspec is the buffer type EDIFF_BUFFER_TYPE_A,
    !> EDIFF_BUFFER_TYPE_B, or EDIFF_BUFFER_TYPE_ANCESTOR.  For instance, if
    !> the value is [(STRING1, A), (STRING2, Ancestor), (STRING3, B),
    !> (STRING4, 0)] then the combined text will look like this:
    !>
    !> STRING1
    !> diff region from variant A
    !> STRING2
    !> diff region from the ancestor
    !> STRING3
    !> diff region from variant B
    !> STRING4
    !> Port of ediff-merg.el ediff-combination-pattern (the delimiters lose
    !> their trailing blanks).
    type(ediff_combination_part_type), allocatable, public :: ediff_combination_pattern(:)

    !> If .true., show only diff regions where both buffers disagree with
    !> the ancestor.
    !> This means that regions that prefer variant A or variant B (only
    !> one of them changed the ancestor's text) will be skipped over.  A value of .false. means show all regions.  Port of
    !> ediff-merg.el ediff-show-clashes-only (each session takes its own
    !> value when it starts).
    logical, public :: ediff_show_clashes_only = .false.

    !> If .true., show only the regions that have not been changed by the
    !> user.
    !> A region is considered to have been changed if it is different from
    !> the current default (EDIFF_DEFAULT_VARIANT_DEFAULT_A, _DEFAULT_B,
    !> _COMBINED) and it hasn't been marked as preferring variant A or B.
    !> A region is considered to have been changed also when it is marked
    !> as preferring A, but is different from the corresponding difference
    !> region in Buffer A or if it is marked as preferring B and is
    !> different from the region in Buffer B.  Port of ediff-merg.el
    !> ediff-skip-merge-regions-that-differ-from-default (each session takes
    !> its own value when it starts).
    logical, public :: ediff_skip_merge_regions_that_differ_from_default = .false.

contains

    !> The state of a merge region set by VARIANT, a value of
    !> `ediff_default_variant' (EDIFF_STATE_OF_MERGE_DEFAULT_A, _DEFAULT_B
    !> or _COMBINED).  Not a port: upstream keeps the variant's symbol as
    !> the state.
    pure integer function ediff_variant_state_of_merge(variant) result(state)
        integer, intent(in) :: variant

        select case (variant)
        case (EDIFF_DEFAULT_VARIANT_DEFAULT_A)
            state = EDIFF_STATE_OF_MERGE_DEFAULT_A
        case (EDIFF_DEFAULT_VARIANT_DEFAULT_B)
            state = EDIFF_STATE_OF_MERGE_DEFAULT_B
        case default
            state = EDIFF_STATE_OF_MERGE_COMBINED
        end select
    end function ediff_variant_state_of_merge

    !> The name of VARIANT (EDIFF_DEFAULT_VARIANT_*) as the user reads and
    !> types it: the named constant.  Not a port: upstream prints the
    !> symbol.
    pure function ediff_default_variant_name(variant) result(name)
        integer, intent(in) :: variant
        character(len=:), allocatable :: name

        select case (variant)
        case (EDIFF_DEFAULT_VARIANT_DEFAULT_A)
            name = "EDIFF_DEFAULT_VARIANT_DEFAULT_A"
        case (EDIFF_DEFAULT_VARIANT_DEFAULT_B)
            name = "EDIFF_DEFAULT_VARIANT_DEFAULT_B"
        case default
            name = "EDIFF_DEFAULT_VARIANT_COMBINED"
        end select
    end function ediff_default_variant_name

    !> The state of merge region N (counted from 0) of session K
    !> (EDIFF_STATE_OF_MERGE_*), none outside merge jobs.  Port of
    !> ediff-init.el:ediff-get-state-of-merge.
    integer function get_ediff_state_of_merge(k, n) result(state)
        integer, intent(in) :: k, n

        state = EDIFF_STATE_OF_MERGE_NONE
        if (.not. allocated(ediff_sessions(k)%merge_states)) return
        if (n < 0 .or. n >= size(ediff_sessions(k)%merge_states)) return
        state = ediff_sessions(k)%merge_states(n + 1)%state_of_merge
    end function get_ediff_state_of_merge

    !> Whether there is no clash between the ancestor and one of the
    !> variants in region N of session K (only one variant changed it);
    !> .true. if it is not a merge job.  Port of
    !> ediff-merg.el:ediff-merge-region-is-non-clash.
    logical function is_ediff_merge_region_non_clash(k, n)
        integer, intent(in) :: k, n

        is_ediff_merge_region_non_clash = .true.
        if (.not. ediff_sessions(k)%merge_job) return
        is_ediff_merge_region_non_clash = any(get_ediff_state_of_merge(k, n) == [EDIFF_STATE_OF_MERGE_PREFER_A, &
                                                                                 EDIFF_STATE_OF_MERGE_PREFER_B])
    end function is_ediff_merge_region_non_clash

    !> If the session's `ediff_show_clashes_only', whether region N of
    !> session K has no clash between the ancestor and one of the variants
    !> (so it is skipped).  Port of
    !> ediff-merg.el:ediff-merge-region-is-non-clash-to-skip.
    logical function is_ediff_merge_region_non_clash_to_skip(k, n)
        integer, intent(in) :: k, n

        is_ediff_merge_region_non_clash_to_skip = .false.
        if (.not. ediff_sessions(k)%merge_job) return
        if (.not. ediff_sessions(k)%show_clashes_only) return
        is_ediff_merge_region_non_clash_to_skip = is_ediff_merge_region_non_clash(k, n)
    end function is_ediff_merge_region_non_clash_to_skip

    !> If the session's `ediff_skip_merge_regions_that_differ_from_default',
    !> give in SKIP whether merge region N of session K differs from the
    !> current default.  If a region is different from the default, it
    !> means that the user has made determination as to how to merge for
    !> this particular region.  Port of
    !> ediff-merg.el:ediff-skip-merge-region-if-changed-from-default-p.
    recursive subroutine is_ediff_merge_region_changed_from_default_to_skip(k, n, skip, status)
        integer, intent(in) :: k, n
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status

        status = status_type()
        skip = .false.
        if (.not. ediff_sessions(k)%merge_job) return
        if (.not. ediff_sessions(k)%skip_merge_regions_that_differ_from_default) return
        call is_ediff_merge_changed_from_default(k, n, skip, status, prefers_too=.true.)
    end subroutine is_ediff_merge_region_changed_from_default_to_skip

    !> The combination of difference region N of buffers A and B (and the
    !> ancestor) of session K by `ediff_combination_pattern', in TEXT.  A
    !> region that cannot be read (the ancestor's in a merge without one)
    !> is left out with its delimiter.  Port of
    !> ediff-merg.el:ediff-get-combined-region.
    recursive subroutine get_ediff_combined_region(k, n, text, status)
        integer, intent(in) :: k, n
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        character(len=*), parameter :: ERR_MSG = &
                                       "ediff_combination_pattern: Invalid format. Please consult the documentation"
        character(len=:), allocatable :: region
        type(status_type) :: st
        integer :: j, m

        status = status_type()
        text = ""
        m = 0
        if (allocated(ediff_combination_pattern)) m = size(ediff_combination_pattern)

        if (m < 3) then
            call set_error(status, FM_GENERIC_ERROR, ERR_MSG)
            return
        end if

        do j = 1, m - 1
            if (all(ediff_combination_pattern(j)%buffer_type /= [EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, &
                                                                 EDIFF_BUFFER_TYPE_ANCESTOR])) then
                call set_error(status, FM_GENERIC_ERROR, ERR_MSG)
                return
            end if

            call get_ediff_region_contents(k, n, ediff_combination_pattern(j)%buffer_type, region, st)
            if (st%ok()) text = text//trim(ediff_combination_pattern(j)%delimiter)//LF//region
        end do

        if (ediff_combination_pattern(m)%buffer_type /= 0) then
            call set_error(status, FM_GENERIC_ERROR, ERR_MSG)
            return
        end if

        text = text//trim(ediff_combination_pattern(m)%delimiter)//LF
    end subroutine get_ediff_combined_region

    !> Set the state of every difference of session K in buffers A, B and
    !> C.  Port of ediff-merg.el:ediff-set-state-of-all-diffs-in-all-buffers.
    recursive subroutine set_ediff_state_of_all_diffs_in_all_buffers(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        integer :: n

        status = status_type()

        do n = 0, ediff_sessions(k)%number_of_differences - 1
            call set_ediff_state_of_diff_in_all_buffers(k, n, status)
            if (.not. status%ok()) return
        end do
    end subroutine set_ediff_state_of_all_diffs_in_all_buffers

    !> Set the state of difference N of session K in buffers A, B and C
    !> ("=diff(B)" in A when A's region equals B's, and so on; "=diff(A+B)"
    !> in C when C holds the combination).  Port of
    !> ediff-merg.el:ediff-set-state-of-diff-in-all-buffers.
    recursive subroutine set_ediff_state_of_diff_in_all_buffers(k, n, status)
        integer, intent(in) :: k, n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: reg_a, reg_b, reg_c, combined

        call get_ediff_region_contents(k, n, EDIFF_BUFFER_TYPE_A, reg_a, status)
        if (.not. status%ok()) return
        call get_ediff_region_contents(k, n, EDIFF_BUFFER_TYPE_B, reg_b, status)
        if (.not. status%ok()) return
        call get_ediff_region_contents(k, n, EDIFF_BUFFER_TYPE_C, reg_c, status)
        if (.not. status%ok()) return

        if (reg_a == reg_b .and. reg_a == reg_c) then
            call set_states("=diff(B)", "=diff(C)", "=diff(A)")
        else if (reg_a == reg_b) then
            call set_states("=diff(B)", "=diff(A)", "")
        else if (reg_a == reg_c) then
            call set_states("=diff(C)", "", "=diff(A)")
        else if (reg_b == reg_c) then
            call set_states("", "=diff(C)", "=diff(B)")
        else
            call get_ediff_combined_region(k, n, combined, status)
            if (.not. status%ok()) return

            if (reg_c == combined) then
                call set_states("", "", "=diff(A+B)")
            else
                call set_states("", "", "")
            end if
        end if

    contains

        ! Set the states of difference N in buffers A, B and C.
        subroutine set_states(state_a, state_b, state_c)
            character(len=*), intent(in) :: state_a, state_b, state_c

            ediff_sessions(k)%differences(EDIFF_BUFFER_TYPE_A, n + 1)%state_of_diff = state_a
            ediff_sessions(k)%differences(EDIFF_BUFFER_TYPE_B, n + 1)%state_of_diff = state_b
            ediff_sessions(k)%differences(EDIFF_BUFFER_TYPE_C, n + 1)%state_of_diff = state_c
        end subroutine set_states
    end subroutine set_ediff_state_of_diff_in_all_buffers

    !> A function of the merge buffer's own `write_file_functions': give
    !> the buffer its mode by its file name (as when it is visited), and
    !> leave the hook.  DONE is .true., as upstream's remove-hook returns
    !> the hook's symbol when it clears the buffer's own value.  Port of
    !> ediff-merg.el:ediff-set-merge-mode.
    recursive subroutine set_ediff_merge_mode(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status

        done = .false.
        call normal_mode(status, find_file=.true.)
        if (.not. status%ok()) return
        call write_file_functions%remove(set_ediff_merge_mode, local=.true.)
        done = .true.
    end subroutine set_ediff_merge_mode

    !> Go over all diffs of session K starting with DIFF_NUM and copy
    !> regions into buffer C according to the state of the difference.
    !> Since `copy_ediff_diff' refuses to copy identical diff regions,
    !> there is no need to optimize this any further.
    !>
    !> If REMERGING, change state of merge in all diffs starting with
    !> DIFF_NUM, except those where the state is prefer-* or where it is
    !> default-* or combined but the buffer C region appears to be modified
    !> since last set by default.  Port of ediff-merg.el:ediff-do-merge.
    recursive subroutine do_ediff_merge(k, diff_num, status, remerging)
        integer, intent(in) :: k, diff_num
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: remerging
        integer :: n, state, from_type
        logical :: do_not_copy, again, changed
        character(len=96) :: progress

        status = status_type()
        again = .false.
        if (present(remerging)) again = remerging
        n = max(0, diff_num)

        do while (n < ediff_sessions(k)%number_of_differences)
            ! Reset after each cycle.
            do_not_copy = .false.

            if (modulo(n, 10) == 0) then
                if (again) then
                    write (progress, '("Re-merging buffers A & B into C ... region ", i0, " of ", i0)') n, &
                        ediff_sessions(k)%number_of_differences
                else
                    write (progress, '("Merging buffers A & B into C ... region ", i0, " of ", i0)') n, &
                        ediff_sessions(k)%number_of_differences
                end if

                call message(trim(progress))
            end if

            state = get_ediff_state_of_merge(k, n)

            if (again) then
                ! If region was edited since it was first set by default,
                ! or was preferred, then ignore.
                call is_ediff_merge_changed_from_default(k, n, changed, status)
                if (.not. status%ok()) return

                if (changed .or. any(state == [EDIFF_STATE_OF_MERGE_PREFER_A, EDIFF_STATE_OF_MERGE_PREFER_B])) then
                    do_not_copy = .true.
                end if

                ! Change state of merge for this diff, if necessary.
                if (.not. do_not_copy .and. any(state == [EDIFF_STATE_OF_MERGE_DEFAULT_A, EDIFF_STATE_OF_MERGE_DEFAULT_B, &
                                                          EDIFF_STATE_OF_MERGE_COMBINED])) then
                    ediff_sessions(k)%merge_states(n + 1)%state_of_merge = &
                        ediff_variant_state_of_merge(ediff_sessions(k)%default_variant%choice)
                end if
            end if

            ! State of merge may have changed, so check it once again.
            state = get_ediff_state_of_merge(k, n)

            if (.not. do_not_copy) then
                if (state == EDIFF_STATE_OF_MERGE_COMBINED) then
                    call combine_ediff_diffs(k, n, status, batch_invocation=.true.)
                else
                    from_type = EDIFF_BUFFER_TYPE_B

                    if (any(state == [EDIFF_STATE_OF_MERGE_DEFAULT_A, EDIFF_STATE_OF_MERGE_PREFER_A])) then
                        from_type = EDIFF_BUFFER_TYPE_A
                    end if

                    call copy_ediff_diff(k, n, from_type, EDIFF_BUFFER_TYPE_C, status, batch_invocation=.true.)
                end if

                if (.not. status%ok()) return
            end if

            n = n + 1
        end do

        call message("Merging buffers A & B into C ... Done")
    end subroutine do_ediff_merge

    !> Remerge unmodified diff regions using a new default.
    !> Start with the current region.  Port of ediff-merg.el:ediff-re-merge.
    recursive subroutine re_merge_ediff(status)
        type(status_type), intent(out) :: status
        type(value_type) :: items(2)
        character(len=:), allocatable :: answer
        integer :: k, variant, j

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        j = 0

        ! The variants other than the current default.
        do variant = EDIFF_DEFAULT_VARIANT_DEFAULT_A, EDIFF_DEFAULT_VARIANT_COMBINED
            if (variant == ediff_sessions(k)%default_variant%choice) cycle
            j = j + 1
            items(j) = value_type(text=ediff_default_variant_name(variant))
        end do

        call completing_read(format_message("Current merge default is `")// &
                             ediff_default_variant_name(ediff_sessions(k)%default_variant%choice)// &
                             format_message("'.  New default: "), completion_list_table(make_list(items(1:j))), answer, &
                             status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return

        do variant = EDIFF_DEFAULT_VARIANT_DEFAULT_A, EDIFF_DEFAULT_VARIANT_COMBINED
            if (answer == ediff_default_variant_name(variant)) ediff_sessions(k)%default_variant%choice = variant
        end do

        call do_ediff_merge(k, ediff_sessions(k)%current_difference, status, remerging=.true.)
        if (.not. status%ok()) return
        call recenter_ediff(status)
    end subroutine re_merge_ediff

    !> Shrink window C to just one line.
    !> With a prefix argument ARG, returns window C to its normal size.
    !> Used only for merging jobs.  Port of
    !> ediff-merg.el:ediff-shrink-window-C.
    recursive subroutine shrink_ediff_window_c(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        real :: saved
        integer :: k, height_a, height_c
        integer(int64) :: n

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return

        if (.not. ediff_sessions(k)%merge_job) then
            call set_error(status, FM_USER_ERROR, "shrink_ediff_window_c can be used only for merging jobs")
            return
        end if

        select case (arg%kind)
        case (PREFIX_MINUS)
            n = -1
        case (PREFIX_NUMBER)
            n = arg%value
        case default
            ! No number: shrink window C, or return it to its size, for
            ! this redisplay only.
            saved = ediff_sessions(k)%merge_window_share
            height_c = window_total_height(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_C), status)
            if (.not. status%ok()) return
            if (height_c >= 3) ediff_sessions(k)%merge_window_share = 0
            ! Force redisplay.
            ediff_sessions(k)%window_config_saved = ""
            call recenter_ediff(status, no_rehighlight=.true.)
            ediff_sessions(k)%merge_window_share = saved
            return
        end select

        height_a = 0
        height_c = 0

        if (n < 0) then
            height_c = window_total_height(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_C), status)
            if (.not. status%ok()) return
        else if (n > 0) then
            height_a = window_total_height(ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_A), status)
            if (.not. status%ok()) return
        end if

        if (n < 0 .and. height_c > 2) then
            ediff_sessions(k)%merge_window_share = ediff_sessions(k)%merge_window_share*0.9
        else if (n > 0 .and. height_a > 2) then
            ediff_sessions(k)%merge_window_share = ediff_sessions(k)%merge_window_share*1.1
        else
            return
        end if

        ! Force redisplay.
        ediff_sessions(k)%window_config_saved = ""
        call recenter_ediff(status, no_rehighlight=.true.)
    end subroutine shrink_ediff_window_c

    !> Combine Nth diff regions of buffers A and B and place the
    !> combination in C (N counted from 0, as Ediff counts internally).
    !> Combining is done according to the specifications in variable
    !> `ediff_combination_pattern'.  Unless BATCH_INVOCATION, the old region
    !> is saved and the session goes to the difference.  Port of
    !> ediff-merg.el:ediff-combine-diffs (the command's N, the user's
    !> number, is read by the command).
    recursive subroutine combine_ediff_diffs(k, n, status, batch_invocation)
        integer, intent(in) :: k, n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: batch_invocation
        character(len=:), allocatable :: reg_combined
        logical :: batch

        batch = .false.
        if (present(batch_invocation)) batch = batch_invocation
        call get_ediff_combined_region(k, n, reg_combined, status)
        if (.not. status%ok()) return
        call copy_ediff_diff(k, n, 0, EDIFF_BUFFER_TYPE_C, status, batch_invocation=batch, region_to_copy=reg_combined)
        if (.not. status%ok()) return
        if (.not. batch) call jump_to_ediff_difference(int(n + 1, int64), status)
    end subroutine combine_ediff_diffs

    !> Give in CHANGED whether the non-preferred merge region N of session
    !> K has been modified since originally set.  This affects only the
    !> regions that are marked as default-A/B or combined.  If PREFERS_TOO,
    !> then look at the regions marked as prefer-A/B as well.  Port of
    !> ediff-merg.el:ediff-merge-changed-from-default-p.
    recursive subroutine is_ediff_merge_changed_from_default(k, n, changed, status, prefers_too)
        integer, intent(in) :: k, n
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: prefers_too
        character(len=:), allocatable :: reg_a, reg_b, reg_c, combined
        logical :: prefers

        changed = .false.
        prefers = .false.
        if (present(prefers_too)) prefers = prefers_too
        call get_ediff_region_contents(k, n, EDIFF_BUFFER_TYPE_A, reg_a, status)
        if (.not. status%ok()) return
        call get_ediff_region_contents(k, n, EDIFF_BUFFER_TYPE_B, reg_b, status)
        if (.not. status%ok()) return
        call get_ediff_region_contents(k, n, EDIFF_BUFFER_TYPE_C, reg_c, status)
        if (.not. status%ok()) return

        ! If region was edited since it was first set by default.
        select case (get_ediff_state_of_merge(k, n))
        case (EDIFF_STATE_OF_MERGE_DEFAULT_A)
            changed = reg_a /= reg_c
        case (EDIFF_STATE_OF_MERGE_DEFAULT_B)
            changed = reg_b /= reg_c
        case (EDIFF_STATE_OF_MERGE_COMBINED)
            call get_ediff_combined_region(k, n, combined, status)
            if (.not. status%ok()) return
            changed = combined /= reg_c
        case (EDIFF_STATE_OF_MERGE_PREFER_A)
            changed = prefers .and. reg_a /= reg_c
        case (EDIFF_STATE_OF_MERGE_PREFER_B)
            changed = prefers .and. reg_b /= reg_c
        case default
            changed = .false.
        end select
    end subroutine is_ediff_merge_changed_from_default

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Shrink window C to just one line.
    !> With a prefix argument, returns window C to its normal size.
    !> Used only for merging jobs.  Port of
    !> ediff-merg.el:ediff-shrink-window-C (the command).
    recursive subroutine cmd_shrink_ediff_window_c(status)
        type(status_type), intent(out) :: status

        call shrink_ediff_window_c(current_prefix_arg, status)
    end subroutine cmd_shrink_ediff_window_c

    !> Combine Nth diff regions of buffers A and B and place the
    !> combination in C.
    !> N is a prefix argument.  If absent, combine the current difference
    !> regions.
    !> Combining is done according to the specifications in variable
    !> `ediff_combination_pattern'.  Port of ediff-merg.el:ediff-combine-diffs
    !> (the command).
    recursive subroutine cmd_combine_ediff_diffs(status)
        type(status_type), intent(out) :: status
        integer :: k, n

        call check_ediff_control_buffer(k, status)
        if (.not. status%ok()) return
        n = ediff_sessions(k)%current_difference
        if (current_prefix_arg%kind == PREFIX_NUMBER) n = int(current_prefix_arg%value) - 1
        call combine_ediff_diffs(k, n, status)
    end subroutine cmd_combine_ediff_diffs

    !> Register the commands of ediff-merg.el and give
    !> `ediff_combination_pattern' its default.  Not a port.
    subroutine init_ediff_merg()
        if (associated(commands%re_merge_ediff%execute)) return
        commands%re_merge_ediff%execute => re_merge_ediff
        commands%shrink_ediff_window_c%execute => cmd_shrink_ediff_window_c
        commands%combine_ediff_diffs%execute => cmd_combine_ediff_diffs

        if (.not. allocated(ediff_combination_pattern)) then
            ediff_combination_pattern = [ediff_combination_part_type(delimiter="<<<<<<< variant A", &
                                                                     buffer_type=EDIFF_BUFFER_TYPE_A), &
                                         ediff_combination_part_type(delimiter=">>>>>>> variant B", &
                                                                     buffer_type=EDIFF_BUFFER_TYPE_B), &
                                         ediff_combination_part_type(delimiter="####### Ancestor", &
                                                                     buffer_type=EDIFF_BUFFER_TYPE_ANCESTOR), &
                                         ediff_combination_part_type(delimiter="======= end", buffer_type=0)]
        end if
    end subroutine init_ediff_merg
end module ediff_merg_mod
