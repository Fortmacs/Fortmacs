! fortmacs_diff_merging_mod.f90 -- merging with Ediff.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-merg.el (in part: see
! docs/status/fortmacs_diff_merging_mod.md).
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
!> the default variant (`fortmacs_diff_default_variant': A's region, B's, or both
!> combined by `fortmacs_diff_combination_pattern'), or, when merging with an
!> ancestor, by the variant that changed the region (prefer-A, prefer-B).
!> The control buffer's keys + (combine), & (merge again with a new
!> default) and s (shrink window C) are here; fortmacs_diff_utilities_mod's keys n and
!> p skip regions with the tests here (through `check_region_skip' of
!> `fortmacs_diff_session_type') when $$ or $* asks for it.  The procedures
!> that act on one session are type-bound procedures of that type, in
!> fortmacs_diff_settings_merging_mod.
module fortmacs_diff_merging_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, substitute_quotes, USER_ERROR_CODE
    use value_mod, only: value_type, make_list
    use data_mod, only: choice_setting_type
    use hooks_mod, only: hook_type
    use execute_interactively_mod, only: prefix_argument_type, current_prefix_argument, PREFIX_NUMBER, PREFIX_MINUS
    use commands_mod, only: commands
    use files_mod, only: normal_mode, write_file_functions
    use minibuffer_reading_mod, only: completion_list_table
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use window_mod, only: window_total_height
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, check_fortmacs_diff_control_buffer, &
                                          FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_B, &
                                          FORTMACS_DIFF_BUFFER_TYPE_C, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR, &
                                          FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A, FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_B, &
                                          FORTMACS_DIFF_STATE_OF_MERGE_COMBINED
    use fortmacs_diff_utilities_mod, only: recenter_fortmacs_diff
    implicit none(type, external)
    private

    public :: fortmacs_diff_variant_state_of_merge, set_fortmacs_diff_merge_mode, regular_expression_merge_fortmacs_diff, &
              shrink_fortmacs_diff_window_c, fortmacs_diff_default_variant_name, initialize_fortmacs_diff_merg

    !> Hooks to run before quitting a merge job.
    !> The most common use is to save and delete the merge buffer.  Port of
    !> ediff-merg.el ediff-quit-merge-hook (its default function,
    !> `maybe_save_and_delete_fortmacs_diff_merge', is added by fortmacs_diff_utilities_mod).
    type(hook_type), target, public :: fortmacs_diff_quit_merge_hook = hook_type(name="fortmacs_diff_quit_merge_hook")

    !> Values of `fortmacs_diff_default_variant'.
    integer, parameter, public :: FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A = 2, FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B = 3, &
                                  FORTMACS_DIFF_DEFAULT_VARIANT_COMBINED = 4

    !> The variant to be used as a default for buffer C in merging.
    !> Valid values are FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A,
    !> FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B, and FORTMACS_DIFF_DEFAULT_VARIANT_COMBINED.
    !> Port of ediff-merg.el ediff-default-variant (each session takes its
    !> own value when it starts).
    type(choice_setting_type), public :: fortmacs_diff_default_variant = &
                                         choice_setting_type(name="fortmacs_diff_default_variant", true_valid=.false., &
                                                             false_valid=.false., choice=FORTMACS_DIFF_DEFAULT_VARIANT_COMBINED)

    !> One part of `fortmacs_diff_combination_pattern': the line DELIMITER, then the
    !> difference region of BUFFER_TYPE (FORTMACS_DIFF_BUFFER_TYPE_A, _B or
    !> _ANCESTOR; 0 for the last delimiter, which has no region after it).
    type, public :: fortmacs_diff_combination_part_type
        character(len=80) :: delimiter = ""
        integer :: buffer_type = 0
    end type fortmacs_diff_combination_part_type

    !> Pattern to be used for combining difference regions in buffers A
    !> and B.
    !> The value must be an array of the form
    !> [fortmacs_diff_combination_part_type(STRING1, bufspec1), (STRING2, bufspec2),
    !> (STRING3, bufspec3), (STRING4, 0)]
    !> where bufspec is the buffer type FORTMACS_DIFF_BUFFER_TYPE_A,
    !> FORTMACS_DIFF_BUFFER_TYPE_B, or FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR.  For instance, if
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
    type(fortmacs_diff_combination_part_type), allocatable, public :: fortmacs_diff_combination_pattern(:)

    !> If .true., show only diff regions where both buffers disagree with
    !> the ancestor.
    !> This means that regions that prefer variant A or variant B (only
    !> one of them changed the ancestor's text) will be skipped over.  A value of .false. means show all regions.  Port of
    !> ediff-merg.el ediff-show-clashes-only (each session takes its own
    !> value when it starts).
    logical, public :: fortmacs_diff_show_clashes_only = .false.

    !> If .true., show only the regions that have not been changed by the
    !> user.
    !> A region is considered to have been changed if it is different from
    !> the current default (FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A, _DEFAULT_B,
    !> _COMBINED) and it hasn't been marked as preferring variant A or B.
    !> A region is considered to have been changed also when it is marked
    !> as preferring A, but is different from the corresponding difference
    !> region in Buffer A or if it is marked as preferring B and is
    !> different from the region in Buffer B.  Port of ediff-merg.el
    !> ediff-skip-merge-regions-that-differ-from-default (each session takes
    !> its own value when it starts).
    logical, public :: fortmacs_diff_skip_merge_regions_that_differ_from_default = .false.

contains

    !> The state of a merge region set by VARIANT, a value of
    !> `fortmacs_diff_default_variant' (FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A, _DEFAULT_B
    !> or _COMBINED).  Not a port: upstream keeps the variant's symbol as
    !> the state.
    pure integer function fortmacs_diff_variant_state_of_merge(variant) result(state)
        integer, intent(in) :: variant

        select case (variant)
        case (FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A)
            state = FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A
        case (FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B)
            state = FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_B
        case default
            state = FORTMACS_DIFF_STATE_OF_MERGE_COMBINED
        end select
    end function fortmacs_diff_variant_state_of_merge

    !> The name of VARIANT (FORTMACS_DIFF_DEFAULT_VARIANT_*) as the user reads and
    !> types it: the named constant.  Not a port: upstream prints the
    !> symbol.
    pure function fortmacs_diff_default_variant_name(variant) result(name)
        integer, intent(in) :: variant
        character(len=:), allocatable :: name

        select case (variant)
        case (FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A)
            name = "FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A"
        case (FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B)
            name = "FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_B"
        case default
            name = "FORTMACS_DIFF_DEFAULT_VARIANT_COMBINED"
        end select
    end function fortmacs_diff_default_variant_name

    !> A function of the merge buffer's own `write_file_functions': give
    !> the buffer its mode by its file name (as when it is visited), and
    !> leave the hook.  DONE is .true., as upstream's remove-hook returns
    !> the hook's symbol when it clears the buffer's own value.  Port of
    !> ediff-merg.el:ediff-set-merge-mode.
    recursive subroutine set_fortmacs_diff_merge_mode(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status

        done = .false.
        call normal_mode(status, find_file=.true.)
        if (.not. status%ok()) return
        call write_file_functions%remove(set_fortmacs_diff_merge_mode, local=.true.)
        done = .true.
    end subroutine set_fortmacs_diff_merge_mode

    !> Remerge unmodified diff regions using a new default.
    !> Start with the current region.  Port of ediff-merg.el:ediff-re-merge.
    recursive subroutine regular_expression_merge_fortmacs_diff(status)
        type(status_type), intent(out) :: status
        type(value_type) :: items(2)
        character(len=:), allocatable :: answer
        integer :: session_index, variant, j

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        j = 0

        ! The variants other than the current default.
        do variant = FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A, FORTMACS_DIFF_DEFAULT_VARIANT_COMBINED
            if (variant == fortmacs_diff_sessions(session_index)%default_variant%choice) cycle
            j = j + 1
            items(j) = value_type(text=fortmacs_diff_default_variant_name(variant))
        end do

        call completing_read(substitute_quotes("Current merge default is `")// &
                             fortmacs_diff_default_variant_name(fortmacs_diff_sessions(session_index)%default_variant%choice)// &
                             substitute_quotes("'.  New default: "), completion_list_table(make_list(items(1:j))), answer, &
                             status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return

        do variant = FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A, FORTMACS_DIFF_DEFAULT_VARIANT_COMBINED
            if (answer == fortmacs_diff_default_variant_name(variant)) then
                fortmacs_diff_sessions(session_index)%default_variant%choice = variant
            end if
        end do

        call fortmacs_diff_sessions(session_index)%merge_differences(fortmacs_diff_sessions(session_index)%current_difference, &
                                                                     status, remerging=.true.)
        if (.not. status%ok()) return
        call recenter_fortmacs_diff(status)
    end subroutine regular_expression_merge_fortmacs_diff

    !> Shrink window C to just one line.
    !> With a prefix argument ARGUMENT, returns window C to its normal size.
    !> Used only for merging jobs.  Port of
    !> ediff-merg.el:ediff-shrink-window-C.
    recursive subroutine shrink_fortmacs_diff_window_c(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        real :: saved
        integer :: session_index, height_a, height_c
        integer(int64) :: n

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return

        if (.not. fortmacs_diff_sessions(session_index)%merge_job) then
            call set_error(status, USER_ERROR_CODE, "shrink_fortmacs_diff_window_c can be used only for merging jobs")
            return
        end if

        select case (argument%kind)
        case (PREFIX_MINUS)
            n = -1
        case (PREFIX_NUMBER)
            n = argument%value
        case default
            ! No number: shrink window C, or return it to its size, for
            ! this redisplay only.
            saved = fortmacs_diff_sessions(session_index)%merge_window_share
            height_c = window_total_height(fortmacs_diff_sessions(session_index)%windows(FORTMACS_DIFF_BUFFER_TYPE_C), status)
            if (.not. status%ok()) return
            if (height_c >= 3) fortmacs_diff_sessions(session_index)%merge_window_share = 0
            ! Force redisplay.
            fortmacs_diff_sessions(session_index)%window_configuration_saved = ""
            call recenter_fortmacs_diff(status, no_rehighlight=.true.)
            fortmacs_diff_sessions(session_index)%merge_window_share = saved
            return
        end select

        height_a = 0
        height_c = 0

        if (n < 0) then
            height_c = window_total_height(fortmacs_diff_sessions(session_index)%windows(FORTMACS_DIFF_BUFFER_TYPE_C), status)
            if (.not. status%ok()) return
        else if (n > 0) then
            height_a = window_total_height(fortmacs_diff_sessions(session_index)%windows(FORTMACS_DIFF_BUFFER_TYPE_A), status)
            if (.not. status%ok()) return
        end if

        if (n < 0 .and. height_c > 2) then
            fortmacs_diff_sessions(session_index)%merge_window_share = fortmacs_diff_sessions(session_index)%merge_window_share*0.9
        else if (n > 0 .and. height_a > 2) then
            fortmacs_diff_sessions(session_index)%merge_window_share = fortmacs_diff_sessions(session_index)%merge_window_share*1.1
        else
            return
        end if

        ! Force redisplay.
        fortmacs_diff_sessions(session_index)%window_configuration_saved = ""
        call recenter_fortmacs_diff(status, no_rehighlight=.true.)
    end subroutine shrink_fortmacs_diff_window_c

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Shrink window C to just one line.
    !> With a prefix argument, returns window C to its normal size.
    !> Used only for merging jobs.  Port of
    !> ediff-merg.el:ediff-shrink-window-C (the command).
    recursive subroutine execute_shrink_fortmacs_diff_window_c(status)
        type(status_type), intent(out) :: status

        call shrink_fortmacs_diff_window_c(current_prefix_argument, status)
    end subroutine execute_shrink_fortmacs_diff_window_c

    !> Combine Nth diff regions of buffers A and B and place the
    !> combination in C.
    !> N is a prefix argument.  If absent, combine the current difference
    !> regions.
    !> Combining is done according to the specifications in variable
    !> `fortmacs_diff_combination_pattern'.  Port of ediff-merg.el:ediff-combine-diffs
    !> (the command).
    recursive subroutine execute_combine_fortmacs_diff_diffs(status)
        type(status_type), intent(out) :: status
        integer :: session_index, n

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        n = fortmacs_diff_sessions(session_index)%current_difference
        if (current_prefix_argument%kind == PREFIX_NUMBER) n = int(current_prefix_argument%value) - 1
        call fortmacs_diff_sessions(session_index)%combine_differences(n, status)
    end subroutine execute_combine_fortmacs_diff_diffs

    !> Register the commands of ediff-merg.el and give
    !> `fortmacs_diff_combination_pattern' its default.  Not a port.
    subroutine initialize_fortmacs_diff_merg()
        if (associated(commands%regular_expression_merge_fortmacs_diff%execute)) return
        commands%regular_expression_merge_fortmacs_diff%execute => regular_expression_merge_fortmacs_diff
        commands%shrink_fortmacs_diff_window_c%execute => execute_shrink_fortmacs_diff_window_c
        commands%combine_fortmacs_diff_diffs%execute => execute_combine_fortmacs_diff_diffs

        if (.not. allocated(fortmacs_diff_combination_pattern)) then
            fortmacs_diff_combination_pattern = [fortmacs_diff_combination_part_type(delimiter="<<<<<<< variant A", &
                                                                                     buffer_type=FORTMACS_DIFF_BUFFER_TYPE_A), &
                                                 fortmacs_diff_combination_part_type(delimiter=">>>>>>> variant B", &
                                                                                     buffer_type=FORTMACS_DIFF_BUFFER_TYPE_B), &
                                                 fortmacs_diff_combination_part_type(delimiter="####### Ancestor", &
                                                                                  buffer_type=FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR), &
                                                 fortmacs_diff_combination_part_type(delimiter="======= end", buffer_type=0)]
        end if
    end subroutine initialize_fortmacs_diff_merg
end module fortmacs_diff_merging_mod
