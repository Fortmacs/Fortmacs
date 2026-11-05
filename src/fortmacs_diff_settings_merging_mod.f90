! fortmacs_diff_settings_merging_mod.f90 -- the merging procedures of an Ediff session,
! a submodule of fortmacs_diff_settings_mod.
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

!> The type-bound procedures of `fortmacs_diff_session_type' that port
!> ediff-merg.el (the merge states, merging and combining the variants, and
!> the merge-region skip check of n and p): a submodule of the type's module,
!> fortmacs_diff_settings_mod, because they use fortmacs_diff_merging_mod,
!> which uses it.
submodule(fortmacs_diff_settings_mod) fortmacs_diff_settings_merging_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use display_engine_mod, only: message
    use fortmacs_diff_utilities_mod, only: jump_to_fortmacs_diff_difference
    use fortmacs_diff_merging_mod, only: fortmacs_diff_combination_pattern, fortmacs_diff_variant_state_of_merge
    implicit none(type, external)

    character, parameter :: LF = achar(10)

contains

    module integer function get_state_of_merge(self, difference) result(state)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(in) :: difference

        state = FORTMACS_DIFF_STATE_OF_MERGE_NONE
        if (.not. allocated(self%merge_states)) return
        if (difference < 0 .or. difference >= size(self%merge_states)) return
        state = self%merge_states(difference + 1)%state_of_merge
    end function get_state_of_merge

    module logical function is_merge_region_non_clash(self, difference)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(in) :: difference

        is_merge_region_non_clash = .true.
        if (.not. self%merge_job) return
        is_merge_region_non_clash = any(self%get_state_of_merge(difference) == [FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A, &
                                                                                FORTMACS_DIFF_STATE_OF_MERGE_PREFER_B])
    end function is_merge_region_non_clash

    recursive module subroutine get_combined_region(self, difference, text, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        character(len=*), parameter :: ERR_MESSAGE = &
                                       "fortmacs_diff_combination_pattern: Invalid format. Please consult the documentation"
        character(len=:), allocatable :: region
        type(status_type) :: st
        integer :: j, m
        integer :: session_index

        session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        status = status_type()
        text = ""
        m = 0
        if (allocated(fortmacs_diff_combination_pattern)) m = size(fortmacs_diff_combination_pattern)

        if (m < 3) then
            call set_error(status, GENERIC_ERROR_CODE, ERR_MESSAGE)
            return
        end if

        do j = 1, m - 1
            if (all(fortmacs_diff_combination_pattern(j)%buffer_type /= [FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_B, &
                                                                         FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR])) then
                call set_error(status, GENERIC_ERROR_CODE, ERR_MESSAGE)
                return
            end if

            call fortmacs_diff_sessions(session_index)%get_region_contents(difference, &
                                                                       fortmacs_diff_combination_pattern(j)%buffer_type, region, st)
            if (st%ok()) text = text//trim(fortmacs_diff_combination_pattern(j)%delimiter)//LF//region
        end do

        if (fortmacs_diff_combination_pattern(m)%buffer_type /= 0) then
            call set_error(status, GENERIC_ERROR_CODE, ERR_MESSAGE)
            return
        end if

        text = text//trim(fortmacs_diff_combination_pattern(m)%delimiter)//LF
    end subroutine get_combined_region

    recursive module subroutine set_all_difference_states(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer :: n
        integer :: session_index

        session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        status = status_type()

        do n = 0, fortmacs_diff_sessions(session_index)%number_of_differences - 1
            call fortmacs_diff_sessions(session_index)%set_difference_state(n, status)
            if (.not. status%ok()) return
        end do
    end subroutine set_all_difference_states

    recursive module subroutine set_difference_state(self, difference, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: reg_a, reg_b, reg_c, combined
        integer :: session_index

        session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, FORTMACS_DIFF_BUFFER_TYPE_A, reg_a, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, FORTMACS_DIFF_BUFFER_TYPE_B, reg_b, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, FORTMACS_DIFF_BUFFER_TYPE_C, reg_c, status)
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
            call fortmacs_diff_sessions(session_index)%get_combined_region(difference, combined, status)
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

            fortmacs_diff_sessions(session_index)%differences(FORTMACS_DIFF_BUFFER_TYPE_A, difference + 1)%state_of_diff = state_a
            fortmacs_diff_sessions(session_index)%differences(FORTMACS_DIFF_BUFFER_TYPE_B, difference + 1)%state_of_diff = state_b
            fortmacs_diff_sessions(session_index)%differences(FORTMACS_DIFF_BUFFER_TYPE_C, difference + 1)%state_of_diff = state_c
        end subroutine set_states
    end subroutine set_difference_state

    recursive module subroutine merge_differences(self, diff_number, status, remerging)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: diff_number
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: remerging
        integer :: n, state, from_type
        logical :: do_not_copy, again, changed
        character(len=96) :: progress
        integer :: session_index

        session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        status = status_type()
        again = .false.
        if (present(remerging)) again = remerging
        n = max(0, diff_number)

        do while (n < fortmacs_diff_sessions(session_index)%number_of_differences)
            ! Reset after each cycle.
            do_not_copy = .false.

            if (modulo(n, 10) == 0) then
                if (again) then
                    write (progress, '("Re-merging buffers A & B into C ... region ", i0, " of ", i0)') n, &
                        fortmacs_diff_sessions(session_index)%number_of_differences
                else
                    write (progress, '("Merging buffers A & B into C ... region ", i0, " of ", i0)') n, &
                        fortmacs_diff_sessions(session_index)%number_of_differences
                end if

                call message(trim(progress))
            end if

            state = fortmacs_diff_sessions(session_index)%get_state_of_merge(n)

            if (again) then
                ! If region was edited since it was first set by default,
                ! or was preferred, then ignore.
                call fortmacs_diff_sessions(session_index)%check_region_changed(n, changed, status)
                if (.not. status%ok()) return

                if (changed .or. any(state == [FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A, FORTMACS_DIFF_STATE_OF_MERGE_PREFER_B])) then
                    do_not_copy = .true.
                end if

                ! Change state of merge for this diff, if necessary.
                if (.not. do_not_copy .and. any(state == [FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A, &
                                                          FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_B, &
                                                          FORTMACS_DIFF_STATE_OF_MERGE_COMBINED])) then
                    fortmacs_diff_sessions(session_index)%merge_states(n + 1)%state_of_merge = &
                        fortmacs_diff_variant_state_of_merge(fortmacs_diff_sessions(session_index)%default_variant%choice)
                end if
            end if

            ! State of merge may have changed, so check it once again.
            state = fortmacs_diff_sessions(session_index)%get_state_of_merge(n)

            if (.not. do_not_copy) then
                if (state == FORTMACS_DIFF_STATE_OF_MERGE_COMBINED) then
                    call fortmacs_diff_sessions(session_index)%combine_differences(n, status, batch_invocation=.true.)
                else
                    from_type = FORTMACS_DIFF_BUFFER_TYPE_B

                    if (any(state == [FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A, FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A])) then
                        from_type = FORTMACS_DIFF_BUFFER_TYPE_A
                    end if

                    call fortmacs_diff_sessions(session_index)%copy_difference(n, from_type, FORTMACS_DIFF_BUFFER_TYPE_C, &
                                                                               status, batch_invocation=.true.)
                end if

                if (.not. status%ok()) return
            end if

            n = n + 1
        end do

        call message("Merging buffers A & B into C ... Done")
    end subroutine merge_differences

    recursive module subroutine combine_differences(self, difference, status, batch_invocation)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: batch_invocation
        character(len=:), allocatable :: reg_combined
        logical :: batch
        integer :: session_index

        session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        batch = .false.
        if (present(batch_invocation)) batch = batch_invocation
        call fortmacs_diff_sessions(session_index)%get_combined_region(difference, reg_combined, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%copy_difference(difference, 0, FORTMACS_DIFF_BUFFER_TYPE_C, status, &
                                                                   batch_invocation=batch, &
                                                                   region_to_copy=reg_combined)
        if (.not. status%ok()) return
        if (.not. batch) call jump_to_fortmacs_diff_difference(int(difference + 1, int64), status)
    end subroutine combine_differences

    recursive module subroutine check_region_changed(self, difference, changed, status, prefers_too)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        integer, intent(in) :: difference
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: prefers_too
        character(len=:), allocatable :: reg_a, reg_b, reg_c, combined
        logical :: prefers
        integer :: session_index

        session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        changed = .false.
        prefers = .false.
        if (present(prefers_too)) prefers = prefers_too
        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, FORTMACS_DIFF_BUFFER_TYPE_A, reg_a, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, FORTMACS_DIFF_BUFFER_TYPE_B, reg_b, status)
        if (.not. status%ok()) return
        call fortmacs_diff_sessions(session_index)%get_region_contents(difference, FORTMACS_DIFF_BUFFER_TYPE_C, reg_c, status)
        if (.not. status%ok()) return

        ! If region was edited since it was first set by default.
        select case (fortmacs_diff_sessions(session_index)%get_state_of_merge(difference))
        case (FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A)
            changed = reg_a /= reg_c
        case (FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_B)
            changed = reg_b /= reg_c
        case (FORTMACS_DIFF_STATE_OF_MERGE_COMBINED)
            call fortmacs_diff_sessions(session_index)%get_combined_region(difference, combined, status)
            if (.not. status%ok()) return
            changed = combined /= reg_c
        case (FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A)
            changed = prefers .and. reg_a /= reg_c
        case (FORTMACS_DIFF_STATE_OF_MERGE_PREFER_B)
            changed = prefers .and. reg_b /= reg_c
        case default
            changed = .false.
        end select
    end subroutine check_region_changed

    module procedure check_region_skip
    integer :: session_index
    logical :: changed

    session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
    status = status_type()
    skip = .false.
    if (.not. fortmacs_diff_sessions(session_index)%merge_job) return

    if (fortmacs_diff_sessions(session_index)%show_clashes_only) then
        skip = fortmacs_diff_sessions(session_index)%is_merge_region_non_clash(difference)
    end if

    if (fortmacs_diff_sessions(session_index)%skip_merge_regions_that_differ_from_default) then
        call fortmacs_diff_sessions(session_index)%check_region_changed(difference, changed, status, prefers_too=.true.)
        if (.not. status%ok()) return
        skip = skip .or. changed
    end if
    end procedure check_region_skip
end submodule fortmacs_diff_settings_merging_mod
