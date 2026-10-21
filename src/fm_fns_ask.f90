! fm_fns_ask.f90 -- yes-or-no-p.
! Ported from GNU Emacs 31.1 src/fns.c.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Part of fm_fns.  A submodule can use fm_minibuf even though the
!> minibuffer depends on fm_fns, which a module cannot.
submodule(fm_fns) fm_fns_ask
    use fm_minibuf, only: read_from_minibuffer, enable_recursive_minibuffers
    use fm_keyboard, only: discard_input, sit_for
    use fm_xdisp, only: message
    use fm_subr, only: y_or_n_p
    implicit none(type, external)

    !> use-short-answers: ask with y-or-n-p instead.
    logical :: use_short_answers = .false.

contains

    recursive module subroutine yes_or_no_p(prompt, answer, status)
        character(len=*), intent(in) :: prompt
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: full, ans
        logical :: saved_enable

        answer = .false.
        if (use_short_answers) then
            call y_or_n_p(prompt, answer, status)
            return
        end if
        full = prompt
        if (len(prompt) > 0) then
            if (prompt(len(prompt):len(prompt)) /= " " .and. prompt(len(prompt):len(prompt)) /= achar(9)) then
                full = full//" "
            end if
        end if
        ! yes-or-no-prompt
        full = full//"(yes or no) "
        saved_enable = enable_recursive_minibuffers
        enable_recursive_minibuffers = .true.
        do
            call read_from_minibuffer(full, ans, status, history="yes-or-no-p-history")
            if (.not. status%ok()) exit
            ans = downcase_ascii(ans)
            if (ans == "yes" .and. len(ans) == 3) then
                answer = .true.
                exit
            end if
            if (ans == "no" .and. len(ans) == 2) exit
            ! (Fding is not ported.)
            call discard_input()
            call message("Please answer yes or no.")
            call sit_for(2000)
        end do
        enable_recursive_minibuffers = saved_enable
    contains
        !> ASCII-only Fdowncase.
        pure function downcase_ascii(s) result(d)
            character(len=*), intent(in) :: s
            character(len=len(s)) :: d
            integer :: i

            d = s
            do i = 1, len(s)
                if (s(i:i) >= "A" .and. s(i:i) <= "Z") d(i:i) = achar(iachar(s(i:i)) + 32)
            end do
        end function downcase_ascii
    end subroutine yes_or_no_p

end submodule fm_fns_ask
