! fns_ask_fm.f90 -- yes-or-no-p.
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

!> Part of fns_fm.  A submodule can use minibuf_fm even though the
!> minibuffer depends on fns_fm, which a module cannot.
submodule(fns_fm) fns_ask_fm
    use minibuf_fm, only: read_from_minibuffer, enable_recursive_minibuffers
    use keyboard_fm, only: discard_input, sit_for, real_this_command
    use lisp_fm, only: value_type
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use subr_fm, only: y_or_n_p
    use casefiddle_fm, only: downcase_string
    use character_fm, only: blankp
    implicit none(type, external)

contains

    recursive module subroutine yes_or_no_p(prompt, answer, status)
        character(len=*), intent(in) :: prompt
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: full, ans
        type(value_type) :: saved_real_this
        integer(int64) :: n
        integer(int32) :: c, clen
        integer(int64) :: byte, last
        logical :: saved_enable, nonspace
        answer = .false.
        if (use_short_answers) then
            call y_or_n_p(prompt, answer, status)
            return
        end if
        ! Whether PROMPT ends in a non-blank character.
        nonspace = .false.
        n = schars(prompt)
        if (n > 0) then
            byte = 1
            last = 0
            do while (byte <= len(prompt))
                call string_char_and_length(prompt, byte, c, clen)
                last = c
                byte = byte + clen
            end do
            nonspace = .not. blankp(int(last, int32))
        end if
        full = prompt
        if (nonspace) full = full//" "
        if (allocated(yes_or_no_prompt)) then
            full = full//yes_or_no_prompt
        else
            full = full//"(yes or no) "
        end if
        saved_enable = enable_recursive_minibuffers
        enable_recursive_minibuffers = .true.
        ! Preserve the actual command that eventually called
        ! `yes_or_no_p' (otherwise `repeat' will be repeating
        ! `exit_minibuffer').
        saved_real_this = real_this_command
        do
            call read_from_minibuffer(full, ans, status, history="yes_or_no_p_history")
            if (.not. status%ok()) exit
            ans = downcase_string(ans)
            if (ans == "yes" .and. len(ans) == 3) then
                answer = .true.
                exit
            end if
            if (ans == "no" .and. len(ans) == 2) exit
            call ding()
            call discard_input()
            call message("Please answer yes or no.")
            call sit_for(2000)
        end do
        enable_recursive_minibuffers = saved_enable
        real_this_command = saved_real_this
    end subroutine yes_or_no_p
end submodule fns_ask_fm
