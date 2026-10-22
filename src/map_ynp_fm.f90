! map_ynp_fm.f90 -- general-purpose boolean question-asker.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/map-ynp.el.
!
! Copyright (C) 1991-1995, 2000-2026 Free Software Foundation, Inc.
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

!> `map-y-or-n-p`: ask about each of a list of objects in turn.
!>
!> Objects are numbered 1..n; the caller's prompter and actor procedures
!> stand in for upstream's PROMPTER and ACTOR functions.  As in subr_fm,
!> keys are read in the echo area (the `y-or-n-p-use-read-key` path).
!> There is no help (C-h) or ACTION-ALIST yet, so the prompt lists only
!> the keys that work.
module map_ynp_fm
    use error_fm, only: status_type, set_error, FM_QUIT
    use lisp_fm, only: value_type
    use keymap_fm, only: single_key_description
    use xdisp_fm, only: message
    use subr_fm, only: read_key, query_replace_answer, ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, &
                       ANSWER_ACT_AND_EXIT, ANSWER_AUTOMATIC, ANSWER_QUIT
    implicit none(type, external)
    private

    public :: map_y_or_n_p, ynp_prompter, ynp_actor
    public :: YNP_SKIP, YNP_ASK, YNP_ACT

    !> What a prompter says about an object: skip it, ask about it (with
    !> the prompt it returns), or act on it without asking (upstream: nil,
    !> a string, or another non-nil value).
    integer, parameter :: YNP_SKIP = 0, YNP_ASK = 1, YNP_ACT = 2

    abstract interface
        !> Whether and how to ask about object `k`.
        subroutine ynp_prompter(k, how, prompt)
            implicit none(type, external)
            integer, intent(in) :: k
            integer, intent(out) :: how
            character(len=:), allocatable, intent(out) :: prompt
        end subroutine ynp_prompter

        !> Act on object `k`.
        subroutine ynp_actor(k, status)
            import :: status_type
            implicit none(type, external)
            integer, intent(in) :: k
            type(status_type), intent(out) :: status
        end subroutine ynp_actor
    end interface

contains

    !> Ask about objects 1..`n` in turn and act on those the user accepts:
    !> SPC or y acts, DEL or n skips, ! acts on all the rest, . acts and
    !> stops, RET or q stops, C-g quits.  `actions` is the number of
    !> objects acted on.  Port of map-ynp.el:map-y-or-n-p.
    subroutine map_y_or_n_p(n, prompter, actor, actions, status)
        integer, intent(in) :: n
        procedure(ynp_prompter) :: prompter
        procedure(ynp_actor) :: actor
        integer, intent(out) :: actions
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prompt, full
        type(value_type) :: key
        integer :: k, how, def
        logical :: automatic, done

        actions = 0
        automatic = .false.
        k = 0

        do while (k < n)
            k = k + 1
            call prompter(k, how, prompt)
            if (how == YNP_SKIP) cycle

            if (how == YNP_ACT .or. automatic) then
                call actor(k, status)
                if (.not. status%ok()) return
                actions = actions + 1
                cycle
            end if

            full = prompt//"(y, n, !, ., or q) "
            done = .false.

            do while (.not. done)
                call read_key(full, key, status)
                if (.not. status%ok()) return
                ! Show the answer to the question.
                call message(full//single_key_description(key))
                def = query_replace_answer(key)
                done = .true.

                select case (def)
                case (ANSWER_QUIT)
                    call set_error(status, FM_QUIT)
                    return
                case (ANSWER_EXIT)
                    ! Skip this and all remaining objects.
                    return
                case (ANSWER_ACT)
                    call actor(k, status)
                    if (.not. status%ok()) return
                    actions = actions + 1
                case (ANSWER_SKIP)
                    ! Nothing to do: on to the next object.
                case (ANSWER_ACT_AND_EXIT)
                    call actor(k, status)
                    if (.not. status%ok()) return
                    actions = actions + 1
                    return
                case (ANSWER_AUTOMATIC)
                    call actor(k, status)
                    if (.not. status%ok()) return
                    actions = actions + 1
                    automatic = .true.
                case default
                    ! A random key: ask again.
                    done = .false.
                end select
            end do
        end do
    end subroutine map_y_or_n_p
end module map_ynp_fm
