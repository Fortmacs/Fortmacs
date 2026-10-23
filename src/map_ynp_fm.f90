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

!> `map_y_or_n_p': ask about each of a list of objects in turn.
!>
!> Objects are numbered 1..n; the caller's prompter and actor procedures
!> stand in for upstream's PROMPTER and ACTOR functions.  The answer is
!> read in the minibuffer, or with `read_key' when
!> `y_or_n_p_use_read_key' is set.  Not ported: ACTION-ALIST, dialog
!> boxes, NO-CURSOR-IN-ECHO-AREA, switch-frame events.
module map_ynp_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_QUIT
    use lisp_fm, only: value_type, command_type
    use handle_fm, only: handle_type
    implicit none(type, external)
    private

    public :: map_y_or_n_p, ynp_prompter, ynp_actor
    public :: YNP_SKIP, YNP_ASK, YNP_ACT

    !> What a prompter says about an object: skip it, ask about it (with
    !> the prompt it returns), or act on it without asking (upstream: nil,
    !> a string, or another non-nil value).
    integer, parameter :: YNP_SKIP = 0, YNP_ASK = 1, YNP_ACT = 2

    abstract interface
        !> Whether and how to ask about object `k'.
        subroutine ynp_prompter(k, how, prompt)
            implicit none(type, external)
            integer, intent(in) :: k
            integer, intent(out) :: how
            character(len=:), allocatable, intent(out) :: prompt
        end subroutine ynp_prompter

        !> Act on object `k'.
        subroutine ynp_actor(k, status)
            import :: status_type
            implicit none(type, external)
            integer, intent(in) :: k
            type(status_type), intent(out) :: status
        end subroutine ynp_actor
    end interface

    interface
        !> Ask a boolean question per PROMPTER for each object in LIST.
        !> The PROMPTER is called for objects 1..N; it returns how to treat
        !> each: YNP_SKIP to ignore it, YNP_ACT to act on it without asking,
        !> or YNP_ASK with a prompt to ask the user about it.
        !>
        !> The user's answers are: `y' or SPC to act on the object, `n' or DEL
        !> to skip it, `!' to act on it and all remaining objects, `.' to act
        !> on it and exit, `q' or RET to exit, and C-g to quit; C-h shows
        !> help.  HELP-OBJECT, HELP-OBJECTS and HELP-ACTION (default
        !> "object", "objects", "act on") name the objects and the action for
        !> the help.  `actions' is the number of actions taken.  Port of
        !> map-ynp.el:map-y-or-n-p.  (In map_ynp_ask_fm.)
        module subroutine map_y_or_n_p(n, prompter, actor, actions, status, help_object, help_objects, help_action)
            implicit none(type, external)
            integer, intent(in) :: n
            procedure(ynp_prompter) :: prompter
            procedure(ynp_actor) :: actor
            integer, intent(out) :: actions
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: help_object, help_objects, help_action
        end subroutine map_y_or_n_p
    end interface

    !> The minibuffer keymap of `map_y_or_n_p': each key of
    !> query-replace-map runs the command that records it and exits
    !> (cmd-char upstream), keys that would self-insert say "Type C-h for
    !> help" (cmd-help).  Made on first use.
    type(handle_type), public :: ynp_remap
    type(command_type), target, public :: cmd_char, cmd_help
    !> The keys cmd-char recorded (unallocated for none).
    type(value_type), allocatable, public :: chars(:)

end module map_ynp_fm
