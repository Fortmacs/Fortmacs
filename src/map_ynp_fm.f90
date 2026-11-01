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

!> `map_read_y_or_n': ask about each of a list of objects in turn.
!>
!> Objects are numbered 1..n; the caller's prompter and actor procedures
!> stand in for upstream's PROMPTER and ACTOR functions.  The answer is
!> read in the minibuffer, or with `read_key' when
!> `read_y_or_n_use_read_key' is set.  Not ported: ACTION-ALIST, dialog
!> boxes, NO-CURSOR-IN-ECHO-AREA, switch-frame events.
!>
!> `read_answer': a question with long answers or their one-character
!> abbreviations.  Not ported: dialog boxes, short answers that are
!> function keys or other non-character events.
module map_ynp_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_QUIT
    use lisp_fm, only: value_type, command_type
    use handle_fm, only: handle_type
    implicit none(type, external)
    private

    public :: map_read_y_or_n, ynp_prompter_procedure, ynp_actor_procedure
    public :: YNP_SKIP, YNP_ASK, YNP_ACT
    public :: read_answer, read_answer_type

    !> What a prompter says about an object: skip it, ask about it (with
    !> the prompt it returns), or act on it without asking (YNP_SKIP, YNP_ASK and YNP_ACT).
    integer, parameter :: YNP_SKIP = 0, YNP_ASK = 1, YNP_ACT = 2

    !> Values of `read_answer_short'.
    integer, parameter, public :: READ_ANSWER_SHORT_NO = 0, READ_ANSWER_SHORT_YES = 1, READ_ANSWER_SHORT_AUTO = 2

    !> Whether the `read_answer' function accepts single-character answers.
    !> If READ_ANSWER_SHORT_YES, accept short (single key-press) answers to the
    !> question.  If READ_ANSWER_SHORT_NO, require long answers.  If
    !> READ_ANSWER_SHORT_AUTO, accept short answers if
    !> `use_short_answers' is true.
    !>
    !> Note that this variable does not affect calls to the more
    !> commonly-used `read_yes_or_no' function; it only affects calls to
    !> the `read_answer' function.  To control whether `read_yes_or_no'
    !> requires a long or a short answer, see the `use_short_answers'
    !> variable.  Port of map-ynp.el read-answer-short
    !> (READ_ANSWER_SHORT_NO, READ_ANSWER_SHORT_YES or
    !> READ_ANSWER_SHORT_AUTO).
    integer, public :: read_answer_short = READ_ANSWER_SHORT_AUTO

    !> One answer of `read_answer': LONG-ANSWER, a complete answer;
    !> SHORT-ANSWER, its one-character abbreviation (a character code);
    !> HELP-MESSAGE, what the answer means.
    type :: read_answer_type
        character(len=:), allocatable :: long
        integer :: short = 0
        character(len=:), allocatable :: help
    end type read_answer_type

    abstract interface
        !> Whether and how to ask about object `k'.  Not a port.
        subroutine ynp_prompter_procedure(k, how, prompt)
            implicit none(type, external)
            integer, intent(in) :: k
            integer, intent(out) :: how
            character(len=:), allocatable, intent(out) :: prompt
        end subroutine ynp_prompter_procedure

        !> Act on object `k'.  Not a port.
        subroutine ynp_actor_procedure(k, status)
            import :: status_type
            implicit none(type, external)
            integer, intent(in) :: k
            type(status_type), intent(out) :: status
        end subroutine ynp_actor_procedure
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
        module subroutine map_read_y_or_n(n, prompter, actor, actions, status, help_object, help_objects, help_action)
            implicit none(type, external)
            integer, intent(in) :: n
            procedure(ynp_prompter_procedure) :: prompter
            procedure(ynp_actor_procedure) :: actor
            integer, intent(out) :: actions
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: help_object, help_objects, help_action
        end subroutine map_read_y_or_n

        !> Read an answer either as a complete word or its character abbreviation.
        !> Ask user a question and accept an answer from the list of possible answers.
        !> Return the long answer even when accepting short ones.
        !>
        !> QUESTION should end in a space; this function adds a list of answers to it.
        !>
        !> ANSWERS is a list of `read_answer_type' entries, each with
        !>   LONG, a complete answer,
        !>   SHORT, an abbreviated one-character answer,
        !>   HELP, a string describing the meaning of the answer.
        !>
        !> Example:
        !>   [read_answer_type("yes", ichar("y"), "perform the action"),
        !>    read_answer_type("no", ichar("n"), "skip to the next"),
        !>    read_answer_type("all", ichar("!"), "accept all remaining without more questions"),
        !>    read_answer_type("help", ichar("h"), "show help"),
        !>    read_answer_type("quit", ichar("q"), "exit")]
        !>
        !> When `read_answer_short' is true, accept short answers.  Port of
        !> map-ynp.el:read-answer (no dialog boxes).  (In map_ynp_ask_fm.)
        recursive module subroutine read_answer(question, answers, answer, status)
            implicit none(type, external)
            character(len=*), intent(in) :: question
            type(read_answer_type), intent(in) :: answers(:)
            character(len=:), allocatable, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine read_answer
    end interface

    !> The minibuffer keymap of `map_read_y_or_n': each key of
    !> query-replace-map runs the command that records it and exits
    !> (cmd-char upstream), keys that would self-insert say "Type C-h for
    !> help" (cmd-help).  Made on first use.
    type(handle_type), public :: ynp_remap
    !> The commands cmd-char and cmd-help of `ynp_remap'.
    type(command_type), target, public :: cmd_char, cmd_help
    !> The keys cmd-char recorded (unallocated for none).
    type(value_type), allocatable, public :: chars(:)

    !> The answers of the `read_answer' being asked with short answers
    !> (its help answer included), the message for a wrong key, and its
    !> minibuffer keymap (read-answer's short-answer-map), whose keys run
    !> the anonymous commands `read_answer_select' and
    !> `read_answer_reject'.
    type(read_answer_type), allocatable, public :: short_answers(:)
    !> The message for a key that is not one of the `short_answers'.
    character(len=:), allocatable, public :: short_answer_message
    !> The minibuffer keymap of the `short_answers' (read-answer's
    !> short-answer-map).
    type(handle_type), public :: short_answer_map
    !> The anonymous commands `read_answer_select' and
    !> `read_answer_reject' of `short_answer_map'.
    type(command_type), target, public :: cmd_answer_select, cmd_answer_reject
end module map_ynp_fm
