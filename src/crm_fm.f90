! crm_fm.f90 -- read multiple strings with completion.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/crm.el.
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

!> This code defines a function, `completing_read_multiple', which
!> provides the ability to read multiple strings in the minibuffer,
!> with completion.  See that function's documentation for details.
!>
!> Deferred: choosing a completion from *Completions* (its
!> `choose_completion_string_functions' and
!> `completion_list_insert_choice_function': choosing is not ported),
!> `minibuffer_visible_completions', INHERIT_INPUT_METHOD.
module crm_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_list, list, stringp, xstring
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur
    use editfns_fm, only: point, goto_char, point_max, eobp, buffer_string
    use search_fm, only: re_search_forward, re_search_backward, looking_at, match_beginning, match_end, string_match, &
                         string_index_to_offset, string_chars
    use keymap_fm, only: make_sparse_keymap, set_keymap_parent, define_key, use_local_map, keymap_live_p, free_keymap
    use lisp_fm, only: list_elements
    use commands_fm, only: commands
    use minibuf_fm, only: minibuffer_prompt_end, minibuffer_setup_function, minibuffer_setup_proc, minibuf_level
    use minibuf_table_fm, only: completion_table_type, completion_predicate, TABLE_FUNCTION
    use minibuffer_fm, only: completing_read, completion_table_with_context, completion_in_region, &
                             minibuffer_completion_help, completion_complete_and_exit, completion_do_completion, &
                             minibuffer_completion_table, minibuffer_completion_predicate, exit_minibuffer, &
                             minibuffer_local_completion_map, minibuffer_local_must_match_map, REQUIRE_MATCH_NIL
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    implicit none(type, external)
    private

    public :: completing_read_multiple, crm_complete, crm_complete_word, crm_complete_and_exit, crm_completion_help, &
              syms_of_crm

    !> Separator regexp used for separating strings in
    !> `completing_read_multiple'.  It should be a regexp that does not
    !> match the list of completion candidates.  Its `separator' and
    !> `description' (`crm_separator_string' and
    !> `crm_separator_description') are shown as part of the prompt.  See
    !> the user option `crm_prompt'.  Port of crm.el crm-separator.
    character(len=:), allocatable, public :: crm_separator

    !> The `separator' property of `crm_separator': the separator shown in
    !> the prompt.
    character(len=:), allocatable, public :: crm_separator_string

    !> The `description' property of `crm_separator'.
    character(len=:), allocatable, public :: crm_separator_description

    !> Prompt format for `completing_read_multiple'.
    !> The prompt is formatted with the keys %d, %s and %p standing for the
    !> separator description, the separator itself and the original prompt
    !> respectively.  Port of crm.el crm-prompt.
    character(len=:), allocatable, public :: crm_prompt

    !> Local keymap for minibuffer multiple input with completion.
    !> Analog of `minibuffer_local_completion_map'.  Port of crm.el
    !> crm-local-completion-map.
    type(handle_type), public :: crm_local_completion_map

    !> Local keymap for minibuffer multiple input with exact match
    !> completion.  Analog of `minibuffer_local_must_match_map' for crm.
    !> Port of crm.el crm-local-must-match-map.
    type(handle_type), public :: crm_local_must_match_map

    ! crm-completion-table: the table of the current
    ! `completing_read_multiple'.
    type(completion_table_type) :: crm_completion_table

    ! The keymap `completing_read_multiple' uses, and the setup function
    ! it runs inside.
    type(handle_type) :: crm_map
    procedure(minibuffer_setup_proc), pointer :: crm_outer_setup => null()

    ! Whether `crm_complete_and_exit' should exit.
    logical :: doexit = .false.

contains

    ! Function used by `completing_read_multiple' to compute completion
    ! values: the element after the last separator of STRING, completed
    ! with `crm_completion_table'.  Port of crm.el:crm--collection-fn.
    recursive subroutine crm_collection_fn(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        integer(int64) :: beg, m, off
        type(status_type) :: st
        ! (BEG is the character index of the element.)
        beg = 1
        do
            m = string_match(crm_separator, string, st, beg)
            if (.not. st%ok() .or. m <= 0) exit
            beg = match_end(0, st)
        end do
        off = string_index_to_offset(string, beg)
        call completion_table_with_context(string(1:off), crm_completion_table, string(off + 1:), action, result, &
                                           status, pred, suffix)
    end subroutine crm_collection_fn

    ! Parse the minibuffer to find the current element: its boundaries
    ! START and END.  Port of crm.el:crm--current-element.
    subroutine current_element(start, end)
        integer(int64), intent(out) :: start, end
        integer(int64) :: bob, saved, found
        type(status_type) :: st
        bob = minibuffer_prompt_end()
        saved = point()
        call re_search_backward(crm_separator, found, st, bound=bob, noerror=1)
        if (st%ok() .and. found /= 0) then
            start = match_end(0, st)
        else
            start = bob
        end if
        call goto_char(saved)
        call re_search_forward(crm_separator, found, st, noerror=1)
        if (st%ok() .and. found /= 0) then
            end = match_beginning(0, st)
        else
            end = point_max()
        end if
        call goto_char(saved)
    end subroutine current_element

    !> Display a list of possible completions of the current minibuffer
    !> element.  Port of crm.el:crm-completion-help.
    recursive subroutine crm_completion_help(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        status = status_type()
        call current_element(beg, end)
        call minibuffer_completion_help(beg, end)
    end subroutine crm_completion_help

    !> Complete the current element.
    !> If no characters can be completed, display a list of possible
    !> completions.
    !>
    !> Return true if the current element is now a valid match; otherwise
    !> return false.  Port of crm.el:crm-complete.
    recursive subroutine crm_complete(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        logical :: valid
        status = status_type()
        call current_element(beg, end)
        if (associated(minibuffer_completion_predicate)) then
            valid = completion_in_region(beg, end, minibuffer_completion_table, minibuffer_completion_predicate)
        else
            valid = completion_in_region(beg, end, minibuffer_completion_table)
        end if
    end subroutine crm_complete

    !> Complete the current element at most a single word.
    !> Like `minibuffer_complete_word' but for `completing_read_multiple'.
    !> Port of crm.el:crm-complete-word.
    recursive subroutine crm_complete_word(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        integer :: bits
        status = status_type()
        call current_element(beg, end)
        ! (completion-in-region--single-word)
        bits = completion_do_completion(beg, end, .true., .false.)
    end subroutine crm_complete_word

    !> Exit if all of the minibuffer elements are valid completions.
    !> Otherwise, try to complete the minibuffer contents.
    !> This behaves like `minibuffer_complete_and_exit' (which see),
    !> adjusted for the presence of multiple elements.  Port of
    !> crm.el:crm-complete-and-exit (NO_EXIT is the prefix argument).
    recursive subroutine crm_complete_and_exit(status)
        use callint_fm, only: current_prefix_arg
        use lisp_fm, only: nilp
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end, m, endpos
        type(handle_type) :: endm
        type(status_type) :: st
        logical :: more
        status = status_type()
        doexit = nilp(current_prefix_arg)
        call goto_char(minibuffer_prompt_end())
        do
            if (.not. doexit) exit
            call current_element(beg, end)
            call copy_marker_at(end, endm, .true., st)
            call goto_char(end)
            doexit = .false.
            call completion_complete_and_exit(beg, end, set_doexit, status)
            call marker_position(endm, endpos, st)
            call free_marker(endm, st)
            call goto_char(endpos)
            if (.not. status%ok()) return
            more = .not. eobp()
            if (.not. more) exit
            if (.not. looking_at(crm_separator, st)) exit
            ! Skip to the next element.
            m = match_end(0, st)
            call goto_char(m)
        end do
        if (doexit) call exit_minibuffer(status)
    end subroutine crm_complete_and_exit

    recursive subroutine set_doexit(status)
        type(status_type), intent(out) :: status
        status = status_type()
        doexit = .true.
    end subroutine set_doexit

    ! The minibuffer setup of `completing_read_multiple'.
    subroutine crm_setup()
        procedure(minibuffer_setup_proc), pointer :: outer
        type(status_type) :: st
        call use_local_map(crm_map, st)
        outer => crm_outer_setup
        crm_outer_setup => null()
        if (associated(outer)) call outer()
    end subroutine crm_setup

    !> Read multiple strings in the minibuffer, with completion.
    !> The arguments are the same as those of `completing_read'.
    !> \<crm_local_completion_map>
    !> Input multiple strings by separating each one with a string that
    !> matches the regexp `crm_separator'.  For example, if the separator
    !> regexp is ",", entering "alice,bob,eve" specifies the strings
    !> "alice", "bob", and "eve".
    !>
    !> We refer to contiguous strings of non-separator-characters as
    !> "elements".  In this example there are three elements.
    !>
    !> Completion is available on a per-element basis.  For example, if the
    !> contents of the minibuffer are "alice,bob,eve" and point is between
    !> "l" and "i", pressing \[minibuffer_complete] operates on the element "alice".
    !>
    !> This function returns in RESULTS the strings that were read,
    !> with empty strings removed.  Port of
    !> crm.el:completing-read-multiple.
    recursive subroutine completing_read_multiple(prompt, table, results, status, predicate, require_match, &
                                                  initial, history, default)
        use minibuf_fm, only: minibuffer_string_type
        character(len=*), intent(in) :: prompt
        type(completion_table_type), intent(in) :: table
        type(minibuffer_string_type), allocatable, intent(out) :: results(:)
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        integer, intent(in), optional :: require_match
        character(len=*), intent(in), optional :: initial, history, default
        type(completion_table_type) :: saved_table, fn_table
        type(handle_type) :: saved_map
        character(len=:), allocatable :: input
        integer :: match
        match = REQUIRE_MATCH_NIL
        if (present(require_match)) match = require_match
        saved_table = crm_completion_table
        saved_map = crm_map
        if (match /= REQUIRE_MATCH_NIL) then
            crm_map = crm_local_must_match_map
        else
            crm_map = crm_local_completion_map
        end if
        crm_completion_table = table
        fn_table%kind = TABLE_FUNCTION
        fn_table%fn => crm_collection_fn
        crm_outer_setup => minibuffer_setup_function
        minibuffer_setup_function => crm_setup
        call completing_read(format_prompt(prompt), fn_table, input, status, predicate, require_match, initial, &
                             history, default)
        minibuffer_setup_function => null()
        crm_completion_table = saved_table
        crm_map = saved_map
        if (.not. status%ok()) then
            allocate (results(0))
            return
        end if
        ! Remove empty strings in the list of read strings.
        call split_string(input, results)
    end subroutine completing_read_multiple

    ! (format-spec crm-prompt ...): %s the separator, %d its description,
    ! %p the PROMPT.
    function format_prompt(prompt) result(s)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable :: s
        integer :: k
        s = ""
        k = 1
        do while (k <= len(crm_prompt))
            if (crm_prompt(k:k) == "%" .and. k < len(crm_prompt)) then
                select case (crm_prompt(k + 1:k + 1))
                case ("s")
                    s = s//crm_separator_string
                case ("d")
                    s = s//crm_separator_description
                case ("p")
                    s = s//prompt
                case ("%")
                    s = s//"%"
                case default
                    s = s//crm_prompt(k:k + 1)
                end select
                k = k + 2
            else
                s = s//crm_prompt(k:k)
                k = k + 1
            end if
        end do
    end function format_prompt

    ! (split-string STRING crm-separator t): the parts of STRING between
    ! matches of `crm_separator', without the empty ones.
    subroutine split_string(string, parts)
        use minibuf_fm, only: minibuffer_string_type
        character(len=*), intent(in) :: string
        type(minibuffer_string_type), allocatable, intent(out) :: parts(:)
        type(minibuffer_string_type), allocatable :: grown(:)
        integer(int64) :: start, m, mb, me
        type(status_type) :: st
        integer :: n
        allocate (parts(8))
        n = 0
        start = 1
        do
            m = string_match(crm_separator, string, st, start)
            if (.not. st%ok() .or. m <= 0) then
                call add(string(string_index_to_offset(string, start) + 1:))
                exit
            end if
            mb = match_beginning(0, st)
            me = match_end(0, st)
            ! (An empty match at START does not split.)
            if (me == mb .and. mb == start) then
                if (start > string_chars(string)) then
                    call add(string(string_index_to_offset(string, start) + 1:))
                    exit
                end if
                m = string_match(crm_separator, string, st, start + 1)
                if (.not. st%ok() .or. m <= 0) then
                    call add(string(string_index_to_offset(string, start) + 1:))
                    exit
                end if
                mb = match_beginning(0, st)
                me = match_end(0, st)
            end if
            call add(string(string_index_to_offset(string, start) + 1:string_index_to_offset(string, mb)))
            start = me
        end do
        parts = parts(1:n)
    contains
        subroutine add(part)
            character(len=*), intent(in) :: part
            if (len(part) == 0) return
            if (n == size(parts)) then
                allocate (grown(2*n))
                grown(1:n) = parts
                call move_alloc(grown, parts)
            end if
            n = n + 1
            parts(n)%s = part
        end subroutine add
    end subroutine split_string

    ! [remap COMMAND] in MAP to NEW.
    subroutine remap(map, command, new)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: command, new
        type(value_type), allocatable :: events(:)
        type(status_type) :: st
        call list_elements(list(sym("remap"), sym(command)), events)
        call define_key(map, events, sym(new), st)
    end subroutine remap

    !> Define crm.el's variables, keymaps and commands.
    subroutine syms_of_crm()
        type(status_type) :: st
        if (.not. allocated(crm_separator)) crm_separator = "[ "//achar(9)//"]*,[ "//achar(9)//"]*"
        if (.not. allocated(crm_separator_string)) crm_separator_string = ","
        if (.not. allocated(crm_separator_description)) crm_separator_description = "comma-separated list"
        if (.not. allocated(crm_prompt)) crm_prompt = "[%d] %p"
        commands%crm_complete%run => crm_complete
        commands%crm_complete_word%run => crm_complete_word
        commands%crm_completion_help%run => crm_completion_help
        commands%crm_complete_and_exit%run => crm_complete_and_exit
        if (keymap_live_p(crm_local_completion_map)) call free_keymap(crm_local_completion_map, st)
        call make_sparse_keymap(crm_local_completion_map)
        call set_keymap_parent(crm_local_completion_map, minibuffer_local_completion_map, st)
        call remap(crm_local_completion_map, "minibuffer_complete", "crm_complete")
        call remap(crm_local_completion_map, "minibuffer_complete_word", "crm_complete_word")
        call remap(crm_local_completion_map, "minibuffer_completion_help", "crm_completion_help")
        ! We'd want to have multiple inheritance here.
        if (keymap_live_p(crm_local_must_match_map)) call free_keymap(crm_local_must_match_map, st)
        call make_sparse_keymap(crm_local_must_match_map)
        call set_keymap_parent(crm_local_must_match_map, minibuffer_local_must_match_map, st)
        call remap(crm_local_must_match_map, "minibuffer_complete", "crm_complete")
        call remap(crm_local_must_match_map, "minibuffer_complete_word", "crm_complete_word")
        call remap(crm_local_must_match_map, "minibuffer_completion_help", "crm_completion_help")
        call remap(crm_local_must_match_map, "minibuffer_complete_and_exit", "crm_complete_and_exit")
    end subroutine syms_of_crm
end module crm_fm
