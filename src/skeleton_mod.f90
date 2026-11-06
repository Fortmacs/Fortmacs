! skeleton_mod.f90 -- statement skeletons: inserting structured text.
! Ported from GNU Emacs 31.1 lisp/skeleton.el.
!
! Copyright (C) 1993-1996, 2001-2026 Free Software Foundation, Inc.
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

!> A very concise way of writing structured statement skeleton insertion
!> commands for programming language modes.
!>
!> A skeleton is typed Fortran data: an array of `skeleton_element_type'
!> records.  The first element is the interactor (`skeleton_prompt',
!> `skeleton_interactor', `skeleton_for_each' or SKELETON_NO_INTERACTOR);
!> the others are strings to insert (`skeleton_text', `skeleton_literal'),
!> the named elements SKELETON_NEWLINE (\n), SKELETON_POINT (_),
!> SKELETON_POINT_OVERRIDE (-), SKELETON_INDENT (>), SKELETON_POSITION
!> (@), SKELETON_IF_MOVED (&), SKELETON_IF_NOT_MOVED (|),
!> SKELETON_RESUME (resume:), SKELETON_STRING (string), SKELETON_V1 and
!> SKELETON_V2, deletions (`skeleton_delete', -NUMBER), procedures called
!> for their side effects (`skeleton_action', upstream's quoted
!> expressions) or for an element to insert (`skeleton_value', upstream's
!> other expressions), and sub-skeletons: an interactor in the middle of
!> a skeleton starts one, SKELETON_END ends it.  For example sh-mode's if
!> statement:
!>
!>     [skeleton_prompt("condition: "), SKELETON_INDENT, skeleton_text("if "), SKELETON_STRING, &
!>      skeleton_text("; then"), SKELETON_NEWLINE, SKELETON_INDENT, SKELETON_POINT, SKELETON_NEWLINE, &
!>      skeleton_prompt(format='("other condition, ", a, ": ")'), SKELETON_INDENT, skeleton_text("elif "), SKELETON_STRING, &
!>      skeleton_text("; then"), SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_END, &
!>      skeleton_text("else"), SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_INDENT, SKELETON_NEWLINE, SKELETON_RESUME, &
!>      skeleton_text("fi"), SKELETON_INDENT, SKELETON_NEWLINE]
!>
!> A command defined by upstream's define-skeleton calls
!> `skeleton_command' with its skeleton.  The variables upstream binds
!> while a skeleton is inserted (string, input, help, v1, v2) are the
!> components of `skeleton_state'.  `skeleton_pair_insert_maybe' inserts
!> matching characters in twos (an opening and a closing one).
!>
!> Deferred: the help text while reading (`help' and upstream's
!> minibuffer-help-form, which is not ported), the no-self-insert
!> property of skeleton commands (an abbrev whose hook is one gives
!> `no_self_insert' itself), `mouse_drag_region' as `last_command' in
!> `skeleton_pair_insert_maybe' (no mouse) and overwrite mode there
!> (overwrite mode is not ported).  Not applicable: skeleton-debug (it
!> keeps the skeleton in a Lisp variable) and the obsolete aliases
!> skeleton-transformation and skeleton-filter.
module skeleton_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, QUIT_CODE, GENERIC_ERROR_CODE, VOID_FUNCTION_CODE, WRONG_TYPE_ARGUMENT_CODE
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, barf_if_buffer_read_only, transient_mark_mode
    use buffer_local_setting_mod, only: buffer_local_setting_type
    use data_mod, only: logical_setting_type, forward_setting, append_locals_clearer
    use hooks_mod, only: hook_type
    use execute_interactively_mod, only: prefix_argument_type, current_prefix_argument, prefix_numeric_value, PREFIX_NONE, &
                                         PREFIX_NUMBER
    use editing_functions_mod, only: point, goto_character, is_beginning_of_line, is_end_of_line, line_beginning_position, &
                                     line_end_position, buffer_substring, delete_region, preceding_character, character_to_string, &
                                     write_with_format
    use string_type_mod, only: string_type
    use editing_commands_mod, only: delete_character, end_of_line, self_insert_command
    use insert_delete_mod, only: insert
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use indent_mod, only: current_column, current_indentation, indent_to, indent_according_to_mode, indent_region, &
                          move_to_column
    use syntax_mod, only: forward_word, skip_characters_backward, character_syntax
    use search_mod, only: looking_at
    use simple_mod, only: newline_and_indent, reindent_then_newline_and_indent, backward_delete_character_untabify, &
                          is_use_region, deactivate_mark
    use basic_subprograms_mod, only: change_group_type, prepare_change_group, activate_change_group, accept_change_group, &
                                     cancel_change_group
    use minibuffer_reading_mod, only: read_string
    use keyboard_mod, only: sit_for, last_command_event
    use value_mod, only: vtag, V_INTEGER
    use window_mod, only: window_buffer, is_position_visible_in_window, recenter
    use frame_mod, only: selected_window
    use help_mod, only: substitute_command_keys
    use commands_mod, only: commands
    implicit none(type, external)
    private

    public :: skeleton_text, skeleton_literal, skeleton_delete, skeleton_prompt, skeleton_interactor, skeleton_for_each, &
              skeleton_action, skeleton_value, skeleton_insert, skeleton_read, skeleton_proxy_new, skeleton_variants_proxy_new, &
              skeleton_command, skeleton_variants_command, skeleton_pair_insert_maybe, initialize_skeleton, &
              skeleton_next_element, skeleton_read_string, skeleton_transform
    public :: skeleton_action_procedure, skeleton_value_procedure, skeleton_read_procedure, &
              skeleton_transformation_procedure, skeleton_filter_procedure, skeleton_test_procedure, skeleton_bind_procedure

    integer, parameter :: KIND_TEXT = 1, KIND_LITERAL = 2, KIND_DELETE = 3, KIND_NEWLINE = 4, KIND_POINT = 5, &
                          KIND_POINT_OVERRIDE = 6, KIND_INDENT = 7, KIND_POSITION = 8, KIND_IF_MOVED = 9, &
                          KIND_IF_NOT_MOVED = 10, KIND_RESUME = 11, KIND_STRING = 12, KIND_V1 = 13, KIND_V2 = 14, &
                          KIND_ACTION = 15, KIND_VALUE = 16, KIND_INTERACTOR = 17, KIND_END = 18
    integer, parameter :: INTERACTOR_NONE = 0, INTERACTOR_PROMPT = 1, INTERACTOR_PROCEDURE = 2, INTERACTOR_STRINGS = 3

    !> The longest text of a skeleton element, and the most strings of a
    !> `skeleton_for_each' interactor.
    integer, parameter, public :: SKELETON_TEXT_MAXIMUM = 400, SKELETON_STRINGS_MAXIMUM = 16

    character(len=*), parameter :: LF = achar(10)
    !> A quit's data while a skeleton runs: SKELETON_QUIT_ITERATION ends the
    !> iteration of a sub-skeleton (upstream signals quit with t), and
    !> SKELETON_QUIT_RECURSIVE leaves a recursive sub-skeleton (quit with
    !> `recursive'); neither is shown, a quit prints "Quit".  Not a port.
    character(len=*), parameter, public :: SKELETON_QUIT_ITERATION = "iteration", SKELETON_QUIT_RECURSIVE = "recursive"

    abstract interface
        !> A procedure called for its side effects while a skeleton is
        !> inserted (upstream's quoted expression).  Not a port.
        recursive subroutine skeleton_action_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine skeleton_action_procedure

        !> An interactor that reads the string itself (upstream's
        !> function or expression interactor): unallocated for none.
        !> Not a port.
        recursive subroutine skeleton_read_procedure(string, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: string
            type(status_type), intent(out) :: status
        end subroutine skeleton_read_procedure

        !> A procedure whose result is treated as an element (upstream's
        !> other expressions): it sets `skeleton_state%value' to one element
        !> (a text, inserted literally, a deletion, a named element or a
        !> sub-skeleton), or leaves it empty for none.  Not a port.
        recursive subroutine skeleton_value_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine skeleton_value_procedure
    end interface

    !> One element of a skeleton (see the module's description); made
    !> with the constructors and named elements of this module.
    type, public :: skeleton_element_type
        integer :: kind = 0
        !> For an interactor: INTERACTOR_NONE, _PROMPT, _PROCEDURE or
        !> _STRINGS.
        integer :: interactor = INTERACTOR_NONE
        !> The text (a string to insert, a prompt, or the strings of a
        !> `skeleton_for_each' one after the other) and its length.
        character(len=SKELETON_TEXT_MAXIMUM) :: text = ""
        integer :: length = 0
        !> The characters to delete, or the number of strings.
        integer :: count = 0
        integer :: lengths(SKELETON_STRINGS_MAXIMUM) = 0
        !> For a prompt: the text is a Fortran format given
        !> `skeleton_subprompt' as its one `a' item (`skeleton_prompt(format=...)').
        logical :: prompt_format = .false.
        procedure(skeleton_action_procedure), pointer, nopass :: action => null()
        procedure(skeleton_value_procedure), pointer, nopass :: value => null()
        procedure(skeleton_read_procedure), pointer, nopass :: read => null()
    end type skeleton_element_type

    !> Go to next line and indent according to mode, unless this is the
    !> first/last element of a skeleton and point is at bol/eol (\n).
    type(skeleton_element_type), parameter, public :: SKELETON_NEWLINE = skeleton_element_type(kind=KIND_NEWLINE)
    !> Interesting point, interregion here (_).
    type(skeleton_element_type), parameter, public :: SKELETON_POINT = skeleton_element_type(kind=KIND_POINT)
    !> Interesting point, no interregion interaction, overrides interesting
    !> point set by SKELETON_POINT (-).
    type(skeleton_element_type), parameter, public :: SKELETON_POINT_OVERRIDE = skeleton_element_type(kind=KIND_POINT_OVERRIDE)
    !> Indent line (or interregion if followed by SKELETON_POINT) according
    !> to major mode (>).
    type(skeleton_element_type), parameter, public :: SKELETON_INDENT = skeleton_element_type(kind=KIND_INDENT)
    !> Add position to `skeleton_positions' (@).
    type(skeleton_element_type), parameter, public :: SKELETON_POSITION = skeleton_element_type(kind=KIND_POSITION)
    !> Do next element if previous moved point (&).
    type(skeleton_element_type), parameter, public :: SKELETON_IF_MOVED = skeleton_element_type(kind=KIND_IF_MOVED)
    !> Do next element if previous didn't move point (|).
    type(skeleton_element_type), parameter, public :: SKELETON_IF_NOT_MOVED = skeleton_element_type(kind=KIND_IF_NOT_MOVED)
    !> Skipped, continue here if a quit occurs (resume:).
    type(skeleton_element_type), parameter, public :: SKELETON_RESUME = skeleton_element_type(kind=KIND_RESUME)
    !> First time: read a string according to the interactor; then:
    !> insert previously read string once more (string).
    type(skeleton_element_type), parameter, public :: SKELETON_STRING = skeleton_element_type(kind=KIND_STRING)
    !> The element in `skeleton_state%v1' (v1).
    type(skeleton_element_type), parameter, public :: SKELETON_V1 = skeleton_element_type(kind=KIND_V1)
    !> The element in `skeleton_state%v2' (v2).
    type(skeleton_element_type), parameter, public :: SKELETON_V2 = skeleton_element_type(kind=KIND_V2)
    !> The end of a sub-skeleton.
    type(skeleton_element_type), parameter, public :: SKELETON_END = skeleton_element_type(kind=KIND_END)
    !> No interactor.
    type(skeleton_element_type), parameter, public :: SKELETON_NO_INTERACTOR = &
                                                      skeleton_element_type(kind=KIND_INTERACTOR, interactor=INTERACTOR_NONE)

    !> The variables available while a skeleton is inserted:
    !>
    !> - `string' (string): the string read according to the interactor
    !>   (`string_pending' until it is first used);
    !> - `initial_input' and `initial_position' (input): initial input
    !>   (and the position of point in it, 0 for its end) while reading string;
    !> - `help': help text during interaction with the user (not shown:
    !>   Deferred);
    !> - `v1', `v2': for memorizing anything you want, each one element
    !>   (empty for none).
    !> - `value': the element a `skeleton_value' procedure gives.
    type, public :: skeleton_state_type
        character(len=:), allocatable :: string
        logical :: string_pending = .false.
        type(skeleton_element_type) :: interactor
        logical :: recursive = .false.
        character(len=:), allocatable :: initial_input
        integer(int64) :: initial_position = 0
        character(len=:), allocatable :: help
        type(skeleton_element_type), allocatable :: v1(:), v2(:)
        type(skeleton_element_type), allocatable :: value(:)
    end type skeleton_state_type

    !> The skeletons of a command for several variants (shells, ...), by
    !> name, from which a `skeleton_filter_function' chooses.
    type, public :: skeleton_variant_type
        character(len=:), allocatable :: name
        type(skeleton_element_type), allocatable :: elements(:)
    end type skeleton_variant_type

    !> A table of `skeleton_variant_type' (`%append' gives a variant).
    type, public :: skeleton_variants_type
        type(skeleton_variant_type), allocatable :: entries(:)
    contains
        !> Append the variant NAME with ELEMENTS.
        procedure :: append => variants_append
    end type skeleton_variants_type

    !> A pairing partner of `skeleton_pairs': the character CHAR and the
    !> skeleton (without interactor) inserted for it.
    type, public :: skeleton_pair_type
        integer :: char = 0
        type(skeleton_element_type), allocatable :: elements(:)
    end type skeleton_pair_type

    !> A list of `skeleton_pair_type' (`%append' adds one).
    type, public :: skeleton_pairs_type
        type(skeleton_pair_type), allocatable :: entries(:)
    contains
        !> Append the partner of CHAR, ELEMENTS (a skeleton without
        !> interactor, CHAR itself usually first).
        procedure :: append => pairs_append
    end type skeleton_pairs_type

    abstract interface
        !> A `skeleton_transformation_function': STRING transformed.
        !> Not a port.
        subroutine skeleton_transformation_procedure(string, result)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: result
        end subroutine skeleton_transformation_procedure

        !> A `skeleton_pair_filter_function': true to inhibit pairing.
        !> Not a port.
        recursive logical function skeleton_test_procedure()
            implicit none(type, external)
        end function skeleton_test_procedure

        !> A `skeleton_further_elements' procedure: give mode-specific
        !> variables their values (BIND true) before a skeleton is
        !> interpreted, and restore them (BIND false) after.  Not a port.
        subroutine skeleton_bind_procedure(bind)
            implicit none(type, external)
            logical, intent(in) :: bind
        end subroutine skeleton_bind_procedure

        !> A `skeleton_filter_function': the skeleton of VARIANTS to
        !> insert (the identity, null, takes none: see
        !> `skeleton_variants_proxy_new').  Not a port.
        recursive subroutine skeleton_filter_procedure(variants, skeleton, status)
            import :: skeleton_variants_type, skeleton_element_type, status_type
            implicit none(type, external)
            type(skeleton_variants_type), intent(in) :: variants
            type(skeleton_element_type), allocatable, intent(out) :: skeleton(:)
            type(status_type), intent(out) :: status
        end subroutine skeleton_filter_procedure
    end interface

    !> The values of string, input, help, v1 and v2 while a skeleton is
    !> inserted (see `skeleton_state_type').
    type(skeleton_state_type), public :: skeleton_state

    !> One buffer's own value of
    !> `skeleton_transformation_function'.
    type :: skeleton_transformation_entry_type
        procedure(skeleton_transformation_procedure), pointer, nopass :: procedure => null()
    end type skeleton_transformation_entry_type

    !> The type of
    !> `skeleton_transformation_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: skeleton_transformation_setting_type
        procedure(skeleton_transformation_procedure), pointer, nopass :: procedure => null()
        type(skeleton_transformation_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_skeleton_transformation_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_skeleton_transformation_current
        procedure :: append_local_entry => append_skeleton_transformation_entry
        procedure :: clear_local_entry => clear_skeleton_transformation_entry
    end type skeleton_transformation_setting_type

    !> If not null, function applied to literal strings before they are inserted.
    !> It should take strings and return them transformed.
    !> Typical examples might be upcase or capitalize.  Port of skeleton.el
    !> skeleton-transformation-function (`%procedure' is the global
    !> value, null for identity; `%set_local' gives a buffer its own).
    type(skeleton_transformation_setting_type), public :: skeleton_transformation_function

    !> Controls wrapping behavior of commands created by `skeleton_command'.
    !> When the region is visible (due to `transient_mark_mode' or marking a region
    !> with the mouse) and this is true and the command was called without an
    !> explicit ARGUMENT, then the ARGUMENT defaults to -1, i.e. wrapping around the visible
    !> region.  Port of skeleton.el skeleton-autowrap.
    type(logical_setting_type), target, public :: skeleton_autowrap = &
                                                  logical_setting_type(name="skeleton_autowrap", value=.true.)

    !> If true, make sure that the skeleton inserted ends with a newline.
    !> Port of skeleton.el skeleton-end-newline.
    type(logical_setting_type), target, public :: skeleton_end_newline = &
                                                  logical_setting_type(name="skeleton_end_newline", value=.true.)

    !> Hook called at end of skeleton but before going to point of interest.
    !> The variables `skeleton_state%v1' and `skeleton_state%v2' are still set
    !> when calling this.  Port of skeleton.el skeleton-end-hook.
    type(hook_type), target, public :: skeleton_end_hook = hook_type(name="skeleton_end_hook")

    !> One buffer's own value of
    !> `skeleton_filter_function'.
    type :: skeleton_filter_entry_type
        procedure(skeleton_filter_procedure), pointer, nopass :: procedure => null()
    end type skeleton_filter_entry_type

    !> The type of
    !> `skeleton_filter_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: skeleton_filter_setting_type
        procedure(skeleton_filter_procedure), pointer, nopass :: procedure => null()
        type(skeleton_filter_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_skeleton_filter_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_skeleton_filter_current
        procedure :: append_local_entry => append_skeleton_filter_entry
        procedure :: clear_local_entry => clear_skeleton_filter_entry
    end type skeleton_filter_setting_type

    !> Function for transforming a skeleton proxy's aliases' variable value.
    !> Port of skeleton.el skeleton-filter-function (`%procedure' is the
    !> global value, null for identity; `%set_local' gives a buffer its
    !> own).
    type(skeleton_filter_setting_type), public :: skeleton_filter_function

    !> When true untabifies when deleting backwards with `skeleton_delete'.
    !> Port of skeleton.el skeleton-untabify.
    type(logical_setting_type), target, public :: skeleton_untabify = &
                                                  logical_setting_type(name="skeleton_untabify", value=.false.)

    !> When true, indent rigidly under current line for element SKELETON_NEWLINE.
    !> Else use mode's `indent_line_function'.  Port of skeleton.el
    !> skeleton-newline-indent-rigidly.
    type(logical_setting_type), target, public :: skeleton_newline_indent_rigidly = &
                                                  logical_setting_type(name="skeleton_newline_indent_rigidly", value=.false.)

    !> One buffer's own value of
    !> `skeleton_further_elements'.
    type :: skeleton_further_elements_entry_type
        procedure(skeleton_bind_procedure), pointer, nopass :: procedure => null()
    end type skeleton_further_elements_entry_type

    !> The type of
    !> `skeleton_further_elements':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: skeleton_further_elements_setting_type
        procedure(skeleton_bind_procedure), pointer, nopass :: procedure => null()
        type(skeleton_further_elements_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_skeleton_further_elements_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_skeleton_further_elements_current
        procedure :: append_local_entry => append_skeleton_further_elements_entry
        procedure :: clear_local_entry => clear_skeleton_further_elements_entry
    end type skeleton_further_elements_setting_type

    !> A buffer-local procedure giving mode specific variables their values
    !> while interpreting a skeleton (upstream's varlist of mode specific
    !> skeleton elements; elements of a mode's own are `skeleton_value'
    !> elements of its module).  Port of skeleton.el
    !> skeleton-further-elements (`%procedure' is the global value,
    !> null for none; `%set_local' gives a buffer its own).
    type(skeleton_further_elements_setting_type), public :: skeleton_further_elements

    !> What the `a' edit descriptor of a prompt format writes in recursive
    !> subskeletons (`skeleton_prompt(format=...)').  Port of
    !> skeleton.el skeleton-subprompt.
    character(len=:), allocatable, public :: skeleton_subprompt

    !> List of positions marked with SKELETON_POSITION, after skeleton
    !> insertion.  The list describes the most recent skeleton insertion,
    !> and its elements are integer buffer positions in the reverse order
    !> of the insertion order.  Port of skeleton.el skeleton-positions.
    integer(int64), allocatable, public :: skeleton_positions(:)

    !> If this is false pairing is turned off, no matter what else is set.
    !> Otherwise modes with `skeleton_pair_insert_maybe' on some keys
    !> will attempt to insert matching characters in twos.  Port of
    !> skeleton.el skeleton-pair.
    type(logical_setting_type), target, public :: skeleton_pair = logical_setting_type(name="skeleton_pair", value=.false.)

    !> If this is false, paired insertion is inhibited before or inside a word.
    !> Port of skeleton.el skeleton-pair-on-word.
    type(logical_setting_type), target, public :: skeleton_pair_on_word = &
                                                  logical_setting_type(name="skeleton_pair_on_word", value=.false.)

    !> One buffer's own value of
    !> `skeleton_pair_filter_function'.
    type :: skeleton_pair_filter_entry_type
        procedure(skeleton_test_procedure), pointer, nopass :: procedure => null()
    end type skeleton_pair_filter_entry_type

    !> The type of
    !> `skeleton_pair_filter_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: skeleton_pair_filter_setting_type
        procedure(skeleton_test_procedure), pointer, nopass :: procedure => null()
        type(skeleton_pair_filter_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_skeleton_pair_filter_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_skeleton_pair_filter_current
        procedure :: append_local_entry => append_skeleton_pair_filter_entry
        procedure :: clear_local_entry => clear_skeleton_pair_filter_entry
    end type skeleton_pair_filter_setting_type

    !> Attempt paired insertion if this function returns false, before inserting.
    !> This allows for context-sensitive checking whether pairing is appropriate.
    !> Port of skeleton.el skeleton-pair-filter-function (`%procedure' is
    !> the global value, null for a function returning false; `%set_local'
    !> gives a buffer its own).
    type(skeleton_pair_filter_setting_type), public :: skeleton_pair_filter_function

    !> The type of `skeleton_pairs' and `skeleton_default_pairs': the
    !> global value `pairs' (`%append' adds to it) and the buffers' own
    !> values (`set_local').
    type, extends(buffer_local_setting_type), public :: skeleton_pairs_setting_type
        type(skeleton_pairs_type) :: pairs
        type(skeleton_pairs_type), allocatable :: locals(:)
    contains
        !> Append the partner of CHAR, ELEMENTS, to the global value.
        procedure :: append => append_skeleton_pairs_setting
        !> Give the current buffer its own value PAIRS.
        procedure :: set_local => set_skeleton_pairs_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_skeleton_pairs_current
        procedure :: append_local_entry => append_skeleton_pairs_entry
        procedure :: clear_local_entry => clear_skeleton_pairs_entry
    end type skeleton_pairs_setting_type

    !> An override list of pairing partners matched against `last_command_event'.
    !> Each element, made with `%append(char, elements)', is passed to
    !> `skeleton_insert' with no interactor.  Variable string does nothing.
    !>
    !> Elements might be (` ` _ "''"), (( SPC _ " )") or ({ \n > _ \n } >).
    !> Port of skeleton.el skeleton-pair-alist (`%append' adds to the global
    !> value; `%set_local' gives a buffer its own).
    type(skeleton_pairs_setting_type), public :: skeleton_pairs

    !> The default pairing partners: (), [], {}, <>, «» and (grave accent,
    !> apostrophe), each closing character standing for itself.  Port of
    !> skeleton.el skeleton-pair-default-alist (`%append' adds to the global
    !> value; `%set_local' gives a buffer its own).
    type(skeleton_pairs_setting_type), public :: skeleton_default_pairs

    ! The dynamic variables of skeleton-insert: skeleton-regions (markers,
    ! the next one at `region_next'), skeleton-point, skeleton-modified.
    type(handle_type), allocatable :: regions(:)
    integer :: region_next = 1
    integer(int64) :: interesting_point = 0
    logical :: point_unmoved = .false.

contains

    ! ------------------------------------------------------------------
    ! Elements
    ! ------------------------------------------------------------------

    !> A string to insert, passed through `skeleton_transformation_function'.
    !> Not a port.
    function skeleton_text(string) result(element)
        character(len=*), intent(in) :: string
        type(skeleton_element_type) :: element

        element%kind = KIND_TEXT
        element%text = string
        element%length = min(len(string), SKELETON_TEXT_MAXIMUM)
    end function skeleton_text

    !> A string to insert literally (upstream's value of an expression).
    !> Not a port.
    function skeleton_literal(string) result(element)
        character(len=*), intent(in) :: string
        type(skeleton_element_type) :: element

        element = skeleton_text(string)
        element%kind = KIND_LITERAL
    end function skeleton_literal

    !> Delete COUNT preceding characters (-NUMBER; see `skeleton_untabify').
    !> Not a port.
    function skeleton_delete(count) result(element)
        integer, intent(in) :: count
        type(skeleton_element_type) :: element

        element%kind = KIND_DELETE
        element%count = count
    end function skeleton_delete

    !> An interactor reading the string with PROMPT, or with the prompt
    !> that FORMAT, a Fortran format, writes with `skeleton_subprompt' as
    !> its one `a' item (`skeleton_prompt(format='("pattern, ", a, ": ")')',
    !> upstream's "pattern, %s: ").  Not a port.
    function skeleton_prompt(prompt, format) result(element)
        character(len=*), intent(in), optional :: prompt, format
        type(skeleton_element_type) :: element

        if (present(format)) then
            element = skeleton_text(format)
            element%prompt_format = .true.
        else
            element = skeleton_text(prompt)
        end if

        element%kind = KIND_INTERACTOR
        element%interactor = INTERACTOR_PROMPT
    end function skeleton_prompt

    !> An interactor whose procedure READ returns the string.  Not a port.
    function skeleton_interactor(read) result(element)
        procedure(skeleton_read_procedure) :: read
        type(skeleton_element_type) :: element

        element%kind = KIND_INTERACTOR
        element%interactor = INTERACTOR_PROCEDURE
        element%read => read
    end function skeleton_interactor

    !> An interactor giving a sub-skeleton once for each of STRINGS (as
    !> string); trailing blanks are not part of them.  Not a port.
    function skeleton_for_each(strings) result(element)
        character(len=*), intent(in) :: strings(:)
        type(skeleton_element_type) :: element
        integer :: k, n

        element%kind = KIND_INTERACTOR
        element%interactor = INTERACTOR_STRINGS
        element%count = min(size(strings), SKELETON_STRINGS_MAXIMUM)
        n = 0

        do k = 1, element%count
            element%lengths(k) = len_trim(strings(k))
            element%text(n + 1:n + element%lengths(k)) = strings(k)
            n = n + element%lengths(k)
        end do

        element%length = n
    end function skeleton_for_each

    !> PROCEDURE called for its side effects (upstream's quoted expression).
    !> Not a port.
    function skeleton_action(procedure) result(element)
        procedure(skeleton_action_procedure) :: procedure
        type(skeleton_element_type) :: element

        element%kind = KIND_ACTION
        element%action => procedure
    end function skeleton_action

    !> PROCEDURE called for the element to insert (upstream's expression).
    !> Not a port.
    function skeleton_value(procedure) result(element)
        procedure(skeleton_value_procedure) :: procedure
        type(skeleton_element_type) :: element

        element%kind = KIND_VALUE
        element%value => procedure
    end function skeleton_value

    ! The text of ELEMENT.  Not a port.
    function text_of(element) result(s)
        type(skeleton_element_type), intent(in) :: element
        character(len=:), allocatable :: s

        s = element%text(1:element%length)
    end function text_of

    ! The index after the element at I of SKELETON (after the END of a
    ! sub-skeleton).  Not a port.
    integer function next_index(skeleton, i) result(j)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(in) :: i

        if (skeleton(i)%kind == KIND_INTERACTOR) then
            j = matching_end(skeleton, i) + 1
        else
            j = i + 1
        end if
    end function next_index

    ! The index of the END of the sub-skeleton starting at I (one past the
    ! array when it has none).  Not a port.
    integer function matching_end(skeleton, i) result(j)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(in) :: i
        integer :: depth

        depth = 0

        do j = i, size(skeleton)
            if (skeleton(j)%kind == KIND_INTERACTOR) depth = depth + 1
            if (skeleton(j)%kind == KIND_END) depth = depth - 1
            if (depth == 0) return
        end do

        j = size(skeleton) + 1
    end function matching_end

    !> The index of the element after the one at I of SKELETON at the same
    !> level (after the end of a sub-skeleton that starts at I; I = 1 is the
    !> interactor, followed by 2).  Not a port.
    integer function skeleton_next_element(skeleton, i) result(j)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(in) :: i

        if (i == 1) then
            j = 2
        else
            j = next_index(skeleton, i)
        end if
    end function skeleton_next_element

    ! Not a port.
    subroutine variants_append(self, name, elements)
        class(skeleton_variants_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(skeleton_element_type), intent(in) :: elements(:)
        type(skeleton_variant_type) :: variant

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        variant%name = name
        variant%elements = elements
        self%entries = [self%entries, variant]
    end subroutine variants_append

    ! Not a port.
    subroutine pairs_append(self, char, elements)
        class(skeleton_pairs_type), intent(inout) :: self
        integer, intent(in) :: char
        type(skeleton_element_type), intent(in) :: elements(:)
        type(skeleton_pair_type) :: pair

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        pair%char = char
        pair%elements = elements
        self%entries = [self%entries, pair]
    end subroutine pairs_append

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `skeleton_transformation_function'.  Not a port.
    subroutine set_skeleton_transformation_local(self, local_procedure)
        class(skeleton_transformation_setting_type), intent(inout) :: self
        procedure(skeleton_transformation_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_skeleton_transformation_local

    !> The current buffer's value of
    !> `skeleton_transformation_function':
    !> its own, else the global one.  Not a port.
    function get_skeleton_transformation_current(self) result(current_procedure)
        class(skeleton_transformation_setting_type), intent(in) :: self
        procedure(skeleton_transformation_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_skeleton_transformation_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_skeleton_transformation_entry(self)
        class(skeleton_transformation_setting_type), intent(inout) :: self
        type(skeleton_transformation_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_skeleton_transformation_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_skeleton_transformation_entry(self, k)
        class(skeleton_transformation_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_skeleton_transformation_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `skeleton_filter_function'.  Not a port.
    subroutine set_skeleton_filter_local(self, local_procedure)
        class(skeleton_filter_setting_type), intent(inout) :: self
        procedure(skeleton_filter_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_skeleton_filter_local

    !> The current buffer's value of
    !> `skeleton_filter_function':
    !> its own, else the global one.  Not a port.
    function get_skeleton_filter_current(self) result(current_procedure)
        class(skeleton_filter_setting_type), intent(in) :: self
        procedure(skeleton_filter_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_skeleton_filter_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_skeleton_filter_entry(self)
        class(skeleton_filter_setting_type), intent(inout) :: self
        type(skeleton_filter_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_skeleton_filter_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_skeleton_filter_entry(self, k)
        class(skeleton_filter_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_skeleton_filter_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `skeleton_pair_filter_function'.  Not a port.
    subroutine set_skeleton_pair_filter_local(self, local_procedure)
        class(skeleton_pair_filter_setting_type), intent(inout) :: self
        procedure(skeleton_test_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_skeleton_pair_filter_local

    !> The current buffer's value of
    !> `skeleton_pair_filter_function':
    !> its own, else the global one.  Not a port.
    function get_skeleton_pair_filter_current(self) result(current_procedure)
        class(skeleton_pair_filter_setting_type), intent(in) :: self
        procedure(skeleton_test_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_skeleton_pair_filter_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_skeleton_pair_filter_entry(self)
        class(skeleton_pair_filter_setting_type), intent(inout) :: self
        type(skeleton_pair_filter_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_skeleton_pair_filter_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_skeleton_pair_filter_entry(self, k)
        class(skeleton_pair_filter_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_skeleton_pair_filter_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `skeleton_further_elements'.  Not a port.
    subroutine set_skeleton_further_elements_local(self, local_procedure)
        class(skeleton_further_elements_setting_type), intent(inout) :: self
        procedure(skeleton_bind_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_skeleton_further_elements_local

    !> The current buffer's value of
    !> `skeleton_further_elements':
    !> its own, else the global one.  Not a port.
    function get_skeleton_further_elements_current(self) result(current_procedure)
        class(skeleton_further_elements_setting_type), intent(in) :: self
        procedure(skeleton_bind_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_skeleton_further_elements_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_skeleton_further_elements_entry(self)
        class(skeleton_further_elements_setting_type), intent(inout) :: self
        type(skeleton_further_elements_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_skeleton_further_elements_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_skeleton_further_elements_entry(self, k)
        class(skeleton_further_elements_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_skeleton_further_elements_entry

    !> Append the partner of CHAR, ELEMENTS (a skeleton without
    !> interactor, CHAR itself usually first), to the global value.
    !> Not a port.
    subroutine append_skeleton_pairs_setting(self, char, elements)
        class(skeleton_pairs_setting_type), intent(inout) :: self
        integer, intent(in) :: char
        type(skeleton_element_type), intent(in) :: elements(:)

        call self%pairs%append(char, elements)
    end subroutine append_skeleton_pairs_setting

    !> Give the current buffer its own value PAIRS.  Not a port.
    subroutine set_skeleton_pairs_local(self, pairs)
        class(skeleton_pairs_setting_type), intent(inout) :: self
        type(skeleton_pairs_type), intent(in) :: pairs
        integer :: k

        call self%make_local_entry(k)
        if (k > 0) self%locals(k) = pairs
    end subroutine set_skeleton_pairs_local

    !> The current buffer's value: its own, else the global one.
    !> Not a port.
    function get_skeleton_pairs_current(self) result(pairs)
        class(skeleton_pairs_setting_type), intent(in) :: self
        type(skeleton_pairs_type) :: pairs
        integer :: k

        k = self%get_local_index()

        if (k > 0) then
            pairs = self%locals(k)
        else
            pairs = self%pairs
        end if
    end function get_skeleton_pairs_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_skeleton_pairs_entry(self)
        class(skeleton_pairs_setting_type), intent(inout) :: self
        type(skeleton_pairs_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_skeleton_pairs_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_skeleton_pairs_entry(self, k)
        class(skeleton_pairs_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        if (allocated(self%locals(k)%entries)) deallocate (self%locals(k)%entries)
    end subroutine clear_skeleton_pairs_entry

    ! Forget the current buffer's values.  Not a port.
    subroutine clear_skeleton_locals()
        call skeleton_transformation_function%clear_local()
        call skeleton_filter_function%clear_local()
        call skeleton_pair_filter_function%clear_local()
        call skeleton_further_elements%clear_local()
        call skeleton_pairs%clear_local()
        call skeleton_default_pairs%clear_local()
    end subroutine clear_skeleton_locals

    !> STRING through the current buffer's `skeleton_transformation_function'.
    !> Not a port.
    function skeleton_transform(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out
        procedure(skeleton_transformation_procedure), pointer :: transformation

        transformation => skeleton_transformation_function%get_current()

        if (associated(transformation)) then
            call transformation(string, out)
        else
            out = string
        end if
    end function skeleton_transform

    ! ------------------------------------------------------------------
    ! Inserting
    ! ------------------------------------------------------------------

    !> Insert the complex statement skeleton SKELETON describes very concisely.
    !>
    !> With optional second argument REGIONS, wrap first interesting point
    !> (SKELETON_POINT) in skeleton around next REGIONS words, if REGIONS is positive.
    !> If REGIONS is negative, wrap REGIONS preceding interregions into first
    !> REGIONS interesting positions (successive SKELETON_POINTs) in skeleton.
    !>
    !> An interregion is the stretch of text between two contiguous marked
    !> points.  If you marked A B C [] (where [] is the cursor) in
    !> alphabetical order, the 3 interregions are simply the last 3 regions.
    !> But if you marked B A [] C, the interregions are B-A, A-[], []-C.
    !>
    !> The optional third argument STRING, if present, is the value for the
    !> variable string within the skeleton.  When this is present, the
    !> interactor gets ignored.
    !>
    !> When done with skeleton, but before going back to SKELETON_POINT, add
    !> a newline (unless `skeleton_end_newline' is false or the point is at end
    !> of line), and run the hook `skeleton_end_hook'.
    !>
    !> SKELETON is made up as [INTERACTOR, ELEMENT, ...].  INTERACTOR may be
    !> SKELETON_NO_INTERACTOR if not needed, a prompt (`skeleton_prompt') or a
    !> procedure for complex read functions (`skeleton_interactor').
    !>
    !> If ELEMENT is a string (`skeleton_text') it gets inserted (see also
    !> `skeleton_transformation_function').  Other possibilities are:
    !>
    !>         SKELETON_NEWLINE        go to next line and indent according to mode, unless
    !>                                 this is the first/last element of a skeleton and point
    !>                                 is at bol/eol
    !>         SKELETON_POINT          interesting point, interregion here
    !>         SKELETON_POINT_OVERRIDE interesting point, no interregion interaction, overrides
    !>                                 interesting point set by SKELETON_POINT
    !>         SKELETON_INDENT         indent line (or interregion if followed by
    !>                                 SKELETON_POINT) according to major mode
    !>         SKELETON_POSITION       add position to `skeleton_positions'
    !>         SKELETON_IF_MOVED       do next ELEMENT if previous moved point
    !>         SKELETON_IF_NOT_MOVED   do next ELEMENT if previous didn't move point
    !>         skeleton_delete(NUMBER)    delete NUMBER preceding characters (see `skeleton_untabify')
    !>         SKELETON_RESUME         skipped, continue here if a quit occurs
    !>
    !> After termination, point will be positioned at the last occurrence of
    !> SKELETON_POINT_OVERRIDE or at the first occurrence of SKELETON_POINT or at
    !> the end of the inserted text.
    !>
    !> Note that SKELETON_NEWLINE as the last element of the skeleton only inserts a
    !> newline if not at eol.  If you want to unconditionally insert a newline
    !> at the end of the skeleton, use skeleton_text(LF) instead.  Likewise with
    !> SKELETON_NEWLINE as the first element when at bol.
    !>
    !> Further elements can be defined via `skeleton_further_elements'.
    !> ELEMENT may itself be a SKELETON with an INTERACTOR, ended by
    !> SKELETON_END.  The user is prompted repeatedly for different inputs.
    !> The SKELETON is processed as often as the user enters a non-empty
    !> string.  \[keyboard_quit] terminates skeleton insertion, but continues
    !> after SKELETON_RESUME and positions at SKELETON_POINT if any.  If
    !> INTERACTOR in such a subskeleton is a prompt given as a Fortran format
    !> (`skeleton_prompt(format=...)'), it writes `skeleton_subprompt' with
    !> its `a' edit descriptor.  Such an INTERACTOR may
    !> also be a list of strings (`skeleton_for_each') with the subskeleton
    !> being repeated once for each string.
    !>
    !> Procedures (`skeleton_action') are called for their side-effects.
    !> Other procedures (`skeleton_value') are called and the element they
    !> give treated as above.  The following variables
    !> (components of `skeleton_state') are available:
    !>
    !>         string  first time: read a string according to INTERACTOR
    !>                 then: insert previously read string once more (SKELETON_STRING)
    !>         help    help text during interaction with the user or unallocated
    !>         initial_input   initial input while reading string
    !>         v1, v2  variables for memorizing anything you want
    !>                 (SKELETON_V1, SKELETON_V2)
    !>
    !> Port of skeleton.el:skeleton-insert.
    recursive subroutine skeleton_insert(skeleton, status, regions_count, string)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: regions_count
        character(len=*), intent(in), optional :: string
        type(handle_type), allocatable :: saved_regions(:)
        type(skeleton_state_type) :: saved_state
        procedure(skeleton_bind_procedure), pointer :: further
        integer :: saved_next
        integer(int64) :: saved_point, beginning
        logical :: saved_unmoved, again
        type(status_type) :: st
        type(handle_type) :: window

        ! skeleton-insert's let and dlet: save, run, restore.
        if (allocated(regions)) call move_alloc(regions, saved_regions)
        saved_next = region_next
        saved_point = interesting_point
        saved_unmoved = point_unmoved
        saved_state = skeleton_state
        allocate (regions(0))
        region_next = 1
        status = status_type()
        if (present(regions_count)) call setup_regions(regions_count, status)

        if (status%ok()) then
            beginning = point()
            interesting_point = 0
            point_unmoved = .false.
            skeleton_state = skeleton_state_type()
            allocate (skeleton_state%v1(0), skeleton_state%v2(0))
            if (allocated(skeleton_positions)) deallocate (skeleton_positions)
            allocate (skeleton_positions(0))
            further => skeleton_further_elements%get_current()
            if (associated(further)) call further(.true.)
            call internal_list(skeleton, 1, size(skeleton), .false., again, status, string)
            if (associated(further)) call further(.false.)
            ! The cleanup of upstream's unwind-protect: it runs after an
            ! error or a quit too.
            if (.not. is_end_of_line() .and. skeleton_end_newline%value) call newline_and_indent(st)
            call skeleton_end_hook%execute(st)
            call sit_for(0_int32)
            window = selected_window()

            if (same_buffer(window_buffer(window, st), current_buffer())) then
                if (.not. is_position_visible_in_window(beginning, window)) then
                    call goto_character(beginning)
                    call recenter(prefix_argument_type(PREFIX_NUMBER, 0_int64), .true., st)
                end if
            end if

            if (interesting_point > 0) call goto_character(interesting_point)
        end if

        call destroy_regions()
        if (allocated(saved_regions)) call move_alloc(saved_regions, regions)
        region_next = saved_next
        interesting_point = saved_point
        point_unmoved = saved_unmoved
        skeleton_state = saved_state
    end subroutine skeleton_insert

    ! Whether handles A and B are the same buffer.  Not a port.
    logical function same_buffer(a, b)
        type(handle_type), intent(in) :: a, b

        same_buffer = a%index == b%index .and. a%generation == b%generation
    end function same_buffer

    ! Set up skeleton-regions for REGIONS_COUNT (see `skeleton_insert').
    ! Not a port.
    subroutine setup_regions(regions_count, status)
        integer(int64), intent(in) :: regions_count
        type(status_type), intent(out) :: status
        type(handle_type) :: m
        type(handle_type), allocatable :: copies(:)
        integer(int64), allocatable :: positions(:)
        integer(int64) :: n, position, key
        integer :: i, j, k, ring
        type(handle_type) :: key_marker

        status = status_type()
        if (regions_count == 0) return

        if (regions_count > 0) then
            deallocate (regions)
            allocate (regions(2))
            call copy_marker_at(point(), regions(1), .true., status)
            position = point()
            call forward_word(regions_count)
            call copy_marker_at(point(), regions(2), .false., status)
            call goto_character(position)
        else
            ! Copy -REGIONS_COUNT elements of the mark and the mark ring.
            n = -regions_count
            allocate (copies(1))
            call copy_marker_at(point(), copies(1), .true., status)
            k = 0
            ring = 0
            if (allocated(buffers(current)%mark_ring)) ring = size(buffers(current)%mark_ring)

            do while (n > 0 .and. k <= ring)
                if (k == 0) then
                    m = buffers(current)%mark
                else
                    m = buffers(current)%mark_ring(k)
                end if

                call marker_position(m, position, status)

                if (.not. status%ok() .or. position <= 0) then
                    call destroy_list(copies)
                    call set_error(status, GENERIC_ERROR_CODE, "Marker does not point anywhere")
                    return
                end if

                copies = [copies, m]
                call copy_marker_at(position, copies(size(copies)), .true., status)
                k = k + 1
                n = n - 1
            end do

            ! Sort by position (stably, newest first as pushed).
            allocate (positions(size(copies)))

            do i = 1, size(copies)
                call marker_position(copies(i), positions(i), status)
            end do

            copies = copies(size(copies):1:-1)
            positions = positions(size(positions):1:-1)

            do i = 2, size(copies)
                key = positions(i)
                key_marker = copies(i)
                j = i - 1

                do while (j >= 1)
                    if (positions(j) <= key) exit
                    positions(j + 1) = positions(j)
                    copies(j + 1) = copies(j)
                    j = j - 1
                end do

                positions(j + 1) = key
                copies(j + 1) = key_marker
            end do

            call move_alloc(copies, regions)
        end if

        call marker_position(regions(1), position, status)
        call goto_character(position)
        region_next = 2
    end subroutine setup_regions

    ! Destroy the markers of LIST.  Not a port.
    subroutine destroy_list(list)
        type(handle_type), intent(in) :: list(:)
        type(status_type) :: st
        integer :: i

        do i = 1, size(list)
            call destroy_marker(list(i), st)
        end do
    end subroutine destroy_list

    ! Destroy the markers of skeleton-regions.  Not a port.
    subroutine destroy_regions()
        if (allocated(regions)) then
            call destroy_list(regions)
            deallocate (regions)
        end if
    end subroutine destroy_regions

    ! Whether skeleton-regions has markers left.  Not a port.
    logical function has_regions()
        has_regions = .false.
        if (allocated(regions)) has_regions = region_next <= size(regions)
    end function has_regions

    ! The position of the next marker of skeleton-regions.  Not a port.
    integer(int64) function next_region() result(position)
        type(status_type) :: st

        call marker_position(regions(region_next), position, st)
    end function next_region

    !> Function for reading a string from the minibuffer within skeletons.
    !>
    !> PROMPT must be a string, or READER a procedure that returns a string.
    !> FORMAT, in place of PROMPT, is a Fortran format that writes the
    !> prompt with `skeleton_subprompt' as its one `a' item.
    !> If INITIAL_INPUT or `skeleton_state%initial_input' is present, it is
    !> inserted before reading.  If RECURSIVE is true, i.e. we are handling the
    !> iterator of a subskeleton, a quit occurs (with the status detail
    !> SKELETON_QUIT_ITERATION) if
    !> the user didn't modify input.  Port of skeleton.el:skeleton-read.
    recursive subroutine skeleton_read(result, status, prompt, reader, initial_input, recursive, format)
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        procedure(skeleton_read_procedure), optional :: reader
        character(len=*), intent(in), optional :: initial_input
        logical, intent(in), optional :: recursive
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: initial, prompt_text
        type(string_type) :: subprompt(1)
        logical :: at_end_of_line, quit
        type(status_type) :: st
        type(handle_type) :: original_point
        integer(int64) :: position_value

        status = status_type()

        if (present(format)) then
            if (.not. allocated(skeleton_subprompt)) call initialize_subprompt()
            subprompt(1)%text = skeleton_subprompt
            call write_with_format(format, subprompt, prompt_text, status)
            if (.not. status%ok()) return
        else if (present(prompt)) then
            prompt_text = prompt
        end if

        at_end_of_line = is_end_of_line()

        ! since Emacs doesn't show main window's cursor, do something noticeable
        if (.not. at_end_of_line) then
            call insert(LF, st)
            call goto_character(point() - 1)
        end if

        if (present(initial_input)) initial = initial_input

        if (allocated(prompt_text)) then
            ! The user may issue commands to move around (like `C-M-v').
            ! Ensure that we insert the skeleton at the correct (initial) point.
            if (.not. allocated(initial)) then
                if (allocated(skeleton_state%initial_input)) initial = skeleton_state%initial_input
            end if

            call copy_marker_at(point(), original_point, .false., st)

            if (allocated(initial)) then
                if (present(initial_input) .or. skeleton_state%initial_position == 0) then
                    call read_string(prompt_text, result, status, initial_input=initial)
                else
                    call read_string(prompt_text, result, status, initial_input=initial, &
                                     position=skeleton_state%initial_position)
                end if
            else
                call read_string(prompt_text, result, status)
            end if

            call marker_position(original_point, position_value, st)
            call goto_character(position_value)
            call destroy_marker(original_point, st)
        else if (present(reader)) then
            call reader(result, status)
        end if

        if (.not. at_end_of_line) call delete_character(1_int64, st)
        if (.not. status%ok()) return

        if (present(recursive)) then
            if (recursive) then
                quit = .not. allocated(result)

                if (.not. quit) then
                    quit = len(result) == 0
                    if (allocated(initial)) quit = quit .or. result == initial
                end if

                if (quit) call set_error(status, QUIT_CODE, SKELETON_QUIT_ITERATION)
            end if
        end if
    end subroutine skeleton_read

    ! The elements of the list from FIRST to LAST of SKELETON: FIRST is its
    ! interactor.  STRING is the value of string (absent: read according to the
    ! interactor), RECURSIVE_IN true for a subskeleton loop.  RESULT is
    ! upstream's return value (true to go on looping).
    ! Port of skeleton.el:skeleton-internal-list.
    recursive subroutine internal_list(skeleton, first, last, recursive_in, result, status, string)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(in) :: first, last
        logical, intent(in) :: recursive_in
        logical, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        character(len=:), allocatable :: line, saved_string
        type(skeleton_element_type) :: saved_interactor, element
        logical :: saved_pending, saved_recursive, first_as_indent, recursive, quitting, has_saved_string
        integer(int64) :: start, column, original_point, reached
        integer :: il, k
        type(status_type) :: st

        status = status_type()
        start = line_beginning_position()
        column = current_column()
        call buffer_substring(start, line_end_position(), line, st)
        ! dlet string: save, set, run, restore.
        has_saved_string = allocated(skeleton_state%string)
        if (has_saved_string) saved_string = skeleton_state%string
        saved_pending = skeleton_state%string_pending
        saved_interactor = skeleton_state%interactor
        saved_recursive = skeleton_state%recursive

        if (present(string)) then
            skeleton_state%string = string
            skeleton_state%string_pending = .false.
        else
            if (allocated(skeleton_state%string)) deallocate (skeleton_state%string)
            skeleton_state%string_pending = .true.
            skeleton_state%interactor = SKELETON_NO_INTERACTOR

            if (first <= last) then
                if (skeleton(first)%kind == KIND_INTERACTOR) then
                    skeleton_state%interactor = skeleton(first)
                else if (skeleton(first)%kind == KIND_TEXT) then
                    skeleton_state%interactor = skeleton_prompt(text_of(skeleton(first)))
                end if
            end if

            skeleton_state%recursive = recursive_in
        end if

        recursive = recursive_in
        quitting = .false.
        first_as_indent = .false.

        if (first + 1 <= last .and. .not. recursive) then
            if (skeleton(first + 1)%kind == KIND_NEWLINE) then
                original_point = point()
                k = int(skip_characters_backward(" "//achar(9), st))
                first_as_indent = is_beginning_of_line()
                call goto_character(original_point)
            end if
        end if

        il = first
        original_point = -1

        do
            point_unmoved = original_point == point()
            original_point = point()

            if (il == first) then
                il = first + 1
            else
                il = next_index(skeleton, il)
            end if

            if (il > last) exit
            element = skeleton(il)
            if (il == first + 1 .and. first_as_indent) element = SKELETON_INDENT
            call internal_1(skeleton, il, last, element, .false., recursive, status)
            if (status%ok()) cycle
            if (status%code /= QUIT_CODE) exit

            if (quit_detail(status) == SKELETON_QUIT_RECURSIVE) then
                status = status_type()
                quitting = .true.
                ! Go on at resume:, if any.
                k = il
                il = last + 1

                do while (k <= last)
                    if (skeleton(k)%kind == KIND_RESUME) then
                        il = k
                        exit
                    end if

                    k = next_index(skeleton, k)
                end do

                if (il > last) exit
            else
                ! Remove the subskeleton as far as it has been shown
                ! the subskeleton shouldn't have deleted outside current line.
                call end_of_line()
                call delete_region(start, point(), st)
                call insert(line, st)
                call move_to_column(column, reached)

                if (quit_detail(status) == SKELETON_QUIT_ITERATION) then
                    status = status_type()
                    quitting = .false.
                    recursive = .false.
                else
                    call set_error(status, QUIT_CODE, SKELETON_QUIT_RECURSIVE)
                end if

                exit
            end if
        end do

        if (has_saved_string) then
            skeleton_state%string = saved_string
        else if (allocated(skeleton_state%string)) then
            deallocate (skeleton_state%string)
        end if

        skeleton_state%string_pending = saved_pending
        skeleton_state%interactor = saved_interactor
        skeleton_state%recursive = saved_recursive
        result = recursive
        ! maybe continue loop or go on to next outer resume: section
        if (status%ok() .and. quitting) call set_error(status, QUIT_CODE, SKELETON_QUIT_RECURSIVE)
    end subroutine internal_list

    ! The data of a quit (the status detail): SKELETON_QUIT_ITERATION (upstream's
    ! t: stop iterating the sub-skeleton), SKELETON_QUIT_RECURSIVE or "".
    ! Not a port.
    function quit_detail(status) result(detail)
        type(status_type), intent(in) :: status
        character(len=:), allocatable :: detail

        detail = ""
        if (allocated(status%detail)) detail = status%detail
    end function quit_detail

    ! One ELEMENT of the list SKELETON, at IL (upstream's skeleton-il,
    ! advanced past the elements it pops) whose top level ends at LAST.
    ! LITERAL: insert strings untransformed.  Port of
    ! skeleton.el:skeleton-internal-1.
    recursive subroutine internal_1(skeleton, il, last, element, literal, recursive, status)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(inout) :: il
        integer, intent(in) :: last
        type(skeleton_element_type), intent(in) :: element
        logical, intent(in) :: literal, recursive
        type(status_type), intent(out) :: status
        type(skeleton_element_type), allocatable :: value(:)
        type(status_type) :: st
        integer(int64) :: position, point_value, column
        integer :: next

        status = status_type()
        next = next_index(skeleton, il)

        select case (element%kind)
        case (KIND_TEXT, KIND_LITERAL)
            if (literal .or. element%kind == KIND_LITERAL) then
                call insert(text_of(element), status)
            else
                call insert(skeleton_transform(text_of(element)), status)
            end if
        case (KIND_DELETE)
            if (skeleton_untabify%value) then
                call backward_delete_character_untabify(int(element%count, int64), status)
            else
                call delete_character(-int(element%count, int64), status)
            end if
        case (KIND_NEWLINE, KIND_INDENT)
            ! The sequence `> \n' is handled specially so as to indent the first
            ! line after inserting the newline (to get the proper indentation).
            position = 0

            if (element%kind == KIND_INDENT) then
                if (next > last) then
                    call indent_alone()
                    return
                end if

                if (skeleton(next)%kind /= KIND_NEWLINE) then
                    call indent_alone()
                    return
                end if

                position = point()
                il = next
                next = next_index(skeleton, il)
            end if

            if (has_regions() .and. is_next(KIND_POINT)) then
                if (.not. is_end_of_line()) call insert(LF, status)
                if (position > 0) call indent_at(position)
                call indent_region(line_beginning_position(), next_region(), status)
            else if (next > last .and. .not. recursive .and. is_end_of_line()) then
                ! \n as last element only inserts \n if not at eol.
                if (position > 0) call indent_according_to_mode(status)
            else if (skeleton_newline_indent_rigidly%value) then
                point_value = point()
                call insert(LF, status)
                column = indentation_at(point_value, position > 0)
                column = indent_to(column, status)
            else
                if (position > 0) then
                    call reindent_then_newline_and_indent(status)
                else
                    call insert(LF, status)
                    call indent_according_to_mode(status)
                end if
            end if
        case (KIND_POINT)
            if (has_regions()) then
                call goto_character(next_region())
                region_next = region_next + 1

                if (current_column() <= current_indentation() .and. is_next(KIND_NEWLINE)) then
                    call end_of_line(0_int64)
                end if
            else
                if (interesting_point == 0) interesting_point = point()
            end if
        case (KIND_POINT_OVERRIDE)
            interesting_point = point()
        case (KIND_IF_MOVED)
            if (point_unmoved) call pop()
        case (KIND_IF_NOT_MOVED)
            if (.not. point_unmoved) call pop()
        case (KIND_POSITION)
            skeleton_positions = [point(), skeleton_positions]
        case (KIND_ACTION)
            if (associated(element%action)) call element%action(status)
        case (KIND_INTERACTOR)
            call sub_skeleton(skeleton, il, status)
        case (KIND_STRING)
            if (skeleton_state%string_pending) then
                call skeleton_read_string(status)
                if (.not. status%ok()) return
            end if

            ! No input read (no interactor, or one that left no string): upstream's
            ! `string' is nil and (null element) skips it.
            if (.not. allocated(skeleton_state%string)) return
            call insert(skeleton_state%string, status)
        case (KIND_VALUE)
            if (.not. associated(element%value)) return
            if (allocated(skeleton_state%value)) deallocate (skeleton_state%value)
            call element%value(status)
            if (.not. status%ok()) return
            if (allocated(skeleton_state%value)) call move_alloc(skeleton_state%value, value)
            call value_element(value)
        case (KIND_V1)
            call value_element(skeleton_state%v1)
        case (KIND_V2)
            call value_element(skeleton_state%v2)
        case default
            ! resume: and nil are skipped.
        end select

    contains

        ! Whether the element after skeleton-il is of KIND.
        logical function is_next(kind)
            integer, intent(in) :: kind

            is_next = .false.
            if (next <= last) is_next = skeleton(next)%kind == kind
        end function is_next

        ! (pop skeleton-il)
        subroutine pop()
            il = next_index(skeleton, il)
        end subroutine pop

        ! The `>' element on its own.
        subroutine indent_alone()
            next = next_index(skeleton, il)

            if (has_regions() .and. is_next(KIND_POINT)) then
                call indent_region(line_beginning_position(), next_region(), status)
            else
                call indent_according_to_mode(status)
            end if
        end subroutine indent_alone

        ! The element a procedure gave, VALUE, treated as one element.
        recursive subroutine value_element(value)
            type(skeleton_element_type), allocatable, intent(in) :: value(:)

            if (.not. allocated(value)) return
            if (size(value) == 0) return

            if (value(1)%kind == KIND_INTERACTOR) then
                call sub_skeleton(value, 1, status)
            else
                call internal_1(skeleton, il, last, value(1), .true., recursive, status)
            end if
        end subroutine value_element
    end subroutine internal_1

    ! Indent the line at POSITION according to mode, point staying where it is
    ! (save-excursion).  Not a port.
    recursive subroutine indent_at(position)
        integer(int64), intent(in) :: position
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: back

        call copy_marker_at(point(), m, .false., st)
        call goto_character(position)
        call indent_according_to_mode(st)
        call marker_position(m, back, st)
        call goto_character(back)
        call destroy_marker(m, st)
    end subroutine indent_at

    ! The indentation of the line at POSITION, after indenting it if INDENT,
    ! point staying where it is (save-excursion).  Not a port.
    recursive integer(int64) function indentation_at(position, indent) result(column)
        integer(int64), intent(in) :: position
        logical, intent(in) :: indent
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: back

        call copy_marker_at(point(), m, .false., st)
        call goto_character(position)
        if (indent) call indent_according_to_mode(st)
        column = current_indentation()
        call marker_position(m, back, st)
        call goto_character(back)
        call destroy_marker(m, st)
    end function indentation_at

    ! The sub-skeleton of SKELETON whose interactor is at FIRST.
    ! Port of skeleton.el:skeleton-internal-1 (its sub-skeleton case).
    recursive subroutine sub_skeleton(skeleton, first, status)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(in) :: first
        type(status_type), intent(out) :: status
        integer :: last, k, n
        logical :: again

        status = status_type()
        last = matching_end(skeleton, first) - 1

        if (skeleton(first)%interactor == INTERACTOR_STRINGS) then
            n = 0

            do k = 1, skeleton(first)%count
                call internal_list(skeleton, first, last, .false., again, status, &
                                   string=skeleton(first)%text(n + 1:n + skeleton(first)%lengths(k)))
                if (.not. status%ok()) return
                n = n + skeleton(first)%lengths(k)
            end do
        else
            do
                call internal_list(skeleton, first, last, .true., again, status)
                if (.not. status%ok()) return
                ! If the interactor is nil, don't infinite loop.
                if (.not. again .or. skeleton(first)%interactor == INTERACTOR_NONE) exit
            end do
        end if
    end subroutine sub_skeleton

    !> Read the string `skeleton_state%string' according to the current
    !> list's interactor, as upstream's skeleton-read of the interactor does when
    !> upstream's str is first evaluated (call it only while
    !> `skeleton_state%string_pending').  Not a port.
    recursive subroutine skeleton_read_string(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        type(skeleton_element_type) :: interactor

        interactor = skeleton_state%interactor

        select case (interactor%interactor)
        case (INTERACTOR_PROMPT)
            if (interactor%prompt_format) then
                call skeleton_read(string, status, recursive=skeleton_state%recursive, format=text_of(interactor))
            else
                call skeleton_read(string, status, prompt=text_of(interactor), recursive=skeleton_state%recursive)
            end if
        case (INTERACTOR_PROCEDURE)
            call skeleton_read(string, status, reader=interactor%read, recursive=skeleton_state%recursive)
        case default
            call skeleton_read(string, status, recursive=skeleton_state%recursive)
        end select

        if (.not. status%ok()) return
        skeleton_state%string_pending = .false.

        if (allocated(string)) then
            call move_alloc(string, skeleton_state%string)
        else if (allocated(skeleton_state%string)) then
            deallocate (skeleton_state%string)
        end if
    end subroutine skeleton_read_string

    !> Insert SKELETON.
    !> Prefix ARGUMENT allows wrapping around words or regions (see `skeleton_insert').
    !> If no ARGUMENT was given, but the region is visible, ARGUMENT defaults to -1 depending
    !> on `skeleton_autowrap'.  An ARGUMENT of  M-0  will prevent this just for once.
    !>
    !> Optional argument STRING may also be a string which will be the value
    !> of string whereas the skeleton's interactor is then ignored.  Port of
    !> skeleton.el:skeleton-proxy-new.
    recursive subroutine skeleton_proxy_new(skeleton, status, string, argument)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument
        integer(int64) :: regions_count
        logical :: has_count

        ! Pretend  C-x a e  passed its prefix arg to us
        has_count = .false.

        if (present(argument)) then
            if (argument%kind /= PREFIX_NONE) then
                regions_count = prefix_numeric_value(argument)
                has_count = .true.
            end if
        end if

        if (.not. has_count .and. current_prefix_argument%kind /= PREFIX_NONE) then
            regions_count = prefix_numeric_value(current_prefix_argument)
            has_count = .true.
        end if

        if (.not. has_count .and. skeleton_autowrap%value) then
            if (is_use_region()) then
                ! Deactivate the mark, in case one of the elements of the
                ! skeleton is sensitive to such situations (e.g. it is itself a
                ! skeleton).
                call deactivate_mark()
                regions_count = -1
                has_count = .true.
            end if
        end if

        if (has_count) then
            call skeleton_insert(skeleton, status, regions_count, string)
        else
            call skeleton_insert(skeleton, status, string=string)
        end if
    end subroutine skeleton_proxy_new

    !> Insert the skeleton the current buffer's `skeleton_filter_function'
    !> chooses from VARIANTS, as `skeleton_proxy_new' does.  With the
    !> identity (null), VARIANTS are taken as the skeleton itself, as
    !> upstream does: its first variant is the interactor and the second
    !> is called, a `void-function' error condition naming it (with one
    !> variant nothing is inserted).  Port of skeleton.el:skeleton-proxy-new
    !> (with skeleton-filter-function).
    recursive subroutine skeleton_variants_proxy_new(variants, status, string, argument)
        type(skeleton_variants_type), intent(in) :: variants
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument
        procedure(skeleton_filter_procedure), pointer :: fn
        type(skeleton_element_type), allocatable :: skeleton(:)

        fn => skeleton_filter_function%get_current()

        if (associated(fn)) then
            call fn(variants, skeleton, status)
            if (.not. status%ok()) return
        else
            allocate (skeleton(0))
        end if

        if (.not. associated(fn) .and. allocated(variants%entries)) then
            if (size(variants%entries) >= 2) then
                call skeleton_proxy_new(skeleton, status, string, argument)
                if (status%ok()) call set_error(status, VOID_FUNCTION_CODE, variants%entries(2)%name)
                return
            end if
        end if

        call skeleton_proxy_new(skeleton, status, string, argument)
    end subroutine skeleton_variants_proxy_new

    !> Insert SKELETON, the body of a command defined with upstream's
    !> define-skeleton: its changes are undone if an error or a quit occurs.
    !> STRING and ARGUMENT are the command's arguments (see `skeleton_proxy_new').
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> skeleton.el:define-skeleton (the command it defines).
    recursive subroutine skeleton_command(skeleton, status, string, argument)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument
        type(change_group_type) :: handle
        type(status_type) :: st

        call prepare_change_group(handle)
        call activate_change_group(handle)
        call skeleton_proxy_new(skeleton, status, string, argument)

        if (status%ok()) then
            call accept_change_group(handle)
        else
            call cancel_change_group(handle, st)
        end if
    end subroutine skeleton_command

    !> `skeleton_command' for a command whose skeletons are VARIANTS (see
    !> `skeleton_variants_proxy_new').  Port of skeleton.el:define-skeleton
    !> (the command it defines).
    recursive subroutine skeleton_variants_command(variants, status, string, argument)
        type(skeleton_variants_type), intent(in) :: variants
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument
        type(change_group_type) :: handle
        type(status_type) :: st

        call prepare_change_group(handle)
        call activate_change_group(handle)
        call skeleton_variants_proxy_new(variants, status, string, argument)

        if (status%ok()) then
            call accept_change_group(handle)
        else
            call cancel_change_group(handle, st)
        end if
    end subroutine skeleton_variants_command

    ! ------------------------------------------------------------------
    ! Paired insertion
    ! ------------------------------------------------------------------

    !> Insert the character you type ARGUMENT times.
    !>
    !> With no ARGUMENT, if `skeleton_pair' is true, pairing can occur.  If the region
    !> is visible the characters are wrapped around it depending on `skeleton_autowrap'.
    !> Else, if `skeleton_pair_on_word' is true or we are not before or inside a
    !> word, and if `skeleton_pair_filter_function' returns false, pairing is performed.
    !> Pairing is also prohibited if we are right after a quoting character
    !> such as backslash.
    !>
    !> If a match is found in `skeleton_pairs', that is inserted, else
    !> the defaults are used.  These are (), [], {}, <> and (grave
    !> accent, apostrophe) for the paired ones, and the same character
    !> twice for the others.  Port of skeleton.el:skeleton-pair-insert-maybe.
    recursive subroutine skeleton_pair_insert_maybe(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(skeleton_element_type), allocatable :: skeleton(:)
        procedure(skeleton_test_procedure), pointer :: filter
        character :: syntax
        character(len=:), allocatable :: text
        integer(int64) :: c
        logical :: mark, saved, inhibit, found

        c = -1
        if (vtag(last_command_event) == V_INTEGER) c = last_command_event%a(1)%i
        status = status_type()

        if (argument%kind /= PREFIX_NONE .or. .not. skeleton_pair%value) then
            call self_insert_command(prefix_numeric_value(argument), c, status)
            return
        end if

        mark = .false.
        if (skeleton_autowrap%value) mark = transient_mark_mode%value .and. buffers(current)%mark_active
        found = .false.
        call find_pair(skeleton_pairs%get_current())
        if (.not. found) call find_pair(skeleton_default_pairs%get_current())

        if (.not. found) then
            call character_to_string(int(c, int32), text, status)
            if (.not. status%ok()) return
            skeleton = [SKELETON_NO_INTERACTOR, skeleton_text(text), SKELETON_POINT, skeleton_text(text)]
        end if

        syntax = character_syntax(preceding_character())
        inhibit = syntax == "\" .or. syntax == "/"

        if (.not. inhibit .and. .not. mark) then
            if (.not. skeleton_pair_on_word%value) inhibit = looking_at("\w", status)

            if (.not. inhibit) then
                filter => skeleton_pair_filter_function%get_current()
                if (associated(filter)) inhibit = filter()
            end if
        end if

        if (inhibit) then
            call self_insert_command(prefix_numeric_value(argument), c, status)
            return
        end if

        ! Newlines not desirable for inserting pairs.  See bug#16138.
        saved = skeleton_end_newline%value
        skeleton_end_newline = .false.

        if (mark) then
            call skeleton_insert(skeleton, status, -1_int64)
        else
            call skeleton_insert(skeleton, status)
        end if

        skeleton_end_newline = saved

    contains

        ! (assq char PAIRS), the skeleton with no interactor.
        subroutine find_pair(pairs)
            type(skeleton_pairs_type), intent(in) :: pairs
            integer :: i

            if (found .or. .not. allocated(pairs%entries)) return

            do i = 1, size(pairs%entries)
                if (pairs%entries(i)%char == c) then
                    skeleton = [SKELETON_NO_INTERACTOR, pairs%entries(i)%elements]
                    found = .true.
                    return
                end if
            end do
        end subroutine find_pair
    end subroutine skeleton_pair_insert_maybe

    ! Not a port.
    recursive subroutine execute_skeleton_pair_insert_maybe(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call skeleton_pair_insert_maybe(current_prefix_argument, status)
    end subroutine execute_skeleton_pair_insert_maybe

    ! skeleton-subprompt's value.  Not a port.
    subroutine initialize_subprompt()
        skeleton_subprompt = substitute_command_keys("RET, \<minibuffer_local_map>\[abort_recursive_edit] or \[help_command]")
    end subroutine initialize_subprompt

    !> Register the commands and the settings, and make the default pairing partners.
    !> Not a port.
    subroutine initialize_skeleton()
        integer :: k
        character(len=:), allocatable :: opening_text, closing_text
        type(status_type) :: st
        integer, parameter :: opening(6) = [40, 91, 123, 60, 171, 96], closing(6) = [41, 93, 125, 62, 187, 39]

        commands%skeleton_pair_insert_maybe%execute => execute_skeleton_pair_insert_maybe
        call forward_setting(skeleton_autowrap)
        call forward_setting(skeleton_end_newline)
        call forward_setting(skeleton_untabify)
        call forward_setting(skeleton_newline_indent_rigidly)
        call forward_setting(skeleton_pair)
        call forward_setting(skeleton_pair_on_word)
        if (allocated(skeleton_default_pairs%pairs%entries)) deallocate (skeleton_default_pairs%pairs%entries)

        do k = 1, size(opening)
            call character_to_string(opening(k), opening_text, st)
            call character_to_string(closing(k), closing_text, st)
            call skeleton_default_pairs%append(opening(k), [skeleton_text(opening_text), SKELETON_POINT, &
                                                            skeleton_text(closing_text)])
            if (k < size(opening)) call skeleton_default_pairs%append(closing(k), [skeleton_text(closing_text)])
        end do

        if (.not. allocated(skeleton_pairs%pairs%entries)) allocate (skeleton_pairs%pairs%entries(0))
        if (.not. allocated(skeleton_positions)) allocate (skeleton_positions(0))
        call append_locals_clearer(clear_skeleton_locals)
    end subroutine initialize_skeleton
end module skeleton_mod
