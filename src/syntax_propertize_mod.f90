! syntax_propertize_mod.f90 -- syntax-table properties and the parse state cache.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/syntax.el.
!
! Copyright (C) 2000-2026 Free Software Foundation, Inc.
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

!> Applying `syntax_table' text properties where needed
!> (`syntax_propertize', run lazily by the syntax scanner through
!> `internal_syntax_propertize') and the cached parse state
!> `syntax_parser_state'.  Named so as not to clash with syntax_mod (syntax.c).
!>
!> The `syntax_propertize_rules' macro becomes the type
!> `syntax_propertize_rules_type': its RULES are data (a regexp and its
!> highlights), and `apply' scans the buffer as the function the macro
!> makes does.  A highlight's SYNTAX is a string (for `string_to_syntax'),
!> a procedure returning the property (upstream's expression), or either
!> of them with code run after the property is applied (upstream's
!> (prog1 SYNTAX . EXPS)); code alone is upstream's (ignore . EXPS).
!> Precompiled rule sets (`syntax_propertize_precompile_rules') are
!> arrays of rules, joined as Fortran arrays.
!>
!> Deferred: `syntax_propertize_via_font_lock' (font-lock-syntactic-keywords
!> are not ported), `syntax_parser_state_debug' and `syntax_parser_state_statistics'.
module syntax_propertize_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use utility_functions_mod, only: count_characters
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, ARGUMENTS_OUT_OF_RANGE_CODE
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current, buffer_serial_live
    use value_mod, only: value_type, list, make_syntax_table_reference, vtag, V_NULL, V_NAME
    use data_mod, only: value_eq, append_locals_clearer
    use hooks_mod, only: region_hook_type, region_entry_type
    use insert_delete_mod, only: before_change_functions
    use text_properties_mod, only: get_text_property, put_text_property, remove_text_properties, &
                                   previous_single_property_change, text_property_any
    use syntax_mod, only: parse_state_type, parse_partial_expression, syntax_table, set_syntax_table, is_syntax_table_live, &
                          string_to_syntax, syntax_descriptor_type, syntax_descriptor_value, syntax_propertize_done, &
                          parse_expression_lookup_properties, internal_syntax_propertize, syntax_parser_state_function
    use search_mod, only: regular_expression_search_forward, string_match, match_beginning, match_end, match_data, set_match_data, &
                          string_index_to_offset, SEARCH_NOERROR
    use basic_subprograms_mod, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end, &
                                     is_subregexp_context
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, &
                                     line_beginning_position, inhibit_field_text_motion
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use display_engine_mod, only: message
    use regular_expression_optimization_mod, only: regular_expression_optimization_depth
    use buffer_local_setting_mod, only: buffer_local_setting_type
    implicit none(type, external)
    private

    public :: syntax_propertize, internal_syntax_propertize_function, syntax_parser_state_cached, syntax_parser_state_flush_cache, &
              syntax_propertize_wholelines, syntax_propertize_multiline, syntax_propertize_shift_groups_and_backrefs, &
              is_syntax_propertize_in_process, syntax_parser_state_depth, syntax_parser_state_toplevel_position, &
              syntax_parser_state_context, &
              initialize_syntax_propertize

    !> What `syntax_parser_state_context' returns: neither, a string, a comment.
    integer, parameter, public :: SYNTAX_PARSER_STATE_NONE = 0, SYNTAX_PARSER_STATE_STRING = 1, SYNTAX_PARSER_STATE_COMMENT = 2

    !> most-positive-fixnum, the value of `syntax_propertize_done' while
    !> `syntax_propertize' runs (saved and restored against recursion, as upstream binds it).
    integer(int64), parameter :: MOST_POSITIVE_FIXNUM = 2_int64**61 - 1

    !> A value of the `syntax_table' property: a raw syntax descriptor,
    !> or the syntax table `table' (when `is_table'); `apply' .false. is none.
    type, public :: syntax_property_type
        logical :: apply = .false.
        type(syntax_descriptor_type) :: descriptor
        logical :: is_table = .false.
        type(handle_type) :: table
    end type syntax_property_type

    abstract interface
        !> A `syntax_propertize_function': apply `syntax_table' properties
        !> to the text from START to END.  Not a port.
        subroutine syntax_propertize_procedure(start, end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine syntax_propertize_procedure

        !> The SYNTAX of a highlight given as code: the value to apply as
        !> `syntax_table' property, in PROPERTY (none when unset).  It may move
        !> point.  Not a port.
        subroutine syntax_value_procedure(property, status)
            import :: syntax_property_type, status_type
            implicit none(type, external)
            type(syntax_property_type), intent(out) :: property
            type(status_type), intent(out) :: status
        end subroutine syntax_value_procedure

        !> Code run by a highlight (the EXPS of (prog1 SYNTAX . EXPS) or
        !> (ignore . EXPS)).  Not a port.
        subroutine syntax_code_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine syntax_code_procedure

        !> `syntax_begin_function': move back outside of any
        !> comment/string/paren.  Not a port.
        subroutine syntax_begin_procedure()
            implicit none(type, external)
        end subroutine syntax_begin_procedure
    end interface

    !> One (NUMBER SYNTAX) of a rule: apply SYNTAX to the chars matched by
    !> subgroup NUMBER (`group') of the rule's regexp.  SYNTAX is `syntax'
    !> (a string, converted with `string_to_syntax') or `value' (code
    !> returning it); `after' is code run after the property is applied;
    !> with neither `syntax' nor `value', only `after' runs.
    type, public :: syntax_highlight_type
        integer :: group = 0
        character(len=:), allocatable :: syntax
        procedure(syntax_value_procedure), pointer, nopass :: value => null()
        procedure(syntax_code_procedure), pointer, nopass :: after => null()
    end type syntax_highlight_type

    !> A rule: (REGULAR_EXPRESSION HIGHLIGHT1 ... HIGHLIGHTn).
    type, public :: syntax_propertize_rule_type
        character(len=:), allocatable :: regular_expression
        type(syntax_highlight_type), allocatable :: highlights(:)
    end type syntax_propertize_rule_type

    ! One highlight as the code of a branch: the group GN after
    ! shifting, whether it runs only when GN matched.
    type :: rule_step_type
        integer :: gn = 0
        logical :: conditional = .false.
        type(syntax_highlight_type) :: highlight
        type(syntax_descriptor_type) :: descriptor
    end type rule_step_type

    ! One branch of the `cond' the macro builds.
    type :: rule_branch_type
        logical :: always = .false.
        integer, allocatable :: groups(:)
        integer :: shift = 0
        type(rule_step_type), allocatable :: steps(:)
    end type rule_branch_type

    !> What `syntax_propertize_rules' makes: RULES (assign them, then use
    !> `apply' as or in a `syntax_propertize_function').  The buffer is
    !> scanned a single time, like "lex" would, rather than once per rule.
    !> There may be at most nine back-references in the regexps of all
    !> rules in total.
    type, public :: syntax_propertize_rules_type
        type(syntax_propertize_rule_type), allocatable :: rules(:)
        logical, private :: compiled = .false.
        character(len=:), allocatable, private :: regular_expression
        type(rule_branch_type), allocatable, private :: branches(:)
    contains
        !> Apply the rules to the text from START to END.
        procedure :: apply => rules_apply
    end type syntax_propertize_rules_type

    !> One buffer's own value of
    !> `syntax_propertize_function'.
    type :: syntax_propertize_entry_type
        procedure(syntax_propertize_procedure), pointer, nopass :: procedure => null()
    end type syntax_propertize_entry_type

    !> The type of
    !> `syntax_propertize_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: syntax_propertize_setting_type
        procedure(syntax_propertize_procedure), pointer, nopass :: procedure => null()
        type(syntax_propertize_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_syntax_propertize_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_syntax_propertize_current
        procedure :: append_local_entry => append_syntax_propertize_entry
        procedure :: clear_local_entry => clear_syntax_propertize_entry
    end type syntax_propertize_setting_type

    ! A cache: LAST, a pair (LAST-POS . LAST-PPSS) caching the last
    ! invocation (`last_position' 0 is nil), and CACHE, (POSITION . PARSER_STATE) pairs in
    ! decreasing POSITION order.
    type :: parser_state_cell_type
        logical :: exists = .false.
        logical :: has_last = .false.
        integer(int64) :: last_position = 0
        type(parse_state_type) :: last_parser_state
        integer(int64), allocatable :: cache_position(:)
        type(parse_state_type), allocatable :: cache_parser_state(:)
    end type parser_state_cell_type

    ! A buffer's own variables: `syntax_parser_state_wide', `syntax_parser_state_narrow',
    ! `syntax_parser_state_narrow_start' (0 is nil) and
    ! `syntax_parser_state__updated_cache'.
    type :: parser_state_data_type
        integer(int64) :: serial = 0
        type(parser_state_cell_type) :: wide, narrow
        integer(int64) :: narrow_start = 0
        logical :: updated_cache = .false.
    end type parser_state_data_type

    !> One buffer's own `syntax_parser_state_table'.
    type :: syntax_parser_state_table_entry_type
        type(handle_type) :: table
    end type syntax_parser_state_table_entry_type

    !> The type of `syntax_parser_state_table': the global value `table'
    !> (the null handle for none) and the buffers' own values
    !> (`set_local').
    type, extends(buffer_local_setting_type), public :: syntax_parser_state_table_setting_type
        type(handle_type) :: table
        type(syntax_parser_state_table_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value, the syntax table TABLE.
        procedure :: set_local => set_syntax_parser_state_table_local
        !> The current buffer's value (its own, else the global one) and
        !> whether it is a table (FOUND).
        procedure :: get_current => get_syntax_parser_state_table_current
        procedure :: append_local_entry => append_syntax_parser_state_table_entry
        procedure :: clear_local_entry => clear_syntax_parser_state_table_entry
    end type syntax_parser_state_table_setting_type

    !> Mode-specific function to apply `syntax_table' text properties.
    !> It is the work horse of `syntax_propertize', which is called by things like
    !> Font-Lock and indentation.
    !>
    !> It is given two arguments, START and END: the start and end of the text to
    !> which `syntax_table' might need to be applied.  Major modes can use this to
    !> override the buffer's syntax table for special syntactic constructs that
    !> cannot be handled just by the buffer's syntax-table.
    !>
    !> The specified function may call `syntax_parser_state' on any position
    !> before END, but if it calls `syntax_parser_state' on some
    !> position and later modifies the buffer on some earlier position,
    !> then it is its responsibility to call `syntax_parser_state_flush_cache' to flush
    !> the now obsolete ppss info from the cache.
    !>
    !> Note: When this variable is a function, it must apply _all_ the
    !> `syntax_table' properties needed in the given text interval.
    !> Using both this function and other means to apply these
    !> properties won't work properly.  Port of syntax.el
    !> syntax-propertize-function.
    type(syntax_propertize_setting_type), public :: syntax_propertize_function

    !> How far `syntax_propertize' goes at least.  Port of syntax.el
    !> syntax-propertize-chunk-size.
    integer(int64), public :: syntax_propertize_chunk_size = 500

    !> Special hook run just before proceeding to propertize a region.
    !> This is used to allow major modes to help `syntax_propertize' find safe buffer
    !> positions as beginning and end of the propertized region.  Its most common use
    !> is to solve the problem of /identification/ of multiline elements by providing
    !> a function that tries to find such elements and move the boundaries such that
    !> they do not fall in the middle of one.
    !> Each function is called with two arguments (START and END) and it should return
    !> the new start and end (START and END themselves if no adjustment should be made).
    !> These functions are run in turn repeatedly until they all return no change.
    !> Put first the functions more likely to cause a change and cheaper to compute.
    !> Port of syntax.el syntax-propertize-extend-region-functions.
    type(region_hook_type), public, target :: syntax_propertize_extend_region_functions = &
                                              region_hook_type(name="syntax_propertize_extend_region_functions")

    !> Maximum line length for syntax operations.
    !> If lines are longer than that, syntax operations will treat them as chunks
    !> of this size.  Misfontification may then occur.
    !> This is a tradeoff between correctly applying the syntax rules,
    !> and avoiding major slowdown on pathologically long lines.  Port of
    !> syntax.el syntax-wholeline-max.
    integer(int64), public :: syntax_wholeline_maximum = 10000

    !> Syntax-table to use during `syntax_parser_state', if any.  Port of
    !> syntax.el syntax-ppss-table.
    type(syntax_parser_state_table_setting_type), public :: syntax_parser_state_table

    !> How far ahead of the position the syntax scanner needs
    !> `internal_syntax_propertize_function' propertizes.  Port of
    !> syntax.el syntax-propertize-chunks.
    integer(int64), public :: syntax_propertize_chunks = 2000

    !> Threshold below which cache info is deemed unnecessary.
    !> We try to make sure that cache entries are at least this far apart
    !> from each other, to avoid keeping too much useless info.  Port of
    !> syntax.el syntax-ppss-max-span.
    integer(int64), public :: syntax_parser_state_maximum_span = 20000

    !> Function to move back outside of any comment/string/paren.
    !> This function should move the cursor back to some syntactically safe
    !> point (where the PARSER_STATE is equivalent to the empty state).  Port of syntax.el
    !> syntax-begin-function (obsolete since 25.1).
    procedure(syntax_begin_procedure), pointer, public :: syntax_begin_function => null()

    ! Statistics about which case is more/less frequent in `syntax_parser_state'
    ! (count . total distance); the 5th slot drives the heuristic to use
    ! `syntax_begin_function'.  syntax.el syntax-ppss-stats.
    integer(int64) :: statistics_count(0:5) = [0_int64, 0_int64, 0_int64, 0_int64, 0_int64, 2_int64]
    integer(int64) :: statistics_total(0:5) = [0_int64, 0_int64, 0_int64, 0_int64, 0_int64, 2500_int64]

    type(parser_state_data_type), allocatable :: data(:)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! Find or make the entry in `data' of the current buffer's variables,
    ! its index K.
    ! Not a port.
    subroutine make_data_entry(k)
        integer, intent(out) :: k
        type(parser_state_data_type) :: entry

        if (.not. allocated(data)) allocate (data(0))
        k = findloc(data%serial, buffers(current)%serial, dim=1)
        if (k > 0) return

        do k = 1, size(data)
            if (.not. buffer_serial_live(data(k)%serial)) exit
        end do

        if (k > size(data)) data = [data, entry]
        data(k) = parser_state_data_type(serial=buffers(current)%serial)
    end subroutine make_data_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `syntax_propertize_function'.  Not a port.
    subroutine set_syntax_propertize_local(self, local_procedure)
        class(syntax_propertize_setting_type), intent(inout) :: self
        procedure(syntax_propertize_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_syntax_propertize_local

    !> The current buffer's value of
    !> `syntax_propertize_function':
    !> its own, else the global one.  Not a port.
    function get_syntax_propertize_current(self) result(current_procedure)
        class(syntax_propertize_setting_type), intent(in) :: self
        procedure(syntax_propertize_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_syntax_propertize_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_syntax_propertize_entry(self)
        class(syntax_propertize_setting_type), intent(inout) :: self
        type(syntax_propertize_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_syntax_propertize_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_syntax_propertize_entry(self, k)
        class(syntax_propertize_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_syntax_propertize_entry

    !> Give the current buffer its own `syntax_parser_state_table', TABLE.
    !> Not a port.
    subroutine set_syntax_parser_state_table_local(self, table)
        class(syntax_parser_state_table_setting_type), intent(inout) :: self
        type(handle_type), intent(in) :: table
        integer :: k

        call self%make_local_entry(k)
        if (k > 0) self%locals(k)%table = table
    end subroutine set_syntax_parser_state_table_local

    !> The current buffer's `syntax_parser_state_table', TABLE: its own
    !> value, else the global one; FOUND .false. for none.  Not a port.
    subroutine get_syntax_parser_state_table_current(self, table, found)
        class(syntax_parser_state_table_setting_type), intent(in) :: self
        type(handle_type), intent(out) :: table
        logical, intent(out) :: found
        integer :: k

        table = self%table
        k = self%get_local_index()
        if (k > 0) table = self%locals(k)%table
        found = table%index /= 0
    end subroutine get_syntax_parser_state_table_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_syntax_parser_state_table_entry(self)
        class(syntax_parser_state_table_setting_type), intent(inout) :: self
        type(syntax_parser_state_table_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_syntax_parser_state_table_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_syntax_parser_state_table_entry(self, k)
        class(syntax_parser_state_table_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%table = null_handle()
    end subroutine clear_syntax_parser_state_table_entry

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine clear_syntax_locals()
        integer :: k

        call syntax_propertize_function%clear_local()
        call syntax_parser_state_table%clear_local()
        if (current == 0) return
        call make_data_entry(k)
        data(k) = parser_state_data_type(serial=buffers(current)%serial)
    end subroutine clear_syntax_locals

    ! The table `syntax_propertize' and `syntax_parser_state' parse with:
    ! (or syntax-ppss-table (syntax-table)).  Not a port.
    function parser_state_syntax_table() result(h)
        type(handle_type) :: h
        logical :: found

        call syntax_parser_state_table%get_current(h, found)

        if (found) then
            if (is_syntax_table_live(h)) return
        end if

        h = syntax_table()
    end function parser_state_syntax_table

    ! ------------------------------------------------------------------
    ! Applying syntax-table properties where needed
    ! ------------------------------------------------------------------

    !> Like `line_beginning_position' but obeying `syntax_wholeline_maximum'.
    !> Port of syntax.el:syntax--lbp.
    integer(int64) function syntax_lbp(argument) result(res)
        integer(int64), intent(in), optional :: argument
        integer(int64) :: position, max

        position = point()
        res = line_beginning_position(argument)
        if (abs(position - res) < syntax_wholeline_maximum) return
        ! For lines that are too long, round to the nearest multiple of
        ! `syntax_wholeline_maximum'.  We use rounding rather than just
        ! (min res (+ pos syntax-wholeline-max)) so that repeated calls
        ! to `syntax_propertize_wholelines' don't keep growing the bounds,
        ! i.e. it really behaves like additional line-breaks.
        max = syntax_wholeline_maximum

        if (res < position) then
            res = maxval([point_minimum(), max*(position/max)])
        else
            res = minval([point_maximum(), max*((position + max - 1)/max)])
        end if
    end function syntax_lbp

    !> Extend the region delimited by BEGINNING and END to whole lines (in
    !> NEW_BEGINNING and NEW_END).
    !> This function is useful for
    !> `syntax_propertize_extend_region_functions';
    !> see Info node `(elisp) Syntax Properties'.  Port of
    !> syntax.el:syntax-propertize-wholelines.
    subroutine syntax_propertize_wholelines(beginning, end, new_beginning, new_end, status)
        integer(int64), intent(in) :: beginning, end
        integer(int64), intent(out) :: new_beginning, new_end
        type(status_type), intent(out) :: status
        logical :: saved

        status = status_type()
        saved = inhibit_field_text_motion
        inhibit_field_text_motion = .true.
        call goto_character(beginning)
        new_beginning = beginning
        if (.not. is_beginning_of_line()) new_beginning = syntax_lbp()
        call goto_character(end)
        new_end = end
        if (.not. is_beginning_of_line()) new_end = syntax_lbp(2_int64)
        inhibit_field_text_motion = saved
    end subroutine syntax_propertize_wholelines

    !> Let `syntax_propertize' pay attention to the syntax_multiline
    !> property.  Port of syntax.el:syntax-propertize-multiline.
    subroutine syntax_propertize_multiline(beginning, end, new_beginning, new_end, status)
        integer(int64), intent(in) :: beginning, end
        integer(int64), intent(out) :: new_beginning, new_end
        type(status_type), intent(out) :: status
        type(value_type) :: value
        integer(int64) :: position
        logical :: found

        new_beginning = beginning
        new_end = end
        status = status_type()

        if (beginning > point_minimum()) then
            call get_text_property(beginning - 1, value_type(name="syntax_multiline"), value, status)
            if (.not. status%ok()) return

            if (allocated(value%a)) then
                call previous_single_property_change(beginning, value_type(name="syntax_multiline"), position, found, status)
                if (.not. status%ok()) return
                new_beginning = point_minimum()
                if (found) new_beginning = position
            end if
        end if

        call get_text_property(end, value_type(name="syntax_multiline"), value, status)
        if (.not. status%ok()) return

        if (allocated(value%a)) then
            call text_property_any(end, point_maximum(), value_type(name="syntax_multiline"), value_type(), position, found, status)
            if (.not. status%ok()) return
            new_end = point_maximum()
            if (found) new_end = position
        end if
    end subroutine syntax_propertize_multiline

    !> REGULAR_EXPRESSION with its explicitly numbered groups and its back-references
    !> shifted by N.  Port of
    !> syntax.el:syntax-propertize--shift-groups-and-backrefs.
    recursive function syntax_propertize_shift_groups_and_backrefs(regular_expression, n, status) result(new_regular_expression)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: new_regular_expression
        integer(int64) :: position, index, mb, me, mb0
        integer :: number, ios
        logical :: context
        character(len=12) :: buffer
        character(len=:), allocatable :: digits

        new_regular_expression = ""
        ! (replace-regexp-in-string "\\\\(\\?\\([0-9]+\\):" ... re t t)
        position = 1

        do
            index = string_match("\\(\?\([0-9]+\):", regular_expression, status, position)
            if (.not. status%ok()) return
            if (index == 0) exit
            mb = match_beginning(1, status)
            me = match_end(1, status)
            digits = chars(regular_expression, mb, me)
            read (digits, *, iostat=ios) number
            if (ios /= 0) number = 0
            write (buffer, "(i0)") n + number
            new_regular_expression = new_regular_expression//chars(regular_expression, position, mb)//trim(buffer)
            position = me
        end do

        new_regular_expression = new_regular_expression//chars(regular_expression, position, &
                                                               count_characters(regular_expression) + 1)
        position = 1

        do
            index = string_match("\\\([0-9]+\)", new_regular_expression, status, position)
            if (.not. status%ok()) return
            if (index == 0) exit
            mb0 = match_beginning(0, status)
            mb = match_beginning(1, status)
            me = match_end(1, status)
            position = 1 + mb
            ! With \N, the \ must be in a subregexp context, i.e.,
            ! not in a character class or in a \{\} repetition.
            context = is_subregexp_context(new_regular_expression, mb0, status)
            if (.not. status%ok()) return

            if (context) then
                digits = chars(new_regular_expression, mb, me)
                read (digits, *, iostat=ios) number
                if (ios /= 0) number = 0
                number = n + number

                if (number > 9) then
                    call set_error(status, GENERIC_ERROR_CODE, "There may be at most nine back-references")
                    return
                end if

                write (buffer, "(i0)") number
                new_regular_expression = chars(new_regular_expression, 1_int64, mb)//trim(buffer)//chars(new_regular_expression, &
                                                                                   me, count_characters(new_regular_expression) + 1)
            end if
        end do
    end function syntax_propertize_shift_groups_and_backrefs

    ! The characters of S from index B (1-based) up to before index E.
    ! Not a port.
    pure function chars(s, b, e) result(sub)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: b, e
        character(len=:), allocatable :: sub

        sub = s(string_index_to_offset(s, b) + 1:string_index_to_offset(s, e))
    end function chars

    ! Build the regexp and the branches of the rules, as
    ! syntax-propertize-rules does at macro-expansion time.
    ! Port of part of syntax.el:syntax-propertize-rules.
    recursive subroutine rules_compile(self, status)
        class(syntax_propertize_rules_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: saved(:)
        character(len=:), allocatable :: pattern
        integer :: offset, local_offset, i, j, nrules, nh, depth
        logical :: has0, nocode

        status = status_type()
        saved = match_data()
        nrules = 0
        if (allocated(self%rules)) nrules = size(self%rules)
        if (allocated(self%branches)) deallocate (self%branches)
        allocate (self%branches(nrules))
        self%regular_expression = ""
        offset = 0

        do i = 1, nrules
            associate (rule => self%rules(i), branch => self%branches(i))
                nh = 0
                if (allocated(rule%highlights)) nh = size(rule%highlights)
                pattern = rule%regular_expression
                has0 = .false.

                do j = 1, nh
                    if (rule%highlights(j)%group == 0) has0 = .true.
                end do

                if (has0 .and. nrules > 1) then
                    ! If there's more than 1 rule, and the rule want to apply
                    ! highlight to match 0, create an extra group to be able to
                    ! tell when *this* match 0 has succeeded.
                    offset = offset + 1
                    pattern = "\("//pattern//"\)"
                end if

                pattern = syntax_propertize_shift_groups_and_backrefs(pattern, offset, status)
                if (.not. status%ok()) exit
                branch%always = .false.

                if (has0) then
                    if (offset == 0) then
                        branch%always = .true.
                        allocate (branch%groups(0))
                    else
                        branch%groups = [offset]
                    end if
                else
                    allocate (branch%groups(nh))

                    do j = 1, nh
                        branch%groups(j) = offset + rule%highlights(j)%group
                    end do
                end if

                local_offset = offset
                nocode = .true.
                branch%shift = 0

                ! If some of the subgroup rules include code, then we
                ! need to set the match-data so it's consistent with what the
                ! code expects.  If not, then we can simply use shifted
                ! offset in our own code.
                if (offset /= 0) then
                    do j = 1, nh
                        if (.not. allocated(rule%highlights(j)%syntax) .or. &
                            associated(rule%highlights(j)%after)) nocode = .false.
                    end do

                    if (.not. nocode) then
                        ! Keep match 0 as is, but shift everything else.
                        branch%shift = offset
                        local_offset = 0
                    end if
                end if

                ! Now construct the code for each subgroup rules.
                allocate (branch%steps(nh))

                do j = 1, nh
                    branch%steps(j)%gn = local_offset + rule%highlights(j)%group
                    branch%steps(j)%highlight = rule%highlights(j)
                    branch%steps(j)%conditional = nh > 1 .and. branch%steps(j)%gn /= 0

                    if (allocated(rule%highlights(j)%syntax)) then
                        call string_to_syntax(rule%highlights(j)%syntax, branch%steps(j)%descriptor, status)
                        if (.not. status%ok()) exit
                    end if
                end do

                if (.not. status%ok()) exit
                depth = regular_expression_optimization_depth(rule%regular_expression, status)
                if (.not. status%ok()) exit
                offset = offset + depth
                if (i > 1) self%regular_expression = self%regular_expression//"\|"
                self%regular_expression = self%regular_expression//pattern
            end associate
        end do

        call set_match_data(saved)
        if (status%ok()) self%compiled = .true.
    end subroutine rules_compile

    !> Apply the rules from START to END: the function
    !> syntax.el:syntax-propertize-rules makes.
    !> Port of part of syntax.el:syntax-propertize-rules.
    recursive subroutine rules_apply(self, start, end, status)
        class(syntax_propertize_rules_type), intent(inout) :: self
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: found
        integer :: i, j
        logical :: matched

        status = status_type()

        if (.not. self%compiled) then
            call rules_compile(self, status)
            if (.not. status%ok()) return
        end if

        call goto_character(start)

        do while (point() < end)
            call regular_expression_search_forward(self%regular_expression, found, status, bound=end, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit

            do i = 1, size(self%branches)
                associate (branch => self%branches(i))
                    matched = branch%always

                    do j = 1, size(branch%groups)
                        if (matched) exit
                        matched = group_matched(branch%groups(j))
                    end do

                    if (.not. matched) cycle
                    if (branch%shift > 0) call shift_match_data(branch%shift)

                    do j = 1, size(branch%steps)
                        if (branch%steps(j)%conditional) then
                            if (.not. group_matched(branch%steps(j)%gn)) cycle
                        end if

                        call execute_step(branch%steps(j), status)
                        if (.not. status%ok()) return
                    end do
                end associate

                exit
            end do
        end do
    end subroutine rules_apply

    ! Whether group N of the last match matched: (match-beginning N).
    ! Not a port.
    logical function group_matched(n)
        integer, intent(in) :: n
        type(status_type) :: st

        group_matched = match_beginning(n, st) > 0
    end function group_matched

    ! Keep match 0 as is, but drop the first N groups.  Not a port.
    subroutine shift_match_data(n)
        integer, intent(in) :: n
        integer(int64), allocatable :: md(:), shifted(:)
        integer :: k

        md = match_data()
        k = 2*(n + 1)

        if (size(md) > k) then
            allocate (shifted(2 + size(md) - k))
            shifted(1:2) = md(1:2)
            shifted(3:) = md(k + 1:)
        else
            shifted = md(1:min(2, size(md)))
        end if

        call set_match_data(shifted)
    end subroutine shift_match_data

    ! Execute one highlight of a matched rule.
    ! Port of part of syntax.el:syntax-propertize-rules.
    recursive subroutine execute_step(step, status)
        type(rule_step_type), intent(in) :: step
        type(status_type), intent(out) :: status
        type(syntax_property_type) :: property
        integer(int64) :: mb, me

        status = status_type()

        if (allocated(step%highlight%syntax)) then
            mb = match_beginning(step%gn, status)
            me = match_end(step%gn, status)
            call put_text_property(mb, me, value_type(name="syntax_table"), syntax_descriptor_value(step%descriptor), status)
            if (.not. status%ok()) return
        else if (associated(step%highlight%value)) then
            mb = match_beginning(step%gn, status)
            me = match_end(step%gn, status)
            call step%highlight%value(property, status)
            if (.not. status%ok()) return

            if (property%apply) then
                call put_property(mb, me, property, status)
                if (.not. status%ok()) return
            end if
        end if

        if (associated(step%highlight%after)) call step%highlight%after(status)
    end subroutine execute_step

    ! Give START..END the `syntax_table' property PROPERTY.  Not a port.
    subroutine put_property(start, end, property, status)
        integer(int64), intent(in) :: start, end
        type(syntax_property_type), intent(in) :: property
        type(status_type), intent(out) :: status

        if (property%is_table) then
            call put_text_property(start, end, value_type(name="syntax_table"), make_syntax_table_reference(property%table), status)
        else
            call put_text_property(start, end, value_type(name="syntax_table"), syntax_descriptor_value(property%descriptor), &
                                   status)
        end if
    end subroutine put_property

    !> True if we're inside `syntax_propertize'.
    !> This is used to avoid infinite recursion as well as to handle cases where
    !> `syntax_parser_state' is called when the final `syntax_table' properties have not
    !> yet been setup, in which case we may end up putting invalid info into the cache.
    !> It's also used so that `syntax_parser_state_flush_cache' can be used from within
    !> `syntax_propertize' without ruining the `syntax_table' already set.
    !> Port of syntax.el:syntax-propertize--in-process-p.
    logical function is_syntax_propertize_in_process()
        is_syntax_propertize_in_process = syntax_propertize_done%value == MOST_POSITIVE_FIXNUM
    end function is_syntax_propertize_in_process

    !> Ensure that syntax-table properties are set until POSITION (a buffer point).
    !> Port of syntax.el:syntax-propertize.
    recursive subroutine syntax_propertize(position, status)
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        procedure(syntax_propertize_procedure), pointer :: fn
        type(region_entry_type), allocatable :: functions(:)
        type(silent_modifications_type) :: silent
        type(handle_type) :: here, old_table
        type(status_type) :: st
        integer(int64) :: start, end, new_beginning, new_end, saved_done, p
        integer :: i, k
        logical :: first, repeat, saved_updated, changed
        character(len=24) :: b1, b2

        status = status_type()
        if (.not. (syntax_propertize_done%value < position)) return
        fn => syntax_propertize_function%get_current()

        if (.not. associated(fn)) then
            call syntax_propertize_done%set_local(max(point_maximum(), position))
            return
        end if

        call parse_expression_lookup_properties%set_local(.true.)

        if (syntax_propertize_done%value < point_minimum()) then
            if (syntax_propertize_done%value > 0) then
                write (b1, "(i0)") syntax_propertize_done%value
                write (b2, "(i0)") point_minimum()
                call message("Cannot syntax-propertize "//trim(b1)//".."//trim(b2)//" because of narrowing!")
            end if

            ! *Usually* syntax-propertize is called via syntax-ppss which
            ! takes care of adding syntax-ppss-flush-cache to b-c-f, but this
            ! is not *always* the case, so since we share a single "flush" function
            ! between syntax-ppss and syntax-propertize, we also have to make
            ! sure the flush function is installed here (bug#29767).
            call before_change_functions%append(flush_cache_hook, depth=99, local=.true.)
        end if

        call copy_marker_at(point(), here, .false., st)
        call silent_modifications_begin(silent)
        old_table = syntax_table()
        call set_syntax_table(parser_state_syntax_table(), st)
        ! Just in case!
        call syntax_propertize_done%make_local()
        ! Make sure we save and restore it only buffer-locally.
        call make_data_entry(k)
        saved_updated = data(k)%updated_cache
        data(k)%updated_cache = .false.
        start = max(min(syntax_propertize_done%value, point_maximum()), point_minimum())
        end = max(position, min(point_maximum(), start + syntax_propertize_chunk_size))
        first = .true.
        repeat = .true.

        body: block
            do while (repeat)
                repeat = .false.
                functions = syntax_propertize_extend_region_functions%functions()

                do i = 1, size(functions)
                    ! Bind `syntax_propertize_done' to avoid recursion!
                    saved_done = syntax_propertize_done%value
                    call syntax_propertize_done%set_local(MOST_POSITIVE_FIXNUM)
                    call functions(i)%fn(start, end, new_beginning, new_end, status)
                    call syntax_propertize_done%set_local(saved_done)
                    if (.not. status%ok()) exit body
                    changed = new_beginning /= start .or. new_end /= end

                    if (changed .and. .not. (new_beginning >= start .and. new_end <= end)) then
                        start = new_beginning
                        end = new_end
                        ! If there's been a change, we should go through the
                        ! list again since this new position may
                        ! warrant a different answer from one of the funs we've
                        ! already seen.
                        if (.not. first) repeat = .true.
                    end if

                    first = .false.
                end do
            end do

            ! Flush ppss cache between the original value of `start' and that
            ! set above by syntax-propertize-extend-region-functions.
            call syntax_parser_state_flush_cache(start)
            ! Move the limit before calling the function, so it's
            ! done in case of errors.
            call syntax_propertize_done%set_local(end)
            call remove_text_properties(start, end, list(value_type(name="syntax_table"), value_type(), &
                                                         value_type(name="syntax_multiline"), value_type()), &
                                        changed, status)
            if (.not. status%ok()) exit body
            ! Bind `syntax_propertize_done' to avoid recursion!
            saved_done = syntax_propertize_done%value
            call syntax_propertize_done%set_local(MOST_POSITIVE_FIXNUM)
            call fn(start, end, status)

            if (status%ok()) then
                call make_data_entry(k)
                ! `syntax_parser_state' was called and updated the cache while we
                ! were propertizing so we need to flush the part of the
                ! cache that may have been rendered out-of-date by the new
                ! properties.
                if (data(k)%updated_cache) call syntax_parser_state_flush_cache(start)
            end if

            call syntax_propertize_done%set_local(saved_done)
        end block body

        call make_data_entry(k)
        data(k)%updated_cache = saved_updated
        call set_syntax_table(old_table, st)
        call silent_modifications_end(silent)
        call marker_position(here, p, st)
        call goto_character(p)
        call destroy_marker(here, st)
    end subroutine syntax_propertize

    !> Propertize far enough beyond CHARACTER_POSITION for the syntax scanner (called
    !> from syntax_mod's parse_expression_propertize; its errors are ignored, as
    !> upstream's safe_calln does).  Port of
    !> syntax.el:internal--syntax-propertize.
    recursive subroutine internal_syntax_propertize_function(character_position)
        integer(int64), intent(in) :: character_position
        integer(int64), allocatable :: saved(:)
        type(status_type) :: status

        ! save-match-data
        saved = match_data()
        call syntax_propertize(min(syntax_propertize_chunks + character_position, point_maximum()), status)
        call set_match_data(saved)
    end subroutine internal_syntax_propertize_function

    ! ------------------------------------------------------------------
    ! Incrementally compute and memoize parser state
    ! ------------------------------------------------------------------

    !> The depth in parens of PARSER_STATE.  Port of syntax.el:syntax-ppss-depth.
    integer(int64) function syntax_parser_state_depth(parser_state)
        type(parse_state_type), intent(in) :: parser_state

        syntax_parser_state_depth = parser_state%depth
    end function syntax_parser_state_depth

    !> Get the latest syntactically outermost position found in a syntactic scan.
    !> PARSER_STATE is a scan state, as returned by `parse_partial_expression' or `syntax_parser_state'.
    !> An "outermost position" means one that it is outside of any syntactic entity:
    !> outside of any parentheses, comments, or strings encountered in the scan.
    !> If no such position is recorded in PARSER_STATE (because the end of the scan was
    !> itself at the outermost level), return 0.  Port of
    !> syntax.el:syntax-ppss-toplevel-pos.
    integer(int64) function syntax_parser_state_toplevel_position(parser_state) result(position)
        type(parse_state_type), intent(in) :: parser_state

        position = 0

        if (allocated(parser_state%open_parentheses)) then
            if (size(parser_state%open_parentheses) > 0) then
                position = parser_state%open_parentheses(1)
                return
            end if
        end if

        position = parser_state%comstr_start
    end function syntax_parser_state_toplevel_position

    !> Say whether PARSER_STATE is a string, a comment, or something else.
    !> If PARSER_STATE is a string, `SYNTAX_PARSER_STATE_STRING' is returned.  If it's a
    !> comment, `SYNTAX_PARSER_STATE_COMMENT' is returned.  If it's something
    !> else, `SYNTAX_PARSER_STATE_NONE' is returned.  Port of
    !> syntax.el:syntax-ppss-context.
    integer function syntax_parser_state_context(parser_state) result(context)
        type(parse_state_type), intent(in) :: parser_state

        context = SYNTAX_PARSER_STATE_NONE

        if (parser_state%instring /= -1) then
            context = SYNTAX_PARSER_STATE_STRING
        else if (parser_state%incomment /= 0) then
            context = SYNTAX_PARSER_STATE_COMMENT
        end if
    end function syntax_parser_state_context

    ! `syntax_parser_state_flush_cache' as a `before_change_functions' function.
    ! Not a port.
    recursive subroutine flush_cache_hook(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        call syntax_parser_state_flush_cache(beginning)
        if (end < beginning) continue
    end subroutine flush_cache_hook

    !> Flush the cache of `syntax_parser_state' starting at position BEGINNING.  Port of
    !> syntax.el:syntax-ppss-flush-cache.
    recursive subroutine syntax_parser_state_flush_cache(beginning)
        integer(int64), intent(in) :: beginning
        integer :: k

        if (current == 0) return

        ! Set syntax-propertize to refontify anything past beginning.
        if (.not. is_syntax_propertize_in_process()) then
            call syntax_propertize_done%set_local(min(beginning, syntax_propertize_done%value))
        end if

        ! Flush invalid cache entries.
        call make_data_entry(k)
        call flush_cell(data(k)%wide)
        call flush_cell(data(k)%narrow)

    contains

        subroutine flush_cell(cell)
            type(parser_state_cell_type), intent(inout) :: cell
            integer :: n, drop
            integer(int64) :: limit

            if (.not. cell%exists) return
            n = 0
            if (allocated(cell%cache_position)) n = size(cell%cache_position)
            drop = 0

            do while (drop < n)
                if (cell%cache_position(drop + 1) <= beginning) exit
                drop = drop + 1
            end do

            if (drop > 0) call remove_entries(cell, drop)

            ! Throw away `last' value if made invalid.
            if (cell%has_last) then
                if (beginning < cell%last_position) then
                    ! If syntax-begin-function jumped to BEGINNING, then the old state at BEGINNING can
                    ! depend on the text after BEGINNING (which is presumably changed).  So if
                    ! BEGINNING=(car (nth 10 syntax-ppss-last)) don't reuse that data because the
                    ! assumed nil state at BEGINNING may not be valid any more.
                    limit = syntax_parser_state_toplevel_position(cell%last_parser_state)
                    if (limit == 0) limit = cell%last_parser_state%last_expression_start

                    if (beginning <= limit) then
                        cell%has_last = .false.
                    else
                        cell%last_position = 0
                    end if
                end if
            end if
        end subroutine flush_cell
    end subroutine syntax_parser_state_flush_cache

    ! Drop the first N entries of CELL's cache.  Not a port.
    subroutine remove_entries(cell, n)
        type(parser_state_cell_type), intent(inout) :: cell
        integer, intent(in) :: n
        integer(int64), allocatable :: position(:)
        type(parse_state_type), allocatable :: parser_state(:)
        integer :: i, m

        m = size(cell%cache_position) - n
        allocate (position(m), parser_state(m))

        do i = 1, m
            position(i) = cell%cache_position(n + i)
            parser_state(i) = cell%cache_parser_state(n + i)
        end do

        call move_alloc(position, cell%cache_position)
        call move_alloc(parser_state, cell%cache_parser_state)
    end subroutine remove_entries

    ! Insert (POSITION . PARSER_STATE) into CELL's cache before entry AT.  Not a port.
    subroutine insert_entry(cell, at, position, parser_state)
        type(parser_state_cell_type), intent(inout) :: cell
        integer, intent(in) :: at
        integer(int64), intent(in) :: position
        type(parse_state_type), intent(in) :: parser_state
        integer(int64), allocatable :: new_position(:)
        type(parse_state_type), allocatable :: new_parser_state(:)
        integer :: i, n

        n = 0
        if (allocated(cell%cache_position)) n = size(cell%cache_position)
        allocate (new_position(n + 1), new_parser_state(n + 1))

        do i = 1, at - 1
            new_position(i) = cell%cache_position(i)
            new_parser_state(i) = cell%cache_parser_state(i)
        end do

        new_position(at) = position
        new_parser_state(at) = parser_state

        do i = at, n
            new_position(i + 1) = cell%cache_position(i)
            new_parser_state(i + 1) = cell%cache_parser_state(i)
        end do

        call move_alloc(new_position, cell%cache_position)
        call move_alloc(new_parser_state, cell%cache_parser_state)
    end subroutine insert_entry

    ! Port of syntax.el:syntax-ppss--update-stats.
    subroutine update_statistics(i, old, new)
        integer, intent(in) :: i
        integer(int64), intent(in) :: old, new

        statistics_count(i) = statistics_count(i) + 1
        statistics_total(i) = statistics_total(i) + (new - old)
    end subroutine update_statistics

    ! The cell of the current buffer's restriction (syntax-ppss--data),
    ! in `data(k)'.  Port of syntax.el:syntax-ppss--data.
    subroutine parser_state_data(k, wide)
        integer, intent(out) :: k
        logical, intent(out) :: wide

        call make_data_entry(k)
        wide = point_minimum() == 1

        if (wide) then
            data(k)%wide%exists = .true.
        else
            if (data(k)%narrow_start /= point_minimum()) then
                data(k)%narrow_start = point_minimum()
                data(k)%narrow = parser_state_cell_type(exists=.true.)
            end if
        end if
    end subroutine parser_state_data

    !> Parse-Partial-Sexp State at POSITION, defaulting to point.
    !> If POSITION is given, this function moves point to POSITION.
    !>
    !> The returned value is the same as that of `parse_partial_expression'
    !> run from `point_minimum' to POSITION except that values at positions 2 and 6
    !> in the returned list (counting from 0) cannot be relied upon.
    !>
    !> It is necessary to call `syntax_parser_state_flush_cache' explicitly if
    !> this function is called while `before_change_functions' is
    !> temporarily saved and changed, or if the buffer is modified without
    !> running the hook.  Port of syntax.el:syntax-ppss.
    recursive subroutine syntax_parser_state_cached(state, position)
        type(parse_state_type), intent(out) :: state
        integer(int64), intent(in), optional :: position
        type(parser_state_cell_type) :: cell
        type(parse_state_type) :: old_parser_state, parser_state, parser_state_best
        type(handle_type) :: old_table
        type(status_type) :: status, st
        type(value_type) :: face
        integer(int64) :: p, old_position, point_minimum_value, point_best, new_minimum
        integer :: k, index, cache_test, n
        logical :: wide, has_old_parser_state, has_parser_state, has_best, use_begin

        ! Default values.
        p = point()
        if (present(position)) p = position
        call syntax_propertize(p, st)
        old_table = syntax_table()
        call set_syntax_table(parser_state_syntax_table(), st)
        call parser_state_data(k, wide)

        if (wide) then
            cell = data(k)%wide
        else
            cell = data(k)%narrow
        end if

        has_old_parser_state = cell%has_last
        if (has_old_parser_state) old_parser_state = cell%last_parser_state
        old_position = 0
        if (cell%has_last) old_position = cell%last_position
        if (old_position /= 0 .and. old_position > p) old_position = 0
        n = 0
        if (allocated(cell%cache_position)) n = size(cell%cache_position)

        body: block
            ! Use the OLD-POS if usable and close.  Don't update the `last' cache.
            if (old_position /= 0) then
                if (p - old_position < (2*statistics_total(5))/statistics_count(5)) then
                    call update_statistics(0, old_position, p)
                    call parse_partial_expression(old_position, p, state, status, oldstate=old_parser_state)
                    if (.not. status%ok()) exit body
                    call set_syntax_table(old_table, st)
                    return
                end if
            end if

            ! Use OLD-PPSS if possible and close enough.
            point_minimum_value = 0

            if (old_position == 0 .and. has_old_parser_state) then
                ! If `pt-min' is too far from `position', we could try to use
                ! other positions in (nth 9 old-ppss), but that doesn't
                ! seem to happen in practice and it would complicate this
                ! code (and the before-change-function code even more).
                ! But maybe it would be useful in "degenerate" cases such
                ! as when the whole file is wrapped in a set
                ! of parentheses.
                point_minimum_value = syntax_parser_state_toplevel_position(old_parser_state)
                if (point_minimum_value == 0) point_minimum_value = old_parser_state%last_expression_start
            end if

            if (point_minimum_value /= 0 .and. point_minimum_value <= p .and. p &
                - point_minimum_value < syntax_parser_state_maximum_span) then
                call update_statistics(1, point_minimum_value, p)
                call parse_partial_expression(point_minimum_value, p, parser_state, status)
                if (.not. status%ok()) exit body
            else
                ! The OLD-* data can't be used.  Consult the cache.
                cache_test = 0
                point_minimum_value = point_minimum()
                ! I differentiate between PT-MIN and PT-BEST because
                ! I feel like it might be important to ensure that the
                ! cache is only filled with 100% sure data (whereas
                ! syntax-begin-function might return incorrect data).
                ! Maybe that's just stupid.
                point_best = point_minimum()
                has_best = .false.
                has_parser_state = .false.
                ! look for a usable cache entry.
                index = 1

                do while (index <= n)
                    if (.not. p < cell%cache_position(index)) exit
                    cache_test = index
                    index = index + 1
                end do

                if (index <= n) then
                    point_minimum_value = cell%cache_position(index)
                    parser_state = cell%cache_parser_state(index)
                    has_parser_state = .true.
                end if

                ! Setup the before-change function if necessary.
                if (n == 0 .and. .not. cell%has_last) then
                    ! Note: combine-change-calls-1 needs to be kept in sync
                    ! with this!
                    ! We should be either the very last function on
                    ! before-change-functions or the very first on
                    ! after-change-functions.
                    call before_change_functions%append(flush_cache_hook, depth=99, local=.true.)
                end if

                ! Use the best of OLD-POS and CACHE.
                if (old_position == 0 .or. old_position < point_minimum_value) then
                    point_best = point_minimum_value
                    has_best = has_parser_state
                    if (has_parser_state) parser_state_best = parser_state
                else
                    call update_statistics(4, old_position, p)
                    point_best = old_position
                    has_best = has_old_parser_state
                    if (has_best) parser_state_best = old_parser_state
                end if

                ! Use the `syntax-begin-function' if available.
                ! We could try using that function earlier, but:
                ! - The result might not be 100% reliable, so it's better to use
                !   the cache if available.
                ! - The function might be slow.
                ! - If this function almost always finds a safe nearby spot,
                !   the cache won't be populated, so consulting it is cheap.
                if (associated(syntax_begin_function)) then
                    call goto_character(p)
                    call syntax_begin_function()
                    ! Make sure it's better.
                    use_begin = point() > point_best
                    ! Simple sanity checks.
                    ! backward-paragraph can fail here.
                    if (use_begin) use_begin = point() < p

                    if (use_begin) then
                        call get_text_property(point(), value_type(name="face"), face, st)

                        if ((vtag(face) == V_NAME .or. vtag(face) == V_NULL)) then
                            if (value_eq(face, value_type(name="font_lock_string_face")) .or. &
                                value_eq(face, value_type(name="font_lock_doc_face")) .or. &
                                value_eq(face, value_type(name="font_lock_comment_face"))) use_begin = .false.
                        end if
                    end if

                    if (use_begin) then
                        call update_statistics(5, point(), p)
                        point_best = point()
                        has_best = .false.
                    end if
                end if

                if (p - point_best < syntax_parser_state_maximum_span) then
                    ! Quick case when we found a nearby pos.
                    call update_statistics(2, point_best, p)

                    if (has_best) then
                        call parse_partial_expression(point_best, p, parser_state, status, oldstate=parser_state_best)
                    else
                        call parse_partial_expression(point_best, p, parser_state, status)
                    end if

                    if (.not. status%ok()) exit body
                else
                    ! Slow case: compute the state from some known position and
                    ! populate the cache so we won't need to do it again soon.
                    call update_statistics(3, point_minimum_value, p)
                    call make_data_entry(k)
                    data(k)%updated_cache = .true.

                    ! If `pt-min' is too far, add a few intermediate entries.
                    do while (p - point_minimum_value > 2*syntax_parser_state_maximum_span)
                        new_minimum = (point_minimum_value + p)/2
                        call parse_parser_state(point_minimum_value, new_minimum)
                        if (.not. status%ok()) exit body
                        point_minimum_value = new_minimum
                        call insert_entry(cell, cache_test + 1, point_minimum_value, parser_state)
                    end do

                    ! Compute the actual return value.
                    call parse_parser_state(point_minimum_value, p)
                    if (.not. status%ok()) exit body
                    ! Store it in the cache.
                    n = 0
                    if (allocated(cell%cache_position)) n = size(cell%cache_position)

                    if (cache_test > 0) then
                        if (cell%cache_position(cache_test) - p > syntax_parser_state_maximum_span) then
                            call insert_entry(cell, cache_test + 1, p, parser_state)
                        else
                            cell%cache_position(cache_test) = p
                            cell%cache_parser_state(cache_test) = parser_state
                        end if
                    else
                        if (n == 0) then
                            call insert_entry(cell, 1, p, parser_state)
                        else if (cell%cache_position(1) - p > syntax_parser_state_maximum_span) then
                            call insert_entry(cell, 1, p, parser_state)
                        else
                            cell%cache_position(1) = p
                            cell%cache_parser_state(1) = parser_state
                        end if
                    end if
                end if
            end if

            call make_data_entry(k)
            data(k)%updated_cache = .true.
            cell%has_last = .true.
            cell%last_position = p
            cell%last_parser_state = parser_state
            cell%exists = .true.

            if (wide) then
                data(k)%wide = cell
            else
                data(k)%narrow = cell
            end if

            state = parser_state
            call set_syntax_table(old_table, st)
            return
        end block body

        ! If the buffer is more narrowed than when we built the cache,
        ! we may end up calling parse-partial-sexp with a position before
        ! point-min.  In that case, just parse from point-min assuming
        ! a nil state.
        if (status%code == ARGUMENTS_OUT_OF_RANGE_CODE) call parse_partial_expression(point_minimum(), p, state, status)
        call set_syntax_table(old_table, st)

    contains

        ! (parse-partial-sexp FROM TO nil nil ppss) into ppss.
        recursive subroutine parse_parser_state(from, to)
            integer(int64), intent(in) :: from, to
            type(parse_state_type) :: next

            if (has_parser_state) then
                call parse_partial_expression(from, to, next, status, oldstate=parser_state)
            else
                call parse_partial_expression(from, to, next, status)
            end if

            parser_state = next
            has_parser_state = .true.
        end subroutine parse_parser_state
    end subroutine syntax_parser_state_cached

    !> Give the variables their defaults and hook syntax-propertize and
    !> the cache into syntax_mod.  Not a port.
    subroutine initialize_syntax_propertize()
        call syntax_propertize_extend_region_functions%prepend(syntax_propertize_wholelines)
        call append_locals_clearer(clear_syntax_locals)
        internal_syntax_propertize => internal_syntax_propertize_function
        syntax_parser_state_function => syntax_parser_state_cached
    end subroutine initialize_syntax_propertize
end module syntax_propertize_mod
