! editing_functions_mod.f90 -- point, narrowing, and reading and editing buffer text.
! Ported from GNU Emacs 31.1 src/editfns.c.
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

!> Editing primitives on the current buffer.
!>
!> Upstream functions that return no position for "no such position" take a `found'
!> (or similar) logical here.  Text comes back as byte strings in the
!> buffer's representation.
!>
!> `goto_char' sets point with `set_point_both' (intervals.c, in text_properties_mod:
!> `intangible', `point_entered' and `point_left' when
!> `inhibit_point_motion_hooks' is false); the other primitives that use
!> SET_PT upstream still set it with `temp_set_point_both' (the same while
!> that variable is true, its default).  What is not ported yet is listed
!> in docs/status/editing_functions_mod.md.
module editing_functions_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use value_mod, only: vtag, V_NULL, V_INTEGER, V_NAME, V_STRING, value_type, xstring, xfixnum, string_intervals
    use error_mod, only: status_type, set_error, FM_ARGS_OUT_OF_RANGE, &
                         FM_WRONG_TYPE_ARGUMENT, FM_GENERIC_ERROR, FM_MARK_INACTIVE, FM_OVERFLOW_ERROR
    use handle_mod, only: handle_type
    use character_mod, only: is_char_head, is_char_valid, char_string, string_char, is_single_byte_char, unibyte_to_char, &
                             multibyte_chars_in_text, MAX_MULTIBYTE_LENGTH, string_char_and_length, bytes_by_char_head, &
                             char_to_byte8, MAX_CHAR, glyph_width, char_hexdigit
    use buffer_mod, only: buffers, current, BEGINNING, BEGINNING_BYTE, BUF_BYTES_MAX, &
                          buf_fetch_byte, buf_fetch_char, buf_prev_char_len, buf_next_char_len, buffer_byte_index, &
                          buf_bytes, temp_set_point_both, current_buffer, &
                          resolve_buffer, transient_mark_mode, set_buffer, is_buffer_live
    use marker_mod, only: char_to_byte, byte_to_char, buf_charpos_to_bytepos, &
                          markers, resolve_marker, make_marker, set_marker_both, marker_position, marker_byte_position, &
                          destroy_marker, marker_buffer, copy_marker_at, set_marker_insertion_type, clear_charpos_cache
    use execute_interactively_mod, only: mark_even_if_inactive
    use insert_delete_mod, only: insert, insert_before_markers, insert_and_inherit, insert_before_markers_and_inherit, &
                                 insert_from_string, insert_from_string_before_markers, del_range, del_range_1, modify_text, &
                                 signal_after_change, move_gap_both, adjust_markers_bytepos, replace_range
    use intervals_mod, only: copy_intervals_to_string, copy_intervals, graft_intervals_into_buffer
    use case_table_mod, only: downcase, case_canon
    use undo_mod, only: record_change
    use window_mod, only: window_type, window_ref, frame_selected_window, window_buffer, set_window_point, is_window_live, &
                          same_handle
    use character_table_mod, only: char_table_subtype, char_table_ref
    use search_mod, only: scan_newline_from_point, find_before_next_newline, find_newline, case_fold_search
    use value_mod, only: value_type
    use data_mod, only: value_eq
    use text_properties_mod, only: get_text_property, text_property_stickiness, &
                                   next_single_char_property_change, previous_single_char_property_change, set_point_both, &
                                   string_add_text_properties, extend_property_ranges, text_property_range_type
    use print_mod, only: readable_text, float_to_string, sprintf_float
    use error_mod, only: format_message
    implicit none(type, external)
    private

    public :: point, point_min, point_max, buffer_size, buffer_size_of, &
              gap_position, gap_size
    public :: goto_char, goto_char_marker, set_point_from_marker
    public :: point_marker, point_min_marker, point_max_marker
    public :: mark_marker, mark_position, region_beginning, region_end, interactive_region, check_mark, region_position_procedure
    public :: position_bytes, byte_to_position
    public :: following_char, preceding_char, is_bob, is_eob, is_bol, is_eol, &
              char_after, char_before
    public :: insert, insert_before_markers, insert_and_inherit, insert_before_markers_and_inherit
    public :: insert_char_n, narrow_to_region, widen, is_buffer_narrowed, validate_region, subst_char_in_region
    public :: save_excursion_begin, save_excursion_end, save_restriction_begin, save_restriction_end
    public :: internal_labeled_narrow_to_region, internal_labeled_widen, labeled_narrow_to_region_begin, &
              labeled_narrow_to_region_end, labeled_restrictions_remove_in_current_buffer, &
              reset_outermost_restrictions_begin, reset_outermost_restrictions_end
    public :: transpose_regions, translate_region_internal
    public :: buffer_substring, buffer_substring_no_properties, buffer_string, insert_buffer_substring, delete_region, &
              delete_and_extract_region, make_buffer_string, compare_buffer_substrings
    public :: char_to_string, string_to_char, is_char_equal
    public :: pos_bol, pos_eol, line_beginning_position, line_end_position
    public :: get_pos_property, find_field, field_beginning, field_end, field_string, field_string_no_properties, &
              delete_field, constrain_to_field
    public :: format_message, ngettext, format_text, styled_format
    public :: user_login_name, user_real_login_name, user_full_name, system_name, fortmacs_pid, group_name, group_real_gid

    !> The names editfns.c:init_editfns computes (Vuser_login_name,
    !> Vuser_real_login_name, Vuser_full_name); unallocated until then.
    !> Vuser_full_name is the variable `user_full_name', renamed as it has
    !> the name of the function `user_full_name'.
    character(len=:), allocatable :: vuser_login_name, vuser_real_login_name, user_full_name_variable

    !> True means text motion commands don't notice fields.
    !> Port of editfns.c inhibit-field-text-motion.
    logical, public :: inhibit_field_text_motion = .false.

    abstract interface
        !> A function of no arguments returning a buffer position.
        !> Not a port.
        recursive subroutine region_position_procedure(pos, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(out) :: pos
            type(status_type), intent(out) :: status
        end subroutine region_position_procedure
    end interface

    !> What `region_beginning' and `region_end' do: the primitives, or
    !> the advice around them (rect.el's rectangle--region-beginning and
    !> rectangle--region-end, which call the primitives outside
    !> rectangle_mark_mode).  (Ours: upstream's advice-add as a procedure
    !> pointer.)
    procedure(region_position_procedure), pointer, public :: region_beginning_function => primitive_region_beginning
    !> The function `region_end' calls, as `region_beginning_function' for
    !> `region_beginning'.
    procedure(region_position_procedure), pointer, public :: region_end_function => primitive_region_end

    !> What `save_excursion_begin' saved for `save_excursion_end' (upstream's
    !> unwind_excursion specbinding): point as a marker, and the selected
    !> window if it showed the current buffer (the null handle otherwise).
    type, public :: excursion_type
        type(handle_type) :: point
        type(handle_type) :: window
    end type excursion_type

    !> One labeled restriction of a buffer: its label and the markers at
    !> its beginning and end.  `outermost' marks the bounds that were
    !> current when the buffer's first labeled restriction was entered
    !> (upstream's uninterned label outermost-restriction, which no other
    !> label equals).
    type, public :: labeled_restriction_type
        type(handle_type) :: buffer
        character(len=:), allocatable :: label
        logical :: outermost = .false.
        type(handle_type) :: beginning, end
    end type labeled_restriction_type

    !> What `save_restriction_begin' saved for `save_restriction_end': the
    !> buffer, its restriction as markers when it is narrowed, and copies of
    !> its labeled restrictions.
    type, public :: restriction_type
        private
        type(handle_type) :: buffer
        logical :: narrowed = .false.
        type(handle_type) :: beginning, end
        type(labeled_restriction_type), allocatable :: labeled(:)
    end type restriction_type

    !> The labeled restrictions of all buffers, the innermost of each
    !> buffer last.  These are used by `narrow_to_region',
    !> `internal_labeled_narrow_to_region', `widen',
    !> `internal_labeled_widen' and `save_restriction_begin'.  One array
    !> for all buffers rather than a value in each, so that
    !> `reset_outermost_restrictions_begin', which every redisplay calls,
    !> need not visit every live buffer.  Port of editfns.c
    !> labeled_restrictions (an array of records instead of an alist).
    type(labeled_restriction_type), allocatable :: labeled_restrictions(:)

    !> Translate the characters from START to END by TABLE, a string or a
    !> char-table (see the specifics).  Port of
    !> editfns.c:Ftranslate_region_internal.
    interface translate_region_internal
        module procedure translate_region_by_string, translate_region_by_char_table
    end interface translate_region_internal

    !> Insert two to eight strings at point, in order (one string is
    !> insert_delete_mod's `insert'): `call insert(status, "Hello, ", name, "!")'.
    interface insert
        module procedure insert_strings
    end interface insert

    !> Insert two to eight strings at point, relocating markers after the
    !> text (one string is insert_delete_mod's `insert_before_markers').
    interface insert_before_markers
        module procedure insert_strings_before_markers
    end interface insert_before_markers

    !> Insert two to eight strings at point, inheriting properties from
    !> adjoining text (one string is insert_delete_mod's `insert_and_inherit').
    interface insert_and_inherit
        module procedure insert_strings_and_inherit
    end interface insert_and_inherit

    !> Insert two to eight strings at point, relocating markers and
    !> inheriting properties (one string is insert_delete_mod's
    !> `insert_before_markers_and_inherit').
    interface insert_before_markers_and_inherit
        module procedure insert_strings_before_markers_and_inherit
    end interface insert_before_markers_and_inherit

    !> Return the contents of part of the current buffer as a string.
    !> The two arguments START and END are character positions;
    !> they can be in either order.
    !> The string returned is multibyte if the buffer is multibyte.
    !>
    !> This function copies the text properties of that part of the buffer
    !> into the result string (a string value); if you don't want the text
    !> properties, use `buffer_substring_no_properties' instead.
    !> Port of editfns.c:Fbuffer_substring (the generic interface: the
    !> text alone into a character variable, with its text properties into
    !> a string value).
    interface buffer_substring
        module procedure buffer_substring, propertized_buffer_substring
    end interface buffer_substring

    !> Kinds of argument of `format_text' (format_argument_type's `tag').
    integer, parameter :: FORMAT_ARGUMENT_INTEGER = 1, FORMAT_ARGUMENT_REAL = 2, FORMAT_ARGUMENT_VALUE = 3

    !> The largest precision for which a %f conversion's last digit can
    !> be nonzero (upstream's USEFUL_PRECISION_MAX for a long double).
    integer(int64), parameter :: USEFUL_PRECISION_MAX = 16382

    !> The largest string length (upstream's STRING_BYTES_BOUND: 2**61 - 1
    !> on a 64-bit host).
    integer(int64), parameter :: STRING_BYTES_BOUND = 2_int64**61 - 1

    !> One argument of `format_text': an integer, a real64 (float values are
    !> deferred) or a value (a string with its text properties, a name,
    !> a list, ...).
    type :: format_argument_type
        integer :: tag = FORMAT_ARGUMENT_VALUE
        integer(int64) :: integer_number = 0
        real(real64) :: real_number = 0
        type(value_type) :: value
    end type format_argument_type

    !> The arguments of `format_text', `styled_format' and display_engine_mod's
    !> `message', added in order with `append': an integer, a real64, a
    !> string or a value (a string with its text properties, a name, ...).
    !>
    !>     type(format_arguments_type) :: arguments
    !>     call arguments%append(count)
    !>     call arguments%append("files")
    !>     call format_text("%d %s", arguments, text, status)
    type, public :: format_arguments_type
        private
        type(format_argument_type), allocatable :: arguments(:)
    contains
        procedure, private :: append_integer => format_arguments_append_integer
        procedure, private :: append_default_integer => format_arguments_append_default_integer
        procedure, private :: append_real => format_arguments_append_real
        procedure, private :: append_text => format_arguments_append_text
        procedure, private :: append_value => format_arguments_append_value
        !> Add an argument after the others.
        generic :: append => append_integer, append_default_integer, append_real, append_text, append_value
    end type format_arguments_type

    !> What styled_format records of each %-sequence: where its output
    !> starts and ends (character positions, 0-based) in the result, the
    !> byte (0-based) where it starts in the format string, and the
    !> string it produced if that has text properties.  Port of
    !> editfns.c:styled_format's struct info.
    type :: format_spec_type
        integer(int64) :: start = 0, end = 0, fbeg = 0
        logical :: intervals = .false.
        type(value_type) :: argument
    end type format_spec_type

    !> Format a string out of a format-string and arguments (see
    !> `format_text'); with MESSAGE true, as `format-message' (grave accents
    !> and apostrophes of the format string become curved quotes).
    !> The specifics: a character format string gives the text alone, a
    !> string value (with text properties) a string value.
    !> Port of editfns.c:styled_format (the generic interface).
    interface styled_format
        module procedure styled_format_text, styled_format_value
    end interface styled_format

    !> Format a string out of a format-string and arguments.
    !> The first argument is a format control string.
    !> The other arguments are substituted into it to make the result, a string.
    !>
    !> The format control string may contain %-sequences meaning to substitute
    !> the next available argument, or the argument explicitly specified:
    !>
    !> %s means produce a string argument: the text of a string or a name as it
    !>    is; any other object as `readable_text' gives it with NOESCAPE true.
    !> %d or %i means produce a signed number in decimal.
    !> %b means produce a number in binary.
    !> %B is like %b, but %#B uses upper case.
    !> %o means produce a number in octal.
    !> %x means produce a number in hex.
    !> %X is like %x, but uses upper case.
    !> %e means produce a number in exponential notation.
    !> %f means produce a number in decimal-point notation.
    !> %g means produce a number in exponential notation if the exponent would be
    !>    less than -4 or greater than or equal to the precision (default: 6);
    !>    otherwise it produces in decimal-point notation.
    !> %c means produce a number as a single character.
    !> %S means produce any object as `readable_text' gives it (its printed
    !>    representation, which can be read back).
    !>
    !> The argument used for %d, %i, %o, %x, %e, %f, %g or %c must be a number.
    !> Use %% to put a single % into the output.
    !>
    !> A %-sequence other than %% may contain optional field number, flag,
    !> width, and precision specifiers, as follows:
    !>
    !>   %<field><flags><width><precision>character
    !>
    !> where field is [0-9]+ followed by a literal dollar "$", flags is
    !> [+ #0-]+, width is [0-9]+, and precision is a literal period "."
    !> followed by [0-9]+.
    !>
    !> If a %-sequence is numbered with a field with positive value N, the
    !> Nth argument is substituted instead of the next one.  A format can
    !> contain either numbered or unnumbered %-sequences but not both, except
    !> that %% can be mixed with numbered %-sequences.
    !>
    !> The + flag character inserts a + before any nonnegative number, while a
    !> space inserts a space before any nonnegative number; these flags
    !> affect only numeric %-sequences, and the + flag takes precedence.
    !> The - and 0 flags affect the width specifier, as described below.
    !>
    !> The # flag means to use an alternate display form for %b, %B, %o, %x,
    !> %X, %e, %f, and %g sequences: for %b and %B, it prefixes nonzero results
    !> with "0b" or "0B"; for %o, it ensures that the result begins with
    !> "0"; for %x and %X, it prefixes nonzero results with "0x" or "0X";
    !> for %e and %f, it causes a decimal point to be included even if the
    !> precision is zero; for %g, it causes a decimal point to be
    !> included even if the precision is zero, and also forces trailing
    !> zeros after the decimal point to be left in place.
    !>
    !> The width specifier supplies a lower limit for the length of the
    !> produced representation.  The padding, if any, normally goes on the
    !> left, but it goes on the right if the - flag is present.  The padding
    !> character is normally a space, but it is 0 if the 0 flag is present.
    !> The 0 flag is ignored if the - flag is present, or the format sequence
    !> is something other than %d, %b, %o, %x, %e, %f, and %g.
    !>
    !> For %e and %f sequences, the number after the "." in the precision
    !> specifier says how many decimal places to show; if zero, the decimal
    !> point itself is omitted.  For %g, the precision specifies how many
    !> significant digits to produce; zero or omitted are treated as 1.
    !> For %s and %S, the precision specifier truncates the string to the
    !> given width.
    !>
    !> Text properties, if any, are copied from the format-string to the
    !> produced text.
    !>
    !> The arguments are a `format_arguments_type'; a character format
    !> string gives the text alone, a string value one with its text
    !> properties: `call format_text("%d %s", arguments, text, status)'.
    !> Port of editfns.c:Fformat (the generic interface; `format_text', as
    !> FORMAT is a Fortran statement and gfortran rejects a local variable
    !> named like a use-associated generic, R6-47).
    interface format_text
        module procedure format_text_string, format_text_value
    end interface format_text

contains

    ! ------------------------------------------------------------------
    ! Positions
    ! ------------------------------------------------------------------

    !> Return value of point, as an integer.
    !> Beginning of buffer is position `point_min()'.  Port of editfns.c:Fpoint.
    integer(int64) function point()
        point = buffers(current)%pt
    end function point

    !> Return the minimum permissible value of point in the current buffer.
    !> This is 1, unless narrowing (a buffer restriction) is in effect.
    !> Port of editfns.c:Fpoint_min.
    integer(int64) function point_min()
        point_min = buffers(current)%begv
    end function point_min

    !> Return the maximum permissible value of point in the current buffer.
    !> This is `buffer_size() + 1', unless narrowing (a buffer restriction)
    !> is in effect, in which case it is less.  Port of editfns.c:Fpoint_max.
    integer(int64) function point_max()
        point_max = buffers(current)%zv
    end function point_max

    !> Characters in the current buffer, ignoring narrowing.
    !> Port of editfns.c:Fbuffer_size.
    integer(int64) function buffer_size()
        buffer_size = buffers(current)%text%z - BEGINNING
    end function buffer_size

    !> Like `buffer_size', for buffer `h'.  Port of editfns.c:Fbuffer_size.
    subroutine buffer_size_of(h, size, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: size
        type(status_type), intent(out) :: status
        integer(int64) :: i

        size = 0
        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        size = buffers(i)%text%z - BEGINNING
    end subroutine buffer_size_of

    !> Port of editfns.c:Fgap_position.
    integer(int64) function gap_position()
        gap_position = buffers(current)%text%gpt
    end function gap_position

    !> Port of editfns.c:Fgap_size.
    integer(int64) function gap_size()
        gap_size = buffers(current)%text%gap_size
    end function gap_size

    !> Set point to `position', clipped to the accessible region.
    !> Port of editfns.c:Fgoto_char (integer POSITION; an error of a
    !> `point_left' or `point_entered' function is dropped).
    recursive subroutine goto_char(position)
        integer(int64), intent(in) :: position
        integer(int64) :: pos
        type(status_type) :: st

        pos = max(buffers(current)%begv, min(position, buffers(current)%zv))
        call set_point_both(pos, char_to_byte(pos), st)
    end subroutine goto_char

    !> Set point to marker `m'.  Port of editfns.c:Fgoto_char (marker
    !> POSITION), i.e. intervals.c:set_point_from_marker.
    recursive subroutine goto_char_marker(m, status)
        type(handle_type), intent(in) :: m
        type(status_type), intent(out) :: status

        call set_point_from_marker(m, status)
    end subroutine goto_char_marker

    !> Port of intervals.c:set_point_from_marker.  The marker's byte
    !> position is only trusted if it is in the current buffer.
    recursive subroutine set_point_from_marker(m, status)
        type(handle_type), intent(in) :: m
        type(status_type), intent(out) :: status
        integer(int64) :: i, charpos, bytepos

        call resolve_marker(m, i, status)
        if (.not. status%ok()) return

        if (markers(i)%buffer == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Marker does not point anywhere")
            return
        end if

        charpos = max(buffers(current)%begv, min(markers(i)%charpos, buffers(current)%zv))

        if (markers(i)%buffer /= current) then
            bytepos = buf_charpos_to_bytepos(current, charpos)
        else
            bytepos = max(buffers(current)%begv_byte, min(markers(i)%bytepos, buffers(current)%zv_byte))
        end if

        call set_point_both(charpos, bytepos, status)
    end subroutine set_point_from_marker

    !> A new marker at point; the caller frees it.
    !> Port of editfns.c:Fpoint_marker.
    subroutine point_marker(m)
        type(handle_type), intent(out) :: m

        call new_marker_at(buffers(current)%pt, buffers(current)%pt_byte, m)
    end subroutine point_marker

    !> Return this buffer's mark, as a marker object.
    !> Watch out!  Moving this marker changes the mark position.
    !> If you set the marker not to point anywhere, the buffer will have no
    !> mark.  Port of editfns.c:Fmark_marker (made on first use here).
    subroutine mark_marker(m)
        type(handle_type), intent(out) :: m
        integer(int64) :: i
        type(status_type) :: st

        call resolve_marker(buffers(current)%mark, i, st)
        if (.not. st%ok()) call make_marker(buffers(current)%mark)
        m = buffers(current)%mark
    end subroutine mark_marker

    !> The position of the current buffer's mark, or 0 if it has none:
    !> the position of `mark_marker'.  Not a port.
    integer(int64) function mark_position() result(pos)
        integer(int64) :: i
        type(status_type) :: st

        pos = 0
        call resolve_marker(buffers(current)%mark, i, st)
        if (.not. st%ok()) return
        if (markers(i)%buffer /= 0) pos = markers(i)%charpos
    end function mark_position

    !> Point or the mark, the smaller (`beginningp') or the larger,
    !> clipped to the accessible part.  Port of editfns.c:region_limit.
    subroutine region_limit(beginningp, pos, status)
        logical, intent(in) :: beginningp
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        integer(int64) :: m

        pos = 0

        if (transient_mark_mode%value .and. .not. mark_even_if_inactive .and. .not. buffers(current)%mark_active) then
            call set_error(status, FM_MARK_INACTIVE)
            return
        end if

        m = mark_position()

        if (m == 0) then
            call set_error(status, FM_GENERIC_ERROR, "The mark is not set now, so there is no region")
            return
        end if

        if ((buffers(current)%pt < m) .eqv. beginningp) then
            pos = buffers(current)%pt
        else
            pos = min(max(m, buffers(current)%begv), buffers(current)%zv)
        end if
    end subroutine region_limit

    !> Return the integer value of point or mark, whichever is smaller.
    !> Port of editfns.c:Fregion_beginning (through
    !> `region_beginning_function', which rectangle_mark_mode advises).
    recursive subroutine region_beginning(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_beginning_function(pos, status)
    end subroutine region_beginning

    !> Return the integer value of point or mark, whichever is larger.
    !> Port of editfns.c:Fregion_end (through `region_end_function').
    recursive subroutine region_end(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_end_function(pos, status)
    end subroutine region_end

    !> Port of editfns.c:Fregion_beginning.
    recursive subroutine primitive_region_beginning(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_limit(.true., pos, status)
    end subroutine primitive_region_beginning

    !> Port of editfns.c:Fregion_end.
    recursive subroutine primitive_region_end(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_limit(.false., pos, status)
    end subroutine primitive_region_end

    !> Check that the mark is set in the current buffer and, in Transient
    !> Mark mode unless `mark_even_if_inactive', active: an error
    !> otherwise.  FOR_REGION words the error for the region.  Port of
    !> callint.c:check_mark.
    subroutine check_mark(for_region, status)
        logical, intent(in) :: for_region
        type(status_type), intent(out) :: status

        if (mark_position() == 0) then
            if (for_region) then
                call set_error(status, FM_GENERIC_ERROR, "The mark is not set now, so there is no region")
            else
                call set_error(status, FM_GENERIC_ERROR, "The mark is not set now")
            end if

            return
        end if

        if (transient_mark_mode%value .and. .not. mark_even_if_inactive .and. .not. buffers(current)%mark_active) then
            call set_error(status, FM_MARK_INACTIVE)
        end if
    end subroutine check_mark

    !> The region as the arguments of a command that reads the region
    !> (`r'): point and the mark, the smaller first (as they are, not
    !> limited to the accessible part).  `check_mark' reports a mark that
    !> is not set or inactive; rectangle_mark_mode's advice of
    !> `region_beginning' does not apply.  Port of the `r' code of
    !> callint.c:Fcall_interactively.
    subroutine interactive_region(beginning, end, status)
        integer(int64), intent(out) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: mark

        beginning = 0
        end = 0
        call check_mark(.true., status)
        if (.not. status%ok()) return
        mark = mark_position()
        beginning = min(buffers(current)%pt, mark)
        end = max(buffers(current)%pt, mark)
    end subroutine interactive_region

    !> Port of editfns.c:Fpoint_min_marker.
    subroutine point_min_marker(m)
        type(handle_type), intent(out) :: m

        call new_marker_at(buffers(current)%begv, buffers(current)%begv_byte, m)
    end subroutine point_min_marker

    !> Port of editfns.c:Fpoint_max_marker.
    subroutine point_max_marker(m)
        type(handle_type), intent(out) :: m

        call new_marker_at(buffers(current)%zv, buffers(current)%zv_byte, m)
    end subroutine point_max_marker

    !> Port of alloc.c:build_marker (current buffer).
    subroutine new_marker_at(charpos, bytepos, m)
        integer(int64), intent(in) :: charpos, bytepos
        type(handle_type), intent(out) :: m
        type(status_type) :: st

        call make_marker(m)
        call set_marker_both(m, current_buffer(), charpos, bytepos, st)
    end subroutine new_marker_at

    !> Byte position of character `position'; `found' is false when it is
    !> outside the buffer.  Port of editfns.c:Fposition_bytes.
    subroutine position_bytes(position, bytepos, found)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: bytepos
        logical, intent(out) :: found

        bytepos = 0
        found = BEGINNING <= position .and. position <= buffers(current)%text%z
        if (found) bytepos = char_to_byte(position)
    end subroutine position_bytes

    !> Character position containing byte `bytepos'; `found' is false when
    !> it is outside the buffer.  Port of editfns.c:Fbyte_to_position.
    subroutine byte_to_position(bytepos, position, found)
        integer(int64), intent(in) :: bytepos
        integer(int64), intent(out) :: position
        logical, intent(out) :: found
        integer(int64) :: pos_byte

        position = 0
        found = BEGINNING_BYTE <= bytepos .and. bytepos <= buffers(current)%text%z_byte
        if (.not. found) return
        pos_byte = bytepos

        if (buffers(current)%text%z /= buffers(current)%text%z_byte) then
            ! Back up to the start of the character containing BYTEPOS.
            do while (pos_byte < buffers(current)%text%z_byte)
                if (is_char_head(buf_fetch_byte(buffers(current), pos_byte))) exit
                pos_byte = pos_byte - 1
            end do
        end if

        position = byte_to_char(pos_byte)
    end subroutine byte_to_position

    ! ------------------------------------------------------------------
    ! Characters around point
    ! ------------------------------------------------------------------

    !> Character after point, or 0 at the end of the accessible region.
    !> Port of editfns.c:Ffollowing_char.
    integer(int32) function following_char()
        following_char = 0

        if (buffers(current)%pt < buffers(current)%zv) then
            following_char = buf_fetch_char(buffers(current), buffers(current)%pt_byte)
        end if
    end function following_char

    !> Character before point, or 0 at the start of the accessible region.
    !> Port of editfns.c:Fprevious_char.
    integer(int32) function preceding_char()
        integer(int64) :: pos

        preceding_char = 0

        associate (b => buffers(current))
            if (b%pt <= b%begv) return

            if (b%multibyte) then
                pos = b%pt_byte - buf_prev_char_len(b, b%pt_byte)
                preceding_char = buf_fetch_char(b, pos)
            else
                preceding_char = buf_fetch_byte(b, b%pt_byte - 1)
            end if
        end associate
    end function preceding_char

    !> Return true if point is at the beginning of the buffer.
    !> If the buffer is narrowed, this means the beginning of the narrowed part.
    !> Port of editfns.c:Fbobp.
    logical function is_bob()
        is_bob = buffers(current)%pt == buffers(current)%begv
    end function is_bob

    !> Return true if point is at the end of the buffer.
    !> If the buffer is narrowed, this means the end of the narrowed part.
    !> Port of editfns.c:Feobp.
    logical function is_eob()
        is_eob = buffers(current)%pt == buffers(current)%zv
    end function is_eob

    !> Return true if point is at the beginning of a line.  Port of
    !> editfns.c:Fbolp.
    logical function is_bol()
        associate (b => buffers(current))
            is_bol = b%pt == b%begv
            if (.not. is_bol) is_bol = buf_fetch_byte(b, b%pt_byte - 1) == 10
        end associate
    end function is_bol

    !> Return true if point is at the end of a line.
    !> `End of a line' includes point being at the end of the buffer.  Port of
    !> editfns.c:Feolp.
    logical function is_eol()
        associate (b => buffers(current))
            is_eol = b%pt == b%zv
            if (.not. is_eol) is_eol = buf_fetch_byte(b, b%pt_byte) == 10
        end associate
    end function is_eol

    !> Character at `pos' (default: point); `found' is false outside the
    !> accessible region.  Port of editfns.c:Fchar_after (integer POS).
    subroutine char_after(c, found, pos)
        integer(int32), intent(out) :: c
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: pos
        integer(int64) :: pos_byte

        c = 0

        associate (b => buffers(current))
            if (.not. present(pos)) then
                pos_byte = b%pt_byte
                found = b%begv_byte <= pos_byte .and. pos_byte < b%zv_byte
            else
                found = b%begv <= pos .and. pos < b%zv
                if (found) pos_byte = char_to_byte(pos)
            end if

            if (found) c = buf_fetch_char(b, pos_byte)
        end associate
    end subroutine char_after

    !> Character before `pos' (default: point); `found' is false outside
    !> the accessible region.  Port of editfns.c:Fchar_before (integer POS).
    subroutine char_before(c, found, pos)
        integer(int32), intent(out) :: c
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: pos
        integer(int64) :: p, pos_byte

        c = 0

        associate (b => buffers(current))
            p = b%pt
            if (present(pos)) p = pos
            found = b%begv < p .and. p <= b%zv
            if (.not. found) return
            pos_byte = char_to_byte(p)

            if (b%multibyte) then
                pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
                c = buf_fetch_char(b, pos_byte)
            else
                c = buf_fetch_byte(b, pos_byte - 1)
            end if
        end associate
    end subroutine char_before

    ! ------------------------------------------------------------------
    ! Inserting, narrowing, extracting, deleting
    ! ------------------------------------------------------------------

    !> Insert the arguments, strings, at point.
    !> Point and after-insertion markers move forward to end up
    !>  after the inserted text.
    !> Any other markers at the point of insertion remain before the text.
    !>
    !> If the current buffer is unibyte, multibyte strings are converted
    !> to unibyte for insertion (see `string_make_unibyte').
    !>
    !> When operating on binary data, it may be necessary to preserve the
    !> original bytes of a unibyte string when inserting it into a multibyte
    !> buffer; to accomplish this, apply `decode_coding_string' with the
    !> `no_conversion' coding system to the string and insert the result.
    !> Port of editfns.c:Finsert (strings; a character is inserted with
    !> `insert_char', or as the string `char_to_string' gives).
    recursive subroutine insert_strings(status, string1, string2, string3, string4, string5, string6, string7, string8)
        type(status_type), intent(out) :: status
        character(len=*), intent(in) :: string1, string2
        character(len=*), intent(in), optional :: string3, string4, string5, string6, string7, string8

        call general_insert_function(.false., .false., status, string1, string2, string3, string4, string5, string6, &
                                     string7, string8)
    end subroutine insert_strings

    !> Insert the arguments at point, inheriting properties from adjoining text.
    !> Point and after-insertion markers move forward to end up
    !>  after the inserted text.
    !> Any other markers at the point of insertion remain before the text.
    !>
    !> If the current buffer is unibyte, multibyte strings are converted
    !> to unibyte for insertion.
    !> Port of editfns.c:Finsert_and_inherit (strings).
    recursive subroutine insert_strings_and_inherit(status, string1, string2, string3, string4, string5, string6, &
                                                    string7, string8)
        type(status_type), intent(out) :: status
        character(len=*), intent(in) :: string1, string2
        character(len=*), intent(in), optional :: string3, string4, string5, string6, string7, string8

        call general_insert_function(.true., .false., status, string1, string2, string3, string4, string5, string6, &
                                     string7, string8)
    end subroutine insert_strings_and_inherit

    !> Insert strings at point, relocating markers after the text.
    !> Point and all markers at or after the insertion point move forward to
    !> end up after the inserted text.  This is unlike with `insert' and other
    !> insertion functions, where before-insertion markers remain before the
    !> inserted text.
    !>
    !> If the current buffer is unibyte, multibyte strings are converted
    !> to unibyte for insertion.
    !>
    !> If an overlay begins at the insertion point, the inserted text falls
    !> outside the overlay; if a nonempty overlay ends at the insertion
    !> point, the inserted text falls inside that overlay.
    !> Port of editfns.c:Finsert_before_markers (strings).
    recursive subroutine insert_strings_before_markers(status, string1, string2, string3, string4, string5, string6, &
                                                       string7, string8)
        type(status_type), intent(out) :: status
        character(len=*), intent(in) :: string1, string2
        character(len=*), intent(in), optional :: string3, string4, string5, string6, string7, string8

        call general_insert_function(.false., .true., status, string1, string2, string3, string4, string5, string6, &
                                     string7, string8)
    end subroutine insert_strings_before_markers

    !> Insert text at point, relocating markers and inheriting properties.
    !> Point and all markers at or after the insertion point move forward to
    !> end up after the inserted text.  This is unlike with `insert' and other
    !> insertion functions, where before-insertion markers remain before the
    !> inserted text.
    !>
    !> If the current buffer is unibyte, multibyte strings are converted
    !> to unibyte for insertion.
    !> Port of editfns.c:Finsert_and_inherit_before_markers (strings).
    recursive subroutine insert_strings_before_markers_and_inherit(status, string1, string2, string3, string4, string5, &
                                                                   string6, string7, string8)
        type(status_type), intent(out) :: status
        character(len=*), intent(in) :: string1, string2
        character(len=*), intent(in), optional :: string3, string4, string5, string6, string7, string8

        call general_insert_function(.true., .true., status, string1, string2, string3, string4, string5, string6, &
                                     string7, string8)
    end subroutine insert_strings_before_markers_and_inherit

    ! Insert the strings present, each as insdel.c's insert_from_string
    ! (with INHERIT) or insert_from_string_before_markers (with
    ! BEFORE_MARKERS) inserts it, so the change hooks run once per string.
    ! Port of editfns.c:general_insert_function (strings).
    recursive subroutine general_insert_function(inherit, before_markers, status, string1, string2, string3, string4, &
                                                 string5, string6, string7, string8)
        logical, intent(in) :: inherit, before_markers
        type(status_type), intent(out) :: status
        character(len=*), intent(in) :: string1, string2
        character(len=*), intent(in), optional :: string3, string4, string5, string6, string7, string8

        status = status_type()
        call insert_one(string1)
        if (.not. status%ok()) return
        call insert_one(string2)
        if (.not. status%ok()) return

        if (present(string3)) then
            call insert_one(string3)
            if (.not. status%ok()) return
        end if

        if (present(string4)) then
            call insert_one(string4)
            if (.not. status%ok()) return
        end if

        if (present(string5)) then
            call insert_one(string5)
            if (.not. status%ok()) return
        end if

        if (present(string6)) then
            call insert_one(string6)
            if (.not. status%ok()) return
        end if

        if (present(string7)) then
            call insert_one(string7)
            if (.not. status%ok()) return
        end if

        if (present(string8)) call insert_one(string8)

    contains

        !> Insert STRING with the insertion function chosen.
        recursive subroutine insert_one(string)
            character(len=*), intent(in) :: string
            type(status_type) :: st
            integer(int64) :: nchars

            call multibyte_chars_in_text(string, nchars, st)
            if (.not. st%ok()) nchars = len(string, kind=int64)

            if (before_markers) then
                call insert_from_string_before_markers(value_type(text=string), 0_int64, nchars, inherit, status)
            else
                call insert_from_string(value_type(text=string), 0_int64, nchars, inherit, status)
            end if
        end subroutine insert_one
    end subroutine general_insert_function

    !> Insert `count' copies of character `c' (nothing if count <= 0).
    !> Port of editfns.c:Finsert_char (without INHERIT).
    recursive subroutine insert_char_n(c, count, status)
        integer(int32), intent(in) :: c
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        character(len=MAX_MULTIBYTE_LENGTH) :: string
        character(len=:), allocatable :: block
        integer(int32) :: clen
        integer(int64) :: n, chunk, reps

        if (.not. is_char_valid(c)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        if (buffers(current)%multibyte) then
            call char_string(c, string, clen, status)
            if (.not. status%ok()) return
        else
            string(1:1) = char(iand(c, 255_int32))
            clen = 1
        end if

        if (count <= 0) return

        if (BUF_BYTES_MAX/clen < count) then
            call set_error(status, FM_GENERIC_ERROR, "Maximum buffer size exceeded")
            return
        end if

        ! Insert in chunks of at most ~4000 bytes, as upstream.
        reps = min(count, 4000_int64/clen)
        block = repeat(string(1:clen), reps)
        n = count

        do while (n > 0)
            chunk = min(n, reps)
            call insert(block(1:chunk*clen), status)
            if (.not. status%ok()) return
            n = n - chunk
        end do
    end subroutine insert_char_n

    !> Restrict editing in this buffer to the current region.
    !> The rest of the text becomes temporarily invisible and untouchable
    !> but is not deleted; if you save the buffer in a file, the invisible
    !> text is included in the file.  `widen' makes all visible again.
    !> See also `save_restriction_begin'.
    !>
    !> When calling from a program, pass two arguments START and END:
    !> positions bounding the text that should remain visible.
    !>
    !> However, when restrictions have been set by `with_restriction' with a
    !> label, `narrow_to_region' can be used only within the limits of these
    !> restrictions.  If the START or END arguments are outside these limits,
    !> the corresponding limit set by `with_restriction' is used instead of the
    !> argument.  To gain access to other portions of the buffer, use
    !> `without_restriction' with the same label.  Port of
    !> editfns.c:Fnarrow_to_region.
    subroutine narrow_to_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: s, e, k, begv_charpos, zv_charpos, begv_byte, zv_byte

        s = min(start, end)
        e = max(start, end)

        if (.not. (BEGINNING <= s .and. e <= buffers(current)%text%z)) then
            call range_error(status, start, end)
            return
        end if

        call labeled_restrictions_peek_label(current_buffer(), k)

        if (k /= 0) then
            ! Limit the start and end positions to those of the innermost
            ! labeled restriction.
            call marker_position(labeled_restrictions(k)%beginning, begv_charpos, status)
            if (.not. status%ok()) return
            call marker_position(labeled_restrictions(k)%end, zv_charpos, status)
            if (.not. status%ok()) return
            if (s < begv_charpos) s = begv_charpos
            if (s > zv_charpos) s = zv_charpos
            if (e < begv_charpos) e = begv_charpos
            if (e > zv_charpos) e = zv_charpos
        end if

        associate (b => buffers(current))
            ! The byte positions first, while begv and zv still match
            ! their byte positions (as buffer.h:SET_BUF_BEGV and SET_BUF_ZV).
            begv_byte = char_to_byte(s)
            zv_byte = char_to_byte(e)
            b%begv = s
            b%begv_byte = begv_byte
            b%zv = e
            b%zv_byte = zv_byte
            if (b%pt < s) call temp_set_point_both(b, s, b%begv_byte)
            if (e < b%pt) call temp_set_point_both(b, e, b%zv_byte)
        end associate
    end subroutine narrow_to_region

    ! ------------------------------------------------------------------
    ! User and system names
    ! ------------------------------------------------------------------

    !> Set up the user's names once, as editfns.c:init_editfns does: the
    !> real login name from the passwd entry of the real uid, the login
    !> name from LOGNAME or USER (else the effective uid's entry), and the
    !> full name of that user (or NAME, or "unknown").  Port of
    !> editfns.c:init_editfns (the user names).
    subroutine init_user_names()
        use system_dependent_mod, only: passwd_entry_by_uid, fortmacs_getuid, fortmacs_geteuid
        character(len=:), allocatable :: name, gecos, user_name
        integer :: length, stat
        logical :: found

        if (allocated(vuser_login_name)) return
        call passwd_entry_by_uid(fortmacs_getuid(), name, gecos, found)

        if (found) then
            vuser_real_login_name = name
        else
            vuser_real_login_name = "unknown"
        end if

        ! Get the effective user name, by consulting environment variables,
        ! or the effective uid if those are unset.
        user_name = env("LOGNAME")
        if (.not. allocated(user_name)) user_name = env("USER")

        if (.not. allocated(user_name)) then
            call passwd_entry_by_uid(fortmacs_geteuid(), name, gecos, found)

            if (found) then
                user_name = name
            else
                user_name = "unknown"
            end if
        end if

        vuser_login_name = user_name

        ! If the user name claimed in the environment vars differs from
        ! the real uid, use the claimed name to find the full name.
        if (vuser_login_name == vuser_real_login_name) then
            call full_name_of(vuser_login_name, user_full_name_variable, found)
        else
            call full_name_of_uid(fortmacs_geteuid(), user_full_name_variable, found)
        end if

        call get_environment_variable("NAME", length=length, status=stat)

        if (stat == 0) then
            user_full_name_variable = env("NAME")
            if (.not. allocated(user_full_name_variable)) user_full_name_variable = ""
        else if (.not. found) then
            user_full_name_variable = "unknown"
        end if

    contains

        !> The value of the environment variable VAR, unallocated if unset.
        function env(var) result(value)
            character(len=*), intent(in) :: var
            character(len=:), allocatable :: value
            integer :: n, st

            call get_environment_variable(var, length=n, status=st)
            if (st /= 0) return
            allocate (character(len=n) :: value)
            if (n > 0) call get_environment_variable(var, value)
        end function env
    end subroutine init_user_names

    !> The full name in the passwd entry of the user called LOGIN.
    !> Not a port.
    subroutine full_name_of(login, full, found)
        use system_dependent_mod, only: passwd_entry_by_name
        character(len=*), intent(in) :: login
        character(len=:), allocatable, intent(out) :: full
        logical, intent(out) :: found
        character(len=:), allocatable :: name, gecos
        integer :: uid

        call passwd_entry_by_name(login, name, gecos, uid, found)
        full = ""
        if (found) full = gecos_full_name(gecos, name)
    end subroutine full_name_of

    !> The full name in the passwd entry of user UID.  Not a port.
    subroutine full_name_of_uid(uid, full, found)
        use system_dependent_mod, only: passwd_entry_by_uid
        integer, intent(in) :: uid
        character(len=:), allocatable, intent(out) :: full
        logical, intent(out) :: found
        character(len=:), allocatable :: name, gecos

        call passwd_entry_by_uid(uid, name, gecos, found)
        full = ""
        if (found) full = gecos_full_name(gecos, name)
    end subroutine full_name_of_uid

    !> The full name in a gecos field: everything before the first comma,
    !> with `&' replaced by the login name LOGIN, its first character
    !> upcased (AMPERSAND_FULL_NAME).
    !> Port of part of editfns.c:Fuser_full_name.
    function gecos_full_name(gecos, login) result(full)
        character(len=*), intent(in) :: gecos, login
        character(len=:), allocatable :: full, sub
        integer :: q

        ! Chop off everything after the first comma, since `pw_gecos' is a
        ! comma-separated list.
        q = index(gecos, ",")

        if (q > 0) then
            full = gecos(1:q - 1)
        else
            full = gecos
        end if

        ! Substitute the login name for the &, upcasing the first character.
        q = index(full, "&")

        if (q > 0 .and. len(login) > 0) then
            sub = login
            if (sub(1:1) >= "a" .and. sub(1:1) <= "z") sub(1:1) = achar(iachar(sub(1:1)) - 32)
            full = full(1:q - 1)//sub//full(q + 1:)
        end if
    end function gecos_full_name

    !> Return the name under which the user logged in, as a string.
    !> This is based on the effective uid, not the real uid.
    !> Also, if the environment variables LOGNAME or USER are set,
    !> that determines the value of this function.  Port of
    !> editfns.c:Fuser_login_name (no UID argument).
    function user_login_name() result(name)
        character(len=:), allocatable :: name

        call init_user_names()
        name = vuser_login_name
    end function user_login_name

    !> Return the name of the user's real uid, as a string.
    !> This ignores the environment variables LOGNAME and USER, so it differs from
    !> `user_login_name' when running under `su'.  Port of
    !> editfns.c:Fuser_real_login_name.
    function user_real_login_name() result(name)
        character(len=:), allocatable :: name

        call init_user_names()
        name = vuser_real_login_name
    end function user_real_login_name

    !> Return the full name of the user logged in, as a string.
    !> If the full name corresponding to Fortmacs's userid is not known,
    !> return "unknown".
    !>
    !> If the full name includes commas, remove everything starting with
    !> the first comma, because the `gecos' field of the `/etc/passwd' file
    !> is in general a comma-separated list.  Port of
    !> editfns.c:Fuser_full_name (no UID argument).
    function user_full_name() result(name)
        character(len=:), allocatable :: name

        call init_user_names()
        name = user_full_name_variable
    end function user_full_name

    !> Return the host name of the machine you are running on, as a string.
    !> Port of editfns.c:Fsystem_name (sysdep.c:init_system_name: spaces
    !> and tabs become dashes).
    function system_name() result(name)
        use system_dependent_mod, only: fortmacs_gethostname
        character(len=:), allocatable :: name
        integer :: k

        name = fortmacs_gethostname()

        do k = 1, len(name)
            if (name(k:k) == " " .or. name(k:k) == achar(9)) name(k:k) = "-"
        end do
    end function system_name

    !> Return the process ID of Fortmacs, as an integer.
    !> Port of editfns.c:Femacs_pid.
    integer(int32) function fortmacs_pid()
        use system_dependent_mod, only: fortmacs_getpid

        fortmacs_pid = fortmacs_getpid()
    end function fortmacs_pid

    !> Return the name of the group whose numeric group ID is GID.
    !> NAME is unallocated if a group with such GID does not exist or is
    !> not known.  Port of editfns.c:Fgroup_name.
    subroutine group_name(gid, name, status)
        use system_dependent_mod, only: group_name_of_gid
        integer(int64), intent(in) :: gid
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: found_name
        integer(int32) :: id
        logical :: found

        ! A gid_t, an unsigned 32-bit integer (int32 holds the upper half
        ! as negative values).
        if (gid < 0 .or. gid > 4294967295_int64) then
            call set_error(status, FM_GENERIC_ERROR, "Not an in-range integer")
            return
        end if

        if (gid > huge(0_int32)) then
            id = int(gid - 4294967296_int64, int32)
        else
            id = int(gid, int32)
        end if

        call group_name_of_gid(id, found_name, found)
        if (found) name = found_name
    end subroutine group_name

    !> Return the real gid of Fortmacs, as an integer.
    !> Port of editfns.c:Fgroup_real_gid.
    integer(int32) function group_real_gid()
        use system_dependent_mod, only: fortmacs_getgid

        group_real_gid = fortmacs_getgid()
    end function group_real_gid

    !> Return true if the current buffer is narrowed.  Port of
    !> subr.el:buffer-narrowed-p.
    logical function is_buffer_narrowed()
        is_buffer_narrowed = buffers(current)%zv - buffers(current)%begv /= buffers(current)%text%z - BEGINNING
    end function is_buffer_narrowed

    !> Remove restrictions (narrowing) from current buffer.
    !>
    !> This allows the buffer's full text to be seen and edited.
    !>
    !> However, when restrictions have been set by `with_restriction' with a
    !> label, `widen' restores the narrowing limits set by `with_restriction'.
    !> To gain access to other portions of the buffer, use
    !> `without_restriction' with the same label.  Port of editfns.c:Fwiden.
    subroutine widen()
        type(handle_type) :: buffer
        integer(int64) :: k

        buffer = current_buffer()
        call labeled_restrictions_peek_label(buffer, k)

        if (k == 0) then
            associate (b => buffers(current))
                b%begv = BEGINNING
                b%begv_byte = BEGINNING_BYTE
                b%zv = b%text%z
                b%zv_byte = b%text%z_byte
            end associate
        else
            call set_bounds_from_markers(current, labeled_restrictions(k)%beginning, labeled_restrictions(k)%end)
            ! If the only remaining bounds in labeled_restrictions for
            ! current_buffer are the bounds that were set by the user, no
            ! labeled restriction is in effect in current_buffer anymore:
            ! remove it from the labeled_restrictions array.
            if (labeled_restrictions(k)%outermost) call labeled_restrictions_pop(buffer)
        end if
    end subroutine widen

    ! ------------------------------------------------------------------
    ! Labeled restrictions
    ! ------------------------------------------------------------------

    !> Set the accessible region of buffer slot B to the positions of the
    !> markers BEGINNING and END.  Not a port (upstream's SET_BUF_BEGV_BOTH
    !> and upstream's SET_BUF_ZV_BOTH from two markers).
    subroutine set_bounds_from_markers(b, beginning, end)
        integer(int64), intent(in) :: b
        type(handle_type), intent(in) :: beginning, end
        type(status_type) :: st

        associate (buffer => buffers(b))
            call marker_position(beginning, buffer%begv, st)
            call marker_byte_position(beginning, buffer%begv_byte, st)
            call marker_position(end, buffer%zv, st)
            call marker_byte_position(end, buffer%zv_byte, st)
        end associate
    end subroutine set_bounds_from_markers

    !> Whether the labeled RESTRICTION belongs to BUFFER.
    !> Not a port.
    elemental logical function is_restriction_of(restriction, buffer)
        type(labeled_restriction_type), intent(in) :: restriction
        type(handle_type), intent(in) :: buffer

        is_restriction_of = restriction%buffer%index == buffer%index .and. &
                            restriction%buffer%generation == buffer%generation
    end function is_restriction_of

    !> Add BUFFER with its labeled RESTRICTIONS (the innermost last) to
    !> `labeled_restrictions', which takes over their markers.  Port of
    !> editfns.c:labeled_restrictions_add.
    subroutine labeled_restrictions_add(buffer, restrictions)
        type(handle_type), intent(in) :: buffer
        type(labeled_restriction_type), intent(inout) :: restrictions(:)
        type(labeled_restriction_type), allocatable :: grown(:)
        integer(int64) :: n, k

        n = 0
        if (allocated(labeled_restrictions)) n = size(labeled_restrictions, kind=int64)
        allocate (grown(n + size(restrictions, kind=int64)))

        do k = 1, n
            call move_restriction(labeled_restrictions(k), grown(k))
        end do

        do k = 1, size(restrictions, kind=int64)
            call move_restriction(restrictions(k), grown(n + k))
            grown(n + k)%buffer = buffer
        end do

        call move_alloc(grown, labeled_restrictions)
    end subroutine labeled_restrictions_add

    !> Move the labeled restriction FROM to TO (FROM's label is
    !> deallocated).  Not a port.
    subroutine move_restriction(from, to)
        type(labeled_restriction_type), intent(inout) :: from
        type(labeled_restriction_type), intent(out) :: to

        to%buffer = from%buffer
        if (allocated(from%label)) call move_alloc(from%label, to%label)
        to%outermost = from%outermost
        to%beginning = from%beginning
        to%end = from%end
    end subroutine move_restriction

    !> Remove BUFFER and its labeled restrictions from
    !> `labeled_restrictions', destroying their markers.  Do nothing if
    !> BUFFER is not present in `labeled_restrictions'.  Port of
    !> editfns.c:labeled_restrictions_remove.
    subroutine labeled_restrictions_remove(buffer)
        type(handle_type), intent(in) :: buffer
        logical, allocatable :: mine(:)

        if (.not. allocated(labeled_restrictions)) return
        mine = is_restriction_of(labeled_restrictions, buffer)
        if (.not. any(mine)) return
        call remove_restrictions(mine)
    end subroutine labeled_restrictions_remove

    !> Remove the elements of `labeled_restrictions' that DOOMED marks,
    !> destroying their markers.  Not a port.
    subroutine remove_restrictions(doomed)
        logical, intent(in) :: doomed(:)
        type(labeled_restriction_type), allocatable :: kept(:)
        type(status_type) :: st
        integer(int64) :: k, n

        allocate (kept(count(.not. doomed)))
        n = 0

        do k = 1, size(doomed, kind=int64)
            if (doomed(k)) then
                call destroy_marker(labeled_restrictions(k)%beginning, st)
                call destroy_marker(labeled_restrictions(k)%end, st)
            else
                n = n + 1
                call move_restriction(labeled_restrictions(k), kept(n))
            end if
        end do

        call move_alloc(kept, labeled_restrictions)
    end subroutine remove_restrictions

    !> One of the labeled restriction bounds of BUFFER, as a marker (the
    !> beginning when BEGV, else the end), or the null handle if BUFFER has
    !> no labeled restriction or is a closed buffer.  When OUTERMOST is
    !> true, the restriction bounds that were current when the first
    !> labeled restriction was entered are returned.  Otherwise the bounds
    !> of the innermost labeled restriction are returned.  Port of
    !> editfns.c:labeled_restrictions_get_bound.
    function labeled_restrictions_get_bound(buffer, begv, outermost) result(marker)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: begv, outermost
        type(handle_type) :: marker
        integer(int64) :: k

        marker = handle_type()
        if (.not. is_buffer_live(buffer) .or. .not. allocated(labeled_restrictions)) return

        if (outermost) then
            k = findloc(is_restriction_of(labeled_restrictions, buffer) .and. labeled_restrictions%outermost, .true., 1, &
                        kind=int64)
        else
            k = findloc(is_restriction_of(labeled_restrictions, buffer), .true., 1, kind=int64, back=.true.)
        end if

        if (k == 0) return

        if (begv) then
            marker = labeled_restrictions(k)%beginning
        else
            marker = labeled_restrictions(k)%end
        end if
    end function labeled_restrictions_get_bound

    !> The innermost labeled restriction of BUFFER, as its index K in
    !> `labeled_restrictions' (whose label and `outermost' the caller
    !> reads), or 0 if BUFFER has no labeled restriction or is a closed
    !> buffer.  Port of editfns.c:labeled_restrictions_peek_label.
    subroutine labeled_restrictions_peek_label(buffer, k)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(out) :: k

        k = 0
        if (.not. is_buffer_live(buffer) .or. .not. allocated(labeled_restrictions)) return
        k = findloc(is_restriction_of(labeled_restrictions, buffer), .true., 1, kind=int64, back=.true.)
    end subroutine labeled_restrictions_peek_label

    !> Add a labeled RESTRICTION for BUFFER to `labeled_restrictions', which
    !> takes over its markers.  Port of editfns.c:labeled_restrictions_push.
    subroutine labeled_restrictions_push(buffer, restriction)
        type(handle_type), intent(in) :: buffer
        type(labeled_restriction_type), intent(inout) :: restriction
        type(labeled_restriction_type) :: one(1)

        call move_restriction(restriction, one(1))
        call labeled_restrictions_add(buffer, one)
    end subroutine labeled_restrictions_push

    !> Remove the innermost labeled restriction of BUFFER from
    !> `labeled_restrictions', destroying its markers; when that is the
    !> outermost bounds, remove BUFFER altogether.  Do nothing if BUFFER is
    !> not present in `labeled_restrictions'.  Port of
    !> editfns.c:labeled_restrictions_pop.
    subroutine labeled_restrictions_pop(buffer)
        type(handle_type), intent(in) :: buffer
        logical, allocatable :: doomed(:)
        integer(int64) :: k

        if (.not. allocated(labeled_restrictions)) return
        k = findloc(is_restriction_of(labeled_restrictions, buffer), .true., 1, kind=int64, back=.true.)
        if (k == 0) return

        if (labeled_restrictions(k)%outermost) then
            call labeled_restrictions_remove(buffer)
        else
            allocate (doomed(size(labeled_restrictions)))
            doomed = .false.
            doomed(k) = .true.
            call remove_restrictions(doomed)
        end if
    end subroutine labeled_restrictions_pop

    !> Unconditionally remove all labeled restrictions in the current
    !> buffer.  Port of editfns.c:labeled_restrictions_remove_in_current_buffer.
    subroutine labeled_restrictions_remove_in_current_buffer()
        call labeled_restrictions_remove(current_buffer())
    end subroutine labeled_restrictions_remove_in_current_buffer

    !> Restore the restriction bounds that were current when the first
    !> labeled restriction was entered, in every buffer that has labeled
    !> restrictions; RESTRICTED gets those buffers, whose innermost
    !> labeled restriction `reset_outermost_restrictions_end' restores.
    !> In particular, redisplay calls this when it starts, so that if a
    !> function executed during redisplay calls `redisplay' while labeled
    !> restrictions are in effect, these restrictions will not become
    !> visible on display.  Port of
    !> editfns.c:reset_outermost_restrictions.
    subroutine reset_outermost_restrictions_begin(restricted)
        type(handle_type), allocatable, intent(out) :: restricted(:)
        type(handle_type), allocatable :: listed(:)
        type(handle_type) :: begv, zv
        integer(int64) :: k, n, b
        type(status_type) :: st

        if (.not. allocated(labeled_restrictions)) then
            allocate (restricted(0))
            return
        end if

        call restricted_buffers(listed)
        allocate (restricted(size(listed)))
        n = 0

        do k = 1, size(listed, kind=int64)
            begv = labeled_restrictions_get_bound(listed(k), .true., .true.)
            zv = labeled_restrictions_get_bound(listed(k), .false., .true.)

            if (begv%index /= 0 .and. zv%index /= 0) then
                call resolve_buffer(listed(k), b, st)
                call set_bounds_from_markers(b, begv, zv)
                n = n + 1
                restricted(n) = listed(k)
            else
                call labeled_restrictions_remove(listed(k))
            end if
        end do

        restricted = restricted(1:n)
    end subroutine reset_outermost_restrictions_begin

    !> Restore the bounds of the innermost labeled restriction of each
    !> buffer in RESTRICTED, which `reset_outermost_restrictions_begin'
    !> gave, the last first; a buffer that has none any more leaves
    !> `labeled_restrictions'.  Port of
    !> editfns.c:unwind_reset_outermost_restriction (for each buffer).
    subroutine reset_outermost_restrictions_end(restricted)
        type(handle_type), intent(in) :: restricted(:)
        type(handle_type) :: begv, zv
        integer(int64) :: k, b
        type(status_type) :: st

        do k = size(restricted, kind=int64), 1, -1
            begv = labeled_restrictions_get_bound(restricted(k), .true., .false.)
            zv = labeled_restrictions_get_bound(restricted(k), .false., .false.)

            if (begv%index /= 0 .and. zv%index /= 0) then
                call resolve_buffer(restricted(k), b, st)
                call set_bounds_from_markers(b, begv, zv)
            else
                call labeled_restrictions_remove(restricted(k))
            end if
        end do
    end subroutine reset_outermost_restrictions_end

    !> The buffers that have labeled restrictions, each once, in the order
    !> of `labeled_restrictions'.  Not a port.
    subroutine restricted_buffers(listed)
        type(handle_type), allocatable, intent(out) :: listed(:)
        integer(int64) :: k, n

        allocate (listed(size(labeled_restrictions)))
        n = 0

        do k = 1, size(labeled_restrictions, kind=int64)
            if (n > 0) then
                if (any(is_restriction_of(labeled_restrictions(k), listed(1:n)))) cycle
            end if

            n = n + 1
            listed(n) = labeled_restrictions(k)%buffer
        end do

        listed = listed(1:n)
    end subroutine restricted_buffers

    !> Narrow the current buffer to BEGV..ZV with a restriction labeled
    !> with LABEL, saving point in the marker POINT; the caller ends it with
    !> `labeled_narrow_to_region_end', which does what upstream leaves to
    !> the end of the enclosing scope.  Port of
    !> editfns.c:labeled_narrow_to_region.
    subroutine labeled_narrow_to_region_begin(begv, zv, label, point, status)
        integer(int64), intent(in) :: begv, zv
        character(len=*), intent(in) :: label
        type(handle_type), intent(out) :: point
        type(status_type), intent(out) :: status

        call point_marker(point)
        call internal_labeled_narrow_to_region(begv, zv, label, status)
    end subroutine labeled_narrow_to_region_begin

    !> End what `labeled_narrow_to_region_begin' began: widen the
    !> restriction labeled with LABEL, then go back to the marker POINT,
    !> which is destroyed.  Port of editfns.c:unwind_labeled_narrow_to_region,
    !> then fileio.c:restore_point_unwind.
    recursive subroutine labeled_narrow_to_region_end(label, point)
        character(len=*), intent(in) :: label
        type(handle_type), intent(in) :: point
        type(status_type) :: st

        call internal_labeled_widen(label)
        call goto_char_marker(point, st)
        call destroy_marker(point, st)
    end subroutine labeled_narrow_to_region_end

    !> Restrict this buffer to START-END, and label the restriction with
    !> LABEL.
    !>
    !> This is an internal function used by `with_restriction'.  Port of
    !> editfns.c:Finternal__labeled_narrow_to_region.
    subroutine internal_labeled_narrow_to_region(start, end, label, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: label
        type(status_type), intent(out) :: status
        type(labeled_restriction_type) :: outermost_restriction, restriction
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: k

        buffer = current_buffer()
        outermost_restriction%outermost = .true.
        call point_min_marker(outermost_restriction%beginning)
        call point_max_marker(outermost_restriction%end)
        call narrow_to_region(start, end, status)
        call labeled_restrictions_peek_label(buffer, k)

        if (status%ok() .and. k == 0) then
            call labeled_restrictions_push(buffer, outermost_restriction)
        else
            call destroy_marker(outermost_restriction%beginning, st)
            call destroy_marker(outermost_restriction%end, st)
        end if

        if (.not. status%ok()) return
        restriction%label = label
        call point_min_marker(restriction%beginning)
        call point_max_marker(restriction%end)
        call labeled_restrictions_push(buffer, restriction)
    end subroutine internal_labeled_narrow_to_region

    !> Remove the current restriction if it is labeled with LABEL, and
    !> widen.
    !>
    !> This is an internal function used by `without_restriction'.  Port of
    !> editfns.c:Finternal__labeled_widen.
    subroutine internal_labeled_widen(label)
        character(len=*), intent(in) :: label
        type(handle_type) :: buffer
        integer(int64) :: k

        buffer = current_buffer()
        call labeled_restrictions_peek_label(buffer, k)

        if (k /= 0) then
            if (.not. labeled_restrictions(k)%outermost) then
                if (labeled_restrictions(k)%label == label .and. len(labeled_restrictions(k)%label) == len(label)) then
                    call labeled_restrictions_pop(buffer)
                end if
            end if
        end if

        call widen()
    end subroutine internal_labeled_widen

    ! ------------------------------------------------------------------
    ! Excursions and restrictions
    ! ------------------------------------------------------------------

    !> Save point and the current buffer in SAVED, for
    !> `save_excursion_end' to restore: the beginning of upstream's
    !> save-excursion, whose docstring follows.
    !>
    !> Save point, and current buffer; execute the code up to
    !> `save_excursion_end'; restore those things.  The values of point and
    !> the current buffer are restored even in case of abnormal exit (a
    !> return with an error status), provided the caller calls
    !> `save_excursion_end' on every path.
    !>
    !> If you only want to save the current buffer but not point, then just
    !> save the buffer's handle and `set_buffer' it again.
    !>
    !> Port of editfns.c:save_excursion_save.
    subroutine save_excursion_begin(saved)
        type(excursion_type), intent(out) :: saved
        type(handle_type) :: window
        type(status_type) :: st

        call point_marker(saved%point)
        ! Selected window if current buffer is shown in it, null otherwise.
        saved%window = handle_type()
        window = frame_selected_window(status=st)
        if (.not. st%ok() .or. window%index == 0) return
        if (same_handle(window_buffer(window, st), current_buffer())) saved%window = window
    end subroutine save_excursion_begin

    !> Restore the buffer and point SAVED by `save_excursion_begin' (whose
    !> marker is destroyed).  Port of editfns.c:save_excursion_restore.
    recursive subroutine save_excursion_end(saved)
        type(excursion_type), intent(in) :: saved
        type(handle_type) :: buffer, selected, contents
        type(window_type), pointer :: w
        type(status_type) :: st

        call marker_buffer(saved%point, buffer, st)

        ! If the saved buffer was closed meanwhile, its markers point
        ! nowhere and BUFFER is null.
        if (buffer%index == 0) then
            call destroy_marker(saved%point, st)
            return
        end if

        call set_buffer(buffer, st)
        ! Point marker.
        call goto_char_marker(saved%point, st)
        call destroy_marker(saved%point, st)
        ! If buffer was visible in a window, and a different window was
        ! selected, and the old selected window is still showing this
        ! buffer, restore point in that window.
        if (saved%window%index == 0) return
        selected = frame_selected_window(status=st)
        if (same_handle(saved%window, selected) .or. .not. is_window_live(saved%window)) return
        ! Set window point if WINDOW is live and shows the current buffer.
        contents = window_buffer(saved%window, st)
        if (.not. same_handle(contents, current_buffer())) return
        w => window_ref(saved%window)
        if (associated(w)) call set_window_point(w, point(), st)
    end subroutine save_excursion_end

    !> Save the current buffer's restrictions in SAVED, for
    !> `save_restriction_end' to restore: the beginning of upstream's
    !> save-restriction, whose docstring follows.
    !>
    !> Execute the code up to `save_restriction_end', saving and restoring
    !> current buffer's restrictions.  The buffer's restrictions make parts
    !> of the beginning and end invisible.  (They are set up with
    !> `narrow_to_region' and eliminated with `widen'.)  This saves the
    !> current buffer's restrictions, including those that were set by
    !> `with_restriction' with a label argument, and `save_restriction_end'
    !> restores them.  So any `narrow_to_region' in between lasts only until
    !> `save_restriction_end'.  The old restrictions settings are restored
    !> even in case of abnormal exit (a return with an error status),
    !> provided the caller calls `save_restriction_end' on every path.
    !>
    !> Note: if you are using both `save_excursion_begin' and
    !> `save_restriction_begin', use `save_excursion_begin' outermost:
    !>     call save_excursion_begin(excursion)
    !>     call save_restriction_begin(restriction)
    !>     ...
    !>     call save_restriction_end(restriction)
    !>     call save_excursion_end(excursion)
    !>
    !> Port of editfns.c:save_restriction_save, with
    !> editfns.c:save_restriction_save_1 and editfns.c:labeled_restrictions_save.
    subroutine save_restriction_begin(saved)
        type(restriction_type), intent(out) :: saved
        logical, allocatable :: mine(:)
        type(status_type) :: st
        integer(int64) :: k, n

        saved%buffer = current_buffer()

        associate (b => buffers(current))
            ! The common case that the buffer isn't narrowed needs no
            ! markers; save_restriction_end recognizes it as meaning no
            ! restriction.
            if (b%begv /= BEGINNING .or. b%zv /= b%text%z) then
                saved%narrowed = .true.
                call new_marker_at(b%begv, b%begv_byte, saved%beginning)
                call new_marker_at(b%zv, b%zv_byte, saved%end)
                ! END must move forward if text is inserted at its exact location.
                call set_marker_insertion_type(saved%end, .true., st)
            end if
        end associate

        ! Copies of the labeled restrictions (markers of their own, at the
        ! same places with the same insertion types).
        if (.not. allocated(labeled_restrictions)) then
            allocate (saved%labeled(0))
            return
        end if

        mine = is_restriction_of(labeled_restrictions, saved%buffer)
        allocate (saved%labeled(count(mine)))
        n = 0

        do k = 1, size(mine, kind=int64)
            if (.not. mine(k)) cycle
            n = n + 1
            saved%labeled(n)%buffer = saved%buffer
            if (allocated(labeled_restrictions(k)%label)) saved%labeled(n)%label = labeled_restrictions(k)%label
            saved%labeled(n)%outermost = labeled_restrictions(k)%outermost

            associate (beginning => markers(labeled_restrictions(k)%beginning%index), &
                       end => markers(labeled_restrictions(k)%end%index))
                call copy_marker_at(beginning%charpos, saved%labeled(n)%beginning, beginning%insertion_type, st)
                call copy_marker_at(end%charpos, saved%labeled(n)%end, end%insertion_type, st)
            end associate
        end do
    end subroutine save_restriction_begin

    !> Restore the restrictions SAVED by `save_restriction_begin' (whose
    !> markers are destroyed or taken over).  Port of
    !> editfns.c:save_restriction_restore, with
    !> editfns.c:labeled_restrictions_restore and
    !> editfns.c:save_restriction_restore_1.
    subroutine save_restriction_end(saved)
        type(restriction_type), intent(inout) :: saved
        integer(int64) :: b, pt, pt_byte, begv, begv_byte, zv, zv_byte
        type(status_type) :: st
        integer(int64) :: k

        ! The labeled restrictions.
        call labeled_restrictions_remove(saved%buffer)

        if (is_buffer_live(saved%buffer) .and. size(saved%labeled) > 0) then
            call labeled_restrictions_add(saved%buffer, saved%labeled)
        else
            do k = 1, size(saved%labeled, kind=int64)
                call destroy_marker(saved%labeled(k)%beginning, st)
                call destroy_marker(saved%labeled(k)%end, st)
            end do
        end if

        deallocate (saved%labeled)
        call resolve_buffer(saved%buffer, b, st)

        if (saved%narrowed) then
            ! Two markers bounding a saved restriction: verify that they
            ! still point to a buffer.
            if (st%ok()) then
                call marker_position(saved%beginning, begv, st)
                call marker_byte_position(saved%beginning, begv_byte, st)
                call marker_position(saved%end, zv, st)
                call marker_byte_position(saved%end, zv_byte, st)

                associate (buffer => buffers(b))
                    ! The restriction has changed from the saved one, so
                    ! restore the saved restriction.
                    if (begv /= buffer%begv .or. zv /= buffer%zv) then
                        pt = buffer%pt
                        pt_byte = buffer%pt_byte
                        buffer%begv = begv
                        buffer%begv_byte = begv_byte
                        buffer%zv = zv
                        buffer%zv_byte = zv_byte

                        ! The point is outside the new visible range, move it inside.
                        if (pt < begv .or. pt > zv) then
                            call temp_set_point_both(buffer, max(begv, min(pt, zv)), &
                                                     max(begv_byte, min(pt_byte, zv_byte)))
                        end if
                    end if
                end associate
            end if

            call destroy_marker(saved%beginning, st)
            call destroy_marker(saved%end, st)
        else if (st%ok()) then
            ! No old restriction: if the buffer has been narrowed, get rid
            ! of the narrowing.
            associate (buffer => buffers(b))
                buffer%begv = BEGINNING
                buffer%begv_byte = BEGINNING_BYTE
                buffer%zv = buffer%text%z
                buffer%zv_byte = buffer%text%z_byte
            end associate
        end if
    end subroutine save_restriction_end

    !> Order `start', `end' and check they lie in the accessible region.
    !> Port of buffer.c:validate_region.
    subroutine validate_region(start, end, status)
        integer(int64), intent(inout) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = min(start, end)
        e = max(start, end)

        if (.not. (buffers(current)%begv <= s .and. e <= buffers(current)%zv)) then
            call range_error(status, start, end)
            return
        end if

        start = s
        end = e
    end subroutine validate_region

    ! An args-out-of-range error for A and B (args_out_of_range).
    ! Not a port.
    subroutine range_error(status, a, b)
        type(status_type), intent(inout) :: status
        integer(int64), intent(in) :: a, b
        character(len=24) :: sa, sb

        write (sa, "(i0)") a
        write (sb, "(i0)") b
        call set_error(status, FM_ARGS_OUT_OF_RANGE, trim(sa)//", "//trim(sb))
    end subroutine range_error

    !> Return the contents of part of the current buffer as a string.
    !> The two arguments START and END are character positions;
    !> they can be in either order.
    !> The string returned is multibyte if the buffer is multibyte.
    !>
    !> This form gives the text alone, as `buffer_substring_no_properties'
    !> does; with a string value for TEXT, the generic `buffer_substring'
    !> copies the text properties of that part of the buffer into it too.
    !> Port of editfns.c:Fbuffer_substring (into a character variable).
    subroutine buffer_substring(start, end, text, status)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status

        call buffer_substring_no_properties(start, end, text, status)
    end subroutine buffer_substring

    !> Return the contents of part of the current buffer as a string.
    !> The two arguments START and END are character positions;
    !> they can be in either order.
    !> The string returned is multibyte if the buffer is multibyte.
    !>
    !> This function copies the text properties of that part of the buffer
    !> into the result string; if you don't want the text properties,
    !> use `buffer_substring_no_properties' instead.
    !> Port of editfns.c:Fbuffer_substring.
    subroutine propertized_buffer_substring(start, end, string, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(out) :: string
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        call make_buffer_string(s, e, .true., string)
    end subroutine propertized_buffer_substring

    !> Return the characters of part of the buffer, without the text properties.
    !> The two arguments START and END are character positions;
    !> they can be in either order.
    !> Port of editfns.c:Fbuffer_substring_no_properties.
    subroutine buffer_substring_no_properties(start, end, text, status)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        text = buf_bytes(buffers(current), char_to_byte(s), char_to_byte(e))
    end subroutine buffer_substring_no_properties

    !> The text of the current buffer from START to END (START <= END, in
    !> the accessible text) as a string value, with the text properties
    !> of that text when PROPS is true.  Port of editfns.c:make_buffer_string
    !> (and editfns.c:make_buffer_string_both, the byte positions found
    !> here; the bytes are joined across the gap by `buf_bytes').
    subroutine make_buffer_string(start, end, props, string)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: props
        type(value_type), intent(out) :: string

        string = value_type(text=buf_bytes(buffers(current), char_to_byte(start), char_to_byte(end)))
        ! Copy the text properties (none if that text has none).
        if (props) call copy_intervals_to_string(string, buffers(current)%text%intervals, start, end - start)
    end subroutine make_buffer_string

    !> The accessible text.  Port of editfns.c:Fbuffer_string.
    function buffer_string() result(text)
        character(len=:), allocatable :: text

        text = buf_bytes(buffers(current), buffers(current)%begv_byte, buffers(current)%zv_byte)
    end function buffer_string

    !> Insert before point a substring of the contents of BUFFER.
    !> BUFFER may be a buffer or a buffer name.
    !> Arguments START and END are character positions specifying the substring.
    !> They default to the values of (point-min) and (point-max) in BUFFER.
    !>
    !> Point and before-insertion markers move forward to end up after the
    !> inserted text.
    !> Any other markers at the point of insertion remain before the text.
    !>
    !> If the current buffer is multibyte and BUFFER is unibyte, or vice
    !> versa, strings are converted from unibyte to multibyte or vice versa
    !> using `string_make_multibyte' or `string_make_unibyte', which see.
    !> Port of editfns.c:Finsert_buffer_substring.
    recursive subroutine insert_buffer_substring(buffer, status, start, end)
        use insert_delete_mod, only: insert_from_buffer
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: b, e, bp

        status = status_type()

        if (.not. is_buffer_live(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Selecting deleted buffer")
            return
        end if

        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        bp = current
        b = point_min()
        e = point_max()
        if (present(start)) b = start
        if (present(end)) e = end
        call validate_region(b, e, status)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call insert_from_buffer(bp, b, e - b, .false., status)
    end subroutine insert_buffer_substring

    !> Delete the text between `start' and `end' (either order).
    !> Port of editfns.c:Fdelete_region.
    recursive subroutine delete_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        call del_range(s, e, status)
    end subroutine delete_region

    !> Delete the text between `start' and `end' and return it.
    !> Port of editfns.c:Fdelete_and_extract_region.
    recursive subroutine delete_and_extract_region(start, end, text, status)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        text = ""
        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        if (s == e) return
        call del_range_1(s, e, .true., status, text)
    end subroutine delete_and_extract_region

    !> Compare two substrings of two buffers; return result as number.
    !> Return -N if first string is less after N-1 chars, +N if first string is
    !> greater after N-1 chars, or 0 if strings match.
    !> The first substring is in BUFFER1 from START1 to END1 and the second
    !> is in BUFFER2 from START2 to END2.
    !> All arguments may be absent.  If BUFFER1 or BUFFER2 is absent, the current
    !> buffer is used.  If START1 or START2 is absent, the value of `point_min'
    !> in the respective buffers is used.  If END1 or END2 is absent, the value
    !> of `point_max' in the respective buffers is used.
    !> The value of `case_fold_search' in the current buffer
    !> determines whether case is significant or ignored.
    !> Port of editfns.c:Fcompare_buffer_substrings.
    subroutine compare_buffer_substrings(result, status, buffer1, start1, end1, buffer2, start2, end2)
        integer(int64), intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer1, buffer2
        integer(int64), intent(in), optional :: start1, end1, start2, end2
        integer(int64) :: bp1, bp2, begp1, endp1, begp2, endp2, i1, i2, i1_byte, i2_byte, chars
        integer(int32) :: c1, c2
        logical :: fold

        result = 0
        fold = case_fold_search%value
        ! Find the first buffer and its substring.
        call find_substring(buffer1, start1, end1, bp1, begp1, endp1)
        if (.not. status%ok()) return
        ! Likewise for second substring.
        call find_substring(buffer2, start2, end2, bp2, begp2, endp2)
        if (.not. status%ok()) return
        i1 = begp1
        i2 = begp2
        i1_byte = buf_charpos_to_bytepos(bp1, i1)
        i2_byte = buf_charpos_to_bytepos(bp2, i2)
        chars = 0

        do while (i1 < endp1 .and. i2 < endp2)
            ! When we find a mismatch, we must compare the characters, not
            ! just the bytes.
            call next_char(bp1, i1, i1_byte, c1)
            call next_char(bp2, i2, i2_byte, c2)

            if (fold) then
                c1 = case_canon(c1)
                c2 = case_canon(c2)
            end if

            if (c1 /= c2) then
                if (c1 < c2) then
                    result = -1 - chars
                else
                    result = chars + 1
                end if

                return
            end if

            chars = chars + 1
        end do

        ! The strings match as far as they go.  If one is shorter, that one
        ! is less.
        if (chars < endp1 - begp1) then
            result = chars + 1
        else if (chars < endp2 - begp2) then
            result = -chars - 1
        end if

    contains

        !> The buffer slot B of BUFFER (default the current buffer) and the
        !> ordered positions FROM and TO of START..END in it (default its
        !> accessible text).
        subroutine find_substring(buffer, start, end, b, from, to)
            type(handle_type), intent(in), optional :: buffer
            integer(int64), intent(in), optional :: start, end
            integer(int64), intent(out) :: b, from, to
            integer(int64) :: temp, given_from, given_to

            b = current
            from = 0
            to = 0

            if (present(buffer)) then
                if (.not. is_buffer_live(buffer)) then
                    call set_error(status, FM_GENERIC_ERROR, "Selecting deleted buffer")
                    return
                end if

                call resolve_buffer(buffer, b, status)
                if (.not. status%ok()) return
            end if

            from = buffers(b)%begv
            to = buffers(b)%zv
            if (present(start)) from = start
            if (present(end)) to = end
            given_from = from
            given_to = to

            if (from > to) then
                temp = from
                from = to
                to = temp
            end if

            if (.not. (buffers(b)%begv <= from .and. to <= buffers(b)%zv)) call range_error(status, given_from, given_to)
        end subroutine find_substring

        !> The character C at I (byte I_BYTE) of buffer slot B, made
        !> multibyte; I and I_BYTE move past it.
        subroutine next_char(b, i, i_byte, c)
            integer(int64), intent(in) :: b
            integer(int64), intent(inout) :: i, i_byte
            integer(int32), intent(out) :: c

            if (buffers(b)%multibyte) then
                c = buf_fetch_char(buffers(b), i_byte)
                i_byte = i_byte + buf_next_char_len(buffers(b), i_byte)
            else
                c = unibyte_to_char(buf_fetch_byte(buffers(b), i_byte))
                i_byte = i_byte + 1
            end if

            i = i + 1
        end subroutine next_char
    end subroutine compare_buffer_substrings

    ! ------------------------------------------------------------------
    ! Characters and strings
    ! ------------------------------------------------------------------

    !> Convert arg C to a string containing that character.
    !> Port of editfns.c:Fchar_to_string.
    subroutine char_to_string(c, string, status)
        integer(int32), intent(in) :: c
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status
        character(len=MAX_MULTIBYTE_LENGTH) :: bytes
        integer(int32) :: nbytes

        string = ""

        if (.not. is_char_valid(c)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        call char_string(c, bytes, nbytes, status)
        if (.not. status%ok()) return
        string = bytes(1:nbytes)
    end subroutine char_to_string

    !> Return the first character in STRING (0 if it is empty).
    !> Port of editfns.c:Fstring_to_char.
    integer(int32) function string_to_char(string) result(c)
        character(len=*), intent(in) :: string

        c = 0
        if (len(string) > 0) c = string_char(string, 1_int64)
    end function string_to_char

    !> Return true if two characters match, optionally ignoring case.
    !> Both arguments must be characters (i.e. integers).
    !> Case is ignored if `case_fold_search' is true in the current buffer.
    !> Port of editfns.c:Fchar_equal.
    logical function is_char_equal(c1, c2, status)
        integer(int32), intent(in) :: c1, c2
        type(status_type), intent(out) :: status
        integer(int32) :: i1, i2

        is_char_equal = .false.

        ! Check they're chars, not just integers, otherwise we could get
        ! array bounds violations in downcase.
        if (.not. is_char_valid(c1) .or. .not. is_char_valid(c2)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        if (c1 == c2) then
            is_char_equal = .true.
            return
        end if

        if (.not. case_fold_search%value) return
        i1 = c1
        i2 = c2

        ! It is possible to compare multibyte characters even when the
        ! current buffer is unibyte.  Unfortunately this is ambiguous for
        ! characters between 128 and 255, as they could be either eight-bit
        ! raw bytes or Latin-1 characters.  Assume the former for now (see
        ! Bug#17011, and also casefiddle.c's casify_object).
        if (.not. buffers(current)%multibyte) then
            if (is_single_byte_char(i1)) i1 = unibyte_to_char(i1)
            if (is_single_byte_char(i2)) i2 = unibyte_to_char(i2)
        end if

        is_char_equal = downcase(i1) == downcase(i2)
    end function is_char_equal

    !> Return the translation of MSGID (plural MSGID_PLURAL) depending on N.
    !> MSGID is the singular form of the string to be converted;
    !> use it as the key for the search in the translation catalog.
    !> MSGID_PLURAL is the plural form.  Use N to select the proper translation.
    !> If no message catalog is found, MSGID is returned if N is equal to 1,
    !> otherwise MSGID_PLURAL.
    !> Port of editfns.c:Fngettext.
    function ngettext(msgid, msgid_plural, n) result(text)
        character(len=*), intent(in) :: msgid, msgid_plural
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: text

        ! Placeholder implementation, as upstream's: no catalog.
        if (n == 1) then
            text = msgid
        else
            text = msgid_plural
        end if
    end function ngettext

    ! ------------------------------------------------------------------
    ! Line positions
    ! ------------------------------------------------------------------

    !> Start of the current line, after moving `n' - 1 lines (default 1);
    !> `count' receives the number of lines scanned (n - 1).
    !> Port of editfns.c:bol.
    integer(int64) function bol(n, count)
        integer(int64), intent(in), optional :: n
        integer(int64), intent(out), optional :: count
        integer(int64) :: cnt, charpos, bytepos, counted

        cnt = 0
        if (present(n)) cnt = max(-BUF_BYTES_MAX, min(n - 1, BUF_BYTES_MAX))
        if (present(count)) count = cnt
        counted = scan_newline_from_point(cnt, charpos, bytepos)
        bol = charpos
    end function bol

    !> Position of the first character on the current line (after moving
    !> `n' - 1 lines first).  Does not move point.  Port of editfns.c:Fpos_bol.
    integer(int64) function pos_bol(n)
        integer(int64), intent(in), optional :: n

        pos_bol = bol(n)
    end function pos_bol

    !> Like `pos_bol', but constrained to the field point is in (so it
    !> stops after a minibuffer prompt).
    !> Port of editfns.c:Fline_beginning_position.
    integer(int64) function line_beginning_position(n)
        integer(int64), intent(in), optional :: n
        integer(int64) :: count, charpos

        charpos = bol(n, count)
        line_beginning_position = constrain_to_field(charpos, buffers(current)%pt, count /= 0, .true.)
    end function line_beginning_position

    !> Port of editfns.c:eol.
    integer(int64) function eol(n)
        integer(int64), intent(in), optional :: n
        integer(int64) :: cnt

        cnt = 1
        if (present(n)) cnt = max(-BUF_BYTES_MAX, min(n, BUF_BYTES_MAX))
        if (cnt <= 0) cnt = cnt - 1
        eol = find_before_next_newline(buffers(current)%pt, cnt)
    end function eol

    !> Position of the last character on the current line (after moving
    !> `n' - 1 lines first).  Does not move point.  Port of editfns.c:Fpos_eol.
    integer(int64) function pos_eol(n)
        integer(int64), intent(in), optional :: n

        pos_eol = eol(n)
    end function pos_eol

    !> Like `pos_eol', but constrained to the field point is in.
    !> Port of editfns.c:Fline_end_position.
    integer(int64) function line_end_position(n)
        integer(int64), intent(in), optional :: n

        line_end_position = constrain_to_field(eol(n), buffers(current)%pt, .false., .true.)
    end function line_end_position

    ! ------------------------------------------------------------------
    ! Fields: runs of text with the same `field` property (text
    ! properties only, as there are no overlays yet).
    ! ------------------------------------------------------------------

    !> The `field' name.  Not a port.
    function qfield() result(v)
        type(value_type) :: v

        v = value_type(name="field")
    end function qfield

    !> `prop' of the current buffer's character at `pos' (unallocated outside the
    !> accessible text); get_char_property without overlays.  Not a port.
    function char_property(pos, prop) result(v)
        integer(int64), intent(in) :: pos
        type(value_type), intent(in) :: prop
        type(value_type) :: v
        type(status_type) :: status

        call get_text_property(pos, prop, v, status)
    end function char_property

    !> The value of `prop' that a character inserted at `position' would
    !> get from its neighbors.  Port of editfns.c:Fget_pos_property (text
    !> properties only).
    function get_pos_property(position, prop) result(v)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        type(value_type) :: v
        integer :: stickiness

        stickiness = text_property_stickiness(prop, position)

        if (stickiness > 0) then
            v = char_property(position, prop)
        else if (stickiness < 0 .and. position > buffers(current)%begv) then
            v = char_property(position - 1, prop)
        else
            v = value_type()
        end if
    end function get_pos_property

    !> The field around `pos' (default point): `beginning' and `end', limited
    !> by `beginning_limit' and `end_limit'.  With `merge_at_boundary', a
    !> position at the start of a field belongs to the previous one, and
    !> `boundary' fields are skipped.  Port of editfns.c:find_field.
    subroutine find_field(pos, merge_at_boundary, beginning, end, beginning_limit, end_limit)
        integer(int64), intent(in) :: pos
        logical, intent(in) :: merge_at_boundary
        integer(int64), intent(out) :: beginning, end
        integer(int64), intent(in), optional :: beginning_limit, end_limit
        type(value_type) :: before_field, after_field, field, boundary
        integer(int64) :: p
        logical :: at_field_start, at_field_end

        at_field_start = .false.
        at_field_end = .false.
        boundary = value_type(name="boundary")
        after_field = char_property(pos, qfield())

        if (pos > buffers(current)%begv) then
            before_field = char_property(pos - 1, qfield())
        else
            ! Not nil: a buffer may start with a non-sticky field.
            before_field = after_field
        end if

        if (.not. merge_at_boundary) then
            field = get_pos_property(pos, qfield())
            if (.not. value_eq(field, after_field)) at_field_end = .true.
            if (.not. value_eq(field, before_field)) at_field_start = .true.

            ! A field without a value between ones with a value is a field not meant for
            ! editing (like a prompt), not a zero-length field.
            if (.not. allocated(field%a) .and. at_field_start .and. at_field_end) then
                at_field_end = .false.
                at_field_start = .false.
            end if
        end if

        if (at_field_start) then
            beginning = pos
        else
            p = pos

            if (merge_at_boundary .and. value_eq(before_field, boundary)) then
                p = previous_single_char_property_change(p, qfield(), beginning_limit)
            end if

            beginning = previous_single_char_property_change(p, qfield(), beginning_limit)
        end if

        if (at_field_end) then
            end = pos
        else
            p = pos

            if (merge_at_boundary .and. value_eq(after_field, boundary)) then
                p = next_single_char_property_change(p, qfield(), end_limit)
            end if

            end = next_single_char_property_change(p, qfield(), end_limit)
        end if
    end subroutine find_field

    !> Start of the field around `pos'.  Port of editfns.c:Ffield_beginning.
    integer(int64) function field_beginning(pos, escape_from_edge, limit)
        integer(int64), intent(in) :: pos
        logical, intent(in) :: escape_from_edge
        integer(int64), intent(in), optional :: limit
        integer(int64) :: end

        call find_field(pos, escape_from_edge, field_beginning, end, beginning_limit=limit)
    end function field_beginning

    !> End of the field around `pos'.  Port of editfns.c:Ffield_end.
    integer(int64) function field_end(pos, escape_from_edge, limit)
        integer(int64), intent(in) :: pos
        logical, intent(in) :: escape_from_edge
        integer(int64), intent(in), optional :: limit
        integer(int64) :: beginning

        call find_field(pos, escape_from_edge, beginning, field_end, end_limit=limit)
    end function field_end

    !> Return the contents of the field surrounding POS as a string.
    !> A field is a region of text with the same `field' property.
    !> Port of editfns.c:Ffield_string.
    function field_string(pos) result(string)
        integer(int64), intent(in) :: pos
        type(value_type) :: string
        integer(int64) :: beginning, end

        call find_field(pos, .false., beginning, end)
        call make_buffer_string(beginning, end, .true., string)
    end function field_string

    !> Return the contents of the field around POS, without text properties.
    !> A field is a region of text with the same `field' property.
    !> Port of editfns.c:Ffield_string_no_properties.
    function field_string_no_properties(pos) result(text)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: text
        integer(int64) :: beginning, end

        call find_field(pos, .false., beginning, end)
        text = buf_bytes(buffers(current), char_to_byte(beginning), char_to_byte(end))
    end function field_string_no_properties

    !> Delete the field around `pos'.  Port of editfns.c:Fdelete_field.
    recursive subroutine delete_field(pos, status)
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call find_field(pos, .false., beginning, end)
        if (beginning /= end) call del_range(beginning, end, status)
    end subroutine delete_field

    !> `new_pos' moved, if needed, to stay in the field of `old_pos'.  With
    !> `escape_from_edge', `old_pos' at a field edge may count as in the
    !> neighboring field; with `only_in_line', constrain only if the
    !> field edge is on the same line.  Port of
    !> editfns.c:Fconstrain_to_field (NEW-POS given; no
    !> INHIBIT-CAPTURE-PROPERTY).
    integer(int64) function constrain_to_field(new_pos, old_pos, escape_from_edge, only_in_line) &
        result(pos)
        integer(int64), intent(in) :: new_pos, old_pos
        logical, intent(in) :: escape_from_edge, only_in_line
        integer(int64) :: field_bound, counted, nl
        logical :: fwd

        pos = new_pos
        if (inhibit_field_text_motion .or. new_pos == old_pos) return

        if (vtag(char_property(new_pos, qfield())) == V_NULL .and. vtag(char_property(old_pos, qfield())) == V_NULL) then
            ! hazard-ok(short-circuit): char_property() takes any position.
            if (.not. (new_pos > buffers(current)%begv .and. &
                       vtag(char_property(new_pos - 1, qfield())) /= V_NULL)) then
                ! hazard-ok(short-circuit): char_property() takes any position.
                if (.not. (old_pos > buffers(current)%begv .and. &
                           vtag(char_property(old_pos - 1, qfield())) /= V_NULL)) return
            end if
        end if

        ! NEW_POS may not be in OLD_POS's field: try to move it there.
        fwd = new_pos > old_pos

        if (fwd) then
            field_bound = field_end(old_pos, escape_from_edge, new_pos)
        else
            field_bound = field_beginning(old_pos, escape_from_edge, new_pos)
        end if

        ! Unless escaping put the bound beyond NEW_POS already, constrain
        ! it (with ONLY_IN_LINE, only to a bound on the same line).
        if (merge(fwd,.not. fwd, field_bound < new_pos)) then
            if (only_in_line) then
                if (fwd) then
                    nl = find_newline(new_pos, field_bound, -1_int64, counted)
                else
                    nl = find_newline(new_pos, field_bound, 1_int64, counted)
                end if

                if (counted /= 0) return
            end if

            pos = field_bound
        end if
    end function constrain_to_field

    !> From START to END, replace FROMCHAR with TOCHAR each time it occurs.
    !> If optional arg NOUNDO is true, don't record this change for undo
    !> and don't mark the buffer as really changed.
    !> Both characters must have the same length of multi-byte form.  Port of
    !> editfns.c:Fsubst_char_in_region (compositions are not ported).
    recursive subroutine subst_char_in_region(start, end, fromchar, tochar, status, noundo)
        integer(int64), intent(in) :: start, end
        integer(int32), intent(in) :: fromchar, tochar
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noundo
        integer(int64) :: pos, pos_byte, end_byte, pos_byte_next, s, e, k, m, changed, last_changed
        integer(int32) :: nlen, tolen
        character(len=MAX_MULTIBYTE_LENGTH) :: fromstr, tostr
        character(len=:), allocatable :: saved_filename
        character(len=64) :: text
        logical :: no_undo, saved_undo, had_filename, rescan

        status = status_type()
        no_undo = .false.
        if (present(noundo)) no_undo = noundo
        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return

        if (.not. is_char_valid(fromchar)) then
            write (text, '("expected integer (a character code), got ", i0)') fromchar
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, trim(text))
            return
        end if

        if (.not. is_char_valid(tochar)) then
            write (text, '("expected integer (a character code), got ", i0)') tochar
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, trim(text))
            return
        end if

        if (buffers(current)%multibyte) then
            call char_string(fromchar, fromstr, nlen, status)
            if (.not. status%ok()) return
            call char_string(tochar, tostr, tolen, status)
            if (.not. status%ok()) return

            if (tolen /= nlen) then
                call set_error(status, FM_GENERIC_ERROR, &
                               format_message("Characters in `subst_char_in_region' have different byte-lengths"))
                return
            end if
        else
            nlen = 1
            fromstr(1:1) = achar(iand(fromchar, 255_int32))
            tostr(1:1) = achar(iand(tochar, 255_int32))
        end if

        ! Keep track of the first change in the buffer: 0 if we haven't
        ! found it yet, < 0 if we've found it and run the before-change
        ! functions, > 0 if we've actually performed it (its position).
        changed = 0
        last_changed = 0
        ! If we don't want undo, turn off putting stuff on the list.
        ! Also inhibit locking the file.
        saved_undo = buffers(current)%undo_enabled
        had_filename = allocated(buffers(current)%filename)

        if (no_undo) then
            buffers(current)%undo_enabled = .false.
            if (had_filename) call move_alloc(buffers(current)%filename, saved_filename)
        end if

        do
            rescan = .false.
            pos = s
            pos_byte = char_to_byte(pos)
            end_byte = char_to_byte(e)

            do while (pos_byte < end_byte)
                pos_byte_next = pos_byte

                if (buffers(current)%multibyte) then
                    pos_byte_next = pos_byte_next + buf_next_char_len(buffers(current), pos_byte_next)
                else
                    pos_byte_next = pos_byte_next + 1
                end if

                if (pos_byte_next - pos_byte == nlen) then
                    if (buf_bytes(buffers(current), pos_byte, pos_byte_next) == fromstr(1:nlen)) then
                        if (changed < 0) then
                            ! We've already seen this and run the
                            ! before-change functions; this time we only
                            ! need to record the actual position.
                            changed = pos
                        else if (changed == 0) then
                            changed = -1
                            call modify_text(pos, e, status)
                            if (.not. status%ok()) exit

                            if (no_undo) then
                                m = buffers(current)%text%modiff
                                if (buffers(current)%text%save_modiff == m - 1) buffers(current)%text%save_modiff = m
                            end if

                            ! The before-change functions may have moved the
                            ! gap or even modified the buffer so we should
                            ! start over.
                            rescan = .true.
                            exit
                        end if

                        if (.not. no_undo) call record_change(pos, 1_int64)

                        do k = 0, nlen - 1
                            associate (t => buffers(current)%text)
                                t%beginning(buffer_byte_index(buffers(current), &
                                                              pos_byte + k):buffer_byte_index(buffers(current), pos_byte + k)) = &
                                    tostr(k + 1:k + 1)
                            end associate
                        end do

                        last_changed = pos + 1
                    end if
                end if

                pos_byte = pos_byte_next
                pos = pos + 1
            end do

            if (.not. rescan) exit
            s = start
            e = end
            call validate_region(s, e, status)
            if (.not. status%ok()) exit
        end do

        if (status%ok() .and. changed > 0) then
            call signal_after_change(changed, last_changed - changed, last_changed - changed, status)
        end if

        ! Restore the undo list and the file name, also after an error
        ! (editfns.c:subst_char_in_region_unwind and
        ! editfns.c:subst_char_in_region_unwind_1).
        if (no_undo) then
            buffers(current)%undo_enabled = saved_undo

            if (had_filename) then
                call move_alloc(saved_filename, buffers(current)%filename)
            else if (allocated(buffers(current)%filename)) then
                deallocate (buffers(current)%filename)
            end if
        end if
    end subroutine subst_char_in_region

    ! ------------------------------------------------------------------
    ! Translating and transposing
    ! ------------------------------------------------------------------

    !> Internal use only.
    !> From START to END, translate characters according to TABLE.
    !> TABLE is a string; the Nth character in it is the mapping for the
    !> character with code N.  CHANGED gets the number of characters
    !> changed.  Port of editfns.c:Ftranslate_region_internal (a string
    !> TABLE).
    recursive subroutine translate_region_by_string(start, end, table, changed, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: table
        integer(int64), intent(out) :: changed
        type(status_type), intent(out) :: status
        type(handle_type) :: none

        call translate_region(start, end, none, table, changed, status)
    end subroutine translate_region_by_string

    !> Internal use only.
    !> From START to END, translate characters according to TABLE.
    !> TABLE is a char-table whose purpose is "translation_table"; the
    !> element for character N is the mapping for the character with code
    !> N.  CHANGED gets the number of characters changed.  Port of
    !> editfns.c:Ftranslate_region_internal (a char-table TABLE).
    recursive subroutine translate_region_by_char_table(start, end, table, changed, status)
        integer(int64), intent(in) :: start, end
        type(handle_type), intent(in) :: table
        integer(int64), intent(out) :: changed
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: purpose

        changed = 0
        purpose = char_table_subtype(table, status)

        if (.not. status%ok() .or. purpose /= "translation_table") then
            call set_error(status, FM_GENERIC_ERROR, "Not a translation table")
            return
        end if

        call translate_region(start, end, table, "", changed, status)
    end subroutine translate_region_by_char_table

    !> Translate the characters from START to END by the char-table TABLE,
    !> or, when it is the null handle, by the string STRING_TABLE; CHANGED
    !> gets the number of characters changed.  Port of
    !> editfns.c:Ftranslate_region_internal (compositions and tree-sitter
    !> are not ported).
    recursive subroutine translate_region(start, end, table, string_table, changed, status)
        integer(int64), intent(in) :: start, end
        type(handle_type), intent(in) :: table
        character(len=*), intent(in) :: string_table
        integer(int64), intent(out) :: changed
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tt
        character(len=MAX_MULTIBYTE_LENGTH) :: bytes
        integer(int64) :: s, e, pos, pos_byte, end_pos, translatable_chars, k, string_byte
        integer(int64) :: val
        integer(int32) :: oc, nc, len1, str_len
        logical :: multibyte, string_multibyte

        changed = 0
        translatable_chars = int(MAX_CHAR, int64) + 1
        multibyte = buffers(current)%multibyte
        string_multibyte = .false.
        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return

        if (table%index == 0) then
            tt = string_table
            ! A string with a non-ASCII byte reads as multibyte text.
            string_multibyte = verify(tt, ascii_chars()) /= 0

            if (.not. multibyte .and. string_multibyte) then
                call string_make_unibyte(tt)
                string_multibyte = .false.
            end if

            translatable_chars = min(translatable_chars, len(tt, kind=int64))
        end if

        pos = s
        pos_byte = char_to_byte(pos)
        end_pos = e
        call modify_text(pos, end_pos, status)
        if (.not. status%ok()) return

        do while (pos < end_pos)
            if (multibyte) then
                call string_char_and_length(buffers(current)%text%beginning, &
                                            buffer_byte_index(buffers(current), pos_byte), oc, len1)
            else
                oc = buf_fetch_byte(buffers(current), pos_byte)
                len1 = 1
            end if

            if (oc < translatable_chars) then
                nc = oc

                if (table%index == 0) then
                    if (string_multibyte) then
                        ! (A code beyond the string's characters keeps its
                        ! character: upstream reads past the string there.)
                        call string_char_byte(tt, int(oc, int64), string_byte)

                        if (string_byte > 0) then
                            call string_char_and_length(tt, string_byte, nc, str_len)
                            bytes = tt(string_byte:string_byte + str_len - 1)
                        end if
                    else
                        nc = ichar(tt(oc + 1:oc + 1), kind=int32)

                        if (nc > 127 .and. multibyte) then
                            call char_string(unibyte_to_char(nc), bytes, str_len, status)
                        else
                            str_len = 1
                            bytes = tt(oc + 1:oc + 1)
                        end if
                    end if
                else
                    val = char_table_ref(table, int(oc))

                    if (val >= 0 .and. val <= MAX_CHAR) then
                        nc = int(val, int32)
                        call char_string(nc, bytes, str_len, status)
                    end if
                end if

                if (nc /= oc) then
                    ! Simple one char to one char translation.
                    if (len1 /= str_len) then
                        ! This is less efficient, because it moves the gap,
                        ! but it should handle multibyte characters correctly.
                        call replace_range(pos, pos + 1, bytes(1:str_len), .true., status)
                        if (.not. status%ok()) return
                        len1 = str_len
                    else
                        call record_change(pos, 1_int64)
                        k = buffer_byte_index(buffers(current), pos_byte)
                        buffers(current)%text%beginning(k:k + str_len - 1) = bytes(1:str_len)
                        call signal_after_change(pos, 1_int64, 1_int64, status)
                        if (.not. status%ok()) return
                    end if

                    changed = changed + 1
                end if
            end if

            pos_byte = pos_byte + len1
            pos = pos + 1
        end do
    end subroutine translate_region

    !> All 128 ASCII characters.  Not a port.
    pure function ascii_chars() result(chars)
        character(len=128) :: chars
        integer :: k

        do k = 0, 127
            chars(k + 1:k + 1) = achar(k)
        end do
    end function ascii_chars

    !> Byte index in the multibyte text STRING of its character number N
    !> (counting from 0), or 0 if STRING has no such character.
    !> Not a port (fns.c's string_char_to_byte, within the string).
    pure subroutine string_char_byte(string, n, byte)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: n
        integer(int64), intent(out) :: byte
        integer(int64) :: k, i

        byte = 0
        i = 1

        do k = 0, n - 1
            if (i > len(string, kind=int64)) return
            i = i + bytes_by_char_head(ichar(string(i:i), kind=int32))
        end do

        if (i <= len(string, kind=int64)) byte = i
    end subroutine string_char_byte

    !> Convert the multibyte text STRING to unibyte: each character becomes
    !> the byte of its low 8 bits (a raw byte its byte).  Not a port
    !> (fns.c's string_make_unibyte, through character.h's CHAR_TO_BYTE8).
    pure subroutine string_make_unibyte(string)
        character(len=:), allocatable, intent(inout) :: string
        character(len=:), allocatable :: unibyte
        integer(int64) :: i, n
        integer(int32) :: c, clen

        allocate (character(len=len(string)) :: unibyte)
        i = 1
        n = 0

        do while (i <= len(string, kind=int64))
            call string_char_and_length(string, i, c, clen)
            n = n + 1
            unibyte(n:n) = achar(char_to_byte8(c))
            i = i + clen
        end do

        string = unibyte(1:n)
    end subroutine string_make_unibyte

    !> Transpose the markers in two regions of the current buffer, and
    !> adjust the ones between them if necessary (i.e.: if the regions
    !> differ in size).
    !>
    !> START1, END1 are the character positions of the first region.
    !> START1_BYTE, END1_BYTE are the byte positions.
    !> START2, END2 are the character positions of the second region.
    !> START2_BYTE, END2_BYTE are the byte positions.
    !>
    !> Traverses the entire marker list of the buffer to do so, adding an
    !> appropriate amount to some, subtracting from some, and leaving the
    !> rest untouched.  It's the caller's job to ensure that
    !> START1 <= END1 <= START2 <= END2.  Port of
    !> editfns.c:transpose_markers.
    subroutine transpose_markers(start1, end1, start2, end2, start1_byte, end1_byte, start2_byte, end2_byte)
        integer(int64), intent(in) :: start1, end1, start2, end2, start1_byte, end1_byte, start2_byte, end2_byte
        integer(int64) :: amt1, amt1_byte, amt2, amt2_byte, diff, diff_byte, mpos, m

        ! Update point as if it were a marker.
        associate (b => buffers(current))
            if (b%pt >= start1) then
                if (b%pt < end1) then
                    call temp_set_point_both(b, b%pt + (end2 - end1), b%pt_byte + (end2_byte - end1_byte))
                else if (b%pt < start2) then
                    call temp_set_point_both(b, b%pt + (end2 - start2) - (end1 - start1), &
                                             b%pt_byte + (end2_byte - start2_byte) - (end1_byte - start1_byte))
                else if (b%pt < end2) then
                    call temp_set_point_both(b, b%pt - (start2 - start1), b%pt_byte - (start2_byte - start1_byte))
                end if
            end if
        end associate

        ! The difference between the region's lengths.
        diff = (end2 - start2) - (end1 - start1)
        diff_byte = (end2_byte - start2_byte) - (end1_byte - start1_byte)
        ! For shifting each marker in a region by the length of the other
        ! region plus the distance between the regions.
        amt1 = (end2 - start2) + (start2 - end1)
        amt2 = (end1 - start1) + (start2 - end1)
        amt1_byte = (end2_byte - start2_byte) + (start2_byte - end1_byte)
        amt2_byte = (end1_byte - start1_byte) + (start2_byte - end1_byte)
        m = buffers(current)%text%markers

        do while (m /= 0)
            associate (marker => markers(m))
                mpos = marker%bytepos

                if (mpos >= start1_byte .and. mpos < end2_byte) then
                    if (mpos < end1_byte) then
                        mpos = mpos + amt1_byte
                    else if (mpos < start2_byte) then
                        mpos = mpos + diff_byte
                    else
                        mpos = mpos - amt2_byte
                    end if

                    marker%bytepos = mpos
                end if

                mpos = marker%charpos

                if (mpos >= start1 .and. mpos < end2) then
                    if (mpos < end1) then
                        mpos = mpos + amt1
                    else if (mpos < start2) then
                        mpos = mpos + diff
                    else
                        mpos = mpos - amt2
                    end if
                end if

                marker%charpos = mpos
                m = marker%next
            end associate
        end do
    end subroutine transpose_markers

    !> Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
    !> The regions should not be overlapping, because the size of the buffer is
    !> never changed in a transposition.
    !>
    !> Optional sixth arg LEAVE_MARKERS, if true, means don't update
    !> any markers that happen to be located in the regions.
    !>
    !> Transposing beyond buffer boundaries is an error.
    !>
    !> Interactively, STARTR1 and ENDR1 are point and mark; STARTR2 and ENDR2
    !> are the last two marks pushed to the mark ring; LEAVE_MARKERS is false.
    !> If a prefix argument N is given, STARTR2 and ENDR2 are the two
    !> successive marks N entries back in the mark ring.  A negative prefix
    !> argument instead counts forward from the oldest mark in the mark
    !> ring.  Port of editfns.c:Ftranspose_regions (compositions and
    !> tree-sitter are not ported).
    recursive subroutine transpose_regions(startr1, endr1, startr2, endr2, status, leave_markers)
        integer(int64), intent(in) :: startr1, endr1, startr2, endr2
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: leave_markers
        integer(int64) :: start1, end1, start2, end2, start1_byte, end1_byte, start2_byte, end2_byte
        integer(int64) :: gap, len1, len_mid, len2, len1_byte, len_mid_byte, len2_byte, glumph, i
        integer(int64), allocatable :: starts1(:), ends1(:), starts2(:), ends2(:), starts_mid(:), ends_mid(:)
        type(value_type), allocatable :: plists1(:), plists2(:), plists_mid(:)
        character(len=:), allocatable :: text
        logical :: leave

        leave = .false.
        if (present(leave_markers)) leave = leave_markers
        start1 = startr1
        end1 = endr1
        call validate_region(start1, end1, status)
        if (.not. status%ok()) return
        start2 = startr2
        end2 = endr2
        call validate_region(start2, end2, status)
        if (.not. status%ok()) return

        ! Swap the regions if they're reversed.
        if (start2 < end1) then
            glumph = start1
            start1 = start2
            start2 = glumph
            glumph = end1
            end1 = end2
            end2 = glumph
        end if

        len1 = end1 - start1
        len2 = end2 - start2

        if (start2 < end1) then
            call set_error(status, FM_GENERIC_ERROR, "Transposed regions overlap")
            return
        else if ((start1 == end1 .or. start2 == end2) .and. end1 == start2) then
            ! Nothing to change for adjacent regions with one being empty.
            return
        end if

        start1_byte = char_to_byte(start1)
        end2_byte = char_to_byte(end2)
        ! Run the before-change hooks *before* we move the gap.
        call modify_text(start1, end2, status)
        if (.not. status%ok()) return
        ! Make sure the gap won't interfere, by moving it out of the text
        ! we will operate on.  (The gap is read after the before-change
        ! hooks, which may have moved it; upstream reads it before them.)
        gap = buffers(current)%text%gpt

        if (start1 < gap .and. gap < end2) then
            if (gap - start1 < end2 - gap) then
                call move_gap_both(start1, start1_byte)
            else
                call move_gap_both(end2, end2_byte)
            end if
        end if

        start2_byte = char_to_byte(start2)
        end1_byte = char_to_byte(end1)
        len1_byte = end1_byte - start1_byte
        len2_byte = end2_byte - start2_byte
        ! Maintaining undo and text properties needs to be done in terms of
        ! characters, swapping text in memory in terms of bytes.  Regions
        ! of equal size in characters leave the text between them alone.
        call copy_intervals(buffers(current)%text%intervals, start1, len1, starts1, ends1, plists1)
        call copy_intervals(buffers(current)%text%intervals, start2, len2, starts2, ends2, plists2)
        len_mid = start2 - end1
        len_mid_byte = start2_byte - end1_byte

        if (len1 == len2) then
            if (end1 == start2) then
                ! Merge the two parts into a single one.
                call record_change(start1, end2 - start1)
            else
                call record_change(start1, len1)
                call record_change(start2, len2)
            end if
        else
            ! Regions have different length, character-wise.  Handle undo
            ! and text properties for both regions as one long piece of
            ! text spanning both regions and the mid.  But while doing so,
            ! save the intervals of the mid to later restore them in their
            ! new position.
            call record_change(start1, end2 - start1)
            call copy_intervals(buffers(current)%text%intervals, end1, len_mid, starts_mid, ends_mid, plists_mid)
        end if

        ! The gap is out of the way: the text is one run of bytes.
        i = buffer_byte_index(buffers(current), start1_byte)

        associate (t => buffers(current)%text%beginning)
            text = t(i:i + end2_byte - start1_byte - 1)
            t(i:i + end2_byte - start1_byte - 1) = text(len1_byte + len_mid_byte + 1:)// &
                                                   text(len1_byte + 1:len1_byte + len_mid_byte)//text(1:len1_byte)
        end associate

        call clear_charpos_cache(current)

        ! Grafting the copies (without inheriting) clears the text
        ! properties of each part first, as upstream's set_text_properties_1
        ! with no properties does.
        associate (t => buffers(current)%text)
            ! Restore intervals of the mid.
            if (len1 /= len2) call graft_intervals_into_buffer(t%intervals, t%z, starts_mid, ends_mid, plists_mid, &
                                                               start1 + len2, len_mid, .false.)
            call graft_intervals_into_buffer(t%intervals, t%z, starts1, ends1, plists1, end2 - len1, len1, .false.)
            call graft_intervals_into_buffer(t%intervals, t%z, starts2, ends2, plists2, start1, len2, .false.)
        end associate

        if (.not. leave) then
            ! Since the undo info doesn't record the transposition as its
            ! own operation, undoing it does not transpose the markers back.
            call transpose_markers(start1, end1, start2, end2, start1_byte, start1_byte + len1_byte, start2_byte, &
                                   start2_byte + len2_byte)
        else
            ! The character positions of the markers remain intact, but we
            ! still need to update their byte positions, because the
            ! transposed regions might include multibyte sequences which
            ! make some original byte positions of the markers invalid.
            call adjust_markers_bytepos(start1, start1_byte, end2_byte)
        end if

        call signal_after_change(start1, end2 - start1, end2 - start1, status)
    end subroutine transpose_regions

    ! ------------------------------------------------------------------
    ! Formatting
    ! ------------------------------------------------------------------

    !> Add the integer N after SELF's arguments.  Not a port.
    subroutine format_arguments_append_integer(self, n)
        class(format_arguments_type), intent(inout) :: self
        integer(int64), intent(in) :: n
        type(format_argument_type) :: argument

        argument%tag = FORMAT_ARGUMENT_INTEGER
        argument%integer_number = n
        call append_format_argument(self, argument)
    end subroutine format_arguments_append_integer

    !> Add the default integer N after SELF's arguments.  Not a port.
    subroutine format_arguments_append_default_integer(self, n)
        class(format_arguments_type), intent(inout) :: self
        integer, intent(in) :: n

        call format_arguments_append_integer(self, int(n, int64))
    end subroutine format_arguments_append_default_integer

    !> Add the real64 X after SELF's arguments.  Not a port.
    subroutine format_arguments_append_real(self, x)
        class(format_arguments_type), intent(inout) :: self
        real(real64), intent(in) :: x
        type(format_argument_type) :: argument

        argument%tag = FORMAT_ARGUMENT_REAL
        argument%real_number = x
        call append_format_argument(self, argument)
    end subroutine format_arguments_append_real

    !> Add the string TEXT (no text properties) after SELF's arguments.
    !> Not a port.
    subroutine format_arguments_append_text(self, text)
        class(format_arguments_type), intent(inout) :: self
        character(len=*), intent(in) :: text
        type(format_argument_type) :: argument

        argument%value = value_type(text=text)
        call append_format_argument(self, argument)
    end subroutine format_arguments_append_text

    !> Add the value V (a string with its text properties, a name, ...)
    !> after SELF's arguments.  Not a port.
    subroutine format_arguments_append_value(self, v)
        class(format_arguments_type), intent(inout) :: self
        type(value_type), intent(in) :: v
        type(format_argument_type) :: argument

        if (vtag(v) == V_INTEGER) then
            argument%tag = FORMAT_ARGUMENT_INTEGER
            argument%integer_number = xfixnum(v)
        else
            argument%value = v
        end if

        call append_format_argument(self, argument)
    end subroutine format_arguments_append_value

    !> Add ARGUMENT after SELF's arguments.  Not a port.
    subroutine append_format_argument(self, argument)
        type(format_arguments_type), intent(inout) :: self
        type(format_argument_type), intent(in) :: argument
        type(format_argument_type), allocatable :: grown(:)
        integer :: k, n

        n = 0
        if (allocated(self%arguments)) n = size(self%arguments)
        allocate (grown(n + 1))

        do k = 1, n
            grown(k) = self%arguments(k)
        end do

        grown(n + 1) = argument
        call move_alloc(grown, self%arguments)
    end subroutine append_format_argument

    !> `format_text' with a character format string: the text alone.
    !> Port of editfns.c:Fformat.
    subroutine format_text_string(string, arguments, result, status)
        character(len=*), intent(in) :: string
        type(format_arguments_type), intent(in) :: arguments
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        call styled_format_text(string, arguments, .false., result, status)
    end subroutine format_text_string

    !> `format_text' with a string value as format string: a string value with
    !> the text properties of the format string and of the arguments.
    !> Port of editfns.c:Fformat.
    subroutine format_text_value(string, arguments, result, status)
        type(value_type), intent(in) :: string
        type(format_arguments_type), intent(in) :: arguments
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status

        call styled_format_value(string, arguments, .false., result, status)
    end subroutine format_text_value

    !> `styled_format' with a character format string: the text alone.
    !> Port of editfns.c:styled_format.
    subroutine styled_format_text(string, arguments, message, result, status)
        character(len=*), intent(in) :: string
        type(format_arguments_type), intent(in) :: arguments
        logical, intent(in) :: message
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(value_type) :: v

        result = ""
        call styled_format_value(value_type(text=string), arguments, message, v, status)
        if (status%ok()) result = xstring(v)
    end subroutine styled_format_text

    !> Implement `format-message' if MESSAGE is true, `format_text' otherwise.
    !> STRING is a string value; RESULT is a string value with the text
    !> properties of STRING (moved to where their text went) and of the
    !> string arguments of %s and %S (extended over their padding on the
    !> right).  Text is multibyte (unibyte strings are deferred).
    !> Port of editfns.c:styled_format.
    subroutine styled_format_value(string, arguments, message, result, status)
        type(value_type), intent(in) :: string
        type(format_arguments_type), intent(in) :: arguments
        logical, intent(in) :: message
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: specification, buffer, text
        integer, allocatable :: discarded(:)
        type(format_spec_type), allocatable :: info(:)
        type(format_argument_type) :: arg
        integer(int64) :: nformat, nargs, n, ispec, nspec, i, format0, num, num_end, field_width, precision, nchars, &
                          nchars_string, nbytes, width, padding
        integer(int32) :: c, clen
        logical :: minus_flag, plus_flag, space_flag, sharp_flag, zero_flag, precision_given, arg_intervals, fmt_props
        character :: conversion
        character(len=20) :: digits

        if (vtag(string) /= V_STRING) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected character(len=*)")
            return
        end if

        specification = xstring(string)
        nformat = len(specification, kind=int64)
        nargs = 0
        if (allocated(arguments%arguments)) nargs = size(arguments%arguments, kind=int64)
        fmt_props = has_intervals(string)
        ! Upper bound on number of format specs.  Each uses at least 2
        ! characters.
        allocate (info(nformat/2 + 1), discarded(nformat))
        discarded = 0
        arg_intervals = .false.
        buffer = ""
        nchars = 0
        ! N is the argument index, ISPEC is the specification index.
        n = 0
        ispec = 0
        nspec = 0
        i = 1

        do while (i <= nformat)
            format0 = i

            if (specification(i:i) /= "%") then
                if (message .and. (specification(i:i) == "`" .or. specification(i:i) == "'")) then
                    ! The `curve' style of `text_quoting_style'.
                    buffer = buffer//format_message(specification(i:i))
                    i = i + 1
                else
                    ! Copy a whole multibyte character.
                    i = i + 1

                    do while (i <= nformat)
                        if (is_char_head(iachar(specification(i:i)))) exit
                        discarded(i) = 2
                        i = i + 1
                    end do

                    buffer = buffer//specification(format0:i - 1)
                end if

                nchars = nchars + 1
                cycle
            end if

            ! General format specifications look like
            !
            !   '%' [field-number] [flags] [field-width] [precision] format
            !
            ! where field-number ::= [0-9]+ '$', flags ::= [-+0# ]+,
            ! field-width ::= [0-9]+ and precision ::= '.' [0-9]*.
            i = i + 1

            if (i <= nformat) then
                if (is_digit(specification(i:i))) then
                    call str2num(specification, i, num, num_end)

                    if (num_end <= nformat) then
                        if (specification(num_end:num_end) == "$") then
                            n = num - 1
                            i = num_end + 1
                        end if
                    end if
                end if
            end if

            minus_flag = .false.
            plus_flag = .false.
            space_flag = .false.
            sharp_flag = .false.
            zero_flag = .false.

            do while (i <= nformat)
                select case (specification(i:i))
                case ("-")
                    minus_flag = .true.
                case ("+")
                    plus_flag = .true.
                case (" ")
                    space_flag = .true.
                case ("#")
                    sharp_flag = .true.
                case ("0")
                    zero_flag = .true.
                case default
                    exit
                end select

                i = i + 1
            end do

            ! Ignore flags when sprintf ignores them.
            space_flag = space_flag .and. .not. plus_flag
            zero_flag = zero_flag .and. .not. minus_flag
            call str2num(specification, i, field_width, num_end)

            if (field_width > STRING_BYTES_BOUND) then
                call set_error(status, FM_GENERIC_ERROR, "Maximum string size exceeded")
                return
            end if

            precision_given = .false.
            precision = huge(0_int64)

            if (num_end <= nformat) then
                if (specification(num_end:num_end) == ".") then
                    precision_given = .true.
                    call str2num(specification, num_end + 1, precision, num_end)
                end if
            end if

            i = num_end

            if (i > nformat) then
                call set_error(status, FM_GENERIC_ERROR, "Format string ends in middle of format specifier")
                return
            end if

            conversion = specification(i:i)
            i = i + 1

            if (conversion == "%") then
                discarded(format0:i - 2) = 1
            else
                discarded(format0:i - 1) = 1
            end if

            info(ispec + 1)%fbeg = format0 - 1

            if (conversion == "%") then
                buffer = buffer//"%"
                nchars = nchars + 1
                cycle
            end if

            n = n + 1

            if (n > nargs) then
                call set_error(status, FM_GENERIC_ERROR, "Not enough arguments for format string")
                return
            end if

            ispec = ispec + 1
            nspec = max(nspec, ispec)

            if (n == 0) then
                ! A field number 0: the format string itself.
                arg = format_argument_type()
                arg%value = string
            else
                arg = arguments%arguments(n)
            end if

            ! For 'S', prin1 the argument, and then treat like 's'.  For
            ! 's', princ any argument that is not a string or name.
            if (conversion == "S" .or. (conversion == "s" .and. .not. (is_string_argument(arg) .or. is_name_argument(arg)))) then
                select case (arg%tag)
                case (FORMAT_ARGUMENT_INTEGER)
                    write (digits, "(i0)") arg%integer_number
                    text = trim(digits)
                case (FORMAT_ARGUMENT_REAL)
                    text = float_to_string(arg%real_number)
                case default
                    text = readable_text(arg%value, noescape=conversion == "s")
                end select

                arg = format_argument_type()
                arg%value = value_type(text=text)
                conversion = "s"
            else if (conversion == "c") then
                if (arg%tag == FORMAT_ARGUMENT_INTEGER) then
                    if (arg%integer_number < 0 .or. arg%integer_number > 127) then
                        if (arg%integer_number > MAX_CHAR) then
                            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
                            return
                        end if

                        call char_to_string(int(arg%integer_number, int32), text, status)
                        if (.not. status%ok()) return
                        arg = format_argument_type()
                        arg%value = value_type(text=text)
                        conversion = "s"
                    end if
                end if

                zero_flag = .false.
            end if

            if (is_name_argument(arg)) then
                text = readable_text(arg%value)
                arg%value = value_type(text=text)
            end if

            if (conversion == "s") then
                text = xstring(arg%value)

                if (precision_given .and. precision == 0) then
                    width = 0
                    nchars_string = 0
                    nbytes = 0
                else
                    ! lisp_string_width up to PRECISION columns.
                    width = 0
                    nchars_string = 0
                    nbytes = 0

                    do while (nbytes < len(text, kind=int64))
                        call string_char_and_length(text, nbytes + 1, c, clen)
                        if (precision_given .and. width + glyph_width(c) > precision) exit
                        width = width + glyph_width(c)
                        nbytes = nbytes + clen
                        nchars_string = nchars_string + 1
                    end do
                end if

                padding = max(0_int64, field_width - width)
                ! If the format spec has properties, the padding on the
                ! left counts in the info array.
                if (fmt_props) info(ispec)%start = nchars

                if (.not. minus_flag) then
                    buffer = buffer//repeat(" ", int(padding))
                    nchars = nchars + padding
                end if

                ! If the properties will come from the argument, they are
                ! not extended to the left over the padding.
                if (.not. fmt_props) info(ispec)%start = nchars
                buffer = buffer//text(1:nbytes)
                nchars = nchars + nchars_string

                if (minus_flag) then
                    buffer = buffer//repeat(" ", int(padding))
                    nchars = nchars + padding
                end if

                info(ispec)%end = nchars

                ! If this argument has text properties, record where in
                ! the result string it appears.
                if (has_intervals(arg%value)) then
                    info(ispec)%intervals = .true.
                    info(ispec)%argument = arg%value
                    arg_intervals = .true.
                end if

                cycle
            end if

            if (scan(conversion, "cdefgioxXbB") == 0) then
                call string_char_and_length(specification, i - 1, c, clen)
                call set_error(status, FM_GENERIC_ERROR, "Invalid format operation %"//specification(i - 1:i - 2 + clen))
                return
            end if

            if (.not. (arg%tag == FORMAT_ARGUMENT_INTEGER .or. (arg%tag == FORMAT_ARGUMENT_REAL .and. conversion /= "c"))) then
                call set_error(status, FM_GENERIC_ERROR, "Format specifier doesn't match argument type")
                return
            end if

            call format_number(arg, conversion, field_width, precision, precision_given, minus_flag, plus_flag, &
                               space_flag, sharp_flag, zero_flag, text, status)
            if (.not. status%ok()) return
            info(ispec)%start = nchars
            buffer = buffer//text
            nchars = nchars + len(text, kind=int64)
            info(ispec)%end = nchars
        end do

        result = value_type(text=buffer)
        if (fmt_props) call translate_format_properties(string, discarded, info(1:nspec), result)

        ! Add text properties from arguments.
        if (arg_intervals) then
            do i = 1, nspec
                if (info(i)%intervals) call add_argument_properties(info(i), result)
            end do
        end if
    end subroutine styled_format_value

    !> The output of a numeric %-sequence (CONVERSION one of c d e f g i
    !> o x X b B) for ARG, an integer or a real64, with its width, precision
    !> and flags.  Port of the numeric part of editfns.c:styled_format
    !> (sprintf for integers and floats, bignum_to_c_string for floats
    !> beyond the unsigned 64-bit range).
    subroutine format_number(arg, conversion, field_width, precision, precision_given, minus_flag, plus_flag, &
                             space_flag, sharp_flag, zero_flag, item, status)
        type(format_argument_type), intent(in) :: arg
        character, intent(in) :: conversion
        integer(int64), intent(in) :: field_width, precision
        logical, intent(in) :: precision_given, minus_flag, plus_flag, space_flag
        logical, intent(in) :: sharp_flag, zero_flag
        character(len=:), allocatable, intent(out) :: item
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: body, prefix, digits
        integer(int64) :: prec, base, excess_precision, trailing_zeros, leading_zeros, numwidth, padding, beglen, endlen, &
                          midlen, leading_padding, trailing_padding, bytes, e
        real(real64) :: x
        logical :: sharp, zero, float_conversion, negative, signedp

        status = status_type()
        item = ""
        sharp = sharp_flag
        zero = zero_flag
        ! Avoid undefined behavior in underlying sprintf.
        if (conversion == "d" .or. conversion == "i") sharp = .false.
        float_conversion = scan(conversion, "efg") > 0
        if (.not. (float_conversion .or. conversion == "c")) zero = zero .and. .not. precision_given
        prec = -1
        if (precision_given) prec = min(precision, USEFUL_PRECISION_MAX)
        prefix = ""

        select case (conversion)
        case ("d", "i", "o", "x", "X", "b", "B")
            base = 10
            if (conversion == "o") base = 8
            if (conversion == "x" .or. conversion == "X") base = 16
            if (conversion == "b" .or. conversion == "B") base = 2
        case default
            base = 10
        end select

        if (float_conversion) then
            if (arg%tag == FORMAT_ARGUMENT_INTEGER) then
                body = sprintf_float(arg%integer_number, conversion, prec, plus_flag, space_flag, sharp)
            else
                body = sprintf_float(arg%real_number, conversion, prec, plus_flag, space_flag, sharp)
            end if
        else if (conversion == "c") then
            ! Not sprintf, which might mishandle the precision.
            body = achar(arg%integer_number)
            if (prec == 0) body = ""
        else if (arg%tag == FORMAT_ARGUMENT_INTEGER) then
            negative = arg%integer_number < 0
            digits = integer_digits(arg%integer_number, base, conversion == "X")
            body = sprintf_integer_digits(digits, negative, arg%integer_number /= 0, conversion, prec, plus_flag, &
                                          space_flag, sharp)
        else if (conversion == "d" .or. conversion == "i") then
            ! Truncate and then convert -0 to 0, to be more consistent
            ! with %x etc.; see Bug#31938.
            x = aint(arg%real_number)
            if (x == 0) x = 0
            body = sprintf_float(x, "f", 0_int64, plus_flag, space_flag, .false.)
            signedp = .not. is_digit(body(1:1))
            prec = min(precision, len(body, kind=int64) - merge(1, 0, signedp))
        else
            x = arg%real_number

            if (abs(x) < 2.0_real64**64) then
                negative = x <= -1
                digits = real_digits(abs(x), base, conversion == "X")
                body = sprintf_integer_digits(digits, negative, aint(abs(x)) /= 0, conversion, prec, plus_flag, &
                                              space_flag, sharp)
            else
                ! An integer beyond the unsigned 64-bit range: as
                ! bignum_to_c_string writes it, then the sign and 0x
                ! prefix as a separate prefix.
                if (.not. ieee_is_finite(x)) then
                    call set_error(status, FM_OVERFLOW_ERROR)
                    return
                end if

                body = real_digits(abs(x), base, conversion == "X")
                if (x < 0) body = "-"//body
                prec = min(precision, len(body, kind=int64))
                if ((plus_flag .or. space_flag) .and. x >= 0) prefix = merge("+", " ", plus_flag)

                if (sharp .and. .not. (conversion == "d" .or. conversion == "i" .or. conversion == "o")) then
                    prefix = prefix//"0"//conversion
                end if
            end if
        end if

        ! Now the length of the formatted item is known, except it omits
        ! padding and excess precision.  Deal with excess precision first.
        bytes = len(body, kind=int64)
        excess_precision = 0
        if (precision_given) excess_precision = precision - prec
        trailing_zeros = 0

        if (excess_precision /= 0 .and. float_conversion) then
            if (bytes == 0) then
                excess_precision = 0
            else if (.not. is_digit(body(bytes:bytes)) .or. &
                     (conversion == "g" .and. .not. (sharp .and. index(body, ".") > 0))) then
                excess_precision = 0
            end if

            trailing_zeros = excess_precision
        end if

        leading_zeros = excess_precision - trailing_zeros
        numwidth = len(prefix, kind=int64) + bytes + excess_precision
        padding = max(0_int64, field_width - numwidth)

        if (numwidth > STRING_BYTES_BOUND .or. padding > STRING_BYTES_BOUND - numwidth) then
            call set_error(status, FM_GENERIC_ERROR, "Maximum string size exceeded")
            return
        end if

        ! Skip any sign and 0[bBxX] prefixes.
        beglen = 0

        if (bytes > 0) then
            if (scan(body(1:1), "-+ ") > 0) beglen = 1
        end if

        if (.not. float_conversion .and. bytes >= beglen + 2) then
            if (body(beglen + 1:beglen + 2) == "0"//conversion) beglen = beglen + 2
        end if

        if (zero .and. bytes > beglen) then
            if (char_hexdigit(iachar(body(beglen + 1:beglen + 1), int32)) >= 0) then
                leading_zeros = leading_zeros + padding
                padding = 0
            end if
        end if

        if (leading_zeros == 0 .and. sharp .and. conversion == "o") then
            if (bytes <= beglen) then
                leading_zeros = 1
                if (padding /= 0) padding = padding - 1
            else if (body(beglen + 1:beglen + 1) /= "0") then
                leading_zeros = 1
                if (padding /= 0) padding = padding - 1
            end if
        end if

        endlen = 0

        if (trailing_zeros > 0 .and. (conversion == "e" .or. conversion == "g")) then
            e = index(body, "e")
            if (e > 0) endlen = bytes - e + 1
        end if

        midlen = bytes - beglen - endlen
        leading_padding = padding
        if (minus_flag) leading_padding = 0
        trailing_padding = padding - leading_padding
        ! LEADING_PADDING spaces, BEGLEN bytes of the start of the sprintf
        ! output, the prefix, LEADING_ZEROS zeros, MIDLEN bytes of its
        ! middle, TRAILING_ZEROS zeros, ENDLEN bytes of its end and
        ! TRAILING_PADDING spaces.
        item = repeat(" ", int(leading_padding))//body(1:beglen)//prefix//repeat("0", int(leading_zeros))// &
               body(beglen + 1:beglen + midlen)//repeat("0", int(trailing_zeros))//body(bytes - endlen + 1:bytes)// &
               repeat(" ", int(trailing_padding))
    end subroutine format_number

    !> What sprintf writes for an integer conversion (o x X b B, or d i
    !> of an integer) of the magnitude DIGITS: the sign, with the # flag the
    !> 0 or 0x prefix of a NONZERO value, and the digits extended with
    !> zeros to PRECISION (none for a zero value and a precision 0).
    !> Not a port.
    pure function sprintf_integer_digits(digits, negative, nonzero, conversion, precision, plus_flag, space_flag, &
                                         sharp_flag) result(s)
        character(len=*), intent(in) :: digits
        logical, intent(in) :: negative, nonzero, plus_flag, space_flag, sharp_flag
        character, intent(in) :: conversion
        integer(int64), intent(in) :: precision
        character(len=:), allocatable :: s
        character(len=:), allocatable :: d

        d = digits

        if (precision >= 0) then
            if (precision == 0 .and. .not. nonzero) d = ""
            if (len(d, kind=int64) < precision) d = repeat("0", int(precision - len(d)))//d
        end if

        if (sharp_flag) then
            select case (conversion)
            case ("o")
                if (len(d) == 0) then
                    d = "0"
                else if (d(1:1) /= "0") then
                    d = "0"//d
                end if
            case ("x", "X", "b", "B")
                if (nonzero) d = "0"//conversion//d
            case default
                continue
            end select
        end if

        if (negative) then
            s = "-"//d
        else if (plus_flag) then
            s = "+"//d
        else if (space_flag) then
            s = " "//d
        else
            s = d
        end if
    end function sprintf_integer_digits

    !> The digits of |N| in BASE (2, 8, 10 or 16), upper case for UPPER.
    !> Not a port.
    pure function integer_digits(n, base, upper) result(s)
        integer(int64), intent(in) :: n, base
        logical, intent(in) :: upper
        character(len=:), allocatable :: s
        character(len=*), parameter :: lower_digits = "0123456789abcdef", upper_digits = "0123456789ABCDEF"
        integer(int64) :: m, d

        s = ""
        m = n

        do
            ! Negative M is divided as it is: -huge(0_int64) - 1 has no
            ! positive int64.
            d = abs(mod(m, base))

            if (upper) then
                s = upper_digits(d + 1:d + 1)//s
            else
                s = lower_digits(d + 1:d + 1)//s
            end if

            m = m/base
            if (m == 0) exit
        end do
    end function integer_digits

    !> The digits in BASE (2, 8 or 16; 10 through `sprintf_float') of the
    !> integer part of the finite X >= 0, exactly: X is its 53-bit
    !> significand times a power of two.  Not a port.
    function real_digits(x, base, upper) result(s)
        real(real64), intent(in) :: x
        integer(int64), intent(in) :: base
        logical, intent(in) :: upper
        character(len=:), allocatable :: s
        character(len=*), parameter :: lower_digits = "0123456789abcdef", upper_digits = "0123456789ABCDEF"
        character(len=:), allocatable :: bits
        real(real64) :: t
        integer(int64) :: significand, shift, k, d, group
        integer :: j

        t = aint(x)

        if (t < 2.0_real64**63) then
            s = integer_digits(int(t, int64), base, upper)
            return
        end if

        if (base == 10) then
            s = sprintf_float(t, "f", 0_int64, .false., .false., .false.)
            return
        end if

        ! T = SIGNIFICAND * 2**SHIFT, SHIFT >= 0 here.
        significand = int(scale(fraction(t), digits(t)), int64)
        shift = exponent(t) - digits(t)
        bits = integer_digits(significand, 2_int64, .false.)//repeat("0", int(shift))
        group = 1
        if (base == 8) group = 3
        if (base == 16) group = 4
        if (mod(len(bits, kind=int64), group) /= 0) bits = repeat("0", int(group - mod(len(bits, kind=int64), group)))//bits
        s = ""

        do k = 1, len(bits, kind=int64), group
            d = 0

            do j = 0, int(group) - 1
                d = 2*d + (iachar(bits(k + j:k + j)) - iachar("0"))
            end do

            if (upper) then
                s = s//upper_digits(d + 1:d + 1)
            else
                s = s//lower_digits(d + 1:d + 1)
            end if
        end do

        k = verify(s, "0")
        if (k == 0) k = len(s, kind=int64)
        s = s(k:)
    end function real_digits

    !> Move the text properties of the format string STRING to where their
    !> text went in RESULT: DISCARDED(byte) is 1 for the bytes of a
    !> %-sequence, 2 for the bytes after the first of a character, and
    !> INFO the output of each %-sequence.  Port of the part of
    !> editfns.c:styled_format that adds text properties from the format
    !> string.
    subroutine translate_format_properties(string, discarded, info, result)
        type(value_type), intent(in) :: string
        integer, intent(in) :: discarded(:)
        type(format_spec_type), intent(in) :: info(:)
        type(value_type), intent(inout) :: result
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        type(status_type) :: st
        integer(int64) :: bytepos, position, translated, fieldn, new_start
        integer :: k
        logical :: changed

        call string_intervals(string, starts, ends, plists)
        ! BYTEPOS is the byte position in the format string, POSITION
        ! the untranslated character position in it, TRANSLATED the
        ! translated character position in the result, and FIELDN the
        ! number of the next %-sequence we will come to.
        bytepos = 1
        position = 0
        translated = 0
        fieldn = 1

        do k = 1, size(starts)
            call advance(starts(k))
            new_start = translated
            call advance(ends(k))
            call string_add_text_properties(new_start, translated, plists(k), result, changed, st)
        end do

    contains

        !> Advance BYTEPOS, POSITION, TRANSLATED and FIELDN up to POS.
        subroutine advance(pos)
            integer(int64), intent(in) :: pos

            do while (position < pos .and. bytepos <= size(discarded))
                if (discarded(bytepos) == 0) then
                    position = position + 1
                    translated = translated + 1
                else if (discarded(bytepos) == 1) then
                    position = position + 1

                    if (fieldn <= size(info)) then
                        if (bytepos - 1 >= info(fieldn)%fbeg .and. translated == info(fieldn)%start) then
                            translated = translated + info(fieldn)%end - info(fieldn)%start
                            fieldn = fieldn + 1
                        end if
                    end if
                end if

                bytepos = bytepos + 1
            end do
        end subroutine advance
    end subroutine translate_format_properties

    !> Add the text properties of the string a %-sequence produced
    !> (SPEC's argument) where it went in RESULT, the last range extended
    !> over the padding on the right.  Port of the part of
    !> editfns.c:styled_format that adds text properties from arguments
    !> (the copy of `composition' values is deferred).
    subroutine add_argument_properties(spec, result)
        type(format_spec_type), intent(in) :: spec
        type(value_type), intent(inout) :: result
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)
        type(text_property_range_type), allocatable :: ranges(:)
        type(status_type) :: st
        integer(int64) :: nchars
        integer :: k
        logical :: changed

        call multibyte_chars_in_text(xstring(spec%argument), nchars, st)
        call string_intervals(spec%argument, starts, ends, plists)
        allocate (ranges(size(starts)))

        do k = 1, size(starts)
            ranges(k)%start = starts(k)
            ranges(k)%end = ends(k)
            ranges(k)%plist = plists(k)
        end do

        call extend_property_ranges(ranges, nchars, spec%end - spec%start)

        do k = 1, size(ranges)
            call string_add_text_properties(ranges(k)%start + spec%start, ranges(k)%end + spec%start, ranges(k)%plist, &
                                            result, changed, st)
        end do
    end subroutine add_argument_properties

    !> Convert the prefix of STRING from position I on from ASCII decimal
    !> digits to a number NUM; NUM_END is the position of the first
    !> non-digit.  NUM is huge(0_int64) on overflow, 0 if there is no
    !> number.  Port of editfns.c:str2num.
    pure subroutine str2num(string, i, num, num_end)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: i
        integer(int64), intent(out) :: num, num_end
        integer(int64) :: d

        num = 0
        num_end = i

        do while (num_end <= len(string, kind=int64))
            if (.not. is_digit(string(num_end:num_end))) exit
            d = iachar(string(num_end:num_end)) - iachar("0")

            if (num > (huge(num) - d)/10) then
                num = huge(num)
            else
                num = 10*num + d
            end if

            num_end = num_end + 1
        end do
    end subroutine str2num

    !> Whether C is an ASCII decimal digit (C's c_isdigit).  Not a port.
    elemental logical function is_digit(c)
        character, intent(in) :: c

        is_digit = lge(c, "0") .and. lle(c, "9")
    end function is_digit

    !> Whether ARG is a string value.  Not a port.
    elemental logical function is_string_argument(arg)
        type(format_argument_type), intent(in) :: arg

        is_string_argument = arg%tag == FORMAT_ARGUMENT_VALUE .and. vtag(arg%value) == V_STRING
    end function is_string_argument

    !> Whether ARG is a name or the null value (upstream's SYMBOLP).
    !> Not a port.
    elemental logical function is_name_argument(arg)
        type(format_argument_type), intent(in) :: arg

        is_name_argument = arg%tag == FORMAT_ARGUMENT_VALUE .and. &
                           (vtag(arg%value) == V_NAME .or. vtag(arg%value) == V_NULL)
    end function is_name_argument

    !> Whether the string value V has text properties (upstream's
    !> string_intervals).  Not a port.
    logical function has_intervals(v)
        type(value_type), intent(in) :: v
        integer(int64), allocatable :: starts(:), ends(:)
        type(value_type), allocatable :: plists(:)

        call string_intervals(v, starts, ends, plists)
        has_intervals = size(starts) > 0
    end function has_intervals
end module editing_functions_mod
