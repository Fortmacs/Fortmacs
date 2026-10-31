! syntax_fm.f90 -- syntax tables and moving over words.
! Ported from GNU Emacs 31.1 src/syntax.c, src/syntax.h and the word
! boundary test of src/category.c.
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

!> Syntax tables say what each character is to the editing commands: a
!> word constituent, whitespace, punctuation, a parenthesis and so on.
!> A table is a registry object referenced by handle; the standard one,
!> made on first use, has init_syntax_once's ASCII entries, word syntax
!> for every other character, and characters.el's entries (characters_fm)
!> on top.  A table made by `make_syntax_table' or `copy_syntax_table'
!> inherits from a parent what it doesn't set.  Each buffer has a syntax
!> table (the standard one unless `set_syntax_table' gave it another).
!>
!> A raw syntax descriptor is a `syntax_descriptor_type': the class plus
!> the flags (bits 16 up, as upstream) and the matching character.
!>
!> Words: `scan_words' and `forward_word'.  Two word-constituent
!> characters are in the same word unless they belong to different
!> scripts (`char_script_table', charscript_fm), with the exceptions of
!> `word_combining_categories'; or, in the same script, the exceptions of
!> `word_separating_categories' (category.c:word_boundary_p, with
!> characters.el's values for those variables).
!>
!> Comments and balanced expressions are in the submodule syntax_scan_fm:
!> `forward_comment', `scan_groups', `scan_expressions' and `parse_partial_expression',
!> whose result is a `parse_state_type' rather than a list.
!>
!> Deferred: `syntax_table' text properties (parse-sexp-lookup-properties,
!> syntax-propertize), `forward_comment_function',
!> `find_word_boundary_function_table', `describe_syntax', unibyte
!> buffers' raw bytes, quitting during long scans (rarely_quit).
module syntax_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT, FM_ARGS_OUT_OF_RANGE, &
                        FM_SCAN_ERROR
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use character_fm, only: MAX_CHAR, string_char_and_length, char_string, MAX_MULTIBYTE_LENGTH, unibyte_to_char
    use error_fm, only: format_message
    use buffer_fm, only: buffers, cur, buf_fetch_char, buf_next_char_len, buf_prev_char_len
    use marker_fm, only: char_to_byte
    use editfns_fm, only: point, goto_char, constrain_to_field
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use characters_fm, only: characters_syntax
    use category_fm, only: char_has_category
    use charscript_fm, only: char_script
    use data_fm, only: logical_setting, integer_setting, forward_setting
    use lisp_fm, only: value_type, consp, fixnump, xfixnum, syntax_table_ref_p, xhandle, make_fixnum, list_elt, list
    use lread_fm, only: sym
    use textprop_fm, only: get_text_property
    implicit none(type, external)
    private

    public :: syntax_descriptor_type, standard_syntax_table, make_syntax_table, copy_syntax_table, &
              free_syntax_table, syntax_table_live_p, syntax_table, set_syntax_table, string_to_syntax, &
              modify_syntax_entry, syntax_entry, char_syntax_class, char_syntax, syntax_class_to_char, &
              matching_paren, scan_words, forward_word, word_boundary_p, syms_of_syntax, &
              skip_chars_forward, skip_chars_backward, skip_syntax_forward, skip_syntax_backward, &
              skip_limit, backward_prefix_chars
    public :: parse_state_type, forward_comment, scan_groups, scan_expressions, parse_partial_expression, syntax_ppss, &
              scan_error_data
    public :: syntax_entry_at, syntax_class_at, syntax_descriptor_value, descriptor_of_value, parse_expression_propertize
    public :: syntax_flags_comment_styleb, syntax_flags_comment_stylec, internal_describe_syntax_value

    !> Syntax classes (syntax.h enum syntaxcode).
    integer, parameter, public :: SWHITESPACE = 0, SPUNCT = 1, SWORD = 2, SSYMBOL = 3, SOPEN = 4, &
                                  SCLOSE = 5, SQUOTE = 6, SSTRING = 7, SMATH = 8, SESCAPE = 9, &
                                  SCHARQUOTE = 10, SCOMMENT = 11, SENDCOMMENT = 12, SINHERIT = 13, &
                                  SCOMMENT_FENCE = 14, SSTRING_FENCE = 15

    !> The descriptor character of each class (syntax.c:syntax_code_spec).
    character(len=16), parameter :: syntax_code_spec = " .w_()'""$\/<>@!|"

    !> True means `forward_word', etc., should treat escape chars part of words.
    !> Port of syntax.c words-include-escapes.
    logical, public :: words_include_escapes = .false.

    !> True means `forward_expression', etc., should treat comments as whitespace.
    !> Port of syntax.c parse-sexp-ignore-comments.
    type(logical_setting), public :: parse_expression_ignore_comments = &
                                     logical_setting(name="parse_expression_ignore_comments", value=.false.)

    !> True means an escaped ender inside a comment doesn't end the comment.
    !> Port of syntax.c comment-end-can-be-escaped (buffer-local when set,
    !> as upstream makes it).
    type(logical_setting), public :: comment_end_can_be_escaped = &
                                     logical_setting(name="comment_end_can_be_escaped", value=.false.)

    !> True means `scan_expressions' treats all multibyte characters as symbol.
    !> Port of syntax.c multibyte-syntax-as-symbol.
    logical, public :: multibyte_syntax_as_symbol = .false.

    !> True means an open paren in column 0 denotes the start of a subprogram.
    !> Port of syntax.c open-paren-in-column-0-is-defun-start.
    logical, public :: open_paren_in_column_0_is_subprogram_start = .true.

    !> True means `forward_expression', etc., obey `syntax_table' property.
    !> Ignoring the `syntax_table' property may result in faster syntax
    !> scanning when this variable is false.  Port of syntax.c
    !> parse-sexp-lookup-properties (buffer-local when set).
    type(logical_setting), target, public :: parse_expression_lookup_properties = &
                                             logical_setting(name="parse_expression_lookup_properties", value=.false.)

    !> Position up to which syntax-table properties have been set.
    !> Port of syntax.c syntax-propertize--done (buffer-local when set).
    type(integer_setting), target, public :: syntax_propertize__done = &
                                             integer_setting(name="syntax_propertize__done", value=-1)

    abstract interface
        !> syntax.el's internal--syntax-propertize, which syntax.c calls
        !> to set `syntax_table' properties up to CHARPOS.  Not a port.
        subroutine internal_syntax_propertize_proc(charpos)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: charpos
        end subroutine internal_syntax_propertize_proc
    end interface

    !> internal--syntax-propertize (set by syntax_propertize_fm).
    procedure(internal_syntax_propertize_proc), pointer, public :: internal_syntax_propertize => null()

    !> True means `forward_comment' can use `syntax_ppss' internally.
    !> Port of syntax.c comment-use-syntax-ppss.
    logical, public :: comment_use_syntax_ppss = .true.

    !> Comment style and string terminator of a parse state in a comment or
    !> string delimited by a generic comment or string fence (syntax.c
    !> ST_COMMENT_STYLE and ST_STRING_STYLE): `comment_style' 257 is the
    !> symbol `syntax-table' upstream, `instring' 258 is t.
    integer, parameter, public :: ST_COMMENT_STYLE = 256 + 1, ST_STRING_STYLE = 256 + 2

    !> One more than the largest syntax class (syntax.h Smax).
    integer, parameter, public :: SMAX = 16

    !> COMMENTSTOP of `parse_partial_expression': nil, non-nil (stop after the
    !> start of a comment) and the symbol `syntax-table' (stop after the
    !> start or end of a comment or a string).
    integer, parameter, public :: COMMENTSTOP_NIL = 0, COMMENTSTOP_COMMENT = 1, COMMENTSTOP_SYNTAX_TABLE = -1

    !> The state of a parse by `parse_partial_expression', the list upstream
    !> returns.  Its components are the list's elements:
    !>  0. `depth': depth in parens.
    !>  1. `innermost_start': character address of start of innermost
    !>     containing group; 0 if none (nil).
    !>  2. `last_expression_start': character address of start of last complete
    !>     expression terminated; 0 if none (nil).
    !>  3. `instring': -1 (nil) if not inside a string; else the character
    !>     that will terminate the string, or ST_STRING_STYLE (t) if the
    !>     string should be terminated by a generic string delimiter.
    !>  4. `incomment': 0 (nil) outside a comment, -1 (t) inside a
    !>     non-nestable comment, else the current comment nesting.
    !>  5. `quoted': true if following a quote character.
    !>  6. `mindepth': the minimum paren-depth encountered during this scan.
    !>  7. `comment_style': style of comment, if any: 0 (nil, style a), 1
    !>     (b), 2 (c), 3 (b and c) or ST_COMMENT_STYLE (the symbol
    !>     `syntax-table': a generic comment fence).
    !>  8. `comstr_start': character address of start of comment or string;
    !>     0 (nil) if not in one.
    !>  9. `open_parens': positions of currently open parens, outermost
    !>     first.
    !> 10. `prev_syntax': when the last position scanned holds the first
    !>     character of a (potential) two character construct, the syntax
    !>     of that position (class and flags), otherwise -1 (nil).  That
    !>     construct can be a two character comment delimiter or an
    !>     Escaped or Char-quoted character.
    !> The default value is the state at the start of a subprogram (an OLDSTATE
    !> of nil).
    type :: parse_state_type
        integer(int64) :: depth = 0
        integer(int64) :: innermost_start = 0
        integer(int64) :: last_expression_start = 0
        integer :: instring = -1
        integer(int64) :: incomment = 0
        logical :: quoted = .false.
        integer(int64) :: mindepth = 0
        integer :: comment_style = 0
        integer(int64) :: comstr_start = 0
        integer(int64), allocatable :: open_parens(:)
        integer :: prev_syntax = -1
    end type parse_state_type

    abstract interface
        !> syntax.el's `syntax_ppss', when it is defined.  Not a port.
        subroutine syntax_ppss_proc(state, pos)
            import :: parse_state_type, int64
            implicit none(type, external)
            type(parse_state_type), intent(out) :: state
            integer(int64), intent(in), optional :: pos
        end subroutine syntax_ppss_proc
    end interface

    !> syntax.el's syntax-ppss with its cache (set by
    !> syntax_propertize_fm); null for the plain parse.
    procedure(syntax_ppss_proc), pointer, public :: syntax_ppss_function => null()

    !> Counts `modify_syntax_entry' calls, so compiled regexps that depend
    !> on syntax can tell they are stale (search.c:clear_regexp_cache).
    integer(int64), public, protected :: syntax_modiff = 0

    !> A raw syntax descriptor, (CODE . MATCHING-CHAR) upstream.  `code'
    !> -1 is nil: inherit from the parent table.  `match' -1 is nil.
    type :: syntax_descriptor_type
        integer(int32) :: code = -1
        integer(int32) :: match = -1
    end type syntax_descriptor_type

    !> A syntax table: entries for ASCII, sorted runs for the rest, and
    !> the parent to inherit unset entries from.
    type :: syntax_table_type
        type(syntax_descriptor_type) :: ascii(0:127)
        integer(int32), allocatable :: starts(:), ends(:)
        type(syntax_descriptor_type), allocatable :: entries(:)
        integer :: n = 0
        type(handle_type) :: parent
    end type syntax_table_type

    type(slot_table_type) :: table
    type(syntax_table_type), allocatable :: tables(:)
    type(handle_type) :: standard

    !> The table characters_syntax paints (it can't take arguments).
    integer(int64) :: painting = 0

    interface
        !> Port of syntax.c:skip_chars.  (In syntax_skip_fm, which can use
        !> regex_emacs_fm for the character classes.)
        module function skip_chars(forwardp, string, status, lim) result(moved)
            implicit none(type, external)
            logical, intent(in) :: forwardp
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: lim
            integer(int64) :: moved
        end function skip_chars

        !> Move forward across up to COUNT comments.  If COUNT is negative, move backward.
        !> Stop scanning if we find something other than a comment or whitespace.
        !> Set point to where scanning stops.
        !> If COUNT comments are found as expected, with nothing except whitespace
        !> between them, return true; otherwise return false.  Port of
        !> syntax.c:Fforward_comment (no `forward_comment_function').
        recursive module logical function forward_comment(count) result(found)
            implicit none(type, external)
            integer(int64), intent(in) :: count
        end function forward_comment

        !> Scan from character number FROM by COUNT groups.
        !> Scan forward if COUNT is positive, backward if COUNT is negative.
        !> Return the character number of the position thus found.
        !>
        !> A "group", in this context, refers to a balanced parenthetical
        !> grouping, as determined by the syntax table.
        !>
        !> If DEPTH is nonzero, treat that as the nesting depth of the starting
        !> point (i.e. the starting point is DEPTH parentheses deep).  This
        !> function scans over parentheses until the depth goes to zero COUNT
        !> times.  Hence, positive DEPTH moves out that number of levels of
        !> parentheses, while negative DEPTH moves to a deeper level.
        !>
        !> Comments are ignored if `parse_expression_ignore_comments' is true.
        !>
        !> If we reach the beginning or end of the accessible part of the buffer
        !> before we have scanned over COUNT groups, return 0 if the depth at
        !> that point is zero, and signal an error if the depth is nonzero.
        !> Port of syntax.c:Fscan_lists (the error is `scan_error').
        recursive module integer(int64) function scan_groups(from, count, depth, status) result(pos)
            implicit none(type, external)
            integer(int64), intent(in) :: from, count, depth
            type(status_type), intent(out) :: status
        end function scan_groups

        !> Scan from character number FROM by COUNT balanced expressions.
        !> If COUNT is negative, scan backwards.
        !> Returns the character number of the position thus found.
        !>
        !> Comments are ignored if `parse_expression_ignore_comments' is true.
        !>
        !> If the beginning or end of (the accessible part of) the buffer is reached
        !> in the middle of a parenthetical grouping, an error is signaled.
        !> If the beginning or end is reached between groupings
        !> but before count is used up, 0 is returned.  Port of
        !> syntax.c:Fscan_sexps.
        recursive module integer(int64) function scan_expressions(from, count, status) result(pos)
            implicit none(type, external)
            integer(int64), intent(in) :: from, count
            type(status_type), intent(out) :: status
        end function scan_expressions

        !> Parse syntax starting at FROM until TO; return status of parse at TO.
        !> Parsing stops at TO or when certain criteria are met;
        !>  point is set to where parsing stops.
        !>
        !> If OLDSTATE is omitted, parsing assumes that FROM is the
        !>  beginning of a function.  If not, OLDSTATE should be the state at
        !>  FROM.
        !>
        !> The value, STATE, describes the final state of parsing (see
        !> `parse_state_type').
        !>
        !> If third arg TARGETDEPTH is present, parsing stops if the depth
        !> in parentheses becomes equal to TARGETDEPTH.
        !> Fourth arg STOPBEFORE true means stop when we come to
        !>  any character that starts an expression.
        !> Fifth arg OLDSTATE is a state like what this function returns.
        !>  It is used to initialize the state of the parse.  Components
        !>  `innermost_start', `last_expression_start' and `mindepth' are ignored.
        !> Sixth arg COMMENTSTOP COMMENTSTOP_COMMENT means stop after the start
        !>  of a comment.  If it is COMMENTSTOP_SYNTAX_TABLE, stop after the
        !>  start of a comment or a string, or after end of a comment or a
        !>  string.  Port of syntax.c:Fparse_partial_sexp (STATE and OLDSTATE
        !> must be different variables).
        recursive module subroutine parse_partial_expression(from, to, state, status, targetdepth, stopbefore, oldstate, &
                                                             commentstop)
            implicit none(type, external)
            integer(int64), intent(in) :: from, to
            type(parse_state_type), intent(out) :: state
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: targetdepth
            logical, intent(in), optional :: stopbefore
            type(parse_state_type), intent(in), optional :: oldstate
            integer, intent(in), optional :: commentstop
        end subroutine parse_partial_expression

        !> Return the parse state of the parenthesis structure at POS
        !> (default: point), as `parse_partial_expression' gives it parsing from
        !> `point_min' to POS.  Point is at POS when this function returns.
        !> Port of emacs-lisp/syntax.el:syntax-ppss: the cached parse of
        !> syntax_propertize_fm (`syntax_ppss_function'), or a parse from
        !> `point_min' before that is set up.
        recursive module subroutine syntax_ppss(state, pos)
            implicit none(type, external)
            type(parse_state_type), intent(out) :: state
            integer(int64), intent(in), optional :: pos
        end subroutine syntax_ppss
    end interface

contains

    ! ------------------------------------------------------------------
    ! Registry
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine new_table(h, parent)
        type(handle_type), intent(out) :: h
        type(handle_type), intent(in) :: parent
        type(syntax_table_type), allocatable :: grown(:)

        call table%acquire(h)

        if (.not. allocated(tables)) then
            allocate (tables(table%capacity()))
        else if (size(tables, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))
            grown(1:size(tables)) = tables
            call move_alloc(grown, tables)
        end if

        tables(h%index) = syntax_table_type()
        allocate (tables(h%index)%starts(0), tables(h%index)%ends(0), tables(h%index)%entries(0))
        tables(h%index)%parent = parent
    end subroutine new_table

    !> Slot of table `h', or a wrong-type-argument error.  Port of
    !> syntax.c:check_syntax_table.
    subroutine resolve_table(h, i, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status

        call table%resolve(h, i, status)
        if (.not. status%ok()) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(syntax_table_type)")
    end subroutine resolve_table

    !> Return true if OBJECT is a syntax table.  Port of
    !> syntax.c:Fsyntax_table_p (syntax tables are their own objects here,
    !> so no other char-table counts as one).
    logical function syntax_table_live_p(h)
        type(handle_type), intent(in) :: h

        syntax_table_live_p = table%is_live(h)
    end function syntax_table_live_p

    !> Free syntax table `h' (not the standard one).  Not a port: upstream
    !> relies on GC.
    subroutine free_syntax_table(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_table(h, i, status)
        if (.not. status%ok()) return

        if (h%index == standard%index .and. h%generation == standard%generation) then
            call set_error(status, FM_GENERIC_ERROR, "Can't free the standard syntax table")
            return
        end if

        tables(i) = syntax_table_type()
        call table%release(h, status)
    end subroutine free_syntax_table

    !> Return the standard syntax table.
    !> This is the one used for new buffers.  Port of
    !> syntax.c:Fstandard_syntax_table (made on first use, by
    !> init_syntax_once and characters.el).
    function standard_syntax_table() result(h)
        type(handle_type) :: h

        if (.not. table%is_live(standard)) call init_syntax_once()
        h = standard
    end function standard_syntax_table

    !> Make the standard syntax table.  Port of syntax.c:init_syntax_once,
    !> then the entries characters.el makes.
    subroutine init_syntax_once()
        integer :: i

        call new_table(standard, null_handle())

        associate (t => tables(standard%index))
            ! Control characters should not be whitespace.
            do i = 0, 127
                t%ascii(i) = syntax_descriptor_type(SPUNCT, -1)
            end do

            ! Except that a few really are whitespace.
            t%ascii(32) = syntax_descriptor_type(SWHITESPACE, -1)
            t%ascii(9) = syntax_descriptor_type(SWHITESPACE, -1)
            t%ascii(10) = syntax_descriptor_type(SWHITESPACE, -1)
            t%ascii(13) = syntax_descriptor_type(SWHITESPACE, -1)
            t%ascii(12) = syntax_descriptor_type(SWHITESPACE, -1)

            do i = iachar("a"), iachar("z")
                t%ascii(i) = syntax_descriptor_type(SWORD, -1)
            end do

            do i = iachar("A"), iachar("Z")
                t%ascii(i) = syntax_descriptor_type(SWORD, -1)
            end do

            do i = iachar("0"), iachar("9")
                t%ascii(i) = syntax_descriptor_type(SWORD, -1)
            end do

            t%ascii(iachar("$")) = syntax_descriptor_type(SWORD, -1)
            t%ascii(iachar("%")) = syntax_descriptor_type(SWORD, -1)
            t%ascii(iachar("(")) = syntax_descriptor_type(SOPEN, iachar(")"))
            t%ascii(iachar(")")) = syntax_descriptor_type(SCLOSE, iachar("("))
            t%ascii(iachar("[")) = syntax_descriptor_type(SOPEN, iachar("]"))
            t%ascii(iachar("]")) = syntax_descriptor_type(SCLOSE, iachar("["))
            t%ascii(iachar("{")) = syntax_descriptor_type(SOPEN, iachar("}"))
            t%ascii(iachar("}")) = syntax_descriptor_type(SCLOSE, iachar("{"))
            t%ascii(iachar("""")) = syntax_descriptor_type(SSTRING, -1)
            t%ascii(iachar("\")) = syntax_descriptor_type(SESCAPE, -1)

            do i = 1, 10
                t%ascii(iachar("_-+*/&|<>=" (i:i))) = syntax_descriptor_type(SSYMBOL, -1)
            end do

            do i = 1, 12
                t%ascii(iachar(".,;:?!#@~^'`" (i:i))) = syntax_descriptor_type(SPUNCT, -1)
            end do

            t%ascii(127) = syntax_descriptor_type(SPUNCT, -1)
        end associate

        ! All multibyte characters have syntax `word' by default.
        call paint(standard%index, 128, MAX_CHAR, syntax_descriptor_type(SWORD, -1))
        painting = standard%index
        call characters_syntax(characters_entry)
        painting = 0
    end subroutine init_syntax_once

    !> characters_syntax's callback: a modify-syntax-entry on the table
    !> being painted.  Not a port.
    subroutine characters_entry(from, to, descriptor)
        integer, intent(in) :: from, to
        character(len=*), intent(in) :: descriptor
        type(syntax_descriptor_type) :: d
        type(status_type) :: status

        call string_to_syntax(descriptor, d, status)
        if (status%ok()) call set_range(painting, from, to, d)
    end subroutine characters_entry

    !> Construct a new syntax table and return it.
    !> It is a copy of the TABLE, which defaults to the standard syntax table.
    !> Port of syntax.c:Fcopy_syntax_table.
    subroutine copy_syntax_table(copy, status, from)
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: from
        type(handle_type) :: source, parent
        integer(int64) :: i

        source = standard_syntax_table()
        if (present(from)) source = from
        call resolve_table(source, i, status)
        if (.not. status%ok()) return
        ! Copied syntax tables should all have parents.  If we copied one
        ! with no parent, such as the standard syntax table, use the
        ! standard syntax table as the copy's parent.
        parent = tables(i)%parent
        if (parent%index == 0) parent = standard
        call new_table(copy, parent)
        call resolve_table(source, i, status)
        tables(copy%index)%ascii = tables(i)%ascii
        tables(copy%index)%starts = tables(i)%starts
        tables(copy%index)%ends = tables(i)%ends
        tables(copy%index)%entries = tables(i)%entries
        tables(copy%index)%n = tables(i)%n
    end subroutine copy_syntax_table

    !> Return a new syntax table.
    !> Create a syntax table that inherits from OLDTABLE (if present) or
    !> from `standard_syntax_table' otherwise.  Port of
    !> subr.el:make-syntax-table.
    subroutine make_syntax_table(h, status, oldtable)
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: oldtable
        type(handle_type) :: parent
        integer(int64) :: i

        parent = standard_syntax_table()

        if (present(oldtable)) then
            call resolve_table(oldtable, i, status)
            if (.not. status%ok()) return
            parent = oldtable
        end if

        status = status_type()
        call new_table(h, parent)
    end subroutine make_syntax_table

    !> Return the current syntax table.
    !> This is the one specified by the current buffer.  Port of
    !> syntax.c:Fsyntax_table.
    function syntax_table() result(h)
        type(handle_type) :: h

        h = standard_syntax_table()
        if (cur == 0) return
        if (table%is_live(buffers(cur)%syntax_table)) h = buffers(cur)%syntax_table
    end function syntax_table

    !> Select a new syntax table for the current buffer.
    !> One argument, a syntax table.  Port of syntax.c:Fset_syntax_table
    !> and syntax.c:bset_syntax_table.
    subroutine set_syntax_table(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_table(h, i, status)
        if (.not. status%ok()) return
        buffers(cur)%syntax_table = h
    end subroutine set_syntax_table

    ! ------------------------------------------------------------------
    ! Entries
    ! ------------------------------------------------------------------

    !> Give characters `from' to `to' entry `d' in table slot `t'.  Port of
    !> syntax.c:SET_RAW_SYNTAX_ENTRY and syntax.c:SET_RAW_SYNTAX_ENTRY_RANGE.
    subroutine set_range(t, from, to, d)
        integer(int64), intent(in) :: t
        integer, intent(in) :: from, to
        type(syntax_descriptor_type), intent(in) :: d
        integer :: c

        do c = max(0, from), min(127, to)
            tables(t)%ascii(c) = d
        end do

        if (to >= 128) call paint(t, max(128, from), to, d)
    end subroutine set_range

    !> Set the non-ASCII characters `from' to `to' of table slot `t' to
    !> `d', keeping the runs sorted and disjoint.  Not a port.
    subroutine paint(t, from, to, d)
        integer(int64), intent(in) :: t
        integer, intent(in) :: from, to
        type(syntax_descriptor_type), intent(in) :: d
        integer(int32), allocatable :: s(:), e(:)
        type(syntax_descriptor_type), allocatable :: v(:)
        integer :: k, m

        associate (tb => tables(t))
            allocate (s(tb%n + 3), e(tb%n + 3), v(tb%n + 3))
            m = 0

            ! The runs (or their parts) before `from`, the new run, then the
            ! runs (or their parts) after `to`.
            do k = 1, tb%n
                if (tb%ends(k) < from) then
                    call add(tb%starts(k), tb%ends(k), tb%entries(k))
                else if (tb%starts(k) < from) then
                    call add(tb%starts(k), from - 1, tb%entries(k))
                end if
            end do

            call add(from, to, d)

            do k = 1, tb%n
                if (tb%starts(k) > to) then
                    call add(tb%starts(k), tb%ends(k), tb%entries(k))
                else if (tb%ends(k) > to) then
                    call add(to + 1, tb%ends(k), tb%entries(k))
                end if
            end do

            tb%starts = s(1:m)
            tb%ends = e(1:m)
            tb%entries = v(1:m)
            tb%n = m
        end associate

    contains

        subroutine add(a, b, x)
            integer, intent(in) :: a, b
            type(syntax_descriptor_type), intent(in) :: x

            if (m + 1 > size(s)) call grow()

            if (m > 0) then
                if (e(m) == a - 1 .and. same(v(m), x)) then
                    e(m) = b
                    return
                end if
            end if

            m = m + 1
            s(m) = a
            e(m) = b
            v(m) = x
        end subroutine add

        subroutine grow()
            integer(int32), allocatable :: s2(:), e2(:)
            type(syntax_descriptor_type), allocatable :: v2(:)

            allocate (s2(2*size(s) + 4), e2(2*size(s) + 4), v2(2*size(s) + 4))
            s2(1:size(s)) = s
            e2(1:size(s)) = e
            v2(1:size(s)) = v
            call move_alloc(s2, s)
            call move_alloc(e2, e)
            call move_alloc(v2, v)
        end subroutine grow
    end subroutine paint

    ! Not a port.
    pure logical function same(a, b)
        type(syntax_descriptor_type), intent(in) :: a, b

        same = a%code == b%code .and. a%match == b%match
    end function same

    !> The entry of character `c' in table slot `t' itself (nil, code -1,
    !> if it has none).  Not a port.
    function own_entry(t, c) result(d)
        integer(int64), intent(in) :: t
        integer, intent(in) :: c
        type(syntax_descriptor_type) :: d
        integer :: lo, hi, mid

        if (c >= 0 .and. c < 128) then
            d = tables(t)%ascii(c)
            return
        end if

        lo = 1
        hi = tables(t)%n

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (c < tables(t)%starts(mid)) then
                hi = mid - 1
            else if (c > tables(t)%ends(mid)) then
                lo = mid + 1
            else
                d = tables(t)%entries(mid)
                return
            end if
        end do
    end function own_entry

    !> The raw syntax descriptor of character `c' in `table' (default: the
    !> current buffer's), inherited from parents as needed; whitespace if
    !> no table has one.  Port of syntax.h:SYNTAX_ENTRY.
    function syntax_entry(c, table_h) result(d)
        integer, intent(in) :: c
        type(handle_type), intent(in), optional :: table_h
        type(syntax_descriptor_type) :: d
        type(handle_type) :: h
        type(status_type) :: status
        integer(int64) :: i
        integer :: depth

        if (present(table_h)) then
            h = table_h
        else
            h = syntax_table()
        end if

        do depth = 1, 100
            call resolve_table(h, i, status)
            if (.not. status%ok()) exit
            d = own_entry(i, c)
            if (d%code >= 0 .and. iand(d%code, 65535) /= SINHERIT) return
            h = tables(i)%parent
        end do

        d = syntax_descriptor_type(SWHITESPACE, -1)
    end function syntax_entry

    ! ------------------------------------------------------------------
    ! `syntax_table' text properties (syntax.c gl_state)
    ! ------------------------------------------------------------------

    !> The raw syntax descriptor that the `syntax_table' property value V
    !> holds, (CODE) or (CODE MATCH) (upstream's (CODE . MATCH)); FOUND
    !> false if V is not one.  Not a port.
    subroutine descriptor_of_value(v, d, found)
        type(value_type), intent(in) :: v
        type(syntax_descriptor_type), intent(out) :: d
        logical, intent(out) :: found
        type(value_type) :: code, match

        found = .false.
        if (.not. consp(v)) return
        code = list_elt(v, 1_int64)
        if (.not. fixnump(code)) return
        d%code = int(xfixnum(code), int32)
        match = list_elt(v, 2_int64)
        if (fixnump(match)) d%match = int(xfixnum(match), int32)
        found = .true.
    end subroutine descriptor_of_value

    !> The `syntax_table' property value of descriptor D: (CODE), or
    !> (CODE MATCH) when it has a matching character (upstream's
    !> (CODE . MATCH), as `string_to_syntax' returns it).  Not a port.
    function syntax_descriptor_value(d) result(v)
        type(syntax_descriptor_type), intent(in) :: d
        type(value_type) :: v

        if (d%match >= 0) then
            v = list(make_fixnum(int(d%code, int64)), make_fixnum(int(d%match, int64)))
        else
            v = list(make_fixnum(int(d%code, int64)))
        end if
    end function syntax_descriptor_value

    !> Set `syntax_table' properties up to CHARPOS when they are not set
    !> that far yet.  Port of syntax.c:parse_sexp_propertize (its checks
    !> that internal--syntax-propertize neither modified the buffer nor
    !> failed to move `syntax_propertize__done' are deferred).
    recursive subroutine parse_expression_propertize(charpos)
        integer(int64), intent(in) :: charpos
        integer(int64) :: zv

        zv = buffers(cur)%zv

        if (syntax_propertize__done%value <= charpos .and. syntax_propertize__done%value < zv) then
            if (associated(internal_syntax_propertize)) call internal_syntax_propertize(min(zv, 1 + charpos))
        end if
    end subroutine parse_expression_propertize

    !> The syntax entry of character C at position POS of the current
    !> buffer, as the syntax scanning functions see it: when
    !> `parse_expression_lookup_properties' is true, from the `syntax_table'
    !> property at POS (a raw descriptor, or a syntax table to look C up
    !> in), with syntax-propertize run first when POS is not propertized
    !> yet; else (or without the property) from the buffer's syntax table.
    !> What syntax.c's SYNTAX_ENTRY gives after UPDATE_SYNTAX_TABLE (POS)
    !> (gl_state's property intervals are looked up afresh).
    !> Port of syntax.h:SYNTAX_ENTRY and syntax.h:UPDATE_SYNTAX_TABLE in part,
    !> with what syntax.c:update_syntax_table,
    !> syntax.c:update_syntax_table_forward, syntax.c:SETUP_SYNTAX_TABLE and
    !> syntax.c:RE_SETUP_SYNTAX_TABLE_FOR_OBJECT find in gl_state (in part:
    !> the property is looked up at each position, objects other than the
    !> current buffer have none).
    recursive function syntax_entry_at(c, pos) result(d)
        integer, intent(in) :: c
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d
        type(value_type) :: prop
        type(status_type) :: st
        logical :: found

        if (.not. parse_expression_lookup_properties%value) then
            d = syntax_entry(c)
            return
        end if

        call parse_expression_propertize(pos)

        if (pos < 1 .or. pos >= buffers(cur)%text%z) then
            d = syntax_entry(c)
            return
        end if

        call get_text_property(pos, sym("syntax_table"), prop, st)

        if (st%ok()) then
            if (consp(prop)) then
                call descriptor_of_value(prop, d, found)
                if (found) return
            else if (syntax_table_ref_p(prop)) then
                if (syntax_table_live_p(xhandle(prop))) then
                    d = syntax_entry(c, xhandle(prop))
                    return
                end if
            end if
        end if

        d = syntax_entry(c)
    end function syntax_entry_at

    !> The syntax class of character C at position POS (SYNTAX after
    !> UPDATE_SYNTAX_TABLE (POS)).
    !> Port of syntax.h:SYNTAX and syntax.h:UPDATE_SYNTAX_TABLE in part.
    recursive integer function syntax_class_at(c, pos)
        integer, intent(in) :: c
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d

        d = syntax_entry_at(c, pos)
        syntax_class_at = iand(d%code, 255)
        if (d%code < 0) syntax_class_at = SWHITESPACE
    end function syntax_class_at

    !> The syntax class of character `c' in the current buffer.
    !> Port of syntax.h:SYNTAX.
    integer function char_syntax_class(c)
        integer, intent(in) :: c
        type(syntax_descriptor_type) :: d

        d = syntax_entry(c)
        char_syntax_class = iand(d%code, 255)
    end function char_syntax_class

    !> Return the syntax code of CHARACTER, described by a character.
    !> For example, if CHARACTER is a word constituent, the
    !> character `w' (119) is returned.
    !> The characters that correspond to various syntax codes
    !> are listed in the documentation of `modify_syntax_entry'.
    !> Port of syntax.c:Fchar_syntax (in a unibyte buffer, 128 to 255 are
    !> raw bytes).
    character function char_syntax(c)
        integer, intent(in) :: c
        integer :: char_int, class

        char_int = c

        if (cur /= 0 .and. c < 256) then
            if (.not. buffers(cur)%multibyte) char_int = unibyte_to_char(int(c, int32))
        end if

        class = char_syntax_class(char_int)
        char_syntax = syntax_code_spec(class + 1:class + 1)
    end function char_syntax

    !> Return the syntax char of CLASS, described by an integer.
    !> For example, if SYNTAX is word constituent (the integer 2), the
    !> character `w' (119) is returned.  Port of
    !> syntax.c:Fsyntax_class_to_char.
    subroutine syntax_class_to_char(class, ch, status)
        integer, intent(in) :: class
        character, intent(out) :: ch
        type(status_type), intent(out) :: status

        ch = " "

        if (class < 0 .or. class >= len(syntax_code_spec)) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            return
        end if

        status = status_type()
        ch = syntax_code_spec(class + 1:class + 1)
    end subroutine syntax_class_to_char

    !> Whether the syntax FLAGS (class and flags) have comment style b.
    !> Port of syntax.c:SYNTAX_FLAGS_COMMENT_STYLEB.
    pure logical function syntax_flags_comment_styleb(flags)
        integer, intent(in) :: flags

        syntax_flags_comment_styleb = btest(flags, 21)
    end function syntax_flags_comment_styleb

    !> Whether the syntax FLAGS have comment style c.  Port of
    !> syntax.c:SYNTAX_FLAGS_COMMENT_STYLEC.
    pure logical function syntax_flags_comment_stylec(flags)
        integer, intent(in) :: flags

        syntax_flags_comment_stylec = btest(flags, 23)
    end function syntax_flags_comment_stylec

    !> Insert a description of the internal syntax description SYNTAX at point.
    !> Port of syntax.c:Finternal_describe_syntax_value, returning the
    !> text it inserts (a descriptor with `code' -1, nil, is "default").
    function internal_describe_syntax_value(syntax) result(text)
        type(syntax_descriptor_type), intent(in) :: syntax
        character(len=:), allocatable :: text
        integer :: syntax_code, code
        logical :: start1, start2, end1, end2, prefix, comstyleb, comstylec, comnested

        if (syntax%code == -1) then
            text = "default"
            return
        end if

        syntax_code = iand(syntax%code, huge(1))
        code = iand(syntax_code, 255)
        start1 = btest(syntax_code, 16)
        start2 = btest(syntax_code, 17)
        end1 = btest(syntax_code, 18)
        end2 = btest(syntax_code, 19)
        prefix = btest(syntax_code, 20)
        comstyleb = syntax_flags_comment_styleb(syntax_code)
        comstylec = syntax_flags_comment_stylec(syntax_code)
        comnested = btest(syntax_code, 22)

        if (SMAX <= code .or. syntax%match < -1 .or. syntax%match > MAX_CHAR) then
            text = "invalid"
            return
        end if

        text = syntax_code_spec(code + 1:code + 1)

        if (syntax%match == -1) then
            text = text//" "
        else
            text = text//char_utf8(syntax%match)
        end if

        if (start1) text = text//"1"
        if (start2) text = text//"2"
        if (end1) text = text//"3"
        if (end2) text = text//"4"
        if (prefix) text = text//"p"
        if (comstyleb) text = text//"b"
        if (comstylec) text = text//"c"
        if (comnested) text = text//"n"
        text = text//achar(9)//"which means: "//class_name(code)
        if (syntax%match /= -1) text = text//", matches "//char_utf8(syntax%match)
        if (start1) text = text//","//achar(10)//achar(9)//"  is the first character of a comment-start sequence"
        if (start2) text = text//","//achar(10)//achar(9)//"  is the second character of a comment-start sequence"
        if (end1) text = text//","//achar(10)//achar(9)//"  is the first character of a comment-end sequence"
        if (end2) text = text//","//achar(10)//achar(9)//"  is the second character of a comment-end sequence"
        if (comstyleb) text = text//" (comment style b)"
        if (comstylec) text = text//" (comment style c)"
        if (comnested) text = text//" (nestable)"

        if (prefix) then
            text = text//format_message(","//achar(10)//achar(9)// &
                                        "  is a prefix character for `backward_prefix_chars'")
        end if
    end function internal_describe_syntax_value

    !> The description of syntax class CODE (part of
    !> syntax.c:Finternal_describe_syntax_value).  Not a port.
    pure function class_name(code) result(name)
        integer, intent(in) :: code
        character(len=:), allocatable :: name

        select case (code)
        case (SWHITESPACE)
            name = "whitespace"
        case (SPUNCT)
            name = "punctuation"
        case (SWORD)
            name = "word"
        case (SSYMBOL)
            name = "symbol"
        case (SOPEN)
            name = "open"
        case (SCLOSE)
            name = "close"
        case (SQUOTE)
            name = "prefix"
        case (SSTRING)
            name = "string"
        case (SMATH)
            name = "math"
        case (SESCAPE)
            name = "escape"
        case (SCHARQUOTE)
            name = "charquote"
        case (SCOMMENT)
            name = "comment"
        case (SENDCOMMENT)
            name = "endcomment"
        case (SINHERIT)
            name = "inherit"
        case (SCOMMENT_FENCE)
            name = "comment fence"
        case default
            name = "string fence"
        end select
    end function class_name

    !> The UTF-8 form of character C.  Not a port.
    function char_utf8(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: tmp
        integer(int32) :: n
        type(status_type) :: st

        call char_string(int(c, int32), tmp, n, st)
        s = tmp(1:n)
    end function char_utf8

    !> Return the matching parenthesis of CHARACTER, or nil if none.
    !> (-1 is nil.)  Port of syntax.c:Fmatching_paren (with
    !> syntax.c:SYNTAX_MATCH, the descriptor's `match').
    integer function matching_paren(c)
        integer, intent(in) :: c
        type(syntax_descriptor_type) :: d

        d = syntax_entry(c)
        matching_paren = -1
        if (iand(d%code, 255) == SOPEN .or. iand(d%code, 255) == SCLOSE) matching_paren = d%match
    end function matching_paren

    !> Convert a syntax descriptor STRING into a raw syntax descriptor.
    !> STRING should be a string of the form allowed as argument of
    !> `modify_syntax_entry'.  The return value is a raw syntax descriptor:
    !> (CODE . MATCHING-CHAR); "@" (inherit) gives nil, code -1.  Port of
    !> syntax.c:Fstring_to_syntax.
    subroutine string_to_syntax(string, d, status)
        character(len=*), intent(in) :: string
        type(syntax_descriptor_type), intent(out) :: d
        type(status_type), intent(out) :: status
        integer :: val, p
        integer(int32) :: ch, length

        status = status_type()

        if (len(string) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid syntax description letter: ")
            return
        end if

        val = syntax_spec_code(string(1:1))

        if (val < 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid syntax description letter: "//string(1:1))
            return
        end if

        if (val == SINHERIT) return
        p = 2

        if (p <= len(string)) then
            call string_char_and_length(string, int(p, int64), ch, length)
            if (ch /= iachar(" ")) d%match = ch
            p = p + length
        end if

        do while (p <= len(string))
            select case (string(p:p))
            case ("1")
                val = ior(val, ishft(1, 16))
            case ("2")
                val = ior(val, ishft(1, 17))
            case ("3")
                val = ior(val, ishft(1, 18))
            case ("4")
                val = ior(val, ishft(1, 19))
            case ("p")
                val = ior(val, ishft(1, 20))
            case ("b")
                val = ior(val, ishft(1, 21))
            case ("n")
                val = ior(val, ishft(1, 22))
            case ("c")
                val = ior(val, ishft(1, 23))
            case default
            end select

            p = p + 1
        end do

        d%code = val
    end subroutine string_to_syntax

    !> Set syntax for character CHAR according to string NEWENTRY.
    !> The syntax is changed only for table SYNTAX_TABLE, which defaults to
    !>  the current buffer's syntax table.
    !> The characters `from' to `to' (one character if equal) are changed.
    !> The first character of NEWENTRY should be one of the following:
    !>   Space or -  whitespace syntax.    w   word constituent.
    !>   _           symbol constituent.   .   punctuation.
    !>   (           open-parenthesis.     )   close-parenthesis.
    !>   "           string quote.         \   escape.
    !>   $           paired delimiter.     \='   expression quote or prefix operator.
    !>   <           comment starter.      >   comment ender.
    !>   /           character-quote.      @   inherit from parent table.
    !>   |           generic string fence. !   generic comment fence.
    !> The second character of NEWENTRY is the matching parenthesis,
    !>  used only if the first character is `(' or `)'.
    !> Any additional characters are flags: 1, 2, 3, 4, b, c, n and p
    !> (comment sequences and prefix characters; see the Emacs manual).
    !> Port of syntax.c:Fmodify_syntax_entry.
    subroutine modify_syntax_entry(from, to, newentry, status, syntax_table_h)
        integer, intent(in) :: from, to
        character(len=*), intent(in) :: newentry
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: syntax_table_h
        type(syntax_descriptor_type) :: d
        type(handle_type) :: h
        integer(int64) :: i

        status = status_type()

        if (from < 0 .or. from > MAX_CHAR .or. to < 0 .or. to > MAX_CHAR) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            return
        end if

        if (present(syntax_table_h)) then
            h = syntax_table_h
        else
            h = syntax_table()
        end if

        call resolve_table(h, i, status)
        if (.not. status%ok()) return
        call string_to_syntax(newentry, d, status)
        if (.not. status%ok()) return
        ! A reversed range sets nothing (char_table_set_range).
        if (from <= to) call set_range(i, from, to, d)
        ! (clear_regexp_cache): compiled regexps that used syntax are stale.
        syntax_modiff = syntax_modiff + 1
    end subroutine modify_syntax_entry

    !> The syntax class designated by C, or -1.  Port of
    !> syntax.c:syntax_spec_code (`-' and ` ' are whitespace, `W' and
    !> `w' are word).
    integer function syntax_spec_code(c) result(class)
        character, intent(in) :: c

        select case (c)
        case ("-", " ")
            class = SWHITESPACE
        case ("W")
            class = SWORD
        case default
            class = index(syntax_code_spec, c) - 1
        end select
    end function syntax_spec_code

    ! ------------------------------------------------------------------
    ! Words
    ! ------------------------------------------------------------------

    !> Whether there is a word boundary between the word-constituent
    !> characters `c1' and `c2'.  Port of category.c:word_boundary_p, with
    !> characters.el's `word_combining_categories' ((nil . ^) (^ . nil)
    !> (C . H) (C . K)) and `word_separating_categories' ((H . K)).
    logical function word_boundary_p(c1, c2)
        integer, intent(in) :: c1, c2

        if (char_script(c1) == char_script(c2)) then
            word_boundary_p = between(c1, c2, "H", "K")
        else
            word_boundary_p = .not. (between(c1, c2, " ", "^") .or. between(c1, c2, "^", " ") .or. &
                                     between(c1, c2, "C", "H") .or. between(c1, c2, "C", "K"))
        end if

    contains

        !> Whether (CAT1 . CAT2) matches: c1 has CAT1 but not CAT2, and c2
        !> has CAT2 but not CAT1; a blank category is nil, which matches.
        logical function between(a, b, cat1, cat2)
            integer, intent(in) :: a, b
            character, intent(in) :: cat1, cat2

            between = .true.
            if (cat1 /= " ") between = char_has_category(a, cat1) .and. .not. char_has_category(b, cat1)
            if (.not. between) return
            if (cat2 /= " ") between = .not. char_has_category(a, cat2) .and. char_has_category(b, cat2)
        end function between
    end function word_boundary_p

    !> Whether syntax class `code' is part of a word.  Not a port.
    logical function word_code_p(code)
        integer, intent(in) :: code

        word_code_p = code == SWORD .or. (words_include_escapes .and. (code == SESCAPE .or. code == SCHARQUOTE))
    end function word_code_p

    !> Return the position across COUNT words from FROM.
    !> If that many words cannot be found before the end of the buffer,
    !> return 0.  COUNT negative means scan backward and stop at word
    !> beginning.  Port of syntax.c:scan_words.
    recursive integer(int64) function scan_words(from_pos, count_in) result(from)
        integer(int64), intent(in) :: from_pos, count_in
        integer(int64) :: count, from_byte, beg, end
        integer :: ch0, ch1, code

        from = from_pos
        count = count_in
        beg = buffers(cur)%begv
        end = buffers(cur)%zv
        from_byte = char_to_byte(from)

        do while (count > 0)
            do
                if (from == end) then
                    from = 0
                    return
                end if

                ch0 = buf_fetch_char(buffers(cur), from_byte)
                code = syntax_class_at(ch0, from)
                from_byte = from_byte + buf_next_char_len(buffers(cur), from_byte)
                from = from + 1
                if (word_code_p(code)) exit
            end do

            ! Now CH0 is a character which begins a word and FROM is the
            ! position of the next character.
            do
                if (from == end) exit
                ch1 = buf_fetch_char(buffers(cur), from_byte)
                code = syntax_class_at(ch1, from)
                if (.not. word_code_p(code)) exit
                if (word_boundary_p(ch0, ch1)) exit
                from_byte = from_byte + buf_next_char_len(buffers(cur), from_byte)
                from = from + 1
                ch0 = ch1
            end do

            count = count - 1
        end do

        do while (count < 0)
            do
                if (from == beg) then
                    from = 0
                    return
                end if

                from_byte = from_byte - buf_prev_char_len(buffers(cur), from_byte)
                from = from - 1
                ch1 = buf_fetch_char(buffers(cur), from_byte)
                code = syntax_class_at(ch1, from)
                if (word_code_p(code)) exit
            end do

            ! Now CH1 is a character which ends a word and FROM is the
            ! position of it.
            do
                if (from == beg) exit
                from_byte = from_byte - buf_prev_char_len(buffers(cur), from_byte)
                from = from - 1
                ch0 = buf_fetch_char(buffers(cur), from_byte)
                code = syntax_class_at(ch0, from)

                if (.not. word_code_p(code) .or. word_boundary_p(ch0, ch1)) then
                    from_byte = from_byte + buf_next_char_len(buffers(cur), from_byte)
                    from = from + 1
                    exit
                end if

                ch1 = ch0
            end do

            count = count + 1
        end do
    end function scan_words

    !> Move point forward ARG words (backward if ARG is negative).
    !> If ARG is omitted, move point forward one word.
    !> Normally returns true (in OK).
    !> If an edge of the buffer or a field boundary is reached, point is
    !> left there and the function returns false.  Field boundaries are not
    !> noticed if `inhibit_field_text_motion' is true.
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char_script_table').
    !> Port of syntax.c:Fforward_word (`ok' is the value; no
    !> `find_word_boundary_function_table').
    recursive subroutine forward_word(arg, ok)
        integer(int64), intent(in) :: arg
        logical, intent(out), optional :: ok
        integer(int64) :: orig_val, val

        orig_val = scan_words(point(), arg)
        val = orig_val

        if (orig_val == 0) then
            if (arg > 0) then
                val = buffers(cur)%zv
            else
                val = buffers(cur)%begv
            end if
        end if

        ! Avoid jumping out of an input field.
        val = constrain_to_field(val, point(), .false., .false.)
        call goto_char(val)
        if (present(ok)) ok = val == orig_val
    end subroutine forward_word

    !> Move point forward ARG words (backward if ARG is negative).
    !> If ARG is omitted, move point forward one word.
    !> If an edge of the buffer or a field boundary is reached, point is
    !> left there.
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char_script_table').
    !> Port of syntax.c:Fforward_word (the command).
    subroutine cmd_forward_word(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call forward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_forward_word

    !> Register the commands.  Port of syntax.c:syms_of_syntax in part.
    subroutine syms_of_syntax()
        call forward_setting(parse_expression_lookup_properties, local_if_set=.true.)
        call forward_setting(syntax_propertize__done, local_if_set=.true.)
        commands%forward_word%run => cmd_forward_word
    end subroutine syms_of_syntax

    ! ------------------------------------------------------------------
    ! Skipping characters and syntax classes
    ! ------------------------------------------------------------------

    !> Move point forward, stopping before a char not in STRING, or at pos LIM.
    !> STRING is like the inside of a `[...]' in a regular expression
    !> except that `]' is never special and `\' quotes `^', `-' or `\'
    !>  (but not at the end of a range; quoting is never needed there).
    !> Thus, with arg "a-zA-Z", this skips letters stopping before first nonletter.
    !> With arg "^a-zA-Z", skips nonletters stopping before first letter.
    !> Char classes, e.g. `[:alpha:]', are supported.
    !>
    !> Returns the distance traveled, either zero or positive.  Port of
    !> syntax.c:Fskip_chars_forward.
    integer(int64) function skip_chars_forward(string, status, lim) result(moved)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: lim

        moved = skip_chars(.true., string, status, lim)
    end function skip_chars_forward

    !> Move point backward, stopping after a char not in STRING, or at pos LIM.
    !> See `skip_chars_forward' for details.
    !> Returns the distance traveled, either zero or negative.  Port of
    !> syntax.c:Fskip_chars_backward.
    integer(int64) function skip_chars_backward(string, status, lim) result(moved)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: lim

        moved = skip_chars(.false., string, status, lim)
    end function skip_chars_backward

    !> Move point forward across chars in specified syntax classes.
    !> SYNTAX is a string of syntax code characters.
    !> Stop before a char whose syntax is not in SYNTAX, or at position LIM.
    !> If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
    !> This function returns the distance traveled, either zero or positive.
    !> Port of syntax.c:Fskip_syntax_forward.
    recursive integer(int64) function skip_syntax_forward(syntax, lim) result(moved)
        character(len=*), intent(in) :: syntax
        integer(int64), intent(in), optional :: lim

        moved = skip_syntaxes(.true., syntax, lim)
    end function skip_syntax_forward

    !> Move point backward across chars in specified syntax classes.
    !> SYNTAX is a string of syntax code characters.
    !> Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
    !> If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
    !> This function returns either zero or a negative number, and the absolute value
    !> of this is the distance traveled.  Port of
    !> syntax.c:Fskip_syntax_backward.
    recursive integer(int64) function skip_syntax_backward(syntax, lim) result(moved)
        character(len=*), intent(in) :: syntax
        integer(int64), intent(in), optional :: lim

        moved = skip_syntaxes(.false., syntax, lim)
    end function skip_syntax_backward

    !> The limit of a skip: LIM (default the end of the accessible region
    !> in the direction of motion), kept inside the accessible region.
    !> Not a port.
    integer(int64) function skip_limit(forwardp, lim) result(limit)
        logical, intent(in) :: forwardp
        integer(int64), intent(in), optional :: lim

        associate (b => buffers(cur))
            limit = merge(b%zv, b%begv, forwardp)
            if (present(lim)) limit = lim
            limit = max(b%begv, min(b%zv, limit))
        end associate
    end function skip_limit

    !> Port of syntax.c:skip_syntaxes.
    recursive integer(int64) function skip_syntaxes(forwardp, syntax, lim) result(moved)
        logical, intent(in) :: forwardp
        character(len=*), intent(in) :: syntax
        integer(int64), intent(in), optional :: lim
        logical :: classes(0:15), negate
        integer :: i, at
        integer(int64) :: limit, start, pos, pos_byte
        integer(int32) :: c

        classes = .false.
        negate = .false.
        i = 1

        if (len(syntax) > 0) then
            if (syntax(1:1) == "^") then
                negate = .true.
                i = 2
            end if
        end if

        do while (i <= len(syntax))
            at = syntax_spec_code(syntax(i:i))
            if (at >= 0) classes(at) = .true.
            i = i + 1
        end do

        limit = skip_limit(forwardp, lim)
        start = point()
        pos = start

        associate (b => buffers(cur))
            if (forwardp) then
                do while (pos < limit)
                    pos_byte = char_to_byte(pos)
                    c = buf_fetch_char(b, pos_byte)
                    if (classes(syntax_class_at(int(c), pos)) .eqv. negate) exit
                    pos = pos + 1
                end do
            else
                do while (pos > limit)
                    pos_byte = char_to_byte(pos)

                    if (b%multibyte) then
                        pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
                    else
                        pos_byte = pos_byte - 1
                    end if

                    c = buf_fetch_char(b, pos_byte)
                    if (classes(syntax_class_at(int(c), pos - 1)) .eqv. negate) exit
                    pos = pos - 1
                end do
            end if
        end associate

        call goto_char(pos)
        moved = pos - start
    end function skip_syntaxes

    !> Move point backward over any number of chars with prefix syntax.
    !> This includes chars with expression prefix syntax class (') and those with
    !> the prefix syntax flag (p).  Port of syntax.c:Fbackward_prefix_chars.
    recursive subroutine backward_prefix_chars()
        integer(int64) :: pos, opoint
        integer :: c
        type(syntax_descriptor_type) :: d

        opoint = point()
        pos = opoint

        do while (pos > buffers(cur)%begv)
            c = fetch_char_before(pos)
            if (char_quoted(pos - 1)) exit
            d = syntax_entry_at(c, pos - 1)
            if (iand(d%code, 255) /= SQUOTE .and. .not. btest(d%code, 20)) exit
            pos = pos - 1
            opoint = pos
        end do

        call goto_char(opoint)
    end subroutine backward_prefix_chars

    !> The data of the `scan_error' in STATUS: its message and the start
    !> and end of the obstacle (an empty message and 0 when STATUS holds no
    !> scan error).  Not a port: upstream takes them from the error's data.
    subroutine scan_error_data(status, message, beg, end)
        type(status_type), intent(in) :: status
        character(len=:), allocatable, intent(out) :: message
        integer(int64), intent(out) :: beg, end
        integer :: q, c, ios

        message = ""
        beg = 0
        end = 0
        if (status%code /= FM_SCAN_ERROR .or. .not. allocated(status%detail)) return
        ! "MESSAGE", BEG, END
        if (index(status%detail, """") /= 1) return
        q = index(status%detail(2:), """") + 1
        if (q < 2) return
        message = status%detail(2:q - 1)
        c = index(status%detail(q + 1:), ",") + q
        if (c == q) return
        q = index(status%detail(c + 1:), ",") + c
        if (q == c) return
        read (status%detail(c + 1:q - 1), *, iostat=ios) beg
        if (ios /= 0) beg = 0
        read (status%detail(q + 1:), *, iostat=ios) end
        if (ios /= 0) end = 0
    end subroutine scan_error_data

    ! The character before POS.  Not a port.
    integer function fetch_char_before(pos) result(c)
        integer(int64), intent(in) :: pos
        integer(int64) :: pos_byte

        associate (b => buffers(cur))
            pos_byte = char_to_byte(pos)

            if (b%multibyte) then
                pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
            else
                pos_byte = pos_byte - 1
            end if

            c = int(buf_fetch_char(b, pos_byte))
        end associate
    end function fetch_char_before

    !> Whether the character at POS is quoted by an odd number of escape
    !> or character-quote characters before it.  Port of
    !> syntax.c:char_quoted.
    recursive logical function char_quoted(pos)
        integer(int64), intent(in) :: pos
        integer(int64) :: p
        integer :: k

        char_quoted = .false.
        p = pos

        do while (p > buffers(cur)%begv)
            k = syntax_class_at(fetch_char_before(p), p - 1)
            if (k /= SESCAPE .and. k /= SCHARQUOTE) exit
            char_quoted = .not. char_quoted
            p = p - 1
        end do
    end function char_quoted
end module syntax_fm
