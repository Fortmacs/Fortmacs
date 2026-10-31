! syntax_scan_fm.f90 -- comments and balanced expressions, a submodule of
! syntax_fm.
! Ported from GNU Emacs 31.1 src/syntax.c.
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

!> Moving over comments (`forward_comment', with forw_comment and
!> back_comment), groups and expressions (`scan_groups', `scan_expressions') and
!> parsing (`parse_partial_expression', scan_expressions_forward): comment styles a,
!> b and c, nested comments, two-character delimiters (syntax flags 1, 2,
!> 3 and 4), generic comment and string fences, `comment_end_can_be_escaped'
!> and `parse_expression_ignore_comments'.  The start of a subprogram that
!> back_comment falls back on comes from `syntax_ppss' while
!> `comment_use_syntax_ppss' is true (as by default), else from an open
!> paren in column 0 (`open_paren_in_column_0_is_subprogram_start').
!>
!> Upstream's gotos become a loop over labelled steps in
!> scan_expressions_forward and exits of named loops elsewhere.  A parse state is
!> a `parse_state_type' outside and the internal `lisp_parse_state' here,
!> as upstream converts between the list and struct lisp_parse_state.
!>
!> Deferred: `syntax_table' text properties (UPDATE_SYNTAX_TABLE is a
!> no-op), `forward_comment_function', quitting (rarely_quit), the
!> redisplay ticks of scan_expressions_forward, unibyte buffers' raw bytes
!> (FETCH_CHAR_AS_MULTIBYTE).
submodule(syntax_fm) syntax_scan_fm
    use buffer_fm, only: temp_set_point_both
    use editfns_fm, only: validate_region
    use search_fm, only: scan_newline
    use print_fm, only: fortran_string
    implicit none(type, external)

    !> struct lisp_parse_state: the internal form of the parse state.
    type :: lisp_parse_state
        !> Depth at end of parsing.
        integer(int64) :: depth = 0
        !> -1 if not within string, else desired terminator.
        integer :: instring = -1
        !> -1 if in unnestable comment else comment nesting.
        integer(int64) :: incomment = 0
        !> Comment style a=0, or b=1, or ST_COMMENT_STYLE.
        integer :: comstyle = 0
        !> True if just after an escape char at end of parsing.
        logical :: quoted = .false.
        !> Minimum depth seen while scanning.
        integer(int64) :: mindepth = 0
        !> Char number of most recent start-of-expression at current level.
        integer(int64) :: thislevelstart = -1
        !> Char number of start of containing expression.
        integer(int64) :: prevlevelstart = -1
        !> Char number at which parsing stopped, and its byte position.
        integer(int64) :: location = 0, location_byte = 0
        !> Position of last comment/string starter.
        integer(int64) :: comstr_start = -1
        !> Char numbers of starts-of-expression of levels (starting from
        !> outermost).
        integer(int64), allocatable :: levelstarts(:)
        !> Syntax of previous position scanned, when that position
        !> (potentially) holds the first char of a 2-char construct, i.e.
        !> comment delimiter or Sescape, etc.  SMAX otherwise.
        integer :: prev_syntax = SMAX
    end type lisp_parse_state

    !> A cache for finding the start of a subprogram: find_start_pos is the
    !> place for which the subprogram start was found, find_start_value the
    !> subprogram start position found for it (find_start_value_byte its byte
    !> position), in the buffer in slot find_start_buffer (with serial
    !> find_start_serial) when BEGV was find_start_begv and MODIFF
    !> find_start_modiff.
    integer(int64) :: find_start_pos = 0, find_start_value = 0, find_start_value_byte = 0
    integer(int64) :: find_start_buffer = 0, find_start_serial = -1, find_start_begv = 0, find_start_modiff = 0

    !> The labelled steps of scan_expressions_forward.
    integer, parameter :: L_LOOP = 1, L_STARTQUOTED = 2, L_SYMSTARTED = 3, L_ATCOMMENT = 4, &
                          L_STARTINCOMMENT = 5, L_STARTINSTRING = 6, L_STARTQUOTEDINSTRING = 7, &
                          L_STOP = 8, L_ENDQUOTED = 9, L_DONE = 10

contains

    ! ------------------------------------------------------------------
    ! Syntax flags (syntax.c SYNTAX_FLAGS_*)
    ! ------------------------------------------------------------------

    !> The syntax of character C with its flags.  SYNTAX_WITH_FLAGS.
    !> Port of syntax.h:SYNTAX_WITH_FLAGS.
    integer function syntax_with_flags(c)
        integer, intent(in) :: c
        type(syntax_descriptor_type) :: d

        d = syntax_entry(c)
        syntax_with_flags = d%code
    end function syntax_with_flags

    !> The syntax class of character C.  SYNTAX.  Port of syntax.h:SYNTAX.
    integer function syntax_of(c)
        integer, intent(in) :: c

        syntax_of = iand(syntax_with_flags(c), 255)
    end function syntax_of

    !> The syntax of character C at position POS with its flags:
    !> SYNTAX_WITH_FLAGS after UPDATE_SYNTAX_TABLE (POS).
    !> Port of syntax.h:syntax_property_with_flags.
    recursive integer function swf_at(c, pos)
        integer, intent(in) :: c
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d

        d = syntax_entry_at(c, pos)
        swf_at = d%code
    end function swf_at

    !> The syntax class of character C at position POS: SYNTAX after
    !> UPDATE_SYNTAX_TABLE (POS).  Port of syntax.h:syntax_property.
    recursive integer function syn_at(c, pos)
        integer, intent(in) :: c
        integer(int64), intent(in) :: pos

        syn_at = iand(swf_at(c, pos), 255)
    end function syn_at

    ! Port of syntax.c:SYNTAX_FLAGS_COMSTART_FIRST.
    logical function comstart_first(flags)
        integer, intent(in) :: flags

        comstart_first = btest(flags, 16)
    end function comstart_first

    ! Port of syntax.c:SYNTAX_FLAGS_COMSTART_SECOND.
    logical function comstart_second(flags)
        integer, intent(in) :: flags

        comstart_second = btest(flags, 17)
    end function comstart_second

    ! Port of syntax.c:SYNTAX_FLAGS_COMEND_FIRST.
    logical function comend_first(flags)
        integer, intent(in) :: flags

        comend_first = btest(flags, 18)
    end function comend_first

    ! Port of syntax.c:SYNTAX_FLAGS_COMEND_SECOND.
    logical function comend_second(flags)
        integer, intent(in) :: flags

        comend_second = btest(flags, 19)
    end function comend_second

    ! Port of syntax.c:SYNTAX_FLAGS_COMSTARTEND_FIRST.
    logical function comstartend_first(flags)
        integer, intent(in) :: flags

        comstartend_first = iand(flags, int(z'50000')) /= 0
    end function comstartend_first

    ! Port of syntax.c:SYNTAX_FLAGS_PREFIX.
    logical function prefix_flag(flags)
        integer, intent(in) :: flags

        prefix_flag = btest(flags, 20)
    end function prefix_flag

    ! Port of syntax.c:SYNTAX_FLAGS_COMMENT_NESTED.
    logical function comment_nested(flags)
        integer, intent(in) :: flags

        comment_nested = btest(flags, 22)
    end function comment_nested

    !> SYNTAX_FLAGS_COMMENT_STYLEC2: 2 if FLAGS have the style c flag.
    !> Port of syntax.c:SYNTAX_FLAGS_COMMENT_STYLEC2.
    integer function stylec2(flags)
        integer, intent(in) :: flags

        stylec2 = iand(ishft(flags, -22), 2)
    end function stylec2

    !> SYNTAX_FLAGS_COMMENT_STYLE.  FLAGS should be the flags of the main
    !> char of the comment marker, e.g. the second for comstart and the
    !> first for comend.  Port of syntax.c:SYNTAX_FLAGS_COMMENT_STYLE.
    integer function comment_style(flags, other_flags)
        integer, intent(in) :: flags, other_flags

        comment_style = ior(ior(merge(1, 0, syntax_flags_comment_styleb(flags)), stylec2(flags)), stylec2(other_flags))
    end function comment_style

    !> Return syntax code of character C if C is an ASCII character or if
    !> MULTIBYTE_SYMBOL_P is false.  Otherwise, return Ssymbol.  POS is the
    !> position of C.  Port of syntax.c:syntax_multibyte.
    recursive integer function syntax_multibyte(c, multibyte_symbol_p, pos)
        integer, intent(in) :: c
        logical, intent(in) :: multibyte_symbol_p
        integer(int64), intent(in) :: pos

        if (c < 128 .or. .not. multibyte_symbol_p) then
            syntax_multibyte = syn_at(c, pos)
        else
            syntax_multibyte = SSYMBOL
        end if
    end function syntax_multibyte

    ! ------------------------------------------------------------------
    ! Positions
    ! ------------------------------------------------------------------

    !> The character at byte position POS_BYTE.  FETCH_CHAR_AS_MULTIBYTE.
    !> Port of buffer.h:FETCH_CHAR_AS_MULTIBYTE.
    integer function fetch(pos_byte)
        integer(int64), intent(in) :: pos_byte

        fetch = int(buf_fetch_char(buffers(cur), pos_byte))
    end function fetch

    !> INC_BOTH.  Port of buffer.h:inc_both.
    subroutine inc_both(pos, pos_byte)
        integer(int64), intent(inout) :: pos, pos_byte

        if (buffers(cur)%multibyte) then
            pos_byte = pos_byte + buf_next_char_len(buffers(cur), pos_byte)
        else
            pos_byte = pos_byte + 1
        end if

        pos = pos + 1
    end subroutine inc_both

    !> DEC_BOTH.  Port of buffer.h:dec_both.
    subroutine dec_both(pos, pos_byte)
        integer(int64), intent(inout) :: pos, pos_byte

        pos_byte = dec_bytepos(pos_byte)
        pos = pos - 1
    end subroutine dec_both

    !> Return the bytepos one character before BYTEPOS.  We assume that
    !> BYTEPOS is not at the start of the buffer.  Port of
    !> syntax.c:dec_bytepos.
    recursive integer(int64) function dec_bytepos(bytepos)
        integer(int64), intent(in) :: bytepos

        if (buffers(cur)%multibyte) then
            dec_bytepos = bytepos - buf_prev_char_len(buffers(cur), bytepos)
        else
            dec_bytepos = bytepos - 1
        end if
    end function dec_bytepos

    !> Return true if the character at CHARPOS is quoted.  Port of
    !> syntax.c:char_quoted (here because gfortran drops syntax_fm's
    !> private copy from what the submodule can link to).
    recursive logical function char_quoted_at(charpos_in)
        integer(int64), intent(in) :: charpos_in
        integer(int64) :: charpos, bytepos
        integer :: code

        charpos = charpos_in
        bytepos = char_to_byte(charpos)
        char_quoted_at = .false.

        do while (charpos > buffers(cur)%begv)
            call dec_both(charpos, bytepos)
            code = syn_at(fetch(bytepos), charpos)
            if (code /= SCHARQUOTE .and. code /= SESCAPE) exit
            char_quoted_at = .not. char_quoted_at
        end do
    end function char_quoted_at

    !> SET_PT_BOTH.  Port of buffer.h:SET_PT_BOTH.
    subroutine set_pt_both(pos, pos_byte)
        integer(int64), intent(in) :: pos, pos_byte

        call temp_set_point_both(buffers(cur), pos, pos_byte)
    end subroutine set_pt_both

    !> Signal `scan_error' with MESSAGE and the positions A and B.
    !> Not a port.
    subroutine scan_error(status, message, a, b)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: message
        integer(int64), intent(in) :: a, b
        character(len=24) :: sa, sb

        write (sa, "(i0)") a
        write (sb, "(i0)") b
        call set_error(status, FM_SCAN_ERROR, fortran_string(message)//", "//trim(sa)//", "//trim(sb))
    end subroutine scan_error

    ! ------------------------------------------------------------------
    ! The start of a subprogram
    ! ------------------------------------------------------------------

    !> Return a defun-start position before POS and not too far before.
    !> It should be the last one before POS, or nearly the last.
    !>
    !> When open_paren_in_column_0_is_subprogram_start is false, only the
    !> beginning of the buffer is treated as a defun-start.
    !>
    !> We record the information about where the scan started and what its
    !> result was, so that another call in the same area can return the
    !> same value very quickly.  Port of syntax.c:find_defun_start.
    recursive integer(int64) function find_subprogram_start(pos, pos_byte) result(start)
        integer(int64), intent(in) :: pos, pos_byte
        type(parse_state_type) :: ppss
        integer(int64) :: opoint, opoint_byte, pt, pt_byte
        integer :: c

        associate (b => buffers(cur))
            opoint = b%pt
            opoint_byte = b%pt_byte

            ! Use previous finding, if it's valid and applies to this
            ! inquiry.
            if (find_start_buffer == cur .and. find_start_serial == b%serial) then
                ! Reuse the defun-start even if POS is a little farther on.
                ! POS might be in the next subprogram, but that's ok.  Our value
                ! may not be the best possible, but will still be usable.
                if (pos <= find_start_pos + 1000 .and. pos >= find_start_value .and. &
                    b%begv == find_start_begv .and. b%text%modiff == find_start_modiff) then
                    start = find_start_value
                    return
                end if
            end if

            if (comment_use_syntax_ppss) then
                call syntax_ppss(ppss, pos)
                call temp_set_point_both(b, opoint, opoint_byte)

                if (ppss%comstr_start /= 0) then
                    find_start_value = ppss%comstr_start
                    find_start_value_byte = char_to_byte(find_start_value)
                else
                    find_start_value = pos
                    find_start_value_byte = pos_byte
                end if
            else if (.not. open_paren_in_column_0_is_subprogram_start) then
                find_start_value = b%begv
                find_start_value_byte = b%begv_byte
            else
                ! Back up to start of line.
                call scan_newline(pos, pos_byte, b%begv, b%begv_byte, -1_int64)

                do while (b%pt > b%begv)
                    ! Open-paren at start of line means we may have found
                    ! our defun-start.
                    c = fetch(b%pt_byte)

                    if (syntax_of(c) == SOPEN) then
                        ! We accept only those `^\s(' which are good in
                        ! global _and_ text-property syntax-tables.
                        if (syn_at(c, b%pt) == SOPEN) exit
                    end if

                    ! Move to beg of previous line.
                    pt = b%pt
                    pt_byte = b%pt_byte
                    call scan_newline(pt, pt_byte, b%begv, b%begv_byte, -2_int64)
                end do

                ! Record what we found, for the next try.
                find_start_value = b%pt
                find_start_value_byte = b%pt_byte
                call temp_set_point_both(b, opoint, opoint_byte)
            end if

            find_start_buffer = cur
            find_start_serial = b%serial
            find_start_modiff = b%text%modiff
            find_start_begv = b%begv
            find_start_pos = pos
        end associate

        start = find_start_value
    end function find_subprogram_start

    ! ------------------------------------------------------------------
    ! Comments
    ! ------------------------------------------------------------------

    !> Return the SYNTAX_COMEND_FIRST of the character before POS,
    !> POS_BYTE.  Port of syntax.c:prev_char_comend_first (with
    !> syntax.c:SYNTAX_COMEND_FIRST at that position).
    recursive logical function prev_char_comend_first(pos_in, pos_byte_in)
        integer(int64), intent(in) :: pos_in, pos_byte_in
        integer(int64) :: pos, pos_byte

        pos = pos_in
        pos_byte = pos_byte_in
        call dec_both(pos, pos_byte)
        prev_char_comend_first = comend_first(swf_at(fetch(pos_byte), pos))
    end function prev_char_comend_first

    !> Check whether charpos FROM is at the end of a comment.  FROM_BYTE
    !> is the bytepos corresponding to FROM.  Do not move back before STOP.
    !>
    !> Return true if we find a comment ending at FROM/FROM_BYTE.
    !>
    !> If successful, store the charpos of the comment's beginning into
    !> CHARPOS, and the bytepos into BYTEPOS.  Port of
    !> syntax.c:back_comment.
    recursive logical function back_comment(from_in, from_byte_in, stop, comnested, comstyle, charpos, bytepos) result(found)
        integer(int64), intent(in) :: from_in, from_byte_in, stop
        logical, intent(in) :: comnested
        integer, intent(in) :: comstyle
        integer(int64), intent(out) :: charpos, bytepos
        integer, parameter :: ENDED = 0, LOSSAGE = 1, DONE = 2
        type(lisp_parse_state) :: state
        integer(int64) :: from, from_byte, comment_end, comment_end_byte, comstart_pos, comstart_byte, &
                          subprogram_start, subprogram_start_byte, nesting, next, next_byte, temp_byte
        integer :: string_style, code, c, syntax, prev_syntax, next_syntax, outcome
        logical :: string_lossage, comment_lossage, com2start, com2end, comstart, lose, adjusted, at_bol

        ! Look back, counting the parity of string-quotes, and recording
        ! the comment-starters seen.  When we reach a safe place, assume
        ! that's not in a string; then step the main scan to the earliest
        ! comment-starter seen an even number of string quotes away from
        ! the safe place.
        from = from_in
        from_byte = from_byte_in
        ! Presumed outside of any string.
        string_style = -1
        string_lossage = .false.
        ! Not a real lossage: indicates that we have passed a matching
        ! comment starter plus a non-matching comment-ender, meaning that
        ! any matching comment-starter we might see later could be a false
        ! positive (hidden inside another comment).
        ! Test case:  { a (* b } c (* d *)
        comment_lossage = .false.
        comment_end = from
        comment_end_byte = from_byte
        comstart_pos = 0
        comstart_byte = 0
        ! Place where the containing subprogram starts, or 0 if we didn't come
        ! across it yet.
        subprogram_start = 0
        subprogram_start_byte = 0
        ! Current comment nesting.
        nesting = 1
        syntax = 0
        outcome = ENDED

        ! At beginning of range to scan, we're outside of strings; that
        ! determines quote parity to the comment-end.
        scan: do while (from /= stop)
            ! Move back and examine a character.
            call dec_both(from, from_byte)
            prev_syntax = syntax
            c = fetch(from_byte)
            syntax = swf_at(c, from)
            code = iand(syntax, 255)
            ! Check for 2-char comment markers.
            com2start = comstart_first(syntax) .and. comstart_second(prev_syntax) .and. &
                        comstyle == comment_style(prev_syntax, syntax) .and. &
                        ((comment_nested(prev_syntax) .or. comment_nested(syntax)) .eqv. comnested)
            com2end = comend_first(syntax) .and. comend_second(prev_syntax)
            comstart = com2start .or. code == SCOMMENT

            ! If a 2-char comment sequence partly overlaps with another, we
            ! don't try to be clever.  E.g. |*| in C, or }% in modes that
            ! have %..\n and %{..}%.
            if (from > stop .and. (com2end .or. comstart)) then
                next = from
                next_byte = from_byte
                call dec_both(next, next_byte)
                next_syntax = swf_at(fetch(next_byte), next)
                lose = (comstart .or. comnested) .and. comend_second(syntax) .and. comend_first(next_syntax)

                if (.not. lose) then
                    lose = (com2end .or. comnested) .and. comstart_second(syntax) .and. &
                           comstyle == comment_style(syntax, prev_syntax) .and. comstart_first(next_syntax)
                end if

                if (lose) then
                    outcome = LOSSAGE
                    exit scan
                end if
            end if

            ! We're looking at a comment starter.  But it might be a comment
            ! ender as well (see snmp-mode).  The first time we see one, we
            ! need to consider it as a comment starter, and the subsequent
            ! times as a comment ender.
            if (com2start .and. comstart_pos == 0) com2end = .false.

            ! Turn a 2-char comment sequences into the appropriate syntax.
            if (com2end) then
                code = SENDCOMMENT
            else if (com2start) then
                code = SCOMMENT
            else if (code == SCOMMENT) then
                ! Ignore comment starters of a different style.
                if (comstyle /= comment_style(syntax, 0) .or. (comment_nested(syntax) .neqv. comnested)) cycle scan
            end if

            ! Ignore escaped characters, except comment-enders which cannot
            ! be escaped.
            if (comment_end_can_be_escaped%value .or. code /= SENDCOMMENT) then
                if (char_quoted_at(from)) cycle scan
            end if

            select case (code)
            case (SSTRING_FENCE, SCOMMENT_FENCE, SSTRING)
                if (code == SSTRING_FENCE) c = ST_STRING_STYLE
                if (code == SCOMMENT_FENCE) c = ST_COMMENT_STYLE

                ! Track parity of quotes.
                if (string_style == -1) then
                    ! Entering a string.
                    string_style = c
                else if (string_style == c) then
                    ! Leaving the string.
                    string_style = -1
                else
                    ! If we have two kinds of string delimiters.  There's no
                    ! way to grok this scanning backwards.
                    string_lossage = .true.
                end if
            case (SCOMMENT)
                ! We've already checked that it is the relevant comstyle.
                if (string_style /= -1 .or. comment_lossage .or. string_lossage) then
                    ! There are odd string quotes involved, so let's be
                    ! careful.  Test case in Pascal: " { " a { " }
                    outcome = LOSSAGE
                    exit scan
                end if

                if (.not. comnested) then
                    ! Record best comment-starter so far.
                    comstart_pos = from
                    comstart_byte = from_byte
                else
                    nesting = nesting - 1

                    if (nesting <= 0) then
                        ! Nested comments have to be balanced, so we don't
                        ! need to keep looking for earlier ones.
                        outcome = DONE
                        exit scan
                    end if
                end if
            case (SENDCOMMENT)
                if (comment_style(syntax, 0) == comstyle .and. &
                    (((com2end .and. comment_nested(prev_syntax)) .or. comment_nested(syntax)) .eqv. comnested)) then
                    ! This is the same style of comment ender as ours.
                    if (comnested) then
                        nesting = nesting + 1
                    else
                        ! Anything before that can't count because it would
                        ! match this comment-ender rather than ours.
                        from = stop
                    end if
                else if (comstart_pos /= 0 .or. c /= 10) then
                    ! We're mixing comment styles here, so we'd better be
                    ! careful.  The failure should only happen in the rare
                    ! cases such as { (* } *)
                    comment_lossage = .true.
                end if
            case (SOPEN)
                ! Assume a defun-start point is outside of strings.
                if (open_paren_in_column_0_is_subprogram_start .and. .not. comment_use_syntax_ppss) then
                    at_bol = from == stop

                    if (.not. at_bol) then
                        temp_byte = dec_bytepos(from_byte)
                        at_bol = fetch(temp_byte) == 10
                    end if

                    if (at_bol) then
                        subprogram_start = from
                        subprogram_start_byte = from_byte
                        from = stop
                    end if
                end if
            case default
                continue
            end select
        end do scan

        select case (outcome)
        case (ENDED)
            if (comstart_pos == 0) then
                from = comment_end
                from_byte = comment_end_byte
            else
                ! We've found the beginning of the non-nested comment.
                from = comstart_pos
                from_byte = comstart_byte
            end if
        case (LOSSAGE)
            ! We had two kinds of string delimiters mixed up together.
            ! Decode this going forwards.  Scan fwd from a known safe place
            ! (beginning-of-defun) to the one in question; this records
            ! where we last passed a comment starter.
            adjusted = .true.

            ! If we did not already find the subprogram start, find it now.
            if (subprogram_start == 0) then
                subprogram_start = find_subprogram_start(comment_end, comment_end_byte)
                subprogram_start_byte = find_start_value_byte
                adjusted = subprogram_start > buffers(cur)%begv
            end if

            do
                call internalize_parse_state(state)
                call scan_expressions_forward(state, subprogram_start, subprogram_start_byte, comment_end, -huge(0_int64) - 1, &
                                              .false., COMMENTSTOP_NIL)
                subprogram_start = comment_end

                if (.not. adjusted) then
                    adjusted = .true.

                    if (size(state%levelstarts) > 0) then
                        find_start_value = state%levelstarts(1)
                    else if (state%thislevelstart >= 0) then
                        find_start_value = state%thislevelstart
                    end if

                    find_start_value_byte = char_to_byte(find_start_value)
                end if

                if (state%incomment == merge(1_int64, -1_int64, comnested) .and. state%comstyle == comstyle) then
                    from = state%comstr_start
                else
                    from = comment_end

                    if (state%incomment /= 0) then
                        ! If comment_end is inside some other comment, maybe
                        ! ours is nested, so we need to try again from within
                        ! the surrounding comment.  Example: { a (* " *)
                        subprogram_start = state%comstr_start + 2
                        subprogram_start_byte = char_to_byte(subprogram_start)
                    end if
                end if

                if (subprogram_start >= comment_end) exit
            end do

            from_byte = char_to_byte(from)
        case default
            continue
        end select

        charpos = from
        bytepos = from_byte
        found = from /= comment_end
    end function back_comment

    !> Jump over a comment, assuming we are at the beginning of one.  FROM
    !> is the current position.  FROM_BYTE is the bytepos corresponding to
    !> FROM.  Do not move beyond STOP.
    !>
    !> NESTING should be positive to indicate the nesting at the beginning
    !> for nested comments and should be zero or negative else.  ST_COMMENT_STYLE
    !> cannot be nested.  PREV_SYNTAX is the SYNTAX_WITH_FLAGS of the
    !> previous character (or 0).
    !>
    !> If successful, return true and store the charpos of the comment's
    !> end into CHARPOS and the corresponding bytepos into BYTEPOS.  Else,
    !> return false and store the charpos STOP into CHARPOS, the
    !> corresponding bytepos into BYTEPOS and the current nesting (as
    !> defined for state%incomment) in INCOMMENT.  Should the last
    !> character scanned in an incomplete comment be a possible first
    !> character of a two character construct, we store its
    !> SYNTAX_WITH_FLAGS into LAST_SYNTAX.  Otherwise, we store SMAX into
    !> LAST_SYNTAX.
    !>
    !> The comment end is the last character of the comment rather than
    !> the character just after the comment.  Port of syntax.c:forw_comment.
    recursive logical function forw_comment(from_in, from_byte_in, stop, nesting_in, style, prev_syntax_in, charpos, &
                                            bytepos, incomment, last_syntax) result(found)
        integer(int64), intent(in) :: from_in, from_byte_in, stop, nesting_in
        integer, intent(in) :: style, prev_syntax_in
        integer(int64), intent(out) :: charpos, bytepos, incomment
        integer, intent(out) :: last_syntax
        integer(int64) :: from, from_byte, nesting
        integer :: c, code, syntax, other_syntax, prev_syntax
        logical :: in_middle, hit

        from = from_in
        from_byte = from_byte_in
        nesting = nesting_in
        incomment = 0
        if (nesting <= 0) nesting = -1
        ! Enter the loop in the middle so that we find a 2-char comment
        ! ender if we start in the middle of it.
        syntax = prev_syntax_in
        prev_syntax = 0
        code = iand(syntax, 255)
        in_middle = syntax /= 0 .and. from < stop

        do
            if (.not. in_middle) then
                if (from == stop) then
                    incomment = nesting
                    charpos = from
                    bytepos = from_byte
                    last_syntax = SMAX

                    if (code == SESCAPE .or. code == SCHARQUOTE .or. comend_first(syntax)) then
                        last_syntax = syntax
                    else if (nesting > 0 .and. comstart_first(syntax)) then
                        last_syntax = syntax
                    end if

                    found = .false.
                    return
                end if

                c = fetch(from_byte)
                prev_syntax = syntax
                syntax = swf_at(c, from)
                code = iand(syntax, 255)

                if (code == SENDCOMMENT) then
                    if (comment_style(syntax, 0) == style) then
                        if (comment_nested(syntax)) then
                            hit = .false.

                            if (nesting > 0) then
                                nesting = nesting - 1
                                hit = nesting == 0
                            end if
                        else
                            hit = nesting < 0
                        end if

                        if (hit .and. comment_end_can_be_escaped%value) then
                            hit = iand(prev_syntax, 255) /= SESCAPE .and. iand(prev_syntax, 255) /= SCHARQUOTE
                        end if

                        ! We have encountered a comment end of the same style
                        ! as the comment sequence which began this comment
                        ! section.
                        if (hit) exit
                    end if
                end if

                ! We have encountered a comment end of the same style as the
                ! comment sequence which began this comment section.
                if (code == SCOMMENT_FENCE .and. style == ST_COMMENT_STYLE) exit

                ! We have encountered a nested comment of the same style as
                ! the comment sequence which began this comment section.
                if (nesting > 0 .and. code == SCOMMENT) then
                    if (comment_nested(syntax) .and. comment_style(syntax, 0) == style) nesting = nesting + 1
                end if

                if (comment_end_can_be_escaped%value .and. (code == SESCAPE .or. code == SCHARQUOTE)) then
                    call inc_both(from, from_byte)
                    ! Failure.
                    if (from == stop) cycle
                    c = fetch(from_byte)
                    prev_syntax = syntax
                    syntax = SMAX
                    code = syntax
                end if

                call inc_both(from, from_byte)
            end if

            in_middle = .false.

            ! forw_incomment:
            if (from < stop .and. comend_first(syntax)) then
                other_syntax = swf_at(fetch(from_byte), from)
                hit = comend_second(other_syntax)
                if (hit) hit = comment_style(syntax, other_syntax) == style

                if (hit) then
                    if (comment_nested(syntax) .or. comment_nested(other_syntax)) then
                        hit = nesting > 0
                    else
                        hit = nesting < 0
                    end if
                end if

                if (hit) then
                    ! So that "|#" (lisp) can not return the syntax of "#" in
                    ! LAST_SYNTAX.
                    syntax = SMAX
                    nesting = nesting - 1
                    ! We have encountered a comment end of the same style as
                    ! the comment sequence which began this comment section.
                    if (nesting <= 0) exit
                    call inc_both(from, from_byte)
                end if
            end if

            if (nesting > 0 .and. from < stop .and. comstart_first(syntax)) then
                other_syntax = swf_at(fetch(from_byte), from)
                hit = comment_style(other_syntax, syntax) == style .and. comstart_second(other_syntax)
                if (hit) hit = comment_nested(syntax) .or. comment_nested(other_syntax)

                if (hit) then
                    ! We have encountered a nested comment of the same style
                    ! as the comment sequence which began this comment
                    ! section.  So that "#|#" isn't also a comment ender.
                    syntax = SMAX
                    call inc_both(from, from_byte)
                    nesting = nesting + 1
                end if
            end if
        end do

        charpos = from
        bytepos = from_byte
        ! Any syntactic power the last byte had is used up.
        last_syntax = SMAX
        found = .true.
    end function forw_comment

    module procedure forward_comment
    integer(int64) :: from, from_byte, stop, count1, out_charpos, out_bytepos, dummy, ini, ini_byte, c_pos
    integer :: c, c1, code, comstyle, syntax, other_syntax, dummy2
    logical :: comnested, comstart_first_p, quoted, fence_found, leave

    count1 = count

    associate (b => buffers(cur))
        if (count1 > 0) then
            stop = b%zv
        else
            stop = b%begv
        end if

        from = b%pt
        from_byte = b%pt_byte
    end associate

    do while (count1 > 0)
        do
            if (from == stop) then
                call set_pt_both(from, from_byte)
                found = .false.
                return
            end if

            c = fetch(from_byte)
            syntax = swf_at(c, from)
            code = iand(syntax, 255)
            comstart_first_p = comstart_first(syntax)
            comnested = comment_nested(syntax)
            comstyle = comment_style(syntax, 0)
            call inc_both(from, from_byte)

            if (from < stop .and. comstart_first_p) then
                c1 = fetch(from_byte)
                other_syntax = swf_at(c1, from)

                if (comstart_second(other_syntax)) then
                    ! We have encountered a comment start sequence and we
                    ! are ignoring all text inside comments.  We must record
                    ! the comment style this sequence begins so that later,
                    ! only a comment end of the same style actually ends the
                    ! comment section.
                    code = SCOMMENT
                    comstyle = comment_style(other_syntax, syntax)
                    comnested = comnested .or. comment_nested(other_syntax)
                    call inc_both(from, from_byte)
                end if
            end if

            if (code == SWHITESPACE) cycle
            if (code == SENDCOMMENT .and. c == 10) cycle
            exit
        end do

        if (code == SCOMMENT_FENCE) then
            comstyle = ST_COMMENT_STYLE
        else if (code /= SCOMMENT) then
            call dec_both(from, from_byte)
            call set_pt_both(from, from_byte)
            found = .false.
            return
        end if

        ! We're at the start of a comment.
        found = forw_comment(from, from_byte, stop, merge(1_int64, 0_int64, comnested), comstyle, 0, &
                             out_charpos, out_bytepos, dummy, dummy2)
        from = out_charpos
        from_byte = out_bytepos

        if (.not. found) then
            call set_pt_both(from, from_byte)
            return
        end if

        call inc_both(from, from_byte)
        ! We have skipped one comment.
        count1 = count1 - 1
    end do

    do while (count1 < 0)
        leave = .false.

        do
            if (from <= stop) then
                call set_pt_both(buffers(cur)%begv, buffers(cur)%begv_byte)
                found = .false.
                return
            end if

            call dec_both(from, from_byte)
            quoted = char_quoted_at(from)
            c = fetch(from_byte)
            c_pos = from
            syntax = swf_at(c, from)
            code = iand(syntax, 255)
            comstyle = 0
            comnested = comment_nested(syntax)
            if (code == SENDCOMMENT) comstyle = comment_style(syntax, 0)

            if (from > stop .and. comend_second(syntax)) then
                if (prev_char_comend_first(from, from_byte)) then
                    if (.not. char_quoted_at(from - 1)) then
                        ! We must record the comment style encountered so
                        ! that later, we can match only the proper comment
                        ! begin sequence of the same style.
                        call dec_both(from, from_byte)
                        code = SENDCOMMENT
                        c1 = fetch(from_byte)
                        other_syntax = swf_at(c1, from)
                        comstyle = comment_style(other_syntax, syntax)
                        comnested = comnested .or. comment_nested(other_syntax)
                    end if
                end if
            end if

            if (code == SCOMMENT_FENCE) then
                ! Skip until first preceding unquoted comment_fence.
                fence_found = .false.
                ini = from
                ini_byte = from_byte

                if (from > stop) then
                    do
                        call dec_both(from, from_byte)
                        c = fetch(from_byte)

                        if (syn_at(c, from) == SCOMMENT_FENCE) then
                            if (.not. char_quoted_at(from)) then
                                fence_found = .true.
                                exit
                            end if
                        end if

                        if (from == stop) exit
                    end do
                end if

                if (.not. fence_found) then
                    ! Set point to ini + 1.
                    from = ini
                    from_byte = ini_byte
                    leave = .true.
                end if

                ! Else we have skipped one comment.
                exit
            else if (code == SENDCOMMENT) then
                found = .false.

                if (.not. quoted .or. .not. comment_end_can_be_escaped%value) then
                    found = back_comment(from, from_byte, stop, comnested, comstyle, out_charpos, out_bytepos)
                end if

                if (.not. found) then
                    ! An end-of-line that is not an end-of-comment is
                    ! treated like a whitespace.  CC-mode (and maybe others)
                    ! relies on this behavior.
                    if (c /= 10) then
                        ! Failure: we should go back to the end of this
                        ! not-quite-endcomment.
                        ! It was a two-char Sendcomment.
                        if (syn_at(c, c_pos) /= code) call inc_both(from, from_byte)
                        leave = .true.
                        exit
                    end if
                else
                    ! We have skipped one comment.
                    from = out_charpos
                    from_byte = out_bytepos
                    exit
                end if
            else if (code /= SWHITESPACE .or. quoted) then
                leave = .true.
                exit
            end if
        end do

        if (leave) then
            call inc_both(from, from_byte)
            call set_pt_both(from, from_byte)
            found = .false.
            return
        end if

        count1 = count1 + 1
    end do

    call set_pt_both(from, from_byte)
    found = .true.
    end procedure forward_comment

    ! ------------------------------------------------------------------
    ! Groups and expressions
    ! ------------------------------------------------------------------

    !> Port of syntax.c:scan_lists (0 is nil).
    recursive integer(int64) function scan_groups_1(from0, count_in, depth_in, expression_flag, status) result(pos)
        integer(int64), intent(in) :: from0, count_in, depth_in
        logical, intent(in) :: expression_flag
        type(status_type), intent(out) :: status
        integer, parameter :: A_NONE = 0, A_WORD = 1, A_COMMENT = 2, A_OPEN = 3, A_CLOSE = 4, A_STRING = 5
        integer(int64) :: stop, count, depth, min_depth, last_good, from, from_byte, temp_pos, out_charpos, &
                          out_bytepos, dummy
        integer :: c, c1, c2, stringterm, code, comstyle, syntax, other_syntax, action, c_code, dummy2
        logical :: quoted, mathexit, comnested, found, multibyte_symbol_p, comstart_first_p, prefix, done, hit

        status = status_type()
        pos = 0
        count = count_in
        depth = depth_in

        associate (b => buffers(cur))
            if (count > 0) then
                stop = b%zv
            else
                stop = b%begv
            end if

            from = max(b%begv, min(from0, b%zv))
        end associate

        mathexit = .false.
        ! Err out if depth gets less than this.
        min_depth = depth
        last_good = from0
        multibyte_symbol_p = expression_flag .and. multibyte_syntax_as_symbol
        if (depth > 0) min_depth = 0
        from_byte = char_to_byte(from)

        do while (count > 0)
            done = .false.

            forward: do while (from < stop)
                c = fetch(from_byte)
                syntax = swf_at(c, from)
                code = syntax_multibyte(c, multibyte_symbol_p, from)
                comstart_first_p = comstart_first(syntax)
                comnested = comment_nested(syntax)
                comstyle = comment_style(syntax, 0)
                prefix = prefix_flag(syntax)
                if (depth == min_depth) last_good = from
                call inc_both(from, from_byte)

                if (from < stop .and. comstart_first_p) then
                    c = fetch(from_byte)
                    other_syntax = swf_at(c, from)

                    if (comstart_second(other_syntax) .and. parse_expression_ignore_comments%value) then
                        ! We have encountered a comment start sequence and
                        ! we are ignoring all text inside comments.  We must
                        ! record the comment style this sequence begins so
                        ! that later, only a comment end of the same style
                        ! actually ends the comment section.
                        code = SCOMMENT
                        comstyle = comment_style(other_syntax, syntax)
                        comnested = comnested .or. comment_nested(other_syntax)
                        call inc_both(from, from_byte)
                    end if
                end if

                if (prefix) cycle forward
                action = A_NONE

                select case (code)
                case (SESCAPE, SCHARQUOTE)
                    if (from == stop) then
                        call lose()
                        return
                    end if

                    call inc_both(from, from_byte)
                    ! Treat following character as a word constituent.
                    action = A_WORD
                case (SWORD, SSYMBOL)
                    action = A_WORD
                case (SCOMMENT_FENCE)
                    comstyle = ST_COMMENT_STYLE
                    action = A_COMMENT
                case (SCOMMENT)
                    action = A_COMMENT
                case (SMATH)
                    if (expression_flag) then
                        if (from /= stop) then
                            if (c == fetch(from_byte)) call inc_both(from, from_byte)
                        end if

                        if (mathexit) then
                            mathexit = .false.
                            action = A_CLOSE
                        else
                            mathexit = .true.
                            action = A_OPEN
                        end if
                    end if
                case (SOPEN)
                    action = A_OPEN
                case (SCLOSE)
                    action = A_CLOSE
                case (SSTRING, SSTRING_FENCE)
                    action = A_STRING
                case default
                    ! Ignore whitespace, punctuation, quote, endcomment.
                    continue
                end select

                select case (action)
                case (A_WORD)
                    if (depth /= 0 .or. .not. expression_flag) cycle forward

                    ! This word counts as an expression; return at end of it.
                    do while (from < stop)
                        c = fetch(from_byte)

                        select case (syntax_multibyte(c, multibyte_symbol_p, from))
                        case (SCHARQUOTE, SESCAPE)
                            call inc_both(from, from_byte)

                            if (from == stop) then
                                call lose()
                                return
                            end if
                        case (SWORD, SSYMBOL, SQUOTE)
                            continue
                        case default
                            exit
                        end select

                        call inc_both(from, from_byte)
                    end do

                    done = .true.
                    exit forward
                case (A_COMMENT)
                    if (.not. parse_expression_ignore_comments%value) cycle forward
                    found = forw_comment(from, from_byte, stop, merge(1_int64, 0_int64, comnested), comstyle, 0, &
                                         out_charpos, out_bytepos, dummy, dummy2)
                    from = out_charpos
                    from_byte = out_bytepos

                    if (.not. found) then
                        if (depth == 0) then
                            done = .true.
                            exit forward
                        end if

                        call lose()
                        return
                    end if

                    call inc_both(from, from_byte)
                case (A_OPEN)
                    depth = depth + 1

                    if (depth == 0) then
                        done = .true.
                        exit forward
                    end if
                case (A_CLOSE)
                    depth = depth - 1

                    if (depth == 0) then
                        done = .true.
                        exit forward
                    end if

                    if (depth < min_depth) then
                        call scan_error(status, "Containing expression ends prematurely", last_good, from)
                        return
                    end if
                case (A_STRING)
                    temp_pos = dec_bytepos(from_byte)
                    stringterm = fetch(temp_pos)

                    do
                        if (from >= stop) then
                            call lose()
                            return
                        end if

                        c = fetch(from_byte)
                        c_code = syntax_multibyte(c, multibyte_symbol_p, from)

                        if (code == SSTRING) then
                            hit = c == stringterm .and. c_code == SSTRING
                        else
                            hit = c_code == SSTRING_FENCE
                        end if

                        if (hit) exit
                        if (c_code == SCHARQUOTE .or. c_code == SESCAPE) call inc_both(from, from_byte)
                        call inc_both(from, from_byte)
                    end do

                    call inc_both(from, from_byte)

                    if (depth == 0 .and. expression_flag) then
                        done = .true.
                        exit forward
                    end if
                case default
                    continue
                end select
            end do forward

            if (.not. done) then
                ! Reached end of buffer.  Error if within object, return nil
                ! if between.
                if (depth /= 0) call lose()
                return
            end if

            ! End of object reached.
            count = count - 1
        end do

        do while (count < 0)
            done = .false.

            backward: do while (from > stop)
                call dec_both(from, from_byte)
                c = fetch(from_byte)
                syntax = swf_at(c, from)
                code = syntax_multibyte(c, multibyte_symbol_p, from)
                if (depth == min_depth) last_good = from
                comstyle = 0
                comnested = comment_nested(syntax)
                if (code == SENDCOMMENT) comstyle = comment_style(syntax, 0)

                if (from > stop .and. comend_second(syntax)) then
                    if (prev_char_comend_first(from, from_byte) .and. parse_expression_ignore_comments%value) then
                        ! We must record the comment style encountered so
                        ! that later, we can match only the proper comment
                        ! begin sequence of the same style.
                        call dec_both(from, from_byte)
                        code = SENDCOMMENT
                        c2 = fetch(from_byte)
                        other_syntax = swf_at(c2, from)
                        comstyle = comment_style(other_syntax, syntax)
                        comnested = comnested .or. comment_nested(other_syntax)
                    end if
                end if

                ! Quoting turns anything except a comment-ender into a word
                ! character.  Note that this cannot be true if we decremented
                ! FROM in the if-statement above.
                quoted = .false.
                if (code /= SENDCOMMENT) quoted = char_quoted_at(from)

                if (quoted) then
                    call dec_both(from, from_byte)
                    code = SWORD
                else if (prefix_flag(syntax)) then
                    cycle backward
                end if

                select case (code)
                case (SWORD, SSYMBOL, SESCAPE, SCHARQUOTE)
                    if (depth /= 0 .or. .not. expression_flag) cycle backward

                    ! This word counts as an expression; count object finished
                    ! after passing it.
                    do while (from > stop)
                        temp_pos = dec_bytepos(from_byte)
                        c1 = fetch(temp_pos)
                        ! Don't allow comment-end to be quoted.
                        if (syntax_multibyte(c1, multibyte_symbol_p, from - 1) == SENDCOMMENT) exit
                        quoted = char_quoted_at(from - 1)

                        if (quoted) then
                            call dec_both(from, from_byte)
                            temp_pos = dec_bytepos(temp_pos)
                        end if

                        c1 = fetch(temp_pos)

                        if (.not. quoted) then
                            select case (syntax_multibyte(c1, multibyte_symbol_p, from - 1))
                            case (SWORD, SSYMBOL, SQUOTE)
                                continue
                            case default
                                exit
                            end select
                        end if

                        call dec_both(from, from_byte)
                    end do

                    done = .true.
                    exit backward
                case (SMATH, SCLOSE, SOPEN)
                    action = A_NONE

                    if (code == SMATH) then
                        if (.not. expression_flag) cycle backward

                        if (from > buffers(cur)%begv) then
                            temp_pos = dec_bytepos(from_byte)

                            if (from /= stop) then
                                if (c == fetch(temp_pos)) call dec_both(from, from_byte)
                            end if
                        end if

                        if (mathexit) then
                            mathexit = .false.
                            action = A_OPEN
                        else
                            mathexit = .true.
                            action = A_CLOSE
                        end if
                    else if (code == SCLOSE) then
                        action = A_CLOSE
                    else
                        action = A_OPEN
                    end if

                    if (action == A_CLOSE) then
                        depth = depth + 1

                        if (depth == 0) then
                            done = .true.
                            exit backward
                        end if
                    else
                        depth = depth - 1

                        if (depth == 0) then
                            done = .true.
                            exit backward
                        end if

                        if (depth < min_depth) then
                            call scan_error(status, "Containing expression ends prematurely", last_good, from)
                            return
                        end if
                    end if
                case (SENDCOMMENT)
                    if (.not. parse_expression_ignore_comments%value) cycle backward
                    found = back_comment(from, from_byte, stop, comnested, comstyle, out_charpos, out_bytepos)

                    ! If not found, it really wasn't a comment-end.  For
                    ! single-char Sendcomment, we can't do much about it
                    ! apart from skipping the char.
                    if (found) then
                        from = out_charpos
                        from_byte = out_bytepos
                    end if
                case (SCOMMENT_FENCE, SSTRING_FENCE)
                    do
                        if (from == stop) then
                            call lose()
                            return
                        end if

                        call dec_both(from, from_byte)

                        if (.not. char_quoted_at(from)) then
                            c = fetch(from_byte)
                            if (syntax_multibyte(c, multibyte_symbol_p, from) == code) exit
                        end if
                    end do

                    if (code == SSTRING_FENCE .and. depth == 0 .and. expression_flag) then
                        done = .true.
                        exit backward
                    end if
                case (SSTRING)
                    stringterm = fetch(from_byte)

                    do
                        if (from == stop) then
                            call lose()
                            return
                        end if

                        call dec_both(from, from_byte)

                        if (.not. char_quoted_at(from)) then
                            c = fetch(from_byte)
                            if (c == stringterm .and. syntax_multibyte(c, multibyte_symbol_p, from) == SSTRING) exit
                        end if
                    end do

                    if (depth == 0 .and. expression_flag) then
                        done = .true.
                        exit backward
                    end if
                case default
                    ! Ignore whitespace, punctuation, quote, endcomment.
                    continue
                end select
            end do backward

            if (.not. done) then
                ! Reached start of buffer.  Error if within object, return
                ! nil if between.
                if (depth /= 0) call lose()
                return
            end if

            count = count + 1
        end do

        pos = from

    contains

        recursive subroutine lose()
            pos = 0
            call scan_error(status, "Unbalanced parentheses", last_good, from)
        end subroutine lose
    end function scan_groups_1

    module procedure scan_groups
    pos = scan_groups_1(from, count, depth, .false., status)
    end procedure scan_groups

    module procedure scan_expressions
    pos = scan_groups_1(from, count, 0_int64, .true., status)
    end procedure scan_expressions

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    !> If the character at FROM_BYTE is the second part of a 2-character
    !> comment opener based on PREV_FROM_SYNTAX, update STATE and return
    !> true.  Port of syntax.c:in_2char_comment_start.
    recursive logical function in_2char_comment_start(state, prev_from_syntax, prev_from, from_byte)
        type(lisp_parse_state), intent(inout) :: state
        integer, intent(in) :: prev_from_syntax
        integer(int64), intent(in) :: prev_from, from_byte
        integer :: syntax

        in_2char_comment_start = .false.
        if (.not. comstart_first(prev_from_syntax)) return
        syntax = swf_at(fetch(from_byte), prev_from + 1)
        if (.not. comstart_second(syntax)) return
        ! Record the comment style we have entered so that only the
        ! comment-end sequence of the same style actually terminates the
        ! comment section.
        state%comstyle = comment_style(syntax, prev_from_syntax)

        if (comment_nested(prev_from_syntax) .or. comment_nested(syntax)) then
            state%incomment = 1
        else
            state%incomment = -1
        end if

        state%comstr_start = prev_from
        in_2char_comment_start = .true.
    end function in_2char_comment_start

    !> Parse forward from FROM / FROM_BYTE to END, assuming that FROM has
    !> state STATE, and return a description of the state of the parse at
    !> END.  If STOPBEFORE, stop at the start of an atom.  If COMMENTSTOP
    !> is 1, stop at the start of a comment.  If COMMENTSTOP is -1, stop
    !> at the start or end of a comment, after the beginning of a string,
    !> or after the end of a string.  Port of syntax.c:scan_sexps_forward.
    recursive subroutine scan_expressions_forward(state, from_in, from_byte_in, end, targetdepth, stopbefore, commentstop)
        type(lisp_parse_state), intent(inout) :: state
        integer(int64), intent(in) :: from_in, from_byte_in, end, targetdepth
        logical, intent(in) :: stopbefore
        integer, intent(in) :: commentstop
        !> The levels: the start of the last expression seen at each (last) and
        !> of the one before (prev).
        integer(int64) :: level_last(100), level_prev(100)
        integer :: curlevel, code, prev_from_syntax, prev_prev_from_syntax, step, c, c_code, symchar, k, &
                   last_syntax
        integer(int64) :: from, from_byte, depth, mindepth, prev_from, prev_from_byte, out_charpos, out_bytepos, &
                          incomment
        logical :: start_quoted, boundary_stop, nofence, found

        from = from_in
        from_byte = from_byte_in
        boundary_stop = commentstop == -1
        nofence = .true.
        ! Keep one character before FROM.
        prev_from = from
        prev_from_byte = from_byte
        if (from /= buffers(cur)%begv) call dec_both(prev_from, prev_from_byte)
        depth = state%depth
        start_quoted = state%quoted
        prev_prev_from_syntax = SMAX
        prev_from_syntax = state%prev_syntax
        level_last = -1
        level_prev = -1
        curlevel = 1

        ! >= second enclosing expressions.
        if (allocated(state%levelstarts)) then
            do k = 1, size(state%levelstarts)
                level_last(curlevel) = state%levelstarts(k)
                curlevel = curlevel + 1
                ! error ("Nesting too deep for parser")
                if (curlevel > 100) curlevel = 100
                level_prev(curlevel) = -1
                level_last(curlevel) = -1
            end do
        end if

        level_prev(curlevel) = -1
        level_last(curlevel) = -1
        state%quoted = .false.
        mindepth = depth
        ! Enter the loop at a place appropriate for initial state.
        step = L_LOOP

        if (state%incomment /= 0) then
            step = L_STARTINCOMMENT
        else if (state%instring >= 0) then
            nofence = state%instring /= ST_STRING_STYLE

            if (start_quoted) then
                step = L_STARTQUOTEDINSTRING
            else
                step = L_STARTINSTRING
            end if
        else if (start_quoted) then
            step = L_STARTQUOTED
        else if (from < end) then
            if (in_2char_comment_start(state, prev_from_syntax, prev_from, from_byte)) then
                call inc_from()
                ! The syntax has already been "used up".
                prev_from_syntax = SMAX
                step = L_ATCOMMENT
            end if
        end if

        do
            select case (step)
            case (L_LOOP)
                if (from >= end) then
                    step = L_DONE
                    cycle
                end if

                call inc_from()

                if (from < end) then
                    if (in_2char_comment_start(state, prev_from_syntax, prev_from, from_byte)) then
                        call inc_from()
                        ! The syntax has already been "used up".
                        prev_from_syntax = SMAX
                        step = L_ATCOMMENT
                        cycle
                    end if
                end if

                if (prefix_flag(prev_from_syntax)) cycle
                code = iand(prev_from_syntax, 255)

                select case (code)
                case (SESCAPE, SCHARQUOTE)
                    ! This arg means stop at expression start.
                    if (stopbefore) then
                        step = L_STOP
                    else
                        level_last(curlevel) = prev_from
                        step = L_STARTQUOTED
                    end if
                case (SWORD, SSYMBOL)
                    if (stopbefore) then
                        step = L_STOP
                    else
                        level_last(curlevel) = prev_from
                        step = L_SYMSTARTED
                    end if
                case (SCOMMENT_FENCE)
                    ! Record the comment style we have entered so that only
                    ! the comment-end sequence of the same style actually
                    ! terminates the comment section.
                    state%comstyle = ST_COMMENT_STYLE
                    state%incomment = -1
                    state%comstr_start = prev_from
                    step = L_ATCOMMENT
                case (SCOMMENT)
                    state%comstyle = comment_style(prev_from_syntax, 0)

                    if (comment_nested(prev_from_syntax)) then
                        state%incomment = 1
                    else
                        state%incomment = -1
                    end if

                    state%comstr_start = prev_from
                    step = L_ATCOMMENT
                case (SOPEN)
                    if (stopbefore) then
                        step = L_STOP
                    else
                        depth = depth + 1
                        level_last(curlevel) = prev_from
                        curlevel = curlevel + 1
                        ! error ("Nesting too deep for parser")
                        if (curlevel > 100) curlevel = 100
                        level_prev(curlevel) = -1
                        level_last(curlevel) = -1
                        if (targetdepth == depth) step = L_DONE
                    end if
                case (SCLOSE)
                    depth = depth - 1
                    if (depth < mindepth) mindepth = depth
                    if (curlevel /= 1) curlevel = curlevel - 1
                    level_prev(curlevel) = level_last(curlevel)
                    if (targetdepth == depth) step = L_DONE
                case (SSTRING, SSTRING_FENCE)
                    state%comstr_start = from - 1

                    if (stopbefore) then
                        step = L_STOP
                    else
                        level_last(curlevel) = prev_from

                        if (code == SSTRING) then
                            state%instring = fetch(prev_from_byte)
                        else
                            state%instring = ST_STRING_STYLE
                        end if

                        if (boundary_stop) then
                            step = L_DONE
                        else
                            step = L_STARTINSTRING
                        end if
                    end if
                case default
                    ! Ignore whitespace, punctuation, quote, endcomment;
                    ! Smath is not handled either.
                    continue
                end select
            case (L_STARTQUOTED)
                if (from == end) then
                    step = L_ENDQUOTED
                else
                    call inc_from()
                    step = L_SYMSTARTED
                end if
            case (L_SYMSTARTED)
                step = L_LOOP

                do while (from < end)
                    if (in_2char_comment_start(state, prev_from_syntax, prev_from, from_byte)) then
                        call inc_from()
                        ! The syntax has already been "used up".
                        prev_from_syntax = SMAX
                        step = L_ATCOMMENT
                        exit
                    end if

                    symchar = fetch(from_byte)

                    select case (syn_at(symchar, from))
                    case (SCHARQUOTE, SESCAPE)
                        call inc_from()

                        if (from == end) then
                            step = L_ENDQUOTED
                            exit
                        end if
                    case (SWORD, SSYMBOL, SQUOTE)
                        continue
                    case default
                        exit
                    end select

                    call inc_from()
                end do

                ! symdone:
                if (step == L_LOOP) level_prev(curlevel) = level_last(curlevel)
            case (L_ATCOMMENT)
                if (commentstop /= 0 .or. boundary_stop) then
                    step = L_DONE
                else
                    step = L_STARTINCOMMENT
                end if
            case (L_STARTINCOMMENT)
                ! The (from == BEGV) test was to enter the loop in the middle
                ! so that we find a 2-char comment ender even if we start in
                ! the middle of it.  We don't want to do that if we're just
                ! at the beginning of the comment (think of (*) ... (*)).
                found = forw_comment(from, from_byte, end, state%incomment, state%comstyle, &
                                     merge(0, prev_from_syntax, from == buffers(cur)%begv), out_charpos, &
                                     out_bytepos, incomment, last_syntax)
                state%incomment = incomment
                prev_from_syntax = last_syntax
                from = out_charpos
                from_byte = out_bytepos

                ! Beware!  prev_from and friends (except prev_from_syntax)
                ! are invalid now.  Luckily, the `done' doesn't use them and
                ! the INC_FROM sets them to a sane value without looking at
                ! them.
                if (.not. found) then
                    if (iand(prev_from_syntax, 255) == SESCAPE .or. iand(prev_from_syntax, 255) == SCHARQUOTE) then
                        step = L_ENDQUOTED
                    else
                        step = L_DONE
                    end if

                    cycle
                end if

                call inc_from()
                state%incomment = 0
                ! Reset the comment style.
                state%comstyle = 0
                ! For the comment closer.
                prev_from_syntax = SMAX

                if (boundary_stop) then
                    step = L_DONE
                else
                    step = L_LOOP
                end if
            case (L_STARTINSTRING, L_STARTQUOTEDINSTRING)
                nofence = state%instring /= ST_STRING_STYLE

                if (step == L_STARTQUOTEDINSTRING) then
                    if (from >= end) then
                        step = L_ENDQUOTED
                        cycle
                    end if

                    call inc_from()
                end if

                step = L_LOOP

                do
                    if (from >= end) then
                        step = L_DONE
                        exit
                    end if

                    c = fetch(from_byte)
                    c_code = syn_at(c, from)
                    ! Check C_CODE here so that if the char has a
                    ! syntax-table property which says it is NOT a string
                    ! character, it does not end the string.
                    if (nofence .and. c == state%instring .and. c_code == SSTRING) exit

                    select case (c_code)
                    case (SSTRING_FENCE)
                        if (.not. nofence) exit
                    case (SCHARQUOTE, SESCAPE)
                        call inc_from()

                        ! startquotedinstring:
                        if (from >= end) then
                            step = L_ENDQUOTED
                            exit
                        end if
                    case default
                        continue
                    end select

                    call inc_from()
                end do

                if (step == L_LOOP) then
                    ! string_end:
                    state%instring = -1
                    level_prev(curlevel) = level_last(curlevel)
                    call inc_from()
                    if (boundary_stop) step = L_DONE
                end if
            case (L_STOP)
                ! Here if stopping before start of expression.  We have just
                ! fetched the char that starts it; but return the position
                ! before it.
                from = prev_from
                from_byte = prev_from_byte
                prev_from_syntax = prev_prev_from_syntax
                step = L_DONE
            case (L_ENDQUOTED)
                state%quoted = .true.
                step = L_DONE
            case default
                exit
            end select
        end do

        ! done:
        state%depth = depth
        state%mindepth = mindepth
        state%thislevelstart = level_prev(curlevel)
        state%prevlevelstart = -1
        if (curlevel > 1) state%prevlevelstart = level_last(curlevel - 1)
        state%location = from
        state%location_byte = from_byte
        state%levelstarts = level_last(1:curlevel - 1)

        if (comstartend_first(prev_from_syntax) .or. state%quoted) then
            state%prev_syntax = prev_from_syntax
        else
            state%prev_syntax = SMAX
        end if

    contains

        !> INC_FROM: use this instead of `from++'.
        recursive subroutine inc_from()
            integer :: temp

            prev_from = from
            prev_from_byte = from_byte
            temp = fetch(prev_from_byte)
            prev_prev_from_syntax = prev_from_syntax
            prev_from_syntax = swf_at(temp, prev_from)
            call inc_both(from, from_byte)
        end subroutine inc_from
    end subroutine scan_expressions_forward

    !> Convert a parse state to the internal form used in
    !> scan_expressions_forward; an absent EXTERNAL is nil.  Port of
    !> syntax.c:internalize_parse_state.
    recursive subroutine internalize_parse_state(state, external)
        type(lisp_parse_state), intent(out) :: state
        type(parse_state_type), intent(in), optional :: external

        allocate (state%levelstarts(0))

        if (.not. present(external)) then
            state%depth = 0
            state%instring = -1
            state%incomment = 0
            state%quoted = .false.
            ! Comment style a by default.
            state%comstyle = 0
            ! No comment/string seen.
            state%comstr_start = -1
            state%prev_syntax = SMAX
            return
        end if

        state%depth = external%depth

        ! Check whether we are inside string_fence-style string.
        if (external%instring < 0) then
            state%instring = -1
        else if (external%instring <= MAX_CHAR) then
            state%instring = external%instring
        else
            state%instring = ST_STRING_STYLE
        end if

        state%incomment = external%incomment
        state%quoted = external%quoted

        ! If the eighth element of the list is nil, we are in comment
        ! style a.  If it is non-nil, we are in comment style b.
        if (external%comment_style < 0 .or. external%comment_style > ST_COMMENT_STYLE) then
            state%comstyle = ST_COMMENT_STYLE
        else
            state%comstyle = external%comment_style
        end if

        state%comstr_start = -1
        if (external%comstr_start /= 0) state%comstr_start = external%comstr_start
        if (allocated(external%open_parens)) state%levelstarts = external%open_parens
        state%prev_syntax = SMAX
        if (external%prev_syntax /= -1) state%prev_syntax = external%prev_syntax
    end subroutine internalize_parse_state

    module procedure parse_partial_expression
    type(lisp_parse_state) :: st
    integer(int64) :: f, t, target
    integer :: stop_at_comments

    ! We won't reach this depth.
    target = -huge(0_int64) - 1
    if (present(targetdepth)) target = targetdepth
    stop_at_comments = COMMENTSTOP_NIL
    if (present(commentstop)) stop_at_comments = commentstop
    status = status_type()
    allocate (state%open_parens(0))

    if (to < from) then
        call set_error(status, FM_GENERIC_ERROR, "End position is smaller than start position")
        return
    end if

    f = from
    t = to
    call validate_region(f, t, status)
    if (.not. status%ok()) return
    call internalize_parse_state(st, oldstate)

    if (present(stopbefore)) then
        call scan_expressions_forward(st, f, char_to_byte(f), t, target, stopbefore, stop_at_comments)
    else
        call scan_expressions_forward(st, f, char_to_byte(f), t, target, .false., stop_at_comments)
    end if

    call set_pt_both(st%location, st%location_byte)
    state%depth = st%depth
    state%innermost_start = max(0_int64, st%prevlevelstart)
    state%last_expression_start = max(0_int64, st%thislevelstart)
    state%instring = st%instring
    state%incomment = st%incomment
    state%quoted = st%quoted
    state%mindepth = st%mindepth
    state%comment_style = st%comstyle
    state%comstr_start = 0
    if (st%incomment /= 0 .or. st%instring >= 0) state%comstr_start = st%comstr_start
    state%open_parens = st%levelstarts
    state%prev_syntax = -1
    if (st%prev_syntax /= SMAX) state%prev_syntax = st%prev_syntax
    end procedure parse_partial_expression

    module procedure syntax_ppss
    type(status_type) :: status

    if (associated(syntax_ppss_function)) then
        call syntax_ppss_function(state, pos)
        return
    end if

    if (present(pos)) then
        call parse_partial_expression(buffers(cur)%begv, pos, state, status)
    else
        call parse_partial_expression(buffers(cur)%begv, buffers(cur)%pt, state, status)
    end if
    end procedure syntax_ppss
end submodule syntax_scan_fm
