! mail_utilities_mod.f90 -- utility functions used both by rmail and rnews.
! Ported from GNU Emacs 31.1 lisp/mail/mail-utils.el.
!
! Copyright (C) 1985, 2001-2026 Free Software Foundation, Inc.
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

!> Utility functions for mail and netnews handling: the fine points of
!> header parsing (`get_mail_field', `strip_mail_quoted_names',
!> `mail_dont_reply_to', `parse_mail_comma_list'), the "quoted
!> printable" Q encoding (`quote_mail_printable',
!> `unquote_mail_printable' and their region commands), the dates of
!> mail headers (`mail_rfc822_date') and mbox "From " lines
!> (`mail_mbox_from').
!>
!> String indices are 1-based character indices, as `string_match'
!> gives them.
module mail_utilities_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, PREFIX_NONE
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, narrow_to_region, widen, &
                                     buffer_substring_no_properties, delete_region, character_after, character_before, &
                                     line_beginning_position, interactive_region, character_to_string, is_end_of_buffer
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, forward_character
    use syntax_mod, only: skip_characters_forward, skip_characters_backward
    use search_mod, only: string_match, regular_expression_quote, regular_expression_search_forward, looking_at, match_beginning, &
                          match_end, replace_match, case_fold_search, string_index_to_offset, SEARCH_NOERROR, match_data, &
                          set_match_data
    use basic_subprograms_mod, only: match_string
    use character_mod, only: string_character_and_length, byte8_to_character
    use utility_functions_mod, only: count_characters
    use file_input_output_mod, only: insert_file_contents
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use time_functions_mod, only: format_date, date_format_type, current_time_string, time_type, DATE_DAY, &
                                  DATE_MONTH_ABBREVIATION, DATE_YEAR, TIME_HOUR, TIME_MINUTE, TIME_SECOND, ZONE_OFFSET
    use time_date_mod, only: date_to_time
    use startup_mod, only: user_mail_address_value
    implicit none(type, external)
    private

    public :: is_mail_file_babyl, quote_mail_printable, quote_mail_printable_region, unquote_mail_printable, &
              unquote_mail_printable_region, strip_mail_quoted_names, mail_dont_reply_to, get_mail_field, &
              parse_mail_comma_list, mail_comma_list_regular_expression, mail_rfc822_date, wrap_mail_re_regular_expression, &
              mail_mbox_from, initialize_mail_utilities

    !> Regexp specifying addresses to prune from a reply message.
    !> If this is unallocated, it is set the first time you compose a reply, to
    !> a value which excludes your own email address.
    !>
    !> Matching addresses are excluded from the Cc field in replies, and
    !> also the To field, unless this would leave an empty To field.
    !> Port of mail-utils.el mail-dont-reply-to-names.
    character(len=:), allocatable, public :: mail_dont_reply_to_names

    !> List of localized "Re" abbreviations in various languages.
    !> Each component can be a regular expression or a simple string.  Matching
    !> is done case-insensitively.  Used to initialize
    !> `message_subject_re_regular_expression'.  Port of mail-utils.el mail-re-regexps
    !> (set by `initialize_mail_utilities' unless already set).
    character(len=:), allocatable, public :: mail_re_regular_expressions(:)

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    ! A restriction saved by `restriction_begin': whether the buffer was
    ! narrowed, and markers at its bounds.
    type :: restriction_type
        logical :: narrowed = .false.
        type(handle_type) :: beginning, end
    end type restriction_type

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! The characters of STRING from index FROM (inclusive) to TO
    ! (exclusive), 1-based (TO absent: to the end).  Not a port.
    function chars(string, from, to) result(text)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: from
        integer(int64), intent(in), optional :: to
        character(len=:), allocatable :: text
        integer(int64) :: a, b

        a = string_index_to_offset(string, from)
        b = len(string, kind=int64)
        if (present(to)) b = string_index_to_offset(string, to)
        text = string(a + 1:b)
    end function chars

    ! The character at index K of STRING.  Not a port.
    integer(int32) function character_at(string, k) result(c)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: k
        integer(int32) :: clen

        call string_character_and_length(string, string_index_to_offset(string, k) + 1, c, clen)
    end function character_at

    ! A marker at point (save-excursion).  Not a port.
    function marker_here() result(m)
        type(handle_type) :: m
        type(status_type) :: st

        call copy_marker_at(point(), m, .false., st)
    end function marker_here

    ! The position of marker M, which is destroyed.  Not a port.
    integer(int64) function marker_end(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, position, st)
        call destroy_marker(m, st)
    end function marker_end

    ! Remember the restriction (save-restriction).  Not a port.
    subroutine restriction_begin(r)
        type(restriction_type), intent(out) :: r
        type(status_type) :: st

        r%narrowed = buffers(current)%beginning_of_accessible_portion /= 1 .or. buffers(current)%end_of_accessible_portion &
                     /= buffers(current)%text%z
        if (.not. r%narrowed) return
        call copy_marker_at(buffers(current)%beginning_of_accessible_portion, r%beginning, .false., st)
        call copy_marker_at(buffers(current)%end_of_accessible_portion, r%end, .true., st)
    end subroutine restriction_begin

    ! Restore the restriction R remembered.  Not a port.
    subroutine restriction_end(r)
        type(restriction_type), intent(in) :: r
        type(status_type) :: st

        call widen()
        if (.not. r%narrowed) return
        call narrow_to_region(marker_end(r%beginning), marker_end(r%end), st)
    end subroutine restriction_end

    ! Make a temporary buffer current (with-temp-buffer); OLD is the
    ! buffer to go back to.  Not a port.
    subroutine temporary_begin(old, temporary)
        type(handle_type), intent(out) :: old, temporary
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
    end subroutine temporary_begin

    ! Close the temporary buffer TEMPORARY and make OLD current again.
    ! Not a port.
    subroutine temporary_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine temporary_end

    ! ------------------------------------------------------------------
    ! Files
    ! ------------------------------------------------------------------

    !> Return true if FILE is a Babyl file.  Port of
    !> mail-utils.el:mail-file-babyl-p.
    logical function is_mail_file_babyl(file, status) result(babyl)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(handle_type) :: old, temporary
        integer(int64) :: inserted

        babyl = .false.
        call temporary_begin(old, temporary)
        call insert_file_contents(file, .false., inserted, status, beginning_byte=0_int64, end_byte=100_int64)
        if (status%ok()) babyl = looking_at("BABYL OPTIONS:", status)
        call temporary_end(old, temporary)
    end function is_mail_file_babyl

    ! ------------------------------------------------------------------
    ! Quoted printable
    ! ------------------------------------------------------------------

    !> Convert a string to the "quoted printable" Q encoding if necessary.
    !> If the string contains only ASCII characters and no troublesome ones,
    !> we return it unconverted.
    !>
    !> If the optional argument WRAPPER is true,
    !> we add the wrapper characters =?ISO-8859-1?Q?....?=.  Port of
    !> mail-utils.el:mail-quote-printable.
    function quote_mail_printable(string, wrapper) result(result)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: wrapper
        character(len=:), allocatable :: result
        integer(int64) :: i, m
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        character(len=16) :: hexadecimal

        i = 1
        result = ""
        ! save-match-data
        saved = match_data()

        do
            m = string_match("[?=""]", string, st, start=i)
            if (m == 0) m = string_match("[^"//achar(0)//"-"//achar(127)//"]", string, st, start=i)
            if (m == 0) exit
            ! (upcase (format "=%02x" C))
            write (hexadecimal, '("=", z0.2)') character_at(string, m)
            result = result//chars(string, i, m)//trim(hexadecimal)
            i = match_end(0, st)
        end do

        call set_match_data(saved)

        if (option(wrapper)) then
            result = "=?ISO-8859-1?Q?"//result//chars(string, i)//"?="
        else
            result = result//chars(string, i)
        end if
    end function quote_mail_printable

    !> Convert the region to the "quoted printable" Q encoding.
    !> If the optional argument WRAPPER is true,
    !> we add the wrapper characters =?ISO-8859-1?Q?....?=.  Port of
    !> mail-utils.el:mail-quote-printable-region.
    recursive subroutine quote_mail_printable_region(beginning, end, status, wrapper)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: wrapper
        integer(int64), allocatable :: saved(:)
        integer(int64) :: found
        integer(int32) :: c
        logical :: ok
        type(restriction_type) :: r
        type(handle_type) :: here
        character(len=16) :: hexadecimal
        character(len=:), allocatable :: low, high

        saved = match_data()
        here = marker_here()
        call restriction_begin(r)
        call goto_character(beginning)
        call narrow_to_region(beginning, end, status)
        ! The raw bytes 128..255 (a unibyte \200-\377 range upstream).
        if (status%ok()) call character_to_string(byte8_to_character(128), low, status)
        if (status%ok()) call character_to_string(byte8_to_character(255), high, status)

        if (status%ok()) then
            do
                call regular_expression_search_forward("[?=""]\|["//low//"-"//high//"]", found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. found == 0) exit
                call character_before(c, ok)
                write (hexadecimal, '("=", z0.2)') c
                call replace_match(trim(hexadecimal), status, fixedcase=.true., literal=.true.)
                if (.not. status%ok()) exit
            end do
        end if

        if (status%ok() .and. option(wrapper)) then
            call goto_character(beginning)
            call insert("=?ISO-8859-1?Q?", status)
            call goto_character(end)
            if (status%ok()) call insert("?=", status)
        end if

        call restriction_end(r)
        call goto_character(marker_end(here))
        call set_match_data(saved)
    end subroutine quote_mail_printable_region

    ! The value of the hexadecimal digit C (upcased).  Port of
    ! mail-utils.el:mail-unquote-printable-hexdigit.
    integer function unquote_mail_printable_hexdigit(c) result(value)
        integer(int32), intent(in) :: c
        integer(int32) :: u

        u = c
        if (u >= iachar("a") .and. u <= iachar("z")) u = u - 32

        if (u >= iachar("A")) then
            value = u - iachar("A") + 10
        else
            value = u - iachar("0")
        end if
    end function unquote_mail_printable_hexdigit

    !> Undo the "quoted printable" encoding.
    !> If the optional argument WRAPPER is true,
    !> we expect to find and remove the wrapper characters =?ISO-8859-1?Q?....?=.
    !> Port of mail-utils.el:mail-unquote-printable.
    function unquote_mail_printable(string_in, wrapper) result(result)
        character(len=*), intent(in) :: string_in
        logical, intent(in), optional :: wrapper
        character(len=:), allocatable :: result
        character(len=:), allocatable :: string, text
        integer(int64) :: i, m, b
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st

        saved = match_data()
        string = string_in

        if (option(wrapper)) then
            if (string_match("\`=\?ISO-8859-1\?Q\?\([^?]*\)\?", string, st) > 0) string = match_string(1, string)
        end if

        i = 1
        result = ""

        do
            m = string_match("=\(..\|"//LF//"\)", string, st, start=i)
            if (m == 0) exit
            result = result//chars(string, i, m)
            b = match_beginning(1, st)

            if (character_at(string, b) /= 10) then
                call character_to_string(16*unquote_mail_printable_hexdigit(character_at(string, b)) &
                                         + unquote_mail_printable_hexdigit(character_at(string, b + 1)), text, st)
                result = result//text
            end if

            i = match_end(0, st)
        end do

        result = result//chars(string, i)
        call set_match_data(saved)
    end function unquote_mail_printable

    !> Undo the "quoted printable" encoding in buffer from BEGINNING to END.
    !> If the optional argument WRAPPER is true,
    !> we expect to find and remove the wrapper characters =?ISO-8859-1?Q?....?=.
    !> On encountering malformed quoted-printable text, exits with an error,
    !> unless NOERROR is true, in which case it continues, and returns
    !> .false. (in SUCCEEDED) when finished.  Returns .true. on successful
    !> completion.
    !> If UNIBYTE is true, insert converted characters as unibyte.
    !> That is useful if you are going to character code decoding afterward,
    !> as Rmail does.  Port of mail-utils.el:mail-unquote-printable-region.
    recursive subroutine unquote_mail_printable_region(beginning, end, status, wrapper, noerror, unibyte, succeeded)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: wrapper, noerror, unibyte
        logical, intent(out), optional :: succeeded
        integer(int64), allocatable :: saved(:)
        integer(int64) :: found, b1, b2, e1, m1
        integer(int32) :: c1, c2, code
        logical :: failed, ok, matched
        type(status_type) :: st
        character(len=:), allocatable :: text
        type(restriction_type) :: r
        type(handle_type) :: here

        failed = .false.
        saved = match_data()
        here = marker_here()
        call restriction_begin(r)
        call narrow_to_region(beginning, end, status)

        body: block
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())

            if (option(wrapper)) then
                matched = looking_at("\`=\?ISO-8859-1\?Q\?\([^?]*\)\?", status)
                if (.not. status%ok()) exit body

                if (matched) then
                    e1 = match_end(1, st)
                    b1 = match_beginning(1, st)
                    call delete_region(e1, point_maximum(), status)
                    if (.not. status%ok()) exit body
                    call delete_region(point(), b1, status)
                    if (.not. status%ok()) exit body
                end if
            end if

            do
                call regular_expression_search_forward("=\(\([0-9A-F][0-9A-F]\)\|[="//LF//"]\|..\)", found, status, &
                                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. found == 0) exit
                call goto_character(match_end(0, st))
                m1 = match_beginning(1, st)
                call character_after(c1, ok, m1)

                if (c1 == 10) then
                    call replace_match("", status)
                else if (c1 == iachar("=")) then
                    call replace_match("=", status)
                else if (match_beginning(2, st) > 0) then
                    b2 = match_beginning(2, st)
                    call character_after(c1, ok, b2)
                    call character_after(c2, ok, b2 + 1)
                    code = 16*unquote_mail_printable_hexdigit(c1) + unquote_mail_printable_hexdigit(c2)

                    if (option(unibyte)) then
                        call replace_match("", status)
                        ! insert-byte will insert this as a
                        ! corresponding eight-bit character.
                        if (code >= 128) code = byte8_to_character(code)
                        if (status%ok()) call character_to_string(code, text, status)
                        if (status%ok()) call insert(text, status)
                    else
                        call character_to_string(code, text, status)
                        if (status%ok()) call replace_match(text, status, fixedcase=.true., literal=.true.)
                    end if
                else if (option(noerror)) then
                    failed = .true.
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Malformed MIME quoted-printable message")
                end if

                if (.not. status%ok()) exit
            end do
        end block body

        call restriction_end(r)
        call goto_character(marker_end(here))
        call set_match_data(saved)
        if (present(succeeded)) succeeded = .not. failed
    end subroutine unquote_mail_printable_region

    ! ------------------------------------------------------------------
    ! Addresses
    ! ------------------------------------------------------------------

    !> Delete comments and quoted strings in an address list ADDRESS.
    !> Also delete leading/trailing whitespace and replace FOO <BAR> with just BAR.
    !> Return a modified address list.  Port of
    !> mail-utils.el:mail-strip-quoted-names (`mail_use_rfc822' and so the
    !> RFC 822 parser are Deferred).
    function strip_mail_quoted_names(address_in) result(address)
        character(len=*), intent(in) :: address_in
        character(len=:), allocatable :: address
        character(len=:), allocatable :: replaced
        integer(int64) :: position, me, e
        type(status_type) :: st

        address = address_in

        ! Strip comments.
        do
            position = string_match("[ "//TAB//"]*(\([^()\]\|\\.\|\\"//LF//"\)*)", address, st)
            if (position == 0) exit
            call replace_match("", st, string=address, subexp=0, result=replaced)
            address = replaced
        end do

        ! strip surrounding whitespace
        position = string_match("\`[ "//TAB//LF//"]*", address, st)
        me = match_end(0, st)
        e = string_match("[ "//TAB//LF//"]*\'", address, st, start=me)
        address = chars(address, me, e)
        ! strip `quoted' names (This is supposed to hack `"Foo Bar" <bar@host>')
        position = 1

        do
            position = string_match("\([ "//TAB//"]?\)\([ "//TAB//"]*""\([^""\]\|\\.\|\\"//LF//"\)*""[ "//TAB//LF//"]*\)", &
                                    address, st, start=position)
            if (position == 0) exit
            me = match_end(0, st)

            ! If the next thing is "@", we have "foo bar"@host.  Leave it.
            if (count_characters(address) >= me .and. is_at_sign(me)) then
                position = me
            else
                ! Otherwise discard the "..." part.
                call replace_match("", st, string=address, subexp=2, result=replaced)
                address = replaced
            end if
        end do

        ! If this address contains <...>, replace it with just
        ! the part between the <...>.
        do
            position = string_match("\(,\s-*\|\`\)\([^,]*<\([^>,:]*\)>[^,]*\)\(\s-*,\|\'\)", address, st)
            if (position == 0) exit
            call replace_match(match_string(3, address), st, literal=.true., string=address, subexp=2, result=replaced)
            address = replaced
        end do

    contains

        logical function is_at_sign(k)
            integer(int64), intent(in) :: k

            is_at_sign = character_at(address, k) == iachar("@")
        end function is_at_sign
    end function strip_mail_quoted_names

    !> Prune addresses from DESTINATIONS, a list of recipient addresses.
    !> Remove all addresses matching `mail_dont_reply_to_names' from the
    !> comma-separated list, and return the pruned list.  Port of
    !> mail-utils.el:mail-dont-reply-to.
    function mail_dont_reply_to(destinations_in) result(destinations)
        character(len=*), intent(in) :: destinations_in
        character(len=:), allocatable :: destinations
        character(len=:), allocatable :: address, naked, mail_address, tail
        integer(int64) :: start_position, current_position, next_position, position
        logical :: saved_fold, quote
        type(status_type) :: st

        destinations = destinations_in

        if (.not. allocated(mail_dont_reply_to_names)) then
            mail_address = user_mail_address_value()
            if (len(mail_address) > 0) mail_dont_reply_to_names = "\`"//regular_expression_quote(mail_address)//"\'"
        end if

        ! Split up DESTINATIONS and match each element separately.
        start_position = 1
        current_position = 1
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.

        do while (start_position > 0)
            current_position = string_match("[,""]", destinations, st, start=current_position)
            quote = .false.
            if (current_position > 0) quote = match_string(0, destinations) == '"'

            if (quote) then
                ! Search for matching quote.
                tail = chars(destinations, current_position + 1)
                next_position = index(tail, '"', kind=int64)

                if (next_position > 0) then
                    next_position = current_position + count_characters(tail(1:next_position - 1)) + 1
                    current_position = next_position + 1
                else
                    ! If the open-quote has no close-quote,
                    ! delete the open-quote to get something well-defined.
                    ! This case is not valid, but it can happen if things
                    ! are weird elsewhere.
                    destinations = chars(destinations, 1_int64, current_position)//chars(destinations, current_position + 1)
                    current_position = start_position
                end if
            else
                if (current_position > 0) then
                    address = chars(destinations, start_position, current_position)
                else
                    address = chars(destinations, start_position)
                end if

                naked = strip_mail_quoted_names(address)

                if (is_pruned(naked)) then
                    if (current_position > 0) then
                        destinations = chars(destinations, 1_int64, start_position)//chars(destinations, current_position + 1)
                    else
                        destinations = chars(destinations, 1_int64, start_position)
                    end if

                    current_position = start_position
                else
                    if (current_position > 0) current_position = current_position + 1
                    start_position = current_position
                end if
            end if
        end do

        case_fold_search%value = saved_fold
        ! get rid of any trailing commas
        position = string_match("[ ,"//TAB//LF//"]*\'", destinations, st)
        if (position > 0) destinations = chars(destinations, 1_int64, position)
        ! remove leading spaces. they bother me.
        if (string_match("\(\s \|,\)*", destinations, st) > 0) destinations = chars(destinations, match_end(0, st))

    contains

        logical function is_pruned(text)
            character(len=*), intent(in) :: text

            is_pruned = .false.
            if (allocated(mail_dont_reply_to_names)) is_pruned = string_match(mail_dont_reply_to_names, text, st) > 0
        end function is_pruned
    end function mail_dont_reply_to

    ! ------------------------------------------------------------------
    ! Header fields
    ! ------------------------------------------------------------------

    !> Return the value of the header field whose type is FIELD_NAME, in
    !> VALUE (unallocated when there is none).
    !> If LAST is true, use the last field of type FIELD_NAME.
    !> If ALL is true, concatenate all such fields with commas between.
    !> If DELETE is true, delete all header lines that are
    !> included in the result.
    !> The buffer should be narrowed to just the headers, else false
    !> matches may be returned from the message body.  Port of
    !> mail-utils.el:mail-fetch-field (its LIST argument is Deferred).
    recursive subroutine get_mail_field(field_name, value, status, last, all, delete)
        character(len=*), intent(in) :: field_name
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: last, all, delete
        character(len=:), allocatable :: name, text
        integer(int64) :: original_point, found
        type(handle_type) :: here
        logical :: saved_fold

        here = marker_here()
        call goto_character(point_minimum())
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        name = "^"//regular_expression_quote(field_name)//"[ "//TAB//"]*:[ "//TAB//"]*"

        body: block
            if (option(all)) then
                value = ""

                do
                    call regular_expression_search_forward(name, found, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok()) exit body
                    if (found == 0) exit
                    call field_end(original_point, status)
                    if (.not. status%ok()) exit body
                    call buffer_substring_no_properties(original_point, point(), text, status)
                    if (.not. status%ok()) exit body

                    if (value == "") then
                        value = text
                    else
                        value = value//", "//text
                    end if

                    if (option(delete)) then
                        call delete_region(line_beginning_position(), point(), status)
                        if (.not. status%ok()) exit body
                    end if
                end do

                if (value == "") deallocate (value)
            else
                call regular_expression_search_forward(name, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. found == 0) exit body

                if (option(last)) then
                    do
                        call regular_expression_search_forward(name, found, status, noerror=SEARCH_NOERROR)
                        if (.not. status%ok()) exit body
                        if (found == 0) exit
                    end do
                end if

                call field_end(original_point, status)
                if (.not. status%ok()) exit body
                call buffer_substring_no_properties(original_point, point(), text, status)
                if (.not. status%ok()) exit body
                value = text

                if (option(delete)) then
                    call delete_region(line_beginning_position(), point() + 1, status)
                    if (.not. status%ok()) exit body
                end if
            end if
        end block body

        case_fold_search%value = saved_fold
        call goto_character(marker_end(here))

    contains

        ! Move over the field's continuation lines, then back over the
        ! newline and the trailing spaces or tabs; ORIGINAL_POINT is where the
        ! value starts.
        recursive subroutine field_end(original_point, status)
            integer(int64), intent(out) :: original_point
            type(status_type), intent(out) :: status
            integer(int64) :: moved, shortage
            logical :: continued

            original_point = point()

            do
                shortage = forward_line(1_int64)
                continued = looking_at("[ "//TAB//"]", status)
                if (.not. status%ok()) return
                if (.not. continued) exit
            end do

            ! Back up over newline, then trailing spaces or tabs
            call forward_character(status, -1_int64)
            if (.not. status%ok()) return
            moved = skip_characters_backward(" "//TAB, status, original_point)
        end subroutine field_end
    end subroutine get_mail_field

    !> Parse a list of tokens separated by commas, from point to the end
    !> of the visible part of the buffer, into ITEMS (the last token
    !> first, as upstream conses them; each blank-padded to the longest).
    !> Whitespace before or after tokens is ignored,
    !> but whitespace within tokens is kept.  Port of
    !> mail-utils.el:mail-parse-comma-list.
    subroutine parse_mail_comma_list(items, status)
        character(len=:), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, moved, k, n, longest
        integer(int64), allocatable :: starts(:), ends(:)
        character(len=:), allocatable :: text

        allocate (starts(0), ends(0))
        moved = skip_characters_forward(" "//TAB//LF, status)
        if (.not. status%ok()) return

        do while (.not. is_end_of_buffer())
            beginning = point()
            moved = skip_characters_forward("^,", status)
            moved = skip_characters_backward(" "//TAB//LF, status)
            starts = [beginning, starts]
            ends = [point(), ends]
            moved = skip_characters_forward("^,", status)
            moved = skip_characters_forward(", "//TAB//LF, status)
            if (.not. status%ok()) return
        end do

        n = size(starts)
        longest = 0

        do k = 1, n
            call buffer_substring_no_properties(starts(k), ends(k), text, status)
            if (.not. status%ok()) return
            longest = max(longest, len(text, kind=int64))
        end do

        allocate (character(len=longest) :: items(n))

        do k = 1, n
            call buffer_substring_no_properties(starts(k), ends(k), text, status)
            if (.not. status%ok()) return
            items(k) = text
        end do
    end subroutine parse_mail_comma_list

    !> LABELS, a comma-separated list, as a regexp matching any of them.
    !> Port of mail-utils.el:mail-comma-list-regexp.
    function mail_comma_list_regular_expression(labels_in) result(labels)
        character(len=*), intent(in) :: labels_in
        character(len=:), allocatable :: labels
        integer(int64) :: position, e
        type(status_type) :: st

        labels = labels_in
        position = string_match("[^ "//TAB//"]", labels, st)
        if (position == 0) position = 1
        ! Remove leading and trailing whitespace.
        e = string_match("[ "//TAB//"]*$", labels, st, start=position)
        labels = chars(labels, position, e)

        ! Change each comma to \|, and flush surrounding whitespace.
        do
            position = string_match("[ "//TAB//"]*,[ "//TAB//"]*", labels, st)
            if (position == 0) exit
            labels = chars(labels, 1_int64, position)//"\|"//chars(labels, match_end(0, st))
        end do
    end function mail_comma_list_regular_expression

    !> The current date as an RFC 822 date ("7 Feb 2021 22:46:37 -0500"),
    !> or an error condition in STATUS.  Port of mail-utils.el:mail-rfc822-date.
    function mail_rfc822_date(status) result(date)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: date

        date = format_date(date_format_type('(i0, 1x, a, 1x, i4.4, 1x, i2.2, ":", i2.2, ":", i2.2, 1x, sp, i5.4)', &
                                            [DATE_DAY, DATE_MONTH_ABBREVIATION, DATE_YEAR, TIME_HOUR, TIME_MINUTE, &
                                             TIME_SECOND, ZONE_OFFSET]), status=status)
    end function mail_rfc822_date

    !> Return an mbox "From " line for the current message.  The buffer
    !> should be narrowed to just the headers.  Port of
    !> mail-utils.el:mail-mbox-from.
    function mail_mbox_from(status) result(line)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: line
        character(len=*), parameter :: FIELDS(4) = [character(len=11) :: "from", "really-from", "sender", "return-path"]
        character(len=:), allocatable :: from, date, time_string
        type(status_type) :: st
        type(time_type) :: time
        integer(int64) :: end
        integer :: k

        line = ""

        do k = 1, size(FIELDS)
            call get_mail_field(trim(FIELDS(k)), from, status)
            if (.not. status%ok()) return
            if (allocated(from)) exit
        end do

        if (.not. allocated(from)) from = "unknown"
        from = strip_mail_quoted_names(from)
        call get_mail_field("date", date, status)
        if (.not. status%ok()) return
        ! A From: header can contain multiple addresses, a "From " line must
        ! contain only one.  (Bug#7760) See, e.g., RFC 5322, 3.6.2.
        ! Originator Fields.
        end = string_match("[ "//TAB//"]*[,"//LF//"]", from, st)
        if (end > 0) from = chars(from, 1_int64, end)
        time_string = ""

        if (allocated(date)) then
            time = date_to_time(date, st)
            if (st%ok()) time_string = current_time_string(time, status=st)
            if (.not. st%ok()) time_string = ""
        end if

        if (len(time_string) == 0) time_string = current_time_string()
        line = "From "//from//" "//time_string//LF
    end function mail_mbox_from

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine execute_quote_mail_printable_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (status%ok()) call quote_mail_printable_region(b, e, status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_quote_mail_printable_region

    recursive subroutine execute_unquote_mail_printable_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (status%ok()) call unquote_mail_printable_region(b, e, status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_unquote_mail_printable_region

    ! The value of an optional logical, false if absent.  Not a port.
    pure logical function option(flag)
        logical, intent(in), optional :: flag

        option = .false.
        if (present(flag)) option = flag
    end function option

    !> ALTERNATIVES (the alternatives of "Re" abbreviations) as a regexp matching
    !> any number of them at the start of a subject, each followed by a
    !> colon: "Re: ", "Re[2]: ", "AW: Re: ".  Port of
    !> mail-utils.el:mail--wrap-re-regexp.
    function wrap_mail_re_regular_expression(alternatives) result(regular_expression)
        use multilingual_environment_configuration_mod, only: password_colon_equivalents, &
                                                              initialize_multilingual_environment_configuration
        use character_mod, only: character_string, MAXIMUM_MULTIBYTE_LENGTH
        character(len=*), intent(in) :: alternatives
        character(len=:), allocatable :: regular_expression
        character(len=MAXIMUM_MULTIBYTE_LENGTH) :: buffer
        character(len=:), allocatable :: colons
        type(status_type) :: st
        integer(int32) :: nbytes
        integer :: i

        if (.not. allocated(password_colon_equivalents)) call initialize_multilingual_environment_configuration()
        colons = ""

        do i = 1, size(password_colon_equivalents)
            call character_string(password_colon_equivalents(i), buffer, nbytes, st)
            colons = colons//buffer(1:nbytes)
        end do

        ! Re(1) or Re[1] or Re^1, then SPC/NBSP followed by various colon
        ! variants and TAB/SPC; repeated, as in "Re[1]: Re[2]:".
        regular_expression = "\`[ "//TAB//"]*\("//alternatives//"\(([0-9]+)\|\[[0-9]+\]\|\^[0-9]+\)? ?"//achar(194)//achar( &
                             160)//"*["//colons// &
                             "][ "//TAB//"]*\)*[ "//TAB//"]*"
    end function wrap_mail_re_regular_expression

    !> Register the commands of mail-utils.el and set `mail_re_regular_expressions'.
    !> Not a port.
    subroutine initialize_mail_utilities()
        if (.not. allocated(mail_re_regular_expressions)) then
            mail_re_regular_expressions = [character(len=16) :: "RE", "RÉ\.?", "FWD?", "رد", "回复", "回覆", &
                                           "SV", "Antw\.?", "VS", "REF", "AW", &
                                           "ΑΠ", "ΣΧΕΤ", "השב", "Vá", "R", "RIF", "BLS", "RES", "Odp", "YNT", "ATB"]
        end if

        commands%quote_mail_printable_region%execute => execute_quote_mail_printable_region
        commands%unquote_mail_printable_region%execute => execute_unquote_mail_printable_region
    end subroutine initialize_mail_utilities
end module mail_utilities_mod
