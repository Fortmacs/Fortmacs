! rfc2047_mod.f90 -- functions for encoding and decoding rfc2047 messages.
! Ported from GNU Emacs 31.1 lisp/mail/rfc2047.el and lisp/mail/mail-prsvr.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> RFC 2047 is "MIME (Multipurpose Internet Mail Extensions) Part Three:
!> Message Header Extensions for Non-ASCII Text": header fields with
!> encoded words such as "=?utf-8?Q?F=C3=B3?=".  Decoding
!> (`decode_rfc2047_string', `decode_rfc2047_region'), encoding
!> (`encode_rfc2047_string', `encode_rfc2047_region',
!> `encode_rfc2047_message_header') and folding (`fold_rfc2047_field',
!> `unfold_rfc2047_field').  mail-parse.el's names for these functions
!> (mail-decode-encoded-word-string, mail-encode-encoded-word-buffer,
!> mail-header-fold-field, ...) are aliases, collapsed to these names;
!> mail-prsvr.el's variables, which the mail parsing functions read, are
!> here too.  What is not ported yet is listed in
!> docs/status/rfc2047_mod.md.
module rfc2047_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name, barf_if_buffer_read_only
    use editing_functions_mod, only: point, point_min, point_max, goto_char, narrow_to_region, widen, char_after, &
                                     char_before, is_eob, is_bol, delete_region, buffer_string, &
                                     buffer_substring_no_properties, line_beginning_position, line_end_position, &
                                     interactive_region, save_excursion_begin, save_excursion_end, &
                                     save_restriction_begin, save_restriction_end, excursion_type, restriction_type, &
                                     subst_char_in_region
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_char, forward_line, beginning_of_line, delete_char
    use syntax_mod, only: skip_chars_forward, skip_chars_backward, char_syntax, make_syntax_table, modify_syntax_entry, &
                          set_syntax_table, syntax_table, standard_syntax_table
    use expression_motion_mod, only: forward_expression, forward_group
    use search_mod, only: re_search_forward, re_search_backward, search_forward, looking_at, string_match, &
                          match_beginning, match_end, replace_match, case_fold_search, string_chars, &
                          string_index_to_offset, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use character_mod, only: MAX_CHAR
    use coding_mod, only: is_coding_system, encode_coding_string, decode_coding_string, encode_coding_region, &
                          decode_coding_region, detect_coding_string, coding_system_list_entry_type
    use utility_functions_mod, only: base64_encode_string, base64_decode_string
    use quoted_printable_mod, only: decode_quoted_printable_string, encode_quoted_printable_region
    use mime_utilities_mod, only: mime_charset_to_coding_system, find_mime_charset_region, mime_coding_system_priorities
    use display_engine_mod, only: message
    implicit none(type, external)
    private

    public :: encode_rfc2047_message_header, check_rfc2047_encodable, encode_rfc2047_region, encode_rfc2047_string, &
              narrow_to_rfc2047_field, rfc2047_field_value, fold_rfc2047_field, fold_rfc2047_region, &
              unfold_rfc2047_field, unfold_rfc2047_region, decode_rfc2047_region, decode_rfc2047_address_region, &
              decode_rfc2047_string, decode_rfc2047_address_string, rfc2047_charset_to_coding_system, &
              pad_rfc2047_base64, strip_rfc2047_backslashes_in_quoted_strings, init_rfc2047

    !> `method' of an `rfc2047_header_encoding_type': no encoding is done.
    integer, parameter, public :: RFC2047_HEADER_ENCODING_NONE = 0
    !> `method': the header will be encoded according to RFC 2047.
    integer, parameter, public :: RFC2047_HEADER_ENCODING_MIME = 1
    !> `method': like RFC2047_HEADER_ENCODING_MIME, but takes account of
    !> the rules for address fields (where quoted strings and comments must
    !> be treated separately).
    integer, parameter, public :: RFC2047_HEADER_ENCODING_ADDRESS_MIME = 2
    !> `method': the field will be encoded as `coding_system'.
    integer, parameter, public :: RFC2047_HEADER_ENCODING_CODING_SYSTEM = 3
    !> `method': the field will be encoded as the rest of the article.
    integer, parameter, public :: RFC2047_HEADER_ENCODING_DEFAULT = 4

    !> One entry of `rfc2047_header_encodings': the header fields whose
    !> names `header' (a regular expression) matches, or all fields when
    !> `all_headers' is true, are encoded as `method' says, one of the
    !> RFC2047_HEADER_ENCODING_ constants (with `coding_system' for
    !> RFC2047_HEADER_ENCODING_CODING_SYSTEM).
    type, public :: rfc2047_header_encoding_type
        character(len=160) :: header = ""
        logical :: all_headers = .false.
        integer :: method = RFC2047_HEADER_ENCODING_NONE
        character(len=40) :: coding_system = ""
    end type rfc2047_header_encoding_type

    !> Header/encoding method records.  The array is traversed
    !> sequentially.  Each entry gives a header regular expression or
    !> stands for all headers (`all_headers').
    !>
    !> The methods can be:
    !>
    !> 1) RFC2047_HEADER_ENCODING_NONE, in which case no encoding is done;
    !> 2) RFC2047_HEADER_ENCODING_MIME, in which case the header will be
    !>    encoded according to RFC2047;
    !> 3) RFC2047_HEADER_ENCODING_ADDRESS_MIME, like
    !>    RFC2047_HEADER_ENCODING_MIME, but takes account of the rules for
    !>    address fields (where quoted strings and comments must be treated
    !>    separately);
    !> 4) RFC2047_HEADER_ENCODING_CODING_SYSTEM, in which case it will be
    !>    encoded as the charset `coding_system';
    !> 5) RFC2047_HEADER_ENCODING_DEFAULT, in which case the field will be
    !>    encoded as the rest of the article.
    !>
    !> (Unallocated: the default, set on first use.)  Port of rfc2047.el
    !> rfc2047-header-encoding-alist.
    type(rfc2047_header_encoding_type), allocatable, public :: rfc2047_header_encodings(:)

    !> `encoding' of an `rfc2047_charset_encoding_type': binary (no)
    !> encoding.
    integer, parameter, public :: RFC2047_CHARSET_ENCODING_NONE = 0
    !> `encoding': quoted-printable, the Q encoding.
    integer, parameter, public :: RFC2047_CHARSET_ENCODING_Q = 1
    !> `encoding': base64, the B encoding.
    integer, parameter, public :: RFC2047_CHARSET_ENCODING_B = 2

    !> One entry of `rfc2047_charset_encodings': a MIME charset and its
    !> RFC2047_CHARSET_ENCODING_ constant.
    type, public :: rfc2047_charset_encoding_type
        character(len=16) :: charset = ""
        integer :: encoding = RFC2047_CHARSET_ENCODING_NONE
    end type rfc2047_charset_encoding_type

    !> MIME charsets and their RFC2047 encodings.  Valid encodings are
    !> RFC2047_CHARSET_ENCODING_NONE, RFC2047_CHARSET_ENCODING_Q and
    !> RFC2047_CHARSET_ENCODING_B.  These indicate binary (no) encoding,
    !> quoted-printable and base64 respectively.  Port of rfc2047.el
    !> rfc2047-charset-encoding-alist.
    type(rfc2047_charset_encoding_type), public :: rfc2047_charset_encodings(25) = [ &
                                                   rfc2047_charset_encoding_type("us-ascii", RFC2047_CHARSET_ENCODING_NONE), &
                                                   rfc2047_charset_encoding_type("iso-8859-1", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-8859-2", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-8859-3", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-8859-4", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-8859-5", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("koi8-r", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("iso-8859-7", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("iso-8859-8", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("iso-8859-9", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-8859-14", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-8859-15", RFC2047_CHARSET_ENCODING_Q), &
                                                   rfc2047_charset_encoding_type("iso-2022-jp", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("iso-2022-kr", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("gb2312", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("gbk", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("gb18030", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("big5", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("cn-big5", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("cn-gb", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("cn-gb-2312", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("euc-kr", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("iso-2022-jp-2", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("iso-2022-int-1", RFC2047_CHARSET_ENCODING_B), &
                                                   rfc2047_charset_encoding_type("viscii", RFC2047_CHARSET_ENCODING_Q)]

    !> Whether encoded words should be encoded again.  Port of rfc2047.el
    !> rfc2047-encode-encoded-words.
    logical, public :: rfc2047_encode_encoded_words = .true.

    !> Whether to decode irregular Q-encoded words.  Port of rfc2047.el
    !> rfc2047-allow-irregular-q-encoded-words.
    logical, public :: rfc2047_allow_irregular_q_encoded_words = .true.

    !> `rfc2047_encoding_type': encode as an unstructured field.
    integer, parameter, public :: RFC2047_ENCODING_TYPE_MIME = 1
    !> `rfc2047_encoding_type': encode as a field of addresses.
    integer, parameter, public :: RFC2047_ENCODING_TYPE_ADDRESS_MIME = 2

    !> The type of encoding done by `encode_rfc2047_region'.  This should be
    !> set around calls to `encode_rfc2047_region' to either
    !> RFC2047_ENCODING_TYPE_MIME or RFC2047_ENCODING_TYPE_ADDRESS_MIME (and
    !> set back after them).  See `rfc2047_header_encodings', for
    !> definitions.  Port of rfc2047.el rfc2047-encoding-type.
    integer, public :: rfc2047_encoding_type = RFC2047_ENCODING_TYPE_ADDRESS_MIME

    !> Maximum characters of each header line that contain encoded-words.
    !> According to RFC 2047, it is 76.  If it is 0, encoded-words will not
    !> be folded.  Too small value may cause an error.  You should not
    !> change this value.  Port of rfc2047.el rfc2047-encode-max-chars.
    integer, public :: rfc2047_encode_max_chars = 76

    !> If true, quote decoded words containing special characters.  Port of
    !> rfc2047.el rfc2047-quote-decoded-words-containing-tspecials.
    logical, public :: rfc2047_quote_decoded_words_containing_tspecials = .false.

    !> True means allow incomplete encoded-text in successive encoded-words.
    !> Dividing of encoded-text in the place other than character
    !> boundaries violates RFC2047 section 5, while we have a capability to
    !> decode it.  If it is true, the decoder will decode B- or Q-encoding
    !> in each encoded-word, concatenate them, and decode it by charset.
    !> Otherwise, the decoder will fully decode each encoded-word before
    !> concatenating them.  Port of rfc2047.el
    !> rfc2047-allow-incomplete-encoded-text.
    logical, public :: rfc2047_allow_incomplete_encoded_text = .true.

    !> Default charset used by low-level libraries ("" for none).  This
    !> variable should never be set.  Instead, it should be set (and set
    !> back) by functions that wish to call mail-parse functions and let
    !> them know what the desired charset is to be.  Port of mail-prsvr.el
    !> mail-parse-charset.
    character(len=:), allocatable, public :: mail_parse_charset

    !> Ignored charsets used by low-level libraries.  This variable should
    !> never be set.  Instead, it should be set (and set back) by functions
    !> that wish to call mail-parse functions and let them know what the
    !> desired charsets is to be ignored ("gnus-all" ignores all of them,
    !> "gnus-unknown" those Fortmacs cannot decode).  Port of mail-prsvr.el
    !> mail-parse-ignored-charsets.
    character(len=24), allocatable, public :: mail_parse_ignored_charsets(:)

    character(len=*), parameter :: TAB = achar(9), LF = achar(10), CR = achar(13)

    ! The special characters of RFC 2045 (ietf-drums.el
    ! ietf-drums-tspecials).
    character(len=*), parameter :: TSPECIALS = "()<>@,;:\""/[]?="

    ! An encoded word as `decode_rfc2047_region' finds it: its charset,
    ! its encoding ("B" or "Q", either case), its encoded text and the
    ! whole word.
    type :: encoded_word_type
        character(len=:), allocatable :: charset, text, word
        character :: encoding = " "
    end type encoded_word_type

    ! A run of decoded text and its coding system ("" for an encoded word
    ! left as it is).
    type :: decoded_piece_type
        character(len=:), allocatable :: cs, text
    end type decoded_piece_type

    ! The syntax table for parsing into regions that may need encoding
    ! (rfc2047-syntax-table; null until first used).
    type(handle_type) :: rfc2047_syntax_table

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Regexp that matches encoded word (rfc2047-encoded-word-regexp).  The
    ! patterns for the B encoding and the Q encoding, i.e. the ones
    ! beginning with "B" and "Q" respectively, are restricted into only
    ! the characters that those encodings may generally use.  Port of
    ! rfc2047.el rfc2047-encoded-word-regexp.
    function encoded_word_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "=\?\([^]["//achar(0)//"-"//achar(32)//"()<>@,;:*\""/?.=]+\)\(?:\*[^?]+\)?\?"// &
                             "\(B\?[+/0-9A-Za-z]*=*\|Q\?[ ->@-~]*\)\?="
    end function encoded_word_regular_expression

    ! Regexp that matches encoded word allowing loose Q encoding.  After
    ! "Q?", it allows "?"s that follow a character other than "=", "="
    ! after "Q?", "?"s in the middle that follow a character other than
    ! "=", any characters other than "?" in the middle, and "?"s at the
    ! end.  Port of rfc2047.el rfc2047-encoded-word-regexp-loose.
    function loose_encoded_word_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "=\?\([^]["//achar(0)//"-"//achar(32)//"()<>@,;:*\""/?.=]+\)\(?:\*[^?]+\)?\?"// &
                             "\(B\?[+/0-9A-Za-z]*=*\|Q\?\(?:\?+[ -<>@-~]\)?\(?:[ ->@-~]+\?+[ -<>@-~]\)*[ ->@-~]*\?*\)\?="
    end function loose_encoded_word_regular_expression

    ! The number of characters of TEXT (multibyte).  Not a port.
    pure integer(int64) function text_length(text)
        character(len=*), intent(in) :: text

        text_length = string_chars(text)
    end function text_length

    ! The characters FROM (inclusive) to TO (exclusive) of TEXT, counted
    ! from 0 as upstream's substring (TO absent: to the end).  Not a port.
    function substring(text, from, to) result(part)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: from
        integer(int64), intent(in), optional :: to
        character(len=:), allocatable :: part
        integer(int64) :: first, last

        first = min(string_index_to_offset(text, from + 1), len(text, kind=int64))
        last = len(text, kind=int64)
        if (present(to)) last = min(string_index_to_offset(text, to + 1), len(text, kind=int64))
        part = text(first + 1:last)
    end function substring

    ! Make a temporary buffer current (with-temp-buffer), unibyte unless
    ! MULTIBYTE; OLD is the buffer to go back to.  Not a port.
    subroutine temporary_buffer_begin(old, temporary, multibyte)
        type(handle_type), intent(out) :: old, temporary
        logical, intent(in) :: multibyte
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        buffers(current)%multibyte = multibyte
    end subroutine temporary_buffer_begin

    ! Close the temporary buffer TEMPORARY and make OLD current again.
    ! Not a port.
    subroutine temporary_buffer_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine temporary_buffer_end

    ! The position of MARKER.  Not a port.
    integer(int64) function position_of(marker) result(position)
        type(handle_type), intent(in) :: marker
        type(status_type) :: st

        call marker_position(marker, position, st)
    end function position_of

    ! The character after POSITION (absent: point), -1 at the end.
    ! Not a port.
    integer(int32) function after(position)
        integer(int64), intent(in), optional :: position
        logical :: found

        call char_after(after, found, position)
        if (.not. found) after = -1
    end function after

    ! The character before POSITION (absent: point), -1 at the beginning.
    ! Not a port.
    integer(int32) function before(position)
        integer(int64), intent(in), optional :: position
        logical :: found

        call char_before(before, found, position)
        if (.not. found) before = -1
    end function before

    ! Whether the mail parse charset asks the decoder to decode text by
    ! it: one is set, and it is neither us-ascii nor gnus-decoded.
    ! Not a port.
    logical function is_parse_charset_decoding()
        is_parse_charset_decoding = .false.
        if (.not. allocated(mail_parse_charset)) return
        if (len(mail_parse_charset) == 0) return
        is_parse_charset_decoding = mail_parse_charset /= "us-ascii" .and. mail_parse_charset /= "gnus-decoded"
    end function is_parse_charset_decoding

    ! The mail parse charset, "" if none.  Not a port.
    function parse_charset() result(charset)
        character(len=:), allocatable :: charset

        charset = ""
        if (allocated(mail_parse_charset)) charset = mail_parse_charset
    end function parse_charset

    ! Whether NAME is one of `mail_parse_ignored_charsets'.  Not a port.
    logical function is_ignored_charset(name)
        character(len=*), intent(in) :: name

        is_ignored_charset = .false.
        if (allocated(mail_parse_ignored_charsets)) is_ignored_charset = any(mail_parse_ignored_charsets == name)
    end function is_ignored_charset

    ! Set the defaults of the variables upstream initializes when it loads
    ! rfc2047.el, if unallocated.  Port of the defcustom of rfc2047.el
    ! rfc2047-header-encoding-alist.
    subroutine initialize_defaults()
        if (allocated(rfc2047_header_encodings)) return
        allocate (rfc2047_header_encodings(6))
        rfc2047_header_encodings(1) = rfc2047_header_encoding_type(header="Newsgroups")
        rfc2047_header_encodings(2) = rfc2047_header_encoding_type(header="Followup-To")
        rfc2047_header_encodings(3) = rfc2047_header_encoding_type(header="Message-ID")
        ! This header must be pre-encoded by the MTA, so avoid
        ! double-encoding it.
        rfc2047_header_encodings(4) = rfc2047_header_encoding_type(header="Content-Disposition", &
                                                                   method=RFC2047_HEADER_ENCODING_DEFAULT)
        rfc2047_header_encodings(5) = rfc2047_header_encoding_type( &
                                      header="\(Resent-\)?\(From\|Cc\|To\|Bcc\|\(In-\)?Reply-To\|Sender"// &
                                      "\|Mail-Followup-To\|Mail-Copies-To\|Approved\|Disposition-Notification-To\)", &
                                      method=RFC2047_HEADER_ENCODING_ADDRESS_MIME)
        rfc2047_header_encodings(6) = rfc2047_header_encoding_type(all_headers=.true., method=RFC2047_HEADER_ENCODING_MIME)
    end subroutine initialize_defaults

    ! The syntax table used when parsing into regions that may need
    ! encoding.  Double quotes are string delimiters, backslash is
    ! character quoting, and all other RFC 822 (or later) special
    ! characters are treated as punctuation so we can use forward-sexp and
    ! forward-word to skip to the end of regions appropriately.  Port of
    ! rfc2047.el rfc2047-syntax-table.
    function encoding_syntax_table() result(table)
        type(handle_type) :: table
        type(status_type) :: st
        character(len=*), parameter :: PUNCTUATION = "<>[]:;,@"
        integer :: k

        if (rfc2047_syntax_table%index == 0) then
            call make_syntax_table(rfc2047_syntax_table, st)
            ! Every character gets the word syntax of ?a in the standard
            ! table.
            call modify_syntax_entry(0, MAX_CHAR, "w", st, rfc2047_syntax_table)
            call modify_syntax_entry(iachar("\"), iachar("\"), "\", st, rfc2047_syntax_table)
            call modify_syntax_entry(iachar(""""), iachar(""""), """", st, rfc2047_syntax_table)
            call modify_syntax_entry(iachar("("), iachar("("), "(", st, rfc2047_syntax_table)
            call modify_syntax_entry(iachar(")"), iachar(")"), ")", st, rfc2047_syntax_table)

            do k = 1, len(PUNCTUATION)
                call modify_syntax_entry(iachar(PUNCTUATION(k:k)), iachar(PUNCTUATION(k:k)), ".", st, &
                                         rfc2047_syntax_table)
            end do
        end if

        table = rfc2047_syntax_table
    end function encoding_syntax_table

    ! ------------------------------------------------------------------
    ! Fields
    ! ------------------------------------------------------------------

    ! Return the type with which to encode the buffer: true for base64,
    ! false for quoted-printable.  Port of rfc2047.el:rfc2047-qp-or-base64.
    logical function is_base64_preferred()
        type(excursion_type) :: excursion
        type(status_type) :: st
        character(len=*), parameter :: SEVEN_BIT = achar(32)//"-"//achar(127)//achar(13)//achar(10)//achar(9)
        integer(int64) :: limit, n8bit, moved

        call save_excursion_begin(excursion)
        limit = min(point_max(), 2000 + point_min())
        n8bit = 0
        call goto_char(point_min())
        moved = skip_chars_forward(SEVEN_BIT, st, limit)

        do while (point() < limit)
            n8bit = n8bit + 1
            call forward_char(st)
            moved = skip_chars_forward(SEVEN_BIT, st, limit)
        end do

        ! Don't base64, say, a short line with a single non-ASCII char when
        ! splitting parts by charset.
        is_base64_preferred = .not. (6*n8bit < limit - point_min() .or. n8bit == 1)
        call save_excursion_end(excursion)
    end function is_base64_preferred

    !> Narrow the buffer to the header on the current line.  Port of
    !> rfc2047.el:rfc2047-narrow-to-field (upstream's
    !> mail-header-narrow-to-field).
    subroutine narrow_to_rfc2047_field(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, shortage, found

        call beginning_of_line()
        start = point()
        shortage = forward_line(1_int64)
        call re_search_forward("^[^ "//LF//TAB//"]", found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return

        if (found > 0) then
            end = line_beginning_position()
        else
            end = point_max()
        end if

        call narrow_to_region(start, end, status)
        call goto_char(point_min())
    end subroutine narrow_to_rfc2047_field

    !> Return the value of the field at point.  Port of
    !> rfc2047.el:rfc2047-field-value (upstream's mail-header-field-value).
    function rfc2047_field_value(status) result(value)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: value
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        integer(int64) :: found

        value = ""
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_rfc2047_field(status)

        if (status%ok()) then
            call re_search_forward(":[ "//TAB//LF//"]*", found, status, noerror=SEARCH_NOERROR)
            if (status%ok()) call buffer_substring_no_properties(point(), point_max(), value, status)
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end function rfc2047_field_value

    !> Fold the current header field.  Return the new end point in END.
    !> Port of rfc2047.el:rfc2047-fold-field (upstream's
    !> mail-header-fold-field).
    subroutine fold_rfc2047_field(end, status)
        integer(int64), intent(out) :: end
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction

        end = 0
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_rfc2047_field(status)

        if (status%ok()) then
            call fold_rfc2047_region(point_min(), point_max(), status)
            end = point_max()
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine fold_rfc2047_field

    !> Fold long lines in region B to E.  Port of
    !> rfc2047.el:rfc2047-fold-region.
    subroutine fold_rfc2047_region(b, e, status)
        integer(int64), intent(in) :: b, e
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction, inner
        type(excursion_type) :: excursion
        integer(int64) :: break, qword_break, bol, moved, found
        integer(int32) :: c
        logical :: first, matched

        call save_restriction_begin(restriction)
        call narrow_to_region(b, e, status)

        body: block
            if (.not. status%ok()) exit body
            call goto_char(point_min())
            break = 0
            qword_break = 0
            call save_restriction_begin(inner)
            call widen()
            bol = line_beginning_position()
            call save_restriction_end(inner)
            ! This function is either called with the Header: name in the
            ! region or not.  If it's not in the region, then we may already
            ! have a space.
            first = bol == point()

            if (.not. first) then
                call save_restriction_begin(inner)
                call widen()
                call save_excursion_begin(excursion)
                call re_search_backward("[ "//TAB//"]", found, status, bound=bol, noerror=SEARCH_NOERROR)
                first = found == 0
                call save_excursion_end(excursion)
                call save_restriction_end(inner)
                if (.not. status%ok()) exit body
            end if

            do while (.not. is_eob())
                if (break > 0 .or. qword_break > 0) then
                    if (point() - bol > 76) then
                        ! We have a line longer than 76 characters, so break
                        ! the line.
                        call break_line(break, qword_break, bol, status)
                        if (.not. status%ok()) exit body
                        break = 0
                        qword_break = 0
                    end if
                end if

                ! See whether we're at a point where we can break the line
                ! (if it turns out to be too long).
                c = after()

                if (c == 10) then
                    ! New line, so there's nothing to break.
                    call forward_char(status)
                    bol = point()
                    break = 0
                    qword_break = 0
                    moved = skip_chars_forward(" "//TAB, status)
                    if (.not. is_eob() .and. after() /= 10) call forward_char(status)
                else if (c == 13) then
                    ! CR in CRLF; shouldn't really as this function shouldn't
                    ! be called after encoding for line transmission.
                    call forward_char(status)
                else if (c == 32 .or. c == 9) then
                    ! Whitespace -- possible break point.
                    moved = skip_chars_forward(" "//TAB, status)

                    ! Don't break just after the header name.
                    if (first) then
                        first = .false.
                    else
                        break = point()
                    end if
                else if (break == 0) then
                    ! If the header has been encoded (with RFC2047 encoding,
                    ! which looks like "=?utf-8?Q?F=C3=B3?=".
                    matched = looking_at("=\?[^=]", status)

                    if (.not. matched) then
                        if (c == iachar("=")) then
                            call forward_char(status)
                        else
                            moved = skip_chars_forward("^ "//TAB//LF//CR//"=", status)
                        end if
                    else
                        ! Don't break at the start of the field.
                        if (point() /= b) qword_break = point()
                        moved = skip_chars_forward("^ "//TAB//LF//CR, status)
                    end if
                else
                    ! Look for the next LWSP (i.e., whitespace character).
                    moved = skip_chars_forward("^ "//TAB//LF//CR, status)
                end if

                if (.not. status%ok()) exit body
            end do

            if (break > 0 .or. qword_break > 0) then
                ! Finally, after the loop, we have a line longer than 76
                ! characters, so break the line.
                if (point() - bol > 76) call break_line(break, qword_break, bol, status)
            end if
        end block body

        call save_restriction_end(restriction)
    end subroutine fold_rfc2047_region

    ! Break the line at BREAK (or QWORD_BREAK when BREAK is 0); BOL becomes
    ! the beginning of the new line.  Port of rfc2047.el:rfc2047--break-line.
    subroutine break_line(break, qword_break, bol, status)
        integer(int64), intent(in) :: break, qword_break
        integer(int64), intent(out) :: bol
        type(status_type), intent(out) :: status
        integer(int64) :: moved

        if (break > 0) then
            call goto_char(break)
        else
            call goto_char(qword_break)
        end if

        moved = skip_chars_backward(" "//TAB, status)

        if (after() == 32 .or. after() == 9) then
            call insert(LF, status)
        else
            call insert(LF//" ", status)
        end if

        ! Return beginning-of-line.
        bol = point() - 1
        ! Don't break before the first non-LWSP characters.
        moved = skip_chars_forward(" "//TAB, status)
        if (.not. is_eob()) call forward_char(status)
    end subroutine break_line

    !> Unfold the current header field.  Port of
    !> rfc2047.el:rfc2047-unfold-field (upstream's mail-header-unfold-field).
    subroutine unfold_rfc2047_field(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_rfc2047_field(status)
        if (status%ok()) call unfold_rfc2047_region(point_min(), point_max(), status)
        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine unfold_rfc2047_field

    !> Unfold lines in region B to E.  Port of
    !> rfc2047.el:rfc2047-unfold-region.
    subroutine unfold_rfc2047_region(b, e, status)
        integer(int64), intent(in) :: b, e
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction, inner
        integer(int64) :: bol, eol, shortage, moved
        logical :: matched

        call save_restriction_begin(restriction)
        call narrow_to_region(b, e, status)

        body: block
            if (.not. status%ok()) exit body
            call goto_char(point_min())
            call save_restriction_begin(inner)
            call widen()
            bol = line_beginning_position()
            call save_restriction_end(inner)
            eol = line_end_position()
            shortage = forward_line(1_int64)

            do while (.not. is_eob())
                matched = looking_at("[ "//TAB//"]", status)
                if (.not. status%ok()) exit body

                if (matched .and. line_end_position() - bol < 76) then
                    call goto_char(eol)
                    moved = skip_chars_forward(CR//LF, status)
                    call delete_region(eol, point(), status)
                    if (.not. status%ok()) exit body
                else
                    bol = line_beginning_position()
                end if

                eol = line_end_position()
                shortage = forward_line(1_int64)
            end do
        end block body

        call save_restriction_end(restriction)
    end subroutine unfold_rfc2047_region

    ! ------------------------------------------------------------------
    ! Encoding
    ! ------------------------------------------------------------------

    !> Encode the message header according to `rfc2047_header_encodings'.
    !> Should be called narrowed to the head of the message.  Port of
    !> rfc2047.el:rfc2047-encode-message-header (upstream's
    !> mail-encode-encoded-word-buffer).
    recursive subroutine encode_rfc2047_message_header(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(rfc2047_header_encoding_type) :: elem
        character(len=:), allocatable :: charsets(:), coding_system
        character(len=24), allocatable :: saved_priorities(:)
        integer(int64) :: found, inserted
        integer :: k, method, saved_type
        logical :: matched_method, saved_fold, encodable, matched

        call initialize_defaults()
        call save_excursion_begin(excursion)
        call goto_char(point_min())
        saved_fold = case_fold_search%value

        fields: do while (.not. is_eob())
            call save_restriction_begin(restriction)

            field: block
                call narrow_to_rfc2047_field(status)
                if (.not. status%ok()) exit field
                method = RFC2047_HEADER_ENCODING_NONE
                coding_system = ""
                call find_mime_charset_region(point_min(), point_max(), charsets, status)
                if (.not. status%ok()) exit field
                ! M$ Outlook boycotts decoding of a header if it consists of
                ! two or more encoded words and those charsets differ; it
                ! seems to decode all words in a header from a charset found
                ! first in the header.  So, we unify the charsets into a
                ! single one used for encoding the whole text in a header.
                if (allocated(mime_coding_system_priorities)) saved_priorities = mime_coding_system_priorities

                if (size(charsets) == 1) then
                    if (allocated(mime_coding_system_priorities)) then
                        mime_coding_system_priorities = [character(len=24) :: &
                                                         mime_charset_to_coding_system(trim(charsets(1))), &
                                                         mime_coding_system_priorities]
                    else
                        mime_coding_system_priorities = [character(len=24) :: &
                                                         mime_charset_to_coding_system(trim(charsets(1)))]
                    end if
                end if

                case_fold_search%value = .true.
                matched_method = .false.
                encodable = .false.

                do k = 1, size(rfc2047_header_encodings)
                    elem = rfc2047_header_encodings(k)

                    if (elem%all_headers) then
                        matched_method = .true.
                    else
                        matched_method = looking_at(trim(elem%header), status)
                        if (.not. status%ok()) exit
                    end if

                    if (matched_method) then
                        method = elem%method
                        coding_system = trim(elem%coding_system)
                        exit
                    end if
                end do

                if (status%ok()) call check_rfc2047_encodable(encodable, status)

                ! We found something that may perhaps be encoded.  (With no
                ! posting charset, a field that needs no encoding is left
                ! alone.)
                if (status%ok() .and. encodable) then
                    call re_search_forward("^[^:]+: *", found, status, noerror=SEARCH_NOERROR)

                    if (status%ok()) then
                        select case (method)
                        case (RFC2047_HEADER_ENCODING_ADDRESS_MIME)
                            call encode_rfc2047_region(point(), point_max(), status)
                        case (RFC2047_HEADER_ENCODING_MIME)
                            saved_type = rfc2047_encoding_type
                            rfc2047_encoding_type = RFC2047_ENCODING_TYPE_MIME
                            call encode_rfc2047_region(point(), point_max(), status)
                            rfc2047_encoding_type = saved_type
                        case (RFC2047_HEADER_ENCODING_DEFAULT)
                            if (len(parse_charset()) > 0) then
                                call encode_coding_region(point(), point_max(), parse_charset(), inserted, status)
                            end if
                        case (RFC2047_HEADER_ENCODING_NONE)
                            ! We get this when Cc'ing messages to newsgroups
                            ! with 8-bit names.  The group name mail copy just
                            ! got unconditionally encoded.
                            call encode_rfc2047_region(point(), point_max(), status)
                        case (RFC2047_HEADER_ENCODING_CODING_SYSTEM)
                            if (len(coding_system) > 0) then
                                matched = is_coding_system(coding_system)
                                if (matched) call encode_coding_region(point(), point_max(), coding_system, inserted, status)
                            end if
                        case default
                            ! Hm.
                        end select
                    end if
                end if

                call goto_char(point_max())
            end block field

            case_fold_search%value = saved_fold

            if (allocated(saved_priorities)) then
                mime_coding_system_priorities = saved_priorities
                deallocate (saved_priorities)
            end if

            call save_restriction_end(restriction)
            if (.not. status%ok()) exit fields
        end do fields

        call save_excursion_end(excursion)
    end subroutine encode_rfc2047_message_header

    !> Set ENCODABLE to whether any characters in current buffer need
    !> encoding in headers.  The buffer may be narrowed.  Point moves to the
    !> beginning of the accessible text, as upstream's does (so this is a
    !> subroutine).  Port of rfc2047.el:rfc2047-encodable-p (with no posting
    !> charset).
    subroutine check_rfc2047_encodable(encodable, status)
        logical, intent(out) :: encodable
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: charsets(:)
        integer(int64) :: found

        encodable = .false.
        call find_mime_charset_region(point_min(), point_max(), charsets, status)
        if (.not. status%ok()) return
        call goto_char(point_min())

        if (rfc2047_encode_encoded_words) then
            call re_search_forward(encoded_word_regular_expression(), found, status, noerror=SEARCH_NOERROR)
            call goto_char(point_min())
            if (.not. status%ok()) return
            encodable = found > 0
        end if

        if (.not. encodable) encodable = size(charsets) > 0
    end subroutine check_rfc2047_encodable

    !> Encode words in region B to E that need encoding.  By default, the
    !> region is treated as containing RFC 822 (or later) addresses.  Set
    !> `rfc2047_encoding_type' to change that.  Unless DONT_FOLD, the lines
    !> are then folded.  Text that cannot be parsed is the error "Invalid
    !> data for rfc2047 encoding: TEXT" in STATUS.  Port of
    !> rfc2047.el:rfc2047-encode-region (upstream's
    !> mail-encode-encoded-word-region).
    recursive subroutine encode_rfc2047_region(b, e, status, dont_fold)
        integer(int64), intent(in) :: b, e
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: dont_fold
        type(restriction_type) :: restriction
        type(handle_type) :: saved_table
        character(len=:), allocatable :: encodable, original_text, word
        integer(int64) :: start, end, begin, moved, found, k
        integer :: saved_type
        character :: syntax
        logical :: last_encoded, matched, has_end, folding, prev_char_ok
        type(status_type) :: st

        folding = .true.
        if (present(dont_fold)) folding = .not. dont_fold
        call save_restriction_begin(restriction)
        call narrow_to_region(b, e, status)

        body: block
            if (.not. status%ok()) exit body

            if (rfc2047_encode_encoded_words) then
                encodable = "[^"//achar(0)//"-"//achar(127)//"]+\|=\?"
            else
                encodable = "[^"//achar(0)//"-"//achar(127)//"]+"
            end if

            ! Whether there's an encoded word before the current token,
            ! either immediately or separated by space.
            last_encoded = .false.
            call buffer_substring_no_properties(b, e, original_text, status)
            if (.not. status%ok()) exit body

            if (rfc2047_encoding_type == RFC2047_ENCODING_TYPE_MIME) then
                ! Simple case.  Continuous words in which all those contain
                ! non-ASCII characters are encoded collectively.  Encoding
                ! ASCII words, including `Re:' used in Subject headers, is
                ! avoided for interoperability with non-MIME clients and for
                ! making it easy to find keywords.
                call goto_char(point_min())

                do
                    moved = skip_chars_forward(" "//TAB//LF, status)
                    if (is_eob()) exit
                    start = point()
                    end = point()

                    do
                        matched = looking_at("[ "//TAB//LF//"]*\([^ "//TAB//LF//"]+\)", status)
                        if (.not. matched) exit
                        end = match_end(0, st)
                        call re_search_forward(encodable, found, status, bound=end, noerror=SEARCH_NOERROR)
                        if (found == 0) exit
                        call goto_char(end)
                    end do

                    if (point() > start) then
                        call encode_words(start, point(), status)
                        if (.not. status%ok()) exit body
                    else
                        call goto_char(end)
                    end if
                end do
            else
                saved_table = syntax_table()
                call set_syntax_table(encoding_syntax_table(), status)
                call goto_char(point_min())

                ! In case of unbalanced quotes.
                tokens: block
                    ! Look for RFC 822 (or later) style: sequences of atoms,
                    ! quoted strings, specials, whitespace.  (Specials mustn't
                    ! be encoded.)
                    do while (.not. is_eob())
                        ! Skip whitespace.
                        moved = skip_chars_forward(" "//TAB//LF, status)
                        start = point()
                        ! eob
                        if (after() < 0) cycle
                        ! else token start
                        syntax = char_syntax(after())

                        select case (syntax)
                        case ("""")
                            ! Quoted word.
                            call forward_expression(1_int64, status)
                            if (.not. status%ok()) exit tokens
                            end = point()
                            ! Does it need encoding?
                            call goto_char(start)
                            call re_search_forward(encodable, found, status, bound=end, noerror=SEARCH_MOVE)

                            if (found > 0) then
                                ! It needs encoding.  Strip the quotes first,
                                ! since encoded words can't occur in quotes.
                                call goto_char(end)
                                call delete_char(-1_int64, status)
                                call goto_char(start)
                                call delete_char(1_int64, status)

                                if (last_encoded) then
                                    ! There was a preceding quoted word.  We
                                    ! need to include any separating whitespace
                                    ! in this word to avoid it getting lost.
                                    moved = skip_chars_backward(" "//TAB, status)
                                    ! A space is needed between the encoded
                                    ! words.
                                    call insert(" ", status)
                                    start = point()
                                    end = end + 1
                                end if

                                ! Adjust the end position for the deleted
                                ! quotes.
                                call encode_words(start, end - 2, status)
                                if (.not. status%ok()) exit tokens
                                ! Record that it was encoded.
                                last_encoded = .true.
                            else
                                last_encoded = .false.
                            end if
                        case (".")
                            ! Skip other delimiters, but record that they've
                            ! potentially separated quoted words.
                            call forward_char(status)
                            last_encoded = .false.
                        case (")")
                            call set_error(status, FM_GENERIC_ERROR, "Unbalanced parentheses")
                            exit tokens
                        case ("(")
                            ! Look for the end of parentheses.
                            call forward_group(1_int64, status)
                            if (.not. status%ok()) exit tokens
                            ! Encode text as an unstructured field.
                            saved_type = rfc2047_encoding_type
                            rfc2047_encoding_type = RFC2047_ENCODING_TYPE_MIME
                            call encode_rfc2047_region(start + 1, point() - 1, status)
                            rfc2047_encoding_type = saved_type
                            if (.not. status%ok()) exit tokens
                            moved = skip_chars_forward(")", status)
                        case default
                            ! Normal token/whitespace sequence.  Find the end.
                            ! Skip one ASCII word, or encode continuous words
                            ! in which all those contain non-ASCII characters.
                            call normal_token(status)
                            if (.not. status%ok()) exit tokens
                            moved = skip_chars_backward(" "//TAB//LF, status)
                            end = point()
                            call goto_char(start)
                            call re_search_forward(encodable, found, status, bound=end, noerror=SEARCH_MOVE)

                            if (found > 0) then
                                prev_char_ok = before(start) == -1 .or. before(start) == 9 .or. before(start) == 32

                                if (.not. prev_char_ok) then
                                    call goto_char(start)
                                    moved = skip_chars_backward("^ "//TAB//LF, status)
                                    matched = looking_at("\Sw+", status)
                                    if (matched) matched = match_end(0, st) == start

                                    if (matched) then
                                        ! Also encode bogus delimiters.
                                        start = point()
                                    else
                                        ! Separate encodable text and delimiter.
                                        call goto_char(start)
                                        call insert(" ", status)
                                        start = start + 1
                                        end = end + 1
                                    end if
                                end if

                                call encode_words(start, end, status)
                                if (.not. status%ok()) exit tokens
                                last_encoded = .true.
                            else
                                last_encoded = .false.
                            end if
                        end select

                        if (.not. status%ok()) exit tokens
                    end do
                end block tokens

                call set_syntax_table(saved_table, st)

                if (.not. status%ok()) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid data for rfc2047 encoding: "// &
                                   squeeze_whitespace(original_text))
                    exit body
                end if
            end if

            if (folding) call fold_rfc2047_region(b, point(), status)
            call goto_char(point_max())
        end block body

        call save_restriction_end(restriction)

    contains

        ! Find the end of a normal token at point (the `t' branch of the
        ! token loop): skip one ASCII word, or move over continuous words
        ! in which all those contain non-ASCII characters.
        subroutine normal_token(status)
            type(status_type), intent(inout) :: status

            ! Where an unset END means there may be text to have to be
            ! encoded following the point.  Otherwise, the point reached to
            ! the end of ASCII words separated by whitespace or a special
            ! char.
            has_end = .false.

            do while (.not. (has_end .or. is_eob()))
                matched = looking_at("["//achar(0)//"-"//achar(127)//"]+", status)

                if (matched) then
                    begin = point()
                    end = match_end(0, st)
                    has_end = .true.

                    do
                        call re_search_forward("[ "//TAB//LF//"]\|\Sw", found, status, bound=end, noerror=SEARCH_MOVE)

                        if (found == 0) then
                            has_end = .false.
                            exit
                        end if

                        ! Skip backslash-quoted characters.
                        if (char_syntax(before()) /= "\") exit
                        call forward_char(status)
                    end do

                    if (has_end) then
                        end = match_beginning(0, st)

                        if (rfc2047_encode_encoded_words) then
                            call goto_char(begin)
                            call search_forward("=?", found, status, bound=end, noerror=SEARCH_MOVE)

                            if (found > 0) then
                                call goto_char(match_beginning(0, st))
                                has_end = .false.
                            end if
                        else
                            call goto_char(end)
                        end if
                    end if
                end if

                if (has_end) cycle
                matched = looking_at(encodable, status)
                if (.not. matched) cycle
                begin = match_end(0, st)
                call goto_char(begin)

                do
                    matched = looking_at("[ "//TAB//LF//"]+\([^ "//TAB//LF//"]+\)", status)
                    if (.not. matched) exit
                    end = match_end(0, st)
                    has_end = .true.

                    do
                        call re_search_forward(encodable, found, status, bound=end, noerror=SEARCH_NOERROR)
                        if (found == 0) exit
                    end do

                    if (.not. begin < point()) exit
                    call goto_char(begin)
                    call re_search_forward("\Sw", found, status, bound=end, noerror=SEARCH_NOERROR)

                    if (found > 0) then
                        call goto_char(match_beginning(0, st))
                        exit
                    end if

                    call goto_char(end)
                end do

                matched = looking_at("[^ "//TAB//LF//"]+", status)
                if (.not. matched) cycle
                end = match_end(0, st)
                has_end = .true.
                call re_search_forward("\Sw+", found, status, bound=end, noerror=SEARCH_NOERROR)

                if (found > 0) then
                    ! There are special characters better to be encoded so
                    ! that MTAs may parse them safely.
                    if (end == point()) cycle
                    matched = looking_at("\sw*\("//encodable//"\)", status)

                    if (matched) then
                        has_end = .false.
                    else
                        call goto_char(match_end(0, st) - 1)
                        ! Separate encodable text and delimiter.
                        if (point() /= match_beginning(0, st)) call insert(" ", status)
                    end if
                else
                    call goto_char(end)
                    moved = skip_chars_forward(" "//TAB//LF, status)
                    matched = looking_at("[^ "//TAB//LF//"]+", status)

                    if (matched) then
                        word = match_string(0)
                        k = string_match(encodable, word, st)
                        matched = k > 0
                    end if

                    if (matched) then
                        has_end = .false.
                    else
                        call goto_char(end)
                    end if
                end if
            end do
        end subroutine normal_token
    end subroutine encode_rfc2047_region

    ! TEXT with each run of spaces, tabs and newlines replaced by one space.
    ! Not a port.
    function squeeze_whitespace(text) result(squeezed)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: squeezed
        integer :: k
        logical :: in_space

        squeezed = ""
        in_space = .false.

        do k = 1, len(text)
            if (scan(text(k:k), " "//TAB//LF) > 0) then
                if (.not. in_space) squeezed = squeezed//" "
                in_space = .true.
            else
                squeezed = squeezed//text(k:k)
                in_space = .false.
            end if
        end do
    end function squeeze_whitespace

    !> Encode words in STRING.  By default, the string is treated as
    !> containing addresses (see `rfc2047_encoding_type').  Unless
    !> DONT_FOLD, long lines are folded.  Port of
    !> rfc2047.el:rfc2047-encode-string (upstream's
    !> mail-encode-encoded-word-string).
    function encode_rfc2047_string(string, status, dont_fold) result(encoded)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: dont_fold
        character(len=:), allocatable :: encoded
        type(handle_type) :: old, temporary

        call temporary_buffer_begin(old, temporary, .true.)
        call insert(string, status)
        if (status%ok()) call encode_rfc2047_region(point_min(), point_max(), status, dont_fold)
        encoded = buffer_string()
        call temporary_buffer_end(old, temporary)
    end function encode_rfc2047_string

    ! Encode the word(s) in the region B to E.  Point moves to the end of
    ! the region.  Port of rfc2047.el:rfc2047-encode.
    recursive subroutine encode_words(b, e, status)
        integer(int64), intent(in) :: b, e
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction
        type(handle_type) :: b_marker, e_marker
        type(status_type) :: st
        character(len=:), allocatable :: charsets(:), charset, cs, text, tail, crest, eword, start, letter
        integer(int64) :: moved, delete_start, column
        integer :: k, encoding

        call find_mime_charset_region(b, e, charsets, status)
        if (.not. status%ok()) return

        if (size(charsets) > 1) then
            call buffer_substring_no_properties(b, e, text, st)
            call set_error(status, FM_GENERIC_ERROR, "Can't rfc2047-encode `"//text//"'")
            return
        end if

        if (size(charsets) == 1) then
            charset = trim(charsets(1))
        else
            charset = "us-ascii"
        end if

        cs = mime_charset_to_coding_system(charset)

        if (.not. buffers(current)%multibyte .or. len(cs) == 0) then
            cs = ""
        else if (.not. is_coding_system(cs)) then
            cs = ""
        end if

        call save_restriction_begin(restriction)

        body: block
            call narrow_to_region(b, e, status)
            if (.not. status%ok()) exit body
            encoding = RFC2047_CHARSET_ENCODING_NONE
            k = findloc(rfc2047_charset_encodings%charset == charset, .true., dim=1)
            if (k > 0) encoding = rfc2047_charset_encodings(k)%encoding

            ! For the charsets that don't have a preferred encoding, choose
            ! the one that's shorter.
            if (encoding == RFC2047_CHARSET_ENCODING_NONE) then
                if (is_base64_preferred()) then
                    encoding = RFC2047_CHARSET_ENCODING_B
                else
                    encoding = RFC2047_CHARSET_ENCODING_Q
                end if
            end if

            call widen()
            call goto_char(e)
            moved = skip_chars_forward("^ "//TAB//LF, status)
            ! TAIL may contain a close parenthesis.
            call buffer_substring_no_properties(e, point(), tail, status)
            if (.not. status%ok()) exit body
            call goto_char(b)
            call copy_marker_at(b, b_marker, .false., st)
            call copy_marker_at(e, e_marker, .false., st)
            call fold_rfc2047_region(line_beginning_position(), position_of(b_marker), status)
            if (.not. status%ok()) exit body
            call goto_char(position_of(b_marker))
            moved = skip_chars_backward("^ "//TAB//LF, status)
            crest = " "

            ! CREST may contain whitespace and an open parenthesis.
            if (skip_chars_backward(" "//TAB, status) /= 0) then
                call buffer_substring_no_properties(point(), position_of(b_marker), crest, status)
            end if

            call buffer_substring_no_properties(position_of(b_marker), position_of(e_marker), text, status)
            if (.not. status%ok()) exit body

            if (encoding == RFC2047_CHARSET_ENCODING_B) then
                letter = "B"
            else
                letter = "Q"
            end if

            start = "=?"//ascii_downcase(charset)//"?"//letter//"?"
            column = position_of(b_marker) - line_beginning_position()
            eword = encode_1(column, without_newlines(text), cs, encoding, start, crest, tail, status)
            if (.not. status%ok()) exit body

            if (eword(1:1) == LF) then
                delete_start = point()
                ! The line was folded before encoding.
                if (is_bol()) delete_start = point() - 1
            else
                call goto_char(position_of(b_marker))
                delete_start = point()
            end if

            ! EWORD contains CREST and TAIL.
            call delete_region(delete_start, position_of(e_marker) + text_length(tail), status)
            if (status%ok()) call insert(eword, status)
            if (.not. status%ok()) exit body

            if (len(tail) == 0 .and. .not. is_eob()) then
                if (.not. looking_at("[ "//TAB//LF//")]", st)) call insert(" ", status)
            end if
        end block body

        if (b_marker%index /= 0) call destroy_marker(b_marker, st)
        if (e_marker%index /= 0) call destroy_marker(e_marker, st)
        call save_restriction_end(restriction)
    end subroutine encode_words

    ! TEXT with ASCII upper-case letters in lower case.  Not a port.
    pure function ascii_downcase(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower
        integer :: k

        lower = text

        do k = 1, len(text)
            if (lge(text(k:k), "A") .and. lle(text(k:k), "Z")) lower(k:k) = achar(iachar(text(k:k)) + 32)
        end do
    end function ascii_downcase

    ! TEXT without its newlines (replace-regexp-in-string "\n\([ \t]?\)"
    ! "\1", which keeps the space or tab after each).  Not a port.
    pure function without_newlines(text) result(joined)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: joined
        integer :: k

        joined = ""

        do k = 1, len(text)
            if (text(k:k) /= LF) joined = joined//text(k:k)
        end do
    end function without_newlines

    ! The encoded words of STRING, starting at COLUMN, as START (the
    ! "=?CHARSET?E?" prefix), the encoded text of STRING encoded by CS ("":
    ! as it is) and "?=", broken into lines of at most
    ! `rfc2047_encode_max_chars' that start with CREST, followed by TAIL;
    ! EWORD is the text already encoded.  Port of
    ! rfc2047.el:rfc2047-encode-1.
    recursive function encode_1(column, string, cs, encoding, start, crest_in, tail_in, status, eword_in) result(eword)
        integer(int64), intent(in) :: column
        character(len=*), intent(in) :: string, cs, start, crest_in, tail_in
        integer, intent(in) :: encoding
        type(status_type), intent(inout) :: status
        character(len=*), intent(in), optional :: eword_in
        character(len=:), allocatable :: eword
        character(len=:), allocatable :: crest, tail, prev, next, done
        type(status_type) :: st
        integer(int64) :: index, limit, length, newline, m
        logical :: has_prev, has_eword

        eword = ""
        crest = crest_in
        tail = tail_in
        has_eword = present(eword_in)
        done = ""
        if (has_eword) done = eword_in

        if (len(string) == 0) then
            eword = done
        else if (rfc2047_encode_max_chars == 0) then
            eword = start//encoded_text(string)//"?="
        else if (column >= rfc2047_encode_max_chars) then
            if (has_eword) then
                if (string_match(LF//"[ "//TAB//"]+\'", done, st) > 0) then
                    ! Remove a superfluous empty line.
                    done = substring(done, 0_int64, match_beginning(0, st) - 1)
                else if (string_match("(+\'", done, st) > 0) then
                    ! Break the line before the open parenthesis.
                    m = match_beginning(0, st) - 1
                    crest = crest//substring(done, m)
                    done = substring(done, 0_int64, m)
                end if
            end if

            eword = encode_1(text_length(crest), string, cs, encoding, start, " ", tail, status, done//LF//crest)
        else
            index = 0
            limit = text_length(string) - 1
            prev = ""
            has_prev = .true.
            next = ""

            do while (has_prev .and. index <= limit)
                next = start//encoded_text(substring(string, 0_int64, index + 1))//"?="
                if (.not. status%ok()) return
                length = column + len(next, kind=int64)

                if (length > rfc2047_encode_max_chars) then
                    next = prev
                    has_prev = .false.
                    cycle
                end if

                newline = int(scan(tail, LF), int64) - 1
                if (newline < 0) newline = text_length(tail)

                if (index < limit .or. length + newline <= rfc2047_encode_max_chars) then
                    prev = next
                    index = index + 1
                else if (string_match("\`)+", tail, st) > 0) then
                    ! Break the line after the close parenthesis.
                    m = match_end(0, st) - 1
                    tail = substring(tail, 0_int64, m)//LF//" "//substring(tail, m)
                    prev = next
                    index = index + 1
                else
                    next = prev
                    has_prev = .false.
                end if
            end do

            if (index > limit) then
                eword = done//next//tail
                return
            end if

            if (index == 0) then
                if (has_eword .and. string_match("(+\'", done, st) > 0) then
                    m = match_beginning(0, st) - 1
                    crest = crest//substring(done, m)
                    done = substring(done, 0_int64, m)
                else
                    done = done//next
                end if
            else
                crest = " "
                done = done//next
            end if

            if (string_match(LF//"[ "//TAB//"]+\'", done, st) > 0) then
                ! Remove a superfluous empty line.
                done = substring(done, 0_int64, match_beginning(0, st) - 1)
            end if

            eword = encode_1(text_length(crest), substring(string, index), cs, encoding, start, " ", tail, status, &
                             done//LF//crest)
        end if

    contains

        ! The B or Q encoding of TEXT, encoded by CS first unless it is "".
        function encoded_text(text) result(encoded)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: encoded
            character(len=:), allocatable :: bytes

            if (len(cs) > 0) then
                call encode_coding_string(text, cs, bytes, status)
                if (.not. status%ok()) bytes = ""
            else
                bytes = text
            end if

            if (encoding == RFC2047_CHARSET_ENCODING_B) then
                ! Base64-encode the header contained in TEXT.  Port of
                ! rfc2047.el:rfc2047-b-encode-string.
                encoded = base64_encode_string(bytes, no_line_break=.true., multibyte=len(cs) == 0, status=status)
            else
                encoded = quoted_printable_encoded(bytes, status)
            end if
        end function encoded_text
    end function encode_1

    ! Quoted-printable-encode the header in BYTES.  = (\075), _ (\137), ?
    ! (\077) are used in the encoded word.  Avoid using 8bit characters.
    ! This class excludes `especials' (see the RFC2047 syntax), meaning
    ! that some characters in non-structured fields will get encoded when
    ! they don't need to be.  Port of rfc2047.el:rfc2047-q-encode-string.
    function quoted_printable_encoded(bytes, status) result(encoded)
        character(len=*), intent(in) :: bytes
        type(status_type), intent(inout) :: status
        character(len=:), allocatable :: encoded
        type(handle_type) :: old, temporary

        call temporary_buffer_begin(old, temporary, .false.)
        call insert(bytes, status)

        if (status%ok()) then
            call encode_quoted_printable_region(point_min(), point_max(), status, &
                                                class="-"//achar(8)//LF//achar(12)//" !#-'*+0-9A-Z\^`-~"//achar(127))
        end if

        if (status%ok()) call subst_char_in_region(point_min(), point_max(), iachar(" "), iachar("_"), status)
        encoded = buffer_string()
        call temporary_buffer_end(old, temporary)
    end function quoted_printable_encoded

    ! ------------------------------------------------------------------
    ! Decoding
    ! ------------------------------------------------------------------

    !> Strip backslashes in quoted strings.  `\"' remains.  Port of
    !> rfc2047.el:rfc2047-strip-backslashes-in-quoted-strings.
    subroutine strip_rfc2047_backslashes_in_quoted_strings(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved_table
        type(restriction_type) :: restriction
        type(status_type) :: st
        integer(int64) :: beginning, found, inner

        status = status_type()
        call goto_char(point_min())
        saved_table = syntax_table()
        call set_syntax_table(standard_syntax_table(), st)

        do
            call search_forward("""", found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            ! (Upstream skips a quote whose preceding character is a
            ! backslash, but tests the character before point, which is
            ! the quote just found: every quote starts a string.)
            beginning = match_end(0, st)
            call goto_char(match_beginning(0, st))

            ! A string that does not end is left alone.
            quoted: block
                call forward_expression(1_int64, st)
                if (.not. st%ok()) exit quoted
                call save_restriction_begin(restriction)
                call narrow_to_region(beginning, point() - 1, st)
                call goto_char(beginning)

                do while (st%ok())
                    call search_forward("\", inner, st, noerror=SEARCH_MOVE)
                    if (inner == 0) exit
                    if (after() /= iachar("""")) call delete_char(-1_int64, st)
                    if (st%ok()) call forward_char(st)
                end do

                call save_restriction_end(restriction)
                if (.not. st%ok()) exit quoted
                call forward_char(st)
            end block quoted

            if (.not. st%ok()) call goto_char(beginning)
        end do

        call set_syntax_table(saved_table, st)
    end subroutine strip_rfc2047_backslashes_in_quoted_strings

    !> Return the coding system corresponding to MIME CHARSET ("" if none),
    !> "" if Fortmacs can't decode CHARSET.
    !>
    !> If ALLOW_OVERRIDE is true, use `mime_charset_overrides' to map
    !> undesired charset names to their replacement.  This should only be
    !> used for decoding, not for encoding.  Port of
    !> rfc2047.el:rfc2047-charset-to-coding-system.
    function rfc2047_charset_to_coding_system(charset, allow_override) result(cs)
        character(len=*), intent(in) :: charset
        logical, intent(in), optional :: allow_override
        character(len=:), allocatable :: cs
        character(len=:), allocatable :: name

        name = ascii_downcase(charset)
        if (len(name) == 0 .or. is_ignored_charset("gnus-all") .or. is_ignored_charset(name)) name = parse_charset()
        cs = mime_charset_to_coding_system(name, allow_override)

        if (cs == "ascii") then
            cs = mime_charset_to_coding_system(parse_charset())
            if (len(cs) == 0) cs = "raw-text"
        else if (.not. is_known_coding_system(cs)) then
            if (len(name) > 0 .and. is_ignored_charset("gnus-unknown")) cs = mime_charset_to_coding_system(parse_charset())
        end if

        if (cs == "ascii") cs = "raw-text"
    end function rfc2047_charset_to_coding_system

    ! Whether CS is a coding system (not "").  Not a port.
    logical function is_known_coding_system(cs)
        character(len=*), intent(in) :: cs

        is_known_coding_system = .false.
        if (len(cs) > 0) is_known_coding_system = is_coding_system(cs)
    end function is_known_coding_system

    ! Decode successive encoded-words in WORDS and return a decoded
    ! string.  Each element of WORDS has a CHARSET, an ENCODING ("B" or
    ! "Q", either case), the encoded text and the encoded word.  Port of
    ! rfc2047.el:rfc2047-decode-encoded-words.
    function decode_encoded_words(words) result(decoded)
        type(encoded_word_type), intent(in) :: words(:)
        character(len=:), allocatable :: decoded
        type(decoded_piece_type), allocatable :: rest(:)
        type(status_type) :: st
        character(len=:), allocatable :: cs, text, piece
        integer :: k, n
        logical :: ok, has_words

        allocate (rest(size(words)))
        n = 0

        do k = 1, size(words)
            cs = rfc2047_charset_to_coding_system(words(k)%charset, .true.)
            ok = len(cs) > 0

            if (ok) then
                st = status_type()

                if (words(k)%encoding == "B" .or. words(k)%encoding == "b") then
                    text = base64_decode_string(pad_rfc2047_base64(words(k)%text), status=st)
                else
                    text = decode_quoted_printable_string(underscores_to_spaces(words(k)%text), status=st)
                end if

                if (.not. st%ok()) then
                    call message(format_error(st))
                    ok = .false.
                end if
            end if

            if (ok) then
                if (rfc2047_allow_incomplete_encoded_text .and. n > 0) then
                    ! Concatenate text of which the charset is the same.
                    if (rest(n)%cs == cs) then
                        rest(n)%text = rest(n)%text//text
                        cycle
                    end if
                end if

                n = n + 1
                rest(n)%cs = cs
                rest(n)%text = text
            else
                ! Don't decode encoded-word.
                n = n + 1
                rest(n)%cs = ""
                rest(n)%text = words(k)%word
            end if
        end do

        decoded = ""
        has_words = .false.

        do k = n, 1, -1
            ok = len(rest(k)%cs) > 0

            if (ok) then
                call decode_coding_string(rest(k)%text, rest(k)%cs, piece, st)

                if (.not. st%ok()) then
                    call message(format_error(st))
                    ok = .false.
                end if
            end if

            if (.not. ok) then
                piece = rest(k)%text
                if (k > 1) piece = " "//piece

                if (has_words) then
                    if (len(decoded) == 0) then
                        piece = piece//" "
                    else if (decoded(1:1) /= " ") then
                        piece = piece//" "
                    end if
                end if
            end if

            decoded = piece//decoded
            has_words = .true.
        end do
    end function decode_encoded_words

    ! TEXT with each "_" replaced by a space.  Not a port.
    pure function underscores_to_spaces(text) result(replaced)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: replaced
        integer :: k

        replaced = text

        do k = 1, len(text)
            if (text(k:k) == "_") replaced(k:k) = " "
        end do
    end function underscores_to_spaces

    !> Decode MIME-encoded words in region between START and END.  If
    !> ADDRESS_MIME is true, strip backslashes which precede characters
    !> other than `"' and `\' in quoted strings.  Port of
    !> rfc2047.el:rfc2047-decode-region (upstream's
    !> mail-decode-encoded-word-region).
    recursive subroutine decode_rfc2047_region(start, end, status, address_mime)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: address_mime
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(encoded_word_type), allocatable :: words(:), grown(:)
        type(status_type) :: st
        character(len=:), allocatable :: eword, word
        integer(int64) :: b, e, words_end, found, inserted
        integer :: n
        logical :: saved_fold, matched

        if (rfc2047_allow_irregular_q_encoded_words) then
            eword = "["//LF//TAB//" ]*\("//loose_encoded_word_regular_expression()//"\)"
        else
            eword = "["//LF//TAB//" ]*\("//encoded_word_regular_expression()//"\)"
        end if

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)

        body: block
            call narrow_to_region(start, end, status)
            if (.not. status%ok()) exit body

            if (present(address_mime)) then
                if (address_mime) call strip_rfc2047_backslashes_in_quoted_strings(status)
            end if

            b = start
            call goto_char(b)

            ! Look for the encoded-words.
            do
                call re_search_forward(eword, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit body
                if (found == 0) exit
                e = match_beginning(1, st)
                words_end = match_end(0, st)
                allocate (words(0))
                n = 0
                matched = .true.

                do while (matched)
                    allocate (grown(n + 1))
                    grown(1:n) = words
                    call move_alloc(grown, words)
                    n = n + 1
                    ! The charset, the encoding, the encoded text and the
                    ! encoded word.
                    words(n)%charset = match_string(2)
                    words(n)%encoding = achar(after(match_beginning(3, st)))
                    word = match_string(3)
                    words(n)%text = word(3:)
                    words(n)%word = match_string(1)
                    ! Look for the subsequent encoded-words.
                    matched = looking_at(eword, st)

                    if (matched) then
                        words_end = match_end(0, st)
                        call goto_char(words_end)
                    end if
                end do

                ! Replace the encoded-words with the decoded one.
                call delete_region(e, words_end, status)
                if (status%ok()) call insert(decode_encoded_words(words), status)
                deallocate (words)
                if (.not. status%ok()) exit body
                call tidy_decoded_words(e, status)
                if (.not. status%ok()) exit body

                if (buffers(current)%multibyte .and. is_parse_charset_decoding()) then
                    call decode_coding_region(b, e, parse_charset(), inserted, status)
                    if (.not. status%ok()) exit body
                end if

                b = point()
            end do

            if (buffers(current)%multibyte .and. is_parse_charset_decoding()) then
                call decode_coding_region(b, point_max(), parse_charset(), inserted, status)
            end if
        end block body

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
        case_fold_search%value = saved_fold
    end subroutine decode_rfc2047_region

    ! With the decoded words from E to point: remove newlines between
    ! them, though such things essentially must not be there, and quote
    ! them if `rfc2047_quote_decoded_words_containing_tspecials' asks and
    ! there are special characters which might violate RFC 822 (or later).
    ! Point ends after the words.  Port of the part of
    ! rfc2047.el:rfc2047-decode-region after an insertion.
    subroutine tidy_decoded_words(e, status)
        integer(int64), intent(in) :: e
        type(status_type), intent(inout) :: status
        type(restriction_type) :: restriction
        integer(int64) :: found, end

        call save_restriction_begin(restriction)
        call narrow_to_region(e, point(), status)

        body: block
            if (.not. status%ok()) exit body
            call goto_char(e)

            do
                call re_search_forward("["//LF//CR//"]+", found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit body
                if (found == 0) exit
                call replace_match(" ", status)
                if (.not. status%ok()) exit body
            end do

            end = point_max()
            if (rfc2047_quote_decoded_words_containing_tspecials) call quote_decoded_words(e, end, status)
            call goto_char(point_max())
        end block body

        call save_restriction_end(restriction)
    end subroutine tidy_decoded_words

    ! Quote the decoded words from E to END, unless they are quoted
    ! already, if the field they are in is one of addresses (whose name the
    ! first header regular expression of RFC2047_HEADER_ENCODING_ADDRESS_MIME
    ! matches) and they hold special characters.  The buffer is narrowed
    ! to the words.  Port of the quoting in rfc2047.el:rfc2047-decode-region.
    subroutine quote_decoded_words(e, end, status)
        integer(int64), intent(in) :: e, end
        type(status_type), intent(inout) :: status
        type(restriction_type) :: inner
        type(status_type) :: st
        type(handle_type) :: end_marker
        character(len=:), allocatable :: header
        integer(int64) :: start, moved, found, shortage
        integer :: k
        logical :: quoted, is_address

        call initialize_defaults()
        k = findloc(rfc2047_header_encodings%method == RFC2047_HEADER_ENCODING_ADDRESS_MIME, .true., dim=1)
        if (k == 0) return

        if (rfc2047_header_encodings(k)%all_headers) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected character(len=*), got all headers")
            return
        end if

        header = trim(rfc2047_header_encodings(k)%header)
        call save_restriction_begin(inner)
        call widen()
        ! Don't quote words if already quoted.
        is_address = .not. (before(e) == iachar("""") .and. after(end) == iachar(""""))

        if (is_address) then
            call beginning_of_line()

            do while (after() == 32 .or. after() == 9)
                shortage = forward_line(-1_int64)
                if (shortage /= 0) exit
            end do

            is_address = looking_at(header, status)
        end if

        call save_restriction_end(inner)
        if (.not. is_address .or. .not. status%ok()) return
        call goto_char(e)
        moved = skip_chars_forward(" "//TAB, st)
        start = point()
        quoted = after() == iachar("""")
        call goto_char(point_max())
        moved = skip_chars_backward(" "//TAB, st, start)
        if (quoted) quoted = point() > start + 1
        if (quoted) quoted = before() == iachar("""")

        if (quoted) then
            call forward_char(st, -1_int64)
            start = start + 1
        end if

        call copy_marker_at(point(), end_marker, .false., st)
        call goto_char(start)

        do
            call search_forward("""", found, st, bound=position_of(end_marker), noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call forward_char(st, -1_int64)
            moved = skip_chars_backward("\\", st)
            call goto_char(match_beginning(0, st))
            if (modulo(moved, 2_int64) == 0) call insert("\", status)
            call forward_char(st)
        end do

        if (.not. quoted) then
            call goto_char(start)
            call re_search_forward("["//TSPECIALS//"]", found, st, bound=position_of(end_marker), noerror=SEARCH_NOERROR)

            if (found > 0) then
                call goto_char(start)
                call insert("""", status)
                call goto_char(position_of(end_marker))
                call insert("""", status)
            end if
        end if

        call destroy_marker(end_marker, st)
    end subroutine quote_decoded_words

    !> Decode MIME-encoded words in region between START and END.
    !> Backslashes which precede characters other than `"' and `\' in quoted
    !> strings are stripped.  Port of rfc2047.el:rfc2047-decode-address-region
    !> (upstream's mail-decode-encoded-address-region).
    subroutine decode_rfc2047_address_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call decode_rfc2047_region(start, end, status, .true.)
    end subroutine decode_rfc2047_address_region

    !> Decode MIME-encoded STRING (multibyte text) and return the result.
    !> If ADDRESS_MIME is true, strip backslashes which precede characters
    !> other than `"' and `\' in quoted strings.  Port of
    !> rfc2047.el:rfc2047-decode-string (upstream's
    !> mail-decode-encoded-word-string).
    function decode_rfc2047_string(string, address_mime, status) result(decoded)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: address_mime
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: decoded
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: plain
        logical :: address

        address = .false.
        if (present(address_mime)) address = address_mime

        if (index(string, "=?") > 0) then
            call temporary_buffer_begin(old, temporary, .true.)
            call insert(string, st)
            if (st%ok()) call decode_rfc2047_region(point_min(), point_max(), st, address)
            decoded = buffer_string()
            call temporary_buffer_end(old, temporary)
            if (present(status)) status = st
            return
        end if

        decoded = string

        if (address) then
            call temporary_buffer_begin(old, temporary, .true.)
            call insert(string, st)
            if (st%ok()) call strip_rfc2047_backslashes_in_quoted_strings(st)
            decoded = buffer_string()
            call temporary_buffer_end(old, temporary)
        end if

        if (st%ok() .and. is_parse_charset_decoding()) then
            ! Bypass decoding if the string is purely ASCII.
            call detect_coding_string(decoded, names, .true., .true.)

            if (size(names) > 0) then
                if (names(1)%name /= "undecided") then
                    plain = decoded
                    call decode_coding_string(plain, parse_charset(), decoded, st, .true.)
                end if
            end if
        end if

        if (present(status)) status = st
    end function decode_rfc2047_string

    !> Decode MIME-encoded STRING and return the result.  Backslashes which
    !> precede characters other than `"' and `\' in quoted strings are
    !> stripped.  Port of rfc2047.el:rfc2047-decode-address-string
    !> (upstream's mail-decode-encoded-address-string).
    function decode_rfc2047_address_string(string, status) result(decoded)
        character(len=*), intent(in) :: string
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: decoded

        decoded = decode_rfc2047_string(string, .true., status)
    end function decode_rfc2047_address_string

    !> Pad STRING to quartets.  Port of rfc2047.el:rfc2047-pad-base64.
    function pad_rfc2047_base64(string) result(padded)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: padded
        type(status_type) :: st
        integer(int64) :: m

        ! Be more liberal to accept buggy base64 strings.  If
        ! `base64_decode_string' accepts buggy strings, this function could
        ! be the identity.
        padded = string
        if (modulo(len(padded), 4) == 0) return
        m = string_match("=+$", padded, st)
        if (m > 0) padded = padded(1:m - 1)

        select case (modulo(len(padded), 4))
        case (2)
            padded = padded//"=="
        case (3)
            padded = padded//"="
        case default
            ! 0, or 1: an error, don't pad it.
        end select
    end function pad_rfc2047_base64

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_encode_rfc2047_message_header(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call encode_rfc2047_message_header(status)
    end subroutine cmd_encode_rfc2047_message_header

    recursive subroutine cmd_decode_rfc2047_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (status%ok()) call decode_rfc2047_region(b, e, status)
    end subroutine cmd_decode_rfc2047_region

    !> Register the RFC 2047 commands.  Not a port.
    subroutine init_rfc2047()
        commands%encode_rfc2047_message_header%execute => cmd_encode_rfc2047_message_header
        commands%decode_rfc2047_region%execute => cmd_decode_rfc2047_region
    end subroutine init_rfc2047
end module rfc2047_mod
