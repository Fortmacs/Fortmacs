! coding_fm.f90 -- coding systems: decoding and encoding text.
! Ported from GNU Emacs 31.1 src/coding.c, with the fundamental coding
! systems of lisp/international/mule-conf.el and the priorities
! reset-language-environment (mule-cmds.el) gives them.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995-1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
! Copyright (C) 2003
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> Coding systems: how text in files and process output is decoded
!> into buffer text (Emacs's internal encoding) and encoded back.
!>
!> A coding system is named by a string, its upstream symbol's name
!> (`"utf-8"', `"iso-latin-1-dos"'), because the names are data that
!> files and the user give (`-*- coding: utf-8 -*-').  The empty string
!> stands for nil.  Each base coding system without an end-of-line type
!> has the three subsidiaries NAME-unix, NAME-dos and NAME-mac, as
!> upstream makes them.
!>
!> Defined, as mule-conf.el and coding.c define them: `undecided',
!> `prefer-utf-8', `no-conversion' (alias `binary'), `raw-text',
!> `no-conversion-multibyte', `iso-latin-1' (aliases `iso-8859-1',
!> `latin-1'), `us-ascii' (aliases `ascii', `iso-safe'), `utf-8'
!> (aliases `mule-utf-8', `cp65001'), `utf-8-with-signature',
!> `utf-8-auto', `utf-8-emacs' (alias `emacs-internal' for its -unix
!> subsidiary), `utf-16', `utf-16le', `utf-16be',
!> `utf-16le-with-signature' (alias `utf-16-le') and
!> `utf-16be-with-signature' (alias `utf-16-be'), plus the aliases
!> `unix', `dos' and `mac' of `undecided''s subsidiaries.  The
!> priorities are those of a UTF-8 locale in the English language
!> environment: utf-8, iso-latin-1, raw-text, then the UTF-8 with BOM
!> and UTF-16 ones.
!>
!> Deferred: the ISO-2022 family (iso-2022-7bit, iso-2022-jp, ctext and
!> their detection), Shift-JIS, Big5, emacs-mule, CCL coding systems,
!> UTF-7, the other charset-based coding systems (the other ISO-8859
!> parts, Windows code pages, KOI8, Mac Roman and the charset maps they
!> need), so their categories reject everything during detection;
!> language environments (`set_language_environment' and the locale
!> decides nothing: the environment is English with a UTF-8 locale);
!> translation tables; compositions and charset annotations; the
!> `:post-read-conversion' and `:pre-write-conversion' functions;
!> `define_coding_system' for user-defined coding systems;
!> `coding_system_put'; `network_coding_system_alist' and
!> `process_coding_system_alist'; and the functions in
!> `file_coding_system_alist' (xml, po and latexenc detection).
module coding_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_CODING_SYSTEM_ERROR, FM_GENERIC_ERROR, FM_ARGS_OUT_OF_RANGE
    use character_fm, only: char_string, string_char_and_length, char_byte8_p, byte8_to_char, char_to_byte8, &
                            multibyte_length, MAX_MULTIBYTE_LENGTH, MAX_CHAR, MAX_UNICODE_CHAR, MAX_5_BYTE_CHAR
    use character_fm, only: str_to_multibyte
    use data_fm, only: logical_setting, string_setting
    use buffer_fm, only: buffers, cur, buf_bytes, temp_set_point_both
    use marker_fm, only: char_to_byte
    use insdel_fm, only: insert, del_range
    use characters_fm, only: characters_preferred_coding_systems
    use charset_fm, only: CHARSET_NAME_LEN, charset_id_of, charset_name_of, charset_decode_id, charset_encode_id, &
                          charset_dimension_of, charset_code_space_of, charset_ascii_compatible_of, char_charset_among
    use charset_fm, only: iso_2022_charset_ids, iso_charset_id, charset_iso_final_of, charset_iso_revision_of, &
                          charset_iso_chars_96_of, charset_ascii, charset_jisx0201_roman, charset_jisx0208_1978, &
                          charset_jisx0208
    use charset_fm, only: emacs_mule_charset_ids, emacs_mule_charset_of, emacs_mule_bytes_of, charset_emacs_mule_id_of, &
                          charset_eight_bit
    implicit none(type, external)
    private

    public :: define_charset_coding_system, define_iso_2022_coding_system, define_language_environments
    public :: set_terminal_coding_system_internal, terminal_coding_system, set_keyboard_coding_system_internal, &
              keyboard_coding_system
    public :: syms_of_coding, coding_system_p, check_coding_system, coding_system_id, coding_system_name, &
              coding_system_eol_type, coding_system_subsidiaries, coding_system_base, coding_system_type, &
              coding_system_mnemonic, coding_system_doc_string, coding_system_aliases, coding_system_equal, &
              coding_system_list, coding_system_priority_list, set_coding_system_priority, coding_system_category, &
              coding_system_for_unibyte_p, coding_system_ascii_compatible_p, coding_system_mime_charset, &
              coding_system_charset_list, coding_system_bom, coding_system_endian, coding_system_prefer_utf_8_p, &
              coding_system_change_eol_conversion, coding_system_change_text_conversion, &
              coding_system_eol_type_mnemonic, coding_inherit_eol_type, raw_text_coding_system, &
              detect_coding_bytes, decode_coding_bytes, encode_coding_text, char_encodable_p, &
              check_coding_systems_text, find_coding_systems_text, define_coding_system_alias, &
              find_operation_coding_system, coding_system_list_entry_type, unencodable_type, decode_coding_string, &
              encode_coding_string, detect_coding_string, detect_coding_region, decode_coding_region, encode_coding_region

    !> Coding types (the `:coding-type' attribute).
    integer, parameter, public :: CODING_UNDECIDED = 0, CODING_UTF_8 = 1, CODING_UTF_16 = 2, &
                                  CODING_CHARSET = 3, CODING_RAW_TEXT = 4, CODING_SJIS = 5, CODING_BIG5 = 6, &
                                  CODING_ISO_2022 = 7, CODING_EMACS_MULE = 8
    !> End-of-line types: `coding_system_eol_type' returns one of the
    !> first three, or EOL_UNDECIDED for a coding system that has
    !> subsidiaries (upstream's vector).
    integer, parameter, public :: EOL_UNIX = 0, EOL_DOS = 1, EOL_MAC = 2, EOL_UNDECIDED = -1
    !> BOM handling of UTF coding systems (the `:bom' attribute: nil, t
    !> or a cons of the two coding systems to choose from).
    integer, parameter, public :: BOM_NONE = 0, BOM_WITH = 1, BOM_DETECT = 2

    ! The charsets a coding system other than one of type `charset' can
    ! encode (its `:charset-list'); those of type `charset' list theirs.
    integer, parameter :: CS_ASCII = 1, CS_UNICODE = 3, CS_EMACS = 4, CS_ANY = 5, CS_LIST = 6
    ! The most charsets one first byte can start in a coding system of
    ! type `charset'.
    integer, parameter :: MAX_VALIDS = 8

    ! The `:flags' of an ISO-2022 coding system, by the bits of
    ! mule.el's coding-system-iso-2022-flags (CODING_ISO_FLAG_XXX).
    integer, parameter :: ISO_LONG_FORM = int(z'0001'), ISO_RESET_AT_EOL = int(z'0002'), &
                          ISO_RESET_AT_CNTL = int(z'0004'), ISO_SEVEN_BITS = int(z'0008'), &
                          ISO_LOCKING_SHIFT = int(z'0010'), ISO_SINGLE_SHIFT = int(z'0020'), &
                          ISO_DESIGNATION = int(z'0040'), ISO_REVISION = int(z'0080'), ISO_DIRECTION = int(z'0100'), &
                          ISO_INIT_AT_BOL = int(z'0200'), ISO_DESIGNATE_AT_BOL = int(z'0400'), ISO_SAFE = int(z'0800'), &
                          ISO_LATIN_EXTRA = int(z'1000'), ISO_COMPOSITION = int(z'2000'), ISO_USE_ROMAN = int(z'8000'), &
                          ISO_USE_OLDJIS = int(z'10000'), ISO_LEVEL_4 = int(z'20000'), ISO_FULL_SUPPORT = int(z'100000')
    character(len=*), parameter :: ISO_2022_FLAG_NAMES(0:17) = [character(len=16) :: &
                                                                "long-form", "ascii-at-eol", "ascii-at-cntl", "7-bit", &
                                                                "locking-shift", "single-shift", "designation", "revision", &
                                                                "direction", "init-at-bol", "designate-at-bol", "safe", &
                                                                "latin-extra", "composition", "euc-tw-shift", "use-roman", &
                                                                "use-oldjis", "8-bit-level-4"]
    integer, parameter :: ISO_CODE_SO = 14, ISO_CODE_SI = 15, ISO_CODE_SS2_7 = 25, ISO_CODE_ESC = 27, &
                          ISO_CODE_SS2 = int(z'8E'), ISO_CODE_SS3 = int(z'8F'), ISO_CODE_CSI = int(z'9B')

    ! Coding categories, in coding.c's order.
    integer, parameter :: CAT_ISO_7 = 0, CAT_ISO_7_TIGHT = 1, CAT_ISO_8_1 = 2, CAT_ISO_8_2 = 3, &
                          CAT_ISO_7_ELSE = 4, CAT_ISO_8_ELSE = 5, CAT_UTF_8_AUTO = 6, CAT_UTF_8_NOSIG = 7, &
                          CAT_UTF_8_SIG = 8, CAT_UTF_16_AUTO = 9, CAT_UTF_16_BE = 10, CAT_UTF_16_LE = 11, &
                          CAT_UTF_16_BE_NOSIG = 12, CAT_UTF_16_LE_NOSIG = 13, CAT_CHARSET = 14, CAT_SJIS = 15, &
                          CAT_BIG5 = 16, CAT_CCL = 17, CAT_EMACS_MULE = 18, CAT_RAW_TEXT = 19, &
                          CAT_UNDECIDED = 20, NCAT = 21
    integer, parameter :: MASK_ANY = 2**CAT_RAW_TEXT - 1
    integer, parameter :: MASK_UTF_8 = 2**CAT_UTF_8_AUTO + 2**CAT_UTF_8_NOSIG + 2**CAT_UTF_8_SIG
    integer, parameter :: MASK_UTF_16 = 2**CAT_UTF_16_AUTO + 2**CAT_UTF_16_BE + 2**CAT_UTF_16_LE + &
                          2**CAT_UTF_16_BE_NOSIG + 2**CAT_UTF_16_LE_NOSIG
    integer, parameter :: MASK_ISO_7BIT = 2**CAT_ISO_7 + 2**CAT_ISO_7_TIGHT, &
                          MASK_ISO_8BIT = 2**CAT_ISO_8_1 + 2**CAT_ISO_8_2, &
                          MASK_ISO_ELSE = 2**CAT_ISO_7_ELSE + 2**CAT_ISO_8_ELSE, &
                          MASK_ISO = MASK_ISO_7BIT + MASK_ISO_8BIT + MASK_ISO_ELSE, &
                          MASK_ISO_ESCAPE = 2**CAT_ISO_7 + 2**CAT_ISO_7_TIGHT + 2**CAT_ISO_7_ELSE + 2**CAT_ISO_8_ELSE
    character(len=*), parameter :: CATEGORY_NAMES(0:NCAT - 1) = [character(len=31) :: &
                                                                 "coding-category-iso-7", "coding-category-iso-7-tight", &
                                                                 "coding-category-iso-8-1", "coding-category-iso-8-2", &
                                                                 "coding-category-iso-7-else", "coding-category-iso-8-else", &
                                                                 "coding-category-utf-8-auto", "coding-category-utf-8", &
                                                                 "coding-category-utf-8-sig", "coding-category-utf-16-auto", &
                                                                 "coding-category-utf-16-be", "coding-category-utf-16-le", &
                                                                 "coding-category-utf-16-be-nosig", &
                                                                 "coding-category-utf-16-le-nosig", "coding-category-charset", &
                                                                 "coding-category-sjis", "coding-category-big5", &
                                                                 "coding-category-ccl", "coding-category-emacs-mule", &
                                                                 "coding-category-raw-text", "coding-category-undecided"]

    integer, parameter :: EOL_SEEN_NONE = 0, EOL_SEEN_LF = 1, EOL_SEEN_CR = 2, EOL_SEEN_CRLF = 4
    integer, parameter :: MAX_EOL_CHECK_COUNT = 3
    integer, parameter :: LF = 10, CR = 13

    !> A coding system (coding.c's spec vector and attribute vector).
    type :: coding_type
        character(len=:), allocatable :: name, doc, mime_charset
        !> The base coding system (itself for a base).
        integer :: base = 0
        integer :: ctype = CODING_UNDECIDED, category = CAT_UNDECIDED
        integer :: mnemonic = iachar("-")
        integer :: eol = EOL_UNDECIDED
        !> The -unix, -dos and -mac subsidiaries of a coding system whose
        !> end-of-line type is undecided.
        integer :: subsidiaries(0:2) = 0
        integer :: bom = BOM_NONE
        !> For BOM_DETECT: the coding systems with and without a BOM.
        character(len=:), allocatable :: bom_with, bom_without
        logical :: big_endian = .true.
        integer :: charset = CS_ASCII
        !> For type `charset': the ids of its charsets, and for each first
        !> byte the charsets it can start, by dimension (coding.c's
        !> coding_attr_charset_valids).
        integer, allocatable :: charset_ids(:)
        character(len=CHARSET_NAME_LEN), allocatable :: charset_names(:)
        integer :: nvalids(0:255) = 0
        integer :: valids(MAX_VALIDS, 0:255) = 0
        !> For type `iso-2022': its flags, the charsets initially
        !> designated to G0..G3 (0 for none), the registers of the
        !> charsets of 94 and 96 characters (4 for none), the charsets that
        !> request a register, and whether it supports all ISO-2022
        !> charsets (`:charset-list' `iso-2022').
        integer :: iso_flags = 0
        integer :: iso_initial(0:3) = 0
        integer :: iso_reg94 = 4, iso_reg96 = 4
        integer, allocatable :: iso_request_ids(:), iso_request_regs(:)
        logical :: ascii_compatible = .false., for_unibyte = .false., prefer_utf_8 = .false.
        integer :: default_char = 32
    end type coding_type

    !> One entry of `coding_system_list': a name and whether it is an
    !> alias.
    type :: coding_system_list_entry_type
        character(len=:), allocatable :: name
    end type coding_system_list_entry_type

    !> Characters a coding system cannot encode: the coding system and the
    !> positions (`check_coding_systems_text').
    type :: unencodable_type
        character(len=:), allocatable :: coding
        integer(int64), allocatable :: positions(:)
    end type unencodable_type

    type :: alias_type
        character(len=:), allocatable :: name
        integer :: id = 0
    end type alias_type

    !> An entry of `file_coding_system_alist': a regexp matching file
    !> names, and the coding systems for decoding and for encoding ("" for
    !> nil).
    type, public :: file_coding_type
        character(len=:), allocatable :: pattern, decoding, encoding
    end type file_coding_type

    type(coding_type), allocatable :: codings(:)
    integer :: ncodings = 0
    !> Every name of every coding system, in order of definition (the
    !> name first, as Vcoding_system_alist and coding-system-aliases).
    type(alias_type), allocatable :: aliases(:)
    integer :: naliases = 0
    !> The coding system of each category (coding_categories), 0 if none.
    integer :: category_id(0:NCAT - 1) = 0
    !> The categories by priority (coding_priorities).
    integer :: priorities(0:NCAT - 1) = 0

    !> Specify the coding system for read operations.
    !> It is useful to bind this variable temporarily, but do not set it globally.
    !> If the value is a coding system, it is used for decoding on read operation.
    !> If not, an appropriate element is used from one of the coding system alists.
    !> There are three such tables: `file_coding_system_alist',
    !> `process_coding_system_alist', and `network_coding_system_alist'.
    !> Port of coding.c coding-system-for-read (unallocated is nil).
    type(string_setting), public :: coding_system_for_read = string_setting(name="coding_system_for_read")

    !> Specify the coding system for write operations.
    !> Programs bind this variable temporarily, but you should not set it globally.
    !> If the value is a coding system, it is used for encoding of output,
    !> when writing it to a file and when sending it to a file or subprocess.
    !>
    !> If this does not specify a coding system, an appropriate element
    !> is used from one of the coding system alists.
    !> There are three such tables: `file_coding_system_alist',
    !> `process_coding_system_alist', and `network_coding_system_alist'.
    !> For output to files, if the above procedure does not specify a coding system,
    !> the value of `buffer_file_coding_system' is used.  Port of coding.c
    !> coding-system-for-write (unallocated is nil).
    type(string_setting), public :: coding_system_for_write = string_setting(name="coding_system_for_write")

    !> Coding system used in the latest file or process I/O.  Port of
    !> coding.c last-coding-system-used.
    type(string_setting), public :: last_coding_system_used = string_setting(name="last_coding_system_used")

    !> True means always inhibit code conversion of end-of-line format.
    !> See info node `Coding Systems' and info node `Text and Binary' concerning
    !> such conversion.  Port of coding.c inhibit-eol-conversion.
    type(logical_setting), public :: inhibit_eol_conversion = logical_setting(name="inhibit_eol_conversion")

    !> If true, Emacs ignores null bytes on code detection.
    !> By default, Emacs treats it as binary data, and does not attempt to
    !> decode it.  The effect is as if you specified `no-conversion' for
    !> reading that text.
    !>
    !> Set this to true when a regular text happens to include null bytes.
    !> Examples are Index nodes of Info files and null-byte delimited output
    !> from GNU Find and GNU Grep.  Emacs will then ignore the null bytes and
    !> decode text as usual.  Port of coding.c inhibit-null-byte-detection.
    type(logical_setting), public :: inhibit_null_byte_detection = logical_setting(name="inhibit_null_byte_detection")

    !> If true, Emacs ignores ISO-2022 escape sequences during code detection.
    !>
    !> When Emacs reads text, it tries to detect how the text is encoded.
    !> This code detection is sensitive to escape sequences.  If Emacs sees
    !> a valid ISO-2022 escape sequence, it assumes the text is encoded in one
    !> of the ISO2022 encodings, and decodes text by the corresponding coding
    !> system (e.g. `iso-2022-7bit').
    !>
    !> However, there may be a case that you want to read escape sequences in
    !> a file as is.  In such a case, you can set this variable to true.
    !> Then the code detection will ignore any escape sequences, and no text is
    !> detected as encoded in some ISO-2022 encoding.  The result is that all
    !> escape sequences become visible in a buffer.
    !>
    !> The default value is false, and it is strongly recommended not to change
    !> it.  That is because many Emacs Lisp source files that contain
    !> non-ASCII characters are encoded by the coding system `iso-2022-7bit'
    !> in Emacs's distribution, and they won't be decoded correctly on
    !> reading if you suppress escape sequence detection.
    !>
    !> The other way to read escape sequences in a file without decoding is
    !> to explicitly specify some coding system that doesn't use ISO-2022
    !> escape sequence (e.g., `latin-1') on reading by \[universal_coding_system_argument].
    !> Port of coding.c inhibit-iso-escape-detection (the ISO-2022 coding
    !> systems are Deferred, so it has no effect yet).
    type(logical_setting), public :: inhibit_iso_escape_detection = &
                                     logical_setting(name="inhibit_iso_escape_detection")

    !> String displayed in mode line for UNIX-like (LF) end-of-line format.
    !> Port of coding.c eol-mnemonic-unix.
    type(string_setting), public :: eol_mnemonic_unix = string_setting(name="eol_mnemonic_unix")

    !> String displayed in mode line for DOS-like (CRLF) end-of-line format.
    !> Port of coding.c eol-mnemonic-dos.
    type(string_setting), public :: eol_mnemonic_dos = string_setting(name="eol_mnemonic_dos")

    !> String displayed in mode line for MAC-like (CR) end-of-line format.
    !> Port of coding.c eol-mnemonic-mac.
    type(string_setting), public :: eol_mnemonic_mac = string_setting(name="eol_mnemonic_mac")

    !> String displayed in mode line when end-of-line format is not yet determined.
    !> Port of coding.c eol-mnemonic-undecided.
    type(string_setting), public :: eol_mnemonic_undecided = string_setting(name="eol_mnemonic_undecided")

    !> Cons of coding systems used for process I/O by default.
    !> The first element is used for decoding a process output,
    !> the second is used for encoding a text to be sent to a process.
    !> Port of coding.c default-process-coding-system (the value
    !> prefer-coding-system gives it in a UTF-8 locale).
    character(len=40), public :: default_process_coding_system(2) = ["utf-8-unix", "utf-8-unix"]

    !> Alist to decide a coding system to use for a file I/O operation.
    !> Each element has a PATTERN, a regular expression matching a file
    !> name, and the coding systems for decoding and for encoding the
    !> file contents ("" for none).  See also the function
    !> `find_operation_coding_system' and the variable `auto_coding_alist'.
    !> Port of coding.c file-coding-system-alist, with mule-conf.el's value
    !> (without its functions: the xml, po and latexenc detection is
    !> Deferred).
    type(file_coding_type), allocatable, public :: file_coding_system_alist(:)

    !> Coding system to use with system messages.
    !> Potentially also used for decoding keyboard input on X Windows, and is
    !> used for encoding standard output and error streams.  Port of
    !> coding.c locale-coding-system (a UTF-8 locale).
    type(string_setting), public :: locale_coding_system = string_setting(name="locale_coding_system")

    !> Internal use only.
    !> If true, on writing a file, `select_safe_coding_system_function' is
    !> called even if `coding_system_for_write' is set.  The command
    !> `universal_coding_system_argument' sets this variable temporarily.
    !> Port of coding.c coding-system-require-warning.
    type(logical_setting), public :: coding_system_require_warning = &
                                     logical_setting(name="coding_system_require_warning")

    !> Coding system to be used for encoding the buffer contents on saving.
    !> This variable applies to saving the buffer, and also to `write_region'
    !> and other functions that use `write_region'.
    !> It does not apply to sending output to subprocesses, however.
    !>
    !> If this is nil, the buffer is saved without any code conversion
    !> unless some coding system is specified in `file_coding_system_alist'
    !> for the buffer file.
    !>
    !> If the text to be saved cannot be encoded as specified by this variable,
    !> an alternative encoding is selected by `select_safe_coding_system', which see.
    !>
    !> The variable `coding_system_for_write', if set, overrides this variable.
    !>
    !> This variable is never applied to a way of decoding a file while reading it.
    !> Port of buffer.c buffer-file-coding-system (local when set and
    !> permanently local, as buffer.c makes it; its default is what
    !> prefer-coding-system gives it in a UTF-8 locale).
    type(string_setting), target, public :: buffer_file_coding_system = &
                                            string_setting(name="buffer_file_coding_system")

    ! Bytes 0x80..0x9F that may appear in Latin-1 text
    ! (mule-conf.el's latin-extra-code-table).
    integer, parameter :: LATIN_EXTRA(6) = [int(z'91'), int(z'92'), int(z'93'), int(z'94'), int(z'95'), int(z'96')]

    !> What detection learns (struct coding_detection_info).
    type :: detect_info_type
        integer :: checked = 0, found = 0, rejected = 0
    end type detect_info_type

    ! The coding systems of the terminal (there is one): for its output and
    ! for decoding keyboard input (TERMINAL_TERMINAL_CODING and
    ! TERMINAL_KEYBOARD_CODING); unallocated means utf-8-unix, the
    ! startup value in a UTF-8 locale.
    character(len=:), allocatable :: terminal_coding, keyboard_coding

    interface
        !> The `charset' coding systems of chinese.el (in chinese_fm).
        module subroutine syms_of_chinese()
            implicit none(type, external)
        end subroutine syms_of_chinese
        !> The `charset' coding systems of cyrillic.el (in cyrillic_fm).
        module subroutine syms_of_cyrillic()
            implicit none(type, external)
        end subroutine syms_of_cyrillic
        !> The `charset' coding systems of english.el (in english_fm).
        module subroutine syms_of_english()
            implicit none(type, external)
        end subroutine syms_of_english
        !> The `charset' coding systems of european.el (in european_fm).
        module subroutine syms_of_european()
            implicit none(type, external)
        end subroutine syms_of_european
        !> The `charset' coding systems of romanian.el (in romanian_fm).
        module subroutine syms_of_romanian()
            implicit none(type, external)
        end subroutine syms_of_romanian
        !> The `charset' coding systems of greek.el (in greek_fm).
        module subroutine syms_of_greek()
            implicit none(type, external)
        end subroutine syms_of_greek
        !> The `charset' coding systems of hebrew.el (in hebrew_fm).
        module subroutine syms_of_hebrew()
            implicit none(type, external)
        end subroutine syms_of_hebrew
        !> The `charset' coding systems of japanese.el (in japanese_fm).
        module subroutine syms_of_japanese()
            implicit none(type, external)
        end subroutine syms_of_japanese
        !> The `charset' coding systems of korean.el (in korean_fm).
        module subroutine syms_of_korean()
            implicit none(type, external)
        end subroutine syms_of_korean
        !> The `charset' coding systems of lao.el (in lao_fm).
        module subroutine syms_of_lao()
            implicit none(type, external)
        end subroutine syms_of_lao
        !> The `charset' coding systems of thai.el (in thai_fm).
        module subroutine syms_of_thai()
            implicit none(type, external)
        end subroutine syms_of_thai
        !> The `charset' coding systems of tibetan.el (in tibetan_fm).
        module subroutine syms_of_tibetan()
            implicit none(type, external)
        end subroutine syms_of_tibetan
        !> The `charset' coding systems of vietnamese.el (in vietnamese_fm).
        module subroutine syms_of_vietnamese()
            implicit none(type, external)
        end subroutine syms_of_vietnamese
        !> The `charset' coding systems of misc-lang.el (in misc_lang_fm).
        module subroutine syms_of_misc_lang()
            implicit none(type, external)
        end subroutine syms_of_misc_lang
        !> The `charset' coding systems of georgian.el (in georgian_fm).
        module subroutine syms_of_georgian()
            implicit none(type, external)
        end subroutine syms_of_georgian
        !> The language environments of chinese.el (in chinese_fm).
        module subroutine language_info_chinese()
            implicit none(type, external)
        end subroutine language_info_chinese
        !> The language environments of cyrillic.el (in cyrillic_fm).
        module subroutine language_info_cyrillic()
            implicit none(type, external)
        end subroutine language_info_cyrillic
        !> The language environments of indian.el (in indian_fm).
        module subroutine language_info_indian()
            implicit none(type, external)
        end subroutine language_info_indian
        !> The language environments of sinhala.el (in sinhala_fm).
        module subroutine language_info_sinhala()
            implicit none(type, external)
        end subroutine language_info_sinhala
        !> The language environments of english.el (in english_fm).
        module subroutine language_info_english()
            implicit none(type, external)
        end subroutine language_info_english
        !> The language environments of ethiopic.el (in ethiopic_fm).
        module subroutine language_info_ethiopic()
            implicit none(type, external)
        end subroutine language_info_ethiopic
        !> The language environments of european.el (in european_fm).
        module subroutine language_info_european()
            implicit none(type, external)
        end subroutine language_info_european
        !> The language environments of czech.el (in czech_fm).
        module subroutine language_info_czech()
            implicit none(type, external)
        end subroutine language_info_czech
        !> The language environments of slovak.el (in slovak_fm).
        module subroutine language_info_slovak()
            implicit none(type, external)
        end subroutine language_info_slovak
        !> The language environments of romanian.el (in romanian_fm).
        module subroutine language_info_romanian()
            implicit none(type, external)
        end subroutine language_info_romanian
        !> The language environments of greek.el (in greek_fm).
        module subroutine language_info_greek()
            implicit none(type, external)
        end subroutine language_info_greek
        !> The language environments of hebrew.el (in hebrew_fm).
        module subroutine language_info_hebrew()
            implicit none(type, external)
        end subroutine language_info_hebrew
        !> The language environments of japanese.el (in japanese_fm).
        module subroutine language_info_japanese()
            implicit none(type, external)
        end subroutine language_info_japanese
        !> The language environments of korean.el (in korean_fm).
        module subroutine language_info_korean()
            implicit none(type, external)
        end subroutine language_info_korean
        !> The language environments of lao.el (in lao_fm).
        module subroutine language_info_lao()
            implicit none(type, external)
        end subroutine language_info_lao
        !> The language environments of tai-viet.el (in tai_viet_fm).
        module subroutine language_info_tai_viet()
            implicit none(type, external)
        end subroutine language_info_tai_viet
        !> The language environments of thai.el (in thai_fm).
        module subroutine language_info_thai()
            implicit none(type, external)
        end subroutine language_info_thai
        !> The language environments of tibetan.el (in tibetan_fm).
        module subroutine language_info_tibetan()
            implicit none(type, external)
        end subroutine language_info_tibetan
        !> The language environments of vietnamese.el (in vietnamese_fm).
        module subroutine language_info_vietnamese()
            implicit none(type, external)
        end subroutine language_info_vietnamese
        !> The language environments of misc-lang.el (in misc_lang_fm).
        module subroutine language_info_misc_lang()
            implicit none(type, external)
        end subroutine language_info_misc_lang
        !> The language environments of utf-8-lang.el (in utf_8_lang_fm).
        module subroutine language_info_utf_8_lang()
            implicit none(type, external)
        end subroutine language_info_utf_8_lang
        !> The language environments of georgian.el (in georgian_fm).
        module subroutine language_info_georgian()
            implicit none(type, external)
        end subroutine language_info_georgian
        !> The language environments of khmer.el (in khmer_fm).
        module subroutine language_info_khmer()
            implicit none(type, external)
        end subroutine language_info_khmer
        !> The language environments of burmese.el (in burmese_fm).
        module subroutine language_info_burmese()
            implicit none(type, external)
        end subroutine language_info_burmese
        !> The language environments of cham.el (in cham_fm).
        module subroutine language_info_cham()
            implicit none(type, external)
        end subroutine language_info_cham
        !> The language environments of philippine.el (in philippine_fm).
        module subroutine language_info_philippine()
            implicit none(type, external)
        end subroutine language_info_philippine
        !> The language environments of indonesian.el (in indonesian_fm).
        module subroutine language_info_indonesian()
            implicit none(type, external)
        end subroutine language_info_indonesian
    end interface

contains

    !> Define the language environments of the language files, in the
    !> order loadup.el loads them (mule_fm calls this once).
    subroutine define_language_environments()
        call language_info_chinese()
        call language_info_cyrillic()
        call language_info_indian()
        call language_info_sinhala()
        call language_info_english()
        call language_info_ethiopic()
        call language_info_european()
        call language_info_czech()
        call language_info_slovak()
        call language_info_romanian()
        call language_info_greek()
        call language_info_hebrew()
        call language_info_japanese()
        call language_info_korean()
        call language_info_lao()
        call language_info_tai_viet()
        call language_info_thai()
        call language_info_tibetan()
        call language_info_vietnamese()
        call language_info_misc_lang()
        call language_info_utf_8_lang()
        call language_info_georgian()
        call language_info_khmer()
        call language_info_burmese()
        call language_info_cham()
        call language_info_philippine()
        call language_info_indonesian()
    end subroutine define_language_environments

    ! ------------------------------------------------------------------
    ! Defining coding systems
    ! ------------------------------------------------------------------

    !> Define the fundamental coding systems and their priorities.  Port of
    !> coding.c:syms_of_coding (no-conversion, undecided), the coding
    !> systems mule-conf.el defines, and reset-language-environment's
    !> priorities.
    recursive subroutine syms_of_coding()
        integer :: id, k

        if (ncodings > 0) return
        allocate (codings(64), aliases(160))
        if (.not. allocated(eol_mnemonic_unix%value)) eol_mnemonic_unix%value = ":"
        if (.not. allocated(eol_mnemonic_dos%value)) eol_mnemonic_dos%value = "\"
        if (.not. allocated(eol_mnemonic_mac%value)) eol_mnemonic_mac%value = "/"
        if (.not. allocated(eol_mnemonic_undecided%value)) eol_mnemonic_undecided%value = ":"
        if (.not. allocated(locale_coding_system%value)) locale_coding_system%value = "utf-8-unix"
        if (.not. allocated(buffer_file_coding_system%value)) buffer_file_coding_system%value = "utf-8-unix"
        id = define("no-conversion", CODING_RAW_TEXT, iachar("="), CAT_RAW_TEXT, eol=EOL_UNIX, ascii_compatible=.true., &
                    for_unibyte=.true., default_char=0, charset=CS_ANY, &
                    doc="Do no conversion."//achar(10)//achar(10)// &
                    "When you visit a file with this coding, the file is read into a"//achar(10)// &
                    "unibyte buffer as is, thus each byte of a file is treated as a"//achar(10)//"character.")
        id = define("undecided", CODING_UNDECIDED, iachar("-"), CAT_UNDECIDED, ascii_compatible=.true., default_char=0, &
                    charset=CS_ASCII, doc="No conversion on encoding, automatic conversion on decoding.")
        ! mule-conf.el.
        call define_coding_system_alias("binary", "no-conversion")
        call define_coding_system_alias("unix", "undecided-unix")
        call define_coding_system_alias("dos", "undecided-dos")
        call define_coding_system_alias("mac", "undecided-mac")
        id = define("prefer-utf-8", CODING_UNDECIDED, iachar("-"), CAT_UNDECIDED, ascii_compatible=.true., &
                    charset=CS_EMACS, prefer_utf_8=.true., &
                    doc="Like `undecided' but prefer UTF-8 when appropriate."//achar(10)// &
                    "On decoding, if the source contains 8-bit codes and they all"//achar(10)// &
                    "are valid UTF-8 sequences, detect the source as UTF-8 encoding"//achar(10)// &
                    "regardless of the coding priority."//achar(10)// &
                    "On encoding, if the source contains non-ASCII characters, encode them"//achar(10)//"by UTF-8.")
        id = define("raw-text", CODING_RAW_TEXT, iachar("t"), CAT_RAW_TEXT, ascii_compatible=.true., for_unibyte=.true., &
                    charset=CS_ANY, &
                    doc="Raw text, which means text contains random 8-bit codes."//achar(10)// &
                    "Encoding text with this coding system produces the actual byte"//achar(10)// &
                    "sequence of the text in buffers and strings.  An exception is made for"//achar(10)// &
                    "characters from the `eight-bit' character set.  Each of them is encoded"//achar(10)// &
                    "into a single byte."//achar(10)//achar(10)// &
                    "When you visit a file with this coding, the file is read into a"//achar(10)// &
                    "unibyte buffer as is (except for EOL format), thus each byte of a file"//achar(10)// &
                    "is treated as a character.")
        id = define("no-conversion-multibyte", CODING_RAW_TEXT, iachar("="), CAT_RAW_TEXT, eol=EOL_UNIX, &
                    ascii_compatible=.true., charset=CS_ANY, &
                    doc="Like `no-conversion' but don't read a file into a unibyte buffer.")
        id = define("iso-latin-1", CODING_CHARSET, iachar("1"), CAT_CHARSET, &
                    charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-1"], mime_charset="iso-8859-1", &
                    doc="ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1).")
        call define_coding_system_alias("iso-8859-1", "iso-latin-1")
        call define_coding_system_alias("latin-1", "iso-latin-1")
        id = define("emacs-mule", CODING_EMACS_MULE, iachar("M"), CAT_EMACS_MULE, ascii_compatible=.true., &
                    doc="Emacs 21 internal format used in buffer and string.")
        codings(id)%charset = CS_LIST
        codings(id)%charset_names = [character(len=CHARSET_NAME_LEN) :: "emacs-mule"]
        allocate (codings(id)%charset_ids(0))

        do k = 0, 2
            codings(codings(id)%subsidiaries(k))%charset = CS_LIST
            codings(codings(id)%subsidiaries(k))%charset_names = codings(id)%charset_names
            allocate (codings(codings(id)%subsidiaries(k))%charset_ids(0))
        end do

        id = define("utf-8", CODING_UTF_8, iachar("U"), CAT_UTF_8_NOSIG, ascii_compatible=.true., charset=CS_UNICODE, &
                    mime_charset="utf-8", doc="UTF-8 (no signature (BOM))")
        id = define("utf-8-with-signature", CODING_UTF_8, iachar("U"), CAT_UTF_8_SIG, bom=BOM_WITH, charset=CS_UNICODE, &
                    doc="UTF-8 (with signature (BOM))")
        id = define("utf-8-auto", CODING_UTF_8, iachar("U"), CAT_UTF_8_AUTO, bom=BOM_DETECT, charset=CS_UNICODE, &
                    bom_with="utf-8-with-signature", bom_without="utf-8", doc="UTF-8 (auto-detect signature (BOM))")
        call define_coding_system_alias("mule-utf-8", "utf-8")
        call define_coding_system_alias("cp65001", "utf-8")
        id = define("utf-8-emacs", CODING_UTF_8, iachar("U"), CAT_UTF_8_NOSIG, ascii_compatible=.true., charset=CS_EMACS, &
                    doc="Support for all Emacs characters (including non-Unicode characters).")
        call define_coding_system_alias("emacs-internal", "utf-8-emacs-unix")
        id = define("utf-16le", CODING_UTF_16, iachar("U"), CAT_UTF_16_LE_NOSIG, big_endian=.false., charset=CS_UNICODE, &
                    mime_charset="utf-16le", doc="UTF-16LE (little endian, no signature (BOM)).")
        id = define("utf-16be", CODING_UTF_16, iachar("U"), CAT_UTF_16_BE_NOSIG, charset=CS_UNICODE, &
                    mime_charset="utf-16be", doc="UTF-16BE (big endian, no signature (BOM)).")
        id = define("utf-16le-with-signature", CODING_UTF_16, iachar("U"), CAT_UTF_16_LE, bom=BOM_WITH, &
                    big_endian=.false., charset=CS_UNICODE, mime_charset="utf-16", &
                    doc="UTF-16 (little endian, with signature (BOM)).")
        id = define("utf-16be-with-signature", CODING_UTF_16, iachar("U"), CAT_UTF_16_BE, bom=BOM_WITH, &
                    charset=CS_UNICODE, mime_charset="utf-16", doc="UTF-16 (big endian, with signature (BOM)).")
        id = define("utf-16", CODING_UTF_16, iachar("U"), CAT_UTF_16_AUTO, bom=BOM_DETECT, charset=CS_UNICODE, &
                    bom_with="utf-16le-with-signature", bom_without="utf-16be-with-signature", mime_charset="utf-16", &
                    doc="UTF-16 (detect endian on decoding, use big endian on encoding with BOM).")
        call define_coding_system_alias("utf-16-le", "utf-16le-with-signature")
        call define_coding_system_alias("utf-16-be", "utf-16be-with-signature")
        call define_iso_2022_coding_system("iso-2022-7bit", iachar("J"), &
                                           doc="ISO 2022 based 7-bit encoding using only G0.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charsets=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           reg_usage=[0, 0], &
                       flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", "composition"])
        call define_iso_2022_coding_system("iso-2022-7bit-ss2", iachar("$"), &
                                           doc="ISO 2022 based 7-bit encoding using SS2 for 96-charset.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charsets=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           reg_usage=[0, 2], &
                                     flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "single-shift", "composition"])
        call define_iso_2022_coding_system("iso-2022-7bit-lock", iachar("&"), &
                                           doc="ISO-2022 coding system using Locking-Shift for 96-charset.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charsets=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           reg_usage=[0, 1], &
                                           flags=[character(len=16) :: "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "locking-shift", "composition"])
        call define_coding_system_alias("iso-2022-int-1", "iso-2022-7bit-lock")
        call define_iso_2022_coding_system("iso-2022-7bit-lock-ss2", iachar("i"), &
                                           doc="Mixture of ISO-2022-JP, ISO-2022-KR, and ISO-2022-CN.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                  request_charsets=[character(len=CHARSET_NAME_LEN) :: "chinese-cns11643-7", "chinese-cns11643-6", &
                                           "chinese-cns11643-5", "chinese-cns11643-4", "chinese-cns11643-3", "chinese-cns11643-2", &
                                                             "chinese-cns11643-1", "chinese-gb2312", "korean-ksc5601", "ascii"], &
                                           request_registers=[integer :: 3, 3, 3, 3, 3, 2, 1, 1, 1, 0], &
                                           reg_usage=[0, 1], &
                                   flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "locking-shift", &
                                                  "single-shift", "init-bol"], &
                                           charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "japanese-jisx0208", &
                                                   "japanese-jisx0208-1978", "latin-jisx0201", "korean-ksc5601", "chinese-gb2312", &
                                           "chinese-cns11643-1", "chinese-cns11643-2", "chinese-cns11643-3", "chinese-cns11643-4", &
                                                     "chinese-cns11643-5", "chinese-cns11643-6", "chinese-cns11643-7"])
        call define_coding_system_alias("iso-2022-cjk", "iso-2022-7bit-lock-ss2")
        call define_iso_2022_coding_system("iso-2022-8bit-ss2", iachar("@"), &
                                           doc="ISO 2022 based 8-bit encoding using SS2 for 96-charset.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charsets=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           reg_usage=[0, 2], &
                         flags=[character(len=16) :: "ascii-at-eol", "ascii-at-cntl", "designation", "single-shift", "composition"])
        call define_iso_2022_coding_system("compound-text", iachar("x"), &
                                           doc="Compound text based generic encoding."//achar(10)//"This coding system is " &
                                           //"an extension of X's ""Compound Text Encoding""."//achar(10)//"It encodes man" &
                                           //"y characters using the normal ISO-2022 designation sequences" &
                                           //","//achar(10)//"but it doesn't support extended segments of CTEXT.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-iso8859-1", "", ""], &
                            request_charsets=[character(len=CHARSET_NAME_LEN) :: "katakana-jisx0201", "latin-iso8859-1", "ascii"], &
                                           request_registers=[integer :: 1, 1, 0], &
                                           reg_usage=[0, 1], &
                                          flags=[character(len=16) :: "ascii-at-eol", "ascii-at-cntl", "long-form", "designation", &
                                                  "locking-shift", "single-shift", "composition"], &
                                           mime_charset="x-ctext")
        call define_coding_system_alias("x-ctext", "compound-text")
        call define_coding_system_alias("ctext", "compound-text")
        call define_iso_2022_coding_system("ctext-no-compositions", iachar("x"), &
                                        doc="Compound text based generic encoding."//achar(10)//achar(10)//"Like `compound-text'," &
                                           //" but does not produce escape sequences for compositions.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-iso8859-1", "", ""], &
                            request_charsets=[character(len=CHARSET_NAME_LEN) :: "katakana-jisx0201", "latin-iso8859-1", "ascii"], &
                                           request_registers=[integer :: 1, 1, 0], &
                                           reg_usage=[0, 1], &
                       flags=[character(len=16) :: "ascii-at-eol", "ascii-at-cntl", "designation", "locking-shift", "single-shift"])
        id = define("us-ascii", CODING_CHARSET, iachar("-"), CAT_CHARSET, &
                    charsets=[character(len=CHARSET_NAME_LEN) :: "ascii"], default_char=iachar("?"), mime_charset="us-ascii", &
                    doc="Encode ASCII as-is and encode non-ASCII characters to `?'.")
        call define_coding_system_alias("iso-safe", "us-ascii")
        call define_coding_system_alias("ascii", "us-ascii")
        ! The language files, in the order loadup.el loads them.
        call syms_of_chinese()
        call syms_of_cyrillic()
        call syms_of_english()
        call syms_of_european()
        call syms_of_romanian()
        call syms_of_greek()
        call syms_of_hebrew()
        call syms_of_japanese()
        call syms_of_korean()
        call syms_of_lao()
        call syms_of_thai()
        call syms_of_tibetan()
        call syms_of_vietnamese()
        call syms_of_misc_lang()
        call syms_of_georgian()
        call characters_preferred_coding_systems()

        ! mule-conf.el's file-coding-system-alist, without its functions.
        if (.not. allocated(file_coding_system_alist)) then
            allocate (file_coding_system_alist(6))
            file_coding_system_alist(1) = file_coding_type("\.elc\'", "utf-8-emacs", "utf-8-emacs")
            file_coding_system_alist(2) = file_coding_type("\.el\'", "prefer-utf-8", "prefer-utf-8")
            file_coding_system_alist(3) = file_coding_type("\.utf\(-8\)?\'", "utf-8", "utf-8")
            file_coding_system_alist(4) = file_coding_type("\(\`\|/\)loaddefs.el\'", "raw-text", "raw-text-unix")
            file_coding_system_alist(5) = file_coding_type("\.tar\'", "no-conversion", "no-conversion")
            file_coding_system_alist(6) = file_coding_type("", "undecided", "")
        end if

        ! reset-language-environment: utf-8, iso-2022-7bit, iso-latin-1,
        ! iso-2022-7bit-lock, iso-2022-8bit-ss2, emacs-mule, raw-text
        ! first, the other categories in coding.c's order.
        priorities = [CAT_UTF_8_NOSIG, CAT_ISO_7, CAT_CHARSET, CAT_ISO_7_ELSE, CAT_ISO_8_ELSE, CAT_EMACS_MULE, &
                      CAT_RAW_TEXT, CAT_ISO_7_TIGHT, CAT_ISO_8_1, CAT_ISO_8_2, CAT_UTF_8_AUTO, CAT_UTF_8_SIG, &
                      CAT_UTF_16_AUTO, CAT_UTF_16_BE, CAT_UTF_16_LE, CAT_UTF_16_BE_NOSIG, CAT_UTF_16_LE_NOSIG, &
                      CAT_SJIS, CAT_BIG5, CAT_CCL, CAT_UNDECIDED]
        category_id(CAT_UTF_8_NOSIG) = coding_system_id("utf-8")
        category_id(CAT_ISO_7) = coding_system_id("iso-2022-7bit")
        category_id(CAT_CHARSET) = coding_system_id("iso-latin-1")
        category_id(CAT_ISO_7_ELSE) = coding_system_id("iso-2022-7bit-lock")
        category_id(CAT_ISO_8_ELSE) = coding_system_id("iso-2022-8bit-ss2")
        category_id(CAT_EMACS_MULE) = coding_system_id("emacs-mule")
        category_id(CAT_RAW_TEXT) = coding_system_id("raw-text")
    end subroutine syms_of_coding

    !> Define a base coding system and, unless EOL is given, its three
    !> subsidiaries; return its id.  The first coding system of a
    !> category becomes the category's.  Port of
    !> coding.c:Fdefine_coding_system_internal.
    recursive integer function define(name, ctype, mnemonic, category, eol, ascii_compatible, for_unibyte, prefer_utf_8, &
                                      default_char, charset, bom, bom_with, bom_without, big_endian, mime_charset, doc, &
                                      charsets) result(id)
        character(len=*), intent(in) :: name
        integer, intent(in) :: ctype, mnemonic, category
        integer, intent(in), optional :: eol, default_char, charset, bom
        logical, intent(in), optional :: ascii_compatible, for_unibyte, prefer_utf_8, big_endian
        character(len=*), intent(in), optional :: bom_with, bom_without, mime_charset, doc
        character(len=*), intent(in), optional :: charsets(:)
        type(coding_type) :: c
        character(len=*), parameter :: SUFFIXES(0:2) = [character(len=5) :: "-unix", "-dos", "-mac"]
        integer :: i, sub

        c%name = name
        c%ctype = ctype
        c%mnemonic = mnemonic
        c%category = category
        if (present(eol)) c%eol = eol
        if (present(ascii_compatible)) c%ascii_compatible = ascii_compatible
        if (present(for_unibyte)) c%for_unibyte = for_unibyte
        if (present(prefer_utf_8)) c%prefer_utf_8 = prefer_utf_8
        if (present(default_char)) c%default_char = default_char
        if (present(charset)) c%charset = charset
        if (present(bom)) c%bom = bom
        if (present(bom_with)) c%bom_with = bom_with
        if (present(bom_without)) c%bom_without = bom_without
        if (present(big_endian)) c%big_endian = big_endian
        c%mime_charset = ""
        if (present(mime_charset)) c%mime_charset = mime_charset
        c%doc = ""
        if (present(doc)) c%doc = doc
        if (present(charsets)) call setup_charset_valids(c, charsets)
        id = add_coding(c)
        codings(id)%base = id
        call add_alias(name, id)

        if (c%eol == EOL_UNDECIDED) then
            do i = 0, 2
                c%name = name//trim(SUFFIXES(i))
                c%eol = i
                sub = add_coding(c)
                codings(sub)%base = id
                codings(id)%subsidiaries(i) = sub
                call add_alias(c%name, sub)
            end do
        end if

        if (category_id(category) == 0) category_id(category) = id
    end function define

    ! The charsets of a coding system of type `charset' and the charsets
    ! each first byte can start, smaller dimensions first; it is ASCII
    ! compatible when one of them is.  Port of the `charset' part of
    ! coding.c:Fdefine_coding_system_internal.
    subroutine setup_charset_valids(c, charsets, with_valids)
        type(coding_type), intent(inout) :: c
        character(len=*), intent(in) :: charsets(:)
        logical, intent(in), optional :: with_valids
        integer :: k, b, j, m, cs, dim, idx
        logical :: valids

        c%charset = CS_LIST
        c%charset_names = charsets
        allocate (c%charset_ids(size(charsets)))
        c%nvalids = 0
        c%valids = 0
        valids = .true.
        if (present(with_valids)) valids = with_valids

        do k = 1, size(charsets)
            cs = charset_id_of(trim(charsets(k)))
            c%charset_ids(k) = cs
            if (cs == 0 .or. .not. valids) cycle
            dim = charset_dimension_of(cs)
            idx = (dim - 1)*4
            if (charset_ascii_compatible_of(cs)) c%ascii_compatible = .true.

            do b = charset_code_space_of(cs, idx), charset_code_space_of(cs, idx + 1)
                ! Insert CS before the first charset of a larger dimension.
                m = c%nvalids(b) + 1

                do j = 1, c%nvalids(b)
                    if (dim < charset_dimension_of(c%valids(j, b))) then
                        m = j
                        exit
                    end if
                end do

                c%valids(m + 1:c%nvalids(b) + 1, b) = c%valids(m:c%nvalids(b), b)
                c%valids(m, b) = cs
                c%nvalids(b) = c%nvalids(b) + 1
            end do
        end do
    end subroutine setup_charset_valids

    !> Define NAME as a coding system of type `charset' (or CODING_TYPE
    !> "shift-jis" or "big5") for the charsets CHARSETS, with mnemonic
    !> character MNEMONIC (and its subsidiaries).  Port of
    !> mule.el:define-coding-system for these coding types.
    subroutine define_charset_coding_system(name, mnemonic, charsets, doc, mime_charset, default_char, coding_type)
        character(len=*), intent(in) :: name
        integer, intent(in) :: mnemonic
        character(len=*), intent(in) :: charsets(:), doc
        character(len=*), intent(in), optional :: mime_charset, coding_type
        integer, intent(in), optional :: default_char
        integer :: id, ctype, category

        ctype = CODING_CHARSET
        category = CAT_CHARSET

        if (present(coding_type)) then
            select case (coding_type)
            case ("shift-jis")
                ctype = CODING_SJIS
                category = CAT_SJIS
            case ("big5")
                ctype = CODING_BIG5
                category = CAT_BIG5
            case default
            end select
        end if

        id = define(name, ctype, mnemonic, category, charsets=charsets, doc=doc, mime_charset=mime_charset, &
                    default_char=default_char)
    end subroutine define_charset_coding_system

    !> Define NAME as a coding system of type `iso-2022' with mnemonic
    !> character MNEMONIC (and its subsidiaries).  CHARSETS is its
    !> `:charset-list' (omitted for `iso-2022': all ISO-2022 charsets).
    !> Its `:designation' is given as mule.el's define-coding-system turns
    !> it into the attributes: INITIAL, the charsets initially designated
    !> to G0..G3 ("" for none); REQUEST_CHARSETS with REQUEST_REGISTERS,
    !> the registers the charsets ask for, in the order of the alist; and
    !> REG_USAGE, the registers of the other charsets of 94 and 96
    !> characters (4 for none).  FLAGS are names in
    !> coding-system-iso-2022-flags (the others are ignored, as there).
    !> Port of mule.el:define-coding-system for `:coding-type' `iso-2022'
    !> and its part of coding.c:Fdefine_coding_system_internal.
    subroutine define_iso_2022_coding_system(name, mnemonic, doc, initial, request_charsets, request_registers, &
                                             reg_usage, flags, charsets, mime_charset)
        character(len=*), intent(in) :: name, doc
        integer, intent(in) :: mnemonic
        character(len=*), intent(in) :: initial(0:3), request_charsets(:), flags(:)
        integer, intent(in) :: request_registers(:), reg_usage(2)
        character(len=*), intent(in), optional :: charsets(:), mime_charset
        type(coding_type) :: c
        character(len=*), parameter :: SUFFIXES(0:2) = [character(len=5) :: "-unix", "-dos", "-mac"]
        integer :: i, k, id, sub, category, g1

        c%name = name
        c%ctype = CODING_ISO_2022
        c%mnemonic = mnemonic
        c%doc = doc
        c%mime_charset = ""
        if (present(mime_charset)) c%mime_charset = mime_charset

        if (present(charsets)) then
            call setup_charset_valids(c, charsets, with_valids=.false.)
        else
            c%charset = CS_LIST
            allocate (c%charset_ids(0), c%charset_names(1))
            c%charset_names(1) = "iso-2022"
            c%iso_flags = ISO_FULL_SUPPORT
        end if

        c%ascii_compatible = .false.

        do i = 0, 3
            c%iso_initial(i) = 0
            if (len_trim(initial(i)) > 0) c%iso_initial(i) = charset_id_of(trim(initial(i)))
        end do

        if (c%iso_initial(0) /= 0) c%ascii_compatible = charset_ascii_compatible_of(c%iso_initial(0))
        c%iso_reg94 = reg_usage(1)
        c%iso_reg96 = reg_usage(2)
        allocate (c%iso_request_ids(size(request_charsets)), c%iso_request_regs(size(request_charsets)))

        do k = 1, size(request_charsets)
            c%iso_request_ids(k) = charset_id_of(trim(request_charsets(k)))
            c%iso_request_regs(k) = request_registers(k)
        end do

        do k = 1, size(flags)
            do i = 0, size(ISO_2022_FLAG_NAMES) - 1
                if (flags(k) == ISO_2022_FLAG_NAMES(i)) c%iso_flags = ior(c%iso_flags, 2**i)
            end do
        end do

        if (iand(c%iso_flags, ISO_SEVEN_BITS) /= 0) then
            if (iand(c%iso_flags, ior(ISO_LOCKING_SHIFT, ISO_SINGLE_SHIFT)) /= 0) then
                category = CAT_ISO_7_ELSE
            else if (iand(c%iso_flags, ISO_FULL_SUPPORT) /= 0) then
                category = CAT_ISO_7
            else
                category = CAT_ISO_7_TIGHT
            end if
        else
            g1 = c%iso_initial(1)

            if (iand(c%iso_flags, ior(ISO_LOCKING_SHIFT, ISO_FULL_SUPPORT)) /= 0 .or. g1 == 0) then
                category = CAT_ISO_8_ELSE
            else if (charset_dimension_of(g1) == 1) then
                category = CAT_ISO_8_1
            else
                category = CAT_ISO_8_2
            end if
        end if

        if (category /= CAT_ISO_8_1 .and. category /= CAT_ISO_8_2) c%ascii_compatible = .false.
        c%category = category
        id = add_coding(c)
        codings(id)%base = id
        call add_alias(name, id)

        do i = 0, 2
            c%name = name//trim(SUFFIXES(i))
            c%eol = i
            sub = add_coding(c)
            codings(sub)%base = id
            codings(id)%subsidiaries(i) = sub
            call add_alias(c%name, sub)
        end do

        if (category_id(category) == 0) category_id(category) = id
    end subroutine define_iso_2022_coding_system

    ! The charsets of coding system ID: for an ISO-2022 coding system that
    ! supports all ISO-2022 charsets, Viso_2022_charset_list.
    function coding_charset_ids(id) result(ids)
        integer, intent(in) :: id
        integer, allocatable :: ids(:)

        if (iand(codings(id)%iso_flags, ISO_FULL_SUPPORT) /= 0) then
            ids = iso_2022_charset_ids()
        else if (codings(id)%ctype == CODING_EMACS_MULE) then
            ids = emacs_mule_charset_ids()
        else
            ids = codings(id)%charset_ids
        end if
    end function coding_charset_ids

    ! The register of charset CS in the safe charsets of ISO-2022 coding
    ! system ID, -1 if it may not use CS.  The definition gives each charset
    ! of the coding system's `:charset-list' register 0; one with
    ! `:charset-list' `iso-2022' gets them from its requests and register
    ! usage once the charset priority has changed, as it has at startup.
    ! Port of coding.c:CODING_ISO_REQUEST with setup_iso_safe_charsets.
    integer function iso_request(id, cs) result(reg)
        integer, intent(in) :: id, cs
        integer, allocatable :: ids(:)
        integer :: k

        reg = -1
        if (cs <= 0) return
        ids = coding_charset_ids(id)
        if (.not. any(ids == cs)) return

        if (iand(codings(id)%iso_flags, ISO_FULL_SUPPORT) == 0) then
            reg = 0
            return
        end if

        do k = 1, size(codings(id)%iso_request_ids)
            if (codings(id)%iso_request_ids(k) == cs) then
                reg = codings(id)%iso_request_regs(k)
                return
            end if
        end do

        if (charset_iso_chars_96_of(cs)) then
            reg = codings(id)%iso_reg96
        else
            reg = codings(id)%iso_reg94
        end if

        if (reg >= 4) reg = -1
    end function iso_request

    ! Whether ISO-2022 coding system ID may use charset CS.  Port of
    ! coding.c:SAFE_CHARSET_P.
    logical function safe_charset_p(id, cs)
        integer, intent(in) :: id, cs

        safe_charset_p = iso_request(id, cs) >= 0
    end function safe_charset_p

    integer function add_coding(c) result(id)
        type(coding_type), intent(in) :: c
        type(coding_type), allocatable :: grown(:)

        if (ncodings == size(codings)) then
            allocate (grown(2*size(codings)))
            grown(1:ncodings) = codings(1:ncodings)
            call move_alloc(grown, codings)
        end if

        ncodings = ncodings + 1
        codings(ncodings) = c
        id = ncodings
    end function add_coding

    subroutine add_alias(name, id)
        character(len=*), intent(in) :: name
        integer, intent(in) :: id
        type(alias_type), allocatable :: grown(:)

        if (naliases == size(aliases)) then
            allocate (grown(2*size(aliases)))
            grown(1:naliases) = aliases(1:naliases)
            call move_alloc(grown, aliases)
        end if

        naliases = naliases + 1
        aliases(naliases)%name = name
        aliases(naliases)%id = id
    end subroutine add_alias

    !> Define ALIAS as an alias for coding system CODING_SYSTEM.  Port of
    !> coding.c:Fdefine_coding_system_alias (the subsidiaries get aliases
    !> too: ALIAS-unix and so on).
    recursive subroutine define_coding_system_alias(alias, coding_system)
        character(len=*), intent(in) :: alias, coding_system
        character(len=*), parameter :: SUFFIXES(0:2) = [character(len=5) :: "-unix", "-dos", "-mac"]
        integer :: id, i

        id = coding_system_id(coding_system)
        if (id == 0) return
        call add_alias(alias, id)

        if (codings(id)%eol == EOL_UNDECIDED) then
            do i = 0, 2
                call add_alias(alias//trim(SUFFIXES(i)), codings(id)%subsidiaries(i))
            end do
        end if
    end subroutine define_coding_system_alias

    ! ------------------------------------------------------------------
    ! Looking up coding systems
    ! ------------------------------------------------------------------

    !> The id of the coding system called NAME (an alias included), or 0.
    recursive integer function coding_system_id(name) result(id)
        character(len=*), intent(in) :: name
        integer :: k

        id = 0
        if (ncodings == 0) call syms_of_coding()

        do k = 1, naliases
            if (aliases(k)%name == name) then
                id = aliases(k)%id
                return
            end if
        end do
    end function coding_system_id

    !> The name of the coding system with id ID (not an alias).
    function coding_system_name(id) result(name)
        integer, intent(in) :: id
        character(len=:), allocatable :: name

        name = ""
        if (id > 0 .and. id <= ncodings) name = codings(id)%name
    end function coding_system_name

    !> Return t if OBJECT is nil or a coding-system.
    !> See the documentation of `define_coding_system' for information
    !> about coding-system objects.  Port of coding.c:Fcoding_system_p
    !> ("" is nil).
    logical function coding_system_p(object)
        character(len=*), intent(in) :: object

        coding_system_p = len(object) == 0
        if (.not. coding_system_p) coding_system_p = coding_system_id(object) > 0
    end function coding_system_p

    !> Check validity of CODING-SYSTEM.
    !> If valid, return CODING-SYSTEM, else signal a `coding_system_error' error.
    !> It is valid if it is nil or a symbol defined as a coding system by the
    !> function `define_coding_system'.  Port of
    !> coding.c:Fcheck_coding_system.
    subroutine check_coding_system(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status

        if (coding_system_p(coding_system)) return
        call set_error(status, FM_CODING_SYSTEM_ERROR, coding_system)
    end subroutine check_coding_system

    !> Return eol-type of CODING-SYSTEM.
    !> An eol-type is an integer 0, 1, 2, or a vector of coding systems.
    !>
    !> Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
    !> and CR respectively.
    !>
    !> A vector value indicates that a format of end-of-line should be
    !> detected automatically.  Nth element of the vector is the subsidiary
    !> coding system whose eol-type is N.  Port of
    !> coding.c:Fcoding_system_eol_type (EOL_UNDECIDED for a vector, whose
    !> elements `coding_system_subsidiaries' gives; EOL_UNDECIDED also for
    !> nil and a name that is not a coding system).
    integer function coding_system_eol_type(coding_system) result(eol)
        character(len=*), intent(in) :: coding_system
        integer :: id

        eol = EOL_UNDECIDED
        if (len(coding_system) == 0) return
        id = coding_system_id(coding_system)
        if (id > 0) eol = codings(id)%eol
    end function coding_system_eol_type

    !> The -unix, -dos and -mac subsidiaries of CODING_SYSTEM, or empty
    !> names if its end-of-line type is decided.
    subroutine coding_system_subsidiaries(coding_system, subs)
        character(len=*), intent(in) :: coding_system
        character(len=40), intent(out) :: subs(0:2)
        integer :: id, i

        subs = ""
        id = coding_system_id(coding_system)
        if (id == 0) return
        if (codings(id)%eol /= EOL_UNDECIDED) return

        do i = 0, 2
            subs(i) = codings(codings(id)%subsidiaries(i))%name
        end do
    end subroutine coding_system_subsidiaries

    !> Return the base of CODING-SYSTEM.
    !> Any alias or subsidiary coding system is not a base coding system.
    !> Port of coding.c:Fcoding_system_base ("" for nil).
    function coding_system_base(coding_system) result(base)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: base
        integer :: id

        base = ""
        id = coding_system_id(coding_system)
        if (id > 0) base = codings(codings(id)%base)%name
    end function coding_system_base

    !> Return the coding type of CODING-SYSTEM.
    !> A coding type is a symbol indicating the encoding method of CODING-SYSTEM.
    !> See the function `define_coding_system' for more detail.  Port of
    !> mule.el:coding-system-type ("undecided", "utf-8", "utf-16",
    !> "charset" or "raw-text").
    function coding_system_type(coding_system) result(t)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: t
        integer :: id

        t = ""
        id = coding_system_id(coding_system)
        if (id == 0) return

        select case (codings(id)%ctype)
        case (CODING_UNDECIDED)
            t = "undecided"
        case (CODING_UTF_8)
            t = "utf-8"
        case (CODING_UTF_16)
            t = "utf-16"
        case (CODING_CHARSET)
            t = "charset"
        case (CODING_SJIS)
            t = "shift-jis"
        case (CODING_BIG5)
            t = "big5"
        case (CODING_ISO_2022)
            t = "iso-2022"
        case (CODING_EMACS_MULE)
            t = "emacs-mule"
        case default
            t = "raw-text"
        end select
    end function coding_system_type

    !> Return the mnemonic character of CODING-SYSTEM.
    !> The mnemonic character of a coding system is used in mode line to
    !> indicate the coding system.  If CODING-SYSTEM is nil, return ?=.
    !> Port of mule.el:coding-system-mnemonic.
    integer function coding_system_mnemonic(coding_system) result(c)
        character(len=*), intent(in) :: coding_system
        integer :: id

        c = iachar("=")
        id = coding_system_id(coding_system)
        if (id > 0) c = codings(id)%mnemonic
    end function coding_system_mnemonic

    !> Return the documentation string of CODING-SYSTEM.  Port of
    !> mule.el:coding-system-doc-string.
    function coding_system_doc_string(coding_system) result(doc)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: doc
        integer :: id

        doc = ""
        id = coding_system_id(coding_system)
        if (id > 0) doc = codings(id)%doc
    end function coding_system_doc_string

    !> The MIME charset of CODING_SYSTEM ("" if none).
    function coding_system_mime_charset(coding_system) result(m)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: m
        integer :: id

        m = ""
        id = coding_system_id(coding_system)
        if (id > 0) m = codings(id)%mime_charset
    end function coding_system_mime_charset

    !> The charsets CODING_SYSTEM can encode, as describe-coding-system
    !> names them.  Port of mule.el:coding-system-charset-list.
    function coding_system_charset_list(coding_system) result(s)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: s
        integer :: id, k

        s = ""
        id = coding_system_id(coding_system)
        if (id == 0) return

        select case (codings(id)%charset)
        case (CS_LIST)
            do k = 1, size(codings(id)%charset_names)
                if (k > 1) s = s//" "
                s = s//trim(codings(id)%charset_names(k))
            end do
        case (CS_ASCII)
            s = "ascii"
        case (CS_UNICODE)
            s = "unicode"
        case (CS_EMACS)
            s = "emacs"
        case default
            s = ""
        end select
    end function coding_system_charset_list

    !> The `:bom' attribute of CODING_SYSTEM (BOM_NONE, BOM_WITH or
    !> BOM_DETECT), and for BOM_DETECT the coding systems with and without
    !> a BOM.
    subroutine coding_system_bom(coding_system, bom, with, without)
        character(len=*), intent(in) :: coding_system
        integer, intent(out) :: bom
        character(len=:), allocatable, intent(out), optional :: with, without
        integer :: id

        bom = BOM_NONE
        if (present(with)) with = ""
        if (present(without)) without = ""
        id = coding_system_id(coding_system)
        if (id == 0) return
        bom = codings(id)%bom

        if (bom == BOM_DETECT) then
            if (present(with)) with = codings(id)%bom_with
            if (present(without)) without = codings(id)%bom_without
        end if
    end subroutine coding_system_bom

    !> Whether CODING_SYSTEM is a UTF-16 one that is big endian.
    logical function coding_system_endian(coding_system) result(big)
        character(len=*), intent(in) :: coding_system
        integer :: id

        big = .true.
        id = coding_system_id(coding_system)
        if (id > 0) big = codings(id)%big_endian
    end function coding_system_endian

    !> Whether CODING_SYSTEM reads files into unibyte buffers (`:for-unibyte').
    logical function coding_system_for_unibyte_p(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        coding_system_for_unibyte_p = .false.
        id = coding_system_id(coding_system)
        if (id > 0) coding_system_for_unibyte_p = codings(id)%for_unibyte
    end function coding_system_for_unibyte_p

    !> Whether CODING_SYSTEM is ASCII compatible (`:ascii-compatible-p').
    logical function coding_system_ascii_compatible_p(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        coding_system_ascii_compatible_p = .false.
        id = coding_system_id(coding_system)
        if (id > 0) coding_system_ascii_compatible_p = codings(id)%ascii_compatible
    end function coding_system_ascii_compatible_p

    !> Whether CODING_SYSTEM prefers UTF-8 (`:prefer-utf-8').
    logical function coding_system_prefer_utf_8_p(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        coding_system_prefer_utf_8_p = .false.
        id = coding_system_id(coding_system)
        if (id > 0) coding_system_prefer_utf_8_p = codings(id)%prefer_utf_8
    end function coding_system_prefer_utf_8_p

    !> The coding category of CODING_SYSTEM (`:category').
    function coding_system_category(coding_system) result(cat)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: cat
        integer :: id

        cat = ""
        id = coding_system_id(coding_system)
        if (id > 0) cat = trim(CATEGORY_NAMES(codings(id)%category))
    end function coding_system_category

    !> Return the list of aliases of CODING-SYSTEM.  Port of
    !> coding.c:Fcoding_system_aliases (in order of definition, the
    !> coding system's own name first).
    subroutine coding_system_aliases(coding_system, names)
        character(len=*), intent(in) :: coding_system
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        integer :: id, k, n

        id = coding_system_id(coding_system)
        allocate (names(naliases))
        n = 0

        if (id > 0) then
            do k = 1, naliases
                if (aliases(k)%id == id) then
                    n = n + 1
                    names(n)%name = aliases(k)%name
                end if
            end do
        end if

        names = names(1:n)
    end subroutine coding_system_aliases

    !> Return true if the two coding systems are the same.
    !> Two coding systems are identical if both symbols are equal
    !> or one is an alias of the other.  Port of
    !> mule.el:coding-system-equal.
    logical function coding_system_equal(coding_system_1, coding_system_2)
        character(len=*), intent(in) :: coding_system_1, coding_system_2

        coding_system_equal = coding_system_1 == coding_system_2

        if (.not. coding_system_equal) then
            coding_system_equal = coding_system_id(coding_system_1) > 0 .and. &
                                  coding_system_id(coding_system_1) == coding_system_id(coding_system_2)
        end if
    end function coding_system_equal

    !> Return a list of all existing non-subsidiary coding systems.
    !> If optional arg BASE-ONLY is true, only base coding systems are
    !> listed.  The value doesn't include subsidiary coding systems which are
    !> made from bases and aliases automatically for various end-of-line
    !> formats (e.g. iso-latin-1-unix, koi8-r-dos).  Port of
    !> mule.el:coding-system-list (in order of definition).
    subroutine coding_system_list(names, base_only)
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        logical, intent(in), optional :: base_only
        logical :: bases
        integer :: k, n, id

        bases = .false.
        if (present(base_only)) bases = base_only
        if (ncodings == 0) call syms_of_coding()
        allocate (names(naliases))
        n = 0

        do k = 1, naliases
            id = aliases(k)%id

            if (bases) then
                if (codings(id)%base /= id .or. aliases(k)%name /= codings(id)%name) cycle
            else
                ! An alias of a subsidiary is a subsidiary too, unless it
                ! names one alone (emacs-internal).
                if (codings(id)%base /= id) then
                    if (is_subsidiary_name(aliases(k)%name, id)) cycle
                end if
            end if

            n = n + 1
            names(n)%name = aliases(k)%name
        end do

        names = names(1:n)
    end subroutine coding_system_list

    !> Whether NAME, a name of subsidiary ID, was made from a base name
    !> by adding -unix, -dos or -mac.
    logical function is_subsidiary_name(name, id)
        character(len=*), intent(in) :: name
        integer, intent(in) :: id
        character(len=*), parameter :: SUFFIXES(0:2) = [character(len=5) :: "-unix", "-dos", "-mac"]
        character(len=:), allocatable :: suffix

        is_subsidiary_name = .false.
        if (codings(id)%eol < 0 .or. codings(id)%eol > 2) return
        suffix = trim(SUFFIXES(codings(id)%eol))
        if (len(name) > len(suffix)) is_subsidiary_name = name(len(name) - len(suffix) + 1:) == suffix
    end function is_subsidiary_name

    !> Return a list of coding systems ordered by their priorities.
    !> The list contains a subset of coding systems; i.e. coding systems
    !> assigned to each coding category (see `coding_category_list').
    !>
    !> HIGHESTP true means just return the highest priority one.  Port of
    !> coding.c:Fcoding_system_priority_list.
    subroutine coding_system_priority_list(names, highestp)
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        logical, intent(in), optional :: highestp
        integer :: i, n, id

        if (ncodings == 0) call syms_of_coding()
        allocate (names(NCAT))
        n = 0

        do i = 0, NCAT - 1
            id = category_id(priorities(i))
            if (id == 0) cycle
            n = n + 1
            names(n)%name = codings(codings(id)%base)%name

            if (present(highestp)) then
                if (highestp) exit
            end if
        end do

        names = names(1:n)
    end subroutine coding_system_priority_list

    !> Assign higher priority to the coding systems given as arguments.
    !> If multiple coding systems belong to the same category,
    !> all but the first one are ignored.  Port of
    !> coding.c:Fset_coding_system_priority.
    subroutine set_coding_system_priority(coding_systems, status)
        character(len=*), intent(in) :: coding_systems(:)
        type(status_type), intent(out) :: status
        logical :: changed(0:NCAT - 1)
        integer :: new(0:NCAT - 1), i, j, id, cat

        changed = .false.
        j = 0

        do i = 1, size(coding_systems)
            id = coding_system_id(trim(coding_systems(i)))

            if (id == 0) then
                call set_error(status, FM_CODING_SYSTEM_ERROR, trim(coding_systems(i)))
                return
            end if

            cat = codings(id)%category
            if (changed(cat)) cycle
            changed(cat) = .true.
            new(j) = cat
            j = j + 1
            category_id(cat) = id
        end do

        i = j
        j = 0

        do while (i < NCAT)
            do while (changed(priorities(j)))
                j = j + 1
            end do

            new(i) = priorities(j)
            i = i + 1
            j = j + 1
        end do

        priorities = new
    end subroutine set_coding_system_priority

    !> Return a coding system which differs from CODING-SYSTEM in EOL conversion.
    !> The returned coding system converts end-of-line by EOL-TYPE
    !> but text as the same way as CODING-SYSTEM.
    !> EOL-TYPE should be `unix', `dos', `mac', or nil (EOL_UNIX, EOL_DOS,
    !> EOL_MAC or EOL_UNDECIDED here).
    !> If EOL-TYPE is nil, the returned coding system detects
    !> how end-of-line is formatted automatically while decoding.
    !>
    !> EOL-TYPE can be specified by an integer 0, 1, or 2.
    !> They means `unix', `dos', and `mac' respectively.  Port of
    !> mule.el:coding-system-change-eol-conversion.
    function coding_system_change_eol_conversion(coding_system, eol_type) result(cs)
        character(len=*), intent(in) :: coding_system
        integer, intent(in) :: eol_type
        character(len=:), allocatable :: cs
        integer :: id, base, orig_eol

        cs = coding_system
        id = coding_system_id(coding_system)
        if (id == 0) return
        orig_eol = codings(id)%eol
        base = codings(id)%base

        if (orig_eol == EOL_UNDECIDED) then
            if (eol_type /= EOL_UNDECIDED) cs = codings(codings(id)%subsidiaries(eol_type))%name
        else if (orig_eol == eol_type) then
            cs = coding_system
        else if (codings(base)%eol == EOL_UNDECIDED) then
            ! A subsidiary: its base, or another subsidiary.
            if (eol_type == EOL_UNDECIDED) then
                cs = codings(base)%name
            else
                cs = codings(codings(base)%subsidiaries(eol_type))%name
            end if
        end if
    end function coding_system_change_eol_conversion

    !> Return a coding system which differs from CODING-SYSTEM in text conversion.
    !> The returned coding system converts text by CODING
    !> but end-of-line as the same way as CODING-SYSTEM.
    !> If CODING is nil, the returned coding system detects
    !> how text is formatted automatically while decoding.  Port of
    !> mule.el:coding-system-change-text-conversion.
    function coding_system_change_text_conversion(coding_system, coding) result(cs)
        character(len=*), intent(in) :: coding_system, coding
        character(len=:), allocatable :: cs
        integer :: eol

        eol = coding_system_eol_type(coding_system)

        if (len(coding) == 0) then
            cs = coding_system_change_eol_conversion("undecided", eol)
        else
            cs = coding_system_change_eol_conversion(coding, eol)
        end if
    end function coding_system_change_text_conversion

    !> Return the string indicating end-of-line format of CODING-SYSTEM.
    !> Port of mule.el:coding-system-eol-type-mnemonic.
    function coding_system_eol_type_mnemonic(coding_system) result(s)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: s

        select case (coding_system_eol_type(coding_system))
        case (EOL_UNIX)
            s = eol_mnemonic_unix%value
        case (EOL_DOS)
            s = eol_mnemonic_dos%value
        case (EOL_MAC)
            s = eol_mnemonic_mac%value
        case default
            s = eol_mnemonic_undecided%value
        end select
    end function coding_system_eol_type_mnemonic

    !> If CODING_SYSTEM doesn't specify end-of-line format, return the
    !> subsidiary that has the end-of-line format of PARENT (if it is
    !> not nil and specifies one) or the system's (unix).  Port of
    !> coding.c:coding_inherit_eol_type ("" is nil, meaning raw-text).
    function coding_inherit_eol_type(coding_system, parent) result(cs)
        character(len=*), intent(in) :: coding_system, parent
        character(len=:), allocatable :: cs
        integer :: eol, parent_eol

        cs = coding_system
        if (len(cs) == 0) cs = "raw-text"
        eol = coding_system_eol_type(cs)
        if (eol /= EOL_UNDECIDED) return
        parent_eol = EOL_UNIX

        if (len(parent) > 0) then
            if (coding_system_eol_type(parent) /= EOL_UNDECIDED) parent_eol = coding_system_eol_type(parent)
        end if

        cs = coding_system_change_eol_conversion(cs, parent_eol)
    end function coding_inherit_eol_type

    !> Return raw-text or one of its subsidiaries that has the same
    !> end-of-line type as CODING_SYSTEM.  Port of
    !> coding.c:raw_text_coding_system.
    function raw_text_coding_system(coding_system) result(cs)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: cs
        integer :: id, eol

        cs = "raw-text"
        if (len(coding_system) == 0) return
        id = coding_system_id(coding_system)
        if (id == 0) return

        if (codings(id)%ctype == CODING_RAW_TEXT) then
            cs = coding_system
            return
        end if

        eol = codings(id)%eol
        if (eol /= EOL_UNDECIDED) cs = coding_system_change_eol_conversion("raw-text", eol)
    end function raw_text_coding_system

    !> Choose a coding system for an operation based on the target name.
    !> The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
    !> DECODING-SYSTEM is the coding system to use for decoding
    !> (in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
    !> for encoding (in case OPERATION does encoding).
    !>
    !> The first argument OPERATION specifies an I/O primitive:
    !>   For file I/O, `insert_file_contents' or `write_region'.
    !>
    !> The remaining arguments should be the same arguments that were passed
    !> to the primitive.  Depending on which primitive, one of those arguments
    !> is selected as the TARGET.  For file I/O, the file name is the target.
    !>
    !> This function looks up what is specified for TARGET in
    !> `file_coding_system_alist'.  If no match is found, both are "".
    !> Port of coding.c:Ffind_operation_coding_system (file operations
    !> only: process and network operations are Deferred; MATCHER, a
    !> regexp matcher returning whether a pattern matches, keeps this
    !> module below the search module).
    subroutine find_operation_coding_system(filename, matcher, decoding, encoding)
        character(len=*), intent(in) :: filename
        interface
            logical function matcher(pattern, string)
                implicit none(type, external)
                character(len=*), intent(in) :: pattern, string
            end function matcher
        end interface
        character(len=:), allocatable, intent(out) :: decoding, encoding
        integer :: k

        decoding = ""
        encoding = ""
        if (ncodings == 0) call syms_of_coding()

        do k = 1, size(file_coding_system_alist)
            if (matcher(file_coding_system_alist(k)%pattern, filename)) then
                decoding = file_coding_system_alist(k)%decoding
                encoding = file_coding_system_alist(k)%encoding
                return
            end if
        end do
    end subroutine find_operation_coding_system

    ! ------------------------------------------------------------------
    ! Source text
    ! ------------------------------------------------------------------

    !> The source as coding.c's ONE_MORE_BYTE sees it: a byte (0 to 255),
    !> or, in multibyte text, minus a character that is not a raw byte.
    subroutine source_units(src, multibyte, units)
        character(len=*), intent(in) :: src
        logical, intent(in) :: multibyte
        integer, allocatable, intent(out) :: units(:)
        integer(int64) :: p
        integer :: n, ch, length

        allocate (units(len(src)))
        n = 0

        if (.not. multibyte) then
            do p = 1, len(src)
                n = n + 1
                units(n) = iachar(src(p:p))
            end do

            return
        end if

        p = 1

        do while (p <= len(src, kind=int64))
            if (iachar(src(p:p)) < 128) then
                ch = iachar(src(p:p))
                length = 1
            else if (multibyte_length(src, p, .true.) == 0) then
                ch = iachar(src(p:p))
                length = 1
            else
                call string_char_and_length(src, p, ch, length)

                if (char_byte8_p(ch)) then
                    ch = char_to_byte8(ch)
                else
                    ch = -ch
                end if
            end if

            n = n + 1
            units(n) = ch
            p = p + length
        end do

        units = units(1:n)
    end subroutine source_units

    ! ------------------------------------------------------------------
    ! Detection
    ! ------------------------------------------------------------------

    logical function has(mask, cat)
        integer, intent(in) :: mask, cat

        has = btest(mask, cat)
    end function has

    !> Whether the text is UTF-8, counting characters and checking for a
    !> BOM.  Port of coding.c:detect_coding_utf_8.
    logical function detect_coding_utf_8(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer(int64) :: src, src_base, nchars, n
        integer :: c, k, trail
        logical :: bom_found, invalid

        n = size(units, kind=int64)
        info%checked = ior(info%checked, MASK_UTF_8)
        src = head_ascii + 1
        nchars = head_ascii
        bom_found = .false.
        ok = .false.

        ! BOM should be at the head, and is 3-byte long.
        if (src == 1 .and. src + 3 <= n) then
            if (units(1) == int(z'EF') .and. units(2) == int(z'BB') .and. units(3) == int(z'BF')) then
                bom_found = .true.
                src = src + 3
                nchars = nchars + 1
            end if
        end if

        invalid = .false.
        src_base = src

        do while (src <= n)
            src_base = src
            c = units(src)
            src = src + 1

            if (c < 128) then
                nchars = nchars + 1

                if (c == CR .and. src <= n) then
                    if (units(src) == LF) then
                        src = src + 1
                        nchars = nchars + 1
                    end if
                end if

                cycle
            end if

            ! The number of trailing bytes the leading byte announces (5
            ! for a byte no sequence starts with).
            if (iand(c, int(z'E0')) == int(z'C0')) then
                trail = 1
            else if (iand(c, int(z'F0')) == int(z'E0')) then
                trail = 2
            else if (iand(c, int(z'F8')) == int(z'F0')) then
                trail = 3
            else if (iand(c, int(z'FC')) == int(z'F8') .and. c < int(z'F8')) then
                trail = 4
            else
                trail = 5
            end if

            do k = 1, min(trail, 4)
                if (src > n) exit

                if (units(src) < 0 .or. iand(units(src), int(z'C0')) /= int(z'80')) then
                    invalid = .true.
                    exit
                end if

                src = src + 1
            end do

            if (invalid .or. src > n .and. k <= min(trail, 4)) exit

            if (trail == 5) then
                invalid = .true.
                exit
            end if

            nchars = nchars + 1
        end do

        ! An incomplete sequence at the end rejects too (the whole text is
        ! the last block).
        if (.not. invalid .and. src > n .and. src_base < src) invalid = sequence_cut(units, src_base)

        if (invalid) then
            info%rejected = ior(info%rejected, MASK_UTF_8)
            return
        end if

        if (bom_found) then
            ! The first character 0xFFFE doesn't necessarily mean a BOM.
            info%found = ior(info%found, 2**CAT_UTF_8_AUTO + 2**CAT_UTF_8_SIG + 2**CAT_UTF_8_NOSIG)
        else
            info%rejected = ior(info%rejected, 2**CAT_UTF_8_SIG)
            ! The found characters are less than source bytes, which
            ! means that we found a valid non-ASCII characters.
            if (nchars < n) info%found = ior(info%found, 2**CAT_UTF_8_AUTO + 2**CAT_UTF_8_NOSIG)
        end if

        ok = .true.
    end function detect_coding_utf_8

    !> Whether the UTF-8 sequence starting at UNITS(START) runs past the
    !> end of the text.
    logical function sequence_cut(units, start)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: start
        integer :: c, need

        sequence_cut = .false.
        c = units(start)
        if (c < 128) return

        if (iand(c, int(z'E0')) == int(z'C0')) then
            need = 2
        else if (iand(c, int(z'F0')) == int(z'E0')) then
            need = 3
        else if (iand(c, int(z'F8')) == int(z'F0')) then
            need = 4
        else
            need = 5
        end if

        sequence_cut = start + need - 1 > size(units, kind=int64)
    end function sequence_cut

    !> Whether the text is UTF-16: a BOM decides; otherwise the spread of
    !> the even and odd bytes rejects the endianness that would make it
    !> binary data.  Port of coding.c:detect_coding_utf_16.
    logical function detect_coding_utf_16(units, info) result(ok)
        integer, intent(in) :: units(:)
        type(detect_info_type), intent(inout) :: info
        logical :: e(0:255), o(0:255)
        integer :: c1, c2, e_num, o_num
        integer(int64) :: src, n

        n = size(units, kind=int64)
        info%checked = ior(info%checked, MASK_UTF_16)
        ok = .false.

        if (mod(n, 2_int64) == 1) then
            info%rejected = ior(info%rejected, MASK_UTF_16)
            return
        end if

        if (n < 2) then
            ! no_more_source
            if (n == 0) then
                ok = .true.
            else
                info%rejected = ior(info%rejected, MASK_UTF_16)
            end if

            return
        end if

        c1 = units(1)
        c2 = units(2)

        if (c1 == int(z'FF') .and. c2 == int(z'FE')) then
            info%found = ior(info%found, 2**CAT_UTF_16_LE + 2**CAT_UTF_16_AUTO)
            info%rejected = ior(info%rejected, 2**CAT_UTF_16_BE + 2**CAT_UTF_16_BE_NOSIG + 2**CAT_UTF_16_LE_NOSIG)
            ok = .true.
        else if (c1 == int(z'FE') .and. c2 == int(z'FF')) then
            info%found = ior(info%found, 2**CAT_UTF_16_BE + 2**CAT_UTF_16_AUTO)
            info%rejected = ior(info%rejected, 2**CAT_UTF_16_LE + 2**CAT_UTF_16_BE_NOSIG + 2**CAT_UTF_16_LE_NOSIG)
            ok = .true.
        else if (c2 < 0) then
            info%rejected = ior(info%rejected, MASK_UTF_16)
        else
            ! We check the dispersion of Eth and Oth bytes where E is even and
            ! O is odd.  If both are high, we assume binary data.
            e = .false.
            o = .false.
            e_num = 1
            o_num = 1
            if (c1 >= 0) e(c1) = .true.
            o(c2) = .true.
            info%rejected = ior(info%rejected, 2**CAT_UTF_16_AUTO + 2**CAT_UTF_16_BE + 2**CAT_UTF_16_LE)
            src = 3

            do while (iand(info%rejected, MASK_UTF_16) /= MASK_UTF_16)
                if (src + 1 > n) exit
                c1 = units(src)
                c2 = units(src + 1)
                src = src + 2
                if (c2 < 0) exit

                if (c1 >= 0) then
                    if (.not. e(c1)) then
                        e(c1) = .true.
                        e_num = e_num + 1
                        if (e_num >= 128) info%rejected = ior(info%rejected, 2**CAT_UTF_16_BE_NOSIG)
                    end if
                end if

                if (.not. o(c2)) then
                    o(c2) = .true.
                    o_num = o_num + 1
                    if (o_num >= 128) info%rejected = ior(info%rejected, 2**CAT_UTF_16_LE_NOSIG)
                end if
            end do
        end if
    end function detect_coding_utf_16

    !> Whether the text is in the charsets of the coding system of the
    !> charset category: each byte must start a code point of one of them;
    !> an iso-8859 or iso-latin coding system rejects 0x80..0x9F except the
    !> bytes of latin-extra-code-table.  Port of
    !> coding.c:detect_coding_charset.
    logical function detect_coding_charset(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer :: id, c, found, k, cs, dim, idx
        integer(int64) :: src, nu
        logical :: check_latin_extra, matched, rejected

        info%checked = ior(info%checked, 2**CAT_CHARSET)
        ok = .false.
        id = category_id(CAT_CHARSET)
        check_latin_extra = index(codings(id)%name, "iso-8859-") == 1 .or. index(codings(id)%name, "iso-latin-") == 1
        nu = size(units, kind=int64)
        src = 1
        if (codings(id)%ascii_compatible) src = head_ascii + 1
        found = 0
        rejected = .false.

        do while (src <= nu)
            c = units(src)
            src = src + 1
            if (c < 0) cycle

            if (c > 255) then
                rejected = .true.
            else if (codings(id)%nvalids(c) == 0) then
                rejected = .true.
            else if (c >= 128 .and. c < int(z'A0') .and. check_latin_extra .and. .not. any(LATIN_EXTRA == c)) then
                rejected = .true.
            end if

            if (rejected) exit
            if (c >= 128) found = 2**CAT_CHARSET
            ! The other bytes of the code point, for one of the charsets.
            idx = 1
            matched = .false.

            do k = 1, codings(id)%nvalids(c)
                cs = codings(id)%valids(k, c)
                dim = charset_dimension_of(cs)

                do while (idx < dim)
                    if (src > nu) then
                        info%rejected = ior(info%rejected, 2**CAT_CHARSET)
                        return
                    end if

                    c = units(src)
                    src = src + 1
                    if (c < charset_code_space_of(cs, (dim - 1 - idx)*4) &
                        .or. c > charset_code_space_of(cs, (dim - 1 - idx)*4 + 1)) exit
                    idx = idx + 1
                end do

                if (idx == dim) then
                    matched = .true.
                    exit
                end if
            end do

            if (.not. matched) then
                rejected = .true.
                exit
            end if
        end do

        if (rejected) then
            info%rejected = ior(info%rejected, 2**CAT_CHARSET)
            return
        end if

        info%found = ior(info%found, found)
        ok = .true.
    end function detect_coding_charset

    !> Whether the text is in one of the ISO-2022 coding systems of the ISO
    !> categories: designations of charsets they can use, single and
    !> locking shifts, and the 8-bit codes, rejecting the categories they
    !> rule out.  Port of coding.c:detect_coding_iso_2022.
    logical function detect_coding_iso_2022(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer(int64) :: src, nu, src_base, length
        integer :: c, c1, id, rejected, found, composition_count
        logical :: single_shifting, ended

        info%checked = ior(info%checked, MASK_ISO)
        ended = .false.
        ok = .false.
        nu = size(units, kind=int64)
        single_shifting = .false.
        rejected = 0
        found = 0
        composition_count = -1
        ! A coding system of this category is always ASCII compatible.
        src = head_ascii + 1

        do while (rejected /= MASK_ISO)
            if (.not. next(c)) exit

            select case (c)
            case (ISO_CODE_ESC)
                if (inhibit_iso_escape_detection%value) cycle
                single_shifting = .false.
                if (.not. next(c)) exit

                if (c == iachar("N") .or. c == iachar("O")) then
                    ! ESC <Fe> for SS2 or SS3.
                    single_shifting = .true.
                    rejected = ior(rejected, ior(MASK_ISO_7BIT, MASK_ISO_8BIT))
                else if (c == iachar("1")) then
                    ! End of composition.
                    if (composition_count < 0 .or. composition_count > 16) cycle
                    composition_count = -1
                    found = ior(found, MASK_ISO)
                else if (c >= iachar("0") .and. c <= iachar("4")) then
                    ! ESC <Fp> for start/end composition.
                    composition_count = 0
                else
                    id = 0

                    if (c >= iachar("(") .and. c <= iachar("/")) then
                        ! Designation sequence for a charset of dimension 1.
                        if (.not. next(c1)) exit
                        if (c1 >= 32 .and. c1 < 128) id = iso_charset_id(1, c >= iachar(","), c1)

                        if (id == 0) then
                            ! Invalid designation sequence.  Just ignore.
                            if (c1 >= 128) rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
                            cycle
                        end if
                    else if (c == iachar("$")) then
                        ! Designation sequence for a charset of dimension 2.
                        if (.not. next(c)) exit

                        if (c >= iachar("@") .and. c <= iachar("B")) then
                            ! Designation for JISX0208.1978, GB2312, or JISX0208.
                            id = iso_charset_id(2, .false., c)
                        else if (c >= iachar("(") .and. c <= iachar("/")) then
                            if (.not. next(c1)) exit
                            if (c1 >= 32 .and. c1 < 128) id = iso_charset_id(2, c >= iachar(","), c1)

                            if (id == 0) then
                                if (c1 >= 128) rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
                                cycle
                            end if
                        else
                            ! Invalid designation sequence.  Just ignore it.
                            if (c >= 128) rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
                            cycle
                        end if
                    else
                        ! Invalid escape sequence.  Just ignore it.
                        if (c >= 128) rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
                        cycle
                    end if

                    ! We found a valid designation sequence for CHARSET.
                    rejected = ior(rejected, MASK_ISO_8BIT)
                    call found_or_rejected(CAT_ISO_7, id)
                    call found_or_rejected(CAT_ISO_7_TIGHT, id)
                    call found_or_rejected(CAT_ISO_7_ELSE, id)
                    call found_or_rejected(CAT_ISO_8_ELSE, id)
                end if
            case (ISO_CODE_SO, ISO_CODE_SI)
                ! Locking shift out/in.
                if (inhibit_iso_escape_detection%value) cycle
                single_shifting = .false.
                rejected = ior(rejected, ior(MASK_ISO_7BIT, MASK_ISO_8BIT))
            case (ISO_CODE_CSI)
                ! Control sequence introducer.
                single_shifting = .false.
                rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
                found = ior(found, 2**CAT_ISO_8_ELSE)
                call check_extra_latin()
            case (ISO_CODE_SS2, ISO_CODE_SS3)
                ! Single shift.
                if (inhibit_iso_escape_detection%value) cycle
                single_shifting = .false.
                rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))

                if (has_flag(CAT_ISO_8_1, ISO_SINGLE_SHIFT)) then
                    found = ior(found, 2**CAT_ISO_8_1)
                    single_shifting = .true.
                end if

                if (has_flag(CAT_ISO_8_2, ISO_SINGLE_SHIFT)) then
                    found = ior(found, 2**CAT_ISO_8_2)
                    single_shifting = .true.
                end if

                if (.not. single_shifting) call check_extra_latin()
            case default
                if (c < 0) cycle

                if (c < 128) then
                    if (composition_count >= 0) composition_count = composition_count + 1
                    single_shifting = .false.
                    cycle
                end if

                rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))

                if (c >= int(z'A0')) then
                    found = ior(found, 2**CAT_ISO_8_1)

                    ! Check the length of succeeding codes of the range
                    ! 0xA0..0FF.  If the byte length is even, we include
                    ! CATEGORY_MASK_ISO_8_2 in `found'.  We can check this
                    ! only when we are not single shifting.
                    if (.not. single_shifting .and. iand(rejected, 2**CAT_ISO_8_2) == 0) then
                        length = 1

                        do while (src <= nu)
                            src_base = src
                            if (.not. next(c)) exit

                            if (c < int(z'A0')) then
                                src = src_base
                                exit
                            end if

                            length = length + 1
                        end do

                        if (btest(length, 0) .and. src <= nu) then
                            rejected = ior(rejected, 2**CAT_ISO_8_2)
                            if (composition_count >= 0) composition_count = composition_count + int(length)
                        else
                            found = ior(found, 2**CAT_ISO_8_2)
                            if (composition_count >= 0) composition_count = composition_count + int(length/2)
                        end if
                    end if
                else
                    call check_extra_latin()
                end if
            end select
        end do

        ! All rejected before the end of the source.
        if (.not. ended) then
            info%rejected = ior(info%rejected, MASK_ISO)
            return
        end if

        info%rejected = ior(info%rejected, rejected)
        info%found = ior(info%found, iand(found, not(rejected)))
        ok = .true.

    contains

        ! ONE_MORE_BYTE: the next unit in C, false at the end of the source.
        logical function next(c)
            integer, intent(out) :: c

            next = src <= nu
            c = 0

            if (.not. next) then
                ended = .true.
                return
            end if

            c = units(src)
            src = src + 1
        end function next

        logical function has_flag(category, flag)
            integer, intent(in) :: category, flag

            has_flag = .false.
            if (category_id(category) == 0) return
            has_flag = iand(codings(category_id(category))%iso_flags, flag) /= 0
        end function has_flag

        subroutine found_or_rejected(category, cs)
            integer, intent(in) :: category, cs
            logical :: safe

            safe = .false.
            if (category_id(category) /= 0) safe = safe_charset_p(category_id(category), cs)

            if (safe) then
                found = ior(found, 2**category)
            else
                rejected = ior(rejected, 2**category)
            end if
        end subroutine found_or_rejected

        subroutine check_extra_latin()
            if (.not. any(LATIN_EXTRA == c)) then
                rejected = MASK_ISO
                return
            end if

            if (has_flag(CAT_ISO_8_1, ISO_LATIN_EXTRA)) then
                found = ior(found, 2**CAT_ISO_8_1)
            else
                rejected = ior(rejected, 2**CAT_ISO_8_1)
            end if

            rejected = ior(rejected, 2**CAT_ISO_8_2)
        end subroutine check_extra_latin
    end function detect_coding_iso_2022

    !> Whether the text is in emacs-mule: each leading code is followed by
    !> the bytes of 0xA0 and above its length needs; 0x80 starts a
    !> composition of more than 4 bytes; ESC, SI and SO rule it out.  Port
    !> of coding.c:detect_coding_emacs_mule.
    logical function detect_coding_emacs_mule(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer(int64) :: src, nu, src_start
        integer :: c, found, more_bytes

        info%checked = ior(info%checked, 2**CAT_EMACS_MULE)
        ok = .false.
        nu = size(units, kind=int64)
        ! A coding system of this category is always ASCII compatible.
        src = head_ascii + 1
        found = 0

        do while (src <= nu)
            c = units(src)
            src = src + 1
            if (c < 0) cycle

            if (c == 128) then
                ! Perhaps the start of composite character.  We simply
                ! skip it because analyzing it is too heavy for detecting.
                ! But, at least, we check that the composite character
                ! constitutes of more than 4 bytes.
                do
                    src_start = src

                    do
                        if (src > nu) then
                            call reject_if_started()
                            return
                        end if

                        c = units(src)
                        src = src + 1
                        if (c < int(z'A0')) exit
                    end do

                    if (src - src_start <= 4) then
                        info%rejected = ior(info%rejected, 2**CAT_EMACS_MULE)
                        return
                    end if

                    found = 2**CAT_EMACS_MULE
                    if (c /= 128) exit
                end do
            end if

            if (c < 128) then
                if (c == ISO_CODE_ESC .or. c == ISO_CODE_SI .or. c == ISO_CODE_SO) then
                    info%rejected = ior(info%rejected, 2**CAT_EMACS_MULE)
                    return
                end if
            else
                more_bytes = emacs_mule_bytes_of(c) - 1

                do while (more_bytes > 0)
                    if (src > nu) then
                        call reject_if_started()
                        return
                    end if

                    c = units(src)
                    src = src + 1

                    if (c < int(z'A0')) then
                        ! Unread the last byte.
                        src = src - 1
                        exit
                    end if

                    more_bytes = more_bytes - 1
                end do

                if (more_bytes /= 0) then
                    info%rejected = ior(info%rejected, 2**CAT_EMACS_MULE)
                    return
                end if

                found = 2**CAT_EMACS_MULE
            end if
        end do

        info%found = ior(info%found, found)
        ok = .true.

    contains

        ! The source ended in the middle of a sequence (the last block):
        ! rejected.
        subroutine reject_if_started()
            info%rejected = ior(info%rejected, 2**CAT_EMACS_MULE)
        end subroutine reject_if_started
    end function detect_coding_emacs_mule

    !> Whether the text is in Shift-JIS: 0x81..0x9F and 0xE0..0xEF (0xFC
    !> with a fourth charset) start two bytes, 0xA1..0xDF are kana.  Port of
    !> coding.c:detect_coding_sjis.
    logical function detect_coding_sjis(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer :: c, found, max_first
        integer(int64) :: src, nu

        info%checked = ior(info%checked, 2**CAT_SJIS)
        ok = .false.
        max_first = int(z'EF')
        if (size(codings(category_id(CAT_SJIS))%charset_ids) > 3) max_first = int(z'FC')
        nu = size(units, kind=int64)
        ! A coding system of this category is always ASCII compatible.
        src = head_ascii + 1
        found = 0

        do while (src <= nu)
            c = units(src)
            src = src + 1
            if (c < 128) cycle

            if ((c >= int(z'81') .and. c <= int(z'9F')) .or. (c >= int(z'E0') .and. c <= max_first)) then
                ! A two-byte code cut short is rejected (the last block).
                if (src > nu) then
                    info%rejected = ior(info%rejected, 2**CAT_SJIS)
                    return
                end if

                c = units(src)
                src = src + 1

                if (c < int(z'40') .or. c == int(z'7F') .or. c > int(z'FC')) then
                    info%rejected = ior(info%rejected, 2**CAT_SJIS)
                    return
                end if

                found = 2**CAT_SJIS
            else if (c >= int(z'A0') .and. c < int(z'E0')) then
                found = 2**CAT_SJIS
            else
                info%rejected = ior(info%rejected, 2**CAT_SJIS)
                return
            end if
        end do

        info%found = ior(info%found, found)
        ok = .true.
    end function detect_coding_sjis

    !> Whether the text is in Big5: 0xA1..0xFE start two bytes.  Port of
    !> coding.c:detect_coding_big5.
    logical function detect_coding_big5(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer :: c, found
        integer(int64) :: src, nu

        info%checked = ior(info%checked, 2**CAT_BIG5)
        ok = .false.
        nu = size(units, kind=int64)
        src = head_ascii + 1
        found = 0

        do while (src <= nu)
            c = units(src)
            src = src + 1
            if (c < 128) cycle

            if (c >= int(z'A1')) then
                if (src > nu) then
                    info%rejected = ior(info%rejected, 2**CAT_BIG5)
                    return
                end if

                c = units(src)
                src = src + 1
                ! An invalid second byte returns without rejecting.
                if (c < int(z'40') .or. (c >= int(z'7F') .and. c <= int(z'A0'))) return
                found = 2**CAT_BIG5
            else
                info%rejected = ior(info%rejected, 2**CAT_BIG5)
                return
            end if
        end do

        info%found = ior(info%found, found)
        ok = .true.
    end function detect_coding_big5

    !> Run the detector of CATEGORY.
    logical function run_detector(category, units, head_ascii, info) result(ok)
        integer, intent(in) :: category
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info

        ok = .false.

        select case (category)
        case (CAT_UTF_8_AUTO, CAT_UTF_8_NOSIG, CAT_UTF_8_SIG)
            ok = detect_coding_utf_8(units, head_ascii, info)
        case (CAT_UTF_16_AUTO, CAT_UTF_16_BE, CAT_UTF_16_LE, CAT_UTF_16_BE_NOSIG, CAT_UTF_16_LE_NOSIG)
            ok = detect_coding_utf_16(units, info)
        case (CAT_CHARSET)
            ok = detect_coding_charset(units, head_ascii, info)
        case (CAT_ISO_7, CAT_ISO_7_TIGHT, CAT_ISO_8_1, CAT_ISO_8_2, CAT_ISO_7_ELSE, CAT_ISO_8_ELSE)
            ok = detect_coding_iso_2022(units, head_ascii, info)
        case (CAT_EMACS_MULE)
            ok = detect_coding_emacs_mule(units, head_ascii, info)
        case (CAT_SJIS)
            ok = detect_coding_sjis(units, head_ascii, info)
        case (CAT_BIG5)
            ok = detect_coding_big5(units, head_ascii, info)
        case default
            info%rejected = ior(info%rejected, 2**category)
        end select
    end function run_detector

    !> The end-of-line format of the text: one of EOL_SEEN_*, looking at
    !> up to three line ends (UTF-16 ones for a UTF-16 category).  Port of
    !> coding.c:detect_eol.
    integer function detect_eol(units, category) result(eol_seen)
        integer, intent(in) :: units(:)
        integer, intent(in) :: category
        integer(int64) :: src, n
        integer :: c, this_eol, total, msb, lsb

        n = size(units, kind=int64)
        eol_seen = EOL_SEEN_NONE
        total = 0

        if (has(MASK_UTF_16, category)) then
            msb = 0
            if (category == CAT_UTF_16_LE .or. category == CAT_UTF_16_LE_NOSIG) msb = 1
            lsb = 1 - msb
            src = 1

            do while (src + 1 <= n)
                c = units(src + lsb)

                if (units(src + msb) == 0 .and. (c == LF .or. c == CR)) then
                    if (c == LF) then
                        this_eol = EOL_SEEN_LF
                    else if (src + 3 > n) then
                        this_eol = EOL_SEEN_CR
                    else if (units(src + msb + 2) /= 0 .or. units(src + lsb + 2) /= LF) then
                        this_eol = EOL_SEEN_CR
                    else
                        this_eol = EOL_SEEN_CRLF
                        src = src + 2
                    end if

                    if (.not. merge_eol(eol_seen, this_eol)) exit
                    total = total + 1
                    if (total == MAX_EOL_CHECK_COUNT) exit
                end if

                src = src + 2
            end do
        else
            src = 1

            do while (src <= n)
                c = units(src)
                src = src + 1

                if (c == LF .or. c == CR) then
                    if (c == LF) then
                        this_eol = EOL_SEEN_LF
                    else if (src > n) then
                        this_eol = EOL_SEEN_CR
                    else if (units(src) /= LF) then
                        this_eol = EOL_SEEN_CR
                    else
                        this_eol = EOL_SEEN_CRLF
                        src = src + 1
                    end if

                    if (.not. merge_eol(eol_seen, this_eol)) exit
                    total = total + 1
                    if (total == MAX_EOL_CHECK_COUNT) exit
                end if
            end do
        end if
    end function detect_eol

    !> Combine the line end THIS_EOL with those seen before; false when
    !> they are mixed (EOL_SEEN becomes LF and detection stops).  Stray ^M
    !> characters in DOS EOL files are allowed.
    logical function merge_eol(eol_seen, this_eol) result(go_on)
        integer, intent(inout) :: eol_seen
        integer, intent(in) :: this_eol

        go_on = .true.

        if (eol_seen == EOL_SEEN_NONE) then
            ! This is the first end-of-line.
            eol_seen = this_eol
        else if (eol_seen /= this_eol) then
            if (eol_seen == EOL_SEEN_CR .and. this_eol == EOL_SEEN_CRLF &
                .or. eol_seen == EOL_SEEN_CRLF .and. this_eol == EOL_SEEN_CR) then
                eol_seen = EOL_SEEN_CRLF
            else
                eol_seen = EOL_SEEN_LF
                go_on = .false.
            end if
        end if
    end function merge_eol

    !> The subsidiary of coding system ID for the line ends EOL_SEEN, or
    !> ID itself if it has none or none was seen.  Port of
    !> coding.c:adjust_coding_eol_type.
    integer function adjust_eol(id, eol_seen) result(adjusted)
        integer, intent(in) :: id, eol_seen

        adjusted = id
        if (codings(id)%eol /= EOL_UNDECIDED) return

        if (iand(eol_seen, EOL_SEEN_LF) /= 0) then
            adjusted = codings(id)%subsidiaries(EOL_UNIX)
        else if (iand(eol_seen, EOL_SEEN_CRLF) /= 0) then
            adjusted = codings(id)%subsidiaries(EOL_DOS)
        else if (iand(eol_seen, EOL_SEEN_CR) /= 0) then
            adjusted = codings(id)%subsidiaries(EOL_MAC)
        end if
    end function adjust_eol

    !> The subsidiary of ID with the end-of-line type EOL, if ID's is
    !> undecided.
    integer function with_eol(id, eol) result(sub)
        integer, intent(in) :: id, eol

        sub = id
        if (eol == EOL_UNDECIDED) return
        if (codings(id)%eol == EOL_UNDECIDED) sub = codings(id)%subsidiaries(eol)
    end function with_eol

    !> Scan the head of the text as detect_coding and detect_coding_system
    !> do: count the leading ASCII bytes and look for null and 8-bit
    !> bytes.  (ISO-2022 escape detection is Deferred.)
    subroutine scan_head(src, head_ascii, null_byte_found, eight_bit_found, inhibit_nbd)
        character(len=*), intent(in) :: src
        integer(int64), intent(out) :: head_ascii
        logical, intent(out) :: null_byte_found, eight_bit_found
        logical, intent(in) :: inhibit_nbd
        integer(int64) :: p
        integer :: c

        head_ascii = 0
        null_byte_found = .false.
        eight_bit_found = .false.

        do p = 1, len(src, kind=int64)
            c = iachar(src(p:p))

            if (c >= 128) then
                eight_bit_found = .true.
                if (null_byte_found) exit
            else if (c == 0 .and. .not. inhibit_nbd) then
                null_byte_found = .true.
                if (eight_bit_found) exit
                if (.not. eight_bit_found) head_ascii = head_ascii + 1
            else if (.not. eight_bit_found) then
                head_ascii = head_ascii + 1
            end if
        end do
    end subroutine scan_head

    !> Scan the ASCII head of UNITS for detection: count it in HEAD_ASCII,
    !> note null bytes and 8-bit bytes, and at the first ESC, SI or SO run
    !> the ISO-2022 detector over the rest; when that scanned the whole
    !> text without finding an 8-bit code, all of it counts as ASCII.  The
    !> head scan of coding.c:detect_coding and detect_coding_system.
    subroutine scan_head_iso(src, units, head_ascii, null_byte_found, eight_bit_found, inhibit_nbd, info)
        character(len=*), intent(in) :: src
        integer, intent(in) :: units(:)
        integer(int64), intent(out) :: head_ascii
        logical, intent(out) :: null_byte_found, eight_bit_found
        logical, intent(in) :: inhibit_nbd
        type(detect_info_type), intent(inout) :: info
        integer(int64) :: p
        integer :: c

        head_ascii = 0
        null_byte_found = .false.
        eight_bit_found = .false.

        do p = 1, len(src, kind=int64)
            c = iachar(src(p:p))

            if (c >= 128) then
                eight_bit_found = .true.
                if (null_byte_found) exit
            else if (c < 32) then
                if ((c == ISO_CODE_ESC .or. c == ISO_CODE_SI .or. c == ISO_CODE_SO) &
                    .and. .not. inhibit_iso_escape_detection%value .and. info%checked == 0) then
                    if (detect_coding_iso_2022(units, head_ascii, info)) then
                        ! We have scanned the whole data.
                        if (iand(info%rejected, 2**CAT_ISO_7_ELSE) == 0) then
                            ! We didn't find an 8-bit code.  We may have
                            ! found a null-byte, but it's very rare that a
                            ! binary file conforms to ISO-2022.
                            head_ascii = len(src, kind=int64)
                        end if

                        info%rejected = ior(info%rejected, not(MASK_ISO_ESCAPE))
                        exit
                    end if
                else if (c == 0 .and. .not. inhibit_nbd) then
                    null_byte_found = .true.
                    if (eight_bit_found) exit
                end if

                if (.not. eight_bit_found) head_ascii = head_ascii + 1
            else if (.not. eight_bit_found) then
                head_ascii = head_ascii + 1
            end if
        end do
    end subroutine scan_head_iso

    !> The coding system the text SRC is in, decided for decoding with
    !> coding system ID: the detected one for an undecided ID, or ID.  Its
    !> end-of-line type stays as ID gives it (the decoder detects it).
    !> Port of coding.c:detect_coding.
    integer function detect_for_decoding(id, units, src) result(found_id)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        character(len=*), intent(in) :: src
        type(detect_info_type) :: info
        integer(int64) :: head_ascii
        logical :: null_byte_found, eight_bit_found, hit
        integer :: i, category, this, specified_eol

        found_id = id
        specified_eol = codings(id)%eol

        if (codings(id)%ctype == CODING_UNDECIDED) then
            call scan_head_iso(src, units, head_ascii, null_byte_found, eight_bit_found, &
                               inhibit_null_byte_detection%value, info)
            if (.not. (null_byte_found .or. eight_bit_found .or. head_ascii < len(src, kind=int64) &
                       .or. info%found /= 0)) return
            this = 0
            hit = .false.

            if (head_ascii == len(src, kind=int64)) then
                ! As all bytes are 7-bit, we can ignore non-ISO-2022
                ! codings.
                do i = 0, CAT_RAW_TEXT - 1
                    category = priorities(i)
                    this = category_id(category)

                    if (has(info%found, category)) then
                        hit = .true.
                        exit
                    end if
                end do
            else
                if (null_byte_found) then
                    info%checked = ior(info%checked, not(MASK_UTF_16))
                    info%rejected = ior(info%rejected, not(MASK_UTF_16))
                else if (codings(id)%prefer_utf_8) then
                    if (detect_coding_utf_8(units, head_ascii, info)) then
                        info%checked = ior(info%checked, not(MASK_UTF_8))
                        info%rejected = ior(info%rejected, not(MASK_UTF_8))
                    end if
                end if
            end if

            ! The first CAT_RAW_TEXT positions of the priorities.
            do i = 0, CAT_RAW_TEXT - 1
                if (head_ascii == len(src, kind=int64)) exit
                category = priorities(i)
                this = category_id(category)

                if (this == 0) then
                    ! No coding system of this category is defined.
                    info%rejected = ior(info%rejected, 2**category)
                else if (category >= CAT_RAW_TEXT) then
                    cycle
                else if (has(info%checked, category)) then
                    if (has(info%found, category)) then
                        hit = .true.
                        exit
                    end if
                else if (run_detector(category, units, head_ascii, info)) then
                    if (has(info%found, category)) then
                        hit = .true.
                        exit
                    end if
                end if
            end do

            found_id = 0

            if (hit) then
                found_id = auto_choice(this, category, info)
            else if (null_byte_found) then
                found_id = coding_system_id("no-conversion")
            else if (iand(info%rejected, MASK_ANY) == MASK_ANY) then
                found_id = coding_system_id("raw-text")
            else if (info%rejected /= 0) then
                do i = 0, CAT_RAW_TEXT - 1
                    category = priorities(i)

                    if (.not. has(info%rejected, category)) then
                        found_id = category_id(category)
                        exit
                    end if
                end do
            end if

            if (found_id == 0) then
                found_id = id
                return
            end if
        else if (codings(id)%category == CAT_UTF_8_AUTO) then
            found_id = id
            call scan_head(src, head_ascii, null_byte_found, eight_bit_found, .true.)

            if (head_ascii == len(src, kind=int64)) then
                found_id = coding_system_id(codings(id)%bom_without)
            else if (detect_coding_utf_8(units, 0_int64, info)) then
                if (has(info%found, CAT_UTF_8_SIG)) then
                    found_id = coding_system_id(codings(id)%bom_with)
                else
                    found_id = coding_system_id(codings(id)%bom_without)
                end if
            end if
        else if (codings(id)%category == CAT_UTF_16_AUTO) then
            found_id = id

            if (detect_coding_utf_16(units, info)) then
                if (has(info%found, CAT_UTF_16_LE)) then
                    found_id = coding_system_id(codings(id)%bom_with)
                else if (has(info%found, CAT_UTF_16_BE)) then
                    found_id = coding_system_id(codings(id)%bom_without)
                end if
            end if

            if (found_id == id) return
        else
            return
        end if

        if (specified_eol /= EOL_UNDECIDED) found_id = with_eol(found_id, specified_eol)
    end function detect_for_decoding

    !> The coding system of the category found: for the auto categories,
    !> the one with or without a BOM (or of the endianness) found.
    integer function auto_choice(this, category, info) result(id)
        integer, intent(in) :: this, category
        type(detect_info_type), intent(in) :: info

        id = this

        if (category == CAT_UTF_8_AUTO) then
            if (codings(this)%bom == BOM_DETECT) then
                if (has(info%found, CAT_UTF_8_SIG)) then
                    id = coding_system_id(codings(this)%bom_with)
                else
                    id = coding_system_id(codings(this)%bom_without)
                end if
            end if
        else if (category == CAT_UTF_16_AUTO) then
            if (codings(this)%bom == BOM_DETECT) then
                if (has(info%found, CAT_UTF_16_LE)) then
                    id = coding_system_id(codings(this)%bom_with)
                else if (has(info%found, CAT_UTF_16_BE)) then
                    id = coding_system_id(codings(this)%bom_without)
                end if
            end if
        end if
    end function auto_choice

    !> Detect coding system of the text SRC (bytes, or Emacs's internal
    !> encoding if MULTIBYTE).
    !> Return a list of possible coding systems ordered by priority.
    !> The coding systems to try and their priorities follows what
    !> the function `coding_system_priority_list' (which see) returns.
    !>
    !> If only ASCII characters are found (except for such ISO-2022 control
    !> characters as ESC), it returns a list of single element `undecided'
    !> or its subsidiary coding system according to a detected end-of-line
    !> format.
    !>
    !> If optional argument HIGHEST is true, return the coding system of
    !> highest priority.  Port of coding.c:detect_coding_system (used by
    !> Fdetect_coding_string and Fdetect_coding_region).
    subroutine detect_coding_bytes(src, multibyte, names, highest)
        character(len=*), intent(in) :: src
        logical, intent(in) :: multibyte
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        logical, intent(in), optional :: highest
        integer, allocatable :: units(:), ids(:)
        type(detect_info_type) :: info
        integer(int64) :: head_ascii
        logical :: null_byte_found, eight_bit_found, top, hit
        integer :: i, category, this, n, mask, found, k, normal_eol, be_eol, le_eol, this_eol

        if (ncodings == 0) call syms_of_coding()
        top = .false.
        if (present(highest)) top = highest
        call source_units(src, multibyte, units)
        allocate (ids(NCAT))
        n = 0
        call scan_head_iso(src, units, head_ascii, null_byte_found, eight_bit_found, &
                           inhibit_null_byte_detection%value, info)
        category = CAT_UNDECIDED
        this = 0
        hit = .false.

        if (null_byte_found .or. eight_bit_found .or. head_ascii < len(src, kind=int64) .or. info%found /= 0) then
            if (head_ascii == len(src, kind=int64)) then
                ! As all bytes are 7-bit, we can ignore non-ISO-2022 codings.
                do i = 0, CAT_RAW_TEXT - 1
                    category = priorities(i)
                    this = category_id(category)

                    if (has(info%found, category)) then
                        hit = .true.
                        exit
                    end if
                end do
            else
                if (null_byte_found) then
                    info%checked = ior(info%checked, not(MASK_UTF_16))
                    info%rejected = ior(info%rejected, not(MASK_UTF_16))
                end if

                do i = 0, CAT_RAW_TEXT - 1
                    category = priorities(i)
                    this = category_id(category)

                    if (this == 0) then
                        info%rejected = ior(info%rejected, 2**category)
                    else if (category >= CAT_RAW_TEXT) then
                        cycle
                    else if (has(info%checked, category)) then
                        if (top .and. has(info%found, category)) then
                            hit = .true.
                            exit
                        end if
                    else if (run_detector(category, units, head_ascii, info)) then
                        if (top .and. has(info%found, category)) then
                            if (category == CAT_UTF_16_AUTO) then
                                if (has(info%found, CAT_UTF_16_LE)) then
                                    category = CAT_UTF_16_LE
                                else
                                    category = CAT_UTF_16_BE
                                end if

                                this = category_id(category)
                            end if

                            hit = .true.
                            exit
                        end if
                    end if
                end do
            end if
        end if

        if (iand(info%rejected, MASK_ANY) == MASK_ANY .or. null_byte_found) then
            info%found = 2**CAT_RAW_TEXT
            n = 1
            ids(1) = coding_system_id("no-conversion")
        else if (info%rejected == 0 .and. info%found == 0) then
            info%found = MASK_ANY
            n = 1
            ids(1) = category_id(CAT_UNDECIDED)
        else if (top) then
            if (info%found /= 0 .and. hit) then
                info%found = 2**category
                n = 1
                ids(1) = this
            else
                do i = 0, CAT_RAW_TEXT - 1
                    if (.not. has(info%rejected, priorities(i))) then
                        info%found = 2**priorities(i)
                        n = 1
                        ids(1) = category_id(priorities(i))
                        exit
                    end if
                end do
            end if
        else
            mask = ior(info%rejected, info%found)
            found = 0

            ! The categories found, in priority order, then one that is
            ! neither rejected nor found.
            do i = 0, CAT_RAW_TEXT - 1
                category = priorities(i)

                if (has(info%found, category)) then
                    if (category_id(category) > 0) then
                        n = n + 1
                        ids(n) = category_id(category)
                    end if
                end if
            end do

            ! Only the first of those that are neither stays in the list:
            ! each replaces the one before (list1i in the loop upstream).
            k = 0

            do i = CAT_RAW_TEXT - 1, 0, -1
                category = priorities(i)

                if (.not. has(mask, category)) then
                    found = ibset(found, category)
                    if (category_id(category) > 0) k = category_id(category)
                end if
            end do

            if (k > 0) then
                n = n + 1
                ids(n) = k
            end if

            info%found = ior(info%found, found)
        end if

        ! Then, detect eol-format if necessary.
        normal_eol = -1
        be_eol = -1
        le_eol = -1

        if (iand(info%found, not(MASK_UTF_16)) /= 0) then
            if (null_byte_found) then
                normal_eol = EOL_SEEN_LF
            else
                normal_eol = detect_eol(units, CAT_RAW_TEXT)
            end if
        end if

        if (iand(info%found, 2**CAT_UTF_16_BE + 2**CAT_UTF_16_BE_NOSIG) /= 0) be_eol = detect_eol(units, CAT_UTF_16_BE)
        if (iand(info%found, 2**CAT_UTF_16_LE + 2**CAT_UTF_16_LE_NOSIG) /= 0) le_eol = detect_eol(units, CAT_UTF_16_LE)
        allocate (names(n))

        do k = 1, n
            category = codings(ids(k))%category

            if (category == CAT_UTF_16_BE .or. category == CAT_UTF_16_BE_NOSIG) then
                this_eol = be_eol
            else if (category == CAT_UTF_16_LE .or. category == CAT_UTF_16_LE_NOSIG) then
                this_eol = le_eol
            else
                this_eol = normal_eol
            end if

            if (codings(ids(k))%eol == EOL_UNDECIDED .and. this_eol > 0) then
                names(k)%name = codings(adjust_eol(ids(k), this_eol))%name
            else
                names(k)%name = codings(ids(k))%name
            end if
        end do
    end subroutine detect_coding_bytes

    ! ------------------------------------------------------------------
    ! Decoding
    ! ------------------------------------------------------------------

    !> Append character C to CHARS(1:N).
    subroutine push(chars, n, c)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        integer, intent(in) :: c
        integer, allocatable :: grown(:)

        if (n == size(chars, kind=int64)) then
            allocate (grown(max(16_int64, 2*n)))
            grown(1:n) = chars(1:n)
            call move_alloc(grown, chars)
        end if

        n = n + 1
        chars(n) = c
    end subroutine push

    !> A byte that could not be decoded, as a character: ASCII as is,
    !> others as raw bytes; a character of multibyte source stays.
    integer function invalid_char(u) result(c)
        integer, intent(in) :: u

        if (u < 0) then
            c = -u
        else if (u < 128) then
            c = u
        else
            c = byte8_to_char(u)
        end if
    end function invalid_char

    !> Decode UTF-8.  Port of coding.c:decode_coding_utf_8.
    subroutine decode_utf_8(id, units, chars, n)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        integer(int64) :: src, nu, base
        integer :: c, c1, k, need, b(4)
        logical :: bad

        nu = size(units, kind=int64)
        src = 1

        if (codings(id)%bom /= BOM_NONE .and. nu >= 3) then
            if (units(1) == int(z'EF') .and. units(2) == int(z'BB') .and. units(3) == int(z'BF')) src = 4
        end if

        do while (src <= nu)
            base = src
            c1 = units(src)
            src = src + 1

            if (c1 < 0) then
                call push(chars, n, -c1)
                cycle
            end if

            if (c1 < 128) then
                call push(chars, n, c1)
                cycle
            end if

            if (iand(c1, int(z'E0')) == int(z'C0')) then
                need = 1
            else if (iand(c1, int(z'F0')) == int(z'E0')) then
                need = 2
            else if (iand(c1, int(z'F8')) == int(z'F0')) then
                need = 3
            else if (iand(c1, int(z'FC')) == int(z'F8')) then
                need = 4
            else
                need = 0
            end if

            bad = need == 0

            do k = 1, need
                if (bad) exit

                if (src > nu) then
                    bad = .true.
                else if (units(src) < 0 .or. iand(units(src), int(z'C0')) /= int(z'80')) then
                    bad = .true.
                else
                    b(k) = iand(units(src), int(z'3F'))
                    src = src + 1
                end if
            end do

            if (.not. bad) then
                select case (need)
                case (1)
                    c = ior(ishft(iand(c1, int(z'1F')), 6), b(1))
                    ! Reject overlong sequences here and below.
                    bad = c < 128
                case (2)
                    c = ior(ior(ishft(iand(c1, int(z'F')), 12), ishft(b(1), 6)), b(2))
                    ! Surrogates are invalid.
                    bad = c < int(z'800') .or. (c >= int(z'D800') .and. c < int(z'E000'))
                case (3)
                    c = ior(ior(ior(ishft(iand(c1, 7), 18), ishft(b(1), 12)), ishft(b(2), 6)), b(3))
                    bad = c < int(z'10000')
                case default
                    c = ior(ior(ior(ior(ishft(iand(c1, 3), 24), ishft(b(1), 18)), ishft(b(2), 12)), &
                                ishft(b(3), 6)), b(4))
                    bad = c > MAX_CHAR .or. c < int(z'200000')
                end select
            end if

            if (bad) then
                ! invalid_code: the first byte alone, as a raw byte.
                src = base + 1
                call push(chars, n, invalid_char(c1))
            else
                call push(chars, n, c)
            end if
        end do
    end subroutine decode_utf_8

    !> Decode UTF-16.  Port of coding.c:decode_coding_utf_16.
    subroutine decode_utf_16(id, units, chars, n)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        integer(int64) :: src, nu
        integer :: c, c1, c2, surrogate
        logical :: big

        nu = size(units, kind=int64)
        big = codings(id)%big_endian
        src = 1
        surrogate = 0

        if (codings(id)%bom == BOM_WITH .and. nu >= 2) then
            if (units(1) >= 0 .and. units(2) >= 0) then
                c = ior(ishft(units(1), 8), units(2))
                if (big .and. c == int(z'FEFF') .or. .not. big .and. c == int(z'FFFE')) src = 3
            end if
        end if

        do while (src <= nu)
            c1 = units(src)
            src = src + 1

            if (c1 < 0) then
                call push(chars, n, -c1)
                cycle
            end if

            if (src > nu) then
                call push(chars, n, invalid_char(c1))
                exit
            end if

            c2 = units(src)
            src = src + 1

            if (c2 < 0) then
                call push(chars, n, invalid_char(c1))
                call push(chars, n, -c2)
                cycle
            end if

            if (big) then
                c = ior(ishft(c1, 8), c2)
            else
                c = ior(ishft(c2, 8), c1)
            end if

            if (surrogate /= 0) then
                if (.not. (c >= int(z'DC00') .and. c <= int(z'DFFF'))) then
                    if (big) then
                        call push(chars, n, ishft(surrogate, -8))
                        call push(chars, n, iand(surrogate, 255))
                    else
                        call push(chars, n, iand(surrogate, 255))
                        call push(chars, n, ishft(surrogate, -8))
                    end if

                    if (c >= int(z'D800') .and. c <= int(z'DBFF')) then
                        surrogate = c
                    else
                        surrogate = 0
                        call push(chars, n, c)
                    end if
                else
                    c = ior(ishft(surrogate - int(z'D800'), 10), c - int(z'DC00'))
                    surrogate = 0
                    call push(chars, n, int(z'10000') + c)
                end if
            else if (c >= int(z'D800') .and. c <= int(z'DBFF')) then
                surrogate = c
            else
                call push(chars, n, c)
            end if
        end do

        ! A high surrogate at the end: its bytes, as decode_coding flushes
        ! unprocessed data.
        if (surrogate /= 0) then
            if (big) then
                call push(chars, n, invalid_char(ishft(surrogate, -8)))
                call push(chars, n, invalid_char(iand(surrogate, 255)))
            else
                call push(chars, n, invalid_char(iand(surrogate, 255)))
                call push(chars, n, invalid_char(ishft(surrogate, -8)))
            end if
        end if
    end subroutine decode_utf_16

    !> Decode with the charsets of coding system ID: each code point
    !> starts with a byte of their valids; the bytes of a code point that
    !> is not valid are decoded as raw bytes, as are those of an incomplete
    !> one at the end.  Port of coding.c:decode_coding_charset (with
    !> decode_coding's flushing of the rest).
    subroutine decode_charset(id, units, chars, n, status)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        type(status_type), intent(inout) :: status
        integer(int64) :: src, src_base, nu, code
        integer :: c, k, cs, dim, length
        logical :: incomplete

        nu = size(units, kind=int64)
        src = 1

        do while (src <= nu)
            src_base = src
            c = units(src)
            src = src + 1
            incomplete = .false.

            if (c >= 0 .and. c <= 255) then
                if (codings(id)%nvalids(c) > 0) then
                    code = c
                    length = 1

                    ! The charsets are sorted by dimension: read the bytes
                    ! each needs.
                    do k = 1, codings(id)%nvalids(c)
                        cs = codings(id)%valids(k, c)
                        dim = charset_dimension_of(cs)

                        do while (length < dim)
                            if (src > nu) then
                                incomplete = .true.
                                exit
                            end if

                            code = ior(ishft(code, 8), int(units(src), int64))
                            src = src + 1
                            length = length + 1
                        end do

                        if (incomplete) exit
                        c = charset_decode_id(cs, code, status)
                        if (status%code /= 0) return
                        if (c >= 0) exit
                    end do

                    if (incomplete) then
                        ! Flush out unprocessed data as binary chars.
                        do src = src_base, nu
                            call push(chars, n, invalid_char(units(src)))
                        end do

                        return
                    end if

                    if (c >= 0) then
                        call push(chars, n, c)
                        cycle
                    end if
                end if
            end if

            src = src_base + 1
            call push(chars, n, invalid_char(units(src_base)))
        end do
    end subroutine decode_charset

    !> Decode Shift-JIS with the charsets of coding system ID (roman, kana,
    !> kanji and an optional second kanji charset).  Port of
    !> coding.c:decode_coding_sjis.
    subroutine decode_sjis(id, units, chars, n, status)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        type(status_type), intent(inout) :: status
        integer(int64) :: src, nu
        integer :: c, c1, cs, roman, kana, kanji, kanji2

        roman = codings(id)%charset_ids(1)
        kana = codings(id)%charset_ids(2)
        kanji = codings(id)%charset_ids(3)
        kanji2 = 0
        if (size(codings(id)%charset_ids) > 3) kanji2 = codings(id)%charset_ids(4)
        nu = size(units, kind=int64)
        src = 1

        do while (src <= nu)
            c = units(src)
            src = src + 1
            cs = 0

            if (c < 0) then
                cs = 0
            else if (c < 128) then
                cs = roman
            else if (c == 128 .or. c == int(z'A0')) then
                cs = 0
            else if (c >= int(z'A1') .and. c <= int(z'DF')) then
                ! SJIS -> JISX0201-Kana
                c = iand(c, 127)
                cs = kana
            else if (c <= int(z'FC') .and. (c <= int(z'EF') .or. kanji2 /= 0)) then
                ! An incomplete code at the end is left to the final
                ! flushing as a raw byte.
                if (src > nu) then
                    call push(chars, n, invalid_char(c))
                    exit
                end if

                c1 = units(src)

                if (.not. (c1 < int(z'40') .or. c1 == int(z'7F') .or. c1 > int(z'FC'))) then
                    src = src + 1

                    if (c <= int(z'EF')) then
                        ! SJIS -> JISX0208
                        c = sjis_to_jis(ior(ishft(c, 8), c1))
                        cs = kanji
                    else
                        ! SJIS -> JISX0213-2
                        c = sjis_to_jis2(ior(ishft(c, 8), c1))
                        cs = kanji2
                    end if
                end if
            end if

            if (cs == 0) then
                call push(chars, n, invalid_char(units(src - 1)))
                cycle
            end if

            c = charset_decode_id(cs, int(c, int64), status)
            if (status%code /= 0) return
            call push(chars, n, c)
        end do
    end subroutine decode_sjis

    !> Decode Big5 with the charsets of coding system ID (roman and Big5).
    !> Port of coding.c:decode_coding_big5.
    subroutine decode_big5(id, units, chars, n, status)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        type(status_type), intent(inout) :: status
        integer(int64) :: src, nu
        integer :: c, c1, cs

        nu = size(units, kind=int64)
        src = 1

        do while (src <= nu)
            c = units(src)
            src = src + 1
            cs = 0

            if (c >= 0 .and. c < 128) then
                cs = codings(id)%charset_ids(1)
            else if (c >= int(z'A1') .and. c <= int(z'FE')) then
                if (src > nu) then
                    call push(chars, n, invalid_char(c))
                    exit
                end if

                c1 = units(src)

                if (.not. (c1 < int(z'40') .or. (c1 > int(z'7E') .and. c1 < int(z'A1')) .or. c1 > int(z'FE'))) then
                    src = src + 1
                    c = ior(ishft(c, 8), c1)
                    cs = codings(id)%charset_ids(2)
                end if
            end if

            if (cs == 0) then
                call push(chars, n, invalid_char(units(src - 1)))
                cycle
            end if

            c = charset_decode_id(cs, int(c, int64), status)
            if (status%code /= 0) return
            call push(chars, n, c)
        end do
    end subroutine decode_big5

    !> Decode ISO-2022 with coding system ID: the graphic registers G0..G3
    !> start with its initial designations and change by designation
    !> sequences, locking shifts invoke them to the graphic planes, single
    !> shifts to one character.  Composition sequences are decoded as
    !> without composition support (compositions are Deferred), CTEXT
    !> extended segments and embedded UTF-8 are kept as they are.  Port of
    !> coding.c:decode_coding_iso_2022 (with decode_coding's flushing of an
    !> incomplete sequence at the end as raw bytes).
    subroutine decode_iso_2022(id, units, chars, n, status)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        type(status_type), intent(inout) :: status
        integer :: invocation(0:1), designation(0:3)
        integer(int64) :: src, src_base, nu, k
        integer :: c, c1, c2, c3, cs, charset_id_0, charset_id_1, flags, extsegment_len, reg, chars96
        integer :: dim, m, l, size_
        logical :: embedded_utf_8, ok

        flags = codings(id)%iso_flags
        invocation(0) = 0
        invocation(1) = merge(-1, 1, iand(flags, ISO_SEVEN_BITS) /= 0)
        designation = codings(id)%iso_initial
        where (designation == 0) designation = -1
        charset_id_0 = invoked(0)
        charset_id_1 = invoked(1)
        extsegment_len = 0
        embedded_utf_8 = .false.
        nu = size(units, kind=int64)
        src = 1

        main: do while (src <= nu)
            src_base = src
            c1 = units(src)
            src = src + 1

            if (c1 < 0) then
                call invalid()
                cycle main
            end if

            if (extsegment_len > 0) then
                call push(chars, n, invalid_char(c1))
                extsegment_len = extsegment_len - 1
                cycle main
            end if

            if (embedded_utf_8) then
                if (c1 == ISO_CODE_ESC) then
                    ! Not enough source for the end sequence: flushed.
                    if (src + 1 > nu) then
                        call flush_rest()
                        exit main
                    end if

                    call push(chars, n, ISO_CODE_ESC)

                    if (units(src) == iachar("%") .and. units(src + 1) == iachar("@")) then
                        src = src + 2
                        call push(chars, n, iachar("%"))
                        call push(chars, n, iachar("@"))
                        embedded_utf_8 = .false.
                    end if
                else
                    call push(chars, n, invalid_char(c1))
                end if

                cycle main
            end if

            cs = 0

            ! We produce at most one character.
            if (c1 == 32 .or. c1 == 127) then
                if (charset_id_0 < 0) then
                    cs = charset_ascii
                else if (.not. charset_iso_chars_96_of(charset_id_0)) then
                    ! This is SPACE or DEL.
                    cs = charset_ascii
                else
                    cs = charset_id_0
                end if
            else if (c1 > 32 .and. c1 < 127) then
                cs = charset_id_0
                if (charset_id_0 < 0) cs = charset_ascii
            else if (c1 == int(z'A0') .or. c1 == 255) then
                if (charset_id_1 < 0) then
                    call invalid()
                    cycle main
                else if (.not. charset_iso_chars_96_of(charset_id_1) .or. iand(flags, ISO_SEVEN_BITS) /= 0) then
                    call invalid()
                    cycle main
                end if

                cs = charset_id_1
            else if (c1 > int(z'A0') .and. c1 < 255) then
                if (charset_id_1 < 0) then
                    call invalid()
                    cycle main
                end if

                cs = charset_id_1
            else if (c1 == ISO_CODE_SO) then
                if (iand(flags, ISO_LOCKING_SHIFT) == 0 .or. designation(1) < 0) then
                    call invalid()
                    cycle main
                end if

                invocation(0) = 1
                charset_id_0 = invoked(0)
                cycle main
            else if (c1 == ISO_CODE_SI) then
                if (iand(flags, ISO_LOCKING_SHIFT) == 0) then
                    call invalid()
                    cycle main
                end if

                invocation(0) = 0
                charset_id_0 = invoked(0)
                cycle main
            else if (c1 == ISO_CODE_SS2_7 .or. c1 == ISO_CODE_SS2) then
                if (c1 == ISO_CODE_SS2_7 .and. iand(flags, ISO_SEVEN_BITS) == 0) then
                    call invalid()
                    cycle main
                end if

                if (iand(flags, ISO_SINGLE_SHIFT) == 0) then
                    call invalid()
                    cycle main
                end if

                ! SS2 is handled as an escape sequence of ESC 'N'.
                c1 = iachar("N")
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (c1 == ISO_CODE_SS3) then
                if (iand(flags, ISO_SINGLE_SHIFT) == 0) then
                    call invalid()
                    cycle main
                end if

                c1 = iachar("O")
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (c1 == ISO_CODE_CSI) then
                ! CSI is handled as an escape sequence of ESC '[' ...
                c1 = iachar("[")
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (c1 == ISO_CODE_ESC) then
                if (.not. more(c1)) exit main
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (c1 < 32) then
                ! A control character (the line ends are decoded later).
                cs = charset_ascii
            else
                ! ISO_control_1.
                call invalid()
                cycle main
            end if

            ! Now we know CHARSET and 1st position code C1 of a character.
            ! Produce a decoded character while getting 2nd and 3rd
            ! position codes C2, C3 if necessary.
            if (charset_dimension_of(cs) > 1) then
                if (.not. more(c2)) exit main

                if (c2 < 32 .or. (c2 >= 128 .and. c2 < int(z'A0')) .or. iand(c1, 128) /= iand(c2, 128)) then
                    ! C2 is not in a valid range.
                    call invalid()
                    cycle main
                end if

                if (charset_dimension_of(cs) == 2) then
                    c1 = ior(ishft(c1, 8), c2)
                else
                    if (.not. more(c3)) exit main

                    if (c3 < 32 .or. (c3 >= 128 .and. c3 < int(z'A0')) .or. iand(c1, 128) /= iand(c3, 128)) then
                        ! C3 is not in a valid range.
                        call invalid()
                        cycle main
                    end if

                    ! As upstream, the third byte is C2 again.
                    c1 = ior(ior(ishft(c1, 16), ishft(c2, 8)), c2)
                end if
            end if

            c1 = iand(c1, int(z'7F7F7F'))
            c = charset_decode_id(cs, int(c1, int64), status)
            if (status%code /= 0) return

            if (c < 0) then
                do k = src_base, src - 1
                    call push(chars, n, invalid_char(units(k)))
                end do
            else
                call push(chars, n, c)
            end if
        end do main

    contains

        integer function invoked(plane)
            integer, intent(in) :: plane

            invoked = -1
            if (invocation(plane) >= 0) invoked = designation(invocation(plane))
        end function invoked

        ! ONE_MORE_BYTE: false at the end of the source, whose bytes from
        ! SRC_BASE on are then flushed as raw bytes.
        logical function more(b)
            integer, intent(out) :: b

            b = 0
            more = src <= nu

            if (.not. more) then
                call flush_rest()
                return
            end if

            b = units(src)
            src = src + 1
        end function more

        subroutine flush_rest()
            do k = src_base, nu
                call push(chars, n, invalid_char(units(k)))
            end do
        end subroutine flush_rest

        ! The byte at SRC_BASE as a raw byte, and the safest state: ASCII
        ! designated to G0, invoked to graphic plane 0.
        subroutine invalid()
            src = src_base + 1
            call push(chars, n, invalid_char(units(src_base)))
            invocation(0) = 0
            designation(0) = charset_ascii
            charset_id_0 = charset_ascii
        end subroutine invalid

        ! Handle the escape sequence (or SS2, SS3, CSI) whose final byte is
        ! C1: false at the end of the source; OK false when it was invalid
        ! or produced no character, else CS is the charset of the
        ! character whose first byte is then in C1.
        recursive logical function escape_sequence(ok) result(more_source)
            logical, intent(out) :: ok

            more_source = .true.
            ok = .false.

            select case (c1)
            case (iachar("&"))
                ! Revision of following character set.
                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                if (.not. (c1 >= iachar("@") .and. c1 <= iachar("~"))) then
                    call invalid()
                    return
                end if

                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                if (c1 /= ISO_CODE_ESC) then
                    call invalid()
                    return
                end if

                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                more_source = escape_sequence(ok)
            case (iachar("$"))
                ! Designation of 2-byte character set.
                if (iand(flags, ISO_DESIGNATION) == 0) then
                    call invalid()
                    return
                end if

                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                if (c1 >= iachar("@") .and. c1 <= iachar("B")) then
                    ! Designation of JISX0208.1978, GB2312.1980, or
                    ! JISX0208.1980.
                    reg = 0
                    chars96 = 0
                else if (c1 >= int(z'28') .and. c1 <= int(z'2B')) then
                    reg = c1 - int(z'28')
                    chars96 = 0

                    if (.not. more(c1)) then
                        more_source = .false.
                        return
                    end if
                else if (c1 >= int(z'2C') .and. c1 <= int(z'2F')) then
                    reg = c1 - int(z'2C')
                    chars96 = 1

                    if (.not. more(c1)) then
                        more_source = .false.
                        return
                    end if
                else
                    call invalid()
                    return
                end if

                call decode_designation(reg, 2, chars96, c1)
                if (reg == 0) charset_id_0 = invoked(0)
                if (reg == 1) charset_id_1 = invoked(1)
                if (chars96 < 0) call invalid()
            case (iachar("n"), iachar("o"))
                ! Invocation of locking-shift-2 or -3.
                reg = merge(2, 3, c1 == iachar("n"))

                if (iand(flags, ISO_LOCKING_SHIFT) == 0 .or. designation(reg) < 0) then
                    call invalid()
                    return
                end if

                invocation(0) = reg
                charset_id_0 = invoked(0)
            case (iachar("N"), iachar("O"))
                ! Invocation of single-shift-2 or -3.
                reg = merge(2, 3, c1 == iachar("N"))

                if (iand(flags, ISO_SINGLE_SHIFT) == 0 .or. designation(reg) < 0) then
                    call invalid()
                    return
                end if

                cs = designation(reg)

                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                if (c1 < 32 .or. (c1 >= 128 .and. c1 < int(z'A0'))) then
                    call invalid()
                    return
                end if

                if (iand(flags, ISO_SEVEN_BITS) == 0) then
                    if (iand(flags, ISO_LEVEL_4) /= 0) then
                        if (c1 >= 128) then
                            call invalid()
                            return
                        end if
                    else if (c1 < 128) then
                        call invalid()
                        return
                    end if
                end if

                ok = .true.
            case (iachar("0"), iachar("1"), iachar("2"), iachar("3"), iachar("4"))
                ! Compositions (CODING_ANNOTATE_COMPOSITION_MASK unset).
                call invalid()
            case (iachar("["))
                ! Specification of direction.
                if (iand(flags, ISO_DIRECTION) == 0) then
                    call invalid()
                    return
                end if

                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                select case (c1)
                case (iachar("]"))
                case (iachar("0"), iachar("1"), iachar("2"))
                    if (.not. more(c1)) then
                        more_source = .false.
                        return
                    end if

                    if (c1 /= iachar("]")) call invalid()
                case default
                    call invalid()
                end select
            case (iachar("%"))
                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                if (c1 == iachar("/")) then
                    ! CTEXT extended segment: ESC % / [0-4] M L
                    ! --ENCODING-NAME-- \002 --BYTES--.  We keep these
                    ! bytes as is.
                    if (.not. more(dim)) then
                        more_source = .false.
                        return
                    end if

                    if (dim < iachar("0") .or. dim > iachar("4")) then
                        call invalid()
                        return
                    end if

                    if (.not. more(m)) then
                        more_source = .false.
                        return
                    end if

                    if (m < 128) then
                        call invalid()
                        return
                    end if

                    if (.not. more(l)) then
                        more_source = .false.
                        return
                    end if

                    if (l < 128) then
                        call invalid()
                        return
                    end if

                    size_ = (m - 128)*128 + (l - 128)
                    call push(chars, n, ISO_CODE_ESC)
                    call push(chars, n, iachar("%"))
                    call push(chars, n, iachar("/"))
                    call push(chars, n, dim)
                    call push(chars, n, byte8_to_char(m))
                    call push(chars, n, byte8_to_char(l))
                    extsegment_len = size_
                else if (c1 == iachar("G")) then
                    ! XFree86 extension for embedding UTF-8 in CTEXT: ESC % G
                    ! --UTF-8-BYTES-- ESC % @.  We keep these bytes as is.
                    call push(chars, n, ISO_CODE_ESC)
                    call push(chars, n, iachar("%"))
                    call push(chars, n, iachar("G"))
                    embedded_utf_8 = .true.
                else
                    call invalid()
                end if
            case default
                if (iand(flags, ISO_DESIGNATION) == 0) then
                    call invalid()
                    return
                end if

                if (c1 >= int(z'28') .and. c1 <= int(z'2B')) then
                    ! Designation of DIMENSION1_CHARS94 character set.
                    reg = c1 - int(z'28')
                    chars96 = 0
                else if (c1 >= int(z'2C') .and. c1 <= int(z'2F')) then
                    ! Designation of DIMENSION1_CHARS96 character set.
                    reg = c1 - int(z'2C')
                    chars96 = 1
                else
                    call invalid()
                    return
                end if

                if (.not. more(c1)) then
                    more_source = .false.
                    return
                end if

                call decode_designation(reg, 1, chars96, c1)
                if (reg == 0) charset_id_0 = invoked(0)
                if (reg == 1) charset_id_1 = invoked(1)
                if (chars96 < 0) call invalid()
            end select
        end function escape_sequence

        ! Designate the charset of DIM, CHARS96 and FINAL to register REG;
        ! CHARS96 becomes -1 when the escape sequence should be kept.
        ! Port of coding.c:DECODE_DESIGNATION.
        subroutine decode_designation(reg, dim, chars96, final)
            integer, intent(in) :: reg, dim, final
            integer, intent(inout) :: chars96
            integer :: cid, prev

            cid = 0
            if (final >= iachar("0") .and. final < 128) cid = iso_charset_id(dim, chars96 == 1, final)

            if (cid == 0) then
                designation(reg) = -2
                chars96 = -1
                return
            end if

            if (.not. safe_charset_p(id, cid)) then
                designation(reg) = -2
                chars96 = -1
                return
            end if

            prev = designation(reg)

            if (cid == charset_jisx0201_roman) then
                if (iand(flags, ISO_USE_ROMAN) /= 0) cid = charset_ascii
            else if (cid == charset_jisx0208_1978) then
                if (iand(flags, ISO_USE_OLDJIS) /= 0) cid = charset_jisx0208
            end if

            designation(reg) = cid
            ! If there was an invalid designation to REG previously, and
            ! this designation is ASCII to REG, we should keep this
            ! designation sequence.
            if (prev == -2 .and. cid == charset_ascii) chars96 = -1
        end subroutine decode_designation
    end subroutine decode_iso_2022

    !> Decode emacs-mule: a leading code (with the extended leading code of
    !> a private charset) and the position codes of 0xA0 and above.  The
    !> compositions 0x80 starts are Deferred: 0x80 decodes as a raw byte.
    !> Port of coding.c:decode_coding_emacs_mule and emacs_mule_char (with
    !> decode_coding's flushing of an incomplete sequence at the end).
    subroutine decode_emacs_mule(units, chars, n, status)
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        type(status_type), intent(inout) :: status
        integer(int64) :: src, src_base, nu
        integer :: c, cs, code

        nu = size(units, kind=int64)
        src = 1

        main: do while (src <= nu)
            src_base = src
            c = units(src)
            src = src + 1

            if (c < 0) then
                call push(chars, n, -c)
                cycle main
            end if

            if (c < 128) then
                call push(chars, n, c)
                cycle main
            end if

            ! emacs_mule_char.
            cs = 0

            if (c == 128 .or. c >= int(z'A0')) then
                call invalid()
                cycle main
            end if

            select case (emacs_mule_bytes_of(c))
            case (2)
                cs = emacs_mule_charset_of(c)

                if (cs == 0) then
                    call invalid()
                    cycle main
                end if

                if (.not. more(c)) exit main

                if (c < int(z'A0')) then
                    call invalid()
                    cycle main
                end if

                code = iand(c, 127)
            case (3)
                if (c == int(z'9A') .or. c == int(z'9B')) then
                    if (.not. more(c)) exit main
                    if (c >= int(z'A0')) cs = emacs_mule_charset_of(c)

                    if (cs == 0) then
                        call invalid()
                        cycle main
                    end if

                    if (.not. more(c)) exit main

                    if (c < int(z'A0')) then
                        call invalid()
                        cycle main
                    end if

                    code = iand(c, 127)
                else
                    cs = emacs_mule_charset_of(c)

                    if (cs == 0) then
                        call invalid()
                        cycle main
                    end if

                    if (.not. more(c)) exit main

                    if (c < int(z'A0')) then
                        call invalid()
                        cycle main
                    end if

                    code = ishft(iand(c, 127), 8)
                    if (.not. more(c)) exit main

                    if (c < int(z'A0')) then
                        call invalid()
                        cycle main
                    end if

                    code = ior(code, iand(c, 127))
                end if
            case (4)
                if (.not. more(c)) exit main
                if (c >= 0) cs = emacs_mule_charset_of(c)

                if (cs == 0) then
                    call invalid()
                    cycle main
                end if

                if (.not. more(c)) exit main

                if (c < int(z'A0')) then
                    call invalid()
                    cycle main
                end if

                code = ishft(iand(c, 127), 8)
                if (.not. more(c)) exit main

                if (c < int(z'A0')) then
                    call invalid()
                    cycle main
                end if

                code = ior(code, iand(c, 127))
            case default
                code = c
                cs = charset_eight_bit
            end select

            c = charset_decode_id(cs, int(code, int64), status)
            if (status%code /= 0) return

            if (c < 0) then
                call invalid()
                cycle main
            end if

            call push(chars, n, c)
        end do main

    contains

        logical function more(b)
            integer, intent(out) :: b
            integer(int64) :: k

            b = 0
            more = src <= nu

            if (.not. more) then
                ! Flush out unprocessed data as binary chars.
                do k = src_base, nu
                    call push(chars, n, invalid_char(units(k)))
                end do

                return
            end if

            b = units(src)
            src = src + 1
        end function more

        subroutine invalid()
            src = src_base + 1
            call push(chars, n, invalid_char(units(src_base)))
        end subroutine invalid
    end subroutine decode_emacs_mule

    ! Port of coding.h:SJIS_TO_JIS.
    integer function sjis_to_jis(code) result(j)
        integer, intent(in) :: code
        integer :: s1, s2, j1, j2

        s1 = ishft(code, -8)
        s2 = iand(code, 255)

        if (s2 >= int(z'9F')) then
            j1 = s1*2 - merge(int(z'160'), int(z'E0'), s1 >= int(z'E0'))
            j2 = s2 - int(z'7E')
        else
            j1 = s1*2 - merge(int(z'161'), int(z'E1'), s1 >= int(z'E0'))
            j2 = s2 - merge(int(z'20'), int(z'1F'), s2 >= int(z'7F'))
        end if

        j = ior(ishft(j1, 8), j2)
    end function sjis_to_jis

    ! Port of coding.h:SJIS_TO_JIS2.
    integer function sjis_to_jis2(code) result(j)
        integer, intent(in) :: code
        integer :: s1, s2, j1, j2

        s1 = ishft(code, -8)
        s2 = iand(code, 255)

        if (s2 >= int(z'9F')) then
            select case (s1)
            case (int(z'F0'))
                j1 = int(z'28')
            case (int(z'F1'))
                j1 = int(z'24')
            case (int(z'F2'))
                j1 = int(z'2C')
            case (int(z'F3'))
                j1 = int(z'2E')
            case default
                j1 = int(z'6E') + (s1 - int(z'F4'))*2
            end select

            j2 = s2 - int(z'7E')
        else
            if (s1 <= int(z'F2')) then
                j1 = int(z'21') + (s1 - int(z'F0'))*2
            else if (s1 <= int(z'F4')) then
                j1 = int(z'2D') + (s1 - int(z'F3'))*2
            else
                j1 = int(z'6F') + (s1 - int(z'F5'))*2
            end if

            j2 = s2 - merge(int(z'20'), int(z'1F'), s2 >= int(z'7F'))
        end if

        j = ior(ishft(j1, 8), j2)
    end function sjis_to_jis2

    ! Port of coding.h:JIS_TO_SJIS.
    integer function jis_to_sjis(code) result(s)
        integer, intent(in) :: code
        integer :: s1, s2, j1, j2

        j1 = ishft(code, -8)
        j2 = iand(code, 255)

        if (btest(j1, 0)) then
            s1 = j1/2 + merge(int(z'71'), int(z'B1'), j1 < int(z'5F'))
            s2 = j2 + merge(int(z'20'), int(z'1F'), j2 >= int(z'60'))
        else
            s1 = j1/2 + merge(int(z'70'), int(z'B0'), j1 < int(z'5F'))
            s2 = j2 + int(z'7E')
        end if

        s = ior(ishft(s1, 8), s2)
    end function jis_to_sjis

    ! Port of coding.h:JIS_TO_SJIS2.
    integer function jis_to_sjis2(code) result(s)
        integer, intent(in) :: code
        integer :: s1, s2, j1, j2

        j1 = ishft(code, -8)
        j2 = iand(code, 255)

        if (btest(j1, 0)) then
            if (j1 <= int(z'25')) then
                s1 = int(z'F0') + (j1 - int(z'21'))/2
            else if (j1 <= int(z'2F')) then
                s1 = int(z'F3') + (j1 - int(z'2D'))/2
            else
                s1 = int(z'F5') + (j1 - int(z'6F'))/2
            end if

            s2 = j2 + merge(int(z'20'), int(z'1F'), j2 >= int(z'60'))
        else
            select case (j1)
            case (int(z'28'))
                s1 = int(z'F0')
            case (int(z'24'))
                s1 = int(z'F1')
            case (int(z'2C'))
                s1 = int(z'F2')
            case (int(z'2E'))
                s1 = int(z'F3')
            case default
                s1 = int(z'F4') + (j1 - int(z'6E'))/2
            end select

            s2 = j2 + int(z'7E')
        end if

        s = ior(ishft(s1, 8), s2)
    end function jis_to_sjis2

    !> Decode with no conversion of the text.  Port of
    !> coding.c:decode_coding_raw_text.
    subroutine decode_raw_text(units, chars, n)
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        integer(int64) :: src

        do src = 1, size(units, kind=int64)
            call push(chars, n, invalid_char(units(src)))
        end do
    end subroutine decode_raw_text

    !> Convert the line ends of the decoded CHARS(1:N) as coding system ID
    !> says, detecting them first if ID's are undecided (then ID becomes
    !> the subsidiary).  Port of coding.c:decode_eol.
    subroutine decode_eol(id, chars, n)
        integer, intent(inout) :: id
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        integer(int64) :: p, q
        integer :: eol_seen

        if (inhibit_eol_conversion%value .or. codings(id)%eol == EOL_UNIX) return

        if (codings(id)%eol == EOL_UNDECIDED) then
            eol_seen = EOL_SEEN_NONE
            p = 1

            do while (p <= n)
                if (chars(p) == LF) then
                    eol_seen = ior(eol_seen, EOL_SEEN_LF)
                else if (chars(p) == CR) then
                    if (p + 1 <= n) then
                        if (chars(p + 1) == LF) then
                            eol_seen = ior(eol_seen, EOL_SEEN_CRLF)
                            p = p + 1
                        else
                            eol_seen = ior(eol_seen, EOL_SEEN_CR)
                        end if
                    else
                        eol_seen = ior(eol_seen, EOL_SEEN_CR)
                    end if
                end if

                p = p + 1
            end do

            ! Handle DOS-style EOLs in a file with stray ^M characters.
            if (iand(eol_seen, EOL_SEEN_CRLF) /= 0 .and. iand(eol_seen, EOL_SEEN_CR) /= 0 &
                .and. iand(eol_seen, EOL_SEEN_LF) == 0) then
                eol_seen = EOL_SEEN_CRLF
            else if (eol_seen /= EOL_SEEN_NONE .and. eol_seen /= EOL_SEEN_LF .and. eol_seen /= EOL_SEEN_CRLF &
                     .and. eol_seen /= EOL_SEEN_CR) then
                eol_seen = EOL_SEEN_LF
            end if

            if (eol_seen /= EOL_SEEN_NONE) id = adjust_eol(id, eol_seen)
        end if

        select case (codings(id)%eol)
        case (EOL_MAC)
            where (chars(1:n) == CR) chars(1:n) = LF
        case (EOL_DOS)
            q = 0
            p = 1

            do while (p <= n)
                if (chars(p) == CR .and. p < n) then
                    if (chars(p + 1) == LF) then
                        p = p + 1
                        cycle
                    end if
                end if

                q = q + 1
                chars(q) = chars(p)
                p = p + 1
            end do

            n = q
        case default
            ! Unix: nothing to convert.
            continue
        end select
    end subroutine decode_eol

    !> Store the characters CHARS(1:N) as text: Emacs's internal encoding
    !> if MULTIBYTE, else one byte each.
    subroutine chars_to_text(chars, n, multibyte, text)
        integer, intent(in) :: chars(:)
        integer(int64), intent(in) :: n
        logical, intent(in) :: multibyte
        character(len=:), allocatable, intent(out) :: text
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nb
        integer(int64) :: k, q
        type(status_type) :: st

        if (.not. multibyte) then
            allocate (character(len=n) :: text)

            do k = 1, n
                if (chars(k) < 256) then
                    text(k:k) = char(chars(k))
                else
                    text(k:k) = char(char_to_byte8(chars(k)))
                end if
            end do

            return
        end if

        allocate (character(len=MAX_MULTIBYTE_LENGTH*n) :: text)
        q = 0

        do k = 1, n
            if (chars(k) < 128) then
                q = q + 1
                text(q:q) = char(chars(k))
            else
                call char_string(chars(k), buf, nb, st)
                text(q + 1:q + nb) = buf(1:nb)
                q = q + nb
            end if
        end do

        text = text(1:q)
    end subroutine chars_to_text

    !> Decode SRC (bytes, or Emacs's internal encoding if
    !> SRC_MULTIBYTE) by coding system CODING ("" for undecided),
    !> detecting what it is in if CODING leaves that undecided, into TEXT
    !> (multibyte unless DST_MULTIBYTE is false).  USED is the coding
    !> system used (`last_coding_system_used').  Port of
    !> coding.c:decode_coding_object and decode_coding.
    subroutine decode_coding_bytes(src, coding, text, used, status, src_multibyte, dst_multibyte)
        character(len=*), intent(in) :: src, coding
        character(len=:), allocatable, intent(out) :: text, used
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: src_multibyte, dst_multibyte
        integer, allocatable :: units(:), chars(:)
        integer(int64) :: n
        integer :: id
        logical :: smb, dmb

        text = ""
        used = coding
        smb = .false.
        if (present(src_multibyte)) smb = src_multibyte
        dmb = .true.
        if (present(dst_multibyte)) dmb = dst_multibyte

        if (len(coding) == 0) then
            id = coding_system_id("undecided")
        else
            id = coding_system_id(coding)
        end if

        if (id == 0) then
            call set_error(status, FM_CODING_SYSTEM_ERROR, coding)
            return
        end if

        call source_units(src, smb, units)
        id = detect_for_decoding(id, units, src)
        allocate (chars(max(16, size(units))))
        n = 0

        select case (codings(id)%ctype)
        case (CODING_UTF_8)
            call decode_utf_8(id, units, chars, n)
        case (CODING_UTF_16)
            call decode_utf_16(id, units, chars, n)
        case (CODING_CHARSET)
            call decode_charset(id, units, chars, n, status)
            if (status%code /= 0) return
        case (CODING_SJIS)
            call decode_sjis(id, units, chars, n, status)
            if (status%code /= 0) return
        case (CODING_BIG5)
            call decode_big5(id, units, chars, n, status)
            if (status%code /= 0) return
        case (CODING_ISO_2022)
            call decode_iso_2022(id, units, chars, n, status)
            if (status%code /= 0) return
        case (CODING_EMACS_MULE)
            call decode_emacs_mule(units, chars, n, status)
            if (status%code /= 0) return
        case default
            call decode_raw_text(units, chars, n)
        end select

        call decode_eol(id, chars, n)
        call chars_to_text(chars, n, dmb, text)
        used = codings(id)%name
    end subroutine decode_coding_bytes

    ! ------------------------------------------------------------------
    ! Encoding
    ! ------------------------------------------------------------------

    !> Append the bytes of BYTES to OUT(1:Q).
    subroutine emit(out, q, bytes)
        character(len=:), allocatable, intent(inout) :: out
        integer(int64), intent(inout) :: q
        character(len=*), intent(in) :: bytes
        character(len=:), allocatable :: grown

        if (q + len(bytes) > len(out, kind=int64)) then
            allocate (character(len=max(2*len(out, kind=int64), q + len(bytes) + 16)) :: grown)
            grown(1:q) = out(1:q)
            call move_alloc(grown, out)
        end if

        out(q + 1:q + len(bytes)) = bytes
        q = q + len(bytes)
    end subroutine emit

    !> The characters of SRC (Emacs's internal encoding if MULTIBYTE, else
    !> bytes, the non-ASCII ones as raw bytes), with the line ends
    !> converted for end-of-line type EOL.  Port of coding.c:consume_chars.
    subroutine consume_chars(src, multibyte, eol, chars, n)
        character(len=*), intent(in) :: src
        logical, intent(in) :: multibyte
        integer, intent(in) :: eol
        integer, allocatable, intent(out) :: chars(:)
        integer(int64), intent(out) :: n
        integer(int64) :: p
        integer :: c, length

        allocate (chars(max(16, len(src))))
        n = 0
        p = 1

        do while (p <= len(src, kind=int64))
            if (multibyte) then
                if (iachar(src(p:p)) < 128) then
                    c = iachar(src(p:p))
                    length = 1
                else if (multibyte_length(src, p, .true.) == 0) then
                    c = byte8_to_char(iachar(src(p:p)))
                    length = 1
                else
                    call string_char_and_length(src, p, c, length)
                end if
            else
                c = iachar(src(p:p))
                if (c >= 128) c = byte8_to_char(c)
                length = 1
            end if

            p = p + length

            if (c == LF .and. eol == EOL_DOS) then
                call push(chars, n, CR)
                call push(chars, n, LF)
            else if (c == LF .and. eol == EOL_MAC) then
                call push(chars, n, CR)
            else
                call push(chars, n, c)
            end if
        end do
    end subroutine consume_chars

    !> The internal (multibyte) form of character C.
    function internal_bytes(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nb
        type(status_type) :: st

        call char_string(c, buf, nb, st)
        s = buf(1:nb)
    end function internal_bytes

    !> Encode TEXT (Emacs's internal encoding unless MULTIBYTE is false)
    !> by coding system CODING ("" for no-conversion) into the bytes OUT.
    !> USED is the coding system used (`last_coding_system_used').  Port of
    !> coding.c:encode_coding_object and encode_coding, with the encoders
    !> encode_coding_utf_8, encode_coding_utf_16, encode_coding_charset and
    !> encode_coding_raw_text.
    subroutine encode_coding_text(text, coding, out, used, status, multibyte)
        character(len=*), intent(in) :: text, coding
        character(len=:), allocatable, intent(out) :: out, used
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: multibyte
        integer, allocatable :: chars(:)
        integer(int64) :: n, k, q, code
        integer :: id, eol, c, u, cs, b
        logical :: mb

        out = ""
        used = coding
        mb = .true.
        if (present(multibyte)) mb = multibyte

        if (len(coding) == 0) then
            id = coding_system_id("no-conversion")
        else
            id = coding_system_id(coding)
        end if

        if (id == 0) then
            call set_error(status, FM_CODING_SYSTEM_ERROR, coding)
            return
        end if

        eol = codings(id)%eol
        if (inhibit_eol_conversion%value .or. eol == EOL_UNDECIDED) eol = EOL_UNIX
        call consume_chars(text, mb, eol, chars, n)
        deallocate (out)
        allocate (character(len=max(16_int64, 2*n)) :: out)
        q = 0

        select case (codings(id)%ctype)
        case (CODING_UTF_8)
            if (codings(id)%bom /= BOM_NONE) call emit(out, q, char(int(z'EF'))//char(int(z'BB'))//char(int(z'BF')))

            do k = 1, n
                c = chars(k)

                if (char_byte8_p(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else if (c < 128) then
                    call emit(out, q, char(c))
                else
                    call emit(out, q, internal_bytes(c))
                end if
            end do
        case (CODING_UTF_16)
            if (codings(id)%bom /= BOM_NONE) then
                if (codings(id)%big_endian) then
                    call emit(out, q, char(int(z'FE'))//char(int(z'FF')))
                else
                    call emit(out, q, char(int(z'FF'))//char(int(z'FE')))
                end if
            end if

            do k = 1, n
                c = chars(k)
                if (c > MAX_UNICODE_CHAR) c = codings(id)%default_char

                if (c < int(z'10000')) then
                    call emit_16(c)
                else
                    c = c - int(z'10000')
                    call emit_16(ishft(c, -10) + int(z'D800'))
                    call emit_16(iand(c, int(z'3FF')) + int(z'DC00'))
                end if
            end do
        case (CODING_CHARSET)
            ! encode_coding_charset.
            do k = 1, n
                c = chars(k)

                if (codings(id)%ascii_compatible .and. c < 128) then
                    call emit(out, q, char(c))
                else if (char_byte8_p(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else
                    cs = char_charset_among(c, codings(id)%charset_ids, code, status)
                    if (status%code /= 0) return

                    if (cs == 0) then
                        call emit(out, q, char(codings(id)%default_char))
                    else
                        do b = charset_dimension_of(cs) - 1, 0, -1
                            call emit(out, q, char(int(iand(ishft(code, -8*b), 255_int64))))
                        end do
                    end if
                end if
            end do
        case (CODING_ISO_2022)
            call encode_iso_2022(id, chars, n, eol, out, q, status)
            if (status%code /= 0) return
        case (CODING_EMACS_MULE)
            ! encode_coding_emacs_mule (the preferred charsets of charset
            ! annotations are Deferred).
            do k = 1, n
                c = chars(k)

                if (c < 128) then
                    call emit(out, q, char(c))
                else if (char_byte8_p(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else
                    cs = char_charset_among(c, emacs_mule_charset_ids(), code, status)
                    if (status%code /= 0) return

                    if (cs == 0) then
                        c = codings(id)%default_char

                        if (c < 128) then
                            call emit(out, q, char(c))
                            cycle
                        end if

                        cs = char_charset_among(c, emacs_mule_charset_ids(), code, status)
                        if (status%code /= 0) return
                    end if

                    call emit_emacs_mule(cs, code)
                end if
            end do
        case (CODING_SJIS, CODING_BIG5)
            ! encode_coding_sjis and encode_coding_big5.
            do k = 1, n
                c = chars(k)

                if (codings(id)%ascii_compatible .and. c < 128) then
                    call emit(out, q, char(c))
                else if (char_byte8_p(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else
                    cs = char_charset_among(c, codings(id)%charset_ids, code, status)
                    if (status%code /= 0) return

                    if (cs == 0) then
                        cs = char_charset_among(codings(id)%default_char, codings(id)%charset_ids, code, status)
                        if (status%code /= 0) return
                    end if

                    call emit_sjis_big5(cs, int(code))
                end if
            end do
        case default
            ! encode_coding_raw_text (also for undecided).
            do k = 1, n
                c = chars(k)

                if (c < 128) then
                    call emit(out, q, char(c))
                else if (char_byte8_p(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else if (mb) then
                    call emit(out, q, internal_bytes(c))
                else
                    u = iand(c, 255)
                    call emit(out, q, char(u))
                end if
            end do
        end select

        out = out(1:q)
        used = codings(id)%name

    contains

        ! The leading codes of charset CS (EMACS_MULE_LEADING_CODES) and its
        ! position codes.
        subroutine emit_emacs_mule(cs, code)
            integer, intent(in) :: cs
            integer(int64), intent(in) :: code
            integer :: mule_id, c0, c1, v

            mule_id = charset_emacs_mule_id_of(cs)

            if (mule_id < int(z'A0')) then
                c0 = mule_id
                c1 = 0
            else if (mule_id < int(z'E0')) then
                c0 = int(z'9A')
                c1 = mule_id
            else if (mule_id < int(z'F0')) then
                c0 = int(z'9B')
                c1 = mule_id
            else if (mule_id < int(z'F5')) then
                c0 = int(z'9C')
                c1 = mule_id
            else
                c0 = int(z'9D')
                c1 = mule_id
            end if

            call emit(out, q, char(c0))
            if (c1 /= 0) call emit(out, q, char(c1))

            if (charset_dimension_of(cs) == 1) then
                call emit(out, q, char(ior(int(code), 128)))
            else
                v = ior(int(code), int(z'8080'))
                call emit(out, q, char(ishft(v, -8))//char(iand(v, 255)))
            end if
        end subroutine emit_emacs_mule

        subroutine emit_sjis_big5(cs, code)
            integer, intent(in) :: cs, code
            integer :: c1, v, kanji2

            kanji2 = 0
            if (size(codings(id)%charset_ids) > 3) kanji2 = codings(id)%charset_ids(4)

            if (codings(id)%ctype == CODING_BIG5) then
                if (cs == codings(id)%charset_ids(2)) then
                    call emit(out, q, char(ishft(code, -8))//char(iand(code, 255)))
                else
                    call emit(out, q, char(iand(code, 127)))
                end if
            else if (cs == codings(id)%charset_ids(3)) then
                v = jis_to_sjis(code)
                call emit(out, q, char(ishft(v, -8))//char(iand(v, 255)))
            else if (cs == codings(id)%charset_ids(2)) then
                call emit(out, q, char(ior(code, 128)))
            else if (kanji2 /= 0 .and. cs == kanji2) then
                c1 = ishft(code, -8)

                if (c1 == int(z'21') .or. (c1 >= int(z'23') .and. c1 <= int(z'25')) .or. c1 == int(z'28') &
                    .or. (c1 >= int(z'2C') .and. c1 <= int(z'2F')) .or. c1 >= int(z'6E')) then
                    v = jis_to_sjis2(code)
                    call emit(out, q, char(ishft(v, -8))//char(iand(v, 255)))
                else
                    call emit(out, q, char(iand(code, 127)))
                end if
            else
                call emit(out, q, char(iand(code, 127)))
            end if
        end subroutine emit_sjis_big5

        subroutine emit_16(v)
            integer, intent(in) :: v

            if (codings(id)%big_endian) then
                call emit(out, q, char(ishft(v, -8))//char(iand(v, 255)))
            else
                call emit(out, q, char(iand(v, 255))//char(ishft(v, -8)))
            end if
        end subroutine emit_16
    end subroutine encode_coding_text

    !> Encode CHARS(1:N) with ISO-2022 coding system ID into OUT(1:Q): the
    !> designation and invocation sequences each character needs, the
    !> resets at line ends and control characters its flags ask for.  Port
    !> of coding.c:encode_coding_iso_2022 (the preferred charsets of charset
    !> annotations are Deferred, as the annotations are).
    subroutine encode_iso_2022(id, chars, n, eol, out, q, status)
        integer, intent(in) :: id
        integer, intent(in) :: chars(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: eol
        character(len=:), allocatable, intent(inout) :: out
        integer(int64), intent(inout) :: q
        type(status_type), intent(inout) :: status
        integer, allocatable :: charset_list(:)
        integer :: invocation(0:1), designation(0:3), initial(0:3)
        integer(int64) :: k, code
        integer :: c, cs, flags, i
        logical :: ascii_compatible, single_shifting, bol_designation

        flags = codings(id)%iso_flags
        charset_list = coding_charset_ids(id)
        initial = codings(id)%iso_initial
        where (initial == 0) initial = -1
        invocation(0) = 0
        invocation(1) = merge(-1, 1, iand(flags, ISO_SEVEN_BITS) /= 0)
        designation = initial
        single_shifting = .false.
        ! Beginning of buffer should also be regarded as bol.
        bol_designation = iand(flags, ISO_DESIGNATE_AT_BOL) /= 0
        ascii_compatible = codings(id)%ascii_compatible .and. iand(flags, ior(ISO_DESIGNATION, ISO_LOCKING_SHIFT)) == 0
        k = 1

        do while (k <= n)
            ! We have to produce designation sequences if any now.
            if (bol_designation) then
                call designation_at_bol(k)
                bol_designation = .false.
            end if

            c = chars(k)
            k = k + 1

            if (c < 32 .or. c == 127) then
                if (c == 10 .or. (c == 13 .and. eol == EOL_MAC)) then
                    if (iand(flags, ISO_RESET_AT_EOL) /= 0) call reset_plane_and_register()

                    if (iand(flags, ISO_INIT_AT_BOL) /= 0) then
                        do i = 0, 3
                            designation(i) = initial(i)
                        end do
                    end if

                    bol_designation = iand(flags, ISO_DESIGNATE_AT_BOL) /= 0
                else if (iand(flags, ISO_RESET_AT_CNTL) /= 0) then
                    call reset_plane_and_register()
                end if

                call emit(out, q, char(c))
            else if (c < 128) then
                if (ascii_compatible) then
                    call emit(out, q, char(c))
                else
                    call encode_iso_character(charset_ascii, c)
                end if
            else if (char_byte8_p(c)) then
                call emit(out, q, char(char_to_byte8(c)))
            else
                cs = char_charset_among(c, charset_list, code, status)
                if (status%code /= 0) return

                if (cs == 0) then
                    if (iand(flags, ISO_SAFE) /= 0) then
                        c = iachar("?")
                        cs = charset_ascii
                    else
                        c = codings(id)%default_char
                        cs = char_charset_among(c, charset_list, code, status)
                        if (status%code /= 0) return
                    end if
                end if

                call encode_iso_character(cs, c)
            end if
        end do

        if (iand(flags, ISO_RESET_AT_EOL) /= 0) call reset_plane_and_register()

    contains

        ! ENCODE_ISO_CHARACTER.
        subroutine encode_iso_character(charset, ch)
            integer, intent(in) :: charset, ch
            integer(int64) :: cd

            cd = charset_encode_id(charset, ch, status)

            if (charset_dimension_of(charset) == 1) then
                call encode_dimension(charset, 1, int(cd), 0)
            else
                call encode_dimension(charset, 2, int(ishft(cd, -8)), int(iand(cd, 255_int64)))
            end if
        end subroutine encode_iso_character

        ! ENCODE_ISO_CHARACTER_DIMENSION1 and _DIMENSION2.
        subroutine encode_dimension(charset, dim, c1, c2)
            integer, intent(in) :: charset, dim, c1, c2
            integer :: cid

            cid = charset

            if (dim == 1) then
                if (iand(flags, ISO_USE_ROMAN) /= 0 .and. cid == charset_ascii) cid = charset_jisx0201_roman
            else
                if (iand(flags, ISO_USE_OLDJIS) /= 0 .and. cid == charset_jisx0208) cid = charset_jisx0208_1978
            end if

            do
                if (single_shifting) then
                    if (iand(flags, ISO_SEVEN_BITS) /= 0) then
                        call emit_codes(dim, iand(c1, 127), iand(c2, 127))
                    else
                        call emit_codes(dim, ior(c1, 128), ior(c2, 128))
                    end if

                    single_shifting = .false.
                    return
                else if (cid == plane(0)) then
                    call emit_codes(dim, iand(c1, 127), iand(c2, 127))
                    return
                else if (cid == plane(1)) then
                    call emit_codes(dim, ior(c1, 128), ior(c2, 128))
                    return
                else
                    ! Since CHARSET is not yet invoked to any graphic
                    ! planes, we must invoke it, or, at first, designate it
                    ! to some graphic register.  Then repeat the loop to
                    ! actually produce the character.
                    call invocation_designation(cid)
                end if
            end do
        end subroutine encode_dimension

        subroutine emit_codes(dim, b1, b2)
            integer, intent(in) :: dim, b1, b2

            if (dim == 1) then
                call emit(out, q, char(b1))
            else
                call emit(out, q, char(b1)//char(b2))
            end if
        end subroutine emit_codes

        integer function plane(p)
            integer, intent(in) :: p

            plane = -1
            if (invocation(p) >= 0) plane = designation(invocation(p))
        end function plane

        ! Port of coding.c:encode_invocation_designation.
        subroutine invocation_designation(charset)
            integer, intent(in) :: charset
            integer :: reg

            ! At first, check designations.
            do reg = 0, 3
                if (designation(reg) == charset) exit
            end do

            if (reg >= 4) then
                ! CHARSET is not yet designated to any graphic registers.
                ! At first check the requested designation.
                reg = iso_request(id, charset)
                ! Since CHARSET requests no special designation, designate
                ! it to graphic register 0.
                if (reg < 0) reg = 0
                call encode_designation(charset, reg)
            end if

            if (invocation(0) /= reg .and. invocation(1) /= reg) then
                ! Since the graphic register REG is not invoked to any
                ! graphic planes, invoke it to graphic plane 0.
                select case (reg)
                case (0)
                    call emit(out, q, char(ISO_CODE_SI))
                    invocation(0) = 0
                case (1)
                    call emit(out, q, char(ISO_CODE_SO))
                    invocation(0) = 1
                case (2)
                    if (iand(flags, ISO_SINGLE_SHIFT) /= 0) then
                        call single_shift(iachar("N"), ISO_CODE_SS2)
                    else
                        call emit(out, q, char(ISO_CODE_ESC)//"n")
                        invocation(0) = 2
                    end if
                case (3)
                    if (iand(flags, ISO_SINGLE_SHIFT) /= 0) then
                        call single_shift(iachar("O"), ISO_CODE_SS3)
                    else
                        ! As upstream, ESC n.
                        call emit(out, q, char(ISO_CODE_ESC)//"n")
                        invocation(0) = 3
                    end if
                case default
                end select
            end if
        end subroutine invocation_designation

        subroutine single_shift(final, code8)
            integer, intent(in) :: final, code8

            if (iand(flags, ISO_SEVEN_BITS) /= 0) then
                call emit(out, q, char(ISO_CODE_ESC)//char(final))
            else
                call emit(out, q, char(code8))
            end if

            single_shifting = .true.
        end subroutine single_shift

        ! Port of coding.c:ENCODE_DESIGNATION.
        subroutine encode_designation(charset, reg)
            integer, intent(in) :: charset, reg
            character(len=*), parameter :: I94 = "()*+", I96 = ",-./"
            integer :: final, revision

            final = charset_iso_final_of(charset)
            revision = -1
            if (iand(flags, ISO_REVISION) /= 0) revision = charset_iso_revision_of(charset)
            if (revision >= 0) call emit(out, q, char(ISO_CODE_ESC)//"&"//char(iachar("@") + revision))
            call emit(out, q, char(ISO_CODE_ESC))

            if (charset_dimension_of(charset) == 1) then
                if (.not. charset_iso_chars_96_of(charset)) then
                    call emit(out, q, I94(reg + 1:reg + 1))
                else
                    call emit(out, q, I96(reg + 1:reg + 1))
                end if
            else
                call emit(out, q, "$")

                if (.not. charset_iso_chars_96_of(charset)) then
                    if (iand(flags, ISO_LONG_FORM) /= 0 .or. reg /= 0 .or. final < iachar("@") &
                        .or. final > iachar("B")) call emit(out, q, I94(reg + 1:reg + 1))
                else
                    call emit(out, q, I96(reg + 1:reg + 1))
                end if
            end if

            call emit(out, q, char(final))
            designation(reg) = charset
        end subroutine encode_designation

        ! Port of coding.c:ENCODE_RESET_PLANE_AND_REGISTER.
        subroutine reset_plane_and_register()
            integer :: reg

            if (invocation(0) /= 0) then
                call emit(out, q, char(ISO_CODE_SI))
                invocation(0) = 0
            end if

            do reg = 0, 3
                if (initial(reg) >= 0) then
                    if (designation(reg) /= initial(reg)) call encode_designation(initial(reg), reg)
                end if
            end do
        end subroutine reset_plane_and_register

        ! The designations the charsets of the line from CHARS(FROM) ask
        ! for.  Port of coding.c:encode_designation_at_bol.
        subroutine designation_at_bol(from)
            integer(int64), intent(in) :: from
            integer :: r(0:3), found, reg, ch, cid
            integer(int64) :: j, cd

            r = -1
            found = 0
            j = from

            do while (j <= n .and. found < 4)
                ch = chars(j)
                j = j + 1
                if (ch == 10) exit
                cid = char_charset_among(ch, charset_list, cd, status)
                if (cid == 0) cycle
                reg = iso_request(id, cid)

                if (reg >= 0) then
                    if (r(reg) < 0) then
                        found = found + 1
                        r(reg) = cid
                    end if
                end if
            end do

            if (found > 0) then
                do reg = 0, 3
                    if (r(reg) >= 0) then
                        if (designation(reg) /= r(reg)) call encode_designation(r(reg), reg)
                    end if
                end do
            end if
        end subroutine designation_at_bol
    end subroutine encode_iso_2022

    ! ------------------------------------------------------------------
    ! Encodability
    ! ------------------------------------------------------------------

    !> Whether coding system CODING can encode character C.  Port of
    !> coding.c:char_encodable_p.
    logical function char_encodable_p(c, coding)
        integer, intent(in) :: c
        character(len=*), intent(in) :: coding
        type(status_type) :: status
        integer(int64) :: code
        integer :: id

        char_encodable_p = .false.
        id = coding_system_id(coding)
        if (id == 0) return

        select case (codings(id)%charset)
        case (CS_LIST)
            if (c < 128 .and. codings(id)%ascii_compatible) then
                char_encodable_p = .true.
            else
                char_encodable_p = char_charset_among(c, coding_charset_ids(id), code, status) /= 0
            end if
        case (CS_ASCII)
            char_encodable_p = c < 128
        case (CS_UNICODE)
            char_encodable_p = c <= MAX_UNICODE_CHAR
        case (CS_EMACS)
            char_encodable_p = c <= MAX_5_BYTE_CHAR
        case default
            char_encodable_p = .true.
        end select
    end function char_encodable_p

    !> Check if TEXT (multibyte, starting at position FIRST) is encodable
    !> by CODING_SYSTEMS.
    !>
    !> If all coding systems in CODING-SYSTEM-LIST can encode the region, the
    !> function returns nil (an empty RESULT).
    !>
    !> If some of the coding systems cannot encode the whole region, value is
    !> an alist, each element of which has the form (CODING-SYSTEM POS1 POS2 ...),
    !> which means that CODING-SYSTEM cannot encode the text at buffer positions
    !> POS1, POS2, ...  (FIRST, FIRST + 1, ...; FIRST is 0 for a string
    !> as upstream counts.)  Port of coding.c:Fcheck_coding_systems_region.
    subroutine check_coding_systems_text(text, first, coding_systems, result, status)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: first
        character(len=*), intent(in) :: coding_systems(:)
        type(unencodable_type), allocatable, intent(out) :: result(:)
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: pos(:, :), counts(:)
        integer(int64) :: p, chars, nc
        integer :: k, c, length, m

        allocate (result(0))

        do k = 1, size(coding_systems)
            if (coding_system_id(trim(coding_systems(k))) == 0) then
                call set_error(status, FM_CODING_SYSTEM_ERROR, trim(coding_systems(k)))
                return
            end if
        end do

        nc = len(text, kind=int64)
        allocate (pos(max(1_int64, nc), size(coding_systems)), counts(size(coding_systems)))
        counts = 0
        p = 1
        chars = 0

        do while (p <= nc)
            if (iachar(text(p:p)) < 128) then
                p = p + 1
            else
                call string_char_and_length(text, p, c, length)
                p = p + length

                do k = 1, size(coding_systems)
                    if (.not. char_encodable_p(c, trim(coding_systems(k)))) then
                        counts(k) = counts(k) + 1
                        pos(counts(k), k) = first + chars
                    end if
                end do
            end if

            chars = chars + 1
        end do

        m = count(counts > 0)
        deallocate (result)
        allocate (result(m))
        m = 0

        ! In the reverse order of the list, as the upstream loops leave it.
        do k = size(coding_systems), 1, -1
            if (counts(k) == 0) cycle
            m = m + 1
            result(m)%coding = trim(coding_systems(k))
            result(m)%positions = pos(1:counts(k), k)
        end do

        result = result(size(result):1:-1)
    end subroutine check_coding_systems_text

    !> The base coding systems that can encode TEXT (multibyte), in priority
    !> order, all of them when it is ASCII (ALL is then true).  Port of
    !> coding.c:Ffind_coding_systems_region_internal with
    !> mule-cmds.el:find-coding-systems-string (sorted by
    !> sort-coding-systems: priority, then the order of definition).
    subroutine find_coding_systems_text(text, names, all)
        character(len=*), intent(in) :: text
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        logical, intent(out) :: all
        logical, allocatable :: safe(:)
        integer(int64) :: p
        integer :: c, length, id, i, n, k

        if (ncodings == 0) call syms_of_coding()
        allocate (safe(ncodings))
        safe = .true.
        all = .true.
        p = 1

        do while (p <= len(text, kind=int64))
            if (iachar(text(p:p)) < 128) then
                p = p + 1
                cycle
            end if

            all = .false.
            call string_char_and_length(text, p, c, length)
            p = p + length

            do id = 1, ncodings
                if (safe(id)) safe(id) = char_encodable_p(c, codings(id)%name)
            end do
        end do

        allocate (names(ncodings))
        n = 0

        if (all) then
            names(1)%name = "undecided"
            n = 1
            names = names(1:n)
            return
        end if

        ! The base coding systems that encode it: those of the categories
        ! by priority first, then the others.
        do i = 0, NCAT - 1
            id = category_id(priorities(i))
            if (id == 0) cycle

            if (safe(id) .and. codings(id)%ctype /= CODING_RAW_TEXT .and. codings(id)%ctype /= CODING_UNDECIDED) then
                n = n + 1
                names(n)%name = codings(id)%name
            end if
        end do

        do id = 1, ncodings
            if (codings(id)%base /= id) cycle
            if (.not. safe(id)) cycle
            if (codings(id)%ctype == CODING_RAW_TEXT .or. codings(id)%ctype == CODING_UNDECIDED) cycle
            if (any([(names(k)%name == codings(id)%name, k=1, n)])) cycle
            n = n + 1
            names(n)%name = codings(id)%name
        end do

        names = names(1:n)
    end subroutine find_coding_systems_text

    ! ------------------------------------------------------------------
    ! Strings and regions
    ! ------------------------------------------------------------------

    !> Decode STRING (bytes, unless MULTIBYTE) by CODING-SYSTEM into RESULT.
    !> If CODING-SYSTEM is nil, it returns STRING itself.  The value is a
    !> multibyte string.  This function sets `last_coding_system_used' to
    !> the precise coding system used to decode STRING, which may be
    !> different from CODING-SYSTEM (e.g. if CODING-SYSTEM is `undecided').
    !> Port of coding.c:Fdecode_coding_string (no NOCOPY or BUFFER).
    subroutine decode_coding_string(string, coding_system, result, status, multibyte)
        character(len=*), intent(in) :: string, coding_system
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: multibyte
        character(len=:), allocatable :: used
        logical :: mb

        mb = .false.
        if (present(multibyte)) mb = multibyte

        if (len(coding_system) == 0) then
            last_coding_system_used = "no-conversion"
            result = string
            return
        end if

        call decode_coding_bytes(string, coding_system, result, used, status, src_multibyte=mb)
        if (.not. status%ok()) return
        last_coding_system_used = used
    end subroutine decode_coding_string

    !> Encode STRING (multibyte, unless MULTIBYTE is false) to CODING-SYSTEM
    !> into RESULT, the bytes.  If CODING-SYSTEM is nil, it returns STRING
    !> itself.  This function sets `last_coding_system_used' to the precise
    !> coding system used to encode STRING, which may be different from
    !> CODING-SYSTEM.  Port of coding.c:Fencode_coding_string (no NOCOPY or
    !> BUFFER).
    subroutine encode_coding_string(string, coding_system, result, status, multibyte)
        character(len=*), intent(in) :: string, coding_system
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: multibyte
        character(len=:), allocatable :: used

        if (len(coding_system) == 0) then
            last_coding_system_used = "no-conversion"
            result = string
            return
        end if

        call encode_coding_text(string, coding_system, result, used, status, multibyte)
        if (.not. status%ok()) return
        last_coding_system_used = used
    end subroutine encode_coding_string

    !> Detect coding system of the text in STRING (bytes, unless
    !> MULTIBYTE).  See `detect_coding_bytes'.  Port of
    !> coding.c:Fdetect_coding_string.
    subroutine detect_coding_string(string, names, highest, multibyte)
        character(len=*), intent(in) :: string
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        logical, intent(in), optional :: highest, multibyte
        logical :: mb

        mb = .false.
        if (present(multibyte)) mb = multibyte
        call detect_coding_bytes(string, mb, names, highest)
    end subroutine detect_coding_string

    !> The bytes of the current buffer between START and END, checked.
    subroutine region_bytes(start, end, from, to, bytes, status)
        integer(int64), intent(in) :: start, end
        integer(int64), intent(out) :: from, to
        character(len=:), allocatable, intent(out) :: bytes
        type(status_type), intent(out) :: status

        from = min(start, end)
        to = max(start, end)

        if (from < buffers(cur)%begv .or. to > buffers(cur)%zv) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            bytes = ""
            return
        end if

        bytes = buf_bytes(buffers(cur), char_to_byte(from), char_to_byte(to))
    end subroutine region_bytes

    !> Detect coding system of the text in the region between START and END.
    !> Return a list of possible coding systems ordered by priority.
    !> See `detect_coding_bytes'.  Port of coding.c:Fdetect_coding_region.
    subroutine detect_coding_region(start, end, names, status, highest)
        integer(int64), intent(in) :: start, end
        type(coding_system_list_entry_type), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: highest
        character(len=:), allocatable :: bytes
        integer(int64) :: from, to

        call region_bytes(start, end, from, to, bytes, status)

        if (.not. status%ok()) then
            allocate (names(0))
            return
        end if

        call detect_coding_bytes(bytes, buffers(cur)%multibyte, names, highest)
    end subroutine detect_coding_region

    !> Decode the current region from the specified coding system.
    !> When called from a program, takes four arguments:
    !>         START, END, CODING-SYSTEM, and DESTINATION.
    !> START and END are buffer positions.
    !>
    !> This function sets `last_coding_system_used' to the precise coding
    !> system used to decode the text, which may be different from
    !> CODING-SYSTEM (e.g. if CODING-SYSTEM is `undecided').  INSERTED is the
    !> length of the decoded text.  Port of coding.c:Fdecode_coding_region
    !> (no DESTINATION).
    subroutine decode_coding_region(start, end, coding_system, inserted, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: coding_system
        integer(int64), intent(out) :: inserted
        type(status_type), intent(out) :: status

        call convert_region(start, end, coding_system, .false., inserted, status)
    end subroutine decode_coding_region

    !> Encode the current region by specified coding system.
    !> When called from a program, takes four arguments:
    !>         START, END, CODING-SYSTEM and DESTINATION.
    !> START and END are buffer positions.
    !>
    !> This function sets `last_coding_system_used' to the precise coding
    !> system used to encode the text, which may be different from
    !> CODING-SYSTEM.  INSERTED is the length of the encoded text.  Port of
    !> coding.c:Fencode_coding_region (no DESTINATION).
    subroutine encode_coding_region(start, end, coding_system, inserted, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: coding_system
        integer(int64), intent(out) :: inserted
        type(status_type), intent(out) :: status

        call convert_region(start, end, coding_system, .true., inserted, status)
    end subroutine encode_coding_region

    !> Replace the region by its decoded or (ENCODE) encoded form.  Port
    !> of coding.c:code_convert_region.
    subroutine convert_region(start, end, coding_system, encode, inserted, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: encode
        integer(int64), intent(out) :: inserted
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bytes, text, used
        integer(int64) :: from, to, k

        inserted = 0
        call region_bytes(start, end, from, to, bytes, status)
        if (.not. status%ok()) return

        if (len(coding_system) == 0) then
            last_coding_system_used = "no-conversion"
            inserted = to - from
            return
        end if

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return

        if (encode) then
            call encode_coding_text(bytes, coding_system, text, used, status, multibyte=buffers(cur)%multibyte)
            ! The bytes, as raw-byte characters in a multibyte buffer.
            if (status%ok() .and. buffers(cur)%multibyte) text = str_to_multibyte(text)
        else
            call decode_coding_bytes(bytes, coding_system, text, used, status, src_multibyte=buffers(cur)%multibyte, &
                                     dst_multibyte=buffers(cur)%multibyte)
        end if

        if (.not. status%ok()) return
        call temp_set_point_both(buffers(cur), from, char_to_byte(from))
        call del_range(from, to, status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return

        if (buffers(cur)%multibyte) then
            inserted = 0

            do k = 1, len(text, kind=int64)
                if (iand(iachar(text(k:k)), int(z'C0')) /= int(z'80')) inserted = inserted + 1
            end do
        else
            inserted = len(text, kind=int64)
        end if

        last_coding_system_used = used
    end subroutine convert_region

    !> Internal use only.  Port of
    !> coding.c:Fset_terminal_coding_system_internal (the one terminal).
    subroutine set_terminal_coding_system_internal(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status

        if (len(coding_system) == 0) then
            ! setup_coding_system of nil: undecided.
            terminal_coding = "undecided"
            status = status_type()
            return
        end if

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        terminal_coding = coding_system
    end subroutine set_terminal_coding_system_internal

    !> Return coding system specified for terminal output on the
    !> terminal ("" for nil).  Port of coding.c:Fterminal_coding_system.
    function terminal_coding_system() result(name)
        character(len=:), allocatable :: name

        name = "utf-8-unix"
        if (allocated(terminal_coding)) name = terminal_coding
        ! For backward compatibility, return nil if it is `undecided'.
        if (name == "undecided") name = ""
    end function terminal_coding_system

    !> Internal use only.  Port of
    !> coding.c:Fset_keyboard_coding_system_internal (the one terminal).
    subroutine set_keyboard_coding_system_internal(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status

        status = status_type()

        if (len(coding_system) == 0) then
            keyboard_coding = "no-conversion"
            return
        end if

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        keyboard_coding = coding_system
    end subroutine set_keyboard_coding_system_internal

    !> Return coding system specified for decoding keyboard input.  Port
    !> of coding.c:Fkeyboard_coding_system.
    function keyboard_coding_system() result(name)
        character(len=:), allocatable :: name

        name = "utf-8-unix"
        if (allocated(keyboard_coding)) name = keyboard_coding
    end function keyboard_coding_system
end module coding_fm
