! coding_mod.f90 -- coding systems: decoding and encoding text.
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
!> stands for none.  Each base coding system without an end-of-line type
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
!> `coding_system_put'; `network_coding_systems' and
!> `process_coding_systems'; and the functions in
!> `file_coding_systems' (xml, po and latexenc detection).
module coding_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, FM_CODING_SYSTEM_ERROR, FM_GENERIC_ERROR, FM_ARGS_OUT_OF_RANGE
    use character_mod, only: char_string, string_char_and_length, is_char_byte8, byte8_to_char, char_to_byte8, &
                             multibyte_length, MAX_MULTIBYTE_LENGTH, MAX_CHAR, MAX_UNICODE_CHAR, MAX_5_BYTE_CHAR
    use character_mod, only: str_to_multibyte
    use data_mod, only: logical_setting_type, string_setting_type
    use buffer_mod, only: buffers, current, buf_bytes, temp_set_point_both
    use marker_mod, only: char_to_byte
    use insdel_mod, only: insert, del_range
    use characters_mod, only: characters_preferred_coding_systems
    use charset_mod, only: CHARSET_NAME_LEN, charsets, find_charset, decode_char_id, encode_char_id, char_charset_id
    use charset_mod, only: find_iso_charset, iso_2022_charset_order, charset_ascii, charset_jisx0201_roman, &
                           charset_jisx0208_1978, charset_jisx0208
    use charset_mod, only: emacs_mule_charset_order, emacs_mule_charsets, emacs_mule_bytes, charset_eight_bit
    implicit none(type, external)
    private

    public :: define_charset_coding_system, define_iso_2022_coding_system, define_language_environments
    public :: set_terminal_coding_system_internal, terminal_coding_system, set_keyboard_coding_system_internal, &
              keyboard_coding_system
    public :: init_coding, is_coding_system, check_coding_system, coding_system_id, coding_system_name, &
              coding_system_eol_type, coding_system_subsidiaries, coding_system_base, coding_system_type, &
              coding_system_mnemonic, coding_system_doc_string, coding_system_aliases, coding_system_equal, &
              coding_system_list, coding_system_priority_list, set_coding_system_priority, coding_system_category, &
              is_coding_system_iso_7_bit, is_coding_system_suitable_for_keyboard, &
              is_coding_system_for_unibyte, is_coding_system_ascii_compatible, coding_system_mime_charset, &
              coding_system_charset_list, coding_system_bom, coding_system_endian, is_coding_system_prefer_utf_8, &
              coding_system_change_eol_conversion, coding_system_change_text_conversion, &
              coding_system_eol_type_mnemonic, coding_inherit_eol_type, raw_text_coding_system, &
              detect_coding_bytes, decode_coding_bytes, encode_coding_text, is_char_encodable, &
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
    !> BOM handling of UTF coding systems (the `:bom' attribute: BOM_NONE, BOM_WITH
    !> or BOM_DETECT, which chooses between two coding systems).
    integer, parameter, public :: BOM_NONE = 0, BOM_WITH = 1, BOM_DETECT = 2

    ! The charsets a coding system other than one of type `charset' can
    ! encode (its `:charset-list'); those of type `charset' list theirs.
    integer, parameter :: CS_ASCII = 1, CS_UNICODE = 3, CS_EMACS = 4, CS_ANY = 5, CS_LIST = 6
    ! The most charsets one first byte can start in a coding system of
    ! type `charset'.
    integer, parameter :: MAX_VALID_CHARSETS = 8

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
        integer :: valid_charset_counts(0:255) = 0
        integer :: valid_charsets(MAX_VALID_CHARSETS, 0:255) = 0
        !> For type `iso-2022': its flags, the charsets initially
        !> designated to G0..G3 (0 for none), the registers of the
        !> charsets of 94 and 96 characters (4 for none), the charsets that
        !> request a register, and whether it supports all ISO-2022
        !> charsets (`:charset-list' `iso-2022').
        integer :: iso_flags = 0
        integer :: iso_initial(0:3) = 0
        integer :: iso_register_94 = 4, iso_register_96 = 4
        integer, allocatable :: iso_request_charsets(:), iso_request_registers(:)
        logical :: ascii_compatible = .false., for_unibyte = .false., prefer_utf_8 = .false.
        !> `:suitable-for-keyboard'.
        logical :: suitable_for_keyboard = .false.
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

    !> An entry of `file_coding_systems': a regexp matching file
    !> names, and the coding systems for decoding and for encoding ("" for
    !> none).
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
    !> If not, an appropriate element is used from one of the coding system tables.
    !> There are three such tables: `file_coding_systems',
    !> `process_coding_systems', and `network_coding_systems'.
    !> Port of coding.c coding-system-for-read (unallocated when unset).
    type(string_setting_type), public :: coding_system_for_read = string_setting_type(name="coding_system_for_read")

    !> Specify the coding system for write operations.
    !> Programs bind this variable temporarily, but you should not set it globally.
    !> If the value is a coding system, it is used for encoding of output,
    !> when writing it to a file and when sending it to a file or subprocess.
    !>
    !> If this does not specify a coding system, an appropriate element
    !> is used from one of the coding system tables.
    !> There are three such tables: `file_coding_systems',
    !> `process_coding_systems', and `network_coding_systems'.
    !> For output to files, if the above procedure does not specify a coding system,
    !> the value of `buffer_file_coding_system' is used.  Port of coding.c
    !> coding-system-for-write (unallocated when unset).
    type(string_setting_type), public :: coding_system_for_write = string_setting_type(name="coding_system_for_write")

    !> Coding system used in the latest file or process I/O.  Port of
    !> coding.c last-coding-system-used.
    type(string_setting_type), public :: last_coding_system_used = string_setting_type(name="last_coding_system_used")

    !> True means always inhibit code conversion of end-of-line format.
    !> See info node `Coding Systems' and info node `Text and Binary' concerning
    !> such conversion.  Port of coding.c inhibit-eol-conversion.
    type(logical_setting_type), public :: inhibit_eol_conversion = logical_setting_type(name="inhibit_eol_conversion")

    !> If true, Emacs ignores null bytes on code detection.
    !> By default, Emacs treats it as binary data, and does not attempt to
    !> decode it.  The effect is as if you specified `no-conversion' for
    !> reading that text.
    !>
    !> Set this to true when a regular text happens to include null bytes.
    !> Examples are Index nodes of Info files and null-byte delimited output
    !> from GNU Find and GNU Grep.  Emacs will then ignore the null bytes and
    !> decode text as usual.  Port of coding.c inhibit-null-byte-detection.
    type(logical_setting_type), public :: inhibit_null_byte_detection = logical_setting_type(name="inhibit_null_byte_detection")

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
    type(logical_setting_type), public :: inhibit_iso_escape_detection = &
                                          logical_setting_type(name="inhibit_iso_escape_detection")

    !> String displayed in mode line for UNIX-like (LF) end-of-line format.
    !> Port of coding.c eol-mnemonic-unix.
    type(string_setting_type), public :: eol_mnemonic_unix = string_setting_type(name="eol_mnemonic_unix")

    !> String displayed in mode line for DOS-like (CRLF) end-of-line format.
    !> Port of coding.c eol-mnemonic-dos.
    type(string_setting_type), public :: eol_mnemonic_dos = string_setting_type(name="eol_mnemonic_dos")

    !> String displayed in mode line for MAC-like (CR) end-of-line format.
    !> Port of coding.c eol-mnemonic-mac.
    type(string_setting_type), public :: eol_mnemonic_mac = string_setting_type(name="eol_mnemonic_mac")

    !> String displayed in mode line when end-of-line format is not yet determined.
    !> Port of coding.c eol-mnemonic-undecided.
    type(string_setting_type), public :: eol_mnemonic_undecided = string_setting_type(name="eol_mnemonic_undecided")

    !> Coding systems used for process I/O by default.
    !> The first (`decoding') is used for decoding a process output,
    !> the second (`encoding') is used for encoding a text to be sent to a process.
    !> Port of coding.c default-process-coding-system (the value
    !> prefer-coding-system gives it in a UTF-8 locale).
    character(len=40), public :: default_process_coding_system(2) = ["utf-8-unix", "utf-8-unix"]

    !> Table to decide a coding system to use for a file I/O operation.
    !> Each element has a PATTERN, a regular expression matching a file
    !> name, and the coding systems for decoding and for encoding the
    !> file contents ("" for none).  See also the function
    !> `find_operation_coding_system' and the variable `auto_codings'.
    !> Port of coding.c file-coding-system-alist, with mule-conf.el's value
    !> (without its functions: the xml, po and latexenc detection is
    !> Deferred).
    type(file_coding_type), allocatable, public :: file_coding_systems(:)

    !> Coding system to use with system messages.
    !> Potentially also used for decoding keyboard input on X Windows, and is
    !> used for encoding standard output and error streams.  Port of
    !> coding.c locale-coding-system (a UTF-8 locale).
    type(string_setting_type), public :: locale_coding_system = string_setting_type(name="locale_coding_system")

    !> Internal use only.
    !> If true, on writing a file, `select_safe_coding_system_function' is
    !> called even if `coding_system_for_write' is set.  The command
    !> `universal_coding_system_argument' sets this variable temporarily.
    !> Port of coding.c coding-system-require-warning.
    type(logical_setting_type), public :: coding_system_require_warning = &
                                          logical_setting_type(name="coding_system_require_warning")

    !> Coding system to be used for encoding the buffer contents on saving.
    !> This variable applies to saving the buffer, and also to `write_region'
    !> and other functions that use `write_region'.
    !> It does not apply to sending output to subprocesses, however.
    !>
    !> If this is unallocated, the buffer is saved without any code conversion
    !> unless some coding system is specified in `file_coding_systems'
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
    type(string_setting_type), target, public :: buffer_file_coding_system = &
                                                 string_setting_type(name="buffer_file_coding_system")

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
        !> Define the `charset' coding systems of chinese.el (in chinese_mod).  Port of
        !> chinese.el's `charset' coding systems.
        module subroutine init_chinese()
            implicit none(type, external)
        end subroutine init_chinese
        !> Define the `charset' coding systems of cyrillic.el (in cyrillic_mod).  Port of
        !> cyrillic.el's `charset' coding systems.
        module subroutine init_cyrillic()
            implicit none(type, external)
        end subroutine init_cyrillic
        !> Define the `charset' coding systems of english.el (in english_mod).  Port of
        !> english.el's `charset' coding systems.
        module subroutine init_english()
            implicit none(type, external)
        end subroutine init_english
        !> Define the `charset' coding systems of european.el (in european_mod).  Port of
        !> european.el's `charset' coding systems.
        module subroutine init_european()
            implicit none(type, external)
        end subroutine init_european
        !> Define the `charset' coding systems of romanian.el (in romanian_mod).  Port of
        !> romanian.el's `charset' coding systems.
        module subroutine init_romanian()
            implicit none(type, external)
        end subroutine init_romanian
        !> Define the `charset' coding systems of greek.el (in greek_mod).  Port of
        !> greek.el's `charset' coding systems.
        module subroutine init_greek()
            implicit none(type, external)
        end subroutine init_greek
        !> Define the `charset' coding systems of hebrew.el (in hebrew_mod).  Port of
        !> hebrew.el's `charset' coding systems.
        module subroutine init_hebrew()
            implicit none(type, external)
        end subroutine init_hebrew
        !> Define the `charset' coding systems of japanese.el (in japanese_mod).  Port of
        !> japanese.el's `charset' coding systems.
        module subroutine init_japanese()
            implicit none(type, external)
        end subroutine init_japanese
        !> Define the `charset' coding systems of korean.el (in korean_mod).  Port of
        !> korean.el's `charset' coding systems.
        module subroutine init_korean()
            implicit none(type, external)
        end subroutine init_korean
        !> Define the `charset' coding systems of lao.el (in lao_mod).  Port of
        !> lao.el's `charset' coding systems.
        module subroutine init_lao()
            implicit none(type, external)
        end subroutine init_lao
        !> Define the `charset' coding systems of thai.el (in thai_mod).  Port of
        !> thai.el's `charset' coding systems.
        module subroutine init_thai()
            implicit none(type, external)
        end subroutine init_thai
        !> Define the `iso-2022' coding systems of tibetan.el (in tibetan_mod).  Port of
        !> tibetan.el's `iso-2022' coding systems.
        module subroutine init_tibetan()
            implicit none(type, external)
        end subroutine init_tibetan
        !> Define the `charset' coding systems of vietnamese.el (in vietnamese_mod).  Port of
        !> vietnamese.el's `charset' coding systems.
        module subroutine init_vietnamese()
            implicit none(type, external)
        end subroutine init_vietnamese
        !> Define the `charset' coding systems of misc-lang.el (in misc_lang_mod).  Port of
        !> misc-lang.el's `charset' coding systems.
        module subroutine init_misc_lang()
            implicit none(type, external)
        end subroutine init_misc_lang
        !> Define the `charset' coding systems of georgian.el (in georgian_mod).  Port of
        !> georgian.el's `charset' coding systems.
        module subroutine init_georgian()
            implicit none(type, external)
        end subroutine init_georgian
        !> Define the language environments of chinese.el (in chinese_mod).
        !> Port of chinese.el's language environments.
        module subroutine define_chinese_language_environments()
            implicit none(type, external)
        end subroutine define_chinese_language_environments
        !> Define the language environments of cyrillic.el (in cyrillic_mod).
        !> Port of cyrillic.el's language environments.
        module subroutine define_cyrillic_language_environments()
            implicit none(type, external)
        end subroutine define_cyrillic_language_environments
        !> Define the language environments of indian.el (in indian_mod).
        !> Port of indian.el's language environments.
        module subroutine define_indian_language_environments()
            implicit none(type, external)
        end subroutine define_indian_language_environments
        !> Define the language environments of sinhala.el (in sinhala_mod).
        !> Port of sinhala.el's language environments.
        module subroutine define_sinhala_language_environments()
            implicit none(type, external)
        end subroutine define_sinhala_language_environments
        !> Define the language environments of english.el (in english_mod).
        !> Port of english.el's language environments.
        module subroutine define_english_language_environments()
            implicit none(type, external)
        end subroutine define_english_language_environments
        !> Define the language environments of ethiopic.el (in ethiopic_mod).
        !> Port of ethiopic.el's language environments.
        module subroutine define_ethiopic_language_environments()
            implicit none(type, external)
        end subroutine define_ethiopic_language_environments
        !> Define the language environments of european.el (in european_mod).
        !> Port of european.el's language environments.
        module subroutine define_european_language_environments()
            implicit none(type, external)
        end subroutine define_european_language_environments
        !> Define the language environments of czech.el (in czech_mod).
        !> Port of czech.el's language environments.
        module subroutine define_czech_language_environments()
            implicit none(type, external)
        end subroutine define_czech_language_environments
        !> Define the language environments of slovak.el (in slovak_mod).
        !> Port of slovak.el's language environments.
        module subroutine define_slovak_language_environments()
            implicit none(type, external)
        end subroutine define_slovak_language_environments
        !> Define the language environments of romanian.el (in romanian_mod).
        !> Port of romanian.el's language environments.
        module subroutine define_romanian_language_environments()
            implicit none(type, external)
        end subroutine define_romanian_language_environments
        !> Define the language environments of greek.el (in greek_mod).
        !> Port of greek.el's language environments.
        module subroutine define_greek_language_environments()
            implicit none(type, external)
        end subroutine define_greek_language_environments
        !> Define the language environments of hebrew.el (in hebrew_mod).
        !> Port of hebrew.el's language environments.
        module subroutine define_hebrew_language_environments()
            implicit none(type, external)
        end subroutine define_hebrew_language_environments
        !> Define the language environments of japanese.el (in japanese_mod).
        !> Port of japanese.el's language environments.
        module subroutine define_japanese_language_environments()
            implicit none(type, external)
        end subroutine define_japanese_language_environments
        !> Define the language environments of korean.el (in korean_mod).
        !> Port of korean.el's language environments.
        module subroutine define_korean_language_environments()
            implicit none(type, external)
        end subroutine define_korean_language_environments
        !> Define the language environments of lao.el (in lao_mod).
        !> Port of lao.el's language environments.
        module subroutine define_lao_language_environments()
            implicit none(type, external)
        end subroutine define_lao_language_environments
        !> Define the language environments of tai-viet.el (in tai_viet_mod).
        !> Port of tai-viet.el's language environments.
        module subroutine define_tai_viet_language_environments()
            implicit none(type, external)
        end subroutine define_tai_viet_language_environments
        !> Define the language environments of thai.el (in thai_mod).
        !> Port of thai.el's language environments.
        module subroutine define_thai_language_environments()
            implicit none(type, external)
        end subroutine define_thai_language_environments
        !> Define the language environments of tibetan.el (in tibetan_mod).
        !> Port of tibetan.el's language environments.
        module subroutine define_tibetan_language_environments()
            implicit none(type, external)
        end subroutine define_tibetan_language_environments
        !> Define the language environments of vietnamese.el (in vietnamese_mod).
        !> Port of vietnamese.el's language environments.
        module subroutine define_vietnamese_language_environments()
            implicit none(type, external)
        end subroutine define_vietnamese_language_environments
        !> Define the language environments of misc-lang.el (in misc_lang_mod).
        !> Port of misc-lang.el's language environments.
        module subroutine define_misc_lang_language_environments()
            implicit none(type, external)
        end subroutine define_misc_lang_language_environments
        !> Define the language environments of utf-8-lang.el (in utf_8_lang_mod).
        !> Port of utf-8-lang.el's language environments.
        module subroutine define_utf_8_lang_language_environments()
            implicit none(type, external)
        end subroutine define_utf_8_lang_language_environments
        !> Define the language environments of georgian.el (in georgian_mod).
        !> Port of georgian.el's language environments.
        module subroutine define_georgian_language_environments()
            implicit none(type, external)
        end subroutine define_georgian_language_environments
        !> Define the language environments of khmer.el (in khmer_mod).
        !> Port of khmer.el's language environments.
        module subroutine define_khmer_language_environments()
            implicit none(type, external)
        end subroutine define_khmer_language_environments
        !> Define the language environments of burmese.el (in burmese_mod).
        !> Port of burmese.el's language environments.
        module subroutine define_burmese_language_environments()
            implicit none(type, external)
        end subroutine define_burmese_language_environments
        !> Define the language environments of cham.el (in cham_mod).
        !> Port of cham.el's language environments.
        module subroutine define_cham_language_environments()
            implicit none(type, external)
        end subroutine define_cham_language_environments
        !> Define the language environments of philippine.el (in philippine_mod).
        !> Port of philippine.el's language environments.
        module subroutine define_philippine_language_environments()
            implicit none(type, external)
        end subroutine define_philippine_language_environments
        !> Define the language environments of indonesian.el (in indonesian_mod).
        !> Port of indonesian.el's language environments.
        module subroutine define_indonesian_language_environments()
            implicit none(type, external)
        end subroutine define_indonesian_language_environments
    end interface

contains

    !> Define the language environments of the language files, in the
    !> order loadup.el loads them (mule_mod calls this once).
    !> Port of loadup.el (the language files' language environments).
    subroutine define_language_environments()
        call define_chinese_language_environments()
        call define_cyrillic_language_environments()
        call define_indian_language_environments()
        call define_sinhala_language_environments()
        call define_english_language_environments()
        call define_ethiopic_language_environments()
        call define_european_language_environments()
        call define_czech_language_environments()
        call define_slovak_language_environments()
        call define_romanian_language_environments()
        call define_greek_language_environments()
        call define_hebrew_language_environments()
        call define_japanese_language_environments()
        call define_korean_language_environments()
        call define_lao_language_environments()
        call define_tai_viet_language_environments()
        call define_thai_language_environments()
        call define_tibetan_language_environments()
        call define_vietnamese_language_environments()
        call define_misc_lang_language_environments()
        call define_utf_8_lang_language_environments()
        call define_georgian_language_environments()
        call define_khmer_language_environments()
        call define_burmese_language_environments()
        call define_cham_language_environments()
        call define_philippine_language_environments()
        call define_indonesian_language_environments()
    end subroutine define_language_environments

    ! ------------------------------------------------------------------
    ! Defining coding systems
    ! ------------------------------------------------------------------

    !> Define the fundamental coding systems and their priorities.  Port of
    !> coding.c:syms_of_coding (no-conversion, undecided), the coding
    !> systems mule-conf.el defines, and reset-language-environment's
    !> priorities.
    recursive subroutine init_coding()
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
                    charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-1"], mime_charset="iso-8859-1", &
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
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           register_usage=[0, 0], &
                                           flags=[character(len=16) :: &
                                                  "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "composition"])
        call define_iso_2022_coding_system("iso-2022-7bit-ss2", iachar("$"), &
                                           doc="ISO 2022 based 7-bit encoding using SS2 for 96-charset.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           register_usage=[0, 2], &
                                           flags=[character(len=16) :: &
                                                  "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "single-shift", "composition"])
        call define_iso_2022_coding_system("iso-2022-7bit-lock", iachar("&"), &
                                           doc="ISO-2022 coding system using Locking-Shift for 96-charset.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           register_usage=[0, 1], &
                                           flags=[character(len=16) :: &
                                                  "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", "locking-shift", &
                                                  "composition"])
        call define_coding_system_alias("iso-2022-int-1", "iso-2022-7bit-lock")
        call define_iso_2022_coding_system("iso-2022-7bit-lock-ss2", iachar("i"), &
                                           doc="Mixture of ISO-2022-JP, ISO-2022-KR, and ISO-2022-CN.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: &
                                                                  "chinese-cns11643-7", "chinese-cns11643-6", &
                                                                  "chinese-cns11643-5", "chinese-cns11643-4", &
                                                                  "chinese-cns11643-3", "chinese-cns11643-2", &
                                                                  "chinese-cns11643-1", "chinese-gb2312", "korean-ksc5601", &
                                                                  "ascii"], &
                                           request_registers=[integer :: 3, 3, 3, 3, 3, 2, 1, 1, 1, 0], &
                                           register_usage=[0, 1], &
                                           flags=[character(len=16) :: &
                                                  "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "locking-shift", &
                                                  "single-shift", "init-bol"], &
                                           charset_names=[character(len=CHARSET_NAME_LEN) :: &
                                                          "ascii", "japanese-jisx0208", "japanese-jisx0208-1978", &
                                                          "latin-jisx0201", "korean-ksc5601", "chinese-gb2312", &
                                                          "chinese-cns11643-1", "chinese-cns11643-2", "chinese-cns11643-3", &
                                                          "chinese-cns11643-4", "chinese-cns11643-5", "chinese-cns11643-6", &
                                                          "chinese-cns11643-7"])
        call define_coding_system_alias("iso-2022-cjk", "iso-2022-7bit-lock-ss2")
        call define_iso_2022_coding_system("iso-2022-8bit-ss2", iachar("@"), &
                                           doc="ISO 2022 based 8-bit encoding using SS2 for 96-charset.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii"], &
                                           request_registers=[integer :: 0], &
                                           register_usage=[0, 2], &
                                           flags=[character(len=16) :: &
                                                  "ascii-at-eol", "ascii-at-cntl", "designation", "single-shift", &
                                                  "composition"])
        call define_iso_2022_coding_system("compound-text", iachar("x"), &
                                           doc="Compound text based generic encoding."//achar(10)//"This coding system is " &
                                           //"an extension of X's ""Compound Text Encoding""."//achar(10)//"It encodes man" &
                                           //"y characters using the normal ISO-2022 designation sequences" &
                                           //","//achar(10)//"but it doesn't support extended segments of CTEXT.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-iso8859-1", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: &
                                                                  "katakana-jisx0201", "latin-iso8859-1", "ascii"], &
                                           request_registers=[integer :: 1, 1, 0], &
                                           register_usage=[0, 1], &
                                           flags=[character(len=16) :: &
                                                  "ascii-at-eol", "ascii-at-cntl", "long-form", "designation", "locking-shift", &
                                                  "single-shift", "composition"], &
                                           mime_charset="x-ctext")
        call define_coding_system_alias("x-ctext", "compound-text")
        call define_coding_system_alias("ctext", "compound-text")
        call define_iso_2022_coding_system("ctext-no-compositions", iachar("x"), &
                                           doc="Compound text based generic encoding."//achar(10)//achar(10) &
                                           //"Like `compound-text', but does not produce escape sequences for compositions.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-iso8859-1", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: &
                                                                  "katakana-jisx0201", "latin-iso8859-1", "ascii"], &
                                           request_registers=[integer :: 1, 1, 0], &
                                           register_usage=[0, 1], &
                                           flags=[character(len=16) :: &
                                                  "ascii-at-eol", "ascii-at-cntl", "designation", "locking-shift", &
                                                  "single-shift"])
        id = define("us-ascii", CODING_CHARSET, iachar("-"), CAT_CHARSET, &
                    charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii"], default_char=iachar("?"), &
                    mime_charset="us-ascii", &
                    doc="Encode ASCII as-is and encode non-ASCII characters to `?'.")
        call define_coding_system_alias("iso-safe", "us-ascii")
        call define_coding_system_alias("ascii", "us-ascii")
        ! The language files, in the order loadup.el loads them.
        call init_chinese()
        call init_cyrillic()
        call init_english()
        call init_european()
        call init_romanian()
        call init_greek()
        call init_hebrew()
        call init_japanese()
        call init_korean()
        call init_lao()
        call init_thai()
        call init_tibetan()
        call init_vietnamese()
        call init_misc_lang()
        call init_georgian()
        call characters_preferred_coding_systems()

        ! mule-conf.el's file-coding-system-alist, without its functions.
        if (.not. allocated(file_coding_systems)) then
            allocate (file_coding_systems(6))
            file_coding_systems(1) = file_coding_type("\.elc\'", "utf-8-emacs", "utf-8-emacs")
            file_coding_systems(2) = file_coding_type("\.el\'", "prefer-utf-8", "prefer-utf-8")
            file_coding_systems(3) = file_coding_type("\.utf\(-8\)?\'", "utf-8", "utf-8")
            file_coding_systems(4) = file_coding_type("\(\`\|/\)loaddefs.el\'", "raw-text", "raw-text-unix")
            file_coding_systems(5) = file_coding_type("\.tar\'", "no-conversion", "no-conversion")
            file_coding_systems(6) = file_coding_type("", "undecided", "")
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
    end subroutine init_coding

    !> Define a base coding system and, unless EOL is given, its three
    !> subsidiaries; return its id.  The first coding system of a
    !> category becomes the category's.  Port of
    !> coding.c:Fdefine_coding_system_internal.
    recursive integer function define(name, ctype, mnemonic, category, eol, ascii_compatible, for_unibyte, prefer_utf_8, &
                                      default_char, charset, bom, bom_with, bom_without, big_endian, mime_charset, doc, &
                                      charset_names) result(id)
        character(len=*), intent(in) :: name
        integer, intent(in) :: ctype, mnemonic, category
        integer, intent(in), optional :: eol, default_char, charset, bom
        logical, intent(in), optional :: ascii_compatible, for_unibyte, prefer_utf_8, big_endian
        character(len=*), intent(in), optional :: bom_with, bom_without, mime_charset, doc
        character(len=*), intent(in), optional :: charset_names(:)
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
        if (present(charset_names)) call set_valid_charsets(c, charset_names)
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
    ! compatible when one of them is; without WITH_VALID_CHARSETS (an
    ! ISO-2022 coding system) only its charsets.  Port of the `charset'
    ! part of coding.c:Fdefine_coding_system_internal.
    subroutine set_valid_charsets(c, charset_names, with_valid_charsets)
        type(coding_type), intent(inout) :: c
        character(len=*), intent(in) :: charset_names(:)
        logical, intent(in), optional :: with_valid_charsets
        integer :: k, b, j, m, charset, dimension, first_byte
        logical :: fill_valid_charsets

        c%charset = CS_LIST
        c%charset_names = charset_names
        allocate (c%charset_ids(size(charset_names)))
        c%valid_charset_counts = 0
        c%valid_charsets = 0
        fill_valid_charsets = .true.
        if (present(with_valid_charsets)) fill_valid_charsets = with_valid_charsets

        do k = 1, size(charset_names)
            charset = find_charset(trim(charset_names(k)))
            c%charset_ids(k) = charset
            if (charset == 0 .or. .not. fill_valid_charsets) cycle
            dimension = charsets(charset)%dimension
            ! Where code_space holds the minimum and maximum first byte.
            first_byte = (dimension - 1)*4
            if (charsets(charset)%ascii_compatible) c%ascii_compatible = .true.

            do b = int(charsets(charset)%code_space(first_byte)), int(charsets(charset)%code_space(first_byte + 1))
                ! Insert CHARSET before the first charset of a larger
                ! dimension.
                m = c%valid_charset_counts(b) + 1

                do j = 1, c%valid_charset_counts(b)
                    if (dimension < charsets(c%valid_charsets(j, b))%dimension) then
                        m = j
                        exit
                    end if
                end do

                c%valid_charsets(m + 1:c%valid_charset_counts(b) + 1, b) = c%valid_charsets(m:c%valid_charset_counts(b), b)
                c%valid_charsets(m, b) = charset
                c%valid_charset_counts(b) = c%valid_charset_counts(b) + 1
            end do
        end do
    end subroutine set_valid_charsets

    !> Define NAME as a coding system of type `charset' (or CTYPE
    !> CODING_SJIS or CODING_BIG5) for the charsets named CHARSET_NAMES,
    !> with mnemonic character MNEMONIC (and its subsidiaries).  Port of
    !> mule.el:define-coding-system for `:coding-type' `charset',
    !> `shift-jis' and `big5'.
    subroutine define_charset_coding_system(name, mnemonic, charset_names, doc, mime_charset, default_char, ctype)
        character(len=*), intent(in) :: name
        integer, intent(in) :: mnemonic
        character(len=*), intent(in) :: charset_names(:), doc
        character(len=*), intent(in), optional :: mime_charset
        integer, intent(in), optional :: default_char, ctype
        integer :: id, actual_ctype, category

        actual_ctype = CODING_CHARSET
        category = CAT_CHARSET

        if (present(ctype)) then
            select case (ctype)
            case (CODING_SJIS)
                actual_ctype = CODING_SJIS
                category = CAT_SJIS
            case (CODING_BIG5)
                actual_ctype = CODING_BIG5
                category = CAT_BIG5
            case default
            end select
        end if

        id = define(name, actual_ctype, mnemonic, category, charset_names=charset_names, doc=doc, &
                    mime_charset=mime_charset, default_char=default_char)
    end subroutine define_charset_coding_system

    !> Define NAME as a coding system of type `iso-2022' with mnemonic
    !> character MNEMONIC (and its subsidiaries).  CHARSET_NAMES is its
    !> `:charset-list' (omitted for `iso-2022': all ISO-2022 charsets).
    !> Its `:designation' is given as mule.el's define-coding-system turns
    !> it into the attributes: INITIAL, the charsets initially designated
    !> to G0..G3 ("" for none); REQUEST_CHARSET_NAMES with
    !> REQUEST_REGISTERS, the registers the charsets ask for, in the order
    !> of the designation table; and REGISTER_USAGE, the registers of the
    !> other charsets of 94 and 96 characters (4 for none).  FLAGS are
    !> names in coding-system-iso-2022-flags (the others are ignored, as
    !> there).  Port of mule.el:define-coding-system for `:coding-type'
    !> `iso-2022' and its part of coding.c:Fdefine_coding_system_internal.
    subroutine define_iso_2022_coding_system(name, mnemonic, doc, initial, request_charset_names, request_registers, &
                                             register_usage, flags, charset_names, mime_charset, suitable_for_keyboard)
        character(len=*), intent(in) :: name, doc
        integer, intent(in) :: mnemonic
        character(len=*), intent(in) :: initial(0:3), request_charset_names(:), flags(:)
        integer, intent(in) :: request_registers(:), register_usage(2)
        character(len=*), intent(in), optional :: charset_names(:), mime_charset
        logical, intent(in), optional :: suitable_for_keyboard
        type(coding_type) :: c
        character(len=*), parameter :: SUFFIXES(0:2) = [character(len=5) :: "-unix", "-dos", "-mac"]
        integer :: i, k, id, sub, category, g1_charset

        c%name = name
        c%ctype = CODING_ISO_2022
        c%mnemonic = mnemonic
        if (present(suitable_for_keyboard)) c%suitable_for_keyboard = suitable_for_keyboard
        c%doc = doc
        c%mime_charset = ""
        if (present(mime_charset)) c%mime_charset = mime_charset

        if (present(charset_names)) then
            call set_valid_charsets(c, charset_names, with_valid_charsets=.false.)
        else
            c%charset = CS_LIST
            allocate (c%charset_ids(0), c%charset_names(1))
            c%charset_names(1) = "iso-2022"
            c%iso_flags = ISO_FULL_SUPPORT
        end if

        c%ascii_compatible = .false.

        do i = 0, 3
            c%iso_initial(i) = 0
            if (len_trim(initial(i)) > 0) c%iso_initial(i) = find_charset(trim(initial(i)))
        end do

        if (c%iso_initial(0) /= 0) c%ascii_compatible = charsets(c%iso_initial(0))%ascii_compatible
        c%iso_register_94 = register_usage(1)
        c%iso_register_96 = register_usage(2)
        allocate (c%iso_request_charsets(size(request_charset_names)), c%iso_request_registers(size(request_charset_names)))

        do k = 1, size(request_charset_names)
            c%iso_request_charsets(k) = find_charset(trim(request_charset_names(k)))
            c%iso_request_registers(k) = request_registers(k)
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
            g1_charset = c%iso_initial(1)

            if (iand(c%iso_flags, ior(ISO_LOCKING_SHIFT, ISO_FULL_SUPPORT)) /= 0 .or. g1_charset == 0) then
                category = CAT_ISO_8_ELSE
            else if (charsets(g1_charset)%dimension == 1) then
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
    ! supports all ISO-2022 charsets, Viso_2022_charset_list.  Port of
    ! coding.c:CODING_ATTR_CHARSET_LIST.
    function coding_charset_ids(id) result(ids)
        integer, intent(in) :: id
        integer, allocatable :: ids(:)

        if (iand(codings(id)%iso_flags, ISO_FULL_SUPPORT) /= 0) then
            ids = iso_2022_charset_order
        else if (codings(id)%ctype == CODING_EMACS_MULE) then
            ids = emacs_mule_charset_order
        else
            ids = codings(id)%charset_ids
        end if
    end function coding_charset_ids

    ! The register of CHARSET in the safe charsets of ISO-2022 coding
    ! system ID, -1 if it may not use CHARSET.  The definition gives each
    ! charset of the coding system's `:charset-list' register 0; one with
    ! `:charset-list' `iso-2022' gets them from its requests and register
    ! usage once the charset priority has changed, as it has at startup.
    ! Port of coding.c:CODING_ISO_REQUEST with setup_iso_safe_charsets.
    integer function iso_request(id, charset) result(register)
        integer, intent(in) :: id, charset
        integer, allocatable :: ids(:)
        integer :: k

        register = -1
        if (charset <= 0) return
        ids = coding_charset_ids(id)
        if (.not. any(ids == charset)) return

        if (iand(codings(id)%iso_flags, ISO_FULL_SUPPORT) == 0) then
            register = 0
            return
        end if

        k = findloc(codings(id)%iso_request_charsets, charset, dim=1)

        if (k > 0) then
            register = codings(id)%iso_request_registers(k)
            return
        end if

        if (charsets(charset)%iso_chars_96) then
            register = codings(id)%iso_register_96
        else
            register = codings(id)%iso_register_94
        end if

        if (register >= 4) register = -1
    end function iso_request

    ! Whether ISO-2022 coding system ID may use CHARSET.  Port of
    ! coding.c:SAFE_CHARSET_P.
    logical function is_safe_charset(id, charset)
        integer, intent(in) :: id, charset

        is_safe_charset = iso_request(id, charset) >= 0
    end function is_safe_charset

    ! Not a port.
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

    ! Not a port.
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
    !> Not a port.
    recursive integer function coding_system_id(name) result(id)
        character(len=*), intent(in) :: name
        integer :: k

        id = 0
        if (ncodings == 0) call init_coding()

        do k = 1, naliases
            if (aliases(k)%name == name) then
                id = aliases(k)%id
                return
            end if
        end do
    end function coding_system_id

    !> The name of the coding system with id ID (not an alias).  Not a port.
    function coding_system_name(id) result(name)
        integer, intent(in) :: id
        character(len=:), allocatable :: name

        name = ""
        if (id > 0 .and. id <= ncodings) name = codings(id)%name
    end function coding_system_name

    !> Return .true. if OBJECT is "" or a coding-system.
    !> See the documentation of `define_coding_system' for information
    !> about coding-system objects.  Port of coding.c:Fcoding_system_p
    !> ("" for none).
    logical function is_coding_system(object)
        character(len=*), intent(in) :: object

        is_coding_system = len(object) == 0
        if (.not. is_coding_system) is_coding_system = coding_system_id(object) > 0
    end function is_coding_system

    !> Check validity of CODING-SYSTEM.
    !> If valid, return CODING-SYSTEM, else a `coding_system_error' error condition occurs.
    !> It is valid if it is "" or a name defined as a coding system by the
    !> function `define_coding_system'.  Port of
    !> coding.c:Fcheck_coding_system.
    subroutine check_coding_system(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status

        if (is_coding_system(coding_system)) return
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
    !> "" and a name that is not a coding system).
    integer function coding_system_eol_type(coding_system) result(eol)
        character(len=*), intent(in) :: coding_system
        integer :: id

        eol = EOL_UNDECIDED
        if (len(coding_system) == 0) return
        id = coding_system_id(coding_system)
        if (id > 0) eol = codings(id)%eol
    end function coding_system_eol_type

    !> The -unix, -dos and -mac subsidiaries of CODING_SYSTEM, or empty
    !> names if its end-of-line type is decided.  Not a port.
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
    !> Port of coding.c:Fcoding_system_base ("" for none).
    function coding_system_base(coding_system) result(base)
        character(len=*), intent(in) :: coding_system
        character(len=:), allocatable :: base
        integer :: id

        base = ""
        id = coding_system_id(coding_system)
        if (id > 0) base = codings(codings(id)%base)%name
    end function coding_system_base

    !> Return the coding type of CODING-SYSTEM.
    !> A coding type is a name indicating the encoding method of CODING-SYSTEM.
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
    !> indicate the coding system.  If CODING-SYSTEM is "", return ?=.
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

    !> The MIME charset of CODING_SYSTEM ("" if none).  Not a port.
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
    !> a BOM.  Not a port.
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
    !> Not a port.
    logical function coding_system_endian(coding_system) result(big)
        character(len=*), intent(in) :: coding_system
        integer :: id

        big = .true.
        id = coding_system_id(coding_system)
        if (id > 0) big = codings(id)%big_endian
    end function coding_system_endian

    !> Whether CODING_SYSTEM reads files into unibyte buffers (`:for-unibyte').
    !> Not a port.
    logical function is_coding_system_for_unibyte(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        is_coding_system_for_unibyte = .false.
        id = coding_system_id(coding_system)
        if (id > 0) is_coding_system_for_unibyte = codings(id)%for_unibyte
    end function is_coding_system_for_unibyte

    !> Whether CODING_SYSTEM is ASCII compatible (`:ascii-compatible-p').
    !> Not a port.
    logical function is_coding_system_ascii_compatible(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        is_coding_system_ascii_compatible = .false.
        id = coding_system_id(coding_system)
        if (id > 0) is_coding_system_ascii_compatible = codings(id)%ascii_compatible
    end function is_coding_system_ascii_compatible

    !> Whether CODING_SYSTEM is `:suitable-for-keyboard'.  Not a port.
    logical function is_coding_system_suitable_for_keyboard(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        is_coding_system_suitable_for_keyboard = .false.
        id = coding_system_id(coding_system)
        if (id > 0) is_coding_system_suitable_for_keyboard = codings(id)%suitable_for_keyboard
    end function is_coding_system_suitable_for_keyboard

    !> Whether CODING_SYSTEM, of type `iso-2022', has the `7-bit' flag (`:flags').
    !> Not a port.
    logical function is_coding_system_iso_7_bit(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        is_coding_system_iso_7_bit = .false.
        id = coding_system_id(coding_system)
        if (id > 0) is_coding_system_iso_7_bit = iand(codings(id)%iso_flags, ISO_SEVEN_BITS) /= 0
    end function is_coding_system_iso_7_bit

    !> Whether CODING_SYSTEM prefers UTF-8 (`:prefer-utf-8').  Not a port.
    logical function is_coding_system_prefer_utf_8(coding_system)
        character(len=*), intent(in) :: coding_system
        integer :: id

        is_coding_system_prefer_utf_8 = .false.
        id = coding_system_id(coding_system)
        if (id > 0) is_coding_system_prefer_utf_8 = codings(id)%prefer_utf_8
    end function is_coding_system_prefer_utf_8

    !> The coding category of CODING_SYSTEM (`:category').  Port of
    !> international/mule.el:coding-system-category.
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
    !> Two coding systems are identical if both names are equal
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
        if (ncodings == 0) call init_coding()
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
    !> by adding -unix, -dos or -mac.  Not a port.
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

        if (ncodings == 0) call init_coding()
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
    !> EOL-TYPE should be `unix', `dos', `mac', or undecided (EOL_UNIX, EOL_DOS,
    !> EOL_MAC or EOL_UNDECIDED here).
    !> If EOL-TYPE is EOL_UNDECIDED, the returned coding system detects
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
    !> If CODING is "", the returned coding system detects
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
    !> not "" and specifies one) or the system's (unix).  Port of
    !> coding.c:coding_inherit_eol_type ("" means raw-text).
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
    !> The value names two coding systems, DECODING-SYSTEM and ENCODING-SYSTEM.
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
    !> `file_coding_systems'.  If no match is found, both are "".
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
        if (ncodings == 0) call init_coding()

        do k = 1, size(file_coding_systems)
            if (matcher(file_coding_systems(k)%pattern, filename)) then
                decoding = file_coding_systems(k)%decoding
                encoding = file_coding_systems(k)%encoding
                return
            end if
        end do
    end subroutine find_operation_coding_system

    ! ------------------------------------------------------------------
    ! Source text
    ! ------------------------------------------------------------------

    !> The source as coding.c's ONE_MORE_BYTE sees it: a byte (0 to 255),
    !> or, in multibyte text, minus a character that is not a raw byte.
    !> Not a port.
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

                if (is_char_byte8(ch)) then
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
    !> end of the text.  Not a port.
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
        integer :: id, c, found, k, charset, dimension, bytes_read
        integer(int64) :: src, unit_count
        logical :: check_latin_extra, matched, rejected

        info%checked = ior(info%checked, 2**CAT_CHARSET)
        ok = .false.
        id = category_id(CAT_CHARSET)
        check_latin_extra = index(codings(id)%name, "iso-8859-") == 1 .or. index(codings(id)%name, "iso-latin-") == 1
        unit_count = size(units, kind=int64)
        src = 1
        if (codings(id)%ascii_compatible) src = head_ascii + 1
        found = 0
        rejected = .false.

        do while (src <= unit_count)
            c = units(src)
            src = src + 1
            if (c < 0) cycle

            if (c > 255) then
                rejected = .true.
            else if (codings(id)%valid_charset_counts(c) == 0) then
                rejected = .true.
            else if (c >= 128 .and. c < int(z'A0') .and. check_latin_extra .and. .not. any(LATIN_EXTRA == c)) then
                rejected = .true.
            end if

            if (rejected) exit
            if (c >= 128) found = 2**CAT_CHARSET
            ! The other bytes of the code point, for one of the charsets.
            bytes_read = 1
            matched = .false.

            do k = 1, codings(id)%valid_charset_counts(c)
                charset = codings(id)%valid_charsets(k, c)
                dimension = charsets(charset)%dimension

                do while (bytes_read < dimension)
                    if (src > unit_count) then
                        info%rejected = ior(info%rejected, 2**CAT_CHARSET)
                        return
                    end if

                    c = units(src)
                    src = src + 1
                    if (c < charsets(charset)%code_space((dimension - 1 - bytes_read)*4) &
                        .or. c > charsets(charset)%code_space((dimension - 1 - bytes_read)*4 + 1)) exit
                    bytes_read = bytes_read + 1
                end do

                if (bytes_read == dimension) then
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
        integer(int64) :: src, unit_count, src_base, length
        integer :: c, final_char, charset, rejected, found, composition_count
        logical :: single_shifting, ended

        info%checked = ior(info%checked, MASK_ISO)
        ended = .false.
        ok = .false.
        unit_count = size(units, kind=int64)
        single_shifting = .false.
        rejected = 0
        found = 0
        composition_count = -1
        ! A coding system of this category is always ASCII compatible.
        src = head_ascii + 1

        do while (rejected /= MASK_ISO)
            if (.not. next_byte(c)) exit

            select case (c)
            case (ISO_CODE_ESC)
                if (inhibit_iso_escape_detection%value) cycle
                single_shifting = .false.
                if (.not. next_byte(c)) exit

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
                    charset = 0

                    if (c >= iachar("(") .and. c <= iachar("/")) then
                        ! Designation sequence for a charset of dimension 1.
                        if (.not. next_byte(final_char)) exit
                        if (final_char >= 32 .and. final_char < 128) charset = find_iso_charset(1, c >= iachar(","), final_char)

                        if (charset == 0) then
                            ! Invalid designation sequence.  Just ignore.
                            if (final_char >= 128) rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
                            cycle
                        end if
                    else if (c == iachar("$")) then
                        ! Designation sequence for a charset of dimension 2.
                        if (.not. next_byte(c)) exit

                        if (c >= iachar("@") .and. c <= iachar("B")) then
                            ! Designation for JISX0208.1978, GB2312, or JISX0208.
                            charset = find_iso_charset(2, .false., c)
                        else if (c >= iachar("(") .and. c <= iachar("/")) then
                            if (.not. next_byte(final_char)) exit
                            if (final_char >= 32 .and. final_char < 128) charset = find_iso_charset(2, c >= iachar(","), final_char)

                            if (charset == 0) then
                                if (final_char >= 128) rejected = ior(rejected, ior(MASK_ISO_7BIT, 2**CAT_ISO_7_ELSE))
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
                    call found_or_rejected(CAT_ISO_7, charset)
                    call found_or_rejected(CAT_ISO_7_TIGHT, charset)
                    call found_or_rejected(CAT_ISO_7_ELSE, charset)
                    call found_or_rejected(CAT_ISO_8_ELSE, charset)
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

                        do while (src <= unit_count)
                            src_base = src
                            if (.not. next_byte(c)) exit

                            if (c < int(z'A0')) then
                                src = src_base
                                exit
                            end if

                            length = length + 1
                        end do

                        if (btest(length, 0) .and. src <= unit_count) then
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
        logical function next_byte(c)
            integer, intent(out) :: c

            next_byte = src <= unit_count
            c = 0

            if (.not. next_byte) then
                ended = .true.
                return
            end if

            c = units(src)
            src = src + 1
        end function next_byte

        logical function has_flag(category, flag)
            integer, intent(in) :: category, flag

            has_flag = .false.
            if (category_id(category) == 0) return
            has_flag = iand(codings(category_id(category))%iso_flags, flag) /= 0
        end function has_flag

        subroutine found_or_rejected(category, charset)
            integer, intent(in) :: category, charset
            logical :: safe

            safe = .false.
            if (category_id(category) /= 0) safe = is_safe_charset(category_id(category), charset)

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
    !> composition of more than 4 bytes; ESC, SI and SO rule it out.
    !> Port of coding.c:detect_coding_emacs_mule.
    logical function detect_coding_emacs_mule(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer(int64) :: src, unit_count, src_start
        integer :: c, found, more_bytes

        info%checked = ior(info%checked, 2**CAT_EMACS_MULE)
        ok = .false.
        unit_count = size(units, kind=int64)
        ! A coding system of this category is always ASCII compatible.
        src = head_ascii + 1
        found = 0

        do while (src <= unit_count)
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
                        ! A source ending in the middle of a sequence is
                        ! rejected (the last block).
                        if (src > unit_count) then
                            info%rejected = ior(info%rejected, 2**CAT_EMACS_MULE)
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
                more_bytes = emacs_mule_bytes(c) - 1

                do while (more_bytes > 0)
                    if (src > unit_count) then
                        info%rejected = ior(info%rejected, 2**CAT_EMACS_MULE)
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
    end function detect_coding_emacs_mule

    !> Whether the text is in Shift-JIS: 0x81..0x9F and 0xE0..0xEF (0xFC
    !> with a fourth charset) start two bytes, 0xA1..0xDF are kana.  Port of
    !> coding.c:detect_coding_sjis.
    logical function detect_coding_sjis(units, head_ascii, info) result(ok)
        integer, intent(in) :: units(:)
        integer(int64), intent(in) :: head_ascii
        type(detect_info_type), intent(inout) :: info
        integer :: c, found, max_first_byte
        integer(int64) :: src, unit_count

        info%checked = ior(info%checked, 2**CAT_SJIS)
        ok = .false.
        max_first_byte = int(z'EF')
        if (size(codings(category_id(CAT_SJIS))%charset_ids) > 3) max_first_byte = int(z'FC')
        unit_count = size(units, kind=int64)
        ! A coding system of this category is always ASCII compatible.
        src = head_ascii + 1
        found = 0

        do while (src <= unit_count)
            c = units(src)
            src = src + 1
            if (c < 128) cycle

            if ((c >= int(z'81') .and. c <= int(z'9F')) .or. (c >= int(z'E0') .and. c <= max_first_byte)) then
                ! A two-byte code cut short is rejected (the last block).
                if (src > unit_count) then
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
        integer(int64) :: src, unit_count

        info%checked = ior(info%checked, 2**CAT_BIG5)
        ok = .false.
        unit_count = size(units, kind=int64)
        src = head_ascii + 1
        found = 0

        do while (src <= unit_count)
            c = units(src)
            src = src + 1
            if (c < 128) cycle

            if (c >= int(z'A1')) then
                if (src > unit_count) then
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

    !> Execute the detector of CATEGORY.  Not a port.
    logical function execute_detector(category, units, head_ascii, info) result(ok)
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
    end function execute_detector

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

        if (btest(MASK_UTF_16, category)) then
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
    !> characters in DOS EOL files are allowed.  Not a port.
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
    !> undecided.  Not a port.
    integer function with_eol(id, eol) result(sub)
        integer, intent(in) :: id, eol

        sub = id
        if (eol == EOL_UNDECIDED) return
        if (codings(id)%eol == EOL_UNDECIDED) sub = codings(id)%subsidiaries(eol)
    end function with_eol

    !> Scan the head of the text as detect_coding and detect_coding_system
    !> do: count the leading ASCII bytes and look for null and 8-bit
    !> bytes.  (ISO-2022 escape detection is Deferred.)  Not a port.
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
    !> text without finding an 8-bit code, all of it counts as ASCII.
    !> Port of the head scan of coding.c:detect_coding and
    !> detect_coding_system.
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

                    if (btest(info%found, category)) then
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
                else if (btest(info%checked, category)) then
                    if (btest(info%found, category)) then
                        hit = .true.
                        exit
                    end if
                else if (execute_detector(category, units, head_ascii, info)) then
                    if (btest(info%found, category)) then
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

                    if (.not. btest(info%rejected, category)) then
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
                if (btest(info%found, CAT_UTF_8_SIG)) then
                    found_id = coding_system_id(codings(id)%bom_with)
                else
                    found_id = coding_system_id(codings(id)%bom_without)
                end if
            end if
        else if (codings(id)%category == CAT_UTF_16_AUTO) then
            found_id = id

            if (detect_coding_utf_16(units, info)) then
                if (btest(info%found, CAT_UTF_16_LE)) then
                    found_id = coding_system_id(codings(id)%bom_with)
                else if (btest(info%found, CAT_UTF_16_BE)) then
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
    !> Not a port.
    integer function auto_choice(this, category, info) result(id)
        integer, intent(in) :: this, category
        type(detect_info_type), intent(in) :: info

        id = this

        if (category == CAT_UTF_8_AUTO) then
            if (codings(this)%bom == BOM_DETECT) then
                if (btest(info%found, CAT_UTF_8_SIG)) then
                    id = coding_system_id(codings(this)%bom_with)
                else
                    id = coding_system_id(codings(this)%bom_without)
                end if
            end if
        else if (category == CAT_UTF_16_AUTO) then
            if (codings(this)%bom == BOM_DETECT) then
                if (btest(info%found, CAT_UTF_16_LE)) then
                    id = coding_system_id(codings(this)%bom_with)
                else if (btest(info%found, CAT_UTF_16_BE)) then
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

        if (ncodings == 0) call init_coding()
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

                    if (btest(info%found, category)) then
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
                    else if (btest(info%checked, category)) then
                        if (top .and. btest(info%found, category)) then
                            hit = .true.
                            exit
                        end if
                    else if (execute_detector(category, units, head_ascii, info)) then
                        if (top .and. btest(info%found, category)) then
                            if (category == CAT_UTF_16_AUTO) then
                                if (btest(info%found, CAT_UTF_16_LE)) then
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
                    if (.not. btest(info%rejected, priorities(i))) then
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

                if (btest(info%found, category)) then
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

                if (.not. btest(mask, category)) then
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

    !> Append character C to CHARS(1:N).  Not a port.
    subroutine append(chars, n, c)
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
    end subroutine append

    !> A byte that could not be decoded, as a character: ASCII as is,
    !> others as raw bytes; a character of multibyte source stays.
    !> Not a port.
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
                call append(chars, n, -c1)
                cycle
            end if

            if (c1 < 128) then
                call append(chars, n, c1)
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
                call append(chars, n, invalid_char(c1))
            else
                call append(chars, n, c)
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
                call append(chars, n, -c1)
                cycle
            end if

            if (src > nu) then
                call append(chars, n, invalid_char(c1))
                exit
            end if

            c2 = units(src)
            src = src + 1

            if (c2 < 0) then
                call append(chars, n, invalid_char(c1))
                call append(chars, n, -c2)
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
                        call append(chars, n, ishft(surrogate, -8))
                        call append(chars, n, iand(surrogate, 255))
                    else
                        call append(chars, n, iand(surrogate, 255))
                        call append(chars, n, ishft(surrogate, -8))
                    end if

                    if (c >= int(z'D800') .and. c <= int(z'DBFF')) then
                        surrogate = c
                    else
                        surrogate = 0
                        call append(chars, n, c)
                    end if
                else
                    c = ior(ishft(surrogate - int(z'D800'), 10), c - int(z'DC00'))
                    surrogate = 0
                    call append(chars, n, int(z'10000') + c)
                end if
            else if (c >= int(z'D800') .and. c <= int(z'DBFF')) then
                surrogate = c
            else
                call append(chars, n, c)
            end if
        end do

        ! A high surrogate at the end: its bytes, as decode_coding flushes
        ! unprocessed data.
        if (surrogate /= 0) then
            if (big) then
                call append(chars, n, invalid_char(ishft(surrogate, -8)))
                call append(chars, n, invalid_char(iand(surrogate, 255)))
            else
                call append(chars, n, invalid_char(iand(surrogate, 255)))
                call append(chars, n, invalid_char(ishft(surrogate, -8)))
            end if
        end if
    end subroutine decode_utf_16

    !> Decode with the charsets of coding system ID: each code point
    !> starts with a byte one of them can start; the bytes of a code point
    !> that is not valid are decoded as raw bytes, as are those of an
    !> incomplete one at the end.  Port of coding.c:decode_coding_charset (with
    !> decode_coding's flushing of the rest).
    subroutine decode_charset(id, units, chars, n, status)
        integer, intent(in) :: id
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        type(status_type), intent(inout) :: status
        integer(int64) :: src, src_base, unit_count, code
        integer :: c, k, charset, dimension, byte_count
        logical :: incomplete

        unit_count = size(units, kind=int64)
        src = 1

        do while (src <= unit_count)
            src_base = src
            c = units(src)
            src = src + 1
            incomplete = .false.

            if (c >= 0 .and. c <= 255) then
                if (codings(id)%valid_charset_counts(c) > 0) then
                    code = c
                    byte_count = 1

                    ! The charsets are sorted by dimension: read the bytes
                    ! each needs.
                    do k = 1, codings(id)%valid_charset_counts(c)
                        charset = codings(id)%valid_charsets(k, c)
                        dimension = charsets(charset)%dimension

                        do while (byte_count < dimension)
                            if (src > unit_count) then
                                incomplete = .true.
                                exit
                            end if

                            code = ior(ishft(code, 8), int(units(src), int64))
                            src = src + 1
                            byte_count = byte_count + 1
                        end do

                        if (incomplete) exit
                        c = decode_char_id(charset, code, status)
                        if (status%code /= 0) return
                        if (c >= 0) exit
                    end do

                    if (incomplete) then
                        ! Flush out unprocessed data as binary chars.
                        do src = src_base, unit_count
                            call append(chars, n, invalid_char(units(src)))
                        end do

                        return
                    end if

                    if (c >= 0) then
                        call append(chars, n, c)
                        cycle
                    end if
                end if
            end if

            src = src_base + 1
            call append(chars, n, invalid_char(units(src_base)))
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
        integer(int64) :: src, unit_count
        integer :: c, second_byte, charset, roman, kana, kanji, kanji2

        roman = codings(id)%charset_ids(1)
        kana = codings(id)%charset_ids(2)
        kanji = codings(id)%charset_ids(3)
        kanji2 = 0
        if (size(codings(id)%charset_ids) > 3) kanji2 = codings(id)%charset_ids(4)
        unit_count = size(units, kind=int64)
        src = 1

        do while (src <= unit_count)
            c = units(src)
            src = src + 1
            charset = 0

            if (c < 0) then
                charset = 0
            else if (c < 128) then
                charset = roman
            else if (c == 128 .or. c == int(z'A0')) then
                charset = 0
            else if (c >= int(z'A1') .and. c <= int(z'DF')) then
                ! SJIS -> JISX0201-Kana
                c = iand(c, 127)
                charset = kana
            else if (c <= int(z'FC') .and. (c <= int(z'EF') .or. kanji2 /= 0)) then
                ! An incomplete code at the end is left to the final
                ! flushing as a raw byte.
                if (src > unit_count) then
                    call append(chars, n, invalid_char(c))
                    exit
                end if

                second_byte = units(src)

                if (.not. (second_byte < int(z'40') .or. second_byte == int(z'7F') .or. second_byte > int(z'FC'))) then
                    src = src + 1

                    if (c <= int(z'EF')) then
                        ! SJIS -> JISX0208
                        c = sjis_to_jis(ior(ishft(c, 8), second_byte))
                        charset = kanji
                    else
                        ! SJIS -> JISX0213-2
                        c = sjis_to_jis2(ior(ishft(c, 8), second_byte))
                        charset = kanji2
                    end if
                end if
            end if

            if (charset == 0) then
                call append(chars, n, invalid_char(units(src - 1)))
                cycle
            end if

            c = decode_char_id(charset, int(c, int64), status)
            if (status%code /= 0) return
            call append(chars, n, c)
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
        integer(int64) :: src, unit_count
        integer :: c, second_byte, charset

        unit_count = size(units, kind=int64)
        src = 1

        do while (src <= unit_count)
            c = units(src)
            src = src + 1
            charset = 0

            if (c >= 0 .and. c < 128) then
                charset = codings(id)%charset_ids(1)
            else if (c >= int(z'A1') .and. c <= int(z'FE')) then
                if (src > unit_count) then
                    call append(chars, n, invalid_char(c))
                    exit
                end if

                second_byte = units(src)

                if (.not. (second_byte < int(z'40') .or. (second_byte > int(z'7E') .and. second_byte < int(z'A1')) &
                           .or. second_byte > int(z'FE'))) then
                    src = src + 1
                    c = ior(ishft(c, 8), second_byte)
                    charset = codings(id)%charset_ids(2)
                end if
            end if

            if (charset == 0) then
                call append(chars, n, invalid_char(units(src - 1)))
                cycle
            end if

            c = decode_char_id(charset, int(c, int64), status)
            if (status%code /= 0) return
            call append(chars, n, c)
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
        integer(int64) :: src, src_base, unit_count, k
        integer :: c, code, first_byte, second_byte, third_byte, charset, plane_0_charset, plane_1_charset, flags
        integer :: extended_segment_length, register, chars_96, dimension_byte, high_byte, low_byte, segment_size
        logical :: embedded_utf_8, ok

        flags = codings(id)%iso_flags
        invocation(0) = 0
        invocation(1) = merge(-1, 1, iand(flags, ISO_SEVEN_BITS) /= 0)
        designation = codings(id)%iso_initial
        where (designation == 0) designation = -1
        plane_0_charset = invoked_charset(0)
        plane_1_charset = invoked_charset(1)
        extended_segment_length = 0
        embedded_utf_8 = .false.
        unit_count = size(units, kind=int64)
        src = 1

        main: do while (src <= unit_count)
            src_base = src
            first_byte = units(src)
            src = src + 1

            if (first_byte < 0) then
                call invalid()
                cycle main
            end if

            if (extended_segment_length > 0) then
                call append(chars, n, invalid_char(first_byte))
                extended_segment_length = extended_segment_length - 1
                cycle main
            end if

            if (embedded_utf_8) then
                if (first_byte == ISO_CODE_ESC) then
                    ! Not enough source for the end sequence: flushed.
                    if (src + 1 > unit_count) then
                        call flush_rest()
                        exit main
                    end if

                    call append(chars, n, ISO_CODE_ESC)

                    if (units(src) == iachar("%") .and. units(src + 1) == iachar("@")) then
                        src = src + 2
                        call append(chars, n, iachar("%"))
                        call append(chars, n, iachar("@"))
                        embedded_utf_8 = .false.
                    end if
                else
                    call append(chars, n, invalid_char(first_byte))
                end if

                cycle main
            end if

            charset = 0

            ! We produce at most one character.
            if (first_byte == 32 .or. first_byte == 127) then
                if (plane_0_charset < 0) then
                    charset = charset_ascii
                else if (.not. charsets(plane_0_charset)%iso_chars_96) then
                    ! This is SPACE or DEL.
                    charset = charset_ascii
                else
                    charset = plane_0_charset
                end if
            else if (first_byte > 32 .and. first_byte < 127) then
                charset = plane_0_charset
                if (plane_0_charset < 0) charset = charset_ascii
            else if (first_byte == int(z'A0') .or. first_byte == 255) then
                if (plane_1_charset < 0) then
                    call invalid()
                    cycle main
                else if (.not. charsets(plane_1_charset)%iso_chars_96 .or. iand(flags, ISO_SEVEN_BITS) /= 0) then
                    call invalid()
                    cycle main
                end if

                charset = plane_1_charset
            else if (first_byte > int(z'A0') .and. first_byte < 255) then
                if (plane_1_charset < 0) then
                    call invalid()
                    cycle main
                end if

                charset = plane_1_charset
            else if (first_byte == ISO_CODE_SO) then
                if (iand(flags, ISO_LOCKING_SHIFT) == 0 .or. designation(1) < 0) then
                    call invalid()
                    cycle main
                end if

                invocation(0) = 1
                plane_0_charset = invoked_charset(0)
                cycle main
            else if (first_byte == ISO_CODE_SI) then
                if (iand(flags, ISO_LOCKING_SHIFT) == 0) then
                    call invalid()
                    cycle main
                end if

                invocation(0) = 0
                plane_0_charset = invoked_charset(0)
                cycle main
            else if (first_byte == ISO_CODE_SS2_7 .or. first_byte == ISO_CODE_SS2) then
                if (first_byte == ISO_CODE_SS2_7 .and. iand(flags, ISO_SEVEN_BITS) == 0) then
                    call invalid()
                    cycle main
                end if

                if (iand(flags, ISO_SINGLE_SHIFT) == 0) then
                    call invalid()
                    cycle main
                end if

                ! SS2 is handled as an escape sequence of ESC 'N'.
                first_byte = iachar("N")
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (first_byte == ISO_CODE_SS3) then
                if (iand(flags, ISO_SINGLE_SHIFT) == 0) then
                    call invalid()
                    cycle main
                end if

                first_byte = iachar("O")
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (first_byte == ISO_CODE_CSI) then
                ! CSI is handled as an escape sequence of ESC '[' ...
                first_byte = iachar("[")
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (first_byte == ISO_CODE_ESC) then
                if (.not. next_byte(first_byte)) exit main
                if (.not. escape_sequence(ok)) exit main
                if (.not. ok) cycle main
            else if (first_byte < 32) then
                ! A control character (the line ends are decoded later).
                charset = charset_ascii
            else
                ! ISO_control_1.
                call invalid()
                cycle main
            end if

            ! Now we know CHARSET and the first position code FIRST_BYTE of
            ! a character.  Produce a decoded character while getting the
            ! second and third position codes SECOND_BYTE, THIRD_BYTE if
            ! necessary.
            code = first_byte

            if (charsets(charset)%dimension > 1) then
                if (.not. next_byte(second_byte)) exit main

                if (second_byte < 32 .or. (second_byte >= 128 .and. second_byte < int(z'A0')) &
                    .or. iand(first_byte, 128) /= iand(second_byte, 128)) then
                    ! SECOND_BYTE is not in a valid range.
                    call invalid()
                    cycle main
                end if

                if (charsets(charset)%dimension == 2) then
                    code = ior(ishft(first_byte, 8), second_byte)
                else
                    if (.not. next_byte(third_byte)) exit main

                    if (third_byte < 32 .or. (third_byte >= 128 .and. third_byte < int(z'A0')) &
                        .or. iand(first_byte, 128) /= iand(third_byte, 128)) then
                        ! THIRD_BYTE is not in a valid range.
                        call invalid()
                        cycle main
                    end if

                    ! As upstream, the third byte is SECOND_BYTE again.
                    code = ior(ior(ishft(first_byte, 16), ishft(second_byte, 8)), second_byte)
                end if
            end if

            code = iand(code, int(z'7F7F7F'))
            c = decode_char_id(charset, int(code, int64), status)
            if (status%code /= 0) return

            if (c < 0) then
                do k = src_base, src - 1
                    call append(chars, n, invalid_char(units(k)))
                end do
            else
                call append(chars, n, c)
            end if
        end do main

    contains

        ! The charset invoked to graphic PLANE, -1 for none.  Port of
        ! coding.c:CODING_ISO_INVOKED_CHARSET.
        integer function invoked_charset(plane)
            integer, intent(in) :: plane

            invoked_charset = -1
            if (invocation(plane) >= 0) invoked_charset = designation(invocation(plane))
        end function invoked_charset

        ! ONE_MORE_BYTE: false at the end of the source, whose bytes from
        ! SRC_BASE on are then flushed as raw bytes.
        logical function next_byte(b)
            integer, intent(out) :: b

            b = 0
            next_byte = src <= unit_count

            if (.not. next_byte) then
                call flush_rest()
                return
            end if

            b = units(src)
            src = src + 1
        end function next_byte

        subroutine flush_rest()
            do k = src_base, unit_count
                call append(chars, n, invalid_char(units(k)))
            end do
        end subroutine flush_rest

        ! The byte at SRC_BASE as a raw byte, and the safest state: ASCII
        ! designated to G0, invoked to graphic plane 0.
        subroutine invalid()
            src = src_base + 1
            call append(chars, n, invalid_char(units(src_base)))
            invocation(0) = 0
            designation(0) = charset_ascii
            plane_0_charset = charset_ascii
        end subroutine invalid

        ! Handle the escape sequence (or SS2, SS3, CSI) whose final byte is
        ! FIRST_BYTE: false at the end of the source; OK false when it was
        ! invalid or produced no character, else CHARSET is the charset of
        ! the character whose first byte is then in FIRST_BYTE.
        recursive logical function escape_sequence(ok) result(more_source)
            logical, intent(out) :: ok

            more_source = .true.
            ok = .false.

            select case (first_byte)
            case (iachar("&"))
                ! Revision of following character set.
                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                if (.not. (first_byte >= iachar("@") .and. first_byte <= iachar("~"))) then
                    call invalid()
                    return
                end if

                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                if (first_byte /= ISO_CODE_ESC) then
                    call invalid()
                    return
                end if

                if (.not. next_byte(first_byte)) then
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

                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                if (first_byte >= iachar("@") .and. first_byte <= iachar("B")) then
                    ! Designation of JISX0208.1978, GB2312.1980, or
                    ! JISX0208.1980.
                    register = 0
                    chars_96 = 0
                else if (first_byte >= int(z'28') .and. first_byte <= int(z'2B')) then
                    register = first_byte - int(z'28')
                    chars_96 = 0

                    if (.not. next_byte(first_byte)) then
                        more_source = .false.
                        return
                    end if
                else if (first_byte >= int(z'2C') .and. first_byte <= int(z'2F')) then
                    register = first_byte - int(z'2C')
                    chars_96 = 1

                    if (.not. next_byte(first_byte)) then
                        more_source = .false.
                        return
                    end if
                else
                    call invalid()
                    return
                end if

                call decode_designation(register, 2, chars_96, first_byte)
                if (register == 0) plane_0_charset = invoked_charset(0)
                if (register == 1) plane_1_charset = invoked_charset(1)
                if (chars_96 < 0) call invalid()
            case (iachar("n"), iachar("o"))
                ! Invocation of locking-shift-2 or -3.
                register = merge(2, 3, first_byte == iachar("n"))

                if (iand(flags, ISO_LOCKING_SHIFT) == 0 .or. designation(register) < 0) then
                    call invalid()
                    return
                end if

                invocation(0) = register
                plane_0_charset = invoked_charset(0)
            case (iachar("N"), iachar("O"))
                ! Invocation of single-shift-2 or -3.
                register = merge(2, 3, first_byte == iachar("N"))

                if (iand(flags, ISO_SINGLE_SHIFT) == 0 .or. designation(register) < 0) then
                    call invalid()
                    return
                end if

                charset = designation(register)

                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                if (first_byte < 32 .or. (first_byte >= 128 .and. first_byte < int(z'A0'))) then
                    call invalid()
                    return
                end if

                if (iand(flags, ISO_SEVEN_BITS) == 0) then
                    if (iand(flags, ISO_LEVEL_4) /= 0) then
                        if (first_byte >= 128) then
                            call invalid()
                            return
                        end if
                    else if (first_byte < 128) then
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

                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                select case (first_byte)
                case (iachar("]"))
                case (iachar("0"), iachar("1"), iachar("2"))
                    if (.not. next_byte(first_byte)) then
                        more_source = .false.
                        return
                    end if

                    if (first_byte /= iachar("]")) call invalid()
                case default
                    call invalid()
                end select
            case (iachar("%"))
                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                if (first_byte == iachar("/")) then
                    ! CTEXT extended segment: ESC % / [0-4] M L
                    ! --ENCODING-NAME-- \002 --BYTES--.  We keep these
                    ! bytes as is.
                    if (.not. next_byte(dimension_byte)) then
                        more_source = .false.
                        return
                    end if

                    if (dimension_byte < iachar("0") .or. dimension_byte > iachar("4")) then
                        call invalid()
                        return
                    end if

                    if (.not. next_byte(high_byte)) then
                        more_source = .false.
                        return
                    end if

                    if (high_byte < 128) then
                        call invalid()
                        return
                    end if

                    if (.not. next_byte(low_byte)) then
                        more_source = .false.
                        return
                    end if

                    if (low_byte < 128) then
                        call invalid()
                        return
                    end if

                    segment_size = (high_byte - 128)*128 + (low_byte - 128)
                    call append(chars, n, ISO_CODE_ESC)
                    call append(chars, n, iachar("%"))
                    call append(chars, n, iachar("/"))
                    call append(chars, n, dimension_byte)
                    call append(chars, n, byte8_to_char(high_byte))
                    call append(chars, n, byte8_to_char(low_byte))
                    extended_segment_length = segment_size
                else if (first_byte == iachar("G")) then
                    ! XFree86 extension for embedding UTF-8 in CTEXT: ESC % G
                    ! --UTF-8-BYTES-- ESC % @.  We keep these bytes as is.
                    call append(chars, n, ISO_CODE_ESC)
                    call append(chars, n, iachar("%"))
                    call append(chars, n, iachar("G"))
                    embedded_utf_8 = .true.
                else
                    call invalid()
                end if
            case default
                if (iand(flags, ISO_DESIGNATION) == 0) then
                    call invalid()
                    return
                end if

                if (first_byte >= int(z'28') .and. first_byte <= int(z'2B')) then
                    ! Designation of DIMENSION1_CHARS94 character set.
                    register = first_byte - int(z'28')
                    chars_96 = 0
                else if (first_byte >= int(z'2C') .and. first_byte <= int(z'2F')) then
                    ! Designation of DIMENSION1_CHARS96 character set.
                    register = first_byte - int(z'2C')
                    chars_96 = 1
                else
                    call invalid()
                    return
                end if

                if (.not. next_byte(first_byte)) then
                    more_source = .false.
                    return
                end if

                call decode_designation(register, 1, chars_96, first_byte)
                if (register == 0) plane_0_charset = invoked_charset(0)
                if (register == 1) plane_1_charset = invoked_charset(1)
                if (chars_96 < 0) call invalid()
            end select
        end function escape_sequence

        ! Designate the charset of DIMENSION, CHARS_96 and FINAL_CHAR to
        ! REGISTER; CHARS_96 becomes -1 when the escape sequence should be
        ! kept.
        ! Port of coding.c:DECODE_DESIGNATION.
        subroutine decode_designation(register, dimension, chars_96, final_char)
            integer, intent(in) :: register, dimension, final_char
            integer, intent(inout) :: chars_96
            integer :: designated, previous

            designated = 0

            if (final_char >= iachar("0") .and. final_char < 128) then
                designated = find_iso_charset(dimension, chars_96 == 1, final_char)
            end if

            if (designated == 0) then
                designation(register) = -2
                chars_96 = -1
                return
            end if

            if (.not. is_safe_charset(id, designated)) then
                designation(register) = -2
                chars_96 = -1
                return
            end if

            previous = designation(register)

            if (designated == charset_jisx0201_roman) then
                if (iand(flags, ISO_USE_ROMAN) /= 0) designated = charset_ascii
            else if (designated == charset_jisx0208_1978) then
                if (iand(flags, ISO_USE_OLDJIS) /= 0) designated = charset_jisx0208
            end if

            designation(register) = designated
            ! If there was an invalid designation to REGISTER previously, and
            ! this designation is ASCII to REGISTER, we should keep this
            ! designation sequence.
            if (previous == -2 .and. designated == charset_ascii) chars_96 = -1
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
        integer(int64) :: src, src_base, unit_count, k
        integer :: c, charset, code

        unit_count = size(units, kind=int64)
        src = 1

        main: do while (src <= unit_count)
            src_base = src
            c = units(src)
            src = src + 1

            if (c < 0) then
                call append(chars, n, -c)
                cycle main
            end if

            if (c < 128) then
                call append(chars, n, c)
                cycle main
            end if

            ! emacs_mule_char.
            charset = 0

            if (c == 128 .or. c >= int(z'A0')) then
                call invalid()
                cycle main
            end if

            select case (emacs_mule_bytes(c))
            case (2)
                charset = emacs_mule_charsets(c)

                if (charset == 0) then
                    call invalid()
                    cycle main
                end if

                if (.not. next_byte(c)) exit main

                if (c < int(z'A0')) then
                    call invalid()
                    cycle main
                end if

                code = iand(c, 127)
            case (3)
                if (c == int(z'9A') .or. c == int(z'9B')) then
                    if (.not. next_byte(c)) exit main
                    if (c >= int(z'A0')) charset = emacs_mule_charsets(c)

                    if (charset == 0) then
                        call invalid()
                        cycle main
                    end if

                    if (.not. next_byte(c)) exit main

                    if (c < int(z'A0')) then
                        call invalid()
                        cycle main
                    end if

                    code = iand(c, 127)
                else
                    charset = emacs_mule_charsets(c)

                    if (charset == 0) then
                        call invalid()
                        cycle main
                    end if

                    if (.not. next_byte(c)) exit main

                    if (c < int(z'A0')) then
                        call invalid()
                        cycle main
                    end if

                    code = ishft(iand(c, 127), 8)
                    if (.not. next_byte(c)) exit main

                    if (c < int(z'A0')) then
                        call invalid()
                        cycle main
                    end if

                    code = ior(code, iand(c, 127))
                end if
            case (4)
                if (.not. next_byte(c)) exit main
                if (c >= 0) charset = emacs_mule_charsets(c)

                if (charset == 0) then
                    call invalid()
                    cycle main
                end if

                if (.not. next_byte(c)) exit main

                if (c < int(z'A0')) then
                    call invalid()
                    cycle main
                end if

                code = ishft(iand(c, 127), 8)
                if (.not. next_byte(c)) exit main

                if (c < int(z'A0')) then
                    call invalid()
                    cycle main
                end if

                code = ior(code, iand(c, 127))
            case default
                code = c
                charset = charset_eight_bit
            end select

            c = decode_char_id(charset, int(code, int64), status)
            if (status%code /= 0) return

            if (c < 0) then
                call invalid()
                cycle main
            end if

            call append(chars, n, c)
        end do main

    contains

        ! ONE_MORE_BYTE: false at the end of the source, whose bytes from
        ! SRC_BASE on are then flushed as raw bytes.
        logical function next_byte(b)
            integer, intent(out) :: b

            b = 0
            next_byte = src <= unit_count

            if (.not. next_byte) then
                do k = src_base, unit_count
                    call append(chars, n, invalid_char(units(k)))
                end do

                return
            end if

            b = units(src)
            src = src + 1
        end function next_byte

        ! The byte at SRC_BASE as a raw byte.
        subroutine invalid()
            src = src_base + 1
            call append(chars, n, invalid_char(units(src_base)))
        end subroutine invalid
    end subroutine decode_emacs_mule

    ! Port of coding.h:SJIS_TO_JIS.
    integer function sjis_to_jis(code) result(jis_code)
        integer, intent(in) :: code
        integer :: first_byte, second_byte, row, cell

        first_byte = ishft(code, -8)
        second_byte = iand(code, 255)

        if (second_byte >= int(z'9F')) then
            row = first_byte*2 - merge(int(z'160'), int(z'E0'), first_byte >= int(z'E0'))
            cell = second_byte - int(z'7E')
        else
            row = first_byte*2 - merge(int(z'161'), int(z'E1'), first_byte >= int(z'E0'))
            cell = second_byte - merge(int(z'20'), int(z'1F'), second_byte >= int(z'7F'))
        end if

        jis_code = ior(ishft(row, 8), cell)
    end function sjis_to_jis

    ! Port of coding.h:SJIS_TO_JIS2.
    integer function sjis_to_jis2(code) result(jis_code)
        integer, intent(in) :: code
        integer :: first_byte, second_byte, row, cell

        first_byte = ishft(code, -8)
        second_byte = iand(code, 255)

        if (second_byte >= int(z'9F')) then
            select case (first_byte)
            case (int(z'F0'))
                row = int(z'28')
            case (int(z'F1'))
                row = int(z'24')
            case (int(z'F2'))
                row = int(z'2C')
            case (int(z'F3'))
                row = int(z'2E')
            case default
                row = int(z'6E') + (first_byte - int(z'F4'))*2
            end select

            cell = second_byte - int(z'7E')
        else
            if (first_byte <= int(z'F2')) then
                row = int(z'21') + (first_byte - int(z'F0'))*2
            else if (first_byte <= int(z'F4')) then
                row = int(z'2D') + (first_byte - int(z'F3'))*2
            else
                row = int(z'6F') + (first_byte - int(z'F5'))*2
            end if

            cell = second_byte - merge(int(z'20'), int(z'1F'), second_byte >= int(z'7F'))
        end if

        jis_code = ior(ishft(row, 8), cell)
    end function sjis_to_jis2

    ! Port of coding.h:JIS_TO_SJIS.
    integer function jis_to_sjis(code) result(sjis_code)
        integer, intent(in) :: code
        integer :: first_byte, second_byte, row, cell

        row = ishft(code, -8)
        cell = iand(code, 255)

        if (btest(row, 0)) then
            first_byte = row/2 + merge(int(z'71'), int(z'B1'), row < int(z'5F'))
            second_byte = cell + merge(int(z'20'), int(z'1F'), cell >= int(z'60'))
        else
            first_byte = row/2 + merge(int(z'70'), int(z'B0'), row < int(z'5F'))
            second_byte = cell + int(z'7E')
        end if

        sjis_code = ior(ishft(first_byte, 8), second_byte)
    end function jis_to_sjis

    ! Port of coding.h:JIS_TO_SJIS2.
    integer function jis_to_sjis2(code) result(sjis_code)
        integer, intent(in) :: code
        integer :: first_byte, second_byte, row, cell

        row = ishft(code, -8)
        cell = iand(code, 255)

        if (btest(row, 0)) then
            if (row <= int(z'25')) then
                first_byte = int(z'F0') + (row - int(z'21'))/2
            else if (row <= int(z'2F')) then
                first_byte = int(z'F3') + (row - int(z'2D'))/2
            else
                first_byte = int(z'F5') + (row - int(z'6F'))/2
            end if

            second_byte = cell + merge(int(z'20'), int(z'1F'), cell >= int(z'60'))
        else
            select case (row)
            case (int(z'28'))
                first_byte = int(z'F0')
            case (int(z'24'))
                first_byte = int(z'F1')
            case (int(z'2C'))
                first_byte = int(z'F2')
            case (int(z'2E'))
                first_byte = int(z'F3')
            case default
                first_byte = int(z'F4') + (row - int(z'6E'))/2
            end select

            second_byte = cell + int(z'7E')
        end if

        sjis_code = ior(ishft(first_byte, 8), second_byte)
    end function jis_to_sjis2

    !> Decode with no conversion of the text.  Port of
    !> coding.c:decode_coding_raw_text.
    subroutine decode_raw_text(units, chars, n)
        integer, intent(in) :: units(:)
        integer, allocatable, intent(inout) :: chars(:)
        integer(int64), intent(inout) :: n
        integer(int64) :: src

        do src = 1, size(units, kind=int64)
            call append(chars, n, invalid_char(units(src)))
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
    !> if MULTIBYTE, else one byte each.  Not a port.
    subroutine chars_to_text(chars, n, multibyte, text)
        integer, intent(in) :: chars(:)
        integer(int64), intent(in) :: n
        logical, intent(in) :: multibyte
        character(len=:), allocatable, intent(out) :: text
        character(len=MAX_MULTIBYTE_LENGTH) :: buffer
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
                call char_string(chars(k), buffer, nb, st)
                text(q + 1:q + nb) = buffer(1:nb)
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

    !> Append the bytes of BYTES to OUT(1:Q).  Not a port.
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
                call append(chars, n, CR)
                call append(chars, n, LF)
            else if (c == LF .and. eol == EOL_MAC) then
                call append(chars, n, CR)
            else
                call append(chars, n, c)
            end if
        end do
    end subroutine consume_chars

    !> The internal (multibyte) form of character C.  Not a port.
    function internal_bytes(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buffer
        integer(int32) :: nb
        type(status_type) :: st

        call char_string(c, buffer, nb, st)
        s = buffer(1:nb)
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
        integer :: id, eol, c, u, charset, b
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

                if (is_char_byte8(c)) then
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
                else if (is_char_byte8(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else
                    charset = char_charset_id(c, code, status, codings(id)%charset_ids)
                    if (status%code /= 0) return

                    if (charset == 0) then
                        call emit(out, q, char(codings(id)%default_char))
                    else
                        do b = charsets(charset)%dimension - 1, 0, -1
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
                else if (is_char_byte8(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else
                    charset = char_charset_id(c, code, status, emacs_mule_charset_order)
                    if (status%code /= 0) return

                    if (charset == 0) then
                        c = codings(id)%default_char

                        if (c < 128) then
                            call emit(out, q, char(c))
                            cycle
                        end if

                        charset = char_charset_id(c, code, status, emacs_mule_charset_order)
                        if (status%code /= 0) return
                    end if

                    call emit_emacs_mule(charset, code)
                end if
            end do
        case (CODING_SJIS, CODING_BIG5)
            ! encode_coding_sjis and encode_coding_big5.
            do k = 1, n
                c = chars(k)

                if (codings(id)%ascii_compatible .and. c < 128) then
                    call emit(out, q, char(c))
                else if (is_char_byte8(c)) then
                    call emit(out, q, char(char_to_byte8(c)))
                else
                    charset = char_charset_id(c, code, status, codings(id)%charset_ids)
                    if (status%code /= 0) return

                    if (charset == 0) then
                        charset = char_charset_id(codings(id)%default_char, code, status, codings(id)%charset_ids)
                        if (status%code /= 0) return
                    end if

                    call emit_sjis_big5(charset, int(code))
                end if
            end do
        case default
            ! encode_coding_raw_text (also for undecided).
            do k = 1, n
                c = chars(k)

                if (c < 128) then
                    call emit(out, q, char(c))
                else if (is_char_byte8(c)) then
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

        ! Write the leading codes of CHARSET (EMACS_MULE_LEADING_CODES) and
        ! the position codes of CODE.  Port of the output of
        ! coding.c:encode_coding_emacs_mule.
        subroutine emit_emacs_mule(charset, code)
            integer, intent(in) :: charset
            integer(int64), intent(in) :: code
            integer :: emacs_mule_id, leading_code, extended_code, position_codes

            emacs_mule_id = charsets(charset)%emacs_mule_id
            extended_code = emacs_mule_id

            if (emacs_mule_id < int(z'A0')) then
                leading_code = emacs_mule_id
                extended_code = 0
            else if (emacs_mule_id < int(z'E0')) then
                leading_code = int(z'9A')
            else if (emacs_mule_id < int(z'F0')) then
                leading_code = int(z'9B')
            else if (emacs_mule_id < int(z'F5')) then
                leading_code = int(z'9C')
            else
                leading_code = int(z'9D')
            end if

            call emit(out, q, char(leading_code))
            if (extended_code /= 0) call emit(out, q, char(extended_code))

            if (charsets(charset)%dimension == 1) then
                call emit(out, q, char(ior(int(code), 128)))
            else
                position_codes = ior(int(code), int(z'8080'))
                call emit(out, q, char(ishft(position_codes, -8))//char(iand(position_codes, 255)))
            end if
        end subroutine emit_emacs_mule

        ! Write CODE of CHARSET as Shift-JIS or Big5 bytes.  Port of the
        ! output of coding.c:encode_coding_sjis and encode_coding_big5.
        subroutine emit_sjis_big5(charset, code)
            integer, intent(in) :: charset, code
            integer :: row, sjis_code, kanji2

            kanji2 = 0
            if (size(codings(id)%charset_ids) > 3) kanji2 = codings(id)%charset_ids(4)

            if (codings(id)%ctype == CODING_BIG5) then
                if (charset == codings(id)%charset_ids(2)) then
                    call emit(out, q, char(ishft(code, -8))//char(iand(code, 255)))
                else
                    call emit(out, q, char(iand(code, 127)))
                end if
            else if (charset == codings(id)%charset_ids(3)) then
                sjis_code = jis_to_sjis(code)
                call emit(out, q, char(ishft(sjis_code, -8))//char(iand(sjis_code, 255)))
            else if (charset == codings(id)%charset_ids(2)) then
                call emit(out, q, char(ior(code, 128)))
            else if (kanji2 /= 0 .and. charset == kanji2) then
                row = ishft(code, -8)

                if (row == int(z'21') .or. (row >= int(z'23') .and. row <= int(z'25')) .or. row == int(z'28') &
                    .or. (row >= int(z'2C') .and. row <= int(z'2F')) .or. row >= int(z'6E')) then
                    sjis_code = jis_to_sjis2(code)
                    call emit(out, q, char(ishft(sjis_code, -8))//char(iand(sjis_code, 255)))
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
    !> resets at line ends and control characters its flags ask for.
    !> Port of coding.c:encode_coding_iso_2022 (the preferred charsets of
    !> charset annotations are Deferred, as the annotations are).
    subroutine encode_iso_2022(id, chars, n, eol, out, q, status)
        integer, intent(in) :: id
        integer, intent(in) :: chars(:)
        integer(int64), intent(in) :: n
        integer, intent(in) :: eol
        character(len=:), allocatable, intent(inout) :: out
        integer(int64), intent(inout) :: q
        type(status_type), intent(inout) :: status
        integer, allocatable :: charset_ids(:)
        integer :: invocation(0:1), designation(0:3), initial(0:3)
        integer(int64) :: k, code
        integer :: c, charset, flags
        logical :: ascii_compatible, single_shifting, bol_designation

        flags = codings(id)%iso_flags
        charset_ids = coding_charset_ids(id)
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
                    if (iand(flags, ISO_INIT_AT_BOL) /= 0) designation = initial
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
            else if (is_char_byte8(c)) then
                call emit(out, q, char(char_to_byte8(c)))
            else
                charset = char_charset_id(c, code, status, charset_ids)
                if (status%code /= 0) return

                if (charset == 0) then
                    if (iand(flags, ISO_SAFE) /= 0) then
                        c = iachar("?")
                        charset = charset_ascii
                    else
                        c = codings(id)%default_char
                        charset = char_charset_id(c, code, status, charset_ids)
                        if (status%code /= 0) return
                    end if
                end if

                call encode_iso_character(charset, c)
            end if
        end do

        if (iand(flags, ISO_RESET_AT_EOL) /= 0) call reset_plane_and_register()

    contains

        ! Port of coding.c:ENCODE_ISO_CHARACTER.
        subroutine encode_iso_character(charset, ch)
            integer, intent(in) :: charset, ch
            integer(int64) :: code

            code = encode_char_id(charset, ch, status)

            if (charsets(charset)%dimension == 1) then
                call encode_dimension(charset, 1, int(code), 0)
            else
                call encode_dimension(charset, 2, int(ishft(code, -8)), int(iand(code, 255_int64)))
            end if
        end subroutine encode_iso_character

        ! Port of coding.c:ENCODE_ISO_CHARACTER_DIMENSION1 and _DIMENSION2.
        subroutine encode_dimension(charset, dimension, first_byte, second_byte)
            integer, intent(in) :: charset, dimension, first_byte, second_byte
            integer :: used_charset

            used_charset = charset

            if (dimension == 1) then
                if (iand(flags, ISO_USE_ROMAN) /= 0 .and. used_charset == charset_ascii) used_charset = charset_jisx0201_roman
            else
                if (iand(flags, ISO_USE_OLDJIS) /= 0 .and. used_charset == charset_jisx0208) used_charset = charset_jisx0208_1978
            end if

            do
                if (single_shifting) then
                    if (iand(flags, ISO_SEVEN_BITS) /= 0) then
                        call emit_codes(dimension, iand(first_byte, 127), iand(second_byte, 127))
                    else
                        call emit_codes(dimension, ior(first_byte, 128), ior(second_byte, 128))
                    end if

                    single_shifting = .false.
                    return
                else if (used_charset == invoked_charset(0)) then
                    call emit_codes(dimension, iand(first_byte, 127), iand(second_byte, 127))
                    return
                else if (used_charset == invoked_charset(1)) then
                    call emit_codes(dimension, ior(first_byte, 128), ior(second_byte, 128))
                    return
                else
                    ! Since CHARSET is not yet invoked to any graphic
                    ! planes, we must invoke it, or, at first, designate it
                    ! to some graphic register.  Then repeat the loop to
                    ! actually produce the character.
                    call invocation_designation(used_charset)
                end if
            end do
        end subroutine encode_dimension

        subroutine emit_codes(dimension, first_byte, second_byte)
            integer, intent(in) :: dimension, first_byte, second_byte

            if (dimension == 1) then
                call emit(out, q, char(first_byte))
            else
                call emit(out, q, char(first_byte)//char(second_byte))
            end if
        end subroutine emit_codes

        ! The charset invoked to graphic PLANE, -1 for none.  Port of
        ! coding.c:CODING_ISO_INVOKED_CHARSET.
        integer function invoked_charset(plane)
            integer, intent(in) :: plane

            invoked_charset = -1
            if (invocation(plane) >= 0) invoked_charset = designation(invocation(plane))
        end function invoked_charset

        ! Port of coding.c:encode_invocation_designation.
        subroutine invocation_designation(charset)
            integer, intent(in) :: charset
            integer :: register

            ! At first, check designations.
            do register = 0, 3
                if (designation(register) == charset) exit
            end do

            if (register >= 4) then
                ! CHARSET is not yet designated to any graphic registers.
                ! At first check the requested designation.
                register = iso_request(id, charset)
                ! Since CHARSET requests no special designation, designate
                ! it to graphic register 0.
                if (register < 0) register = 0
                call encode_designation(charset, register)
            end if

            if (invocation(0) /= register .and. invocation(1) /= register) then
                ! Since the graphic register REGISTER is not invoked to any
                ! graphic planes, invoke it to graphic plane 0.
                select case (register)
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

        subroutine single_shift(final_char, eight_bit_code)
            integer, intent(in) :: final_char, eight_bit_code

            if (iand(flags, ISO_SEVEN_BITS) /= 0) then
                call emit(out, q, char(ISO_CODE_ESC)//char(final_char))
            else
                call emit(out, q, char(eight_bit_code))
            end if

            single_shifting = .true.
        end subroutine single_shift

        ! Port of coding.c:ENCODE_DESIGNATION.
        subroutine encode_designation(charset, register)
            integer, intent(in) :: charset, register
            character(len=*), parameter :: INTERMEDIATES_94 = "()*+", INTERMEDIATES_96 = ",-./"
            integer :: final_char, revision

            final_char = charsets(charset)%iso_final
            revision = -1
            if (iand(flags, ISO_REVISION) /= 0) revision = charsets(charset)%iso_revision
            if (revision >= 0) call emit(out, q, char(ISO_CODE_ESC)//"&"//char(iachar("@") + revision))
            call emit(out, q, char(ISO_CODE_ESC))

            if (charsets(charset)%dimension == 1) then
                if (.not. charsets(charset)%iso_chars_96) then
                    call emit(out, q, INTERMEDIATES_94(register + 1:register + 1))
                else
                    call emit(out, q, INTERMEDIATES_96(register + 1:register + 1))
                end if
            else
                call emit(out, q, "$")

                if (.not. charsets(charset)%iso_chars_96) then
                    if (iand(flags, ISO_LONG_FORM) /= 0 .or. register /= 0 .or. final_char < iachar("@") &
                        .or. final_char > iachar("B")) call emit(out, q, INTERMEDIATES_94(register + 1:register + 1))
                else
                    call emit(out, q, INTERMEDIATES_96(register + 1:register + 1))
                end if
            end if

            call emit(out, q, char(final_char))
            designation(register) = charset
        end subroutine encode_designation

        ! Port of coding.c:ENCODE_RESET_PLANE_AND_REGISTER.
        subroutine reset_plane_and_register()
            integer :: register

            if (invocation(0) /= 0) then
                call emit(out, q, char(ISO_CODE_SI))
                invocation(0) = 0
            end if

            do register = 0, 3
                if (initial(register) >= 0) then
                    if (designation(register) /= initial(register)) call encode_designation(initial(register), register)
                end if
            end do
        end subroutine reset_plane_and_register

        ! The designations the charsets of the line from CHARS(FROM) ask
        ! for.  Port of coding.c:encode_designation_at_bol.
        subroutine designation_at_bol(from)
            integer(int64), intent(in) :: from
            integer :: requested(0:3), found, register, ch, charset
            integer(int64) :: j, code

            requested = -1
            found = 0
            j = from

            do while (j <= n .and. found < 4)
                ch = chars(j)
                j = j + 1
                if (ch == 10) exit
                charset = char_charset_id(ch, code, status, charset_ids)
                if (charset == 0) cycle
                register = iso_request(id, charset)

                if (register >= 0) then
                    if (requested(register) < 0) then
                        found = found + 1
                        requested(register) = charset
                    end if
                end if
            end do

            if (found > 0) then
                do register = 0, 3
                    if (requested(register) >= 0) then
                        if (designation(register) /= requested(register)) call encode_designation(requested(register), register)
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
    logical function is_char_encodable(c, coding)
        integer, intent(in) :: c
        character(len=*), intent(in) :: coding
        type(status_type) :: status
        integer(int64) :: code
        integer :: id

        is_char_encodable = .false.
        id = coding_system_id(coding)
        if (id == 0) return

        select case (codings(id)%charset)
        case (CS_LIST)
            if (c < 128 .and. codings(id)%ascii_compatible) then
                is_char_encodable = .true.
            else
                is_char_encodable = char_charset_id(c, code, status, coding_charset_ids(id)) /= 0
            end if
        case (CS_ASCII)
            is_char_encodable = c < 128
        case (CS_UNICODE)
            is_char_encodable = c <= MAX_UNICODE_CHAR
        case (CS_EMACS)
            is_char_encodable = c <= MAX_5_BYTE_CHAR
        case default
            is_char_encodable = .true.
        end select
    end function is_char_encodable

    !> Check if TEXT (multibyte, starting at position FIRST) is encodable
    !> by CODING_SYSTEMS.
    !>
    !> If all coding systems in CODING-SYSTEM-LIST can encode the region, the
    !> function returns an empty RESULT.
    !>
    !> If some of the coding systems cannot encode the whole region, value is
    !> a table, each element of which has the form (CODING-SYSTEM POS1 POS2 ...),
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
                    if (.not. is_char_encodable(c, trim(coding_systems(k)))) then
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

        if (ncodings == 0) call init_coding()
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
                if (safe(id)) safe(id) = is_char_encodable(c, codings(id)%name)
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
    !> If CODING-SYSTEM is "", it returns STRING itself.  The value is a
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
    !> into RESULT, the bytes.  If CODING-SYSTEM is "", it returns STRING
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
    !> Not a port.
    subroutine region_bytes(start, end, from, to, bytes, status)
        integer(int64), intent(in) :: start, end
        integer(int64), intent(out) :: from, to
        character(len=:), allocatable, intent(out) :: bytes
        type(status_type), intent(out) :: status

        from = min(start, end)
        to = max(start, end)

        if (from < buffers(current)%begv .or. to > buffers(current)%zv) then
            call set_error(status, FM_ARGS_OUT_OF_RANGE)
            bytes = ""
            return
        end if

        bytes = buf_bytes(buffers(current), char_to_byte(from), char_to_byte(to))
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

        call detect_coding_bytes(bytes, buffers(current)%multibyte, names, highest)
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

    !> Replace the region by its decoded or (ENCODE) encoded form.
    !> Port of coding.c:code_convert_region.
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
            call encode_coding_text(bytes, coding_system, text, used, status, multibyte=buffers(current)%multibyte)
            ! The bytes, as raw-byte characters in a multibyte buffer.
            if (status%ok() .and. buffers(current)%multibyte) text = str_to_multibyte(text)
        else
            call decode_coding_bytes(bytes, coding_system, text, used, status, src_multibyte=buffers(current)%multibyte, &
                                     dst_multibyte=buffers(current)%multibyte)
        end if

        if (.not. status%ok()) return
        call temp_set_point_both(buffers(current), from, char_to_byte(from))
        call del_range(from, to, status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return

        if (buffers(current)%multibyte) then
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
    !> terminal ("" for none).  Port of coding.c:Fterminal_coding_system.
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

    !> Return coding system specified for decoding keyboard input.
    !> Port of coding.c:Fkeyboard_coding_system.
    function keyboard_coding_system() result(name)
        character(len=:), allocatable :: name

        name = "utf-8-unix"
        if (allocated(keyboard_coding)) name = keyboard_coding
    end function keyboard_coding_system
end module coding_mod
