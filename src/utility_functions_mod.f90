! utility_functions_mod.f90 -- equality, sequences, property lists, symbol properties.
! Ported from GNU Emacs 31.1 src/fns.c.
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

!> Where upstream takes a TEST function object, these take an
!> optional Fortran procedure with the `value_test_procedure' interface; the
!> default is `eq' (data_mod:value_eq), as upstream.
module utility_functions_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_mod, only: status_type, set_error, WRONG_TYPE_ARGUMENT_CODE, ARGUMENTS_OUT_OF_RANGE_CODE, GENERIC_ERROR_CODE
    use print_mod, only: fortran_string
    use character_mod, only: multibyte_characters_in_text, string_character_and_length, bytes_by_character_head, &
                             is_character_byte8, character_to_byte8
    use case_table_mod, only: upcase
    use value_mod, only: atom_type, list, list_elements, list_element, list_length, list_tail, &
                         make_list, symbols, v_buffer, V_INTEGER, v_keymap, V_ARRAY, v_marker, V_NULL, &
                         v_string, V_NAME, v_syntax_table, v_window, value_type, vtag, xsymbol, string_intervals, &
                         set_string_intervals
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, resolve_buffer, nsberror, BEGINNING_BYTE
    use system_dependent_mod, only: get_load_averages
    use names_mod, only: ensure_obarray
    use data_mod, only: value_eq, check_symbol
    use marker_mod, only: markers, resolve_marker
    use keymap_mod, only: keymap_equal
    implicit none(type, external)
    private

    abstract interface
        !> A two-argument test such as `eq', `equal' or `string<'.
        !> Not a port.
        logical function value_test_procedure(a, b)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: a, b
        end function value_test_procedure
    end interface

    public :: value_test_procedure
    public :: value_equal
    public :: is_string_less, is_string_less_text, is_string_greater, compare_strings, COMPARE_EQUAL, count_characters, substring

    !> `is_string_less(a, b)': whether string A sorts before B, comparing
    !> characters by code (a prefix first); A and B are values (symbols
    !> stand for their names) or character strings.  The character
    !> specific is not the comparison `<`, which pads the shorter string
    !> with blanks.  Port of fns.c:Fstring_lessp.
    interface is_string_less
        module procedure is_string_less, is_string_less_text
    end interface is_string_less

    !> The value of `compare_strings' for strings that match (t).
    integer(int64), parameter :: COMPARE_EQUAL = 0
    public :: plist_get, plist_put, plist_member, get, put
    public :: read_yes_or_no
    public :: string_search, string_distance, string_character_to_byte, string_byte_to_character, substring_no_properties
    public :: is_string_equal, is_string_collate_less, is_string_collate_equal, clear_string
    public :: value_equal_including_properties
    public :: base64_encode_string, base64url_encode_string, base64_decode_string
    public :: base64_encode_region, base64url_encode_region, base64_decode_region
    ! Public for submodule utility_functions_buffer_mod: gfortran does not link a private
    ! module procedure that only a submodule calls.
    public :: base64_encode_1, base64_decode_1
    ! Public for submodule utility_functions_hash_mod, likewise.
    public :: validate_subarray
    public :: buffer_line_statistics, random, load_average, locale_info, initialize_functions
    public :: md5, secure_hash, secure_hash_algorithms, buffer_hash, hash_random_bytes

    !> The largest integer `random' gives (lisp.h:MOST_POSITIVE_FIXNUM on a
    !> 64-bit host).
    integer(int64), parameter :: MOST_POSITIVE_FIXNUM = 2_int64**61 - 1
    !> The value bits of upstream's small integers, 62 (lisp.h:INTMASK).
    integer(int64), parameter :: INTMASK = 2_int64**62 - 1
    !> The length of a base64-encoded line (fns.c:MIME_LINE_LENGTH).
    integer, parameter :: MIME_LINE_LENGTH = 76
    !> The base64 alphabets, standard and URL (fns.c:base64_value_to_char).
    character(len=64), parameter :: BASE64_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    character(len=64), parameter :: BASE64URL_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    !> Whether the random numbers have been seeded (upstream seeds them at
    !> startup, emacs.c's initialize_random).
    logical :: random_initialized = .false.

    !> Return a pseudo-random integer.
    !> By default, return an integer from -2**61 to 2**61 - 1; all are equally likely.
    !> With positive integer LIMIT, return random integer in interval [0,LIMIT).
    !> With argument .true., set the random number seed from the system's entropy
    !> pool if available, otherwise from less-random volatile data such as the time.
    !> With a string argument, set the seed based on the string's contents.
    !>
    !> See Info node `(elisp)Random Numbers' for more details.
    !>
    !> `random()', `random(limit, status)' (a LIMIT that is not positive
    !> is an `arguments_out_of_range' error condition), `random(seed)' and
    !> `random(reseed)'; the last two return a random integer after
    !> seeding.  Port of fns.c:Frandom (no integers beyond int64).
    interface random
        module procedure random_integer, random_below, random_below_default, random_seeded, random_reseeded
    end interface random

    !> Return the 1 minute, 5 minute and 15 minute load averages.
    !>
    !> Each of the three load averages is multiplied by 100, then converted
    !> to integer.
    !>
    !> When LOADS is real(real64), the values are real instead of integers.
    !> These are not multiplied by 100.
    !>
    !> If the 5-minute or 15-minute load averages are not available, return a
    !> shorter array, containing only those averages which are available.
    !>
    !> An error condition occurs if the load average can't be obtained.
    !> `call load_average(loads, status)'.  Port of fns.c:Fload_average.
    interface load_average
        module procedure load_average_integer, load_average_real
    end interface load_average

    !> The items of `locale_info' (upstream's codeset, days, months and
    !> paper).
    integer, parameter, public :: LOCALE_INFO_CODESET = 1, LOCALE_INFO_DAYS = 2, LOCALE_INFO_MONTHS = 3, &
                                  LOCALE_INFO_PAPER = 4

    !> Access locale data ITEM for the current C locale, if available.
    !> ITEM should be one of the following:
    !>
    !> `LOCALE_INFO_CODESET', returning the character set as a string (locale item CODESET);
    !>
    !> `LOCALE_INFO_DAYS', returning a 7-element array of day names (locale items DAY_n);
    !>
    !> `LOCALE_INFO_MONTHS', returning a 12-element array of month names (locale items MON_n);
    !>
    !> `LOCALE_INFO_PAPER', returning an array of 2 integers (WIDTH HEIGHT) for the default
    !>   paper size, both measured in millimeters (locale items _NEWLINE_PAPER_WIDTH,
    !>   _NEWLINE_PAPER_HEIGHT).
    !>
    !> If the system can't provide such information through a call to
    !> `newline_langinfo', or if ITEM isn't from the list above, the result is
    !> unallocated.
    !>
    !> See also Info node `(libc)Locales'.
    !>
    !> `call locale_info(item, text)' (CODESET), `call locale_info(item,
    !> names)' (DAYS, MONTHS) or `call locale_info(item, sizes)' (PAPER); an
    !> ITEM of another kind than the result leaves it unallocated.  The
    !> locale is the C locale (glibc's values).  Port of fns.c:Flocale_info.
    interface locale_info
        module procedure locale_info_text, locale_info_names, locale_info_sizes
    end interface locale_info

    !> The algorithms of `secure_hash' (upstream's symbols md5, sha1,
    !> sha224, sha256, sha384, sha512, sha3-224, sha3-256, sha3-384 and
    !> sha3-512).
    integer, parameter, public :: SECURE_HASH_MD5 = 1, SECURE_HASH_SHA1 = 2, SECURE_HASH_SHA224 = 3, &
                                  SECURE_HASH_SHA256 = 4, SECURE_HASH_SHA384 = 5, SECURE_HASH_SHA512 = 6, &
                                  SECURE_HASH_SHA3_224 = 7, SECURE_HASH_SHA3_256 = 8, SECURE_HASH_SHA3_384 = 9, &
                                  SECURE_HASH_SHA3_512 = 10

    !> Return MD5 message digest of OBJECT, a buffer or string.
    !>
    !> A message digest is the string representation of the cryptographic checksum
    !> of a document, and the algorithm to calculate it is defined in RFC 1321.
    !> The MD5 digest is 32-character long.
    !>
    !> The two optional arguments BEGINNING and END are character positions
    !> specifying for which part of OBJECT the message digest should be
    !> computed.  If absent, the digest is computed for the whole
    !> OBJECT.
    !>
    !> The MD5 message digest is computed from the result of encoding the
    !> text in a coding system, not directly from the internal Emacs form of
    !> the text.  The optional argument CODING_SYSTEM specifies which
    !> coding system to encode the text with.  It should be the same coding
    !> system that you used or will use when actually writing the text into a
    !> file.
    !>
    !> If CODING_SYSTEM is "" or absent, the default depends on OBJECT.  If
    !> OBJECT is a buffer, the default for CODING_SYSTEM is whatever coding
    !> system would be chosen by default for writing this text into a file.
    !>
    !> If OBJECT is a string, the most preferred coding system (see the
    !> command `prefer_coding_system') is used.
    !>
    !> If NOERROR is true, silently assume the `raw-text' coding if the
    !> guesswork fails.  Normally, an error condition occurs in such case.
    !>
    !> This function is semi-obsolete, since for most purposes it is equivalent
    !> to calling `secure_hash' with `SECURE_HASH_MD5' as the ALGORITHM
    !> argument.  The OBJECT, BEGINNING and END arguments have the same meanings as
    !> in `secure_hash'.
    !>
    !> Note that MD5 is not collision resistant and should not be used for
    !> anything security-related.  See `secure_hash' for alternatives.
    !>
    !> `call md5(object, hash, status, beginning, end, coding_system,
    !> noerror)', OBJECT a string (with `multibyte=.false.' for a unibyte
    !> one, as base64_encode_string takes it) or a buffer.  Port of
    !> fns.c:Fmd5.  (In submodule utility_functions_hash_mod, as the coding systems
    !> depend on this module.)
    interface md5
        !> The MD5 digest of STRING.  Port of fns.c:Fmd5 (a string).
        recursive module subroutine md5_string(string, hash, status, beginning, end, coding_system, noerror, multibyte)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: hash
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: beginning, end
            character(len=*), intent(in), optional :: coding_system
            logical, intent(in), optional :: noerror, multibyte
        end subroutine md5_string

        !> The MD5 digest of the text of BUFFER.  Port of fns.c:Fmd5 (a
        !> buffer).
        recursive module subroutine md5_buffer(buffer, hash, status, beginning, end, coding_system, noerror)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            character(len=:), allocatable, intent(out) :: hash
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: beginning, end
            character(len=*), intent(in), optional :: coding_system
            logical, intent(in), optional :: noerror
        end subroutine md5_buffer
    end interface md5

    !> Return the secure hash of OBJECT, a buffer or string.
    !> ALGORITHM is a named constant specifying the hash to use:
    !> - SECURE_HASH_MD5      corresponds to MD5, produces a 32-character signature
    !> - SECURE_HASH_SHA1     corresponds to SHA-1, produces a 40-character signature
    !> - SECURE_HASH_SHA224   corresponds to SHA-2 (SHA-224), produces a 56-character signature
    !> - SECURE_HASH_SHA256   corresponds to SHA-2 (SHA-256), produces a 64-character signature
    !> - SECURE_HASH_SHA384   corresponds to SHA-2 (SHA-384), produces a 96-character signature
    !> - SECURE_HASH_SHA512   corresponds to SHA-2 (SHA-512), produces a 128-character signature
    !> - SECURE_HASH_SHA3_224 corresponds to SHA-3 (SHA3-224), produces a 56-character signature
    !> - SECURE_HASH_SHA3_256 corresponds to SHA-3 (SHA3-256), produces a 64-character signature
    !> - SECURE_HASH_SHA3_384 corresponds to SHA-3 (SHA3-384), produces a 96-character signature
    !> - SECURE_HASH_SHA3_512 corresponds to SHA-3 (SHA3-512), produces a 128-character signature
    !>
    !> The two optional arguments BEGINNING and END are positions specifying for
    !> which part of OBJECT to compute the hash.  If absent, uses the
    !> whole OBJECT.
    !>
    !> The full list of algorithms can be obtained with `secure_hash_algorithms'.
    !>
    !> If BINARY is true, returns a string in binary form.  In this case,
    !> the function returns a unibyte string whose length is half the number
    !> of characters it returns when BINARY is false.
    !>
    !> Note that MD5 and SHA-1 are not collision resistant and should not be
    !> used for anything security-related.  For these applications, use one
    !> of the other hash types instead, e.g. SECURE_HASH_SHA256 or SECURE_HASH_SHA512.
    !>
    !> `call secure_hash(algorithm, object, hash, status, beginning, end,
    !> binary)', OBJECT a string (with `multibyte=.false.' for a unibyte
    !> one) or a buffer.  For a string, BEGINNING and END index the bytes
    !> of its encoded text, as upstream's.  Port of fns.c:Fsecure_hash (the
    !> `iv-auto' OBJECT is `hash_random_bytes').  (In submodule utility_functions_hash_mod.)
    interface secure_hash
        !> The secure hash of STRING.  Port of fns.c:Fsecure_hash (a
        !> string).
        recursive module subroutine secure_hash_string(algorithm, string, hash, status, beginning, end, binary, multibyte)
            implicit none(type, external)
            integer, intent(in) :: algorithm
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: hash
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: beginning, end
            logical, intent(in), optional :: binary, multibyte
        end subroutine secure_hash_string

        !> The secure hash of the text of BUFFER.  Port of
        !> fns.c:Fsecure_hash (a buffer).
        recursive module subroutine secure_hash_buffer(algorithm, buffer, hash, status, beginning, end, binary)
            implicit none(type, external)
            integer, intent(in) :: algorithm
            type(handle_type), intent(in) :: buffer
            character(len=:), allocatable, intent(out) :: hash
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: beginning, end
            logical, intent(in), optional :: binary
        end subroutine secure_hash_buffer
    end interface secure_hash

    !> True means `read_yes_or_no' accepts single-key answers "y" or "n".
    !> When true, `read_yes_or_no' will use `read_y_or_n' to read the answer.
    !> This means the user will be able to press just one key to answer, whereas
    !> by default the user needs to type the full "yes" or "no" response
    !> and then press RET.  Port of fns.c use-short-answers.
    logical, public :: use_short_answers = .false.

    !> String to append when `read_yes_or_no' asks a question.
    !> For best results this should end in a space.  Port of fns.c
    !> yes-or-no-prompt.
    character(len=:), allocatable, public :: yes_or_no_prompt

    interface
        !> Ask user a yes-or-no question.
        !> Return true if answer is yes, and false if the answer is no.
        !>
        !> PROMPT is the string to display to ask the question; `read_yes_or_no'
        !> appends `yes_or_no_prompt' (default "(yes or no) ") to it.  If
        !> PROMPT is a non-empty string, and it ends with a non-space character,
        !> a space character will be appended to it.
        !>
        !> The user must confirm the answer with RET, and can edit it until it
        !> has been confirmed.
        !>
        !> If the `use_short_answers' variable is true, instead of asking for
        !> "yes" or "no", this function will ask for "y" or "n" (and
        !> ignore the value of `yes_or_no_prompt').  Port of
        !> fns.c:Fyes_or_no_p (no dialog boxes; fns.c:do_yes_or_no_p, which
        !> calls it).  (In submodule utility_functions_ask_mod,
        !> as the minibuffer depends on this module.)
        recursive module subroutine read_yes_or_no(prompt, answer, status)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine read_yes_or_no

        !> Base64-encode the region between BEGINNING and END.
        !> The data in the region is assumed to represent bytes, not text.  If
        !> you want to base64-encode text, the text has to be converted into data
        !> first by using `encode_coding_region' with the appropriate coding
        !> system first.
        !>
        !> LENGTH is the length of the encoded data.
        !>
        !> Optional third argument NO_LINE_BREAK means do not break long lines
        !> into shorter lines.  Port of fns.c:Fbase64_encode_region.  (In
        !> submodule utility_functions_buffer_mod, as the insertion depends on this module.)
        recursive module subroutine base64_encode_region(beginning, end, length, status, no_line_break)
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            integer(int64), intent(out) :: length
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: no_line_break
        end subroutine base64_encode_region

        !> Base64url-encode the region between BEGINNING and END.
        !> LENGTH is the length of the encoded text.
        !> Optional argument NO_PAD means do not add padding char =.
        !>
        !> This produces the URL variant of base 64 encoding defined in RFC 4648.
        !> Port of fns.c:Fbase64url_encode_region.  (In submodule
        !> utility_functions_buffer_mod.)
        recursive module subroutine base64url_encode_region(beginning, end, length, status, no_pad)
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            integer(int64), intent(out) :: length
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: no_pad
        end subroutine base64url_encode_region

        !> Base64-decode the region between BEGINNING and END.
        !> LENGTH is the length of the decoded data.
        !>
        !> Note that after calling this function, the data in the region will
        !> represent bytes, not text.  If you want to end up with text, you have
        !> to call `decode_coding_region' afterwards with an appropriate coding
        !> system.
        !>
        !> If the region can't be decoded, an error condition occurs and the
        !> buffer is not modified.
        !> Optional argument BASE64URL determines whether to use the URL variant
        !> of the base 64 encoding, as defined in RFC 4648.
        !> If optional argument IGNORE_INVALID is true invalid characters
        !> are ignored instead of an error condition occurring.  Port of
        !> fns.c:Fbase64_decode_region.  (In submodule utility_functions_buffer_mod.)
        recursive module subroutine base64_decode_region(beginning, end, length, status, base64url, ignore_invalid)
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            integer(int64), intent(out) :: length
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: base64url, ignore_invalid
        end subroutine base64_decode_region

        !> Register the base64 region commands.  Port of fns.c:syms_of_fns
        !> (the commands).  (In submodule utility_functions_buffer_mod.)
        module subroutine initialize_functions()
            implicit none(type, external)
        end subroutine initialize_functions

        !> Return a list of all the supported `secure_hash' algorithms.
        !> Port of fns.c:Fsecure_hash_algorithms.  (In submodule
        !> utility_functions_hash_mod.)
        pure module function secure_hash_algorithms() result(algorithms)
            implicit none(type, external)
            integer :: algorithms(10)
        end function secure_hash_algorithms

        !> Return a hash of the contents of BUFFER.
        !> This hash is performed on the raw internal format of the buffer,
        !> disregarding any coding systems.  If absent, use the current buffer.
        !>
        !> This function is useful for comparing two buffers running in the same
        !> Emacs, but is not guaranteed to return the same hash between different
        !> Emacs versions.  It should be somewhat more efficient on larger
        !> buffers than `secure_hash' is, and should not allocate more memory.
        !>
        !> It should not be used for anything security-related.  See
        !> `secure_hash' for these applications.
        !>
        !> `call buffer_hash(hash, status, buffer)'; a closed BUFFER is an
        !> error ("Invalid buffer argument").  Port of fns.c:Fbuffer_hash (a
        !> buffer name is looked up with `get_buffer' by the caller).  (In
        !> submodule utility_functions_hash_mod.)
        module subroutine buffer_hash(hash, status, buffer)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: hash
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: buffer
        end subroutine buffer_hash

        !> The secure hash, by ALGORITHM (as in `secure_hash'), of LENGTH random
        !> bytes from the system's entropy pool (getrandom), in hexadecimal
        !> or, if BINARY is true, as the bytes of the digest.  A negative
        !> LENGTH is an error ("Without a length, `hash_random_bytes' can't be
        !> used; see Info node `(elisp)Format of GnuTLS Cryptography Inputs'");
        !> a failure to get the bytes is a `file_error' error condition
        !> ("Getting random data").
        !>
        !> `call hash_random_bytes(algorithm, length, hash, status, binary)'.
        !> Port of fns.c:Fsecure_hash with the `iv-auto' OBJECT (upstream's
        !> START is LENGTH; fns.c:extract_data_from_object).  (In submodule
        !> utility_functions_hash_mod.)
        module subroutine hash_random_bytes(algorithm, length, hash, status, binary)
            implicit none(type, external)
            integer, intent(in) :: algorithm
            integer(int64), intent(in) :: length
            character(len=:), allocatable, intent(out) :: hash
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: binary
        end subroutine hash_random_bytes
    end interface

contains

    !> Structural equality.  Markers are equal when they point at the same
    !> place in the same buffer (or both nowhere); keymaps when their
    !> bindings, prompts and parents are.  Port of fns.c:Fequal
    !> (internal_equal, without text properties or floats).
    recursive logical function value_equal(a, b) result(eq)
        type(value_type), intent(in) :: a, b
        integer(int64) :: i, j

        eq = .false.
        if (vtag(a) /= vtag(b)) return

        if (vtag(a) == V_NULL) then
            eq = .true.
            return
        end if

        ! Same preorder atoms <=> same tree; a string's text properties
        ! (its children) don't count.
        i = 1
        j = 1

        do while (i <= size(a%a, kind=int64) .and. j <= size(b%a, kind=int64))
            if (.not. atom_equal(a%a(i), b%a(j))) return

            if (a%a(i)%tag == V_STRING) then
                i = i + a%a(i)%span
                j = j + b%a(j)%span
            else
                i = i + 1
                j = j + 1
            end if
        end do

        eq = i > size(a%a, kind=int64) .and. j > size(b%a, kind=int64)
    end function value_equal

    ! Port of part of fns.c:internal_equal_1 (atoms).
    recursive logical function atom_equal(x, y) result(eq)
        type(atom_type), intent(in) :: x, y
        integer(int64) :: ix, iy
        type(status_type) :: sx, sy

        eq = .false.
        if (x%tag /= y%tag) return

        select case (x%tag)
        case (V_NULL)
            eq = .true.
        case (V_INTEGER, V_NAME, V_ARRAY)
            eq = x%i == y%i
        case (V_STRING)
            eq = len(x%s) == len(y%s)
            if (eq) eq = x%s == y%s
        case (V_BUFFER, V_SYNTAX_TABLE, V_WINDOW)
            eq = x%h%index == y%h%index .and. x%h%generation == y%h%generation
        case (V_KEYMAP)
            ! A keymap is a list upstream: compare its contents.
            eq = keymap_equal(x%h, y%h, value_equal)
        case (V_MARKER)
            call resolve_marker(x%h, ix, sx)
            call resolve_marker(y%h, iy, sy)

            if (.not. (sx%ok() .and. sy%ok())) then
                eq = x%h%index == y%h%index .and. x%h%generation == y%h%generation
            else
                eq = markers(ix)%buffer == markers(iy)%buffer .and. &
                     (markers(ix)%buffer == 0 .or. markers(ix)%character_position == markers(iy)%character_position)
            end if
        case default
            ! Nothing to do for other values.
        end select
    end function atom_equal

    !> Text of a string, or the name of a symbol; `ok' false otherwise.
    !> Not a port.
    subroutine string_or_name(v, text, ok)
        type(value_type), intent(in) :: v
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: ok

        ok = .true.

        select case (vtag(v))
        case (V_STRING)
            text = v%a(1)%s
        case (V_NAME, V_NULL)
            call ensure_obarray()
            text = symbols(xsymbol(v))%name
        case default
            ok = .false.
            text = ""
        end select
    end subroutine string_or_name

    !> True if `a' sorts before `b', comparing characters by code.
    !> Symbols stand for their names; anything else compares false (the
    !> test interface has no status).  Port of fns.c:Fstring_lessp
    !> (string_cmp).
    logical function is_string_less(a, b)
        type(value_type), intent(in) :: a, b
        character(len=:), allocatable :: sa, sb
        logical :: oka, okb
        integer(int64) :: pa, pb
        integer(int32) :: ca, cb, na, nb

        is_string_less = .false.
        call string_or_name(a, sa, oka)
        call string_or_name(b, sb, okb)
        if (.not. (oka .and. okb)) return
        pa = 1
        pb = 1

        do while (pa <= len(sa, kind=int64) .and. pb <= len(sb, kind=int64))
            call string_character_and_length(sa, pa, ca, na)
            call string_character_and_length(sb, pb, cb, nb)

            if (ca /= cb) then
                is_string_less = ca < cb
                return
            end if

            pa = pa + na
            pb = pb + nb
        end do

        ! Equal up to the shorter length: the shorter one is less.
        is_string_less = pa > len(sa, kind=int64) .and. pb <= len(sb, kind=int64)
    end function is_string_less

    !> True if string `a' sorts before `b', comparing characters by code: the
    !> bytes in order (UTF-8 keeps the order of the codes), a prefix first.
    !> Port of fns.c:Fstring_lessp (string_cmp).
    pure logical function is_string_less_text(a, b) result(less)
        character(len=*), intent(in) :: a, b
        integer :: n

        n = min(len(a), len(b))

        if (a(1:n) == b(1:n)) then
            less = len(a) < len(b)
        else
            less = a(1:n) < b(1:n)
        end if
    end function is_string_less_text

    !> Compare the contents of two strings.
    !> The arguments START1, END1, START2, and END2, if present, are
    !> positions specifying which parts of STR1 or STR2 to compare.  In
    !> string STR1, compare the part between START1 (inclusive) and END1
    !> (exclusive).  If START1 is absent, it defaults to 0, the beginning
    !> of the string; if END1 is absent, it defaults to the length of the
    !> string.  Likewise, in string STR2, compare the part between START2
    !> and END2.  Like in `substring', negative values are counted from the
    !> end.
    !>
    !> The strings are compared by the numeric values of their characters.
    !> For instance, STR1 is "less than" STR2 if its first differing
    !> character has a smaller numeric value.  If IGNORE-CASE is true,
    !> characters are converted to upper-case before comparing them.
    !>
    !> The value is COMPARE_EQUAL (t) if the strings (or specified
    !> portions) match.  If string STR1 is less, the value is a negative
    !> number N; - 1 - N is the number of characters that match at the
    !> beginning.  If string STR1 is greater, the value is a positive
    !> number N; N - 1 is the number of characters that match at the
    !> beginning.
    !>
    !> Positions out of range signal args-out-of-range in STATUS (with
    !> STATUS absent the caller guarantees them; the value is then
    !> COMPARE_EQUAL).  Port of fns.c:Fcompare_strings.
    integer(int64) function compare_strings(str1, str2, ignore_case, start1, end1, start2, end2, status) result(r)
        character(len=*), intent(in) :: str1, str2
        logical, intent(in), optional :: ignore_case
        integer(int64), intent(in), optional :: start1, end1, start2, end2
        type(status_type), intent(out), optional :: status
        integer(int32), allocatable :: c1(:), c2(:)
        integer(int64) :: from1, to1, from2, to2, i1, i2, e1, e2
        integer(int32) :: a, b
        logical :: fold, ok
        type(status_type) :: st

        r = COMPARE_EQUAL
        call string_characters(str1, c1)
        call string_characters(str2, c2)
        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        ! For backward compatibility, silently bring too-large positive end
        ! values into range.
        e1 = size(c1, kind=int64)
        if (present(end1)) e1 = min(end1, e1)
        e2 = size(c2, kind=int64)
        if (present(end2)) e2 = min(end2, e2)
        call validate_subarray(str1, size(c1, kind=int64), from1, to1, ok, st, start1, e1, present(end1))
        if (ok) call validate_subarray(str2, size(c2, kind=int64), from2, to2, ok, st, start2, e2, present(end2))
        if (present(status)) status = st
        if (.not. ok) return
        i1 = from1
        i2 = from2

        do while (i1 < to1 .and. i2 < to2)
            a = c1(i1 + 1)
            b = c2(i2 + 1)
            i1 = i1 + 1
            i2 = i2 + 1
            if (a == b) cycle

            if (fold) then
                a = upcase(a)
                b = upcase(b)
            end if

            if (a == b) cycle

            ! I1 has already been incremented past the character compared.
            if (a < b) then
                r = -(i1 - from1)
            else
                r = i1 - from1
            end if

            return
        end do

        if (i1 < to1) then
            r = i1 - from1 + 1
        else if (i2 < to2) then
            r = -(i1 - from1 + 1)
        else
            r = COMPARE_EQUAL
        end if
    end function compare_strings

    !> Check the range FROM..TO of the N characters of STRING
    !> (negative values count from the end; absent FROM is 0, TO absent
    !> or not `to_given' is N) and give it as IFROM..ITO, or signal
    !> args-out-of-range (OK false).  Port of fns.c:validate_subarray.
    pure subroutine validate_subarray(string, n, ifrom, ito, ok, status, from, to, to_given)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: n
        integer(int64), intent(out) :: ifrom, ito
        logical, intent(out) :: ok
        type(status_type), intent(inout) :: status
        integer(int64), intent(in), optional :: from, to
        logical, intent(in), optional :: to_given
        integer(int64) :: f, t
        logical :: has_to
        character(len=20) :: from_text, to_text

        f = 0
        from_text = ".false."

        if (present(from)) then
            f = from
            write (from_text, "(i0)") from
            if (f < 0) f = f + n
        end if

        has_to = present(to)
        if (present(to_given)) has_to = has_to .and. to_given
        t = n
        to_text = ".false."

        if (has_to) then
            t = to
            write (to_text, "(i0)") to
            if (t < 0) t = t + n
        end if

        ok = 0 <= f .and. f <= t .and. t <= n
        ifrom = f
        ito = t

        if (.not. ok) then
            call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, fortran_string(string)//", "//trim(from_text)//", "//trim(to_text))
        end if
    end subroutine validate_subarray

    !> Number of characters in UTF-8 string `s'.  Port of lisp.h:SCHARS.
    pure integer(int64) function count_characters(s)
        character(len=*), intent(in) :: s
        integer :: i

        count_characters = 0

        do i = 1, len(s)
            ! Count the bytes that start a character.
            if (iand(iachar(s(i:i)), 192) /= 128) count_characters = count_characters + 1
        end do
    end function count_characters

    !> Return a new string whose contents are a substring of STRING.
    !> The returned string consists of the characters between index FROM
    !> (inclusive) and index TO (exclusive) of STRING.  FROM and TO are
    !> zero-indexed: 0 means the first character of STRING.  Negative
    !> values are counted from the end of STRING.  If TO is absent, the
    !> substring runs to the end of STRING.
    !>
    !> A range outside STRING signals args-out-of-range in STATUS (with
    !> STATUS absent the caller guarantees the range; the value is then
    !> empty).  Port of fns.c:Fsubstring.
    function substring(string, from, to, status) result(r)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: from
        integer(int64), intent(in), optional :: to
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: r
        integer(int64) :: a, b, count
        integer :: i, first, last
        logical :: ok
        type(status_type) :: st

        r = ""
        call validate_subarray(string, count_characters(string), a, b, ok, st, from, to)
        if (present(status)) status = st
        if (.not. ok) return
        ! Byte positions of characters a and b.
        first = len(string) + 1
        last = len(string) + 1
        count = 0

        do i = 1, len(string)
            if (iand(iachar(string(i:i)), 192) /= 128) then
                if (count == a) first = min(first, i)

                if (count == b) then
                    last = i
                    exit
                end if

                count = count + 1
            end if
        end do

        r = string(first:last - 1)
    end function substring

    !> The characters of UTF-8 string `s'.  Not a port.
    pure subroutine string_characters(s, chars)
        character(len=*), intent(in) :: s
        integer(int32), allocatable, intent(out) :: chars(:)
        integer(int32), allocatable :: temporary(:)
        integer(int64) :: i
        integer(int32) :: c, clen
        integer :: n

        allocate (temporary(len(s)))
        n = 0
        i = 1

        do while (i <= len(s, kind=int64))
            call string_character_and_length(s, i, c, clen)
            n = n + 1
            temporary(n) = c
            i = i + clen
        end do

        chars = temporary(1:n)
    end subroutine string_characters

    !> Port of fns.c:Fstring_greaterp.
    logical function is_string_greater(a, b)
        type(value_type), intent(in) :: a, b

        is_string_greater = is_string_less(b, a)
    end function is_string_greater

    !> Compare with `test', or `eq' by default.  Not a port.
    logical function matches(a, b, test)
        type(value_type), intent(in) :: a, b
        procedure(value_test_procedure), optional :: test

        if (present(test)) then
            matches = test(a, b)
        else
            matches = value_eq(a, b)
        end if
    end function matches

    !> Value of `property' in property list `plist', or nil.  Degenerate
    !> plists (odd length) are tolerated.  Port of fns.c:Fplist_get.
    function plist_get(plist, property, test) result(v)
        type(value_type), intent(in) :: plist, property
        procedure(value_test_procedure), optional :: test
        type(value_type) :: v
        type(value_type), allocatable :: elements(:)
        integer(int64) :: k

        call list_elements(plist, elements)

        do k = 1, size(elements, kind=int64) - 1, 2
            if (matches(elements(k), property, test)) then
                v = elements(k + 1)
                return
            end if
        end do
    end function plist_get

    !> Set `property' to `value' in `plist', adding the pair at the end if absent.
    !> An odd-length plist is a wrong-type-argument (plistp), as upstream.
    !> Port of fns.c:Fplist_put.
    subroutine plist_put(plist, property, value, status, test)
        type(value_type), intent(inout) :: plist
        type(value_type), intent(in) :: property, value
        type(status_type), intent(out) :: status
        procedure(value_test_procedure), optional :: test
        type(value_type), allocatable :: elements(:)
        integer(int64) :: k, n

        if ((vtag(plist) /= V_ARRAY .and. vtag(plist) /= V_NULL)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected array")
            return
        end if

        call list_elements(plist, elements)
        n = size(elements, kind=int64)

        do k = 1, n - 1, 2
            if (matches(elements(k), property, test)) then
                elements(k + 1) = value
                plist = make_list(elements)
                return
            end if
        end do

        if (mod(n, 2_int64) /= 0) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected array")
            return
        end if

        elements = [elements, property, value]
        plist = make_list(elements)
    end subroutine plist_put

    !> The tail of `plist' starting at `property', or nil.
    !> Port of fns.c:Fplist_member.
    function plist_member(plist, property, test) result(tail)
        type(value_type), intent(in) :: plist, property
        procedure(value_test_procedure), optional :: test
        type(value_type) :: tail
        type(value_type), allocatable :: elements(:)
        integer(int64) :: k

        call list_elements(plist, elements)

        do k = 1, size(elements, kind=int64), 2
            if (matches(elements(k), property, test)) then
                tail = list_tail(plist, k - 1)
                return
            end if
        end do
    end function plist_member

    !> Property `property' of `symbol'.  Port of fns.c:Fget (without
    !> `overriding_plist_environment').
    subroutine get(symbol, property, v, status)
        type(value_type), intent(in) :: symbol, property
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        v = plist_get(symbols(xsymbol(symbol))%plist, property)
    end subroutine get

    !> Set property `property' of `symbol' to `value'.  Port of fns.c:Fput.
    subroutine put(symbol, property, value, status)
        type(value_type), intent(in) :: symbol, property, value
        type(status_type), intent(out) :: status

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        call plist_put(symbols(xsymbol(symbol))%plist, property, value, status)
    end subroutine put

    ! ------------------------------------------------------------------
    ! Strings
    ! ------------------------------------------------------------------

    !> The byte offset (counting from 0) in the multibyte text STRING of
    !> its character CHARACTER_INDEX (counting from 0); the length of STRING
    !> for CHARACTER_INDEX at or past its end.  Port of
    !> fns.c:string_char_to_byte (without the cache of the last string).
    pure integer(int64) function string_character_to_byte(string, character_index) result(byte_index)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: character_index
        integer(int64) :: n

        byte_index = 0
        n = 0

        do while (n < character_index .and. byte_index < len(string, kind=int64))
            byte_index = byte_index + bytes_by_character_head(ichar(string(byte_index + 1:byte_index + 1), kind=int32))
            n = n + 1
        end do

        byte_index = min(byte_index, len(string, kind=int64))
    end function string_character_to_byte

    !> The character index (counting from 0) in the multibyte text STRING
    !> of the character at byte offset BYTE_INDEX (counting from 0).
    !> Port of fns.c:string_byte_to_char (without the cache of the last string).
    pure integer(int64) function string_byte_to_character(string, byte_index) result(character_index)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: byte_index

        character_index = count_characters(string(1:min(byte_index, len(string, kind=int64))))
    end function string_byte_to_character

    !> Search for the string NEEDLE in the string HAYSTACK.
    !> The return value is the position of the first occurrence of NEEDLE in
    !> HAYSTACK, or -1 if no match was found.
    !>
    !> The optional START_POSITION argument says where to start searching in
    !> HAYSTACK and defaults to zero (start at the beginning).
    !> It must be between zero and the length of HAYSTACK, inclusive.
    !>
    !> Case is always significant and text properties are ignored.
    !>
    !> A START_POSITION out of range is an `arguments_out_of_range' error condition
    !> in STATUS (with STATUS absent the caller guarantees it; the value is
    !> then -1).  Port of fns.c:Fstring_search (both strings multibyte, as
    !> character strings always are here).
    integer(int64) function string_search(needle, haystack, start_position, status) result(position)
        character(len=*), intent(in) :: needle, haystack
        integer(int64), intent(in), optional :: start_position
        type(status_type), intent(out), optional :: status
        integer(int64) :: start, start_byte
        integer :: k
        character(len=20) :: digits

        position = -1
        start = 0
        start_byte = 0

        if (present(start_position)) then
            start = start_position

            if (start < 0 .or. start > count_characters(haystack)) then
                write (digits, "(i0)") start
                if (present(status)) call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, trim(digits))
                return
            end if

            start_byte = string_character_to_byte(haystack, start)
        end if

        ! If NEEDLE is longer than (the remaining length of) haystack, then
        ! we can't have a match, and return early.
        if (count_characters(needle) > count_characters(haystack) - start) return
        ! The UTF-8 forms of the characters match only at character
        ! boundaries, so a byte search finds the characters.
        k = index(haystack(start_byte + 1:), needle)
        if (k == 0) return
        position = string_byte_to_character(haystack, start_byte + k - 1)
    end function string_search

    !> Return Levenshtein distance between STRING1 and STRING2.
    !> The distance is the number of deletions, insertions, and substitutions
    !> required to transform STRING1 into STRING2.
    !> If BYTECOMPARE is false or absent, compute distance in terms of characters.
    !> If BYTECOMPARE is true, compute distance in terms of bytes.
    !> Letter-case is significant, but text properties are ignored.
    !> Port of fns.c:Fstring_distance.
    pure integer(int64) function string_distance(string1, string2, bytecompare) result(distance)
        character(len=*), intent(in) :: string1, string2
        logical, intent(in), optional :: bytecompare
        integer(int32), allocatable :: c1(:), c2(:)
        integer(int64), allocatable :: column(:)
        integer(int64) :: x, y, lastdiag, olddiag, len1, len2
        logical :: use_byte_compare

        use_byte_compare = .false.
        if (present(bytecompare)) use_byte_compare = bytecompare

        if (use_byte_compare) then
            c1 = [(ichar(string1(x:x), kind=int32), x=1, len(string1, kind=int64))]
            c2 = [(ichar(string2(x:x), kind=int32), x=1, len(string2, kind=int64))]
        else
            call string_characters(string1, c1)
            call string_characters(string2, c2)
        end if

        len1 = size(c1, kind=int64)
        len2 = size(c2, kind=int64)
        column = [(y, y=0, len1)]

        do x = 1, len2
            column(1) = x
            lastdiag = x - 1

            do y = 1, len1
                olddiag = column(y + 1)
                column(y + 1) = min(column(y + 1) + 1, column(y) + 1, lastdiag + merge(0, 1, c1(y) == c2(x)))
                lastdiag = olddiag
            end do
        end do

        distance = column(len1 + 1)
    end function string_distance

    !> Return true if two strings have identical contents.
    !> Case is significant, but text properties are ignored.
    !> Names are also allowed; their text is used instead.  Anything else
    !> compares false (the test interface has no status).
    !>
    !> See also `string_equal_ignore_case'.  Port of fns.c:Fstring_equal.
    logical function is_string_equal(s1, s2)
        type(value_type), intent(in) :: s1, s2
        character(len=:), allocatable :: text1, text2
        logical :: ok1, ok2

        is_string_equal = .false.
        call string_or_name(s1, text1, ok1)
        call string_or_name(s2, text2, ok2)
        if (.not. (ok1 .and. ok2)) return
        if (len(text1) /= len(text2)) return
        is_string_equal = text1 == text2
    end function is_string_equal

    !> Return a substring of STRING, without text properties.
    !> It starts at index FROM and ends before TO.
    !> TO may be absent; then the substring runs to the end of STRING.
    !> If FROM is absent, the substring starts at the beginning of STRING.
    !> If FROM or TO is negative, it counts from the end.
    !>
    !> With one argument, just copy STRING without its properties.
    !>
    !> A STRING that is not a string is a `wrong_type_argument' error
    !> condition, and a range outside it `arguments_out_of_range', in STATUS
    !> (with STATUS absent the caller guarantees them; the value is then
    !> empty).  Port of fns.c:Fsubstring_no_properties.
    function substring_no_properties(string, from, to, status) result(text)
        type(value_type), intent(in) :: string
        integer(int64), intent(in), optional :: from, to
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: text
        integer(int64) :: from_character, to_character, from_byte, to_byte
        logical :: ok
        type(status_type) :: st

        text = ""

        if (vtag(string) /= V_STRING) then
            if (present(status)) call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected character(len=*)")
            return
        end if

        associate (s => string%a(1)%s)
            call validate_subarray(s, count_characters(s), from_character, to_character, ok, st, from, to)
            if (present(status)) status = st
            if (.not. ok) return
            from_byte = string_character_to_byte(s, from_character)
            to_byte = string_character_to_byte(s, to_character)
            text = s(from_byte + 1:to_byte)
        end associate
    end function substring_no_properties

    !> Clear the contents of STRING.
    !> This clears its contents to null characters, and removes all text
    !> properties (STRING becomes the text of so many null characters as it
    !> had bytes, as upstream's unibyte STRING).  A STRING that is not a
    !> string is a `wrong_type_argument' error condition.  Port of
    !> fns.c:Fclear_string.
    subroutine clear_string(string, status)
        type(value_type), intent(inout) :: string
        type(status_type), intent(out) :: status
        integer(int64) :: none(0)
        type(value_type) :: no_plists(0)

        if (vtag(string) /= V_STRING) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected character(len=*)")
            return
        end if

        call set_string_intervals(string, none, none, no_plists)
        string%a(1)%s = repeat(achar(0), len(string%a(1)%s))
    end subroutine clear_string

    !> Return true if first arg string is less than second in collation order.
    !>
    !> This function obeys the conventions for collation order in your
    !> locale settings.  For example, punctuation and whitespace characters
    !> might be considered less significant for sorting.
    !>
    !> The optional argument LOCALE, a string, overrides the setting of your
    !> current locale identifier for collation.
    !>
    !> If IGNORE_CASE is true, characters are converted to lower-case
    !> before comparing them.
    !>
    !> The locale here is the C locale ("C", "POSIX", or "" and absent for
    !> the current one), whose collation order is that of the character
    !> codes; another LOCALE is an error, as for a locale that is not
    !> installed (in STATUS, if present).  Port of
    !> fns.c:Fstring_collate_lessp.
    logical function is_string_collate_less(s1, s2, locale, ignore_case, status)
        character(len=*), intent(in) :: s1, s2
        character(len=*), intent(in), optional :: locale
        logical, intent(in), optional :: ignore_case
        type(status_type), intent(out), optional :: status
        type(status_type) :: st

        is_string_collate_less = collate_strings(s1, s2, st, locale, ignore_case) < 0
        if (present(status)) status = st
    end function is_string_collate_less

    !> Return true if two strings have identical contents.
    !>
    !> This function obeys the conventions for collation order in your locale
    !> settings.  For example, characters with different coding points but
    !> the same meaning might be considered as equal.
    !>
    !> The optional argument LOCALE, a string, overrides the setting of your
    !> current locale identifier for collation.
    !>
    !> If IGNORE_CASE is true, characters are converted to lower-case
    !> before comparing them.
    !>
    !> Do NOT use this function to compare file names for equality.
    !>
    !> The locale here is the C locale, as for `is_string_collate_less'.
    !> Port of fns.c:Fstring_collate_equalp.
    logical function is_string_collate_equal(s1, s2, locale, ignore_case, status)
        character(len=*), intent(in) :: s1, s2
        character(len=*), intent(in), optional :: locale
        logical, intent(in), optional :: ignore_case
        type(status_type), intent(out), optional :: status
        type(status_type) :: st

        is_string_collate_equal = collate_strings(s1, s2, st, locale, ignore_case) == 0
        if (present(status)) status = st
    end function is_string_collate_equal

    !> Compare S1 and S2 in the collation order of LOCALE: negative, zero
    !> or positive.  The C locale (the only one here) orders by character
    !> code, and lowers only A to Z for IGNORE_CASE (towlower in the C
    !> locale).  Port of sysdep.c:str_collate (wcscoll_l in the C locale).
    integer function collate_strings(s1, s2, status, locale, ignore_case) result(r)
        character(len=*), intent(in) :: s1, s2
        type(status_type), intent(inout) :: status
        character(len=*), intent(in), optional :: locale
        logical, intent(in), optional :: ignore_case
        integer(int32), allocatable :: c1(:), c2(:)
        integer(int64) :: k
        logical :: fold

        r = 0

        if (present(locale)) then
            if (len(locale) > 0 .and. locale /= "C" .and. locale /= "POSIX") then
                call set_error(status, GENERIC_ERROR_CODE, "Invalid locale "//locale//": No such file or directory")
                return
            end if
        end if

        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        call string_characters(s1, c1)
        call string_characters(s2, c2)

        if (fold) then
            where (c1 >= iachar("A") .and. c1 <= iachar("Z")) c1 = c1 + 32
            where (c2 >= iachar("A") .and. c2 <= iachar("Z")) c2 = c2 + 32
        end if

        do k = 1, min(size(c1, kind=int64), size(c2, kind=int64))
            if (c1(k) /= c2(k)) then
                r = merge(-1, 1, c1(k) < c2(k))
                return
            end if
        end do

        r = merge(-1, merge(1, 0, size(c1) > size(c2)), size(c1) < size(c2))
    end function collate_strings

    !> Return true if two values have similar structure and contents.
    !> This is like `value_equal' except that it compares the text
    !> properties of strings.  (`value_equal' ignores text properties.)
    !> Port of fns.c:Fequal_including_properties (internal_equal with
    !> EQUAL_INCLUDING_PROPERTIES).
    recursive logical function value_equal_including_properties(o1, o2) result(eq)
        type(value_type), intent(in) :: o1, o2
        type(value_type), allocatable :: e1(:), e2(:)
        integer(int64) :: k

        eq = value_equal(o1, o2)
        if (.not. eq) return

        select case (vtag(o1))
        case (V_STRING)
            eq = compare_string_intervals(o1, o2)
        case (V_ARRAY)
            call list_elements(o1, e1)
            call list_elements(o2, e2)

            do k = 1, size(e1, kind=int64)
                eq = value_equal_including_properties(e1(k), e2(k))
                if (.not. eq) return
            end do
        case default
            ! The structural comparison is all.
        end select
    end function value_equal_including_properties

    !> Whether the strings S1 and S2 (of the same length) have the same
    !> text properties at each character, the values compared with
    !> `value_equal'.  Port of intervals.c:compare_string_intervals (with
    !> intervals.c:intervals_equal_1, USE_EQUAL).
    logical function compare_string_intervals(s1, s2) result(same)
        type(value_type), intent(in) :: s1, s2
        integer(int64), allocatable :: starts1(:), ends1(:), starts2(:), ends2(:), bounds(:)
        type(value_type), allocatable :: plists1(:), plists2(:)
        integer(int64) :: k

        call string_intervals(s1, starts1, ends1, plists1)
        call string_intervals(s2, starts2, ends2, plists2)
        ! The properties are constant between these positions.
        bounds = [0_int64, starts1, ends1, starts2, ends2]
        same = .true.

        do k = 1, size(bounds, kind=int64)
            same = plists_equal(plist_at(bounds(k), starts1, ends1, plists1), &
                                plist_at(bounds(k), starts2, ends2, plists2))
            if (.not. same) return
        end do
    end function compare_string_intervals

    !> The property list of the interval covering character POSITION
    !> (an unallocated value if none does).  Not a port.
    function plist_at(position, starts, ends, plists) result(plist)
        integer(int64), intent(in) :: position, starts(:), ends(:)
        type(value_type), intent(in) :: plists(:)
        type(value_type) :: plist
        integer :: k

        do k = 1, size(starts)
            if (starts(k) <= position .and. position < ends(k)) then
                plist = plists(k)
                return
            end if
        end do
    end function plist_at

    !> Whether the property lists P0 and P1 have the same properties
    !> (compared with `value_eq') with values that are `value_equal', in
    !> any order.  Port of intervals.c:intervals_equal_1 (USE_EQUAL).
    logical function plists_equal(p0, p1) result(same)
        type(value_type), intent(in) :: p0, p1
        type(value_type), allocatable :: e0(:), e1(:)
        integer(int64) :: i, j
        logical :: found

        call list_elements(p0, e0)
        call list_elements(p1, e1)
        same = size(e0) == size(e1)
        if (.not. same) return

        do i = 1, size(e0, kind=int64) - 1, 2
            found = .false.

            do j = 1, size(e1, kind=int64) - 1, 2
                if (value_eq(e0(i), e1(j))) then
                    found = value_equal(e0(i + 1), e1(j + 1))
                    exit
                end if
            end do

            same = found
            if (.not. same) return
        end do
    end function plists_equal

    ! ------------------------------------------------------------------
    ! Base64
    ! ------------------------------------------------------------------

    !> Base64-encode STRING and return the result.
    !> Optional second argument NO_LINE_BREAK means do not break long lines
    !> into shorter lines.
    !>
    !> STRING is multibyte text (its raw bytes encode as those bytes)
    !> unless MULTIBYTE is false: then its bytes are the data.  Another
    !> character than ASCII and raw bytes is an error ("Multibyte
    !> character in data for base64 encoding") in STATUS (with STATUS absent
    !> the caller guarantees there is none; the value is then empty).
    !> Port of fns.c:Fbase64_encode_string (base64_encode_string_1).
    function base64_encode_string(string, no_line_break, multibyte, status) result(encoded)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: no_line_break, multibyte
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: encoded
        logical :: line_break

        line_break = .true.
        if (present(no_line_break)) line_break = .not. no_line_break
        encoded = base64_encode_string_1(string, line_break, .true., .false., multibyte, status)
    end function base64_encode_string

    !> Base64url-encode STRING and return the result.
    !> Optional second argument NO_PAD means do not add padding char =.
    !>
    !> This produces the URL variant of base 64 encoding defined in RFC 4648.
    !>
    !> MULTIBYTE and STATUS are as for `base64_encode_string'.  Port of
    !> fns.c:Fbase64url_encode_string.
    function base64url_encode_string(string, no_pad, multibyte, status) result(encoded)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: no_pad, multibyte
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: encoded
        logical :: pad

        pad = .true.
        if (present(no_pad)) pad = .not. no_pad
        encoded = base64_encode_string_1(string, .false., pad, .true., multibyte, status)
    end function base64url_encode_string

    !> The base64 encoding of STRING (multibyte unless MULTIBYTE is false)
    !> as LINE_BREAK, PAD and BASE64URL ask.  Port of
    !> fns.c:base64_encode_string_1.
    function base64_encode_string_1(string, line_break, pad, base64url, multibyte, status) result(encoded)
        character(len=*), intent(in) :: string
        logical, intent(in) :: line_break, pad, base64url
        logical, intent(in), optional :: multibyte
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: encoded
        logical :: is_multibyte, ok

        is_multibyte = .true.
        if (present(multibyte)) is_multibyte = multibyte
        call base64_encode_1(string, line_break, pad, base64url, is_multibyte, encoded, ok)

        if (.not. ok) then
            ! The encoding wasn't possible.
            encoded = ""
            if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Multibyte character in data for base64 encoding")
        end if
    end function base64_encode_string_1

    !> Base64-encode the bytes of FROM (the characters of multibyte text if
    !> MULTIBYTE, each ASCII or a raw byte) into ENCODED; OK false if a
    !> character is neither.  Lines are broken every 76 characters if
    !> LINE_BREAK; PAD adds the padding; BASE64URL takes the URL alphabet.
    !> Port of fns.c:base64_encode_1.
    subroutine base64_encode_1(from, line_break, pad, base64url, multibyte, encoded, ok)
        character(len=*), intent(in) :: from
        logical, intent(in) :: line_break, pad, base64url, multibyte
        character(len=:), allocatable, intent(out) :: encoded
        logical, intent(out) :: ok
        character(len=:), allocatable :: e
        character(len=64) :: alphabet
        integer(int64) :: i, length, n, allength
        integer(int32) :: c, value
        integer :: counter

        alphabet = merge(BASE64URL_CHARACTERS, BASE64_CHARACTERS, base64url)
        length = len(from, kind=int64)
        ! We need to allocate enough room for encoding the text.
        ! We need 33 1/3% more space, plus a newline every 76
        ! characters, and then we round up.
        allength = length + length/3 + 1
        allength = allength + allength/MIME_LINE_LENGTH + 1 + 6
        allocate (character(len=allength) :: e)
        encoded = ""
        ok = .true.
        counter = 0
        i = 1
        n = 0

        do while (i <= length)
            call next_byte(c)
            if (.not. ok) return

            ! Wrap line every 76 characters.
            if (line_break) then
                if (counter < MIME_LINE_LENGTH/4) then
                    counter = counter + 1
                else
                    call append_text(new_line("a"))
                    counter = 1
                end if
            end if

            ! Process first byte of a triplet.
            call append_text(alphabet(iand(ishft(c, -2), 63) + 1:iand(ishft(c, -2), 63) + 1))
            value = ishft(iand(c, 3), 4)

            ! Process second byte of a triplet.
            if (i > length) then
                call append_text(alphabet(value + 1:value + 1))
                if (pad) call append_text("==")
                exit
            end if

            call next_byte(c)
            if (.not. ok) return
            value = ior(value, iand(ishft(c, -4), 15))
            call append_text(alphabet(value + 1:value + 1))
            value = ishft(iand(c, 15), 2)

            ! Process third byte of a triplet.
            if (i > length) then
                call append_text(alphabet(value + 1:value + 1))
                if (pad) call append_text("=")
                exit
            end if

            call next_byte(c)
            if (.not. ok) return
            value = ior(value, iand(ishft(c, -6), 3))
            call append_text(alphabet(value + 1:value + 1))
            call append_text(alphabet(iand(c, 63) + 1:iand(c, 63) + 1))
        end do

        encoded = e(1:n)

    contains

        !> The next byte of the data, advancing I.
        subroutine next_byte(byte)
            integer(int32), intent(out) :: byte
            integer(int32) :: bytes

            if (multibyte) then
                call string_character_and_length(from, i, byte, bytes)

                if (is_character_byte8(byte)) then
                    byte = character_to_byte8(byte)
                else if (byte >= 128) then
                    ok = .false.
                end if

                i = i + bytes
            else
                byte = ichar(from(i:i), kind=int32)
                i = i + 1
            end if
        end subroutine next_byte

        !> Append TEXT to the encoding.
        subroutine append_text(text)
            character(len=*), intent(in) :: text

            e(n + 1:n + len(text)) = text
            n = n + len(text)
        end subroutine append_text
    end subroutine base64_encode_1

    !> Base64-decode STRING and return the result as a string.
    !> Optional argument BASE64URL determines whether to use the URL variant of
    !> the base 64 encoding, as defined in RFC 4648.
    !> If optional third argument IGNORE_INVALID is true invalid characters are
    !> ignored instead of an error condition occurring.
    !>
    !> The result is the decoded bytes (upstream's unibyte string).  Data
    !> that cannot be decoded is an error ("Invalid base64 data") in STATUS
    !> (with STATUS absent the caller guarantees it is valid; the value is
    !> then empty).  Port of fns.c:Fbase64_decode_string.
    function base64_decode_string(string, base64url, ignore_invalid, status) result(decoded)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: base64url, ignore_invalid
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: decoded
        integer(int64) :: decoded_characters
        logical :: url, ignore, ok

        url = .false.
        if (present(base64url)) url = base64url
        ignore = .false.
        if (present(ignore_invalid)) ignore = ignore_invalid
        ! The decoded result should be unibyte.
        call base64_decode_1(string, url, .false., ignore, decoded, decoded_characters, ok)

        if (.not. ok) then
            decoded = ""
            if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Invalid base64 data")
        end if
    end function base64_decode_string

    !> Base64-decode FROM into DECODED; OK false if it cannot be decoded.
    !> If MULTIBYTE, the decoded result is in multibyte form (bytes 128 to
    !> 255 as raw-byte characters).  If IGNORE_INVALID, ignore invalid
    !> base64 characters.  NCHARS is the number of characters produced.
    !> Port of fns.c:base64_decode_1.
    subroutine base64_decode_1(from, base64url, multibyte, ignore_invalid, decoded, nchars, ok)
        character(len=*), intent(in) :: from
        logical, intent(in) :: base64url, multibyte, ignore_invalid
        character(len=:), allocatable, intent(out) :: decoded
        integer(int64), intent(out) :: nchars
        logical, intent(out) :: ok
        character(len=:), allocatable :: e
        character :: c
        integer(int64) :: f, n
        integer(int32) :: v1, value

        allocate (character(len=merge(2, 1, multibyte)*len(from)) :: e)
        decoded = ""
        nchars = 0
        ok = .false.
        f = 1
        n = 0

        do
            ! Process first byte of a quadruplet.
            do
                if (f > len(from, kind=int64)) then
                    decoded = e(1:n)
                    ok = .true.
                    return
                end if

                call next_value(v1)
                if (.not. (v1 < 0 .or. (v1 == 0 .and. ignore_invalid))) exit
            end do

            if (v1 == 0) return
            value = ishft(v1 - 1, 18)

            ! Process second byte of a quadruplet.
            do
                if (f > len(from, kind=int64)) return
                call next_value(v1)
                if (.not. (v1 < 0 .or. (v1 == 0 .and. ignore_invalid))) exit
            end do

            if (v1 == 0) return
            value = value + ishft(v1 - 1, 12)
            call append_byte(iand(ishft(value, -16), 255))

            ! Process third byte of a quadruplet.
            do
                if (f > len(from, kind=int64)) then
                    if (.not. base64url .and. .not. ignore_invalid) return
                    decoded = e(1:n)
                    ok = .true.
                    return
                end if

                call next_value(v1)
                if (.not. (v1 < 0 .or. (v1 == 0 .and. ignore_invalid))) exit
            end do

            if (c == "=") then
                do
                    if (f > len(from, kind=int64)) return
                    call next_value(v1)
                    if (v1 >= 0) exit
                end do

                if (c /= "=") return
                cycle
            end if

            if (v1 == 0) return
            value = value + ishft(v1 - 1, 6)
            call append_byte(iand(ishft(value, -8), 255))

            ! Process fourth byte of a quadruplet.
            do
                if (f > len(from, kind=int64)) then
                    if (.not. base64url .and. .not. ignore_invalid) return
                    decoded = e(1:n)
                    ok = .true.
                    return
                end if

                call next_value(v1)
                if (.not. (v1 < 0 .or. (v1 == 0 .and. ignore_invalid))) exit
            end do

            if (c == "=") cycle
            if (v1 == 0) return
            value = value + v1 - 1
            call append_byte(iand(value, 255))
        end do

    contains

        !> Read the next character C of FROM and its value in the
        !> alphabet: 1 + the value, 0 if invalid, -1 if ignorable.
        subroutine next_value(v)
            integer(int32), intent(out) :: v

            c = from(f:f)
            f = f + 1

            select case (c)
            case (char(9), char(10), char(12), char(13), " ")
                v = -1
            case default
                v = index(merge(BASE64URL_CHARACTERS, BASE64_CHARACTERS, base64url), c)
            end select
        end subroutine next_value

        !> Append the byte BYTE (a raw-byte character if MULTIBYTE).
        subroutine append_byte(byte)
            integer(int32), intent(in) :: byte

            if (multibyte .and. byte >= 128) then
                ! Port of character.h:BYTE8_STRING.
                e(n + 1:n + 2) = achar(ior(192, iand(ishft(byte, -6), 1)))//achar(ior(128, iand(byte, 63)))
                n = n + 2
            else
                e(n + 1:n + 1) = achar(byte)
                n = n + 1
            end if

            nchars = nchars + 1
        end subroutine append_byte
    end subroutine base64_decode_1

    ! ------------------------------------------------------------------
    ! Buffers, random numbers, the load average
    ! ------------------------------------------------------------------

    !> Return data about lines in BUFFER.
    !> LINES is the number of lines in the buffer, LONGEST the length of
    !> the longest line, and MEAN the mean line length.  The lengths
    !> returned are in bytes, not characters.
    !>
    !> BUFFER is the current buffer if absent; a closed one is an error
    !> ("Invalid buffer argument").  Port of fns.c:Fbuffer_line_statistics.
    subroutine buffer_line_statistics(lines, longest, mean, status, buffer)
        integer(int64), intent(out) :: lines, longest
        real(real64), intent(out) :: mean
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: b, start, area, pre_gap, this_line, k

        lines = 0
        longest = 0
        mean = 0
        b = current

        if (present(buffer)) then
            call resolve_buffer(buffer, b, status)

            if (.not. status%ok()) then
                call nsberror(status)
                return
            end if
        end if

        associate (t => buffers(b)%text)
            start = BEGINNING_BYTE
            area = t%gpt_byte - BEGINNING_BYTE
            pre_gap = 0

            ! Process the first part of the buffer.
            do while (area > 0)
                k = index(t%beginning(start:start + area - 1), new_line("a"))

                if (k > 0) then
                    this_line = k - 1
                    longest = max(longest, this_line)
                    lines = lines + 1
                    ! Blame Knuth.
                    mean = mean + (this_line - mean)/lines
                    area = area - this_line - 1
                    start = start + this_line + 1
                else
                    ! Didn't have a newline here, so save the rest for the
                    ! post-gap calculation.
                    pre_gap = area
                    area = 0
                end if
            end do

            ! If the gap is before the end of the buffer, process the last half
            ! of the buffer.
            if (t%gpt_byte < t%z_byte) then
                start = t%gpt_byte + t%gap_size
                area = t%z_byte - t%gpt_byte

                do while (area > 0)
                    k = index(t%beginning(start:start + area - 1), new_line("a"))
                    this_line = merge(k - 1 + pre_gap, area + pre_gap, k > 0)
                    longest = max(longest, this_line)
                    lines = lines + 1
                    ! Blame Knuth again.
                    mean = mean + (this_line - mean)/lines
                    area = area - this_line - 1
                    start = start + this_line + 1
                    pre_gap = 0
                end do
            else if (pre_gap > 0) then
                longest = max(longest, pre_gap)
                lines = lines + 1
                mean = mean + (pre_gap - mean)/lines
            end if
        end associate
    end subroutine buffer_line_statistics

    !> Return a pseudo-random integer: all integers from -2**61 to 2**61 - 1
    !> are equally likely.  Port of fns.c:Frandom (no LIMIT).
    integer(int64) function random_integer() result(r)
        r = get_random()
        ! The low 62 bits as a signed integer (make_ufixnum).
        if (r > MOST_POSITIVE_FIXNUM) r = r - 2*(MOST_POSITIVE_FIXNUM + 1)
    end function random_integer

    !> Return a random integer in the interval [0, LIMIT).  A LIMIT that is
    !> not positive is an `arguments_out_of_range' error condition in STATUS
    !> (with STATUS absent the caller guarantees it; the value is then 0).
    !> Port of fns.c:Frandom (a positive integer LIMIT).
    integer(int64) function random_below(limit, status) result(r)
        integer(int64), intent(in) :: limit
        type(status_type), intent(out), optional :: status
        character(len=20) :: digits

        r = 0

        if (limit <= 0) then
            write (digits, "(i0)") limit
            if (present(status)) call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, trim(digits))
            return
        end if

        r = get_random_below(limit)
    end function random_below

    !> `random_below' for a default integer LIMIT.  Port of fns.c:Frandom.
    integer(int64) function random_below_default(limit, status) result(r)
        integer, intent(in) :: limit
        type(status_type), intent(out), optional :: status

        r = random_below(int(limit, int64), status)
    end function random_below_default

    !> Set the random number seed based on the contents of SEED, then
    !> return a random integer (as `random' with no argument).  The same
    !> SEED gives the same numbers after it.  Port of fns.c:Frandom (a
    !> string LIMIT).
    integer(int64) function random_seeded(seed) result(r)
        character(len=*), intent(in) :: seed

        call seed_random(seed)
        r = random_integer()
    end function random_seeded

    !> With RESEED true, set the random number seed from the system's
    !> entropy pool, then return a random integer (as `random' with no
    !> argument).  Port of fns.c:Frandom (LIMIT t).
    integer(int64) function random_reseeded(reseed) result(r)
        logical, intent(in) :: reseed

        if (reseed) call initialize_random()
        r = random_integer()
    end function random_reseeded

    !> Return a random integer R in the range 0 <= R < LIMIT: the remainder
    !> of a random integer (in range 0..INTMASK) divided by LIMIT, except
    !> rejecting the rare case where it is so close to INTMASK that the
    !> remainder isn't random.  Port of fns.c:get_random_fixnum.
    integer(int64) function get_random_below(limit) result(remainder)
        integer(int64), intent(in) :: limit
        integer(int64) :: difference_limit, difference, r

        difference_limit = INTMASK - limit + 1

        do
            r = get_random()
            remainder = mod(r, limit)
            difference = r - remainder
            if (difference_limit >= difference) exit
        end do
    end function get_random_below

    !> A random integer in 0..INTMASK (62 random bits), from the intrinsic
    !> `random_number' (seeded from the system's entropy pool on first use,
    !> as upstream seeds at startup).  Port of sysdep.c:get_random.
    integer(int64) function get_random() result(r)
        real(real64) :: halves(2)

        if (.not. random_initialized) call initialize_random()
        call random_number(halves)
        r = ior(ishft(int(halves(1)*2.0_real64**31, int64), 31), int(halves(2)*2.0_real64**31, int64))
    end function get_random

    !> Seed the random numbers from the system's entropy pool (the
    !> intrinsic `random_seed' with no argument asks the operating system,
    !> as getrandom does).  Port of sysdep.c:init_random.
    subroutine initialize_random()
        call random_seed()
        random_initialized = .true.
    end subroutine initialize_random

    !> Seed the random numbers from the bytes of SEED, folded into one
    !> 32-bit value as upstream folds them into its seed.  Port of
    !> sysdep.c:seed_random (set_random_seed with the intrinsic
    !> `random_seed').
    subroutine seed_random(seed)
        character(len=*), intent(in) :: seed
        integer, allocatable :: seeds(:)
        integer(int64) :: argument, x
        integer :: i, n

        argument = 0

        do i = 1, len(seed)
            argument = ieor(argument, ishft(int(ichar(seed(i:i)), int64), 8*mod(i - 1, 4)))
        end do

        ! Spread the value over the seed with a Lehmer generator (MINSTD).
        call random_seed(size=n)
        allocate (seeds(n))
        x = mod(argument, 2147483646_int64) + 1

        do i = 1, n
            x = mod(x*48271_int64, 2147483647_int64)
            seeds(i) = int(x)
        end do

        call random_seed(put=seeds)
        random_initialized = .true.
    end subroutine seed_random

    !> The 1 minute, 5 minute and 15 minute load averages, each multiplied
    !> by 100, then converted to integer (truncated).  If the 5-minute or
    !> 15-minute load averages are not available, LOADS is shorter,
    !> containing only those averages which are available.
    !>
    !> An error condition occurs if the load average can't be obtained.
    !> Port of fns.c:Fload_average (USE-FLOATS false).
    subroutine load_average_integer(loads, status)
        integer(int64), allocatable, intent(out) :: loads(:)
        type(status_type), intent(out) :: status
        real(real64), allocatable :: averages(:)

        call load_average_real(averages, status)
        loads = int(100.0_real64*averages, int64)
    end subroutine load_average_integer

    !> The 1 minute, 5 minute and 15 minute load averages, not multiplied
    !> by 100.  Port of fns.c:Fload_average (USE-FLOATS true).
    subroutine load_average_real(loads, status)
        real(real64), allocatable, intent(out) :: loads(:)
        type(status_type), intent(out) :: status
        real(real64) :: averages(3)
        integer :: n

        n = get_load_averages(averages)

        if (n < 0) then
            allocate (loads(0))
            call set_error(status, GENERIC_ERROR_CODE, "load_average not implemented for this operating system")
            return
        end if

        loads = averages(1:n)
    end subroutine load_average_real

    !> The character set of the C locale for ITEM `LOCALE_INFO_CODESET'
    !> (unallocated for another ITEM).  Port of fns.c:Flocale_info
    !> (codeset).
    subroutine locale_info_text(item, text)
        integer, intent(in) :: item
        character(len=:), allocatable, intent(out) :: text

        if (item == LOCALE_INFO_CODESET) text = "ANSI_X3.4-1968"
    end subroutine locale_info_text

    !> The day names (`LOCALE_INFO_DAYS') or month names
    !> (`LOCALE_INFO_MONTHS') of the C locale, padded with blanks to the
    !> longest (unallocated for another ITEM).  Port of fns.c:Flocale_info
    !> (days, months).
    subroutine locale_info_names(item, names)
        integer, intent(in) :: item
        character(len=:), allocatable, intent(out) :: names(:)

        select case (item)
        case (LOCALE_INFO_DAYS)
            names = [character(len=9) :: "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
        case (LOCALE_INFO_MONTHS)
            names = [character(len=9) :: "January", "February", "March", "April", "May", "June", "July", "August", &
                     "September", "October", "November", "December"]
        case default
            ! Not available as names.
        end select
    end subroutine locale_info_names

    !> The default paper size of the C locale in millimeters, width and
    !> height, for ITEM `LOCALE_INFO_PAPER' (unallocated for another
    !> ITEM).  Port of fns.c:Flocale_info (paper).
    subroutine locale_info_sizes(item, sizes)
        integer, intent(in) :: item
        integer, allocatable, intent(out) :: sizes(:)

        if (item == LOCALE_INFO_PAPER) sizes = [210, 297]
    end subroutine locale_info_sizes
end module utility_functions_mod
