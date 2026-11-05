! test_utility_functions_mod.f90 -- tests for utility_functions_mod.
! Ports GNU Emacs 31.1 test/src/fns-tests.el in part, without the cases
! Fortran cannot write.
!
! Copyright (C) 2014-2026 Free Software Foundation, Inc.
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

!> Ports test/src/fns-tests.el fns-tests-compare-strings (the cases with
!> a missing or non-integer argument cannot be written in Fortran),
!> string-search (the cases on multibyte strings: character strings are
!> always multibyte), test-string-distance, fns-tests-collate-strings and
!> fns-tests-collate-sort (the C locale's cases), fns-tests-equal-including-properties
!> and its /string-prop-vals variant, fns-tests-base64-encode-region,
!> fns-tests-base64-encode-string, fns-test-base64url-encode-region,
!> fns-test-base64url-encode-string, fns-tests-base64-decode-string,
!> fns-tests-random, test-buffer-line-stats-nogap and -gap.
!> fns-tests-string-bytes tests the intrinsic `len'.  Own tests for
!> `substring', `substring_no_properties', `clear_string', the character
!> and byte indexes, `is_string_equal', the base64 decoding of a region
!> and `load_average'.  test-secure-hash (the `iv-auto' case as
!> `hash_random_bytes') and fns-tests-hash-buffer (`sha1' being
!> `secure_hash' with SECURE_HASH_SHA1); own tests for the ranges, buffers, BINARY,
!> coding systems and errors of `md5' and `secure_hash', and for
!> `hash_random_bytes'.
module test_utility_functions_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, ARGUMENTS_OUT_OF_RANGE_CODE, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE, &
                         CODING_SYSTEM_ERROR_CODE
    use utility_functions_mod, only: compare_strings, substring, COMPARE_EQUAL, string_search, string_distance, &
                                     string_character_to_byte, string_byte_to_character, substring_no_properties, is_string_equal, &
                                  is_string_collate_less, is_string_collate_equal, clear_string, value_equal_including_properties, &
                                     base64_encode_string, base64url_encode_string, base64_decode_string, base64_encode_region, &
                                     base64url_encode_region, base64_decode_region, buffer_line_statistics, random, load_average, &
                                     locale_info, LOCALE_INFO_CODESET, LOCALE_INFO_DAYS, LOCALE_INFO_MONTHS, LOCALE_INFO_PAPER, &
                                     md5, secure_hash, secure_hash_algorithms, buffer_hash, hash_random_bytes, SECURE_HASH_MD5, &
                                 SECURE_HASH_SHA1, SECURE_HASH_SHA224, SECURE_HASH_SHA256, SECURE_HASH_SHA384, SECURE_HASH_SHA512, &
                                     SECURE_HASH_SHA3_224, SECURE_HASH_SHA3_256, SECURE_HASH_SHA3_384, SECURE_HASH_SHA3_512
    use value_mod, only: value_type, list, xstring
    use text_properties_mod, only: propertize
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, current_buffer, is_buffer_live
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point_minimum, point_maximum, point, delete_region
    use character_mod, only: string_to_multibyte
    implicit none(type, external)
    private

    public :: collect_utility_functions_mod

    character(len=*), parameter :: hello = "こんにちは"//"ｺﾝﾆﾁﾊ"
    character(len=*), parameter :: rotated = "んにちは"//"ｺﾝﾆﾁﾊ"//"こ"

contains

    !> Collect all exported unit tests.
    subroutine collect_utility_functions_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("fns-tests-compare-strings", test_compare_strings), &
                    new_unittest("substring", test_substring), &
                    new_unittest("string-search", test_string_search), &
                    new_unittest("test-string-distance", test_string_distance), &
                    new_unittest("string-indexes", test_string_indexes), &
                    new_unittest("substring-no-properties", test_substring_no_properties), &
                    new_unittest("string-equal", test_string_equal), &
                    new_unittest("clear-string", test_clear_string), &
                    new_unittest("fns-tests-collate-strings", test_collate_strings), &
                    new_unittest("fns-tests-collate-sort", test_collate_sort), &
                    new_unittest("fns-tests-equal-including-properties", test_equal_including_properties), &
                    new_unittest("fns-tests-equal-including-properties/string-prop-vals", test_equal_including_string_properties), &
                    new_unittest("fns-tests-base64-encode-region", test_base64_encode_region), &
                    new_unittest("fns-tests-base64-encode-string", test_base64_encode_string), &
                    new_unittest("fns-test-base64url-encode-region", test_base64url_encode_region), &
                    new_unittest("fns-test-base64url-encode-string", test_base64url_encode_string), &
                    new_unittest("fns-tests-base64-decode-string", test_base64_decode_string), &
                    new_unittest("base64-decode-region", test_base64_decode_region), &
                    new_unittest("fns-tests-random", test_random), &
                    new_unittest("test-buffer-line-stats-nogap", test_line_statistics_nogap), &
                    new_unittest("test-buffer-line-stats-gap", test_line_statistics_gap), &
                    new_unittest("load-average", test_load_average), &
                    new_unittest("locale-info", test_locale_info), &
                    new_unittest("test-secure-hash", test_secure_hash), &
                    new_unittest("hash-random-bytes", test_hash_random_bytes), &
                    new_unittest("fns-tests-hash-buffer", test_hash_buffer), &
                    new_unittest("secure-hash-ranges", test_secure_hash_ranges), &
                    new_unittest("secure-hash-buffer", test_secure_hash_buffer), &
                    new_unittest("md5", test_md5) &
                    ]
    end subroutine collect_utility_functions_mod

    subroutine test_compare_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: r

        r = compare_strings("xyzzy", "zyxxy", start1=0_int64, end1=10_int64, start2=0_int64, end2=5_int64, status=st)
        call check(error, st%ok() .and. r == -1, "end1 brought into range")
        if (allocated(error)) return
        r = compare_strings("xyzzy", "zyxxy", start1=0_int64, end1=5_int64, start2=-1_int64, end2=2_int64, status=st)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE .and. format_error(st) == 'Args out of range: "zyxxy", -1, 2', &
                   format_error(st))
        if (allocated(error)) return
        call check(error, compare_strings("", "") == COMPARE_EQUAL, "empty")
        if (allocated(error)) return
        call check(error, compare_strings("", "", start1=0_int64, end1=0_int64, start2=0_int64, end2=0_int64) &
                   == COMPARE_EQUAL, "empty 0 0")
        if (allocated(error)) return
        call check(error, compare_strings("test", "test") == COMPARE_EQUAL, "test")
        if (allocated(error)) return
        call check(error, compare_strings("test", "test", .true.) == COMPARE_EQUAL, "test t")
        if (allocated(error)) return
        call check(error, compare_strings("test", "test", .false.) == COMPARE_EQUAL, "test nil")
        if (allocated(error)) return
        call check(error, compare_strings("Test", "test", .true.) == COMPARE_EQUAL, "Test t")
        if (allocated(error)) return
        call check(error, compare_strings("Test", "test") == -1, "Test")
        if (allocated(error)) return
        call check(error, compare_strings("test", "Test") == 1, "test Test")
        if (allocated(error)) return
        call check(error, compare_strings("foobaz", "barbaz") == 1, "foobaz")
        if (allocated(error)) return
        call check(error, compare_strings("barbaz", "foobar") == -1, "barbaz")
        if (allocated(error)) return
        call check(error, compare_strings("foobaz", "farbaz") == 2, "farbaz")
        if (allocated(error)) return
        call check(error, compare_strings("farbaz", "foobar") == -2, "foobar")
        if (allocated(error)) return
        call check(error, compare_strings("abcxyz", "abcprq", start1=0_int64, end1=2_int64, start2=0_int64, &
                                          end2=2_int64) == COMPARE_EQUAL, "0 2")
        if (allocated(error)) return
        call check(error, compare_strings("abcxyz", "abcprq", start1=0_int64, end1=-3_int64, start2=0_int64, &
                                          end2=-3_int64) == COMPARE_EQUAL, "0 -3")
        if (allocated(error)) return
        call check(error, compare_strings("abcxyz", "abcprq", start1=0_int64, end1=6_int64, start2=0_int64, &
                                          end2=6_int64) == 4, "0 6")
        if (allocated(error)) return
        call check(error, compare_strings("abcprq", "abcxyz", start1=0_int64, end1=6_int64, start2=0_int64, &
                                          end2=6_int64) == -4, "0 6 reversed")
        if (allocated(error)) return
        call check(error, compare_strings("xyzzy", "azza", start1=-3_int64, end1=4_int64, start2=-3_int64, &
                                          end2=3_int64) == COMPARE_EQUAL, "negative starts")
        if (allocated(error)) return
        call check(error, compare_strings(hello, hello) == COMPARE_EQUAL, "multibyte")
        if (allocated(error)) return
        call check(error, compare_strings(rotated, hello) == 1, "multibyte greater")
        if (allocated(error)) return
        call check(error, compare_strings(hello, rotated) == -1, "multibyte less")
    end subroutine test_compare_strings

    !> Negative indices count from the end; a range outside the string
    !> signals args-out-of-range (fns.c:validate_subarray).
    subroutine test_substring(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: r

        r = substring("héllo", 1_int64, 3_int64, st)
        call check(error, st%ok() .and. r == "él", r)
        if (allocated(error)) return
        r = substring("héllo", -3_int64, status=st)
        call check(error, st%ok() .and. r == "llo", r)
        if (allocated(error)) return
        r = substring("héllo", 1_int64, -1_int64, st)
        call check(error, st%ok() .and. r == "éll", r)
        if (allocated(error)) return
        r = substring("abc", 0_int64, 4_int64, st)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE .and. format_error(st) == 'Args out of range: "abc", 0, 4', &
                   format_error(st))
        if (allocated(error)) return
        r = substring("abc", 2_int64, 1_int64, st)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE, "from after to")
        if (allocated(error)) return
        r = substring("abc", -4_int64, status=st)
        call check(error, &
                   st%code == ARGUMENTS_OUT_OF_RANGE_CODE .and. format_error(st) == 'Args out of range: "abc", -4, .false.', &
                   format_error(st))
    end subroutine test_substring

    subroutine enter(name, h, old)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
    end subroutine enter

    subroutine leave(h, old)
        type(handle_type), intent(in) :: h, old
        type(status_type) :: st

        if (is_buffer_live(old)) call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine leave

    !> The bytes 14 fb 9c 03 d9 followed by LAST, the data of the base64
    !> tests (a unibyte string upstream).
    function test_bytes(last) result(bytes)
        integer, intent(in) :: last
        character(len=:), allocatable :: bytes

        bytes = achar(20)//achar(251)//achar(156)//achar(3)//achar(217)//achar(last)
    end function test_bytes

    !> fns-tests--with-region for base64-encode-region and
    !> base64url-encode-region: TEXT inserted in a buffer (multibyte, its raw
    !> bytes as raw-byte characters), the region encoded, the text.
    function encoded_region(text, url, no_line_break, status) result(encoded)
        character(len=*), intent(in) :: text
        logical, intent(in) :: url
        logical, intent(in), optional :: no_line_break
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: encoded
        type(handle_type) :: h, old
        integer(int64) :: length

        call enter("tf-base64", h, old)
        call insert(text, status)

        if (url) then
            call base64url_encode_region(point_minimum(), point_maximum(), length, status, no_line_break)
        else
            call base64_encode_region(point_minimum(), point_maximum(), length, status, no_line_break)
        end if

        encoded = buffer_string()
        call leave(h, old)
    end function encoded_region

    subroutine test_string_search(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=*), parameter :: c130 = achar(194)//achar(130), c10f98 = achar(240)//achar(144)//achar(190)//achar(152)
        character(len=26) :: letters
        integer :: k

        call check(error, string_search("zot", "foobarzot") == 6)
        if (allocated(error)) return
        call check(error, string_search("foo", "foobarzot") == 0)
        if (allocated(error)) return
        call check(error, string_search("fooz", "foobarzot") == -1)
        if (allocated(error)) return
        call check(error, string_search("zot", "foobarzo") == -1)
        if (allocated(error)) return
        call check(error, string_search("ab", "ab") == 0)
        if (allocated(error)) return
        call check(error, string_search("ab"//achar(0), "ab") == -1)
        if (allocated(error)) return
        call check(error, string_search("ab", "abababab", 3_int64) == 4)
        if (allocated(error)) return
        call check(error, string_search("ab", "ababac", 3_int64) == -1)
        if (allocated(error)) return
        call check(error, string_search("aaa", "aa") == -1)
        if (allocated(error)) return
        call check(error, string_search("ab", "AB") == -1, "case is significant")
        if (allocated(error)) return
        call check(error, string_search(repeat(c130, 2), "helló"//repeat(c130, 5)//"bár") == 5)
        if (allocated(error)) return
        call check(error, string_search(repeat(achar(127), 2), "helló"//repeat(achar(127), 5)//"bár") == 5)
        if (allocated(error)) return
        call check(error, string_search("fóo", "zotfóo") == 3)
        if (allocated(error)) return
        call check(error, string_search("ø", repeat("a", 32)) == -1)
        if (allocated(error)) return

        do k = 1, 26
            letters(k:k) = achar(iachar("a") + k - 1)
        end do

        call check(error, string_search("o", letters) == 14)
        if (allocated(error)) return
        call check(error, string_search("a"//c10f98//"z", "a"//c10f98//"a"//c10f98//"z") == 2)
        if (allocated(error)) return
        call check(error, string_search("a", "abc", -1_int64, st) == -1 .and. st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        call check(error, string_search("a", "abc", 4_int64, st) == -1 .and. st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        call check(error, string_search("a", "abc", 100000000000_int64, st) == -1 .and. st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        call check(error, string_search("a", "aaa", 3_int64) == -1)
        if (allocated(error)) return
        call check(error, string_search("aa", "aa", 1_int64) == -1)
        if (allocated(error)) return
        call check(error, string_search(achar(0), "") == -1)
        if (allocated(error)) return
        call check(error, string_search("", "") == 0)
        if (allocated(error)) return
        call check(error, string_search("", "", 1_int64, st) == -1 .and. st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        call check(error, string_search("", "abc") == 0)
        if (allocated(error)) return
        call check(error, string_search("", "abc", 2_int64) == 2)
        if (allocated(error)) return
        call check(error, string_search("", "abc", 3_int64) == 3)
        if (allocated(error)) return
        call check(error, string_search("", "abc", 4_int64, st) == -1 .and. st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        call check(error, string_search("", "abc", -1_int64, st) == -1 .and. st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
    end subroutine test_string_search

    subroutine test_string_distance(error)
        type(error_type), allocatable, intent(out) :: error

        ! ASCII characters are always fine
        call check(error, string_distance("heelo", "hello") == 1)
        if (allocated(error)) return
        call check(error, string_distance("aeelo", "hello") == 2)
        if (allocated(error)) return
        call check(error, string_distance("ab", "ab", .true.) == 0)
        if (allocated(error)) return
        call check(error, string_distance("ab", "abc", .true.) == 1)
        if (allocated(error)) return
        ! string containing hanzi character, compare by byte
        call check(error, string_distance("ab", "ab我她", .true.) == 6)
        if (allocated(error)) return
        call check(error, string_distance("ab", "a我b", .true.) == 3)
        if (allocated(error)) return
        call check(error, string_distance("我", "她", .true.) == 3)
        if (allocated(error)) return
        ! string containing hanzi character, compare by character
        call check(error, string_distance("ab", "ab我她") == 2)
        if (allocated(error)) return
        call check(error, string_distance("ab", "a我b") == 1)
        if (allocated(error)) return
        call check(error, string_distance("我", "她") == 1)
        if (allocated(error)) return
        ! correct behavior with empty strings
        call check(error, string_distance("", "") == 0)
        if (allocated(error)) return
        call check(error, string_distance("", "", .true.) == 0)
        if (allocated(error)) return
        call check(error, string_distance("x", "") == 1)
        if (allocated(error)) return
        call check(error, string_distance("x", "", .true.) == 1)
        if (allocated(error)) return
        call check(error, string_distance("", "x") == 1)
        if (allocated(error)) return
        call check(error, string_distance("", "x", .true.) == 1)
    end subroutine test_string_distance

    !> Character and byte offsets, counting from 0.
    subroutine test_string_indexes(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, string_character_to_byte("héllo", 2_int64) == 3)
        if (allocated(error)) return
        call check(error, string_character_to_byte("héllo", 0_int64) == 0)
        if (allocated(error)) return
        call check(error, string_character_to_byte("héllo", 9_int64) == 6, "past the end")
        if (allocated(error)) return
        call check(error, string_byte_to_character("héllo", 3_int64) == 2)
        if (allocated(error)) return
        call check(error, string_byte_to_character("héllo", 6_int64) == 5)
    end subroutine test_string_indexes

    subroutine test_substring_no_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: v
        character(len=:), allocatable :: r

        v = propertize(value_type(text="héllo"), list(value_type(name="face"), value_type(name="bold")))
        r = substring_no_properties(v, 1_int64, 3_int64, st)
        call check(error, st%ok() .and. r == "él", r)
        if (allocated(error)) return
        r = substring_no_properties(v, status=st)
        call check(error, st%ok() .and. r == "héllo", r)
        if (allocated(error)) return
        r = substring_no_properties(v, -2_int64, status=st)
        call check(error, st%ok() .and. r == "lo", r)
        if (allocated(error)) return
        r = substring_no_properties(v, 4_int64, 2_int64, st)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE, "from after to")
        if (allocated(error)) return
        r = substring_no_properties(value_type(name="x"), status=st)
        call check(error, st%code == WRONG_TYPE_ARGUMENT_CODE, "not a string")
    end subroutine test_substring_no_properties

    subroutine test_string_equal(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_string_equal(value_type(text="abc"), value_type(text="abc")))
        if (allocated(error)) return
        call check(error, is_string_equal(value_type(text="abc"), value_type(name="abc")), "a name's text")
        if (allocated(error)) return
        call check(error,.not. is_string_equal(value_type(text="abc"), value_type(text="abc ")), "trailing blank")
        if (allocated(error)) return
        call check(error,.not. is_string_equal(value_type(text="abc"), value_type(text="ABC")), "case")
        if (allocated(error)) return
        call check(error, is_string_equal(propertize(value_type(text="é"), list(value_type(name="k"), value_type(1_int64))), &
                                          value_type(text="é")), "text properties are ignored")
    end subroutine test_string_equal

    subroutine test_clear_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: v

        v = propertize(value_type(text="hé"), list(value_type(name="k"), value_type(name="v")))
        call clear_string(v, st)
        call check(error, st%ok() .and. xstring(v) == repeat(achar(0), 3))
        if (allocated(error)) return
        call check(error, value_equal_including_properties(v, value_type(text=repeat(achar(0), 3))), "no properties")
        if (allocated(error)) return
        v = value_type(1_int64)
        call clear_string(v, st)
        call check(error, st%code == WRONG_TYPE_ARGUMENT_CODE)
    end subroutine test_clear_string

    !> The C locale's cases.
    subroutine test_collate_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: r

        call check(error, is_string_collate_equal("xyzzy", "xyzzy"))
        if (allocated(error)) return
        call check(error,.not. is_string_collate_equal("xyzzy", "XYZZY"))
        if (allocated(error)) return
        ! In POSIX or C locales, collation order is lexicographic.
        call check(error, is_string_collate_less("XYZZY", "xyzzy", "POSIX"))
        if (allocated(error)) return
        ! Ignore case.
        call check(error, is_string_collate_equal("xyzzy", "XYZZY", ignore_case=.true.))
        if (allocated(error)) return
        ! Locale must be valid.
        r = is_string_collate_equal("xyzzy", "xyzzy", "not-a-locale", status=st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. &
                   format_error(st) == "Invalid locale not-a-locale: No such file or directory", format_error(st))
    end subroutine test_collate_strings

    !> The C locale's cases: each list is in order.
    subroutine test_collate_sort(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: sorted(6) = [character(len=3) :: "1 1", "1 2", "1.1", "1.2", "11 ", "12 "]
        integer :: k

        ! Punctuation and whitespace characters are relevant for POSIX.
        do k = 1, 5
            call check(error, is_string_collate_less(trim(sorted(k)), trim(sorted(k + 1)), "POSIX"), sorted(k))
            if (allocated(error)) return
        end do

        ! Diacritics are different letters for POSIX, they sort lexicographical.
        call check(error, is_string_collate_less("Adrian", "Agustín", "POSIX"))
        if (allocated(error)) return
        call check(error, is_string_collate_less("Agustín", "Eli", "POSIX"))
        if (allocated(error)) return
        call check(error, is_string_collate_less("Eli", "Ævar", "POSIX"))
    end subroutine test_collate_sort

    !> The string FOO with the text properties PROPERTIES.
    function propertized(foo, properties) result(v)
        character(len=*), intent(in) :: foo
        type(value_type), intent(in) :: properties
        type(value_type) :: v

        v = propertize(value_type(text=foo), properties)
    end function propertized

    subroutine test_equal_including_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: a, b, c, d, e, k, v, x

        a = value_type(name="a")
        b = value_type(name="b")
        c = value_type(name="c")
        d = value_type(name="d")
        e = value_type(name="e")
        k = value_type(name="k")
        v = value_type(name="v")
        x = value_type(name="x")
        call check(error, value_equal_including_properties(value_type(text=""), value_type(text="")))
        if (allocated(error)) return
        call check(error, value_equal_including_properties(value_type(text="foo"), value_type(text="foo")))
        if (allocated(error)) return
        call check(error, value_equal_including_properties(propertized("foo", list(a, b)), propertized("foo", list(a, b))))
        if (allocated(error)) return
        call check(error, value_equal_including_properties(propertized("foo", list(a, b, c, d)), &
                                                           propertized("foo", list(a, b, c, d))))
        if (allocated(error)) return
        call check(error, value_equal_including_properties(propertized("foo", list(c, d, a, b)), &
                                                           propertized("foo", list(a, b, c, d))), "in any order")
        if (allocated(error)) return
        call check(error, value_equal_including_properties(propertized("a", list(k, v)), propertized("a", list(k, v))))
        if (allocated(error)) return
        call check(error,.not. value_equal_including_properties(propertized("a", list(k, v)), propertized("a", list(k, x))))
        if (allocated(error)) return
        call check(error,.not. value_equal_including_properties(propertized("a", list(k, v)), propertized("b", list(k, v))))
        if (allocated(error)) return
        call check(error,.not. value_equal_including_properties(propertized("foo", list(a, b, c, e)), &
                                                                propertized("foo", list(a, b, c, d))))
        if (allocated(error)) return
        call check(error,.not. value_equal_including_properties(propertized("foo", list(a, b)), value_type(text="foo")), &
                   "properties against none")
    end subroutine test_equal_including_properties

    !> Handle string property values.  (Bug#6581)
    subroutine test_equal_including_string_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: a, k

        a = value_type(name="a")
        k = value_type(name="k")
        call check(error, value_equal_including_properties(propertized("a", list(k, value_type(text="v"))), &
                                                           propertized("a", list(k, value_type(text="v")))))
        if (allocated(error)) return
        call check(error, value_equal_including_properties(propertized("foo", list(a, list(value_type(.true.)))), &
                                                           propertized("foo", list(a, list(value_type(.true.))))))
        if (allocated(error)) return
        call check(error,.not. value_equal_including_properties(propertized("a", list(k, value_type(text="v"))), &
                                                                propertized("a", list(k, value_type(text="x")))))
        if (allocated(error)) return
        call check(error,.not. value_equal_including_properties(propertized("a", list(k, value_type(text="v"))), &
                                                                propertized("b", list(k, value_type(text="v")))))
    end subroutine test_equal_including_string_properties

    subroutine test_base64_encode_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! standard variant RFC2045
        call check(error, encoded_region("", .false., status=st) == "")
        if (allocated(error)) return
        call check(error, encoded_region("f", .false., status=st) == "Zg==")
        if (allocated(error)) return
        call check(error, encoded_region("fo", .false., status=st) == "Zm8=")
        if (allocated(error)) return
        call check(error, encoded_region("foo", .false., status=st) == "Zm9v")
        if (allocated(error)) return
        call check(error, encoded_region("foob", .false., status=st) == "Zm9vYg==")
        if (allocated(error)) return
        call check(error, encoded_region("fooba", .false., status=st) == "Zm9vYmE=")
        if (allocated(error)) return
        call check(error, encoded_region("foobar", .false., status=st) == "Zm9vYmFy")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(test_bytes(126)), .false., status=st) == "FPucA9l+")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(test_bytes(127)), .false., status=st) == "FPucA9l/")
    end subroutine test_base64_encode_region

    subroutine test_base64_encode_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: r

        ! standard variant RFC2045
        call check(error, base64_encode_string("") == "")
        if (allocated(error)) return
        call check(error, base64_encode_string("f") == "Zg==")
        if (allocated(error)) return
        call check(error, base64_encode_string("fo") == "Zm8=")
        if (allocated(error)) return
        call check(error, base64_encode_string("foo") == "Zm9v")
        if (allocated(error)) return
        call check(error, base64_encode_string("foob") == "Zm9vYg==")
        if (allocated(error)) return
        call check(error, base64_encode_string("fooba") == "Zm9vYmE=")
        if (allocated(error)) return
        call check(error, base64_encode_string("foobar") == "Zm9vYmFy")
        if (allocated(error)) return
        call check(error, base64_encode_string(test_bytes(126), multibyte=.false.) == "FPucA9l+")
        if (allocated(error)) return
        call check(error, base64_encode_string(test_bytes(127), multibyte=.false.) == "FPucA9l/")
        if (allocated(error)) return
        call check(error, base64_encode_string(string_to_multibyte(test_bytes(127))) == "FPucA9l/", "raw bytes")
        if (allocated(error)) return
        r = base64_encode_string("ƒ", status=st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. &
                   format_error(st) == "Multibyte character in data for base64 encoding", format_error(st))
        if (allocated(error)) return
        r = base64_encode_string("ü", status=st)
        call check(error, st%code == GENERIC_ERROR_CODE)
        if (allocated(error)) return
        ! A line break every 76 characters.
        r = base64_encode_string(repeat("f", 60))
        call check(error, r == repeat("Zm", 38)//new_line("a")//"ZmZm", r)
    end subroutine test_base64_encode_string

    subroutine test_base64url_encode_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! url variant with padding
        call check(error, encoded_region("", .true., status=st) == "")
        if (allocated(error)) return
        call check(error, encoded_region("f", .true., status=st) == "Zg==")
        if (allocated(error)) return
        call check(error, encoded_region("fo", .true., status=st) == "Zm8=")
        if (allocated(error)) return
        call check(error, encoded_region("foo", .true., status=st) == "Zm9v")
        if (allocated(error)) return
        call check(error, encoded_region("foob", .true., status=st) == "Zm9vYg==")
        if (allocated(error)) return
        call check(error, encoded_region("fooba", .true., status=st) == "Zm9vYmE=")
        if (allocated(error)) return
        call check(error, encoded_region("foobar", .true., status=st) == "Zm9vYmFy")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(test_bytes(126)), .true., status=st) == "FPucA9l-")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(test_bytes(127)), .true., status=st) == "FPucA9l_")
        if (allocated(error)) return
        ! url variant no padding
        call check(error, encoded_region("", .true., .true., st) == "")
        if (allocated(error)) return
        call check(error, encoded_region("f", .true., .true., st) == "Zg")
        if (allocated(error)) return
        call check(error, encoded_region("fo", .true., .true., st) == "Zm8")
        if (allocated(error)) return
        call check(error, encoded_region("foo", .true., .true., st) == "Zm9v")
        if (allocated(error)) return
        call check(error, encoded_region("foob", .true., .true., st) == "Zm9vYg")
        if (allocated(error)) return
        call check(error, encoded_region("fooba", .true., .true., st) == "Zm9vYmE")
        if (allocated(error)) return
        call check(error, encoded_region("foobar", .true., .true., st) == "Zm9vYmFy")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(test_bytes(126)), .true., .true., st) == "FPucA9l-")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(test_bytes(127)), .true., .true., st) == "FPucA9l_")
        if (allocated(error)) return
        ! url variant no line break no padding
        call check(error, encoded_region(repeat("f", 100), .true., .true., st) == repeat("Zm", 66)//"Zg")
        if (allocated(error)) return
        call check(error, encoded_region(repeat("fo", 50), .true., .true., st) == repeat("Zm9mb2Zv", 16)//"Zm9mbw")
        if (allocated(error)) return
        call check(error, encoded_region(repeat("foo", 25), .true., .true., st) == repeat("Zm9v", 25))
        if (allocated(error)) return
        call check(error, encoded_region(repeat("foob", 15), .true., .true., st) == repeat("Zm9vYmZvb2Jmb29i", 5))
        if (allocated(error)) return
        call check(error, encoded_region(repeat("fooba", 15), .true., .true., st) == repeat("Zm9vYmFmb29iYWZvb2Jh", 5))
        if (allocated(error)) return
        call check(error, encoded_region(repeat("foobar", 15), .true., .true., st) == repeat("Zm9vYmFyZm9vYmFy", 7)//"Zm9vYmFy")
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(repeat(test_bytes(126), 10)), .true., .true., st) &
                   == repeat("FPucA9l-", 10))
        if (allocated(error)) return
        call check(error, encoded_region(string_to_multibyte(repeat(test_bytes(127), 10)), .true., .true., st) &
                   == repeat("FPucA9l_", 10))
        if (allocated(error)) return
        call check(error, encoded_region("ƒ", .true., status=st) == "ƒ" .and. st%code == GENERIC_ERROR_CODE, &
                   "a multibyte character")
        if (allocated(error)) return
        call check(error, encoded_region("ü", .true., status=st) == "ü" .and. st%code == GENERIC_ERROR_CODE)
    end subroutine test_base64url_encode_region

    subroutine test_base64url_encode_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: r

        ! url variant with padding
        call check(error, base64url_encode_string("") == "")
        if (allocated(error)) return
        call check(error, base64url_encode_string("f") == "Zg==")
        if (allocated(error)) return
        call check(error, base64url_encode_string("fo") == "Zm8=")
        if (allocated(error)) return
        call check(error, base64url_encode_string("foo") == "Zm9v")
        if (allocated(error)) return
        call check(error, base64url_encode_string("foob") == "Zm9vYg==")
        if (allocated(error)) return
        call check(error, base64url_encode_string("fooba") == "Zm9vYmE=")
        if (allocated(error)) return
        call check(error, base64url_encode_string("foobar") == "Zm9vYmFy")
        if (allocated(error)) return
        call check(error, base64url_encode_string(test_bytes(126), multibyte=.false.) == "FPucA9l-")
        if (allocated(error)) return
        call check(error, base64url_encode_string(test_bytes(127), multibyte=.false.) == "FPucA9l_")
        if (allocated(error)) return
        ! url variant no padding
        call check(error, base64url_encode_string("", .true.) == "")
        if (allocated(error)) return
        call check(error, base64url_encode_string("f", .true.) == "Zg")
        if (allocated(error)) return
        call check(error, base64url_encode_string("fo", .true.) == "Zm8")
        if (allocated(error)) return
        call check(error, base64url_encode_string("foo", .true.) == "Zm9v")
        if (allocated(error)) return
        call check(error, base64url_encode_string("foob", .true.) == "Zm9vYg")
        if (allocated(error)) return
        call check(error, base64url_encode_string("fooba", .true.) == "Zm9vYmE")
        if (allocated(error)) return
        call check(error, base64url_encode_string("foobar", .true.) == "Zm9vYmFy")
        if (allocated(error)) return
        call check(error, base64url_encode_string(test_bytes(126), .true., .false.) == "FPucA9l-")
        if (allocated(error)) return
        call check(error, base64url_encode_string(test_bytes(127), .true., .false.) == "FPucA9l_")
        if (allocated(error)) return
        ! url variant no line break no padding
        call check(error, base64url_encode_string(repeat("f", 100), .true.) == repeat("Zm", 66)//"Zg")
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat("fo", 50), .true.) == repeat("Zm9mb2Zv", 16)//"Zm9mbw")
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat("foo", 25), .true.) == repeat("Zm9v", 25))
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat("foob", 15), .true.) == repeat("Zm9vYmZvb2Jmb29i", 5))
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat("fooba", 15), .true.) == repeat("Zm9vYmFmb29iYWZvb2Jh", 5))
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat("foobar", 15), .true.) == repeat("Zm9vYmFyZm9vYmFy", 7)//"Zm9vYmFy")
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat(test_bytes(126), 10), .true., .false.) == repeat("FPucA9l-", 10))
        if (allocated(error)) return
        call check(error, base64url_encode_string(repeat(test_bytes(127), 10), .true., .false.) == repeat("FPucA9l_", 10))
        if (allocated(error)) return
        r = base64url_encode_string("ƒ", status=st)
        call check(error, st%code == GENERIC_ERROR_CODE)
        if (allocated(error)) return
        r = base64url_encode_string("ü", status=st)
        call check(error, st%code == GENERIC_ERROR_CODE)
    end subroutine test_base64url_encode_string

    subroutine test_base64_decode_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: r
        character(len=9), parameter :: inputs(4) = [character(len=9) :: "Zg=", "Zm9vYmE", "Zm9vYmFy=", "Zg=Zg="]
        integer :: k

        ! standard variant RFC2045
        call check(error, base64_decode_string("") == "")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zg==") == "f")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm8=") == "fo")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9v") == "foo")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYg==") == "foob")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYmE=") == "fooba")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYmFy") == "foobar")
        if (allocated(error)) return
        call check(error, base64_decode_string("FPucA9l+") == test_bytes(126))
        if (allocated(error)) return
        call check(error, base64_decode_string("FPucA9l/") == test_bytes(127))
        if (allocated(error)) return
        ! no padding
        call check(error, base64_decode_string("", .true.) == "")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zg", .true.) == "f")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm8", .true.) == "fo")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9v", .true.) == "foo")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYg", .true.) == "foob")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYmE", .true.) == "fooba")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYmFy", .true.) == "foobar")
        if (allocated(error)) return
        ! url variant with padding
        call check(error, base64_decode_string("Zg==", .true.) == "f")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm8=", .true.) == "fo")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYg==", .true.) == "foob")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9vYmE=", .true.) == "fooba")
        if (allocated(error)) return
        call check(error, base64_decode_string("FPucA9l-", .true.) == test_bytes(126))
        if (allocated(error)) return
        call check(error, base64_decode_string("FPucA9l_", .true.) == test_bytes(127))
        if (allocated(error)) return
        ! url variant no line break no padding
        call check(error, base64_decode_string(repeat("Zm", 66)//"Zg", .true.) == repeat("f", 100))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("Zm9mb2Zv", 16)//"Zm9mbw", .true.) == repeat("fo", 50))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("Zm9v", 25), .true.) == repeat("foo", 25))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("Zm9vYmZvb2Jmb29i", 5), .true.) == repeat("foob", 15))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("Zm9vYmFmb29iYWZvb2Jh", 5), .true.) == repeat("fooba", 15))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("Zm9vYmFyZm9vYmFy", 7)//"Zm9vYmFy", .true.) == repeat("foobar", 15))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("FPucA9l-", 10), .true.) == repeat(test_bytes(126), 10))
        if (allocated(error)) return
        call check(error, base64_decode_string(repeat("FPucA9l_", 10), .true.) == repeat(test_bytes(127), 10))
        if (allocated(error)) return

        ! errors check
        do k = 1, size(inputs)
            r = base64_decode_string(trim(inputs(k)), status=st)
            call check(error, st%code == GENERIC_ERROR_CODE .and. format_error(st) == "Invalid base64 data", inputs(k))
            if (allocated(error)) return
        end do

        ! Own: ignored and invalid characters.
        call check(error, base64_decode_string("Zm9v"//new_line("a")//" YmFy") == "foobar", "whitespace")
        if (allocated(error)) return
        call check(error, base64_decode_string("Zm9v*YmFy", ignore_invalid=.true.) == "foobar", "ignore invalid")
        if (allocated(error)) return
        r = base64_decode_string("Zm9v*YmFy", status=st)
        call check(error, st%code == GENERIC_ERROR_CODE, "invalid")
    end subroutine test_base64_decode_string

    !> Own: decoding a region gives raw-byte characters in a multibyte
    !> buffer; data that cannot be decoded leaves the buffer as it was.
    subroutine test_base64_decode_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: length

        call enter("tf-base64-decode", h, old)
        call insert("<Zm9vYmFy>", st)
        call goto_character(point_maximum())
        call base64_decode_region(2_int64, 10_int64, length, st)
        call check(error, st%ok() .and. length == 6 .and. buffer_string() == "<foobar>" .and. point() == point_maximum(), &
                   buffer_string())

        if (allocated(error)) then
            call leave(h, old)
            return
        end if

        call delete_region(point_minimum(), point_maximum(), st)
        call insert("FPucA9l+", st)
        call base64_decode_region(point_minimum(), point_maximum(), length, st)
        call check(error, st%ok() .and. length == 6 .and. buffer_string() == string_to_multibyte(test_bytes(126)), "raw bytes")

        if (allocated(error)) then
            call leave(h, old)
            return
        end if

        call delete_region(point_minimum(), point_maximum(), st)
        call insert("Zg=", st)
        call base64_decode_region(point_minimum(), point_maximum(), length, st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. buffer_string() == "Zg=", "invalid data")
        call leave(h, old)
    end subroutine test_base64_decode_region

    subroutine test_random(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: r, r1, r2, r3, r4
        integer :: k

        r = random(-1_int64, st)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        r = random(0_int64, st)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE)
        if (allocated(error)) return
        r = random()
        call check(error, r >= -2_int64**61 .and. r < 2_int64**61, "in range")
        if (allocated(error)) return
        call check(error, random(1) == 0)
        if (allocated(error)) return

        do k = 1, 100
            r = random(10)
            call check(error, r >= 0 .and. r < 10)
            if (allocated(error)) return
        end do

        r1 = random("seed")
        r2 = random("seed")
        call check(error, r1 == r2, "a seed string gives the same numbers")
        if (allocated(error)) return
        ! The probability of four calls being the same is low.
        ! This makes sure that the value isn't constant.
        r1 = random(.true.)
        r2 = random(.true.)
        r3 = random(.true.)
        r4 = random(.true.)
        call check(error,.not. (r1 == r2 .and. r2 == r3 .and. r3 == r4))
        ! Reset the PRNG seed after testing.
        r = random(.true.)
    end subroutine test_random

    !> The statistics of the buffer after TEXT is inserted.
    subroutine line_statistics(text, lines, longest, mean)
        character(len=*), intent(in) :: text
        integer(int64), intent(out) :: lines, longest
        real(real64), intent(out) :: mean
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tf-line-stats", h, old)
        call insert(text, st)
        call buffer_line_statistics(lines, longest, mean, st)
        call leave(h, old)
    end subroutine line_statistics

    !> Whether the statistics are approximately LINES, LONGEST and MEAN.
    logical function is_approx(lines, longest, mean, lines2, longest2, mean2)
        integer(int64), intent(in) :: lines, longest, lines2, longest2
        real(real64), intent(in) :: mean, mean2

        is_approx = lines == lines2 .and. longest == longest2 .and. abs(mean - mean2) < 0.1_real64
    end function is_approx

    subroutine test_line_statistics_nogap(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: lf = new_line("a")
        integer(int64) :: lines, longest
        real(real64) :: mean

        call line_statistics("", lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 0_int64, 0_int64, 0.0_real64))
        if (allocated(error)) return
        call line_statistics("123"//lf, lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 1_int64, 3_int64, 3.0_real64))
        if (allocated(error)) return
        call line_statistics("123"//lf//"12345"//lf//"123"//lf, lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 3_int64, 5_int64, 3.66_real64))
        if (allocated(error)) return
        call line_statistics("123"//lf//"12345"//lf//"123", lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 3_int64, 5_int64, 3.66_real64))
        if (allocated(error)) return
        call line_statistics("123"//lf//"12345", lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 2_int64, 5_int64, 4.0_real64))
        if (allocated(error)) return
        call line_statistics("123"//lf//"12é45"//lf//"123"//lf, lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 3_int64, 6_int64, 4.0_real64))
        if (allocated(error)) return
        call line_statistics(lf//lf//lf, lines, longest, mean)
        call check(error, is_approx(lines, longest, mean, 3_int64, 0_int64, 0.0_real64))
    end subroutine test_line_statistics_nogap

    subroutine test_line_statistics_gap(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: lf = new_line("a")
        character(len=*), parameter :: line = "12345678901234567890123456789012345678901234567890"//lf
        type(handle_type) :: h, old
        type(status_type) :: st
        integer(int64) :: lines, longest
        real(real64) :: mean
        integer :: pass, k

        do pass = 1, 3
            call enter("tf-line-stats-gap", h, old)

            do k = 1, 1000
                call insert(line, st)
            end do

            call goto_character(point_minimum())
            ! This should make a gap appear.
            call insert("123"//lf, st)

            select case (pass)
            case (1)
                call delete_region(point_minimum(), point(), st)
                call buffer_line_statistics(lines, longest, mean, st)
                call check(error, is_approx(lines, longest, mean, 1000_int64, 50_int64, 50.0_real64))
            case (2)
                call buffer_line_statistics(lines, longest, mean, st)
                call check(error, is_approx(lines, longest, mean, 1001_int64, 50_int64, 49.9_real64))
            case default
                call goto_character(point_maximum())
                call insert("fóo", st)
                call buffer_line_statistics(lines, longest, mean, st)
                call check(error, is_approx(lines, longest, mean, 1002_int64, 50_int64, 49.9_real64))
            end select

            call leave(h, old)
            if (allocated(error)) return
        end do
    end subroutine test_line_statistics_gap

    !> Own: three averages from /proc/loadavg, as integers (times 100)
    !> and as reals.
    subroutine test_load_average(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64), allocatable :: loads(:)
        real(real64), allocatable :: averages(:)

        call load_average(loads, st)
        call check(error, st%ok() .and. size(loads) == 3, format_error(st))
        if (allocated(error)) return
        call check(error, all(loads >= 0))
        if (allocated(error)) return
        call load_average(averages, st)
        call check(error, st%ok() .and. size(averages) == 3 .and. all(averages >= 0))
    end subroutine test_load_average

    !> Own: the C locale's data, and an item of another kind than the
    !> result (upstream's nil).
    subroutine test_locale_info(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text, names(:)
        integer, allocatable :: sizes(:)

        call locale_info(LOCALE_INFO_CODESET, text)
        call check(error, allocated(text), "codeset")
        if (allocated(error)) return
        call check(error, text == "ANSI_X3.4-1968", text)
        if (allocated(error)) return
        call locale_info(LOCALE_INFO_DAYS, names)
        call check(error, allocated(names), "days")
        if (allocated(error)) return
        call check(error, size(names) == 7, "7 days")
        if (allocated(error)) return
        call check(error, names(1) == "Sunday" .and. names(7) == "Saturday", "day names")
        if (allocated(error)) return
        call locale_info(LOCALE_INFO_MONTHS, names)
        call check(error, allocated(names), "months")
        if (allocated(error)) return
        call check(error, size(names) == 12, "12 months")
        if (allocated(error)) return
        call check(error, names(9) == "September", "month names")
        if (allocated(error)) return
        call locale_info(LOCALE_INFO_PAPER, sizes)
        call check(error, allocated(sizes), "paper")
        if (allocated(error)) return
        call check(error, all(sizes == [210, 297]), "paper size")
        if (allocated(error)) return
        call locale_info(LOCALE_INFO_PAPER, text)
        call check(error,.not. allocated(text), "paper as text")
    end subroutine test_locale_info

    !> test-secure-hash: the "foobar" digests of every algorithm.
    subroutine test_secure_hash(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: algorithms(10) = [SECURE_HASH_MD5, SECURE_HASH_SHA1, SECURE_HASH_SHA224, &
                                                SECURE_HASH_SHA256, SECURE_HASH_SHA384, SECURE_HASH_SHA512, &
                                                SECURE_HASH_SHA3_224, SECURE_HASH_SHA3_256, SECURE_HASH_SHA3_384, &
                                                SECURE_HASH_SHA3_512]
        character(len=128), parameter :: expected(10) = [character(len=128) :: &
                                                         "3858f62230ac3c915f300c664312c63f", &
                                                         "8843d7f92416211de9ebb963ff4ce28125932878", &
                                                         "de76c3e567fca9d246f5f8d3b2e704a38c3c5e258988ab525f941db8", &
                                                         "c3ab8ff13720e8ad9047dd39466b3c89"// &
                                                         "74e592c2fa383d4a3960714caef0c4f2", &
                                                         "3c9c30d9f665e74d515c842960d4a451c83a0125fd3de739"// &
                                                         "2d7b37231af10c72ea58aedfcdf89a5765bf902af93ecf06", &
                                                         "0a50261ebd1a390fed2bf326f2673c145582a6342d5"// &
                                                         "23204973d0219337f81616a8069b012587cf5635f69"// &
                                                         "25f1b56c360230c19b273500ee013e030601bf2425", &
                                                         "1ad852ba147a715fe5a3df39a741fad08186c303c7d21cefb7be763b", &
                                                         "09234807e4af85f17c66b48ee3bca89d"// &
                                                         "ffd1f1233659f9f940a2b17b0b8c6bc5", &
                                                         "0fa8abfbdaf924ad307b74dd2ed183b9a4a398891a2f6bac"// &
                                                         "8fd2db7041b77f068580f9c6c66f699b496c2da1cbcc7ed8", &
                                                         "ff32a30c3af5012ea395827a3e99a13073c3a8d8410"// &
                                                         "a708568ff7e6eb85968fccfebaea039bc21411e9d43"// &
                                                         "fdb9a851b529b9960ffea8679199781b8f45ca85e2"]
        character(len=:), allocatable :: hash
        type(status_type) :: st
        integer :: k

        call check(error, all(secure_hash_algorithms() == algorithms), "secure_hash_algorithms")
        if (allocated(error)) return

        do k = 1, size(algorithms)
            call secure_hash(algorithms(k), "foobar", hash, st)
            call check(error, st%ok() .and. hash == trim(expected(k)), hash)
            if (allocated(error)) return
        end do

        ! BINARY: the digest's bytes, half as long.
        call secure_hash(SECURE_HASH_MD5, "foobar", hash, st, binary=.true.)
        call check(error, len(hash) == 16, "binary length")
        if (allocated(error)) return
        call check(error, iachar(hash(1:1)) == 56 .and. iachar(hash(16:16)) == 63, "binary bytes")
        if (allocated(error)) return
        call secure_hash(11, "foobar", hash, st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. st%detail == "Invalid algorithm arg: 11", format_error(st))
        if (allocated(error)) return
        ! A call to getrandom returns the right format (the `iv-auto'
        ! OBJECT): not a test of randomness.
        call hash_random_bytes(SECURE_HASH_SHA512, 100_int64, hash, st)
        call check(error, st%ok() .and. len(hash) == 128 .and. verify(hash, "0123456789abcdef") == 0, hash)
    end subroutine test_secure_hash

    !> `hash_random_bytes' (secure-hash's `iv-auto' OBJECT): the digest
    !> length of each algorithm, BINARY, a length of 0, two calls that
    !> differ and a negative length.
    subroutine test_hash_random_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: algorithms(10) = [SECURE_HASH_MD5, SECURE_HASH_SHA1, SECURE_HASH_SHA224, &
                                                SECURE_HASH_SHA256, SECURE_HASH_SHA384, SECURE_HASH_SHA512, &
                                                SECURE_HASH_SHA3_224, SECURE_HASH_SHA3_256, SECURE_HASH_SHA3_384, &
                                                SECURE_HASH_SHA3_512]
        integer, parameter :: hexadecimal_lengths(10) = [32, 40, 56, 64, 96, 128, 56, 64, 96, 128]
        character(len=:), allocatable :: hash, other
        type(status_type) :: st
        integer :: k

        do k = 1, size(algorithms)
            call hash_random_bytes(algorithms(k), 100_int64, hash, st)
            call check(error, st%ok() .and. len(hash) == hexadecimal_lengths(k) .and. verify(hash, "0123456789abcdef") == 0, &
                       hash)
            if (allocated(error)) return
            call hash_random_bytes(algorithms(k), 100_int64, hash, st, binary=.true.)
            call check(error, st%ok() .and. 2*len(hash) == hexadecimal_lengths(k), "binary length")
            if (allocated(error)) return
        end do

        call hash_random_bytes(SECURE_HASH_SHA256, 64_int64, hash, st)
        call hash_random_bytes(SECURE_HASH_SHA256, 64_int64, other, st)
        call check(error, st%ok() .and. hash /= other, "two calls differ")
        if (allocated(error)) return
        ! No bytes: the digest of the empty string.
        call hash_random_bytes(SECURE_HASH_MD5, 0_int64, hash, st)
        call check(error, st%ok() .and. hash == "d41d8cd98f00b204e9800998ecf8427e", hash)
        if (allocated(error)) return
        call hash_random_bytes(SECURE_HASH_SHA256, -1_int64, hash, st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. &
                   st%detail == "Without a length, `hash_random_bytes' can't be used; see Info node "// &
                   "`(elisp)Format of GnuTLS Cryptography Inputs'", format_error(st))
        if (allocated(error)) return
        call hash_random_bytes(11, 10_int64, hash, st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. st%detail == "Invalid algorithm arg: 11", format_error(st))
    end subroutine test_hash_random_bytes

    !> fns-tests-hash-buffer: `buffer_hash' is the SHA-1 digest of the
    !> buffer's text, the gap in the middle or not.
    subroutine test_hash_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: hash, sha1
        type(handle_type) :: h, old
        type(status_type) :: st

        call secure_hash(SECURE_HASH_SHA1, "foo", sha1, st)
        call check(error, sha1 == "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33", sha1)
        if (allocated(error)) return
        call enter("tf-hash-buffer", h, old)
        call buffer_hash(hash, st)
        call check(error, hash == "da39a3ee5e6b4b0d3255bfef95601890afd80709", "empty: "//hash)

        if (.not. allocated(error)) then
            call insert("foo", st)
            call buffer_hash(hash, st)
            call check(error, hash == sha1, hash)
        end if

        if (.not. allocated(error)) then
            ! This tests whether the presence of a gap in the middle of the
            ! buffer is handled correctly.
            call goto_character(2_int64)
            call insert(" ", st)
            call delete_region(2_int64, 3_int64, st)
            call buffer_hash(hash, st, h)
            call check(error, hash == sha1, "gap: "//hash)
        end if

        call leave(h, old)
        if (allocated(error)) return
        call buffer_hash(hash, st, h)
        call check(error, st%detail == "Invalid buffer argument", format_error(st))
    end subroutine test_hash_buffer

    !> Own: BEGINNING and END index the bytes of the encoded text, from
    !> the end when negative; a range outside it is an error.
    subroutine test_secure_hash_ranges(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: hash, expected
        type(status_type) :: st

        call secure_hash(SECURE_HASH_SHA256, "abc", expected, st)
        call secure_hash(SECURE_HASH_SHA256, "xxabcx", hash, st, 2_int64, 5_int64)
        call check(error, st%ok() .and. hash == expected, "2..5")
        if (allocated(error)) return
        call secure_hash(SECURE_HASH_SHA256, "xxabc", hash, st, -3_int64)
        call check(error, st%ok() .and. hash == expected, "-3")
        if (allocated(error)) return
        ! "é" is two bytes in UTF-8: the range counts them.
        call secure_hash(SECURE_HASH_SHA256, "éabc", hash, st, 2_int64)
        call check(error, st%ok() .and. hash == expected, "after é")
        if (allocated(error)) return
        call secure_hash(SECURE_HASH_SHA256, "abc", hash, st, 2_int64, 1_int64)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE, "reversed")
        if (allocated(error)) return
        call secure_hash(SECURE_HASH_SHA256, "abc", hash, st, 0_int64, 4_int64)
        call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE, "past the end")
    end subroutine test_secure_hash_ranges

    !> Own: a buffer's text (the positions in either order, a range outside
    !> the accessible text an error) gives the digest of the same string.
    subroutine test_secure_hash_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: hash, expected
        type(handle_type) :: h, old
        type(status_type) :: st

        call secure_hash(SECURE_HASH_SHA512, "hello", expected, st)
        call enter("tf-secure-hash-buffer", h, old)
        call insert("hello world", st)
        call secure_hash(SECURE_HASH_SHA512, h, hash, st, 1_int64, 6_int64)
        call check(error, st%ok() .and. hash == expected, format_error(st))

        if (.not. allocated(error)) then
            call secure_hash(SECURE_HASH_SHA512, h, hash, st, 6_int64, 1_int64)
            call check(error, st%ok() .and. hash == expected, "reversed")
        end if

        if (.not. allocated(error)) then
            call secure_hash(SECURE_HASH_SHA512, "hello world", expected, st)
            call secure_hash(SECURE_HASH_SHA512, h, hash, st)
            call check(error, st%ok() .and. hash == expected, "whole buffer")
        end if

        if (.not. allocated(error)) then
            call secure_hash(SECURE_HASH_SHA512, h, hash, st, 1_int64, 13_int64)
            call check(error, st%code == ARGUMENTS_OUT_OF_RANGE_CODE, "past the end")
        end if

        call leave(h, old)
    end subroutine test_secure_hash_buffer

    !> Own: `md5' of strings and buffers (RFC 1321's "abc"), with a
    !> coding system, a unibyte string, an invalid coding system and
    !> NOERROR.
    subroutine test_md5(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: hash
        type(handle_type) :: h, old
        type(status_type) :: st

        call md5("abc", hash, st)
        call check(error, st%ok() .and. hash == "900150983cd24fb0d6963f7d28e17f72", hash)
        if (allocated(error)) return
        call md5("", hash, st, coding_system="utf-8")
        call check(error, st%ok() .and. hash == "d41d8cd98f00b204e9800998ecf8427e", hash)
        if (allocated(error)) return
        ! "é" as UTF-8 (c3 a9) and as Latin-1 (e9).
        call md5("é", hash, st, coding_system="utf-8")
        call check(error, st%ok() .and. hash == "66ddcd97cfdeabb2f6fb8a999b4bc76f", "utf-8: "//hash)
        if (allocated(error)) return
        call md5(achar(195)//achar(169), hash, st, multibyte=.false.)
        call check(error, st%ok() .and. hash == "66ddcd97cfdeabb2f6fb8a999b4bc76f", "unibyte: "//hash)
        if (allocated(error)) return
        call md5("é", hash, st, coding_system="iso-latin-1")
        call check(error, st%ok() .and. hash == "3406877694691ddd1dfb0aca54681407", "latin-1: "//hash)
        if (allocated(error)) return
        call md5(achar(233), hash, st, multibyte=.false.)
        call check(error, st%ok() .and. hash == "3406877694691ddd1dfb0aca54681407", "unibyte latin-1: "//hash)
        if (allocated(error)) return
        call md5("abc", hash, st, coding_system="no-such-coding")
        call check(error, st%code == CODING_SYSTEM_ERROR_CODE .and. st%detail == "no-such-coding", format_error(st))
        if (allocated(error)) return
        call md5("abc", hash, st, coding_system="no-such-coding", noerror=.true.)
        call check(error, st%ok() .and. hash == "900150983cd24fb0d6963f7d28e17f72", "noerror")
        if (allocated(error)) return
        call enter("tf-md5", h, old)
        call insert("xabc", st)
        call md5(h, hash, st, 2_int64)
        call check(error, st%ok() .and. hash == "900150983cd24fb0d6963f7d28e17f72", "buffer: "//hash)
        call leave(h, old)
    end subroutine test_md5
end module test_utility_functions_mod
