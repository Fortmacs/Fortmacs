! test_charset_mod.f90 -- tests for charset_mod.
! Ports GNU Emacs 31.1 test/src/charset-tests.el; the other expected
! values are those GNU Emacs gives.
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

module test_charset_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, FM_OK, FM_WRONG_TYPE_ARGUMENT, FM_GENERIC_ERROR
    use buffer_mod, only: get_or_make_buffer, set_buffer, erase_buffer, close_buffer
    use handle_mod, only: handle_type
    use insdel_mod, only: insert
    use charset_mod, only: CHARSET_NAME_LEN, is_charset, define_charset, decode_char, encode_char, make_char, &
                           split_char, char_charset, charset_list, charset_priority_list, &
                           charset_priority_highest, sort_charsets, iso_charset, get_unused_iso_final_char, &
                           map_charset_chars, find_charset_string, find_charset_region, charset_dimension, &
                           charset_chars, charset_iso_final_char, charset_description, charset_short_name, &
                           charset_long_name, charset_id_internal, charset_after, charset_attributes, charset_attributes_type, &
                           CHARSET_METHOD_SUPERSET
    implicit none(type, external)
    private

    public :: collect_charset_mod

    ! The ranges map_charset_chars reports.
    integer(int32) :: ranges(2, 64)
    integer :: range_count = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_charset_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [new_unittest("charset-decode-char", test_decode_char_error), &
                     new_unittest("charset-tests-define-charset", test_define_charset), &
                     new_unittest("charset-list", test_charset_list), &
                     new_unittest("priority-list", test_priority_list), &
                     new_unittest("decode-encode-maps", test_maps), &
                     new_unittest("subset-superset", test_subset_superset), &
                     new_unittest("unified-charsets", test_unified), &
                     new_unittest("offset-charsets", test_offset), &
                     new_unittest("split-char-char-charset", test_split_char), &
                     new_unittest("make-char", test_make_char), &
                     new_unittest("map-charset-chars", test_map_charset_chars), &
                     new_unittest("find-charset", test_find_charset), &
                     new_unittest("charset-attributes", test_attributes), &
                     new_unittest("iso-charset", test_iso_charset), &
                     new_unittest("deferred-maps", test_deferred)]
    end subroutine collect_charset_mod

    subroutine record_range(from, to)
        integer(int32), intent(in) :: from, to

        range_count = range_count + 1
        ranges(:, range_count) = [from, to]
    end subroutine record_range

    ! (should-error (decode-char 'ascii 0.5)): here a charset that is not
    ! one, as a code point is always an integer.
    subroutine test_decode_char_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: c

        c = decode_char("no-such-charset", 65_int64, status)
        call check(error, status%code == FM_WRONG_TYPE_ARGUMENT)
        if (allocated(error)) return
        call check(error, c == -1)
    end subroutine test_decode_char_error

    subroutine test_define_charset(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call define_charset("charset-tests-cs-1", "Only used for testing", status, short_name="CTCS1", &
                            long_name="Charset-Tests-Charset-1", code_space=[33, 126, 33, 126], &
                            code_offset=int(z'28083A'), unify_map="CNS-F")
        call check(error, status%code == FM_OK)
        if (allocated(error)) return
        call check(error, is_charset("charset-tests-cs-1"))
    end subroutine test_define_charset

    subroutine test_charset_list(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        integer :: n

        names = charset_list()
        n = size(names)
        ! 203 in GNU Emacs, here with the charset of the test above when it
        ! ran first.
        call check(error, n == 203 .or. n == 204)
        if (allocated(error)) return
        names = names(n - 7:n)
        call check(error, names(8) == "ascii" .and. names(7) == "iso-8859-1" .and. names(6) == "unicode" &
                   .and. names(5) == "emacs" .and. names(4) == "eight-bit" .and. names(3) == "ucs" &
                   .and. names(2) == "latin-iso8859-1" .and. names(1) == "control-1")
        if (allocated(error)) return
        call check(error, is_charset("ucs") .and. is_charset("cp1252") .and. .not. is_charset("cp1252x"))
    end subroutine test_charset_list

    subroutine test_priority_list(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        type(status_type) :: status
        integer :: n

        names = charset_priority_list()
        n = size(names)
        call check(error, n == 179 .or. n == 180)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "iso-8859-1" .and. names(3) == "unicode" &
                   .and. names(4) == "latin-iso8859-1" .and. names(5) == "control-1" &
                   .and. names(6) == "iso-8859-2")
        if (allocated(error)) return
        call check(error, names(n - 5) == "ethiopic" .and. names(n - 4) == "gb18030-2-byte" &
                   .and. names(n) == "gb18030-4-byte-ext-2")
        if (allocated(error)) return
        call check(error, charset_priority_highest() == "ascii")
        if (allocated(error)) return
        names = sort_charsets([character(len=CHARSET_NAME_LEN) :: "unicode", "iso-8859-2", "ascii"], status)
        call check(error, names(1) == "ascii" .and. names(2) == "unicode" .and. names(3) == "iso-8859-2")
    end subroutine test_priority_list

    subroutine test_maps(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_char("iso-8859-2", 161_int64, status) == 260)
        if (allocated(error)) return
        call check(error, encode_char(int(z'104'), "iso-8859-2", status) == 161)
        if (allocated(error)) return
        call check(error, decode_char("cp1252", int(z'80', int64), status) == 8364)
        if (allocated(error)) return
        call check(error, encode_char(int(z'20AC'), "windows-1252", status) == 128)
        if (allocated(error)) return
        call check(error, decode_char("cp437", int(z'80', int64), status) == 199)
        if (allocated(error)) return
        call check(error, decode_char("jisx0201", int(z'B1', int64), status) == 65393)
        if (allocated(error)) return
        call check(error, encode_char(int(z'20AC'), "iso-8859-2", status) == -1)
        if (allocated(error)) return
        ! ASCII in an ASCII-compatible charset.
        call check(error, decode_char("koi8-r", 65_int64, status) == 65)
        if (allocated(error)) return
        call check(error, decode_char("koi8-r", 256_int64, status) == -1)
    end subroutine test_maps

    subroutine test_subset_superset(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_char("katakana-jisx0201", int(z'31', int64), status) == 65393)
        if (allocated(error)) return
        call check(error, encode_char(int(z'FF71'), "katakana-jisx0201", status) == 49)
        if (allocated(error)) return
        call check(error, decode_char("tis620-2533", int(z'A1', int64), status) == 3585)
        if (allocated(error)) return
        call check(error, encode_char(int(z'E01'), "tis620-2533", status) == 161)
    end subroutine test_subset_superset

    subroutine test_unified(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_char("ipa", int(z'21', int64), status) == 618)
        if (allocated(error)) return
        call check(error, decode_char("vietnamese-viscii-lower", int(z'21', int64), status) == 7855)
        if (allocated(error)) return
        call check(error, encode_char(int(z'1EA1'), "vietnamese-viscii-lower", status) == 85)
        if (allocated(error)) return
        call check(error, make_char("ethiopic", status, 33, 33) == 4608)
    end subroutine test_unified

    subroutine test_offset(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_char("eight-bit", int(z'80', int64), status) == 4194176)
        if (allocated(error)) return
        call check(error, encode_char(int(z'3FFF80'), "eight-bit", status) == 128)
        if (allocated(error)) return
        call check(error, decode_char("mule-unicode-0100-24ff", int(z'2121', int64), status) == 353)
        if (allocated(error)) return
        call check(error, encode_char(int(z'100'), "mule-unicode-0100-24ff", status) == 8224)
        if (allocated(error)) return
        call check(error, decode_char("unicode-bmp", int(z'20AC', int64), status) == 8364)
    end subroutine test_offset

    subroutine test_split_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        character(len=:), allocatable :: charset
        integer, allocatable :: codes(:)

        ! An interactive GNU Emacs; in batch, iso-8859-1 233.
        call split_char(int(z'E9'), charset, codes, status)
        call check(error, charset == "unicode" .and. size(codes) == 3)
        if (allocated(error)) return
        call check(error, all(codes == [0, 0, 233]))
        if (allocated(error)) return
        call split_char(int(z'20AC'), charset, codes, status)
        call check(error, charset == "unicode" .and. size(codes) == 3)
        if (allocated(error)) return
        call check(error, all(codes == [0, 32, 172]))
        if (allocated(error)) return
        call split_char(int(z'3FFF80'), charset, codes, status)
        call check(error, charset == "eight-bit" .and. size(codes) == 1)
        if (allocated(error)) return
        call check(error, codes(1) == 128)
        if (allocated(error)) return
        call check(error, char_charset(int(z'20AC'), status) == "unicode")
        if (allocated(error)) return
        call check(error, char_charset(65, status) == "ascii")
        if (allocated(error)) return
        call check(error, char_charset(int(z'3FFF80'), status) == "eight-bit")
        if (allocated(error)) return
        call check(error, char_charset(int(z'104'), status, [character(len=CHARSET_NAME_LEN) :: "iso-8859-1", &
                                                             "iso-8859-2"]) == "iso-8859-2")
        if (allocated(error)) return
        call check(error, char_charset(int(z'20AC'), status, [character(len=CHARSET_NAME_LEN) :: "iso-8859-2"]) == "")
    end subroutine test_split_char

    subroutine test_make_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, make_char("latin-iso8859-2", status, int(z'21')) == 260)
        if (allocated(error)) return
        call check(error, make_char("ascii", status) == 0)
        if (allocated(error)) return
        call check(error, make_char("latin-iso8859-2", status, int(z'A1')) == 260)
        if (allocated(error)) return
        call check(error, make_char("latin-iso8859-2", status, 16) == -1)
        if (allocated(error)) return
        call check(error, status%code == FM_GENERIC_ERROR)
    end subroutine test_make_char

    subroutine test_map_charset_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        range_count = 0
        call map_charset_chars(record_range, "koi8-r", status)
        call check(error, range_count == 32)
        if (allocated(error)) return
        call check(error, all(ranges(:, 1) == [0, 127]) .and. all(ranges(:, 9) == [1040, 1103]) &
                   .and. all(ranges(:, 32) == [9632, 9632]))
        if (allocated(error)) return
        range_count = 0
        call map_charset_chars(record_range, "iso-8859-2", status, int(z'A0', int64), int(z'AF', int64))
        call check(error, range_count == 15)
        if (allocated(error)) return
        call check(error, all(ranges(:, 3) == [167, 168]) .and. all(ranges(:, 15) == [728, 728]))
        if (allocated(error)) return
        range_count = 0
        call map_charset_chars(record_range, "ipa", status)
        call check(error, range_count == 29)
        if (allocated(error)) return
        call check(error, all(ranges(:, 1) == [97, 98]) .and. all(ranges(:, 28) == [952, 952]) &
                   .and. all(ranges(:, 29) == [2097280, 2097375]))
    end subroutine test_map_charset_chars

    subroutine test_find_charset(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        type(handle_type) :: buffer
        type(status_type) :: status

        names = find_charset_string("")
        call check(error, size(names) == 0)
        if (allocated(error)) return
        names = find_charset_string("a"//achar(195)//achar(169)//achar(226)//achar(130)//achar(172))
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "unicode")
        if (allocated(error)) return
        names = find_charset_string("a"//achar(128), multibyte=.false.)
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "eight-bit")
        if (allocated(error)) return
        call get_or_make_buffer(" *charset-test*", buffer, status)
        call set_buffer(buffer, status)
        call erase_buffer(status)
        call insert("x"//achar(197)//achar(129)//"y", status)
        names = find_charset_region(1_int64, 4_int64, status)
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "unicode")
        if (allocated(error)) return
        call check(error, charset_after(2_int64) == "unicode" .and. charset_after(9_int64) == "")
        call close_buffer(buffer, status)
    end subroutine test_find_charset

    subroutine test_attributes(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        type(charset_attributes_type) :: attributes
        logical :: found

        call check(error, charset_dimension("japanese-jisx0208", status) == 2)
        if (allocated(error)) return
        call check(error, charset_chars("latin-iso8859-1", status) == 96)
        if (allocated(error)) return
        call check(error, charset_iso_final_char("latin-iso8859-2", status) == 66)
        if (allocated(error)) return
        call check(error, charset_description("ascii") == "ASCII (ISO646 IRV)")
        if (allocated(error)) return
        call check(error, charset_short_name("iso-8859-2") == "Latin-2" .and. charset_long_name("eight-bit") == "")
        if (allocated(error)) return
        call check(error, charset_id_internal("ascii", status) == 0 &
                   .and. charset_id_internal("eight-bit", status) == 4)
        if (allocated(error)) return
        call charset_attributes("cp932", attributes, found)
        call check(error, found .and. attributes%method == CHARSET_METHOD_SUPERSET .and. attributes%dimension == 2)
        if (allocated(error)) return
        call check(error, attributes%docstring == "CP932 (Microsoft shift-jis)")
    end subroutine test_attributes

    subroutine test_iso_charset(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, iso_charset(1, 96, iachar("A"), status) == "latin-iso8859-1")
        if (allocated(error)) return
        call check(error, get_unused_iso_final_char(1, 94, status) == 54)
    end subroutine test_iso_charset

    ! The maps of the CJK charsets are not built in yet.
    subroutine test_deferred(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: c

        c = decode_char("japanese-jisx0208", int(z'3021', int64), status)
        call check(error, status%code == FM_GENERIC_ERROR)
        if (allocated(error)) return
        call check(error, index(status%detail, "not ported yet") > 0)
    end subroutine test_deferred
end module test_charset_mod
