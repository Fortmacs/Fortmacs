! test_character_set_mod.f90 -- tests for character_set_mod.
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

module test_character_set_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, OK_CODE, WRONG_TYPE_ARGUMENT_CODE, GENERIC_ERROR_CODE
    use buffer_mod, only: get_or_make_buffer, set_buffer, erase_buffer, close_buffer
    use handle_mod, only: handle_type
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_character
    use character_set_mod, only: CHARACTER_SET_NAME_LENGTH, is_character_set, define_character_set, decode_character, &
                                 encode_character, make_character, split_character, character_character_set, character_set_list, &
                                 character_set_priority_list, character_set_priority_highest, sort_character_sets, &
                                 iso_character_set, get_unused_iso_final_character, map_character_set_characters, &
                                 find_character_set_string, find_character_set_region, character_set_dimension, &
                                 character_set_characters, character_set_iso_final_character, character_set_description, &
                                 character_set_short_name, character_set_long_name, character_set_id_internal, &
                                 character_set_after, character_set_attributes, character_set_attributes_type, &
                                 CHARACTER_SET_METHOD_SUPERSET
    implicit none(type, external)
    private

    public :: collect_character_set_mod

    ! The ranges map_character_set_characters reports.
    integer(int32) :: ranges(2, 64)
    integer :: range_count = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_character_set_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [new_unittest("charset-decode-char", test_decode_character_error), &
                     new_unittest("charset-tests-define-charset", test_define_character_set), &
                     new_unittest("charset-list", test_character_set_list), &
                     new_unittest("priority-list", test_priority_list), &
                     new_unittest("decode-encode-maps", test_maps), &
                     new_unittest("subset-superset", test_subset_superset), &
                     new_unittest("unified-charsets", test_unified), &
                     new_unittest("offset-charsets", test_offset), &
                     new_unittest("split-char-char-charset", test_split_character), &
                     new_unittest("make-char", test_make_character), &
                     new_unittest("map-charset-chars", test_map_character_set_characters), &
                     new_unittest("find-charset", test_find_character_set), &
                     new_unittest("charset-attributes", test_attributes), &
                     new_unittest("iso-charset", test_iso_character_set), &
                     new_unittest("cjk-charsets", test_cjk)]
    end subroutine collect_character_set_mod

    subroutine record_range(from, to)
        integer(int32), intent(in) :: from, to

        range_count = range_count + 1
        ranges(:, range_count) = [from, to]
    end subroutine record_range

    ! (should-error (decode-char 'ascii 0.5)): here a charset that is not
    ! one, as a code point is always an integer.
    subroutine test_decode_character_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        integer :: c

        c = decode_character("no-such-charset", 65_int64, status)
        call check(error, status%code == WRONG_TYPE_ARGUMENT_CODE)
        if (allocated(error)) return
        call check(error, c == -1)
    end subroutine test_decode_character_error

    subroutine test_define_character_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call define_character_set("charset-tests-cs-1", "Only used for testing", status, short_name="CTCS1", &
                                  long_name="Charset-Tests-Charset-1", code_space=[33, 126, 33, 126], &
                                  code_offset=int(z'28083A'), unify_map="CNS-F")
        call check(error, status%code == OK_CODE)
        if (allocated(error)) return
        call check(error, is_character_set("charset-tests-cs-1"))
    end subroutine test_define_character_set

    subroutine test_character_set_list(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=CHARACTER_SET_NAME_LENGTH), allocatable :: names(:)
        integer :: n

        names = character_set_list()
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
        call check(error, is_character_set("ucs") .and. is_character_set("cp1252") .and. .not. is_character_set("cp1252x"))
    end subroutine test_character_set_list

    subroutine test_priority_list(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=CHARACTER_SET_NAME_LENGTH), allocatable :: names(:)
        type(status_type) :: status
        integer :: n

        names = character_set_priority_list()
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
        call check(error, character_set_priority_highest() == "ascii")
        if (allocated(error)) return
        names = sort_character_sets([character(len=CHARACTER_SET_NAME_LENGTH) :: "unicode", "iso-8859-2", "ascii"], status)
        call check(error, names(1) == "ascii" .and. names(2) == "unicode" .and. names(3) == "iso-8859-2")
    end subroutine test_priority_list

    subroutine test_maps(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_character("iso-8859-2", 161_int64, status) == 260)
        if (allocated(error)) return
        call check(error, encode_character(int(z'104'), "iso-8859-2", status) == 161)
        if (allocated(error)) return
        call check(error, decode_character("cp1252", int(z'80', int64), status) == 8364)
        if (allocated(error)) return
        call check(error, encode_character(int(z'20AC'), "windows-1252", status) == 128)
        if (allocated(error)) return
        call check(error, decode_character("cp437", int(z'80', int64), status) == 199)
        if (allocated(error)) return
        call check(error, decode_character("jisx0201", int(z'B1', int64), status) == 65393)
        if (allocated(error)) return
        call check(error, encode_character(int(z'20AC'), "iso-8859-2", status) == -1)
        if (allocated(error)) return
        ! ASCII in an ASCII-compatible charset.
        call check(error, decode_character("koi8-r", 65_int64, status) == 65)
        if (allocated(error)) return
        call check(error, decode_character("koi8-r", 256_int64, status) == -1)
    end subroutine test_maps

    subroutine test_subset_superset(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_character("katakana-jisx0201", int(z'31', int64), status) == 65393)
        if (allocated(error)) return
        call check(error, encode_character(int(z'FF71'), "katakana-jisx0201", status) == 49)
        if (allocated(error)) return
        call check(error, decode_character("tis620-2533", int(z'A1', int64), status) == 3585)
        if (allocated(error)) return
        call check(error, encode_character(int(z'E01'), "tis620-2533", status) == 161)
    end subroutine test_subset_superset

    subroutine test_unified(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_character("ipa", int(z'21', int64), status) == 618)
        if (allocated(error)) return
        call check(error, decode_character("vietnamese-viscii-lower", int(z'21', int64), status) == 7855)
        if (allocated(error)) return
        call check(error, encode_character(int(z'1EA1'), "vietnamese-viscii-lower", status) == 85)
        if (allocated(error)) return
        call check(error, make_character("ethiopic", status, 33, 33) == 4608)
    end subroutine test_unified

    subroutine test_offset(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_character("eight-bit", int(z'80', int64), status) == 4194176)
        if (allocated(error)) return
        call check(error, encode_character(int(z'3FFF80'), "eight-bit", status) == 128)
        if (allocated(error)) return
        call check(error, decode_character("mule-unicode-0100-24ff", int(z'2121', int64), status) == 353)
        if (allocated(error)) return
        call check(error, encode_character(int(z'100'), "mule-unicode-0100-24ff", status) == 8224)
        if (allocated(error)) return
        call check(error, decode_character("unicode-bmp", int(z'20AC', int64), status) == 8364)
    end subroutine test_offset

    subroutine test_split_character(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        character(len=:), allocatable :: character_set
        integer, allocatable :: codes(:)

        ! An interactive GNU Emacs; in batch, iso-8859-1 233.
        call split_character(int(z'E9'), character_set, codes, status)
        call check(error, character_set == "unicode" .and. size(codes) == 3)
        if (allocated(error)) return
        call check(error, all(codes == [0, 0, 233]))
        if (allocated(error)) return
        call split_character(int(z'20AC'), character_set, codes, status)
        call check(error, character_set == "unicode" .and. size(codes) == 3)
        if (allocated(error)) return
        call check(error, all(codes == [0, 32, 172]))
        if (allocated(error)) return
        call split_character(int(z'3FFF80'), character_set, codes, status)
        call check(error, character_set == "eight-bit" .and. size(codes) == 1)
        if (allocated(error)) return
        call check(error, codes(1) == 128)
        if (allocated(error)) return
        call check(error, character_character_set(int(z'20AC'), status) == "unicode")
        if (allocated(error)) return
        call check(error, character_character_set(65, status) == "ascii")
        if (allocated(error)) return
        call check(error, character_character_set(int(z'3FFF80'), status) == "eight-bit")
        if (allocated(error)) return
        call check(error, character_character_set(int(z'104'), status, [character(len=CHARACTER_SET_NAME_LENGTH) :: "iso-8859-1", &
                                                                        "iso-8859-2"]) == "iso-8859-2")
        if (allocated(error)) return
        call check(error, character_character_set(int(z'20AC'), status, &
                                                  [character(len=CHARACTER_SET_NAME_LENGTH) :: "iso-8859-2"]) == "")
    end subroutine test_split_character

    subroutine test_make_character(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, make_character("latin-iso8859-2", status, int(z'21')) == 260)
        if (allocated(error)) return
        call check(error, make_character("ascii", status) == 0)
        if (allocated(error)) return
        call check(error, make_character("latin-iso8859-2", status, int(z'A1')) == 260)
        if (allocated(error)) return
        call check(error, make_character("latin-iso8859-2", status, 16) == -1)
        if (allocated(error)) return
        call check(error, status%code == GENERIC_ERROR_CODE)
    end subroutine test_make_character

    subroutine test_map_character_set_characters(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        range_count = 0
        call map_character_set_characters(record_range, "koi8-r", status)
        call check(error, range_count == 32)
        if (allocated(error)) return
        call check(error, all(ranges(:, 1) == [0, 127]) .and. all(ranges(:, 9) == [1040, 1103]) &
                   .and. all(ranges(:, 32) == [9632, 9632]))
        if (allocated(error)) return
        range_count = 0
        call map_character_set_characters(record_range, "iso-8859-2", status, int(z'A0', int64), int(z'AF', int64))
        call check(error, range_count == 15)
        if (allocated(error)) return
        call check(error, all(ranges(:, 3) == [167, 168]) .and. all(ranges(:, 15) == [728, 728]))
        if (allocated(error)) return
        range_count = 0
        call map_character_set_characters(record_range, "ipa", status)
        call check(error, range_count == 29)
        if (allocated(error)) return
        call check(error, all(ranges(:, 1) == [97, 98]) .and. all(ranges(:, 28) == [952, 952]) &
                   .and. all(ranges(:, 29) == [2097280, 2097375]))
    end subroutine test_map_character_set_characters

    subroutine test_find_character_set(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=CHARACTER_SET_NAME_LENGTH), allocatable :: names(:)
        type(handle_type) :: buffer
        type(status_type) :: status

        names = find_character_set_string("")
        call check(error, size(names) == 0)
        if (allocated(error)) return
        names = find_character_set_string("a"//achar(195)//achar(169)//achar(226)//achar(130)//achar(172))
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "unicode")
        if (allocated(error)) return
        names = find_character_set_string("a"//achar(128), multibyte=.false.)
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "eight-bit")
        if (allocated(error)) return
        call get_or_make_buffer(" *charset-test*", buffer, status)
        call set_buffer(buffer, status)
        call erase_buffer(status)
        call insert("x"//achar(197)//achar(129)//"y", status)
        names = find_character_set_region(1_int64, 4_int64, status)
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1) == "ascii" .and. names(2) == "unicode")
        if (allocated(error)) return
        call check(error, character_set_after(2_int64) == "unicode" .and. character_set_after(9_int64) == "")
        if (allocated(error)) return
        ! POSITION defaults to point.
        call goto_character(3_int64)
        call check(error, character_set_after() == "ascii" .and. character_set_after(1_int64) == "ascii")
        if (allocated(error)) return
        call goto_character(4_int64)
        call check(error, character_set_after() == "")
        call close_buffer(buffer, status)
    end subroutine test_find_character_set

    subroutine test_attributes(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status
        type(character_set_attributes_type) :: attributes
        logical :: found

        call check(error, character_set_dimension("japanese-jisx0208", status) == 2)
        if (allocated(error)) return
        call check(error, character_set_characters("latin-iso8859-1", status) == 96)
        if (allocated(error)) return
        call check(error, character_set_iso_final_character("latin-iso8859-2", status) == 66)
        if (allocated(error)) return
        call check(error, character_set_description("ascii") == "ASCII (ISO646 IRV)")
        if (allocated(error)) return
        call check(error, character_set_short_name("iso-8859-2") == "Latin-2" .and. character_set_long_name("eight-bit") == "")
        if (allocated(error)) return
        call check(error, character_set_id_internal("ascii", status) == 0 &
                   .and. character_set_id_internal("eight-bit", status) == 4)
        if (allocated(error)) return
        call character_set_attributes("cp932", attributes, found)
        call check(error, found .and. attributes%method == CHARACTER_SET_METHOD_SUPERSET .and. attributes%dimension == 2)
        if (allocated(error)) return
        call check(error, attributes%docstring == "CP932 (Microsoft shift-jis)")
    end subroutine test_attributes

    subroutine test_iso_character_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, iso_character_set(1, 96, iachar("A"), status) == "latin-iso8859-1")
        if (allocated(error)) return
        call check(error, get_unused_iso_final_character(1, 94, status) == 54)
    end subroutine test_iso_character_set

    ! The Chinese, Japanese and Korean charsets.
    subroutine test_cjk(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: status

        call check(error, decode_character("japanese-jisx0208", int(z'3021', int64), status) == 20124)
        if (allocated(error)) return
        call check(error, encode_character(int(z'4E9C'), "japanese-jisx0208", status) == 12321)
        if (allocated(error)) return
        call check(error, decode_character("chinese-gb2312", int(z'3021', int64), status) == 21834)
        if (allocated(error)) return
        call check(error, decode_character("big5", int(z'A440', int64), status) == 19968)
        if (allocated(error)) return
        call check(error, decode_character("korean-ksc5601", int(z'3021', int64), status) == 44032)
        if (allocated(error)) return
        call check(error, decode_character("cp932-2-byte", int(z'889F', int64), status) == 20124)
        if (allocated(error)) return
        call check(error, decode_character("gb18030", 2167439664_int64, status) == 128)
        if (allocated(error)) return
        call check(error, encode_character(128, "gb18030", status) == 2167439664_int64)
        if (allocated(error)) return
        call check(error, decode_character("cp949", int(z'B0A1', int64), status) == 44032)
        if (allocated(error)) return
        call check(error, decode_character("chinese-cns11643-1", int(z'4421', int64), status) == 19968)
        if (allocated(error)) return
        call check(error, decode_character("japanese-jisx0213-1", int(z'2121', int64), status) == 12288)
        if (allocated(error)) return
        call check(error, decode_character("chinese-big5-1", int(z'2121', int64), status) == 12288)
        if (allocated(error)) return
        call check(error, status%code == OK_CODE)
    end subroutine test_cjk
end module test_character_set_mod
