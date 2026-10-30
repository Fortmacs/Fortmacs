! test_coding_fm.f90 -- tests for coding_fm and mule_fm.
! Ports test/src/coding-tests.el and test/lisp/international/mule-tests.el
! where the ported coding systems allow.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> The coding-tests.el cases (ert-test-unibyte-buffer-dos-eol-decode
!> needs euc-jp and the prefer-utf-8 case with sjis needs Shift-JIS, both
!> Deferred; coding-nocopy-trivial tests `eq', which Fortran strings have
!> not), find-auto-coding--bug27391 from mule-tests.el, and own tests:
!> detection, the coding system functions, visiting and saving files
!> with other coding systems and line ends, the mode line, the commands.
module test_coding_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, FM_CODING_SYSTEM_ERROR, condition_matches
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, buffers, cur, current_buffer, &
                         resolve_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, buffer_string, point_min, point_max
    use character_fm, only: str_to_multibyte
    use bindings_fm, only: init_bindings
    use fileio_fm, only: insert_file_contents, write_region, make_directory_internal, delete_file_internal, &
                         delete_directory_internal, emacs_wd, init_fileio, file_name_as_directory, read_file
    use files_fm, only: find_file_noselect, save_buffer, write_file
    use xdisp_fm, only: mode_line_mule_info
    use coding_fm, only: coding_system_p, check_coding_system, coding_system_base, coding_system_eol_type, &
                         coding_system_equal, coding_system_aliases, coding_system_priority_list, &
                         set_coding_system_priority, coding_system_change_eol_conversion, &
                         coding_system_change_text_conversion, coding_system_type, coding_system_mnemonic, &
                         coding_system_list_entry_type, unencodable_type, detect_coding_string, decode_coding_string, &
                         encode_coding_string, check_coding_systems_text, coding_system_for_read, &
                         coding_system_for_write, last_coding_system_used, buffer_file_coding_system, &
                         inhibit_eol_conversion, EOL_UNIX, EOL_DOS, EOL_MAC, EOL_UNDECIDED
    use mule_fm, only: find_auto_coding, set_buffer_file_coding_system, prefer_coding_system, describe_coding_system, &
                       set_buffer_process_coding_system, set_terminal_coding_system, set_keyboard_coding_system
    use mule_fm, only: get_language_info, set_language_environment, describe_language_environment, language_info_type, &
                       current_language_environment, default_input_method, set_locale_environment, locale_name_match, &
                       locale_language_names, locale_charset_to_coding_system
    use coding_fm, only: terminal_coding_system, keyboard_coding_system, eol_mnemonic_dos, eol_mnemonic_mac
    use term_fm, only: encode_terminal_code
    use dispextern_fm, only: glyph_type
    use lisp_fm, only: value_type, xfixnum
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, read_event
    implicit none(type, external)
    private

    public :: collect_coding_fm

    character(len=*), parameter :: LF = achar(10), CR = achar(13)
    character(len=*), parameter :: ASCII = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"//LF
    character(len=*), parameter :: LATIN = ASCII//"ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ"//LF

contains

    subroutine collect_coding_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ert-test-coding-bogus-coding-systems", test_bogus), &
                    new_unittest("ert-test-coding-ascii", test_ascii), &
                    new_unittest("ert-test-coding-latin", test_latin), &
                    new_unittest("ert-test-coding-binary", test_binary), &
                    new_unittest("ert-test-coding-prefer-utf-8", test_prefer_utf_8), &
                    new_unittest("coding-nocopy-ascii", test_nocopy_ascii), &
                    new_unittest("coding-check-coding-systems-region", test_check_region), &
                    new_unittest("find-auto-coding--bug27391", test_bug27391), &
                    new_unittest("detect-coding-string", test_detect), &
                    new_unittest("coding-system-functions", test_functions), &
                    new_unittest("find-auto-coding-tags", test_auto_coding), &
                    new_unittest("visit-and-save", test_visit_save), &
                    new_unittest("mode-line-mule-info", test_mode_line), &
                    new_unittest("prefer-coding-system", test_prefer), &
                    new_unittest("describe-coding-system", test_describe), &
                    new_unittest("universal-coding-system-argument", test_universal_coding_system_argument), &
                    new_unittest("terminal-keyboard-coding", test_terminal_keyboard_coding), &
                    new_unittest("charset-coding-systems", test_charset_coding), &
                    new_unittest("shift-jis-big5", test_sjis_big5), &
                    new_unittest("iso-2022", test_iso_2022), &
                    new_unittest("emacs-mule", test_emacs_mule), &
                    new_unittest("language-environments", test_language_environments), &
                    new_unittest("set-locale-environment", test_set_locale_environment) &
                    ]
    end subroutine collect_coding_fm

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The work directory (coding-tests-workdir), as a directory name.
    function tdir() result(d)
        character(len=:), allocatable :: d

        d = file_name_as_directory(emacs_wd)//"build/test-coding-workdir/"
    end function tdir

    subroutine setup()
        type(status_type) :: st

        call init_fileio()
        call init_bindings()
        call make_directory_internal(file_name_as_directory(emacs_wd)//"build", st)
        call make_directory_internal(tdir(), st)
    end subroutine setup

    !> Remove all generated test files (coding-tests-remove-files).
    subroutine remove_files(names)
        character(len=*), intent(in) :: names(:)
        type(status_type) :: st
        integer :: k

        do k = 1, size(names)
            call delete_file_internal(tdir()//trim(names(k)), st)
            ! Saving made backups of some.
            call delete_file_internal(tdir()//trim(names(k))//"~", st)
        end do

        call delete_directory_internal(tdir(), st)
    end subroutine remove_files

    !> A fresh temporary buffer, made current (with-temp-buffer).
    subroutine temp_buffer(buf)
        type(handle_type), intent(out) :: buf
        type(status_type) :: st

        call get_buffer_create(" *coding-test*", buf, st)
        call set_buffer(buf, st)
        call kill_buffer(buf, st)
        call get_buffer_create(" *coding-test*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
    end subroutine temp_buffer

    !> Generate FILE with CONTENTS encoded by CODING-SYSTEM
    !> (coding-tests-gen-file: with-temp-file and
    !> set-buffer-file-coding-system).
    subroutine gen_file(file, contents, coding)
        character(len=*), intent(in) :: file, contents, coding
        type(handle_type) :: buf
        type(status_type) :: st

        call temp_buffer(buf)
        call set_buffer_file_coding_system(coding, .false., .false., .false., st)
        call insert(contents, st)
        call write_region(tdir()//file, .false., st)
        call kill_buffer(buf, st)
    end subroutine gen_file

    !> The contents (coding-tests-file-contents) of CONTENT_TYPE: ascii,
    !> latin or binary.
    function contents_of(content_type) result(s)
        character(len=*), intent(in) :: content_type
        character(len=:), allocatable :: s

        select case (content_type)
        case ("ascii")
            s = ASCII
        case ("latin")
            s = LATIN
        case default
            ! (string-to-multibyte (concat (string-as-unibyte latin)
            ! (unibyte-string #xC0 #xC1 ?\n)))
            s = str_to_multibyte(LATIN//char(192)//char(193)//LF)
        end select
    end function contents_of

    !> S translated: all LFs to CR LF ("crlf"), to CR ("cr"), to LF LF
    !> ("lflf"), or with the UTF-8 BOM prepended ("bom").
    function translate(s, how) result(t)
        character(len=*), intent(in) :: s, how
        character(len=:), allocatable :: t
        integer :: k

        if (how == "bom") then
            t = char(int(z'EF'))//char(int(z'BB'))//char(int(z'BF'))//s
            return
        end if

        t = ""

        do k = 1, len(s)
            if (s(k:k) /= LF) then
                t = t//s(k:k)
            else if (how == "crlf") then
                t = t//CR//LF
            else if (how == "cr") then
                t = t//CR
            else
                t = t//LF//LF
            end if
        end do
    end function translate

    !> The coding priority list, to restore after `with_priority'.
    subroutine save_priority(saved)
        type(coding_system_list_entry_type), allocatable, intent(out) :: saved(:)

        call coding_system_priority_list(saved)
    end subroutine save_priority

    subroutine restore_priority(saved)
        type(coding_system_list_entry_type), intent(in) :: saved(:)
        character(len=40), allocatable :: names(:)
        type(status_type) :: st
        integer :: k

        allocate (names(size(saved)))

        do k = 1, size(saved)
            names(k) = saved(k)%name
        end do

        call set_coding_system_priority(names, st)
    end subroutine restore_priority

    !> Test the decoding of a file whose contents and encoding are
    !> specified by CONTENT_TYPE and WRITE_CODING: it passes if the file is
    !> read by READ_CODING and detected as DETECTED_CODING and the contents
    !> is correctly decoded (TRANSLATOR, if given, translates the original
    !> contents for comparison).  "" if it passes, else what went wrong
    !> (coding-tests, with utf-8-auto first in the priorities).
    function coding_tests(content_type, write_coding, read_coding, detected, translator) result(err)
        character(len=*), intent(in) :: content_type, write_coding, read_coding, detected
        character(len=*), intent(in), optional :: translator
        character(len=:), allocatable :: err, contents, text
        type(coding_system_list_entry_type), allocatable :: saved(:)
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: inserted

        call save_priority(saved)
        call set_coding_system_priority(["utf-8-auto"], st)
        contents = contents_of(content_type)
        if (present(translator)) contents = translate(contents, translator)
        call temp_buffer(buf)
        coding_system_for_read = read_coding
        call insert_file_contents(tdir()//content_type//"-"//write_coding, .false., inserted, st)
        call coding_system_for_read%clear()
        text = buffer_string()
        err = ""

        if (.not. st%ok()) then
            err = format_error(st)
        else if (.not. coding_system_equal(buffer_file_coding_system%value, detected)) then
            err = content_type//"-"//write_coding//" read by "//read_coding//": "//buffer_file_coding_system%value
        else if (text /= contents) then
            err = content_type//"-"//write_coding//" read by "//read_coding//": wrong text "//text
        end if

        call kill_buffer(buf, st)
        call restore_priority(saved)
    end function coding_tests

    ! ------------------------------------------------------------------
    ! coding-tests.el
    ! ------------------------------------------------------------------

    subroutine test_bogus(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: inserted

        call setup()
        call temp_buffer(buf)
        call delete_file_internal(tdir()//"nonexistent", st)
        coding_system_for_read = "bogus"
        call insert_file_contents(tdir()//"nonexistent", .false., inserted, st)
        call coding_system_for_read%clear()
        call check(error,.not. st%ok())
        if (allocated(error)) return
        ! See bug #21602.
        call insert("some text", st)
        coding_system_for_write = """us-ascii"""
        call write_region(tdir()//"writing", .false., st)
        call coding_system_for_write%clear()
        call check(error, st%code == FM_CODING_SYSTEM_ERROR, format_error(st))
        call kill_buffer(buf, st)
        call remove_files([character(len=16) :: "writing"])
    end subroutine test_bogus

    subroutine test_ascii(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call gen_file("ascii-unix", ASCII, "unix")
        call gen_file("ascii-dos", ASCII, "dos")
        call gen_file("ascii-mac", ASCII, "mac")
        call expect(coding_tests("ascii", "unix", "undecided", "unix"))
        call expect(coding_tests("ascii", "dos", "undecided", "dos"))
        call expect(coding_tests("ascii", "dos", "dos", "dos"))
        call expect(coding_tests("ascii", "mac", "undecided", "mac"))
        call expect(coding_tests("ascii", "mac", "mac", "mac"))
        call expect(coding_tests("ascii", "dos", "utf-8", "utf-8-dos"))
        call expect(coding_tests("ascii", "dos", "unix", "unix", "crlf"))
        call expect(coding_tests("ascii", "mac", "dos", "dos", "cr"))
        call expect(coding_tests("ascii", "dos", "mac", "mac", "lflf"))
        call remove_files([character(len=16) :: "ascii-unix", "ascii-dos", "ascii-mac"])

    contains

        subroutine expect(err)
            character(len=*), intent(in) :: err

            if (allocated(error)) return
            call check(error, len(err) == 0, err)
        end subroutine expect
    end subroutine test_ascii

    subroutine test_latin(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: CODINGS(2) = [character(len=20) :: "utf-8", "utf-8-with-signature"]
        character(len=*), parameter :: EOLS(3) = [character(len=4) :: "unix", "dos", "mac"]
        character(len=40) :: names(6)
        integer :: i, j

        call setup()

        do i = 1, 2
            do j = 1, 3
                names((i - 1)*3 + j) = "latin-"//trim(CODINGS(i))//"-"//trim(EOLS(j))
                call gen_file(trim(names((i - 1)*3 + j)), LATIN, trim(CODINGS(i))//"-"//trim(EOLS(j)))
            end do
        end do

        call expect(coding_tests("latin", "utf-8-unix", "undecided", "utf-8-unix"))
        call expect(coding_tests("latin", "utf-8-unix", "utf-8-unix", "utf-8-unix"))
        call expect(coding_tests("latin", "utf-8-dos", "undecided", "utf-8-dos"))
        call expect(coding_tests("latin", "utf-8-dos", "utf-8-dos", "utf-8-dos"))
        call expect(coding_tests("latin", "utf-8-mac", "undecided", "utf-8-mac"))
        call expect(coding_tests("latin", "utf-8-mac", "utf-8-mac", "utf-8-mac"))
        call expect(coding_tests("latin", "utf-8-dos", "unix", "utf-8-unix", "crlf"))
        call expect(coding_tests("latin", "utf-8-mac", "dos", "utf-8-dos", "cr"))
        call expect(coding_tests("latin", "utf-8-dos", "mac", "utf-8-mac", "lflf"))
        call expect(coding_tests("latin", "utf-8-with-signature-unix", "undecided", "utf-8-with-signature-unix"))
        call expect(coding_tests("latin", "utf-8-with-signature-unix", "utf-8-auto", "utf-8-with-signature-unix"))
        call expect(coding_tests("latin", "utf-8-with-signature-dos", "undecided", "utf-8-with-signature-dos"))
        call expect(coding_tests("latin", "utf-8-with-signature-unix", "utf-8", "utf-8-unix", "bom"))
        call remove_files(names)

    contains

        subroutine expect(err)
            character(len=*), intent(in) :: err

            if (allocated(error)) return
            call check(error, len(err) == 0, err)
        end subroutine expect
    end subroutine test_latin

    subroutine test_binary(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call gen_file("binary-raw-text-unix", contents_of("binary"), "raw-text-unix")
        call gen_file("binary-raw-text-dos", contents_of("binary"), "raw-text-dos")
        call gen_file("binary-raw-text-mac", contents_of("binary"), "raw-text-mac")
        call expect(coding_tests("binary", "raw-text-unix", "undecided", "raw-text-unix"))
        call expect(coding_tests("binary", "raw-text-dos", "undecided", "raw-text-dos"))
        call expect(coding_tests("binary", "raw-text-mac", "undecided", "raw-text-mac"))
        call expect(coding_tests("binary", "raw-text-dos", "unix", "raw-text-unix", "crlf"))
        call expect(coding_tests("binary", "raw-text-mac", "dos", "raw-text-dos", "cr"))
        call expect(coding_tests("binary", "raw-text-dos", "mac", "raw-text-mac", "lflf"))
        call remove_files([character(len=24) :: "binary-raw-text-unix", "binary-raw-text-dos", "binary-raw-text-mac"])

    contains

        subroutine expect(err)
            character(len=*), intent(in) :: err

            if (allocated(error)) return
            call check(error, len(err) == 0, err)
        end subroutine expect
    end subroutine test_binary

    !> Read FILE (with PREFER first in the priorities, if given): "" if
    !> it was detected as DETECT (coding-tests-prefer-utf-8-read).
    function prefer_read(file, detect, prefer) result(err)
        character(len=*), intent(in) :: file, detect
        character(len=*), intent(in), optional :: prefer
        character(len=:), allocatable :: err
        type(coding_system_list_entry_type), allocatable :: saved(:)
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: inserted

        call save_priority(saved)
        if (present(prefer)) call set_coding_system_priority([character(len=len(prefer)) :: prefer], st)
        call temp_buffer(buf)
        call insert_file_contents(tdir()//file, .false., inserted, st)
        call restore_priority(saved)
        err = ""
        if (buffer_file_coding_system%value /= detect) err = "Invalid detection: "//buffer_file_coding_system%value
        call kill_buffer(buf, st)
    end function prefer_read

    !> Read FILE, modify it, and write it: "" if the coding system used for
    !> writing was CODING.  If CODING_TAG is not empty, insert a coding tag
    !> with it before writing; if STR is given, insert it before writing
    !> (coding-tests-prefer-utf-8-write).
    function prefer_write(file, coding_tag, coding, str) result(err)
        character(len=*), intent(in) :: file, coding_tag, coding
        character(len=*), intent(in), optional :: str
        character(len=:), allocatable :: err
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: inserted

        call temp_buffer(buf)
        call insert_file_contents(tdir()//file, .false., inserted, st)
        call goto_char(point_min())

        if (len(coding_tag) > 0) then
            call insert(";; -*- coding: "//coding_tag//"; -*-"//LF, st)
        else
            call insert(";;"//LF, st)
        end if

        if (present(str)) call insert(str, st)
        call write_file(tdir()//"test-test.el", .false., st)
        err = ""

        if (.not. st%ok()) then
            err = format_error(st)
        else if (.not. coding_system_equal(buffer_file_coding_system%value, coding)) then
            err = "Incorrect encoding: "//last_coding_system_used%value
        end if

        call kill_buffer(current_buffer(), st)
    end function prefer_write

    subroutine test_prefer_utf_8(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call gen_file("ascii.el", contents_of("ascii"), "unix")
        call gen_file("utf-8.el", contents_of("latin"), "utf-8-unix")
        call expect(prefer_read("ascii.el", "prefer-utf-8-unix"))
        call expect(prefer_read("utf-8.el", "utf-8-unix"))
        call expect(prefer_read("utf-8.el", "utf-8-unix", "iso-8859-1"))
        call expect(prefer_write("ascii.el", "", "prefer-utf-8-unix"))
        call expect(prefer_write("ascii.el", "iso-8859-1", "iso-8859-1-unix"))
        call expect(prefer_write("ascii.el", "", "utf-8-unix", "À"))
        call remove_files([character(len=16) :: "ascii.el", "utf-8.el", "test-test.el"])

    contains

        subroutine expect(err)
            character(len=*), intent(in) :: err

            if (allocated(error)) return
            call check(error, len(err) == 0, err)
        end subroutine expect
    end subroutine test_prefer_utf_8

    !> Check that ASCII-only strings come back unchanged.
    subroutine test_nocopy_ascii(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: uni, out, nocr, nolf
        character(len=*), parameter :: PLAIN(3) = [character(len=16) :: "us-ascii-unix", "iso-latin-1-unix", "utf-8-unix"]
        character(len=*), parameter :: BARE(3) = [character(len=16) :: "us-ascii", "iso-latin-1", "utf-8"]
        character(len=*), parameter :: DOS(4) = [character(len=16) :: "us-ascii-dos", "iso-latin-1-dos", "utf-8-dos", &
                                                                                                               "mac-roman-mac"]
        type(status_type) :: st
        integer :: k

        uni = ""
        nocr = ""
        nolf = ""

        do k = 0, 127
            uni = uni//achar(k)
            if (k /= 13) nocr = nocr//achar(k)
            if (k /= 10) nolf = nolf//achar(k)
        end do

        ! Encodings without EOL conversion.
        do k = 1, 3
            call decode_coding_string(uni, trim(PLAIN(k)), out, st)
            call check(error, out == uni .and. last_coding_system_used%value == trim(PLAIN(k)), trim(PLAIN(k)))
            if (allocated(error)) return
            call encode_coding_string(uni, trim(PLAIN(k)), out, st)
            call check(error, out == uni .and. last_coding_system_used%value == trim(PLAIN(k)), trim(PLAIN(k)))
            if (allocated(error)) return
        end do

        ! With EOL conversion inhibited.
        inhibit_eol_conversion%value = .true.

        do k = 1, 3
            call decode_coding_string(uni, trim(BARE(k)), out, st)
            call check(error, out == uni, trim(BARE(k)))
            if (allocated(error)) exit
            call encode_coding_string(uni, trim(BARE(k)), out, st)
            call check(error, out == uni, trim(BARE(k)))
            if (allocated(error)) exit
        end do

        inhibit_eol_conversion%value = .false.
        if (allocated(error)) return

        ! Identity decoding with EOL conversion for ASCII except CR, and
        ! encoding for ASCII except LF.
        do k = 1, 4
            call decode_coding_string(nocr, trim(DOS(k)), out, st)
            call check(error, out == nocr, trim(DOS(k)))
            if (allocated(error)) return
            call encode_coding_string(nolf, trim(DOS(k)), out, st)
            call check(error, out == nolf, trim(DOS(k)))
            if (allocated(error)) return
        end do
    end subroutine test_nocopy_ascii

    subroutine test_check_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(unencodable_type), allocatable :: r(:)
        type(status_type) :: st

        call check_coding_systems_text("aå", 0_int64, [character(len=8) :: "utf-8"], r, st)
        call check(error, st%ok() .and. size(r) == 0)
        if (allocated(error)) return
        call check_coding_systems_text("aåbγc", 0_int64, [character(len=12) :: "utf-8", "iso-latin-1", "us-ascii"], r, st)
        call check(error, size(r) == 2)
        if (allocated(error)) return
        call check(error, r(1)%coding == "iso-latin-1" .and. size(r(1)%positions) == 1 .and. r(2)%coding == "us-ascii" &
                   .and. size(r(2)%positions) == 2)
        if (allocated(error)) return
        call check(error, r(1)%positions(1) == 3 .and. r(2)%positions(1) == 1 .and. r(2)%positions(2) == 3)
        if (allocated(error)) return
        call check_coding_systems_text("å", 0_int64, [character(len=20) :: "bad-coding-system"], r, st)
        call check(error, st%code == FM_CODING_SYSTEM_ERROR)
    end subroutine test_check_region

    ! ------------------------------------------------------------------
    ! mule-tests.el
    ! ------------------------------------------------------------------

    !> Check that Bug#27391 is fixed.
    subroutine test_bug27391(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: coding, source

        call init_bindings()
        call find_auto_coding("", LF//"[comment]: # ( Local Variables: )"//LF// &
                              "[comment]: # ( coding: utf-8"//achar(9)//")"//LF// &
                              "[comment]: # ( End:"//achar(9)//achar(9)//")"//LF, coding, source)
        call check(error, coding == "utf-8" .and. source == ":coding", coding//" "//source)
    end subroutine test_bug27391

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> The coding system detect-coding-string finds first, as GNU Emacs
    !> finds it for the ported coding systems.
    subroutine test_detect(error)
        type(error_type), allocatable, intent(out) :: error

        call init_bindings()
        call expect("ABC"//LF, "undecided-unix")
        call expect("ABC", "undecided")
        call expect("A"//CR//LF//"B"//CR//LF, "undecided-dos")
        call expect("A"//CR//"B"//CR, "undecided-mac")
        call expect(char(int(z'C3'))//char(int(z'A9'))//LF, "utf-8-unix")
        call expect(char(int(z'E9'))//LF, "iso-latin-1-unix")
        call expect(char(int(z'80'))//LF, "raw-text-unix")
        call expect(achar(0)//"A"//achar(0)//"B", "no-conversion")
        call expect(char(int(z'91'))//char(int(z'92'))//LF, "iso-latin-1-unix")
        call expect("A"//char(int(z'80')), "raw-text")
        call expect(char(int(z'C3'))//char(int(z'A9'))//CR//LF, "utf-8-dos")
        call expect("A"//CR//LF//"B"//LF, "undecided-unix")

    contains

        subroutine expect(bytes, coding)
            character(len=*), intent(in) :: bytes, coding
            type(coding_system_list_entry_type), allocatable :: names(:)

            if (allocated(error)) return
            call detect_coding_string(bytes, names, .true.)
            call check(error, size(names) == 1, coding)
            if (allocated(error)) return
            call check(error, names(1)%name, coding)
        end subroutine expect
    end subroutine test_detect

    subroutine test_functions(error)
        type(error_type), allocatable, intent(out) :: error
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(status_type) :: st

        call init_bindings()
        call check(error, coding_system_p("utf-8") .and. coding_system_p("latin-1-dos") .and. coding_system_p("") &
                   .and. .not. coding_system_p("bogus"))
        if (allocated(error)) return
        call check_coding_system("bogus", st)
        call check(error, st%code == FM_CODING_SYSTEM_ERROR .and. format_error(st) == "Invalid coding system: bogus", &
                   format_error(st))
        if (allocated(error)) return
        call check(error, coding_system_base("latin-1-dos"), "iso-latin-1")
        if (allocated(error)) return
        call check(error, coding_system_eol_type("utf-8") == EOL_UNDECIDED .and. coding_system_eol_type("dos") == EOL_DOS &
                   .and. coding_system_eol_type("no-conversion") == EOL_UNIX .and. coding_system_eol_type("utf-8-mac") &
                   == EOL_MAC)
        if (allocated(error)) return
        call check(error, coding_system_change_eol_conversion("iso-8859-1", EOL_UNIX), "iso-latin-1-unix")
        if (allocated(error)) return
        call check(error, coding_system_change_eol_conversion("latin-1-dos", EOL_UNDECIDED), "iso-latin-1")
        if (allocated(error)) return
        call check(error, coding_system_change_eol_conversion("emacs-internal", EOL_DOS), "utf-8-emacs-dos")
        if (allocated(error)) return
        call check(error, coding_system_change_text_conversion("dos", "utf-8"), "utf-8-dos")
        if (allocated(error)) return
        call check(error, coding_system_type("latin-1") == "charset" .and. coding_system_type("binary") == "raw-text")
        if (allocated(error)) return
        call check(error, achar(coding_system_mnemonic("iso-latin-1"))//achar(coding_system_mnemonic("binary")), "1=")
        if (allocated(error)) return
        call coding_system_aliases("utf-8", names)
        call check(error, size(names) == 3)
        if (allocated(error)) return
        call check(error, names(1)%name//" "//names(2)%name//" "//names(3)%name, "utf-8 mule-utf-8 cp65001")
        if (allocated(error)) return
        call coding_system_priority_list(names)
        ! As GNU Emacs lists them, without in-is13194-devanagari (Deferred).
        call check(error, joined(names), "utf-8 iso-2022-7bit iso-latin-1 iso-2022-7bit-lock iso-2022-8bit-ss2 "// &
                   "emacs-mule raw-text iso-2022-jp chinese-iso-8bit utf-8-auto utf-8-with-signature utf-16 "// &
                   "utf-16be-with-signature utf-16le-with-signature utf-16be utf-16le japanese-shift-jis chinese-big5 " &
                   //"undecided")

    contains

        function joined(list) result(s)
            type(coding_system_list_entry_type), intent(in) :: list(:)
            character(len=:), allocatable :: s
            integer :: k

            s = ""

            do k = 1, size(list)
                if (k > 1) s = s//" "
                s = s//list(k)%name
            end do
        end function joined
    end subroutine test_functions

    !> `coding:' tags in the -*- line and the local variables list, the
    !> BOM regexps, and the file names of `auto_coding_alist'.
    subroutine test_auto_coding(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: coding, source

        call init_bindings()
        call find_auto_coding("/tmp/x.txt", ";; -*- mode: text; coding: latin-1 -*-"//LF//"x"//LF, coding, source)
        call check(error, coding == "latin-1" .and. source == ":coding", coding)
        if (allocated(error)) return
        call find_auto_coding("/tmp/x.sh", "#!/bin/sh"//LF//"# -*- coding: utf-8-dos -*-"//LF, coding, source)
        call check(error, coding, "utf-8-dos")
        if (allocated(error)) return
        call find_auto_coding("/tmp/x.txt", "x"//LF//"Local Variables:"//LF//"coding: iso-latin-1!"//LF//"End:"//LF, &
                              coding, source)
        call check(error, coding, "iso-latin-1")
        if (allocated(error)) return
        call find_auto_coding("/tmp/x.txt", char(int(z'EF'))//char(int(z'BB'))//char(int(z'BF'))//"x", coding, source)
        call check(error, coding == "utf-8-with-signature" .and. source == "auto-coding-regexp-alist", coding)
        if (allocated(error)) return
        call find_auto_coding("/tmp/x.tar", ";; -*- coding: latin-1 -*-", coding, source)
        call check(error, coding == "no-conversion" .and. source == "auto-coding-alist", coding)
        if (allocated(error)) return
        call find_auto_coding("/tmp/x.txt", "x"//LF, coding, source)
        call check(error, len(coding) == 0, coding)
    end subroutine test_auto_coding

    !> Visiting files in Latin-1 and with DOS line ends, and saving them
    !> back unchanged; setting the buffer's coding system for saving.
    subroutine test_visit_save(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        character(len=:), allocatable :: text, data
        character(len=*), parameter :: LAT1 = "caf"//char(int(z'E9'))//LF
        character(len=*), parameter :: DOSTEXT = "one"//CR//LF//"two"//CR//LF

        call setup()
        call write_raw("latin1.txt", LAT1)
        call write_raw("dos.txt", DOSTEXT)
        call find_file_noselect(tdir()//"latin1.txt", buf, st)
        call set_buffer(buf, st)
        text = buffer_string()
        call check(error, text == "café"//LF .and. buffer_file_coding_system%value == "iso-latin-1-unix", &
                   buffer_file_coding_system%value)
        if (allocated(error)) return
        call check(error, mode_line_mule_info(cur, .false.), "UU1:")
        if (allocated(error)) return
        call goto_char(point_max())
        call insert("x"//LF, st)
        call save_buffer(0_int64, .false., st)
        call read_file(tdir()//"latin1.txt", "Opening input file", data, st)
        call check(error, data == LAT1//"x"//LF, data)
        if (allocated(error)) return
        call kill_buffer(buf, st)
        call find_file_noselect(tdir()//"dos.txt", buf, st)
        call set_buffer(buf, st)
        text = buffer_string()
        call check(error, text == "one"//LF//"two"//LF .and. buffer_file_coding_system%value == "undecided-dos", &
                   buffer_file_coding_system%value)
        if (allocated(error)) return
        ! As startup.el:command-line sets it on GNU/Linux.
        eol_mnemonic_dos = "(DOS)"
        call check(error, mode_line_mule_info(cur, .false.), "UU-(DOS)")
        if (allocated(error)) return
        call goto_char(point_max())
        call insert("thrée"//LF, st)
        ! The DOS line ends stay, and é is saved as UTF-8 (the default).
        call save_buffer(0_int64, .false., st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call read_file(tdir()//"dos.txt", "Opening input file", data, st)
        call check(error, data == DOSTEXT//"thrée"//CR//LF, data)
        if (allocated(error)) return
        call check(error, buffer_file_coding_system%value, "utf-8-dos")
        if (allocated(error)) return
        ! C-x RET f latin-1: kept from the DOS line ends; marks the buffer
        ! modified.
        call set_buffer_file_coding_system("latin-1", .false., .false., .false., st)
        call check(error, buffer_file_coding_system%value == "iso-latin-1-dos" .and. &
                   buffers(cur)%text%modiff > buffers(cur)%text%save_modiff, buffer_file_coding_system%value)
        if (allocated(error)) return
        call save_buffer(0_int64, .false., st)
        call read_file(tdir()//"dos.txt", "Opening input file", data, st)
        call check(error, data == DOSTEXT//"thr"//char(int(z'E9'))//"e"//CR//LF, data)
        call kill_buffer(buf, st)
        call remove_files([character(len=16) :: "latin1.txt", "dos.txt", "latin1.txt~", "dos.txt~"])

    contains

        subroutine write_raw(name, bytes)
            character(len=*), intent(in) :: name, bytes
            type(handle_type) :: b
            type(status_type) :: s

            call temp_buffer(b)
            buffers(cur)%multibyte = .false.
            call insert(bytes, s)
            coding_system_for_write = "no-conversion"
            call write_region(tdir()//name, .false., s)
            call coding_system_for_write%clear()
            call kill_buffer(b, s)
        end subroutine write_raw
    end subroutine test_visit_save

    !> mode-line-mule-info for buffers that read nothing, a unibyte one, and
    !> on a window-system frame.
    subroutine test_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call temp_buffer(buf)
        call check(error, mode_line_mule_info(cur, .false.), "UUU:")
        if (allocated(error)) return
        call check(error, mode_line_mule_info(cur, .true.), "U:")
        if (allocated(error)) return
        call buffer_file_coding_system%set_local("undecided")
        call check(error, mode_line_mule_info(cur, .false.), "UU-:")
        if (allocated(error)) return
        call buffer_file_coding_system%set_local("raw-text-mac")
        buffers(cur)%multibyte = .false.
        eol_mnemonic_mac = "(Mac)"
        call check(error, mode_line_mule_info(cur, .false.), "UU (Mac)")
        call kill_buffer(buf, st)
    end subroutine test_mode_line

    !> prefer-coding-system puts the category first and sets the defaults.
    subroutine test_prefer(error)
        type(error_type), allocatable, intent(out) :: error
        type(coding_system_list_entry_type), allocatable :: saved(:), names(:)
        type(status_type) :: st

        call init_bindings()
        call save_priority(saved)
        call prefer_coding_system("latin-1-dos", st)
        call coding_system_priority_list(names, .true.)
        call check(error, st%ok() .and. names(1)%name == "iso-latin-1", format_error(st))
        if (allocated(error)) return
        call prefer_coding_system("undecided", st)
        call check(error, format_error(st), "Can"//char(int(z'E2'))//char(int(z'80'))//char(int(z'99'))// &
                   "t prefer the coding system "//char(int(z'E2'))//char(int(z'80'))//char(int(z'98'))//"undecided"// &
                   char(int(z'E2'))//char(int(z'80'))//char(int(z'99')))
        call prefer_coding_system("utf-8-unix", st)
        call restore_priority(saved)
    end subroutine test_prefer

    !> C-h C utf-8.
    subroutine test_describe(error)
        use buffer_fm, only: get_buffer
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: help, old
        type(status_type) :: st
        character(len=:), allocatable :: text

        call init_bindings()
        old = current_buffer()
        call describe_coding_system("utf-8", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        help = get_buffer("*Help*")
        call set_buffer(help, st)
        text = buffer_string()
        call set_buffer(old, st)
        call check(error, text, "U -- utf-8 (alias: mule-utf-8 cp65001)"//LF//LF//"UTF-8 (no signature (BOM))"//LF// &
                   "Type: utf-8 (UTF-8: Emacs internal multibyte form)"//LF//"EOL type: Automatic selection from:"//LF// &
                   achar(9)//"[utf-8-unix utf-8-dos utf-8-mac]"//LF// &
                   "This coding system encodes the following charsets:"//LF//"  unicode"//LF)
        ! The help window goes, for the suites after this one.
        call kill_buffer(help, st)
        call show_alone()
    end subroutine test_describe

    !> C-x RET c runs the next command with its coding system: C-x C-f
    !> reads a UTF-8 file as Latin-1, and the coding system is not left
    !> bound.  C-x RET p needs a process.
    subroutine test_universal_coding_system_argument(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, old
        type(status_type) :: st

        call setup()
        call get_buffer_create(" coding-c-tmp", buf, st)
        call set_buffer(buf, st)
        call insert("café"//LF, st)
        call write_region(tdir()//"utf8.txt", .false., st)
        call kill_buffer(buf, st)
        call get_buffer_create(" coding-c", old, st)
        call set_buffer(old, st)
        call show_alone()
        call discard_input()
        ! C-x RET c iso-latin-1 RET C-x C-f FILE RET
        call kbd_store_bytes(achar(24)//CR//"ciso-latin-1"//CR//achar(24)//achar(6)//tdir()//"utf8.txt"//CR)
        call command_loop(st)
        call check(error, buffer_string(), "caf"//"Ã"//"©"//LF)
        if (.not. allocated(error)) call check(error,.not. allocated(coding_system_for_read%value), "not left bound")

        if (.not. allocated(error)) then
            call set_buffer(old, st)
            call set_buffer_process_coding_system("utf-8", "utf-8", st)
            call check(error, format_error(st), "No process")
        end if

        buf = current_buffer()
        call kill_buffer(buf, st)
        call kill_buffer(old, st)
        call remove_files(["utf8.txt"])
    end subroutine test_universal_coding_system_argument

    !> C-x RET t: the terminal output is encoded with the terminal coding
    !> system ("?" for what it can't encode).  C-x RET k: keyboard input is
    !> decoded with the keyboard coding system.
    subroutine test_terminal_keyboard_coding(error)
        type(error_type), allocatable, intent(out) :: error
        type(glyph_type) :: glyphs(3)
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have

        call setup()
        call set_terminal_coding_system("iso-latin-1-unix", st, inhibit_refresh=.true.)
        call check(error, terminal_coding_system(), "iso-latin-1-unix")

        if (.not. allocated(error)) then
            glyphs(1)%ch = iachar("a")
            glyphs(2)%ch = 233
            glyphs(3)%ch = int(z'20AC')
            call check(error, encode_terminal_code(glyphs), "a"//char(233)//"?")
        end if

        call set_terminal_coding_system("utf-8-unix", st, inhibit_refresh=.true.)
        if (.not. allocated(error)) call check(error, encode_terminal_code(glyphs), "aé€")

        if (.not. allocated(error)) then
            call set_keyboard_coding_system("iso-latin-1", st)
            call check(error, keyboard_coding_system(), "iso-latin-1-unix")
        end if

        if (.not. allocated(error)) then
            call discard_input()
            call kbd_store_bytes(char(233))
            call read_event(ev, have, st)
            call check(error, have .and. xfixnum(ev) == 233, "é from Latin-1")
        end if

        call set_keyboard_coding_system("utf-8", st)

        if (.not. allocated(error)) then
            call kbd_store_bytes("é")
            call read_event(ev, have, st)
            call check(error, have .and. xfixnum(ev) == 233, "é from UTF-8")
        end if

        if (.not. allocated(error)) then
            call set_keyboard_coding_system("", st)
            call check(error, format_error(st), "Unsuitable coding system for keyboard: nil")
        end if
    end subroutine test_terminal_keyboard_coding

    ! The coding systems of type `charset' (values as GNU Emacs gives
    ! them).
    subroutine test_charset_coding(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(unencodable_type), allocatable :: result(:)
        type(status_type) :: st

        call init_bindings()
        call decode_coding_string(char(161)//char(177)//char(128), "iso-latin-2", out, st)
        call check(error, out == char(196)//char(132)//char(196)//char(133)//char(194)//char(128))
        if (allocated(error)) return
        call encode_coding_string(char(196)//char(132)//char(196)//char(133)//char(226)//char(130)//char(172), &
                                  "iso-latin-2", out, st)
        call check(error, out == char(161)//char(177)//" ")
        if (allocated(error)) return
        call encode_coding_string(char(226)//char(130)//char(172), "windows-1252", out, st)
        call check(error, out == char(128))
        if (allocated(error)) return
        ! 0x81 is not in windows-1252: a raw byte.
        call decode_coding_string(char(129), "windows-1252", out, st)
        call check(error, out == char(int(z'C0'))//char(int(z'81')))
        if (allocated(error)) return
        call decode_coding_string(char(int(z'C1'))//char(int(z'C2')), "cyrillic-koi8", out, st)
        call check(error, out == char(208)//char(176)//char(208)//char(177))
        if (allocated(error)) return
        call decode_coding_string(char(128)//char(129), "cp437", out, st)
        call check(error, out == char(195)//char(135)//char(195)//char(188))
        if (allocated(error)) return
        call encode_coding_string("a"//char(208)//char(150), "us-ascii", out, st)
        call check(error, out == "a?")
        if (allocated(error)) return
        call encode_coding_string(char(224)//char(184)//char(129), "thai-tis620", out, st)
        call check(error, out == char(161))
        if (allocated(error)) return
        call check(error, coding_system_type("cyrillic-koi8") == "charset")
        if (allocated(error)) return
        call coding_system_aliases("iso-latin-2", names)
        call check(error, size(names) == 3)
        if (allocated(error)) return
        call check(error, names(1)%name == "iso-latin-2" .and. names(2)%name == "iso-8859-2" &
                   .and. names(3)%name == "latin-2")
        if (allocated(error)) return
        call check_coding_systems_text("a"//char(196)//char(132)//char(208)//char(150), 0_int64, &
                                       [character(len=16) :: "iso-latin-2", "cyrillic-koi8", "windows-1250"], result, st)
        call check(error, size(result) == 3)
        if (allocated(error)) return
        call check(error, result(1)%coding == "iso-latin-2" .and. result(2)%coding == "cyrillic-koi8" &
                   .and. result(3)%coding == "windows-1250")
        if (allocated(error)) return
        call check(error, size(result(1)%positions) == 1 .and. size(result(2)%positions) == 1)
        if (allocated(error)) return
        call check(error, result(1)%positions(1) == 2 .and. result(2)%positions(1) == 1)
        if (allocated(error)) return
        ! Detection by the coding system of the charset category.
        call set_coding_system_priority([character(len=16) :: "iso-latin-2"], st)
        call detect_coding_string("abc"//char(161)//char(177), names, .false.)
        call check(error, names(1)%name == "iso-latin-2")

        if (.not. allocated(error)) then
            call detect_coding_string("abc"//char(129), names, .false.)
            call check(error, size(names) == 1)

            if (.not. allocated(error)) then
                call check(error, names(1)%name == "raw-text")
            end if
        end if

        call set_coding_system_priority([character(len=16) :: "iso-latin-1"], st)
        call set_coding_system_priority([character(len=16) :: "utf-8"], st)
    end subroutine test_charset_coding

    ! Shift-JIS and Big5 (values as GNU Emacs gives them).
    subroutine test_sjis_big5(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(status_type) :: st
        ! "a亜ｱあ" and "a一€".
        character(len=*), parameter :: JA = "a"//char(228)//char(186)//char(156)//char(239)//char(189)//char(177) &
                                       //char(227)//char(129)//char(130)
        character(len=*), parameter :: ZH = "a"//char(228)//char(184)//char(128)//char(226)//char(130)//char(172)

        call init_bindings()
        call encode_coding_string(JA, "japanese-shift-jis", out, st)
        call check(error, out == "a"//char(136)//char(159)//char(177)//char(130)//char(160))
        if (allocated(error)) return
        call decode_coding_string("a"//char(136)//char(159)//char(177)//char(130)//char(160), "japanese-shift-jis", &
                                  out, st)
        call check(error, out == JA)
        if (allocated(error)) return
        call encode_coding_string(ZH, "chinese-big5", out, st)
        call check(error, out == "a"//char(164)//"@"//char(163)//char(225))
        if (allocated(error)) return
        ! An incomplete code at the end is a raw byte.
        call decode_coding_string(char(164)//"@"//char(164), "chinese-big5", out, st)
        call check(error, out == char(228)//char(184)//char(128)//char(int(z'C0'))//char(164))
        if (allocated(error)) return
        call decode_coding_string(char(136), "japanese-shift-jis", out, st)
        call check(error, out == char(int(z'C0'))//char(136))
        if (allocated(error)) return
        call encode_coding_string(char(228)//char(186)//char(156), "japanese-cp932", out, st)
        call check(error, out == char(136)//char(159))
        if (allocated(error)) return
        call check(error, coding_system_type("sjis") == "shift-jis")
        if (allocated(error)) return
        call set_coding_system_priority([character(len=24) :: "japanese-shift-jis"], st)
        call detect_coding_string("abc"//char(136)//char(159), names, .false.)
        call check(error, names(1)%name == "japanese-shift-jis")

        if (.not. allocated(error)) then
            call set_coding_system_priority([character(len=24) :: "chinese-big5"], st)
            call detect_coding_string("abc"//char(164)//"@", names, .false.)
            call check(error, names(1)%name == "chinese-big5")
        end if

        call set_coding_system_priority([character(len=16) :: "iso-latin-1"], st)
        call set_coding_system_priority([character(len=16) :: "utf-8"], st)
    end subroutine test_sjis_big5

    ! ISO-2022 coding systems (values as GNU Emacs gives them).
    subroutine test_iso_2022(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(status_type) :: st
        character(len=*), parameter :: E = achar(27)
        ! 亜, ｱ, é, 가, €.
        character(len=*), parameter :: A = char(228)//char(186)//char(156), KANA = char(239)//char(189)//char(177), &
                                       EA = char(195)//char(169), GA = char(234)//char(176)//char(128), &
                                       EURO = char(226)//char(130)//char(172)

        call init_bindings()
        call encode_coding_string("a"//A//LF//"b", "iso-2022-jp", out, st)
        call check(error, out == "a"//E//"$B0!"//E//"(B"//LF//"b")
        if (allocated(error)) return
        call decode_coding_string("a"//E//"$B0!"//E//"(B"//LF//"b", "iso-2022-jp", out, st)
        call check(error, out == "a"//A//LF//"b")
        if (allocated(error)) return
        call encode_coding_string("a"//A//EA, "iso-2022-7bit", out, st)
        call check(error, out == "a"//E//"$(I$B"//E//",Ai"//E//"(B")
        if (allocated(error)) return
        call encode_coding_string(EA//A, "japanese-iso-8bit", out, st)
        call check(error, out == char(143)//char(171)//char(177)//char(176)//char(161))
        if (allocated(error)) return
        call decode_coding_string(char(176)//char(161)//char(142)//char(177), "japanese-iso-8bit", out, st)
        call check(error, out == A//KANA)
        if (allocated(error)) return
        call encode_coding_string("a"//GA//"b", "iso-2022-kr", out, st)
        call check(error, out == "a"//E//"$(C0!"//E//"(Bb")
        if (allocated(error)) return
        call decode_coding_string("a"//E//"$)C"//achar(14)//"0!"//achar(15)//"b", "iso-2022-kr", out, st)
        call check(error, out == "a"//GA//"b")
        if (allocated(error)) return
        call encode_coding_string(EA//EURO, "compound-text", out, st)
        call check(error, out == char(233)//E//"-F"//char(164)//E//"-A")
        if (allocated(error)) return
        ! An incomplete code at the end: the designation is used, the
        ! rest flushed.
        call decode_coding_string(E//"$B0", "iso-2022-jp", out, st)
        call check(error, out == "0")
        if (allocated(error)) return
        call encode_coding_string(char(228)//char(184)//char(128), "iso-2022-cn", out, st)
        call check(error, out == E//"$AR;"//E//"(B")
        if (allocated(error)) return
        call encode_coding_string(EA, "iso-2022-8bit-ss2", out, st)
        call check(error, out == E//".A"//char(142)//char(233))
        if (allocated(error)) return
        call check(error, coding_system_type("iso-2022-jp") == "iso-2022")
        if (allocated(error)) return
        ! Detection: an escape sequence starts the ISO-2022 detector.
        call detect_coding_string("a"//E//"$B0!"//E//"(B", names, .false.)
        call check(error, size(names) == 4)
        if (allocated(error)) return
        call check(error, names(1)%name == "iso-2022-7bit" .and. names(2)%name == "iso-2022-7bit-lock" &
                   .and. names(3)%name == "iso-2022-8bit-ss2" .and. names(4)%name == "iso-2022-jp")
        if (allocated(error)) return
        call detect_coding_string("abc"//E//"$)C"//achar(14)//"0!"//achar(15), names, .false.)
        call check(error, size(names) == 2)
        if (allocated(error)) return
        call check(error, names(1)%name == "iso-2022-7bit-lock" .and. names(2)%name == "iso-2022-8bit-ss2")
    end subroutine test_iso_2022

    ! The emacs-mule coding system (values as GNU Emacs gives them).
    subroutine test_emacs_mule(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(status_type) :: st
        integer :: k
        ! é, 亜, €, ก, ā.
        character(len=*), parameter :: EA = char(195)//char(169), A = char(228)//char(186)//char(156), &
                                       EURO = char(226)//char(130)//char(172), KO = char(224)//char(184)//char(129), &
                                       AMACRON = char(196)//char(129)

        call init_bindings()
        call encode_coding_string("a"//EA//A//EURO, "emacs-mule", out, st)
        call check(error, out == "a"//char(129)//char(233)//char(157)//char(246)//char(164)//char(194)//char(134) &
                   //char(164))
        if (allocated(error)) return
        call decode_coding_string("a"//char(129)//char(233)//char(146)//char(176)//char(161), "emacs-mule", out, st)
        call check(error, out == "a"//EA//A)
        if (allocated(error)) return
        ! An incomplete sequence at the end: raw bytes.
        call decode_coding_string(char(146)//char(176), "emacs-mule", out, st)
        call check(error, out == char(int(z'C0'))//char(146)//char(int(z'C0'))//char(176))
        if (allocated(error)) return
        call encode_coding_string(KO//AMACRON, "emacs-mule", out, st)
        call check(error, out == char(133)//char(161)//char(132)//char(224))
        if (allocated(error)) return
        call check(error, coding_system_type("emacs-mule") == "emacs-mule")
        if (allocated(error)) return
        call detect_coding_string("abc"//char(129)//char(233)//char(146)//char(176)//char(161), names, .false.)
        ! GNU Emacs gives (emacs-mule japanese-shift-jis raw-text); the
        ! tests before this one change the priority of the categories.
        call check(error, size(names) == 3)
        if (allocated(error)) return
        call check(error, any([(names(k)%name == "emacs-mule", k=1, 3)]) &
                   .and. any([(names(k)%name == "japanese-shift-jis", k=1, 3)]) .and. names(3)%name == "raw-text")
    end subroutine test_emacs_mule

    !> get_language_info, set-language-environment (coding priorities,
    !> charset priority, the default input method, Catalan's setup and
    !> exit functions), C-x RET l and describe-language-environment, as
    !> in Emacs (whose input methods wait for them).
    subroutine test_language_environments(error)
        use buffer_fm, only: get_buffer
        use charset_fm, only: charset_priority_list, set_charset_priority, CHARSET_NAME_LEN
        use syntax_fm, only: char_syntax
        type(error_type), allocatable, intent(out) :: error
        type(coding_system_list_entry_type), allocatable :: saved(:), names(:)
        character(len=CHARSET_NAME_LEN), allocatable :: charsets(:), prio(:)
        type(language_info_type) :: info
        type(handle_type) :: buf, help
        type(status_type) :: st
        character(len=:), allocatable :: text, old_env, old_method
        logical :: found

        call save_priority(saved)
        charsets = charset_priority_list()
        old_env = current_language_environment%value
        old_method = default_input_method%value
        call get_language_info("greek", info, found)
        call check(error, found .and. info%name == "Greek" .and. info%input_method == "greek")
        if (allocated(error)) return
        call check(error, trim(info%coding_priority(1)), "greek-iso-8bit")
        if (allocated(error)) return
        call get_language_info("Klingon", info, found)
        call check(error,.not. found)
        if (allocated(error)) return
        call set_language_environment("Klingon", st)
        call check(error, format_error(st), "Language environment not defined: ""Klingon""")
        if (allocated(error)) return
        call get_buffer_create(" lang-env", buf, st)
        call set_buffer(buf, st)
        call show_alone()
        call discard_input()
        ! C-x RET l greek RET
        call kbd_store_bytes(achar(24)//CR//"lgreek"//CR)
        call command_loop(st)
        call check(error, current_language_environment%value, "Greek")
        if (.not. allocated(error)) call check(error, default_input_method%value, "greek")

        if (.not. allocated(error)) then
            call coding_system_priority_list(names)
            call check(error, names(1)%name, "greek-iso-8bit")
        end if

        if (.not. allocated(error)) then
            prio = charset_priority_list()
            call check(error, trim(prio(1)), "iso-8859-7")
        end if

        if (.not. allocated(error)) then
            call set_language_environment("Catalan", st)
            call check(error, char_syntax(183), "w")
        end if

        if (.not. allocated(error)) then
            call set_language_environment("English", st)
            call check(error, char_syntax(183), "_")
        end if

        if (.not. allocated(error)) then
            call describe_language_environment("English", st)
            help = get_buffer("*Help*")
            call set_buffer(help, st)
            text = buffer_string()
            call check(error, text, "English language environment"//LF//LF// &
                       "Nothing special is needed to handle English."//LF//LF//"Sample text:"//LF// &
                       "  Hello!, Hi!, How are you?"//LF//LF//"Character sets:"//LF//"  ascii: ASCII (ISO646 IRV)"//LF// &
                       LF//"Coding systems:"//LF//"  nothing specific to English"//LF)
            call kill_buffer(help, st)
        end if

        call kill_buffer(buf, st)
        call show_alone()
        current_language_environment%value = old_env
        default_input_method%value = old_method
        call prefer_coding_system("utf-8-unix", st)
        call restore_priority(saved)
        call set_charset_priority(charsets, st)
    end subroutine test_language_environments
    !> set-locale-environment, as in Emacs: the language environment, the
    !> preferred, locale, terminal and keyboard coding systems for locales
    !> naming a language and a codeset, and the codeset names it knows.
    subroutine test_set_locale_environment(error)
        use charset_fm, only: charset_priority_list, set_charset_priority, CHARSET_NAME_LEN, current_iso639_language
        use coding_fm, only: locale_coding_system
        type(error_type), allocatable, intent(out) :: error
        type(coding_system_list_entry_type), allocatable :: saved(:), names(:)
        character(len=CHARSET_NAME_LEN), allocatable :: charsets(:)
        character(len=:), allocatable :: old_env, old_method, old_locale_coding
        type(status_type) :: st
        integer :: k

        k = locale_name_match("de_de.utf-8", locale_language_names)
        call check(error, trim(locale_language_names(k)%language), "German")
        if (allocated(error)) return
        call check(error, locale_charset_to_coding_system("ISO8859-2"), "iso-latin-2")
        if (allocated(error)) return
        call check(error, locale_charset_to_coding_system("eucJP"), "japanese-iso-8bit")
        if (allocated(error)) return
        call save_priority(saved)
        charsets = charset_priority_list()
        old_env = current_language_environment%value
        old_method = default_input_method%value
        old_locale_coding = locale_coding_system%value
        call set_locale_environment("de_DE.ISO-8859-1", st, inhibit_refresh=.true.)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call coding_system_priority_list(names)
            call check(error, current_language_environment%value//" "//locale_coding_system%value//" "//names(1)%name &
                       //" "//default_input_method%value//" "//buffer_file_coding_system%value//" " &
                       //terminal_coding_system()//" "//keyboard_coding_system()//" "//current_iso639_language, &
                       "German iso-latin-1-unix iso-latin-1 german-postfix iso-latin-1-unix iso-latin-1-unix " &
                       //"iso-latin-1-unix de")
        end if

        if (.not. allocated(error)) then
            call set_locale_environment("ja_JP.eucJP", st, inhibit_refresh=.true.)
            call coding_system_priority_list(names)
            call check(error, current_language_environment%value//" "//locale_coding_system%value//" "//names(1)%name &
                       //" "//keyboard_coding_system(), &
                       "Japanese japanese-iso-8bit-unix japanese-iso-8bit japanese-iso-8bit-unix")
        end if

        if (.not. allocated(error)) then
            call set_locale_environment("ru_RU.KOI8-R", st, inhibit_refresh=.true.)
            call check(error, current_language_environment%value//" "//locale_coding_system%value, &
                       "Russian cyrillic-koi8-unix")
        end if

        call set_language_environment("English", st)
        current_language_environment%value = old_env
        default_input_method%value = old_method
        locale_coding_system%value = old_locale_coding
        call prefer_coding_system("utf-8-unix", st)
        call restore_priority(saved)
        call set_charset_priority(charsets, st)
        call set_terminal_coding_system("utf-8-unix", st, inhibit_refresh=.true.)
        call set_keyboard_coding_system("utf-8", st)
    end subroutine test_set_locale_environment
end module test_coding_fm
