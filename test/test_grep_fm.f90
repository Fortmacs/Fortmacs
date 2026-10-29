! test_grep_fm.f90 -- tests for grep_fm.
!
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

!> Ports of grep-tests.el (the GNU/Linux case of the rgrep abbreviation
!> tests; the darwin and windows-nt cases bind `system-type', which is
!> always gnu/linux here) and of compile-tests.el compile-test-grep-regexps,
!> and own tests: `wildcard_to_regexp', `grep_expand_template', and grep
!> run on a file with `next_error' visiting the matches.
module test_grep_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, stringp, xstring
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, get_buffer, &
                         buffer_live_p, set_buffer_read_only, current_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, point, point_min, point_max, goto_char
    use textprop_fm, only: get_text_property
    use search_fm, only: re_search_forward, string_match, case_fold_search
    use subr_fm, only: match_string
    use fileio_fm, only: file_name_as_directory
    use files_fm, only: temporary_file_directory, wildcard_to_regexp
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use font_lock_fm, only: font_lock_ensure
    use process_fm, only: accept_process_output, get_buffer_process
    use compile_fm, only: compilation_parse_errors, compilation_forget_errors, compilation_message_info, &
                          compilation_message_info_type, compilation_message_counts
    use grep_fm, only: grep, grep_mode, grep_compute_defaults, rgrep_default_command, grep_expand_template, &
                       grep_find_abbreviate, grep_heading_regexp, grep_highlight_matches, GREP_NIL, GREP_ALWAYS, &
                       GREP_AUTO
    use simple_fm, only: next_error
    implicit none(type, external)
    private

    public :: collect_grep_fm

    integer, parameter :: TIMEOUT = 600
    character(len=*), parameter :: NUL = achar(0)

contains

    subroutine collect_grep_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("grep-tests--rgrep-abbreviate-properties-gnu-linux", test_rgrep_abbreviate), &
                    new_unittest("grep-tests--grep-heading-regexp-without-null", test_heading_regexp_without_null), &
                    new_unittest("grep-tests--grep-heading-regexp-with-null", test_heading_regexp_with_null), &
                    new_unittest("compile-test-grep-regexps", test_grep_regexps), &
                    new_unittest("wildcard-to-regexp", test_wildcard_to_regexp), &
                    new_unittest("grep-expand-template", test_expand_template), &
                    new_unittest("grep-run", test_grep_run) &
                    ]
    end subroutine collect_grep_fm

    !> Port of grep-tests.el grep-tests--get-rgrep-abbreviation: the
    !> `display' property of the excessive part of the rgrep command ("" for
    !> none).
    function rgrep_abbreviation() result(display)
        character(len=:), allocatable :: display
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: v
        integer(int64) :: found
        display = ""
        call get_buffer_create(" *grep-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call grep_compute_defaults(st)
        call insert(rgrep_default_command("search", "*"), st)
        call grep_mode(st)
        buffers(cur)%read_only = .false.
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
        call goto_char(point_min())
        call re_search_forward("find ", found, st)
        call get_text_property(point(), sym("display"), v, st)
        if (stringp(v)) display = xstring(v)
        call kill_buffer(h, st)
    end function rgrep_abbreviation

    !> Port of grep-tests.el grep-tests--rgrep-abbreviate-properties-gnu-linux
    !> (grep-tests--check-rgrep-abbreviation): the excessive part of the
    !> rgrep command is abbreviated iff `grep_find_abbreviate' is true.
    subroutine test_rgrep_abbreviate(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: saved
        call init_bindings()
        saved = grep_find_abbreviate
        grep_find_abbreviate = .true.
        call check(error, rgrep_abbreviation(), "[…]")
        if (.not. allocated(error)) then
            grep_find_abbreviate = .false.
            call check(error, rgrep_abbreviation(), "")
        end if
        grep_find_abbreviate = saved
    end subroutine test_rgrep_abbreviate

    !> Port of grep-tests.el grep-tests--grep-heading-regexp-without-null.
    subroutine test_heading_regexp_without_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=1), parameter :: SEPS(3) = [":", "-", "="]
        character(len=:), allocatable :: string
        integer :: k
        call init_bindings()
        do k = 1, 3
            string = "filename"//SEPS(k)//"123"//SEPS(k)//"text"
            call check(error, string_match(grep_heading_regexp, string, st) /= 0, string)
            if (.not. allocated(error)) call check(error, match_string(1, string), "filename")
            if (.not. allocated(error)) call check(error, match_string(2, string), "filename"//SEPS(k))
            if (allocated(error)) return
        end do
    end subroutine test_heading_regexp_without_null

    !> Port of grep-tests.el grep-tests--grep-heading-regexp-with-null.
    subroutine test_heading_regexp_with_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=1), parameter :: SEPS(3) = [":", "-", "="]
        character(len=:), allocatable :: string
        integer :: k
        call init_bindings()
        do k = 1, 3
            string = "funny:0:filename"//NUL//"123"//SEPS(k)//"text"
            call check(error, string_match(grep_heading_regexp, string, st) /= 0, string)
            if (.not. allocated(error)) call check(error, match_string(1, string), "funny:0:filename")
            if (.not. allocated(error)) call check(error, match_string(2, string), "funny:0:filename"//NUL)
            if (allocated(error)) return
        end do
    end subroutine test_heading_regexp_with_null

    !> Port of compile-tests.el compile--test-error-line for the grep test
    !> cases (no rule name, no column): parse STR and check the message at
    !> POS (1) has LINE and FILE; INFO gets the message.
    subroutine grep_error_line(error, str, line, file, info)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: str, file
        integer(int64), intent(in) :: line
        type(compilation_message_info_type), intent(out) :: info
        type(status_type) :: st
        logical :: found
        character(len=:), allocatable :: what
        what = "testcase: "//str
        call erase_buffer(st)
        call compilation_forget_errors()
        call insert(str, st)
        call compilation_parse_errors(point_min(), point_max(), st)
        call check(error, st%ok(), what//": "//format_error(st))
        if (allocated(error)) return
        call compilation_message_info(1_int64, info, found)
        call check(error, found, what//": no message")
        if (.not. allocated(error)) call check(error, info%has_loc, what//": no location")
        if (allocated(error)) return
        call check(error,.not. info%has_column, what//": column")
        if (.not. allocated(error)) call check(error, info%has_line, what//": line")
        if (.not. allocated(error)) call check(error, info%line, line, what//": line")
        if (.not. allocated(error)) call check(error, allocated(info%file), what//": file")
        if (.not. allocated(error)) call check(error, info%file, file, what//": file")
        if (.not. allocated(error)) call check(error,.not. allocated(info%rule), what//": rule")
    end subroutine grep_error_line

    !> Port of compile-tests.el compile-test-grep-regexps: the
    !> `grep_regexp_alist' regexps, on file names that NUL bytes or colons
    !> separate from the line numbers.
    subroutine test_grep_regexps(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(compilation_message_info_type) :: info1, info2
        integer(int64) :: errors, warnings, infos
        call init_bindings()
        call get_buffer_create(" *grep-regexps*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call grep_mode(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        buffers(cur)%read_only = .false.
        call minor_modes%font_lock_mode%disable(st)
        ! Bug#32051.
        call pair("c:/Users/my.name/src/project\src\kbhit.hpp", "29:#include <termios.h>", 29_int64)
        if (.not. allocated(error)) call pair("d:/gnu/emacs/branch/src/callproc.c", "214:#ifdef DOS_NT", 214_int64)
        if (.not. allocated(error)) call pair("/gnu/emacs/branch/src/callproc.c", "214:#ifdef DOS_NT", 214_int64)
        if (allocated(error)) return
        ! Bug#7378: these can only work with the NUL byte to disambiguate
        ! colons.
        call grep_error_line(error, "./x11-libs---nx/3.4.0:0:C.30253.1289557929.792611.C/nx-3.4.0.exheres-0"//NUL// &
                             "42:some text", 42_int64, &
                             "./x11-libs---nx/3.4.0:0:C.30253.1289557929.792611.C/nx-3.4.0.exheres-0", info1)
        if (.not. allocated(error)) call grep_error_line(error, "2011-08-31_11:57:03_1"//NUL// &
                                                         "7:Date: Wed, 31 Aug 2011 11:57:03 +0000", 7_int64, &
                                                         "2011-08-31_11:57:03_1", info1)
        if (allocated(error)) return
        call compilation_message_counts(errors, warnings, infos)
        call check(error, errors, 8_int64, "errors")
        call kill_buffer(h, st)
    contains

        !> The test case with the NUL byte, then with a colon in its place:
        !> the same message.
        subroutine pair(file, rest, line)
            character(len=*), intent(in) :: file, rest
            integer(int64), intent(in) :: line
            call grep_error_line(error, file//NUL//rest, line, file, info1)
            if (allocated(error)) return
            call grep_error_line(error, file//":"//rest, line, file, info2)
            if (allocated(error)) return
            call check(error, info1%line, info2%line, "same line")
            if (.not. allocated(error)) call check(error, info1%file, info2%file, "same file")
            if (.not. allocated(error)) call check(error, info1%type, info2%type, "same type")
        end subroutine pair
    end subroutine test_grep_regexps

    !> `wildcard_to_regexp' on shell patterns.
    subroutine test_wildcard_to_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, wildcard_to_regexp("foo"), "\`foo\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("*.el"), "\`[^"//NUL//"]*\.el\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("a?c"), "\`a[^"//NUL//"]c\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("*.[ch]"), "\`[^"//NUL//"]*\.[ch]\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("[!a]x"), "\`[^a]x\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("[^a]x"), "\`["//NUL//"^a]x\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("a+b$"), "\`a\+b\$\'")
        if (.not. allocated(error)) call check(error, wildcard_to_regexp("[]"), "\`\[]\'")
    end subroutine test_wildcard_to_regexp

    !> `grep_expand_template' substitutes the place holders once each.
    subroutine test_expand_template(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: saved_fold
        integer :: saved_highlight
        call init_bindings()
        saved_fold = case_fold_search%value
        saved_highlight = grep_highlight_matches
        case_fold_search = .true.
        grep_highlight_matches = GREP_NIL
        call check(error, grep_expand_template("grep <X> <C> -nH <R> <F>", "foo", "*.c", excl=" --exclude=x"), &
                   "grep  --exclude=x -i -nH foo *.c")
        if (.not. allocated(error)) then
            ! An upper case letter: no -i.
            call check(error, grep_expand_template("grep <C> <R> <F>", "Foo bar", "*"), "grep  Foo\ bar *")
        end if
        if (.not. allocated(error)) then
            grep_highlight_matches = GREP_ALWAYS
            call check(error, grep_expand_template("find <D> <X> -type f <F> -exec grep <C> <R> {} +", "x", "-name y", &
                                                   "/tmp/"), "find /tmp/  -type f -name y -exec grep --color=always -i x {} +")
        end if
        if (.not. allocated(error)) then
            grep_highlight_matches = GREP_AUTO
            call check(error, grep_expand_template("grep <C> <R>", "x", more_opts=[character(len=4) :: "-w"]), &
                       "grep --color=auto -i -w x")
        end if
        case_fold_search = saved_fold
        grep_highlight_matches = saved_highlight
    end subroutine test_expand_template

    !> Wait until the grep in *grep* has finished.
    subroutine wait_grep()
        type(handle_type) :: buf, proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got
        buf = get_buffer("*grep*")
        call system_clock(start, rate)
        do
            proc = get_buffer_process(buf)
            if (proc%index == 0) exit
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > TIMEOUT) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_grep

    !> `grep' runs grep in *grep*, in Grep mode, with the exit message and
    !> the matches that `next_error' visits.
    subroutine test_grep_run(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        character(len=:), allocatable :: dir, path, text
        integer(int64) :: errors, warnings, infos
        integer :: unit
        call init_bindings()
        dir = file_name_as_directory(temporary_file_directory())
        path = dir//"fortmacs-grep-test.txt"
        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, "(a)") "alpha"
        write (unit, "(a)") "beta needle"
        write (unit, "(a)") "gamma"
        write (unit, "(a)") "needle delta"
        close (unit)
        call get_buffer_create("*grep-test*", h, st)
        call set_buffer(h, st)
        buffers(cur)%directory = dir
        call show_alone()
        call set_buffer(h, st)
        call grep("grep -nH -e needle fortmacs-grep-test.txt", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_grep()
        buf = get_buffer("*grep*")
        call check(error, buffer_live_p(buf), "*grep*")
        if (allocated(error)) return
        call set_buffer(buf, st)
        call check(error, buffers(cur)%major_mode, "grep_mode")
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, "Grep started at") > 0, text)
        if (.not. allocated(error)) call check(error, index(text, "Grep finished with matches found") > 0 .or. &
                                               index(text, "Grep finished with 2 matches found") > 0, text)
        if (allocated(error)) return
        call compilation_message_counts(errors, warnings, infos)
        call check(error, errors, 2_int64, "hits")
        if (allocated(error)) return
        ! next-error visits the first match, then the second.
        call next_error(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffers(cur)%name, "fortmacs-grep-test.txt")
        if (.not. allocated(error)) call check(error, point(), 7_int64, "line 2")
        if (allocated(error)) return
        call next_error(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, point(), 25_int64, "line 4")
        if (allocated(error)) return
        call next_error(st)
        call check(error,.not. st%ok(), "past last hit")
        if (.not. allocated(error)) call check(error, format_error(st), "Past last grep hit")
        call kill_buffer(get_buffer("fortmacs-grep-test.txt"), st)
        call set_buffer_read_only(buf, .false., st)
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
        open (newunit=unit, file=path, status="old")
        close (unit, status="delete")
    end subroutine test_grep_run

end module test_grep_fm
