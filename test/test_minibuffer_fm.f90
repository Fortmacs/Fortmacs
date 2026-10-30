! test_minibuffer_fm.f90 -- tests for the completion styles of minibuffer_fm.
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

!> Ports the minibuffer-tests.el style tests that go through a style's
!> try and all functions (run here as the only entry of
!> `completion_styles'): completion-test--pcm-bug38458,
!> completion-pcm-test-1 to 4, 6 to 8, -anydelim, -bug4219,
!> completion-substring-test-1 to 3 and 5, completion-flex-test-1 and 3.
!> Not ported: the tests of internal functions (completion-pcm--merge-try,
!> --optimize-pattern, --pattern->regex), of completions-first-difference
!> (no faces on strings) and of completion-initials-expand on the file
!> system.  The rest are own tests.
module test_minibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_string, make_list, list_length, list_elements, xstring
    use buffer_fm, only: get_buffer_create, kill_buffer
    use minibuf_fm, only: completion_table_type, completion_list_table, completion_function_table, &
                          completion_ignore_case
    use minibuffer_fm, only: completion_try_completion, completion_all_completions, try_result_type, TRY_STRING, &
                             TRY_NONE, completion_styles, internal_complete_buffer, completion_at_point_functions
    use minibuf_table_fm, only: capf_data_type
    use test_support, only: show_alone
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, command_loop
    use bindings_fm, only: init_bindings
    use buffer_fm, only: set_buffer, buffers, cur
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, point, goto_char
    use modes_fm, only: minor_modes
    use window_fm, only: get_buffer_window, window_live_p
    use buffer_fm, only: get_buffer
    implicit none(type, external)
    private

    public :: collect_minibuffer_fm

contains

    subroutine collect_minibuffer_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("completion-test--pcm-bug38458", test_pcm_bug38458), &
                    new_unittest("completion-pcm-test-1", test_pcm_1), &
                    new_unittest("completion-pcm-test-2", test_pcm_2), &
                    new_unittest("completion-pcm-test-3", test_pcm_3), &
                    new_unittest("completion-pcm-test-4", test_pcm_4), &
                    new_unittest("completion-pcm-test-6", test_pcm_6), &
                    new_unittest("completion-pcm-test-7", test_pcm_7), &
                    new_unittest("completion-pcm-test-8", test_pcm_8), &
                    new_unittest("completion-pcm-test-anydelim", test_pcm_anydelim), &
                    new_unittest("completion-pcm-bug4219", test_pcm_bug4219), &
                    new_unittest("completion-substring-test-1", test_substring_1), &
                    new_unittest("completion-substring-test-2", test_substring_2), &
                    new_unittest("completion-substring-test-3", test_substring_3), &
                    new_unittest("completion-substring-test-5", test_substring_5), &
                    new_unittest("completion-flex-test-1", test_flex_1), &
                    new_unittest("completion-flex-test-3", test_flex_3), &
                    new_unittest("flex-non-ascii", test_flex_non_ascii), &
                    new_unittest("default-styles-pcm", test_default_styles), &
                    new_unittest("buffer-category-substring", test_buffer_category), &
                    new_unittest("completion-at-point", test_completion_at_point), &
                    new_unittest("completion-at-point-non-exclusive", test_capf_non_exclusive) &
                    ]
    end subroutine collect_minibuffer_fm

    !> A completion table of the strings NAMES (trailing blanks dropped).
    function table_of(names) result(table)
        character(len=*), intent(in) :: names(:)
        type(completion_table_type) :: table
        type(value_type), allocatable :: elts(:)
        integer :: k

        allocate (elts(size(names)))

        do k = 1, size(names)
            elts(k) = make_string(trim(names(k)))
        end do

        table = completion_list_table(make_list(elts))
    end function table_of

    !> STYLE's try-completion of STRING with point at POINT over NAMES.
    subroutine style_try(style, string, names, point, res, ignore_case)
        character(len=*), intent(in) :: style, string
        character(len=*), intent(in) :: names(:)
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        logical, intent(in), optional :: ignore_case
        character(len=24), allocatable :: saved(:)
        logical :: saved_case

        call completion_all_defaults()
        saved = completion_styles
        saved_case = completion_ignore_case%value
        if (present(ignore_case)) completion_ignore_case%value = ignore_case
        completion_styles = [character(len=24) :: style]
        call completion_try_completion(string, table_of(names), point, res)
        completion_styles = saved
        completion_ignore_case%value = saved_case
    end subroutine style_try

    !> STYLE's all-completions of STRING with point at POINT over NAMES:
    !> the number of them and the first.
    subroutine style_all(style, string, names, point, n, first)
        character(len=*), intent(in) :: style, string
        character(len=*), intent(in) :: names(:)
        integer(int64), intent(in) :: point
        integer, intent(out) :: n
        character(len=:), allocatable, intent(out) :: first
        character(len=24), allocatable :: saved(:)
        type(value_type) :: all
        type(value_type), allocatable :: elts(:)
        integer(int64) :: base_size

        call completion_all_defaults()
        saved = completion_styles
        completion_styles = [character(len=24) :: style]
        call completion_all_completions(string, table_of(names), point, all, base_size)
        completion_styles = saved
        call list_elements(all, elts)
        n = size(elts)
        first = ""
        if (n > 0) first = xstring(elts(1))
    end subroutine style_all

    !> Make sure `completion_styles' has its default.
    subroutine completion_all_defaults()
        type(value_type) :: all
        integer(int64) :: base_size

        if (.not. allocated(completion_styles)) then
            call completion_all_completions("", table_of([character(len=1) :: "a"]), 0_int64, all, base_size)
        end if
    end subroutine completion_all_defaults

    !> Check that RES is (STRING . POINT).
    subroutine check_try(error, res, string, point, what)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type), intent(in) :: res
        character(len=*), intent(in) :: string, what
        integer(int64), intent(in) :: point

        call check(error, res%kind == TRY_STRING, what//": a string")
        if (allocated(error)) return
        call check(error, res%string == string .and. len(res%string) == len(string), &
                   what//": got "//res%string//", expected "//string)
        if (allocated(error)) return
        call check(error, res%point == point, what//": point")
    end subroutine check_try

    subroutine test_pcm_bug38458(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        call style_try("partial-completion", "tes", [character(len=7) :: "Testing", "testing"], 3_int64, res, .true.)
        call check_try(error, res, "testing", 7_int64, "tes")
    end subroutine test_pcm_bug38458

    subroutine test_pcm_1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        ! Point is at end, this does not match anything.
        call style_all("partial-completion", "foo", [character(len=9) :: "hello", "world", "barfoobar"], 3_int64, &
                       n, first)
        call check(error, n == 0, "no match")
    end subroutine test_pcm_1

    subroutine test_pcm_2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        ! Point is at beginning, this matches "barfoobar".
        call style_all("partial-completion", "foo", [character(len=9) :: "hello", "world", "barfoobar"], 0_int64, &
                       n, first)
        call check(error, first == "barfoobar", "got "//first)
    end subroutine test_pcm_2

    subroutine test_pcm_3(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        call style_all("partial-completion", "R", [character(len=5) :: "R", "hello"], 1_int64, n, first)
        call check(error, n > 0, "a match")
    end subroutine test_pcm_3

    subroutine test_pcm_4(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        ! No match due to point being at the end.
        call style_all("partial-completion", "RO", [character(len=4) :: "RaOb"], 2_int64, n, first)
        call check(error, n == 0, "no match")
    end subroutine test_pcm_4

    subroutine test_pcm_6(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        ! Wildcards and delimiters work.
        call style_all("partial-completion", "li-pac*", [character(len=13) :: "list-packages"], 7_int64, n, first)
        call check(error, first == "list-packages", "got "//first)
        if (allocated(error)) return
        call style_all("partial-completion", "li-pac*", [character(len=20) :: "do-not-list-packages"], 7_int64, &
                       n, first)
        call check(error, n == 0, "anchored")
    end subroutine test_pcm_6

    subroutine test_pcm_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        ! Wildcards are preserved even when right before a delimiter.
        call style_try("partial-completion", "x*/", [character(len=5) :: "x1/y1", "x2/y2"], 3_int64, res)
        call check_try(error, res, "x*/y", 4_int64, "x*/")
        if (allocated(error)) return
        ! This is important if the wildcard is at the start of a component.
        call style_try("partial-completion", "*/minibuf", [character(len=18) :: "lisp/minibuffer.el", "src/minibuf.c"], &
                       9_int64, res)
        call check_try(error, res, "*/minibuf", 9_int64, "*/minibuf")
    end subroutine test_pcm_7

    subroutine test_pcm_8(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        ! try-completion inserts the common prefix and suffix at point.
        call style_try("partial-completion", "r", [character(len=7) :: "fooxbar", "fooybar"], 0_int64, res)
        call check_try(error, res, "foobar", 3_int64, "r")
        if (allocated(error)) return
        ! Even if point is at the end of the minibuffer.
        call style_try("partial-completion", "", [character(len=7) :: "fooxbar", "fooybar"], 0_int64, res)
        call check_try(error, res, "foobar", 3_int64, "empty")
    end subroutine test_pcm_8

    subroutine test_pcm_anydelim(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        ! After each delimiter is a special wildcard which matches any
        ! sequence of delimiters.
        call style_try("partial-completion", "-x", [character(len=4) :: "-_.x", "-__x"], 2_int64, res)
        call check_try(error, res, "-_x", 3_int64, "-x")
    end subroutine test_pcm_anydelim

    subroutine test_pcm_bug4219(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        ! With `completion_ignore_case', try-completion should change the
        ! case of existing text when the completions have different
        ! casing.
        call style_try("partial-completion", "a", [character(len=3) :: "ABC", "ABD"], 1_int64, res, .true.)
        call check_try(error, res, "AB", 2_int64, "a")
        if (allocated(error)) return
        ! Even when the text isn't growing.
        call style_try("partial-completion", "ab", [character(len=3) :: "ABC", "ABD"], 2_int64, res, .true.)
        call check_try(error, res, "AB", 2_int64, "ab")
        if (allocated(error)) return
        ! Or when point is in the middle of the region changing case.
        call style_try("partial-completion", "ab", [character(len=3) :: "ABC", "ABD"], 1_int64, res, .true.)
        call check_try(error, res, "AB", 2_int64, "ab at 1")
        if (allocated(error)) return
        ! Even when the existing minibuffer contents has mixed case.
        call style_try("partial-completion", "Ab", [character(len=3) :: "ABC", "ABD"], 1_int64, res, .true.)
        call check_try(error, res, "AB", 2_int64, "Ab")
        if (allocated(error)) return
        ! But not if the completions don't actually all have the same
        ! case.
        call style_try("partial-completion", "Ab", [character(len=3) :: "abc", "ABD"], 1_int64, res, .true.)
        call check_try(error, res, "Ab", 2_int64, "Ab mixed")
    end subroutine test_pcm_bug4219

    subroutine test_substring_1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        call style_all("substring", "foo", [character(len=9) :: "hello", "world", "barfoobar"], 3_int64, n, first)
        call check(error, first == "barfoobar", "got "//first)
    end subroutine test_substring_1

    subroutine test_substring_2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        call style_all("substring", "R", [character(len=5) :: "R", "hello"], 1_int64, n, first)
        call check(error, n > 0, "a match")
    end subroutine test_substring_2

    subroutine test_substring_3(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        ! Substring match.
        call style_all("substring", "custgroup", [character(len=15) :: "customize-group"], 4_int64, n, first)
        call check(error, first == "customize-group", "got "//first)
        if (allocated(error)) return
        call style_all("substring", "custgroup", [character(len=15) :: "customize-group"], 5_int64, n, first)
        call check(error, n == 0, "no match")
    end subroutine test_substring_3

    subroutine test_substring_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        ! substring completion should successfully complete the entire
        ! string.
        call style_try("substring", "b", [character(len=2) :: "ab", "ab"], 0_int64, res)
        call check_try(error, res, "ab", 2_int64, "b")
    end subroutine test_substring_5

    subroutine test_flex_1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        call style_all("flex", "foo", [character(len=9) :: "hello", "world", "fabrobazo"], 3_int64, n, first)
        call check(error, first == "fabrobazo", "got "//first)
    end subroutine test_flex_1

    subroutine test_flex_3(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        call style_all("flex", "custgroup", [character(len=28) :: "customize-group-other-window"], 4_int64, n, first)
        call check(error, first == "customize-group-other-window", "got "//first)
    end subroutine test_flex_3

    !> Flex matching counts characters, not bytes.
    subroutine test_flex_non_ascii(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: first
        integer :: n

        call style_all("flex", "日本", [character(len=9) :: "日本語", "日本", "中国"], 2_int64, n, first)
        call check(error, n == 2, "two matches")
        if (allocated(error)) return
        call style_all("flex", "tsk", [character(len=18) :: "図書館-toshokan", "task", "desk"], 3_int64, n, first)
        call check(error, n == 2, "toshokan and task")
    end subroutine test_flex_non_ascii

    !> The default styles: basic finds nothing, partial-completion
    !> completes each word.
    subroutine test_default_styles(error)
        type(error_type), allocatable, intent(out) :: error
        type(try_result_type) :: res

        call completion_all_defaults()
        call completion_try_completion("l-p", table_of([character(len=13) :: "list-packages", "load-file"]), 3_int64, res)
        call check_try(error, res, "list-packages", 13_int64, "l-p")
        if (allocated(error)) return
        call completion_try_completion("zz", table_of([character(len=13) :: "list-packages"]), 2_int64, res)
        call check(error, res%kind == TRY_NONE, "no match")
    end subroutine test_default_styles

    !> Buffer names complete with the basic and substring styles first.
    subroutine test_buffer_category(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: all
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: base_size

        call get_buffer_create("xyzzy-category-test", buf, st)
        call completion_all_completions("zzy-cat", completion_function_table(internal_complete_buffer, "buffer"), &
                                        7_int64, all, base_size)
        call check(error, list_length(all) == 1, "substring match")
        call kill_buffer(buf, st)
    end subroutine test_buffer_category

    !> A completion-at-point function completing the letters and digits
    !> before point from foo1, foo2 and bar.
    recursive subroutine test_capf(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer(int64) :: start

        status = status_type()
        text = buffer_string()
        start = point()

        do while (start > 1)
            if (index("abcdefghijklmnopqrstuvwxyz0123456789", text(start - 1:start - 1)) == 0) exit
            start = start - 1
        end do

        data%found = .true.
        data%start = start
        data%end = point()
        data%table = table_of([character(len=4) :: "foo1", "foo2", "bar"])
    end subroutine test_capf

    !> A non-exclusive completion-at-point function offering zzz.
    recursive subroutine test_capf_zzz(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status

        status = status_type()
        call test_capf(data, status)
        data%table = table_of([character(len=3) :: "zzz"])
        data%exclusive = .false.
    end subroutine test_capf_zzz

    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    subroutine capf_buffer(buf, text)
        type(handle_type), intent(out) :: buf
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_buffer_create("capf-test", buf, st)
        call set_buffer(buf, st)
        call show_alone()
        call insert(text, st)
    end subroutine capf_buffer

    !> C-M-i completes the text before point, then shows *Completions*
    !> in completion-in-region-mode, which ends once point leaves the
    !> field.
    subroutine test_completion_at_point(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, comp
        type(status_type) :: st

        call capf_buffer(buf, "x fo")
        call completion_at_point_functions%add(test_capf, local=.true.)
        call type_keys("C-M-i")
        call command_loop(st)
        call check(error, buffer_string() == "x foo", buffer_string())
        if (allocated(error)) return
        call check(error,.not. minor_modes%completion_in_region_mode%enabled(), "mode off")
        if (allocated(error)) return
        call type_keys("C-M-i")
        call command_loop(st)
        call check(error, minor_modes%completion_in_region_mode%enabled(), "mode on")
        if (allocated(error)) return
        comp = get_buffer("*Completions*")
        call check(error, window_live_p(get_buffer_window(comp)), "*Completions* shown")
        if (allocated(error)) return
        call type_keys("C-a")
        call command_loop(st)
        call check(error,.not. minor_modes%completion_in_region_mode%enabled(), "mode off after C-a")
        if (allocated(error)) return
        call check(error,.not. window_live_p(get_buffer_window(comp)), "*Completions* hidden")
        call kill_buffer(buf, st)
        call show_alone()
    end subroutine test_completion_at_point

    !> A non-exclusive function whose table does not match lets the next
    !> one complete.
    subroutine test_capf_non_exclusive(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st

        call capf_buffer(buf, "x b")
        call completion_at_point_functions%add(test_capf, local=.true.)
        call completion_at_point_functions%add(test_capf_zzz, local=.true.)
        call type_keys("C-M-i")
        call command_loop(st)
        call check(error, buffer_string() == "x bar", buffer_string())
        if (allocated(error)) return
        call erase_all()
        call insert("x z", st)
        call type_keys("C-M-i")
        call command_loop(st)
        call check(error, buffer_string() == "x zzz", buffer_string())
        call kill_buffer(buf, st)
        call show_alone()

    contains

        subroutine erase_all()
            use buffer_fm, only: erase_buffer
            type(status_type) :: st2

            call erase_buffer(st2)
        end subroutine erase_all
    end subroutine test_capf_non_exclusive
end module test_minibuffer_fm
