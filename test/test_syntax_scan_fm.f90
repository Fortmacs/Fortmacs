! test_syntax_scan_fm.f90 -- tests for comments, lists and sexps
! (syntax_scan_fm) and the sexp commands (lisp_cmds_fm).
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

!> Ports syntax-tests.el: parse-partial-sexp-continue-over-comment-marker,
!> parse-partial-sexp-paren-comments, test-from-to-parse-partial-sexp and
!> every syntax-comments, syntax-br-comments and syntax-pps-comments test
!> over test/syntax-resources/syntax-comments.txt (one test-drive test per
!> section and kind; a failure names the ERT test, e.g.
!> syntax-comments-c-b13); and lisp-tests.el's lisp-forward-sexp-* and
!> lisp-backward-sexp-* tests.  Own tests for nested comments, two-character
!> delimiters, scan errors, `parse_partial_sexp' arguments and the sexp
!> commands.
module test_syntax_scan_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, FM_SCAN_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, t_value
    use lread_fm, only: sym
    use data_fm, only: set
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_string, mark_position, bobp, eobp
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, command_loop
    use bindings_fm, only: init_bindings
    use syntax_fm, only: modify_syntax_entry, make_syntax_table, set_syntax_table, free_syntax_table, &
                         standard_syntax_table, syntax_table_live_p, forward_comment, scan_lists, scan_sexps, parse_partial_sexp, &
                         parse_state_type, scan_error_data, syntax_ppss, parse_sexp_ignore_comments, &
                         comment_end_can_be_escaped, COMMENTSTOP_SYNTAX_TABLE, COMMENTSTOP_COMMENT, ST_STRING_STYLE
    use lisp_cmds_fm, only: forward_sexp, backward_sexp, up_list, backward_up_list, down_list, forward_list, &
                            kill_sexp
    implicit none(type, external)
    private

    public :: collect_syntax_scan_fm

    !> STOP absent in a syntax-comments case (the label START is used).
    integer, parameter :: NONE = 1000000

    !> Comment syntaxes of syntax-tests.el: Pascal, Lisp, nested Lisp, C.
    integer, parameter :: PASCAL = 1, LISP = 2, LISP_N = 3, C_STYLE = 4

contains

    subroutine collect_syntax_scan_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("parse-partial-sexp-continue-over-comment-marker", test_continue_over_marker), &
                    new_unittest("parse-partial-sexp-paren-comments", test_paren_comments), &
                    new_unittest("test-from-to-parse-partial-sexp", test_from_to), &
                    new_unittest("syntax-comments-pascal", test_comments_pascal), &
                    new_unittest("syntax-comments-lisp", test_comments_lisp), &
                    new_unittest("syntax-br-comments-lisp", test_br_comments_lisp), &
                    new_unittest("syntax-pps-comments-lisp", test_pps_comments_lisp), &
                    new_unittest("syntax-comments-lisp-n", test_comments_lisp_n), &
                    new_unittest("syntax-comments-c", test_comments_c), &
                    new_unittest("syntax-br-comments-c", test_br_comments_c), &
                    new_unittest("syntax-pps-comments-c", test_pps_comments_c), &
                    new_unittest("lisp-forward-sexp", test_lisp_forward_sexp), &
                    new_unittest("nested-comments", test_nested_comments), &
                    new_unittest("c-comments", test_c_comments), &
                    new_unittest("scan-lists-and-errors", test_scan_lists), &
                    new_unittest("scan-sexps", test_scan_sexps), &
                    new_unittest("parse-partial-sexp-state", test_pps_state), &
                    new_unittest("comment-end-can-be-escaped-variable", test_escaped_variable), &
                    new_unittest("sexp-commands", test_sexp_commands), &
                    new_unittest("sexp-keys", test_sexp_keys) &
                    ]
    end subroutine collect_syntax_scan_fm

    !> A fresh buffer holding TEXT, point at its start, with a new syntax
    !> table (inheriting from the standard one) as its table; the previous
    !> TABLE is freed.
    subroutine temp_buffer(text, table)
        character(len=*), intent(in) :: text
        type(handle_type), intent(inout) :: table
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_buffer_create(" *syntax-scan-test*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%mark_active = .false.
        buffers(cur)%syntax_table = handle_type()
        call erase_buffer(st)
        call insert(text, st)
        call goto_char(point_min())
        if (syntax_table_live_p(table)) call free_syntax_table(table, st)
        call make_syntax_table(table, st)
        call set_syntax_table(table, st)
        call show_alone()
    end subroutine temp_buffer

    !> Undo temp_buffer's table and the settings the tests change.
    subroutine done_buffer(table)
        type(handle_type), intent(in) :: table
        type(status_type) :: st

        call set_syntax_table(standard_syntax_table(), st)
        call free_syntax_table(table, st)
        parse_sexp_ignore_comments = .false.
        comment_end_can_be_escaped = .false.
    end subroutine done_buffer

    subroutine entry(c, descriptor, table)
        character, intent(in) :: c
        character(len=*), intent(in) :: descriptor
        type(handle_type), intent(in) :: table
        type(status_type) :: st

        call modify_syntax_entry(iachar(c), iachar(c), descriptor, st, table)
    end subroutine entry

    !> Whether two parse states are `equal'.
    logical function same_state(a, b)
        type(parse_state_type), intent(in) :: a, b

        same_state = a%depth == b%depth .and. a%innermost_start == b%innermost_start .and. &
                     a%last_sexp_start == b%last_sexp_start .and. a%instring == b%instring .and. &
                     a%incomment == b%incomment .and. (a%quoted .eqv. b%quoted) .and. a%mindepth == b%mindepth .and. &
                     a%comment_style == b%comment_style .and. a%comstr_start == b%comstr_start .and. &
                     a%prev_syntax == b%prev_syntax
        if (.not. same_state) return
        same_state = size(a%open_parens) == size(b%open_parens)
        if (same_state) same_state = all(a%open_parens == b%open_parens)
    end function same_state

    character(len=24) function str(n)
        integer(int64), intent(in) :: n

        write (str, "(i0)") n
    end function str

    ! ------------------------------------------------------------------
    ! syntax-tests.el
    ! ------------------------------------------------------------------
    !> Continue a parse that stopped in the middle of a comment marker.
    !> (Upstream modifies the standard table by mistake, then installs a
    !> table inheriting from it; here the new table gets the entries.)
    subroutine test_continue_over_marker(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: pps_c, pps_pre_c, pps_aft_c, pps_x, s
        integer(int64) :: point_c, pre_c, point_x, aft_c

        call temp_buffer("/*C*/"//achar(10)//"X", table)
        call entry("/", ". 124", table)
        call entry("*", ". 23b", table)
        point_c = 3
        pre_c = point_c - 1
        point_x = 7
        aft_c = point_c + 2
        call parse_partial_sexp(point_min(), point_c, pps_c, st)
        call parse_partial_sexp(point_min(), pre_c, pps_pre_c, st)
        call parse_partial_sexp(point_min(), aft_c, pps_aft_c, st)
        call parse_partial_sexp(point_min(), point_x, pps_x, st)
        ! C should be inside comment.
        call check(error, pps_c%depth == 0 .and. pps_c%incomment == -1 .and. pps_c%comstr_start == point_c - 2)
        if (allocated(error)) return
        ! X should not be in comment or list.
        call check(error, pps_x%depth == 0 .and. pps_x%incomment == 0)
        if (allocated(error)) return
        ! Try using OLDSTATE.
        call parse_partial_sexp(pre_c, point_c, s, st, oldstate=pps_pre_c)
        call check(error, same_state(s, pps_c), "preC to pointC")
        if (allocated(error)) return
        call parse_partial_sexp(point_c, aft_c, s, st, oldstate=pps_c)
        call check(error, same_state(s, pps_aft_c), "pointC to aftC")
        if (allocated(error)) return
        call parse_partial_sexp(pre_c, aft_c, s, st, oldstate=pps_pre_c)
        call check(error, same_state(s, pps_aft_c), "preC to aftC")
        if (allocated(error)) return
        call parse_partial_sexp(aft_c, point_x, s, st, oldstate=pps_aft_c)
        call check(error, same_state(s, pps_x), "aftC to pointX")
        call done_buffer(table)
    end subroutine test_continue_over_marker

    !> Test syntax parsing with paren comment markers.  Specifically, where
    !> the first character of the comment marker is also has open paren
    !> syntax (see Bug#24870).
    subroutine test_paren_comments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: pps_c, pps_x, s
        integer(int64) :: point_c, point_x

        call temp_buffer("{-C-}"//achar(10)//"X", table)
        call entry("{", "(}1nb", table)
        call entry("}", "){4nb", table)
        call entry("-", ". 123", table)
        point_c = 3
        point_x = 7
        call parse_partial_sexp(point_min(), point_c, pps_c, st)
        call parse_partial_sexp(point_min(), point_x, pps_x, st)
        ! C should be inside nestable comment, not list.
        call check(error, pps_c%depth == 0 .and. pps_c%incomment == 1 .and. pps_c%comstr_start == point_c - 2)
        if (allocated(error)) return
        ! X should not be in comment or list.
        call check(error, pps_x%depth == 0 .and. pps_x%incomment == 0)
        if (allocated(error)) return
        ! Try using OLDSTATE.
        call parse_partial_sexp(point_c, point_x, s, st, oldstate=pps_c)
        call check(error, same_state(s, pps_x))
        call done_buffer(table)
    end subroutine test_paren_comments

    subroutine test_from_to(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: s

        call temp_buffer("foo", table)
        call parse_partial_sexp(1_int64, 1_int64, s, st)
        call check(error, st%ok())
        if (allocated(error)) return
        call parse_partial_sexp(2_int64, 1_int64, s, st)
        call check(error,.not. st%ok() .and. format_error(st) == "End position is smaller than start position")
        call done_buffer(table)
    end subroutine test_from_to

    ! ------------------------------------------------------------------
    ! The comment tests over syntax-comments.txt
    ! ------------------------------------------------------------------
    !> Visit syntax-comments.txt (in a buffer of its own, with a new
    !> syntax table, as its Local Variables make it) and set up the entries
    !> of comment syntax KIND (the -in functions of syntax-tests.el).
    subroutine visit_comments_file(kind, table, error)
        integer, intent(in) :: kind
        type(handle_type), intent(inout) :: table
        type(error_type), allocatable, intent(inout) :: error
        character(len=:), allocatable :: data
        integer :: u, ios
        integer(int64) :: fsize

        open (newunit=u, file="test/syntax-resources/syntax-comments.txt", access="stream", form="unformatted", &
              status="old", action="read", iostat=ios)
        call check(error, ios == 0, "can't open syntax-comments.txt")
        if (allocated(error)) return
        inquire (unit=u, size=fsize)
        allocate (character(len=fsize) :: data)
        read (u, iostat=ios) data
        close (u)
        call check(error, ios == 0, "can't read syntax-comments.txt")
        if (allocated(error)) return
        call temp_buffer(data, table)
        parse_sexp_ignore_comments = .true.

        select case (kind)
        case (PASCAL)
            comment_end_can_be_escaped = .false.
            call entry("{", "<", table)
            call entry("}", ">", table)
        case (LISP)
            comment_end_can_be_escaped = .false.
            call entry(achar(10), ">", table)
            call entry(";", "<", table)
            call entry("{", ".", table)
            call entry("}", ".", table)
        case (LISP_N)
            call entry("#", ". 14", table)
            call entry("|", ". 23n", table)
            call entry(";", "< b", table)
            call entry(achar(10), "> b", table)
        case (C_STYLE)
            comment_end_can_be_escaped = .true.
            call entry("/", ". 124b", table)
            call entry("*", ". 23", table)
            call entry(achar(10), "> b", table)
        case default
            continue
        end select
    end subroutine visit_comments_file

    !> The buffer offset corresponding to the "label" N.  N is a decimal
    !> number which appears in the data file, usually twice, as "labels".
    !> FORW is true when we're using the label at BOL, false for the one
    !> at EOL.  With FORW and N positive, the position after the first
    !> occurrence of label N at BOL; with FORW false, the position before
    !> the last occurrence of the label at EOL.  When N is negative, the
    !> position of the end of line that the -N label is on; when it is
    !> zero, point.  0 if there is no such label.  Port of
    !> syntax-tests.el:syntax-comments-point.
    integer(int64) function label_point(n, forw) result(pos)
        integer(int64), intent(in) :: n
        logical, intent(in) :: forw
        character(len=:), allocatable :: text, s
        integer :: i, l

        pos = 0

        if (n == 0) then
            pos = point()
            return
        end if

        text = buffer_string()
        s = trim(str(abs(n)))
        l = len(s)

        if (forw) then
            do i = 1, len(text) - l + 1
                if (i > 1) then
                    if (text(i - 1:i - 1) /= achar(10)) cycle
                end if

                if (text(i:i + l - 1) /= s) cycle

                if (i + l <= len(text)) then
                    if (index("0123456789", text(i + l:i + l)) > 0) cycle
                end if

                pos = i + l
                exit
            end do
        else
            do i = len(text) - l + 1, 1, -1
                if (text(i:i + l - 1) /= s) cycle

                if (i + l <= len(text)) then
                    if (text(i + l:i + l) /= achar(10)) cycle
                end if

                if (i > 1) then
                    if (index("0123456789", text(i - 1:i - 1)) > 0) cycle
                end if

                pos = i
                exit
            end do
        end if

        if (pos == 0 .or. n > 0) return
        ! (end-of-line)
        i = index(text(pos:), achar(10))

        if (i == 0) then
            pos = len(text) + 1
        else
            pos = pos + i - 1
        end if
    end function label_point

    !> The position just before the label N, which should appear in the
    !> buffer exactly once, not necessarily at the beginning or end of a
    !> line.  Port of syntax-tests.el:syntax-comments-midpoint.
    integer(int64) function label_midpoint(n) result(pos)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: text, s
        integer :: i, l

        pos = 0
        text = buffer_string()
        s = trim(str(n))
        l = len(s)

        do i = 1, len(text) - l + 1
            if (text(i:i + l - 1) /= s) cycle

            if (i > 1) then
                if (index("0123456789", text(i - 1:i - 1)) > 0) cycle
            end if

            if (i + l <= len(text)) then
                if (index("0123456789", text(i + l:i + l)) > 0) cycle
            end if

            pos = i
            exit
        end do
    end function label_midpoint

    !> The syntax-comments tests: (forward-comment 1/-1) from label START
    !> gives RES and stops at label STOP.  DIRS has an f or b per case.
    subroutine run_comments(error, kind, section, dirs, res, start, stop)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        character(len=*), intent(in) :: section, dirs
        logical, intent(in) :: res(:)
        integer, intent(in) :: start(:), stop(:)
        type(handle_type) :: table
        integer :: k
        integer(int64) :: stop_pos, last
        logical :: forw, got
        character(len=:), allocatable :: name

        call visit_comments_file(kind, table, error)
        if (allocated(error)) return

        do k = 1, size(start)
            forw = dirs(k:k) == "f"
            name = "syntax-comments-"//section//"-"//dirs(k:k)//trim(str(int(abs(start(k)), int64)))
            call goto_char(label_point(int(start(k), int64), forw))

            if (stop(k) == NONE) then
                stop_pos = label_point(int(start(k), int64),.not. forw)
            else
                stop_pos = label_point(int(stop(k), int64),.not. forw)
            end if

            if (forw) then
                got = forward_comment(1_int64)
            else
                got = forward_comment(-1_int64)
            end if

            last = point()
            call check(error, (got .eqv. res(k)) .and. last == stop_pos, name//": point "//trim(str(last))// &
                       ", expected "//trim(str(stop_pos)))
            if (allocated(error)) exit
        end do

        call done_buffer(table)
    end subroutine run_comments

    !> The syntax-br-comments tests: (scan-lists START 1/-1 0) returns
    !> label STOP if RES, else signals scan-error.
    subroutine run_br_comments(error, kind, section, dirs, res, start, stop)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        character(len=*), intent(in) :: section, dirs
        logical, intent(in) :: res(:)
        integer, intent(in) :: start(:), stop(:)
        type(handle_type) :: table
        type(status_type) :: st
        integer :: k
        integer(int64) :: start_pos, stop_pos, got
        logical :: forw
        character(len=:), allocatable :: name

        call visit_comments_file(kind, table, error)
        if (allocated(error)) return

        do k = 1, size(start)
            forw = dirs(k:k) == "f"
            name = "syntax-br-comments-"//section//"-"//dirs(k:k)//trim(str(int(abs(start(k)), int64)))
            start_pos = label_point(int(start(k), int64), forw)

            if (forw) then
                got = scan_lists(start_pos, 1_int64, 0_int64, st)
            else
                got = scan_lists(start_pos, -1_int64, 0_int64, st)
            end if

            if (res(k)) then
                if (stop(k) == NONE) then
                    stop_pos = label_point(int(start(k), int64),.not. forw)
                else
                    stop_pos = label_point(int(stop(k), int64),.not. forw)
                end if

                call check(error, st%ok() .and. got == stop_pos, name//": "//trim(str(got))//", expected "// &
                           trim(str(stop_pos)))
            else
                call check(error, st%code == FM_SCAN_ERROR, name//": no scan-error")
            end if

            if (allocated(error)) exit
        end do

        call done_buffer(table)
    end subroutine run_br_comments

    !> The syntax-pps-comments tests: parse-partial-sexp from label START
    !> stops after the comment opener (label OPEN) and closer (label
    !> CLOSE) with COMMENTSTOP `syntax-table', then at the paren matching
    !> the one at START (label STOP).
    subroutine run_pps_comments(error, kind, section, start, open, close, stop)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        character(len=*), intent(in) :: section
        integer, intent(in) :: start(:), open (:), close (:), stop(:)
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: s1, s2, s3
        integer :: k
        integer(int64) :: start_pos, open_pos, close_pos, stop_pos
        character(len=:), allocatable :: name

        call visit_comments_file(kind, table, error)
        if (allocated(error)) return

        do k = 1, size(start)
            name = "syntax-pps-comments-"//section//"-"//trim(str(int(start(k), int64)))
            start_pos = label_point(int(start(k), int64), .true.)
            open_pos = label_midpoint(int(open (k), int64))
            close_pos = label_midpoint(int(close (k), int64))

            if (stop(k) == NONE) then
                stop_pos = label_point(int(start(k), int64), .false.)
            else
                stop_pos = label_point(int(stop(k), int64), .false.)
            end if

            call parse_partial_sexp(start_pos, point_max(), s1, st, targetdepth=0_int64, &
                                    commentstop=COMMENTSTOP_SYNTAX_TABLE)
            call check(error, point() == open_pos, name//": opener at "//trim(str(point()))//", expected "// &
                       trim(str(open_pos)))
            if (allocated(error)) exit
            call parse_partial_sexp(point(), point_max(), s2, st, targetdepth=0_int64, oldstate=s1, &
                                    commentstop=COMMENTSTOP_SYNTAX_TABLE)
            call check(error, point() == close_pos, name//": closer at "//trim(str(point()))//", expected "// &
                       trim(str(close_pos)))
            if (allocated(error)) exit
            call parse_partial_sexp(point(), point_max(), s3, st, targetdepth=0_int64, oldstate=s2)
            call check(error, point() == stop_pos, name//": stop at "//trim(str(point()))//", expected "// &
                       trim(str(stop_pos)))
            if (allocated(error)) exit
        end do

        call done_buffer(table)
    end subroutine run_pps_comments

    !> "Pascal" style comments - single character delimiters, the closing
    !> delimiter not being newline.
    subroutine test_comments_pascal(error)
        type(error_type), allocatable, intent(out) :: error

        call run_comments(error, PASCAL, "pascal", "fbfbfbfbfbfb", &
                          [.false., .false., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true.], &
                          [20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 26, 26], &
                          [0, 0, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE])
    end subroutine test_comments_pascal

    !> "Lisp" style comments - single character opening delimiters on line
    !> comments.
    subroutine test_comments_lisp(error)
        type(error_type), allocatable, intent(out) :: error

        call run_comments(error, LISP, "lisp", "bfbfbfb", [.false., .true., .true., .true., .true., .true., .true.], &
                          [30, 31, 31, 32, 32, 33, 33], [30, NONE, NONE, NONE, NONE, NONE, NONE])
    end subroutine test_comments_lisp

    !> "Lisp" style comments inside lists.
    subroutine test_br_comments_lisp(error)
        type(error_type), allocatable, intent(out) :: error

        call run_br_comments(error, LISP, "lisp", "bfbfbf", [.false., .true., .true., .true., .true., .false.], &
                             [40, 41, 41, 42, 42, 43], [NONE, NONE, NONE, NONE, NONE, NONE])
    end subroutine test_br_comments_lisp

    !> "Lisp" style comments parsed by `parse_partial_sexp'.
    subroutine test_pps_comments_lisp(error)
        type(error_type), allocatable, intent(out) :: error

        call run_pps_comments(error, LISP, "lisp", [41, 42, 43], [90, 92, 94], [91, 93, 95], [NONE, NONE, -999])
    end subroutine test_pps_comments_lisp

    !> "Lisp" style nested comments: between delimiters #|  |#; and mixed
    !> "Lisp" style (nested and unnested) comments.
    subroutine test_comments_lisp_n(error)
        type(error_type), allocatable, intent(out) :: error

        call run_comments(error, LISP_N, "lisp-n", "fbffbfbfbfbfbfbfbfb", &
                          [.false., .false., .false., .true., .true., .true., .true., .true., .true., .false., .true., &
                           .true., .true., .true., .true., .true., .true., .true., .true.], &
                          [100, 100, 101, 102, 102, 103, 103, 104, 104, 105, 105, 106, 106, 107, 107, 110, 110, 111, 111], &
                          [0, 0, -999, NONE, NONE, NONE, NONE, NONE, NONE, -999, NONE, NONE, NONE, NONE, NONE, NONE, NONE, &
                           NONE, NONE])
    end subroutine test_comments_lisp_n

    !> Emacs 27 "C" style comments - `comment_end_can_be_escaped' is true.
    subroutine test_comments_c(error)
        type(error_type), allocatable, intent(out) :: error

        call run_comments(error, C_STYLE, "c", "fbfbfbfbfbfbfbfbfbfbfbfbfb", &
                          [.true., .true., .true., .true., .true., .true., .true., .true., .true., .false., .false., &
                           .true., .true., .false., .false., .true., .true., .true., .false., .false., .true., .true., &
                           .true., .false., .true., .true.], &
                          [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 13, 13, 15, 15], &
                          [NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 6, 0, 0, 5, 8, 0, 0, 7, NONE, NONE, 0, 0, &
                           NONE, NONE, 14, -14, NONE, NONE])
    end subroutine test_comments_c

    !> Emacs 27 "C" style comments inside brace lists.
    subroutine test_br_comments_c(error)
        type(error_type), allocatable, intent(out) :: error

        call run_br_comments(error, C_STYLE, "c", "fbfbfbfbfbfbfbbfb", &
                             [.true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                              .true., .true., .true., .false., .true., .true.], &
                             [50, 50, 51, 51, 52, 52, 53, 53, 54, 54, 55, 55, 56, 58, 59, 60, 60], &
                             [NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, 20, NONE, NONE, NONE, 58, 56, NONE, NONE, &
                              NONE])
    end subroutine test_br_comments_c

    !> Emacs 27 "C" style comments parsed by `parse_partial_sexp'.
    subroutine test_pps_comments_c(error)
        type(error_type), allocatable, intent(out) :: error

        call run_pps_comments(error, C_STYLE, "c", [50, 52, 54, 56, 60], [70, 72, 74, 76, 78], [71, 73, 55, 77, 79], &
                              [NONE, NONE, 20, 58, NONE])
    end subroutine test_pps_comments_c

    ! ------------------------------------------------------------------
    ! lisp-tests.el
    ! ------------------------------------------------------------------
    !> lisp-forward-sexp-1-empty-parens, -1-error-mismatch, -1-eobp,
    !> -2-eobp, -2-eobp-and-subsequent; lisp-backward-sexp-1-empty-parens,
    !> -1-error-mismatch, -1-eobp, -2-bobp, -2-bobp-and-subsequent.
    subroutine test_lisp_forward_sexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st

        call temp_buffer("()", table)
        call forward_sexp(1_int64, st)
        call check(error, st%ok(), "lisp-forward-sexp-1-empty-parens")
        if (allocated(error)) return
        call temp_buffer("(", table)
        call forward_sexp(1_int64, st)
        call check(error,.not. st%ok(), "lisp-forward-sexp-1-error-mismatch")
        if (allocated(error)) return
        call temp_buffer("()", table)
        call goto_char(point_max())
        call forward_sexp(-1_int64, st)
        call check(error, st%ok(), "lisp-backward-sexp-1-empty-parens")
        if (allocated(error)) return
        call temp_buffer("(", table)
        call goto_char(point_max())
        call forward_sexp(-1_int64, st)
        call check(error,.not. st%ok(), "lisp-backward-sexp-1-error-mismatch")
        if (allocated(error)) return
        call temp_buffer("()", table)
        call goto_char(point_max())
        call forward_sexp(1_int64, st)
        call check(error, st%ok(), "lisp-forward-sexp-1-eobp")
        if (allocated(error)) return
        call temp_buffer("()", table)
        call forward_sexp(-1_int64, st)
        call check(error, st%ok(), "lisp-backward-sexp-1-eobp")
        if (allocated(error)) return
        call temp_buffer("()", table)
        call forward_sexp(2_int64, st)
        call check(error, st%ok() .and. eobp(), "lisp-forward-sexp-2-eobp")
        if (allocated(error)) return
        call forward_sexp(1_int64, st)
        call check(error, st%ok(), "lisp-forward-sexp-2-eobp-and-subsequent")
        if (allocated(error)) return
        call temp_buffer("()", table)
        call goto_char(point_max())
        call forward_sexp(-2_int64, st)
        call check(error, st%ok() .and. bobp(), "lisp-backward-sexp-2-bobp")
        if (allocated(error)) return
        call forward_sexp(-1_int64, st)
        call check(error, st%ok(), "lisp-backward-sexp-2-bobp-and-subsequent")
        call done_buffer(table)
    end subroutine test_lisp_forward_sexp

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------
    !> Nested comments (flag n) with two-character delimiters, and a
    !> one-character nested comment pair.
    subroutine test_nested_comments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: s
        logical :: ok

        call temp_buffer("#| a #| b |# c |# x", table)
        call entry("#", ". 14", table)
        call entry("|", ". 23n", table)
        ok = forward_comment(1_int64)
        call check(error, ok .and. point() == 18, "forward over the nested comment: "//trim(str(point())))
        if (allocated(error)) return
        ok = forward_comment(-1_int64)
        call check(error, ok .and. point() == 1, "backward over it: "//trim(str(point())))
        if (allocated(error)) return
        call parse_partial_sexp(1_int64, 9_int64, s, st)
        call check(error, s%incomment == 2 .and. s%comstr_start == 1, "nesting 2 inside the inner comment")
        if (allocated(error)) return
        call parse_partial_sexp(1_int64, 15_int64, s, st)
        call check(error, s%incomment == 1, "nesting 1 after the inner comment")
        if (allocated(error)) return
        ! One-character nestable delimiters.
        call temp_buffer("{ a { b } c } d", table)
        call entry("{", "< n", table)
        call entry("}", "> n", table)
        ok = forward_comment(1_int64)
        call check(error, ok .and. point() == 14, "{ } nested: "//trim(str(point())))
        if (allocated(error)) return
        ok = forward_comment(-1_int64)
        call check(error, ok .and. point() == 1)
        call done_buffer(table)
    end subroutine test_nested_comments

    !> A C-like table: / is ". 124b", * ". 23", newline "> b".
    subroutine test_c_comments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: s
        integer(int64) :: pos
        logical :: ok

        call temp_buffer("a /* x /* y */ b // c"//achar(10)//"d /**/ /**/ e", table)
        call entry("/", ". 124b", table)
        call entry("*", ". 23", table)
        call entry(achar(10), "> b", table)
        parse_sexp_ignore_comments = .true.
        call goto_char(2_int64)
        ! /* x /* y */ is one comment: style a comments don't nest.
        ok = forward_comment(1_int64)
        call check(error, ok .and. point() == 15, "block comment: "//trim(str(point())))
        if (allocated(error)) return
        ok = forward_comment(1_int64)
        call check(error,.not. ok .and. point() == 16, "b is no comment: "//trim(str(point())))
        if (allocated(error)) return
        call goto_char(17_int64)
        ! A // comment ends with its newline.
        ok = forward_comment(1_int64)
        call check(error, ok .and. point() == 23, "line comment: "//trim(str(point())))
        if (allocated(error)) return
        ok = forward_comment(-1_int64)
        call check(error, ok .and. point() == 18, "backward over the line comment: "//trim(str(point())))
        if (allocated(error)) return
        call goto_char(15_int64)
        ok = forward_comment(-1_int64)
        call check(error, ok .and. point() == 3, "backward over the block comment: "//trim(str(point())))
        if (allocated(error)) return
        ! Two comments in a row, with whitespace.
        call goto_char(24_int64)
        ok = forward_comment(2_int64)
        call check(error, ok .and. point() == 34, "two comments: "//trim(str(point())))
        if (allocated(error)) return
        ok = forward_comment(-2_int64)
        call check(error, ok .and. point() == 25, "two comments back: "//trim(str(point())))
        if (allocated(error)) return
        ! Sexps skip comments.
        pos = scan_sexps(24_int64, 1_int64, st)
        call check(error, st%ok() .and. pos == point_max(), "scan_sexps over comments: "//trim(str(pos)))
        if (allocated(error)) return
        ! The parse state inside each kind.
        call parse_partial_sexp(1_int64, 6_int64, s, st)
        call check(error, s%incomment == -1 .and. s%comment_style == 0 .and. s%comstr_start == 3, "in /* */")
        if (allocated(error)) return
        call parse_partial_sexp(1_int64, 20_int64, s, st)
        call check(error, s%incomment == -1 .and. s%comment_style == 1 .and. s%comstr_start == 18, "in //")
        if (allocated(error)) return
        ! Stopping between / and * leaves the first character's syntax.
        call parse_partial_sexp(1_int64, 4_int64, s, st)
        call check(error, s%incomment == 0 .and. s%prev_syntax >= 0, "after the /")
        if (allocated(error)) return
        ! COMMENTSTOP t stops after the start of a comment.
        call parse_partial_sexp(1_int64, point_max(), s, st, commentstop=COMMENTSTOP_COMMENT)
        call check(error, point() == 5 .and. s%incomment == -1, "commentstop: "//trim(str(point())))
        call done_buffer(table)
    end subroutine test_c_comments

    !> scan-lists over lists and its two errors; prefix characters; strings.
    subroutine test_scan_lists(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        integer(int64) :: pos, beg, end
        character(len=:), allocatable :: message

        call temp_buffer("(a (b) ""c)"" d)", table)
        pos = scan_lists(1_int64, 1_int64, 0_int64, st)
        call check(error, st%ok() .and. pos == 15, "forward over the list: "//trim(str(pos)))
        if (allocated(error)) return
        pos = scan_lists(15_int64, -1_int64, 0_int64, st)
        call check(error, st%ok() .and. pos == 1)
        if (allocated(error)) return
        ! Up one level from inside (b): the end of the outer list.
        pos = scan_lists(3_int64, 1_int64, 1_int64, st)
        call check(error, st%ok() .and. pos == 15, "up: "//trim(str(pos)))
        if (allocated(error)) return
        ! Down one level.
        pos = scan_lists(1_int64, 1_int64, -1_int64, st)
        call check(error, st%ok() .and. pos == 2, "down: "//trim(str(pos)))
        if (allocated(error)) return
        ! Between lists at the end: nil.
        pos = scan_lists(15_int64, 1_int64, 0_int64, st)
        call check(error, st%ok() .and. pos == 0)
        if (allocated(error)) return
        call temp_buffer("(a", table)
        pos = scan_lists(1_int64, 1_int64, 0_int64, st)
        call check(error, st%code == FM_SCAN_ERROR .and. pos == 0)
        if (allocated(error)) return
        call check(error, format_error(st) == "Scan error: ""Unbalanced parentheses"", 1, 3", format_error(st))
        if (allocated(error)) return
        call scan_error_data(st, message, beg, end)
        call check(error, message == "Unbalanced parentheses" .and. beg == 1 .and. end == 3)
        if (allocated(error)) return
        call temp_buffer("a)", table)
        pos = scan_lists(1_int64, 1_int64, 0_int64, st)
        call check(error, format_error(st) == "Scan error: ""Containing expression ends prematurely"", 2, 3", &
                   format_error(st))
        call done_buffer(table)
    end subroutine test_scan_lists

    !> scan-sexps over words, strings, escapes and expression prefixes.
    subroutine test_scan_sexps(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        integer(int64) :: pos

        call temp_buffer("foo-bar ""x\""y"" 'baz a\ b", table)
        call entry("'", "'", table)
        pos = scan_sexps(1_int64, 1_int64, st)
        call check(error, st%ok() .and. pos == 8, "a symbol: "//trim(str(pos)))
        if (allocated(error)) return
        pos = scan_sexps(8_int64, 1_int64, st)
        call check(error, st%ok() .and. pos == 15, "a string with an escaped quote: "//trim(str(pos)))
        if (allocated(error)) return
        pos = scan_sexps(15_int64, 1_int64, st)
        call check(error, st%ok() .and. pos == 20, "a prefixed symbol: "//trim(str(pos)))
        if (allocated(error)) return
        pos = scan_sexps(20_int64, 1_int64, st)
        call check(error, st%ok() .and. pos == 25, "an escaped space joins: "//trim(str(pos)))
        if (allocated(error)) return
        pos = scan_sexps(25_int64, -1_int64, st)
        call check(error, st%ok() .and. pos == 21, "back over a\ b: "//trim(str(pos)))
        if (allocated(error)) return
        ! forward-sexp backward skips the prefix too (backward-prefix-chars).
        call goto_char(20_int64)
        call forward_sexp(-1_int64, st)
        call check(error, st%ok() .and. point() == 16, "back over 'baz: "//trim(str(point())))
        call done_buffer(table)
    end subroutine test_scan_sexps

    !> The components of the parse state, TARGETDEPTH and STOPBEFORE.
    subroutine test_pps_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(parse_state_type) :: s, s2

        call temp_buffer("(a (b ""c\", table)
        call parse_partial_sexp(1_int64, point_max(), s, st)
        call check(error, st%ok() .and. s%depth == 2 .and. s%innermost_start == 4 .and. s%instring == iachar("""") &
                   .and. s%comstr_start == 7 .and. s%quoted .and. s%mindepth == 0, "in a string after an escape")
        if (allocated(error)) return
        call check(error, size(s%open_parens) == 2, "two open parens")
        if (allocated(error)) return
        call check(error, s%open_parens(1) == 1 .and. s%open_parens(2) == 4 .and. s%last_sexp_start == 5)
        if (allocated(error)) return
        call check(error, point() == point_max())
        if (allocated(error)) return
        ! TARGETDEPTH stops when the depth reaches it.
        call parse_partial_sexp(1_int64, point_max(), s, st, targetdepth=2_int64)
        call check(error, point() == 5 .and. s%depth == 2, "targetdepth: "//trim(str(point())))
        if (allocated(error)) return
        ! STOPBEFORE stops at the start of a sexp.
        call parse_partial_sexp(2_int64, point_max(), s2, st, stopbefore=.true., oldstate=s)
        call check(error, point() == 2 .and. s2%depth == 2, "stopbefore: "//trim(str(point())))
        if (allocated(error)) return
        ! A generic string fence.
        call temp_buffer("|a b| c", table)
        call entry("|", "|", table)
        call parse_partial_sexp(1_int64, 3_int64, s, st)
        call check(error, s%instring == ST_STRING_STYLE .and. s%comstr_start == 1, "in a fenced string")
        if (allocated(error)) return
        call syntax_ppss(s, 7_int64)
        call check(error, s%instring == -1 .and. s%comstr_start == 0 .and. point() == 7 .and. s%last_sexp_start == 1, &
                   "syntax_ppss after it")
        call done_buffer(table)
    end subroutine test_pps_state

    !> comment_end_can_be_escaped is buffer-local when set by name.
    subroutine test_escaped_variable(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st

        call temp_buffer("", table)
        call set(sym("comment_end_can_be_escaped"), t_value(), st)
        call check(error, st%ok() .and. comment_end_can_be_escaped%value .and. comment_end_can_be_escaped%local_p())
        call comment_end_can_be_escaped%kill_local()
        call done_buffer(table)
    end subroutine test_escaped_variable

    !> forward-sexp, up-list, down-list, forward-list, kill-sexp and their
    !> interactive errors.
    subroutine test_sexp_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st

        call temp_buffer("(a ""b c"" (d) e)", table)
        call goto_char(7_int64)
        ! Out of the string, then out of the list.
        call up_list(1_int64, st, .true., .true.)
        call check(error, st%ok() .and. point() == 9, "up out of the string: "//trim(str(point())))
        if (allocated(error)) return
        call up_list(1_int64, st, .true., .true.)
        call check(error, st%ok() .and. point() == 16, "up out of the list: "//trim(str(point())))
        if (allocated(error)) return
        call up_list(1_int64, st, .true., .true.)
        call check(error, st%code == FM_USER_ERROR .and. format_error(st) == "At top level", format_error(st))
        if (allocated(error)) return
        call up_list(1_int64, st)
        call check(error, st%code == FM_SCAN_ERROR)
        if (allocated(error)) return
        call goto_char(12_int64)
        call backward_up_list(1_int64, st)
        call check(error, st%ok() .and. point() == 10, "backward up: "//trim(str(point())))
        if (allocated(error)) return
        call goto_char(1_int64)
        call down_list(1_int64, st)
        call check(error, st%ok() .and. point() == 2)
        if (allocated(error)) return
        call goto_char(5_int64)
        call down_list(1_int64, st, .true.)
        call check(error, format_error(st) == "This command doesn't work in strings or comments", format_error(st))
        if (allocated(error)) return
        call goto_char(15_int64)
        call down_list(1_int64, st, .true.)
        call check(error, format_error(st) == "At bottom level", format_error(st))
        if (allocated(error)) return
        call goto_char(2_int64)
        call forward_list(1_int64, st)
        call check(error, st%ok() .and. point() == 13, "forward-list: "//trim(str(point())))
        if (allocated(error)) return
        call forward_list(1_int64, st, .true.)
        call check(error, format_error(st) == "No next group", format_error(st))
        if (allocated(error)) return
        call goto_char(15_int64)
        call forward_sexp(1_int64, st, .true.)
        call check(error, format_error(st) == "No next sexp", format_error(st))
        if (allocated(error)) return
        call backward_sexp(1_int64, st, .true.)
        call check(error, st%ok() .and. point() == 14, "backward-sexp: "//trim(str(point())))
        if (allocated(error)) return
        call goto_char(10_int64)
        call kill_sexp(1_int64, st)
        call check(error, st%ok() .and. buffer_string() == "(a ""b c""  e)", buffer_string())
        call done_buffer(table)
    end subroutine test_sexp_commands

    !> C-M-f, C-M-b, C-M-n, C-M-p, C-M-d, C-M-u, C-M-k and C-M-@ through the
    !> command loop.
    subroutine test_sexp_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        type(status_type) :: st
        type(value_type), allocatable :: evs(:)

        call temp_buffer("(a (b c) d) e", table)
        call key_parse("C-M-f", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 12, "C-M-f: "//trim(str(point())))
        if (allocated(error)) return
        call key_parse("C-M-b C-M-d C-M-n", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 9, "C-M-b C-M-d C-M-n: "//trim(str(point())))
        if (allocated(error)) return
        call key_parse("C-M-p C-M-d C-M-u", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 4, "C-M-p C-M-d C-M-u: "//trim(str(point())))
        if (allocated(error)) return
        call key_parse("C-M-@", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, mark_position() == 9 .and. point() == 4, "C-M-@")
        if (allocated(error)) return
        call key_parse("C-M-k", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == "(a  d) e", buffer_string())
        call done_buffer(table)
    end subroutine test_sexp_keys
end module test_syntax_scan_fm
