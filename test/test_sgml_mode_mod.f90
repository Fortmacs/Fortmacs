! test_sgml_mode_mod.f90 -- tests for SGML mode (sgml_mode_mod).
! Ports GNU Emacs 31.1 test/lisp/textmodes/sgml-mode-tests.el.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/textmodes/sgml-mode-tests.el (all 10), with own tests
!> of indentation and `sgml_close_tag'.
module test_sgml_mode_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, erase_buffer, close_buffer
    use insdel_mod, only: insert
    use value_mod, only: value_type, vtag, V_NAME, xsymbol, symbols
    use editfns_mod, only: point, point_min, point_max, goto_char, widen, is_eob, buffer_substring
    use indent_mod, only: indent_region
    use syntax_mod, only: parse_state_type, syntax_ppss
    use syntax_propertize_mod, only: syntax_propertize_chunk_size
    use textprop_mod, only: get_text_property, next_single_char_property_change
    use font_lock_mod, only: font_lock_ensure
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use sgml_mode_mod, only: sgml_mode, html_mode, sgml_delete_tag, sgml_quote, sgml_close_tag
    implicit none(type, external)
    private

    public :: collect_sgml_mode_mod

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    !> Collect all exported unit tests.
    !> Collect all exported unit tests.
    subroutine collect_sgml_mode_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [new_unittest("sgml-delete-tag-should-not-delete-tags-when-wrong-args", test_delete_wrong_args), &
                     new_unittest("sgml-delete-tag-should-delete-tags-n-times", test_delete_n_times), &
                     new_unittest("sgml-delete-tag-should-delete-unclosed-tag", test_delete_unclosed), &
                     new_unittest("sgml-delete-tag-should-signal-error-for-malformed-tags", test_delete_malformed), &
                     new_unittest("sgml-delete-tag-should-signal-error-when-deleting-too-much", test_delete_too_much), &
                     new_unittest("sgml-delete-tag-bug-8203-should-not-delete-apostrophe", test_delete_apostrophe), &
                     new_unittest("sgml-quote-works", test_quote_works), &
                     new_unittest("sgml-tests--quotes-syntax", test_quotes_syntax), &
                     new_unittest("sgml-mode-quote-in-long-text", test_quote_in_long_text), &
                     new_unittest("sgml-test-brackets", test_brackets), &
                     new_unittest("sgml-indent", test_indent), &
                     new_unittest("sgml-close-tag", test_close_tag)]
    end subroutine collect_sgml_mode_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! A temporary buffer in SGML mode (HTML mode if HTML; Fundamental mode
    ! if PLAIN) holding CONTENT, point at its beginning
    ! (sgml-with-content).
    subroutine with_content(content, html, plain)
        character(len=*), intent(in) :: content
        logical, intent(in), optional :: html, plain
        type(handle_type) :: buffer
        type(status_type) :: st
        logical :: is_html, is_plain

        is_html = .false.
        if (present(html)) is_html = html
        is_plain = .false.
        if (present(plain)) is_plain = plain
        call init_bindings()
        call get_or_make_buffer(" *sgml-test*", buffer, st)
        call close_buffer(buffer, st)
        call get_or_make_buffer(" *sgml-test*", buffer, st)
        call set_buffer(buffer, st)
        call widen()
        call erase_buffer(st)

        if (is_html) then
            call html_mode(st)
        else if (.not. is_plain) then
            call sgml_mode(st)
        end if

        call insert(content, st)
        call goto_char(point_min())
    end subroutine with_content

    ! The text of the current buffer.
    function buffer_string() result(s)
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(point_min(), point_max(), s, st)
        if (.not. allocated(s)) s = ""
    end function buffer_string

    ! The depth of the parse state at POS.
    integer function depth_at(pos)
        integer(int64), intent(in) :: pos
        type(parse_state_type) :: state

        call goto_char(pos)
        call syntax_ppss(state, pos)
        depth_at = int(state%depth)
    end function depth_at

    ! sgml_delete_tag ARG: whether it signaled an error.
    logical function delete_tag_fails(arg)
        integer(int64), intent(in) :: arg
        type(status_type) :: st

        call sgml_delete_tag(arg, st)
        delete_tag_fails = .not. st%ok()
    end function delete_tag_fails

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    !> Don't delete tag, when number of tags to delete is not positive
    !> number.
    subroutine test_delete_wrong_args(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: CONTENT = "<p>Valar Morghulis</p>"
        logical :: failed

        call with_content(CONTENT)
        failed = delete_tag_fails(-1_int64)
        call check(error, buffer_string(), CONTENT)
        if (allocated(error)) return
        failed = delete_tag_fails(0_int64)
        call check(error, buffer_string(), CONTENT)
    end subroutine test_delete_wrong_args

    subroutine test_delete_n_times(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: failed

        ! Delete only 1, when 1 available:
        call with_content("<br />")
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "")
        if (allocated(error)) return
        ! Delete from position on whitespaces before tag:
        call with_content(" "//TAB//LF//"<br />")
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "")
        if (allocated(error)) return
        ! Delete from position on tag:
        call with_content("<br />")
        call goto_char(3_int64)
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "")
        if (allocated(error)) return
        ! Delete one by one:
        call with_content("<h1><p>You know nothing, Jon Snow.</p></h1>")
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "<p>You know nothing, Jon Snow.</p>")
        if (allocated(error)) return
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "You know nothing, Jon Snow.")
        if (allocated(error)) return
        ! Delete 2 at a time, when 2 available:
        call with_content("<h1><p>You know nothing, Jon Snow.</p></h1>")
        failed = delete_tag_fails(2_int64)
        call check(error, buffer_string(), "You know nothing, Jon Snow.")
    end subroutine test_delete_n_times

    subroutine test_delete_unclosed(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: failed

        call with_content("<ul><li>Keep your stones connected.</ul>")
        ! position on "li" tag
        call goto_char(5_int64)
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "<ul>Keep your stones connected.</ul>")
    end subroutine test_delete_unclosed

    subroutine test_delete_malformed(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: CONTENT = "<h1><h2>Drakaris!</h1></h2>"
        logical :: failed

        ! Delete outside tag:
        call with_content(CONTENT)
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "<h2>Drakaris!</h2>")
        if (allocated(error)) return
        ! Delete inner tag:
        call with_content(CONTENT)
        ! position the inner tag
        call goto_char(5_int64)
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "<h1>Drakaris!</h1>")
    end subroutine test_delete_malformed

    subroutine test_delete_too_much(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: CONTENT = "<emph>Drakaris!</emph>"

        ! No tags to delete:
        call with_content("Drakaris!")
        call check(error, delete_tag_fails(1_int64), "no error with no tags")
        if (allocated(error)) return
        call check(error, buffer_string(), "Drakaris!")
        if (allocated(error)) return
        ! Trying to delete 2 tags, when only 1 available:
        call with_content(CONTENT)
        call check(error, delete_tag_fails(2_int64), "no error deleting 2 tags")
        if (allocated(error)) return
        call check(error, buffer_string(), "Drakaris!")
        if (allocated(error)) return
        ! Trying to delete a tag, but not on/before a tag:
        call with_content(CONTENT)
        ! D in Drakaris
        call goto_char(7_int64)
        call check(error, delete_tag_fails(1_int64), "no error not on a tag")
        if (allocated(error)) return
        call check(error, buffer_string(), CONTENT)
        if (allocated(error)) return
        ! Trying to delete a tag from position outside tag:
        call with_content(CONTENT)
        call goto_char(point_max())
        call check(error, delete_tag_fails(1_int64), "no error outside the tag")
        if (allocated(error)) return
        call check(error, buffer_string(), CONTENT)
    end subroutine test_delete_too_much

    subroutine test_delete_apostrophe(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: failed

        call with_content("<title>Winter is comin'</title>")
        failed = delete_tag_fails(1_int64)
        call check(error, buffer_string(), "Winter is comin'")
    end subroutine test_delete_apostrophe

    subroutine test_quote_works(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: TEXT = "Foo<Bar> ""Baz"" 'Qux'"//LF
        type(status_type) :: st

        ! Back and forth transformation.
        call with_content(TEXT, plain=.true.)
        call sgml_quote(point_min(), point_max(), st)
        call check(error, buffer_string(), "Foo&lt;Bar&gt; &#34;Baz&#34; &#39;Qux&#39;"//LF)
        if (allocated(error)) return
        call sgml_quote(point_min(), point_max(), st, .true.)
        call check(error, buffer_string(), TEXT)
        if (allocated(error)) return
        ! The same text escaped differently.
        call with_content("Foo&lt;Bar&gt; &#34;Baz&quot; &#x27;Qux&#X27;"//LF, plain=.true.)
        call sgml_quote(point_min(), point_max(), st, .true.)
        call check(error, buffer_string(), TEXT)
        if (allocated(error)) return
        ! Lack of semicolon.
        call with_content("&amp&amp", plain=.true.)
        call sgml_quote(point_min(), point_max(), st, .true.)
        call check(error, buffer_string(), "&&")
        if (allocated(error)) return
        ! Double quoting
        call sgml_quote(point_min(), point_max(), st)
        call sgml_quote(point_min(), point_max(), st)
        call sgml_quote(point_min(), point_max(), st, .true.)
        call sgml_quote(point_min(), point_max(), st, .true.)
        call check(error, buffer_string(), "&&")
    end subroutine test_quote_works

    subroutine test_quotes_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=24), parameter :: STRS(12) = [character(len=24) :: "a""b <t>c'd</t>", "a'b <t>c""d</t>", &
                                                                    "<t>""a'</t>", "<t>'a""</t>", "<t>""a'""</t>", "<t>'a""'</t>", &
                                                                   "a""b <tag>c'd</tag>", "<tag>c>'d</tag>", "<t><!-- "" --></t>", &
                                                                                   "<t><!-- ' --></t>", "<t>(')</t>", "<t>("")</t>"]
        integer :: i

        do i = 1, size(STRS)
            call with_content(trim(STRS(i)))
            ! Check that last tag is parsed as a tag.
            call check(error, depth_at(point_max() - 1), 1, "Test case: "//trim(STRS(i)))
            if (allocated(error)) return
            call check(error, depth_at(point_max()), 0, "Test case: "//trim(STRS(i)))
            if (allocated(error)) return
        end do
    end subroutine test_quotes_syntax

    subroutine test_quote_in_long_text(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: n, pos
        integer :: d

        n = syntax_propertize_chunk_size
        ! `syntax-propertize-wholelines' extends chunk size based on line
        ! length, so newlines are significant!
        call with_content("<t>"//repeat("a", int(n))//LF//"'"//repeat("a", int(n))//LF//"</t>")
        ! If we just check (syntax-ppss (point-max)) immediately, then
        ! we'll end up propertizing the whole buffer in one chunk (so the
        ! test is useless).  Simulate something more like what happens when
        ! the buffer is viewed normally.
        pos = point_min()

        do while (pos <= point_max())
            d = depth_at(pos)
            pos = pos + n
        end do

        d = depth_at(point_max())
        ! Check that last tag is parsed as a tag.
        call check(error, depth_at(point_max() - 1) - depth_at(point_max()), 1)
    end subroutine test_quote_in_long_text

    !> Test fontification of apostrophe preceded by paired-bracket
    !> character.
    subroutine test_brackets(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: CHARS(134) = [40, 41, 91, 93, 123, 125, 3898, 3899, 3900, 3901, 5787, 5788, &
                        8261, 8262, 8317, 8318, 8333, 8334, 8968, 8969, 8970, 8971, 9001, 9002, 10088, 10089, 10090, 10091, 10092, &
                          10093, 10094, 10095, 10096, 10097, 10098, 10099, 10100, 10101, 10181, 10182, 10214, 10215, 10216, 10217, &
                          10218, 10219, 10220, 10221, 10222, 10223, 10627, 10628, 10629, 10630, 10631, 10632, 10633, 10634, 10635, &
                          10636, 10637, 10638, 10639, 10640, 10641, 10642, 10643, 10644, 10645, 10646, 10647, 10648, 10712, 10713, &
                          10714, 10715, 10748, 10749, 11810, 11811, 11812, 11813, 11814, 11815, 11816, 11817, 11861, 11862, 11863, &
                          11864, 11865, 11866, 11867, 11868, 12296, 12297, 12298, 12299, 12300, 12301, 12302, 12303, 12304, 12305, &
                          12308, 12309, 12310, 12311, 12312, 12313, 12314, 12315, 65113, 65114, 65115, 65116, 65117, 65118, 65288, &
                                            65289, 65339, 65341, 65371, 65373, 65375, 65376, 65378, 65379, 36, 37, 38, 42, 43, 47]
        type(status_type) :: st
        type(value_type) :: v
        character(len=:), allocatable :: text
        integer :: i

        text = ""

        do i = 1, size(CHARS)
            text = text//"<p>"//utf8(CHARS(i))//"'s</p>"//LF
        end do

        call with_content(text, plain=.true.)
        call html_mode(st)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
        call goto_char(point_min())

        do while (.not. is_eob())
            call goto_char(next_single_char_property_change(point(), value_type(name="face")))
            call get_text_property(point(), value_type(name="face"), v, st)

            if (vtag(v) == V_NAME) then
                call check(error, symbols(xsymbol(v))%name /= "font_lock_string_face", "string face at a bracket")
                if (allocated(error)) return
            end if
        end do

    contains

        ! The UTF-8 text of the character C.
        function utf8(c) result(s)
            integer, intent(in) :: c
            character(len=:), allocatable :: s

            if (c < 128) then
                s = achar(c)
            else if (c < 2048) then
                s = achar(192 + c/64)//achar(128 + mod(c, 64))
            else
                s = achar(224 + c/4096)//achar(128 + mod(c/64, 64))//achar(128 + mod(c, 64))
            end if
        end function utf8
    end subroutine test_brackets

    ! Own: nested tags are indented by `sgml_basic_offset' (the result of
    ! Emacs 31.1).
    subroutine test_indent(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call with_content("<html>"//LF//"<body>"//LF//"<p>text"//LF//"</p>"//LF//"</body>"//LF//"</html>"//LF)
        call indent_region(point_min(), point_max(), st)
        call check(error, buffer_string(), "<html>"//LF//"  <body>"//LF//"    <p>text"//LF//"    </p>"//LF &
                   //"  </body>"//LF//"</html>"//LF)
    end subroutine test_indent

    ! Own: `sgml_close_tag' closes the element around point.
    subroutine test_close_tag(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call with_content("<a><b>x")
        call goto_char(point_max())
        call sgml_close_tag(st)
        call check(error, buffer_string(), "<a><b>x</b>")
    end subroutine test_close_tag
end module test_sgml_mode_mod
