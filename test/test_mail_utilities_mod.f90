! test_mail_utilities_mod.f90 -- tests for mail_utilities_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/mail-utils-tests.el in part; the
! test of mail-rfc822-time-zone (obsolete, not ported) waits for it.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> The mail-utils-tests.el tests: quoted printable, stripping quoted
!> names, pruning replies, fetching fields, comma lists and dates; and
!> own tests of the field options of `get_mail_field'.
module test_mail_utilities_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point_minimum, point_maximum
    use bindings_mod, only: initialize_bindings
    use search_mod, only: string_match
    use mail_utilities_mod, only: quote_mail_printable, quote_mail_printable_region, unquote_mail_printable, &
                                  unquote_mail_printable_region, strip_mail_quoted_names, mail_dont_reply_to, &
                                  get_mail_field, parse_mail_comma_list, mail_comma_list_regular_expression, mail_rfc822_date, &
                                  mail_dont_reply_to_names, mail_mbox_from
    use time_functions_mod, only: set_time_zone_rule
    implicit none(type, external)
    private

    public :: collect_mail_utilities_mod

    character(len=*), parameter :: LF = achar(10)
    ! "åäö" in UTF-8.
    character(len=*), parameter :: AAO = char(195)//char(165)//char(195)//char(164)//char(195)//char(182)

contains

    !> Collect all exported unit tests.
    subroutine collect_mail_utilities_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("mail-utils-tests-mail-quote-printable", test_quote_printable), &
                    new_unittest("mail-utils-tests-mail-quote-printable-region", test_quote_printable_region), &
                    new_unittest("mail-utils-tests-mail-unquote-printable", test_unquote_printable), &
                    new_unittest("mail-utils-tests-mail-unquote-printable-region", test_unquote_printable_region), &
                    new_unittest("mail-utils-tests-mail-strip-quoted-names", test_strip_quoted_names), &
                    new_unittest("mail-utils-tests-mail-dont-reply-to", test_dont_reply_to), &
                    new_unittest("mail-utils-tests-mail-fetch-field", test_fetch_field), &
                    new_unittest("mail-utils-tests-mail-parse-comma-list", test_parse_comma_list), &
                    new_unittest("mail-utils-tests-mail-comma-list-regexp", test_comma_list_regular_expression), &
                    new_unittest("mail-utils-test-mail-rfc822-date/contains-year", test_rfc822_date), &
                    new_unittest("mail-utils-test-mail-mbox-from", test_mbox_from), &
                    new_unittest("get-mail-field-options", test_fetch_field_options), &
                    new_unittest("strip-mail-quoted-names-comments", test_strip_comments) &
                    ]
    end subroutine collect_mail_utilities_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> (with-temp-buffer (insert TEXT)).
    subroutine temporary_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *mail-utils-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temporary_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine leave

    subroutine check_string(error, got, wanted)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: got, wanted

        call check(error, got == wanted, "got """//got//""", wanted """//wanted//"""")
    end subroutine check_string

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    subroutine test_quote_printable(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, quote_mail_printable("abc"), "abc")
        if (allocated(error)) return
        call check_string(error, quote_mail_printable(AAO), "=E5=E4=F6")
        if (allocated(error)) return
        call check_string(error, quote_mail_printable(AAO, .true.), "=?ISO-8859-1?Q?=E5=E4=F6?=")
    end subroutine test_quote_printable

    subroutine test_quote_printable_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer('?=""', h)
        call quote_mail_printable_region(point_minimum(), point_maximum(), st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), "=3F=3D=22=22")
        call leave(h)
        if (allocated(error)) return
        call temporary_buffer("x", h)
        call quote_mail_printable_region(point_minimum(), point_maximum(), st, .true.)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), "=?=?ISO-8859-1?Q?x")
        call leave(h)
    end subroutine test_quote_printable_region

    subroutine test_unquote_printable(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, unquote_mail_printable("=E5=E4=F6"), AAO)
        if (allocated(error)) return
        call check_string(error, unquote_mail_printable("=?ISO-8859-1?Q?=E5=E4=F6?=", .true.), AAO)
    end subroutine test_unquote_printable

    subroutine test_unquote_printable_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer("=E5=E4=F6", h)
        call unquote_mail_printable_region(point_minimum(), point_maximum(), st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), AAO)
        call leave(h)
        if (allocated(error)) return
        call temporary_buffer("=?ISO-8859-1?Q?=E5=E4=F6?=", h)
        call unquote_mail_printable_region(point_minimum(), point_maximum(), st, .true.)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), AAO)
        call leave(h)
    end subroutine test_unquote_printable_region

    subroutine test_strip_quoted_names(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, strip_mail_quoted_names('"foo" <foo@example.org>, bar@example.org'), &
                          "foo@example.org, bar@example.org")
    end subroutine test_strip_quoted_names

    subroutine test_dont_reply_to(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: saved
        logical :: had

        had = allocated(mail_dont_reply_to_names)
        if (had) saved = mail_dont_reply_to_names
        mail_dont_reply_to_names = "foo@example.org"
        call check_string(error, mail_dont_reply_to("foo@example.org, bar@example.org"), "bar@example.org")

        if (had) then
            mail_dont_reply_to_names = saved
        else
            deallocate (mail_dont_reply_to_names)
        end if
    end subroutine test_dont_reply_to

    subroutine test_fetch_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: value

        call temporary_buffer("Foo: bar"//LF//"Baz: zut", h)
        call get_mail_field("Foo", value, st)
        call check(error, st%ok() .and. allocated(value), "no value")
        if (.not. allocated(error)) call check_string(error, value, "bar")
        call leave(h)
    end subroutine test_fetch_field

    subroutine test_parse_comma_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: items(:)

        call temporary_buffer("foo@example.org,bar@example.org,baz@example.org", h)
        call goto_character(point_minimum())
        call parse_mail_comma_list(items, st)
        call check(error, st%ok() .and. size(items) == 3, "three items wanted")

        if (.not. allocated(error)) then
            call check(error, trim(items(1)) == "baz@example.org" .and. trim(items(2)) == "bar@example.org" &
                       .and. trim(items(3)) == "foo@example.org", "wrong items")
        end if

        call leave(h)
    end subroutine test_parse_comma_list

    subroutine test_comma_list_regular_expression(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, mail_comma_list_regular_expression("foo@example.org,bar@example.org,baz@example.org"), &
                          "foo@example.org\|bar@example.org\|baz@example.org")
    end subroutine test_comma_list_regular_expression

    subroutine test_rfc822_date(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: date

        date = mail_rfc822_date(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, string_match(" 20[0-9][0-9] ", date, st) > 0, date)
    end subroutine test_rfc822_date

    !> The date of the message is shown in local time, here a zone 5 hours
    !> west of Greenwich.
    subroutine test_mbox_from(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: line

        call temporary_buffer("Subject: Hello"//LF//"From: jrh@example.org"//LF//"To: emacs-devel@gnu.org"//LF// &
                              "Date: Sun, 07 Feb 2021 22:46:37 -0500", h)
        call set_time_zone_rule("EST5")
        line = mail_mbox_from(st)
        call set_time_zone_rule()
        call check_string(error, line, "From jrh@example.org Sun Feb  7 22:46:37 2021"//LF)
        call leave(h)
    end subroutine test_mbox_from

    !> LAST, ALL, DELETE and continuation lines.
    subroutine test_fetch_field_options(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: value

        call temporary_buffer("To: a@x"//LF//"Cc: b@x,"//LF//"  c@x  "//LF//"To: d@x"//LF, h)
        call get_mail_field("to", value, st, last=.true.)
        call check(error, allocated(value), "no last value")
        if (.not. allocated(error)) call check_string(error, value, "d@x")
        if (allocated(error)) return
        call get_mail_field("TO", value, st, all=.true.)
        call check(error, allocated(value), "no values")
        if (.not. allocated(error)) call check_string(error, value, "a@x, d@x")
        if (allocated(error)) return
        call get_mail_field("cc", value, st)
        call check(error, allocated(value), "no continued value")
        if (.not. allocated(error)) call check_string(error, value, "b@x,"//LF//"  c@x")
        if (allocated(error)) return
        call get_mail_field("subject", value, st)
        call check(error,.not. allocated(value), "a missing field has no value")
        if (allocated(error)) return
        call get_mail_field("to", value, st, last=.true., delete=.true.)
        call check_string(error, buffer_string(), "To: a@x"//LF//"Cc: b@x,"//LF//"  c@x  "//LF)
        call leave(h)
    end subroutine test_fetch_field_options

    !> Comments, surrounding whitespace and FOO <BAR>.
    subroutine test_strip_comments(error)
        type(error_type), allocatable, intent(out) :: error

        call check_string(error, strip_mail_quoted_names("  joe@x (Joe Smith) "), "joe@x")
        if (allocated(error)) return
        call check_string(error, strip_mail_quoted_names("Joe Smith <joe@x>, ann@y"), "joe@x, ann@y")
        if (allocated(error)) return
        call check_string(error, strip_mail_quoted_names('"foo bar"@x'), '"foo bar"@x')
    end subroutine test_strip_comments
end module test_mail_utilities_mod
