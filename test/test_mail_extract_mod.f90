! test_mail_extract_mod.f90 -- tests for mail_extract_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/mail-extr-tests.el.
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

!> The mail-extr-tests.el tests (`extract_mail_address_components' and
!> `what_domain'), and own tests whose expected values are what Emacs
!> 31.1 gives: the many shapes of addresses (comments, quotes, routes,
!> groups, UUCP paths, % routing, titles, suffixes, initials, trailing
!> comments), every address with ALL, the options, a buffer as the
!> address, folded lines and the error upstream signals.
module test_mail_extract_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, WRONG_TYPE_ARGUMENT_CODE, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type, same_handle
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: narrow_to_region, point_maximum
    use bindings_mod, only: initialize_bindings
    use mail_extract_mod, only: extract_mail_address_components, what_domain, mail_address_components_type, &
                                mail_extr_guess_middle_initial, mail_extr_mangle_uucp, &
                                mail_extract_at_sign_binds_tighter_than_exclamation_mark, mail_extr_ignore_single_names, &
                                mail_extr_ignore_realname_equals_mailbox_name, mail_extr_disable_voodoo
    implicit none(type, external)
    private

    public :: collect_mail_extract_mod

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)
    ! "Ååå Äää" in UTF-8.
    character(len=*), parameter :: SWEDISH_NAME = char(195)//char(133)//char(195)//char(165)//char(195)//char(165)//" "// &
                                   char(195)//char(132)//char(195)//char(164)//char(195)//char(164)

contains

    !> Collect all exported unit tests.
    subroutine collect_mail_extract_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("mail-extract-address-components", test_extract), &
                    new_unittest("what-domain", test_what_domain), &
                    new_unittest("extract-mail-address-components-cases", test_cases), &
                    new_unittest("extract-mail-address-components-buffer", test_buffer), &
                    new_unittest("extract-mail-address-components-folded", test_folded), &
                    new_unittest("extract-mail-address-components-error", test_error), &
                    new_unittest("what-domain-unknown", test_unknown_domain) &
                    ]
    end subroutine collect_mail_extract_mod

    ! The components as text: NAME|ADDRESS for each, joined by ";",
    ! FALSE for an unallocated one (and "FALSE|FALSE" for none at all without
    ! ALL, upstream's nil).
    function shown(components, all) result(text)
        type(mail_address_components_type), intent(in) :: components(:)
        logical, intent(in) :: all
        character(len=:), allocatable :: text
        integer :: k

        text = ""
        if (.not. all .and. size(components) == 0) text = "NIL|NIL"

        do k = 1, size(components)
            if (k > 1) text = text//";"

            if (allocated(components(k)%full_name)) then
                text = text//components(k)%full_name//"|"
            else
                text = text//"NIL|"
            end if

            if (allocated(components(k)%address)) then
                text = text//components(k)%address
            else
                text = text//"NIL"
            end if
        end do
    end function shown

    ! Check that ADDRESS (with ALL) gives WANTED, as `shown' writes it.
    subroutine check_address(error, address, wanted, all)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: address, wanted
        logical, intent(in), optional :: all
        type(mail_address_components_type), allocatable :: components(:)
        type(status_type) :: st
        character(len=:), allocatable :: got
        logical :: every

        if (allocated(error)) return
        every = .false.
        if (present(all)) every = all
        call extract_mail_address_components(address, components, st, all=every)

        if (.not. st%ok()) then
            call check(error, .false., "["//address//"]: "//format_error(st))
            return
        end if

        got = shown(components, every)
        call check(error, got == wanted, "["//address//"]: got ["//got//"], wanted ["//wanted//"]")
    end subroutine check_address

    subroutine test_extract(error)
        type(error_type), allocatable, intent(out) :: error

        call initialize_bindings()
        call check_address(error, "foo@example.org", "NIL|foo@example.org")
        call check_address(error, "J. Random Hacker <foo@example.org>", "J. Random Hacker|foo@example.org")
        call check_address(error, """J. Random Hacker"" <foo@example.org>", "J. Random Hacker|foo@example.org")
        call check_address(error, SWEDISH_NAME//" <foo@example.org>", SWEDISH_NAME//"|foo@example.org")
    end subroutine test_extract

    subroutine test_what_domain(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text

        call initialize_bindings()
        call what_domain("cu", st, text)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, text == "CU: Cuba", "got "//text)
        if (allocated(error)) return
        call what_domain("UK", st, text)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, text == "UK: The United Kingdom of Great Britain and Northern Ireland", "got "//text)
    end subroutine test_what_domain

    ! Expected values from Emacs 31.1.
    subroutine test_cases(error)
        type(error_type), allocatable, intent(out) :: error

        call initialize_bindings()
        call check_address(error, "foo@example.org", "NIL|foo@example.org")
        call check_address(error, "foo@example.org", "NIL|foo@example.org", all=.true.)
        call check_address(error, "J. Random Hacker <foo@example.org>", "J. Random Hacker|foo@example.org")
        call check_address(error, "J. Random Hacker <foo@example.org>", "J. Random Hacker|foo@example.org", all=.true.)
        call check_address(error, "fml@foo.bar.dom (First M. Last)", "First M. Last|fml@foo.bar.dom")
        call check_address(error, "fml@foo.bar.dom (First M. Last)", "First M. Last|fml@foo.bar.dom", all=.true.)
        call check_address(error, "First_M_Last@foo.bar.dom", "First M. Last|First_M_Last@foo.bar.dom")
        call check_address(error, "First_M_Last@foo.bar.dom", "First M. Last|First_M_Last@foo.bar.dom", all=.true.)
        call check_address(error, "John Smith <jqs@host.com>", "John Smith|jqs@host.com")
        call check_address(error, "John Smith <jqs@host.com>", "John Smith|jqs@host.com", all=.true.)
        call check_address(error, "danj1@cb.att.com (daniel.jacobson)", "daniel jacobson|danj1@cb.att.com")
        call check_address(error, "danj1@cb.att.com (daniel.jacobson)", "daniel jacobson|danj1@cb.att.com", all=.true.)
        call check_address(error, "Smith, John <js@x.org>", "NIL|Smith")
        call check_address(error, "Smith, John <js@x.org>", "NIL|Smith;John|js@x.org", all=.true.)
        call check_address(error, "John Smith Jr <js@x.org>", "John Smith, Jr.|js@x.org")
        call check_address(error, "John Smith Jr <js@x.org>", "John Smith, Jr.|js@x.org", all=.true.)
        call check_address(error, "@foo.bar.dom,@xxx.yyy.zzz:mailbox@aaa.bbb.ccc", &
                           "NIL|mailbox%aaa.bbb.ccc%xxx.yyy.zzz@foo.bar.dom ")
        call check_address(error, "@foo.bar.dom,@xxx.yyy.zzz:mailbox@aaa.bbb.ccc", &
                           "NIL|mailbox%aaa.bbb.ccc%xxx.yyy.zzz@foo.bar.dom ", all=.true.)
        call check_address(error, "<@relay.com:user@host.com>", "NIL|user@host.com")
        call check_address(error, "<@relay.com:user@host.com>", "NIL|user@host.com", all=.true.)
        call check_address(error, "<@a.com,@b.com:user@host.com>", "NIL|user@host.com")
        call check_address(error, "<@a.com,@b.com:user@host.com>", "NIL|user@host.com", all=.true.)
        call check_address(error, "foo!bar!baz@host", "NIL|foo!bar!baz@host")
        call check_address(error, "foo!bar!baz@host", "NIL|foo!bar!baz@host", all=.true.)
        call check_address(error, "user%host@relay.EXAMPLE.COM", "NIL|user%host@relay.example.com")
        call check_address(error, "user%host@relay.EXAMPLE.COM", "NIL|user%host@relay.example.com", all=.true.)
        call check_address(error, "baz%bar.com@relay.org", "NIL|baz@bar.com")
        call check_address(error, "baz%bar.com@relay.org", "NIL|baz@bar.com", all=.true.)
        call check_address(error, "a%b%c", "a.|a%b@c")
        call check_address(error, "a%b%c", "a.|a%b@c", all=.true.)
        call check_address(error, "Single <single@address.com>", "NIL|single@address.com")
        call check_address(error, "Single <single@address.com>", "NIL|single@address.com", all=.true.)
        call check_address(error, """Bob & Susie"" <bs@x.org>", "Bob & Susie|bs@x.org")
        call check_address(error, """Bob & Susie"" <bs@x.org>", "Bob & Susie|bs@x.org", all=.true.)
        call check_address(error, "Prof. Jane Doe <jd@x.edu>", "Jane Doe|jd@x.edu")
        call check_address(error, "Prof. Jane Doe <jd@x.edu>", "Jane Doe|jd@x.edu", all=.true.)
        call check_address(error, "group: a@b.c, d@e.f;", "group a.|group  a@b.c")
        call check_address(error, "group: a@b.c, d@e.f;", "group a.|group  a@b.c;d.|d@e.f ", all=.true.)
        call check_address(error, "John (Jack) Smith <js@x.org>", "John Smith|js@x.org")
        call check_address(error, "John (Jack) Smith <js@x.org>", "John Smith|js@x.org", all=.true.)
        call check_address(error, "Mr. Foo Bar <foo@BAR.COM>", "Foo Bar|foo@bar.com")
        call check_address(error, "Mr. Foo Bar <foo@BAR.COM>", "Foo Bar|foo@bar.com", all=.true.)
        call check_address(error, "jim@x.org (Jim Jones  x1234)", "Jim Jones|jim@x.org")
        call check_address(error, "jim@x.org (Jim Jones  x1234)", "Jim Jones|jim@x.org", all=.true.)
        call check_address(error, "First.Last@x.org", "First Last|First.Last@x.org")
        call check_address(error, "First.Last@x.org", "First Last|First.Last@x.org", all=.true.)
        call check_address(error, """user""@host", "user|""user""@host")
        call check_address(error, """user""@host", "user|""user""@host", all=.true.)
        call check_address(error, "Multiple recipients of list FOO-L <FOO-L@x.edu>", "NIL|FOO-L@x.edu")
        call check_address(error, "Multiple recipients of list FOO-L <FOO-L@x.edu>", "NIL|FOO-L@x.edu", all=.true.)
        call check_address(error, "a@b.c, ""D E"" <d@e.f>, g@h (G H)", "a.|a@b.c")
        call check_address(error, "a@b.c, ""D E"" <d@e.f>, g@h (G H)", "a.|a@b.c;D. E.|d@e.f;G. H.|g@h", all=.true.)
        call check_address(error, "Jim & <jim@x.org>", "Jim Jim|jim@x.org")
        call check_address(error, "Jim & <jim@x.org>", "Jim Jim|jim@x.org", all=.true.)
        call check_address(error, "13-Jan-1993 1234 Jane Doe <jd@x.org>", "Jane Doe|jd@x.org")
        call check_address(error, "13-Jan-1993 1234 Jane Doe <jd@x.org>", "Jane Doe|jd@x.org", all=.true.)
        call check_address(error, "John Doe N2NJZ <jd@x.org>", "John Doe|jd@x.org")
        call check_address(error, "John Doe N2NJZ <jd@x.org>", "John Doe|jd@x.org", all=.true.)
        call check_address(error, "MKA Fred Flint <ff@x.org>", "Fred Flint|ff@x.org")
        call check_address(error, "MKA Fred Flint <ff@x.org>", "Fred Flint|ff@x.org", all=.true.)
        call check_address(error, "foo.hoser@xerox.com (Foo PARC)", "NIL|foo.hoser@xerox.com")
        call check_address(error, "foo.hoser@xerox.com (Foo PARC)", "NIL|foo.hoser@xerox.com", all=.true.)
        call check_address(error, "John 'Q' Smith <js@x.org>", "John Q. Smith|js@x.org")
        call check_address(error, "John 'Q' Smith <js@x.org>", "John Q. Smith|js@x.org", all=.true.)
        call check_address(error, "Jones, John Q. <jj@x.org>", "NIL|Jones")
        call check_address(error, "Jones, John Q. <jj@x.org>", "NIL|Jones;John Q.|jj@x.org", all=.true.)
        call check_address(error, "user@host (Last, First)", "First Last|user@host")
        call check_address(error, "user@host (Last, First)", "First Last|user@host", all=.true.)
        call check_address(error, "Mary Jane McXavier XT-DEM <m@x.org>", "Mary Jane McXavier|m@x.org")
        call check_address(error, "Mary Jane McXavier XT-DEM <m@x.org>", "Mary Jane McXavier|m@x.org", all=.true.)
        call check_address(error, """John \""Jack\"" Smith"" <js@x.org>", "John Smith|js@x.org")
        call check_address(error, """John \""Jack\"" Smith"" <js@x.org>", "John Smith|js@x.org", all=.true.)
        call check_address(error, "/G=John/S=Smith/O=Org/@gw.org", "John Smith|/G=John/S=Smith/O=Org/@gw.org")
        call check_address(error, "/G=John/S=Smith/O=Org/@gw.org", "John Smith|/G=John/S=Smith/O=Org/@gw.org", all=.true.)
        call check_address(error, "John Smith -- hello <js@x.org>", "John Smith|js@x.org")
        call check_address(error, "John Smith -- hello <js@x.org>", "John Smith|js@x.org", all=.true.)
        call check_address(error, "<a@b> <c@d>", "a. b. c.|a@b   c@d")
        call check_address(error, "<a@b> <c@d>", "a. b. c.|a@b   c@d", all=.true.)
        call check_address(error, "jo@x.org (Joe.Bloggs)", "Joe Bloggs|jo@x.org")
        call check_address(error, "jo@x.org (Joe.Bloggs)", "Joe Bloggs|jo@x.org", all=.true.)
        call check_address(error, "ann_lee@x.org", "ann lee|ann_lee@x.org")
        call check_address(error, "ann_lee@x.org", "ann lee|ann_lee@x.org", all=.true.)
        call check_address(error, "(comment) <a@b.c>", "NIL|a@b.c")
        call check_address(error, "(comment) <a@b.c>", "NIL|a@b.c", all=.true.)
        call check_address(error, "Hey!!! <a@b.c>", "Hey!!!|a@b.c")
        call check_address(error, "Hey!!! <a@b.c>", "Hey!!!|a@b.c", all=.true.)
        call check_address(error, "Jean-Luc Picard <jlp@x.org>", "Jean-Luc Picard|jlp@x.org")
        call check_address(error, "Jean-Luc Picard <jlp@x.org>", "Jean-Luc Picard|jlp@x.org", all=.true.)
        call check_address(error, "a@b, c@d", "a.|a@b")
        call check_address(error, "a@b, c@d", "a.|a@b;c.|c@d", all=.true.)
        call check_address(error, """Doe, John"" <jd@x.org>", "John Doe|jd@x.org")
        call check_address(error, """Doe, John"" <jd@x.org>", "John Doe|jd@x.org", all=.true.)
        call check_address(error, "[1.2.3.4] <a@b>", "1|a@b")
        call check_address(error, "[1.2.3.4] <a@b>", "1|a@b", all=.true.)
        call check_address(error, "JOHN SMITH <js@x.org>", "JOHN SMITH|js@x.org")
        call check_address(error, "JOHN SMITH <js@x.org>", "JOHN SMITH|js@x.org", all=.true.)
        call check_address(error, "john smith brown <js@x.org>", "john smith brown|js@x.org")
        call check_address(error, "john smith brown <js@x.org>", "john smith brown|js@x.org", all=.true.)
        call check_address(error, "Who? <who@x.org>", "Who?|who@x.org")
        call check_address(error, "Who? <who@x.org>", "Who?|who@x.org", all=.true.)
        call check_address(error, "John III Smith <j@x.org>", "John III Smith|j@x.org")
        call check_address(error, "John III Smith <j@x.org>", "John III Smith|j@x.org", all=.true.)
        call check_address(error, "Dr. Who and the Daleks <dw@x.org>", "Who and the Daleks|dw@x.org")
        call check_address(error, "Dr. Who and the Daleks <dw@x.org>", "Who and the Daleks|dw@x.org", all=.true.)
        call check_address(error, "foo@bar.COM (Fo\(o)", "Fo|foo@bar.com")
        call check_address(error, "foo@bar.COM (Fo\(o)", "Fo|foo@bar.com", all=.true.)
        call check_address(error, "Ken Smith <""ken smith""@x.org>", "Ken Smith|""ken smith""@x.org")
        call check_address(error, "Ken Smith <""ken smith""@x.org>", "Ken Smith|""ken smith""@x.org", all=.true.)
        call check_address(error, """"" <a@b>", "NIL|a@b")
        call check_address(error, """"" <a@b>", "NIL|a@b", all=.true.)
        call check_address(error, "first.m.last@x.org", "first m. last|first.m.last@x.org")
        call check_address(error, "first.m.last@x.org", "first m. last|first.m.last@x.org", all=.true.)
        call check_address(error, "x@y (a (nested) comment)", "a. comment|x@y")
        call check_address(error, "x@y (a (nested) comment)", "a. comment|x@y", all=.true.)
        call check_address(error, "<foo@bar>, <baz@qux>", "NIL|foo@bar")
        call check_address(error, "<foo@bar>, <baz@qux>", "NIL|foo@bar;NIL|baz@qux", all=.true.)
        call check_address(error, "foo!bar", "NIL|foo!bar")
        call check_address(error, "foo!bar", "NIL|foo!bar", all=.true.)
        call check_address(error, "foo@bar>", "NIL|foo@bar ")
        call check_address(error, "foo@bar>", "NIL|foo@bar ", all=.true.)
        call check_address(error, "foo <bar", "foo|bar")
        call check_address(error, "foo <bar", "foo|bar", all=.true.)
        call check_address(error, "foo@a@b", "foo a.|foo@a@b")
        call check_address(error, "foo@a@b", "foo a.|foo@a@b", all=.true.)
        call check_address(error, "John ~{abcd~} Smith <j@x.org>", "John ~{abcd~} Smith|j@x.org")
        call check_address(error, "John ~{abcd~} Smith <j@x.org>", "John ~{abcd~} Smith|j@x.org", all=.true.)
        mail_extr_guess_middle_initial = .true.
        call check_address(error, "John Smith <jqs@host.com>", "John Q. Smith|jqs@host.com")
        mail_extr_guess_middle_initial = .false.
        mail_extr_mangle_uucp = .true.
        call check_address(error, "foo!bar!baz@host", "NIL|baz@bar.uucp")
        call check_address(error, "foo!bar!baz", "NIL|baz@bar.uucp")
        call check_address(error, "a!b%c@d", "b.|b@a.uucp")
        call check_address(error, "x!y.com!z", "z.|z@y.com")
        mail_extract_at_sign_binds_tighter_than_exclamation_mark = .true.
        call check_address(error, "foo!bar!baz@host", "NIL|baz%host@bar.uucp")
        call check_address(error, "foo!bar", "NIL|bar@foo.uucp")
        mail_extract_at_sign_binds_tighter_than_exclamation_mark = .false.
        mail_extr_mangle_uucp = .false.
        mail_extr_ignore_single_names = .true.
        call check_address(error, "Idiot <dumb@stupid.com>", "NIL|dumb@stupid.com")
        call check_address(error, "Two Words <tw@x.org>", "Two Words|tw@x.org")
        mail_extr_ignore_single_names = .false.
        mail_extr_ignore_realname_equals_mailbox_name = .false.
        call check_address(error, "Single <single@address.com>", "Single|single@address.com")
        mail_extr_ignore_realname_equals_mailbox_name = .true.
        mail_extr_disable_voodoo = .true.
        call check_address(error, "Prof. Jane Doe (x) <jd@x.edu>", "Prof. Jane Doe (x)|jd@x.edu")
        mail_extr_disable_voodoo = .false.
        call check_address(error, "山田 太郎 (x) <y@x.jp>", "山田 太郎|y@x.jp")
        ! A regexp that matches: no change (category j, which the default
        ! "\cj" needs, is Deferred in characters_mod).
        mail_extr_disable_voodoo%regular_expression = "Prof"
        call check_address(error, "Prof. Jane Doe (x) <jd@x.edu>", "Prof. Jane Doe (x)|jd@x.edu")
        mail_extr_disable_voodoo%regular_expression = "Nomatch"
        call check_address(error, "Prof. Jane Doe (x) <jd@x.edu>", "Jane Doe|jd@x.edu")
        mail_extr_disable_voodoo%regular_expression = "\cj"
    end subroutine test_cases

    ! The accessible part of a buffer is the address; the current buffer
    ! stays current.
    subroutine test_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(mail_address_components_type), allocatable :: components(:)
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *mail-extract-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert("To: Jane Doe <jd@x.org>", st)
        call narrow_to_region(5_int64, point_maximum(), st)
        call extract_mail_address_components(h, components, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, shown(components, .false.) == "Jane Doe|jd@x.org", "got "//shown(components, .false.))
        if (allocated(error)) return
        call check(error, same_handle(current_buffer(), h), "the current buffer changed")
        call close_buffer(h, st)
    end subroutine test_buffer

    ! Folded lines are unfolded.
    subroutine test_folded(error)
        type(error_type), allocatable, intent(out) :: error

        call initialize_bindings()
        call check_address(error, "John"//LF//TAB//"Smith <js@x.org>, Ann"//LF//" Lee <al@x.org>", &
                           "John Smith|js@x.org;Ann Lee|al@x.org", all=.true.)
    end subroutine test_folded

    ! Upstream compares nil with a number here: (wrong-type-argument
    ! number-or-marker-p nil).
    subroutine test_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(mail_address_components_type), allocatable :: components(:)
        type(status_type) :: st

        call initialize_bindings()
        call extract_mail_address_components("x>,@y", components, st)
        call check(error, st%code == WRONG_TYPE_ARGUMENT_CODE, "wanted a wrong_type_argument error: "//format_error(st))
    end subroutine test_error

    subroutine test_unknown_domain(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call initialize_bindings()
        call what_domain("nosuch", st)
        call check(error, st%code == GENERIC_ERROR_CODE .and. format_error(st) == "No such domain", format_error(st))
    end subroutine test_unknown_domain
end module test_mail_extract_mod
