! test_rmail_mime_mod.f90 -- tests for rmail_mime_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/rmailmm-tests.el.
!
! Copyright (C) 2006-2026 Free Software Foundation, Inc.
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

!> The rmailmm-tests.el tests (the two that upstream marks unstable, which
!> have no condition for success there, check what the code does), and
!> own tests of a MIME message shown, toggled and searched in Rmail.
module test_rmail_mime_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, &
                          generate_new_buffer_name, get_file_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point_minimum
    use search_mod, only: search_forward, SEARCH_NOERROR
    use bindings_mod, only: initialize_bindings
    use character_mod, only: string_to_multibyte
    use rmail_mod, only: swap_rmail_buffers_maybe
    use rmail_mime_mod, only: show_rmail_mime_buffer, toggle_rmail_mime, toggle_rmail_mime_hidden, &
                              search_rmail_mime_message, &
                              rmail_mime_render_html_function, rmail_mime_prefer_html
    use test_rmail_support_mod, only: set_up_rmail, leave_rmail, visit_rmail
    implicit none(type, external)
    private

    public :: collect_rmail_mime_mod

    character(len=*), parameter :: LF = achar(10)

    !> A MIME message whose plain text part is quoted-printable and whose
    !> HTML part is base64.
    character(len=*), parameter :: MIME_MBOX = &
                                   "From alice@example.com  Mon Nov  2 10:00:00 2026"//LF// &
                                   "From: Alice <alice@example.com>"//LF// &
                                   "To: me@example.org"//LF// &
                                   "Subject: MIME message"//LF// &
                                   "MIME-Version: 1.0"//LF// &
                                   'Content-Type: multipart/alternative; boundary="b1"'//LF// &
                                   "X-RMAIL-ATTRIBUTES: ------U-"//LF// &
                                   LF// &
                                   "--b1"//LF// &
                                   "Content-Type: text/plain; charset=utf-8"//LF// &
                                   "Content-Transfer-Encoding: quoted-printable"//LF// &
                                   LF// &
                                   "Caf=C3=A9 au lait."//LF// &
                                   "--b1"//LF// &
                                   "Content-Type: text/html; charset=utf-8"//LF// &
                                   "Content-Transfer-Encoding: base64"//LF// &
                                   LF// &
                                   "PHA+SFRNTDwvcD4K"//LF// &
                                   "--b1--"//LF// &
                                   LF

contains

    !> Collect all exported unit tests.
    subroutine collect_rmail_mime_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rmailmm-test-handler", test_handler), &
                    new_unittest("rmailmm-test-bulk-handler", test_bulk_handler), &
                    new_unittest("rmailmm-test-multipart-handler", test_multipart_handler), &
                    new_unittest("rmailmm-bulk-handler-unibyte", test_bulk_handler_unibyte), &
                    new_unittest("rmailmm-show-alternative", test_show_alternative), &
                    new_unittest("rmailmm-toggle-raw", test_toggle_raw), &
                    new_unittest("rmailmm-search-decoded", test_search_decoded) &
                    ]
    end subroutine collect_rmail_mime_mod

    ! Make a new buffer current, unibyte unless MULTIBYTE, holding TEXT.
    subroutine temporary_buffer(text, multibyte, h)
        character(len=*), intent(in) :: text
        logical, intent(in) :: multibyte
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(generate_new_buffer_name(" *rmailmm-test*"), h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        buffers(current)%multibyte = multibyte
        call insert(text, st)
    end subroutine temporary_buffer

    !> Test of a mail using no MIME parts at all.  Ports
    !> rmailmm-tests.el:rmailmm-test-handler (in a multibyte buffer holding
    !> the bytes as raw bytes: upstream decodes in a unibyte buffer and then
    !> makes it multibyte, and set-buffer-multibyte is Deferred in
    !> buffer_mod).
    subroutine test_handler(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: HEADER = &
                                       "To: alex@gnu.org"//LF// &
                                       "Content-Type: text/plain; charset=koi8-r"//LF// &
                                       "Content-Transfer-Encoding: 8bit"//LF// &
                                       "MIME-Version: 1.0"//LF// &
                                       LF
        ! "Здравствуйте!" in KOI8-R, and in UTF-8.
        character(len=*), parameter :: KOI8 = char(250)//char(196)//char(210)//char(193)//char(215)// &
                                       char(211)//char(212)//char(215)//char(213)//char(202)//char(212)// &
                                       char(197)//"!"
        character(len=*), parameter :: UTF8 = &
                                       char(208)//char(151)//char(208)//char(180)//char(209)//char(128)// &
                                       char(208)//char(176)//char(208)//char(178)//char(209)//char(129)// &
                                       char(209)//char(130)//char(208)//char(178)//char(209)//char(131)// &
                                       char(208)//char(185)//char(209)//char(130)//char(208)//char(181)//"!"
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(HEADER//string_to_multibyte(KOI8), .true., h)
        call show_rmail_mime_buffer(st, .true.)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == HEADER//UTF8//LF, buffer_string())
        call close_buffer(h, st)
    end subroutine test_handler

    !> Test of a mail used as an example in RFC 2183: an attachment in a
    !> multibyte buffer, whose data upstream asserts to be unibyte.
    !> Ports rmailmm-tests.el:rmailmm-test-bulk-handler (upstream marks it
    !> unstable and has no condition for success: the assertion fails).
    subroutine test_bulk_handler(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(genome_mail(), .true., h)
        call show_rmail_mime_buffer(st)
        call check(error,.not. st%ok(), buffer_string())
        call close_buffer(h, st)
    end subroutine test_bulk_handler

    !> The RFC 2183 attachment in a unibyte buffer: its tag line with the
    !> Save button and the size of the decoded data.
    subroutine test_bulk_handler_unibyte(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(genome_mail(), .false., h)
        call show_rmail_mime_buffer(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == LF//"[image/jpeg Show Save:genome.jpeg (337B)]"//LF//LF, buffer_string())
        call close_buffer(h, st)
    end subroutine test_bulk_handler_unibyte

    ! The mail of rmailmm-test-bulk-handler.
    function genome_mail() result(mail)
        character(len=:), allocatable :: mail

        mail = "Content-Type: image/jpeg"//LF// &
               "Content-Disposition: attachment; filename=genome.jpeg;"//LF// &
               '  modification-date="Wed, 12 Feb 1997 16:29:51 -0500";'//LF// &
               "Content-Description: a complete map of the human genome"//LF// &
               "Content-Transfer-Encoding: base64"//LF// &
               LF// &
               "iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAABGdBTUEAALGPC/xhBQAAAAZQ"//LF// &
               "TFRF////AAAAVcLTfgAAAPZJREFUeNq9ldsOwzAIQ+3//+l1WlvA5ZLsoUiTto4TB+ISoAjy"//LF// &
               "+ITfRBfcAmgRFFeAm+J6uhdKdFhFWUgDkFsK0oUp/9G2//Kj7Jx+5tSKOdBscgUYiKHRS/me"//LF// &
               "WATQdRUvAK0Bnmshmtn79PpaLBbbOZkjKvRnjRZoRswOkG1wFchKew2g9wXVJVZL/m4+B+vv"//LF// &
               "9AxQQR2Q33SgAYJzzVACdAWjAfRYzYFO9n6SLnydtQHSMxYDMAKqZ/8FS/lTK+zuq3CtK64L"//LF// &
               "UDwbgUEAUmk2Zyg101d6PhCDySgAvTvDgKiuOrc4dLxUb7UMnhGIexyI+d6U+ABuNAP4Simx"//LF// &
               "lgAAAABJRU5ErkJggg=="//LF
    end function genome_mail

    !> Test of a mail used as an example in RFC 2046.  Ports
    !> rmailmm-tests.el:rmailmm-test-multipart-handler (upstream marks it
    !> unstable and expects it to fail; the condition here is what the
    !> code does: the top-level header and the preamble are deleted with
    !> the text before the first boundary, and each part is shown without
    !> its header).
    subroutine test_multipart_handler(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: MAIL = &
                                       "From: Nathaniel Borenstein <nsb@bellcore.com>"//LF// &
                                       "To: Ned Freed <ned@innosoft.com>"//LF// &
                                       "Date: Sun, 21 Mar 1993 23:56:48 -0800 (PST)"//LF// &
                                       "Subject: Sample message"//LF// &
                                       "MIME-Version: 1.0"//LF// &
                                       'Content-type: multipart/mixed; boundary="simple boundary"'//LF// &
                                       LF// &
                                       "This is the preamble.  It is to be ignored, though it"//LF// &
                                       "is a handy place for composition agents to include an"//LF// &
                                       "explanatory note to non-MIME conformant readers."//LF// &
                                       LF// &
                                       "--simple boundary"//LF// &
                                       LF// &
                                       "This is implicitly typed plain US-ASCII text."//LF// &
                                       "It does NOT end with a linebreak."//LF// &
                                       "--simple boundary"//LF// &
                                       "Content-type: text/plain; charset=us-ascii"//LF// &
                                       LF// &
                                       "This is explicitly typed plain US-ASCII text."//LF// &
                                       "It DOES end with a linebreak."//LF// &
                                       LF// &
                                       "--simple boundary--"//LF// &
                                       LF// &
                                       "This is the epilogue.  It is also to be ignored."
        character(len=*), parameter :: SHOWN = &
                                       "This is implicitly typed plain US-ASCII text."//LF// &
                                       "It does NOT end with a linebreak."//LF// &
                                       "This is explicitly typed plain US-ASCII text."//LF// &
                                       "It DOES end with a linebreak."//LF
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(MAIL, .true., h)
        call show_rmail_mime_buffer(st, .true.)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == SHOWN, buffer_string())
        call close_buffer(h, st)
    end subroutine test_multipart_handler

    ! Visit MIME_MBOX in Rmail with no HTML renderer; FILE is its name.
    subroutine visit_mime_mbox(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status

        call set_up_rmail(file)
        rmail_mime_render_html_function => null()
        rmail_mime_prefer_html = .false.
        call visit_rmail(file, MIME_MBOX, status)
    end subroutine visit_mime_mbox

    !> A multipart/alternative message shows each part with a tag line: its
    !> plain text part decoded, and its HTML part hidden, with the button
    !> that shows it and the Save button.
    subroutine test_show_alternative(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: CAFE = "Caf"//char(195)//char(169)//" au lait."
        character(len=:), allocatable :: file, text
        integer(int64) :: found
        type(status_type) :: st

        call visit_mime_mbox(file, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, "Subject: MIME message") > 0, text)
        if (allocated(error)) return
        call check(error, index(text, CAFE//LF) > 0, text)
        if (allocated(error)) return
        call check(error, index(text, LF//"[1:text/plain Hide]"//LF//LF//CAFE//LF) > 0, text)
        if (allocated(error)) return
        call check(error, index(text, LF//"[2:text/html Show Save:noname (12B)]"//LF//LF) > 0, text)
        if (allocated(error)) return
        call check(error, index(text, "<p>HTML</p>") == 0, text)
        if (allocated(error)) return
        ! Show the HTML part.
        call goto_character(point_minimum())
        call search_forward("[2:", found, st, noerror=SEARCH_NOERROR)
        call check(error, found > 0, text)
        if (allocated(error)) return
        call toggle_rmail_mime_hidden(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, "[2:text/html Hide Save:noname (12B)]"//LF//LF//"<p>HTML</p>"//LF) > 0, text)
        if (allocated(error)) return
        call check(error, index(text, CAFE//LF) > 0, text)
        call leave_rmail(file)
    end subroutine test_show_alternative

    !> \[toggle_rmail_mime] shows the raw message, then the decoded one.
    subroutine test_toggle_raw(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, text
        type(status_type) :: st

        call visit_mime_mbox(file, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call toggle_rmail_mime(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, "PHA+SFRNTDwvcD4K") > 0, text)
        if (allocated(error)) return
        call check(error, index(text, "Caf=C3=A9 au lait.") > 0, text)
        if (allocated(error)) return
        call toggle_rmail_mime(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, "PHA+SFRNTDwvcD4K") == 0, text)
        if (allocated(error)) return
        call check(error, index(text, "[2:text/html Show Save:noname (12B)]") > 0, text)
        call leave_rmail(file)
    end subroutine test_toggle_raw

    !> Searching a MIME message searches its decoded parts, those shown.
    subroutine test_search_decoded(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st
        logical :: found

        call visit_mime_mbox(file, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! search_rmail_mime_message works in the message collection.
        call set_buffer(get_file_buffer(file), st)
        call swap_rmail_buffers_maybe(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call search_rmail_mime_message(1_int64, "Caf"//char(195)//char(169), found, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, found)
        if (allocated(error)) return
        ! The hidden part of an alternative is not searched.
        call search_rmail_mime_message(1_int64, "<p>HTML", found, st)
        call check(error,.not. found)
        if (allocated(error)) return
        call search_rmail_mime_message(1_int64, "not in the message", found, st)
        call check(error,.not. found)
        call leave_rmail(file)
    end subroutine test_search_decoded
end module test_rmail_mime_mod
