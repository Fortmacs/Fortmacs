! test_message_mod.f90 -- tests for message_mod.
! Ports GNU Emacs 31.1 test/lisp/gnus/message-tests.el in part; the
! others need what is not ported (syntax properties, epg, mml, the
! asking form of message-strip-subject-trailing-was, gnus-util.el).
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

!> Message mode: composing with \[compose_mail] and the default
!> `message_user_agent', the headers Message generates, sending through
!> test/sendmail-resources/fake-sendmail (a shell script that writes what
!> it gets to a file) with Fcc, and the field commands.
module test_message_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, VOID_FUNCTION_CODE
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_buffer, set_buffer, close_buffer, is_buffer_live, current_buffer, &
                          get_or_make_buffer, is_buffer_modified
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point, point_minimum, point_maximum
    use bindings_mod, only: initialize_bindings
    use test_support_mod, only: show_alone
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, file_exists, initialize_fileio
    use files_mod, only: create_temporary_file, delete_file
    use environment_mod, only: setenv
    use startup_mod, only: user_mail_address
    use simple_mod, only: mail_user_agent, compose_mail
    use mail_extract_mod, only: mail_address_components_type
    use send_mail_mod, only: send_mail_function, send_mail_with_sendmail, sendmail_program
    use message_mod, only: message_user_agent, message_signature, message_auto_save_directory, &
                           message_user_fully_qualified_domain_name, message_from_style, get_message_all_recipients, &
                           replace_message_header, get_message_field_value, make_message_from, make_message_id, &
                           send_message_and_exit, send_message, sort_message_headers, generate_message_headers, &
                           message_required_mail_headers, message_header_type, set_message_reply_headers, &
                           message_reply_headers_type, fold_message_field, encode_message_body, &
                           MESSAGE_FROM_STYLE_ANGLES, MESSAGE_FROM_STYLE_PARENTHESES, MESSAGE_FROM_STYLE_SIMPLE, &
                           position_message_on_field, narrow_to_message_headers, message_syntax_checks
    implicit none(type, external)
    private

    public :: collect_message_mod

    character(len=*), parameter :: LF = achar(10)
    character(len=*), parameter :: SEPARATOR = "--text follows this line--"

contains

    !> Collect all exported unit tests.
    subroutine collect_message_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("message-all-recipients", test_all_recipients), &
                    new_unittest("message-replace-header", test_replace_header), &
                    new_unittest("message-compose-mail-default-agent", test_compose), &
                    new_unittest("message-send-with-sendmail", test_send), &
                    new_unittest("message-send-fcc", test_fcc), &
                    new_unittest("message-send-method-header", test_method_header), &
                    new_unittest("message-send-non-ascii-body", test_non_ascii), &
                    new_unittest("message-syntax-checks", test_syntax_checks), &
                    new_unittest("message-make-from", test_make_from), &
                    new_unittest("message-make-message-id", test_message_id), &
                    new_unittest("message-reply-headers", test_reply_headers), &
                    new_unittest("message-sort-headers", test_sort), &
                    new_unittest("message-fold-field", test_fold), &
                    new_unittest("message-position-on-field", test_position_on_field) &
                    ]
    end subroutine collect_message_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The editor with known settings: no signature, no drafts, a known
    !> address and domain, sending with the fake sendmail into OUTPUT.
    subroutine set_up(output)
        character(len=:), allocatable, intent(out), optional :: output
        character(len=:), allocatable :: program, file
        type(status_type) :: st

        call initialize_bindings()
        mail_user_agent => message_user_agent
        message_signature = .false.
        if (allocated(message_auto_save_directory)) deallocate (message_auto_save_directory)
        message_user_fully_qualified_domain_name = "grassland.com"
        message_from_style = MESSAGE_FROM_STYLE_SIMPLE
        user_mail_address = "king@grassland.com"
        send_mail_function => send_mail_with_sendmail
        call initialize_fileio()
        call expand_file_name("test/sendmail-resources/fake-sendmail", program, st, fortmacs_wd)
        sendmail_program = program
        call setenv("FORTMACS_TEST_SENDMAIL_EXIT")
        call setenv("FORTMACS_TEST_SENDMAIL_ERROR")

        if (present(output)) then
            call create_temporary_file("message-test", file, st)
            output = file
            call setenv("FORTMACS_TEST_SENDMAIL_OUTPUT", output)
        end if
    end subroutine set_up

    !> Close the message buffers and delete FILE if present.
    subroutine leave(file)
        character(len=*), intent(in), optional :: file
        character(len=*), parameter :: NAMES(4) = [character(len=24) :: "*unsent mail*", "*unsent mail to joe@x*", &
                                                                                             "*sent mail to joe@x*", "message test"]
        type(status_type) :: st
        type(handle_type) :: h
        integer :: k

        do k = 1, size(NAMES)
            h = get_buffer(trim(NAMES(k)))
            if (is_buffer_live(h)) call close_buffer(h, st)
        end do

        if (present(file)) then
            if (file_exists(file)) call delete_file(file, st)
        end if
    end subroutine leave

    !> A plain buffer named "message test" holding TEXT, point at the start.
    subroutine test_buffer(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: h
        type(status_type) :: st

        call get_or_make_buffer("message test", h, st)
        call set_buffer(h, st)
        call goto_character(point_minimum())
        call insert(text, st)
        call goto_character(point_minimum())
    end subroutine test_buffer

    !> The contents of FILE.
    function file_text(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text
        integer :: unit, size_bytes, ios

        text = ""
        open (newunit=unit, file=file, access="stream", form="unformatted", status="old", action="read", iostat=ios)
        if (ios /= 0) return
        inquire (unit=unit, size=size_bytes)

        if (size_bytes > 0) then
            deallocate (text)
            allocate (character(len=size_bytes) :: text)
            read (unit, iostat=ios) text
            if (ios /= 0) text = ""
        end if

        close (unit)
    end function file_text

    subroutine check_string(error, got, wanted)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: got, wanted

        call check(error, got == wanted, "got """//got//""", wanted """//wanted//"""")
    end subroutine check_string

    !> A message buffer to joe@x with subject Hi and the body "Hello".
    subroutine compose(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call compose_mail(st, to="joe@x", subject="Hi")
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call show_alone()
        call goto_character(point_maximum())
        call insert("Hello"//LF, st)
    end subroutine compose

    ! ------------------------------------------------------------------
    ! Ported from message-tests.el
    ! ------------------------------------------------------------------

    subroutine test_all_recipients(error)
        type(error_type), allocatable, intent(out) :: error
        type(mail_address_components_type), allocatable :: recipients(:)
        type(status_type) :: st
        integer :: k
        character(len=1) :: digit

        call set_up()
        call test_buffer("To: Person 1 <p1@p1.org>, Person 2 <p2@p2.org>"//LF// &
                         "Cc: Person 3 <p3@p3.org>, Person 4 <p4@p4.org>"//LF// &
                         "Bcc: Person 5 <p5@p5.org>, Person 6 <p6@p6.org>"//LF)
        call get_message_all_recipients(recipients, st)
        call check(error, st%ok() .and. size(recipients) == 6, format_error(st))

        do k = 1, size(recipients)
            if (allocated(error)) exit
            write (digit, "(i1)") k
            call check_string(error, recipients(k)%full_name, "Person "//digit)
            if (.not. allocated(error)) call check_string(error, recipients(k)%address, "p"//digit//"@p"//digit//".org")
        end do

        call leave()
    end subroutine test_all_recipients

    subroutine test_replace_header(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: value, text

        call set_up()
        call test_buffer("From: dang@gnus.org"//LF//"To: user1,"//LF//"    user2"//LF//"Cc: user3,"//LF//"    user4"//LF// &
                         SEPARATOR//LF//"Hello."//LF)
        call replace_message_header("From", "ding@gnus.org", st)
        call get_message_field_value("From", value, st)
        call check(error, index(value, "ding") > 0, value)
        if (allocated(error)) return
        call replace_message_header("From", "dong@gnus.org", st, [character(len=2) :: "To"])
        call get_message_field_value("From", value, st)
        call check(error, index(value, "dong") > 0, value)
        if (allocated(error)) return
        ! The From field comes after To (and its continuation line).
        text = buffer_string()
        call check(error, index(text, "To:") < index(text, "From:") .and. index(text, "From:") < index(text, "Cc:"), text)
        if (allocated(error)) return
        call replace_message_header("From", "dang@gnus.org", st, [character(len=2) :: "To", "Cc"])
        call get_message_field_value("From", value, st)
        call check(error, index(value, "dang") > 0, value)
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, "To:") < index(text, "From:"), text)
        call leave()
    end subroutine test_replace_header

    ! ------------------------------------------------------------------
    ! Composing and sending
    ! ------------------------------------------------------------------

    subroutine test_compose(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: modified

        call set_up()
        call compose_mail(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check_string(error, buffers(current)%name, "*unsent mail*")
        if (.not. allocated(error)) call check(error, buffers(current)%major_mode == "message_mode", "not in Message mode")
        if (.not. allocated(error)) call check_string(error, buffer_string(), "To: "//LF//"Subject: "//LF//SEPARATOR//LF)
        if (.not. allocated(error)) call check(error, point() == 5, "point is not after To:")
        call is_buffer_modified(current_buffer(), modified, st)
        if (.not. allocated(error)) call check(error,.not. modified, "the buffer is modified")
        call leave()
    end subroutine test_compose

    subroutine test_send(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output, text

        call set_up(output)
        call compose(error)
        if (allocated(error)) return
        call check_string(error, buffers(current)%name, "*unsent mail to joe@x*")
        if (allocated(error)) return
        call send_message_and_exit(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = file_text(output)
        call check(error, index(text, "-oi -f king@grassland.com -t"//LF) == 1, text)
        if (.not. allocated(error)) call check(error, index(text, LF//"To: joe@x"//LF//"Subject: Hi"//LF// &
                                                            "From: king@grassland.com"//LF//"Date: ") > 0, text)
        if (.not. allocated(error)) call check(error, index(text, LF//"Message-ID: <") > 0 .and. &
                                               index(text, "@grassland.com>"//LF) > 0, text)
        if (.not. allocated(error)) call check(error, index(text, LF//"MIME-Version: 1.0"//LF//"Content-Type: text/plain; "// &
                                                            "charset=us-ascii"//LF//LF//"Hello"//LF) > 0, text)
        if (.not. allocated(error)) call check(error, index(text, SEPARATOR) == 0, text)
        ! The buffer is renamed after sending.
        if (.not. allocated(error)) call check(error, is_buffer_live(get_buffer("*sent mail to joe@x*")), "not renamed")
        call leave(output)
    end subroutine test_send

    subroutine test_fcc(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output, fcc, text

        call set_up(output)
        call create_temporary_file("message-fcc", fcc, st)
        call compose(error)
        if (allocated(error)) return
        call goto_character(point_minimum())
        call insert("Fcc: "//fcc//LF, st)
        call send_message(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = file_text(fcc)
        call check(error, index(text, "From nobody ") == 1, text)
        if (.not. allocated(error)) call check(error, index(text, LF//"To: joe@x"//LF) > 0 .and. index(text, "Fcc:") == 0, text)
        if (.not. allocated(error)) call check(error, index(file_text(output), "Fcc:") == 0, file_text(output))
        call leave(output)
        call delete_file(fcc, st)
    end subroutine test_fcc

    subroutine test_method_header(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output, text

        call set_up(output)
        send_mail_function => null()
        call compose(error)
        if (allocated(error)) return
        call goto_character(point_minimum())
        call insert("X-Message-SMTP-Method: sendmail"//LF, st)
        call send_message(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = file_text(output)
        call check(error, index(text, "-oi -f king@grassland.com -t"//LF) == 1, text)
        if (.not. allocated(error)) call check(error, index(text, "X-Message-SMTP-Method") == 0, text)
        send_mail_function => send_mail_with_sendmail
        call leave(output)
    end subroutine test_method_header

    subroutine test_non_ascii(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output

        call set_up(output)
        call compose(error)
        if (allocated(error)) return
        call goto_character(point_maximum())
        call insert("Grüße"//LF, st)
        call send_message(st)
        ! MML is not ported: the message is not sent.
        call check(error, st%code == VOID_FUNCTION_CODE, format_error(st))
        if (.not. allocated(error)) call check(error, len(file_text(output)) == 0, file_text(output))
        call leave(output)
    end subroutine test_non_ascii

    subroutine test_syntax_checks(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output, text

        call set_up(output)
        ! Upstream's default disables the sender check only.
        call check(error,.not. message_syntax_checks%sender .and. message_syntax_checks%illegible_text, "defaults")
        if (allocated(error)) return
        call compose(error)
        if (allocated(error)) return
        call goto_character(point_maximum())
        call insert("a"//achar(1)//"b"//LF, st)
        ! The illegible-text check would ask; disabled, the text is sent.
        message_syntax_checks%illegible_text = .false.
        call send_message(st)
        message_syntax_checks%illegible_text = .true.
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = file_text(output)
        call check(error, index(text, "a"//achar(1)//"b") > 0 .and. index(text, "Sender:") == 0, text)
        call leave(output)
    end subroutine test_syntax_checks

    ! ------------------------------------------------------------------
    ! The headers
    ! ------------------------------------------------------------------

    subroutine test_make_from(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: from

        call set_up()
        call make_message_from(from, st, name="Elvis Parsley", address="king@grassland.com")
        call check_string(error, from, "king@grassland.com")
        if (allocated(error)) return
        message_from_style = MESSAGE_FROM_STYLE_ANGLES
        call make_message_from(from, st, name="Elvis Parsley", address="king@grassland.com")
        call check_string(error, from, "Elvis Parsley <king@grassland.com>")
        if (allocated(error)) return
        call make_message_from(from, st, name="Parsley, Elvis", address="king@grassland.com")
        call check_string(error, from, """Parsley, Elvis"" <king@grassland.com>")
        if (allocated(error)) return
        message_from_style = MESSAGE_FROM_STYLE_PARENTHESES
        call make_message_from(from, st, name="Elvis (The King)", address="king@grassland.com")
        call check_string(error, from, "king@grassland.com (Elvis (The King))")
        if (allocated(error)) return
        call make_message_from(from, st, name="Elvis :-)", address="king@grassland.com")
        call check_string(error, from, "king@grassland.com (Elvis :-\))")
        message_from_style = MESSAGE_FROM_STYLE_SIMPLE
        call leave()
    end subroutine test_make_from

    subroutine test_message_id(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: id

        call set_up()
        call test_buffer("Subject: Hi"//LF//SEPARATOR//LF)
        call make_message_id(id, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, id(1:1) == "<" .and. index(id, ".fsf@grassland.com>") == len(id) - 18, id)
        if (allocated(error)) return
        call goto_character(point_minimum())
        call insert("Supersedes: <abc_-_@x>"//LF, st)
        call make_message_id(id, st)
        call check(error, index(id, ".fsf_-_@grassland.com>") > 0, id)
        call leave()
    end subroutine test_message_id

    subroutine test_reply_headers(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(message_reply_headers_type) :: reply
        character(len=:), allocatable :: id, text
        type(message_header_type) :: headers(2)

        call set_up()
        call test_buffer("Subject: Re: Other"//LF//SEPARATOR//LF)
        reply%subject = "Original"
        reply%from = "Joe <joe@x>"
        reply%id = "<1@x>"
        reply%references = "<0@x>"
        call set_message_reply_headers(reply)
        ! A changed subject marks the Message-ID.
        call make_message_id(id, st)
        call check(error, index(id, "_-_@grassland.com>") > 0, id)
        if (allocated(error)) return
        headers(1) = message_header_type("In-Reply-To", optional=.true.)
        headers(2) = message_header_type("References", optional=.true.)
        call generate_message_headers(headers, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = buffer_string()
        call check(error, index(text, LF//"In-Reply-To: <1@x>"//LF) > 0 .and. index(text, LF//"References: <0@x> <1@x>"//LF) > 0, &
                   text)
        call leave()
    end subroutine test_reply_headers

    subroutine test_sort(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        call test_buffer("X-Foo: 1"//LF//"Subject: Hi"//LF//"To: a,"//LF//" b"//LF//"From: me"//LF//SEPARATOR//LF//"Body"//LF)
        call sort_message_headers(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), "From: me"//LF//"To: a,"//LF//" b"//LF// &
                                                      "Subject: Hi"//LF//"X-Foo: 1"//LF//SEPARATOR//LF//"Body"//LF)
        call leave()
    end subroutine test_sort

    subroutine test_fold(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: e
        character(len=:), allocatable :: words

        call set_up()
        words = repeat("word ", 20)
        call test_buffer("Keywords: "//words(:len(words) - 1)//LF//SEPARATOR//LF)
        call fold_message_field(e, st)
        call check(error, st%ok(), format_error(st))
        ! Broken after the last whitespace that keeps the line within 76.
        if (.not. allocated(error)) call check_string(error, buffer_string(), "Keywords: "//repeat("word ", 12)//"word"//LF// &
                                                      " "//repeat("word ", 6)//"word"//LF//SEPARATOR//LF)
        call leave()
    end subroutine test_fold

    subroutine test_position_on_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        call test_buffer("To: joe@x"//LF//"Subject: Hi"//LF//SEPARATOR//LF)
        call position_message_on_field("Cc", st, [character(len=2) :: "To"])
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), "To: joe@x"//LF//"Cc: "//LF//"Subject: Hi"//LF// &
                                                      SEPARATOR//LF)
        if (.not. allocated(error)) call check(error, point() == len("To: joe@x"//LF//"Cc: ") + 1, "point not after Cc:")
        call leave()
    end subroutine test_position_on_field
end module test_message_mod
