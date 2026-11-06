! test_send_mail_mod.f90 -- tests for send_mail_mod.
! Own tests; upstream has no sendmail-tests.el.
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

!> Mail mode and sending: setting up the *mail* buffer, the field
!> commands, the From field, `compose_mail' with `sendmail_user_agent',
!> and sending through test/sendmail-resources/fake-sendmail (a shell
!> script that writes what it gets to a file) with Fcc and the errors.
module test_send_mail_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_buffer, set_buffer, close_buffer, is_buffer_live, is_buffer_modified, &
                          current_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point, point_minimum, point_maximum, delete_region
    use bindings_mod, only: initialize_bindings
    use test_support_mod, only: show_alone
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, file_exists, initialize_fileio
    use files_mod, only: create_temporary_file, delete_file
    use environment_mod, only: setenv
    use startup_mod, only: user_mail_address
    use simple_mod, only: mail_user_agent, compose_mail
    use send_mail_mod, only: mail, mail_cc, mail_subject, mail_to, insert_mail_from_field, send_mail, send_mail_with_sendmail, &
                             mail_signature, set_up_mail_with_from, mail_signature_variable, &
                             mail_from_style, send_mail_function, sendmail_program, mail_archive_file_name, &
                             sendmail_user_agent, mail_header_end, mail_text_start, &
                             MAIL_FROM_STYLE_SIMPLE, MAIL_FROM_STYLE_ANGLES, MAIL_NOERASE_NO, mail_specify_envelope_from, &
                             mail_envelope_from, MAIL_ENVELOPE_FROM_HEADER
    implicit none(type, external)
    private

    public :: collect_send_mail_mod

    character(len=*), parameter :: LF = achar(10)
    character(len=*), parameter :: SEPARATOR = "--text follows this line--"

contains

    !> Collect all exported unit tests.
    subroutine collect_send_mail_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("mail-setup", test_set_up_mail), &
                    new_unittest("mail-setup-empty", test_set_up_mail_empty), &
                    new_unittest("mail-field-commands", test_field_commands), &
                    new_unittest("mail-from-field", test_from_field), &
                    new_unittest("mail-signature", test_signature), &
                    new_unittest("compose-mail-agent", test_compose_mail), &
                    new_unittest("send-with-sendmail", test_send), &
                    new_unittest("send-with-sendmail-fcc", test_send_fcc), &
                    new_unittest("send-with-sendmail-failure", test_send_failure), &
                    new_unittest("send-with-sendmail-error-output", test_send_error_output), &
                    new_unittest("send-with-sendmail-no-recipients", test_no_recipients), &
                    new_unittest("send-with-sendmail-envelope-from", test_envelope_from) &
                    ]
    end subroutine collect_send_mail_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The editor with known settings: no From field, no signature, a
    !> known address, sending with the fake sendmail into OUTPUT.
    subroutine set_up(output)
        character(len=:), allocatable, intent(out), optional :: output
        character(len=:), allocatable :: program, file
        type(status_type) :: st

        call initialize_bindings()
        set_up_mail_with_from = .false.
        mail_signature_variable = .false.
        mail_from_style = MAIL_FROM_STYLE_SIMPLE
        user_mail_address = "king@grassland.com"
        send_mail_function => send_mail_with_sendmail
        call initialize_fileio()
        call expand_file_name("test/sendmail-resources/fake-sendmail", program, st, fortmacs_wd)
        sendmail_program = program
        call setenv("FORTMACS_TEST_SENDMAIL_EXIT")
        call setenv("FORTMACS_TEST_SENDMAIL_ERROR")

        if (present(output)) then
            call create_temporary_file("sendmail-test", file, st)
            output = file
            call setenv("FORTMACS_TEST_SENDMAIL_OUTPUT", output)
        end if
    end subroutine set_up

    !> Close *mail* and delete FILE if present.
    subroutine leave(file)
        character(len=*), intent(in), optional :: file
        type(status_type) :: st
        type(handle_type) :: h

        h = get_buffer("*mail*")
        if (is_buffer_live(h)) call close_buffer(h, st)

        if (present(file)) then
            if (file_exists(file)) call delete_file(file, st)
        end if

        if (allocated(mail_archive_file_name)) deallocate (mail_archive_file_name)
    end subroutine leave

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

    !> A *mail* buffer to joe@x with subject Hi and the body "Hello".
    subroutine compose(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call mail(st, MAIL_NOERASE_NO, to="joe@x", subject="Hi")
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call show_alone()
        call goto_character(point_maximum())
        call insert("Hello"//LF, st)
    end subroutine compose

    ! ------------------------------------------------------------------
    ! Composing
    ! ------------------------------------------------------------------

    subroutine test_set_up_mail(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: initialized

        call set_up()
        call mail(st, MAIL_NOERASE_NO, to="joe@x", subject="Hi", initialized=initialized)
        call check(error, st%ok() .and. initialized, format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), "To: joe@x"//LF//"Subject: Hi"//LF//SEPARATOR//LF)
        if (.not. allocated(error)) call check(error, buffers(current)%major_mode == "mail_mode", "not in Mail mode")
        if (.not. allocated(error)) call check(error, point() == point_maximum(), "point is not after the separator")
        if (.not. allocated(error)) call check(error, mail_header_end() == len("To: joe@x"//LF//"Subject: Hi"//LF) + 1, &
                                               "mail_header_end")
        if (.not. allocated(error)) call check(error, mail_text_start() == point_maximum(), "mail_text_start")
        call leave()
    end subroutine test_set_up_mail

    subroutine test_set_up_mail_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: modified

        call set_up()
        call mail(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check_string(error, buffer_string(), "To: "//LF//"Subject: "//LF//SEPARATOR//LF)
        if (.not. allocated(error)) call check(error, point() == 5, "point is not after To:")
        call is_buffer_modified(current_buffer(), modified, st)
        if (.not. allocated(error)) call check(error,.not. modified, "the buffer is modified")
        call leave()
    end subroutine test_set_up_mail_empty

    subroutine test_field_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        call mail(st, MAIL_NOERASE_NO, to="joe@x", subject="Hi")
        call mail_cc(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check_string(error, buffer_string(), "To: joe@x"//LF//"CC: "//LF//"Subject: Hi"//LF//SEPARATOR//LF)
        if (.not. allocated(error)) call check(error, point() == len("To: joe@x"//LF//"CC: ") + 1, "point not after CC:")
        if (allocated(error)) return
        call mail_subject(st)
        call check(error, point() == len("To: joe@x"//LF//"CC: "//LF//"Subject: Hi") + 1, "point not after the subject")
        if (allocated(error)) return
        call mail_to(st)
        call check(error, point() == len("To: joe@x") + 1, "point not after To")
        call leave()
    end subroutine test_field_commands

    subroutine test_from_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text

        call set_up()
        call mail(st)
        call goto_character(point_minimum())
        call insert_mail_from_field(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(buffer_string(), "From: king@grassland.com"//LF//"To: ") == 1, &
                                               buffer_string())
        if (allocated(error)) return
        call leave()
        call mail(st)
        mail_from_style = MAIL_FROM_STYLE_ANGLES
        call goto_character(point_minimum())
        call insert_mail_from_field(st)
        text = buffer_string()
        call check(error, index(text, "From: ") == 1 .and. index(text, " <king@grassland.com>"//LF//"To: ") > 0, text)
        call leave()
    end subroutine test_from_field

    subroutine test_signature(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        mail_signature_variable%text = LF//LF//"-- "//LF//"Elvis"
        call mail(st, MAIL_NOERASE_NO, to="joe@x")
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            ! The signature goes at point after the separator (upstream's
            ! ATPOINT), so its own blank lines follow the separator line.
            call check_string(error, buffer_string(), "To: joe@x"//LF//"Subject: "//LF//SEPARATOR//LF//LF//LF//"-- "// &
                              LF//"Elvis"//LF)
        end if

        if (.not. allocated(error)) call check(error, point() == len("To: joe@x"//LF//"Subject: "//LF//SEPARATOR//LF) + 1, &
                                               "point is not at the text")
        mail_signature_variable = .false.
        call leave()
    end subroutine test_signature

    subroutine test_compose_mail(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        mail_user_agent => null()
        call compose_mail(st)
        call check(error,.not. st%ok(), "no agent but no error")
        if (.not. allocated(error)) call check(error, index(format_error(st), "Invalid value for `mail_user_agent'") > 0, &
                                               format_error(st))
        if (allocated(error)) return
        mail_user_agent => sendmail_user_agent
        call compose_mail(st, to="ann@y")
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffers(current)%name == "*mail*", "not in *mail*")
        if (.not. allocated(error)) call check(error, index(buffer_string(), "To: ann@y"//LF) == 1, buffer_string())
        mail_user_agent => null()
        call leave()
    end subroutine test_compose_mail

    ! ------------------------------------------------------------------
    ! Sending
    ! ------------------------------------------------------------------

    subroutine test_send(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output
        logical :: modified

        call set_up(output)
        call compose(error)
        if (allocated(error)) return
        call send_mail(st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call check_string(error, file_text(output), "-oi -oep -odi -t"//LF//"From: king@grassland.com"//LF// &
                              "To: joe@x"//LF//"Subject: Hi"//LF//LF//"Hello"//LF)
        end if

        ! The mail buffer is unchanged and unmodified.
        if (.not. allocated(error)) then
            call check_string(error, buffer_string(), "To: joe@x"//LF//"Subject: Hi"//LF//SEPARATOR//LF//"Hello"//LF)
        end if

        call is_buffer_modified(current_buffer(), modified, st)
        if (.not. allocated(error)) call check(error,.not. modified, "the buffer is modified")
        call leave(output)
    end subroutine test_send

    subroutine test_send_fcc(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output, fcc, text

        call set_up(output)
        call create_temporary_file("sendmail-fcc", fcc, st)
        call delete_file(fcc, st)
        mail_archive_file_name = fcc
        call compose(error)
        if (allocated(error)) return
        call send_mail(st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call check_string(error, file_text(output), "-oi -oep -odi -t"//LF//"From: king@grassland.com"//LF// &
                              "To: joe@x"//LF//"Subject: Hi"//LF//LF//"Hello"//LF)
        end if

        if (.not. allocated(error)) then
            text = file_text(fcc)
            call check(error, index(text, "From ") == 1 .and. index(text, LF//"Date: ") > 0 .and. &
                       index(text, LF//"To: joe@x"//LF//"Subject: Hi"//LF//LF//"Hello"//LF//LF) > 0 .and. &
                       index(text, "Fcc:") == 0, text)
        end if

        ! A second message is appended after a blank line.
        if (.not. allocated(error)) then
            call compose(error)
            call send_mail(st)
            text = file_text(fcc)
            call check(error, index(text, "Hello"//LF//LF//LF//"From ") > 0, text)
        end if

        call leave(output)
        if (file_exists(fcc)) call delete_file(fcc, st)
    end subroutine test_send_fcc

    subroutine test_send_failure(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output

        call set_up(output)
        call setenv("FORTMACS_TEST_SENDMAIL_EXIT", "3")
        call compose(error)
        if (allocated(error)) return
        call send_mail(st)
        call check(error,.not. st%ok(), "no error")
        if (.not. allocated(error)) call check(error, index(format_error(st), "Sending...failed with exit value 3") > 0, &
                                               format_error(st))
        call setenv("FORTMACS_TEST_SENDMAIL_EXIT")
        call leave(output)
    end subroutine test_send_failure

    subroutine test_send_error_output(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output
        type(handle_type) :: errors

        call set_up(output)
        call setenv("FORTMACS_TEST_SENDMAIL_ERROR", "no route to host")
        call compose(error)
        if (allocated(error)) return
        call send_mail(st)
        call check(error,.not. st%ok(), "no error")
        if (.not. allocated(error)) call check(error, index(format_error(st), "Sending...failed to no route to host; ") > 0, &
                                               format_error(st))
        call setenv("FORTMACS_TEST_SENDMAIL_ERROR")
        errors = get_buffer(" sendmail errors")
        if (is_buffer_live(errors)) call close_buffer(errors, st)
        call leave(output)
    end subroutine test_send_error_output

    subroutine test_no_recipients(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output

        call set_up(output)
        call mail(st, MAIL_NOERASE_NO, subject="Hi")
        ! Without the To field.
        call delete_region(point_minimum(), point_minimum() + len("To: "//LF), st)
        call goto_character(point_maximum())
        call insert("Hello"//LF, st)
        call send_mail(st)
        call check(error,.not. st%ok(), "no error")
        if (.not. allocated(error)) call check(error, index(format_error(st), "No recipients") > 0, format_error(st))
        call leave(output)
    end subroutine test_no_recipients

    !> The envelope-from address passed with -f: from the From field with
    !> MAIL_ENVELOPE_FROM_HEADER, then the address `mail_envelope_from'
    !> gives.
    subroutine test_envelope_from(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: output

        call set_up(output)
        mail_specify_envelope_from = .true.
        mail_envelope_from = MAIL_ENVELOPE_FROM_HEADER
        call compose(error)
        if (allocated(error)) return
        call goto_character(point_minimum())
        call insert("From: ""Ann Lee"" <ann@x.org>"//LF, st)
        call send_mail(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(file_text(output), "-oi -f ann@x.org -oep") == 1, file_text(output))
        call leave(output)

        if (.not. allocated(error)) then
            call set_up(output)
            mail_envelope_from%address = "bounce@x.org"
            call compose(error)

            if (.not. allocated(error)) then
                call send_mail(st)
                call check(error, st%ok(), format_error(st))
            end if

            if (.not. allocated(error)) then
                call check(error, index(file_text(output), "-oi -f bounce@x.org -oep") == 1, file_text(output))
            end if

            call leave(output)
        end if

        mail_specify_envelope_from = .false.
        mail_envelope_from = .false.
    end subroutine test_envelope_from
end module test_send_mail_mod
