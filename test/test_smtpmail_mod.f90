! test_smtpmail_mod.f90 -- tests for smtpmail_mod.
! Own tests; upstream has no smtpmail-tests.el.
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

!> Sending mail over SMTP to a fake server: a child process (forked by
!> the test) listening on 127.0.0.1 answers each line the client sends
!> with the next of a list of responses and writes what it received to a
!> file, so no network is needed.  The dialogue, dot-stuffing, the
!> recipients and Bcc, HELO after a refused EHLO, AUTH PLAIN and
!> CRAM-MD5 (RFC 2195's example), a failed AUTH with its message
!> sanitized, refused recipients and greetings, a transient error and
!> its retry, the trace buffer, a refused connection, and queueing then
!> sending the queue.
module test_smtpmail_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use keyboard_mod, only: unread_command_events
    use buffer_mod, only: get_buffer, set_buffer, close_buffer, is_buffer_live, get_or_make_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string, system_name
    use bindings_mod, only: init_bindings
    use fileio_mod, only: file_exists, init_fileio
    use files_mod, only: create_temp_file, delete_file
    use startup_mod, only: user_mail_address
    use sendmail_mod, only: mail_from_style, mail_specify_envelope_from, mail_envelope_from, MAIL_FROM_STYLE_SIMPLE, &
                            MAIL_ENVELOPE_FROM_HEADER
    use smtpmail_mod, only: send_mail_with_smtp, send_queued_mail_with_smtp, smtpmail_smtp_server, smtpmail_smtp_service, &
                            smtpmail_debug_info, smtpmail_queue_mail, smtpmail_queue_dir, &
                            smtpmail_servers_requiring_authorization, smtpmail_smtp_user, smtpmail_auth_supported
    use sysdep_mod, only: close_fd, fortmacs_read, fortmacs_creat, fortmacs_full_write, wait_for_termination
    use sys_socket_mod, only: fortmacs_listen_loopback, fortmacs_accept, fortmacs_send, fortmacs_fork, fortmacs_exit_now
    use string_type_mod, only: string_type
    implicit none(type, external)
    private

    public :: collect_smtpmail_mod

    character(len=*), parameter :: LF = achar(10), CRLF = achar(13)//achar(10)
    character(len=*), parameter :: SEPARATOR = "--text follows this line--"
    character(len=*), parameter :: TRACE = "*trace of SMTP session to 127.0.0.1*"
    !> A response that makes the fake server close the connection and
    !> accept the next one.
    character(len=*), parameter :: RECONNECT = "<reconnect>"

    !> The fake server: its pid and the file it writes what it received to.
    type :: server_type
        integer(int32) :: pid = -1
        character(len=:), allocatable :: log
    end type server_type

contains

    !> Collect all exported unit tests.
    subroutine collect_smtpmail_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("smtpmail-send", test_send), &
                    new_unittest("smtpmail-helo", test_helo), &
                    new_unittest("smtpmail-auth-plain-trace", test_auth_plain_trace), &
                    new_unittest("smtpmail-auth-cram-md5", test_auth_cram_md5), &
                    new_unittest("smtpmail-auth-failed", test_auth_failed), &
                    new_unittest("smtpmail-recipient-refused", test_recipient_refused), &
                    new_unittest("smtpmail-greeting-refused", test_greeting_refused), &
                    new_unittest("smtpmail-transient-retry", test_transient_retry), &
                    new_unittest("smtpmail-connection-refused", test_connection_refused), &
                    new_unittest("smtpmail-no-recipients", test_no_recipients), &
                    new_unittest("smtpmail-queue", test_queue), &
                    new_unittest("smtpmail-from-field", test_from_field), &
                    new_unittest("smtpmail-envelope-from-header", test_envelope_from_header), &
                    new_unittest("smtpmail-service-name", test_service_name), &
                    new_unittest("smtpmail-service-name-unknown", test_service_name_unknown) &
                    ]
    end subroutine collect_smtpmail_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The editor with known settings: a known address and From style, the
    !> envelope address from it, the trace buffer killed.
    subroutine set_up()
        call init_bindings()
        call init_fileio()
        user_mail_address = "king@grassland.com"
        mail_from_style = MAIL_FROM_STYLE_SIMPLE
        mail_specify_envelope_from = .false.
        mail_envelope_from = .false.
        if (allocated(smtpmail_smtp_service%service_name)) deallocate (smtpmail_smtp_service%service_name)
        smtpmail_debug_info = .false.
        smtpmail_queue_mail = .false.
        if (allocated(smtpmail_servers_requiring_authorization)) deallocate (smtpmail_servers_requiring_authorization)
        if (allocated(smtpmail_smtp_user)) deallocate (smtpmail_smtp_user)
        smtpmail_auth_supported = [character(len=8) :: "cram-md5", "plain", "login"]
    end subroutine set_up

    !> Make the *mail* buffer current with HEADER, the separator and BODY.
    subroutine make_mail(header, body)
        character(len=*), intent(in) :: header, body
        type(handle_type) :: h
        type(status_type) :: st

        h = get_buffer("*mail*")
        if (is_buffer_live(h)) call close_buffer(h, st)
        call get_or_make_buffer("*mail*", h, st)
        call set_buffer(h, st)
        call insert(header//SEPARATOR//LF//body, st)
    end subroutine make_mail

    !> Close *mail* and the trace buffer.
    subroutine leave()
        type(handle_type) :: h
        type(status_type) :: st

        h = get_buffer("*mail*")
        if (is_buffer_live(h)) call close_buffer(h, st)
        h = get_buffer(TRACE)
        if (is_buffer_live(h)) call close_buffer(h, st)
    end subroutine leave

    !> Queue TEXT as input events (for the minibuffer).
    subroutine type_text(text)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(text)))

        do i = 1, len(text)
            evs(i) = value_type(int(iachar(text(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_text

    !> Start the fake server answering with RESPONSES (the greeting first)
    !> and point `smtpmail_smtp_server' at it.
    subroutine start_server(responses, server)
        type(string_type), intent(in) :: responses(:)
        type(server_type), intent(out) :: server
        integer(int32) :: fd, port, err
        type(status_type) :: st

        call create_temp_file("smtpmail-test", server%log, st)
        call fortmacs_listen_loopback(fd, port, err)
        if (err /= 0) return
        smtpmail_smtp_server = "127.0.0.1"
        smtpmail_smtp_service%port = port
        server%pid = fortmacs_fork()

        if (server%pid == 0) then
            call serve(fd, responses, server%log)
            call fortmacs_exit_now(0)
        end if

        call close_fd(fd)
    end subroutine start_server

    !> The child: accept a connection on FD, send the greeting, then the
    !> next response after each line received (after the data, for a
    !> response starting 354 to DATA), and write what was received to
    !> LOG once the client has closed.  Uses system calls only.
    subroutine serve(fd, responses, log)
        integer(int32), intent(in) :: fd
        type(string_type), intent(in) :: responses(:)
        character(len=*), intent(in) :: log
        character(len=:), allocatable :: received, pending, line
        character(len=1024) :: chunk
        integer(int32) :: conn, err, out
        integer(int64) :: nread, written
        integer :: next, e
        logical :: in_data
        type(status_type) :: st

        received = ""
        pending = ""
        in_data = .false.
        next = 1
        call accept_next()

        do while (conn >= 0)
            e = index(pending, CRLF)

            if (e == 0) then
                call fortmacs_read(conn, chunk, nread, st)

                if (nread > 0) then
                    pending = pending//chunk(:nread)
                    cycle
                end if

                ! The client closed: the next connection, or the end.
                if (next > size(responses)) exit
                if (responses(next)%text /= RECONNECT) exit
                next = next + 1
                call close_fd(conn)
                pending = ""
                call accept_next()
                cycle
            end if

            line = pending(:e - 1)
            pending = pending(e + 2:)
            received = received//line//CRLF

            if (in_data) then
                if (line /= ".") cycle
                in_data = .false.
            end if

            if (next > size(responses)) cycle
            if (line == "DATA" .and. index(responses(next)%text, "354") == 1) in_data = .true.
            call fortmacs_send(conn, responses(next)%text, err)
            next = next + 1
        end do

        if (conn >= 0) call close_fd(conn)
        err = fortmacs_creat(log, int(o'644', int32), out)

        if (err == 0) then
            call fortmacs_full_write(out, received, written, st)
            call close_fd(out)
        end if

    contains

        !> Accept a connection and send the next response, the greeting.
        subroutine accept_next()
            call fortmacs_accept(fd, conn, err)
            if (conn < 0) return

            if (next <= size(responses)) then
                call fortmacs_send(conn, responses(next)%text, err)
                next = next + 1
            end if
        end subroutine accept_next
    end subroutine serve

    !> Wait for the fake server to end; RECEIVED is what it got.
    subroutine finish_server(server, received)
        type(server_type), intent(inout) :: server
        character(len=:), allocatable, intent(out) :: received
        integer(int32) :: code, sig, err
        type(status_type) :: st

        received = ""
        if (server%pid <= 0) return
        err = wait_for_termination(server%pid, code, sig)
        received = file_text(server%log)
        if (file_exists(server%log)) call delete_file(server%log, st)
    end subroutine finish_server

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

    !> RESPONSES from the strings given, each with CR LF after it.
    function responses_of(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16) result(responses)
        character(len=*), intent(in) :: r1
        character(len=*), intent(in), optional :: r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16
        type(string_type), allocatable :: responses(:)

        allocate (responses(0))
        call add(r1)
        if (present(r2)) call add(r2)
        if (present(r3)) call add(r3)
        if (present(r4)) call add(r4)
        if (present(r5)) call add(r5)
        if (present(r6)) call add(r6)
        if (present(r7)) call add(r7)
        if (present(r8)) call add(r8)
        if (present(r9)) call add(r9)
        if (present(r10)) call add(r10)
        if (present(r11)) call add(r11)
        if (present(r12)) call add(r12)
        if (present(r13)) call add(r13)
        if (present(r14)) call add(r14)
        if (present(r15)) call add(r15)
        if (present(r16)) call add(r16)

    contains

        !> Add one response.
        subroutine add(text)
            character(len=*), intent(in) :: text
            type(string_type) :: one

            if (text == RECONNECT) then
                one%text = text
            else
                one%text = text//CRLF
            end if

            responses = [responses, one]
        end subroutine add
    end function responses_of

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    !> The whole dialogue: EHLO, MAIL FROM with the address of
    !> `user_mail_address', RCPT TO for each recipient (the last first),
    !> DATA with the header completed, Bcc removed and a dot doubled, QUIT;
    !> the trace buffer is gone after.
    subroutine test_send(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        call start_server(responses_of("220 fake ESMTP", "250-fake"//CRLF//"250-8BITMIME"//CRLF//"250 HELP", "250 ok", &
                                       "250 ok", "250 ok", "250 ok", "354 go ahead", "250 queued", "221 bye"), server)
        call make_mail("To: a@example.com"//LF//"Cc: Bob <b@example.com>"//LF//"Bcc: c@example.com"//LF// &
                       "Subject: hi"//LF, "hello"//LF//".dot line"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call check(error, index(received, "EHLO "//system_name()//CRLF//"MAIL FROM:<king@grassland.com>"//CRLF// &
                                    "RCPT TO:<c@example.com>"//CRLF//"RCPT TO:<b@example.com>"//CRLF// &
                                    "RCPT TO:<a@example.com>"//CRLF//"DATA"//CRLF) == 1, received)
        end if

        if (.not. allocated(error)) then
            call check(error, index(received, CRLF//"hello"//CRLF//"..dot line"//CRLF//"."//CRLF//"QUIT"//CRLF) > 0, &
                       received)
        end if

        if (.not. allocated(error)) call check(error, index(received, "Bcc") == 0, received)
        if (.not. allocated(error)) call check(error, index(received, CRLF//"From: king@grassland.com"//CRLF) > 0, received)
        if (.not. allocated(error)) call check(error, index(received, CRLF//"Message-Id: <") > 0, received)
        if (.not. allocated(error)) call check(error, index(received, "DATA"//CRLF//"Date: ") > 0, received)
        if (.not. allocated(error)) call check(error,.not. is_buffer_live(get_buffer(TRACE)), "trace buffer kept")
        call leave()
    end subroutine test_send

    !> A server that refuses EHLO gets HELO.
    subroutine test_helo(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        call start_server(responses_of("220 fake", "502 what?", "250 hello", "250 ok", "250 ok", "354 go ahead", &
                                       "250 queued", "221 bye"), server)
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call check(error, index(received, "EHLO "//system_name()//CRLF//"HELO "//system_name()//CRLF// &
                                    "MAIL FROM:<king@grassland.com>"//CRLF) == 1, received)
        end if

        call leave()
    end subroutine test_helo

    !> With `smtpmail_servers_requiring_authorization' matching, the user
    !> name and password are asked for and sent with AUTH PLAIN; the trace
    !> buffer kept by `smtpmail_debug_info' omits them and holds the data.
    subroutine test_auth_plain_trace(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received, trace_text
        type(handle_type) :: h

        call set_up()
        smtpmail_debug_info = .true.
        smtpmail_servers_requiring_authorization = "127\.0"
        call start_server(responses_of("220 fake", "250-fake"//CRLF//"250 AUTH PLAIN LOGIN", "235 welcome", "250 ok", &
                                       "250 ok", "354 go ahead", "250 queued", "221 bye"), server)
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call type_text("alice"//achar(13)//"secret"//achar(13))
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call check(error, index(received, "EHLO "//system_name()//CRLF//"AUTH PLAIN AGFsaWNlAHNlY3JldA=="//CRLF// &
                                    "MAIL FROM:") == 1, received)
        end if

        if (.not. allocated(error)) then
            h = get_buffer(TRACE)
            call check(error, is_buffer_live(h), "no trace buffer")
        end if

        if (.not. allocated(error)) then
            call set_buffer(h, st)
            trace_text = buffer_string()
            call check(error, index(trace_text, "220 fake"//CRLF//"250-fake"//CRLF//"250 AUTH PLAIN LOGIN"//CRLF// &
                                    "AUTH PLAIN <omitted>"//CRLF//"235 welcome"//CRLF) == 1, trace_text)
        end if

        if (.not. allocated(error)) call check(error, index(trace_text, CRLF//"hello"//CRLF//"."//CRLF) > 0, trace_text)

        if (.not. allocated(error)) then
            call check(error, index(trace_text, "QUIT"//CRLF//"221 bye"//CRLF//LF//"Process smtpmail deleted"//LF) > 0, &
                       trace_text)
        end if

        call leave()
    end subroutine test_auth_plain_trace

    !> CRAM-MD5 answers the challenge of RFC 2195's example.
    subroutine test_auth_cram_md5(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        smtpmail_servers_requiring_authorization = "."
        smtpmail_smtp_user = "tim"
        call start_server(responses_of("220 fake", "250-fake"//CRLF//"250 AUTH CRAM-MD5 PLAIN", &
                                       "334 PDE4OTYuNjk3MTcwOTUyQHBvc3RvZmZpY2UucmVzdG9uLm1jaS5uZXQ+", "235 welcome", &
                                       "250 ok", "250 ok", "354 go ahead", "250 queued", "221 bye"), server)
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call type_text("tanstaaftanstaaf"//achar(13))
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call check(error, index(received, "AUTH CRAM-MD5"//CRLF//"dGltIGI5MTNhNjAyYzdlZGE3YTQ5NWI0ZTZlNzMzNGQzODkw"// &
                                    CRLF//"MAIL FROM:") > 0, received)
        end if

        call leave()
    end subroutine test_auth_cram_md5

    !> When every mechanism fails, sending fails with the server's answer,
    !> the credentials removed from the message.
    subroutine test_auth_failed(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        smtpmail_servers_requiring_authorization = "."
        smtpmail_smtp_user = "alice"
        call start_server(responses_of("220 fake", "250-fake"//CRLF//"250 AUTH PLAIN", "535 bad credentials", "221 bye"), &
                          server)
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call type_text("secret"//achar(13))
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, format_error(st), "Sending failed: 535 bad credentials in response to AUTH")
        if (.not. allocated(error)) call check(error, index(received, "QUIT"//CRLF) > 0, received)
        call leave()
    end subroutine test_auth_failed

    !> A refused recipient fails with the server's answer.
    subroutine test_recipient_refused(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        call start_server(responses_of("220 fake", "250 fake", "250 ok", "550 no such user", "221 bye"), server)
        call make_mail("To: nobody@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, format_error(st), "Sending failed: 550 no such user")
        if (.not. allocated(error)) call check(error, index(received, "RCPT TO:<nobody@example.com>"//CRLF//"QUIT") > 0)
        call leave()
    end subroutine test_recipient_refused

    !> A greeting with an error code refuses the connection.
    subroutine test_greeting_refused(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        call start_server(responses_of("554 go away", "554 still no", "221 bye"), server)
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, format_error(st), "Sending failed: Connection not allowed: 554 go away"//CRLF)
        call leave()
    end subroutine test_greeting_refused

    !> A transient error (4xx) to MAIL FROM closes the connection and
    !> sends again.
    subroutine test_transient_retry(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received
        character(len=:), allocatable :: ehlo

        call set_up()
        call start_server(responses_of("220 fake", "250 fake", "451 try later", "221 bye", RECONNECT, "220 fake", &
                                       "250 fake", "250 ok", "250 ok", "354 go ahead", "250 queued", "221 bye"), server)
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))
        ehlo = "EHLO "//system_name()//CRLF

        if (.not. allocated(error)) then
            call check(error, index(received, ehlo//"MAIL FROM:<king@grassland.com>"//CRLF//"QUIT"//CRLF//ehlo// &
                                    "MAIL FROM:<king@grassland.com>"//CRLF//"RCPT TO:<a@example.com>"//CRLF) == 1, received)
        end if

        call leave()
    end subroutine test_transient_retry

    !> A port nobody listens on is a file error naming the connection.
    subroutine test_connection_refused(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int32) :: fd, port, err

        call set_up()
        call fortmacs_listen_loopback(fd, port, err)
        call close_fd(fd)
        smtpmail_smtp_server = "127.0.0.1"
        smtpmail_smtp_service%port = port
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call check(error, index(format_error(st), "make client process failed: Connection refused, :name, smtpmail, "// &
                                ":buffer, #<buffer "//TRACE//">, :host, 127.0.0.1, :service, ") == 1, format_error(st))
        call leave()
    end subroutine test_connection_refused

    !> A message without recipients is not sent.
    subroutine test_no_recipients(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        smtpmail_smtp_server = "127.0.0.1"
        call make_mail("Subject: nothing"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call check(error, format_error(st), "Sending failed; no recipients")
        call leave()
    end subroutine test_no_recipients

    !> With `smtpmail_queue_mail', the message goes to the queue directory
    !> with its recipients in a namelist file and its name in the index;
    !> `send_queued_mail_with_smtp' sends it and empties the queue.
    subroutine test_queue(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: dir, index_text, data_file, received

        call set_up()
        call create_temp_file("smtpmail-queue", dir, st, directory=.true.)
        smtpmail_queue_dir = dir//"/"
        smtpmail_queue_mail = .true.
        call make_mail("To: a@example.com"//LF, "queued"//LF)
        call send_mail_with_smtp(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) index_text = file_text(dir//"/index")
        if (.not. allocated(error)) call check(error, index(index_text, dir//"/") == 1, index_text)

        if (.not. allocated(error)) then
            data_file = index_text(:len(index_text) - 1)
            call check(error, file_text(data_file//".nml"), "&smtpmail"//LF// &
                       " smtpmail_recipient_addresses(1) = ""a@example.com"","//LF//"/"//LF)
        end if

        if (.not. allocated(error)) call check(error, index(file_text(data_file), LF//"queued"//LF) > 0)

        if (.not. allocated(error)) then
            smtpmail_queue_mail = .false.
            call start_server(responses_of("220 fake", "250 fake", "250 ok", "250 ok", "354 go ahead", "250 queued", &
                                           "221 bye"), server)
            call send_queued_mail_with_smtp(st)
            call finish_server(server, received)
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) call check(error, index(received, "RCPT TO:<a@example.com>"//CRLF) > 0, received)
        if (.not. allocated(error)) call check(error, index(received, CRLF//"queued"//CRLF//"."//CRLF) > 0, received)
        if (.not. allocated(error)) call check(error,.not. file_exists(data_file), "queued file kept")
        if (.not. allocated(error)) call check(error,.not. file_exists(data_file//".nml"), "variables file kept")
        if (.not. allocated(error)) call check(error, file_text(dir//"/index"), "")
        call delete_file(dir//"/index", st)
        call leave()
    end subroutine test_queue

    !> The envelope address is the address of the From field, as
    !> mail-extract-address-components finds it.
    subroutine test_from_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        call start_server(responses_of("220 fake", "250 fake", "250 ok", "250 ok", "354 go ahead", "250 queued", &
                                       "221 bye"), server)
        call make_mail("From: ""Ann Lee"" <ann@example.org>"//LF//"To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(received, "MAIL FROM:<ann@example.org>"//CRLF) > 0, received)
        call leave()
    end subroutine test_from_field

    !> With `mail_specify_envelope_from' and MAIL_ENVELOPE_FROM_HEADER, the
    !> envelope address comes from the From field through
    !> `get_mail_envelope_from'.
    subroutine test_envelope_from_header(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received

        call set_up()
        mail_specify_envelope_from = .true.
        mail_envelope_from = MAIL_ENVELOPE_FROM_HEADER
        call start_server(responses_of("220 fake", "250 fake", "250 ok", "250 ok", "354 go ahead", "250 queued", &
                                       "221 bye"), server)
        call make_mail("From: bob@example.net (Bob)"//LF//"To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(received, "MAIL FROM:<bob@example.net>"//CRLF) > 0, received)
        call leave()
        call set_up()
    end subroutine test_envelope_from_header

    !> A service name (here the port's digits, which getaddrinfo takes as
    !> a name too) is used before the port number.
    subroutine test_service_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(server_type) :: server
        type(status_type) :: st
        character(len=:), allocatable :: received
        character(len=12) :: number

        call set_up()
        call start_server(responses_of("220 fake", "250 fake", "250 ok", "250 ok", "354 go ahead", "250 queued", &
                                       "221 bye"), server)
        write (number, "(i0)") smtpmail_smtp_service%port
        smtpmail_smtp_service%service_name = trim(number)
        smtpmail_smtp_service%port = 1
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call finish_server(server, received)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, index(received, "RCPT TO:<a@example.com>"//CRLF) > 0, received)
        call leave()
        call set_up()
    end subroutine test_service_name

    !> A service name getaddrinfo does not know is an error naming it.
    subroutine test_service_name_unknown(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_up()
        smtpmail_smtp_server = "127.0.0.1"
        smtpmail_smtp_service%service_name = "no-such-smtp-service"
        call make_mail("To: a@example.com"//LF, "hello"//LF)
        call send_mail_with_smtp(st)
        call check(error, index(format_error(st), "127.0.0.1/no-such-smtp-service ") == 1, format_error(st))
        call leave()
        call set_up()
    end subroutine test_service_name_unknown
end module test_smtpmail_mod
