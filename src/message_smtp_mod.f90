! message_smtp_mod.f90 -- sending a message by the method of its
! X-Message-SMTP-Method header, a submodule of message_mod.
! Ported from GNU Emacs 31.1 lisp/gnus/message.el (in part:
! message-multi-smtp-send-mail).
!
! Copyright (C) 1996-2026 Free Software Foundation, Inc.
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

!> `send_message_mail_by_method': a submodule because the "smtp" method
!> sends with smtpmail_mod's `send_mail_with_smtp', and smtpmail_mod uses
!> message_mod for the Message-ID.
submodule(message_mod) message_smtp_mod
    use smtpmail_mod, only: send_mail_with_smtp, smtpmail_smtp_server, smtpmail_smtp_service, smtpmail_smtp_user, &
                            smtpmail_store_queue_variables
    implicit none(type, external)

contains

    module procedure send_message_mail_by_method
    character(len=:), allocatable :: method, saved_server, saved_user, saved_service_name, stream_type
    type(string_type), allocatable :: words(:)
    type(restriction_type) :: restriction
    logical :: had_server, had_user, had_service_name, saved_store
    integer :: saved_port, port, ios

    call get_message_field_value("X-Message-SMTP-Method", method, status)
    if (.not. status%ok()) return

    if (.not. allocated(method)) then
        call message_send_mail_function(status)
        return
    end if

    call save_restriction_begin(restriction)
    call narrow_to_message_headers_or_head(status)
    if (status%ok()) call remove_message_header("X-Message-SMTP-Method", status)
    call save_restriction_end(restriction)
    if (.not. status%ok()) return
    call split_words(method, words)
    if (size(words) == 0) call append_string(words, "")

    select case (words(1)%text)
    case ("smtp")
        port = 0
        if (size(words) >= 3) read (words(3)%text, *, iostat=ios) port
        if (size(words) < 3 .or. ios /= 0) port = 0

        if (size(words) >= 5) then
            stream_type = words(5)%text

            select case (stream_type)
            case ("nil", "plain")
                continue
            case ("starttls", "tls")
                ! TLS is not available (smtpmail_mod: no GnuTLS).
                call set_error(status, GENERIC_ERROR_CODE, "Unable to negotiate a TLS connection with "// &
                               word(words, 2)//"/"//word(words, 3))
                return
            case default
                call set_error(status, USER_ERROR_CODE, "Invalid stream type: "//stream_type)
                return
            end select
        else if (port == 465) then
            ! If the user didn't say anything and we're talking to the TLS
            ! SMTP port, then force a TLS connection.
            call set_error(status, GENERIC_ERROR_CODE, "Unable to negotiate a TLS connection with "// &
                           word(words, 2)//"/"//word(words, 3))
            return
        end if

        ! Bind the server, the service and the user for this message.
        saved_store = smtpmail_store_queue_variables
        had_server = allocated(smtpmail_smtp_server)
        if (had_server) saved_server = smtpmail_smtp_server
        had_user = allocated(smtpmail_smtp_user)
        if (had_user) saved_user = smtpmail_smtp_user
        saved_port = smtpmail_smtp_service%port
        had_service_name = allocated(smtpmail_smtp_service%service_name)
        if (had_service_name) saved_service_name = smtpmail_smtp_service%service_name
        smtpmail_store_queue_variables = .true.
        smtpmail_smtp_server = word(words, 2)

        if (port > 0) then
            smtpmail_smtp_service%port = port
            if (allocated(smtpmail_smtp_service%service_name)) deallocate (smtpmail_smtp_service%service_name)
        else
            smtpmail_smtp_service%port = -1
            smtpmail_smtp_service%service_name = word(words, 3)
        end if

        if (size(words) >= 4) smtpmail_smtp_user = words(4)%text
        ! message-smtpmail-send-it
        call message_send_mail_hook%execute(status)
        if (status%ok()) call send_mail_with_smtp(status)
        smtpmail_store_queue_variables = saved_store

        if (had_server) then
            smtpmail_smtp_server = saved_server
        else
            deallocate (smtpmail_smtp_server)
        end if

        if (had_user) then
            smtpmail_smtp_user = saved_user
        else if (allocated(smtpmail_smtp_user)) then
            deallocate (smtpmail_smtp_user)
        end if

        smtpmail_smtp_service%port = saved_port

        if (had_service_name) then
            smtpmail_smtp_service%service_name = saved_service_name
        else if (allocated(smtpmail_smtp_service%service_name)) then
            deallocate (smtpmail_smtp_service%service_name)
        end if
    case ("sendmail")
        call send_message_mail_with_sendmail(status)
    case ("qmail", "mh", "mailclient")
        ! message-send-mail-with-qmail, -mh and -mailclient are not ported.
        call set_error(status, VOID_FUNCTION_CODE, "send_message_mail_with_"//words(1)%text)
    case default
        call set_error(status, GENERIC_ERROR_CODE, "Unknown mail method "//method_list(words))
    end select

contains

    ! The words of TEXT, separated by whitespace (split-string).  Not a port.
    subroutine split_words(text, words)
        character(len=*), intent(in) :: text
        type(string_type), allocatable, intent(out) :: words(:)
        character(len=*), parameter :: WHITE = " "//achar(9)//achar(10)//achar(12)//achar(13)//achar(11)
        integer :: i, j

        allocate (words(0))
        i = 1

        do while (i <= len(text))
            if (index(WHITE, text(i:i)) > 0) then
                i = i + 1
                cycle
            end if

            j = scan(text(i:), WHITE)

            if (j == 0) then
                call append_string(words, text(i:))
                exit
            end if

            call append_string(words, text(i:i + j - 2))
            i = i + j
        end do
    end subroutine split_words

    ! The Kth of WORDS, "" when there are fewer (nth).  Not a port.
    function word(words, k) result(text)
        type(string_type), intent(in) :: words(:)
        integer, intent(in) :: k
        character(len=:), allocatable :: text

        text = ""
        if (k <= size(words)) text = words(k)%text
    end function word

    ! WORDS as the error shows the method list: ("smtp" "host").  Not a port.
    function method_list(words) result(text)
        type(string_type), intent(in) :: words(:)
        character(len=:), allocatable :: text
        integer :: k

        text = "("

        do k = 1, size(words)
            if (k > 1) text = text//" "
            text = text//words(k)%text
        end do

        text = text//")"
    end function method_list
    end procedure send_message_mail_by_method
end submodule message_smtp_mod
