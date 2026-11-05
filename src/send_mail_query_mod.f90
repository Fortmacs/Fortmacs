! send_mail_query_mod.f90 -- asking how to send mail, a submodule of
! send_mail_mod.
! Ported from GNU Emacs 31.1 lisp/mail/sendmail.el (in part:
! sendmail-query-user-about-smtp).
!
! Copyright (C) 1985-1986, 1992-1996, 1998, 2000-2026 Free Software
! Foundation, Inc.
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

!> `query_sendmail_user_about_smtp': a submodule because its "smtp"
!> choice is smtpmail_mod's `send_mail_with_smtp', and smtpmail_mod uses
!> send_mail_mod.
submodule(send_mail_mod) send_mail_query_mod
    use smtpmail_mod, only: send_mail_with_smtp
    implicit none(type, external)

contains

    module procedure query_sendmail_user_about_smtp
    type(value_type), allocatable :: options(:)
    type(value_type) :: item
    type(completion_table_type) :: table
    type(handle_type) :: old, help, window
    character(len=:), allocatable :: choice, path, first
    logical :: transport, saved_ignore
    type(status_type) :: st

    chosen => null()
    transport = .false.

    if (allocated(sendmail_program)) then
        call executable_find(sendmail_program, path)
        transport = len(path) > 0
    end if

    allocate (options(0))
    item = value_type(text="mail client")
    options = [options, item]

    if (transport) then
        item = value_type(text="transport")
        options = [options, item]
    end if

    item = value_type(text="smtp")
    options = [options, item]
    first = "mail client"
    ! Query the user.
    old = current_buffer()
    call get_or_make_buffer(generate_new_buffer_name("*Fortmacs Mail Setup Help*"), help, status)
    if (.not. status%ok()) return
    call set_buffer(help, status)
    call insert(substitute_command_keys(" Fortmacs is about to send an email message, but it has not been"//LF// &
                                        " configured for sending email.  To tell Fortmacs how to send email:"//LF// &
                                        LF//" - Type `")//"mail client"// &
                substitute_command_keys("' to start your default email client and"//LF// &
                                        "   pass it the message text."//LF//LF), status)

    if (transport) then
        call insert(substitute_command_keys(" - Type `")//"transport"// &
                    substitute_command_keys("' to invoke the system's mail transport agent"//LF//"   (the `")// &
                    sendmail_program//substitute_command_keys("' program)."//LF//LF), status)
    end if

    call insert(substitute_command_keys(" - Type `")//"smtp"// &
                substitute_command_keys("' to send mail directly to an ""outgoing mail"" server."//LF// &
                                        "   (Fortmacs may prompt you for SMTP settings)."//LF//LF// &
                                        " Fortmacs will record your selection and will use it thereafter."//LF// &
                                        " To change it later, set `send_mail_function'."//LF), status)
    call goto_char(point_min())
    call display_buffer(help, window, status)

    if (status%ok()) then
        table%kind = TABLE_LIST
        table%list = make_list(options)
        saved_ignore = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read(format_prompt("Send mail via", first), table, choice, status, &
                             require_match=REQUIRE_MATCH_YES, default=first)
        completion_ignore_case%value = saved_ignore
    end if

    ! The buffer of with-temp-buffer goes away.
    call close_buffer(help, st)
    if (is_buffer_live(old)) call set_buffer(old, st)
    if (.not. status%ok()) return

    select case (downcase_string(choice))
    case ("transport")
        chosen => send_mail_with_sendmail
    case ("smtp")
        chosen => send_mail_with_smtp
    case default
        call set_error(status, FM_VOID_FUNCTION, "mailclient_send_it")
    end select
    end procedure query_sendmail_user_about_smtp
end submodule send_mail_query_mod
