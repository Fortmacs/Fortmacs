! smtpmail_mod.f90 -- simple SMTP protocol (RFC 821) for sending mail.
! Ported from GNU Emacs 31.1 lisp/mail/smtpmail.el, with the parts of
! lisp/net/network-stream.el it uses (in part: the plain connection),
! lisp/net/rfc2104.el (rfc2104-hash) and lisp/gnus/message.el (in part:
! message-make-message-id).
!
! Copyright (C) 1995-1996, 2001-2026 Free Software Foundation, Inc.
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

!> Sending mail over SMTP.  To send mail from Mail mode through an SMTP
!> server, the configuration writes
!>
!>     send_mail_function => send_mail_with_smtp
!>     smtpmail_smtp_server = "YOUR SMTP HOST"
!>     smtpmail_smtp_service%port = 25
!>
!> `send_mail_with_smtp' talks to the server over a TCP connection
!> (src/sys/system_socket_mod): the greeting, EHLO (or HELO), AUTH with the
!> mechanisms of `smtpmail_authentication_supported', MAIL FROM, RCPT TO, DATA and
!> QUIT, keeping the dialogue in the buffer "*trace of SMTP session to
!> HOST*".  With `smtpmail_queue_mail' true, mail is stored in
!> `smtpmail_queue_directory' instead and sent later with
!> \[send_queued_mail_with_smtp].  The variables of a queued message are
!> kept beside it in Fortran namelist syntax.
!>
!> See docs/status/smtpmail_mod.md for what is deferred.
module smtpmail_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, FILE_ERROR_CODE, ARGUMENTS_OUT_OF_RANGE_CODE
    use handle_mod, only: handle_type
    use data_mod, only: choice_setting_type
    use string_type_mod, only: string_type, append_string, prepend_string
    use character_mod, only: downcase_ascii
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, erase_buffer, &
                          close_buffer, generate_new_buffer_name
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_end_of_buffer, &
                                     buffer_substring_no_properties, buffer_string, delete_region, narrow_to_region, widen, &
                                     insert_buffer_substring, line_beginning_position, line_end_position, preceding_character, &
                                     system_name, user_full_name, user_login_name
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, end_of_line
    use syntax_mod, only: skip_characters_forward
    use search_mod, only: regular_expression_search_forward, looking_at, match_beginning, match_end, replace_match, string_match, &
                          case_fold_search, string_index_to_offset, regular_expression_quote, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string, read_number, progress_reporter_type, make_progress_reporter, &
                                     progress_reporter_update, progress_reporter_done
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker, set_marker
    use file_input_output_mod, only: expand_file_name, file_exists, file_name_nondirectory, insert_file_contents, write_region, &
                                     user_uid
    use files_mod, only: create_directory, delete_file, write_file
    use environment_mod, only: getenv
    use display_engine_mod, only: message
    use minibuffer_reading_mod, only: read_string
    use replace_mod, only: format_prompt
    use authentication_source_mod, only: read_password
    use coding_mod, only: buffer_file_coding_system, coding_system_mime_character_set, coding_system_for_write, &
                          coding_system_for_read, coding_system_change_end_of_line_conversion, encode_coding_string, &
                          decode_coding_string, END_OF_LINE_UNIX
    use time_functions_mod, only: format_time_string
    use utility_functions_mod, only: base64_encode_string, base64_decode_string, md5, secure_hash, random, SECURE_HASH_MD5
    use simple_mod, only: count_lines
    use startup_mod, only: mail_host_address, user_mail_address_value
    use mail_utilities_mod, only: get_mail_field, strip_mail_quoted_names
    use mail_extract_mod, only: extract_mail_address_components, mail_address_components_type
    use namelist_mod, only: namelist_group_type, namelist_item_type, namelist_text, read_namelist, NAMELIST_STRING, &
                            NAMELIST_INTEGER, NAMELIST_LOGICAL
    use send_mail_mod, only: mail_interactive, mail_specify_envelope_from, get_mail_envelope_from, mail_from_style, &
                             mail_send_nonascii, sendmail_coding_system, mail_header_separator, &
                             undelimit_mail_sendmail_header, do_mail_fcc, select_message_coding_system, &
                             MAIL_FROM_STYLE_SIMPLE, MAIL_FROM_STYLE_PARENTHESES, MAIL_FROM_STYLE_ANGLES, MAIL_SEND_NONASCII_MIME
    use system_dependent_mod, only: close_fd, input_available, inputs_available, fortmacs_read, fortmacs_strerror
    use system_socket_mod, only: fortmacs_connect, fortmacs_send
    implicit none(type, external)
    private

    public :: send_mail_with_smtp, send_queued_mail_with_smtp, initialize_smtpmail

    !> The setting `smtpmail_smtp_service': a port number (R6-39:
    !> `smtpmail_smtp_service%port = 587'; negative when none is set) or a
    !> service name (`smtpmail_smtp_service%service_name = "submission"';
    !> unallocated when none is set), the name first when both are set.
    !> Not a port.
    type, extends(choice_setting_type), public :: smtpmail_smtp_service_choice_setting_type
        integer :: port = -1
        character(len=:), allocatable :: service_name
    contains
        procedure :: clear_kinds => smtpmail_smtp_service_clear_kinds
        procedure :: other_value => smtpmail_smtp_service_other_value
        procedure :: kind_hint => smtpmail_smtp_service_kind_hint
    end type smtpmail_smtp_service_choice_setting_type

    !> Specify default SMTP server.
    !> This only has effect if you specify it before `initialize_smtpmail' runs.
    !> Port of smtpmail.el smtpmail-default-smtp-server (unallocated for
    !> none).
    character(len=:), allocatable, public :: smtpmail_default_smtp_server

    !> The name of the host running SMTP server.  Port of smtpmail.el
    !> smtpmail-smtp-server (unallocated for none; `initialize_smtpmail' sets
    !> it from the environment variable SMTPSERVER, else
    !> `smtpmail_default_smtp_server').
    character(len=:), allocatable, public :: smtpmail_smtp_server

    !> SMTP service port number.
    !> The default value would be "smtp" or 25.  A port number is
    !> `smtpmail_smtp_service%port = 25', a service name
    !> `smtpmail_smtp_service%service_name = "smtp"' (a service name that is
    !> set is used).  Port of smtpmail.el smtpmail-smtp-service.
    type(smtpmail_smtp_service_choice_setting_type), public :: smtpmail_smtp_service = &
                                       smtpmail_smtp_service_choice_setting_type(name="smtpmail_smtp_service", true_valid=.false., &
                                                                                                       false_valid=.false., port=25)

    !> User name to use when looking up credentials in the authinfo file.
    !> If set, only consider credentials for the specified user.  Port of
    !> smtpmail.el smtpmail-smtp-user (unallocated for none).
    character(len=:), allocatable, public :: smtpmail_smtp_user

    !> Local domain name without a host name.
    !> If the function `system_name' returns the full internet address,
    !> don't define this value.  Port of smtpmail.el smtpmail-local-domain
    !> (unallocated for none).
    character(len=:), allocatable, public :: smtpmail_local_domain

    !> Local domain name without a host name.
    !> This is appended (with an @-sign) to any specified recipients which do
    !> not include an @-sign, so that each RCPT TO address is fully qualified.
    !> (Some configurations of sendmail require this.)
    !>
    !> Don't bother to set this unless you have get an error like:
    !>     Sending failed; 501 <someone>: recipient address must contain a domain.
    !> Port of smtpmail.el smtpmail-sendto-domain (unallocated for none).
    character(len=:), allocatable, public :: smtpmail_sendto_domain

    !> Whether to print info in buffer *trace of SMTP session to <somewhere>*.
    !> See also `smtpmail_debug_verb' which determines if the SMTP protocol should
    !> be verbose as well.  Port of smtpmail.el smtpmail-debug-info.
    logical, public :: smtpmail_debug_info = .false.

    !> Whether this library sends the SMTP VERB command or not.
    !> The commands enables verbose information from the SMTP server.
    !> Port of smtpmail.el smtpmail-debug-verb.
    logical, public :: smtpmail_debug_verb = .false.

    !> Coding system for encoding outgoing mail.
    !> Used for the value of `sendmail_coding_system' when
    !> `select_message_coding_system' is called.  Port of smtpmail.el
    !> smtpmail-code-conv-from (unallocated for none).
    character(len=:), allocatable, public :: smtpmail_code_conv_from

    !> True means mail is queued; otherwise it is sent immediately.
    !> If queued, it is stored in the directory `smtpmail_queue_directory' and
    !> sent with `send_queued_mail_with_smtp'.  Also see
    !> `smtpmail_store_queue_variables'.  Port of smtpmail.el
    !> smtpmail-queue-mail.
    logical, public :: smtpmail_queue_mail = .false.

    !> Directory where smtpmail_mod.f90 stores queued mail.
    !> This directory should not be writable by other users.  Port of
    !> smtpmail.el smtpmail-queue-dir (set by `initialize_smtpmail' unless
    !> already set).
    character(len=:), allocatable, public :: smtpmail_queue_directory

    !> If set, print warnings about unknown SMTP extensions.
    !> This is mainly useful for development purposes, to learn about
    !> new SMTP extensions that might be useful to support.  Port of
    !> smtpmail.el smtpmail-warn-about-unknown-extensions.
    logical, public :: smtpmail_warn_about_unknown_extensions = .false.

    !> File name of queued mail index.
    !> This is relative to `smtpmail_queue_directory'.  Port of smtpmail.el
    !> smtpmail-queue-index-file (set by `initialize_smtpmail' unless already
    !> set).
    character(len=:), allocatable, public :: smtpmail_queue_index_file

    !> Regexp matching servers that require authorization.
    !> Normally smtpmail will try first to send emails via SMTP without
    !> user/password credentials, and then retry using credentials if
    !> the server says that it requires it.  If the server name matches
    !> this regexp, smtpmail will send over the credentials on the first
    !> attempt.  Port of smtpmail.el smtpmail-servers-requiring-authorization
    !> (unallocated for none).
    character(len=:), allocatable, public :: smtpmail_servers_requiring_authorization

    !> The number of times smtpmail will retry sending when getting transient errors.
    !> These are errors with a code of 4xx from the SMTP server, which
    !> mean "try again".  Port of smtpmail.el smtpmail-retries.
    integer, public :: smtpmail_retries = 10

    !> If true, store SMTP variables when queueing mail.
    !> These will then be used when sending the queue.  Port of
    !> smtpmail.el smtpmail-store-queue-variables.
    logical, public :: smtpmail_store_queue_variables = .false.

    !> List of supported SMTP AUTH mechanisms.
    !> The list is in preference order: "cram-md5", "plain", "login" and
    !> "xoauth2" are known.  Port of smtpmail.el smtpmail-auth-supported
    !> (set by `initialize_smtpmail' unless already set).
    character(len=:), allocatable, public :: smtpmail_authentication_supported(:)

    !> Value to use for envelope-from address for mail from ambient buffer.
    !> Port of smtpmail.el smtpmail-mail-address (unallocated for none).
    character(len=:), allocatable, public :: smtpmail_mail_address

    !> The recipients of the message being sent (smtpmail.el
    !> smtpmail-recipient-address-list).
    type(string_type), allocatable :: smtpmail_recipient_addresses(:)

    !> The number of messages queued in this session (smtpmail.el
    !> smtpmail-queue-counter).
    integer :: smtpmail_queue_counter = 0

    !> The counter of Message-Id fields, randomly started (message.el
    !> message-unique-id-char; negative until first used).
    integer(int64) :: message_unique_id_character = -1

    !> The suffix of the file that keeps the variables of a queued
    !> message beside it (upstream's ".el": the Lisp file is a namelist
    !> here).
    character(len=*), parameter :: QUEUE_VARIABLES_SUFFIX = ".nml"

    !> The name of the connection, as the trace buffer's messages give it.
    character(len=*), parameter :: STREAM_NAME = "smtpmail"

    character(len=*), parameter :: TAB = achar(9), LF = achar(10), CR = achar(13), CRLF = CR//LF

    !> The end of a server's response (open-network-stream's
    !> :end-of-command in smtpmail-via-smtp).
    character(len=*), parameter :: END_OF_COMMAND = "^[0-9]+ .*"//CR//LF

    !> A connection to an SMTP server and the buffer holding its
    !> dialogue (upstream's network process): FD is the socket (-1 when
    !> closed), MARK the end of the output inserted (the process mark)
    !> and READ_POINT where the next response starts (smtpmail.el's
    !> buffer-local smtpmail-read-point).
    type :: smtp_stream_type
        integer(int32) :: fd = -1
        logical :: open = .false.
        logical :: exists = .false.
        type(handle_type) :: buffer
        type(handle_type) :: mark
        integer(int64) :: read_point = 1
    end type smtp_stream_type

    !> A response of the server: its CODE (0 when there is none, as for
    !> a response not starting with a number or a closed connection),
    !> its lines joined by newlines (TEXT, smtpmail-response-text) and its
    !> first line.
    type :: smtp_response_type
        integer :: code = 0
        character(len=:), allocatable :: text
        character(len=:), allocatable :: first_line
    end type smtp_response_type

contains

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    ! Search forward for REGULAR_EXPRESSION up to BOUND; FOUND tells whether it
    ! matched (with MOVE, a failed search moves to BOUND).  Not a port.
    recursive subroutine search_regular_expression(regular_expression, found, status, bound, move)
        character(len=*), intent(in) :: regular_expression
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: move
        integer(int64) :: position
        integer :: noerror

        noerror = SEARCH_NOERROR

        if (present(move)) then
            if (move) noerror = SEARCH_MOVE
        end if

        call regular_expression_search_forward(regular_expression, position, status, bound, noerror)
        found = position /= 0
    end subroutine search_regular_expression

    ! The position of marker M.  Not a port.
    integer(int64) function marked_position(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, position, st)
    end function marked_position

    ! A new buffer named after NAME, made current's neighbor (as
    ! generate-new-buffer).  Not a port.
    subroutine generate_new_buffer(name, buffer, status)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call get_or_make_buffer(generate_new_buffer_name(name), buffer, status)
    end subroutine generate_new_buffer

    ! The text of STRING between the character indices FIRST and LAST
    ! (exclusive), as a string's match data gives them.  Not a port.
    function string_part(string, first, last) result(part)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: first, last
        character(len=:), allocatable :: part

        part = string(string_index_to_offset(string, first) + 1:string_index_to_offset(string, last))
    end function string_part

    ! Narrow the current buffer to its header (message-narrow-to-headers),
    ! remembering the restriction in BEGINNING_OF_ACCESSIBLE_PORTION and END_OF_ACCESSIBLE_PORTION for `restore_restriction'.
    ! Port of message.el:message-narrow-to-headers.
    recursive subroutine narrow_to_headers(beginning_of_accessible_portion, end_of_accessible_portion, status)
        integer(int64), intent(out) :: beginning_of_accessible_portion, end_of_accessible_portion
        type(status_type), intent(out) :: status
        integer(int64) :: e
        logical :: found

        beginning_of_accessible_portion = point_minimum()
        end_of_accessible_portion = point_maximum()
        call widen()
        call goto_character(point_minimum())
        call search_regular_expression("^"//regular_expression_quote(mail_header_separator)//LF, found, status)
        if (.not. status%ok()) return

        if (found) then
            e = match_beginning(0, status)
        else
            e = point_maximum()
        end if

        call narrow_to_region(point_minimum(), e, status)
        call goto_character(point_minimum())
    end subroutine narrow_to_headers

    ! Put back the restriction BEGINNING_OF_ACCESSIBLE_PORTION to END_OF_ACCESSIBLE_PORTION (the end of save-restriction).
    ! Not a port.
    recursive subroutine restore_restriction(beginning_of_accessible_portion, end_of_accessible_portion)
        integer(int64), intent(in) :: beginning_of_accessible_portion, end_of_accessible_portion
        type(status_type) :: st

        call widen()
        call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, st)
    end subroutine restore_restriction

    ! ------------------------------------------------------------------
    ! Sending
    ! ------------------------------------------------------------------

    !> Send the current mail buffer through the SMTP server
    !> `smtpmail_smtp_server', or queue it in `smtpmail_queue_directory' when
    !> `smtpmail_queue_mail' is true.  This is a suitable value for
    !> `send_mail_function'.  Port of smtpmail.el:smtpmail-send-it
    !> (expanding mail aliases, RFC 2047 encoding of the header and
    !> taking the envelope address from the From field, which needs
    !> mail-extr.el, are deferred).
    recursive subroutine send_mail_with_smtp(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: errbuf, tembuf, mailbuf, delimline
        character(len=:), allocatable :: saved_address, saved_conv, saved_sendmail_coding, conv, failure, character_set, &
                                         message_id, &
                                         sanitized
        logical :: had_address, had_conv, had_sendmail_coding, saved_fold, found, failed
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion, moved
        type(status_type) :: st

        status = status_type()
        saved_fold = case_fold_search%value
        mailbuf = current_buffer()
        ! Examine this variable now, so that
        ! local binding in the mail buffer will take effect.
        had_address = allocated(smtpmail_mail_address)
        if (had_address) saved_address = smtpmail_mail_address
        call narrow_to_headers(beginning_of_accessible_portion, end_of_accessible_portion, status)

        if (status%ok()) then
            ! Only look at the headers when fetching the
            ! envelope address.
            call get_mail_address(.false., status)
            call restore_restriction(beginning_of_accessible_portion, end_of_accessible_portion)
        end if

        if (.not. status%ok()) then
            call put_back_mail_address()
            return
        end if

        had_conv = allocated(smtpmail_code_conv_from)
        if (had_conv) saved_conv = smtpmail_code_conv_from
        conv = ""

        if (buffers(current)%multibyte) then
            had_sendmail_coding = allocated(sendmail_coding_system)
            if (had_sendmail_coding) saved_sendmail_coding = sendmail_coding_system

            if (had_conv) then
                sendmail_coding_system = smtpmail_code_conv_from
            else if (had_sendmail_coding) then
                deallocate (sendmail_coding_system)
            end if

            conv = select_message_coding_system()

            if (had_sendmail_coding) then
                sendmail_coding_system = saved_sendmail_coding
            else if (allocated(sendmail_coding_system)) then
                deallocate (sendmail_coding_system)
            end if
        end if

        if (len(conv) > 0) then
            smtpmail_code_conv_from = conv
        else if (allocated(smtpmail_code_conv_from)) then
            deallocate (smtpmail_code_conv_from)
        end if

        if (mail_interactive) then
            call generate_new_buffer(" smtpmail errors", errbuf, status)
            if (.not. status%ok()) call finish()
            if (.not. status%ok()) return
        end if

        call generate_new_buffer(" smtpmail temp", tembuf, status)

        if (.not. status%ok()) then
            call finish()
            return
        end if

        case_fold_search%value = .false.
        call set_buffer(tembuf, status)

        body: block
            if (.not. status%ok()) exit body
            call erase_buffer(status)
            if (.not. status%ok()) exit body
            ! Use the same `buffer_file_coding_system' as in the mail
            ! buffer, otherwise any `write_region' invocations (e.g., in
            ! do_mail_fcc below) will annoy with asking for a suitable
            ! encoding.
            if (len(conv) > 0) call buffer_file_coding_system%set_local(conv)
            call insert_buffer_substring(mailbuf, status)
            if (.not. status%ok()) exit body
            call goto_character(point_maximum())
            ! require one newline at the end.
            if (preceding_character() /= 10) call insert(LF, status)
            if (.not. status%ok()) exit body
            ! Change header-delimiter to be what sendmail expects.
            call undelimit_mail_sendmail_header(status)
            if (.not. status%ok()) exit body
            call copy_marker_at(point(), delimline, .false., status)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())

            ! ignore any blank lines in the header
            do
                call search_regular_expression(LF//LF//LF//"*", found, status, marked_position(delimline))
                if (.not. status%ok()) exit body
                if (.not. found) exit
                if (point() >= marked_position(delimline)) exit
                call replace_match(LF, status)
                if (.not. status%ok()) exit body
            end do

            case_fold_search%value = .true.
            ! Don't send out a blank subject line
            call goto_character(point_minimum())
            call search_regular_expression("^Subject:\([ "//TAB//"]*"//LF//"\)+\b", found, status, marked_position(delimline))
            if (.not. status%ok()) exit body

            if (found) then
                call replace_match("", status)
            else
                ! This one matches a Subject just before the header delimiter.
                call search_regular_expression("^Subject:\([ "//TAB//"]*"//LF//"\)+", found, status, marked_position(delimline))

                if (status%ok() .and. found) then
                    if (match_end(0, st) == marked_position(delimline)) call replace_match("", status)
                end if
            end if

            if (.not. status%ok()) exit body
            ! Put the "From:" field in unless for some odd reason
            ! they put one in themselves.
            call goto_character(point_minimum())
            call search_regular_expression("^From:", found, status, marked_position(delimline))
            if (.not. status%ok()) exit body
            if (.not. found) call insert_from_field(status)
            if (.not. status%ok()) exit body
            ! Insert a `Message-Id:' field if there isn't one yet.
            call goto_character(point_minimum())
            call search_regular_expression("^Message-Id:", found, status, marked_position(delimline))
            if (.not. status%ok()) exit body

            if (.not. found) then
                call make_message_id(message_id, status)
                if (status%ok()) call insert("Message-Id: "//message_id//LF, status)
            end if

            if (.not. status%ok()) exit body
            ! Insert a `Date:' field if there isn't one yet.
            call goto_character(point_minimum())
            call search_regular_expression("^Date:", found, status, marked_position(delimline))
            if (.not. status%ok()) exit body
            if (.not. found) call insert("Date: "//format_time_string("%a, %d %b %Y %T %z")//LF, status)
            if (.not. status%ok()) exit body
            ! Possibly add a MIME header for the current coding system
            call goto_character(point_minimum())

            if (mail_send_nonascii%choice == MAIL_SEND_NONASCII_MIME) then
                call search_regular_expression("^MIME-version:", found, status, marked_position(delimline))
                if (.not. status%ok()) exit body

                if (.not. found) then
                    moved = skip_characters_forward(achar(0)//"-"//achar(127), status)

                    if (point() /= point_maximum() .and. len(conv) > 0) then
                        character_set = coding_system_mime_character_set(conv)

                        if (len(character_set) > 0) then
                            call goto_character(marked_position(delimline))
                            call insert("MIME-version: 1.0"//LF//"Content-type: text/plain; charset="//character_set// &
                                        LF//"Content-Transfer-Encoding: 8bit"//LF, status)
                            if (.not. status%ok()) exit body
                        end if
                    end if
                end if
            end if

            ! Find and handle any Fcc fields.
            call goto_character(point_minimum())
            call search_regular_expression("^Fcc:", found, status, marked_position(delimline))
            if (.not. status%ok()) exit body

            if (found) then
                ! Force `do_mail_fcc' to use the encoding of the mail
                ! buffer to encode outgoing messages on Fcc files.
                call fcc_with_coding(delimline, conv, status)
                if (.not. status%ok()) exit body
            end if

            if (mail_interactive) then
                call set_buffer(errbuf, status)
                if (status%ok()) call erase_buffer(status)
                call set_buffer(tembuf, st)
                if (.not. status%ok()) exit body
            end if

            case_fold_search%value = .false.
            ! Get recipients' addresses
            call deduce_recipients(tembuf, point_minimum(), marked_position(delimline), smtpmail_recipient_addresses, status)
            if (.not. status%ok()) exit body
            call delete_bcc(delimline, status)
            if (.not. status%ok()) exit body

            ! Send or queue
            if (.not. smtpmail_queue_mail) then
                if (size(smtpmail_recipient_addresses) > 0) then
                    call send_via_smtp(smtpmail_recipient_addresses, tembuf, failed, failure, status)
                    if (.not. status%ok()) exit body

                    if (failed) then
                        call sanitize_error_message(failure, sanitized)
                        call set_error(status, GENERIC_ERROR_CODE, "Sending failed: "//sanitized)
                        exit body
                    end if
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Sending failed; no recipients")
                    exit body
                end if
            else
                call queue_message(tembuf, conv, status)
            end if
        end block body

        call destroy_marker(delimline, st)
        call close_buffer(tembuf, st)

        if (mail_interactive) then
            if (is_buffer_live(errbuf)) call close_buffer(errbuf, st)
        end if

        call finish()

    contains

        ! The end of the temporary values: the mail buffer current again,
        ! and the variables restored.  Not a port.
        subroutine finish()
            type(status_type) :: fst

            case_fold_search%value = saved_fold
            if (is_buffer_live(mailbuf)) call set_buffer(mailbuf, fst)
            call put_back_mail_address()

            if (had_conv) then
                smtpmail_code_conv_from = saved_conv
            else if (allocated(smtpmail_code_conv_from)) then
                deallocate (smtpmail_code_conv_from)
            end if
        end subroutine finish

        ! The temporary value of `smtpmail_mail_address' ends.  Not a port.
        subroutine put_back_mail_address()
            if (had_address) then
                smtpmail_mail_address = saved_address
            else if (allocated(smtpmail_mail_address)) then
                deallocate (smtpmail_mail_address)
            end if
        end subroutine put_back_mail_address
    end subroutine send_mail_with_smtp

    ! Set `smtpmail_mail_address' as smtpmail-send-it binds it, in the
    ! narrowed header of the current buffer: `mail_envelope_from' when
    ! `mail_specify_envelope_from' is true, else the address of the From
    ! field, else `smtpmail_user_mail_address'.  With KEEP, a value
    ! already set is kept (smtpmail-via-smtp's envelope-from).  Port of
    ! the binding in smtpmail.el:smtpmail-send-it.
    recursive subroutine get_mail_address(keep, status)
        logical, intent(in) :: keep
        type(status_type), intent(out) :: status
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: from, address

        status = status_type()
        if (keep .and. allocated(smtpmail_mail_address)) return

        if (mail_specify_envelope_from) then
            call get_mail_envelope_from(address, status)
            if (.not. status%ok()) return

            if (allocated(address)) then
                smtpmail_mail_address = address
                return
            end if
        end if

        call get_mail_field("from", from, status)
        if (.not. status%ok()) return

        if (allocated(from)) then
            call extract_mail_address_components(from, components, status)
            if (.not. status%ok()) return

            if (size(components) > 0) then
                if (allocated(components(1)%address)) then
                    smtpmail_mail_address = components(1)%address
                    return
                end if
            end if
        end if

        call get_smtpmail_user_mail_address(address)

        if (allocated(address)) then
            smtpmail_mail_address = address
        else if (allocated(smtpmail_mail_address)) then
            deallocate (smtpmail_mail_address)
        end if
    end subroutine get_mail_address

    ! Insert a From field before point as smtpmail-send-it does: the
    ! address is `smtpmail_mail_address', the full name `user_full_name',
    ! in the form `mail_from_style' asks for (none for the other styles).
    ! Port of the From part of smtpmail.el:smtpmail-send-it.
    recursive subroutine insert_from_field(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: login, fullname
        integer(int64) :: fullname_start
        type(handle_type) :: fullname_end
        logical :: found
        type(status_type) :: st

        status = status_type()
        login = "nil"
        if (allocated(smtpmail_mail_address)) login = smtpmail_mail_address
        fullname = user_full_name()

        select case (mail_from_style)
        case (MAIL_FROM_STYLE_ANGLES)
            call insert("From: "//fullname, status)
            if (.not. status%ok()) return
            fullname_start = point_minimum() + 6
            call copy_marker_at(point(), fullname_end, .false., status)
            if (.not. status%ok()) return
            call goto_character(fullname_start)
            ! Look for a character that cannot appear unquoted
            ! according to RFC 822 or its successors.
            call search_regular_expression("[^- !#-'*+/-9=?A-Z^-~]", found, status, marked_position(fullname_end), move=.true.)

            if (status%ok() .and. found) then
                ! Quote fullname, escaping specials.
                call goto_character(fullname_start)
                call insert('"', status)

                do while (status%ok())
                    call search_regular_expression("[""\]", found, status, marked_position(fullname_end), move=.true.)
                    if (.not. (status%ok() .and. found)) exit
                    call replace_match("\\\&", status, fixedcase=.true.)
                end do

                if (status%ok()) call insert('"', status)
            end if

            call destroy_marker(fullname_end, st)
            if (status%ok()) call insert(" <"//login//">"//LF, status)
        case (MAIL_FROM_STYLE_PARENTHESES)
            call insert("From: "//login//" (", status)
            if (.not. status%ok()) return
            fullname_start = point()
            call insert(fullname, status)
            if (.not. status%ok()) return
            call copy_marker_at(point(), fullname_end, .false., status)
            if (.not. status%ok()) return
            call goto_character(fullname_start)

            ! RFC 822 and its successors say \ and
            ! nonmatching parentheses must be
            ! escaped in comments.
            ! Escape every instance of ()\ ...
            do while (status%ok())
                call search_regular_expression("[()\]", found, status, marked_position(fullname_end), move=.true.)
                if (.not. (status%ok() .and. found)) exit
                call replace_match("\\\&", status, fixedcase=.true.)
            end do

            ! ... then undo escaping of matching parentheses,
            ! including matching nested parentheses.
            call goto_character(fullname_start)

            do while (status%ok())
                call search_regular_expression("\(\=\|[^\]\(\\\\\)*\)\\(\(\([^\]\|\\\\\)*\)\\)", found, status, &
                                               marked_position(fullname_end), move=.true.)
                if (.not. (status%ok() .and. found)) exit
                call replace_match("\1(\3)", status, fixedcase=.true.)
                call goto_character(fullname_start)
            end do

            call goto_character(marked_position(fullname_end))
            call destroy_marker(fullname_end, st)
            if (status%ok()) call insert(")"//LF, status)
        case (MAIL_FROM_STYLE_SIMPLE)
            call insert("From: "//login//LF, status)
        case default
            continue
        end select
    end subroutine insert_from_field

    ! do_mail_fcc with `coding_system_for_write' bound to CONV with Unix
    ! line ends (mbox files must have Unix EOLs).  Not a port.
    recursive subroutine fcc_with_coding(delimline, conv, status)
        type(handle_type), intent(in) :: delimline
        character(len=*), intent(in) :: conv
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved
        logical :: had_coding

        had_coding = allocated(coding_system_for_write%value)
        if (had_coding) saved = coding_system_for_write%value

        if (len(conv) > 0) then
            coding_system_for_write = coding_system_change_end_of_line_conversion(conv, END_OF_LINE_UNIX)
        else
            call coding_system_for_write%clear()
        end if

        call do_mail_fcc(delimline, status)

        if (had_coding) then
            coding_system_for_write = saved
        else
            call coding_system_for_write%clear()
        end if
    end subroutine fcc_with_coding

    ! Store the message of TEMBUF (the current buffer) in
    ! `smtpmail_queue_directory': a file named after the time and
    ! `smtpmail_queue_counter', written with CONV and Unix line ends, a
    ! namelist file beside it with the recipients (and the server
    ! settings with `smtpmail_store_queue_variables'), and its name added
    ! to the index `smtpmail_queue_index_file'.  Port of the queueing
    ! part of smtpmail.el:smtpmail-send-it.
    recursive subroutine queue_message(tembuf, conv, status)
        type(handle_type), intent(in) :: tembuf
        character(len=*), intent(in) :: conv
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file_data, file_variables, coding, index_file
        character(len=40) :: name
        type(handle_type) :: buffer_data, temporary
        type(namelist_group_type) :: group
        type(status_type) :: st

        smtpmail_queue_counter = smtpmail_queue_counter + 1
        write (name, "(a, '_', i0)") format_time_string("%Y-%m-%d_%H:%M:%S"), smtpmail_queue_counter
        call expand_file_name(trim(name), file_data, status, smtpmail_queue_directory)
        if (.not. status%ok()) return
        file_variables = file_data//QUEUE_VARIABLES_SUFFIX
        call generate_new_buffer(file_name_nondirectory(file_data), buffer_data, status)
        if (.not. status%ok()) return

        if (.not. file_exists(smtpmail_queue_directory)) then
            call create_directory(smtpmail_queue_directory, status, parents=.true.)

            if (.not. status%ok()) then
                call close_buffer(buffer_data, st)
                return
            end if
        end if

        call set_buffer(buffer_data, status)

        body: block
            if (.not. status%ok()) exit body
            call erase_buffer(status)
            if (.not. status%ok()) exit body
            ! We will be reading the file with no-conversion in
            ! send_queued_mail_with_smtp below, so write it out
            ! with Unix EOLs.
            coding = "undecided"
            if (len(conv) > 0) coding = conv
            coding = coding_system_change_end_of_line_conversion(coding, END_OF_LINE_UNIX)
            call buffer_file_coding_system%set_local(coding)
            call insert_buffer_substring(tembuf, status)
            if (.not. status%ok()) exit body
            call write_file(file_data, .false., status)
            if (.not. status%ok()) exit body
            call queue_variables(group)
            call generate_new_buffer(" *temp*", temporary, status)
            if (.not. status%ok()) exit body
            call set_buffer(temporary, status)

            if (status%ok()) then
                call insert(namelist_text(group), status)
                if (status%ok()) call write_with_coding(file_variables, "utf-8", .false., status)
            end if

            if (status%ok()) then
                call erase_buffer(status)
                if (status%ok()) call insert(file_data//LF, status)
                call expand_file_name(smtpmail_queue_index_file, index_file, st, smtpmail_queue_directory)
                if (status%ok()) call write_with_coding(index_file, coding, .true., status)
            end if

            call close_buffer(temporary, st)
        end block body

        call close_buffer(buffer_data, st)
        if (is_buffer_live(tembuf)) call set_buffer(tembuf, st)
    end subroutine queue_message

    ! The namelist group of the variables kept with a queued message:
    ! the recipients, and with `smtpmail_store_queue_variables' the
    ! server settings (upstream's nil as .false.).  Not a port.
    subroutine queue_variables(group)
        type(namelist_group_type), intent(out) :: group
        type(namelist_item_type) :: item
        integer :: k

        group%name = "smtpmail"
        item%name = "smtpmail_recipient_addresses"
        item%component = ""

        if (size(smtpmail_recipient_addresses) > 0) then
            item%subscripted = .true.
            item%lower = 1
            item%upper = size(smtpmail_recipient_addresses)

            do k = 1, size(smtpmail_recipient_addresses)
                call item%append_string(smtpmail_recipient_addresses(k)%text)
            end do
        else
            call item%append_logical(.false.)
        end if

        call group%append(item)
        ! Perhaps store the server etc.
        if (.not. smtpmail_store_queue_variables) return
        call string_item("smtpmail_smtp_server", smtpmail_smtp_server)

        if (allocated(smtpmail_smtp_service%service_name)) then
            item = namelist_item_type(name="smtpmail_smtp_service", component="service_name")
            call item%append_string(smtpmail_smtp_service%service_name)
        else
            item = namelist_item_type(name="smtpmail_smtp_service", component="port")
            call item%append_integer(int(smtpmail_smtp_service%port, int64))
        end if

        call group%append(item)
        call string_item("smtpmail_smtp_user", smtpmail_smtp_user)

    contains

        ! The item NAME = VALUE, .false. when VALUE is unallocated.
        ! Not a port.
        subroutine string_item(item_name, value)
            character(len=*), intent(in) :: item_name
            character(len=:), allocatable, intent(in) :: value
            type(namelist_item_type) :: one

            one%name = item_name
            one%component = ""

            if (allocated(value)) then
                call one%append_string(value)
            else
                call one%append_logical(.false.)
            end if

            call group%append(one)
        end subroutine string_item
    end subroutine queue_variables

    ! Write the current buffer to FILE with CODING (appending with
    ! APPEND), silently (write-region with VISIT `silent').  Not a port.
    recursive subroutine write_with_coding(file, coding, append, status)
        character(len=*), intent(in) :: file, coding
        logical, intent(in) :: append
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved
        logical :: had_coding

        had_coding = allocated(coding_system_for_write%value)
        if (had_coding) saved = coding_system_for_write%value
        coding_system_for_write = coding
        call write_region(file, .false., status, append=append)

        if (had_coding) then
            coding_system_for_write = saved
        else
            call coding_system_for_write%clear()
        end if
    end subroutine write_with_coding

    !> Send mail that was queued as a result of setting `smtpmail_queue_mail'.
    !> Port of smtpmail.el:smtpmail-send-queued-mail (the variables of a
    !> message are read from its namelist file).
    recursive subroutine send_queued_mail_with_smtp(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: index_buffer, data_buffer, old
        character(len=:), allocatable :: qfile, file_data, file_variables, failure, saved_server, saved_user, &
                                         saved_address, message_text, sanitized, &
                                         user_address, envelope_from
        type(string_type), allocatable :: saved_recipients(:)
        logical :: had_server, had_user, had_address, failed
        type(smtpmail_smtp_service_choice_setting_type) :: saved_service
        integer(int64) :: inserted, beginning_of_accessible_portion, end_of_accessible_portion
        type(status_type) :: st

        status = status_type()
        old = current_buffer()
        call expand_file_name(smtpmail_queue_index_file, qfile, status, smtpmail_queue_directory)
        if (.not. status%ok()) return
        ! The stored variables get temporary values.
        if (allocated(smtpmail_recipient_addresses)) call move_alloc(smtpmail_recipient_addresses, saved_recipients)
        allocate (smtpmail_recipient_addresses(0))
        had_server = allocated(smtpmail_smtp_server)
        if (had_server) saved_server = smtpmail_smtp_server
        had_user = allocated(smtpmail_smtp_user)
        if (had_user) saved_user = smtpmail_smtp_user
        saved_service = smtpmail_smtp_service
        had_address = allocated(smtpmail_mail_address)
        if (had_address) saved_address = smtpmail_mail_address
        call generate_new_buffer(" *temp*", index_buffer, status)

        if (status%ok()) then
            call set_buffer(index_buffer, status)
            ! Get index, get first mail, send it, update index, get second
            ! mail, send it, etc...
            if (status%ok()) call insert_file_contents(qfile, .false., inserted, status)
        end if

        if (status%ok()) then
            call goto_character(point_minimum())

            do while (.not. is_end_of_buffer())
                call buffer_substring_no_properties(point(), line_end_position(), file_data, status)
                if (.not. status%ok()) exit
                file_variables = file_data//QUEUE_VARIABLES_SUFFIX
                call read_queue_variables(file_variables, status)
                if (.not. status%ok()) exit
                ! Insert the message literally: it is already encoded as per
                ! the MIME headers, and code conversions might guess the
                ! encoding wrongly.
                call generate_new_buffer(" *temp*", data_buffer, status)
                if (.not. status%ok()) exit
                call set_buffer(data_buffer, status)
                if (status%ok()) call insert_no_conversion(file_data, status)
                if (allocated(envelope_from)) deallocate (envelope_from)

                if (status%ok() .and. mail_specify_envelope_from) then
                    ! Only look at the headers when fetching the
                    ! envelope address.
                    call narrow_to_headers(beginning_of_accessible_portion, end_of_accessible_portion, status)
                    if (status%ok()) call get_mail_envelope_from(envelope_from, status)
                    call restore_restriction(beginning_of_accessible_portion, end_of_accessible_portion)
                end if

                if (status%ok()) then
                    if (allocated(envelope_from)) then
                        smtpmail_mail_address = envelope_from
                    else
                        user_address = user_mail_address_value()

                        if (allocated(user_address)) then
                            smtpmail_mail_address = user_address
                        else if (allocated(smtpmail_mail_address)) then
                            deallocate (smtpmail_mail_address)
                        end if
                    end if
                end if

                if (status%ok()) then
                    if (size(smtpmail_recipient_addresses) == 0) then
                        call set_error(status, GENERIC_ERROR_CODE, "Sending failed; no recipients")
                    else
                        call send_via_smtp(smtpmail_recipient_addresses, data_buffer, failed, failure, status)

                        if (status%ok() .and. failed) then
                            call sanitize_error_message(failure, sanitized)
                            call set_error(status, GENERIC_ERROR_CODE, "Sending failed: "//sanitized)
                        end if
                    end if
                end if

                call close_buffer(data_buffer, st)
                call set_buffer(index_buffer, st)
                if (.not. status%ok()) exit
                call delete_file(file_data, status)
                if (status%ok()) call delete_file(file_variables, status)
                if (.not. status%ok()) exit
                call delete_region(line_beginning_position(), line_beginning_position(2_int64), status)
                if (.not. status%ok()) exit
            end do
        end if

        if (status%ok()) then
            call write_region(qfile, .false., status, point_minimum(), point_maximum(), message=message_text)
            if (status%ok()) call message(message_text)
        end if

        if (is_buffer_live(index_buffer)) call close_buffer(index_buffer, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
        if (allocated(smtpmail_recipient_addresses)) deallocate (smtpmail_recipient_addresses)
        if (allocated(saved_recipients)) call move_alloc(saved_recipients, smtpmail_recipient_addresses)

        if (had_server) then
            smtpmail_smtp_server = saved_server
        else if (allocated(smtpmail_smtp_server)) then
            deallocate (smtpmail_smtp_server)
        end if

        if (had_user) then
            smtpmail_smtp_user = saved_user
        else if (allocated(smtpmail_smtp_user)) then
            deallocate (smtpmail_smtp_user)
        end if

        smtpmail_smtp_service = saved_service

        if (had_address) then
            smtpmail_mail_address = saved_address
        else if (allocated(smtpmail_mail_address)) then
            deallocate (smtpmail_mail_address)
        end if
    end subroutine send_queued_mail_with_smtp

    ! Insert FILE literally (coding_system_for_read no-conversion).
    ! Not a port.
    recursive subroutine insert_no_conversion(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved
        logical :: had_coding
        integer(int64) :: inserted

        had_coding = allocated(coding_system_for_read%value)
        if (had_coding) saved = coding_system_for_read%value
        coding_system_for_read = "no-conversion"
        call insert_file_contents(file, .false., inserted, status)

        if (had_coding) then
            coding_system_for_read = saved
        else
            call coding_system_for_read%clear()
        end if
    end subroutine insert_no_conversion

    ! Set the variables a queued message keeps in FILE, the namelist
    ! group `smtpmail' (read as UTF-8): its recipients and the stored
    ! server settings; other objects are ignored.  A file with no such
    ! group is an error.  Port of the reading in
    ! smtpmail.el:smtpmail-send-queued-mail.
    recursive subroutine read_queue_variables(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(handle_type) :: temporary, old
        type(namelist_group_type), allocatable :: groups(:)
        character(len=:), allocatable :: text, saved
        logical :: had_coding
        integer(int64) :: inserted
        integer :: k, j
        type(status_type) :: st

        old = current_buffer()
        call generate_new_buffer(" *temp*", temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)

        if (status%ok()) then
            had_coding = allocated(coding_system_for_read%value)
            if (had_coding) saved = coding_system_for_read%value
            coding_system_for_read = "utf-8"
            call insert_file_contents(file, .false., inserted, status)

            if (had_coding) then
                coding_system_for_read = saved
            else
                call coding_system_for_read%clear()
            end if
        end if

        if (status%ok()) text = buffer_string()
        call close_buffer(temporary, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
        if (.not. status%ok()) return
        call read_namelist(text, groups, status)
        if (.not. status%ok()) return

        if (size(groups) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Unexpected code in """//file//"""")
            return
        end if

        if (groups(1)%name /= "smtpmail") then
            call set_error(status, GENERIC_ERROR_CODE, "Unexpected code in """//file//""": &"//groups(1)%name)
            return
        end if

        if (.not. allocated(groups(1)%items)) return

        do k = 1, size(groups(1)%items)
            associate (it => groups(1)%items(k))
                if (.not. allocated(it%values)) cycle

                if (it%name == "smtpmail_recipient_addresses") then
                    if (allocated(smtpmail_recipient_addresses)) deallocate (smtpmail_recipient_addresses)
                    allocate (smtpmail_recipient_addresses(0))

                    do j = 1, size(it%values)
                        if (it%values(j)%kind == NAMELIST_STRING) then
                            call append_string(smtpmail_recipient_addresses, it%values(j)%string)
                        end if
                    end do
                else if (it%name == "smtpmail_smtp_server") then
                    call set_stored_string(it%values(1), smtpmail_smtp_server)
                else if (it%name == "smtpmail_smtp_user") then
                    call set_stored_string(it%values(1), smtpmail_smtp_user)
                else if (it%name == "smtpmail_smtp_service") then
                    if (it%values(1)%kind == NAMELIST_INTEGER) then
                        call set_service(int(it%values(1)%integer))
                    else if (it%values(1)%kind == NAMELIST_STRING) then
                        call set_service(service_name=it%values(1)%string)
                    end if
                end if
            end associate
        end do

    contains

        ! VARIABLE set from a stored string, unset by .false.  Not a port.
        subroutine set_stored_string(value, variable)
            use namelist_mod, only: namelist_value_type
            type(namelist_value_type), intent(in) :: value
            character(len=:), allocatable, intent(inout) :: variable

            select case (value%kind)
            case (NAMELIST_STRING)
                variable = value%string
            case (NAMELIST_LOGICAL)
                if (allocated(variable)) deallocate (variable)
            case default
                continue
            end select
        end subroutine set_stored_string
    end subroutine read_queue_variables

    ! STRING with what follows the word AUTH removed, as passwords may be
    ! there.  Port of smtpmail.el:smtpmail--sanitize-error-message.
    subroutine sanitize_error_message(string, sanitized)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: sanitized
        integer(int64) :: start, m, e
        type(status_type) :: st

        sanitized = ""
        start = 1

        do
            m = string_match("\bAUTH\b.*", string, st, start)
            if (m == 0) exit
            e = match_end(0, st)
            sanitized = sanitized//string_part(string, start, m)//"AUTH"
            start = e
            ! An empty match would match again at the same place.
            if (e == m) exit
        end do

        sanitized = sanitized//string(string_index_to_offset(string, start) + 1:)
    end subroutine sanitize_error_message

    ! The fully qualified domain name of this host: `system_name', with
    ! `smtpmail_local_domain' after it when set.  Port of
    ! smtpmail.el:smtpmail-fqdn.
    function smtpmail_fqdn() result(fqdn)
        character(len=:), allocatable :: fqdn

        fqdn = system_name()
        if (allocated(smtpmail_local_domain)) fqdn = fqdn//"."//smtpmail_local_domain
    end function smtpmail_fqdn

    ! RECIPIENT with "@" and `smtpmail_sendto_domain' added when it has no
    ! "@".  Port of smtpmail.el:smtpmail-maybe-append-domain.
    function with_sendto_domain(recipient) result(address)
        character(len=*), intent(in) :: recipient
        character(len=:), allocatable :: address

        address = recipient

        if (allocated(smtpmail_sendto_domain)) then
            if (index(recipient, "@") == 0) address = recipient//"@"//smtpmail_sendto_domain
        end if
    end function with_sendto_domain

    ! `user_mail_address' if it's a valid email address (one "@" and a
    ! dot in the domain), else unallocated.  Port of
    ! smtpmail.el:smtpmail-user-mail-address.
    subroutine get_smtpmail_user_mail_address(address)
        character(len=:), allocatable, intent(out) :: address
        character(len=:), allocatable :: user_address
        integer :: at

        user_address = user_mail_address_value()
        if (.not. allocated(user_address)) return
        at = index(user_address, "@")
        if (at == 0) return
        if (index(user_address(at + 1:), "@") /= 0) return
        ! There's a dot in the domain name.
        if (index(user_address(at + 1:), ".") /= 0) address = user_address
    end subroutine get_smtpmail_user_mail_address

    ! ------------------------------------------------------------------
    ! The dialogue
    ! ------------------------------------------------------------------

    ! Send the message in TEXT_BUFFER to RECIPIENTS through the server
    ! `smtpmail_smtp_server' (asked for when unset).  FAILED is true when
    ! the server refused, FAILURE then the reason (upstream's string
    ! result); an error of another kind is in STATUS.  ASK_FOR_PASSWORD
    ! makes it authenticate from the start; SEND_ATTEMPTS counts the
    ! attempts after transient errors.  Port of smtpmail.el:smtpmail-via-smtp.
    recursive subroutine send_via_smtp(recipients, text_buffer, failed, failure, status, ask_for_password, send_attempts)
        type(string_type), intent(in) :: recipients(:)
        type(handle_type), intent(in) :: text_buffer
        logical, intent(out) :: failed
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ask_for_password
        integer, intent(in), optional :: send_attempts
        type(smtp_stream_type) :: process
        type(smtp_response_type) :: result
        type(handle_type) :: process_buffer, old
        type(string_type), allocatable :: extensions(:), authentication_mechanisms(:)
        character(len=:), allocatable :: host, envelope_from, greeting, capabilities, size_part
        character(len=24) :: number
        character(len=100) :: retry_message
        logical :: ask, has_greeting, has_capabilities, authenticated, retried
        integer :: attempts, code, n
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion, estimate
        type(status_type) :: st

        failed = .false.
        failure = ""
        retried = .false.

        if (.not. allocated(smtpmail_smtp_server)) then
            call read_smtp_server(status)
            if (.not. status%ok()) return
        end if

        attempts = 1
        if (present(send_attempts)) attempts = send_attempts
        ask = .false.
        if (present(ask_for_password)) ask = ask_for_password

        if (.not. allocated(smtpmail_smtp_server)) then
            call set_error(status, GENERIC_ERROR_CODE, "`smtpmail_smtp_server' not defined")
            return
        end if

        host = smtpmail_smtp_server
        ! `smtpmail_mail_address' should be set to the appropriate
        ! buffer-local value by the caller, but in case not:
        call narrow_to_headers(beginning_of_accessible_portion, end_of_accessible_portion, status)

        if (status%ok()) then
            ! Only look at the headers when fetching the
            ! envelope address.
            call envelope_address(envelope_from, status)
            call restore_restriction(beginning_of_accessible_portion, end_of_accessible_portion)
        end if

        if (.not. status%ok()) return

        if (allocated(smtpmail_servers_requiring_authorization)) then
            if (string_match(smtpmail_servers_requiring_authorization, smtpmail_smtp_server, st, inhibit_modify=.true.) &
                > 0) ask = .true.
        end if

        old = current_buffer()

        done: block
            ! get or create the trace buffer
            call get_or_make_buffer("*trace of SMTP session to "//host//"*", process_buffer, status)
            if (.not. status%ok()) exit done
            ! clear the trace buffer of old output
            call set_buffer(process_buffer, status)
            if (.not. status%ok()) exit done
            buffers(current)%undo_enabled = .false.
            buffers(current)%undo_count = 0
            call erase_buffer(status)
            if (.not. status%ok()) exit done
            ! Open the connection to the server.
            call open_stream(process, process_buffer, host, service_text(), greeting, has_greeting, capabilities, &
                             has_capabilities, status)
            if (.not. status%ok()) exit done
            code = 0
            if (has_greeting) code = smtp_response_code(greeting)

            if (code == 0) then
                failed = .true.
                failure = "No greeting: nil"
                if (has_greeting) failure = "No greeting: "//greeting
                exit done
            end if

            if (code >= 400) then
                failed = .true.
                failure = "Connection not allowed: "//greeting
                exit done
            end if

            call set_buffer(process_buffer, status)
            if (.not. status%ok()) exit done
            process%read_point = point_minimum()
            code = 0
            if (has_capabilities) code = smtp_response_code(capabilities)

            if (code == 0 .or. code >= 400) then
                ! The server didn't accept EHLO, so we fall back on HELO.
                call send_command_or_fail(process, "HELO "//smtpmail_fqdn(), result, failed, failure, status)
                if (failed .or. .not. status%ok()) exit done
                allocate (extensions(0))
            else
                ! EHLO was successful, so we parse the extensions.
                call parse_extensions(capabilities, extensions, status)
                if (.not. status%ok()) exit done
            end if

            call get_authentication_extension(extensions, authentication_mechanisms)
            call authenticate(process, authentication_mechanisms, host, ask, authenticated, failed, failure, status)
            if (failed .or. .not. status%ok()) exit done

            if (has_extension(extensions, "onex") .or. has_extension(extensions, "xone")) then
                call send_command_or_fail(process, "ONEX", result, failed, failure, status)
                if (failed .or. .not. status%ok()) exit done
            end if

            if (smtpmail_debug_verb) then
                if (has_extension(extensions, "verb") .or. has_extension(extensions, "xvrb")) then
                    call send_command_or_fail(process, "VERB", result, failed, failure, status)
                    if (failed .or. .not. status%ok()) exit done
                end if
            end if

            if (has_extension(extensions, "xusr")) then
                call send_command_or_fail(process, "XUSR", result, failed, failure, status)
                if (failed .or. .not. status%ok()) exit done
            end if

            ! MAIL FROM:<sender>
            size_part = ""

            if (has_extension(extensions, "size") .or. has_extension_value(extensions, "size")) then
                call set_buffer(text_buffer, status)
                if (.not. status%ok()) exit done
                ! size estimate:
                ! Add one byte for each change-of-line
                ! because of CR-LF representation:
                estimate = (point_maximum() - point_minimum()) + count_lines(point_minimum(), point_maximum())
                write (number, "(i0)") estimate
                size_part = " SIZE="//trim(number)
                call set_buffer(process_buffer, status)
                if (.not. status%ok()) exit done
            end if

            ! The 8BITMIME extension changes nothing: upstream sends no
            ! BODY=8BITMIME either.
            call send_command(process, "MAIL FROM:<"//envelope_from//">"//size_part, status)
            if (.not. status%ok()) exit done
            call receive_response(process, result, status)
            if (.not. status%ok()) exit done

            if (is_smtp_ok(result)) then
                ! Success.
                continue
            else if (result%code >= 400 .and. result%code <= 499 .and. attempts < smtpmail_retries) then
                write (retry_message, '("Got transient error code ", i0, " when sending; retrying attempt ", i0, "...")') &
                    result%code, attempts
                call message(trim(retry_message))
                ! Retry on getting a transient 4xx code; see
                ! https://tools.ietf.org/html/rfc5321#section-4.2.1
                call quit_quietly(process)
                call delete_stream(process)
                call sleep_for_seconds(1)
                call send_via_smtp(recipients, text_buffer, failed, failure, status, ask, attempts + 1)
                retried = .true.
                exit done
            else if (authenticated .and. .not. ask .and. result%code == 530) then
                ! We got a "530 auth required", so we close and try
                ! again, this time asking the user for a password.
                ! We ignore any errors here, because some MTAs just
                ! close the connection immediately after giving the
                ! error message.
                call quit_quietly(process)
                call delete_stream(process)
                call send_via_smtp(recipients, text_buffer, failed, failure, status, .true.)
                retried = .true.
                exit done
            else
                ! Return the error code.
                failed = .true.
                failure = result%text
                exit done
            end if

            ! RCPT TO:<recipient>
            do n = 1, size(recipients)
                call send_command(process, "RCPT TO:<"//with_sendto_domain(recipients(n)%text)//">", status)
                if (.not. status%ok()) exit done
                call receive_response(process, result, status)
                if (.not. status%ok()) exit done

                if (is_smtp_ok(result)) then
                    ! Success.
                    continue
                else if (authenticated .and. .not. ask .and. result%code >= 550 .and. result%code <= 554) then
                    ! We got a "550 relay not permitted" (or the like),
                    ! and the server accepts credentials, so we try
                    ! again, but ask for a password first.
                    call send_command(process, "QUIT", status)
                    if (.not. status%ok()) exit done
                    call receive_response(process, result, status)
                    if (.not. status%ok()) exit done
                    call delete_stream(process)
                    call send_via_smtp(recipients, text_buffer, failed, failure, status, .true.)
                    retried = .true.
                    exit done
                else
                    ! Return the error code.
                    failed = .true.
                    failure = result%text
                    exit done
                end if
            end do

            ! Send the contents.
            call send_command_or_fail(process, "DATA", result, failed, failure, status)
            if (failed .or. .not. status%ok()) exit done
            call send_data(process, text_buffer, failed, failure, status)
        end block done

        if (process%exists .and. .not. retried) then
            if (is_buffer_live(process_buffer)) then
                call set_buffer(process%buffer, st)
                call send_command(process, "QUIT", st)
                if (st%ok()) call receive_response(process, result, st)
                if (status%ok() .and. .not. st%ok()) status = st
                call delete_stream(process)
                if (.not. smtpmail_debug_info) call close_buffer(process_buffer, st)
            end if
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine send_via_smtp

    ! The envelope address of the message in the current buffer, its
    ! header narrowed (smtpmail-via-smtp's envelope-from): "nil" when
    ! nothing gives one, as upstream's format writes it.  Port of the
    ! binding in smtpmail.el:smtpmail-via-smtp.
    recursive subroutine envelope_address(address, status)
        character(len=:), allocatable, intent(out) :: address
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved
        logical :: had

        had = allocated(smtpmail_mail_address)
        if (had) saved = smtpmail_mail_address
        call get_mail_address(.true., status)
        address = "nil"
        if (allocated(smtpmail_mail_address)) address = smtpmail_mail_address

        if (had) then
            smtpmail_mail_address = saved
        else if (allocated(smtpmail_mail_address)) then
            deallocate (smtpmail_mail_address)
        end if
    end subroutine envelope_address

    ! Send QUIT and read the answer, ignoring errors (ignore-errors).
    ! Not a port.
    recursive subroutine quit_quietly(process)
        type(smtp_stream_type), intent(inout) :: process
        type(smtp_response_type) :: response
        type(status_type) :: st

        call send_command(process, "QUIT", st)
        if (st%ok()) call receive_response(process, response, st)
    end subroutine quit_quietly

    ! Wait SECONDS seconds (sleep-for).  Not a port.
    subroutine sleep_for_seconds(seconds)
        integer, intent(in) :: seconds
        integer(int32) :: none(0)
        logical :: ready(0)

        ! poll(2) with no descriptors only waits.
        call inputs_available(none, int(seconds*1000, int32), ready)
    end subroutine sleep_for_seconds

    ! The extensions of the EHLO response CAPABILITIES (its lines after
    ! the code, in ASCII lower case), keeping those smtpmail knows; the
    ! others are reported with `smtpmail_warn_about_unknown_extensions'.
    ! Port of the parsing in smtpmail.el:smtpmail-via-smtp.
    recursive subroutine parse_extensions(capabilities, extensions, status)
        character(len=*), intent(in) :: capabilities
        type(string_type), allocatable, intent(out) :: extensions(:)
        type(status_type), intent(out) :: status
        character(len=*), parameter :: KNOWN(15) = [character(len=19) :: &
                                                    "verb", "xvrb", "8bitmime", "onex", "xone", "expn", "size", "dsn", "etrn", &
                                                    "enhancedstatuscodes", "help", "xusr", "auth=login", "auth", "starttls"]
        character(len=:), allocatable :: line, name, first, rest
        integer :: start, e, space

        status = status_type()
        allocate (extensions(0))
        start = 1

        do while (start <= len(capabilities))
            e = index(capabilities(start:), CRLF)

            if (e == 0) then
                line = capabilities(start:)
                start = len(capabilities) + 1
            else
                line = capabilities(start:start + e - 2)
                start = start + e + 1
            end if

            if (len(line) == 0) cycle

            if (len(line) < 4) then
                call set_error(status, ARGUMENTS_OUT_OF_RANGE_CODE, """"//line//""", 4")
                return
            end if

            ! Use ASCII lower case, so that I does not become a dotless i
            ! in some language environments.
            name = downcase_ascii(line(5:))
            space = index(name, " ")
            first = name
            if (space > 0) first = name(:space - 1)

            if (any(KNOWN == first)) then
                call append_string(extensions, name)
            else if (smtpmail_warn_about_unknown_extensions) then
                if (space > 0) then
                    rest = "("//name//")"
                else
                    rest = name
                end if

                call message("Unknown extension "//rest)
            end if
        end do
    end subroutine parse_extensions

    ! True if the extension NAME, with no parameters, is among
    ! EXTENSIONS (a member of upstream's supported-extensions).  Not a port.
    logical function has_extension(extensions, name)
        type(string_type), intent(in) :: extensions(:)
        character(len=*), intent(in) :: name
        integer :: k

        has_extension = .false.

        do k = 1, size(extensions)
            if (extensions(k)%text == name) has_extension = .true.
        end do
    end function has_extension

    ! True if the extension NAME with parameters is among EXTENSIONS (an
    ! extension with a value in upstream's supported-extensions).  Not a port.
    logical function has_extension_value(extensions, name)
        type(string_type), intent(in) :: extensions(:)
        character(len=*), intent(in) :: name
        integer :: k

        has_extension_value = .false.

        do k = 1, size(extensions)
            if (index(extensions(k)%text, name//" ") == 1) has_extension_value = .true.
        end do
    end function has_extension_value

    ! The mechanisms of the last AUTH extension with parameters in
    ! EXTENSIONS (the value of upstream's auth extension).
    ! Not a port.
    subroutine get_authentication_extension(extensions, mechanisms)
        type(string_type), intent(in) :: extensions(:)
        type(string_type), allocatable, intent(out) :: mechanisms(:)
        character(len=:), allocatable :: rest
        integer :: k, space

        allocate (mechanisms(0))

        do k = size(extensions), 1, -1
            if (index(extensions(k)%text, "auth ") /= 1) cycle
            rest = extensions(k)%text(6:)

            do
                space = index(rest, " ")

                if (space == 0) then
                    call append_string(mechanisms, rest)
                    exit
                end if

                call append_string(mechanisms, rest(:space - 1))
                rest = rest(space + 1:)
            end do

            exit
        end do
    end subroutine get_authentication_extension

    ! Authenticate with the first mechanism of `smtpmail_authentication_supported'
    ! the server offers (SERVER_MECHANISMS) that works.  No source of
    ! credentials is ported (auth-source.el): with ASK, the user name
    ! (`smtpmail_smtp_user' when set) and the password are read as the
    ! netrc source's creation asks for them; without, nothing is sent.
    ! AUTHENTICATED tells whether a method returned a response (a result
    ! upstream counts as true); FAILED and FAILURE give the reason when every
    ! method failed.  Port of smtpmail.el:smtpmail-try-auth-methods.
    recursive subroutine authenticate(process, server_mechanisms, host, ask, authenticated, failed, failure, status)
        type(smtp_stream_type), intent(inout) :: process
        type(string_type), intent(in) :: server_mechanisms(:)
        character(len=*), intent(in) :: host
        logical, intent(in) :: ask
        logical, intent(out) :: authenticated, failed
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: mechanisms(:)
        character(len=:), allocatable :: user, password, login
        integer :: k, j

        authenticated = .false.
        failed = .false.
        failure = ""
        status = status_type()
        allocate (mechanisms(0))

        if (allocated(smtpmail_authentication_supported)) then
            do k = 1, size(smtpmail_authentication_supported)
                do j = 1, size(server_mechanisms)
                    if (server_mechanisms(j)%text == trim(smtpmail_authentication_supported(k))) then
                        call append_string(mechanisms, trim(smtpmail_authentication_supported(k)))
                        exit
                    end if
                end do
            end do
        end if

        if (.not. ask) return

        if (allocated(smtpmail_smtp_user)) then
            user = smtpmail_smtp_user
        else
            login = user_login_name()
            call read_string(format_prompt("SMTP user name for "//host, login), user, status, default=login)
            if (.not. status%ok()) return
        end if

        call read_password("SMTP password for "//user//"@"//host//": ", password, status)
        if (.not. status%ok()) return
        if (size(mechanisms) == 0) return
        failed = .true.

        do k = 1, size(mechanisms)
            call authenticate_with(process, mechanisms(k)%text, user, password, authenticated, failed, failure, status)
            if (.not. status%ok()) return
            if (.not. failed) exit
        end do
    end subroutine authenticate

    ! Perform authentication of type MECHANISM for USER with PASSWORD.
    ! MECHANISM should be one of the values in `smtpmail_authentication_supported'.
    ! Port of smtpmail.el:smtpmail-try-auth-method (its methods for
    ! cram-md5, login, plain and xoauth2).
    recursive subroutine authenticate_with(process, mechanism, user, password, authenticated, failed, failure, status)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: mechanism, user, password
        logical, intent(out) :: authenticated, failed
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        type(smtp_response_type) :: ret
        character(len=:), allocatable :: challenge, decoded, user_bytes, password_bytes, hash, encoded

        authenticated = .false.
        failed = .false.
        failure = ""
        status = status_type()

        if (mechanism == "cram-md5") then
            call send_command_or_fail(process, "AUTH CRAM-MD5", ret, failed, failure, status)
            if (failed .or. .not. status%ok()) return
            if (ret%code /= 334) return
            challenge = ret%first_line(5:)
            decoded = base64_decode_string(challenge, status=status)
            if (.not. status%ok()) return
            call encode_coding_string(password, "utf-8", password_bytes, status)
            if (.not. status%ok()) return
            call encode_coding_string(user, "utf-8", user_bytes, status)
            if (.not. status%ok()) return
            call hmac_md5(password_bytes, decoded, hash, status)
            if (.not. status%ok()) return
            ! Osamu Yamane <yamane@green.ocn.ne.jp>:
            ! SMTP auth fails because the SMTP server identifies
            ! only the first part of the string (delimited by
            ! new line characters) as a response from the
            ! client, and the rest as distinct commands.
            ! In my case, the response string is 80 characters
            ! long.  Without the no-line-break option for
            ! `base64_encode_string', only the first 76 characters
            ! are taken as a response to the server, and the
            ! authentication fails.
            encoded = base64_encode_string(user_bytes//" "//hash, no_line_break=.true., multibyte=.false., status=status)
            if (.not. status%ok()) return
            call send_command_or_fail(process, encoded, ret, failed, failure, status)
            authenticated = .not. failed
        else if (mechanism == "login") then
            call send_command_or_fail(process, "AUTH LOGIN", ret, failed, failure, status)
            if (failed .or. .not. status%ok()) return
            call encode_coding_string(password, "utf-8", password_bytes, status)
            if (.not. status%ok()) return
            call encode_coding_string(user, "utf-8", user_bytes, status)
            if (.not. status%ok()) return
            encoded = base64_encode_string(user_bytes, no_line_break=.true., multibyte=.false., status=status)
            if (.not. status%ok()) return
            call send_command_or_fail(process, encoded, ret, failed, failure, status)
            if (failed .or. .not. status%ok()) return
            encoded = base64_encode_string(password_bytes, no_line_break=.true., multibyte=.false., status=status)
            if (.not. status%ok()) return
            call send_command_or_fail(process, encoded, ret, failed, failure, status)
            authenticated = .not. failed
        else if (mechanism == "plain") then
            ! We used to send an empty initial request, and wait for an
            ! empty response, and then send the password, but this
            ! violate a SHOULD in RFC 2222 paragraph 5.1.  Note that this
            ! is not sent if the server did not advertise AUTH PLAIN in
            ! the EHLO response.  See RFC 2554 for more info.
            call encode_coding_string(password, "utf-8", password_bytes, status)
            if (.not. status%ok()) return
            call encode_coding_string(user, "utf-8", user_bytes, status)
            if (.not. status%ok()) return
            encoded = base64_encode_string(achar(0)//user_bytes//achar(0)//password_bytes, no_line_break=.true., &
                                           multibyte=.false., status=status)
            if (.not. status%ok()) return
            call send_command_or_fail(process, "AUTH PLAIN "//encoded, ret, failed, failure, status, 235)
            authenticated = .not. failed
        else if (mechanism == "xoauth2") then
            encoded = base64_encode_string("user="//user//achar(1)//"auth=Bearer "//password//achar(1)//achar(1), &
                                           no_line_break=.true., status=status)
            if (.not. status%ok()) return
            call send_command_or_fail(process, "AUTH XOAUTH2 "//encoded, ret, failed, failure, status)
            if (failed .or. .not. status%ok()) return

            if (ret%code == 334) then
                ! When a server returns 334 server challenge, it usually means
                ! the credentials it received were wrong (e.g. was an actual
                ! password instead of an access token).  In such a case, we
                ! should return a string with 535 to indicate a failure so that
                ! smtpmail will try other authentication mechanisms.  See also
                ! https://debbugs.gnu.org/78366.
                failed = .true.
                failure = "535 5.7.8 Authentication credentials invalid"
            else
                authenticated = .true.
            end if
        else
            call set_error(status, GENERIC_ERROR_CODE, "Mechanism "//mechanism//" not implemented")
        end if
    end subroutine authenticate_with

    ! The keyed MD5 of TEXT with KEY (HMAC, RFC 2104), as lower-case hex
    ! digits.  Port of rfc2104.el:rfc2104-hash (MD5, block length 64,
    ! hash length 16).
    recursive subroutine hmac_md5(key_in, text, hash, status)
        character(len=*), intent(in) :: key_in, text
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        integer, parameter :: BLOCK_LENGTH = 64
        integer, parameter :: IPAD = 54, OPAD = 92
        character(len=:), allocatable :: key, digest
        character(len=BLOCK_LENGTH) :: k_ipad, k_opad
        integer :: k

        ! if key is longer than B, reset it to HASH(key)
        key = key_in

        if (len(key) > BLOCK_LENGTH) then
            call secure_hash(SECURE_HASH_MD5, key_in, key, status, binary=.true., multibyte=.false.)
            if (.not. status%ok()) return
        end if

        k_ipad = repeat(achar(0), BLOCK_LENGTH)
        k_ipad(:len(key)) = key
        k_opad = k_ipad

        do k = 1, BLOCK_LENGTH
            k_ipad(k:k) = achar(ieor(iachar(k_ipad(k:k)), IPAD))
            k_opad(k:k) = achar(ieor(iachar(k_opad(k:k)), OPAD))
        end do

        ! perform inner hash
        call secure_hash(SECURE_HASH_MD5, k_ipad//text, digest, status, binary=.true., multibyte=.false.)
        if (.not. status%ok()) return
        ! perform outer hash
        call md5(k_opad//digest, hash, status, multibyte=.false.)
    end subroutine hmac_md5

    ! The code of the first line of STRING that starts with a number and
    ! a space; 0 for none.  Port of smtpmail.el:smtpmail-response-code.
    integer function smtp_response_code(string) result(code)
        character(len=*), intent(in) :: string
        integer :: start, e, digits, ios

        code = 0
        start = 1

        do while (start <= len(string))
            e = index(string(start:), LF)

            if (e == 0) then
                e = len(string)
            else
                e = start + e - 1
            end if

            digits = verify(string(start:e), "0123456789") - 1

            if (digits > 0) then
                if (string(start + digits:start + digits) == " ") then
                    read (string(start:start + digits - 1), *, iostat=ios) code
                    if (ios /= 0) code = 0
                    return
                end if
            end if

            start = e + 1
        end do
    end function smtp_response_code

    ! True if RESPONSE has a code below 400, equal to CODE when present.
    ! Port of smtpmail.el:smtpmail-ok-p.
    logical function is_smtp_ok(response, code)
        type(smtp_response_type), intent(in) :: response
        integer, intent(in), optional :: code

        is_smtp_ok = response%code > 0 .and. response%code < 400

        if (present(code)) then
            if (code /= response%code) is_smtp_ok = .false.
        end if
    end function is_smtp_ok

    ! Ask for an SMTP server and try to contact it.
    ! If the contact succeeds, sets `smtpmail_smtp_server' and
    ! `smtpmail_smtp_service'.  This tries standard SMTP ports, and if
    ! none works asks you to supply one.  If you know that you need to use
    ! a non-standard port, you can set `smtpmail_smtp_service' in advance.
    ! Returns an error if the server cannot be contacted.  Port of
    ! smtpmail.el:smtpmail-query-smtp-server (saving the settings into
    ! the init file, customize-save-variable, is deferred).
    recursive subroutine read_smtp_server(status)
        type(status_type), intent(out) :: status
        ! The ports to try, as getaddrinfo takes them, and which of them are
        ! service names (upstream's strings).
        character(len=:), allocatable :: server, lookup_error, port
        character(len=32), allocatable :: ports(:)
        logical, allocatable :: named(:)
        logical :: port_named
        integer :: port_number
        integer(int64) :: answer
        integer(int32) :: fd, err
        logical :: prompted
        character(len=32) :: number

        call read_string("Outgoing SMTP mail server: ", server, status)
        if (.not. status%ok()) return
        ports = [character(len=32) :: "25", "587"]
        named = [.false., .false.]
        prompted = .false.

        ! A service name is never a member of the numbers.
        if (allocated(smtpmail_smtp_service%service_name)) then
            ports = [character(len=32) :: smtpmail_smtp_service%service_name, ports]
            named = [.true., named]
        else if (smtpmail_smtp_service%port >= 0) then
            write (number, "(i0)") smtpmail_smtp_service%port

            if (.not. any(ports == number)) then
                ports = [number, ports]
                named = [.false., named]
            end if
        end if

        do while (.not. allocated(smtpmail_smtp_server) .and. size(ports) > 0)
            port = trim(ports(1))
            port_named = named(1)
            ports = ports(2:)
            named = named(2:)
            call fortmacs_connect(server, port, fd, err, lookup_error)

            if (fd < 0) then
                ! We've used up the list of default ports, so query the user.
                if (size(ports) == 0 .and. .not. prompted) then
                    call read_number("Port number to use when contacting "//server//"? ", answer, status)
                    if (.not. status%ok()) return
                    write (number, "(i0)") answer
                    ports = [number]
                    named = [.false.]
                    prompted = .true.
                end if
            else
                smtpmail_smtp_server = server

                if (port_named) then
                    call set_service(service_name=port)
                else
                    read (port, *) port_number
                    call set_service(port_number)
                end if

                call close_fd(fd)
            end if
        end do

        if (.not. allocated(smtpmail_smtp_server)) call set_error(status, GENERIC_ERROR_CODE, "Couldn't contact an SMTP server")
    end subroutine read_smtp_server

    ! ------------------------------------------------------------------
    ! The connection
    ! ------------------------------------------------------------------

    ! Open the connection PROCESS to SERVICE (a port number or a service
    ! name) on HOST, its output going to
    ! BUFFER, read the greeting (GREETING, when HAS_GREETING) and send
    ! EHLO, reading its response (CAPABILITIES, when HAS_CAPABILITIES).
    ! The connection stays unencrypted: TLS and STARTTLS are deferred.
    ! Port of network-stream.el:open-network-stream with
    ! network-stream-open-starttls, as smtpmail-via-smtp calls them
    ! (:return-list, :capability-command, :end-of-command), without TLS.
    recursive subroutine open_stream(process, buffer, host, service, greeting, has_greeting, capabilities, has_capabilities, &
                                     status)
        type(smtp_stream_type), intent(inout) :: process
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: host, service
        character(len=:), allocatable, intent(out) :: greeting, capabilities
        logical, intent(out) :: has_greeting, has_capabilities
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: lookup_error, shown
        integer(int32) :: fd, err
        integer(int64) :: start

        has_greeting = .false.
        has_capabilities = .false.
        status = status_type()
        call fortmacs_connect(host, service, fd, err, lookup_error)

        if (len(lookup_error) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, host//"/"//service//" "//lookup_error)
            return
        end if

        if (fd < 0) then
            ! The error data shows a service name as a string.
            shown = service
            if (verify(service, "0123456789") /= 0) shown = """"//service//""""
            call set_error(status, FILE_ERROR_CODE, "make client process failed: "//fortmacs_strerror(err)//", :name, "// &
                           STREAM_NAME//", :buffer, #<buffer "//buffers(buffer%index)%name//">, :host, "//host// &
                           ", :service, "//shown//", :coding, .false.")
            return
        end if

        process%fd = fd
        process%open = .true.
        process%exists = .true.
        process%buffer = buffer
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call copy_marker_at(point_maximum(), process%mark, .false., status)
        if (.not. status%ok()) return
        start = point()
        call get_stream_response(process, start, greeting, has_greeting, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        start = point_maximum()
        call send_to_stream(process, "EHLO "//smtpmail_fqdn()//CRLF, status)
        if (.not. status%ok()) return
        call get_stream_response(process, start, capabilities, has_capabilities, status)
    end subroutine open_stream

    ! Wait until the output of PROCESS after START ends a response
    ! (`END_OF_COMMAND'); TEXT is that output (HAS_TEXT false if the
    ! connection closed first).  Port of
    ! network-stream.el:network-stream-get-response.
    recursive subroutine get_stream_response(process, start, text, has_text, status)
        type(smtp_stream_type), intent(inout) :: process
        integer(int64), intent(in) :: start
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: has_text
        type(status_type), intent(out) :: status
        integer(int64) :: original_point
        logical :: found

        has_text = .false.
        call set_buffer(process%buffer, status)
        if (.not. status%ok()) return
        original_point = point()

        do
            call goto_character(start)
            if (.not. process%open) exit
            call search_regular_expression(END_OF_COMMAND, found, status)
            if (.not. status%ok()) return
            if (found) exit
            call accept_output(process, 50_int32, status)
            if (.not. status%ok()) return
            call set_buffer(process%buffer, status)
            if (.not. status%ok()) return
        end do

        ! Return the data we got back, or nothing if the process died.
        if (point() /= start) then
            call buffer_substring_no_properties(start, point(), text, status)
            has_text = status%ok()
        end if

        call goto_character(original_point)
    end subroutine get_stream_response

    ! Wait up to TIMEOUT_MS milliseconds (negative: until something comes)
    ! for output of PROCESS and insert it at the end of its buffer
    ! (smtpmail-process-filter).  At the end of the output the connection
    ! is closed and the message of the default sentinel inserted.  Port of
    ! smtpmail.el:smtpmail-process-filter, with the reading
    ! accept-process-output does.
    recursive subroutine accept_output(process, timeout_ms, status)
        type(smtp_stream_type), intent(inout) :: process
        integer(int32), intent(in) :: timeout_ms
        type(status_type), intent(out) :: status
        character(len=4096) :: chunk
        character(len=:), allocatable :: text
        integer(int64) :: nread
        type(status_type) :: rst

        status = status_type()
        if (.not. process%open) return
        if (.not. input_available(process%fd, timeout_ms)) return
        call fortmacs_read(process%fd, chunk, nread, rst)

        if (nread <= 0) then
            process%open = .false.
            call insert_sentinel_message(process, "connection broken by remote peer"//LF)
            return
        end if

        call decode_coding_string(chunk(:nread), "raw-text-unix", text, status)
        if (.not. status%ok()) return
        call set_buffer(process%buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_maximum())
        call insert(text, status)
        if (.not. status%ok()) return
        call set_marker(process%mark, point(), status, process%buffer)
    end subroutine accept_output

    ! Insert "Process smtpmail MESSAGE" at the process mark of PROCESS, as
    ! the default sentinel does.  Port of
    ! process.c:internal_default_process_sentinel, for this connection.
    recursive subroutine insert_sentinel_message(process, message)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: message
        type(handle_type) :: old
        integer(int64) :: original_point, before, after
        type(status_type) :: st

        if (.not. is_buffer_live(process%buffer)) return
        old = current_buffer()
        call set_buffer(process%buffer, st)
        if (.not. st%ok()) return
        original_point = point()
        call goto_character(marked_position(process%mark))
        before = point()
        call insert(LF//"Process "//STREAM_NAME//" "//message, st)
        after = point()
        call set_marker(process%mark, after, st, process%buffer)

        if (original_point >= before) then
            call goto_character(original_point + (after - before))
        else
            call goto_character(original_point)
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine insert_sentinel_message

    ! Send TEXT to PROCESS (encoded as raw text).  A closed connection is
    ! an error.  Port of process.c:send_process, for this connection.
    recursive subroutine send_to_stream(process, text, status)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bytes

        if (.not. process%open) then
            call set_error(status, GENERIC_ERROR_CODE, "Process "//STREAM_NAME//" not running")
            return
        end if

        call encode_coding_string(text, "raw-text-unix", bytes, status)
        if (.not. status%ok()) return
        call send_bytes(process, bytes, status)
    end subroutine send_to_stream

    ! Send the bytes BYTES to PROCESS.  Not a port.
    recursive subroutine send_bytes(process, bytes, status)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: bytes
        type(status_type), intent(out) :: status
        integer(int32) :: err

        status = status_type()

        if (.not. process%open) then
            call set_error(status, GENERIC_ERROR_CODE, "Process "//STREAM_NAME//" not running")
            return
        end if

        call fortmacs_send(process%fd, bytes, err)

        if (err /= 0) then
            process%open = .false.
            call insert_sentinel_message(process, "connection broken by remote peer"//LF)
            call set_error(status, GENERIC_ERROR_CODE, "process "//STREAM_NAME//" no longer connected to pipe; closed it")
        end if
    end subroutine send_bytes

    ! Close PROCESS and insert the sentinel's "deleted" message.  Port of
    ! process.c:Fdelete_process, for this connection.
    recursive subroutine delete_stream(process)
        type(smtp_stream_type), intent(inout) :: process
        type(status_type) :: st

        if (.not. process%exists) return
        if (process%fd >= 0) call close_fd(process%fd)
        process%fd = -1
        process%open = .false.
        call insert_sentinel_message(process, "deleted"//LF)
        call destroy_marker(process%mark, st)
        process%exists = .false.
    end subroutine delete_stream

    ! Read the next response of the server after the read point of
    ! PROCESS, waiting for it; a closed connection gives a response with
    ! no code and no text.  Port of smtpmail.el:smtpmail-read-response.
    recursive subroutine receive_response(process, response, status)
        type(smtp_stream_type), intent(inout) :: process
        type(smtp_response_type), intent(out) :: response
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text, line
        integer(int64) :: found, match_end_position, begin, e
        logical :: continue_response, is_first, matched
        integer :: ios

        response%text = ""
        response%first_line = ""
        text = ""
        is_first = .true.
        continue_response = .true.
        call set_buffer(process%buffer, status)
        if (.not. status%ok()) return

        do while (continue_response)
            call goto_character(process%read_point)

            do
                call search_forward_crlf(found, status)
                if (.not. status%ok()) return
                if (found /= 0) exit

                if (.not. process%open) then
                    response%text = ""
                    response%first_line = ""
                    response%code = 0
                    return
                end if

                call accept_output(process, -1_int32, status)
                if (.not. status%ok()) return
                call set_buffer(process%buffer, status)
                if (.not. status%ok()) return
                call goto_character(process%read_point)
            end do

            match_end_position = point()
            call buffer_substring_no_properties(process%read_point, match_end_position - 2, line, status)
            if (.not. status%ok()) return

            if (is_first) then
                text = line
                response%first_line = line
                is_first = .false.
            else
                text = text//LF//line
            end if

            call goto_character(process%read_point)
            matched = looking_at("[0-9]+ ", status)
            if (.not. status%ok()) return

            if (matched) then
                begin = match_beginning(0, status)
                e = match_end(0, status)
                if (smtpmail_debug_info) call message(line)
                process%read_point = match_end_position
                ! ignore lines that start with "0"
                matched = looking_at("0[0-9]+ ", status)
                if (.not. status%ok()) return

                if (.not. matched) then
                    continue_response = .false.
                    call buffer_substring_no_properties(begin, e, line, status)
                    if (.not. status%ok()) return
                    read (line, *, iostat=ios) response%code
                    if (ios /= 0) response%code = 0
                end if
            else
                matched = looking_at("[0-9]+-", status)
                if (.not. status%ok()) return

                if (matched) then
                    if (smtpmail_debug_info) call message(line)
                    process%read_point = match_end_position
                    continue_response = .true.
                else
                    process%read_point = match_end_position
                    continue_response = .false.
                    response%code = 0
                end if
            end if
        end do

        process%read_point = match_end_position
        response%text = text
    end subroutine receive_response

    ! Search forward from point for CR LF; FOUND is the end of the match
    ! (0 for none, point unmoved).  Not a port.
    recursive subroutine search_forward_crlf(found, status)
        use search_mod, only: search_forward
        integer(int64), intent(out) :: found
        type(status_type), intent(out) :: status

        call search_forward(CRLF, found, status, noerror=SEARCH_NOERROR)
    end subroutine search_forward_crlf

    ! Send COMMAND and a line end to the server, recording it at the end
    ! of the trace buffer (the arguments of an AUTH command omitted).
    ! Port of smtpmail.el:smtpmail-send-command.
    recursive subroutine send_command(process, command, status)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        integer(int64) :: m
        type(status_type) :: st

        call set_buffer(process%buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_maximum())
        m = string_match("\`AUTH [A-Z]+ ", command, st)

        if (m > 0) then
            call insert(match_string(0, command)//"<omitted>"//CRLF, status)
        else
            call insert(command//CRLF, status)
        end if

        if (.not. status%ok()) return
        process%read_point = point()
        call send_to_stream(process, command//CRLF, status)
    end subroutine send_command

    ! Send STRING and read the response (RESPONSE); FAILED is true when it
    ! is not a success (with code CODE when present), FAILURE then its
    ! text and the command.  Port of smtpmail.el:smtpmail-command-or-throw.
    recursive subroutine send_command_or_fail(process, string, response, failed, failure, status, code)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: string
        type(smtp_response_type), intent(out) :: response
        logical, intent(out) :: failed
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: code

        failed = .false.
        failure = ""
        call send_command(process, string, status)
        if (.not. status%ok()) return
        call receive_response(process, response, status)
        if (.not. status%ok()) return

        if (.not. is_smtp_ok(response, code)) then
            failed = .true.
            failure = response%text//" in response to "//string
        end if
    end subroutine send_command_or_fail

    ! Send the line DATA of the message, a "." before it when it starts
    ! with one, encoded with `smtpmail_code_conv_from' when MULTIBYTE, and
    ! record it in the trace buffer with `smtpmail_debug_info'.  Port of
    ! smtpmail.el:smtpmail-send-data-1.
    recursive subroutine send_data_line(process, data, multibyte, status)
        type(smtp_stream_type), intent(inout) :: process
        character(len=*), intent(in) :: data
        logical, intent(in) :: multibyte
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bytes, shown
        logical :: encoded

        call set_buffer(process%buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_maximum())
        encoded = .false.

        if (multibyte .and. allocated(smtpmail_code_conv_from)) then
            call encode_coding_string(data, smtpmail_code_conv_from, bytes, status)
            if (.not. status%ok()) return
            encoded = .true.
        else
            call encode_coding_string(data, "raw-text-unix", bytes, status)
            if (.not. status%ok()) return
        end if

        if (smtpmail_debug_info) then
            if (encoded) then
                call decode_coding_string(bytes, "raw-text-unix", shown, status)
                if (.not. status%ok()) return
            else
                shown = data
            end if

            call insert(shown//CRLF, status)
            if (.not. status%ok()) return
        end if

        process%read_point = point()

        ! Escape "." at start of a line
        if (len(bytes) > 0) then
            if (bytes(1:1) == ".") then
                call send_bytes(process, ".", status)
                if (.not. status%ok()) return
            end if
        end if

        call send_bytes(process, bytes, status)
        if (.not. status%ok()) return
        call send_bytes(process, CRLF, status)
    end subroutine send_data_line

    ! Send the lines of BUFFER, then the "." that ends the data.  Port of
    ! smtpmail.el:smtpmail-send-data.
    recursive subroutine send_data(process, buffer, failed, failure, status)
        type(smtp_stream_type), intent(inout) :: process
        type(handle_type), intent(in) :: buffer
        logical, intent(out) :: failed
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        type(progress_reporter_type) :: pr
        type(smtp_response_type) :: response
        character(len=:), allocatable :: sending_data
        logical :: data_continue, multibyte

        failed = .false.
        failure = ""
        data_continue = .true.
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        multibyte = buffers(current)%multibyte
        call make_progress_reporter(pr, "Sending email ", point_minimum(), point_maximum())
        call goto_character(point_minimum())

        do while (data_continue)
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call progress_reporter_update(pr, point())
            call buffer_substring_no_properties(line_beginning_position(), line_end_position(), sending_data, status)
            if (.not. status%ok()) return
            call end_of_line(2_int64)
            data_continue = .not. is_end_of_buffer()
            call send_data_line(process, sending_data, multibyte, status)
            if (.not. status%ok()) return
        end do

        ! DATA end "."
        call send_command_or_fail(process, ".", response, failed, failure, status)
        if (failed .or. .not. status%ok()) return
        call progress_reporter_done(pr)
    end subroutine send_data

    ! ------------------------------------------------------------------
    ! The header
    ! ------------------------------------------------------------------

    ! The addresses of the To, Cc and Bcc fields (or their Resent-
    ! forms, when there are any) between HEADER_START and HEADER_END of
    ! TEXT_BUFFER, suitable for RCPT TO: <address>, in RECIPIENTS (last
    ! first, as upstream's list).  Port of
    ! smtpmail.el:smtpmail-deduce-address-list.
    recursive subroutine deduce_recipients(text_buffer, header_start, header_end, recipients, status)
        type(handle_type), intent(in) :: text_buffer
        integer(int64), intent(in) :: header_start, header_end
        type(string_type), allocatable, intent(out) :: recipients(:)
        type(status_type), intent(out) :: status
        type(handle_type) :: temporary, old
        character(len=:), allocatable :: simple_address_list, addr_regular_expression, part
        integer(int64) :: this_line, moved
        logical :: found, continued, saved_fold
        type(status_type) :: st
        integer :: k

        allocate (recipients(0))
        old = current_buffer()
        saved_fold = case_fold_search%value
        call generate_new_buffer(" *temp*", temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)

        body: block
            if (.not. status%ok()) exit body
            case_fold_search%value = .true.
            simple_address_list = ""
            call insert_buffer_substring(text_buffer, status, header_start, header_end)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())
            ! RESENT-* fields should stop processing of regular fields.
            call search_regular_expression("^Resent-\(To\|Cc\|Bcc\):", found, status, min(header_end, point_maximum()))
            if (.not. status%ok()) exit body

            if (found) then
                addr_regular_expression = "^Resent-\(To\|Cc\|Bcc\):"
            else
                addr_regular_expression = "^\(To:\|Cc:\|Bcc:\)"
            end if

            call goto_character(point_minimum())

            do
                call search_regular_expression(addr_regular_expression, found, status, min(header_end, point_maximum()))
                if (.not. status%ok()) exit body
                if (.not. found) exit
                this_line = match_beginning(0, st)
                call replace_match("", status)
                if (.not. status%ok()) exit body
                moved = forward_line(1_int64)

                ! get any continuation lines
                do
                    continued = looking_at("^[ "//TAB//"]+", status)
                    if (.not. status%ok()) exit body
                    if (.not. (continued .and. point() < header_end)) exit
                    moved = forward_line(1_int64)
                end do

                call buffer_substring_no_properties(this_line, point(), part, status)
                if (.not. status%ok()) exit body
                simple_address_list = simple_address_list//" "//strip_mail_quoted_names(part)
            end do

            call erase_buffer(status)
            if (.not. status%ok()) exit body
            ! newline, comma and tab --> blank
            part = " "//simple_address_list//LF

            do k = 1, len(part)
                if (scan(part(k:k), LF//","//TAB) > 0) part(k:k) = " "
            end do

            call insert(part, status)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())

            ! tidiness in case hook is not robust when it looks at this
            do
                call search_regular_expression("[ "//TAB//"]+", found, status, min(header_end, point_maximum()))
                if (.not. status%ok()) exit body
                if (.not. found) exit
                call replace_match(" ", status)
                if (.not. status%ok()) exit body
            end do

            call goto_character(point_minimum())

            do
                call search_regular_expression(" \([^ ]+\) ", found, status, point_maximum())
                if (.not. status%ok()) exit body
                if (.not. found) exit
                call goto_character(point() - 1)
                call prepend_string(recipients, match_string(1))
            end do
        end block body

        case_fold_search%value = saved_fold
        call close_buffer(temporary, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine deduce_recipients

    ! Delete [Resent-]Bcc: and their continuation lines from the header
    ! area before HEADER_END.  There may be multiple Bcc: lines, and each
    ! may have arbitrarily many continuation lines.  Port of
    ! smtpmail.el:smtpmail-do-bcc.
    recursive subroutine delete_bcc(header_end, status)
        type(handle_type), intent(in) :: header_end
        type(status_type), intent(out) :: status
        integer(int64) :: b, moved
        logical :: found, saved_fold, continued
        type(handle_type) :: original_point
        type(status_type) :: st

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call copy_marker_at(point(), original_point, .false., status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())

        ! iterate over all Bcc: lines
        do
            call search_regular_expression("^\(RESENT-\)?Bcc:", found, status, marked_position(header_end))
            if (.not. status%ok()) exit
            if (.not. found) exit
            b = match_beginning(0, status)
            moved = forward_line(1_int64)
            call delete_region(b, point(), status)
            if (.not. status%ok()) exit

            ! get rid of any continuation lines
            do
                continued = looking_at("^[ "//TAB//"].*"//LF, status)
                if (.not. status%ok()) exit
                if (.not. (continued .and. point() < marked_position(header_end))) exit
                call replace_match("", status)
                if (.not. status%ok()) exit
            end do

            if (.not. status%ok()) exit
        end do

        case_fold_search%value = saved_fold
        call goto_character(marked_position(original_point))
        call destroy_marker(original_point, st)
    end subroutine delete_bcc

    ! A unique Message-Id field value for the message in the current
    ! buffer.  Port of message.el:message-make-message-id (the reply
    ! headers of Message mode are not ported: only Supersedes adds "_-_").
    subroutine make_message_id(id, status)
        character(len=:), allocatable, intent(out) :: id
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: supersedes, unique, fqdn

        call make_message_unique_id(unique)
        id = "<"//unique
        call get_mail_field("supersedes", supersedes, status, all=.true.)
        if (.not. status%ok()) return

        if (allocated(supersedes)) then
            if (index(supersedes, "_-_@") > 0) id = id//"_-_"
        end if

        call make_message_fqdn(fqdn, status)
        if (.not. status%ok()) return
        id = id//"@"//fqdn//">"
    end subroutine make_message_id

    ! The unique part of a Message-Id: the user id, the time and a counter
    ! in base 36, and ".fsf".  Port of message.el:message-unique-id.
    subroutine make_message_unique_id(id)
        use time_functions_mod, only: current_time, time_type
        character(len=:), allocatable, intent(out) :: id
        integer(int64), parameter :: BASE = 25*25
        integer(int64) :: tm
        type(time_type) :: now

        ! 2^16 * 25 just fits into 4 digits i base 36.
        if (message_unique_id_character < 0) then
            message_unique_id_character = random(BASE)
        else
            message_unique_id_character = mod(message_unique_id_character + 1, BASE)
        end if

        now = current_time()
        tm = now%seconds
        ! Append a given name, because while the generated ID is unique
        ! to this newsreader, other newsreaders might otherwise generate
        ! the same ID via another algorithm.
        id = number_base36(int(user_uid(), int64), -1)//number_base36(ishft(tm, -16) + &
                                                                      ishft(mod(message_unique_id_character, 25_int64), 16), 4)// &
             number_base36(iand(tm, int(z'ffff', int64)) + ishft(message_unique_id_character/25, 16), 4)//".fsf"
    end subroutine make_message_unique_id

    ! NUMBER in LEN digits of base 36 (as many as needed when LEN is
    ! negative), digit 0 being "z".  Port of message.el:message-number-base36.
    recursive function number_base36(number, length) result(digits)
        integer(int64), intent(in) :: number
        integer, intent(in) :: length
        character(len=:), allocatable :: digits
        character(len=*), parameter :: ALPHABET = "zyxwvutsrqponmlkjihgfedcba9876543210"
        logical :: stop_here
        integer(int64) :: k

        if (length < 0) then
            stop_here = number <= 0
        else
            stop_here = length == 0
        end if

        if (stop_here) then
            digits = ""
        else
            k = mod(number, 36_int64) + 1
            digits = number_base36(number/36, length - 1)//ALPHABET(k:k)
        end if
    end function number_base36

    ! The user's fully qualified domain name: `system_name' when it has a
    ! dot, else `mail_host_address', else the domain of `user_mail_address',
    ! else the system name with ".mail-host-address-is-not-set".  Port of
    ! message.el:message-make-fqdn (`message_user_fqdn' comes with
    ! message.el).
    subroutine make_message_fqdn(fqdn, status)
        character(len=*), parameter :: BOGUS_SYSTEM_NAMES = "\`localhost\.\|\.local\'"
        character(len=:), allocatable, intent(out) :: fqdn
        type(status_type), intent(out) :: status
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: sysname, user_mail, user_domain
        logical :: saved_fold
        type(status_type) :: st
        integer :: at

        status = status_type()
        sysname = system_name()
        user_mail = user_mail_address_value()
        user_domain = ""

        ! message-user-mail-address: the address of `user_mail_address'.
        if (allocated(user_mail)) then
            if (string_match("@.*\.", user_mail, st, inhibit_modify=.true.) == 0) deallocate (user_mail)
        end if

        if (allocated(user_mail)) then
            if (index(user_mail, " ") > 0) then
                call extract_mail_address_components(user_mail, components, status)
                if (.not. status%ok()) return
                deallocate (user_mail)

                if (size(components) > 0) then
                    if (allocated(components(1)%address)) user_mail = components(1)%address
                end if
            end if
        end if

        if (allocated(user_mail)) then
            at = index(user_mail, "@")
            if (at > 0) user_domain = user_mail(at + 1:)
        end if

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.

        ! A system name without any dots is unlikely to be a good fully
        ! qualified domain name.
        if (index(sysname, ".") > 0 .and. .not. is_bogus(sysname)) then
            ! `system_name' returned the right result.
            fqdn = sysname
        else if (is_bogus_unset(mail_host_address)) then
            ! Try `user_mail_address' as a backup.
            if (len(user_domain) > 0 .and. .not. is_bogus(user_domain)) then
                fqdn = user_domain
            else
                ! Default to this bogus thing.
                fqdn = sysname//".mail-host-address-is-not-set"
            end if
        else
            ! Try `mail_host_address'.
            fqdn = mail_host_address
        end if

        case_fold_search%value = saved_fold

    contains

        ! True if NAME matches the bogus system names.  Not a port.
        logical function is_bogus(name)
            character(len=*), intent(in) :: name
            type(status_type) :: bst

            is_bogus = string_match(BOGUS_SYSTEM_NAMES, name, bst, inhibit_modify=.true.) > 0
        end function is_bogus

        ! True if NAME is unset or bogus.  Not a port.
        logical function is_bogus_unset(name)
            character(len=:), allocatable, intent(in) :: name

            is_bogus_unset = .true.
            if (allocated(name)) is_bogus_unset = is_bogus(name)
        end function is_bogus_unset
    end subroutine make_message_fqdn

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> Set the defaults of smtpmail's options that are computed
    !> (`smtpmail_smtp_server' from the environment variable SMTPSERVER,
    !> `smtpmail_queue_directory', `smtpmail_queue_index_file',
    !> `smtpmail_authentication_supported') and define its command.  Not a port.
    subroutine initialize_smtpmail()
        character(len=:), allocatable :: value
        logical :: found

        if (.not. allocated(smtpmail_smtp_server)) then
            call getenv("SMTPSERVER", value, found)

            if (found) then
                smtpmail_smtp_server = value
            else if (allocated(smtpmail_default_smtp_server)) then
                smtpmail_smtp_server = smtpmail_default_smtp_server
            end if
        end if

        if (.not. allocated(smtpmail_queue_directory)) smtpmail_queue_directory = "~/Mail/queued-mail/"
        if (.not. allocated(smtpmail_queue_index_file)) smtpmail_queue_index_file = "index"
        if (.not. allocated(smtpmail_authentication_supported)) smtpmail_authentication_supported = [character( &
                                                                                             len=8) :: "cram-md5", "plain", "login"]
        if (.not. allocated(smtpmail_recipient_addresses)) allocate (smtpmail_recipient_addresses(0))
        commands%send_queued_mail_with_smtp%execute => send_queued_mail_with_smtp
    end subroutine initialize_smtpmail

    ! ------------------------------------------------------------------
    ! The setting smtpmail_smtp_service
    ! ------------------------------------------------------------------

    ! Set `smtpmail_smtp_service' to PORT or SERVICE_NAME, the other kind
    ! cleared (as an assignment to upstream's variable).  Not a port.
    subroutine set_service(port, service_name)
        integer, intent(in), optional :: port
        character(len=*), intent(in), optional :: service_name

        if (allocated(smtpmail_smtp_service%service_name)) deallocate (smtpmail_smtp_service%service_name)
        smtpmail_smtp_service%port = -1
        if (present(port)) smtpmail_smtp_service%port = port
        if (present(service_name)) smtpmail_smtp_service%service_name = service_name
    end subroutine set_service

    ! `smtpmail_smtp_service' as getaddrinfo takes it: the service name if
    ! one is set, else the port number.  Not a port.
    function service_text() result(text)
        character(len=:), allocatable :: text
        character(len=12) :: number

        if (allocated(smtpmail_smtp_service%service_name)) then
            text = smtpmail_smtp_service%service_name
        else
            write (number, "(i0)") smtpmail_smtp_service%port
            text = trim(number)
        end if
    end function service_text

    !> Forget the port and the service name of `smtpmail_smtp_service', as
    !> any assignment to it does.  Not a port.
    subroutine smtpmail_smtp_service_clear_kinds(self)
        class(smtpmail_smtp_service_choice_setting_type), intent(inout) :: self

        self%port = -1
        if (allocated(self%service_name)) deallocate (self%service_name)
    end subroutine smtpmail_smtp_service_clear_kinds

    !> The port or service name of `smtpmail_smtp_service' as help shows it
    !> (`%port = 25', `%service_name = "smtp"'); "" when neither is set.
    !> Not a port.
    function smtpmail_smtp_service_other_value(self) result(text)
        class(smtpmail_smtp_service_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=32) :: component_text

        text = ""

        if (allocated(self%service_name)) then
            text = "%service_name = """//self%service_name//""""
        else if (self%port >= 0) then
            write (component_text, '("%port = ", i0)') self%port
            text = trim(component_text)
        end if
    end function smtpmail_smtp_service_other_value

    !> The assignment that sets the port or the service name
    !> `smtpmail_smtp_service' takes, for the error of a rejected value.
    !> Not a port.
    function smtpmail_smtp_service_kind_hint(self, number) result(text)
        class(smtpmail_smtp_service_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a port number is `"//trim(self%name)//"%port = "//number//"' and a service name `"//trim(self%name)// &
               "%service_name = ""...""'"
    end function smtpmail_smtp_service_kind_hint
end module smtpmail_mod
