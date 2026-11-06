! message_mod.f90 -- composing and sending mail messages (Message mode).
! Ported from GNU Emacs 31.1 lisp/gnus/message.el (in part: Message mode,
! composing and sending mail), with lisp/gnus/gnus-util.el (in part:
! gnus-output-to-mail) and lisp/mail/mailheader.el (in part:
! mail-header-format).
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

!> Message mode, the default mail composition package: \[compose_mail]
!> with `mail_user_agent' set to `message_user_agent' (its default)
!> prepares a buffer such as "*unsent mail*" in Message mode, whose
!> headers end at the line `mail_header_separator'; \[send_message_and_exit]
!> completes the headers (From, Date, Message-ID, ...) and sends the
!> message with `message_send_mail_function', which by default uses the
!> setting of `send_mail_function' (`send_mail_with_sendmail' or
!> `send_mail_with_smtp').  See docs/status/message_mod.md for what is
!> deferred.
module message_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE, PREFIX_RAW, PREFIX_NUMBER
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE, VOID_FUNCTION_CODE, QUIT_CODE, &
                         substitute_quotes
    use rfc2047_mod, only: encode_rfc2047_message_header, fold_rfc2047_field
    use casefiddle_mod, only: downcase_string, capitalize_string
    use handle_mod, only: handle_type
    use value_mod, only: value_type, command_procedure, make_buffer_reference, make_list, vtag, V_INTEGER, V_STRING
    use hooks_mod, only: hook_type
    use string_type_mod, only: string_type, append_string, prepend_string
    use data_mod, only: append_locals_clearer, choice_setting_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          erase_buffer, close_buffer, buffer_serial_live, generate_new_buffer_name, set_buffer_modified, &
                          is_buffer_modified, barf_if_buffer_read_only, fill_column, get_file_buffer, rename_buffer, buffer_list
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, is_end_of_line, &
                                     is_end_of_buffer, is_beginning_of_buffer, buffer_substring, buffer_substring_no_properties, &
                                     buffer_string, delete_region, narrow_to_region, widen, insert_buffer_substring, &
                                     line_beginning_position, line_end_position, preceding_character, following_character, &
                                     buffer_size, delete_and_extract_region, system_name, user_full_name, user_login_name, &
                                     save_excursion_begin, save_excursion_end, excursion_type, save_restriction_begin, &
                                     save_restriction_end, restriction_type, mark_position
    use insert_delete_mod, only: insert, insert_before_markers
    use editing_commands_mod, only: forward_line, forward_character, end_of_line, beginning_of_line
    use syntax_mod, only: skip_characters_forward, skip_characters_backward, make_syntax_table, modify_syntax_entry, &
                          parse_expression_ignore_comments
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, search_backward, &
                          looking_at, match_beginning, match_end, replace_match, string_match, regular_expression_quote, &
                          case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: read_y_or_n, match_string, mail_user_agent_type, mail_switch_procedure
    use utility_functions_mod, only: read_yes_or_no
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use file_input_output_mod, only: expand_file_name, file_name_as_directory, file_name_directory, directory_file_name, &
                                     file_exists, is_file_readable, is_file_writable, is_file_directory, &
                                     insert_file_contents, write_region
    use files_mod, only: save_buffer, create_directory
    use environment_mod, only: getenv
    use display_engine_mod, only: message
    use simple_mod, only: fill_prefix, push_mark, do_auto_fill, normal_auto_fill_function, newline, cut_region, count_lines, &
                          turn_on_auto_fill, mail_user_agent, auto_fill_inhibit_regular_expression, execute_shell_region
    use fill_mod, only: fill_region_as_paragraph, fill_paragraph, fill_paragraph_function, &
                        adaptive_fill_regular_expression, adaptive_fill_first_line_regular_expression
    use indent_mod, only: current_column, indent_tabs_mode
    use newcomment_mod, only: comment_start, comment_start_skip
    use paragraphs_mod, only: paragraph_start, paragraph_separate, page_delimiter
    use abbreviation_mod, only: expand_abbreviation
    use modes_mod, only: major_modes
    use derived_mod, only: is_derived_mode
    use keymap_mod, only: keymap_set
    use text_mode_mod, only: text_mode_syntax_table, initialize_text_mode
    use window_mod, only: pop_to_buffer, pop_to_buffer_same_window, get_buffer_window, select_window, bury_buffer, &
                          switch_to_buffer_other_window
    use minibuffer_reading_mod, only: read_from_minibuffer
    use minibuffer_mod, only: read_file_name, REQUIRE_MATCH_YES
    use read_multiple_choice_mod, only: read_multiple_choice, read_multiple_choice_choice_type
    use coding_mod, only: buffer_file_coding_system
    use time_functions_mod, only: format_date, date_format_type, MAIL_DATE, current_time, current_time_string, time_type, &
                                  DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND
    use startup_mod, only: mail_host_address, user_mail_address_value
    use execute_process_mod, only: execute_process_region
    use undo_mod, only: undo_boundary
    use mail_utilities_mod, only: get_mail_field, strip_mail_quoted_names, is_mail_file_babyl, mail_re_regular_expressions, &
                                  wrap_mail_re_regular_expression
    use mail_extract_mod, only: extract_mail_address_components, mail_address_components_type
    use mail_abbreviations_mod, only: set_up_mail_abbreviations
    use send_mail_mod, only: mail_header_separator, send_mail_function, query_sendmail_once, send_mail_with_sendmail, &
                             sendmail_program, mail_interactive, mail_envelope_from, mail_paste_prefix, &
                             mail_citation_prefix_regular_expression, mail_signature_file, mail_default_reply_to, mail_self_blind, &
                             mail_archive_file_name, mail_default_headers, MAIL_ENVELOPE_FROM_HEADER
    implicit none(type, external)
    private

    public :: message_mode, mail_message, mail_message_other_window, set_up_message, send_message_and_exit, send_message, &
              close_message_buffer, message_dont_send, bury_message, send_message_mail, send_message_mail_with_sendmail, &
              send_message_mail_with_send_mail_function, send_message_mail_by_default, get_message_default_send_mail_function, &
              generate_message_headers, make_message_id, make_message_unique_id, make_message_date, &
              make_message_fully_qualified_domain_name, make_message_domain, make_message_from, make_message_address, &
              make_message_sender, get_message_user_mail_address, get_message_sendmail_envelope_from, &
              narrow_to_message_headers, narrow_to_message_headers_or_head, get_message_field, get_message_field_value, &
              remove_message_header, remove_message_first_header, add_message_header, replace_message_header, &
              position_message_on_field, get_message_all_recipients, get_message_buffers, generate_message_buffer_name, &
              pop_to_message_buffer, sort_message_headers, insert_message_signature, insert_message_headers, &
              goto_message_body, goto_message_end_of_headers, goto_message_signature, is_message_point_in_header, &
              is_message_in_body, cleanup_message_headers, encode_message_body, &
              append_message_to_mail_file, rename_sent_message_buffer, set_message_reply_headers, &
              set_message_reply_buffer, initialize_message

    !> Values of `message_from_style': the return address only
    !> (MESSAGE_FROM_STYLE_SIMPLE), the full name in parentheses after it
    !> (MESSAGE_FROM_STYLE_PARENTHESES), the address in angles after the full
    !> name (MESSAGE_FROM_STYLE_ANGLES), and angles unless they would need
    !> quoting and parentheses would not (MESSAGE_FROM_STYLE_DEFAULT).
    integer, parameter, public :: MESSAGE_FROM_STYLE_SIMPLE = 0, MESSAGE_FROM_STYLE_PARENTHESES = 1, &
                                  MESSAGE_FROM_STYLE_ANGLES = 2, MESSAGE_FROM_STYLE_DEFAULT = 3

    !> Values of `message_generate_new_buffers' besides .true. (a new
    !> buffer named the Message way) and .false. (continue editing the
    !> buffer of that name): a new buffer whose name begins with "*unsent "
    !> (MESSAGE_GENERATE_NEW_BUFFERS_UNSENT), or the buffer of a simpler
    !> name such as "*mail message*" (MESSAGE_GENERATE_NEW_BUFFERS_STANDARD).
    integer, parameter, public :: MESSAGE_GENERATE_NEW_BUFFERS_UNSENT = 2, MESSAGE_GENERATE_NEW_BUFFERS_STANDARD = 3

    !> Values of `message_sendmail_envelope_from' besides .false. (use
    !> `user_mail_address'): obey `mail_envelope_from'
    !> (MESSAGE_SENDMAIL_ENVELOPE_FROM_OBEY_MAIL_ENVELOPE_FROM), or use the
    !> From field of the message (MESSAGE_SENDMAIL_ENVELOPE_FROM_HEADER).
    integer, parameter, public :: MESSAGE_SENDMAIL_ENVELOPE_FROM_OBEY_MAIL_ENVELOPE_FROM = 2, &
                                  MESSAGE_SENDMAIL_ENVELOPE_FROM_HEADER = 3

    !> Values of `message_allow_no_recipients' besides .true. (always
    !> allow) and .false. (never allow): ask (MESSAGE_ALLOW_NO_RECIPIENTS_ASK).
    integer, parameter, public :: MESSAGE_ALLOW_NO_RECIPIENTS_ASK = 2

    !> The length of a header name in `message_required_mail_headers' and
    !> the other header lists.  Not a port.
    integer, parameter, public :: MESSAGE_HEADER_NAME_MAXIMUM = 40

    !> The length of a header value in `message_required_mail_headers'.
    !> Not a port.
    integer, parameter, public :: MESSAGE_HEADER_VALUE_MAXIMUM = 200

    abstract interface
        !> A procedure giving text for a setting that can also be a
        !> function (`message_signature', `message_user_organization',
        !> `message_default_headers', a header's value): TEXT, unallocated
        !> for none.  Not a port.
        recursive subroutine message_text_procedure(text, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: text
            type(status_type), intent(out) :: status
        end subroutine message_text_procedure

        !> A `message_generate_new_buffers' procedure: the name of the new
        !> buffer for a message of TYPE ("mail") to TO in GROUP (either
        !> may be absent).  Not a port.
        recursive subroutine message_buffer_name_procedure(type, name, status, to, group)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: type
            character(len=:), allocatable, intent(out) :: name
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: to, group
        end subroutine message_buffer_name_procedure

        !> A `message_fcc_handler_function': save the message in the
        !> current buffer in the file FILENAME.  Not a port.
        recursive subroutine message_fcc_procedure(filename, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
        end subroutine message_fcc_procedure
    end interface
    public :: message_text_procedure, message_buffer_name_procedure, message_fcc_procedure

    !> A header for `message_required_mail_headers' and
    !> `message_required_headers': its NAME, whether it is OPTIONAL
    !> (inserted only when a value can be made), and the VALUE to insert
    !> or the PROCEDURE giving it:
    !>
    !>     message_required_mail_headers = [message_header_type("From"), &
    !>         message_header_type("User-Agent", optional=.true.), &
    !>         message_header_type("X-Mailer", value="Fortmacs")]
    type, public :: message_header_type
        character(len=MESSAGE_HEADER_NAME_MAXIMUM) :: name = ""
        logical :: optional = .false.
        character(len=MESSAGE_HEADER_VALUE_MAXIMUM) :: value = ""
        procedure(message_text_procedure), pointer, nopass :: procedure => null()
    end type message_header_type

    !> The headers of the message being replied to (the subject, from,
    !> date, id and references of upstream's message-reply-headers):
    !> `make_message_id', `generate_message_headers' (In-Reply-To and
    !> References) use them.  Unallocated components are absent.
    type, public :: message_reply_headers_type
        character(len=:), allocatable :: subject, from, date, id, references
    end type message_reply_headers_type

    !> The setting `message_user_organization': .true. consults
    !> `message_user_organization_file', .false. inserts none; its text
    !> (`%text') or procedure (`%procedure', set with `=>') gives the
    !> organization instead (R6-39, R6-37).  Not a port.
    type, extends(choice_setting_type), public :: message_user_organization_choice_setting_type
        !> The organization (upstream's string value).
        character(len=:), allocatable :: text
        !> What to call for the organization (upstream's function value).
        procedure(message_text_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => organization_clear_kinds
        procedure :: other_value => organization_other_value
        procedure :: kind_hint => organization_kind_hint
    end type message_user_organization_choice_setting_type

    !> The setting `message_signature': .true. inserts the file
    !> `message_signature_file', .false. nothing; its text (`%text') or
    !> procedure (`%procedure', set with `=>') gives the signature instead
    !> (R6-39, R6-37).  Not a port.
    type, extends(choice_setting_type), public :: message_signature_choice_setting_type
        !> The signature (upstream's string value).
        character(len=:), allocatable :: text
        !> What to call for the signature (upstream's function value).
        procedure(message_text_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => signature_clear_kinds
        procedure :: other_value => signature_other_value
        procedure :: kind_hint => signature_kind_hint
    end type message_signature_choice_setting_type

    !> The setting `message_default_headers': .false. inserts none; its
    !> text (`%text') or procedure (`%procedure', set with `=>') gives the
    !> header lines (R6-39, R6-37).  Not a port.
    type, extends(choice_setting_type), public :: message_default_headers_choice_setting_type
        !> The header lines (upstream's string value).
        character(len=:), allocatable :: text
        !> What to call for the header lines (upstream's function value).
        procedure(message_text_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => default_headers_clear_kinds
        procedure :: other_value => default_headers_other_value
        procedure :: kind_hint => default_headers_kind_hint
    end type message_default_headers_choice_setting_type

    !> The setting `message_generate_new_buffers': .true., .false.,
    !> MESSAGE_GENERATE_NEW_BUFFERS_UNSENT, MESSAGE_GENERATE_NEW_BUFFERS_STANDARD,
    !> or a procedure (`%procedure', set with `=>'; R6-37).  Not a port.
    type, extends(choice_setting_type), public :: message_generate_new_buffers_choice_setting_type
        !> What to call for the buffer name (upstream's function value).
        procedure(message_buffer_name_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => new_buffers_clear_kinds
        procedure :: other_value => new_buffers_other_value
        procedure :: kind_hint => new_buffers_kind_hint
    end type message_generate_new_buffers_choice_setting_type

    !> The setting `message_sendmail_envelope_from':
    !> MESSAGE_SENDMAIL_ENVELOPE_FROM_OBEY_MAIL_ENVELOPE_FROM, .false.,
    !> MESSAGE_SENDMAIL_ENVELOPE_FROM_HEADER or an address (R6-39:
    !> `message_sendmail_envelope_from%address = "..."').  Not a port.
    type, extends(choice_setting_type), public :: message_sendmail_envelope_from_choice_setting_type
        !> The envelope-from address (upstream's string value).
        character(len=:), allocatable :: address
    contains
        procedure :: clear_kinds => envelope_from_clear_kinds
        procedure :: other_value => envelope_from_other_value
        procedure :: kind_hint => envelope_from_kind_hint
    end type message_sendmail_envelope_from_choice_setting_type

    !> The setting `message_fill_column': .false. (don't turn on Auto
    !> Fill mode) or a column (R6-39: `message_fill_column%columns = 72';
    !> negative when none is set).  Not a port.
    type, extends(choice_setting_type), public :: message_fill_column_choice_setting_type
        !> The fill column (upstream's integer value).
        integer :: columns = -1
    contains
        procedure :: clear_kinds => fill_column_clear_kinds
        procedure :: other_value => fill_column_other_value
        procedure :: kind_hint => fill_column_kind_hint
    end type message_fill_column_choice_setting_type

    !> Include extra information in the In-Reply-To header.
    !> This form has been obsolete since RFC 2822.  Port of message.el
    !> message-header-use-obsolete-in-reply-to.
    logical, public :: message_header_use_obsolete_in_reply_to = .false.

    !> Directory from which all other mail file variables are derived.
    !> Port of message.el message-directory.
    character(len=:), allocatable, public :: message_directory

    !> How many buffers to keep before starting to kill them off.  Port of
    !> message.el message-max-buffers.
    integer, public :: message_maximum_buffers = 10

    !> Function called to rename the buffer after sending it.  Port of
    !> message.el message-send-rename-function.
    procedure(command_procedure), pointer, public :: message_send_rename_function => rename_sent_message_buffer

    !> A function called to save outgoing articles.
    !> This function will be called with the name of the file to store the
    !> article in.  The default function is `append_message_to_mail_file'
    !> which saves in Unix mailbox format.  Port of message.el
    !> message-fcc-handler-function.
    procedure(message_fcc_procedure), pointer, public :: message_fcc_handler_function => append_message_to_mail_file

    !> Specifies how "From" headers look.
    !>
    !> If MESSAGE_FROM_STYLE_SIMPLE, they contain just the return address like:
    !>     king@grassland.com
    !> If MESSAGE_FROM_STYLE_PARENTHESES, they look like:
    !>     king@grassland.com (Elvis Parsley)
    !> If MESSAGE_FROM_STYLE_ANGLES, they look like:
    !>     Elvis Parsley <king@grassland.com>
    !>
    !> Otherwise, most addresses look like MESSAGE_FROM_STYLE_ANGLES, but
    !> they look like MESSAGE_FROM_STYLE_PARENTHESES if MESSAGE_FROM_STYLE_ANGLES
    !> would need quoting and MESSAGE_FROM_STYLE_PARENTHESES would not.  Port of
    !> message.el message-from-style.
    integer, public :: message_from_style = MESSAGE_FROM_STYLE_ANGLES

    !> The syntax checks of outgoing messages, one logical per check:
    !> .true. performs it, .false. does not
    !> (`message_syntax_checks%signature = .false.').  Not a port: the
    !> keys of upstream's message-syntax-checks.
    type, public :: message_syntax_checks_type
        logical :: approved = .true., bogus_recipient = .true., continuation_headers = .true., &
                   control_characters = .true., empty = .true., existing_newsgroups = .true., from = .true., &
                   illegible_text = .true., invisible_text = .true., long_header_lines = .true., long_lines = .true., &
                   message_id = .true., multiple_headers = .true., new_text = .true., newsgroups = .true., &
                   quoting_style = .true., repeated_newsgroups = .true., reply_to = .true., sender = .true., &
                   sendsys = .true., shoot = .true., shorten_followup_to = .true., signature = .true., size = .true., &
                   subject = .true., subject_control_message = .true., valid_newsgroups = .true.
    end type message_syntax_checks_type

    !> Controls what syntax checks should not be performed on outgoing
    !> posts.  To disable checking of long signatures, for instance, set
    !> `message_syntax_checks%signature = .false.'.
    !>
    !> Don't touch this variable unless you really know what you're doing.
    !>
    !> See the Message manual for the meanings of the valid syntax check
    !> types.  Port of message.el message-syntax-checks (its default
    !> disables the sender check; the value
    !> dont-check-for-anything-just-trust-me is Deferred).
    type(message_syntax_checks_type), public :: message_syntax_checks = message_syntax_checks_type(sender=.false.)

    !> Headers to be generated or prompted for when sending a message.
    !> Also see `message_required_mail_headers'.  Port of message.el
    !> message-required-headers.
    type(message_header_type), allocatable, public :: message_required_headers(:)

    !> Headers to be generated or prompted for when mailing a message.
    !> It is recommended that From, Date, To, Subject and Message-ID be
    !> included.  Organization and User-Agent are optional.  Port of
    !> message.el message-required-mail-headers.
    type(message_header_type), allocatable, public :: message_required_mail_headers(:)

    !> Headers to delete if present and previously generated by message.
    !> Port of message.el message-deletable-headers.
    character(len=:), allocatable, public :: message_deletable_headers(:)

    !> Regexp of headers to be removed unconditionally before mailing.
    !> Port of message.el message-ignored-mail-headers.
    character(len=:), allocatable, public :: message_ignored_mail_headers

    !> Regexp matching the signature separator.  Port of message.el
    !> message-signature-separator.
    character(len=:), allocatable, public :: message_signature_separator

    !> The string which is inserted for elided text.
    !> You can use %l to say how many lines were removed, and %c to say
    !> how many characters were removed.  Port of message.el
    !> message-elide-ellipsis.
    character(len=:), allocatable, public :: message_elide_ellipsis

    !> True means when sending a message wait for and display errors.
    !> A value of .false. means let mailer mail back a message to report
    !> errors.  Port of message.el message-interactive.
    logical, public :: message_interactive = .true.

    !> When true, ask for confirmation when sending a message.  Port of
    !> message.el message-confirm-send.
    logical, public :: message_confirm_send = .false.

    !> Say whether to create a new message buffer to compose a message.
    !> Valid values include:
    !>
    !> .false.
    !>   Generate the buffer name in the Message way (e.g., *mail*, *news*,
    !>   *mail to whom*, *news on group*, etc.) and continue editing in the
    !>   existing buffer of that name.  If there is no such buffer, it will
    !>   be newly created.
    !>
    !> .true.
    !>   Create the new buffer with the name generated in the Message way.
    !>
    !> MESSAGE_GENERATE_NEW_BUFFERS_UNSENT
    !>   Similar to .true. but the buffer name begins with "*unsent ".
    !>
    !> MESSAGE_GENERATE_NEW_BUFFERS_STANDARD
    !>   Similar to .false. but the buffer name is simpler like *mail message*.
    !>
    !> a procedure (`%procedure')
    !>   If this is a procedure, call it with three arguments:
    !>   The type, the To address and the group name (any of these may be
    !>   absent).  The procedure should return the new buffer name.  Port of
    !>   message.el message-generate-new-buffers.
    type(message_generate_new_buffers_choice_setting_type), public :: message_generate_new_buffers = &
                                             message_generate_new_buffers_choice_setting_type(name="message_generate_new_buffers", &
                                                                                         choice=MESSAGE_GENERATE_NEW_BUFFERS_UNSENT)

    !> True means that the message buffer will be closed after sending a
    !> message.  Port of message.el message-kill-buffer-on-exit.
    logical, public :: message_close_buffer_on_exit = .false.

    !> True means that closing a modified message buffer has to be confirmed.
    !> This is used by `close_message_buffer'.  Port of message.el
    !> message-kill-buffer-query.
    logical, public :: message_close_buffer_query = .true.

    !> String to be used as an Organization header.
    !> If .true., use `message_user_organization_file'.  Port of
    !> message.el message-user-organization (set by `initialize_message' from
    !> the environment variable ORGANIZATION unless already set).
    type(message_user_organization_choice_setting_type), public :: message_user_organization = &
                                       message_user_organization_choice_setting_type(name="message_user_organization", value=.true.)

    !> Local news organization file.  Port of message.el
    !> message-user-organization-file.
    character(len=:), allocatable, public :: message_user_organization_file

    !> Function to call to send the current buffer as mail.
    !> The headers should be delimited by a line whose contents match the
    !> variable `mail_header_separator'.
    !>
    !> Valid values include `send_message_mail_with_sendmail',
    !> `send_mail_with_smtp' and `send_message_mail_by_default', the
    !> default, which uses the setting of `send_mail_function'.
    !>
    !> See also `send_mail_function'.  Port of message.el
    !> message-send-mail-function.
    procedure(command_procedure), pointer, public :: message_send_mail_function => send_message_mail_by_default

    !> Specifies what to do when there are no recipients other than Fcc.
    !> If it is .true., the posting is allowed.  If it is .false., the
    !> posting is not allowed.  If it is MESSAGE_ALLOW_NO_RECIPIENTS_ASK,
    !> you are prompted.  Port of message.el message-allow-no-recipients.
    type(choice_setting_type), public :: message_allow_no_recipients = &
                                     choice_setting_type(name="message_allow_no_recipients", choice=MESSAGE_ALLOW_NO_RECIPIENTS_ASK)

    !> True means don't add "-f username" to the "sendmail" command line.
    !> The "sendmail" program has a useful feature to let you set the
    !> envelope FROM address via a command line option, "-f".
    !> Unfortunately, it also has a widely disliked default behavior of
    !> disclosing your actual user name anyway by inserting an
    !> unattractive warning in the headers.  It looks something like
    !> this:
    !>
    !>   X-Authentication-Warning: u1.example.com: niceguy set
    !>       sender to niceguy@example.com using -f
    !>
    !> It is possible to configure "sendmail" to not do this, but such a
    !> reconfiguration is not an option for some users.
    !>
    !> Note that this user option is mostly useful for actual "sendmail"
    !> installations, which are rare these days.  Port of message.el
    !> message-sendmail-f-is-evil.
    logical, public :: message_sendmail_f_is_evil = .false.

    !> Envelope-from when sending mail with sendmail.
    !> If this is MESSAGE_SENDMAIL_ENVELOPE_FROM_OBEY_MAIL_ENVELOPE_FROM,
    !> then use `mail_envelope_from' to decide what to do.  If it is
    !> .false., use `user_mail_address'.  If it is
    !> MESSAGE_SENDMAIL_ENVELOPE_FROM_HEADER, use the "From:" header of the
    !> message.  Port of message.el message-sendmail-envelope-from.
    type(message_sendmail_envelope_from_choice_setting_type), public :: message_sendmail_envelope_from = &
                     message_sendmail_envelope_from_choice_setting_type(name="message_sendmail_envelope_from", true_valid=.false., &
                                                                      choice=MESSAGE_SENDMAIL_ENVELOPE_FROM_OBEY_MAIL_ENVELOPE_FROM)

    !> Additional arguments to `sendmail_program'.
    !> An array of strings, e.g. ["-a", "account"] for msmtp.  Port of
    !> message.el message-sendmail-extra-arguments.
    character(len=:), allocatable, public :: message_sendmail_extra_arguments(:)

    !> Column beyond which automatic line-wrapping should happen.
    !> Local value for message buffers.  If set, also turn on
    !> auto-fill in message buffers.  Port of message.el message-fill-column.
    type(message_fill_column_choice_setting_type), public :: message_fill_column = &
                                 message_fill_column_choice_setting_type(name="message_fill_column", true_valid=.false., columns=72)

    !> Normal hook, run each time a new outgoing message is initialized.
    !> The procedure `set_up_message' runs this hook.  Port of message.el
    !> message-setup-hook.
    type(hook_type), target, public :: set_up_message_hook = hook_type(name="set_up_message_hook")

    !> Normal hook, run each time a new outgoing message is initialized.
    !> It is run after the headers have been inserted and before
    !> the signature is inserted.  Port of message.el
    !> message-signature-setup-hook.
    type(hook_type), target, public :: message_signature_setup_hook = hook_type(name="message_signature_setup_hook")

    !> Hook run in message mode buffers.  Port of message.el message-mode-hook.
    type(hook_type), target, public :: message_mode_hook = hook_type(name="message_mode_hook")

    !> Hook run in a message mode buffer narrowed to the headers.  Port of
    !> message.el message-header-hook.
    type(hook_type), target, public :: message_header_hook = hook_type(name="message_header_hook")

    !> Hook called narrowed to the headers when setting up a message buffer.
    !> Port of message.el message-header-setup-hook.
    type(hook_type), target, public :: message_header_setup_hook = hook_type(name="message_header_setup_hook")

    !> Prefix inserted on the lines of pasted messages, or unallocated for
    !> none.  Port of message.el message-yank-prefix (set by
    !> `initialize_message' from `mail_paste_prefix' unless already set).
    character(len=:), allocatable, public :: message_paste_prefix

    !> Regexp matching the longest possible citation prefix on a line.
    !> Port of message.el message-cite-prefix-regexp (set by
    !> `initialize_message' from `mail_citation_prefix_regular_expression' unless already set).
    character(len=:), allocatable, public :: message_cite_prefix_regular_expression

    !> Text inserted at end of message buffer when a message is initialized.
    !> If .false., no signature is inserted.
    !> If .true., it means to insert the contents of the file
    !> `message_signature_file'.
    !> If its text is set (`message_signature%text = "..."'), that string
    !> is inserted.
    !> If its procedure is set (`message_signature%procedure => f'), it is
    !> called, and the text it gives is inserted.  Port of message.el
    !> message-signature.
    type(message_signature_choice_setting_type), public :: message_signature = &
                                                       message_signature_choice_setting_type(name="message_signature", value=.true.)

    !> Name of file containing the text inserted at end of message buffer.
    !> Ignored if the named file doesn't exist.
    !> If unallocated, `message_signature_directory' is not used either.
    !> If a path is specified, the value of `message_signature_directory'
    !> is ignored, even if set.  Port of message.el message-signature-file
    !> (set by `initialize_message' from `mail_signature_file' unless already set).
    character(len=:), allocatable, public :: message_signature_file

    !> Name of directory containing signature files.
    !> Comes in handy if you have many such files, handled via posting
    !> styles for instance.  If unallocated, `message_signature_file' is
    !> expected to specify the directory if needed.  Port of message.el
    !> message-signature-directory.
    character(len=:), allocatable, public :: message_signature_directory

    !> If true, insert an empty line before the signature separator.
    !> Port of message.el message-signature-insert-empty-line.
    logical, public :: message_signature_insert_empty_line = .true.

    !> Header lines to be inserted in outgoing messages.
    !> This can be set to a string containing or a procedure giving
    !> header lines to be inserted before you edit the message, so you
    !> can edit or delete these lines.  If set to a procedure, it is
    !> called and the text it gives is inserted.  Port of message.el
    !> message-default-headers.
    type(message_default_headers_choice_setting_type), public :: message_default_headers = &
                                     message_default_headers_choice_setting_type(name="message_default_headers", true_valid=.false.)

    !> A string of header lines to be inserted in outgoing mails.  Port of
    !> message.el message-default-mail-headers (set by `initialize_message' from
    !> `mail_default_reply_to', `mail_self_blind', `mail_archive_file_name'
    !> and `mail_default_headers' unless already set).
    character(len=:), allocatable, public :: message_default_mail_headers

    !> Hook run before sending messages.
    !> This hook is run quite early when sending.  Port of message.el
    !> message-send-hook.
    type(hook_type), target, public :: message_send_hook = hook_type(name="message_send_hook")

    !> Hook run before sending mail messages.
    !> This hook is run very late -- just before the message is sent as
    !> mail.  Port of message.el message-send-mail-hook.
    type(hook_type), target, public :: message_send_mail_hook = hook_type(name="message_send_mail_hook")

    !> Hook run after sending messages.  Port of message.el message-sent-hook.
    type(hook_type), target, public :: message_sent_hook = hook_type(name="message_sent_hook")

    !> Domain part of Message-IDs.  Port of message.el message-user-fqdn.
    character(len=:), allocatable, public :: message_user_fully_qualified_domain_name

    !> Directory where Message auto-saves buffers if Gnus isn't running.
    !> If unallocated, Message won't auto-save.  Port of message.el
    !> message-auto-save-directory (set by `initialize_message' unless already
    !> set).
    character(len=:), allocatable, public :: message_auto_save_directory

    !> Whether \[message_beginning_of_line] goes to beginning of header
    !> values.  Port of message.el message-beginning-of-line.
    logical, public :: message_beginning_of_line_variable = .true.

    !> How the text inserted with \[mark_message_inserted_region] begins.
    !> Port of message.el message-mark-insert-begin.
    character(len=:), allocatable, public :: message_mark_insert_begin

    !> How the text inserted with \[mark_message_inserted_region] ends.
    !> Port of message.el message-mark-insert-end.
    character(len=:), allocatable, public :: message_mark_insert_end

    !> Header to insert when you don't want your article to be archived.
    !> Archives (such as groups.google.com) respect this header.  Port of
    !> message.el message-archive-header.
    character(len=:), allocatable, public :: message_archive_header

    !> Note to insert why you wouldn't want this posting archived.
    !> If unallocated, don't insert any text in the body.  Port of
    !> message.el message-archive-note.
    character(len=:), allocatable, public :: message_archive_note

    !> Number of days before your article expires.  Port of message.el
    !> message-expires.
    integer, public :: message_expires = 14

    !> Your Path header, or unallocated for the system name.  Port of
    !> message.el message-user-path (its value .true., the login name
    !> alone, is Deferred).
    character(len=:), allocatable, public :: message_user_path

    !> Regular expressions matching mail addresses that look bogus (as
    !> "noreply" addresses do).  Port of message.el message-bogus-addresses.
    character(len=:), allocatable, public :: message_bogus_addresses(:)

    !> Replacement character used instead of unprintable or not decodable
    !> chars.  Port of message.el message-replacement-char.
    character(len=:), allocatable, public :: message_replacement_character

    !> What to do with trailing "(was: <old subject>)" in subject lines.
    !> If .false., leave the subject unchanged.  If .true., always strip
    !> the trailing old subject, matched by
    !> `message_subject_trailing_was_regular_expression'.  Port of message.el
    !> message-subject-trailing-was-query (its value `ask' is Deferred).
    logical, public :: message_subject_trailing_was_query = .true.

    !> Regexp matching "(was: <old subject>)" in the subject line.  Port of
    !> message.el message-subject-trailing-was-regexp.
    character(len=:), allocatable, public :: message_subject_trailing_was_regular_expression

    !> Regexp matching "Re: " in the subject line.  Port of message.el
    !> message-subject-re-regexp.
    character(len=:), allocatable, public :: message_subject_re_regular_expression

    !> Keymap of `message_mode'.  Port of message.el message-mode-map.
    type(handle_type), public :: message_mode_map

    !> Syntax table used while in Message mode.  Port of message.el
    !> message-mode-syntax-table.
    type(handle_type), public :: message_mode_syntax_table

    !> The Message package as a mail user agent for `mail_user_agent'.
    !> Port of message.el message-user-agent.
    type(mail_user_agent_type), target, public :: message_user_agent

    !> The counter of `make_message_unique_id' (negative until the first
    !> use).  Port of message.el message-unique-id-char.
    integer(int64), public :: message_unique_id_character = -1

    ! A Message mode buffer's own values (set locally by message-mode), by
    ! serial number: message-reply-buffer, message-reply-headers,
    ! message-inserted-headers, message-sent-message-via (mail only) and
    ! message-encoded-mail-cache.
    type :: local_type
        integer(int64) :: serial = 0
        type(handle_type) :: reply_buffer
        logical :: has_reply_headers = .false.
        type(message_reply_headers_type) :: reply_headers
        character(len=:), allocatable :: inserted_headers
        logical :: sent_via_mail = .false.
        character(len=:), allocatable :: encoded_mail_cache
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! message-buffer-list: the sent message buffers, oldest first.
    type(handle_type), allocatable :: message_buffer_list(:)

    ! message-this-is-mail, bound while composing and sending mail.
    logical :: message_this_is_mail = .false.

    ! Whether `load_message' set the defaults that come from other settings.
    logical :: loaded = .false.

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    ! The upstream `message-header-format-alist' order (its keys), for
    ! `sort_message_headers' and `set_up_message'.
    character(len=*), parameter :: HEADER_FORMAT_ORDER(*) = [character(len=12) :: "From", "Newsgroups", "To", "Cc", &
                                                                   "Subject", "In-Reply-To", "Fcc", "Bcc", "Date", "Organization", &
                                                                   "Distribution", "Lines", "Expires", "Message-ID", "References", &
                                                                                                                       "User-Agent"]

    ! message-bogus-system-names.
    character(len=*), parameter :: BOGUS_SYSTEM_NAMES = "\`localhost\.\|\.local\'"

    ! The text property of the header names Message generated, and the
    ! face it gives them (message-deletable, face italic).
    character(len=*), parameter :: P_MESSAGE_DELETABLE = "message_deletable", P_FACE = "face", FACE_ITALIC = "italic"

    interface
        !> Send the current buffer to `message_send_mail_function'.
        !> Or, if there's a header that specifies a different method, use
        !> that instead.  Port of message.el:message-multi-smtp-send-mail.
        !> (In submodule message_smtp_mod, as its "smtp" method uses
        !> smtpmail_mod, which uses this module.)
        recursive module subroutine send_message_mail_by_method(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine send_message_mail_by_method
    end interface

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals' (0 if none).
    ! Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, dim=1)
    end function local_index

    ! Find or make the current buffer's entry in `locals', reusing the
    ! entry of a closed buffer; K is its index (0 without a current
    ! buffer).  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, entry]
        locals(k) = local_type(serial=buffers(current)%serial)
    end subroutine make_local_entry

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine clear_message_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = local_type()
    end subroutine clear_message_locals

    !> Make BUFFER the buffer of the message the current Message mode
    !> buffer replies to (`message_reply_buffer').  Not a port: upstream
    !> sets the variable.
    subroutine set_message_reply_buffer(buffer)
        type(handle_type), intent(in) :: buffer
        integer :: k

        call make_local_entry(k)
        if (k > 0) locals(k)%reply_buffer = buffer
    end subroutine set_message_reply_buffer

    !> Make HEADERS the headers of the message the current Message mode
    !> buffer replies to (`message_reply_headers'), which Message-ID,
    !> In-Reply-To and References are made from.  Not a port: upstream
    !> sets the variable.
    subroutine set_message_reply_headers(headers)
        type(message_reply_headers_type), intent(in) :: headers
        integer :: k

        call make_local_entry(k)

        if (k > 0) then
            locals(k)%has_reply_headers = .true.
            locals(k)%reply_headers = headers
        end if
    end subroutine set_message_reply_headers

    ! The current buffer's reply headers in HEADERS; FOUND is false when
    ! it has none (upstream's message-reply-headers nil).  Not a port.
    subroutine current_reply_headers(headers, found)
        type(message_reply_headers_type), intent(out) :: headers
        logical, intent(out) :: found
        integer :: k

        found = .false.
        k = local_index()
        if (k == 0) return
        found = locals(k)%has_reply_headers
        if (found) headers = locals(k)%reply_headers
    end subroutine current_reply_headers

    ! A marker at POSITION.  Not a port.
    function marker_at(position, insertion_type) result(m)
        integer(int64), intent(in) :: position
        logical, intent(in), optional :: insertion_type
        type(handle_type) :: m
        type(status_type) :: st
        logical :: it

        it = .false.
        if (present(insertion_type)) it = insertion_type
        call copy_marker_at(position, m, it, st)
    end function marker_at

    ! The position of marker M.  Not a port.
    integer(int64) function marked_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, marked_position, st)
    end function marked_position

    ! The position of marker M, which is destroyed.  Not a port.
    integer(int64) function marker_end(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, position, st)
        call destroy_marker(m, st)
    end function marker_end

    ! Search forward for REGULAR_EXPRESSION, telling whether it was found; MOVE
    ! moves to BOUND (or the end) on failure.  Not a port.
    recursive logical function search_regular_expression(regular_expression, status, bound, move) result(found)
        character(len=*), intent(in) :: regular_expression
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
    end function search_regular_expression

    ! Search backward for REGULAR_EXPRESSION.  Not a port.
    recursive logical function search_regular_expression_backward(regular_expression, status, bound) result(found)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        integer(int64) :: position

        call regular_expression_search_backward(regular_expression, position, status, bound, SEARCH_NOERROR)
        found = position /= 0
    end function search_regular_expression_backward

    ! Search forward for STRING; MOVE moves to the end on failure.
    ! Not a port.
    recursive logical function search_string(string, status, bound, move) result(found)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: move
        integer(int64) :: position
        integer :: noerror

        noerror = SEARCH_NOERROR

        if (present(move)) then
            if (move) noerror = SEARCH_MOVE
        end if

        call search_forward(string, position, status, bound, noerror)
        found = position /= 0
    end function search_string

    ! True if REGULAR_EXPRESSION matches STRING (case as `case_fold_search' says).
    ! Not a port.
    logical function matches(regular_expression, string)
        character(len=*), intent(in) :: regular_expression, string
        type(status_type) :: st

        matches = string_match(regular_expression, string, st, inhibit_modify=.true.) > 0
    end function matches

    ! (y-or-n-p PROMPT), false when the question fails.  Not a port.
    recursive logical function ask(prompt, status) result(answer)
        character(len=*), intent(in) :: prompt
        type(status_type), intent(out) :: status

        call read_y_or_n(prompt, answer, status)
        if (.not. status%ok()) answer = .false.
    end function ask

    ! (yes-or-no-p PROMPT), false when the question fails.  Not a port.
    recursive logical function ask_yes_or_no(prompt, status) result(answer)
        character(len=*), intent(in) :: prompt
        type(status_type), intent(out) :: status

        call read_yes_or_no(prompt, answer, status)
        if (.not. status%ok()) answer = .false.
    end function ask_yes_or_no

    ! Make a new buffer named after NAME (generate-new-buffer) and make it
    ! current, remembering the buffer that was current in OLD.  Not a port.
    subroutine temporary_buffer_begin(name, old, buffer, status)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: old, buffer
        type(status_type), intent(out) :: status

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(name), buffer, status)
        if (status%ok()) call set_buffer(buffer, status)
    end subroutine temporary_buffer_begin

    ! Close the temporary BUFFER and make OLD current again.  Not a port.
    subroutine temporary_buffer_end(old, buffer)
        type(handle_type), intent(in) :: old, buffer
        type(status_type) :: st

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine temporary_buffer_end

    ! (string-trim TEXT): TEXT without leading and trailing whitespace.
    ! Not a port.
    function trimmed(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        character(len=*), parameter :: WHITE = " "//TAB//LF//achar(13)
        integer :: first, last

        first = verify(text, WHITE)

        if (first == 0) then
            out = ""
        else
            last = verify(text, WHITE, back=.true.)
            out = text(first:last)
        end if
    end function trimmed

    ! Whether the check of the header HEADER (a lower-case header name,
    ! as upstream interns it) is disabled in `message_syntax_checks'; a
    ! header that names no check is checked.  Port of
    ! message.el:message-check-element (for a header name).
    logical function is_header_check_disabled(header) result(disabled)
        character(len=*), intent(in) :: header

        associate (c => message_syntax_checks)
            select case (header)
            case ("approved")
                disabled = .not. c%approved
            case ("from")
                disabled = .not. c%from
            case ("message-id")
                disabled = .not. c%message_id
            case ("newsgroups")
                disabled = .not. c%newsgroups
            case ("reply-to")
                disabled = .not. c%reply_to
            case ("sender")
                disabled = .not. c%sender
            case ("subject")
                disabled = .not. c%subject
            case default
                disabled = .false.
            end select
        end associate
    end function is_header_check_disabled

    ! ------------------------------------------------------------------
    ! The headers
    ! ------------------------------------------------------------------

    !> Narrow the buffer to the head of the message.  Port of
    !> message.el:message-narrow-to-headers.
    recursive subroutine narrow_to_message_headers(status)
        type(status_type), intent(out) :: status
        integer(int64) :: e

        call widen()
        call goto_character(point_minimum())

        if (search_regular_expression("^"//regular_expression_quote(mail_header_separator)//LF, status)) then
            e = match_beginning(0, status)
        else
            e = point_maximum()
        end if

        if (.not. status%ok()) return
        call narrow_to_region(point_minimum(), e, status)
        call goto_character(point_minimum())
    end subroutine narrow_to_message_headers

    !> Narrow the buffer to the head of the message: up to the first
    !> empty line or the line `mail_header_separator'.  Port of
    !> message.el:message-narrow-to-headers-or-head.
    recursive subroutine narrow_to_message_headers_or_head(status)
        type(status_type), intent(out) :: status
        integer(int64) :: e

        call widen()
        call goto_character(point_minimum())

        if (search_regular_expression("\("//LF//"\)"//LF//"\|^\("//regular_expression_quote(mail_header_separator)//LF//"\)", &
                                      status)) then
            e = match_end(1, status)
            if (e <= 0) e = match_beginning(2, status)
        else
            e = point_maximum()
        end if

        if (.not. status%ok()) return
        call narrow_to_region(point_minimum(), e, status)
        call goto_character(point_minimum())
    end subroutine narrow_to_message_headers_or_head

    !> The value of the header field named HEADER in VALUE (unallocated
    !> when there is none).
    !> Continuation lines are folded (i.e., newlines are removed).
    !> Surrounding whitespace is also removed.
    !>
    !> By default, if there's more than one header field named HEADER,
    !> all the values are returned as one concatenated string, and
    !> values are comma-separated.
    !>
    !> If FIRST is true, only the first value is returned.
    !>
    !> The buffer is expected to be narrowed to just the header of the
    !> message; see `narrow_to_message_headers_or_head'.  Port of
    !> message.el:message-fetch-field.
    recursive subroutine get_message_field(header, value, status, first)
        character(len=*), intent(in) :: header
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: first
        logical :: all
        integer :: k

        all = .true.
        if (present(first)) all = .not. first
        call get_mail_field(header, value, status, all=all)
        if (.not. status%ok()) return
        if (.not. allocated(value)) return

        do
            k = index(value, LF)
            if (k == 0) exit
            ! "\n[\t ]+" becomes one space.
            if (k == len(value)) exit
            if (verify(value(k + 1:k + 1), " "//TAB) /= 0) exit
            value = value(:k - 1)//" "//value(k + verify(value(k + 1:)//"x", " "//TAB):)
        end do

        value = trimmed(value)
    end subroutine get_message_field

    !> The same as `get_message_field', only narrow to the headers first.
    !> Port of message.el:message-field-value.
    recursive subroutine get_message_field_value(header, value, status, first)
        character(len=*), intent(in) :: header
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: first
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers_or_head(status)
        if (status%ok()) call get_message_field(header, value, status, first)
        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine get_message_field_value

    !> Remove HEADER in the narrowed buffer.
    !> If IS_REGULAR_EXPRESSION, HEADER is a regular expression.
    !> If FIRST, only remove the first instance of the header.
    !> If REVERSE, remove headers that doesn't match HEADER.
    !> Return the number of headers removed in NUMBER.  Port of
    !> message.el:message-remove-header.
    recursive subroutine remove_message_header(header, status, is_regular_expression, first, reverse, number)
        character(len=*), intent(in) :: header
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: is_regular_expression, first, reverse
        integer, intent(out), optional :: number
        character(len=:), allocatable :: regular_expression
        logical :: last, saved_fold, matched, revision, one
        integer(int64) :: start, moved
        integer :: removed

        status = status_type()
        call goto_character(point_minimum())
        regular_expression = "^"//regular_expression_quote(header)//":"

        if (present(is_regular_expression)) then
            if (is_regular_expression) regular_expression = header
        end if

        revision = .false.
        if (present(reverse)) revision = reverse
        one = .false.
        if (present(first)) one = first
        removed = 0
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        last = .false.

        do while (.not. is_end_of_buffer() .and. .not. last)
            if (revision) then
                matched = .not. looking_at(regular_expression, status)
                ! Don't remove things not looking like header.
                if (matched .and. status%ok()) matched = looking_at("[!-9;-~]+:", status)
            else
                matched = looking_at(regular_expression, status)
            end if

            if (.not. status%ok()) exit

            if (matched) then
                removed = removed + 1
                if (one) last = .true.
                start = point()
                ! There might be a continuation header, so we have to search
                ! until we find a new non-continuation line.
                moved = forward_line(1_int64)

                if (search_regular_expression("^[^ "//TAB//"]", status)) then
                    call goto_character(match_beginning(0, status))
                else
                    call goto_character(point_maximum())
                end if

                call delete_region(start, point(), status)
                if (.not. status%ok()) exit
            else
                moved = forward_line(1_int64)

                if (search_regular_expression("^[^ "//TAB//"]", status)) then
                    call goto_character(match_beginning(0, status))
                else
                    call goto_character(point_maximum())
                end if
            end if
        end do

        case_fold_search%value = saved_fold
        if (present(number)) number = removed
    end subroutine remove_message_header

    !> Remove the first instance of HEADER if there is more than one.
    !> Port of message.el:message-remove-first-header.
    recursive subroutine remove_message_first_header(header, status)
        character(len=*), intent(in) :: header
        type(status_type), intent(out) :: status
        integer :: count
        integer(int64) :: original_point

        original_point = point()
        count = 0
        call goto_character(point_minimum())

        do while (search_regular_expression("^"//regular_expression_quote(header)//":", status))
            count = count + 1
        end do

        call goto_character(original_point)
        if (.not. status%ok()) return

        do while (count > 1)
            call remove_message_header(header, status, first=.true.)
            if (.not. status%ok()) return
            count = count - 1
        end do
    end subroutine remove_message_first_header

    !> Add the header line HEADER to the message header, unless one of its
    !> name is already present.  Port of message.el:message-add-header
    !> (with one header).
    recursive subroutine add_message_header(header, status)
        character(len=*), intent(in) :: header
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: hclean
        type(restriction_type) :: restriction

        if (string_match("^\([^:]+\):[ "//TAB//"]*[^ "//TAB//"]", header, status) == 0) then
            if (status%ok()) call set_error(status, GENERIC_ERROR_CODE, "Invalid header `"//header//"'")
            return
        end if

        hclean = match_string(1, header)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)

        if (status%ok()) then
            if (.not. search_regular_expression("^"//regular_expression_quote(hclean)//":", status) .and. status%ok()) then
                call goto_character(point_maximum())

                if (header(len(header):) == LF) then
                    call insert(header, status)
                else
                    call insert(header//LF, status)
                end if
            end if
        end if

        call save_restriction_end(restriction)
    end subroutine add_message_header

    !> Remove HEADER and insert the NEW_VALUE.
    !> If AFTER, insert after these headers.  If FORCE, insert new field
    !> even if NEW_VALUE is empty.  Port of message.el:message-replace-header.
    recursive subroutine replace_message_header(header, new_value, status, after, force)
        character(len=*), intent(in) :: header, new_value
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: after(:)
        logical, intent(in), optional :: force
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        logical :: forced

        forced = .false.
        if (present(force)) forced = force
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call remove_message_header(header, status)
        call save_restriction_end(restriction)

        if (status%ok() .and. (forced .or. len(new_value) > 0)) then
            call position_message_on_field(header, status, after)
            if (status%ok()) call insert(new_value, status)
        end if

        call save_excursion_end(excursion)
    end subroutine replace_message_header

    !> Move point to header HEADER or insert it if not found.
    !>
    !> If HEADER is not present, insert it with an empty value, after any
    !> headers specified in AFTERS.  FOUND tells whether it was present.
    !> Port of message.el:message-position-on-field.
    recursive subroutine position_message_on_field(header, status, afters, found)
        character(len=*), intent(in) :: header
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: afters(:)
        logical, intent(out), optional :: found
        type(restriction_type) :: restriction
        logical :: saved_fold, present_header
        integer(int64) :: e, moved
        integer :: k

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        present_header = .false.
        call save_restriction_begin(restriction)
        call goto_character(point_minimum())

        body: block
            if (.not. search_regular_expression("^"//regular_expression_quote(mail_header_separator)//"$", status)) then
                if (status%ok()) call set_error(status, GENERIC_ERROR_CODE, "Search failed: """// &
                                                "^"//regular_expression_quote(mail_header_separator)//"$""")
                exit body
            end if

            e = match_beginning(0, status)
            call narrow_to_region(point_minimum(), e, status)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())

            if (search_regular_expression("^"//regular_expression_quote(header)//":", status)) then
                present_header = .true.
                if (.not. search_regular_expression("^[^ "//TAB//"]", status, move=.true.)) continue
                call beginning_of_line()
                moved = skip_characters_backward(LF, status)
            else
                if (.not. status%ok()) exit body
                k = 0

                if (present(afters)) then
                    do k = 1, size(afters)
                        if (search_regular_expression("^"//regular_expression_quote(trim(afters(k)))//":", status)) exit
                        if (.not. status%ok()) exit body
                    end do

                    if (k > size(afters)) k = 0
                end if

                if (k > 0) then
                    if (.not. search_regular_expression("^[^ "//TAB//"]", status, move=.true.)) continue
                    call beginning_of_line()
                end if

                call insert(header//": "//LF, status)
                if (.not. status%ok()) exit body
                call goto_character(point() - 1)
            end if
        end block body

        call save_restriction_end(restriction)
        case_fold_search%value = saved_fold
        if (present(found)) found = present_header
    end subroutine position_message_on_field

    !> All recipients in the message, looking at To, Cc and Bcc, in
    !> RECIPIENTS, each in the form `extract_mail_address_components'
    !> gives.  Port of message.el:message-all-recipients.
    recursive subroutine get_message_all_recipients(recipients, status)
        type(mail_address_components_type), allocatable, intent(out) :: recipients(:)
        type(status_type), intent(out) :: status
        character(len=*), parameter :: HEADERS(3) = [character(len=3) :: "To", "Cc", "Bcc"]
        type(mail_address_components_type), allocatable :: part(:)
        character(len=:), allocatable :: value
        integer :: k

        allocate (recipients(0))

        do k = 1, size(HEADERS)
            call get_message_field(trim(HEADERS(k)), value, status)
            if (.not. status%ok()) return

            if (allocated(value)) then
                call extract_mail_address_components(value, part, status, all=.true.)
                if (.not. status%ok()) return
                recipients = [recipients, part]
            end if
        end do
    end subroutine get_message_all_recipients

    !> Go to the beginning of the next header; FOUND is false at the end.
    !> Port of message.el:message-next-header.
    recursive subroutine next_message_header(found, status)
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        call beginning_of_line()

        if (.not. is_end_of_buffer()) then
            call forward_character(status)
            if (.not. status%ok()) return
        end if

        if (search_regular_expression("^[^ "//TAB//"]", status)) then
            call beginning_of_line()
            found = .true.
        else
            call goto_character(point_maximum())
            found = .false.
        end if
    end subroutine next_message_header

    !> Sort headers of the current message in the order of the header
    !> formats (From, Newsgroups, To, Cc,
    !> Subject, In-Reply-To, Fcc, Bcc, Date, Organization, Distribution,
    !> Lines, Expires, Message-ID, References, User-Agent, then the
    !> others).  Port of message.el:message-sort-headers (and
    !> message-sort-headers-1: the records sorted stably by their rank as
    !> sort-subr does; the rank is computed again each time rather than
    !> kept in a text property).
    recursive subroutine sort_message_headers(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(string_type), allocatable :: records(:)
        character(len=:), allocatable :: text, name
        integer, allocatable :: ranks(:), order(:)
        integer(int64), allocatable :: starts(:), ends(:)
        integer(int64) :: e
        integer :: n, k, j, key
        logical :: found

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)

        body: block
            call narrow_to_message_headers(status)
            if (.not. status%ok()) exit body
            ! The records: each header with its continuation lines, without
            ! the newline that ends it (the text between records stays).
            allocate (starts(0), ends(0), ranks(0), records(0))
            call goto_character(point_minimum())

            do while (.not. is_end_of_buffer())
                starts = [starts, point()]
                ! The rank: the header's place in the format order.
                key = 10000

                if (looking_at("[^ "//LF//"]+:", status)) then
                    name = match_string(0)
                    name = name(:len(name) - 1)
                    key = size(HEADER_FORMAT_ORDER) + 1
                    j = findloc(HEADER_FORMAT_ORDER == name, .true., dim=1)
                    if (j > 0) key = j
                end if

                ranks = [ranks, key]
                call next_message_header(found, status)
                if (.not. status%ok()) exit body
                e = point()
                if (.not. is_beginning_of_buffer()) e = e - 1
                ends = [ends, e]
                call buffer_substring(starts(size(starts)), e, text, status)
                if (.not. status%ok()) exit body
                call append_string(records, text)
                if (.not. found) exit
            end do

            n = size(starts)
            if (n < 2) exit body
            allocate (order(n))
            order = [(k, k=1, n)]

            ! A stable insertion sort by rank.
            do k = 2, n
                key = order(k)
                j = k - 1

                do while (j >= 1)
                    if (ranks(order(j)) <= ranks(key)) exit
                    order(j + 1) = order(j)
                    j = j - 1
                end do

                order(j + 1) = key
            end do

            ! Replace the records from the last, so the earlier positions hold.
            do k = n, 1, -1
                if (order(k) == k) cycle
                call delete_region(starts(k), ends(k), status)
                if (.not. status%ok()) exit body
                call goto_character(starts(k))
                call insert(records(order(k))%text, status)
                if (.not. status%ok()) exit body
            end do
        end block body

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine sort_message_headers

    ! ------------------------------------------------------------------
    ! Making header values
    ! ------------------------------------------------------------------

    !> Make a valid date header, of the time NOW (by default the current
    !> time), or an error condition in STATUS.  Port of
    !> message.el:message-make-date.
    function make_message_date(status, now) result(date)
        type(status_type), intent(out) :: status
        type(time_type), intent(in), optional :: now
        character(len=:), allocatable :: date

        date = format_date(MAIL_DATE, now, status=status)
    end function make_message_date

    !> Make a unique Message-ID in ID.  Port of message.el:message-make-message-id.
    recursive subroutine make_message_id(id, status)
        character(len=:), allocatable, intent(out) :: id
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: unique, psubject, psupersedes, fqdn
        type(message_reply_headers_type) :: reply
        type(excursion_type) :: excursion
        logical :: has_reply, mark

        call make_message_unique_id(unique)
        id = "<"//unique
        call save_excursion_begin(excursion)
        call get_message_field("subject", psubject, status)
        if (status%ok()) call get_message_field("supersedes", psupersedes, status)
        call save_excursion_end(excursion)
        if (.not. status%ok()) return
        call current_reply_headers(reply, has_reply)
        mark = .false.

        if (has_reply .and. allocated(psubject)) then
            if (allocated(reply%references) .and. allocated(reply%subject)) then
                mark = strip_message_subject_re_prefix(reply%subject) /= strip_message_subject_re_prefix(psubject)
            end if
        end if

        if (.not. mark .and. allocated(psupersedes)) mark = index(psupersedes, "_-_@") > 0
        if (mark) id = id//"_-_"
        call make_message_fully_qualified_domain_name(fqdn, status)
        if (.not. status%ok()) return
        id = id//"@"//fqdn//">"
    end subroutine make_message_id

    !> The unique part of a Message-ID: the user id, the time and a counter
    !> in base 36, and ".fsf".  Port of message.el:message-unique-id.
    subroutine make_message_unique_id(id)
        use utility_functions_mod, only: random
        use file_input_output_mod, only: user_uid
        character(len=:), allocatable, intent(out) :: id
        integer(int64), parameter :: BASE = 25*25
        integer(int64) :: tm
        type(time_type) :: now

        ! Don't use fractional seconds from timestamp; they may be unsupported.
        ! Instead we use this randomly inited counter.
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

    !> The pertinent part of `user_mail_address' in ADDRESS (unallocated
    !> when it has none).  Port of message.el:message-user-mail-address.
    recursive subroutine get_message_user_mail_address(address, status)
        character(len=:), allocatable, intent(out) :: address
        type(status_type), intent(out) :: status
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: user_mail

        status = status_type()
        user_mail = user_mail_address_value()
        if (.not. allocated(user_mail)) return
        if (.not. matches("@.*\.", user_mail)) return

        if (index(user_mail, " ") > 0) then
            call extract_mail_address_components(user_mail, components, status)
            if (.not. status%ok()) return

            if (size(components) > 0) then
                if (allocated(components(1)%address)) address = components(1)%address
            end if
        else
            address = user_mail
        end if
    end subroutine get_message_user_mail_address

    !> The user's fully qualified domain name in FQDN.  Port of
    !> message.el:message-make-fqdn.
    recursive subroutine make_message_fully_qualified_domain_name(fqdn, status)
        character(len=:), allocatable, intent(out) :: fqdn
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: sysname, user_mail, user_domain
        logical :: saved_fold
        integer :: at

        sysname = system_name()
        call get_message_user_mail_address(user_mail, status)
        if (.not. status%ok()) return
        user_domain = ""

        if (allocated(user_mail)) then
            at = index(user_mail, "@")
            if (at > 0) user_domain = user_mail(at + 1:)
        end if

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        fqdn = ""

        if (allocated(message_user_fully_qualified_domain_name)) then
            ! `message_user_fully_qualified_domain_name' seems to be valid
            if (.not. matches(BOGUS_SYSTEM_NAMES, message_user_fully_qualified_domain_name)) then
                fqdn = message_user_fully_qualified_domain_name
            end if
        end if

        if (len(fqdn) == 0) then
            ! A system name without any dots is unlikely to be a good fully
            ! qualified domain name.
            if (index(sysname, ".") > 0 .and. .not. matches(BOGUS_SYSTEM_NAMES, sysname)) then
                ! `system_name' returned the right result.
                fqdn = sysname
            end if
        end if

        if (len(fqdn) == 0 .and. allocated(mail_host_address)) then
            ! Try `mail_host_address'.
            if (.not. matches(BOGUS_SYSTEM_NAMES, mail_host_address)) fqdn = mail_host_address
        end if

        if (len(fqdn) == 0 .and. len(user_domain) > 0) then
            ! We try `user_mail_address' as a backup.
            if (.not. matches(BOGUS_SYSTEM_NAMES, user_domain)) fqdn = user_domain
        end if

        ! Default to this bogus thing.
        if (len(fqdn) == 0) fqdn = sysname//".mail-host-address-is-not-set"
        case_fold_search%value = saved_fold
    end subroutine make_message_fully_qualified_domain_name

    !> The domain name in DOMAIN.  Port of message.el:message-make-domain.
    recursive subroutine make_message_domain(domain, status)
        character(len=:), allocatable, intent(out) :: domain
        type(status_type), intent(out) :: status

        status = status_type()

        if (allocated(mail_host_address)) then
            domain = mail_host_address
        else
            call make_message_fully_qualified_domain_name(domain, status)
        end if
    end subroutine make_message_domain

    !> Make the address of the user in ADDRESS.  Port of
    !> message.el:message-make-address.
    recursive subroutine make_message_address(address, status)
        character(len=:), allocatable, intent(out) :: address
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: domain

        call get_message_user_mail_address(address, status)
        if (.not. status%ok()) return
        if (allocated(address)) return
        call make_message_domain(domain, status)
        if (status%ok()) address = user_login_name()//"@"//domain
    end subroutine make_message_address

    !> The "real" user address.
    !> This function tries to ignore all user modifications, and
    !> give as trustworthy answer as possible.  Port of
    !> message.el:message-make-sender.
    function make_message_sender() result(sender)
        character(len=:), allocatable :: sender

        sender = user_login_name()//"@"//system_name()
    end function make_message_sender

    !> Make a From header in FROM, of NAME and ADDRESS (by default the
    !> user's).  Port of message.el:message-make-from.
    recursive subroutine make_message_from(from, status, name, address)
        character(len=:), allocatable, intent(out) :: from
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name, address
        character(len=*), parameter :: SPECIALS = "[^- !#-'*+/-9=?A-Z^-~]"
        character(len=:), allocatable :: login, fullname, temporary, quoted
        integer(int64) :: b, e
        integer :: k
        logical :: angles

        status = status_type()

        if (present(address)) then
            login = address
        else
            call make_message_address(login, status)
            if (.not. status%ok()) return
        end if

        if (present(name)) then
            fullname = name
        else
            fullname = user_full_name()
        end if

        if (fullname == "&") fullname = user_login_name()

        if (message_from_style == MESSAGE_FROM_STYLE_SIMPLE .or. len(fullname) == 0) then
            from = login
            return
        end if

        angles = message_from_style == MESSAGE_FROM_STYLE_ANGLES

        if (.not. angles .and. message_from_style /= MESSAGE_FROM_STYLE_PARENTHESES) then
            ! Use angles if no quoting is needed, or if parens would
            ! need quoting too.
            angles = .not. matches(SPECIALS, fullname)

            if (.not. angles) then
                temporary = fullname

                do while (string_match("([^()]*)", temporary, status) > 0)
                    b = match_beginning(0, status)
                    e = match_end(0, status)
                    temporary(b:b) = "-"
                    temporary(e - 1:e - 1) = "-"
                end do

                angles = matches("[\()]", temporary)
            end if
        end if

        if (angles) then
            ! Look for a character that cannot appear unquoted
            ! according to RFC 822 (or later).
            if (matches(SPECIALS, fullname)) then
                ! Quote fullname, escaping specials.
                quoted = """"

                do k = 1, len(fullname)
                    if (fullname(k:k) == """" .or. fullname(k:k) == "\") quoted = quoted//"\"
                    quoted = quoted//fullname(k:k)
                end do

                fullname = quoted//""""
            end if

            from = fullname//" <"//login//">"
        else
            ! \ and nonmatching parentheses must be escaped in comments.
            call escape_comment(fullname, quoted, status)
            if (.not. status%ok()) return
            from = login//" ("//quoted//")"
        end if

    contains

        ! FULLNAME with every ( ) \ escaped, then the escaping of matching
        ! parentheses undone, nested ones included.  Not a port.
        recursive subroutine escape_comment(fullname, out, status)
            character(len=*), intent(in) :: fullname
            character(len=:), allocatable, intent(out) :: out
            type(status_type), intent(out) :: status
            type(handle_type) :: old, buffer

            call temporary_buffer_begin(" *temp*", old, buffer, status)

            if (status%ok()) then
                call insert(fullname, status)
                call goto_character(point_minimum())

                ! Escape every instance of ()\ ...
                do while (search_regular_expression("[()\]", status, move=.true.))
                    call replace_match("\\\&", status, fixedcase=.true.)
                    if (.not. status%ok()) exit
                end do

                ! ... then undo escaping of matching parentheses,
                ! including matching nested parentheses.
                call goto_character(point_minimum())

                do while (status%ok())
                    if (.not. search_regular_expression("\(\=\|[^\]\(\\\\\)*\)\\(\(\([^\]\|\\\\\)*\)\\)", status, move=.true.)) exit
                    call replace_match("\1(\3)", status, fixedcase=.true.)
                    call goto_character(point_minimum())
                end do

                if (status%ok()) out = buffer_string()
            end if

            call temporary_buffer_end(old, buffer)
        end subroutine escape_comment
    end subroutine make_message_from

    !> Make an Organization header in ORGANIZATION (unallocated for none).
    !> Port of message.el:message-make-organization.
    recursive subroutine make_message_organization(organization, status)
        character(len=:), allocatable, intent(out) :: organization
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        type(handle_type) :: old, buffer
        integer(int64) :: inserted
        logical :: from_file

        status = status_type()
        from_file = .false.

        if (associated(message_user_organization%procedure)) then
            call message_user_organization%procedure(text, status)
            if (.not. status%ok()) return
        else if (allocated(message_user_organization%text)) then
            text = message_user_organization%text
        else if (message_user_organization%value) then
            from_file = .true.
        end if

        call temporary_buffer_begin(" *temp*", old, buffer, status)

        if (status%ok()) then
            if (allocated(text)) then
                call insert(text, status)
            else if (from_file .and. allocated(message_user_organization_file)) then
                if (file_exists(message_user_organization_file)) then
                    call insert_file_contents(message_user_organization_file, .false., inserted, status)
                end if
            end if

            call goto_character(point_minimum())

            do while (status%ok())
                if (.not. search_regular_expression("["//TAB//LF//"]+", status)) exit
                call replace_match("", status, fixedcase=.true., literal=.true.)
            end do

            if (status%ok() .and. buffer_size() /= 0) organization = buffer_string()
        end if

        call temporary_buffer_end(old, buffer)
    end subroutine make_message_organization

    !> Count the number of lines of the body and return the numeric string.
    !> Port of message.el:message-make-lines.
    recursive function make_message_lines() result(lines)
        character(len=:), allocatable :: lines
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=24) :: number
        integer(int64) :: body

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call widen()
        call goto_message_body(body)
        write (number, "(i0)") count_lines(point(), point_maximum())
        lines = trim(number)
        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end function make_message_lines

    !> The References header for this message in REFERENCES (unallocated
    !> for none).  Port of message.el:message-make-references.
    subroutine make_message_references(references)
        character(len=:), allocatable, intent(out) :: references
        type(message_reply_headers_type) :: reply
        logical :: found

        call current_reply_headers(reply, found)
        if (.not. found) return
        if (.not. (allocated(reply%references) .or. allocated(reply%id))) return
        references = ""
        if (allocated(reply%references)) references = reply%references//" "
        if (allocated(reply%id)) references = references//reply%id
    end subroutine make_message_references

    !> The In-Reply-To header for this message in IN_REPLY_TO
    !> (unallocated for none).  Port of message.el:message-make-in-reply-to.
    recursive subroutine make_message_in_reply_to(in_reply_to, status)
        character(len=:), allocatable, intent(out) :: in_reply_to
        type(status_type), intent(out) :: status
        type(message_reply_headers_type) :: reply
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: name, quoted
        logical :: found
        integer :: k, backslashes

        status = status_type()
        call current_reply_headers(reply, found)
        if (.not. found) return
        if (.not. allocated(reply%from)) return
        call extract_mail_address_components(reply%from, components, status)
        if (.not. status%ok()) return
        in_reply_to = ""
        if (allocated(reply%id)) in_reply_to = reply%id
        if (.not. message_header_use_obsolete_in_reply_to) return
        if (allocated(reply%id)) in_reply_to = in_reply_to//" ("
        name = ""

        if (size(components) > 0) then
            if (allocated(components(1)%full_name)) then
                name = components(1)%full_name

                if (any(iachar([(name(k:k), k=1, len(name))]) > 127)) then
                    ! Quote a string containing non-ASCII characters.
                    quoted = """"
                    backslashes = 0

                    do k = 1, len(name)
                        if (name(k:k) == """" .and. mod(backslashes, 2) == 0) quoted = quoted//"\"
                        quoted = quoted//name(k:k)

                        if (name(k:k) == "\") then
                            backslashes = backslashes + 1
                        else
                            backslashes = 0
                        end if
                    end do

                    name = quoted//""""
                end if
            else if (allocated(components(1)%address)) then
                name = components(1)%address
            end if
        end if

        in_reply_to = in_reply_to//name//"'s message of """

        if (.not. allocated(reply%date)) then
            in_reply_to = in_reply_to//"(unknown date)"
        else if (len(reply%date) == 0) then
            in_reply_to = in_reply_to//"(unknown date)"
        else
            in_reply_to = in_reply_to//reply%date
        end if

        in_reply_to = in_reply_to//""""
        if (allocated(reply%id)) in_reply_to = in_reply_to//")"
    end subroutine make_message_in_reply_to

    !> Return an Expires header based on `message_expires', or an error
    !> condition in STATUS.  Port of message.el:message-make-expires.
    function make_message_expires(status) result(expires)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expires
        type(time_type) :: future

        future = current_time()
        future%seconds = future%seconds + 60_int64*60*24*message_expires
        expires = make_message_date(status, future)
    end function make_message_expires

    !> Make date string for the Expires header.  Expiry in DAYS days; a
    !> check of STATUS finds an error condition.  Port of
    !> message.el:message-make-expires-date.
    function make_message_expires_date(days, status) result(expires)
        integer(int64), intent(in) :: days
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expires
        type(time_type) :: future

        ! decode-time, a later day, encode-time: the same clock time DAYS
        ! days later in the local time zone.
        future = current_time()
        future%seconds = future%seconds + 60_int64*60*24*days
        expires = make_message_date(status, future)
    end function make_message_expires_date

    !> Return uucp path.  Port of message.el:message-make-path.
    function make_message_path() result(path)
        character(len=:), allocatable :: path

        if (allocated(message_user_path)) then
            ! Support GENERICPATH.  Suggested by vixie@decwrl.dec.com.
            path = message_user_path//"!"//user_login_name()
        else
            path = system_name()//"!"//user_login_name()
        end if
    end function make_message_path

    !> Remove "Re:" from subject lines in string SUBJECT.
    !> Matching is done case-insensitively.  Port of
    !> message.el:message-strip-subject-re.
    function strip_message_subject_re_prefix(subject) result(stripped)
        character(len=*), intent(in) :: subject
        character(len=:), allocatable :: stripped
        logical :: saved_fold
        type(status_type) :: st
        integer(int64) :: e

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        stripped = subject

        if (string_match(message_subject_re_regular_expression, subject, st) > 0) then
            e = match_end(0, st)
            stripped = subject(string_offset(subject, e) + 1:)
        end if

        case_fold_search%value = saved_fold
    end function strip_message_subject_re_prefix

    ! The byte offset of the 1-based character index INDEX of STRING (the
    ! bytes before it).  Not a port.
    integer function string_offset(string, index) result(offset)
        use search_mod, only: string_index_to_offset
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: index

        offset = int(string_index_to_offset(string, index))
    end function string_offset

    !> Remove trailing "(was: <old subject>)" from SUBJECT lines.
    !> Leading "Re: " is not stripped by this function.  Use the function
    !> `strip_message_subject_re_prefix' for this.  Port of
    !> message.el:message-strip-subject-trailing-was (the query of
    !> `ask' is Deferred).
    function strip_message_subject_trailing_was(subject) result(stripped)
        character(len=*), intent(in) :: subject
        character(len=:), allocatable :: stripped
        type(status_type) :: st
        integer(int64) :: b

        stripped = subject
        if (.not. message_subject_trailing_was_query) return

        if (string_match(message_subject_trailing_was_regular_expression, subject, st) > 0) then
            b = match_beginning(0, st)
            stripped = subject(:string_offset(subject, b))
        end if
    end function strip_message_subject_trailing_was

    ! ------------------------------------------------------------------
    ! Generating headers
    ! ------------------------------------------------------------------

    ! Whether NAME is in the current buffer's message-inserted-headers.
    ! Not a port.
    logical function is_header_inserted(name)
        character(len=*), intent(in) :: name
        integer :: k

        is_header_inserted = .false.
        k = local_index()
        if (k == 0) return
        if (.not. allocated(locals(k)%inserted_headers)) return
        is_header_inserted = index(locals(k)%inserted_headers, LF//name//LF) > 0
    end function is_header_inserted

    ! Add NAME to the current buffer's message-inserted-headers.  Not a port.
    subroutine push_inserted_header(name)
        character(len=*), intent(in) :: name
        integer :: k

        call make_local_entry(k)
        if (k == 0) return
        if (.not. allocated(locals(k)%inserted_headers)) locals(k)%inserted_headers = LF
        locals(k)%inserted_headers = LF//name//locals(k)%inserted_headers
    end subroutine push_inserted_header

    ! The value Message makes for the header NAME (upstream's
    ! header-values plist), in VALUE; KNOWN is false for a header with no
    ! entry there.  Not a port.
    recursive subroutine header_value(header, value, known, status)
        character(len=*), intent(in) :: header
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: known
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        status = status_type()
        known = .true.

        select case (header)
        case ("Date")
            value = make_message_date(status)
        case ("Message-ID")
            call make_message_id(value, status)
        case ("Organization")
            call make_message_organization(value, status)
        case ("From")
            call make_message_from(value, status)
        case ("Path")
            value = make_message_path()
        case ("Subject", "Newsgroups", "To", "User-Agent")
            continue
        case ("In-Reply-To")
            call make_message_in_reply_to(value, status)
        case ("References")
            call make_message_references(value)
        case ("Distribution")
            call get_reply_field("distribution", text, status)
            if (allocated(text)) value = text
        case ("Lines")
            value = make_message_lines()
        case ("Expires")
            value = make_message_expires(status)
        case default
            known = .false.
        end select
    end subroutine header_value

    ! The field HEADER of the message being replied to (message-fetch-reply-field).
    ! Port of message.el:message-fetch-reply-field.
    recursive subroutine get_reply_field(header, value, status)
        character(len=*), intent(in) :: header
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        type(handle_type) :: old, reply
        type(restriction_type) :: restriction
        integer :: k
        integer(int64) :: e

        status = status_type()
        k = local_index()
        if (k == 0) return
        reply = locals(k)%reply_buffer
        if (.not. is_buffer_live(reply)) return
        old = current_buffer()
        call set_buffer(reply, status)
        if (.not. status%ok()) return
        call save_restriction_begin(restriction)
        ! mail-narrow-to-head
        call widen()
        call goto_character(point_minimum())

        if (search_string(LF//LF, status)) then
            e = point() - 1
        else
            e = point_maximum()
        end if

        call narrow_to_region(point_minimum(), e, status)
        call goto_character(point_minimum())
        if (status%ok()) call get_message_field(header, value, status)
        call save_restriction_end(restriction)
        call set_buffer(old, status)
    end subroutine get_reply_field

    !> Prepare article HEADERS (followed by `message_required_headers').
    !> Headers already prepared in the buffer are not modified.  Port of
    !> message.el:message-generate-headers (encoding non-ASCII domain names
    !> with IDNA is Deferred: a non-ASCII domain is a `void_function'
    !> error condition).
    recursive subroutine generate_message_headers(headers_in, status)
        type(message_header_type), intent(in) :: headers_in(:)
        type(status_type), intent(out) :: status
        type(message_header_type), allocatable :: headers(:)
        type(restriction_type) :: restriction
        character(len=:), allocatable :: header, value, from, sender, secure_sender, from_address, sender_address
        type(mail_address_components_type), allocatable :: components(:)
        logical :: saved_fold, optionalp, found, known, empty, modified, deletable
        integer(int64) :: original_point, e, moved
        integer :: k, j

        headers = headers_in
        if (allocated(message_required_headers)) headers = [headers, message_required_headers]
        saved_fold = case_fold_search%value
        call save_restriction_begin(restriction)

        body: block
            call narrow_to_message_headers(status)
            if (.not. status%ok()) exit body
            case_fold_search%value = .true.
            optionalp = .false.
            ! First we remove any old generated headers.
            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) exit body

            if (allocated(message_deletable_headers)) then
                do k = 1, size(message_deletable_headers)
                    if (.not. modified .and. trim(message_deletable_headers(k)) == "Message-ID") cycle
                    call goto_character(point_minimum())
                    ! Only headers Message generated itself are deleted
                    ! (upstream's message-deletable text property).
                    if (.not. search_regular_expression("^"//trim(message_deletable_headers(k))//": *", status)) cycle
                    if (.not. is_generated()) cycle
                    call delete_region(line_beginning_position(), line_beginning_position(2_int64), status)
                    if (.not. status%ok()) exit body
                end do
            end if

            if (.not. status%ok()) exit body

            ! Go through all the required headers and see if they are in the
            ! articles already.  If they are not, or are empty, they are
            ! inserted automatically - except for Subject, Newsgroups and
            ! Distribution.
            do k = 1, size(headers)
                call goto_character(point_minimum())
                header = trim(headers(k)%name)
                if (headers(k)%optional) optionalp = .true.
                found = search_regular_expression("^"//regular_expression_quote(downcase_string(header))//":", status)
                if (.not. status%ok()) exit body
                empty = .false.

                if (found) then
                    ! The header was found.  We insert a space after the
                    ! colon, if there is none.
                    if (following_character() /= iachar(" ")) then
                        call insert(" ", status)
                        if (.not. status%ok()) exit body
                    else
                        call forward_character(status)
                    end if

                    ! Find out whether the header is empty.
                    empty = looking_at("[ "//TAB//"]*"//LF//"[^ "//TAB//"]", status)
                end if

                if (found .and. .not. empty) cycle
                ! So we find out what value we should insert.
                if (allocated(value)) deallocate (value)

                if (headers(k)%optional .and. .not. is_header_inserted(header)) then
                    ! This is an optional header.  If the value is
                    ! none, then we do not insert this header.
                    if (associated(headers(k)%procedure)) then
                        call headers(k)%procedure(value, status)
                    else
                        call header_value(header, value, known, status)
                    end if
                else if (headers(k)%optional) then
                    continue
                else if (associated(headers(k)%procedure)) then
                    ! The procedure gives the value to insert.
                    call headers(k)%procedure(value, status)
                else if (len_trim(headers(k)%value) > 0) then
                    ! The value is inserted verbatim.
                    value = trim(headers(k)%value)
                else
                    call header_value(header, value, known, status)

                    if (status%ok() .and. .not. known .and. .not. is_header_check_disabled(downcase_string(header))) then
                        ! We couldn't generate a value for this header,
                        ! so we just ask the user.
                        call read_from_minibuffer("Empty header for "//header//"; enter value: ", value, status)
                    end if
                end if

                if (.not. status%ok()) exit body
                if (.not. allocated(value)) cycle
                if (len(value) == 0) cycle
                ! Finally insert the header.
                original_point = point()

                if (is_beginning_of_line()) then
                    ! This header didn't exist, so we insert it.
                    call goto_character(point_maximum())

                    if (header == "References") then
                        call insert_shortened_references(header, value, status)
                    else
                        call insert(header//": "//value, status)
                    end if

                    if (.not. status%ok()) exit body
                    call push_inserted_header(header)
                    call fill_message_field(e, status)
                    if (.not. status%ok()) exit body
                    call goto_character(e)
                    ! We check whether the value was ended by a
                    ! newline.  If not, we insert one.
                    if (.not. is_beginning_of_line()) call insert(LF, status)
                    if (.not. status%ok()) exit body
                    moved = forward_line(-1_int64)
                else
                    ! The value of this header was empty, so we clear
                    ! totally and insert the new value.
                    call delete_region(point(), line_end_position(), status)
                    if (.not. status%ok()) exit body

                    ! If the header is optional, and the header was
                    ! empty, we can't insert it anyway.
                    if (.not. optionalp) then
                        call push_inserted_header(header)
                        call insert(value, status)
                        if (.not. status%ok()) exit body
                        call fill_message_field(e, status)
                        if (.not. status%ok()) exit body
                    end if
                end if

                ! Add the deletable property to the headers that require it.
                deletable = .false.

                if (allocated(message_deletable_headers)) then
                    do j = 1, size(message_deletable_headers)
                        if (trim(message_deletable_headers(j)) == header) deletable = .true.
                    end do
                end if

                if (deletable) then
                    call beginning_of_line()
                    if (looking_at("[^:]+: ", status)) call mark_generated()
                end if

                call goto_character(original_point)
            end do

            ! Insert new Sender if the From is strange.
            call get_message_field("from", from, status)
            if (status%ok()) call get_message_field("sender", sender, status)
            if (.not. status%ok()) exit body
            secure_sender = make_message_sender()

            if (allocated(from) .and. message_syntax_checks%sender) then
                call extract_mail_address_components(from, components, status)
                if (.not. status%ok()) exit body
                from_address = ""

                if (size(components) > 0) then
                    if (allocated(components(1)%address)) from_address = components(1)%address
                end if

                found = downcase_string(from_address) /= downcase_string(secure_sender)

                if (found .and. allocated(sender)) then
                    call extract_mail_address_components(sender, components, status)
                    if (.not. status%ok()) exit body
                    sender_address = ""

                    if (size(components) > 0) then
                        if (allocated(components(1)%address)) sender_address = components(1)%address
                    end if

                    found = downcase_string(sender_address) /= downcase_string(secure_sender)
                end if

                if (found) then
                    call goto_character(point_minimum())

                    ! Rename any old Sender headers to Original-Sender.
                    if (search_regular_expression("^\(Original-\)*Sender:", status)) then
                        call beginning_of_line()
                        call insert("Original-", status)
                        call beginning_of_line()
                    end if

                    if (status%ok() .and. matches("@.+\..", secure_sender)) call insert("Sender: "//secure_sender//LF, status)
                    if (.not. status%ok()) exit body
                end if
            end if

            ! Check for IDNA
            call check_idna(status)
        end block body

        call save_restriction_end(restriction)
        case_fold_search%value = saved_fold
    end subroutine generate_message_headers

    ! Whether the header line at point, found by the search just done,
    ! was generated by Message (its message_deletable text property).
    ! Port of the test in message.el:message-generate-headers.
    logical function is_generated()
        use text_properties_mod, only: get_text_property
        type(value_type) :: value
        type(status_type) :: st

        call get_text_property(match_beginning(0, st) + 1, value_type(name=P_MESSAGE_DELETABLE), value, st)
        is_generated = allocated(value%a)
    end function is_generated

    ! Give the header name at point and the space after it, as matched by
    ! the search just done, the message_deletable property and the face
    ! italic.  Port of the add-text-properties in
    ! message.el:message-generate-headers.
    subroutine mark_generated()
        use text_properties_mod, only: put_text_property
        type(status_type) :: st
        integer(int64) :: e

        e = match_end(0, st)
        call put_text_property(point(), e, value_type(name=P_MESSAGE_DELETABLE), value_type(.true.), st)
        call put_text_property(point(), e, value_type(name=P_FACE), value_type(name=FACE_ITALIC), st)
    end subroutine mark_generated

    !> Narrow the buffer to the header on the current line.  Port of
    !> message.el:message-narrow-to-field.
    recursive subroutine narrow_to_message_field(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, e, moved

        call beginning_of_line()

        do while (looking_at("[ "//TAB//"]", status))
            moved = forward_line(-1_int64)
            if (moved /= 0) exit
        end do

        if (.not. status%ok()) return
        start = point()
        moved = forward_line(1_int64)

        if (search_regular_expression("^[^ "//LF//TAB//"]", status)) then
            e = line_beginning_position()
        else
            e = point_maximum()
        end if

        if (.not. status%ok()) return
        call narrow_to_region(start, e, status)
        call goto_character(point_minimum())
    end subroutine narrow_to_message_field

    !> Fill the header field at point (addresses for To, Cc and From); E is
    !> where it ends afterwards.  Port of message.el:message-fill-field.
    recursive subroutine fill_message_field(e, status)
        integer(int64), intent(out) :: e
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: field_name

        e = point()
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_field(status)

        if (status%ok()) then
            ! message-field-name
            field_name = ""
            call goto_character(point_minimum())
            if (looking_at("\([^:]+\):", status)) field_name = capitalize_string(match_string(1))

            select case (field_name)
            case ("To", "Cc", "From")
                call fill_message_field_address(status)
            case default
                call fill_message_field_general(status)
            end select

            e = point_maximum()
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine fill_message_field

    ! Move past the address at point and the comma after it.  Port of
    ! message.el:message-skip-to-next-address.
    recursive subroutine skip_to_next_message_address(status)
        type(status_type), intent(out) :: status
        integer(int64) :: e, moved
        type(excursion_type) :: excursion
        logical :: quoted, found
        integer(int32) :: char

        call save_excursion_begin(excursion)
        call next_message_header(found, status)
        e = point()
        call save_excursion_end(excursion)
        if (.not. status%ok()) return
        quoted = .false.
        char = 0

        if (looking_at(",", status)) then
            call forward_character(status)
            if (.not. status%ok()) return
        end if

        do while (point() /= e .and. (char /= iachar(",") .or. quoted))
            moved = skip_characters_forward("^,""", status, e)
            char = following_character()
            if (char == iachar("""")) quoted = .not. quoted
            if (point() /= e) call forward_character(status)
            if (.not. status%ok()) return
        end do

        moved = skip_characters_forward(" "//TAB//LF, status)
    end subroutine skip_to_next_message_address

    ! Fill an address field.  Port of message.el:message-fill-field-address.
    recursive subroutine fill_message_field_address(status)
        type(status_type), intent(out) :: status
        integer(int64) :: last, end_line, moved
        type(handle_type) :: here
        logical :: done

        last = 0
        done = .false.
        end_line = 0

        do while (.not. done)
            call skip_to_next_message_address(status)
            if (.not. status%ok()) return

            if (is_beginning_of_line()) then
                call end_of_line(0_int64)
                end_line = 1
                done = .true.
            else if (is_end_of_buffer()) then
                end_line = 0
                done = .true.
            end if

            if (current_column() > 78 .and. last > 0) then
                here = marker_at(point())
                call goto_character(last)
                moved = skip_characters_backward(" "//TAB, status)
                call delete_region(point(), last, status)
                if (status%ok()) call insert(LF//TAB, status)
                call goto_character(marker_end(here))
                if (.not. status%ok()) return
            end if

            last = point()
        end do

        moved = forward_line(end_line)
    end subroutine fill_message_field_address

    ! Fill a general field.  Port of message.el:message-fill-field-general.
    recursive subroutine fill_message_field_general(status)
        type(status_type), intent(out) :: status
        integer(int64) :: begin, saved_column, moved
        character(len=:), allocatable :: saved_prefix
        logical :: had_prefix

        begin = point()
        saved_column = fill_column%value
        had_prefix = allocated(fill_prefix%value)
        if (had_prefix) saved_prefix = fill_prefix%value
        fill_column%value = 78
        fill_prefix%value = TAB
        status = status_type()

        do while (search_string(LF, status))
            if (is_end_of_buffer()) exit
            call replace_match(" ", status, fixedcase=.true., literal=.true.)
            if (.not. status%ok()) exit
        end do

        if (status%ok()) call fill_region_as_paragraph(begin, point_maximum(), status)

        if (status%ok()) then
            ! Tapdance around looong Message-IDs.
            moved = forward_line(-1_int64)

            if (looking_at("[ "//TAB//"]*$", status)) then
                call delete_region(line_beginning_position(), line_beginning_position(2_int64), status)
            end if
        end if

        if (status%ok()) then
            call goto_character(begin)

            if (search_string(":", status)) then
                if (looking_at(LF//"[ "//TAB//"]+", status)) call replace_match(" ", status, fixedcase=.true., literal=.true.)
            end if

            call goto_character(point_maximum())
        end if

        fill_column%value = saved_column

        if (had_prefix) then
            fill_prefix%value = saved_prefix
        else
            deallocate (fill_prefix%value)
        end if
    end subroutine fill_message_field_general

    ! Insert the References header HEADER with REFERENCES trimmed to 21
    ! Message-IDs or less (the valid ones).  Port of
    ! message.el:message-shorten-references (with message-shorten-1; the
    ! length limit of news is not needed for mail).
    recursive subroutine insert_shortened_references(header, references, status)
        character(len=*), intent(in) :: header, references
        type(status_type), intent(out) :: status
        integer, parameter :: MAXCOUNT = 21, CUT = 2
        type(string_type), allocatable :: shortened_references(:)
        character(len=:), allocatable :: text
        type(handle_type) :: old, buffer
        integer :: count, k

        allocate (shortened_references(0))
        call temporary_buffer_begin(" *temp*", old, buffer, status)

        if (status%ok()) then
            call insert(references, status)
            call goto_character(point_minimum())

            ! Cons a list of valid references.  GNKSA says we must not include
            ! MIDs with whitespace or missing brackets.
            do while (status%ok())
                if (.not. search_regular_expression("<[^ <]+@[^ <]+>", status)) exit
                call append_string(shortened_references, match_string(0))
            end do
        end if

        call temporary_buffer_end(old, buffer)
        if (.not. status%ok()) return
        count = size(shortened_references)
        ! If the list has more than MAXCOUNT elements, trim it by
        ! removing the CUTth element and the required number of
        ! elements that follow.
        if (count > MAXCOUNT) shortened_references = [shortened_references(:CUT - 1), shortened_references(CUT + count - MAXCOUNT:)]
        text = ""

        do k = 1, size(shortened_references)
            if (k > 1) text = text//" "
            text = text//shortened_references(k)%text
        end do

        ! message-insert-header
        call insert(capitalize_string(header)//": "//text, status)
    end subroutine insert_shortened_references

    ! The IDNA check of the headers: a non-ASCII domain name in From, To,
    ! Cc, Reply-To, Mail-Reply-To, Mail-Followup-To or Cc would be encoded
    ! (puny.el), which is not ported: a `void_function' error condition.
    ! Port of message.el:message-idna-to-ascii-rhs (in part: finding
    ! the non-ASCII domain names; `message_use_idna' is Deferred).
    recursive subroutine check_idna(status)
        type(status_type), intent(out) :: status
        character(len=*), parameter :: HEADERS(6) = [character(len=16) :: "From", "To", "Reply-To", "Mail-Reply-To", &
                                                                                                           "Mail-Followup-To", "Cc"]
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: value, address
        integer :: k, j, at, i

        do k = 1, size(HEADERS)
            call get_message_field(trim(HEADERS(k)), value, status)
            if (.not. status%ok()) return
            if (.not. allocated(value)) cycle
            call extract_mail_address_components(value, components, status, all=.true.)
            if (.not. status%ok()) return

            do j = 1, size(components)
                if (.not. allocated(components(j)%address)) cycle
                address = components(j)%address
                at = index(address, "@")
                if (at == 0) cycle

                if (any(iachar([(address(i:i), i=at, len(address))]) > 127)) then
                    call set_error(status, VOID_FUNCTION_CODE, "puny_encode_domain")
                    return
                end if
            end do
        end do
    end subroutine check_idna

    ! Fold too-long header lines.
    ! Each line should be no more than 79 characters long.  Port of
    ! message.el:message--fold-long-headers.
    recursive subroutine fold_long_message_headers(status)
        type(status_type), intent(out) :: status
        integer(int64) :: e, moved

        status = status_type()
        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            if (looking_at("[^:]+:", status) .and. line_end_position() - point() > 79) then
                call fold_rfc2047_field(e, status)
                if (.not. status%ok()) return
                call goto_character(e)
            else
                if (.not. status%ok()) return
                moved = forward_line(1_int64)
            end if
        end do
    end subroutine fold_long_message_headers

    ! Offer to fix continuation lines that are not indented.  Port of
    ! message.el:message--check-continuation-headers.
    recursive subroutine check_message_continuation_headers(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved

        status = status_type()
        if (.not. message_syntax_checks%continuation_headers) return
        call goto_character(point_minimum())

        do while (search_regular_expression("^[^ "//TAB//LF//"][^ "//TAB//LF//":]*[ "//TAB//LF//"]", status))
            call goto_character(match_beginning(0, status))

            if (ask("Fix continuation lines? ", status)) then
                call insert(" ", status)
            else
                if (.not. status%ok()) return
                moved = forward_line(1_int64)

                if (.not. ask("Send anyway? ", status)) then
                    if (status%ok()) call set_error(status, GENERIC_ERROR_CODE, "Failed to send the message")
                    return
                end if
            end if

            if (.not. status%ok()) return
        end do
    end subroutine check_message_continuation_headers

    !> Do various automatic cleanups of the headers: remove empty lines,
    !> and in Newsgroups and Followup-To change sequences of spaces to
    !> commas.  Port of message.el:message-cleanup-headers.
    recursive subroutine cleanup_message_headers(status)
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction, inner
        integer(int64) :: start, e, moved

        call save_restriction_begin(restriction)

        body: block
            call narrow_to_message_headers(status)
            if (.not. status%ok()) exit body

            ! Remove blank lines.
            do while (search_regular_expression("^[ "//TAB//"]*"//LF, status))
                call replace_match("", status, fixedcase=.true., literal=.true.)
                if (.not. status%ok()) exit body
            end do

            if (.not. status%ok()) exit body
            ! Correct Newsgroups and Followup-To headers:  Change sequence of
            ! spaces to comma and eliminate spaces around commas.  Eliminate
            ! embedded line breaks.
            call goto_character(point_minimum())

            do while (search_regular_expression("^\(Newsgroups\|Followup-To\): +", status))
                start = point()

                if (search_regular_expression("^[^ "//TAB//"]", status)) then
                    e = match_beginning(0, status)
                else
                    moved = forward_line(1_int64)
                    e = point()
                end if

                call save_restriction_begin(inner)
                call narrow_to_region(start, e, status)
                call goto_character(point_minimum())

                do while (status%ok())
                    if (.not. search_regular_expression(LF//"[ "//TAB//"]+", status)) exit
                    ! No line breaks (too confusing)
                    call replace_match(" ", status, fixedcase=.true., literal=.true.)
                end do

                call goto_character(point_minimum())

                do while (status%ok())
                    if (.not. search_regular_expression("[ "//TAB//LF//"]*,[ "//TAB//LF//"]*\|[ "//TAB//"]+", status)) exit
                    call replace_match(",", status, fixedcase=.true., literal=.true.)
                end do

                call goto_character(point_minimum())

                ! Remove trailing commas.
                if (status%ok()) then
                    if (search_regular_expression(",+$", status)) call replace_match("", status, fixedcase=.true., literal=.true.)
                end if

                call save_restriction_end(inner)
                if (.not. status%ok()) exit body
            end do
        end block body

        call save_restriction_end(restriction)
    end subroutine cleanup_message_headers

    !> Encode the body of the message as MIME: a body of ASCII text with
    !> no MML tags and no Content-Type field gets the fields
    !> "MIME-Version: 1.0" and "Content-Type: text/plain;
    !> charset=us-ascii".  Port of message.el:message-encode-message-body
    !> (with what mml.el's mml-generate-mime makes of such a body; other
    !> bodies need MML, which is not ported: a `void_function' error
    !> condition).
    recursive subroutine encode_message_body(status)
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction
        character(len=:), allocatable :: content_type
        integer(int64) :: body, moved
        logical :: saved_fold, found

        found = .false.
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call save_restriction_begin(restriction)

        body_block: block
            call narrow_to_message_headers(status)
            if (status%ok()) call get_mail_field("content-type", content_type, status)
            call save_restriction_end(restriction)
            if (.not. status%ok()) exit body_block
            call goto_message_body(body)

            ! mml-generate-mime: a part of another type, MML tags or
            ! non-ASCII text are not ported.
            if (.not. allocated(content_type)) then
                if (search_string("<#", status)) then
                    call set_error(status, VOID_FUNCTION_CODE, "mml_generate_mime")
                    exit body_block
                end if

                ! mm-long-lines-p: a line of more than 1000 characters is
                ! encoded quoted-printable.
                call goto_character(body)

                do while (.not. is_end_of_buffer())
                    if (line_end_position() - point() > 1000) then
                        call set_error(status, VOID_FUNCTION_CODE, "mml_generate_mime")
                        exit body_block
                    end if

                    moved = forward_line(1_int64)
                    if (moved /= 0) exit
                end do

                call goto_character(body)
                moved = skip_characters_forward(achar(0)//"-"//achar(127), status)
            end if

            if (allocated(content_type) .or. .not. is_end_of_buffer()) then
                call set_error(status, VOID_FUNCTION_CODE, "mml_generate_mime")
                exit body_block
            end if

            call save_restriction_begin(restriction)
            call narrow_to_message_headers_or_head(status)
            if (status%ok()) call remove_message_header("Mime-Version", status)

            if (status%ok()) then
                call goto_character(point_maximum())
                call insert("MIME-Version: 1.0"//LF, status)
            end if

            if (status%ok()) found = search_regular_expression_backward("^Content-Type:", status)
            if (status%ok()) call remove_message_first_header("Content-Type", status)
            if (status%ok()) call remove_message_first_header("Content-Transfer-Encoding", status)
            call save_restriction_end(restriction)
            if (.not. status%ok()) exit body_block

            ! We always make sure that the message has a Content-Type
            ! header.  This is because some broken MTAs and MUAs get
            ! awfully confused when confronted with a message with a
            ! MIME-Version header and without a Content-Type header.
            if (.not. found) then
                call goto_character(point_minimum())

                ! For unknown reason, MIME-Version doesn't exist.
                if (search_regular_expression("^MIME-Version:", status)) then
                    moved = forward_line(1_int64)
                    call insert("Content-Type: text/plain; charset=us-ascii"//LF, status)
                end if
            end if
        end block body_block

        case_fold_search%value = saved_fold
    end subroutine encode_message_body

    ! ------------------------------------------------------------------
    ! Moving in the message
    ! ------------------------------------------------------------------

    !> Move point to the beginning of the message body; BODY is point.
    !> With INTERACTIVE, expand an abbreviation before point at the end
    !> of a line and push the mark first.  Port of
    !> message.el:message-goto-body.
    recursive subroutine goto_message_body(body, interactive)
        integer(int64), intent(out) :: body
        logical, intent(in), optional :: interactive
        type(status_type) :: st
        integer(int64) :: moved
        logical :: inter

        inter = .false.
        if (present(interactive)) inter = interactive

        if (inter) then
            if (looking_at("[ "//TAB//"]*"//LF, st)) call expand_abbreviation(st)
            call push_mark()
        end if

        call goto_character(point_minimum())

        if (.not. search_string(LF//mail_header_separator//LF, st)) then
            ! If the message is mangled, find the end of the headers the
            ! hard way.
            ! Skip past all headers and continuation lines.
            do while (looking_at("[^"//TAB//LF//" :]+:\|["//TAB//" ]+[^"//TAB//LF//" ]", st))
                moved = forward_line(1_int64)
                if (moved /= 0) exit
            end do

            ! We're now at the first empty line, so perhaps move past it.
            if (is_end_of_line() .and. .not. is_end_of_buffer()) moved = forward_line(1_int64)
        end if

        body = point()
    end subroutine goto_message_body

    !> Return true if point is in the message body.  Port of
    !> message.el:message-in-body-p.
    recursive logical function is_message_in_body()
        type(excursion_type) :: excursion
        integer(int64) :: original_point, body

        original_point = point()
        call save_excursion_begin(excursion)
        call goto_message_body(body)
        call save_excursion_end(excursion)
        is_message_in_body = original_point >= body
    end function is_message_in_body

    !> Move point to the end of the headers.  Port of
    !> message.el:message-goto-eoh.
    recursive subroutine goto_message_end_of_headers(interactive)
        logical, intent(in), optional :: interactive
        integer(int64) :: body, moved

        call goto_message_body(body, interactive)
        moved = forward_line(-1_int64)
    end subroutine goto_message_end_of_headers

    !> Move point to the beginning of the message signature.
    !> If there is no signature in the article, go to the end and
    !> FOUND is false.  Port of message.el:message-goto-signature.
    recursive subroutine goto_message_signature(found)
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: moved

        call push_mark()
        call goto_character(point_minimum())
        found = search_regular_expression(message_signature_separator, st)

        if (found) then
            moved = forward_line(1_int64)
        else
            call goto_character(point_maximum())
        end if
    end subroutine goto_message_signature

    !> Return true if point is in the header.  Port of
    !> message.el:message-point-in-header-p.
    recursive logical function is_message_point_in_header()
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(status_type) :: st
        integer(int64) :: bound
        logical :: saved_fold

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call widen()
        bound = line_end_position() + 1
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        call goto_character(point_minimum())
        is_message_point_in_header = .not. search_string(LF//mail_header_separator//LF, st, min(bound, point_maximum()))
        case_fold_search%value = saved_fold
        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end function is_message_point_in_header

    ! ------------------------------------------------------------------
    ! The signature
    ! ------------------------------------------------------------------

    !> Insert a signature at the end of the buffer.
    !>
    !> See the documentation for the `message_signature' variable for
    !> more information.
    !>
    !> With INTERACTIVE (as when called interactively) and no
    !> `message_signature', a signature file is inserted only if the
    !> message has no signature yet.  Port of
    !> message.el:message-insert-signature (FORCE t is not ported: no
    !> caller passes it).
    recursive subroutine insert_message_signature(status, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        character(len=:), allocatable :: text, signature_file
        logical :: inter, use_file, insert_it
        type(excursion_type) :: excursion
        integer(int64) :: inserted

        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        use_file = .false.

        if (message_signature%is_off()) then
            if (inter) then
                call save_excursion_begin(excursion)
                call goto_character(point_maximum())
                use_file = .not. search_regular_expression_backward(message_signature_separator, status)
                call save_excursion_end(excursion)
                if (.not. status%ok()) return
            end if
        else if (associated(message_signature%procedure)) then
            call message_signature%procedure(text, status)
            if (.not. status%ok()) return
        else if (allocated(message_signature%text)) then
            text = message_signature%text
        else
            use_file = message_signature%value
        end if

        insert_it = allocated(text)

        if (.not. insert_it .and. use_file .and. allocated(message_signature_file)) then
            ! Don't actually use the signature directory if
            ! `message_signature_file' contains a path.
            signature_file = message_signature_file

            if (allocated(message_signature_directory)) then
                if (len(file_name_directory(message_signature_file)) == 0) then
                    call expand_file_name(message_signature_file, signature_file, status, message_signature_directory)
                    if (.not. status%ok()) return
                end if
            end if

            insert_it = file_exists(signature_file)
        end if

        if (.not. insert_it) return
        call goto_character(point_maximum())
        ! Insert the signature.
        if (.not. is_beginning_of_line()) call newline(status)
        if (status%ok() .and. message_signature_insert_empty_line) call newline(status)
        if (status%ok()) call insert("-- ", status)
        if (status%ok()) call newline(status)
        if (.not. status%ok()) return

        if (allocated(text)) then
            call insert(text, status)
        else
            call insert_file_contents(signature_file, .false., inserted, status)
        end if

        if (.not. status%ok()) return
        call goto_character(point_maximum())
        if (.not. is_beginning_of_line()) call newline(status)
    end subroutine insert_message_signature

    ! ------------------------------------------------------------------
    ! Message buffers
    ! ------------------------------------------------------------------

    !> The names of the active message buffers (in Message mode and not
    !> sent) in NAMES, most recently selected first.  Port of
    !> message.el:message-buffers.
    subroutine get_message_buffers(names)
        type(string_type), allocatable, intent(out) :: names(:)
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k, j

        allocate (names(0))
        old = current_buffer()
        list = buffer_list()

        do j = 1, size(list)
            call set_buffer(list(j), st)
            if (.not. st%ok()) cycle
            if (.not. is_derived_mode(major_modes%message_mode)) cycle
            k = local_index()

            if (k > 0) then
                if (locals(k)%sent_via_mail) cycle
            end if

            call append_string(names, buffers(current)%name)
        end do

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine get_message_buffers

    !> A new (unique) buffer name in NAME, based on TYPE ("mail") and TO
    !> and GROUP.  Port of message.el:message-buffer-name.
    recursive subroutine generate_message_buffer_name(type, name, status, to, group)
        character(len=*), intent(in) :: type
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: to, group
        type(handle_type), allocatable :: list(:)
        character(len=:), allocatable :: new, regular_expression, part, bname
        integer :: best, k, number
        logical :: saved_fold

        status = status_type()

        if (associated(message_generate_new_buffers%procedure)) then
            ! Call the procedure of `message_generate_new_buffers'.
            call message_generate_new_buffers%procedure(type, name, status, to, group)
        else if (message_generate_new_buffers%value) then
            ! Generate a new buffer name The Message Way.
            call message_way_name("*"//type, part, status)
            if (status%ok()) name = generate_new_buffer_name(part)
        else if (message_generate_new_buffers%choice == MESSAGE_GENERATE_NEW_BUFFERS_UNSENT) then
            call message_way_name("*unsent "//type, part, status)
            if (status%ok()) name = generate_new_buffer_name(part)
        else
            ! Search for the existing message buffer with the specified name.
            if (message_generate_new_buffers%choice == MESSAGE_GENERATE_NEW_BUFFERS_STANDARD) then
                new = generate_new_buffer_name("*"//type//" message*")
            else
                call message_way_name("*"//type, part, status)
                if (.not. status%ok()) return
                new = generate_new_buffer_name(part)
            end if

            saved_fold = case_fold_search%value
            case_fold_search%value = .false.

            if (string_match("<[0-9]+>\'", new, status) > 0) then
                part = new(:string_offset(new, match_beginning(0, status)))
            else
                part = new
            end if

            regular_expression = "\`"//regular_expression_quote(part)//"\(?:<\([0-9]+\)>\)?\'"
            list = buffer_list()
            best = 0
            name = new
            ! The buffer of the highest number (sort by number, last).
            number = -huge(0)

            do k = 1, size(list)
                bname = buffers(list(k)%index)%name
                if (string_match(regular_expression, bname, status) == 0) cycle
                if (.not. allocated(buffers(list(k)%index)%major_mode)) cycle
                if (buffers(list(k)%index)%major_mode /= "message_mode") cycle
                best = 1
                part = match_string(1, bname)
                if (len(part) == 0) part = "1"
                read (part, *) best

                if (best >= number) then
                    number = best
                    name = bname
                end if
            end do

            case_fold_search%value = saved_fold
        end if

    contains

        ! PREFIX, " to " and TO's full name (or TO), " on " and GROUP, and
        ! "*".  Not a port.
        recursive subroutine message_way_name(prefix, out, status)
            character(len=*), intent(in) :: prefix
            character(len=:), allocatable, intent(out) :: out
            type(status_type), intent(out) :: status
            type(mail_address_components_type), allocatable :: components(:)

            status = status_type()
            out = prefix

            if (present(to)) then
                call extract_mail_address_components(to, components, status)
                if (.not. status%ok()) return
                out = out//" to "

                if (size(components) > 0) then
                    if (allocated(components(1)%full_name)) then
                        out = out//components(1)%full_name
                    else
                        out = out//to
                    end if
                else
                    out = out//to
                end if
            end if

            if (present(group)) then
                if (len(group) > 0) out = out//" on "//group
            end if

            out = out//"*"
        end subroutine message_way_name
    end subroutine generate_message_buffer_name

    !> Pop to buffer NAME, and warn if it already exists and is modified;
    !> then erase it and put it in Message mode.  SWITCH_FUNCTION, if
    !> present, displays the buffer.  Port of
    !> message.el:message-pop-to-buffer.
    recursive subroutine pop_to_message_buffer(name, status, switch_function)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        procedure(mail_switch_procedure), optional :: switch_function
        type(handle_type) :: buffer, window
        logical :: modified

        buffer = get_buffer(name)

        if (is_buffer_live(buffer)) then
            window = get_buffer_window(buffer)

            if (window%index > 0) then
                ! Raise the frame already displaying the message buffer.
                call select_window(window, status)
            else if (present(switch_function)) then
                call switch_function(name, status)
            else
                call pop_to_buffer(buffer, status)
            end if

            if (.not. status%ok()) return
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call is_buffer_modified(buffer, modified, status)
            if (.not. status%ok()) return

            if (modified) then
                if (.not. ask("Message already being composed; erase? ", status)) then
                    if (status%ok()) call set_error(status, GENERIC_ERROR_CODE, "Message being composed")
                    return
                end if

                call message("")
            end if
        else
            if (present(switch_function)) then
                call switch_function(name, status)
                if (.not. status%ok()) return
                buffer = get_buffer(name)
            else
                call get_or_make_buffer(name, buffer, status)
                if (status%ok()) call pop_to_buffer_same_window(buffer, status)
            end if

            if (.not. status%ok()) return
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
        end if

        call erase_buffer(status)
        if (status%ok()) call message_mode(status)
    end subroutine pop_to_message_buffer

    ! Associate the message buffer with a file in the drafts directory.
    ! Port of message.el:message-set-auto-save-file-name (auto-saving
    ! itself is Deferred: `buffer_auto_save_file_name' is not set).
    recursive subroutine set_message_auto_save_file_name(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, suffix

        status = status_type()
        if (.not. allocated(message_auto_save_directory)) return

        if (.not. is_file_directory(directory_file_name(message_auto_save_directory))) then
            call create_directory(message_auto_save_directory, status, parents=.true.)
            if (.not. status%ok()) return
        end if

        ! Gnus is not alive, so arrange to save the draft message in a
        ! regular file in `message_auto_save_directory'.  Append a unique
        ! time-based suffix to the filename to allow multiple drafts to be
        ! saved simultaneously without overwriting each other.
        suffix = format_date(date_format_type('("-", i4.4, 2i2.2, "-", 3i2.2)', [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, &
                                                                                 TIME_MINUTE, TIME_SECOND]), status=status)
        if (.not. status%ok()) return
        call expand_file_name("*message*"//suffix, file, status, message_auto_save_directory)
        if (.not. status%ok()) return
        buffers(current)%filename = file
        ! clear-visited-file-modtime
        buffers(current)%modtime_known = .false.
        call buffer_file_coding_system%set_local("utf-8-emacs-unix")
    end subroutine set_message_auto_save_file_name

    !> Move point to where the user probably wants to find it: after the
    !> first empty header, else at the start of the body.  Port of
    !> message.el:message-position-point.
    recursive subroutine position_message_point(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved, found

        call narrow_to_message_headers(status)
        if (.not. status%ok()) return

        if (search_regular_expression("^[^:]+:[ "//TAB//"]*$", status)) then
            call search_backward(":", found, status)
            call widen()
            call forward_character(status)

            if (following_character() == iachar(" ")) then
                call forward_character(status)
            else
                call insert(" ", status)
            end if
        else
            if (.not. status%ok()) return
            call goto_character(point_maximum())
            call widen()
            moved = forward_line(1_int64)
            if (.not. looking_at("$", status)) moved = forward_line(2_int64)
        end if
    end subroutine position_message_point

    !> Set up the current buffer for a mail message to TO with SUBJECT:
    !> the headers, `message_default_headers', the separator,
    !> `message_default_mail_headers', the headers of
    !> `message_required_mail_headers' generated first (References and
    !> In-Reply-To), the signature, then `set_up_message_hook'.  Port of
    !> message.el:message-setup (with message-setup-1 and
    !> mailheader.el's mail-header-format; the headers of OTHER-HEADERS,
    !> the actions and `message-mail-user-agent' are Deferred).
    recursive subroutine set_up_message(status, to, subject)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: to, subject
        type(message_header_type), allocatable :: generate(:)
        character(len=:), allocatable :: text
        type(restriction_type) :: restriction
        integer(int64) :: moved, start
        integer :: k

        status = status_type()
        call goto_character(point_minimum())
        ! Insert all the headers (mail-header-format, in the order of
        ! message-header-format-alist: To, then Subject).
        if (present(to)) call insert("To: "//to//LF, status)
        if (status%ok() .and. present(subject)) call insert("Subject: "//subject//LF, status)
        if (status%ok()) call insert(LF, status)
        if (.not. status%ok()) return
        start = point()
        moved = forward_line(-1_int64)
        call delete_region(point(), start, status)
        if (.not. status%ok()) return

        if (.not. message_default_headers%is_off()) then
            if (associated(message_default_headers%procedure)) then
                call message_default_headers%procedure(text, status)
                if (.not. status%ok()) return
            else if (allocated(message_default_headers%text)) then
                text = message_default_headers%text
            end if

            if (allocated(text)) call insert(text, status)
            if (status%ok() .and. .not. is_beginning_of_line()) call insert(LF, status)
            if (.not. status%ok()) return
        end if

        call insert(mail_header_separator//LF, status)
        if (.not. status%ok()) return
        moved = forward_line(-1_int64)

        ! If a crash happens while replying, the auto-save file would *not*
        ! have a `References:' header if `message-generate-headers-first'
        ! was nil.  Therefore, always generate it first.  (And why not
        ! include the `In-Reply-To' header as well.)
        if (is_message_mail()) then
            if (allocated(message_default_mail_headers)) then
                call insert(message_default_mail_headers, status)
                if (status%ok() .and. .not. is_beginning_of_line()) call insert(LF, status)
                if (.not. status%ok()) return
            end if

            ! message-headers-to-generate: the required headers named
            ! References or In-Reply-To, without Lines and Subject.
            allocate (generate(0))

            do k = 1, size(message_required_mail_headers)
                select case (trim(message_required_mail_headers(k)%name))
                case ("References", "In-Reply-To")
                    generate = [generate, message_required_mail_headers(k)]
                case default
                    continue
                end select
            end do

            if (allocated(message_required_headers)) then
                do k = 1, size(message_required_headers)
                    select case (trim(message_required_headers(k)%name))
                    case ("References", "In-Reply-To")
                        generate = [generate, message_required_headers(k)]
                    case default
                        continue
                    end select
                end do
            end if

            call generate_message_headers_only(generate, status)
            if (.not. status%ok()) return
        end if

        call message_signature_setup_hook%execute(status)
        if (status%ok()) call insert_message_signature(status)
        if (.not. status%ok()) return
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call message_header_setup_hook%execute(status)
        call save_restriction_end(restriction)
        if (.not. status%ok()) return
        ! Gnus posting styles are applied via buffer-local
        ! `set_up_message_hook' values.
        call set_up_message_hook%execute(status)
        if (status%ok()) call position_message_point(status)
        if (.not. status%ok()) return
        ! Allow correct handling of `message-checksum' in
        ! `message-yank-original':
        call set_buffer_modified(.false.)
        call undo_boundary()
    end subroutine set_up_message

    ! `generate_message_headers' of HEADERS alone (message-setup-1 lets
    ! message-generate-headers-first choose the headers among
    ! `message_required_mail_headers' and `message_required_headers', so
    ! the latter are not added again).  Not a port.
    recursive subroutine generate_message_headers_only(headers, status)
        type(message_header_type), intent(in) :: headers(:)
        type(status_type), intent(out) :: status
        type(message_header_type), allocatable :: saved(:)
        logical :: had

        had = allocated(message_required_headers)
        if (had) call move_alloc(message_required_headers, saved)
        call generate_message_headers(headers, status)
        if (had) call move_alloc(saved, message_required_headers)
    end subroutine generate_message_headers_only

    ! Whether the current buffer holds a mail message: composing mail, or
    ! a To, Cc or Bcc field.  Port of message.el:message-mail-p.
    recursive logical function is_message_mail()
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: value
        type(status_type) :: st

        is_message_mail = message_this_is_mail
        if (is_message_mail) return
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(st)

        if (st%ok()) then
            call get_message_field("to", value, st)
            if (.not. allocated(value)) call get_message_field("cc", value, st)
            if (.not. allocated(value)) call get_message_field("bcc", value, st)
            is_message_mail = allocated(value)
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end function is_message_mail

    ! ------------------------------------------------------------------
    ! Message mode
    ! ------------------------------------------------------------------

    !> Major mode for editing mail and news to be sent.
    !> Like `text_mode', but with these additional commands:
    !>
    !> \{message_mode_map}
    !> Port of message.el:message-mode (MML mode, the syntax properties
    !> of citations and smileys, the forbidden text properties, the
    !> fontification, the tool bar and auto-saving are Deferred).
    recursive subroutine message_mode(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: quote_prefix_regular_expression
        logical :: saved
        integer :: k

        if (.not. associated(major_modes%message_mode%execute)) call initialize_message()
        call load_message()
        call major_modes%message_mode%begin(saved, status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        if (k > 0) locals(k) = local_type(serial=buffers(current)%serial)
        ! message-setup-fill-variables
        call fill_paragraph_function%set_local(fill_message_paragraph)
        ! User should change message_cite_prefix_regular_expression if
        ! message_paste_prefix is set to an abnormal value.
        quote_prefix_regular_expression = "\("//message_cite_prefix_regular_expression//"\)[ "//TAB//"]*"
        call paragraph_start%set_local(regular_expression_quote(mail_header_separator)//"$\|"// &
                                       "[ "//TAB//"]*$\|"// &
                                       "-- $\|"// &
                                       "---+$\|"// &
                                       page_delimiter%value//"$\|"// &
                                       ".*wrote:$\|"// &
                                       quote_prefix_regular_expression//"$\|"// &
                                       "<#!*/?\(multipart\|part\|external\|mml\|secure\)")
        call paragraph_separate%set_local(paragraph_start%value)
        call auto_fill_inhibit_regular_expression%set_local("")
        call auto_fill_inhibit_regular_expression%clear()
        call normal_auto_fill_function%set_local(do_message_auto_fill)

        if (.not. message_fill_column%is_off()) then
            call fill_column%set_local(message_fill_column%columns)
            call turn_on_auto_fill(status)
            if (.not. status%ok()) return
        end if

        ! Allow using comment commands to add/remove quoting.
        if (allocated(message_paste_prefix)) then
            call comment_start%set_local(message_paste_prefix)
            call comment_start_skip%set_local("^"//regular_expression_quote(message_paste_prefix)//"[ "//TAB//"]*")
        end if

        ! Allow mail alias things.
        call set_up_mail_abbreviations(status)
        if (.not. status%ok()) return

        if (.not. allocated(buffers(current)%filename)) then
            call set_message_auto_save_file_name(status)
            if (.not. status%ok()) return
        end if

        buffers(current)%multibyte = .true.
        ! No tabs for indentation.
        call indent_tabs_mode%set_local(.false.)
        call parse_expression_ignore_comments%set_local(.true.)
        call major_modes%message_mode%end(saved, status)
    end subroutine message_mode

    ! Like `do_auto_fill', but don't fill in message header.  Port of
    ! message.el:message-do-auto-fill.
    recursive subroutine do_message_auto_fill(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved
        logical :: had

        status = status_type()
        if (is_message_point_in_header()) return
        had = allocated(paragraph_separate%value)
        if (had) saved = paragraph_separate%value
        call paragraph_separate%clear_local()
        call do_auto_fill(status)
        if (had) call paragraph_separate%set_local(saved)
    end subroutine do_message_auto_fill

    ! Message specific function to fill a paragraph: the header field at
    ! point, or the paragraph with `newline_and_reformat_message'.  Port of
    ! message.el:message-fill-paragraph.
    recursive subroutine fill_message_paragraph(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        integer(int64) :: e

        done = .true.

        if (is_message_point_in_header()) then
            call fill_message_field(e, status)
        else
            call newline_and_reformat_message(status, justify, not_break=.true.)
        end if
    end subroutine fill_message_paragraph

    ! ------------------------------------------------------------------
    ! Composing
    ! ------------------------------------------------------------------

    ! Set the defaults that upstream computes from other settings when
    ! message.el is loaded, here on the first use (after the init file),
    ! unless the configuration set them.  Not a port.
    subroutine load_message()
        character(len=:), allocatable :: value, user
        logical :: found
        integer :: k

        if (loaded) return
        loaded = .true.
        if (.not. allocated(message_paste_prefix) .and. allocated(mail_paste_prefix)) message_paste_prefix = mail_paste_prefix

        if (.not. allocated(message_cite_prefix_regular_expression)) then
            message_cite_prefix_regular_expression = "\(?:[ "//TAB//"]*\(?:[[:word:]_.]+>\|[>|]\)\)+"
            if (allocated(mail_citation_prefix_regular_expression)) message_cite_prefix_regular_expression = &
                mail_citation_prefix_regular_expression
        end if

        if (.not. allocated(message_signature_file) .and. allocated(mail_signature_file)) then
            message_signature_file = mail_signature_file
        end if

        if (.not. allocated(message_subject_re_regular_expression)) then
            value = ""

            if (allocated(mail_re_regular_expressions)) then
                do k = 1, size(mail_re_regular_expressions)
                    if (k > 1) value = value//"\|"
                    value = value//trim(mail_re_regular_expressions(k))
                end do
            end if

            message_subject_re_regular_expression = wrap_mail_re_regular_expression("\("//value//"\)")
        end if

        if (.not. allocated(message_default_mail_headers)) then
            ! Ease the transition from mail-mode to message-mode.
            value = ""
            if (allocated(mail_default_reply_to)) value = value//"Reply-To: "//mail_default_reply_to//LF

            if (mail_self_blind) then
                user = user_mail_address_value()
                if (.not. allocated(user)) user = ""
                value = value//"Bcc: "//user//LF
            end if

            if (allocated(mail_archive_file_name)) value = value//"Fcc: "//mail_archive_file_name//LF
            if (allocated(mail_default_headers)) value = value//mail_default_headers
            message_default_mail_headers = value
        end if

        if (.not. message_user_organization%void) then
            if (message_user_organization%value .and. .not. allocated(message_user_organization%text) .and. &
                .not. associated(message_user_organization%procedure)) then
                call getenv("ORGANIZATION", value, found)
                if (found) message_user_organization%text = value
            end if
        end if
    end subroutine load_message

    !> Start editing a mail message to be sent (the COMPOSE of
    !> `message_user_agent').
    !> CONTINUE says whether to continue editing a message already being
    !> composed.  SWITCH_FUNCTION is a procedure used to switch to and
    !> display the mail buffer.  Port of message.el:message-mail
    !> (OTHER-HEADERS, YANK-ACTION, SEND-ACTIONS and RETURN-ACTION come with
    !> their users, Rmail and Reporter).
    recursive subroutine mail_message(status, to, subject, continue, switch_function)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: to, subject
        logical, intent(in), optional :: continue
        procedure(mail_switch_procedure), optional :: switch_function
        type(string_type), allocatable :: names(:)
        character(len=:), allocatable :: name
        logical :: cont, saved_mail

        call load_message()
        cont = .false.
        if (present(continue)) cont = continue
        saved_mail = message_this_is_mail
        message_this_is_mail = .true.

        body: block
            ! Search for the existing message buffer if CONTINUE is true.
            if (cont) then
                call get_message_buffers(names)

                if (size(names) > 0) then
                    call pop_to_buffer(get_buffer(names(1)%text), status)
                    exit body
                end if
            end if

            ! Start a new buffer.
            if (present(to)) then
                call generate_message_buffer_name("mail", name, status, to=to)
            else
                call generate_message_buffer_name("mail", name, status)
            end if

            if (.not. status%ok()) exit body

            if (present(switch_function)) then
                call pop_to_message_buffer(name, status, switch_function)
            else
                call pop_to_message_buffer(name, status)
            end if

            if (.not. status%ok()) exit body

            if (present(to) .and. present(subject)) then
                call set_up_message(status, to, subject)
            else if (present(to)) then
                call set_up_message(status, to, "")
            else if (present(subject)) then
                call set_up_message(status, "", subject)
            else
                call set_up_message(status, "", "")
            end if
        end block body

        message_this_is_mail = saved_mail
    end subroutine mail_message

    !> Like `mail_message' command, but display mail buffer in another
    !> window.  Port of message.el:message-mail-other-window.
    recursive subroutine mail_message_other_window(status, to, subject)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: to, subject
        character(len=:), allocatable :: name
        logical :: saved_mail

        call load_message()
        saved_mail = message_this_is_mail
        message_this_is_mail = .true.

        body: block
            if (present(to)) then
                call generate_message_buffer_name("mail", name, status, to=to)
            else
                call generate_message_buffer_name("mail", name, status)
            end if

            if (.not. status%ok()) exit body
            call pop_to_message_buffer(name, status, switch_to_buffer_other_window_by_name)
            if (.not. status%ok()) exit body

            ! (or to "") and (or subject "")
            if (present(to) .and. present(subject)) then
                call set_up_message(status, to, subject)
            else if (present(to)) then
                call set_up_message(status, to, "")
            else if (present(subject)) then
                call set_up_message(status, "", subject)
            else
                call set_up_message(status, "", "")
            end if
        end block body

        message_this_is_mail = saved_mail
    end subroutine mail_message_other_window

    ! `switch_to_buffer_other_window' on the buffer named BUFFER_NAME, made
    ! if needed (as upstream's does with a buffer name).  Not a port.
    recursive subroutine switch_to_buffer_other_window_by_name(buffer_name, status)
        character(len=*), intent(in) :: buffer_name
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        call get_or_make_buffer(buffer_name, buffer, status)
        if (status%ok()) call switch_to_buffer_other_window(buffer, status)
    end subroutine switch_to_buffer_other_window_by_name

    ! ------------------------------------------------------------------
    ! Sending
    ! ------------------------------------------------------------------

    !> Send message like `send_message', then, if no errors, exit from mail
    !> buffer.  Port of message.el:message-send-and-exit (the exit actions
    !> come with their users, Rmail and Reporter).
    recursive subroutine send_message_and_exit(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, position
        type(status_type) :: st

        buffer = current_buffer()
        position = marker_at(point())
        call send_message(status)

        if (status%ok() .and. is_buffer_live(buffer)) then
            if (message_close_buffer_on_exit) then
                call close_buffer(buffer, status)
            else
                ! Restore the point in the message buffer.
                call set_buffer(buffer, status)
                if (status%ok()) call goto_character(marked_position(position))
                if (status%ok()) call bury_message(buffer, status)
            end if
        end if

        call destroy_marker(position, st)
    end subroutine send_message_and_exit

    ! Make BUFFER current again if it is live.  Not a port.
    subroutine restore_buffer(buffer)
        type(handle_type), intent(in) :: buffer
        type(status_type) :: st

        if (is_buffer_live(buffer)) call set_buffer(buffer, st)
    end subroutine restore_buffer

    !> Don't send the message you have been editing.
    !> Instead, just auto-save the buffer and then bury it.  Port of
    !> message.el:message-dont-send (the postpone actions come with their
    !> users).
    recursive subroutine message_dont_send(status)
        type(status_type), intent(out) :: status

        call set_buffer_modified(.true.)
        call save_buffer(1_int64, .false., status)
        if (status%ok()) call bury_message(current_buffer(), status)
    end subroutine message_dont_send

    !> Close the current buffer, after asking when it is modified (as
    !> `message_close_buffer_query' says).  Port of
    !> message.el:message-kill-buffer (the kill actions come with their
    !> users; auto-saving is Deferred, so there is no auto-save file to
    !> delete).
    recursive subroutine close_message_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file_name
        logical :: modified, proceed

        call is_buffer_modified(current_buffer(), modified, status)
        if (.not. status%ok()) return
        proceed = .not. modified .or. .not. message_close_buffer_query
        if (.not. proceed) proceed = ask_yes_or_no("Message modified; kill anyway? ", status)
        if (.not. status%ok() .or. .not. proceed) return
        if (allocated(buffers(current)%filename)) call move_alloc(buffers(current)%filename, file_name)
        call close_buffer(current_buffer(), status)
        if (.not. status%ok()) return

        if (allocated(file_name)) then
            if (file_exists(file_name)) then
                if (modified) then
                    proceed = ask_yes_or_no("Remove the backup file too? ", status)
                else
                    proceed = ask_yes_or_no("Remove the backup file? ", status)
                end if

                ! Upstream deletes the auto-save file, which Fortmacs does
                ! not make (auto-saving is Deferred): nothing to delete.
                if (.not. status%ok()) return
            end if
        end if
    end subroutine close_message_buffer

    !> Bury this mail BUFFER.  Port of message.el:message-bury (the return
    !> action comes with its users, Rmail and Reporter).
    recursive subroutine bury_message(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: old

        old = current_buffer()
        call set_buffer(buffer, status)
        if (status%ok()) call bury_buffer(status)
        if (is_buffer_live(old) .and. .not. is_buffer_live(buffer)) call set_buffer(old, status)
    end subroutine bury_message

    !> Send the message in the current buffer.
    !> If `message_interactive' is true, wait for success indication or
    !> error messages, and inform user.
    !> Otherwise any failure is reported in a message back to the user from
    !> the mailer.  Port of message.el:message-send (posting news, the MML
    !> security checks, `message_server_alist' and the send actions are
    !> Deferred).
    recursive subroutine send_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: fcc
        logical :: success, sent, dont_barf, proceed, news
        integer :: k

        ! Make it possible to undo the coming changes.
        call undo_boundary()
        call fix_message_before_sending(status)
        if (.not. status%ok()) return
        call message_send_hook%execute(status)
        if (.not. status%ok()) return

        if (message_confirm_send) then
            proceed = ask("Send message? ", status)
            if (.not. status%ok()) return

            if (.not. proceed) then
                call set_error(status, QUIT_CODE, "Quit")
                return
            end if
        end if

        call message("Sending...")
        success = .true.
        sent = .false.
        dont_barf = .false.
        ! message-send-method-alist: news, then mail.
        call is_message_news(news, status)
        if (.not. status%ok()) return

        if (news) then
            call set_error(status, VOID_FUNCTION_CODE, "send_message_news")
            return
        end if

        if (is_message_mail()) then
            k = local_index()
            proceed = .true.

            if (k > 0) then
                if (locals(k)%sent_via_mail) then
                    call get_message_field_value("supersedes", fcc, status)
                    if (.not. status%ok()) return
                    if (.not. allocated(fcc)) proceed = ask("Already sent message via mail; resend? ", status)
                    if (.not. status%ok()) return
                end if
            end if

            if (proceed) then
                call send_message_mail(status)
                if (.not. status%ok()) return
                sent = .true.
            end if
        end if

        if (.not. sent .and. success) then
            call get_message_field_value("Fcc", fcc, status)
            if (.not. status%ok()) return

            if (allocated(fcc)) then
                if (message_allow_no_recipients%value) then
                    dont_barf = .true.
                else if (message_allow_no_recipients%choice == MESSAGE_ALLOW_NO_RECIPIENTS_ASK) then
                    dont_barf = ask("No receiver, perform Fcc anyway? ", status)
                    if (.not. status%ok()) return
                end if
            end if

            if (.not. dont_barf) then
                call set_error(status, GENERIC_ERROR_CODE, "No methods specified to send by")
                return
            end if
        end if

        if (dont_barf .or. (success .and. sent)) then
            call do_message_fcc(status)
            if (status%ok()) call message_sent_hook%execute(status)
            if (.not. status%ok()) return
            call message("Sending...done")
            ! Mark the buffer as unmodified (and delete auto-save: Deferred).
            call set_buffer_modified(.false.)
            ! Delete other mail buffers and stuff.
            call do_send_message_housekeeping(status)
        end if
    end subroutine send_message

    ! Whether the current buffer contains a news message: a Newsgroups
    ! field and no Posted-To field, unless composing mail.  Port of
    ! message.el:message-news-p.
    recursive subroutine is_message_news(news, status)
        logical, intent(out) :: news
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: newsgroups, posted_to

        news = .false.
        status = status_type()
        if (message_this_is_mail) return
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call get_message_field("newsgroups", newsgroups, status)
        if (status%ok()) call get_message_field("posted-to", posted_to, status)
        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
        news = allocated(newsgroups) .and. .not. allocated(posted_to)
    end subroutine is_message_news

    ! Do various things to make the message nice before sending it: a
    ! final newline, sorted headers, and the checks of illegible text and
    ! bogus recipients.  Port of message.el:message-fix-before-sending (the
    ! check of invisible text is Deferred).
    recursive subroutine fix_message_before_sending(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion

        status = status_type()
        ! Make sure there's a newline at the end of the message.
        call goto_character(point_maximum())
        if (.not. is_beginning_of_line()) call insert(LF, status)
        if (.not. status%ok()) return
        ! Make the hidden headers visible.
        call widen()
        ! Sort headers before sending the message.
        call sort_message_headers(status)
        if (.not. status%ok()) return

        if (message_syntax_checks%illegible_text) then
            call save_excursion_begin(excursion)
            call check_illegible_text(status)
            call save_excursion_end(excursion)
            if (.not. status%ok()) return
        end if

        if (message_syntax_checks%bogus_recipient) then
            ! Warn before sending a mail to an invalid address.
            call save_excursion_begin(excursion)
            call check_message_recipients(status)
            call save_excursion_end(excursion)
        end if
    end subroutine fix_message_before_sending

    ! The check of illegible text: offer to delete, replace or encode
    ! control characters (and NUL).  Port of the illegible-text check of
    ! message.el:message-fix-before-sending (raw bytes, upstream's
    ! eight-bit characters, are Deferred: the buffer holds UTF-8 text).
    recursive subroutine check_illegible_text(status)
        type(status_type), intent(out) :: status
        character(len=*), parameter :: SEVEN_BIT = " -"//achar(127)//LF//TAB//achar(7)//achar(8)//achar(11)//achar(12)//achar(31)
        type(read_multiple_choice_choice_type) :: choices(5)
        integer(int64) :: moved
        integer(int32) :: char
        logical :: found, nul_characters
        integer :: choice
        character(len=8) :: hexadecimal

        call goto_character(point_minimum())
        nul_characters = search_string(achar(0), status)
        call goto_character(point_minimum())
        found = .false.

        do
            moved = skip_characters_forward(SEVEN_BIT, status)
            if (is_end_of_buffer()) exit
            char = following_character()
            if (char < 128) found = .true.
            call forward_character(status)
            if (.not. status%ok()) return
        end do

        if (.not. found) return
        choices(1) = read_multiple_choice_choice_type(iachar("d"), "delete", "Remove non-printable characters and send")
        choices(2) = read_multiple_choice_choice_type(iachar("r"), "replace", "Replace non-printable characters with """// &
                                                      message_replacement_character//""" and send")
        choices(3) = read_multiple_choice_choice_type(iachar("u"), "url-encode", "Use URL %hex encoding")
        choices(4) = read_multiple_choice_choice_type(iachar("s"), "send", "Send as is without removing anything")
        choices(5) = read_multiple_choice_choice_type(iachar("e"), "edit", "Continue editing")

        if (nul_characters) then
            call read_multiple_choice("NUL characters found, which may cause problems.  Continue sending?", choices, choice, &
                                      status)
        else
            call read_multiple_choice("Non-printable characters found.  Continue sending?", choices, choice, status)
        end if

        if (.not. status%ok()) return

        if (choice == iachar("e")) then
            call set_error(status, GENERIC_ERROR_CODE, "Non-printable characters")
            return
        end if

        call goto_character(point_minimum())
        moved = skip_characters_forward(SEVEN_BIT, status)

        do while (.not. is_end_of_buffer())
            char = following_character()

            if (char < 128) then
                select case (choice)
                case (117)
                    ! "u": URL encoding.
                    call delete_region(point(), point() + 1, status)
                    write (hexadecimal, "(z0)") char
                    call insert("%"//to_lower(trim(hexadecimal)), status)
                case (115)
                    ! "s": send as is.
                    call forward_character(status)
                case default
                    call delete_region(point(), point() + 1, status)
                    if (status%ok() .and. choice == iachar("r")) call insert(message_replacement_character, status)
                end select

                if (.not. status%ok()) return
                if (choice /= 115) call forward_character_if_possible()
            else
                call forward_character(status)
            end if

            moved = skip_characters_forward(SEVEN_BIT, status)
        end do

    contains

        ! (forward-char) after the deletion: upstream moves one character
        ! on, skipping the character after the one replaced.  Not a port.
        subroutine forward_character_if_possible()
            type(status_type) :: st

            if (.not. is_end_of_buffer()) call forward_character(st)
        end subroutine forward_character_if_possible

        ! TEXT in lower case (hexadecimal digits).  Not a port.
        function to_lower(text) result(out)
            character(len=*), intent(in) :: text
            character(len=len(text)) :: out
            integer :: i

            out = text

            do i = 1, len(text)
                if (text(i:i) >= "A" .and. text(i:i) <= "Z") out(i:i) = achar(iachar(text(i:i)) + 32)
            end do
        end function to_lower
    end subroutine check_illegible_text

    !> Warn before composing or sending a mail to an invalid address: ask
    !> about each address of To, Cc and Bcc that looks bogus (no address,
    !> or matching `message_bogus_addresses').  Port of
    !> message.el:message-check-recipients (with message-bogus-recipient-p;
    !> the syntactic check, which needs mail-parse.el's
    !> mail-header-parse-addresses, is Deferred).
    recursive subroutine check_message_recipients(status)
        type(status_type), intent(out) :: status
        character(len=*), parameter :: HEADERS(3) = [character(len=3) :: "To", "Cc", "Bcc"]
        type(restriction_type) :: restriction
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: value, address, bogus_regular_expression
        logical :: bogus
        integer :: k, j, i

        call save_restriction_begin(restriction)

        body: block
            call narrow_to_message_headers(status)
            if (.not. status%ok()) exit body
            bogus_regular_expression = ""

            if (allocated(message_bogus_addresses)) then
                do i = 1, size(message_bogus_addresses)
                    if (i > 1) bogus_regular_expression = bogus_regular_expression//"\|"
                    bogus_regular_expression = bogus_regular_expression//trim(message_bogus_addresses(i))
                end do
            end if

            do k = 1, size(HEADERS)
                call get_message_field(trim(HEADERS(k)), value, status)
                if (.not. status%ok()) exit body
                if (.not. allocated(value)) cycle
                ! Then check for likely-bogus addresses.
                call extract_mail_address_components(value, components, status, all=.true.)
                if (.not. status%ok()) exit body

                do j = size(components), 1, -1
                    address = ""
                    if (allocated(components(j)%address)) address = components(j)%address
                    bogus = len(address) == 0
                    if (.not. bogus .and. len(bogus_regular_expression) > 0) bogus = matches(bogus_regular_expression, address)
                    if (.not. bogus) cycle
                    ! An empty address is no address at all upstream (and
                    ! nothing to ask about).
                    if (len(address) == 0) cycle

                    if (.not. ask(substitute_quotes("Address `"//address//"' might be bogus.  Continue? "), status)) then
                        if (status%ok()) call set_error(status, USER_ERROR_CODE, "Bogus address")
                        exit body
                    end if
                end do
            end do
        end block body

        call save_restriction_end(restriction)
    end subroutine check_message_recipients

    !> Send the message in the current buffer as mail: generate the
    !> required headers, then encode a copy and hand it to
    !> `message_send_mail_function'.  Port of message.el:message-send-mail
    !> (with message--send-mail-maybe-partially; Mail-Followup-To for
    !> subscribed lists, the check of suspicious addresses and sending in
    !> parts are Deferred).
    recursive subroutine send_message_mail(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: mailbuf, tembuf, old
        type(restriction_type) :: restriction
        character(len=:), allocatable :: text, mft
        type(message_header_type) :: lines(1)
        logical :: saved_fold, saved_mail
        integer :: k

        mailbuf = current_buffer()
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        saved_mail = message_this_is_mail
        message_this_is_mail = .true.
        call save_restriction_begin(restriction)

        headers: block
            call narrow_to_message_headers(status)
            if (.not. status%ok()) exit headers
            ! Delete the MFT header if the field is empty.
            call get_mail_field("mail-followup-to", mft, status)
            if (.not. status%ok()) exit headers

            if (allocated(mft)) then
                if (len(mft) == 0) call remove_message_header("Mail-Followup-To", status)
                if (.not. status%ok()) exit headers
            end if

            ! Insert some headers.
            call generate_message_headers(message_required_mail_headers, status)
            if (.not. status%ok()) exit headers
            ! Check continuation headers.
            call check_message_continuation_headers(status)
            if (status%ok()) call fold_long_message_headers(status)
            ! Let the user do all of the above.
            if (status%ok()) call message_header_hook%execute(status)
        end block headers

        call save_restriction_end(restriction)

        if (.not. status%ok()) then
            case_fold_search%value = saved_fold
            message_this_is_mail = saved_mail
            return
        end if

        call temporary_buffer_begin(" message temp", old, tembuf, status)

        body: block
            if (.not. status%ok()) exit body
            case_fold_search%value = .false.
            ! Avoid copying text props.
            call set_buffer(mailbuf, status)
            if (status%ok()) call buffer_substring_no_properties(point_minimum(), point_maximum(), text, status)
            if (status%ok()) call set_buffer(tembuf, status)
            if (.not. status%ok()) exit body
            call insert(text, status)
            if (status%ok()) call encode_message_body(status)
            if (.not. status%ok()) exit body
            ! message--cache-encoded
            text = buffer_string()
            call set_buffer(mailbuf, status)
            call make_local_entry(k)
            if (k > 0) locals(k)%encoded_mail_cache = text
            call set_buffer(tembuf, status)
            call save_restriction_begin(restriction)
            call narrow_to_message_headers(status)

            ! We (re)generate the Lines header.
            if (status%ok() .and. any(message_required_mail_headers%name == "Lines")) then
                lines(1) = message_header_type("Lines")
                call generate_message_headers_only(lines, status)
            end if

            ! Remove some headers.
            if (status%ok()) call remove_message_header(message_ignored_mail_headers, status, is_regular_expression=.true.)
            if (status%ok()) call encode_rfc2047_message_header(status)
            call save_restriction_end(restriction)
            if (.not. status%ok()) exit body
            call goto_character(point_maximum())
            ! require one newline at the end.
            if (preceding_character() /= 10) call insert(LF, status)
            if (status%ok()) call cleanup_message_headers(status)
            if (.not. status%ok()) exit body
            call message("Sending via mail...")
            call send_message_mail_by_method(status)
        end block body

        call temporary_buffer_end(old, tembuf)
        call restore_buffer(mailbuf)
        case_fold_search%value = saved_fold
        message_this_is_mail = saved_mail

        if (status%ok()) then
            call make_local_entry(k)
            if (k > 0) locals(k)%sent_via_mail = .true.
        end if
    end subroutine send_message_mail

    !> The envelope-from address for sending with sendmail in ADDRESS.
    !> Port of message.el:message-sendmail-envelope-from (with
    !> message--sendmail-envelope-from).
    recursive subroutine get_message_sendmail_envelope_from(address, status)
        character(len=:), allocatable, intent(out) :: address
        type(status_type), intent(out) :: status
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: from
        logical :: header

        status = status_type()
        header = .false.

        if (message_sendmail_envelope_from%choice == MESSAGE_SENDMAIL_ENVELOPE_FROM_OBEY_MAIL_ENVELOPE_FROM) then
            if (allocated(mail_envelope_from%address)) then
                address = mail_envelope_from%address
                return
            end if

            header = mail_envelope_from%choice == MAIL_ENVELOPE_FROM_HEADER
        else if (allocated(message_sendmail_envelope_from%address)) then
            address = message_sendmail_envelope_from%address
            return
        else
            header = message_sendmail_envelope_from%choice == MESSAGE_SENDMAIL_ENVELOPE_FROM_HEADER
        end if

        if (header) then
            call get_message_field("from", from, status)
            if (.not. status%ok()) return

            if (.not. allocated(from)) then
                call set_error(status, GENERIC_ERROR_CODE, "Invalid address: .false.")
                return
            end if

            call extract_mail_address_components(from, components, status)
            if (.not. status%ok()) return
            address = ""

            if (size(components) > 0) then
                if (allocated(components(1)%address)) address = components(1)%address
            end if
        else
            call make_message_address(address, status)
        end if
    end subroutine get_message_sendmail_envelope_from

    !> Send off the prepared buffer with sendmail.  Port of
    !> message.el:message-send-mail-with-sendmail.
    recursive subroutine send_message_mail_with_sendmail(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: errbuf, mailbuf, delimline
        type(value_type) :: result, destination, item
        type(value_type), allocatable :: argv(:)
        type(restriction_type) :: restriction
        character(len=:), allocatable :: resend_to, envelope, errors
        character(len=64) :: exit_text
        logical :: saved_fold, failed
        integer :: k
        type(status_type) :: st

        status = status_type()
        failed = .false.
        mailbuf = current_buffer()

        if (message_interactive) then
            call get_or_make_buffer(generate_new_buffer_name(" sendmail errors"), errbuf, status)
            if (.not. status%ok()) return
        end if

        saved_fold = case_fold_search%value

        body: block
            case_fold_search%value = .true.
            call save_restriction_begin(restriction)
            call narrow_to_message_headers(status)
            if (status%ok()) call get_message_field("resent-to", resend_to, status)
            call save_restriction_end(restriction)
            if (.not. status%ok()) exit body
            ! Change header-delimiter to be what sendmail expects.
            call goto_character(point_minimum())

            if (.not. search_regular_expression("^"//regular_expression_quote(mail_header_separator)//LF, status)) then
                if (status%ok()) call set_error(status, GENERIC_ERROR_CODE, "Search failed: ""^"// &
                                                regular_expression_quote(mail_header_separator)//"\n""")
                exit body
            end if

            call replace_match(LF, status)
            if (.not. status%ok()) exit body
            call goto_character(point() - 1)
            delimline = marker_at(point())
            call message_send_mail_hook%execute(status)
            if (.not. status%ok()) exit body

            if (message_interactive) then
                call set_buffer(errbuf, status)
                if (status%ok()) call erase_buffer(status)
                call set_buffer(mailbuf, st)
                if (.not. status%ok()) exit body
            end if

            allocate (argv(0))
            call append_argument("-oi")

            if (allocated(message_sendmail_extra_arguments)) then
                do k = 1, size(message_sendmail_extra_arguments)
                    call append_argument(trim(message_sendmail_extra_arguments(k)))
                end do
            end if

            ! Always specify who from,
            ! since some systems have broken sendmails.
            ! But some systems are more broken with -f, so
            ! we'll let users override this.
            if (.not. message_sendmail_f_is_evil) then
                call get_message_sendmail_envelope_from(envelope, status)
                if (.not. status%ok()) exit body
                call append_argument("-f")
                call append_argument(envelope)
            end if

            ! These mean "report errors by mail"
            ! and "deliver in background".
            if (.not. message_interactive) then
                call append_argument("-oem")
                call append_argument("-odb")
            end if

            ! Get the addresses from the message
            ! unless this is a resend.
            ! We must not do that for a resend
            ! because we would find the original addresses.
            ! For a resend, include the specific addresses.
            if (allocated(resend_to)) then
                call append_argument(resend_to)
            else
                call append_argument("-t")
            end if

            if (message_interactive) then
                destination = make_buffer_reference(errbuf)
            else
                destination = value_type(0_int64)
            end if

            buffers(current)%directory = "/"
            call execute_process_region(sendmail_program, result, status, point_minimum(), point_maximum(), .false., &
                                        destination, make_list(argv))
            if (.not. status%ok()) exit body

            select case (vtag(result))
            case (V_INTEGER)
                if (result%a(1)%i /= 0) then
                    failed = .true.
                    write (exit_text, '("Sending...failed with exit value ", i0)') result%a(1)%i
                    call set_error(status, GENERIC_ERROR_CODE, trim(exit_text))
                    exit body
                end if
            case (V_STRING)
                failed = .true.
                call set_error(status, GENERIC_ERROR_CODE, "Format specifier doesn't match argument type")
                exit body
            case default
                continue
            end select

            if (message_interactive) then
                call set_buffer(errbuf, status)
                call goto_character(point_minimum())

                do while (search_regular_expression(LF//"+ *", status))
                    call replace_match("; ", status)
                end do

                if (buffer_size() /= 0) then
                    errors = buffer_string()
                    call set_error(status, GENERIC_ERROR_CODE, "Sending...failed to "//errors)
                end if

                call set_buffer(mailbuf, st)
            end if
        end block body

        case_fold_search%value = saved_fold
        if (delimline%index > 0) call destroy_marker(delimline, st)

        if (message_interactive) then
            if (failed .and. is_buffer_live(errbuf)) then
                call pop_to_buffer(errbuf, st)
                call set_buffer(mailbuf, st)
            else if (is_buffer_live(errbuf)) then
                call close_buffer(errbuf, st)
            end if
        end if

    contains

        subroutine append_argument(argument)
            character(len=*), intent(in) :: argument

            item = value_type(text=argument)
            argv = [argv, item]
        end subroutine append_argument
    end subroutine send_message_mail_with_sendmail

    !> Run `message_send_mail_hook', then send the current buffer with
    !> `send_mail_function'.  Port of message.el:message-use-send-mail-function.
    recursive subroutine send_message_mail_with_send_mail_function(status)
        type(status_type), intent(out) :: status

        call message_send_mail_hook%execute(status)
        if (status%ok()) call send_mail_function(status)
    end subroutine send_message_mail_with_send_mail_function

    !> The procedure that sends a message as `send_mail_function' says:
    !> `query_sendmail_once' asks, `send_mail_with_sendmail' becomes
    !> `send_message_mail_with_sendmail', and any other is called by
    !> `send_message_mail_with_send_mail_function'.  Port of
    !> message.el:message-default-send-mail-function (feedmail.el is not
    !> ported).
    subroutine get_message_default_send_mail_function(chosen)
        procedure(command_procedure), pointer, intent(out) :: chosen

        if (associated(send_mail_function, query_sendmail_once)) then
            chosen => query_sendmail_once
        else if (associated(send_mail_function, send_mail_with_sendmail)) then
            chosen => send_message_mail_with_sendmail
        else
            chosen => send_message_mail_with_send_mail_function
        end if
    end subroutine get_message_default_send_mail_function

    !> Use the setting of `send_mail_function' if applicable: the default
    !> of `message_send_mail_function'.  Port of
    !> message.el:message--default-send-mail-function.
    recursive subroutine send_message_mail_by_default(status)
        type(status_type), intent(out) :: status
        procedure(command_procedure), pointer :: chosen

        call get_message_default_send_mail_function(chosen)
        call chosen(status)
    end subroutine send_message_mail_by_default

    ! ------------------------------------------------------------------
    ! Fcc
    ! ------------------------------------------------------------------

    ! Process Fcc headers in the current buffer.  Port of
    ! message.el:message-do-fcc.
    recursive subroutine do_message_fcc(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: mail_buffer, old, temporary
        type(restriction_type) :: restriction
        type(string_type), allocatable :: files(:)
        character(len=:), allocatable :: file, cache, expanded, directory, text
        type(value_type) :: result
        logical :: saved_fold
        integer :: k

        call get_message_field_value("fcc", file, status, first=.true.)
        if (.not. status%ok() .or. .not. allocated(file)) return
        mail_buffer = current_buffer()
        k = local_index()

        if (k > 0) then
            if (allocated(locals(k)%encoded_mail_cache)) cache = locals(k)%encoded_mail_cache
        end if

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call buffer_substring_no_properties(point_minimum(), point_maximum(), text, status)
        if (.not. status%ok()) return
        call temporary_buffer_begin(" *temp*", old, temporary, status)

        body: block
            if (.not. status%ok()) exit body

            ! Avoid re-doing things like GPG-encoding secret parts.
            if (.not. allocated(cache)) then
                call insert(text, status)
                if (status%ok()) call encode_message_body(status)
            else
                call insert(cache, status)
            end if

            if (.not. status%ok()) exit body
            call save_restriction_begin(restriction)
            call narrow_to_message_headers(status)
            allocate (files(0))

            do while (status%ok())
                call get_message_field("fcc", file, status, first=.true.)
                if (.not. allocated(file)) exit
                call prepend_string(files, file)
                call remove_message_header("fcc", status, first=.true.)
            end do

            if (status%ok()) call encode_rfc2047_message_header(status)
            call save_restriction_end(restriction)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())

            if (search_regular_expression("^"//regular_expression_quote(mail_header_separator)//"$", status)) then
                call replace_match("", status, fixedcase=.true., literal=.true.)
            end if

            if (.not. status%ok()) exit body

            ! Process Fcc operations.
            do k = 1, size(files)
                file = files(k)%text

                if (string_match("^[ "//TAB//"]*|[ "//TAB//"]*\(.*\)[ "//TAB//"]*$", file, status) > 0) then
                    ! Pipe the article to the program in question.
                    call pipe_to_program(match_string(1, file), result, status)
                else
                    ! Save the article.
                    call expand_file_name(file, expanded, status)
                    if (.not. status%ok()) exit body
                    directory = file_name_directory(expanded)

                    if (.not. file_exists(directory)) then
                        call create_directory(directory, status, parents=.true.)
                        if (.not. status%ok()) exit body
                    end if

                    if (associated(message_fcc_handler_function)) then
                        call message_fcc_handler_function(expanded, status)
                    else
                        call set_error(status, VOID_FUNCTION_CODE, "output_rmail_message")
                    end if
                end if

                if (.not. status%ok()) exit body
            end do
        end block body

        call temporary_buffer_end(old, temporary)
        call restore_buffer(mail_buffer)
        case_fold_search%value = saved_fold

    contains

        ! call-shell-region: the buffer as the input of COMMAND, run by the
        ! shell, its output discarded.  Not a port.
        recursive subroutine pipe_to_program(command, result, status)
            use execute_process_mod, only: shell_file_name
            use simple_mod, only: shell_command_switch
            use value_mod, only: list
            character(len=*), intent(in) :: command
            type(value_type), intent(out) :: result
            type(status_type), intent(out) :: status

            call execute_process_region(shell_file_name%value, result, status, point_minimum(), point_maximum(), .false., &
                                        value_type(), list(value_type(text=shell_command_switch%value), &
                                                           value_type(text=command)))
        end subroutine pipe_to_program
    end subroutine do_message_fcc

    !> Append this article to Unix/babyl mail file FILENAME.  Port of
    !> message.el:message-output (a Babyl file, which needs
    !> gnus-rmail.el's gnus-output-to-rmail, is a `void_function' error
    !> condition).
    recursive subroutine append_message_to_mail_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical :: babyl

        babyl = .false.

        if (is_file_readable(filename)) then
            babyl = is_mail_file_babyl(filename, status)
            if (.not. status%ok()) return
        end if

        if (babyl) then
            call set_error(status, VOID_FUNCTION_CODE, "gnus_output_to_rmail")
        else
            call output_to_mail_file(filename, .true., status)
        end if
    end subroutine append_message_to_mail_file

    ! Append the current article to a mail file named FILENAME; with ASK,
    ! ask before creating it.  Port of gnus-util.el:gnus-output-to-mail.
    recursive subroutine output_to_mail_file(filename_in, ask_first, status)
        character(len=*), intent(in) :: filename_in
        logical, intent(in) :: ask_first
        type(status_type), intent(out) :: status
        type(handle_type) :: artbuf, tmpbuf, outbuf, old
        character(len=:), allocatable :: filename, text
        logical :: proceed, saved_fold
        integer(int64) :: moved

        call expand_file_name(filename_in, filename, status)
        if (.not. status%ok()) return
        artbuf = current_buffer()
        outbuf = get_file_buffer(filename)

        ! Create the file, if it doesn't exist.
        if (outbuf%index == 0 .and. .not. file_exists(filename)) then
            proceed = .true.
            if (ask_first) proceed = ask(""""//filename//""" does not exist, create it? ", status)
            if (.not. status%ok()) return

            if (.not. proceed) then
                call set_error(status, GENERIC_ERROR_CODE, "Output file does not exist")
                return
            end if

            call temporary_buffer_begin(" *temp*", old, tmpbuf, status)
            if (status%ok()) call write_region(filename, .false., status, point_minimum(), point_maximum())
            call temporary_buffer_end(old, tmpbuf)
            if (.not. status%ok()) return
        end if

        call buffer_substring_no_properties(point_minimum(), point_maximum(), text, status)
        if (.not. status%ok()) return
        call temporary_buffer_begin(" *Gnus-output*", old, tmpbuf, status)

        body: block
            if (.not. status%ok()) exit body
            call insert(text, status)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())

            if (looking_at("From ", status)) then
                moved = forward_line(1_int64)
            else
                call insert("From nobody "//current_time_string()//LF, status)
                if (.not. status%ok()) exit body
            end if

            saved_fold = case_fold_search%value
            case_fold_search%value = .false.

            do while (search_regular_expression("^From ", status))
                call beginning_of_line()
                call insert(">", status)
                if (.not. status%ok()) exit
                moved = forward_line(1_int64)
            end do

            case_fold_search%value = saved_fold
            if (.not. status%ok()) exit body

            ! Decide whether to append to a file or to a buffer.
            if (outbuf%index == 0) then
                call goto_character(point_maximum() - 2)

                if (.not. looking_at(LF//LF, status)) then
                    call goto_character(point_maximum())
                    if (.not. is_beginning_of_line()) call insert(LF, status)
                    if (status%ok()) call insert(LF, status)
                end if

                if (status%ok()) call write_region(filename, .false., status, point_minimum(), point_maximum(), append=.true.)
            else
                ! File has been visited, in buffer OUTBUF.
                text = buffer_string()
                call set_buffer(outbuf, status)
                if (.not. status%ok()) exit body
                call goto_character(point_maximum())
                if (.not. is_end_of_buffer()) call insert(LF, status)
                if (status%ok()) call insert(LF//text, status)
            end if
        end block body

        call temporary_buffer_end(old, tmpbuf)
        call restore_buffer(artbuf)
    end subroutine output_to_mail_file

    ! ------------------------------------------------------------------
    ! After sending
    ! ------------------------------------------------------------------

    ! Close old message buffers and rename this one.  Port of
    ! message.el:message-do-send-housekeeping.
    recursive subroutine do_send_message_housekeeping(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        logical :: modified

        if (.not. allocated(message_buffer_list)) allocate (message_buffer_list(0))
        ! We might have sent this buffer already.  Delete it from the
        ! list of buffers.
        message_buffer_list = pack(message_buffer_list,.not. same_buffer(message_buffer_list, current_buffer()))

        do while (message_maximum_buffers > 0 .and. size(message_buffer_list) >= message_maximum_buffers)
            ! Close the oldest buffer -- unless it has been changed.
            buffer = message_buffer_list(1)
            message_buffer_list = message_buffer_list(2:)

            if (is_buffer_live(buffer)) then
                call is_buffer_modified(buffer, modified, status)
                if (.not. status%ok()) return
                if (.not. modified) call close_buffer(buffer, status)
                if (.not. status%ok()) return
            end if
        end do

        ! Rename the buffer.
        call message_send_rename_function(status)
        if (.not. status%ok()) return
        ! Push the current buffer onto the list.
        if (message_maximum_buffers > 0) message_buffer_list = [message_buffer_list, current_buffer()]

    contains

        ! Whether each of LIST is BUFFER.  Not a port.
        elemental logical function same_buffer(list, buffer)
            type(handle_type), intent(in) :: list, buffer

            same_buffer = list%index == buffer%index .and. list%generation == buffer%generation
        end function same_buffer
    end subroutine do_send_message_housekeeping

    !> Rename the buffer of a sent message: "*sent mail to NAME*",
    !> "*sent mail*" or "*sent NAME*".  The default
    !> `message_send_rename_function'.  Port of
    !> message.el:message-default-send-rename-function.
    recursive subroutine rename_sent_message_buffer(status)
        type(status_type), intent(out) :: status
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: bname, name, to, group, actual

        status = status_type()
        bname = buffers(current)%name
        if (string_match("\`\*\(sent \|unsent \)?\(.+\)\*[^\*]*\|\`mail to ", bname, status) == 0) return
        name = match_string(2, bname)

        if (len(name) > 0 .and. name /= "mail" .and. name /= "posting") then
            name = "*sent "//name//"*"
        else
            call narrow_to_message_headers(status)
            if (status%ok()) call get_message_field("to", to, status)
            if (status%ok()) call get_message_field("newsgroups", group, status)
            call widen()
            if (.not. status%ok()) return

            if (allocated(to)) then
                call extract_mail_address_components(to, components, status)
                if (.not. status%ok()) return
                name = "*sent mail to "//to//"*"

                if (size(components) > 0) then
                    if (allocated(components(1)%full_name)) name = "*sent mail to "//components(1)%full_name//"*"
                end if
            else if (allocated(group)) then
                if (len(group) > 0) then
                    name = "*sent posting on "//group//"*"
                else
                    name = "*sent mail*"
                end if
            else
                name = "*sent mail*"
            end if
        end if

        if (name /= bname) call rename_buffer(name, .true., actual, status)
    end subroutine rename_sent_message_buffer

    ! ------------------------------------------------------------------
    ! Editing commands
    ! ------------------------------------------------------------------

    !> Insert four newlines, and then reformat if inside quoted text.
    !> JUSTIFY (a DEFAULT_JUSTIFICATION_ constant, interactively full
    !> justification with a prefix arg) means justify as well.
    !>
    !> This function tries to guess what the quote prefix is based on
    !> the text on the current line before point.  If point is at the
    !> start of the line, the formatted text (if any) is filled without
    !> a quote prefix.  With NOT_BREAK, only fill the paragraph.  Port of
    !> message.el:message-newline-and-reformat.
    recursive subroutine newline_and_reformat_message(status, justify, not_break)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify
        logical, intent(in), optional :: not_break
        type(restriction_type) :: restriction
        character(len=:), allocatable :: quoted, leading_space, saved_regular_expression, saved_first, space
        integer(int64) :: original_point, beginning, e, after, moved, newpoint
        logical :: at_line_beginning, no_break, has_quote, had_regular_expression, had_first
        type(status_type) :: st

        status = status_type()
        no_break = .false.
        if (present(not_break)) no_break = not_break

        if (.not. is_message_in_body()) then
            call set_error(status, GENERIC_ERROR_CODE, "This command only works in the body of the message")
            return
        end if

        original_point = point()
        call beginning_of_line()
        beginning = point()
        at_line_beginning = beginning == original_point

        ! Find first line of the paragraph.
        if (no_break) then
            do while (.not. is_end_of_buffer())
                if (looking_at(message_cite_prefix_regular_expression, st)) exit
                if (.not. looking_at(paragraph_start%value, st)) exit
                moved = forward_line(1_int64)
            end do
        end if

        ! Find the prefix
        has_quote = looking_at(message_cite_prefix_regular_expression, st)
        leading_space = ""

        if (has_quote) then
            quoted = match_string(0)
            call goto_character(match_end(0, st))
            after = point()

            ! This is a line with no text after the cite prefix.  In that
            ! case, the trailing space is commonly not present, so look
            ! around for other lines that have some data.
            if (looking_at(LF, st)) then
                if (search_regular_expression_backward("^"//message_cite_prefix_regular_expression//"[ "//TAB//"]", st)) then
                    call goto_character(match_end(0, st) - 1)
                else if (search_regular_expression("^"//message_cite_prefix_regular_expression//"[ "//TAB//"]", st)) then
                    call goto_character(match_end(0, st) - 1)
                end if
            end if

            if (looking_at("[ "//TAB//"]*", st)) leading_space = match_string(0)
            call goto_character(after)
        end if

        if (has_quote .and. .not. no_break .and. .not. at_line_beginning) then
            ! break inside the cite prefix.
            if (original_point - beginning < count_characters(quoted)) has_quote = .false.
        end if

        if (has_quote) then
            moved = forward_line(1_int64)

            do while (.not. is_end_of_buffer())
                if (looking_at(paragraph_separate%value, st)) exit
                if (.not. looking_at(message_cite_prefix_regular_expression, st)) exit
                if (match_string(0) /= quoted) exit
                call goto_character(match_end(0, st))
                space = ""
                if (looking_at("[ "//TAB//"]*", st)) space = match_string(0)
                if (len(leading_space) > len(space)) leading_space = space
                moved = forward_line(1_int64)
            end do

            e = point()
            call goto_character(beginning)

            do while (.not. is_beginning_of_buffer())
                moved = forward_line(-1_int64)
                if (looking_at(paragraph_start%value, st)) exit
                if (.not. looking_at(message_cite_prefix_regular_expression, st)) exit
                if (match_string(0) /= quoted) exit
                beginning = point()
                call goto_character(match_end(0, st))
                space = ""
                if (looking_at("[ "//TAB//"]*", st)) space = match_string(0)
                if (len(leading_space) > len(space)) leading_space = space
            end do
        else
            do while (.not. is_end_of_buffer())
                if (looking_at(paragraph_separate%value, st)) exit
                if (looking_at(message_cite_prefix_regular_expression, st)) exit
                moved = forward_line(1_int64)
            end do

            e = point()
            call goto_character(beginning)

            do while (.not. is_beginning_of_buffer())
                moved = forward_line(-1_int64)
                if (looking_at(paragraph_start%value, st)) exit
                if (looking_at(message_cite_prefix_regular_expression, st)) exit
                beginning = point()
            end do
        end if

        call goto_character(original_point)
        call save_restriction_begin(restriction)

        body: block
            call narrow_to_region(beginning, e, status)
            if (.not. status%ok()) exit body
            newpoint = 0

            if (.not. no_break) then
                call newline(status)
                if (status%ok() .and. .not. at_line_beginning) call newline(status)
                if (.not. status%ok()) exit body
                newpoint = point()
                ! (newline 2) doesn't mark both newline's as hard, so call
                ! newline twice. -jas
                call newline(status)
                if (status%ok()) call newline(status)
                if (.not. status%ok()) exit body
                e = point()
                if (search_regular_expression("[ "//TAB//"]*", status)) call delete_region(e, point(), status)
                if (.not. status%ok()) exit body
                if (has_quote .and. .not. at_line_beginning) call insert(quoted//leading_space, status)
                if (.not. status%ok()) exit body
            end if

            call undo_boundary()

            if (has_quote) then
                had_regular_expression = allocated(adaptive_fill_regular_expression)
                if (had_regular_expression) saved_regular_expression = adaptive_fill_regular_expression
                had_first = allocated(adaptive_fill_first_line_regular_expression)
                if (had_first) saved_first = adaptive_fill_first_line_regular_expression
                adaptive_fill_regular_expression = regular_expression_quote(quoted//leading_space)
                adaptive_fill_first_line_regular_expression = adaptive_fill_regular_expression
                call fill_paragraph(status, justify)

                if (had_regular_expression) then
                    adaptive_fill_regular_expression = saved_regular_expression
                else
                    deallocate (adaptive_fill_regular_expression)
                end if

                if (had_first) then
                    adaptive_fill_first_line_regular_expression = saved_first
                else
                    deallocate (adaptive_fill_first_line_regular_expression)
                end if
            else
                call fill_paragraph(status, justify)
            end if

            if (status%ok() .and. newpoint > 0) call goto_character(newpoint)
        end block body

        call save_restriction_end(restriction)

    contains

        ! The number of characters of TEXT (UTF-8).  Not a port.
        integer(int64) function count_characters(text)
            character(len=*), intent(in) :: text
            integer :: i

            count_characters = 0

            do i = 1, len(text)
                if (iand(iachar(text(i:i)), 192) /= 128) count_characters = count_characters + 1
            end do
        end function count_characters
    end subroutine newline_and_reformat_message

    !> Insert header to mark message as important (IMPORTANCE "high") or
    !> unimportant ("low").  Port of message.el:message-insert-importance-high
    !> and message.el:message-insert-importance-low.
    recursive subroutine insert_message_importance(importance, status)
        character(len=*), intent(in) :: importance
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call remove_message_header("Importance", status)
        call save_restriction_end(restriction)

        if (status%ok()) then
            call goto_message_end_of_headers()
            call insert("Importance: "//importance//LF, status)
        end if

        call save_excursion_end(excursion)
    end subroutine insert_message_importance

    !> Insert a "Importance: high" header, or cycle through the header
    !> values.  The three allowed values according to RFC 1327 are `high',
    !> `normal' and `low'.  Port of
    !> message.el:message-insert-or-toggle-importance.
    recursive subroutine insert_or_toggle_message_importance(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: new, current_value

        new = "high"
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call get_message_field("Importance", current_value, status)

        if (status%ok() .and. allocated(current_value)) then
            call remove_message_header("Importance", status)

            select case (current_value)
            case ("high")
                new = "low"
            case ("low")
                new = "normal"
            case default
                new = "high"
            end select
        end if

        call save_restriction_end(restriction)

        if (status%ok()) then
            call goto_message_end_of_headers()
            call insert("Importance: "//new//LF, status)
        end if

        call save_excursion_end(excursion)
    end subroutine insert_or_toggle_message_importance

    !> Request a disposition notification (return receipt) to this message.
    !> Note that this should not be used in newsgroups.  Port of
    !> message.el:message-insert-disposition-notification-to.
    recursive subroutine insert_message_disposition_notification_to(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: address

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call remove_message_header("Disposition-Notification-To", status)
        call save_restriction_end(restriction)

        body: block
            if (.not. status%ok()) exit body
            call goto_message_end_of_headers()
            call get_message_field_value("Reply-To", address, status)
            if (status%ok() .and. .not. allocated(address)) call get_message_field_value("From", address, status)
            if (status%ok() .and. .not. allocated(address)) call make_message_from(address, status)
            if (status%ok()) call insert("Disposition-Notification-To: "//address//LF, status)
        end block body

        call save_excursion_end(excursion)
    end subroutine insert_message_disposition_notification_to

    !> Insert the Expires header.  Expiry in DAYS days.  Port of
    !> message.el:message-insert-expires.
    recursive subroutine insert_message_expires(days, status)
        integer(int64), intent(in) :: days
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        character(len=:), allocatable :: expires

        call save_excursion_begin(excursion)
        call position_message_on_field("Expires", status, [character(len=12) :: "X-Draft-From"])

        if (status%ok()) then
            expires = make_message_expires_date(days, status)
            if (status%ok()) call insert(expires, status)
        end if

        call save_excursion_end(excursion)
    end subroutine insert_message_expires

    !> Generate the headers for the article.  Port of
    !> message.el:message-insert-headers (the headers of news are Deferred).
    recursive subroutine insert_message_headers(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(message_header_type), allocatable :: headers(:)
        integer :: k

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)

        if (status%ok() .and. is_message_mail()) then
            allocate (headers(0))

            do k = 1, size(message_required_mail_headers)
                select case (trim(message_required_mail_headers(k)%name))
                case ("Lines", "Subject")
                    continue
                case default
                    headers = [headers, message_required_mail_headers(k)]
                end select
            end do

            call generate_message_headers(headers, status)
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine insert_message_headers

    !> Replace contents of To: header with contents of Cc: or Bcc: header.
    !> Port of message.el:message-reduce-to-to-cc.
    recursive subroutine reduce_message_to_to_cc(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: cc_content
        logical :: bcc

        bcc = .false.
        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call get_message_field("cc", cc_content, status)

        if (status%ok() .and. .not. allocated(cc_content)) then
            call get_message_field("bcc", cc_content, status)
            bcc = allocated(cc_content)
        end if

        call save_restriction_end(restriction)
        if (.not. status%ok() .or. .not. allocated(cc_content)) return
        call save_excursion_begin(excursion)

        body: block
            call push_mark()
            call position_message_on_field("To", status)
            if (.not. status%ok()) exit body
            call delete_region(line_beginning_position(), line_beginning_position(2_int64), status)
            if (status%ok()) call insert("To: "//cc_content//LF, status)
            if (.not. status%ok()) exit body
            call save_restriction_begin(restriction)
            call narrow_to_message_headers(status)

            if (status%ok()) then
                if (bcc) then
                    call remove_message_header("bcc", status)
                else
                    call remove_message_header("cc", status)
                end if
            end if

            call save_restriction_end(restriction)
        end block body

        call save_excursion_end(excursion)
    end subroutine reduce_message_to_to_cc

    !> Insert "X-No-Archive: Yes" in the header and a note in the body.
    !> The note can be customized using `message_archive_note'.  When
    !> NOTE is present (interactively with a prefix argument, asked for),
    !> it becomes the note.  If you don't want the note in the body, set
    !> `message_archive_note' unallocated.  Port of
    !> message.el:message-add-archive-header.
    recursive subroutine add_message_archive_header(status, note)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: note
        type(excursion_type) :: excursion
        logical :: found

        if (present(note)) message_archive_note = note
        call save_excursion_begin(excursion)

        body: block
            call goto_message_signature(found)

            if (found) then
                if (.not. search_regular_expression_backward(message_signature_separator, status)) then
                    if (status%ok()) call set_error(status, GENERIC_ERROR_CODE, "Search failed: """// &
                                                    message_signature_separator//"""")
                    exit body
                end if
            end if

            if (allocated(message_archive_note)) then
                call insert(message_archive_note, status)
                if (status%ok()) call newline(status)
                if (.not. status%ok()) exit body
            end if

            call add_message_header(message_archive_header, status)
            if (status%ok()) call sort_message_headers(status)
        end block body

        call save_excursion_end(excursion)
    end subroutine add_message_archive_header

    !> Mark some region in the current article with enclosing tags.
    !> See `message_mark_insert_begin' and `message_mark_insert_end'.
    !> If VERBATIM, use slrn style verbatim marks ("#v+" and "#v-").
    !> Port of message.el:message-mark-inserted-region.
    recursive subroutine mark_message_inserted_region(beginning, end, status, verbatim)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: verbatim
        type(excursion_type) :: excursion
        logical :: verb

        verb = .false.
        if (present(verbatim)) verb = verbatim
        call save_excursion_begin(excursion)
        ! add to the end of the region first, otherwise end would be invalid
        call goto_character(end)
        status = status_type()
        if (.not. is_beginning_of_line()) call insert(LF, status)

        if (status%ok()) then
            if (verb) then
                call insert("#v-"//LF, status)
            else
                call insert(message_mark_insert_end, status)
            end if
        end if

        if (status%ok()) then
            call goto_character(beginning)

            if (verb) then
                call insert("#v+"//LF, status)
            else
                call insert(message_mark_insert_begin, status)
            end if
        end if

        call save_excursion_end(excursion)
    end subroutine mark_message_inserted_region

    !> Insert FILE at point, marking it with enclosing tags.
    !> See `message_mark_insert_begin' and `message_mark_insert_end'.
    !> If VERBATIM, use slrn style verbatim marks ("#v+" and "#v-").
    !> Port of message.el:message-mark-insert-file.
    recursive subroutine mark_message_insert_file(file, status, verbatim)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: verbatim
        integer(int64) :: p, inserted
        logical :: verb

        verb = .false.
        if (present(verbatim)) verb = verbatim
        ! reverse insertion to get correct result.
        p = point()

        if (verb) then
            call insert("#v-"//LF, status)
        else
            call insert(message_mark_insert_end, status)
        end if

        if (.not. status%ok()) return
        call goto_character(p)
        call insert_file_contents(file, .false., inserted, status)
        if (.not. status%ok()) return
        call goto_character(p)

        if (verb) then
            call insert("#v+"//LF, status)
        else
            call insert(message_mark_insert_begin, status)
        end if
    end subroutine mark_message_insert_file

    !> Change subject to NEW_SUBJECT with "(was: <Old Subject>)" suffix.
    !> If NEW_SUBJECT is absent, the user is prompted for the new subject,
    !> with the old subject as the default.  Port of
    !> message.el:message-change-subject.
    recursive subroutine change_message_subject(status, new_subject)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: new_subject
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: old_subject, subject

        call save_restriction_begin(restriction)
        call narrow_to_message_headers(status)
        if (status%ok()) call get_message_field("Subject", old_subject, status)
        call save_restriction_end(restriction)
        if (.not. status%ok()) return

        if (.not. allocated(old_subject)) then
            call set_error(status, GENERIC_ERROR_CODE, "No current subject")
            return
        end if

        if (present(new_subject)) then
            subject = new_subject
        else
            call read_from_minibuffer("New subject: ", subject, status, default=old_subject)
            if (.not. status%ok()) return
        end if

        ! Abort on empty subject.
        if (len(subject) == 0 .or. matches("^[ "//TAB//"]*$", subject)) then
            call message("Subject empty")
        else if (matches("^[ "//TAB//"]*"//regular_expression_quote(subject)//"[ "//TAB//"]*$", old_subject)) then
            ! Abort on unchanged subject.
            call message("Subject unchanged")
        else
            ! Otherwise, proceed.
            call save_excursion_begin(excursion)
            call push_mark()
            call position_message_on_field("Subject", status)

            if (status%ok()) then
                call delete_region(line_beginning_position(), line_beginning_position(2_int64), status)
                if (status%ok()) call insert("Subject: "//subject//" (was: "//strip_message_subject_re_prefix(old_subject)//")"// &
                                             LF, status)
            end if

            call save_excursion_end(excursion)
        end if
    end subroutine change_message_subject

    !> Elide the text in the region from BEGINNING to END.
    !> An ellipsis (from `message_elide_ellipsis') will be inserted where
    !> the text was cut.  Port of message.el:message-elide-region.
    recursive subroutine elide_message_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: ellipsis
        character(len=24) :: number
        integer(int64) :: lines, chars
        integer :: k

        lines = count_lines(beginning, end)
        chars = end - beginning
        call cut_region(beginning, end, status)
        if (.not. status%ok()) return
        ! format-spec with %l and %c.
        ellipsis = ""
        k = 1

        do while (k <= len(message_elide_ellipsis))
            if (message_elide_ellipsis(k:k) == "%" .and. k < len(message_elide_ellipsis)) then
                select case (message_elide_ellipsis(k + 1:k + 1))
                case ("l")
                    write (number, "(i0)") lines
                    ellipsis = ellipsis//trim(number)
                case ("c")
                    write (number, "(i0)") chars
                    ellipsis = ellipsis//trim(number)
                case ("%")
                    ellipsis = ellipsis//"%"
                case default
                    call set_error(status, GENERIC_ERROR_CODE, "Invalid format character: `%"// &
                                   message_elide_ellipsis(k + 1:k + 1)//"'")
                    return
                end select

                k = k + 2
            else
                ellipsis = ellipsis//message_elide_ellipsis(k:k)
                k = k + 1
            end if
        end do

        call insert(ellipsis, status)
    end subroutine elide_message_region

    !> Delete everything in the body of the current message outside of the
    !> region from BEGINNING to END.  Port of
    !> message.el:message-delete-not-region.
    recursive subroutine delete_message_not_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        character(len=:), allocatable :: citeprefix
        integer(int64) :: moved, body, e
        logical :: found
        type(status_type) :: st

        call save_excursion_begin(excursion)

        body_block: block
            call goto_character(beginning)

            ! snarf citation prefix, if appropriate
            if (point() /= line_beginning_position()) then
                call beginning_of_line()
                if (looking_at(message_cite_prefix_regular_expression, st)) citeprefix = match_string(0)
            end if

            call goto_character(end)
            e = point()
            call goto_message_signature(found)

            if (found) then
                moved = forward_line(-2_int64)
            else
                call goto_character(e)
            end if

            call delete_region(e, point(), status)
            if (.not. status%ok()) exit body_block
            call goto_character(min(e, point()))
            call insert(LF, status)
            if (.not. status%ok()) exit body_block
            call goto_character(beginning)
            call goto_message_body(body)
            moved = forward_line(2_int64)
            call delete_region(beginning, point(), status)
            if (.not. status%ok()) exit body_block
            if (allocated(citeprefix)) call insert(citeprefix, status)
        end block body_block

        call save_excursion_end(excursion)
        if (.not. status%ok()) return
        call goto_message_signature(found)
        if (found) moved = forward_line(-2_int64)
    end subroutine delete_message_not_region

    !> Cut all text up to the signature.
    !> If a numeric argument ARGUMENT is given, leave that number of lines
    !> before the signature intact.  Port of message.el:message-kill-to-signature.
    recursive subroutine cut_message_to_signature(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        integer(int64) :: original_point, moved
        logical :: found

        status = status_type()
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)

        body: block
            original_point = point()
            call narrow_to_region(original_point, point_maximum(), status)
            if (.not. status%ok()) exit body
            call goto_message_signature(found)

            if (.not. is_end_of_buffer()) then
                if (present(argument)) then
                    moved = forward_line(-1 - argument)
                else
                    call end_of_line(-1_int64)
                end if
            end if

            if (original_point /= point()) then
                call cut_region(original_point, point(), status)
                if (status%ok() .and. .not. is_beginning_of_line()) call insert(LF, status)
            end if
        end block body

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine cut_message_to_signature

    !> Cut the address under point.  Port of message.el:message-kill-address.
    recursive subroutine cut_message_address(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start

        start = point()
        call skip_to_next_message_address(status)
        if (.not. status%ok()) return

        if (is_beginning_of_line()) then
            call cut_region(start, point() - 1, status)
        else
            call cut_region(start, point(), status)
        end if
    end subroutine cut_message_address

    !> Rename the *message* buffer to "*message* RECIPIENT".
    !> If ENTER_STRING (interactively, with a prefix), ask for a new buffer
    !> name, rather than giving an automatic name.  Port of
    !> message.el:message-rename-buffer.
    recursive subroutine rename_message_buffer(status, enter_string)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: enter_string
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        character(len=:), allocatable :: mail_to, trimmed_to, name_default, name, actual
        logical :: news, enter
        integer(int64) :: k

        enter = .false.
        if (present(enter_string)) enter = enter_string
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)

        body: block
            call goto_character(point_minimum())
            if (.not. search_string(mail_header_separator, status, move=.true.)) continue
            call narrow_to_region(point_minimum(), point(), status)
            if (.not. status%ok()) exit body
            call is_message_news(news, status)
            if (.not. status%ok()) exit body

            if (news) then
                call get_message_field("Newsgroups", mail_to, status)
            else
                call get_message_field("To", mail_to, status)
            end if

            if (.not. status%ok()) exit body
            if (.not. allocated(mail_to)) mail_to = ""
            k = index(mail_to, ",")

            if (k > 0) then
                trimmed_to = mail_to(:k - 1)//", ..."
            else
                trimmed_to = mail_to
            end if

            name_default = "*message* "//trimmed_to

            if (enter) then
                call read_string("New buffer name: ", name, status, default=name_default)
                if (.not. status%ok()) exit body
            else
                name = name_default
            end if

            call rename_buffer(name, .true., actual, status)
        end block body

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine rename_message_buffer

    !> Move point to beginning of header value or to beginning of line.
    !> The argument N is passed directly to `beginning_of_line'.
    !>
    !> This command is identical to `beginning_of_line' if point is
    !> outside the message header or if the option
    !> `message_beginning_of_line_variable' is .false..
    !>
    !> If point is in the message header and on a header line, move
    !> point to the beginning of the header value or the beginning of
    !> line, whichever is closer.  If point is already at beginning of
    !> line, move point to beginning of header value.  Therefore,
    !> repeated calls will toggle point between beginning of field and
    !> beginning of line.  Port of message.el:message-beginning-of-line
    !> (with message-beginning-of-header; Visual Line mode is Deferred).
    recursive subroutine message_beginning_of_line(n)
        integer(int64), intent(in), optional :: n
        integer(int64) :: original_point, line_beginning, boh, moved
        logical :: found
        type(status_type) :: st

        if (message_beginning_of_line_variable .and. is_message_point_in_header()) then
            original_point = point()
            call beginning_of_line(n)
            line_beginning = point()
            ! message-beginning-of-header with HANDLE-FOLDED nil.
            call beginning_of_line()
            found = following_character() == iachar(" ") .or. following_character() == 9

            if (.not. found) then
                if (search_string(":", st, line_end_position())) found = .true.
            end if

            boh = 0

            if (found) then
                moved = skip_characters_forward(" "//TAB, st, line_end_position())
                boh = point()
            end if

            if (boh > 0 .and. (boh < original_point .or. line_beginning == original_point)) then
                call goto_character(boh)
            else
                call goto_character(line_beginning)
            end if
        else
            call beginning_of_line(n)
        end if
    end subroutine message_beginning_of_line

    !> Widen the buffer and go to the start.  Port of
    !> message.el:message-widen-and-recenter.
    recursive subroutine widen_and_recenter_message(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call widen()
        call goto_character(point_minimum())
    end subroutine widen_and_recenter_message

    ! The field commands: push the mark and move to the field (inserted
    ! after the others named if it is missing).  Not a port: the
    ! shared body of message.el's message-goto-to and the others.
    recursive subroutine goto_field(header, status, afters)
        character(len=*), intent(in) :: header
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: afters(:)

        call push_mark()
        call position_message_on_field(header, status, afters)
    end subroutine goto_field

    !> Move point to the To header or insert an empty one.  Port of
    !> message.el:message-goto-to.
    recursive subroutine goto_message_to(status)
        type(status_type), intent(out) :: status

        call goto_field("To", status)
    end subroutine goto_message_to

    !> Move point to the From header or insert an empty one.  Port of
    !> message.el:message-goto-from.
    recursive subroutine goto_message_from(status)
        type(status_type), intent(out) :: status

        call goto_field("From", status)
    end subroutine goto_message_from

    !> Move point to the Subject header or insert an empty one.  Port of
    !> message.el:message-goto-subject.
    recursive subroutine goto_message_subject(status)
        type(status_type), intent(out) :: status

        call goto_field("Subject", status)
    end subroutine goto_message_subject

    !> Move point to the Cc header or insert an empty one.  Port of
    !> message.el:message-goto-cc.
    recursive subroutine goto_message_cc(status)
        type(status_type), intent(out) :: status

        call goto_field("Cc", status, [character(len=2) :: "To"])
    end subroutine goto_message_cc

    !> Move point to the Bcc  header or insert an empty one.  Port of
    !> message.el:message-goto-bcc.
    recursive subroutine goto_message_bcc(status)
        type(status_type), intent(out) :: status

        call goto_field("Bcc", status, [character(len=2) :: "Cc", "To"])
    end subroutine goto_message_bcc

    !> Move point to the Fcc header or insert an empty one.  Port of
    !> message.el:message-goto-fcc.
    recursive subroutine goto_message_fcc(status)
        type(status_type), intent(out) :: status

        call goto_field("Fcc", status, [character(len=10) :: "To", "Newsgroups"])
    end subroutine goto_message_fcc

    !> Move point to the Reply-To header or insert an empty one.  Port of
    !> message.el:message-goto-reply-to.
    recursive subroutine goto_message_reply_to(status)
        type(status_type), intent(out) :: status

        call goto_field("Reply-To", status, [character(len=7) :: "Subject"])
    end subroutine goto_message_reply_to

    !> Move point to the Newsgroups header or insert an empty one.  Port of
    !> message.el:message-goto-newsgroups.
    recursive subroutine goto_message_newsgroups(status)
        type(status_type), intent(out) :: status

        call goto_field("Newsgroups", status)
    end subroutine goto_message_newsgroups

    !> Move point to the Distribution header or insert an empty one.
    !> Port of message.el:message-goto-distribution.
    recursive subroutine goto_message_distribution(status)
        type(status_type), intent(out) :: status

        call goto_field("Distribution", status)
    end subroutine goto_message_distribution

    !> Move point to the Followup-To header or insert an empty one.
    !> Port of message.el:message-goto-followup-to.
    recursive subroutine goto_message_followup_to(status)
        type(status_type), intent(out) :: status

        call goto_field("Followup-To", status, [character(len=10) :: "Newsgroups"])
    end subroutine goto_message_followup_to

    !> Move point to the Mail-Followup-To header or insert an empty one.
    !> Port of message.el:message-goto-mail-followup-to.
    recursive subroutine goto_message_mail_followup_to(status)
        type(status_type), intent(out) :: status

        call goto_field("Mail-Followup-To", status, [character(len=2) :: "To"])
    end subroutine goto_message_mail_followup_to

    !> Move point to the Keywords header or insert an empty one.  Port of
    !> message.el:message-goto-keywords.
    recursive subroutine goto_message_keywords(status)
        type(status_type), intent(out) :: status

        call goto_field("Keywords", status, [character(len=7) :: "Subject"])
    end subroutine goto_message_keywords

    !> Move point to the Summary header or insert an empty one.  Port of
    !> message.el:message-goto-summary.
    recursive subroutine goto_message_summary(status)
        type(status_type), intent(out) :: status

        call goto_field("Summary", status, [character(len=7) :: "Subject"])
    end subroutine goto_message_summary

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The `*' of an interactive spec.  Not a port.
    logical function read_only(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        read_only = .not. status%ok()
    end function read_only

    ! The region of an interactive "r" spec, false when there is none.
    ! Not a port.
    logical function get_region(beginning, end, status)
        use editing_functions_mod, only: interactive_region
        integer(int64), intent(out) :: beginning, end
        type(status_type), intent(out) :: status

        call interactive_region(beginning, end, status)
        get_region = status%ok()
    end function get_region

    recursive subroutine execute_message_mode(status)
        type(status_type), intent(out) :: status

        call message_mode(status)
    end subroutine execute_message_mode

    recursive subroutine execute_mail_message(status)
        type(status_type), intent(out) :: status

        call mail_message(status)
    end subroutine execute_mail_message

    recursive subroutine execute_mail_message_other_window(status)
        type(status_type), intent(out) :: status

        call mail_message_other_window(status)
    end subroutine execute_mail_message_other_window

    recursive subroutine execute_goto_message_body(status)
        type(status_type), intent(out) :: status
        integer(int64) :: body

        status = status_type()
        call goto_message_body(body, .true.)
    end subroutine execute_goto_message_body

    recursive subroutine execute_goto_message_end_of_headers(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call goto_message_end_of_headers(.true.)
    end subroutine execute_goto_message_end_of_headers

    recursive subroutine execute_goto_message_signature(status)
        type(status_type), intent(out) :: status
        logical :: found

        status = status_type()
        call goto_message_signature(found)
    end subroutine execute_goto_message_signature

    recursive subroutine execute_insert_message_signature(status)
        type(status_type), intent(out) :: status

        call insert_message_signature(status, interactive=.true.)
    end subroutine execute_insert_message_signature

    recursive subroutine execute_insert_message_importance_high(status)
        type(status_type), intent(out) :: status

        call insert_message_importance("high", status)
    end subroutine execute_insert_message_importance_high

    recursive subroutine execute_insert_message_importance_low(status)
        type(status_type), intent(out) :: status

        call insert_message_importance("low", status)
    end subroutine execute_insert_message_importance_low

    recursive subroutine execute_insert_message_expires(status)
        use basic_subprograms_mod, only: read_number
        type(status_type), intent(out) :: status
        integer(int64) :: days

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            days = prefix_numeric_value(current_prefix_argument)
            status = status_type()
        else
            call read_number("Expire article in how many days? ", days, status)
            if (.not. status%ok()) return
        end if

        call insert_message_expires(days, status)
    end subroutine execute_insert_message_expires

    recursive subroutine execute_add_message_archive_header(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: note

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            if (allocated(message_archive_note)) then
                call read_from_minibuffer("Reason for No-Archive: ", note, status, initial_input=message_archive_note, &
                                          position=1_int64)
            else
                call read_from_minibuffer("Reason for No-Archive: ", note, status)
            end if

            if (status%ok()) call add_message_archive_header(status, note)
        else
            call add_message_archive_header(status)
        end if
    end subroutine execute_add_message_archive_header

    recursive subroutine execute_mark_message_inserted_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        if (.not. get_region(beginning, end, status)) return
        call mark_message_inserted_region(beginning, end, status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_mark_message_inserted_region

    recursive subroutine execute_mark_message_insert_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("File to insert: ", file, status, mustmatch=REQUIRE_MATCH_YES)
        if (status%ok()) call mark_message_insert_file(file, status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_mark_message_insert_file

    recursive subroutine execute_change_message_subject(status)
        type(status_type), intent(out) :: status

        call change_message_subject(status)
    end subroutine execute_change_message_subject

    recursive subroutine execute_elide_message_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        if (.not. get_region(beginning, end, status)) return
        call elide_message_region(beginning, end, status)
    end subroutine execute_elide_message_region

    recursive subroutine execute_delete_message_not_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        if (.not. get_region(beginning, end, status)) return
        call delete_message_not_region(beginning, end, status)
    end subroutine execute_delete_message_not_region

    recursive subroutine execute_cut_message_to_signature(status)
        type(status_type), intent(out) :: status

        ! (and arg (numberp arg)): only a numeric prefix argument counts.
        if (current_prefix_argument%kind == PREFIX_NUMBER) then
            call cut_message_to_signature(status, prefix_numeric_value(current_prefix_argument))
        else
            call cut_message_to_signature(status)
        end if
    end subroutine execute_cut_message_to_signature

    recursive subroutine execute_rename_message_buffer(status)
        type(status_type), intent(out) :: status

        call rename_message_buffer(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_rename_message_buffer

    recursive subroutine execute_message_beginning_of_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call message_beginning_of_line(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_message_beginning_of_line

    recursive subroutine execute_newline_and_reformat_message(status)
        use fill_mod, only: DEFAULT_JUSTIFICATION_FULL
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call newline_and_reformat_message(status, DEFAULT_JUSTIFICATION_FULL)
        else
            call newline_and_reformat_message(status)
        end if
    end subroutine execute_newline_and_reformat_message

    recursive subroutine execute_send_message_and_exit(status)
        type(status_type), intent(out) :: status

        call send_message_and_exit(status)
    end subroutine execute_send_message_and_exit

    recursive subroutine execute_send_message(status)
        type(status_type), intent(out) :: status

        call send_message(status)
    end subroutine execute_send_message

    recursive subroutine execute_check_message_recipients(status)
        type(status_type), intent(out) :: status

        call check_message_recipients(status)
    end subroutine execute_check_message_recipients

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> Set the defaults of Message's options, define Message mode, its
    !> keymap, syntax table and commands, and `message_user_agent', the
    !> default `mail_user_agent'.  Not a port.
    subroutine initialize_message()
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k

        if (.not. allocated(message_directory)) message_directory = "~/Mail/"

        if (.not. allocated(message_ignored_mail_headers)) then
            message_ignored_mail_headers = "^\([GF]cc\|Resent-Fcc\|Xref\|X-Draft-From\|X-Gnus-Agent-Meta-Information\):"
        end if

        if (.not. allocated(message_signature_separator)) message_signature_separator = "^-- $"
        if (.not. allocated(message_elide_ellipsis)) message_elide_ellipsis = LF//"[...]"//LF//LF

        if (.not. allocated(message_deletable_headers)) then
            message_deletable_headers = [character(len=10) :: "Message-ID", "Date", "Lines"]
        end if

        if (.not. allocated(message_sendmail_extra_arguments)) allocate (character(len=0) :: message_sendmail_extra_arguments(0))
        if (.not. allocated(message_replacement_character)) message_replacement_character = "."

        if (.not. allocated(message_subject_trailing_was_regular_expression)) then
            message_subject_trailing_was_regular_expression = "[ "//TAB//"]*\((*[Ww][Aa][Ss]:.*)\)"
        end if

        if (.not. allocated(message_mark_insert_begin)) then
            message_mark_insert_begin = "--8<---------------cut here---------------start------------->8---"//LF
        end if

        if (.not. allocated(message_mark_insert_end)) then
            message_mark_insert_end = "--8<---------------cut here---------------end--------------->8---"//LF
        end if

        if (.not. allocated(message_archive_header)) message_archive_header = "X-No-Archive: Yes"//LF

        if (.not. allocated(message_archive_note)) then
            message_archive_note = "X-No-Archive: Yes - save https://groups.google.com/"
        end if

        if (.not. allocated(message_bogus_addresses)) then
            message_bogus_addresses = [character(len=16) :: "noreply", "nospam", "invalid", "@.*@", "[^[:ascii:]].*@", &
                                       "[ "//TAB//"]"]
        end if

        if (.not. allocated(message_required_headers)) then
            allocate (message_required_headers(2))
            message_required_headers(1) = message_header_type("References", optional=.true.)
            message_required_headers(2) = message_header_type("From")
        end if

        if (.not. allocated(message_required_mail_headers)) then
            allocate (message_required_mail_headers(6))
            message_required_mail_headers(1) = message_header_type("From")
            message_required_mail_headers(2) = message_header_type("Subject")
            message_required_mail_headers(3) = message_header_type("Date")
            message_required_mail_headers(4) = message_header_type("In-Reply-To", optional=.true.)
            message_required_mail_headers(5) = message_header_type("Message-ID")
            message_required_mail_headers(6) = message_header_type("User-Agent", optional=.true.)
        end if

        if (.not. allocated(message_user_organization_file)) then
            do k = 1, 3
                select case (k)
                case (1)
                    directory = "/etc/organization"
                case (2)
                    directory = "/etc/news/organization"
                case default
                    directory = "/usr/lib/news/organization"
                end select

                if (is_file_readable(directory)) message_user_organization_file = directory
            end do
        end if

        if (.not. allocated(message_auto_save_directory)) then
            if (is_file_writable(message_directory)) then
                call expand_file_name("drafts", directory, st, message_directory)
                message_auto_save_directory = file_name_as_directory(directory)
            else
                message_auto_save_directory = "~/"
            end if
        end if

        if (.not. associated(major_modes%text_mode%execute)) call initialize_text_mode()

        if (message_mode_syntax_table%index == 0) then
            call make_syntax_table(message_mode_syntax_table, st, text_mode_syntax_table)
            call modify_syntax_entry(iachar("%"), iachar("%"), ". ", st, message_mode_syntax_table)
            call modify_syntax_entry(iachar(">"), iachar(">"), ". ", st, message_mode_syntax_table)
            call modify_syntax_entry(iachar("<"), iachar("<"), ". ", st, message_mode_syntax_table)
        end if

        call major_modes%message_mode%define("message_mode", "Message", message_mode, message_mode_hook, &
                                             parent=major_modes%text_mode, syntax_table=message_mode_syntax_table)
        message_mode_map = major_modes%message_mode%map
        commands%message_mode%execute => execute_message_mode
        commands%mail_message%execute => execute_mail_message
        commands%mail_message_other_window%execute => execute_mail_message_other_window
        commands%goto_message_to%execute => goto_message_to
        commands%goto_message_from%execute => goto_message_from
        commands%goto_message_subject%execute => goto_message_subject
        commands%goto_message_cc%execute => goto_message_cc
        commands%goto_message_bcc%execute => goto_message_bcc
        commands%goto_message_fcc%execute => goto_message_fcc
        commands%goto_message_reply_to%execute => goto_message_reply_to
        commands%goto_message_newsgroups%execute => goto_message_newsgroups
        commands%goto_message_distribution%execute => goto_message_distribution
        commands%goto_message_followup_to%execute => goto_message_followup_to
        commands%goto_message_mail_followup_to%execute => goto_message_mail_followup_to
        commands%goto_message_keywords%execute => goto_message_keywords
        commands%goto_message_summary%execute => goto_message_summary
        commands%goto_message_body%execute => execute_goto_message_body
        commands%goto_message_end_of_headers%execute => execute_goto_message_end_of_headers
        commands%goto_message_signature%execute => execute_goto_message_signature
        commands%insert_or_toggle_message_importance%execute => insert_or_toggle_message_importance
        commands%insert_message_importance_high%execute => execute_insert_message_importance_high
        commands%insert_message_importance_low%execute => execute_insert_message_importance_low
        commands%insert_message_disposition_notification_to%execute => insert_message_disposition_notification_to
        commands%insert_message_expires%execute => execute_insert_message_expires
        commands%insert_message_signature%execute => execute_insert_message_signature
        commands%insert_message_headers%execute => insert_message_headers
        commands%sort_message_headers%execute => sort_message_headers
        commands%reduce_message_to_to_cc%execute => reduce_message_to_to_cc
        commands%add_message_archive_header%execute => execute_add_message_archive_header
        commands%mark_message_inserted_region%execute => execute_mark_message_inserted_region
        commands%mark_message_insert_file%execute => execute_mark_message_insert_file
        commands%change_message_subject%execute => execute_change_message_subject
        commands%elide_message_region%execute => execute_elide_message_region
        commands%delete_message_not_region%execute => execute_delete_message_not_region
        commands%cut_message_to_signature%execute => execute_cut_message_to_signature
        commands%cut_message_address%execute => cut_message_address
        commands%rename_message_buffer%execute => execute_rename_message_buffer
        commands%message_beginning_of_line%execute => execute_message_beginning_of_line
        commands%newline_and_reformat_message%execute => execute_newline_and_reformat_message
        commands%widen_and_recenter_message%execute => widen_and_recenter_message
        commands%send_message_and_exit%execute => execute_send_message_and_exit
        commands%send_message%execute => execute_send_message
        commands%close_message_buffer%execute => close_message_buffer
        commands%message_dont_send%execute => message_dont_send
        commands%check_message_recipients%execute => execute_check_message_recipients
        call keymap_set(message_mode_map, "C-c ?", "describe_mode", st)
        call keymap_set(message_mode_map, "C-c C-f C-t", "goto_message_to", st)
        call keymap_set(message_mode_map, "C-c C-f C-o", "goto_message_from", st)
        call keymap_set(message_mode_map, "C-c C-f C-b", "goto_message_bcc", st)
        call keymap_set(message_mode_map, "C-c C-f C-w", "goto_message_fcc", st)
        call keymap_set(message_mode_map, "C-c C-f C-c", "goto_message_cc", st)
        call keymap_set(message_mode_map, "C-c C-f C-s", "goto_message_subject", st)
        call keymap_set(message_mode_map, "C-c C-f C-r", "goto_message_reply_to", st)
        call keymap_set(message_mode_map, "C-c C-f C-n", "goto_message_newsgroups", st)
        call keymap_set(message_mode_map, "C-c C-f C-d", "goto_message_distribution", st)
        call keymap_set(message_mode_map, "C-c C-f C-f", "goto_message_followup_to", st)
        call keymap_set(message_mode_map, "C-c C-f C-m", "goto_message_mail_followup_to", st)
        call keymap_set(message_mode_map, "C-c C-f C-k", "goto_message_keywords", st)
        call keymap_set(message_mode_map, "C-c C-f C-u", "goto_message_summary", st)
        call keymap_set(message_mode_map, "C-c C-f C-i", "insert_or_toggle_message_importance", st)
        ! modify headers (and insert notes in body)
        call keymap_set(message_mode_map, "C-c C-f s", "change_message_subject", st)
        call keymap_set(message_mode_map, "C-c C-f t", "reduce_message_to_to_cc", st)
        call keymap_set(message_mode_map, "C-c C-f a", "add_message_archive_header", st)
        ! mark inserted text
        call keymap_set(message_mode_map, "C-c M-m", "mark_message_inserted_region", st)
        call keymap_set(message_mode_map, "C-c M-f", "mark_message_insert_file", st)
        call keymap_set(message_mode_map, "C-c C-b", "goto_message_body", st)
        call keymap_set(message_mode_map, "C-c C-i", "goto_message_signature", st)
        call keymap_set(message_mode_map, "C-c C-f C-e", "insert_message_expires", st)
        call keymap_set(message_mode_map, "C-c C-u", "insert_or_toggle_message_importance", st)
        call keymap_set(message_mode_map, "C-c M-n", "insert_message_disposition_notification_to", st)
        call keymap_set(message_mode_map, "C-c C-w", "insert_message_signature", st)
        call keymap_set(message_mode_map, "C-c M-h", "insert_message_headers", st)
        call keymap_set(message_mode_map, "C-c C-o", "sort_message_headers", st)
        call keymap_set(message_mode_map, "C-c M-r", "rename_message_buffer", st)
        call keymap_set(message_mode_map, "C-c C-c", "send_message_and_exit", st)
        call keymap_set(message_mode_map, "C-c C-s", "send_message", st)
        call keymap_set(message_mode_map, "C-c C-k", "close_message_buffer", st)
        call keymap_set(message_mode_map, "C-c C-d", "message_dont_send", st)
        call keymap_set(message_mode_map, "C-c M-k", "cut_message_address", st)
        call keymap_set(message_mode_map, "C-c C-e", "elide_message_region", st)
        call keymap_set(message_mode_map, "C-c C-v", "delete_message_not_region", st)
        call keymap_set(message_mode_map, "C-c C-z", "cut_message_to_signature", st)
        call keymap_set(message_mode_map, "M-RET", "newline_and_reformat_message", st)
        call keymap_set(message_mode_map, "C-a", "message_beginning_of_line", st)
        call message_user_agent%define("message_user_agent", mail_message, commands%send_message_and_exit, &
                                       commands%close_message_buffer, message_send_hook)
        if (.not. associated(mail_user_agent)) mail_user_agent => message_user_agent
        call append_locals_clearer(clear_message_locals)
    end subroutine initialize_message

    ! ------------------------------------------------------------------
    ! The choice settings
    ! ------------------------------------------------------------------

    !> Forget the text and procedure of `message_user_organization', as any
    !> assignment to it does.  Not a port.
    subroutine organization_clear_kinds(self)
        class(message_user_organization_choice_setting_type), intent(inout) :: self

        if (allocated(self%text)) deallocate (self%text)
        nullify (self%procedure)
    end subroutine organization_clear_kinds

    !> The text or procedure of `message_user_organization' as help shows
    !> it; "" when neither is set.  Not a port.
    function organization_other_value(self) result(text)
        class(message_user_organization_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = text_or_procedure(self%text, associated(self%procedure))
    end function organization_other_value

    !> The assignments that set the text or procedure
    !> `message_user_organization' takes, for the error of a rejected
    !> value.  Not a port.
    function organization_kind_hint(self, number) result(text)
        class(message_user_organization_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = text_hint(self%name, number)
    end function organization_kind_hint

    !> Forget the text and procedure of `message_signature', as any
    !> assignment to it does.  Not a port.
    subroutine signature_clear_kinds(self)
        class(message_signature_choice_setting_type), intent(inout) :: self

        if (allocated(self%text)) deallocate (self%text)
        nullify (self%procedure)
    end subroutine signature_clear_kinds

    !> The text or procedure of `message_signature' as help shows it; ""
    !> when neither is set.  Not a port.
    function signature_other_value(self) result(text)
        class(message_signature_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = text_or_procedure(self%text, associated(self%procedure))
    end function signature_other_value

    !> The assignments that set the text or procedure `message_signature'
    !> takes, for the error of a rejected value.  Not a port.
    function signature_kind_hint(self, number) result(text)
        class(message_signature_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = text_hint(self%name, number)
    end function signature_kind_hint

    !> Forget the text and procedure of `message_default_headers', as any
    !> assignment to it does.  Not a port.
    subroutine default_headers_clear_kinds(self)
        class(message_default_headers_choice_setting_type), intent(inout) :: self

        if (allocated(self%text)) deallocate (self%text)
        nullify (self%procedure)
    end subroutine default_headers_clear_kinds

    !> The text or procedure of `message_default_headers' as help shows it;
    !> "" when neither is set.  Not a port.
    function default_headers_other_value(self) result(text)
        class(message_default_headers_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = text_or_procedure(self%text, associated(self%procedure))
    end function default_headers_other_value

    !> The assignments that set the text or procedure
    !> `message_default_headers' takes, for the error of a rejected value.
    !> Not a port.
    function default_headers_kind_hint(self, number) result(text)
        class(message_default_headers_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = text_hint(self%name, number)
    end function default_headers_kind_hint

    !> Forget the procedure of `message_generate_new_buffers', as any
    !> assignment to it does.  Not a port.
    subroutine new_buffers_clear_kinds(self)
        class(message_generate_new_buffers_choice_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine new_buffers_clear_kinds

    !> The procedure of `message_generate_new_buffers' as help shows it;
    !> "" when none is set.  Not a port.
    function new_buffers_other_value(self) result(text)
        class(message_generate_new_buffers_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function new_buffers_other_value

    !> The assignment that sets the procedure `message_generate_new_buffers'
    !> takes, for the error of a rejected value.  Not a port.
    function new_buffers_kind_hint(self, number) result(text)
        class(message_generate_new_buffers_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a procedure is `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function new_buffers_kind_hint

    !> Forget the address of `message_sendmail_envelope_from', as any
    !> assignment to it does.  Not a port.
    subroutine envelope_from_clear_kinds(self)
        class(message_sendmail_envelope_from_choice_setting_type), intent(inout) :: self

        if (allocated(self%address)) deallocate (self%address)
    end subroutine envelope_from_clear_kinds

    !> The address of `message_sendmail_envelope_from' as help shows it
    !> (`%address = "..."'); "" when none is set.  Not a port.
    function envelope_from_other_value(self) result(text)
        class(message_sendmail_envelope_from_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (allocated(self%address)) text = "%address = """//self%address//""""
    end function envelope_from_other_value

    !> The assignment that sets the address `message_sendmail_envelope_from'
    !> takes, for the error of a rejected value.  Not a port.
    function envelope_from_kind_hint(self, number) result(text)
        class(message_sendmail_envelope_from_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "an address is `"//trim(self%name)//"%address = ""...""'"
        if (len(number) < 0) text = number
    end function envelope_from_kind_hint

    !> Forget the column of `message_fill_column', as any assignment to it
    !> does.  Not a port.
    subroutine fill_column_clear_kinds(self)
        class(message_fill_column_choice_setting_type), intent(inout) :: self

        self%columns = -1
    end subroutine fill_column_clear_kinds

    !> The column of `message_fill_column' as help shows it (`%columns =
    !> 72'); "" when none is set.  Not a port.
    function fill_column_other_value(self) result(text)
        class(message_fill_column_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        text = ""

        if (self%columns >= 0) then
            write (buffer, '("%columns = ", i0)') self%columns
            text = trim(buffer)
        end if
    end function fill_column_other_value

    !> The assignment that sets the column `message_fill_column' takes, for
    !> the error of a rejected value.  Not a port.
    function fill_column_kind_hint(self, number) result(text)
        class(message_fill_column_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of columns is `"//trim(self%name)//"%columns = "//number//"'"
    end function fill_column_kind_hint

    ! The help text of a setting's text or procedure: `%text = "..."',
    ! "a procedure set by the configuration", or "".  Not a port.
    function text_or_procedure(text_value, has_procedure) result(text)
        character(len=:), allocatable, intent(in) :: text_value
        logical, intent(in) :: has_procedure
        character(len=:), allocatable :: text

        text = ""
        if (allocated(text_value)) text = "%text = """//text_value//""""
        if (has_procedure) text = "a procedure set by the configuration"
    end function text_or_procedure

    ! The assignments that set a setting's text or procedure, for the
    ! error of a rejected value.  Not a port.
    function text_hint(name, number) result(text)
        character(len=*), intent(in) :: name, number
        character(len=:), allocatable :: text

        text = "a string is `"//trim(name)//"%text = ""...""' and a procedure `"//trim(name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function text_hint
end module message_mod
