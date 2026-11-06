! send_mail_mod.f90 -- mail sending commands for Fortmacs.
! Ported from GNU Emacs 31.1 lisp/mail/sendmail.el.
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

!> Mail mode and sending mail: \[mail] (and \[compose_mail] with
!> `sendmail_user_agent') prepares the *mail* buffer in Mail mode, whose
!> headers end at the line `mail_header_separator'; C-c C-c
!> (`send_mail_and_exit') sends it with `send_mail_function', by default
!> `query_sendmail_once', which asks how to send mail the first time;
!> `send_mail_with_sendmail' hands the message to the program
!> `sendmail_program' and writes copies to the files of Fcc fields.  To
!> use Mail mode from \[compose_mail], the configuration writes
!>
!>     mail_user_agent => sendmail_user_agent
!>     send_mail_function => send_mail_with_sendmail
!>
!> See docs/status/send_mail_mod.md for what is deferred.
module send_mail_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE, PREFIX_RAW
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE, VOID_FUNCTION_CODE
    use casefiddle_mod, only: downcase_string
    use handle_mod, only: handle_type
    use value_mod, only: value_type, command_procedure, make_buffer_reference, make_list, vtag, V_INTEGER, V_STRING
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          erase_buffer, close_buffer, buffer_serial_live, generate_new_buffer_name, set_buffer_modified, &
                          is_buffer_modified, barf_if_buffer_read_only, fill_column, get_file_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, is_end_of_line, &
                                     is_end_of_buffer, buffer_substring, buffer_substring_no_properties, buffer_string, &
                                     delete_region, narrow_to_region, widen, insert_buffer_substring, line_beginning_position, &
                                     line_end_position, preceding_character, insert_before_markers, mark_position, mark_marker, &
                                     buffer_size
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, forward_character, end_of_line, beginning_of_line
    use syntax_mod, only: skip_characters_forward, skip_characters_backward, make_syntax_table, modify_syntax_entry
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, looking_at, &
                          match_beginning, match_end, replace_match, string_match, regular_expression_quote, case_fold_search, &
                          SEARCH_NOERROR, SEARCH_MOVE, string_index_to_offset
    use basic_subprograms_mod, only: read_y_or_n, match_string, mail_user_agent_type, mail_switch_procedure
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker, set_marker, set_marker_insertion_type
    use file_input_output_mod, only: expand_file_name, file_name_as_directory, file_exists, is_file_readable, &
                                     insert_file_contents, write_region
    use files_mod, only: executable_find, set_visited_file_name, save_buffer
    use environment_mod, only: getenv
    use display_engine_mod, only: message
    use help_mod, only: substitute_command_keys
    use simple_mod, only: rfc822_goto_end_of_headers, push_mark, delete_horizontal_space, do_auto_fill, normal_auto_fill_function, &
                          fill_prefix
    use fill_mod, only: fill_region_as_paragraph, fill_paragraph_function
    use indent_mod, only: indent_rigidly, current_column
    use newcomment_mod, only: comment_start, comment_start_skip
    use paragraphs_mod, only: paragraph_separate, page_delimiter
    use abbreviation_mod, only: expand_abbreviation
    use modes_mod, only: major_modes
    use keymap_mod, only: keymap_set
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, FONT_LOCK_OVERRIDE_NONE
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use text_mode_mod, only: text_mode_syntax_table, initialize_text_mode
    use window_mod, only: pop_to_buffer_same_window, switch_to_buffer_other_window, bury_buffer, other_buffer, &
                          switch_to_buffer, display_buffer
    use minibuffer_reading_mod, only: completion_ignore_case
    use minibuffer_mod, only: completing_read, read_file_name, REQUIRE_MATCH_YES
    use replace_mod, only: format_prompt
    use minibuffer_reading_tables_mod, only: completion_table_type, TABLE_LIST
    use coding_mod, only: buffer_file_coding_system, coding_system_mime_character_set
    use regular_expression_optimization_mod, only: regular_expression_optimization, REGULAR_EXPRESSION_OPTIMIZATION_GROUP
    use time_functions_mod, only: format_time_string
    use editing_functions_mod, only: user_full_name, user_login_name
    use startup_mod, only: user_mail_address_value
    use execute_process_mod, only: execute_process_region
    use mail_utilities_mod, only: get_mail_field, strip_mail_quoted_names, parse_mail_comma_list, is_mail_file_babyl, &
                                  mail_rfc822_date
    use data_mod, only: append_locals_clearer, choice_setting_type
    use mail_extract_mod, only: extract_mail_address_components, mail_address_components_type
    use editing_functions_mod, only: save_restriction_begin, save_restriction_end, restriction_type
    implicit none(type, external)
    private

    public :: get_mail_envelope_from
    public :: query_sendmail_once, compose_sendmail_user_agent, set_up_mail, mail_mode, mail_header_end, mail_text_start, &
              delimit_mail_sendmail_header, undelimit_mail_sendmail_header, mail_mode_auto_fill, fill_mail_mode_paragraph, &
              send_mail_and_exit, mail_dont_send, bury_mail, send_mail, insert_mail_from_field, combine_mail_fields, &
              send_mail_with_sendmail, do_mail_fcc, mail_to, mail_subject, mail_cc, mail_bcc, mail_fcc, mail_reply_to, &
              mail_mail_reply_to, mail_mail_followup_to, position_mail_on_field, mail_text, &
              insert_mail_disposition_notification_to, mail_signature, indent_mail_citation, paste_mail_original, &
              clear_mail_paste_headers, paste_mail_region, insert_mail_file, mail, mail_other_window, &
              select_message_coding_system, set_mail_reply_buffer, initialize_sendmail

    !> Values of `mail_from_style': the return address only
    !> (MAIL_FROM_STYLE_SIMPLE), the full name in parentheses after it
    !> (MAIL_FROM_STYLE_PARENTHESES), the address in angles after the full name
    !> (MAIL_FROM_STYLE_ANGLES), angles unless they would need quoting
    !> and parentheses would not (MAIL_FROM_STYLE_DEFAULT), and the
    !> deprecated MAIL_FROM_STYLE_SYSTEM_DEFAULT, which inserts no field.
    integer, parameter, public :: MAIL_FROM_STYLE_SIMPLE = 0, MAIL_FROM_STYLE_PARENTHESES = 1, MAIL_FROM_STYLE_ANGLES = 2, &
                                  MAIL_FROM_STYLE_DEFAULT = 3, MAIL_FROM_STYLE_SYSTEM_DEFAULT = 4

    !> Values of `mail_send_nonascii' besides .true. (allow non-ASCII
    !> characters) and .false. (don't): ask the user each time
    !> (MAIL_SEND_NONASCII_ASK), or add a MIME header if none is present
    !> (MAIL_SEND_NONASCII_MIME).
    integer, parameter, public :: MAIL_SEND_NONASCII_ASK = 2, MAIL_SEND_NONASCII_MIME = 3

    !> Values of NOERASE for `mail': erase an existing modified *mail*
    !> buffer after asking (MAIL_NOERASE_NO), make a new mail buffer
    !> (MAIL_NOERASE_NEW), or select the old buffer without erasing it
    !> (MAIL_NOERASE_YES).
    integer, parameter, public :: MAIL_NOERASE_NO = 0, MAIL_NOERASE_YES = 1, MAIL_NOERASE_NEW = 2

    !> True means insert `From:' field when setting up the message.  Port
    !> of sendmail.el mail-setup-with-from.
    logical, public :: set_up_mail_with_from = .true.

    !> Program used to send messages.
    !> If the program returns a nonzero error code, or outputs any
    !> text, sending is considered "failed" by Fortmacs.  Port of
    !> sendmail.el sendmail-program (set by `initialize_sendmail' unless
    !> already set).
    character(len=:), allocatable, public :: sendmail_program

    !> Specifies how "From:" fields look.
    !>
    !> If MAIL_FROM_STYLE_SIMPLE, they contain just the return address like:
    !>     king@grassland.com
    !> If MAIL_FROM_STYLE_PARENTHESES, they look like:
    !>     king@grassland.com (Elvis Parsley)
    !> If MAIL_FROM_STYLE_ANGLES, they look like:
    !>     Elvis Parsley <king@grassland.com>
    !>
    !> Otherwise, most addresses look like MAIL_FROM_STYLE_ANGLES, but they look like
    !> MAIL_FROM_STYLE_PARENTHESES if MAIL_FROM_STYLE_ANGLES would need quoting and MAIL_FROM_STYLE_PARENTHESES would not.
    !> Port of sendmail.el mail-from-style.
    integer, public :: mail_from_style = MAIL_FROM_STYLE_ANGLES

    !> If true, specify the envelope-from address when sending mail.
    !> The value used to specify it is whatever is found in
    !> the variable `mail_envelope_from', with `user_mail_address' as fallback.
    !>
    !> On most systems, specifying the envelope-from address is a
    !> privileged operation.  This variable affects sendmail and
    !> smtpmail.  Port of sendmail.el mail-specify-envelope-from.
    logical, public :: mail_specify_envelope_from = .false.

    !> The setting `mail_envelope_from': .false., MAIL_ENVELOPE_FROM_HEADER
    !> or an address (R6-39: `mail_envelope_from%address = "..."';
    !> unallocated when none is set).  Not a port.
    type, extends(choice_setting_type), public :: mail_envelope_from_choice_setting_type
        !> The envelope-from address (upstream's string value).
        character(len=:), allocatable :: address
    contains
        procedure :: clear_kinds => mail_envelope_from_clear_kinds
        procedure :: other_value => mail_envelope_from_other_value
        procedure :: kind_hint => mail_envelope_from_kind_hint
    end type mail_envelope_from_choice_setting_type

    !> Values of `mail_envelope_from' besides .false.
    integer, parameter, public :: MAIL_ENVELOPE_FROM_HEADER = 2

    !> If set, designate the envelope-from address when sending mail.
    !> This only has an effect if `mail_specify_envelope_from' is true.
    !> The value should be either an address (`mail_envelope_from%address =
    !> "..."'), or MAIL_ENVELOPE_FROM_HEADER (in
    !> which case the contents of the "From" header of the message
    !> being sent is used), or .false. (in which case the value of
    !> `user_mail_address' is used).  Port of sendmail.el mail-envelope-from.
    type(mail_envelope_from_choice_setting_type), public :: mail_envelope_from = &
                                               mail_envelope_from_choice_setting_type(name="mail_envelope_from", true_valid=.false.)

    !> True means insert Bcc to self in messages to be sent.
    !> This is done when the message is initialized,
    !> so you can remove or alter the Bcc field to override the default.
    !> Port of sendmail.el mail-self-blind.
    logical, public :: mail_self_blind = .false.

    !> True means when sending a message wait for and display errors.
    !> Otherwise, let mailer send back a message to report errors.  Port
    !> of sendmail.el mail-interactive.
    logical, public :: mail_interactive = .true.

    !> Delete these headers from old message when it's inserted in a reply.
    !> Port of sendmail.el mail-yank-ignored-headers (set by
    !> `initialize_sendmail' unless already set).
    character(len=:), allocatable, public :: mail_paste_ignored_headers

    !> Function to call to send the current buffer as mail.
    !> The headers should be delimited by a line which is
    !> not a valid RFC 822 (or later) header or continuation line,
    !> that matches the variable `mail_header_separator'.
    !> This is used by the default mail-sending commands.  Port of
    !> sendmail.el send-mail-function.
    procedure(command_procedure), pointer, public :: send_mail_function => query_sendmail_once

    !> Line used to separate headers from text in messages being composed.
    !> Port of sendmail.el mail-header-separator.
    character(len=:), allocatable, public :: mail_header_separator

    !> Name of file to write all outgoing messages in, or unallocated for none.
    !> This is normally an mbox file, but for backwards compatibility may also
    !> be a Babyl file.  Port of sendmail.el mail-archive-file-name.
    character(len=:), allocatable, public :: mail_archive_file_name

    !> Address to insert as default Reply-To field of outgoing messages.
    !> If unallocated, it will be initialized from the REPLYTO environment variable
    !> when you first send mail.  Port of sendmail.el mail-default-reply-to.
    character(len=:), allocatable, public :: mail_default_reply_to

    !> If allocated, the name of a file to use instead of the sendmail default.
    !> This file defines aliases to be expanded by the mailer; this is a different
    !> feature from that of defining aliases in `.mailrc' to be expanded in Fortmacs.
    !> This variable has no effect unless your system uses sendmail as its mailer.
    !> The default file is defined in sendmail's configuration file, e.g.
    !> `/etc/aliases'.  Port of sendmail.el mail-alias-file.
    character(len=:), allocatable, public :: mail_alias_file

    !> If allocated, the name of the user's personal mail alias file.
    !> This file typically should be in same format as the `.mailrc' file used by
    !> the `Mail' or `mailx' program.
    !> This file need not actually exist.  Port of sendmail.el
    !> mail-personal-alias-file.
    character(len=:), allocatable, public :: mail_personal_alias_file

    !> Normal hook, run each time a new outgoing message is initialized.
    !> Port of sendmail.el mail-setup-hook.
    type(hook_type), target, public :: set_up_mail_hook = hook_type(name="set_up_mail_hook")

    !> Prefix insert on lines of pasted message being replied to.
    !> If this is unallocated, use indentation, as specified by
    !> `mail_indentation_spaces'.  Port of sendmail.el mail-yank-prefix.
    character(len=:), allocatable, public :: mail_paste_prefix

    !> Number of spaces to insert at the beginning of each cited line.
    !> Used by `paste_mail_original' via `indent_mail_citation'.  Port of
    !> sendmail.el mail-indentation-spaces.
    integer, public :: mail_indentation_spaces = 3

    !> Hook for modifying a citation just inserted in the mail buffer.
    !> Each hook function can find the citation between point and the mark,
    !> and should leave point and mark around the citation text as modified.
    !> The hook functions can find the header of the cited message
    !> in the variable `mail_citation_header', whether or not this is included
    !> in the cited portion of the message.
    !>
    !> If this hook is entirely empty, a default action is taken
    !> instead of no action.  Port of sendmail.el mail-citation-hook.
    type(hook_type), target, public :: mail_citation_hook = hook_type(name="mail_citation_hook")

    !> While running `mail_citation_hook', this variable holds the message header.
    !> This enables the hook functions to see the whole message header
    !> regardless of what part of it (if any) is included in the cited text.
    !> Port of sendmail.el mail-citation-header.
    character(len=:), allocatable, public :: mail_citation_header

    !> Regular expression to match a citation prefix plus whitespace.
    !> It should match whatever sort of citation prefixes you want to handle,
    !> including leading whitespace.  The default value matches citations
    !> like `foo_bar>' plus any leading whitespace.  Port of sendmail.el
    !> mail-citation-prefix-regexp.
    character(len=:), allocatable, public :: mail_citation_prefix_regular_expression

    !> The setting `mail_signature_variable': .true. inserts the file
    !> `mail_signature_file', .false. nothing; its text (`%text') or
    !> procedure (`%procedure', set with `=>') is inserted or called instead
    !> (R6-39, R6-37).  Not a port.
    type, extends(choice_setting_type), public :: mail_signature_variable_choice_setting_type
        !> The signature to insert (upstream's string value).
        character(len=:), allocatable :: text
        !> What to call to insert a signature (upstream's expression value).
        procedure(command_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => mail_signature_variable_clear_kinds
        procedure :: other_value => mail_signature_variable_other_value
        procedure :: kind_hint => mail_signature_variable_kind_hint
    end type mail_signature_variable_choice_setting_type

    !> Keymap of `mail_mode'.  Port of sendmail.el mail-mode-map.
    type(handle_type), public :: mail_mode_map

    !> Syntax table used while in `mail_mode'.  Port of sendmail.el
    !> mail-mode-syntax-table.
    type(handle_type), public :: mail_mode_syntax_table

    !> Text inserted at end of mail buffer when a message is initialized.
    !> If .false., no signature is inserted.
    !> If .true., it means to insert the contents of the file
    !> `mail_signature_file'.
    !> If its text is set (`mail_signature_variable%text = "..."'), that
    !> string is inserted.
    !>  (To make a proper signature, the string should begin with a blank
    !>  line and "-- " and a newline, which is the standard way to delimit
    !>  a signature in a message.)
    !> If its procedure is set (`mail_signature_variable%procedure => f'),
    !> it is called, and should insert whatever you want to insert.  Port of
    !> sendmail.el mail-signature (named so beside the command
    !> `mail_signature').
    type(mail_signature_variable_choice_setting_type), public :: mail_signature_variable = &
                                           mail_signature_variable_choice_setting_type(name="mail_signature_variable", value=.true.)

    !> File containing the text inserted at end of mail buffer.  Port of
    !> sendmail.el mail-signature-file.
    character(len=:), allocatable, public :: mail_signature_file

    !> Value of `default_directory' for Mail mode buffers.
    !> This directory is used for auto-save files of Mail mode buffers.
    !> Port of sendmail.el mail-default-directory.
    character(len=:), allocatable, public :: mail_default_directory

    !> A string containing header lines, to be inserted in outgoing messages.
    !> It can contain newlines, and should end in one.  It is inserted
    !> before you edit the message, so you can edit or delete the lines.
    !> Port of sendmail.el mail-default-headers.
    character(len=:), allocatable, public :: mail_default_headers

    !> Specify whether to allow sending non-ASCII characters in mail.
    !> If .true., that means do allow it.
    !> .false. means don't allow it.
    !> MAIL_SEND_NONASCII_ASK means ask the user each time.
    !> MAIL_SEND_NONASCII_MIME means add an appropriate MIME header if none
    !> already present.  The default is MAIL_SEND_NONASCII_MIME.
    !> Including non-ASCII characters in a mail message can be problematical
    !> for the recipient, who may not know how to decode them properly.
    !> Port of sendmail.el mail-send-nonascii.
    type(choice_setting_type), public :: mail_send_nonascii = &
                                         choice_setting_type(name="mail_send_nonascii", choice=MAIL_SEND_NONASCII_MIME)

    !> Ask MTA for notification of failed, delayed or successful delivery
    !> (an array of "failure", "delay" and "success").
    !> Note that only some MTAs (currently only recent versions of Sendmail)
    !> support Delivery Status Notification.  Port of sendmail.el
    !> mail-use-dsn.
    character(len=:), allocatable, public :: mail_use_dsn(:)

    !> Hook run by Mail mode.
    !> When composing a mail, this runs immediately after creating, or
    !> switching to, the `*mail*' buffer.  See also `set_up_mail_hook'.
    !> Port of sendmail.el mail-mode-hook.
    type(hook_type), target, public :: mail_mode_hook = hook_type(name="mail_mode_hook")

    !> Hook run just before sending a message.  Port of sendmail.el
    !> mail-send-hook.
    type(hook_type), target, public :: mail_send_hook = hook_type(name="mail_send_hook")

    !> List of mailing list addresses the user is subscribed to.
    !> The variable is used to trigger insertion of the "Mail-Followup-To"
    !> header when sending a message to a mailing list.  Port of
    !> sendmail.el mail-mailing-lists (an array of addresses, blank-padded).
    character(len=:), allocatable, public :: mail_mailing_lists(:)

    !> Coding system for encoding the outgoing mail.
    !> This has higher priority than the default `buffer_file_coding_system'
    !> and `default_sendmail_coding_system',
    !> but lower priority than the local value of `buffer_file_coding_system'.
    !> See also the function `select_message_coding_system'.  Port of
    !> sendmail.el sendmail-coding-system (unallocated for none).
    character(len=:), allocatable, public :: sendmail_coding_system

    !> Default coding system for encoding the outgoing mail.
    !> This variable is used only when `sendmail_coding_system' is unallocated.
    !> See also the function `select_message_coding_system'.  Port of
    !> sendmail.el default-sendmail-coding-system.
    character(len=:), allocatable, public :: default_sendmail_coding_system

    !> The options of the sendmail program that mean "report errors to
    !> terminal" and "deliver interactively", used when `mail_interactive'
    !> is true.  Port of sendmail.el sendmail-error-reporting-interactive.
    character(len=:), allocatable, public :: sendmail_error_reporting_interactive(:)

    !> The options that mean "report errors by mail" and "deliver in
    !> background".  Port of sendmail.el
    !> sendmail-error-reporting-non-interactive.
    character(len=:), allocatable, public :: sendmail_error_reporting_non_interactive(:)

    !> The Mail package as a mail user agent for `mail_user_agent'.
    !> Port of sendmail.el sendmail-user-agent.
    type(mail_user_agent_type), target, public :: sendmail_user_agent

    ! A Mail mode buffer's own value of mail-reply-action (a buffer
    ! holding the message being replied to), by serial number.
    type :: local_type
        integer(int64) :: serial = 0
        type(handle_type) :: reply_buffer
    end type local_type

    type(local_type), allocatable :: locals(:)

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    interface
        !> Ask the user how to send mail, showing the choices in a help
        !> buffer; CHOSEN is the function chosen.  The "mail client" choice
        !> names a function not ported yet (mailclient.el), so choosing it
        !> is a `void_function' error condition.  Port of
        !> sendmail.el:sendmail-query-user-about-smtp.  (In submodule
        !> send_mail_query_mod, as the "smtp" choice is smtpmail_mod's
        !> `send_mail_with_smtp', and smtpmail_mod uses this module.)
        recursive module subroutine query_sendmail_user_about_smtp(chosen, status)
            implicit none(type, external)
            procedure(command_procedure), pointer, intent(out) :: chosen
            type(status_type), intent(out) :: status
        end subroutine query_sendmail_user_about_smtp
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
    subroutine clear_sendmail_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = local_type()
    end subroutine clear_sendmail_locals

    !> Make BUFFER the message the current Mail mode buffer replies to
    !> (`mail_reply_action'), which \[paste_mail_original] inserts.
    !> Not a port: upstream sets the variable.
    subroutine set_mail_reply_buffer(buffer)
        type(handle_type), intent(in) :: buffer
        integer :: k

        call make_local_entry(k)
        if (k > 0) locals(k)%reply_buffer = buffer
    end subroutine set_mail_reply_buffer

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

    ! The searches, telling whether they found a match.  Not a port.
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
    recursive logical function search_regular_expression_backward(regular_expression, status, bound, move) result(found)
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

        call regular_expression_search_backward(regular_expression, position, status, bound, noerror)
        found = position /= 0
    end function search_regular_expression_backward

    ! Search forward for STRING.  Not a port.
    recursive logical function search_string(string, status, bound) result(found)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        integer(int64) :: position

        call search_forward(string, position, status, bound, SEARCH_NOERROR)
        found = position /= 0
    end function search_string

    ! Narrow to the headers (message-narrow-to-headers).  Port of
    ! message.el:message-narrow-to-headers.
    recursive subroutine narrow_to_mail_headers(status)
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
    end subroutine narrow_to_mail_headers

    ! The date as a Date field gives it (message-make-date, of the
    ! current time).  Port of message.el:message-make-date.
    function make_message_date() result(date)
        character(len=:), allocatable :: date

        date = format_time_string("%a, %d %b %Y %T %z")
    end function make_message_date

    ! A buffer named NAME that did not exist.  Not a port.
    subroutine generate_new_buffer(name, buffer, status)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call get_or_make_buffer(generate_new_buffer_name(name), buffer, status)
    end subroutine generate_new_buffer

    ! (y-or-n-p PROMPT), false when the question fails.  Not a port.
    recursive logical function ask(prompt, status) result(answer)
        character(len=*), intent(in) :: prompt
        type(status_type), intent(out) :: status

        call read_y_or_n(prompt, answer, status)
        if (.not. status%ok()) answer = .false.
    end function ask

    ! ------------------------------------------------------------------
    ! Choosing how to send
    ! ------------------------------------------------------------------

    !> Query for `send_mail_function' and send mail with it.
    !> This also sets `send_mail_function' when the choice is to be saved.
    !> Port of sendmail.el:sendmail-query-once (saving the choice into
    !> the init file with `customize_save_variable' is Deferred).
    recursive subroutine query_sendmail_once(status)
        type(status_type), intent(out) :: status
        procedure(command_procedure), pointer :: chosen
        logical :: save

        ! If send-mail-function is already setup, we're incorrectly called
        ! a second time, probably because someone's using an old value
        ! of send-mail-function.
        if (.not. associated(send_mail_function, query_sendmail_once)) then
            call send_mail_function(status)
            return
        end if

        call query_sendmail_user_about_smtp(chosen, status)
        if (.not. status%ok()) return
        call chosen(status)
        if (.not. status%ok()) return
        save = ask("Save this mail sending choice?", status)
        if (.not. status%ok()) return
        if (save) send_mail_function => chosen
    end subroutine query_sendmail_once

    ! ------------------------------------------------------------------
    ! Composing
    ! ------------------------------------------------------------------

    !> Compose a message with Mail mode for `compose_mail' (the COMPOSE
    !> of `sendmail_user_agent').  Port of
    !> sendmail.el:sendmail-user-agent-compose (OTHER-HEADERS and the
    !> actions come with Rmail and Reporter).
    recursive subroutine compose_sendmail_user_agent(status, to, subject, continue, switch_function)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: to, subject
        logical, intent(in), optional :: continue
        procedure(mail_switch_procedure), optional :: switch_function
        logical :: initialized, cont
        integer :: noerase

        if (present(switch_function)) then
            call switch_function("*mail*", status)
            if (.not. status%ok()) return
        end if

        cont = .false.
        if (present(continue)) cont = continue
        noerase = MAIL_NOERASE_NO
        if (cont) noerase = MAIL_NOERASE_YES
        call mail(status, noerase, to, subject, initialized=initialized)
        if (.not. status%ok()) return
        if (.not. (initialized .or. cont)) call set_error(status, GENERIC_ERROR_CODE, "Message aborted")
    end subroutine compose_sendmail_user_agent

    !> Set up the current buffer for a message to TO with SUBJECT,
    !> IN_REPLY_TO and CC: the From, To, Cc, In-Reply-To and Subject
    !> fields, `mail_default_headers', Reply-To, Bcc and Fcc fields as the
    !> options say, the separator and the signature; then run
    !> `set_up_mail_hook'.  REPLYBUFFER, if present, is the buffer holding the
    !> message being replied to.  Port of sendmail.el:mail-setup (the
    !> actions, mail aliases and input methods are Deferred).
    recursive subroutine set_up_mail(status, to, subject, in_reply_to, cc, replybuffer)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: to, subject, in_reply_to, cc
        type(handle_type), intent(in), optional :: replybuffer
        character(len=:), allocatable :: value, saved_prefix, headers
        integer(int64) :: address_start, saved_column, to_position
        logical :: found, has_prefix
        type(handle_type) :: here
        integer :: k

        if (.not. allocated(mail_default_reply_to)) then
            call getenv("REPLYTO", value, found)
            if (found) mail_default_reply_to = value
        end if

        ! Don't leave this around from a previous message.
        call buffer_file_coding_system%clear_local()
        buffers(current)%multibyte = .true.
        ! Local variables for Mail mode.
        call make_local_entry(k)

        if (k > 0) then
            locals(k)%reply_buffer = handle_type()
            if (present(replybuffer)) locals(k)%reply_buffer = replybuffer
        end if

        call goto_character(point_minimum())

        if (set_up_mail_with_from) then
            call insert_mail_from_field(status)
            if (.not. status%ok()) return
        end if

        call insert("To: ", status)
        if (.not. status%ok()) return
        here = marker_at(point())
        has_prefix = allocated(fill_prefix%value)
        if (has_prefix) saved_prefix = fill_prefix%value

        body: block
            if (present(to)) then
                ! Here removed code to extract names from within <...>
                ! on the assumption that strip_mail_quoted_names
                ! has been called and has done so.
                fill_prefix%value = TAB
                address_start = point()
                call insert(to//LF, status)
                if (.not. status%ok()) exit body
                call fill_region_as_paragraph(address_start, point_maximum(), status)
                if (.not. status%ok()) exit body
                call goto_character(point_maximum())
                if (.not. is_beginning_of_line()) call insert(LF, status)
            else
                call insert(LF, status)
            end if

            if (.not. status%ok()) exit body

            if (present(cc)) then
                fill_prefix%value = TAB
                call insert("Cc: ", status)
                address_start = point()
                call insert(cc//LF, status)
                if (.not. status%ok()) exit body
                call fill_region_as_paragraph(address_start, point_maximum(), status)
                if (.not. status%ok()) exit body
                call goto_character(point_maximum())
                if (.not. is_beginning_of_line()) call insert(LF, status)
                if (.not. status%ok()) exit body
            end if

            if (present(in_reply_to)) then
                fill_prefix%value = TAB
                saved_column = fill_column%value
                fill_column%value = 78
                address_start = point()
                call insert("In-Reply-To: "//in_reply_to//LF, status)
                if (status%ok()) call fill_region_as_paragraph(address_start, point_maximum(), status)
                fill_column%value = saved_column
                if (.not. status%ok()) exit body
                call goto_character(point_maximum())
                if (.not. is_beginning_of_line()) call insert(LF, status)
                if (.not. status%ok()) exit body
            end if

            if (present(subject)) then
                headers = "Subject: "//subject//LF
            else
                headers = "Subject: "//LF
            end if

            if (allocated(mail_default_headers)) headers = headers//mail_default_headers
            if (allocated(mail_default_reply_to)) headers = headers//"Reply-To: "//mail_default_reply_to//LF
            if (mail_self_blind) headers = headers//"Bcc: "//user_mail_address_value()//LF
            if (allocated(mail_archive_file_name)) headers = headers//"Fcc: "//mail_archive_file_name//LF
            call insert(headers//mail_header_separator//LF, status)
            if (.not. status%ok()) exit body
            ! Insert the signature.  But remember the beginning of the message.
            to_position = 0
            if (present(to)) to_position = point()

            if (.not. mail_signature_variable%is_off()) then
                call mail_signature(status, .true.)
                if (.not. status%ok()) exit body
            end if

            call goto_character(point_maximum())
            if (.not. is_beginning_of_line()) call insert(LF, status)
        end block body

        if (has_prefix) then
            fill_prefix%value = saved_prefix
        else if (allocated(fill_prefix%value)) then
            deallocate (fill_prefix%value)
        end if

        call goto_character(marker_end(here))
        if (.not. status%ok()) return
        if (present(to)) call goto_character(to_position)
        if (.not. (present(to) .or. present(subject) .or. present(in_reply_to))) call set_buffer_modified(.false.)
        call set_up_mail_hook%execute(status)
    end subroutine set_up_mail

    !> Major mode for editing mail to be sent.
    !> Like Text Mode but with these additional commands:
    !>
    !> \[send_mail]  `send_mail' (send the message)
    !> \[send_mail_and_exit]  `send_mail_and_exit' (send the message and exit)
    !>
    !> Here are commands that move to a header field (and create it if there isn't):
    !>          \[mail_to]  move to To:      \[mail_subject]  move to Subj:
    !>          \[mail_bcc]  move to Bcc:    \[mail_cc]  move to Cc:
    !>          \[mail_fcc]  move to Fcc:    \[mail_reply_to] move to Reply-To:
    !>          \[mail_mail_reply_to]  move to Mail-Reply-To:
    !>          \[mail_mail_followup_to] move to Mail-Followup-To:
    !> \[mail_text]  move to message text.
    !> \[mail_signature]  `mail_signature' (insert `mail_signature_file' file).
    !> \[paste_mail_original]  `paste_mail_original' (insert current message, in Rmail).
    !> \[insert_mail_file] insert a text file into the message.
    !> Turning on Mail mode runs the normal hooks `text_mode_hook' and
    !> `mail_mode_hook' (in that order).  Port of sendmail.el:mail-mode.
    recursive subroutine mail_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        character(len=*), parameter :: separator_rest = "\t*[-–!|#%;>*·•‣⁃◦ ]+$"// &
                                       "\|[ \t]*[-[:alnum:]]*>+[ \t]*$\|[ \t]*$\|"// &
                                       "--\( \|-+\)$\|"
        character(len=:), allocatable :: rest
        logical :: saved

        if (.not. associated(major_modes%mail_mode%execute)) call initialize_sendmail()
        call major_modes%mail_mode%begin(saved, status)
        if (.not. status%ok()) return
        call mail_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true., case_fold=.true.)
        call normal_auto_fill_function%set_local(mail_mode_auto_fill)
        call fill_paragraph_function%set_local(fill_mail_mode_paragraph)

        ! Allow using comment commands to add/remove quoting (this only does
        ! anything if mail_paste_prefix is set).
        if (allocated(mail_paste_prefix)) then
            call comment_start%set_local(mail_paste_prefix)
            call comment_start_skip%set_local("^"//regular_expression_quote(mail_paste_prefix)//"[ "//TAB//"]*")
        end if

        ! `-- ' precedes the signature.  `-----' appears at the start of the
        ! lines that delimit forwarded messages.
        ! Lines containing just >= 3 dashes, perhaps after whitespace,
        ! are also sometimes used and should be separators.
        rest = tabs(separator_rest)//page_delimiter%value

        if (len(mail_header_separator) == 0) then
            ! This is based on adaptive-fill-regexp (presumably
            ! the idea is to allow navigation etc of cited paragraphs).
            call paragraph_separate%set_local(rest)
        else
            call paragraph_separate%set_local(regular_expression_quote(mail_header_separator)//"$\|"//rest)
        end if

        call major_modes%mail_mode%end(saved, status)

    contains

        ! TEXT with each \t written as a tab.
        function tabs(text) result(out)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: out
            integer :: i

            out = ""
            i = 1

            do while (i <= len(text))
                if (i < len(text)) then
                    if (text(i:i + 1) == "\t") then
                        out = out//TAB
                        i = i + 2
                        cycle
                    end if
                end if

                out = out//text(i:i)
                i = i + 1
            end do
        end function tabs
    end subroutine mail_mode

    !> Additional expressions to highlight in Mail mode.  Port of
    !> sendmail.el mail-font-lock-keywords.
    subroutine mail_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        type(font_lock_highlight_type) :: h(1), h2(2), a(1)
        character(len=*), parameter :: cite_characters = "[>|}]", cite_prefix = "[:alpha:]", &
                                       cite_suffix = cite_prefix//"0-9_.@-`'"""
        character(len=:), allocatable :: separator

        allocate (keywords(6))
        keywords(1) = font_lock_keyword("^\(To\|Newsgroups\):", face="font_lock_function_name_face")
        keywords(2) = font_lock_keyword("^\(B?Cc\|Reply-To\|Mail-\(Reply\|Followup\)-To\):", face="font_lock_keyword_face")
        h(1) = font_lock_highlight(1, face="font_lock_comment_face")
        keywords(3) = font_lock_keyword("^\(Subject:\)[ "//TAB//"]*\(.+\)?", highlights=h)

        if (len(mail_header_separator) == 0) then
            separator = " \`\' "
        else
            separator = regular_expression_quote(mail_header_separator)
        end if

        keywords(4) = font_lock_keyword("^"//separator//"$", face="font_lock_warning_face")
        ! Use MATCH-ANCHORED to effectively anchor the regexp left side.
        h2(1) = font_lock_highlight(1, face="font_lock_comment_delimiter_face", override=FONT_LOCK_OVERRIDE_NONE, &
                                    laxmatch=.true.)
        h2(2) = font_lock_highlight(5, face="font_lock_comment_face", override=FONT_LOCK_OVERRIDE_NONE, laxmatch=.true.)
        a(1) = font_lock_anchored(h2, regular_expression="\=[ "//TAB//"]*"// &
                                  "\(\(\(["//cite_prefix//"]+["//cite_suffix//"]*\)?"// &
                                  "\("//cite_characters//"[ "//TAB//"]*\)\)+\)"// &
                                  "\(.*\)", pre_match_form=pre_beginning_of_line, post_match_form=post_end_of_line)
        keywords(5) = font_lock_keyword(cite_characters, highlights=a)
        keywords(6) = font_lock_keyword("^\(X-[A-Za-z0-9-]+\|In-Reply-To\):.*\("//LF//"[ "//TAB//"]+.*\)*$", &
                                        face="font_lock_string_face")
    end subroutine mail_font_lock_keywords

    ! The PRE-MATCH-FORM (beginning-of-line) of the citation keyword.
    ! Not a port.
    subroutine pre_beginning_of_line(limit)
        integer(int64), intent(out) :: limit

        call beginning_of_line()
        limit = 0
    end subroutine pre_beginning_of_line

    ! The POST-MATCH-FORM (end-of-line) of the citation keyword.
    ! Not a port.
    subroutine post_end_of_line(limit)
        integer(int64), intent(out) :: limit

        call goto_character(line_end_position())
        limit = 0
    end subroutine post_end_of_line

    !> Return the buffer location of the end of headers, as a number.
    !> Port of sendmail.el:mail-header-end.
    recursive integer(int64) function mail_header_end() result(position)
        integer(int64) :: here, beginning_of_accessible_portion, end_of_accessible_portion
        type(status_type) :: st
        type(handle_type) :: mb, me

        here = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion = buffers(current)%end_of_accessible_portion
        mb = marker_at(beginning_of_accessible_portion)
        me = marker_at(end_of_accessible_portion, .true.)
        call widen()
        call rfc822_goto_end_of_headers(st)
        position = point()
        call narrow_to_region(marker_end(mb), marker_end(me), st)
        call goto_character(here)
    end function mail_header_end

    !> Return the buffer location of the start of text, as a number.
    !> Port of sendmail.el:mail-text-start.
    recursive integer(int64) function mail_text_start() result(position)
        integer(int64) :: here, shortage
        type(status_type) :: st
        type(handle_type) :: mb, me

        here = point()
        mb = marker_at(buffers(current)%beginning_of_accessible_portion)
        me = marker_at(buffers(current)%end_of_accessible_portion, .true.)
        call widen()
        call rfc822_goto_end_of_headers(st)
        shortage = forward_line(1_int64)
        position = point()
        call narrow_to_region(marker_end(mb), marker_end(me), st)
        call goto_character(here)
    end function mail_text_start

    !> Set up whatever header delimiter convention sendmail will use.
    !> Concretely: replace the first blank line in the header with the
    !> separator; POSITION is where it ends.  Port of
    !> sendmail.el:mail-sendmail-delimit-header.
    recursive subroutine delimit_mail_sendmail_header(position, status)
        integer(int64), intent(out) :: position
        type(status_type), intent(out) :: status

        position = 0
        call rfc822_goto_end_of_headers(status)
        if (.not. status%ok()) return
        call insert(mail_header_separator, status)
        position = point()
    end subroutine delimit_mail_sendmail_header

    !> Remove header separator to put the message in correct form for sendmail.
    !> Leave point at the start of the delimiter line.  Port of
    !> sendmail.el:mail-sendmail-undelimit-header.
    recursive subroutine undelimit_mail_sendmail_header(status)
        type(status_type), intent(out) :: status

        call goto_character(point_minimum())

        if (len(mail_header_separator) > 0) then
            if (search_regular_expression("^"//regular_expression_quote(mail_header_separator)//LF, &
                                          status)) call replace_match(LF, status)
            if (.not. status%ok()) return
        end if

        call rfc822_goto_end_of_headers(status)
    end subroutine undelimit_mail_sendmail_header

    !> Carry out Auto Fill for Mail mode.
    !> If within the headers, this makes the new lines into continuation
    !> lines.  Port of sendmail.el:mail-mode-auto-fill.
    recursive subroutine mail_mode_auto_fill(status)
        type(status_type), intent(out) :: status
        integer(int64) :: old_line_start, shortage
        type(handle_type) :: here

        if (point() < mail_header_end()) then
            old_line_start = line_beginning_position()
            call do_auto_fill(status)
            if (.not. status%ok()) return
            ! do-auto-fill broke the line if point is on another line now.
            if (line_beginning_position() == old_line_start) return
            here = marker_at(point())
            call beginning_of_line()

            do while (point() /= old_line_start)
                ! Use insert-before-markers in case we're inserting
                ! before the saved value of point (which is common).
                call insert_before_markers("   ", status)
                if (.not. status%ok()) exit
                shortage = forward_line(-1_int64)
            end do

            call goto_character(marker_end(here))
        else
            call do_auto_fill(status)
        end if
    end subroutine mail_mode_auto_fill

    !> The `fill_paragraph_function' of Mail mode: within the headers, fill
    !> the field at point, keeping its continuation lines indented (DONE
    !> .true.); elsewhere DONE is .false. and filling is as usual.  Port of
    !> sendmail.el:mail-mode-fill-paragraph.
    recursive subroutine fill_mail_mode_paragraph(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        integer(int64) :: beginning, shortage
        type(handle_type) :: end
        character(len=:), allocatable :: fieldname
        logical :: found, continued

        done = .false.
        status = status_type()
        ! Do something special only if within the headers.
        if (point() >= mail_header_end()) return
        fieldname = ""
        found = search_regular_expression_backward("^[-a-zA-Z]+:", status, move=.true.)
        if (.not. status%ok()) return
        beginning = point()

        if (found) then
            call buffer_substring(beginning, match_end(0, status) - 1, fieldname, status)
            fieldname = downcase_string(fieldname)
        end if

        shortage = forward_line(1_int64)

        ! Find continuation lines and get rid of their continuation markers.
        do
            continued = looking_at("[ "//TAB//"]", status)
            if (.not. status%ok()) return
            if (.not. continued) exit
            call delete_horizontal_space(.false., status)
            if (.not. status%ok()) return
            shortage = forward_line(1_int64)
        end do

        end = marker_at(point())
        call goto_character(beginning)

        ! If this field contains addresses,
        ! make sure we can fill after each address.
        select case (fieldname)
        case ("to", "cc", "bcc", "from", "reply-to", "mail-reply-to", "mail-followup-to", "resent-to", "resent-cc", &
              "resent-bcc", "resent-from", "resent-reply-to")
            do while (search_string(",", status, marked_position(end)))
                continued = looking_at("[ "//TAB//"]", status)
                if (.not. continued) call insert(" ", status)
                if (.not. status%ok()) exit
            end do
        case default
            continue
        end select

        if (status%ok()) call fill_region_as_paragraph(beginning, marked_position(end), status, justify)

        if (status%ok()) then
            ! Mark all lines except the first as continuations.
            call goto_character(beginning)
            shortage = forward_line(1_int64)

            do while (point() < marked_position(end))
                call insert("  ", status)
                if (.not. status%ok()) exit
                shortage = forward_line(1_int64)
            end do
        end if

        call destroy_marker(end, status)
        done = .true.
    end subroutine fill_mail_mode_paragraph

    ! ------------------------------------------------------------------
    ! Sending
    ! ------------------------------------------------------------------

    !> Send message like `send_mail', then, if no errors, exit from mail buffer.
    !> Prefix arg means don't delete this window.  Port of
    !> sendmail.el:mail-send-and-exit.
    recursive subroutine send_mail_and_exit(status, argument)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument

        call send_mail(status)
        if (status%ok()) call bury_mail(status, argument)
    end subroutine send_mail_and_exit

    !> Don't send the message you have been editing.
    !> Prefix arg means don't delete this window.  Port of
    !> sendmail.el:mail-dont-send.
    recursive subroutine mail_dont_send(status, argument)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument

        call bury_mail(status, argument)
    end subroutine mail_dont_send

    !> Bury this mail buffer.  ARGUMENT, true, would keep the window when
    !> returning with a return action.  Port of sendmail.el:mail-bury
    !> (return actions, and so returning to an Rmail buffer, come with
    !> Rmail; without one, ARGUMENT makes no difference, as upstream).
    recursive subroutine bury_mail(status, argument)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument
        type(handle_type) :: newbuf
        logical :: keep_window

        keep_window = .false.
        if (present(argument)) keep_window = argument
        newbuf = other_buffer(current_buffer())
        call bury_buffer(status, current_buffer())
        if (.not. status%ok()) return
        if (is_buffer_live(newbuf)) call switch_to_buffer(buffers(newbuf%index)%name, status)
    end subroutine bury_mail

    !> Send the message in the current buffer.
    !> If `mail_interactive' is true, wait for success indication
    !> or error messages, and inform user.
    !> Otherwise any failure is reported in a message back to
    !> the user from the mailer.  Port of sendmail.el:mail-send (checking
    !> for suspicious addresses, MIME encoding with MML and the send
    !> actions are Deferred).
    recursive subroutine send_mail(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: ml, to, cc, new_header_values, previous, value
        character(len=:), allocatable :: seen(:)
        integer(int64) :: original_point, header_end, moved
        logical :: proceed, modified, found, has_to, has_cc, valid
        integer :: k

        status = status_type()

        if (allocated(buffers(current)%filename)) then
            proceed = ask("Send buffer contents as mail message? ", status)
        else
            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) return
            proceed = modified
            if (.not. proceed) proceed = ask("Message already sent; resend? ", status)
        end if

        if (.not. status%ok() .or. .not. proceed) return
        original_point = point()
        ml = ""

        if (allocated(mail_mailing_lists)) then
            if (size(mail_mailing_lists) > 0) then
                ! The surrounding regexp assumes the use of
                ! `strip_mail_quoted_names' on addresses before matching
                ! Cannot deal with full RFC 822 (or later), but that is
                ! unlikely to be problematic.
                ml = "\(?:[[:space:];,]\|\`\)"//regular_expression_optimization(mail_mailing_lists, &
                                                                                REGULAR_EXPRESSION_OPTIMIZATION_GROUP)// &
                     "\(?:[[:space:];,]\|\'\)"
            end if
        end if

        call combine_mail_fields("To", status)
        if (status%ok()) call combine_mail_fields("Cc", status)
        if (.not. status%ok()) return

        ! If there are mailing lists defined
        if (len(ml) > 0) then
            call get_mail_field("to", to, status, all=.true.)
            if (.not. status%ok()) return
            call get_mail_field("cc", cc, status, all=.true.)
            if (.not. status%ok()) return
            has_to = allocated(to)
            has_cc = allocated(cc)
            if (.not. has_to) to = ""

            ! To: and Cc:
            if (has_cc) then
                new_header_values = strip_mail_quoted_names(to//", "//cc)
            else
                new_header_values = strip_mail_quoted_names(to)
            end if

            ! If message goes to known mailing list ...
            if (string_match(ml, new_header_values, status) > 0) then
                ! Add Mail-Followup-To if none yet
                call get_mail_field("mail-followup-to", previous, status)
                if (.not. status%ok()) return

                if (.not. allocated(previous)) then
                    call goto_character(mail_header_end())
                    call split_addresses(new_header_values, seen)
                    value = ""

                    ! remove duplicates
                    do k = 1, size(seen)
                        if (k > 1) then
                            if (any(seen(1:k - 1) == seen(k))) cycle
                        end if

                        if (len(value) == 0) then
                            value = trim(seen(k))
                        else
                            value = trim(seen(k))//", "//value
                        end if
                    end do

                    call insert("Mail-Followup-To: "//value//LF, status)
                    if (.not. status%ok()) return
                end if

                ! Add Mail-Reply-To if none yet
                call get_mail_field("mail-reply-to", previous, status)
                if (.not. status%ok()) return

                if (.not. allocated(previous)) then
                    call goto_character(mail_header_end())
                    call get_mail_field("reply-to", value, status)
                    if (.not. allocated(value)) value = user_mail_address_value()
                    call insert("Mail-Reply-To: "//value//LF, status)
                    if (.not. status%ok()) return
                end if
            end if

            call goto_character(original_point)
        end if

        if (.not. mail_send_nonascii%value .and. mail_send_nonascii%choice /= MAIL_SEND_NONASCII_MIME) then
            call goto_character(point_minimum())
            moved = skip_characters_forward(achar(0)//"-"//achar(127), status)

            if (point() /= point_maximum()) then
                if (mail_send_nonascii%choice == MAIL_SEND_NONASCII_ASK) then
                    found = ask("Message contains non-ASCII characters; send anyway? ", status)
                    if (.not. status%ok()) return
                    if (.not. found) call set_error(status, GENERIC_ERROR_CODE, "Aborted")
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Message contains non-ASCII characters")
                end if

                if (.not. status%ok()) return
            end if
        end if

        ! Complain about any invalid line.
        call goto_character(point_minimum())
        ! Search for mail-header-separator as whole line.
        found = search_regular_expression("^"//regular_expression_quote(mail_header_separator)//"$", status, point_maximum())
        if (.not. status%ok()) return
        header_end = point_maximum()
        if (found) header_end = match_beginning(0, status)
        call goto_character(point_minimum())

        do while (point() < header_end)
            valid = looking_at("[ "//TAB//"]\|.*:\|$", status)
            if (.not. status%ok()) return

            if (.not. valid) then
                call push_mark(original_point)
                call set_error(status, GENERIC_ERROR_CODE, &
                               "Invalid header line (maybe a continuation line lacks initial whitespace)")
                return
            end if

            moved = forward_line(1_int64)
        end do

        call goto_character(original_point)
        call mail_send_hook%execute(status)
        if (.not. status%ok()) return
        call message("Sending...")
        call send_mail_function(status)
        if (.not. status%ok()) return
        call message("Sending...done")
        ! If buffer has no file, mark it as unmodified.
        if (.not. allocated(buffers(current)%filename)) call set_buffer_modified(.false.)

    contains

        ! (split-string TEXT ",[[:space:]]+" t), the last part first (as
        ! upstream pushes them).
        subroutine split_addresses(text, parts)
            character(len=*), intent(in) :: text
            character(len=:), allocatable, intent(out) :: parts(:)
            integer(int64) :: start, m, longest, n
            integer(int64), allocatable :: from(:), to(:)
            type(status_type) :: st

            allocate (from(0), to(0))
            start = 1
            longest = 0

            ! (FROM and TO are byte positions; START and M character
            ! indices.)
            do
                m = string_match(",[[:space:]]+", text, st, start=start)

                if (m == 0) then
                    if (string_index_to_offset(text, start) < len(text)) then
                        from = [string_index_to_offset(text, start) + 1, from]
                        to = [int(len(text), int64), to]
                    end if

                    exit
                end if

                if (m > start) then
                    from = [string_index_to_offset(text, start) + 1, from]
                    to = [string_index_to_offset(text, m), to]
                end if

                start = match_end(0, st)
            end do

            n = size(from)
            if (n > 0) longest = maxval(to - from + 1)
            allocate (character(len=longest) :: parts(n))

            do m = 1, n
                parts(m) = text(from(m):to(m))
            end do
        end subroutine split_addresses
    end subroutine send_mail

    !> Insert the "From:" field of a mail header.
    !> The style of the field is determined by the variable `mail_from_style'.
    !> This function does not perform RFC 2047 encoding.  Port of
    !> sendmail.el:mail-insert-from-field.
    recursive subroutine insert_mail_from_field(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: login, fullname, temporary
        logical :: quote_fullname, angles, found
        integer(int64) :: fullname_start, m, k
        type(handle_type) :: fullname_end

        status = status_type()
        login = user_mail_address_value()
        fullname = user_full_name()
        quote_fullname = string_match("[^"//achar(0)//"-"//achar(127)//"]", fullname, status) > 0

        select case (mail_from_style)
        case (MAIL_FROM_STYLE_SIMPLE)
            call insert("From: "//login//LF, status)
            return
        case (MAIL_FROM_STYLE_SYSTEM_DEFAULT)
            ! This is deprecated.
            return
        case (MAIL_FROM_STYLE_ANGLES)
            angles = .true.
        case (MAIL_FROM_STYLE_PARENTHESES)
            angles = .false.
        case default
            ! Use angles if no quoting is needed, or if
            ! parens would need quoting too.
            angles = string_match("[^- !#-'*+/-9=?A-Z^-~]", fullname, status) == 0

            if (.not. angles) then
                temporary = fullname

                do
                    m = string_match("([^()]*)", temporary, status)
                    if (m == 0) exit
                    ! The parentheses are ASCII: their byte offsets.
                    k = string_index_to_offset(temporary, m) + 1
                    temporary(k:k) = "-"
                    k = string_index_to_offset(temporary, match_end(0, status) - 1) + 1
                    temporary(k:k) = "-"
                end do

                angles = string_match("[\()]", temporary, status) > 0
            end if
        end select

        if (angles) then
            call insert("From: "//fullname, status)
            if (.not. status%ok()) return
            fullname_start = point_minimum() + 6
            fullname_end = marker_at(point())
            call goto_character(fullname_start)
            ! Look for a character that cannot appear unquoted
            ! according to RFC 822 (or later).
            found = search_regular_expression("[^- !#-'*+/-9=?A-Z^-~]", status, marked_position(fullname_end), move=.true.)

            if (status%ok() .and. (found .or. quote_fullname)) then
                ! Quote fullname, escaping specials.
                call goto_character(fullname_start)
                call insert('"', status)

                do while (status%ok())
                    if (.not. search_regular_expression("[""\]", status, marked_position(fullname_end), move=.true.)) exit
                    call replace_match("\\\&", status, fixedcase=.true.)
                end do

                if (status%ok()) call insert('"', status)
            end if

            call goto_character(marker_end(fullname_end))
            if (status%ok()) call insert(" <"//login//">"//LF, status)
        else
            ! parens or default
            call insert("From: "//login//" (", status)
            if (.not. status%ok()) return
            fullname_start = point()
            if (quote_fullname) call insert('"', status)
            call insert(fullname, status)
            if (quote_fullname) call insert('"', status)
            fullname_end = marker_at(point())
            call goto_character(fullname_start)

            ! \ and nonmatching parentheses must be escaped in comments.
            ! Escape every instance of ()\ ...
            do while (status%ok())
                if (.not. search_regular_expression("[()\]", status, marked_position(fullname_end), move=.true.)) exit
                call replace_match("\\\&", status, fixedcase=.true.)
            end do

            ! ... then undo escaping of matching parentheses,
            ! including matching nested parentheses.
            call goto_character(fullname_start)

            do while (status%ok())
                if (.not. search_regular_expression("\(\=\|[^\]\(\\\\\)*\)\\(\(\([^\]\|\\\\\)*\)\\)", status, &
                                                    marked_position(fullname_end), &
                                                    move=.true.)) exit
                call replace_match("\1(\3)", status, fixedcase=.true.)
                call goto_character(fullname_start)
            end do

            call goto_character(marker_end(fullname_end))
            if (status%ok()) call insert(")"//LF, status)
        end if
    end subroutine insert_mail_from_field

    !> Offer to combine all FIELD fields in buffer into one FIELD field.
    !> If this finds multiple FIELD fields, it asks the user whether
    !> to combine them into one, and does so if the user says y.  Port of
    !> sendmail.el:mail-combine-fields.
    recursive subroutine combine_mail_fields(field, status)
        character(len=*), intent(in) :: field
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: search_pattern, this_to
        type(handle_type) :: first_to_end, here, mb, me
        integer(int64) :: this_to_start, this_to_end, moved, prefix_length, k
        logical :: query_asked, query_answer, found, narrowed

        search_pattern = "^"//field//"[ "//TAB//"]*:"
        query_asked = .false.
        query_answer = .false.
        here = marker_at(point())
        narrowed = buffers(current)%beginning_of_accessible_portion /= 1 .or. buffers(current)%end_of_accessible_portion &
                   /= buffers(current)%text%z
        mb = marker_at(buffers(current)%beginning_of_accessible_portion)
        me = marker_at(buffers(current)%end_of_accessible_portion, .true.)
        call goto_character(point_minimum())
        call narrow_to_region(point_minimum(), mail_header_end(), status)

        body: block
            if (.not. status%ok()) exit body
            ! Find the first FIELD field and record where it ends.
            if (.not. search_regular_expression(search_pattern, status)) exit body
            moved = forward_line(1_int64)
            found = search_regular_expression("^[^ "//TAB//"]", status)
            if (.not. status%ok()) exit body
            call beginning_of_line()
            first_to_end = marker_at(point(), .true.)

            ! Find each following FIELD field
            ! and combine it with the first FIELD field.
            do while (search_regular_expression(search_pattern, status))
                ! For the second FIELD field, ask user to
                ! approve combining them.
                if (.not. query_asked) then
                    query_asked = .true.
                    query_answer = ask("Message contains multiple "//field//" fields.  Combine? ", status)
                    if (.not. status%ok()) exit
                end if

                if (query_answer) then
                    this_to_start = line_beginning_position()
                    moved = forward_line(1_int64)
                    found = search_regular_expression("^[^ "//TAB//"]", status)
                    if (.not. status%ok()) exit
                    call beginning_of_line()
                    this_to_end = point()
                    ! Get the text of this FIELD field.
                    call buffer_substring(this_to_start, this_to_end, this_to, status)
                    ! Delete it.
                    if (status%ok()) call delete_region(this_to_start, this_to_end, status)
                    if (.not. status%ok()) exit
                    k = point()
                    ! Put a comma after the first FIELD field.
                    call goto_character(marked_position(first_to_end) - 1)
                    call insert(",", status)
                    ! Copy this one after it.
                    call goto_character(marked_position(first_to_end))
                    k = point()
                    call insert(this_to, status)
                    call goto_character(k)
                    ! Replace the FIELD: with spaces.
                    found = looking_at(search_pattern, status)
                    if (.not. status%ok()) exit
                    ! Try to preserve alignment of contents of the field
                    prefix_length = match_end(0, status) - match_beginning(0, status)
                    call replace_match(" ", status)

                    do k = 1, prefix_length - 1
                        call insert(" ", status)
                    end do

                    if (.not. status%ok()) exit
                end if
            end do

            call destroy_marker(first_to_end, status)
        end block body

        call widen()
        if (narrowed) call narrow_to_region(marked_position(mb), marked_position(me), status)
        call destroy_marker(mb, status)
        call destroy_marker(me, status)
        call goto_character(marker_end(here))
    end subroutine combine_mail_fields

    !> Return a coding system to encode the outgoing message of the current buffer.
    !> It at first tries the first coding system found in these variables
    !> in this order:
    !>   (1) local value of `buffer_file_coding_system'
    !>   (2) value of `sendmail_coding_system'
    !>   (3) value of `default_sendmail_coding_system'
    !>   (4) default value of `buffer_file_coding_system'
    !> Port of mule-cmds.el:select-message-coding-system (here, beside the
    !> variables it reads; checking that the coding system can encode the
    !> buffer and asking for another, `select_safe_coding_system', are
    !> Deferred).
    function select_message_coding_system() result(coding)
        character(len=:), allocatable :: coding

        if (buffer_file_coding_system%is_local()) then
            coding = buffer_file_coding_system%value
        else if (allocated(sendmail_coding_system)) then
            coding = sendmail_coding_system
        else if (allocated(default_sendmail_coding_system)) then
            coding = default_sendmail_coding_system
        else if (allocated(buffer_file_coding_system%value)) then
            coding = buffer_file_coding_system%value
        else
            coding = ""
        end if

        ! We should never use no-conversion for outgoing mail.
        if (coding == "no-conversion") coding = ""
    end function select_message_coding_system

    !> Send the current mail buffer using the Sendmail package.
    !> This is a suitable value for `send_mail_function'.  It sends using the
    !> external program defined by `sendmail_program'.  Port of
    !> sendmail.el:sendmail-send-it (expanding mail aliases, which needs
    !> mailalias.el, and RFC 2047 encoding of the header, which rfc2047_mod's
    !> `encode_rfc2047_message_header' does but this does not call yet, are
    !> Deferred).
    recursive subroutine send_mail_with_sendmail(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: errbuf, tembuf, mailbuf, delimline
        type(value_type) :: result, destination, item
        type(value_type), allocatable :: argv(:)
        character(len=:), allocatable :: selected_coding, envelope_from, character_set, errors, resend_to(:), part(:), merged(:), &
                                         address
        type(restriction_type) :: restriction
        logical :: failed, fcc_was_found, multibyte, saved_fold, found, has_errbuf
        integer(int64) :: moved, where_content_type, k, longest
        type(status_type) :: st
        character(len=64) :: exit_text

        status = status_type()
        failed = .false.
        has_errbuf = mail_interactive

        if (mail_interactive) then
            call generate_new_buffer(" sendmail errors", errbuf, status)
            if (.not. status%ok()) return
        end if

        call generate_new_buffer(" sendmail temp", tembuf, status)
        if (.not. status%ok()) return
        multibyte = buffers(current)%multibyte
        saved_fold = case_fold_search%value
        selected_coding = select_message_coding_system()
        mailbuf = current_buffer()
        allocate (character(len=0) :: resend_to(0))
        fcc_was_found = .false.
        ! Examine these variables now, so that
        ! local binding in the mail buffer will take effect.
        envelope_from = ""

        if (mail_specify_envelope_from) then
            ! Only look at the headers when fetching the envelope address.
            call save_restriction_begin(restriction)
            call narrow_to_mail_headers(status)
            if (status%ok()) call get_mail_envelope_from(address, status)
            call save_restriction_end(restriction)
            if (.not. status%ok()) return

            if (allocated(address)) then
                envelope_from = address
            else
                envelope_from = user_mail_address_value()
            end if
        end if

        call set_buffer(tembuf, status)

        body: block
            if (.not. status%ok()) exit body
            case_fold_search%value = .false.
            call erase_buffer(status)
            if (.not. status%ok()) exit body
            if (.not. multibyte) buffers(current)%multibyte = .false.
            call insert_buffer_substring(mailbuf, status)
            if (.not. status%ok()) exit body
            if (len(selected_coding) > 0) call buffer_file_coding_system%set_local(selected_coding)
            call goto_character(point_maximum())
            ! require one newline at the end.
            if (preceding_character() /= 10) call insert(LF, status)
            if (.not. status%ok()) exit body
            ! Change header-delimiter to be what sendmail expects.
            call goto_character(mail_header_end())
            call delete_region(point(), line_end_position(), status)
            if (.not. status%ok()) exit body
            delimline = marker_at(point())
            call goto_character(point_minimum())

            ! Ignore any blank lines in the header
            do
                if (.not. search_regular_expression(LF//LF//LF//"*", status, marked_position(delimline))) exit
                if (point() >= marked_position(delimline)) exit
                call replace_match(LF, status)
                if (.not. status%ok()) exit body
            end do

            if (.not. status%ok()) exit body
            call goto_character(point_minimum())
            ! Look for Resent- headers.  They require sending
            ! the message specially.
            case_fold_search%value = .true.
            call goto_character(point_minimum())

            do while (search_regular_expression("^Resent-\(To\|Cc\|Bcc\):", status, marked_position(delimline)))
                ! Put a list of such addresses in resend-to-addresses.
                call resent_addresses(part, status)
                if (.not. status%ok()) exit body
                longest = len(resend_to)
                if (size(part) > 0) longest = max(longest, int(len(part), int64))
                allocate (character(len=longest) :: merged(size(part) + size(resend_to)))

                do k = 1, size(part)
                    merged(k) = part(k)
                end do

                do k = 1, size(resend_to)
                    merged(size(part) + k) = resend_to(k)
                end do

                call move_alloc(merged, resend_to)
                ! Delete Resent-Bcc ourselves
                k = point()
                call beginning_of_line()
                found = looking_at("resent-bcc", status)
                call goto_character(k)

                if (found) then
                    call delete_region(line_beginning_position(), line_beginning_position(2_int64), status)
                    if (.not. status%ok()) exit body
                end if
            end do

            if (.not. status%ok()) exit body
            ! Don't send out a blank subject line
            call goto_character(point_minimum())

            if (search_regular_expression("^Subject:\([ "//TAB//"]*"//LF//"\)+\b", status, marked_position(delimline))) then
                call replace_match("", status)
            else if (search_regular_expression("^Subject:\([ "//TAB//"]*"//LF//"\)+", status, marked_position(delimline))) then
                ! This one matches a Subject just before the header delimiter.
                if (match_end(0, status) == marked_position(delimline)) call replace_match("", status)
            end if

            if (.not. status%ok()) exit body
            ! Put the "From:" field in unless for some odd reason
            ! they put one in themselves.
            call goto_character(point_minimum())

            if (.not. search_regular_expression("^From:", status, marked_position(delimline))) then
                if (status%ok()) call insert_mail_from_field(status)
            end if

            if (.not. status%ok()) exit body
            ! Possibly add a MIME header for the current coding system
            call goto_character(point_minimum())
            where_content_type = 0
            if (search_regular_expression("^Content-type:", status, marked_position(delimline))) where_content_type = point()
            call goto_character(point_minimum())

            if (mail_send_nonascii%choice == MAIL_SEND_NONASCII_MIME) then
                found = search_regular_expression("^MIME-version:", status, marked_position(delimline))

                if (.not. found) then
                    moved = skip_characters_forward(achar(0)//"-"//achar(127), status)

                    if (point() /= point_maximum() .and. len(selected_coding) > 0) then
                        character_set = coding_system_mime_character_set(selected_coding)

                        if (len(character_set) > 0) then
                            call goto_character(marked_position(delimline))
                            call insert("MIME-version: 1.0"//LF//"Content-type: text/plain; charset="//character_set// &
                                        LF//"Content-Transfer-Encoding: 8bit"//LF, status)

                            ! The character set we will actually use
                            ! should override any specified in the message itself.
                            if (status%ok() .and. where_content_type > 0) then
                                call goto_character(where_content_type)
                                k = line_beginning_position()
                                moved = forward_line(1_int64)
                                call delete_region(k, point(), status)
                            end if
                        end if
                    end if
                end if
            end if

            if (.not. status%ok()) exit body
            ! Find and handle any Fcc fields.
            call goto_character(point_minimum())

            if (search_regular_expression("^Fcc:", status, marked_position(delimline))) then
                fcc_was_found = .true.
                call do_mail_fcc(delimline, status)
                if (.not. status%ok()) exit body
            end if

            if (mail_interactive) then
                call set_buffer(errbuf, status)
                if (status%ok()) call erase_buffer(status)
                call set_buffer(tembuf, st)
                if (.not. status%ok()) exit body
            end if

            call goto_character(point_minimum())

            if (size(resend_to) > 0) then
                found = .true.
            else
                found = search_regular_expression("^To:\|^cc:\|^bcc:", status, marked_position(delimline))
            end if

            if (.not. status%ok()) exit body
            case_fold_search%value = .false.

            if (found) then
                buffers(current)%directory = "/"
                allocate (argv(0))
                call append_argument("-oi")

                if (len(envelope_from) > 0) then
                    call append_argument("-f")
                    call append_argument(envelope_from)
                end if

                if (allocated(mail_alias_file)) call append_argument("-oA"//mail_alias_file)

                if (mail_interactive) then
                    do k = 1, size(sendmail_error_reporting_interactive)
                        call append_argument(trim(sendmail_error_reporting_interactive(k)))
                    end do
                else
                    do k = 1, size(sendmail_error_reporting_non_interactive)
                        call append_argument(trim(sendmail_error_reporting_non_interactive(k)))
                    end do
                end if

                ! Get the addresses from the message
                ! unless this is a resend.
                ! We must not do that for a resend
                ! because we would find the original addresses.
                ! For a resend, include the specific addresses.
                if (size(resend_to) > 0) then
                    do k = 1, size(resend_to)
                        call append_argument(trim(resend_to(k)))
                    end do
                else
                    call append_argument("-t")
                end if

                if (allocated(mail_use_dsn)) then
                    if (size(mail_use_dsn) > 0) then
                        call append_argument("-N")
                        errors = trim(mail_use_dsn(1))

                        do k = 2, size(mail_use_dsn)
                            errors = errors//","//trim(mail_use_dsn(k))
                        end do

                        call append_argument(errors)
                    end if
                end if

                if (mail_interactive) then
                    destination = make_buffer_reference(errbuf)
                else
                    destination = value_type(0_int64)
                end if

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
                    call set_error(status, GENERIC_ERROR_CODE, "Sending...terminated by signal: "//result%a(1)%s)
                    exit body
                case default
                    continue
                end select
            else if (.not. fcc_was_found) then
                call set_error(status, GENERIC_ERROR_CODE, "No recipients")
                exit body
            end if

            if (mail_interactive) then
                call set_buffer(errbuf, status)
                call goto_character(point_minimum())

                do while (search_regular_expression(LF//LF//"* *", status))
                    call replace_match("; ", status)
                end do

                if (buffer_size() /= 0) then
                    failed = .true.
                    errors = buffer_string()
                    call set_error(status, GENERIC_ERROR_CODE, "Sending...failed to "//errors)
                end if
            end if
        end block body

        case_fold_search%value = saved_fold
        call close_buffer(tembuf, st)
        if (is_buffer_live(mailbuf)) call set_buffer(mailbuf, st)

        if (has_errbuf) then
            if (is_buffer_live(errbuf)) then
                if (failed) then
                    call switch_to_buffer_other_window(errbuf, st)
                else
                    call close_buffer(errbuf, st)
                end if
            end if
        end if

    contains

        subroutine append_argument(argument)
            character(len=*), intent(in) :: argument

            item = value_type(text=argument)
            argv = [argv, item]
        end subroutine append_argument
    end subroutine send_mail_with_sendmail

    ! The addresses of the Resent- field at point (its continuation lines
    ! included) as `parse_mail_comma_list' gives them.  Not a port.
    recursive subroutine resent_addresses(addresses, status)
        character(len=:), allocatable, intent(out) :: addresses(:)
        type(status_type), intent(out) :: status
        integer(int64) :: here, e, moved
        type(handle_type) :: mb, me
        logical :: continued

        here = point()
        moved = forward_line(1_int64)

        do
            continued = looking_at("^[ "//TAB//"]", status)
            if (.not. status%ok()) return
            if (.not. continued) exit
            moved = forward_line(1_int64)
        end do

        e = point()
        call goto_character(here)
        mb = marker_at(buffers(current)%beginning_of_accessible_portion)
        me = marker_at(buffers(current)%end_of_accessible_portion, .true.)
        call narrow_to_region(here, e, status)
        if (status%ok()) call parse_mail_comma_list(addresses, status)
        call widen()
        call narrow_to_region(marker_end(mb), marker_end(me), status)
        call goto_character(here)
    end subroutine resent_addresses

    !> Find and act on any Fcc: headers in the current message before HEADER_END.
    !> If a buffer is visiting the Fcc file, append to it before
    !> offering to save it, if it was modified initially.  If there is no buffer,
    !> just append to the file, in Babyl format if necessary.  Port of
    !> sendmail.el:mail-do-fcc (Rmail buffers are handled as other
    !> buffers until Rmail is ported).
    recursive subroutine do_mail_fcc(header_end, status)
        type(handle_type), intent(in) :: header_end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: fcc_list(:), fcc, text
        type(handle_type) :: mailbuf, temporary, buffer, curbuf
        integer(int64) :: beginning, end, beginning2, moved, fcc_start, k
        logical :: dont_write_the_file, buffer_matches_file, modified, babyl, saved_fold, save, read_only
        type(status_type) :: st
        integer :: i

        mailbuf = current_buffer()
        allocate (character(len=0) :: fcc_list(0))
        k = point()
        call goto_character(point_minimum())
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.

        do
            if (.not. search_regular_expression("^Fcc:[ "//TAB//"]*", status, marked_position(header_end))) exit
            fcc_start = point()
            call end_of_line()
            moved = skip_characters_backward(" "//TAB, status)
            call buffer_substring(fcc_start, point(), fcc, status)
            if (.not. status%ok()) exit
            call push_fcc(fcc)
            moved = forward_line(1_int64)
            call delete_region(match_beginning(0, st), point(), status)
            if (.not. status%ok()) exit
        end do

        case_fold_search%value = saved_fold
        if (.not. status%ok()) return
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)

        body: block
            ! This initial newline is not written out if we create a new
            ! file (see below).
            call insert(LF//"From "//user_login_name()//" "//format_time_string("%a %b %e %T %z %Y")//LF// &
                        "Date: "//make_message_date()//LF, status)
            if (.not. status%ok()) exit body
            call insert_buffer_substring(mailbuf, status)
            if (.not. status%ok()) exit body
            ! Make sure messages are separated.
            call goto_character(point_maximum())
            call insert(LF, status)
            call goto_character(2_int64)
            ! ``Quote'' "^From " as ">From "
            !  (note that this isn't really quoting, as there is no requirement
            !   that "^[>]+From " be quoted in the same transparent way.)
            case_fold_search%value = .false.

            do while (search_string(LF//"From ", status))
                call goto_character(point() - 5)
                call insert(">", status)
                if (.not. status%ok()) exit
            end do

            case_fold_search%value = saved_fold
            if (.not. status%ok()) exit body
            curbuf = current_buffer()
            ! the initial blank line
            beginning = point_minimum()
            end = point_maximum()
            ! After the ^From line.
            call goto_character(point_minimum())
            moved = forward_line(2_int64)
            beginning2 = point()

            do i = size(fcc_list), 1, -1
                fcc = trim(fcc_list(i))
                buffer = find_buffer_visiting(fcc)
                dont_write_the_file = .false.

                if (is_buffer_live(buffer)) then
                    ! File is present in a buffer => append to that buffer.
                    call set_buffer(buffer, status)
                    call is_buffer_modified(buffer, modified, status)
                    buffer_matches_file = .not. modified
                    read_only = buffers(current)%read_only
                    buffers(current)%read_only = .false.
                    ! Output file not in Rmail mode => just insert
                    ! at the end.
                    call widen()
                    call goto_character(point_maximum())
                    call insert_buffer_substring(curbuf, status, beginning, end)
                    buffers(current)%read_only = read_only
                    if (.not. status%ok()) exit body

                    ! Offer to save the buffer if it was modified
                    ! before we started.
                    if (.not. buffer_matches_file) then
                        save = ask("Save file "//fcc//"? ", status)
                        if (.not. status%ok()) exit body

                        if (save) then
                            call save_buffer(1_int64, .false., status)
                            if (.not. status%ok()) exit body
                        end if

                        dont_write_the_file = .true.
                    end if

                    call set_buffer(curbuf, status)
                end if

                ! Append to the file directly, unless we've already taken
                ! care of it.
                if (.not. dont_write_the_file) then
                    babyl = .false.

                    if (file_exists(fcc)) then
                        babyl = is_mail_file_babyl(fcc, status)
                        if (.not. status%ok()) exit body
                    end if

                    if (babyl) then
                        ! If the file is a Babyl file, convert the message to
                        ! Babyl format.
                        call buffer_substring(beginning2, end, text, status)
                        if (.not. status%ok()) exit body
                        call write_babyl_message(fcc, text, status)
                    else if (file_exists(fcc)) then
                        ! Ensure there is a blank line between messages, but
                        ! not at the very start of the file.
                        call write_region(fcc, .false., status, point_minimum(), point_maximum(), append=.true.)
                    else
                        call write_region(fcc, .false., status, point_minimum() + 1, point_maximum(), append=.true.)
                    end if

                    if (.not. status%ok()) exit body

                    if (is_buffer_live(buffer)) then
                        call set_buffer(buffer, status)
                        call set_visited_file_modtime_now()
                        call set_buffer(curbuf, status)
                    end if
                end if
            end do
        end block body

        case_fold_search%value = saved_fold
        call close_buffer(temporary, st)
        call set_buffer(mailbuf, st)
        call goto_character(min(k, point_maximum()))

    contains

        subroutine push_fcc(name)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: grown(:)
            integer :: j, n

            n = max(len(fcc_list), len(name))
            allocate (character(len=n) :: grown(size(fcc_list) + 1))

            do j = 1, size(fcc_list)
                grown(j) = fcc_list(j)
            end do

            grown(size(grown)) = name
            call move_alloc(grown, fcc_list)
        end subroutine push_fcc
    end subroutine do_mail_fcc

    ! Append TEXT to the Babyl file FILE as a message.  Port of the
    ! Babyl part of sendmail.el:mail-do-fcc.
    recursive subroutine write_babyl_message(file, text, status)
        character(len=*), intent(in) :: file, text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, temporary
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)
        call insert(achar(12)//LF//"0, unseen,,"//LF//"*** EOOH ***"//LF//"Date: "//mail_rfc822_date()//LF//text// &
                    LF//achar(31), status)
        if (status%ok()) call write_region(file, .false., status, point_minimum(), point_maximum(), append=.true.)
        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine write_babyl_message

    ! Record the visited file's modification time (set-visited-file-modtime).
    ! Not a port.
    subroutine set_visited_file_modtime_now()
        use file_input_output_mod, only: set_visited_file_modtime
        type(status_type) :: st

        call set_visited_file_modtime(status=st)
    end subroutine set_visited_file_modtime_now

    ! The buffer visiting FILE, or the null handle.  Port of
    ! files.el:find-buffer-visiting (in part: without truenames).
    function find_buffer_visiting(file) result(h)
        character(len=*), intent(in) :: file
        type(handle_type) :: h
        character(len=:), allocatable :: expanded
        type(status_type) :: st

        h = handle_type()
        call expand_file_name(file, expanded, st, buffers(current)%directory)
        if (st%ok()) h = get_file_buffer(expanded)
    end function find_buffer_visiting

    ! ------------------------------------------------------------------
    ! Moving to fields
    ! ------------------------------------------------------------------

    !> Move point to end of To field, creating it if necessary.  Port of
    !> sendmail.el:mail-to.
    recursive subroutine mail_to(status)
        type(status_type), intent(out) :: status
        logical :: found

        call expand_abbreviation(status)
        if (status%ok()) call position_mail_on_field("To", found, status)
    end subroutine mail_to

    !> Move point to end of Subject field, creating it if necessary.
    !> Port of sendmail.el:mail-subject.
    recursive subroutine mail_subject(status)
        type(status_type), intent(out) :: status
        logical :: found

        call expand_abbreviation(status)
        if (status%ok()) call position_mail_on_field("Subject", found, status)
    end subroutine mail_subject

    !> Move point to end of Cc field, creating it if necessary.  Port of
    !> sendmail.el:mail-cc.
    recursive subroutine mail_cc(status)
        type(status_type), intent(out) :: status

        call field_after_to("cc", "CC", status)
    end subroutine mail_cc

    !> Move point to end of Bcc field, creating it if necessary.  Port of
    !> sendmail.el:mail-bcc.
    recursive subroutine mail_bcc(status)
        type(status_type), intent(out) :: status

        call field_after_to("bcc", "Bcc", status)
    end subroutine mail_bcc

    !> Move point to end of Mail-Reply-To field, creating it if necessary.
    !> Port of sendmail.el:mail-mail-reply-to.
    recursive subroutine mail_mail_reply_to(status)
        type(status_type), intent(out) :: status

        call field_after_to("mail-reply-to", "Mail-Reply-To", status)
    end subroutine mail_mail_reply_to

    !> Move point to end of Mail-Followup-To field, creating it if necessary.
    !> Port of sendmail.el:mail-mail-followup-to.
    recursive subroutine mail_mail_followup_to(status)
        type(status_type), intent(out) :: status

        call field_after_to("mail-followup-to", "Mail-Followup-To", status)
    end subroutine mail_mail_followup_to

    ! (or (mail-position-on-field FIELD t) (progn (mail-position-on-field
    ! "to") (insert "\nNAME: "))), after `expand_abbreviation'.  Not a port.
    recursive subroutine field_after_to(field, name, status)
        character(len=*), intent(in) :: field, name
        type(status_type), intent(out) :: status
        logical :: found

        call expand_abbreviation(status)
        if (.not. status%ok()) return
        call position_mail_on_field(field, found, status, soft=.true.)
        if (.not. status%ok() .or. found) return
        call position_mail_on_field("to", found, status)
        if (status%ok()) call insert(LF//name//": ", status)
    end subroutine field_after_to

    !> Add a new Fcc field, with file name completion.  Port of
    !> sendmail.el:mail-fcc.
    recursive subroutine mail_fcc(folder, status)
        character(len=*), intent(in) :: folder
        type(status_type), intent(out) :: status
        logical :: found

        call expand_abbreviation(status)
        if (.not. status%ok()) return
        ! Put new field after exiting Fcc.
        call position_mail_on_field("fcc", found, status, soft=.true.)
        if (status%ok() .and. .not. found) call position_mail_on_field("to", found, status)
        if (status%ok()) call insert(LF//"Fcc: "//folder, status)
    end subroutine mail_fcc

    !> Move point to end of Reply-To field, creating it if necessary.
    !> Port of sendmail.el:mail-reply-to.
    recursive subroutine mail_reply_to(status)
        type(status_type), intent(out) :: status
        logical :: found

        call expand_abbreviation(status)
        if (status%ok()) call position_mail_on_field("Reply-To", found, status)
    end subroutine mail_reply_to

    !> Move to the end of the contents of header field FIELD.
    !> If there is no such header, insert one, unless SOFT is true.
    !> If there are multiple FIELD fields, this goes to the first.
    !> FOUND is true if FIELD was originally present.  Port of
    !> sendmail.el:mail-position-on-field.
    recursive subroutine position_mail_on_field(field, found, status, soft)
        character(len=*), intent(in) :: field
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: soft
        integer(int64) :: end, moved
        logical :: saved_fold, other, is_soft

        is_soft = .false.
        if (present(soft)) is_soft = soft
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        end = mail_header_end()
        call goto_character(point_minimum())
        found = search_regular_expression("^"//regular_expression_quote(field)//":", status, end)

        if (found .and. status%ok()) then
            other = search_regular_expression("^[^ "//TAB//"]", status, move=.true.)
            call beginning_of_line()
            moved = skip_characters_backward(LF, status)
        else if (status%ok() .and. .not. is_soft) then
            call goto_character(end)
            call insert(field//": "//LF, status)
            moved = skip_characters_backward(LF, status)
        end if

        case_fold_search%value = saved_fold
    end subroutine position_mail_on_field

    !> Move point to beginning of text field.  Port of sendmail.el:mail-text.
    recursive subroutine mail_text(status)
        type(status_type), intent(out) :: status

        call expand_abbreviation(status)
        if (status%ok()) call goto_character(mail_text_start())
    end subroutine mail_text

    !> Insert a Disposition-Notification-To header, if it doesn't already
    !> exist.  Port of sendmail.el:mail-insert-disposition-notification-to.
    recursive subroutine insert_mail_disposition_notification_to(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: value, text
        type(handle_type) :: here, mb, me, old, temporary
        logical :: found, narrowed
        type(status_type) :: st

        call expand_abbreviation(status)
        if (.not. status%ok()) return
        here = marker_at(point())
        call position_mail_on_field("Disposition-Notification-To", found, status)

        if (status%ok() .and. .not. found) then
            narrowed = buffers(current)%beginning_of_accessible_portion /= 1 .or. buffers(current)%end_of_accessible_portion &
                       /= buffers(current)%text%z
            mb = marker_at(buffers(current)%beginning_of_accessible_portion)
            me = marker_at(buffers(current)%end_of_accessible_portion, .true.)

            k_block: block
                type(handle_type) :: at
                at = marker_at(point())
                call narrow_to_mail_headers(status)
                if (status%ok()) call get_mail_field("Reply-To", value, status)
                if (status%ok() .and. .not. allocated(value)) call get_mail_field("From", value, status)
                call widen()
                if (narrowed) call narrow_to_region(marked_position(mb), marked_position(me), st)
                call goto_character(marker_end(at))
            end block k_block

            call destroy_marker(mb, st)
            call destroy_marker(me, st)

            if (status%ok() .and. .not. allocated(value)) then
                old = current_buffer()
                call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
                call set_buffer(temporary, status)
                call insert_mail_from_field(status)
                text = buffer_string()
                call close_buffer(temporary, st)
                call set_buffer(old, st)
                value = text(len("From: ") + 1:len(text) - 1)
            end if

            if (status%ok()) call insert(value, status)
        end if

        call goto_character(marker_end(here))
    end subroutine insert_mail_disposition_notification_to

    ! ------------------------------------------------------------------
    ! Signatures and citations
    ! ------------------------------------------------------------------

    !> Sign letter with signature.
    !> If the variable `mail_signature_variable' has its text set,
    !> inserts that text; if it has its procedure set, calls it.
    !> Otherwise (.true. or .false.), inserts the
    !> contents of the file `mail_signature_file'.
    !> ATPOINT means insert at point rather than the end.  Port of
    !> sendmail.el:mail-signature.
    recursive subroutine mail_signature(status, atpoint, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: atpoint, interactive
        integer(int64) :: moved, inserted
        type(handle_type) :: here
        character(len=:), allocatable :: file
        logical :: at, from_file

        status = status_type()
        at = .false.
        if (present(atpoint)) at = atpoint
        from_file = .not. (allocated(mail_signature_variable%text) .or. associated(mail_signature_variable%procedure))

        ! Test for an unreadable file here, before we delete trailing
        ! whitespace, so that we don't modify the buffer needlessly.
        if (from_file .and. .not. is_file_readable(mail_signature_file)) then
            if (present(interactive)) then
                if (interactive) call message("The signature file `"//mail_signature_file//"' could not be read")
            end if

            return
        end if

        here = marker_at(point())

        if (.not. at) then
            call goto_character(point_maximum())
            ! Delete trailing whitespace and blank lines.
            moved = skip_characters_backward(" "//TAB//LF, status)
            call goto_character(line_end_position())
            call delete_region(point(), point_maximum(), status)
        end if

        if (status%ok()) then
            if (allocated(mail_signature_variable%text)) then
                call insert(mail_signature_variable%text, status)
            else if (associated(mail_signature_variable%procedure)) then
                call mail_signature_variable%procedure(status)
            else
                call insert(LF//LF//"-- "//LF, status)
                if (status%ok()) call expand_file_name(mail_signature_file, file, status)
                if (status%ok()) call insert_file_contents(file, .false., inserted, status)
            end if
        end if

        call goto_character(marker_end(here))
    end subroutine mail_signature

    !> Modify text just inserted from a message to be cited.
    !> The inserted text should be the region.
    !> When this function returns, the region is again around the modified text.
    !>
    !> Normally, indent each nonblank line `mail_indentation_spaces' spaces.
    !> However, if `mail_paste_prefix' is allocated, insert that prefix on
    !> each line.  Port of sendmail.el:mail-indent-citation.
    recursive subroutine indent_mail_citation(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e, moved
        type(handle_type) :: end, here

        b = min(point(), mark_position())
        e = max(point(), mark_position())
        call clear_mail_paste_headers(b, e, status)
        if (.not. status%ok()) return
        b = min(point(), mark_position())
        e = max(point(), mark_position())

        if (.not. allocated(mail_paste_prefix)) then
            call indent_rigidly(b, e, int(mail_indentation_spaces, int64), status)
        else
            here = marker_at(point())
            end = marker_at(e)
            call goto_character(b)

            do while (point() < marked_position(end))
                call insert(mail_paste_prefix, status)
                if (.not. status%ok()) exit
                moved = forward_line(1_int64)
            end do

            call destroy_marker(end, status)
            call goto_character(marker_end(here))
        end if
    end subroutine indent_mail_citation

    !> Insert the message being replied to, if any.
    !> Puts point after the text and mark before.
    !> Normally, indents each nonblank line ARGUMENT spaces (default 3).
    !> However, if `mail_paste_prefix' is allocated, insert that prefix on
    !> each line.
    !>
    !> Just \[universal_argument] as argument (RAW) means don't indent,
    !> insert no prefix, and don't delete any header fields.  Port of
    !> sendmail.el:mail-yank-original (with a buffer as the message being
    !> replied to; the action form comes with Rmail).
    recursive subroutine paste_mail_original(status, argument, raw)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: argument
        logical, intent(in), optional :: raw
        type(handle_type) :: original, mark
        integer(int64) :: start, end_of_headers, e
        integer :: k, saved_spaces
        logical :: is_raw, has_mark
        type(status_type) :: st

        status = status_type()
        k = local_index()
        if (k == 0) return
        original = locals(k)%reply_buffer
        if (.not. is_buffer_live(original)) return
        is_raw = .false.
        if (present(raw)) is_raw = raw
        start = point()
        ! insert-buffer: insert the contents, mark after them.
        call insert_buffer_substring(original, status)
        if (.not. status%ok()) return
        call push_mark(point())
        call goto_character(start)

        if (.not. is_raw) then
            call goto_character(start)
            saved_spaces = mail_indentation_spaces
            if (present(argument)) mail_indentation_spaces = argument

            if (.not. mail_citation_hook%empty()) then
                ! Bind mail_citation_header to the inserted
                ! message's header.
                call narrow_to_region(start, point_maximum(), status)
                call goto_character(start)
                call rfc822_goto_end_of_headers(st)
                end_of_headers = point()
                call widen()
                call goto_character(start)
                call buffer_substring_no_properties(start, end_of_headers, mail_citation_header, status)
                if (status%ok()) call mail_citation_hook%execute(status)
                if (allocated(mail_citation_header)) deallocate (mail_citation_header)
            else
                call indent_mail_citation(status)
            end if

            mail_indentation_spaces = saved_spaces
            if (.not. status%ok()) return
        end if

        ! This is like exchange-point-and-mark, but doesn't activate the mark.
        ! It is cleaner to avoid activation, even though the command
        ! loop would deactivate the mark because we inserted text.
        has_mark = mark_position() > 0

        if (has_mark) then
            e = mark_position()
            call mark_marker(mark)
            call set_marker(mark, point(), status, current_buffer())
            call goto_character(e)
        end if

        if (.not. is_end_of_line()) call insert(LF, status)
    end subroutine paste_mail_original

    !> Delete the headers matching `mail_paste_ignored_headers' from the
    !> message between START and END.  Port of
    !> sendmail.el:mail-yank-clear-headers.
    recursive subroutine clear_mail_paste_headers(start_in, end_in, status)
        integer(int64), intent(in) :: start_in, end_in
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, here
        type(handle_type) :: mb, me
        logical :: saved_fold, found, narrowed

        status = status_type()
        start = min(start_in, end_in)
        end = max(start_in, end_in)
        if (.not. allocated(mail_paste_ignored_headers)) return
        here = point()
        call goto_character(start)

        if (search_string(LF//LF, status, end)) then
            narrowed = buffers(current)%beginning_of_accessible_portion /= 1 .or. buffers(current)%end_of_accessible_portion &
                       /= buffers(current)%text%z
            mb = marker_at(buffers(current)%beginning_of_accessible_portion)
            me = marker_at(buffers(current)%end_of_accessible_portion, .true.)
            call narrow_to_region(start, point(), status)
            call goto_character(start)
            saved_fold = case_fold_search%value

            do while (status%ok())
                case_fold_search%value = .true.
                found = search_regular_expression(mail_paste_ignored_headers, status)
                case_fold_search%value = saved_fold
                if (.not. found) exit
                call beginning_of_line()
                here = point()
                if (.not. search_regular_expression(LF//"[^ "//TAB//"]", status)) exit
                call forward_character(status, -1_int64)
                call delete_region(here, point(), status)
            end do

            call widen()
            if (narrowed) call narrow_to_region(marked_position(mb), marked_position(me), status)
            call destroy_marker(mb, status)
            call destroy_marker(me, status)
        end if

        call goto_character(min(start, point_maximum()))
    end subroutine clear_mail_paste_headers

    !> Insert the selected region from the message being replied to.
    !> Puts point after the text and mark before.
    !> Normally, indents each nonblank line ARGUMENT spaces (default 3).
    !> However, if `mail_paste_prefix' is allocated, insert that prefix on
    !> each line.
    !>
    !> Just \[universal_argument] as argument means don't indent, insert no prefix,
    !> and don't delete any header fields.  Port of
    !> sendmail.el:mail-yank-region (it acts only when the message being
    !> replied to is given as an action, which comes with Rmail; with a
    !> buffer it does nothing, as upstream).
    recursive subroutine paste_mail_region(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine paste_mail_region

    !> Insert a file at the end of the buffer, with separator lines around
    !> it.  Port of sendmail.el:mail-insert-file.
    recursive subroutine insert_mail_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        integer(int64) :: here, start, middle, inserted, column

        here = point()
        call goto_character(point_maximum())

        if (.not. is_beginning_of_line()) then
            call insert(LF, status)
            if (.not. status%ok()) return
        end if

        call insert(LF, status)
        if (.not. status%ok()) return
        start = point()
        call insert("===File "//file//"===", status)
        if (.not. status%ok()) return
        column = current_column()
        call insert(repeat("=", int(max(0_int64, 60 - column)))//LF, status)
        if (.not. status%ok()) return
        middle = point()
        call insert(repeat("=", 60)//LF, status)
        if (.not. status%ok()) return
        call push_mark()
        call goto_character(middle)
        call insert_file_contents(file, .false., inserted, status)
        if (.not. status%ok()) return
        if (.not. is_beginning_of_line()) call insert(LF, status)
        call goto_character(start)
        call goto_character(here)
    end subroutine insert_mail_file

    ! ------------------------------------------------------------------
    ! The mail command
    ! ------------------------------------------------------------------

    !> Edit a message to be sent.  Prefix arg means resume editing (don't erase).
    !> When this function returns, the buffer `*mail*' is selected.
    !> INITIALIZED is .true. if the message was newly initialized;
    !> otherwise, .false..
    !>
    !> Optionally, the signature file `mail_signature_file' can be inserted at the
    !> end; see the variable `mail_signature_variable'.
    !>
    !> \<mail_mode_map>
    !> While editing message, type \[send_mail_and_exit] to send the message and exit.
    !>
    !> Various special commands starting with C-c are available in sendmail mode
    !> to move to message header fields:
    !> \{mail_mode_map}
    !>
    !> If `mail_self_blind' is true, a Bcc to yourself is inserted
    !> when the message is initialized.
    !>
    !> If `mail_default_reply_to' is allocated, it should be an address (a string);
    !> a Reply-To: field with that address is inserted.
    !>
    !> If `mail_archive_file_name' is allocated, an Fcc field with that file name
    !> is inserted.
    !>
    !> The normal hook `set_up_mail_hook' is run after the message is
    !> initialized.  It can add more default fields to the message.
    !>
    !> The first argument, NOERASE, determines what to do when there is
    !> an existing modified `*mail*' buffer.  If NOERASE is MAIL_NOERASE_NO,
    !> the existing mail buffer is used, and the user is prompted whether to
    !> keep the old contents or to erase them.  If NOERASE is
    !> MAIL_NOERASE_NEW, a new mail buffer will be created instead of using
    !> the old one.  MAIL_NOERASE_YES means to always select the old
    !> buffer without erasing the contents.
    !>
    !> The arguments TO, SUBJECT, IN_REPLY_TO and CC, if present, specify
    !> the initial contents of those header fields.
    !> These arguments should not have final newlines.
    !> REPLYBUFFER is a buffer which contains an
    !> original message being replied to.  Port of sendmail.el:mail (the
    !> actions, auto-saving and `recover_mail' are Deferred).
    recursive subroutine mail(status, noerase_in, to, subject, in_reply_to, cc, replybuffer, initialized)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: noerase_in
        character(len=*), intent(in), optional :: to, subject, in_reply_to, cc
        type(handle_type), intent(in), optional :: replybuffer
        logical, intent(out), optional :: initialized
        type(handle_type) :: buffer
        character(len=:), allocatable :: directory
        integer :: noerase
        logical :: initialize, modified, proceed

        noerase = MAIL_NOERASE_NO
        if (present(noerase_in)) noerase = noerase_in
        if (present(initialized)) initialized = .false.

        if (noerase == MAIL_NOERASE_NEW) then
            call generate_new_buffer("*mail*", buffer, status)
        else
            if (noerase /= MAIL_NOERASE_NO .and. .not. is_buffer_live(get_buffer("*mail*"))) noerase = MAIL_NOERASE_NO
            call get_or_make_buffer("*mail*", buffer, status)
        end if

        if (.not. status%ok()) return
        call pop_to_buffer_same_window(buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        ! Avoid danger that the auto-save file can't be written.
        call expand_file_name(file_name_as_directory(mail_default_directory), directory, status)
        if (status%ok() .and. file_exists(directory)) buffers(current)%directory = directory
        call mail_mode(status)
        if (.not. status%ok()) return
        initialize = .false.

        if (.not. (noerase /= MAIL_NOERASE_NO .and. noerase /= MAIL_NOERASE_NEW)) then
            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) return

            if (allocated(buffers(current)%filename)) then
                if (modified) then
                    proceed = ask("Buffer has unsaved changes; reinitialize it and discard them? ", status)

                    if (proceed) then
                        if (ask("Disconnect buffer from visited file? ", status)) call set_visited_file_name("", status)
                    end if
                else
                    proceed = ask("Reinitialize buffer, and disconnect it from the visited file? ", status)
                    if (proceed) call set_visited_file_name("", status)
                end if
            else if (modified) then
                ! A non-file-visiting buffer.
                proceed = ask("Unsent message being composed; erase it? ", status)
            else
                proceed = .true.
            end if

            if (.not. status%ok()) return

            if (proceed) then
                call erase_buffer(status)
                if (.not. status%ok()) return
                call set_up_mail(status, to, subject, in_reply_to, cc, replybuffer)
                if (.not. status%ok()) return
                initialize = .true.
            end if
        end if

        if (present(initialized)) initialized = initialize
    end subroutine mail

    !> Like `mail' command, but display mail buffer in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of sendmail.el:mail-other-window.
    recursive subroutine mail_other_window(status, noerase, to, subject, in_reply_to, cc, replybuffer)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: noerase
        character(len=*), intent(in), optional :: to, subject, in_reply_to, cc
        type(handle_type), intent(in), optional :: replybuffer
        type(handle_type) :: buffer

        call get_or_make_buffer("*mail*", buffer, status)
        if (status%ok()) call switch_to_buffer_other_window(buffer, status)
        if (status%ok()) call mail(status, noerase, to, subject, in_reply_to, cc, replybuffer)
    end subroutine mail_other_window

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The `*' of an interactive spec.  Not a port.
    logical function read_only(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        read_only = .not. status%ok()
    end function read_only

    ! The raw prefix argument as NOERASE.  Not a port.
    integer function noerase_argument()
        noerase_argument = MAIL_NOERASE_NO
        if (current_prefix_argument%kind /= PREFIX_NONE) noerase_argument = MAIL_NOERASE_YES
    end function noerase_argument

    recursive subroutine execute_mail(status)
        type(status_type), intent(out) :: status

        call mail(status, noerase_argument())
    end subroutine execute_mail

    recursive subroutine execute_mail_other_window(status)
        type(status_type), intent(out) :: status

        call mail_other_window(status, noerase_argument())
    end subroutine execute_mail_other_window

    recursive subroutine execute_send_mail_and_exit(status)
        type(status_type), intent(out) :: status

        call send_mail_and_exit(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_send_mail_and_exit

    recursive subroutine execute_mail_dont_send(status)
        type(status_type), intent(out) :: status

        call mail_dont_send(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_mail_dont_send

    recursive subroutine execute_send_mail(status)
        type(status_type), intent(out) :: status

        call send_mail(status)
    end subroutine execute_send_mail

    recursive subroutine execute_mail_fcc(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: folder

        call read_file_name("Folder carbon copy: ", folder, status)
        if (status%ok()) call mail_fcc(folder, status)
    end subroutine execute_mail_fcc

    recursive subroutine execute_mail_signature(status)
        type(status_type), intent(out) :: status

        if (read_only(status)) return
        call mail_signature(status, current_prefix_argument%kind /= PREFIX_NONE, .true.)
    end subroutine execute_mail_signature

    recursive subroutine execute_paste_mail_original(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_RAW) then
            call paste_mail_original(status, raw=.true.)
        else if (current_prefix_argument%kind /= PREFIX_NONE) then
            call paste_mail_original(status, int(prefix_numeric_value(current_prefix_argument)))
        else
            call paste_mail_original(status)
        end if
    end subroutine execute_paste_mail_original

    recursive subroutine execute_insert_mail_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Attach file: ", file, status, mustmatch=REQUIRE_MATCH_YES)
        if (status%ok()) call insert_mail_file(file, status)
    end subroutine execute_insert_mail_file

    !> Define Mail mode, its keymap, syntax table and the commands, and
    !> `sendmail_user_agent'.  Not a port.
    subroutine initialize_sendmail()
        use regular_expression_optimization_mod, only: regular_expression_optimization
        type(status_type) :: st
        character(len=:), allocatable :: path

        if (.not. allocated(mail_header_separator)) mail_header_separator = "--text follows this line--"
        if (.not. allocated(mail_personal_alias_file)) mail_personal_alias_file = "~/.mailrc"
        if (.not. allocated(mail_paste_prefix)) mail_paste_prefix = "> "
        if (.not. allocated(mail_signature_file)) mail_signature_file = "~/.signature"
        if (.not. allocated(mail_default_directory)) mail_default_directory = "~/"
        if (.not. allocated(default_sendmail_coding_system)) default_sendmail_coding_system = "utf-8"
        if (.not. allocated(mail_use_dsn)) allocate (character(len=7) :: mail_use_dsn(0))
        if (.not. allocated(mail_mailing_lists)) allocate (character(len=0) :: mail_mailing_lists(0))

        if (.not. allocated(mail_citation_prefix_regular_expression)) then
            ! Use [:word:] rather than \w so we don't get tripped up if one
            ! of those chars has a weird `syntax-table' text property.
            mail_citation_prefix_regular_expression = "\(?:[ "//TAB//"]*\(?:[[:word:]_.]+>\|[>|]\)\)+"
        end if

        if (.not. allocated(sendmail_error_reporting_interactive)) then
            sendmail_error_reporting_interactive = [character(len=4) :: "-oep", "-odi"]
        end if

        if (.not. allocated(sendmail_error_reporting_non_interactive)) then
            sendmail_error_reporting_non_interactive = [character(len=4) :: "-oem", "-odb"]
        end if

        if (.not. allocated(mail_paste_ignored_headers)) then
            mail_paste_ignored_headers = "^"//regular_expression_optimization([character(len=18) :: "via", "mail-from", "origin", &
                                                                               "status", &
                                                                       "remailed", "received", "message-id", "summary-line", "to", &
                                                                         "subject", "in-reply-to", "return-path", "mail-reply-to", &
                                                                    "x-rmail-attributes", "x-rmail-keywords", "mail-followup-to"], &
                                                                              open="\(?:")//":"
        end if

        if (.not. allocated(sendmail_program)) then
            call executable_find("sendmail", path)

            if (len(path) > 0) then
                sendmail_program = path
            else if (file_exists("/usr/sbin/sendmail")) then
                sendmail_program = "/usr/sbin/sendmail"
            else if (file_exists("/usr/lib/sendmail")) then
                sendmail_program = "/usr/lib/sendmail"
            else if (file_exists("/usr/ucblib/sendmail")) then
                sendmail_program = "/usr/ucblib/sendmail"
            else
                sendmail_program = "sendmail"
            end if
        end if

        if (.not. associated(major_modes%text_mode%execute)) call initialize_text_mode()

        if (mail_mode_syntax_table%index == 0) then
            ! define-derived-mode will make it inherit from text-mode-syntax-table.
            call make_syntax_table(mail_mode_syntax_table, st, text_mode_syntax_table)
            ! FIXME this is probably very obsolete now ("percent hack").
            call modify_syntax_entry(iachar("%"), iachar("%"), ". ", st, mail_mode_syntax_table)
        end if

        call major_modes%mail_mode%define("mail_mode", "Mail", mail_mode, mail_mode_hook, parent=major_modes%text_mode, &
                                          syntax_table=mail_mode_syntax_table)
        mail_mode_map = major_modes%mail_mode%map
        commands%mail_mode%execute => mail_mode
        commands%mail%execute => execute_mail
        commands%mail_other_window%execute => execute_mail_other_window
        commands%mail_to%execute => mail_to
        commands%mail_bcc%execute => mail_bcc
        commands%mail_fcc%execute => execute_mail_fcc
        commands%mail_cc%execute => mail_cc
        commands%mail_subject%execute => mail_subject
        commands%mail_reply_to%execute => mail_reply_to
        commands%mail_mail_reply_to%execute => mail_mail_reply_to
        commands%mail_mail_followup_to%execute => mail_mail_followup_to
        commands%insert_mail_disposition_notification_to%execute => insert_mail_disposition_notification_to
        commands%mail_text%execute => mail_text
        commands%paste_mail_original%execute => execute_paste_mail_original
        commands%paste_mail_region%execute => paste_mail_region
        commands%mail_signature%execute => execute_mail_signature
        commands%send_mail_and_exit%execute => execute_send_mail_and_exit
        commands%send_mail%execute => execute_send_mail
        commands%mail_dont_send%execute => execute_mail_dont_send
        commands%insert_mail_file%execute => execute_insert_mail_file
        call keymap_set(mail_mode_map, "C-c ?", "describe_mode", st)
        call keymap_set(mail_mode_map, "C-c C-f C-t", "mail_to", st)
        call keymap_set(mail_mode_map, "C-c C-f C-b", "mail_bcc", st)
        call keymap_set(mail_mode_map, "C-c C-f C-f", "mail_fcc", st)
        call keymap_set(mail_mode_map, "C-c C-f C-c", "mail_cc", st)
        call keymap_set(mail_mode_map, "C-c C-f C-s", "mail_subject", st)
        call keymap_set(mail_mode_map, "C-c C-f C-r", "mail_reply_to", st)
        ! author
        call keymap_set(mail_mode_map, "C-c C-f C-a", "mail_mail_reply_to", st)
        ! list
        call keymap_set(mail_mode_map, "C-c C-f C-l", "mail_mail_followup_to", st)
        call keymap_set(mail_mode_map, "C-c C-f C-d", "insert_mail_disposition_notification_to", st)
        call keymap_set(mail_mode_map, "C-c C-t", "mail_text", st)
        call keymap_set(mail_mode_map, "C-c C-y", "paste_mail_original", st)
        call keymap_set(mail_mode_map, "C-c C-r", "paste_mail_region", st)
        call keymap_set(mail_mode_map, "C-c C-w", "mail_signature", st)
        call keymap_set(mail_mode_map, "C-c C-c", "send_mail_and_exit", st)
        call keymap_set(mail_mode_map, "C-c C-s", "send_mail", st)
        call keymap_set(mail_mode_map, "C-c C-i", "insert_mail_file", st)
        call sendmail_user_agent%define("sendmail_user_agent", compose_sendmail_user_agent, commands%send_mail_and_exit, &
                                        hook=mail_send_hook)
        call append_locals_clearer(clear_sendmail_locals)
    end subroutine initialize_sendmail

    !> Forget the text and procedure of `mail_signature_variable', as any
    !> assignment to it does.  Not a port.
    subroutine mail_signature_variable_clear_kinds(self)
        class(mail_signature_variable_choice_setting_type), intent(inout) :: self

        if (allocated(self%text)) deallocate (self%text)
        nullify (self%procedure)
    end subroutine mail_signature_variable_clear_kinds

    !> The text or procedure of `mail_signature_variable' as help shows it
    !> (`%text = "..."'); "" when neither is set.  Not a port.
    function mail_signature_variable_other_value(self) result(text)
        class(mail_signature_variable_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (allocated(self%text)) text = "%text = """//self%text//""""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function mail_signature_variable_other_value

    !> The assignment that sets the number or procedure `mail_signature_variable' takes, for the error of a rejected
    !> value.  Not a port.
    function mail_signature_variable_kind_hint(self, number) result(text)
        class(mail_signature_variable_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a string is `"//trim(self%name)//"%text = ""...""' and a procedure `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function mail_signature_variable_kind_hint

    !> The envelope mail address to use when sending mail, in ADDRESS
    !> (unallocated for none).
    !> This procedure uses the `mail_envelope_from' variable.
    !>
    !> The buffer should be narrowed to the headers of the mail message
    !> before this procedure is called.  Port of
    !> sendmail.el:mail-envelope-from.
    recursive subroutine get_mail_envelope_from(address, status)
        character(len=:), allocatable, intent(out) :: address
        type(status_type), intent(out) :: status
        type(mail_address_components_type), allocatable :: components(:)
        character(len=:), allocatable :: from

        status = status_type()

        if (allocated(mail_envelope_from%address)) then
            address = mail_envelope_from%address
        else if (mail_envelope_from%choice == MAIL_ENVELOPE_FROM_HEADER) then
            call get_mail_field("From", from, status)
            if (.not. status%ok()) return

            ! Upstream passes the missing field on to
            ! mail-extract-address-components, which refuses it.
            if (.not. allocated(from)) then
                call set_error(status, GENERIC_ERROR_CODE, "Invalid address: .false.")
                return
            end if

            call extract_mail_address_components(from, components, status)
            if (.not. status%ok()) return

            if (size(components) > 0) then
                if (allocated(components(1)%address)) address = components(1)%address
            end if
        end if
    end subroutine get_mail_envelope_from

    !> Forget the address of `mail_envelope_from', as any assignment to it
    !> does.  Not a port.
    subroutine mail_envelope_from_clear_kinds(self)
        class(mail_envelope_from_choice_setting_type), intent(inout) :: self

        if (allocated(self%address)) deallocate (self%address)
    end subroutine mail_envelope_from_clear_kinds

    !> The address of `mail_envelope_from' as help shows it
    !> (`%address = "..."'); "" when none is set.  Not a port.
    function mail_envelope_from_other_value(self) result(text)
        class(mail_envelope_from_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (allocated(self%address)) text = "%address = """//self%address//""""
    end function mail_envelope_from_other_value

    !> The assignment that sets the address `mail_envelope_from' takes, for
    !> the error of a rejected value.  Not a port.
    function mail_envelope_from_kind_hint(self, number) result(text)
        class(mail_envelope_from_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "an address is `"//trim(self%name)//"%address = ""...""'"
        if (len(number) < 0) text = number
    end function mail_envelope_from_kind_hint
end module send_mail_mod
