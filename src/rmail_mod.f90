! rmail_mod.f90 -- the Rmail mail reader: mbox files, new mail, Rmail mode.
! Ported from GNU Emacs 31.1 lisp/mail/rmail.el (in part: see
! docs/status/rmail_mod.md).
!
! Copyright (C) 1985-1988, 1993-1998, 2000-2026 Free Software
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

!> Rmail: \[rmail] visits the mail file `rmail_file_name' (an mbox
!> file), moves new mail into it from the inboxes (`rmail_primary_inboxes',
!> the MAIL environment variable or the user's file in
!> `rmail_spool_directory', through movemail_mod) and shows the first
!> unseen message in Rmail mode.  The file's buffer (the Rmail buffer)
!> shows one message at a time: the message collection is kept in a
!> hidden viewer buffer whose text is swapped with the Rmail buffer's
!> (`swap_buffer_text'), and swapped back whenever the collection is
!> needed.  Each message keeps its attributes (deleted, unseen,
!> answered, ...) in an X-RMAIL-ATTRIBUTES header field and its labels
!> in X-RMAIL-KEYWORDS.  See docs/status/rmail_mod.md for what is
!> deferred.
module rmail_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, USER_ERROR_CODE, VOID_FUNCTION_CODE, QUIT_CODE
    use character_mod, only: downcase_ascii, string_as_unibyte
    use handle_mod, only: handle_type, null_handle
    use string_type_mod, only: string_type, append_string
    use hooks_mod, only: hook_type, change_major_mode_hook
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_name, get_file_buffer, &
                          set_buffer_modified, restore_buffer_modified, is_buffer_modified, swap_buffer_text, &
                          buffer_serial_live, erase_buffer, close_buffer, generate_new_buffer_name, &
                          get_or_make_buffer, transient_mark_mode
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, &
                                     is_end_of_buffer, is_beginning_of_buffer, buffer_substring, &
                                     buffer_substring_no_properties, delete_region, narrow_to_region, widen, &
                                     insert_buffer_substring, line_end_position, line_beginning_position, character_after, &
                                     preceding_character, buffer_size, save_restriction_begin, save_restriction_end, &
                                     save_excursion_begin, save_excursion_end, restriction_type, excursion_type, &
                                     user_login_name
    use insert_delete_mod, only: insert, insert_character
    use editing_commands_mod, only: forward_line, forward_character, end_of_line, beginning_of_line, delete_character
    use syntax_mod, only: skip_characters_forward, skip_characters_backward
    use search_mod, only: regular_expression_search_forward, search_forward, search_backward, looking_at, &
                          match_beginning, match_end, &
                          string_match, regular_expression_quote, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE, &
                          regular_expression_search_backward
    use basic_subprograms_mod, only: read_y_or_n, looking_back
    use utility_functions_mod, only: read_yes_or_no
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker, detach_marker
    use file_input_output_mod, only: expand_file_name, file_exists, file_name_directory, file_name_nondirectory, &
                                     is_file_directory, substitute_in_file_name, file_attributes, insert_file_contents, &
                                     write_region, write_region_annotate_functions, rename_file, &
                                     is_visited_file_modtime_current
    use files_mod, only: find_file_noselect, find_file, file_truename, delete_file, save_buffer, after_save_hook, &
                         close_buffer_hook, abbreviate_file_name
    use system_dependent_mod, only: file_status_type
    use environment_mod, only: getenv
    use display_engine_mod, only: message, set_mode_line_process, clear_mode_line_process, mode_line_part_type
    use display_update_mod, only: ding
    use keyboard_mod, only: sit_for
    use window_mod, only: switch_to_buffer, quit_window, replace_buffer_in_windows, bury_buffer, get_buffer_window, &
                          display_buffer, recenter, window_body_height
    use minibuffer_reading_mod, only: read_string
    use minibuffer_mod, only: read_file_name
    use simple_mod, only: deactivate_mark
    use keymap_mod, only: keymap_set, use_local_map
    use frame_mod, only: selected_window
    use modes_mod, only: major_modes
    use basic_subprograms_mod, only: execute_mode_hooks
    use data_mod, only: clear_all_local_variables, choice_setting_type
    use text_mode_mod, only: text_mode_syntax_table, initialize_text_mode
    use interval_tree_mod, only: make_overlay, overlay_put, move_overlay
    use value_mod, only: value_type, make_list, list
    use faces_mod, only: named_face_type, display_when, face_attributes
    use coding_mod, only: coding_system_for_read, buffer_file_coding_system, last_coding_system_used, is_coding_system, &
                          coding_system_change_end_of_line_conversion, decode_coding_string, END_OF_LINE_UNIX, END_OF_LINE_DOS
    use multilingual_environment_mod, only: coding_system_from_name
    use mail_utilities_mod, only: get_mail_field, unquote_mail_printable_region, strip_mail_quoted_names
    use utility_functions_mod, only: base64_decode_region
    use execute_process_mod, only: execute_process
    use movemail_mod, only: move_mail
    implicit none(type, external)
    private

    public :: rmail, rmail_mode, rmail_input, get_rmail_new_mail, show_rmail_message, next_rmail_message, &
              previous_rmail_message, next_rmail_undeleted_message, previous_rmail_undeleted_message, show_first_rmail_message, &
              show_last_rmail_message, search_rmail, search_rmail_backwards, delete_rmail_message, delete_rmail_message_forward, &
              delete_rmail_message_backward, undelete_previous_rmail_message, expunge_rmail_only, expunge_rmail, &
              expunge_and_save_rmail, quit_rmail, bury_rmail, toggle_rmail_header, beginning_of_rmail_message, &
              end_of_rmail_message, widen_rmail
    public :: set_up_rmail_mode_1, set_rmail_variables, get_rmail_header, set_rmail_header, get_rmail_attribute_names, &
              get_rmail_keywords, get_rmail_labels, display_rmail_labels, set_rmail_attribute, has_rmail_message_attributes, &
              is_rmail_message_unseen, has_rmail_message_labels, rmail_message_beginning, rmail_message_end, &
              apply_in_rmail_message_begin, apply_in_rmail_message_end, set_rmail_message_counters_maybe, &
              count_rmail_new_messages, &
              set_rmail_message_counters, is_rmail_message_pruned, show_rmail_message_1, highlight_rmail_headers, &
              get_rmail_message_at, get_first_rmail_unseen_message, is_rmail_message_deleted, set_rmail_message_deleted, &
              has_rmail_summary, is_rmail_summary_displayed, display_rmail_summary_maybe, select_rmail_summary, &
              swap_rmail_buffers, swap_rmail_buffers_maybe, is_rmail_buffers_swapped, modify_rmail_format, &
              keep_rmail_format, ensure_rmail_blank_line, get_rmail_coding_system, is_rmail_text, rmail_buffer, &
              set_rmail_buffer, rmail_state_index, set_rmail_bad_format_error, annotate_rmail_write_region, &
              rmail_summary_procedure, pop_to_rmail_buffer, read_rmail_search, is_rmail_expunge_confirmed, initialize_rmail
    public :: rmail_states, rmail_state_type

    !> The name of the header field that records a message's attributes.
    !> Port of rmail.el rmail-attribute-header.
    character(len=*), parameter, public :: RMAIL_ATTRIBUTE_HEADER = "X-RMAIL-ATTRIBUTES"

    !> The name of the header field that records a message's keywords.
    !> Port of rmail.el rmail-keyword-header.
    character(len=*), parameter, public :: RMAIL_KEYWORD_HEADER = "X-RMAIL-KEYWORDS"

    !> The indices of the attributes in `RMAIL_ATTRIBUTE_HEADER': answered,
    !> deleted, edited, filed, retried, forwarded, unseen and resent (0
    !> for the first character of the value).  Port of rmail.el
    !> rmail-answered-attr-index ... rmail-resent-attr-index.
    integer, parameter, public :: RMAIL_ANSWERED_ATTRIBUTE_INDEX = 0, RMAIL_DELETED_ATTRIBUTE_INDEX = 1, &
                                  RMAIL_EDITED_ATTRIBUTE_INDEX = 2, RMAIL_FILED_ATTRIBUTE_INDEX = 3, &
                                  RMAIL_RETRIED_ATTRIBUTE_INDEX = 4, RMAIL_FORWARDED_ATTRIBUTE_INDEX = 5, &
                                  RMAIL_UNSEEN_ATTRIBUTE_INDEX = 6, RMAIL_RESENT_ATTRIBUTE_INDEX = 7

    !> The characters of the attributes in `RMAIL_ATTRIBUTE_HEADER', in the
    !> order of their indices.  Port of rmail.el rmail-attr-array (its
    !> characters).
    character(len=*), parameter, public :: RMAIL_ATTRIBUTE_CHARACTERS = "ADEFRSUr"

    !> The names of the attributes, in the order of their indices.  Port of
    !> rmail.el rmail-attr-array (its names).
    character(len=9), parameter, public :: RMAIL_ATTRIBUTE_NAMES(8) = &
                                           [character(len=9) :: "answered", "deleted", "edited", "filed", "retried", &
                                                                 "forwarded", "unseen", "resent"]

    !> Values of `rmail_header_style': the header pruned of the ignored
    !> fields, or shown in full.  Port of the values of rmail.el
    !> rmail-header-style.
    integer, parameter, public :: RMAIL_HEADER_STYLE_NORMAL = 1, RMAIL_HEADER_STYLE_FULL = 2

    !> Values of `rmail_mbox_format'.  Port of the values of rmail.el
    !> rmail-mbox-format.
    integer, parameter, public :: RMAIL_MBOX_FORMAT_MBOXRD = 2, RMAIL_MBOX_FORMAT_MBOXO = 3

    !> The header style messages are shown with: RMAIL_HEADER_STYLE_NORMAL
    !> or RMAIL_HEADER_STYLE_FULL.  Port of rmail.el rmail-header-style.
    integer, public :: rmail_header_style = RMAIL_HEADER_STYLE_NORMAL

    !> The label last given to \[add_rmail_label] or \[remove_rmail_label]
    !> (unallocated for none).  Port of rmail.el rmail-last-label.
    character(len=:), allocatable, public :: rmail_last_label

    !> The labels last given to a command that finds messages by labels
    !> (unallocated for none).  Port of rmail.el rmail-last-multi-labels.
    character(len=:), allocatable, public :: rmail_last_multi_labels

    !> Name of user's primary mail file.  Port of rmail.el rmail-file-name.
    character(len=:), allocatable, public :: rmail_file_name

    !> Name of directory used by system mailer for delivering new mail.
    !> Its name should end with a slash.  Port of rmail.el
    !> rmail-spool-directory (set by `initialize_rmail' unless already set).
    character(len=:), allocatable, public :: rmail_spool_directory

    !> If allocated, the file name of the `movemail' program.
    !> If you customize this after starting Rmail, reset the
    !> variable `rmail_movemail_variant_in_use' to "",
    !> and then restart Rmail.  Port of rmail.el rmail-movemail-program
    !> (unallocated: Fortmacs moves the mail itself, see movemail_mod,
    !> unless `rmail_movemail_search_path' or `exec_path' has a movemail
    !> program).
    character(len=:), allocatable, public :: rmail_movemail_program

    !> List of flags to pass to movemail.
    !> Most commonly used to specify `-g' to enable GSS-API authentication
    !> or `-k' to enable Kerberos authentication.  Port of rmail.el
    !> rmail-movemail-flags.
    character(len=:), allocatable, public :: rmail_movemail_flags(:)

    !> True means leave incoming mail in the user's inbox--don't delete it.
    !> Port of rmail.el rmail-preserve-inbox.
    logical, public :: rmail_preserve_inbox = .false.

    !> List of directories to search for movemail (in addition to
    !> `exec_path').  Port of rmail.el rmail-movemail-search-path.
    character(len=:), allocatable, public :: rmail_movemail_search_path(:)

    !> The movemail variant currently in use.  Known variants are:
    !>
    !>   "emacs"     Means any implementation, compatible with the native Emacs one.
    !>               This is the default;
    !>   "mailutils" Means GNU mailutils implementation, capable of handling full
    !> mail URLs as the source mailbox.
    !>
    !> "" until it is found out.  Port of rmail.el
    !> rmail-movemail-variant-in-use.
    character(len=:), allocatable, public :: rmail_movemail_variant_in_use

    !> Regexp matching user mail addresses.
    !> If allocated, this variable is used to identify the correspondent
    !> when receiving new mail.  If it matches the address of the sender,
    !> the recipient is taken as correspondent of a mail.
    !> If unallocated (default value), your `user_login_name' and `user_mail_address'
    !> are used to exclude yourself as correspondent.
    !>
    !> Usually you don't have to set this variable, except if you collect mails
    !> sent by you under different user names.
    !> Then it should be a regexp matching your mail addresses.
    !>
    !> Setting this variable has an effect only before reading a mail.
    !> Port of rmail.el rmail-user-mail-address-regexp.
    character(len=:), allocatable, public :: rmail_user_mail_address_regular_expression

    !> Regexp to match header fields that Rmail should normally hide.
    !> (See also `rmail_nonignored_headers', which overrides this regexp.)
    !> This variable is used for reformatting the message header,
    !> which normally happens once for each message,
    !> when you view the message for the first time in Rmail.
    !> To make a change in this variable take effect
    !> for a message that you have already viewed,
    !> go to that message and type \[toggle_rmail_header] twice.
    !> Port of rmail.el rmail-ignored-headers (set by `initialize_rmail' unless
    !> already set).
    character(len=:), allocatable, public :: rmail_ignored_headers

    !> Regexp to match X header fields that Rmail should show.
    !> This regexp overrides `rmail_ignored_headers'; if both this regexp
    !> and that one match a certain header field, Rmail shows the field.
    !> If this is unallocated, ignore all header fields in `rmail_ignored_headers'.
    !>
    !> This variable is used for reformatting the message header,
    !> which normally happens once for each message,
    !> when you view the message for the first time in Rmail.
    !> To make a change in this variable take effect
    !> for a message that you have already viewed,
    !> go to that message and type \[toggle_rmail_header] twice.
    !> Port of rmail.el rmail-nonignored-headers (set by `initialize_rmail'
    !> unless already set).
    character(len=:), allocatable, public :: rmail_nonignored_headers

    !> Regexp to match Header fields that Rmail should display.
    !> If unallocated, display all header fields except those matched by
    !> `rmail_ignored_headers'.  Port of rmail.el rmail-displayed-headers.
    character(len=:), allocatable, public :: rmail_displayed_headers

    !> Headers that should be stripped when retrying a failed message.
    !> Port of rmail.el rmail-retry-ignored-headers (set by `initialize_rmail'
    !> unless already set).
    character(len=:), allocatable, public :: rmail_retry_ignored_headers

    !> Regexp to match Header fields that Rmail should normally highlight.
    !> An unallocated value means don't highlight.  Uses the face `rmail_highlight'.
    !> Port of rmail.el rmail-highlighted-headers (set by `initialize_rmail'
    !> unless already set).
    character(len=:), allocatable, public :: rmail_highlighted_headers

    !> True means automatically delete a message that is copied to a file.
    !> Port of rmail.el rmail-delete-after-output.
    logical, public :: rmail_delete_after_output = .false.

    !> List of files that are inboxes for your primary mail file `rmail_file_name'.
    !> If this is unallocated, uses the environment variable MAIL.  If that is
    !> unset, uses a file named by the function `user_login_name' in the
    !> directory `rmail_spool_directory' (whose value depends on the
    !> operating system).  For example, "/var/mail/USER".
    !> Port of rmail.el rmail-primary-inbox-list.
    character(len=:), allocatable, public :: rmail_primary_inboxes(:)

    !> True means Rmail makes a new frame for composing outgoing mail.
    !> This is handy if you want to preserve the window configuration of
    !> the frame where you have the RMAIL buffer displayed.  Port of
    !> rmail.el rmail-mail-new-frame.
    logical, public :: rmail_mail_new_frame = .false.

    !> Directory for additional secondary Rmail files.  Port of rmail.el
    !> rmail-secondary-file-directory.
    character(len=:), allocatable, public :: rmail_secondary_file_directory

    !> Regexp for which files are secondary Rmail files.  Port of rmail.el
    !> rmail-secondary-file-regexp.
    character(len=:), allocatable, public :: rmail_secondary_file_regular_expression

    !> Values of `rmail_confirm_expunge' besides .true. (ask with
    !> `read_y_or_n') and .false. (don't ask): ask with `read_yes_or_no',
    !> which wants a full answer.
    integer, parameter, public :: RMAIL_CONFIRM_EXPUNGE_READ_YES_OR_NO = 2

    !> Whether and how to ask for confirmation before expunging deleted messages.
    !> If .true., ask the question with `read_y_or_n'; with
    !> RMAIL_CONFIRM_EXPUNGE_READ_YES_OR_NO, ask it with `read_yes_or_no'.
    !> Port of rmail.el rmail-confirm-expunge (a function of your own is
    !> Deferred).
    type(choice_setting_type), public :: rmail_confirm_expunge = &
                                         choice_setting_type(name="rmail_confirm_expunge", value=.true.)

    !> List of functions to call when Rmail is invoked.  Port of rmail.el
    !> rmail-mode-hook.
    type(hook_type), target, public :: rmail_mode_hook = hook_type(name="rmail_mode_hook")

    !> List of functions to call when Rmail has retrieved new mail.
    !> The functions are called in `rmail_buffer' narrowed to include
    !> only the new email messages, with point at the first new mail.
    !> Port of rmail.el rmail-get-new-mail-hook.
    type(hook_type), target, public :: rmail_get_new_mail_hook = hook_type(name="rmail_get_new_mail_hook")

    !> Run by \[get_rmail_new_mail] before it does anything.  Port of
    !> rmail.el rmail-before-get-new-mail-hook.
    type(hook_type), target, public :: rmail_before_get_new_mail_hook = hook_type(name="rmail_before_get_new_mail_hook")

    !> Run by \[get_rmail_new_mail] just before it returns, whether or not
    !> there is new mail.  Port of rmail.el rmail-after-get-new-mail-hook.
    type(hook_type), target, public :: rmail_after_get_new_mail_hook = hook_type(name="rmail_after_get_new_mail_hook")

    !> List of functions to call when Rmail displays a message.  Port of
    !> rmail.el rmail-show-message-hook.
    type(hook_type), target, public :: rmail_show_message_hook = hook_type(name="rmail_show_message_hook")

    !> List of functions to call when quitting out of Rmail.  Port of
    !> rmail.el rmail-quit-hook.
    type(hook_type), target, public :: rmail_quit_hook = hook_type(name="rmail_quit_hook")

    !> List of functions to call when Rmail deletes a message.
    !> When the hooks are called, the message has been marked deleted but is
    !> still the current message in the Rmail buffer.  Port of rmail.el
    !> rmail-delete-message-hook.
    type(hook_type), target, public :: rmail_delete_message_hook = hook_type(name="rmail_delete_message_hook")

    !> String to prepend to Subject line when replying to a message.  Port
    !> of rmail.el rmail-reply-prefix.
    character(len=:), allocatable, public :: rmail_reply_prefix

    !> True means Rmail always displays the summary buffer.  Port of
    !> rmail.el rmail-display-summary.
    logical, public :: rmail_display_summary = .false.

    !> Default file name for \[output_rmail_message].  Port of rmail.el
    !> rmail-default-file (set by `initialize_rmail' unless already set).
    character(len=:), allocatable, public :: rmail_default_file

    !> Default file name for \[output_rmail_message_body_to_file].  Port of
    !> rmail.el rmail-default-body-file (set by `initialize_rmail' unless
    !> already set).
    character(len=:), allocatable, public :: rmail_default_body_file

    !> Coding system used in RMAIL file.
    !>
    !> This is unallocated by default.  Port of rmail.el
    !> rmail-file-coding-system.
    character(len=:), allocatable, public :: rmail_file_coding_system

    !> True means RMAIL automatically displays decoded MIME messages.
    !> For this to work, MIME support (rmailmm.el, Deferred) must be
    !> available.  Port of rmail.el rmail-enable-mime.
    logical, public :: rmail_enable_mime = .true.

    !> True means use `rmail_insert_mime_forwarded_message_function' to
    !> forward.  Port of rmail.el rmail-enable-mime-composing.
    logical, public :: rmail_enable_mime_composing = .true.

    !> Regexp to match MIME-charset specification in a header of message.
    !> The first parenthesized expression should match the MIME-charset name.
    !> Port of rmail.el rmail-mime-charset-pattern (set by `initialize_rmail'
    !> unless already set).
    character(len=:), allocatable, public :: rmail_mime_character_set_pattern

    !> Regexp matching the delimiter of messages in UNIX mail format
    !> (UNIX From lines), minus the initial ^.  Port of rmail.el
    !> rmail-unix-mail-delimiter (set by `initialize_rmail').
    character(len=:), allocatable, public :: rmail_unix_mail_delimiter

    !> Message size at which to show progress messages for displaying it.
    !> Messages longer than this (in characters) will produce echo-area
    !> messages when Rmail processes such a message for display.  Port of
    !> rmail.el rmail-show-message-verbose-min.
    integer(int64), public :: rmail_show_message_verbose_min = 200000

    !> The mbox format that your system uses.
    !> There is no way to determine this, so you should set the appropriate value.
    !> The formats quote lines containing "From " differently.
    !> The choices are:
    !>   RMAIL_MBOX_FORMAT_MBOXO : lines that start with "From " quoted as ">From "
    !>   RMAIL_MBOX_FORMAT_MBOXRD: lines that start with ">*From " quoted with another ">"
    !> The `mboxo' format is ambiguous, in that one cannot know whether
    !> a line starting with ">From " originally had a ">" or not.
    !>
    !> It is not critical to set this to the correct value; it only affects
    !> how Rmail displays lines starting with ">*From " in non-MIME messages.
    !> Port of rmail.el rmail-mbox-format.
    type(choice_setting_type), public :: rmail_mbox_format = &
                                         choice_setting_type(name="rmail_mbox_format", true_valid=.false., false_valid=.false., &
                                                             choice=RMAIL_MBOX_FORMAT_MBOXRD)

    !> If true, displaying an unseen message marks the Rmail buffer as
    !> modified.  Port of rmail.el rmail-show-message-set-modified.
    logical, public :: rmail_show_message_set_modified = .false.

    !> True means Rmail should show the summary when it changes.
    !> This has an effect only if a summary buffer exists.  Port of
    !> rmail.el rmail-redisplay-summary.
    logical, public :: rmail_redisplay_summary = .false.

    !> A positive value specifies the height for an Rmail summary window
    !> (0: none).  Port of rmail.el rmail-summary-window-size.
    integer, public :: rmail_summary_window_size = 0

    !> Face to use for highlighting the most important header fields.
    !> The variable `rmail_highlighted_headers' specifies which headers.
    type(named_face_type), target, public :: rmail_highlight

    !> Face to use for highlighting the header names.
    !> The variable `rmail_font_lock_keywords' specifies which headers
    !> get highlighted.
    type(named_face_type), target, public :: rmail_header_name

    !> The keymap of Rmail mode.  Port of rmail.el rmail-mode-map.
    type(handle_type), public :: rmail_mode_map

    abstract interface
        !> A summary operation that `select_rmail_summary' runs in the
        !> summary buffer, for message N.  Not a port.
        recursive subroutine rmail_summary_procedure(n, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine rmail_summary_procedure
    end interface

    ! The summary operations rmail_summary_mod provides (upstream's
    ! declare-function of rmailsum.el's functions).

    !> Move to message N in the summary (rmailsum.el's
    !> rmail-summary-goto-msg with NOWARN and SKIP-RMAIL).
    procedure(rmail_summary_procedure), pointer, public :: goto_rmail_summary_message_function => null()

    !> Mark message N deleted in the summary (rmailsum.el's
    !> rmail-summary-mark-deleted).
    procedure(rmail_summary_procedure), pointer, public :: mark_rmail_summary_deleted_function => null()

    !> Mark message N undeleted in the summary (rmailsum.el's
    !> rmail-summary-mark-undeleted).
    procedure(rmail_summary_procedure), pointer, public :: mark_rmail_summary_undeleted_function => null()

    !> Update the summary line of message N (rmailsum.el's
    !> rmail-summary-update-line).
    procedure(rmail_summary_procedure), pointer, public :: update_rmail_summary_line_function => null()

    !> Remake the summary (rmailsum.el's rmail-update-summary).
    procedure(rmail_summary_procedure), pointer, public :: update_rmail_summary_function => null()

    !> Make a summary of all messages (rmailsum.el's rmail-summary).
    procedure(rmail_summary_procedure), pointer, public :: rmail_summary_function => null()

    !> Disable the summary's commands while a message is edited
    !> (rmailsum.el's rmail-summary-disable).
    procedure(rmail_summary_procedure), pointer, public :: disable_rmail_summary_function => null()

    !> Enable the summary's commands again (rmailsum.el's
    !> rmail-summary-enable).
    procedure(rmail_summary_procedure), pointer, public :: enable_rmail_summary_function => null()

    !> The state of an Rmail buffer, in upstream its buffer-local
    !> variables: the viewer buffer holding the other text and whether
    !> the texts are swapped, the message arrays and counters, the
    !> summary buffer and lines, the inboxes, and the local variables of
    !> the viewer buffer (its header style and overlays).  Not a port.
    type :: rmail_state_type
        !> The Rmail buffer's serial number.
        integer(int64) :: serial = 0
        !> The buffer the message collection is swapped with
        !> (`rmail_view_buffer').
        type(handle_type) :: view_buffer
        !> Whether the message collection is in the viewer buffer
        !> (`rmail_buffer_swapped').
        logical :: swapped = .false.
        !> `rmail_was_converted' and `rmail_seriously_modified'.
        logical :: was_converted = .false., seriously_modified = .false.
        !> `rmail_current_message' and `rmail_total_messages'.
        integer(int64) :: current_message = 0, total_messages = 0
        !> Whether the message arrays are set (upstream: rmail-message-vector is set).
        logical :: counted = .false.
        !> Markers at the beginning of each message, then the end of the
        !> last (upstream's rmail-message-vector, from 0: element 0 unused).
        type(handle_type), allocatable :: messages(:)
        !> Whether each message is deleted (upstream's rmail-deleted-vector,
        !> from 0).
        logical, allocatable :: deleted(:)
        !> The summary line of each message, unallocated until made
        !> (upstream's rmail-summary-vector).
        type(string_type), allocatable :: summaries(:)
        !> The summary buffer (`rmail_summary_buffer').
        type(handle_type) :: summary_buffer
        !> `rmail_last_regular_expression'.
        character(len=:), allocatable :: last_regular_expression
        !> The inboxes (`rmail_inboxes'), unallocated for none.
        type(string_type), allocatable :: inboxes(:)
        !> The viewer buffer's own `rmail_header_style' (0: none).
        integer :: view_header_style = 0
        !> The viewer buffer's `rmail_overlay_list'.
        type(handle_type), allocatable :: overlays(:)
    end type rmail_state_type

    !> What `apply_in_rmail_message_begin' saved for `apply_in_rmail_message_end': the
    !> buffer to return to and, when a message was found, the excursion
    !> and restriction of the buffer holding it.  Not a port.
    type, public :: rmail_message_scope_type
        logical :: active = .false.
        type(handle_type) :: saved
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
    end type rmail_message_scope_type

    !> The Rmail buffers' states.
    type(rmail_state_type), allocatable, target :: rmail_states(:)

    ! A buffer's own value of `rmail_buffer' (the Rmail buffer of an Rmail
    ! buffer or of its summary), by serial number.
    type :: rmail_link_type
        integer(int64) :: serial = 0
        type(handle_type) :: rmail_buffer
    end type rmail_link_type

    type(rmail_link_type), allocatable :: links(:)

    ! The default regexp of `search_rmail' (rmail-search-last-regexp).
    character(len=:), allocatable :: search_last_regular_expression

    ! The encoding of the current message while the collection is saved
    ! (rmail-message-encoding).
    character(len=:), allocatable :: message_encoding

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    ! ------------------------------------------------------------------
    ! The Rmail buffer and its state
    ! ------------------------------------------------------------------

    !> The current buffer's `rmail_buffer': the Rmail buffer of an Rmail
    !> buffer or of its summary (the null handle for none).  Not a port:
    !> upstream reads the buffer-local variable.
    function rmail_buffer() result(h)
        type(handle_type) :: h
        integer :: k

        h = null_handle()
        if (current == 0 .or. .not. allocated(links)) return
        k = findloc(links%serial, buffers(current)%serial, dim=1)
        if (k > 0) h = links(k)%rmail_buffer
    end function rmail_buffer

    !> Make BUFFER the current buffer's own `rmail_buffer'.
    !> Not a port.
    subroutine set_rmail_buffer(buffer)
        type(handle_type), intent(in) :: buffer
        type(rmail_link_type) :: entry
        integer :: k

        if (current == 0) return
        if (.not. allocated(links)) allocate (links(0))
        k = findloc(links%serial, buffers(current)%serial, dim=1)

        if (k == 0) then
            do k = 1, size(links)
                if (.not. buffer_serial_live(links(k)%serial)) exit
            end do

            if (k > size(links)) links = [links, entry]
        end if

        links(k)%serial = buffers(current)%serial
        links(k)%rmail_buffer = buffer
    end subroutine set_rmail_buffer

    !> The index in `rmail_states' of the state of the Rmail buffer
    !> BUFFER, made when MAKE is true (0 if there is none).  Not a port.
    integer function rmail_state_index(buffer, make) result(k)
        type(handle_type), intent(in) :: buffer
        logical, intent(in), optional :: make
        type(rmail_state_type) :: entry
        logical :: making

        k = 0
        making = .false.
        if (present(make)) making = make
        if (.not. is_buffer_live(buffer)) return
        if (.not. allocated(rmail_states)) allocate (rmail_states(0))
        k = findloc(rmail_states%serial, buffers(buffer%index)%serial, dim=1)
        if (k > 0 .or. .not. making) return

        do k = 1, size(rmail_states)
            if (.not. buffer_serial_live(rmail_states(k)%serial)) exit
        end do

        if (k > size(rmail_states)) rmail_states = [rmail_states, entry]
        rmail_states(k) = entry
        rmail_states(k)%serial = buffers(buffer%index)%serial
    end function rmail_state_index

    ! Make the current buffer's `rmail_buffer' current; K is the index of
    ! its state.  An error condition occurs when there is none.
    ! Not a port: upstream's (set-buffer rmail-buffer).
    subroutine enter_rmail_buffer(k, status)
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: h

        k = 0
        h = rmail_buffer()

        if (.not. is_buffer_live(h)) then
            call set_error(status, GENERIC_ERROR_CODE, "There is no Rmail buffer")
            return
        end if

        call set_buffer(h, status)
        if (.not. status%ok()) return
        k = rmail_state_index(h)
        if (k == 0) call set_error(status, GENERIC_ERROR_CODE, "There is no Rmail buffer")
    end subroutine enter_rmail_buffer

    ! The state of the current buffer if it is an Rmail buffer (0 if
    ! not).  Not a port.
    integer function current_state() result(k)
        k = 0
        if (current == 0) return
        k = rmail_state_index(current_buffer())
    end function current_state

    !> Return true if the message collection is in `rmail_view_buffer'.
    !> Port of rmail.el:rmail-buffers-swapped-p.
    logical function is_rmail_buffers_swapped() result(swapped)
        integer :: k

        swapped = .false.
        k = current_state()
        if (k > 0) swapped = rmail_states(k)%swapped
    end function is_rmail_buffers_swapped

    !> Return a reusable buffer suitable for viewing messages.
    !> Create the buffer if necessary.  Port of
    !> rmail.el:rmail-generate-viewer-buffer (the viewer buffer's
    !> kill-buffer hook, which refuses to kill it alone, is Deferred).
    subroutine generate_rmail_viewer_buffer(k, view, status)
        integer, intent(in) :: k
        type(handle_type), intent(out) :: view
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        status = status_type()

        ! We want to reuse any existing view buffer, so as not to create
        ! an endless number of them.  But we must avoid clashes if we
        ! visit two different rmail files with the same basename.
        if (is_buffer_live(rmail_states(k)%view_buffer)) then
            view = rmail_states(k)%view_buffer
            return
        end if

        if (allocated(buffers(current)%filename)) then
            name = file_name_nondirectory(buffers(current)%filename)
        else
            name = buffers(current)%name
        end if

        call get_or_make_buffer(generate_new_buffer_name(" *message-viewer "//name//"*"), view, status)
    end subroutine generate_rmail_viewer_buffer

    !> Swap text between current buffer and `rmail_view_buffer'.
    !> This function preserves the current buffer's modified flag, and also
    !> sets the current buffer's `buffer_file_coding_system' to that of
    !> `rmail_view_buffer'.  Port of rmail.el:rmail-swap-buffers.
    subroutine swap_rmail_buffers(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: this, view
        character(len=:), allocatable :: coding_this, coding_that
        logical :: modified_this, modified_that
        integer :: k

        k = current_state()

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Not an Rmail buffer")
            return
        end if

        this = current_buffer()
        view = rmail_states(k)%view_buffer
        call is_buffer_modified(this, modified_this, status)
        if (.not. status%ok()) return
        call is_buffer_modified(view, modified_that, status)
        if (.not. status%ok()) return
        coding_this = ""
        if (allocated(buffer_file_coding_system%value)) coding_this = buffer_file_coding_system%value
        call set_buffer(view, status)
        if (.not. status%ok()) return
        coding_that = ""
        if (allocated(buffer_file_coding_system%value)) coding_that = buffer_file_coding_system%value
        call set_buffer(this, status)
        if (.not. status%ok()) return
        call swap_buffer_text(view, status)
        if (.not. status%ok()) return
        if (len(coding_that) > 0) call buffer_file_coding_system%set_local(coding_that)
        call set_buffer(view, status)
        if (.not. status%ok()) return
        if (len(coding_this) > 0) call buffer_file_coding_system%set_local(coding_this)
        call restore_buffer_modified(modified_that)
        call set_buffer(this, status)
        if (.not. status%ok()) return
        call restore_buffer_modified(modified_this)
    end subroutine swap_rmail_buffers

    ! Bring the actual Rmail messages back into the main buffer.  Port of
    ! rmail.el:rmail-change-major-mode-hook.
    recursive subroutine change_rmail_major_mode(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = current_state()
        if (k == 0) return

        if (rmail_states(k)%swapped) then
            call swap_rmail_buffers(status)
            rmail_states(k)%swapped = .false.
        end if
    end subroutine change_rmail_major_mode

    !> Determine if the Rmail buffer is showing a message.
    !> If so restore the actual mbox message collection.  Port of
    !> rmail.el:rmail-swap-buffers-maybe.
    subroutine swap_rmail_buffers_maybe(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved, h
        integer :: k

        status = status_type()
        h = rmail_buffer()
        if (.not. is_buffer_live(h)) return
        saved = current_buffer()
        call set_buffer(h, status)
        if (.not. status%ok()) return
        k = rmail_state_index(h)

        if (k > 0) then
            if (rmail_states(k)%swapped) then
                call swap_rmail_buffers(status)
                rmail_states(k)%swapped = .false.
            end if
        end if

        if (is_buffer_live(saved)) call set_buffer(saved, status)
    end subroutine swap_rmail_buffers_maybe

    !> Warn if important modifications would change Rmail file's format.
    !> Port of rmail.el:rmail-modify-format.
    recursive subroutine modify_rmail_format(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        character(len=:), allocatable :: name, question
        logical :: proceed
        integer :: k

        status = status_type()
        saved = current_buffer()
        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return

        ! If it's already modified, don't warn again.
        if (rmail_states(k)%was_converted .and. .not. rmail_states(k)%seriously_modified) then
            call buffer_name(current_buffer(), name, status)
            if (.not. status%ok()) return
            question = "After this, "//name//" would be saved in mbox format.  Proceed? "
            call message(question)
            call read_yes_or_no(question, proceed, status)
            if (.not. status%ok()) return

            if (.not. proceed) then
                call set_error(status, GENERIC_ERROR_CODE, "Aborted")
                return
            end if
        end if

        rmail_states(k)%seriously_modified = .true.
        call set_buffer(saved, status)
    end subroutine modify_rmail_format

    !> Mark the buffer unmodified rather than rewrite a converted Babyl
    !> file as mbox.  Port of rmail.el:rmail-dont-modify-format.
    subroutine keep_rmail_format()
        integer :: k

        k = current_state()
        if (k == 0) return

        if (rmail_states(k)%was_converted .and. .not. rmail_states(k)%seriously_modified) then
            call set_buffer_modified(.false.)
            call message("Marking buffer unmodified to avoid rewriting Babyl file as mbox file")
        end if
    end subroutine keep_rmail_format

    ! Turn off the hook on the view buffer, so we can kill it, then kill
    ! it.  Port of rmail.el:rmail-mode-kill-buffer-hook.
    recursive subroutine close_rmail_view_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: view
        integer :: k

        status = status_type()
        k = current_state()
        if (k == 0) return
        view = rmail_states(k)%view_buffer
        if (is_buffer_live(view)) call close_buffer(view, status)
    end subroutine close_rmail_view_buffer

    ! Close the summary buffer too.  Port of
    ! rmail.el:rmail-mode-kill-summary.
    recursive subroutine close_rmail_summary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: summary
        integer :: k

        status = status_type()
        k = current_state()
        if (k == 0) return
        summary = rmail_states(k)%summary_buffer
        if (is_buffer_live(summary)) call close_buffer(summary, status)
    end subroutine close_rmail_summary

    ! ------------------------------------------------------------------
    ! Rmail mode
    ! ------------------------------------------------------------------

    !> Rmail Mode is used by \<rmail_mode_map>\[rmail] for editing Rmail files.
    !> All normal editing commands are turned off.
    !> Instead, these commands are available:
    !>
    !> \[beginning_of_rmail_message]        Move point to front of this message.
    !> \[end_of_rmail_message]        Move point to bottom of this message.
    !> \[scroll_up]        Scroll to next screen of this message.
    !> \[scroll_down]        Scroll to previous screen of this message.
    !> \[next_rmail_undeleted_message]        Move to Next non-deleted message.
    !> \[previous_rmail_undeleted_message]        Move to Previous non-deleted message.
    !> \[next_rmail_message]        Move to Next message whether deleted or not.
    !> \[previous_rmail_message]        Move to Previous message whether deleted or not.
    !> \[show_first_rmail_message]        Move to the first message in Rmail file.
    !> \[show_last_rmail_message]        Move to the last message in Rmail file.
    !> \[show_rmail_message]        Jump to message specified by numeric position in file.
    !> \[search_rmail]        Search for string and show message it is found in.
    !> \[delete_rmail_message_forward]        Delete this message, move to next nondeleted.
    !> \[delete_rmail_message_backward]        Delete this message, move to previous nondeleted.
    !> \[undelete_previous_rmail_message]        Undelete message.  Tries current message, then earlier messages
    !>         till a deleted message is found.
    !> \[edit_rmail_current_message]        Edit the current message.  \[cease_rmail_edit] to return to Rmail.
    !> \[rmail_epa_decrypt]        Decrypt the current message.
    !> \[expunge_rmail]        Expunge deleted messages.
    !> \[expunge_and_save_rmail]        Expunge and save the file.
    !> \[quit_rmail]       Quit Rmail: expunge, save, then switch to another buffer.
    !> \[save_buffer] Save without expunging.
    !> \[get_rmail_new_mail]        Move new mail from system spool directory into this file.
    !> \[rmail_mail]        Mail a message (same as \[mail_other_window]).
    !> \[rmail_continue]        Continue composing outgoing message started before.
    !> \[rmail_reply]        Reply to this message.  Like \[rmail_mail] but initializes some fields.
    !> \[rmail_retry_failure]        Send this message again.  Used on a mailer failure message.
    !> \[rmail_forward]        Forward this message to another user.
    !> \[output_rmail_message]        Output (append) this message to another mail file.
    !> \[output_rmail_message_as_seen]        Output (append) this message to file as it's displayed.
    !> \[output_rmail_message_body_to_file]        Save message body to a file.  Default filename comes from Subject line.
    !> \[rmail_input]        Input Rmail file.  Run Rmail on that file.
    !> \[add_rmail_label]        Add label to message.  It will be displayed in the mode line.
    !> \[remove_rmail_label]        Remove label.  Remove a label from current message.
    !> \[next_rmail_labeled_message]   Move to Next message with specified label
    !>           (label defaults to last one specified).
    !>           Standard labels: filed, unseen, answered, forwarded, deleted.
    !>           Any other label is present only if you add it with \[add_rmail_label].
    !> \[previous_rmail_labeled_message]   Move to Previous message with specified label
    !> \[rmail_summary]        Show headers buffer, with a one line summary of each message.
    !> \[rmail_summary_by_labels]        Summarize only messages with particular label(s).
    !> \[rmail_summary_by_recipients]   Summarize only messages with particular recipient(s).
    !> \[rmail_summary_by_regular_expression]   Summarize only messages with particular regexp(s).
    !> \[rmail_summary_by_topic]   Summarize only messages with subject line regexp(s).
    !> \[toggle_rmail_header]        Toggle display of complete header.
    !> Port of rmail.el:rmail-mode (`rmail_require_mime_maybe' waits for
    !> rmailmm.el, and making the buffer multibyte for set-buffer-multibyte).
    recursive subroutine rmail_mode(status)
        type(status_type), intent(out) :: status
        logical :: finding_rmail_file
        integer :: k

        finding_rmail_file = buffers(current)%major_mode /= "rmail_mode"
        call set_up_rmail_mode_2(status)
        if (.not. status%ok()) return

        if (finding_rmail_file .and. .not. allocated(coding_system_for_read%value)) then
            call convert_rmail_file_maybe(status)
            if (.not. status%ok()) return
            call goto_character(point_maximum())
        end if

        call set_rmail_message_counters()
        k = current_state()
        call show_rmail_message(status, rmail_states(k)%total_messages)
        if (.not. status%ok()) return

        if (finding_rmail_file .and. rmail_display_summary) then
            if (associated(rmail_summary_function)) call rmail_summary_function(0_int64, status)
            if (.not. status%ok()) return
        end if

        call rmail_mode_hook%execute(status)
    end subroutine rmail_mode

    ! Set up Rmail mode in the current buffer, with its local variables.
    ! Port of rmail.el:rmail-mode-2.
    recursive subroutine set_up_rmail_mode_2(status)
        type(status_type), intent(out) :: status

        call clear_all_local_variables()
        call set_up_rmail_mode_1(status)
        if (.not. status%ok()) return
        call set_rmail_permanent_variables(status)
        if (.not. status%ok()) return
        call set_rmail_variables()
        call execute_mode_hooks(status)
    end subroutine set_up_rmail_mode_2

    !> Make the current buffer an Rmail mode buffer: read-only, with the
    !> keymap, syntax table and the hooks that support buffer swapping.
    !> Port of rmail.el:rmail-mode-1 (no auto-save file name: auto-saving
    !> is not ported).
    recursive subroutine set_up_rmail_mode_1(status)
        type(status_type), intent(out) :: status

        buffers(current)%major_mode = "rmail_mode"
        buffers(current)%mode_name = "RMAIL"
        buffers(current)%read_only = .true.
        call use_local_map(rmail_mode_map, status)
        if (.not. status%ok()) return
        buffers(current)%syntax_table = text_mode_syntax_table
        ! Functions to support buffer swapping:
        call write_region_annotate_functions%prepend(annotate_rmail_write_region, local=.true.)
        call close_buffer_hook%prepend(close_rmail_view_buffer, local=.true.)
        call change_major_mode_hook%prepend(change_rmail_major_mode, local=.true.)
    end subroutine set_up_rmail_mode_1

    ! Set up the permanent locals associated with an Rmail file.  Port of
    ! rmail.el:rmail-perm-variables (set-buffer-multibyte is Deferred: the
    ! message collection stays multibyte, its bytes being the same).
    subroutine set_rmail_permanent_variables(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: this, view
        character(len=:), allocatable :: expanded, mail, true_name
        logical :: found, primary
        integer :: k, i

        this = current_buffer()
        k = rmail_state_index(this, make=.true.)
        call set_rmail_buffer(this)
        rmail_states(k)%was_converted = .false.
        rmail_states(k)%seriously_modified = .false.
        call generate_rmail_viewer_buffer(k, view, status)
        if (.not. status%ok()) return
        rmail_states(k)%view_buffer = view
        call set_buffer(view, status)
        if (.not. status%ok()) return
        buffers(current)%undo_enabled = .false.
        buffers(current)%undo_count = 0
        ! Note that this does not erase the buffer.
        if (allocated(rmail_states(k)%overlays)) deallocate (rmail_states(k)%overlays)
        ! Force C-x C-s write Unix EOLs.
        call buffer_file_coding_system%set_local("undecided-unix")
        call set_buffer(this, status)
        if (.not. status%ok()) return
        rmail_states(k)%total_messages = 0

        ! Provide default set of inboxes for primary mail file ~/RMAIL.
        if (.not. allocated(rmail_states(k)%inboxes) .and. allocated(buffers(current)%filename)) then
            call expand_file_name(rmail_file_name, expanded, status)
            if (.not. status%ok()) return
            primary = buffers(current)%filename == expanded

            if (.not. primary) then
                call file_truename(rmail_file_name, true_name, status)
                if (.not. status%ok()) return
                primary = buffers(current)%filename == abbreviate_file_name(true_name)
            end if

            if (primary) then
                if (allocated(rmail_primary_inboxes)) then
                    allocate (rmail_states(k)%inboxes(size(rmail_primary_inboxes)))

                    do i = 1, size(rmail_primary_inboxes)
                        rmail_states(k)%inboxes(i)%text = trim(rmail_primary_inboxes(i))
                    end do
                else
                    allocate (rmail_states(k)%inboxes(1))
                    call getenv("MAIL", mail, found)

                    if (found) then
                        rmail_states(k)%inboxes(1)%text = mail
                    else
                        rmail_states(k)%inboxes(1)%text = rmail_spool_directory//user_login_name()
                    end if
                end if
            end if
        end if
    end subroutine set_rmail_permanent_variables

    !> Set up the non-permanent locals associated with Rmail mode.
    !> Port of rmail.el:rmail-variables (undo and the summary's kill hook; the
    !> other local settings wait for their variables: see
    !> docs/status/rmail_mod.md).
    subroutine set_rmail_variables()
        ! Turn off undo.  We turn it back on in rmail-edit.
        buffers(current)%undo_enabled = .false.
        buffers(current)%undo_count = 0
        call close_buffer_hook%prepend(close_rmail_summary, local=.true.)
    end subroutine set_rmail_variables

    !> Used in `write_region_annotate_functions' to write rmail files: when
    !> the whole buffer is written and the message collection is in the
    !> viewer buffer, write that.  Port of
    !> rmail.el:rmail-write-region-annotate.
    recursive subroutine annotate_rmail_write_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: view_name, name
        integer :: k

        status = status_type()
        if (beginning /= 0 .or. end < 0) return
        k = current_state()
        if (k == 0) return
        if (.not. rmail_states(k)%swapped) return

        if (.not. is_buffer_live(rmail_states(k)%view_buffer)) then
            view_name = "?"
            call buffer_name(current_buffer(), name, status)
            call set_error(status, GENERIC_ERROR_CODE, "Buffer `"//view_name//"' with real text of `"//name// &
                           "' has disappeared")
            return
        end if

        message_encoding = ""
        if (allocated(buffer_file_coding_system%value)) message_encoding = buffer_file_coding_system%value
        call set_buffer(rmail_states(k)%view_buffer, status)
        if (.not. status%ok()) return
        call widen()
    end subroutine annotate_rmail_write_region

    ! Used to restore the encoding of the buffer where we show the
    ! current message, after we save the message collection.  Port of
    ! rmail.el:rmail-after-save-hook.
    recursive subroutine update_rmail_after_save(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved, target
        integer :: k

        status = status_type()
        if (.not. allocated(rmail_states) .or. current == 0) return

        do k = 1, size(rmail_states)
            if (.not. buffer_serial_live(rmail_states(k)%serial)) cycle

            if (buffers(current)%serial == rmail_states(k)%serial .or. &
                is_same_buffer(rmail_states(k)%view_buffer, current_buffer())) then
                if (.not. allocated(message_encoding)) return
                saved = current_buffer()

                if (rmail_states(k)%swapped) then
                    target = buffer_of_serial(rmail_states(k)%serial)
                else
                    target = rmail_states(k)%view_buffer
                end if

                call set_buffer(target, status)
                if (.not. status%ok()) return
                if (len(message_encoding) > 0) call buffer_file_coding_system%set_local(message_encoding)
                call set_buffer(saved, status)
                return
            end if
        end do
    end subroutine update_rmail_after_save

    ! Whether handles A and B name the same live buffer.  Not a port.
    logical function is_same_buffer(a, b)
        type(handle_type), intent(in) :: a, b

        is_same_buffer = .false.
        if (.not. is_buffer_live(a)) return
        is_same_buffer = a%index == b%index .and. a%generation == b%generation
    end function is_same_buffer

    ! The live buffer whose serial number is SERIAL (the null handle if
    ! none).  Not a port.
    function buffer_of_serial(serial) result(h)
        integer(int64), intent(in) :: serial
        type(handle_type) :: h
        integer :: k

        h = null_handle()
        if (.not. allocated(links)) return

        do k = 1, size(links)
            if (links(k)%serial == serial .and. is_buffer_live(links(k)%rmail_buffer)) then
                if (buffers(links(k)%rmail_buffer%index)%serial == serial) then
                    h = links(k)%rmail_buffer
                    return
                end if
            end if
        end do
    end function buffer_of_serial

    ! ------------------------------------------------------------------
    ! Reading the mail file and new mail
    ! ------------------------------------------------------------------

    !> Read and edit incoming mail.
    !> Moves messages into file named by `rmail_file_name' and edits that
    !> file in RMAIL Mode.
    !> Type \[describe_mode] once editing that file, for a list of RMAIL commands.
    !>
    !> May be called with file name as argument; then performs rmail editing on
    !> that file, but does not copy any new mail into the file.
    !> Interactively, if you supply a prefix argument, then you
    !> have a chance to specify a file name with the minibuffer.
    !>
    !> If `rmail_display_summary' is true, make a summary for this RMAIL file.
    !> Port of rmail.el:rmail (`rmail_require_mime_maybe' waits for
    !> rmailmm.el, the I/O menus for the menu bar; local variables in the
    !> file are not read as upstream turns enable-local-variables off,
    !> which Fortmacs does not have yet).
    recursive subroutine rmail(status, file_name_argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_name_argument
        character(len=:), allocatable :: file_name, saved_coding
        type(handle_type) :: existed, buffer, mail_buffer
        type(status_type) :: st
        logical :: run_mail_hook, message_shown, changed, coding_set
        integer :: k

        if (present(file_name_argument)) then
            call expand_file_name(file_name_argument, file_name, status)
        else
            call expand_file_name(rmail_file_name, file_name, status)
        end if

        if (.not. status%ok()) return
        existed = get_file_buffer(file_name)
        changed = .false.

        ! Determine if an existing mail file has been changed behind the
        ! scene...
        if (is_buffer_live(existed)) then
            call set_buffer(existed, status)
            if (.not. status%ok()) return
            changed = .not. is_visited_file_modtime_current()
        end if

        if (changed) then
            ! The mail file has been changed.  Revisit it and reset the
            ! message state variables when in rmail mode.
            call find_file(file_name, status)
            if (.not. status%ok()) return
            call set_buffer(existed, status)
            if (.not. status%ok()) return

            if (is_visited_file_modtime_current() .and. buffers(current)%major_mode == "rmail_mode") then
                call swap_rmail_buffers_maybe(status)
                if (.not. status%ok()) return
                call set_rmail_message_counters()
            end if
        else
            ! The mail file is either unchanged or not visited.  Visit it.
            ! Force no-conversion by default, since that's what pre-mbox
            ! Rmail did with BABYL files.
            coding_set = allocated(coding_system_for_read%value)
            if (coding_set) saved_coding = coding_system_for_read%value
            if (.not. coding_set) coding_system_for_read%value = "no-conversion"
            call find_file_noselect(file_name, buffer, status)
            if (.not. coding_set) deallocate (coding_system_for_read%value)
            if (.not. status%ok()) return
            call buffer_name(buffer, file_name, status)
            if (.not. status%ok()) return
            call switch_to_buffer(file_name, status)
            if (.not. status%ok()) return
        end if

        ! Ensure that the collection and view buffers are in sync and
        ! ensure that a message is not being edited.
        if (buffers(current)%major_mode == "rmail_mode") then
            call swap_rmail_buffers_maybe(status)
            if (.not. status%ok()) return
        end if

        if (buffers(current)%major_mode == "rmail_edit_mode") then
            call set_error(status, GENERIC_ERROR_CODE, "Exit Rmail Edit mode before getting new mail")
            return
        end if

        run_mail_hook = .true.

        if (is_buffer_live(existed)) then
            if (buffer_size() > 0) run_mail_hook = .false.
        end if

        ! Ensure that the Rmail file is in mbox format, the buffer is in
        ! Rmail mode and has been scanned to find all the messages
        ! (setting the global message variables in the process).
        call convert_rmail_file_maybe(status)
        if (.not. status%ok()) return

        if (buffers(current)%major_mode /= "rmail_mode") then
            call set_up_rmail_mode_2(status)
            if (.not. status%ok()) return
        end if

        call goto_character(point_maximum())
        call set_rmail_message_counters_maybe()
        mail_buffer = rmail_buffer()
        message_shown = .false.
        ! Show the first unread message and process summary mode.
        ! Only get new mail when there is not a file name argument.
        if (.not. present(file_name_argument)) call get_rmail_new_mail(status, shown=message_shown)
        call set_buffer(mail_buffer, st)

        if (.not. message_shown) then
            k = current_state()
            call show_rmail_message(st, get_first_rmail_unseen_message())
            if (status%ok()) status = st
        end if

        if (.not. status%ok()) return

        if (rmail_display_summary) then
            if (associated(rmail_summary_function)) call rmail_summary_function(0_int64, status)
            if (.not. status%ok()) return
        end if

        if (run_mail_hook) call rmail_mode_hook%execute(status)
    end subroutine rmail

    !> Run Rmail on file FILENAME.  Port of rmail.el:rmail-input.
    recursive subroutine rmail_input(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call rmail(status, filename)
    end subroutine rmail_input

    !> Determine if the file needs to be converted to mbox format.
    !> Port of rmail.el:rmail-convert-file-maybe (converting Babyl files waits
    !> for unrmail.el: a `void_function' error condition occurs).
    subroutine convert_rmail_file_maybe(status)
        type(status_type), intent(out) :: status
        logical :: saved_fold

        status = status_type()
        call widen()
        call goto_character(point_minimum())
        ! Detect previous Babyl format files.
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        if (looking_at("BABYL OPTIONS:", status) .or. looking_at("Version: 5"//LF, status)) then
            ! The file is Babyl version 5.  Use unrmail to convert it.
            call set_error(status, VOID_FUNCTION_CODE, "unrmail")
        else if (point_minimum() == point_maximum()) then
            call message("Empty Rmail file.")
        else if (.not. looking_at("From ", status)) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid mbox file")
        end if

        case_fold_search%value = saved_fold
    end subroutine convert_rmail_file_maybe

    !> Report that the buffer is not in the mbox file format.
    !> MSGNUM, if present, indicates the malformed message.  Port of
    !> rmail.el:rmail-error-bad-format.
    subroutine set_rmail_bad_format_error(status, msgnum)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: msgnum
        character(len=100) :: text

        if (present(msgnum)) then
            write (text, '("Message ", i0, " is not a valid RFC 822 (or later) message")') msgnum
            call set_error(status, GENERIC_ERROR_CODE, trim(text))
        else
            call set_error(status, GENERIC_ERROR_CODE, "Message is not a valid RFC 822 (or later) message")
        end if
    end subroutine set_rmail_bad_format_error

    !> Return a suitable coding system to use for the current mail message.
    !> The buffer is expected to be narrowed to just the header of the
    !> message.  Port of rmail.el:rmail-get-coding-system (with
    !> `rmail_get_coding_function' unset, which is Deferred).
    function get_rmail_coding_system() result(coding)
        character(len=:), allocatable :: coding
        type(excursion_type) :: saved
        type(status_type) :: st
        integer(int64) :: found
        logical :: saved_fold

        call save_excursion_begin(saved)
        call goto_character(point_minimum())
        coding = "undecided"
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call regular_expression_search_forward(rmail_mime_character_set_pattern, found, st, noerror=SEARCH_NOERROR)
        case_fold_search%value = saved_fold
        if (found > 0 .and. st%ok()) coding = coding_system_from_name(match_text(1))
        call save_excursion_end(saved)
    end function get_rmail_coding_system

    ! The text matched by subexpression N of the last search in the
    ! current buffer ("" if it did not match).  Not a port.
    function match_text(n) result(text)
        integer, intent(in) :: n
        character(len=:), allocatable :: text
        type(status_type) :: st
        integer(int64) :: b, e

        text = ""
        b = match_beginning(n, st)
        e = match_end(n, st)
        if (b > 0 .and. e >= b) call buffer_substring_no_properties(b, e, text, st)
    end function match_text

    !> Move any new mail from this Rmail file's inbox files.
    !> The buffer-local variable `rmail_inboxes' specifies the list
    !> of inbox files.  By default, this is unallocated, except for your primary
    !> Rmail file `rmail_file_name'.  In this case, when you first visit
    !> the Rmail file it is initialized using either
    !> `rmail_primary_inboxes', or the "MAIL" environment variable,
    !> or the function `user_login_name' and the directory
    !> `rmail_spool_directory' (whose value depends on the operating system).
    !>
    !> The command `set_rmail_inboxes' sets `rmail_inboxes' to the
    !> value you specify.
    !>
    !> You can also specify the file to get new mail from just for one
    !> instance of this command.  In this case, the file of new mail is
    !> not changed or deleted.  Noninteractively, you can pass the inbox
    !> file name as an argument.  Interactively, a prefix argument
    !> causes us to read a file name and use that file as the inbox.
    !>
    !> If the variable `rmail_preserve_inbox' is true, new mail will
    !> always be left in inbox files rather than deleted.
    !>
    !> Before doing anything, this runs `rmail_before_get_new_mail_hook'.
    !> Just before returning, it runs `rmail_after_get_new_mail_hook',
    !> whether or not there is new mail.
    !>
    !> If there is new mail, it runs `rmail_get_new_mail_hook', saves
    !> the updated file, and shows the first unseen message (which might
    !> not be a new one).  SHOWN is true if it got any new messages.
    !> Port of rmail.el:rmail-get-new-mail (the spam filter waits for
    !> rmail-spam-filter.el).
    recursive subroutine get_rmail_new_mail(status, file_name, shown)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_name
        logical, intent(out), optional :: shown
        type(string_type), allocatable :: all_files(:), files(:), names(:)
        type(status_type) :: st
        character(len=:), allocatable :: last_name
        logical :: found, found_one, saved_read_only
        integer :: k, next, i

        found = .false.
        if (present(shown)) shown = .false.
        call rmail_before_get_new_mail_hook%execute(status)
        if (.not. status%ok()) return

        ! If the disk file has been changed from under us, revert to it
        ! before we get new mail.
        if (.not. is_visited_file_modtime_current()) then
            call find_file(buffers(current)%filename, status)
            if (.not. status%ok()) return
        end if

        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        call modify_rmail_format(status)
        if (.not. status%ok()) return
        call swap_rmail_buffers_maybe(status)
        if (.not. status%ok()) return
        call set_rmail_message_counters_maybe()
        call widen()
        ! Get rid of all undo records for this buffer.
        buffers(current)%undo_count = 0

        if (present(file_name)) then
            allocate (all_files(1))
            all_files(1)%text = file_name
        else if (allocated(rmail_states(k)%inboxes)) then
            all_files = rmail_states(k)%inboxes
        else
            allocate (all_files(0))
        end if

        next = 1

        ! This loops if any members of the inbox list have the same
        ! basename (see "name conflict" below).
        do while (next <= size(all_files))
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            allocate (files(0), names(0))

            ! Pull files off all-files onto files as long as there is no
            ! name conflict.  A conflict happens when two inbox file names
            ! have the same last component.
            do while (next <= size(all_files))
                last_name = file_name_nondirectory(all_files(next)%text)
                if (any([(names(i)%text == last_name, i=1, size(names))])) exit
                call append_string(files, all_files(next)%text)
                call append_string(names, last_name)
                next = next + 1
            end do

            call goto_character(point_maximum())

            ! Make sure we end with a blank line unless there are no
            ! messages, as required by mbox format.
            if (.not. is_beginning_of_buffer()) then
                do while (.not. looking_back(LF//LF, st, point() - 2))
                    call insert(LF, status)
                    if (.not. status%ok()) exit
                end do
            end if

            if (status%ok()) call get_rmail_new_mail_1(present(file_name), files, found_one, status)
            buffers(current)%read_only = saved_read_only
            deallocate (files, names)
            if (.not. status%ok()) exit
            found = found .or. found_one
        end do

        if (status%ok()) then
            ! Move to the first new message unless we have other unseen
            ! messages before it.
            if (found) call show_rmail_message(status, get_first_rmail_unseen_message())
            if (status%ok()) call rmail_after_get_new_mail_hook%execute(status)
        else
            ! Don't leave the buffer screwed up if we get a disk-full
            ! error.
            call show_rmail_message(st)
        end if

        if (present(shown)) shown = found .and. status%ok()
    end subroutine get_rmail_new_mail

    ! Read the inbox files FILES (not to be deleted when ONE_FILE) and
    ! add their messages; FOUND is true if new messages were found
    ! without error.  Port of rmail.el:rmail-get-new-mail-1.
    recursive subroutine get_rmail_new_mail_1(one_file, files, found, status)
        logical, intent(in) :: one_file
        type(string_type), intent(in) :: files(:)
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        type(restriction_type) :: restriction
        type(string_type), allocatable :: delete_files(:)
        type(status_type) :: st
        character(len=200) :: text
        character(len=:), allocatable :: suffix
        integer(int64) :: new_messages, moved
        logical :: modified
        integer :: i, count

        found = .false.
        new_messages = 0
        call save_excursion_begin(saved)
        call save_restriction_begin(restriction)

        body: block
            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) exit body
            call narrow_to_region(point(), point(), status)
            if (.not. status%ok()) exit body
            ! Set modified now to lock the file, so that we don't encounter
            ! locking problems later in the middle of reading the mail.
            call set_buffer_modified(.true.)
            call insert_rmail_inbox_text(files,.not. one_file, delete_files, status)
            ! If there was no new mail, or we aborted before actually trying
            ! to get any, mark buffer unmodified, unless it was modified
            ! originally.  Otherwise the buffer is correctly marked modified
            ! and the file locked until we save out the new mail.
            if (.not. modified .and. point_minimum() == point_maximum()) call set_buffer_modified(.false.)
            if (.not. status%ok()) exit body
            ! Scan the new text and convert each message to Rmail/mbox format.
            call goto_character(point_minimum())
            moved = skip_characters_forward(" "//LF, st)
            call narrow_to_region(point(), point_maximum(), status)
            if (.not. status%ok()) exit body
            call add_rmail_mbox_headers(new_messages, status)

            if (.not. status%ok()) then
                ! Try to delete the garbage just inserted.
                call delete_region(point_minimum(), point_maximum(), st)

                ! If we could not convert the file's inboxes, rename the files
                ! we tried to read so we won't over and over again.
                if (.not. one_file .and. allocated(delete_files)) then
                    count = 0

                    do i = 1, size(delete_files)
                        do
                            write (text, '("RMAILOSE.", i0)') count
                            if (.not. file_exists(trim(text))) exit
                            count = count + 1
                        end do

                        call rename_file(delete_files(i)%text, trim(text), st)
                    end do
                end if

                exit body
            end if

            ! Determine if there are messages.
            if (new_messages /= 0) then
                ! There are.  Process them.
                call goto_character(point_minimum())
                call count_rmail_new_messages()
                call rmail_get_new_mail_hook%execute(status)
                if (.not. status%ok()) exit body
                call save_buffer(1_int64, .false., status)
                if (.not. status%ok()) exit body
            end if

            ! Delete the old files, now that the Rmail file is saved.
            if (allocated(delete_files)) then
                do i = 1, size(delete_files)
                    ! First, try deleting.
                    call delete_file(delete_files(i)%text, st)
                    ! If we can't delete it, truncate it.
                    if (.not. st%ok()) call write_region(delete_files(i)%text, .false., st, point(), point())
                end do
            end if

            if (new_messages == 0) then
                if (one_file .or. has_inboxes()) call message("(No new mail has arrived)")
            end if

            if (has_rmail_summary()) then
                call select_rmail_summary(update_rmail_summary_function, 0_int64, status)
                if (.not. status%ok()) exit body
            end if

            suffix = "s"
            if (new_messages == 1) suffix = ""
            write (text, '(i0, " new message", a, " read")') new_messages, suffix
            call message(trim(text))
            ! Establish the return value.
            found = new_messages > 0
        end block body

        call save_restriction_end(restriction)
        call save_excursion_end(saved)

    contains

        ! Whether the Rmail buffer has inboxes.
        logical function has_inboxes()
            integer :: k

            has_inboxes = .false.
            k = current_state()
            if (k > 0) has_inboxes = allocated(rmail_states(k)%inboxes)
        end function has_inboxes
    end subroutine get_rmail_new_mail_1

    ! Move the mail of the inbox FILES into the current buffer at its
    ! end, renaming each inbox to .newmail-NAME in the directory of the
    ! Rmail file first when RENAME; DELETE_FILES are the files to delete
    ! once the mail is saved.  Port of rmail.el:rmail-insert-inbox-text
    ! (local inboxes: remote mailboxes (POP and IMAP URLs) and their
    ! passwords are Deferred, and so is converting new mail in Babyl
    ! format).
    recursive subroutine insert_rmail_inbox_text(files, rename, delete_files, status)
        type(string_type), intent(in) :: files(:)
        logical, intent(in) :: rename
        type(string_type), allocatable, intent(out) :: delete_files(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, tofile, expanded, errors, truename, saved_coding
        type(file_status_type) :: st
        type(status_type) :: ignored
        integer(int64) :: inserted
        logical :: found, coding_set
        integer :: i

        allocate (delete_files(0))

        do i = 1, size(files)
            call expand_file_name(files(i)%text, expanded, status)
            if (.not. status%ok()) return
            call file_truename(substitute_in_file_name(expanded), truename, status)
            if (.not. status%ok()) return
            file = truename
            ! Generate name to move to from inbox name, in case of
            ! multiple inboxes that need moving.  Use the directory of
            ! this rmail file because it's a nuisance to use the homedir
            ! if that is on a full disk and this rmail file isn't.
            call expand_file_name(buffers(current)%filename, expanded, status)
            if (.not. status%ok()) return
            call expand_file_name(".newmail-"//file_name_nondirectory(file), tofile, status, &
                                  file_name_directory(expanded))
            if (.not. status%ok()) return

            ! On some systems, /usr/spool/mail/foo is a directory and the
            ! actual inbox is /usr/spool/mail/foo/foo.
            if (is_file_directory(file)) then
                call expand_file_name(user_login_name(), expanded, status, file)
                if (.not. status%ok()) return
                file = expanded
            end if

            call file_attributes(tofile, st, found)

            if (found .and. st%size /= 0) then
                call message("Getting mail from "//tofile//"...")
            else
                call file_attributes(file, st, found)
                if (found .and. st%size /= 0) call message("Getting mail from "//file//"...")
            end if

            ! Set TOFILE if have not already done so, and rename or copy
            ! the file FILE to TOFILE if and as appropriate.
            if (.not. rename) then
                tofile = file
            else if (file_exists(tofile) .or. .not. file_exists(file)) then
                continue
            else
                call move_rmail_mail(file, tofile, errors, status)
                if (.not. status%ok()) return

                if (len(errors) > 0) then
                    call report_movemail_errors(errors)
                end if
            end if

            ! At this point, TOFILE contains the name to read: Either the
            ! alternate name (if we renamed) or the actual inbox (if not
            ! renaming).
            if (file_exists(tofile)) then
                call goto_character(point_maximum())
                coding_set = allocated(coding_system_for_read%value)
                if (coding_set) saved_coding = coding_system_for_read%value
                coding_system_for_read%value = "no-conversion"
                call insert_file_contents(tofile, .false., inserted, status)

                if (coding_set) then
                    coding_system_for_read%value = saved_coding
                else
                    deallocate (coding_system_for_read%value)
                end if

                if (.not. status%ok()) return
                call goto_character(point_maximum())

                ! Make sure the read-in mbox data properly ends with a
                ! blank line unless it is of size 0.
                if (inserted /= 0) then
                    do while (.not. looking_back(LF//LF, ignored, point() - 2))
                        call insert(LF, status)
                        if (.not. status%ok()) return
                    end do
                end if

                if (.not. (rmail_preserve_inbox .and. file == tofile)) then
                    call prepend_string_to(delete_files, tofile)
                end if
            end if

            call message("")
        end do

    contains

        ! Show the errors movemail reported, as upstream reads them from
        ! the program's output.
        subroutine report_movemail_errors(text)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: line
            type(status_type) :: st2
            integer :: k

            line = text

            ! subst-char-in-region ?\n ?\s
            do k = 1, len(line)
                if (line(k:k) == LF) line(k:k) = " "
            end do

            ! Delete trailing spaces and tabs.
            k = verify(line, " "//TAB, back=.true.)
            line = line(1:k)
            if (index(line, "movemail: ") == 1) line = line(11:)

            ! If using Mailutils, remove initial error code abbreviation.
            if (rmail_movemail_variant_in_use == "mailutils") then
                k = scan(line, ":")

                if (k > 1) then
                    if (verify(line(1:1), "ABCDEFGHIJKLMNOPQRSTUVWXYZ") == 0 .and. &
                        verify(line(1:k - 1), "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") == 0) line = line(k + 1:)
                end if
            end if

            call ding(.true., st2)
            call message("movemail: "//line)
            call sit_for(3000)
        end subroutine report_movemail_errors
    end subroutine insert_rmail_inbox_text

    ! Add TEXT at the front of LIST.  Not a port.
    subroutine prepend_string_to(list, text)
        type(string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: text
        type(string_type), allocatable :: grown(:)

        allocate (grown(size(list) + 1))
        grown(1)%text = text
        grown(2:) = list
        call move_alloc(grown, list)
    end subroutine prepend_string_to

    ! Move the mail of FILE to TOFILE with movemail: the program
    ! `rmail_movemail_program' if one was found or set, else movemail_mod;
    ! ERRORS is what it wrote.  Port of the movemail call of
    ! rmail.el:rmail-insert-inbox-text, with rmail.el:rmail-movemail-variant-p.
    subroutine move_rmail_mail(file, tofile, errors, status)
        character(len=*), intent(in) :: file, tofile
        character(len=:), allocatable, intent(out) :: errors
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: arguments(:)
        type(string_type), allocatable :: words(:)
        type(value_type) :: result
        type(handle_type) :: saved, temporary
        type(status_type) :: st
        integer :: i

        errors = ""
        status = status_type()
        call autodetect_rmail_movemail()

        if (.not. allocated(rmail_movemail_program)) then
            call move_mail(file, tofile, rmail_preserve_inbox, errors)
            return
        end if

        allocate (words(0))
        if (rmail_preserve_inbox) call append_string(words, "-p")
        if (rmail_movemail_variant_in_use == "mailutils") call append_string(words, "--emacs")

        if (allocated(rmail_movemail_flags)) then
            do i = 1, size(rmail_movemail_flags)
                call append_string(words, trim(rmail_movemail_flags(i)))
            end do
        end if

        call append_string(words, file, tofile)
        allocate (arguments(size(words)))

        do i = 1, size(words)
            arguments(i) = value_type(text=words(i)%text)
        end do

        saved = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)

        if (status%ok()) then
            call execute_process(rmail_movemail_program, result, status, destination=value_type(.true.), &
                                 arguments=make_list(arguments))
            if (status%ok()) call buffer_substring_no_properties(point_minimum(), point_maximum(), errors, status)
        end if

        call set_buffer(saved, st)
        call close_buffer(temporary, st)
    end subroutine move_rmail_mail

    ! Find the movemail program and its variant, once: a movemail in
    ! `rmail_movemail_search_path' or on the search path for programs,
    ! else the movemail of movemail_mod (variant "emacs").  Port of
    ! rmail.el:rmail-autodetect and rmail.el:rmail-probe.
    subroutine autodetect_rmail_movemail()
        use files_mod, only: executable_find
        use file_input_output_mod, only: is_file_executable
        character(len=:), allocatable :: program
        integer :: i

        if (allocated(rmail_movemail_variant_in_use)) then
            if (len(rmail_movemail_variant_in_use) > 0) return
        end if

        rmail_movemail_variant_in_use = "emacs"

        if (allocated(rmail_movemail_program)) then
            call probe(rmail_movemail_program)
            return
        end if

        if (allocated(rmail_movemail_search_path)) then
            do i = 1, size(rmail_movemail_search_path)
                program = trim(rmail_movemail_search_path(i))
                if (len(program) == 0) cycle
                if (.not. is_file_directory(program)) cycle
                if (program(len(program):len(program)) /= "/") program = program//"/"
                program = program//"movemail"

                if (.not. is_file_directory(program) .and. is_file_executable(program)) then
                    rmail_movemail_program = program
                    call probe(program)
                    return
                end if
            end do
        end if

        call executable_find("movemail", program)

        if (len(program) > 0) then
            rmail_movemail_program = program
            call probe(program)
        end if

    contains

        ! Determine what flavor of movemail PROGRAM is, from its
        ! --version output.
        subroutine probe(programming)
            character(len=*), intent(in) :: programming
            character(len=:), allocatable :: output
            type(value_type) :: result
            type(handle_type) :: saved, temporary
            type(status_type) :: st

            saved = current_buffer()
            call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
            if (.not. st%ok()) return
            call set_buffer(temporary, st)
            output = ""

            if (st%ok()) then
                call execute_process(programming, result, st, destination=value_type(.true.), &
                                     arguments=list(value_type(text="--version")))
                call buffer_substring_no_properties(point_minimum(), point_maximum(), output, st)
            end if

            call set_buffer(saved, st)
            call close_buffer(temporary, st)
            if (index(output, "movemail (GNU Mailutils") == 1) rmail_movemail_variant_in_use = "mailutils"
        end subroutine probe
    end subroutine autodetect_rmail_movemail

    !> Decode the region FROM TO by CODING into DESTINATION at its point
    !> (a buffer; the current one if absent).  If CODING is "" or an
    !> invalid coding system, decode by `undecided'.  Port of
    !> rmail.el:rmail-decode-region.
    subroutine decode_rmail_region(from, to, coding, status, destination)
        integer(int64), intent(in) :: from, to
        character(len=*), intent(in) :: coding
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: destination
        character(len=:), allocatable :: cs, text, decoded, used
        type(handle_type) :: saved

        cs = coding
        if (len(cs) == 0) cs = "undecided"
        if (.not. is_coding_system(cs)) cs = "undecided"
        call buffer_substring_no_properties(from, to, text, status)
        if (.not. status%ok()) return
        ! Use -dos decoding, to remove ^M characters left from base64 or
        ! rogue qp-encoded text.
        call decode_coding_string(text, coding_system_change_end_of_line_conversion(cs, END_OF_LINE_DOS), decoded, status, &
                                  multibyte=buffers(current)%multibyte)
        if (.not. status%ok()) return
        used = last_coding_system_used%value
        saved = current_buffer()

        if (present(destination)) then
            call set_buffer(destination, status)
            if (.not. status%ok()) return
            call insert(decoded, status)
        else
            call delete_region(from, to, status)
            if (.not. status%ok()) return
            call goto_character(from)
            call insert(decoded, status)
        end if

        if (.not. status%ok()) return
        ! Don't reveal the fact we used -dos decoding, as users generally
        ! will not expect the RMAIL buffer to use DOS END_OF_LINE format.
        last_coding_system_used%value = coding_system_change_end_of_line_conversion(used, END_OF_LINE_UNIX)
        call buffer_file_coding_system%set_local(last_coding_system_used%value)
        call set_buffer(saved, status)
    end subroutine decode_rmail_region

    !> Ensure a message ends in a blank line.
    !> Call with point at the end of the message.  Port of
    !> rmail.el:rmail-ensure-blank-line.
    subroutine ensure_rmail_blank_line(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()
        if (.not. is_beginning_of_line()) call insert(LF, status)
        if (.not. status%ok()) return
        if (.not. looking_back(LF//LF, st, point() - 2)) call insert(LF, status)
    end subroutine ensure_rmail_blank_line

    ! Validate the RFC 822 (or later) format for the new messages.  Point
    ! should be at the first new message.  An error condition occurs if
    ! the new messages are not RFC 822 (or later) compliant.  Unless an
    ! Rmail attribute header already exists, add it to the new messages.
    ! COUNT is the number of new messages.  Port of
    ! rmail.el:rmail-add-mbox-headers.
    subroutine add_rmail_mbox_headers(count, status)
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        type(restriction_type) :: restriction
        character(len=:), allocatable :: value
        integer(int64) :: start, found
        logical :: saved_fold, stop

        count = 0
        status = status_type()
        call save_excursion_begin(saved)
        call save_restriction_begin(restriction)
        start = point()
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        ! Detect an empty inbox file.
        if (start /= point_maximum()) then
            ! Scan the new messages to establish a count and to ensure
            ! that an attribute header is present.
            if (looking_at(rmail_unix_mail_delimiter, status)) then
                stop = .false.

                do while (.not. stop)
                    ! Determine if a new attribute header needs to be added
                    ! to the message.
                    call search_forward(LF//LF, found, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok()) exit

                    if (found > 0) then
                        count = count + 1
                        call narrow_to_region(start, point(), status)
                        if (.not. status%ok()) exit
                        case_fold_search%value = saved_fold
                        call get_mail_field(RMAIL_ATTRIBUTE_HEADER, value, status)
                        case_fold_search%value = .false.
                        if (.not. status%ok()) exit

                        if (.not. allocated(value)) then
                            call goto_character(point() - 1)
                            call insert(RMAIL_ATTRIBUTE_HEADER//": ------U-"//LF, status)
                            if (.not. status%ok()) exit
                        end if

                        call widen()
                    else
                        call set_rmail_bad_format_error(status)
                        exit
                    end if

                    ! Move to the next message.
                    call regular_expression_search_forward(LF//LF//rmail_unix_mail_delimiter, found, status, noerror=SEARCH_MOVE)
                    if (.not. status%ok()) exit

                    if (found == 0) then
                        stop = .true.
                    else
                        call goto_character(match_beginning(0, status))
                        call goto_character(point() + 2)
                    end if

                    start = point()
                end do
            else
                call set_rmail_bad_format_error(status)
            end if
        end if

        case_fold_search%value = saved_fold
        call save_restriction_end(restriction)
        call save_excursion_end(saved)
    end subroutine add_rmail_mbox_headers

    ! ------------------------------------------------------------------
    ! Header fields, attributes and keywords
    ! ------------------------------------------------------------------

    !> Return the value of message header NAME, unallocated if it has
    !> none.  MSGNUM specifies the message number to get it from.
    !> If MSGNUM is absent, use the current message.  Port of
    !> rmail.el:rmail-get-header (with rmail.el:rmail-get-header-1).
    subroutine get_rmail_header(name, value, status, msgnum)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: msgnum
        type(rmail_message_scope_type) :: scope
        integer(int64) :: found

        call apply_in_rmail_message_begin(scope, status, msgnum)
        if (.not. status%ok() .or. .not. scope%active) return
        call search_forward(LF//LF, found, status, noerror=SEARCH_NOERROR)

        if (status%ok()) then
            if (found > 0) then
                call narrow_to_region(point_minimum(), point(), status)
                if (status%ok()) call get_mail_field(name, value, status)
            else
                call set_rmail_bad_format_error(status)
            end if
        end if

        call apply_in_rmail_message_end(scope)
    end subroutine get_rmail_header

    !> Set message header NAME to VALUE in message number MSGNUM.
    !> If MSGNUM is absent, use the current message.  VALUE absent means
    !> to remove the header.  Only changes the first instance of NAME.
    !> If VALUE is multi-line, continuation lines should already be
    !> indented.  VALUE should not end in a newline.  Port of
    !> rmail.el:rmail-set-header (with rmail.el:rmail-set-header-1).
    recursive subroutine set_rmail_header(name, status, msgnum, value)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: msgnum
        character(len=*), intent(in), optional :: value
        type(rmail_message_scope_type) :: scope
        type(handle_type) :: saved
        integer(int64) :: found, start, end
        logical :: saved_fold, saved_inhibit

        call apply_in_rmail_message_begin(scope, status, msgnum)
        if (.not. status%ok()) return

        if (scope%active) then
            saved_inhibit = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call search_forward(LF//LF, found, status, noerror=SEARCH_NOERROR)

            if (status%ok() .and. found == 0) then
                call set_rmail_bad_format_error(status)
            else if (status%ok()) then
                call goto_character(point() - 1)
                call narrow_to_region(point_minimum(), point(), status)
                call goto_character(point_minimum())
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                call regular_expression_search_forward("^"//regular_expression_quote(name)//"[ "//TAB//"]*:", found, &
                                                       status, noerror=SEARCH_MOVE)
                case_fold_search%value = saved_fold

                if (status%ok() .and. found > 0) then
                    start = point()

                    do
                        if (forward_line(1_int64) /= 0) exit
                        if (.not. looking_at("[ "//TAB//"]", status)) exit
                    end do

                    ! Back up over newline.
                    call goto_character(point() - 1)
                    end = point()
                    call goto_character(start)

                    if (present(value)) then
                        call delete_region(start, end, status)
                        if (status%ok()) call insert(" "//value, status)
                    else
                        call delete_region(line_beginning_position(1_int64), end + 1, status)
                    end if
                else if (status%ok()) then
                    ! Not already present: insert at end of headers.
                    if (present(value)) call insert(name//": "//value//LF, status)
                end if
            end if

            buffers(current)%read_only = saved_inhibit
        end if

        call apply_in_rmail_message_end(scope)
        if (.not. status%ok()) return
        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        ! Ensure header changes get saved.
        call set_buffer_modified(.true.)
        ! However: don't save in mbox format over a Babyl file merely
        ! because of this.
        call keep_rmail_format()
        call set_buffer(saved, status)
    end subroutine set_rmail_header

    !> Return the message attributes in a comma separated string
    !> (unallocated for none).  MESSAGE specifies the message number to get it
    !> from.  If MESSAGE is absent, use the current message.  Port of
    !> rmail.el:rmail-get-attr-names.
    subroutine get_rmail_attribute_names(names, status, message_number)
        character(len=:), allocatable, intent(out) :: names
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: message_number
        character(len=:), allocatable :: value
        integer :: i

        call get_rmail_header(RMAIL_ATTRIBUTE_HEADER, value, status, message_number)
        if (.not. status%ok() .or. .not. allocated(value)) return

        if (len(value) > len(RMAIL_ATTRIBUTE_CHARACTERS)) then
            call message("Warning: corrupt attribute header in message")
            return
        end if

        do i = 1, len(value)
            if (value(i:i) == "-") cycle

            if (allocated(names)) then
                names = names//", "//trim(RMAIL_ATTRIBUTE_NAMES(i))
            else
                names = trim(RMAIL_ATTRIBUTE_NAMES(i))
            end if
        end do
    end subroutine get_rmail_attribute_names

    !> Return the message keywords in a comma separated string
    !> (unallocated for none).  MESSAGE, if present, identifies the message
    !> number to use.  If absent, that means the current message.  Port of
    !> rmail.el:rmail-get-keywords.
    subroutine get_rmail_keywords(keywords, status, message)
        character(len=:), allocatable, intent(out) :: keywords
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: message

        call get_rmail_header(RMAIL_KEYWORD_HEADER, keywords, status, message)
    end subroutine get_rmail_keywords

    !> Return a string with the labels (attributes and keywords) of msg MESSAGE.
    !> It is put in comma-separated form.
    !> MESSAGE, if present, identifies the message number to use.
    !> If absent, that means the current message.  Port of
    !> rmail.el:rmail-get-labels.
    subroutine get_rmail_labels(labels, status, message)
        character(len=:), allocatable, intent(out) :: labels
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: message
        character(len=:), allocatable :: attribute_names, keywords
        integer(int64) :: n
        integer :: k

        labels = ""
        k = rmail_state_index(rmail_buffer())

        if (present(message)) then
            n = message
        else
            n = 0
            if (k > 0) n = rmail_states(k)%current_message
        end if

        ! Combine the message attributes and keywords into a
        ! comma-separated list.
        call get_rmail_attribute_names(attribute_names, status, n)
        if (.not. status%ok()) return
        call get_rmail_keywords(keywords, status, n)
        if (.not. status%ok()) return

        if (allocated(keywords)) then
            if (len(keywords) == 0) deallocate (keywords)
        end if

        if (allocated(attribute_names) .and. allocated(keywords)) then
            labels = " "//attribute_names//"; "//keywords
        else if (allocated(attribute_names)) then
            labels = " "//attribute_names
        else if (allocated(keywords)) then
            labels = " "//keywords
        end if
    end subroutine get_rmail_labels

    !> Update the current messages's attributes and keywords in mode line.
    !> Port of rmail.el:rmail-display-labels.
    subroutine display_rmail_labels(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: blurb
        character(len=40) :: counts
        type(mode_line_part_type) :: parts(1)
        integer :: k

        call get_rmail_labels(blurb, status)
        if (.not. status%ok()) return
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        write (counts, '(" ", i0, "/", i0)') rmail_states(k)%current_message, rmail_states(k)%total_messages
        parts(1)%text = trim(counts)//blurb
        parts(1)%face = ""
        call set_mode_line_process(parts)
    end subroutine display_rmail_labels

    ! The character value for attribute ATTRIBUTE in STATE (.true.: on).
    ! Port of rmail.el:rmail-get-attr-value.
    character function get_rmail_attribute_value(attribute, state) result(c)
        integer, intent(in) :: attribute
        logical, intent(in) :: state

        c = "-"
        if (state) c = RMAIL_ATTRIBUTE_CHARACTERS(attribute + 1:attribute + 1)
    end function get_rmail_attribute_value

    ! Set Rmail attribute ATTRIBUTE to STATE in `RMAIL_ATTRIBUTE_HEADER',
    ! creating the header if necessary; ALTERED is true if a significant
    ! attribute change was made.  Port of rmail.el:rmail-set-attribute-1.
    subroutine set_rmail_attribute_1(attribute, state, altered, status)
        integer, intent(in) :: attribute
        logical, intent(in) :: state
        logical, intent(out) :: altered
        type(status_type), intent(out) :: status
        character(len=8) :: header_value
        character :: value
        integer(int64) :: limit, found, missing
        logical :: saved_inhibit, exists
        integer(int32) :: c

        altered = .false.
        call search_forward(LF//LF, limit, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return
        value = get_rmail_attribute_value(attribute, state)
        saved_inhibit = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call goto_character(point_minimum())

        if (limit > 0) then
            call search_forward(RMAIL_ATTRIBUTE_HEADER//": ", found, status, bound=limit, noerror=SEARCH_NOERROR)
        else
            call search_forward(RMAIL_ATTRIBUTE_HEADER//": ", found, status, noerror=SEARCH_NOERROR)
        end if

        if (status%ok() .and. found > 0) then
            ! If this message already records attributes, just change the
            ! value for this one.
            missing = point() + attribute - line_end_position(1_int64)

            ! Position point at this attribute, adding attributes if
            ! necessary.
            if (missing > 0) then
                call end_of_line(1_int64)
                call insert(repeat("-", int(missing)), status)
                call goto_character(point() - 1)
            else
                call goto_character(point() + attribute)
            end if

            ! Change this attribute.
            call character_after(c, exists)

            if (status%ok() .and. c /= iachar(value)) then
                altered = .true.
                call delete_region(point(), point() + 1, status)
                if (status%ok()) call insert(value, status)
            end if
        else if (status%ok()) then
            ! Otherwise add a header line to record the attributes and set
            ! all but this one to no.
            header_value = "--------"
            header_value(attribute + 1:attribute + 1) = value

            if (limit > 0) then
                call goto_character(limit - 1)
            else
                call goto_character(point_maximum())
            end if

            altered = value /= "-"
            call insert(RMAIL_ATTRIBUTE_HEADER//": "//header_value//LF, status)
        end if

        buffers(current)%read_only = saved_inhibit
    end subroutine set_rmail_attribute_1

    !> Turn an attribute of a message on or off according to STATE.
    !> ATTRIBUTE is the index number of the attribute (RMAIL_ANSWERED_ATTRIBUTE_INDEX ...)
    !> or NAME its name, from `RMAIL_ATTRIBUTE_NAMES'.  MSGNUM is message
    !> number to change; absent means current message.  Port of
    !> rmail.el:rmail-set-attribute.
    recursive subroutine set_rmail_attribute(attribute, state, status, msgnum, name)
        integer, intent(in) :: attribute
        logical, intent(in) :: state
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: msgnum
        character(len=*), intent(in), optional :: name
        type(rmail_message_scope_type) :: scope
        type(handle_type) :: saved
        integer(int64) :: n
        logical :: altered
        integer :: index, k

        index = attribute

        if (present(name)) then
            index = findloc(RMAIL_ATTRIBUTE_NAMES == name, .true., dim=1) - 1

            if (index < 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Unknown attribute `"//name//"'")
                return
            end if
        end if

        ! Ask for confirmation before setting any attribute except
        ! `unseen' if it would force a format change.
        if (index /= RMAIL_UNSEEN_ATTRIBUTE_INDEX) then
            call modify_rmail_format(status)
            if (.not. status%ok()) return
        end if

        saved = current_buffer()
        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        n = rmail_states(k)%current_message
        if (present(msgnum)) n = msgnum

        if (n > 0) then
            ! The "deleted" attribute is also stored in a special array
            ! so update that too.
            if (index == RMAIL_DELETED_ATTRIBUTE_INDEX) call set_rmail_message_deleted(n, state)
            call apply_in_rmail_message_begin(scope, status, n)
            if (.not. status%ok()) return
            altered = .false.
            if (scope%active) call set_rmail_attribute_1(index, state, altered, status)
            call apply_in_rmail_message_end(scope)
            if (.not. status%ok()) return
            if (n == rmail_states(k)%current_message) call display_rmail_labels(status)
            if (.not. status%ok()) return

            if (altered) then
                ! Don't save in mbox format over a Babyl file merely
                ! because of a change in `unseen' attribute.
                if (index == RMAIL_UNSEEN_ATTRIBUTE_INDEX) then
                    call keep_rmail_format()
                else
                    ! Otherwise, if we modified the file text via the view
                    ! buffer, mark the main buffer modified too.
                    call set_buffer_modified(.true.)
                end if
            end if
        end if

        if (is_buffer_live(saved)) call set_buffer(saved, status)
    end subroutine set_rmail_attribute

    !> Return true if message number MESSAGE has attributes matching regexp
    !> ATTRIBUTES.  Port of rmail.el:rmail-message-attr-p.
    logical function has_rmail_message_attributes(message, attributes) result(found)
        integer(int64), intent(in) :: message
        character(len=*), intent(in) :: attributes
        character(len=:), allocatable :: value
        type(status_type) :: st

        found = .false.
        call get_rmail_header(RMAIL_ATTRIBUTE_HEADER, value, st, message)
        if (.not. st%ok() .or. .not. allocated(value)) return
        found = string_match(attributes, value, st) > 0
    end function has_rmail_message_attributes

    !> Return true if message number MSGNUM has the unseen attribute.
    !> Port of rmail.el:rmail-message-unseen-p.
    logical function is_rmail_message_unseen(msgnum)
        integer(int64), intent(in) :: msgnum

        is_rmail_message_unseen = has_rmail_message_attributes(msgnum, "......U")
    end function is_rmail_message_unseen

    !> Return true if message number MESSAGE has labels matching regexp
    !> LABELS.  Port of rmail.el:rmail-message-labels-p.
    logical function has_rmail_message_labels(message, labels) result(found)
        integer(int64), intent(in) :: message
        character(len=*), intent(in) :: labels
        character(len=:), allocatable :: text
        type(status_type) :: st

        call get_rmail_labels(text, st, message)
        found = string_match(labels, text, st) > 0
    end function has_rmail_message_labels

    ! ------------------------------------------------------------------
    ! Message selection and support
    ! ------------------------------------------------------------------

    !> Return the end position for message number N.  Port of
    !> rmail.el:rmail-msgend.
    integer(int64) function rmail_message_end(n) result(position)
        integer(int64), intent(in) :: n
        type(status_type) :: st
        integer :: k

        position = 0
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        call marker_position(rmail_states(k)%messages(n + 1), position, st)
    end function rmail_message_end

    !> Return the start position for message number N.  Port of
    !> rmail.el:rmail-msgbeg.
    integer(int64) function rmail_message_beginning(n) result(position)
        integer(int64), intent(in) :: n
        type(status_type) :: st
        integer :: k

        position = 0
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        call marker_position(rmail_states(k)%messages(n), position, st)
    end function rmail_message_beginning

    !> Narrow to message MSGNUM (the current message if absent) of the
    !> current buffer's Rmail buffer, in the buffer holding the message
    !> collection, with point at the start of the message, until
    !> `apply_in_rmail_message_end'; SCOPE is what to restore (inactive when
    !> there is no such message).  Port of rmail.el:rmail-apply-in-message
    !> (two steps, before and after the code upstream calls).
    subroutine apply_in_rmail_message_begin(scope, status, msgnum)
        type(rmail_message_scope_type), intent(out) :: scope
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: msgnum
        integer(int64) :: n, beginning, end
        integer :: k

        status = status_type()
        scope%saved = current_buffer()
        call enter_rmail_buffer(k, status)

        if (.not. status%ok()) then
            call set_buffer(scope%saved, status)
            return
        end if

        n = rmail_states(k)%current_message
        if (present(msgnum)) n = msgnum

        if (n <= 0 .or. .not. rmail_states(k)%counted) then
            call set_buffer(scope%saved, status)
            return
        end if

        beginning = rmail_message_beginning(n)
        end = rmail_message_end(n)
        ! It is ok to go to a different buffer.
        if (rmail_states(k)%swapped) call set_buffer(rmail_states(k)%view_buffer, status)
        if (.not. status%ok()) return
        scope%active = .true.
        call save_excursion_begin(scope%excursion)
        call save_restriction_begin(scope%restriction)
        call widen()
        call goto_character(beginning)
        call narrow_to_region(beginning, end, status)
    end subroutine apply_in_rmail_message_begin

    !> End what `apply_in_rmail_message_begin' began.  Port of
    !> rmail.el:rmail-apply-in-message (its end).
    subroutine apply_in_rmail_message_end(scope)
        type(rmail_message_scope_type), intent(inout) :: scope
        type(status_type) :: st

        if (scope%active) then
            call save_restriction_end(scope%restriction)
            call save_excursion_end(scope%excursion)
            scope%active = .false.
        end if

        if (is_buffer_live(scope%saved)) call set_buffer(scope%saved, st)
    end subroutine apply_in_rmail_message_end

    ! ------------------------------------------------------------------
    ! The message arrays and counters
    ! ------------------------------------------------------------------

    ! Forget the message arrays of state K, detaching their markers.
    ! Port of rmail.el:rmail-forget-messages.
    subroutine forget_rmail_messages(k)
        integer, intent(in) :: k
        type(status_type) :: st
        integer :: i

        if (allocated(rmail_states(k)%messages)) then
            do i = lbound(rmail_states(k)%messages, 1), ubound(rmail_states(k)%messages, 1)
                if (rmail_states(k)%messages(i)%index /= 0) call destroy_marker(rmail_states(k)%messages(i), st)
            end do

            deallocate (rmail_states(k)%messages)
        end if

        if (allocated(rmail_states(k)%deleted)) deallocate (rmail_states(k)%deleted)
        if (allocated(rmail_states(k)%summaries)) deallocate (rmail_states(k)%summaries)
        rmail_states(k)%counted = .false.
    end subroutine forget_rmail_messages

    !> Count the messages unless that is done.  Port of
    !> rmail.el:rmail-maybe-set-message-counters.
    subroutine set_rmail_message_counters_maybe()
        integer :: k

        k = current_state()
        if (k == 0) return
        if (.not. rmail_states(k)%counted) call set_rmail_message_counters()
    end subroutine set_rmail_message_counters_maybe

    ! The start positions of the messages from point back to the
    ! beginning of the accessible text, in buffer order (STARTS), whether
    ! each is deleted as rmail-collect-deleted sees it (DELETED), and
    ! AFTER, the number of messages after the one containing
    ! SPOT_TO_FIND (-1 for none).  Counting starts at TOTAL, which ends as
    ! the number of messages more.  Port of
    ! rmail.el:rmail-set-message-counters-counter (with
    ! rmail.el:rmail-collect-deleted).
    subroutine set_rmail_message_counters_counter(spot_to_find, starts, deleted, total, after)
        integer(int64), intent(in) :: spot_to_find
        integer(int64), allocatable, intent(out) :: starts(:)
        logical, allocatable, intent(out) :: deleted(:)
        integer(int64), intent(inout) :: total
        integer(int64), intent(out) :: after
        integer(int64), allocatable :: positions(:)
        logical, allocatable :: flags(:)
        type(status_type) :: st
        character(len=60) :: text
        integer(int64) :: start, found, n
        integer :: i

        start = point()
        after = -1
        n = 0
        allocate (positions(16), flags(16))

        do
            call search_backward(LF//LF//"From ", found, st, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. st%ok()) exit
            call goto_character(point() + 2)

            if (looking_at(rmail_unix_mail_delimiter, st)) then
                call collect(start)
                start = point()

                ! Show progress after every 20 messages or so.
                if (mod(total, 20_int64) == 0) then
                    write (text, '("Counting messages...", i0)') total
                    call message(trim(text))
                end if
            end if
        end do

        ! Handle the first message, maybe.
        call goto_character(point_minimum())
        if (looking_at(rmail_unix_mail_delimiter, st)) call collect(start)
        allocate (starts(n), deleted(n))

        do i = 1, int(n)
            starts(i) = positions(n - i + 1)
            deleted(i) = flags(n - i + 1)
        end do

    contains

        ! Record the message starting at point, which ends at MESSAGE_END.
        subroutine collect(message_end)
            integer(int64), intent(in) :: message_end
            integer(int64), allocatable :: grown(:)
            logical, allocatable :: grown_flags(:)
            type(excursion_type) :: saved
            integer(int64) :: at

            if (point() <= spot_to_find .and. after < 0) after = total

            if (n == size(positions)) then
                allocate (grown(2*n), grown_flags(2*n))
                grown(1:n) = positions
                grown_flags(1:n) = flags
                call move_alloc(grown, positions)
                call move_alloc(grown_flags, flags)
            end if

            n = n + 1
            positions(n) = point()
            ! Collect the message deletion flags for each message.
            call save_excursion_begin(saved)
            call search_forward(RMAIL_ATTRIBUTE_HEADER//": ", at, st, bound=message_end, noerror=SEARCH_NOERROR)
            flags(n) = .false.
            if (at > 0) flags(n) = looking_at("\?D", st)
            call save_excursion_end(saved)
            total = total + 1
        end subroutine collect
    end subroutine set_rmail_message_counters_counter

    !> Count the number of new messages.
    !> The buffer should be narrowed to include only the new messages.
    !> Output a helpful message unless NOMSG is true.  Port of
    !> rmail.el:rmail-count-new-messages.
    subroutine count_rmail_new_messages(nomsg)
        logical, intent(in), optional :: nomsg
        integer(int64), allocatable :: starts(:)
        logical, allocatable :: deleted(:), deleted_array(:)
        type(handle_type), allocatable :: messages(:)
        type(string_type), allocatable :: summaries(:)
        type(handle_type) :: end_marker
        type(status_type) :: st
        character(len=60) :: text
        integer(int64) :: total, after, old_total, i
        logical :: quiet, saved_fold
        integer :: k

        k = current_state()
        if (k == 0) return
        quiet = .false.
        if (present(nomsg)) quiet = nomsg
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        total = 0
        if (.not. quiet) call message("Counting new messages...")
        call goto_character(point_maximum())
        ! The entry for message N+1 marks the end of message N.
        call copy_marker_at(point(), end_marker, .false., st)
        call set_rmail_message_counters_counter(point_minimum(), starts, deleted, total, after)
        old_total = rmail_states(k)%total_messages
        rmail_states(k)%current_message = old_total + 1
        rmail_states(k)%total_messages = old_total + total
        allocate (messages(0:rmail_states(k)%total_messages + 1))
        allocate (deleted_array(0:rmail_states(k)%total_messages))
        allocate (summaries(rmail_states(k)%total_messages))
        deleted_array = .false.

        if (allocated(rmail_states(k)%messages)) then
            messages(0:old_total) = rmail_states(k)%messages(0:old_total)

            ! The old end marker is replaced by the first new message.
            if (rmail_states(k)%messages(old_total + 1)%index /= 0) then
                call destroy_marker(rmail_states(k)%messages(old_total + 1), st)
            end if

            deleted_array(0:old_total) = rmail_states(k)%deleted(0:old_total)
            if (old_total > 0) summaries(1:old_total) = rmail_states(k)%summaries(1:old_total)
        end if

        do i = 1, total
            call copy_marker_at(starts(i), messages(old_total + i), .false., st)
            deleted_array(old_total + i) = deleted(i)
        end do

        messages(rmail_states(k)%total_messages + 1) = end_marker
        call move_alloc(messages, rmail_states(k)%messages)
        call move_alloc(deleted_array, rmail_states(k)%deleted)
        call move_alloc(summaries, rmail_states(k)%summaries)
        rmail_states(k)%counted = .true.
        call goto_character(point_minimum())
        case_fold_search%value = saved_fold

        if (.not. quiet) then
            write (text, '("Counting new messages...done (", i0, ")")') total
            call message(trim(text))
        end if
    end subroutine count_rmail_new_messages

    !> Find the messages of the current Rmail buffer and set its message
    !> arrays and counters; the current message is the one containing
    !> point.  Port of rmail.el:rmail-set-message-counters.
    subroutine set_rmail_message_counters()
        integer(int64), allocatable :: starts(:)
        logical, allocatable :: deleted(:)
        type(excursion_type) :: saved
        type(restriction_type) :: restriction
        type(handle_type) :: end_marker
        type(status_type) :: st
        integer(int64) :: point_save, total, after, i
        logical :: saved_fold
        integer :: k

        k = current_state()
        if (k == 0) return
        call forget_rmail_messages(k)
        call save_excursion_begin(saved)
        call save_restriction_begin(restriction)
        call widen()
        point_save = point()
        total = 0
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        ! Determine how many messages follow point.
        call message("Counting messages...")
        call goto_character(point_maximum())
        ! The entry for message N+1 marks the end of message N.
        call copy_marker_at(point(), end_marker, .false., st)
        call set_rmail_message_counters_counter(min(point(), point_save), starts, deleted, total, after)
        if (after < 0) after = 0
        rmail_states(k)%total_messages = total
        rmail_states(k)%current_message = min(total, max(1_int64, total - after))
        ! Make an element 0 in the message array and the deleted array
        ! which will never be used.
        allocate (rmail_states(k)%messages(0:total + 1))
        allocate (rmail_states(k)%deleted(0:total))
        allocate (rmail_states(k)%summaries(total))

        do i = 1, total
            call copy_marker_at(starts(i), rmail_states(k)%messages(i), .false., st)
        end do

        rmail_states(k)%messages(total + 1) = end_marker
        rmail_states(k)%deleted = .false.
        rmail_states(k)%counted = .true.
        case_fold_search%value = saved_fold

        do i = 1, total
            call set_rmail_message_deleted(i, has_rmail_message_attributes(i, ".D"))
        end do

        call message("Counting messages...done")
        call save_restriction_end(restriction)
        call save_excursion_end(saved)
    end subroutine set_rmail_message_counters

    !> Return true if message number N is deleted (in the deleted array).
    !> Port of rmail.el:rmail-message-deleted-p.
    logical function is_rmail_message_deleted(n) result(deleted)
        integer(int64), intent(in) :: n
        integer :: k

        deleted = .false.
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        if (.not. allocated(rmail_states(k)%deleted)) return
        if (n < 0 .or. n > ubound(rmail_states(k)%deleted, 1)) return
        deleted = rmail_states(k)%deleted(n)
    end function is_rmail_message_deleted

    !> Set the deleted state of message number N (in the deleted array).
    !> STATE true means mark as deleted.  Port of
    !> rmail.el:rmail-set-message-deleted-p.
    subroutine set_rmail_message_deleted(n, state)
        integer(int64), intent(in) :: n
        logical, intent(in) :: state
        integer :: k

        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        if (.not. allocated(rmail_states(k)%deleted)) return
        if (n < 0 .or. n > ubound(rmail_states(k)%deleted, 1)) return
        rmail_states(k)%deleted(n) = state
    end subroutine set_rmail_message_deleted

    ! ------------------------------------------------------------------
    ! Showing a message
    ! ------------------------------------------------------------------

    !> Return false if the current message is showing full headers.
    !> Port of rmail.el:rmail-msg-is-pruned.
    logical function is_rmail_message_pruned() result(pruned)
        integer :: k

        pruned = rmail_header_style == RMAIL_HEADER_STYLE_NORMAL
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return

        if (rmail_states(k)%swapped .and. rmail_states(k)%view_header_style /= 0) then
            pruned = rmail_states(k)%view_header_style == RMAIL_HEADER_STYLE_NORMAL
        end if
    end function is_rmail_message_pruned

    !> Toggle between showing full and normal message headers.
    !> With optional integer ARGUMENT, show the normal message header if ARGUMENT
    !> is greater than zero; otherwise, show it in full.  Port of
    !> rmail.el:rmail-toggle-header.
    recursive subroutine toggle_rmail_header(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        integer :: saved

        saved = rmail_header_style

        if (present(argument)) then
            if (argument > 0) then
                rmail_header_style = RMAIL_HEADER_STYLE_NORMAL
            else
                rmail_header_style = RMAIL_HEADER_STYLE_FULL
            end if
        else if (is_rmail_message_pruned()) then
            rmail_header_style = RMAIL_HEADER_STYLE_FULL
        else
            rmail_header_style = RMAIL_HEADER_STYLE_NORMAL
        end if

        call show_rmail_message(status)
        rmail_header_style = saved
    end subroutine toggle_rmail_header

    !> Show current message starting from the beginning.  Port of
    !> rmail.el:rmail-beginning-of-message.
    recursive subroutine beginning_of_rmail_message(status)
        type(status_type), intent(out) :: status

        call show_current_message(.false., status)
    end subroutine beginning_of_rmail_message

    !> Show bottom of current message.  Port of
    !> rmail.el:rmail-end-of-message.
    recursive subroutine end_of_rmail_message(status)
        type(status_type), intent(out) :: status

        call show_current_message(.true., status)
    end subroutine end_of_rmail_message

    ! Show the current message again with its header style, with point
    ! at its end (and the end at the bottom of the window) if AT_END,
    ! else at its beginning: `rmail_show_message_hook' bound to that.
    ! Port of the bodies of rmail.el:rmail-beginning-of-message and
    ! rmail.el:rmail-end-of-message.
    recursive subroutine show_current_message(at_end, status)
        logical, intent(in) :: at_end
        type(status_type), intent(out) :: status
        type(hook_type) :: saved_hook
        integer :: saved_style, k

        saved_style = rmail_header_style
        k = rmail_state_index(rmail_buffer())

        if (k > 0) then
            if (rmail_states(k)%swapped .and. rmail_states(k)%view_header_style /= 0) then
                rmail_header_style = rmail_states(k)%view_header_style
            end if
        end if

        saved_hook = rmail_show_message_hook
        call rmail_show_message_hook%clear()

        if (at_end) then
            call rmail_show_message_hook%prepend(goto_end_and_recenter)
        else
            call rmail_show_message_hook%prepend(goto_beginning)
        end if

        status = status_type()
        if (k > 0) call show_rmail_message(status, rmail_states(k)%current_message)
        rmail_show_message_hook = saved_hook
        rmail_header_style = saved_style
    end subroutine show_current_message

    ! Go to the beginning of the message shown.  Not a port: the hook function
    ! of rmail.el:rmail-beginning-of-message.
    recursive subroutine goto_beginning(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call goto_character(point_minimum())
    end subroutine goto_beginning

    ! Go to the end of the message shown, at the bottom of the window.
    ! Not a port: the hook function of rmail.el:rmail-end-of-message.
    recursive subroutine goto_end_and_recenter(status)
        use execute_interactively_mod, only: prefix_argument_type, PREFIX_NUMBER
        type(status_type), intent(out) :: status
        integer :: height

        call goto_character(point_maximum())
        height = window_body_height(selected_window(), status)
        if (.not. status%ok()) return
        call recenter(prefix_argument_type(PREFIX_NUMBER, int(height - 1, int64)), .false., status)
    end subroutine goto_end_and_recenter

    !> Display the entire mailbox file.  Port of rmail.el:rmail-widen.
    recursive subroutine widen_rmail(status)
        type(status_type), intent(out) :: status

        call swap_rmail_buffers_maybe(status)
        call widen()
    end subroutine widen_rmail

    ! "No mail." if there are no messages (erasing the viewer buffer),
    ! else unallocated.  Port of rmail.el:rmail-no-mail-p.
    subroutine check_rmail_no_mail(k, blurb)
        integer, intent(in) :: k
        character(len=:), allocatable, intent(out) :: blurb
        type(handle_type) :: saved
        type(status_type) :: st

        if (rmail_states(k)%total_messages /= 0) return
        saved = current_buffer()
        ! Eg we deleted all the messages, so remove the old N/M mark.
        call set_buffer(buffer_of_serial(rmail_states(k)%serial), st)
        call clear_mode_line_process()
        call set_buffer(rmail_states(k)%view_buffer, st)
        call erase_buffer(st)
        call set_buffer(saved, st)
        blurb = "No mail."
    end subroutine check_rmail_no_mail

    !> Show message number N (prefix argument), counting from start of file.
    !> If summary buffer is currently displayed, update current message there also.
    !> N defaults to the current message; with NO_SUMMARY the summary is
    !> left alone.  Port of rmail.el:rmail-show-message (asking whether to
    !> add a Mail-Followup-To list to `mail_mailing_lists' waits for
    !> saving settings into init.f90).
    recursive subroutine show_rmail_message(status, n, no_summary)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        logical, intent(in), optional :: no_summary
        character(len=:), allocatable :: blurb
        logical :: skip_summary
        integer :: k

        skip_summary = .false.
        if (present(no_summary)) skip_summary = no_summary

        if (buffers(current)%major_mode /= "rmail_mode") then
            call set_buffer(rmail_buffer(), status)
            if (.not. status%ok()) return
            call switch_to_buffer(buffers(current)%name, status)
            if (.not. status%ok()) return
        end if

        k = current_state()

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Not an Rmail buffer")
            return
        end if

        call swap_rmail_buffers_maybe(status)
        if (.not. status%ok()) return
        call set_rmail_message_counters_maybe()
        call widen()
        call show_rmail_message_1(blurb, status, n)
        if (.not. status%ok()) return

        if (rmail_states(k)%total_messages /= 0) then
            if (transient_mark_mode%value) call deactivate_mark(.false.)

            ! If there is a summary buffer, try to move to this message in
            ! that buffer.  But don't complain if this message is not
            ! mentioned in the summary.
            if (has_rmail_summary() .and. .not. skip_summary) then
                call select_rmail_summary(goto_rmail_summary_message_function, rmail_states(k)%current_message, status)
                if (.not. status%ok()) return
            end if
        end if

        if (allocated(blurb)) call message(blurb)
    end subroutine show_rmail_message

    !> Return true if the region contains a text message.  Port of
    !> rmail.el:rmail-is-text-p.
    logical function is_rmail_text() result(text)
        character(len=:), allocatable :: content_type
        type(excursion_type) :: saved
        type(status_type) :: st

        call save_excursion_begin(saved)
        call get_mail_field("content-type", content_type, st)
        call save_excursion_end(saved)
        ! The message is text if either there is no content type header (a
        ! default of "text/plain; charset=US-ASCII" is assumed) or the base
        ! content type is either text or message.
        text = .true.
        if (allocated(content_type)) text = string_match("\(text\|message\)/", content_type, st) > 0
    end function is_rmail_text

    !> Show message MESSAGE (default: current message) using `rmail_view_buffer'.
    !> BLURB is text to display in the minibuffer if MESSAGE is out of
    !> range (displaying a reasonable choice as well), unallocated
    !> otherwise.  The current mail message becomes the message displayed.
    !> Port of rmail.el:rmail-show-message-1 (MIME messages are shown as
    !> other messages until rmailmm.el is ported: `rmail_show_mime_function'
    !> is unset; decoding RFC 2047 header fields waits for rfc2047.el,
    !> and the suspicious From check for textsec.el).
    recursive subroutine show_rmail_message_1(blurb, status, message_number)
        character(len=:), allocatable, intent(out) :: blurb
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: message_number
        type(handle_type) :: mbox_buffer, view_buffer, temporary
        character(len=:), allocatable :: coding_system, character_coding, text
        character(len=100) :: line
        integer(int64) :: n, beginning, end, body_start, found, length, moved
        integer :: header_style, k
        logical :: modified, is_text_message, showing_message, saved_fold, succeeded
        type(status_type) :: st

        status = status_type()
        k = current_state()
        if (k == 0) return
        mbox_buffer = current_buffer()
        view_buffer = rmail_states(k)%view_buffer
        showing_message = .false.
        n = rmail_states(k)%current_message
        if (present(message_number)) n = message_number
        call check_rmail_no_mail(k, blurb)
        if (allocated(blurb)) return

        if (n <= 0) then
            n = 1
            rmail_states(k)%current_message = 1
            blurb = "No previous message"
        else if (n > rmail_states(k)%total_messages) then
            n = rmail_states(k)%total_messages
            rmail_states(k)%current_message = n
            blurb = "No following message"
        else
            rmail_states(k)%current_message = n
        end if

        header_style = rmail_header_style
        ! Mark the message as seen, but preserve buffer modified flag.
        call is_buffer_modified(mbox_buffer, modified, status)
        if (.not. status%ok()) return
        call set_rmail_attribute(RMAIL_UNSEEN_ATTRIBUTE_INDEX, .false., status)
        if (.not. status%ok()) return
        if (rmail_show_message_set_modified) modified = .true.
        if (.not. modified) call restore_buffer_modified(modified)
        ! Bracket the message in the mail buffer and determine the coding
        ! system the transfer encoding.
        call swap_rmail_buffers_maybe(status)
        if (.not. status%ok()) return
        beginning = rmail_message_beginning(n)
        end = rmail_message_end(n)

        if (end - beginning > rmail_show_message_verbose_min) then
            showing_message = .true.
            write (line, '("Showing message ", i0, "...")') n
            call message(trim(line))
        end if

        call narrow_to_region(beginning, end, status)
        if (.not. status%ok()) return
        call goto_character(beginning)
        ! We give the view buffer a buffer-local value of
        ! rmail-header-style based on the binding in effect when this
        ! function is called; `toggle_rmail_header' can inspect this value
        ! to determine how to toggle.
        rmail_states(k)%view_header_style = header_style
        ! In case viewing the previous message sets the paragraph direction,
        ! we reset it here to allow independent dynamic determination of
        ! paragraph direction in every message.
        buffers(view_buffer%index)%bidirectional_paragraph_direction = 0
        call search_forward(LF//LF, body_start, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return
        call narrow_to_region(beginning, point(), status)
        if (.not. status%ok()) return
        call goto_character(beginning)
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call regular_expression_search_forward("^X-Coding-System: *\(.*\)$", found, status, noerror=SEARCH_NOERROR)
        case_fold_search%value = saved_fold
        if (.not. status%ok()) return

        if (found > 0) then
            coding_system = match_text(1)
        else
            coding_system = get_rmail_coding_system()
        end if

        call goto_character(beginning)
        call get_mail_field("content-transfer-encoding", character_coding, status)
        if (.not. status%ok()) return
        is_text_message = is_rmail_text()
        if (allocated(character_coding)) character_coding = downcase_ascii(character_coding)
        call narrow_to_region(beginning, end, status)
        if (.not. status%ok()) return
        ! Decode the message body into an empty view buffer using a unibyte
        ! temporary buffer where the character decoding takes place.
        call set_buffer(view_buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(status)
        if (.not. status%ok()) return
        call set_buffer(mbox_buffer, status)
        if (.not. status%ok()) return
        if (body_start == 0) body_start = end

        if (.not. allocated(character_coding)) then
            ! Do it directly since that is fast.
            call decode_rmail_region(body_start, end, coding_system, status, view_buffer)
            if (.not. status%ok()) return
        else
            call buffer_substring_no_properties(body_start, end, text, status)
            if (.not. status%ok()) return
            call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
            if (.not. status%ok()) return
            call set_buffer(temporary, status)
            buffers(current)%multibyte = .false.
            if (buffers(mbox_buffer%index)%multibyte) text = string_as_unibyte(text)
            if (status%ok()) call insert(text, status)

            if (status%ok()) then
                if (character_coding == "quoted-printable") then
                    call unquote_mail_printable_region(point_minimum(), point_maximum(), st, noerror=.true., unibyte=.true., &
                                                       succeeded=succeeded)
                    if (.not. succeeded) call message("Malformed MIME quoted-printable message")
                else if (character_coding == "base64" .and. is_text_message) then
                    call base64_decode_region(point_minimum(), point_maximum(), length, st)
                    if (.not. st%ok()) call message(format_error(st))
                end if

                call decode_rmail_region(point_minimum(), point_maximum(), coding_system, status, view_buffer)
            end if

            call set_buffer(mbox_buffer, st)
            call close_buffer(temporary, st)
            if (.not. status%ok()) return
        end if

        call set_buffer(view_buffer, status)
        if (.not. status%ok()) return
        ! Prepare the separator (blank line) before the body.
        call goto_character(point_minimum())
        call insert(LF, status)
        if (.not. status%ok()) return
        ! Unquote quoted From lines.
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        do
            if (rmail_mbox_format%choice == RMAIL_MBOX_FORMAT_MBOXRD) then
                call regular_expression_search_forward("^>+From ", found, status, noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_forward("^>From ", found, status, noerror=SEARCH_NOERROR)
            end if

            if (found == 0 .or. .not. status%ok()) exit
            call beginning_of_line(1_int64)
            call delete_region(point(), point() + 1, status)
            if (.not. status%ok()) exit
            moved = forward_line(1_int64)
        end do

        case_fold_search%value = saved_fold
        if (.not. status%ok()) return
        call goto_character(point_minimum())
        call set_buffer(mbox_buffer, status)
        if (.not. status%ok()) return
        ! Copy the headers to the front of the message view buffer.
        call copy_rmail_headers(k, beginning, status)
        if (.not. status%ok()) return
        ! Highlight the message.
        call set_buffer(view_buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())
        call highlight_rmail_headers(status)
        if (.not. status%ok()) return
        ! Update the mode-line with message status information and swap
        ! the view buffer/mail buffer contents.
        call set_buffer(mbox_buffer, status)
        if (.not. status%ok()) return
        call display_rmail_labels(status)
        if (.not. status%ok()) return
        call swap_rmail_buffers(status)
        if (.not. status%ok()) return
        rmail_states(k)%swapped = .true.

        if (showing_message) then
            write (line, '("Showing message ", i0, "...done")') n
            blurb = trim(line)
        end if

        call rmail_show_message_hook%execute(status)
    end subroutine show_rmail_message_1

    !> Copy displayed header fields to the message viewer buffer.
    !> BEGINNING marks the start position of the message in the mbox
    !> buffer.  If IGNORED_HEADERS is present, ignore all header fields
    !> whose names match that regexp.
    !> Otherwise, if `rmail_displayed_headers' is allocated, copy only
    !> those header fields whose names match that regexp.  Otherwise,
    !> copy all header fields whose names do not match
    !> `rmail_ignored_headers' (unless they also match
    !> `rmail_nonignored_headers').  Moves point in the message viewer
    !> buffer to the end of the headers.  Port of rmail.el:rmail-copy-headers.
    recursive subroutine copy_rmail_headers(k, beginning, status, ignored_headers)
        integer, intent(in) :: k
        integer(int64), intent(in) :: beginning
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: ignored_headers
        character(len=*), parameter :: HEADER_START_REGULAR_EXPRESSION = LF//"[^ "//TAB//"]"
        type(restriction_type) :: restriction
        type(handle_type) :: view, saved
        character(len=:), allocatable :: text, ignored
        integer(int64) :: found, limit, length
        logical :: matched, kept
        type(status_type) :: st

        view = rmail_states(k)%view_buffer
        call search_forward(LF//LF, found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok() .or. found == 0) return
        call goto_character(point() - 1)
        call save_restriction_begin(restriction)

        body: block
            ! Put point right after the From header line.
            call narrow_to_region(beginning, point(), status)
            if (.not. status%ok()) exit body
            call goto_character(point_minimum())
            call regular_expression_search_forward(HEADER_START_REGULAR_EXPRESSION, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) exit body

            if (found == 0) then
                call set_rmail_bad_format_error(status)
                exit body
            end if

            call goto_character(point() - 1)

            if (rmail_header_style == RMAIL_HEADER_STYLE_FULL) then
                ! Handle the case where all headers should be copied.
                call buffer_substring(beginning, point_maximum(), text, status)
                if (.not. status%ok()) exit body
                length = point_maximum() - beginning
                saved = current_buffer()
                call set_buffer(view, status)
                if (.not. status%ok()) exit body
                call goto_character(point_minimum())
                call insert(text, status)
                ! rmail-show-message-1 expects this function to leave point
                ! at the end of the headers.
                call goto_character(1 + length)
                call set_buffer(saved, st)
            else if (allocated(rmail_displayed_headers) .and. .not. present(ignored_headers)) then
                ! Handle the case where the headers matching the displayed
                ! headers regexp should be copied.
                do while (.not. is_end_of_buffer())
                    call find_limit(limit)
                    matched = looking_at(rmail_displayed_headers, status)
                    if (.not. status%ok()) exit body
                    if (matched) call append_to_view(point(), limit)
                    if (.not. status%ok()) exit body
                    call goto_character(limit)
                end do
            else
                ! Handle the ignored headers.
                if (present(ignored_headers)) then
                    ignored = ignored_headers
                else if (allocated(rmail_ignored_headers)) then
                    ignored = rmail_ignored_headers
                else
                    call set_error(status, GENERIC_ERROR_CODE, "No headers selected for display!")
                    exit body
                end if

                do while (.not. is_end_of_buffer())
                    call find_limit(limit)
                    matched = looking_at(ignored, status)
                    kept = .false.
                    if (matched .and. allocated(rmail_nonignored_headers)) kept = looking_at(rmail_nonignored_headers, status)
                    if (.not. status%ok()) exit body
                    if (.not. matched .or. kept) call append_to_view(point(), limit)
                    if (.not. status%ok()) exit body
                    call goto_character(limit)
                end do
            end if
        end block body

        call save_restriction_end(restriction)

    contains

        ! The end of the header field at point.
        subroutine find_limit(limit)
            integer(int64), intent(out) :: limit
            type(excursion_type) :: excursion
            type(status_type) :: st2
            integer(int64) :: at

            call save_excursion_begin(excursion)
            call regular_expression_search_forward(HEADER_START_REGULAR_EXPRESSION, at, st2, noerror=SEARCH_NOERROR)

            if (at > 0) then
                limit = 1 + match_beginning(0, st2)
            else
                limit = point_maximum()
            end if

            call save_excursion_end(excursion)
        end subroutine find_limit

        ! Insert the text from FROM to TO at the viewer buffer's point
        ! (append-to-buffer).
        subroutine append_to_view(from, to)
            integer(int64), intent(in) :: from, to
            character(len=:), allocatable :: field
            type(handle_type) :: here
            type(status_type) :: st2

            call buffer_substring(from, to, field, status)
            if (.not. status%ok()) return
            here = current_buffer()
            call set_buffer(view, status)
            if (.not. status%ok()) return
            call insert(field, status)
            call set_buffer(here, st2)
        end subroutine append_to_view
    end subroutine copy_rmail_headers

    !> Highlight the headers specified by `rmail_highlighted_headers'.
    !> Uses the face `rmail_highlight'.  Port of
    !> rmail.el:rmail-highlight-headers.
    subroutine highlight_rmail_headers(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        type(restriction_type) :: restriction
        type(handle_type) :: overlay
        type(handle_type), allocatable :: overlays(:)
        integer(int64) :: found, beginning, moved
        integer :: k, used
        logical :: saved_fold, saved_inhibit

        status = status_type()
        if (.not. allocated(rmail_highlighted_headers)) return
        k = rmail_state_of_view()
        if (k == 0) return
        call save_excursion_begin(saved)
        call search_forward(LF//LF, found, status, noerror=SEARCH_MOVE)
        call save_restriction_begin(restriction)
        call narrow_to_region(point_minimum(), point(), status)
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        saved_inhibit = buffers(current)%read_only
        buffers(current)%read_only = .false.
        ! List of overlays to reuse.
        if (.not. allocated(rmail_states(k)%overlays)) allocate (rmail_states(k)%overlays(0))
        overlays = rmail_states(k)%overlays
        used = 0
        call goto_character(point_minimum())

        do while (status%ok())
            call regular_expression_search_forward(rmail_highlighted_headers, found, status, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. status%ok()) exit
            moved = skip_characters_forward(" "//TAB, status)
            beginning = point()

            do
                if (forward_line(1_int64) /= 0) exit
                if (.not. looking_at("[ "//TAB//"]", status)) exit
            end do

            ! Back up over newline, then trailing spaces or tabs.
            call goto_character(point() - 1)

            do while (preceding_character() == iachar(" ") .or. preceding_character() == iachar(TAB))
                call goto_character(point() - 1)
            end do

            if (used < size(overlays)) then
                ! Reuse an overlay we already have.
                used = used + 1
                overlay = overlays(used)
                call overlay_put(overlay, "face", value_type(name=trim(rmail_highlight%name)), status)
                if (status%ok()) call move_overlay(overlay, beginning, point(), status)
            else
                ! Make a new overlay and add it to the overlay list.
                call make_overlay(overlay, beginning, point(), status)
                if (status%ok()) call overlay_put(overlay, "face", value_type(name=trim(rmail_highlight%name)), status)
                rmail_states(k)%overlays = [overlay, rmail_states(k)%overlays]
            end if
        end do

        buffers(current)%read_only = saved_inhibit
        case_fold_search%value = saved_fold
        call save_restriction_end(restriction)
        call save_excursion_end(saved)
    end subroutine highlight_rmail_headers

    ! The state whose viewer buffer is the current buffer, or whose Rmail
    ! buffer is current (0 if none).  Not a port.
    integer function rmail_state_of_view() result(k)
        k = 0
        if (.not. allocated(rmail_states) .or. current == 0) return

        do k = 1, size(rmail_states)
            if (is_same_buffer(rmail_states(k)%view_buffer, current_buffer())) return
        end do

        k = current_state()
    end function rmail_state_of_view

    ! ------------------------------------------------------------------
    ! Moving between messages
    ! ------------------------------------------------------------------

    !> Show following message whether deleted or not.
    !> With prefix arg N, moves forward N messages, or backward if N is
    !> negative.  Port of rmail.el:rmail-next-message.
    recursive subroutine next_rmail_message(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer :: k

        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        call set_rmail_message_counters_maybe()
        call show_rmail_message(status, rmail_states(k)%current_message + n)
    end subroutine next_rmail_message

    !> Show previous message whether deleted or not.
    !> With prefix arg N, moves backward N messages, or forward if N is
    !> negative.  Port of rmail.el:rmail-previous-message.
    recursive subroutine previous_rmail_message(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call next_rmail_message(-n, status)
    end subroutine previous_rmail_message

    !> Show following non-deleted message.
    !> With prefix arg N, moves forward N non-deleted messages,
    !> or backward if N is negative.
    !>
    !> SHOWN is true if a new message is being shown.  Port of
    !> rmail.el:rmail-next-undeleted-message.
    recursive subroutine next_rmail_undeleted_message(n, status, shown)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(out), optional :: shown
        integer(int64) :: count, last_window, at
        integer :: k

        if (present(shown)) shown = .false.
        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        call set_rmail_message_counters_maybe()
        count = n
        last_window = rmail_states(k)%current_message
        at = rmail_states(k)%current_message

        do while (count > 0 .and. at < rmail_states(k)%total_messages)
            at = at + 1

            if (.not. is_rmail_message_deleted(at)) then
                last_window = at
                count = count - 1
            end if
        end do

        do while (count < 0 .and. at > 1)
            at = at - 1

            if (.not. is_rmail_message_deleted(at)) then
                last_window = at
                count = count + 1
            end if
        end do

        if (last_window /= rmail_states(k)%current_message) then
            call show_rmail_message(status, last_window)
            if (present(shown)) shown = status%ok()
        else
            if (count < 0) call message("No previous nondeleted message")
            if (count > 0) call message("No following nondeleted message")
        end if
    end subroutine next_rmail_undeleted_message

    !> Show previous non-deleted message.
    !> With prefix argument N, moves backward N non-deleted messages,
    !> or forward if N is negative.  Port of
    !> rmail.el:rmail-previous-undeleted-message.
    recursive subroutine previous_rmail_undeleted_message(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call next_rmail_undeleted_message(-n, status)
    end subroutine previous_rmail_undeleted_message

    !> Show first message in file.  Port of rmail.el:rmail-first-message.
    recursive subroutine show_first_rmail_message(status)
        type(status_type), intent(out) :: status

        call set_rmail_message_counters_maybe()
        call show_rmail_message(status, 1_int64)
    end subroutine show_first_rmail_message

    !> Show last message in file.  Port of rmail.el:rmail-last-message.
    recursive subroutine show_last_rmail_message(status)
        type(status_type), intent(out) :: status
        integer :: k

        call set_rmail_message_counters_maybe()
        k = rmail_state_index(rmail_buffer())
        status = status_type()
        if (k > 0) call show_rmail_message(status, rmail_states(k)%total_messages)
    end subroutine show_last_rmail_message

    !> Return message number POSITION (or point) is in.  Port of
    !> rmail.el:rmail-what-message.
    integer(int64) function get_rmail_message_at(position) result(n)
        integer(int64), intent(in), optional :: position
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: high, middle, low, where
        integer :: k

        n = 0
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        high = rmail_states(k)%total_messages
        middle = high/2
        low = 1

        if (present(position)) then
            where = position
        else if (rmail_states(k)%swapped) then
            saved = current_buffer()
            call set_buffer(rmail_states(k)%view_buffer, st)
            where = point()
            call set_buffer(saved, st)
        else
            where = point()
        end if

        do while (high - low > 1)
            if (where >= rmail_message_beginning(middle)) then
                low = middle
            else
                high = middle
            end if

            middle = low + (high - low)/2
        end do

        n = low
        if (where >= rmail_message_beginning(high)) n = high
    end function get_rmail_message_at

    ! Whether regexp REGULAR_EXPRESSION matches in message number MESSAGE; point is moved
    ! there.  Port of rmail.el:rmail-search-message (the MIME search waits
    ! for rmailmm.el).
    logical function search_rmail_message(message, regular_expression, status) result(found)
        integer(int64), intent(in) :: message
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64) :: at

        call goto_character(rmail_message_beginning(message))
        call regular_expression_search_forward(regular_expression, at, status, bound=rmail_message_end(message), &
                                               noerror=SEARCH_NOERROR)
        found = at > 0 .and. status%ok()
    end function search_rmail_message

    !> Show message containing next match for REGULAR_EXPRESSION (but not the current msg).
    !> Prefix argument gives repeat count; negative argument means search
    !> backwards (through earlier messages).
    !> Interactively, empty argument means use same regexp used last time.
    !> Port of rmail.el:rmail-search.
    recursive subroutine search_rmail(regular_expression, status, n)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        type(status_type) :: st
        character(len=:), allocatable :: reverse
        integer(int64) :: count, original_message, message_number, original_point, at
        logical :: reversep, found, had_original_point
        integer :: k

        count = 1
        if (present(n)) count = n
        reverse = ""
        if (count < 0) reverse = "Reverse "
        call message(reverse//"Rmail search for "//regular_expression//"...")
        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        original_message = rmail_states(k)%current_message
        message_number = rmail_states(k)%current_message
        reversep = count < 0
        had_original_point = rmail_states(k)%swapped
        original_point = point()
        call swap_rmail_buffers_maybe(status)
        if (.not. status%ok()) return
        call set_rmail_message_counters_maybe()
        call widen()
        found = .false.

        ! Check messages one by one, advancing message number up or down
        ! but searching forward through each message.
        do while (count /= 0 .and. status%ok())
            if (reversep) then
                do while (.not. found .and. message_number > 1)
                    message_number = message_number - 1
                    found = search_rmail_message(message_number, regular_expression, status)
                    if (.not. status%ok()) exit
                end do
            else
                do while (.not. found .and. message_number < rmail_states(k)%total_messages)
                    message_number = message_number + 1
                    found = search_rmail_message(message_number, regular_expression, status)
                    if (.not. status%ok()) exit
                end do
            end if

            count = count + merge(1, -1, reversep)
        end do

        if (found .and. status%ok()) then
            call show_rmail_message(status, message_number)
            if (.not. status%ok()) return

            ! Search forward (if this is a normal search) or backward (if
            ! this is a reverse search) through this message to position
            ! point.  This search may fail because REGULAR_EXPRESSION was found in the
            ! hidden portion of this message.  In that case, move point to
            ! the beginning of visible portion.
            if (reversep) then
                call goto_character(point_maximum())
                call regular_expression_search_backward(regular_expression, at, st, noerror=SEARCH_MOVE)
            else
                call goto_character(point_minimum())
                call regular_expression_search_forward(regular_expression, at, st, noerror=SEARCH_NOERROR)
            end if

            call message(reverse//"Rmail search for "//regular_expression//"...done")
        else
            call show_rmail_message(st, original_message)
            if (had_original_point) call goto_character(original_point)
            call ding(.false., st)
            if (status%ok()) call message("Search failed: "//regular_expression)
        end if
    end subroutine search_rmail

    !> Show message containing previous match for REGULAR_EXPRESSION.
    !> Prefix argument gives repeat count; negative argument means search
    !> forward (through later messages).
    !> Interactively, empty argument means use same regexp used last time.
    !> Port of rmail.el:rmail-search-backwards.
    recursive subroutine search_rmail_backwards(regular_expression, status, n)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        integer(int64) :: count

        count = 1
        if (present(n)) count = n
        call search_rmail(regular_expression, status, -count)
    end subroutine search_rmail_backwards

    !> Read the REGULAR_EXPRESSION and the count N of \[search_rmail] (if BACKWARDS, of
    !> \[search_rmail_backwards]); empty input means the last regexp.
    !> Port of the argument reading of rmail.el:rmail-search and
    !> rmail.el:rmail-search-backwards.
    recursive subroutine read_rmail_search(backwards, regular_expression, n, status)
        logical, intent(in) :: backwards
        character(len=:), allocatable, intent(out) :: regular_expression
        integer(int64), intent(out) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prompt, input
        logical :: reversep

        n = prefix_numeric_value(current_prefix_argument)

        if (backwards) then
            reversep = n >= 0
        else
            reversep = n < 0
        end if

        prompt = "Rmail search (regexp"
        if (reversep) prompt = "Reverse "//prompt

        if (allocated(search_last_regular_expression)) then
            prompt = prompt//", default "//search_last_regular_expression//"): "
        else
            prompt = prompt//"): "
        end if

        call read_string(prompt, input, status)
        if (.not. status%ok()) return

        if (len(input) > 0) then
            search_last_regular_expression = input
        else if (.not. allocated(search_last_regular_expression)) then
            call set_error(status, GENERIC_ERROR_CODE, "No previous Rmail search string")
            return
        end if

        regular_expression = search_last_regular_expression
    end subroutine read_rmail_search

    !> Return message number of first message which has `unseen'
    !> attribute (0 for none).  Port of rmail.el:rmail-first-unseen-message.
    integer(int64) function get_first_rmail_unseen_message() result(found)
        type(restriction_type) :: restriction
        integer(int64) :: at
        integer :: k

        found = 0
        call set_rmail_message_counters_maybe()
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        call save_restriction_begin(restriction)
        call widen()
        at = 1

        do while (found == 0 .and. at <= rmail_states(k)%total_messages)
            if (has_rmail_message_attributes(at, "......U")) found = at
            at = at + 1
        end do

        call save_restriction_end(restriction)
    end function get_first_rmail_unseen_message

    ! ------------------------------------------------------------------
    ! Deleting and expunging
    ! ------------------------------------------------------------------

    !> Delete this message and stay on it.  Port of
    !> rmail.el:rmail-delete-message.
    recursive subroutine delete_rmail_message(status)
        type(status_type), intent(out) :: status
        integer(int64) :: deleted_message
        integer :: k

        call set_rmail_attribute(RMAIL_DELETED_ATTRIBUTE_INDEX, .true., status)
        if (.not. status%ok()) return
        call rmail_delete_message_hook%execute(status)
        if (.not. status%ok()) return
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        deleted_message = rmail_states(k)%current_message
        if (has_rmail_summary()) call select_rmail_summary(mark_rmail_summary_deleted_function, deleted_message, status)
    end subroutine delete_rmail_message

    !> Back up to deleted message, select it, and undelete it.  Port of
    !> rmail.el:rmail-undelete-previous-message.
    recursive subroutine undelete_previous_rmail_message(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        integer(int64) :: message, i
        type(status_type) :: st
        integer :: k

        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return

        do i = 1, count
            message = rmail_states(k)%current_message

            do while (message > 0)
                if (is_rmail_message_deleted(message)) exit
                message = message - 1
            end do

            if (message == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "No previous deleted message")
                return
            end if

            if (message /= rmail_states(k)%current_message) then
                call show_rmail_message(status, message)
                if (.not. status%ok()) return
            end if

            call set_rmail_attribute(RMAIL_DELETED_ATTRIBUTE_INDEX, .false., status)
            if (.not. status%ok()) return

            if (has_rmail_summary() .and. associated(mark_rmail_summary_undeleted_function)) then
                saved = current_buffer()
                call set_buffer(rmail_states(k)%summary_buffer, status)
                if (status%ok()) call mark_rmail_summary_undeleted_function(message, status)
                call set_buffer(saved, st)
                if (.not. status%ok()) return
            end if
        end do

        call display_rmail_summary_maybe(status)
    end subroutine undelete_previous_rmail_message

    !> Delete this message and move to next nondeleted one.
    !> Deleted messages stay in the file until the \[expunge_rmail] command is given.
    !> Optional argument COUNT (interactively, prefix argument) is a repeat count;
    !> negative argument means move backwards instead of forwards.
    !>
    !> SHOWN is true if a new message is displayed after the delete.
    !> Port of rmail.el:rmail-delete-forward.
    recursive subroutine delete_rmail_message_forward(status, count, shown)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(out), optional :: shown
        integer(int64) :: n, i, deleted_message
        logical :: value, backward
        integer :: k

        n = 1
        if (present(count)) n = count
        value = .false.
        backward = n < 0
        n = abs(n)

        do i = 1, n
            call set_rmail_attribute(RMAIL_DELETED_ATTRIBUTE_INDEX, .true., status)
            if (.not. status%ok()) return
            call rmail_delete_message_hook%execute(status)
            if (.not. status%ok()) return
            k = rmail_state_index(rmail_buffer())
            deleted_message = rmail_states(k)%current_message

            if (has_rmail_summary()) then
                call select_rmail_summary(mark_rmail_summary_deleted_function, deleted_message, status)
                if (.not. status%ok()) return
            end if

            call next_rmail_undeleted_message(merge(-1_int64, 1_int64, backward), status, value)
            if (.not. status%ok()) return
        end do

        call display_rmail_summary_maybe(status)
        if (present(shown)) shown = value
    end subroutine delete_rmail_message_forward

    !> Delete this message and move to previous nondeleted one.
    !> Deleted messages stay in the file until the \[expunge_rmail] command is given.
    !> Optional argument COUNT (interactively, prefix argument) is a repeat count;
    !> negative argument means move forwards instead of backwards.
    !>
    !> SHOWN is true if a new message is displayed after the delete.
    !> Port of rmail.el:rmail-delete-backward.
    recursive subroutine delete_rmail_message_backward(status, count, shown)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(out), optional :: shown
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call delete_rmail_message_forward(status, -n, shown)
    end subroutine delete_rmail_message_backward

    !> CONFIRMED is true if expunge is needed and desirable: there are
    !> deleted messages and, if `rmail_confirm_expunge' asks, the user
    !> confirms.  Port of rmail.el:rmail-expunge-confirmed.
    recursive subroutine is_rmail_expunge_confirmed(confirmed, status)
        logical, intent(out) :: confirmed
        type(status_type), intent(out) :: status
        character(len=*), parameter :: QUESTION = "Erase deleted messages from Rmail file? "
        integer :: k

        confirmed = .false.
        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        if (.not. allocated(rmail_states(k)%deleted)) return
        if (.not. any(rmail_states(k)%deleted)) return

        if (rmail_confirm_expunge%is_off()) then
            confirmed = .true.
        else if (rmail_confirm_expunge%choice == RMAIL_CONFIRM_EXPUNGE_READ_YES_OR_NO) then
            call read_yes_or_no(QUESTION, confirmed, status)
        else
            call read_y_or_n(QUESTION, confirmed, status)
        end if
    end subroutine is_rmail_expunge_confirmed

    !> Actually erase all deleted messages in the file.  Port of
    !> rmail.el:rmail-only-expunge.
    recursive subroutine expunge_rmail_only(status, dont_show)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: dont_show
        type(excursion_type) :: saved
        type(handle_type), allocatable :: messages(:)
        type(string_type), allocatable :: summaries(:)
        type(handle_type) :: saved_buffer
        type(status_type) :: st
        character(len=60) :: text
        integer(int64) :: omax, omin, original_point, counter, number, total, new_message_number, beginning, end
        logical :: window, showing, saved_read_only
        integer :: k

        call swap_rmail_buffers_maybe(status)
        if (.not. status%ok()) return
        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        call message("Expunging deleted messages...")
        ! Discard all undo records for this buffer.
        buffers(current)%undo_count = 0
        call set_rmail_message_counters_maybe()
        omax = buffer_size() - point_maximum()
        omin = buffer_size() - point_minimum()
        original_point = 0

        if (.not. (rmail_states(k)%current_message > 0 .and. &
                   is_rmail_message_deleted(rmail_states(k)%current_message))) then
            if (rmail_enable_mime) then
                saved_buffer = current_buffer()
                call set_buffer(rmail_states(k)%view_buffer, st)
                original_point = point() - point_minimum()
                call set_buffer(saved_buffer, st)
            else
                original_point = point() - point_minimum()
            end if
        end if

        window = .false.
        call save_excursion_begin(saved)
        call widen()
        call goto_character(point_minimum())
        counter = 0
        number = 1
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        total = rmail_states(k)%total_messages
        new_message_number = rmail_states(k)%current_message
        allocate (messages(0:total + 1), summaries(total))
        messages(0) = rmail_states(k)%messages(0)

        do while (number <= total)
            if (rmail_states(k)%deleted(number)) then
                call marker_position(rmail_states(k)%messages(number), beginning, st)
                call marker_position(rmail_states(k)%messages(number + 1), end, st)
                call delete_region(beginning, end, status)
                if (.not. status%ok()) exit
                call destroy_marker(rmail_states(k)%messages(number), st)
                if (new_message_number > counter) new_message_number = new_message_number - 1
            else
                counter = counter + 1
                messages(counter) = rmail_states(k)%messages(number)

                if (counter == number .and. allocated(rmail_states(k)%summaries(number)%text)) then
                    summaries(counter)%text = rmail_states(k)%summaries(number)%text
                end if
            end if

            number = number + 1

            if (mod(number, 20_int64) == 0) then
                write (text, '("Expunging deleted messages...", i0)') number
                call message(trim(text))
            end if
        end do

        buffers(current)%read_only = saved_read_only

        if (status%ok()) then
            messages(counter + 1) = rmail_states(k)%messages(number)
            rmail_states(k)%current_message = new_message_number
            rmail_states(k)%total_messages = counter
            deallocate (rmail_states(k)%messages)
            allocate (rmail_states(k)%messages(0:counter + 1))
            rmail_states(k)%messages(0:counter + 1) = messages(0:counter + 1)
            deallocate (rmail_states(k)%deleted)
            allocate (rmail_states(k)%deleted(0:counter))
            rmail_states(k)%deleted = .false.
            rmail_states(k)%summaries = summaries(1:counter)
            window = .true.
        end if

        call save_excursion_end(saved)
        call message("Expunging deleted messages...done")
        if (.not. window) call narrow_to_region(buffer_size() - omin, buffer_size() - omax, st)
        if (.not. status%ok()) return
        showing = .true.
        if (present(dont_show)) showing = .not. dont_show

        if (showing) then
            call show_rmail_message(status, min(rmail_states(k)%current_message, rmail_states(k)%total_messages))
            if (.not. status%ok()) return
        end if

        if (rmail_enable_mime) then
            call goto_character(point_minimum() + original_point)
        else
            call goto_character(point() + original_point)
        end if
    end subroutine expunge_rmail_only

    !> Erase deleted messages from Rmail file and summary buffer.
    !> This always shows a message (so as not to leave the Rmail buffer
    !> unswapped), and always updates any summary (so that it remains
    !> consistent with the Rmail buffer).  If DONT_SHOW is true, it
    !> does not pop any summary buffer.  Port of rmail.el:rmail-expunge.
    recursive subroutine expunge_rmail(status, dont_show)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: dont_show
        character(len=:), allocatable :: blurb
        type(handle_type) :: saved
        type(status_type) :: st
        logical :: confirmed, was_deleted, was_swapped, quiet
        integer :: k

        call is_rmail_expunge_confirmed(confirmed, status)
        if (.not. status%ok() .or. .not. confirmed) return
        call modify_rmail_format(status)
        if (.not. status%ok()) return
        k = current_state()
        if (k == 0) return
        was_deleted = is_rmail_message_deleted(rmail_states(k)%current_message)
        was_swapped = rmail_states(k)%swapped
        call expunge_rmail_only(status, .true.)
        if (.not. status%ok()) return
        quiet = .false.
        if (present(dont_show)) quiet = dont_show

        ! We always update the summary buffer, so that the contents remain
        ! consistent with the rmail buffer.
        if (has_rmail_summary()) then
            if (quiet) then
                if (associated(update_rmail_summary_function)) then
                    saved = current_buffer()
                    call set_buffer(rmail_states(k)%summary_buffer, status)
                    if (status%ok()) call update_rmail_summary_function(0_int64, status)
                    call set_buffer(saved, st)
                end if
            else
                call select_rmail_summary(update_rmail_summary_function, 0_int64, status)
            end if

            if (.not. status%ok()) return
        end if

        ! We always show a message, because (rmail-only-expunge t) leaves
        ! the rmail buffer unswapped.  If we expunged the current message,
        ! a new one is current now, so show it.  If we weren't showing a
        ! message, show it.
        if (was_deleted .or. .not. was_swapped) then
            call show_rmail_message_1(blurb, status, rmail_states(k)%current_message)
        else
            ! We can just show the same message that was being shown before.
            call display_rmail_labels(status)
            if (.not. status%ok()) return
            call swap_rmail_buffers(status)
            if (.not. status%ok()) return
            rmail_states(k)%swapped = .true.
        end if
    end subroutine expunge_rmail

    !> Expunge and save RMAIL file.  Port of
    !> rmail.el:rmail-expunge-and-save.
    recursive subroutine expunge_and_save_rmail(status)
        type(status_type), intent(out) :: status
        integer :: k

        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        call expunge_rmail(status)
        if (.not. status%ok()) return
        ! No need to swap buffers: rmail-write-region-annotate takes care
        ! of it.
        call save_buffer(1_int64, .false., status)
        if (.not. status%ok()) return
        if (has_rmail_summary()) call select_rmail_summary(mark_unmodified, 0_int64, status)
    end subroutine expunge_and_save_rmail

    ! Mark the current buffer unmodified.  Not a port: the
    ! (set-buffer-modified-p nil) upstream runs in the summary.
    recursive subroutine mark_unmodified(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        status = status_type()
        if (n < 0) return
        call set_buffer_modified(.false.)
    end subroutine mark_unmodified

    !> Quit out of RMAIL.
    !> Hook `rmail_quit_hook' is run after expunging.  Port of
    !> rmail.el:rmail-quit.
    recursive subroutine quit_rmail(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: summary, rmail_buffer_handle, saved
        type(status_type) :: st
        integer :: k

        call enter_rmail_buffer(k, status)
        if (.not. status%ok()) return
        call expunge_rmail(status, .true.)
        if (.not. status%ok()) return
        call save_buffer(1_int64, .false., status)
        if (.not. status%ok()) return
        call rmail_quit_hook%execute(status)
        if (.not. status%ok()) return

        ! Don't switch to the summary buffer even if it was recently
        ! visible.
        if (has_rmail_summary()) then
            summary = rmail_states(k)%summary_buffer
            saved = current_buffer()
            call set_buffer(summary, st)
            call set_buffer_modified(.false.)
            call set_buffer(saved, st)
            call replace_buffer_in_windows(summary)
            call bury_buffer(status, summary)
            if (.not. status%ok()) return
        end if

        rmail_buffer_handle = current_buffer()
        call quit_window(.false., status)
        if (.not. status%ok()) return
        call replace_buffer_in_windows(rmail_buffer_handle)
    end subroutine quit_rmail

    !> Bury current Rmail buffer and its summary buffer.  Port of
    !> rmail.el:rmail-bury.
    recursive subroutine bury_rmail(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: window, summary
        integer :: k

        status = status_type()
        k = rmail_state_index(rmail_buffer())

        if (k > 0) then
            summary = rmail_states(k)%summary_buffer

            if (is_buffer_live(summary)) then
                do
                    window = get_buffer_window(summary)
                    if (window%index == 0) exit
                    call quit_window(.false., status, window)
                    if (.not. status%ok()) return
                end do

                call bury_buffer(status, summary)
                if (.not. status%ok()) return
            end if
        end if

        call quit_window(.false., status)
    end subroutine bury_rmail

    ! ------------------------------------------------------------------
    ! The summary, as Rmail uses it
    ! ------------------------------------------------------------------

    !> True if in an RMAIL buffer and an associated summary buffer exists.
    !> Port of rmail.el:rmail-summary-exists.
    logical function has_rmail_summary() result(exists)
        integer :: k

        exists = .false.
        k = rmail_state_index(rmail_buffer())
        if (k > 0) exists = is_buffer_live(rmail_states(k)%summary_buffer)
    end function has_rmail_summary

    !> Return true if in RMAIL buffer and an associated summary buffer is
    !> displayed.  Port of rmail.el:rmail-summary-displayed.
    logical function is_rmail_summary_displayed() result(displayed)
        type(handle_type) :: window
        integer :: k

        displayed = .false.
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        if (.not. is_buffer_live(rmail_states(k)%summary_buffer)) return
        window = get_buffer_window(rmail_states(k)%summary_buffer)
        displayed = window%index /= 0
    end function is_rmail_summary_displayed

    !> Put the summary buffer back on the screen, if user wants that.
    !> Port of rmail.el:rmail-maybe-display-summary (resizing the window
    !> with `rmail_summary_window_size' waits for window-resize-no-error).
    recursive subroutine display_rmail_summary_maybe(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        integer :: k

        status = status_type()
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        if (.not. is_buffer_live(rmail_states(k)%summary_buffer)) return

        if (rmail_redisplay_summary) then
            ! If `rmail-redisplay-summary' is true, make sure the summary
            ! buffer is displayed.
            call display_buffer(rmail_states(k)%summary_buffer, window, status)
        end if
    end subroutine display_rmail_summary_maybe

    !> Perform BODY (with message number N) in the summary buffer in such a
    !> way that its cursor is properly updated in its own window.  Port of
    !> rmail.el:rmail-select-summary (code upstream expands in place).
    recursive subroutine select_rmail_summary(body, n, status)
        use window_mod, only: pop_to_buffer, select_window
        procedure(rmail_summary_procedure), optional :: body
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: window, saved, summary
        type(excursion_type) :: excursion
        type(status_type) :: st
        integer :: k

        status = status_type()
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        summary = rmail_states(k)%summary_buffer

        if (is_rmail_summary_displayed()) then
            window = selected_window()
            call save_excursion_begin(excursion)
            call pop_to_rmail_buffer(summary, status)
            if (status%ok() .and. present(body)) call body(n, status)
            call select_window(window, st)
            call save_excursion_end(excursion)
        else
            saved = current_buffer()
            call set_buffer(summary, status)
            if (status%ok() .and. present(body)) call body(n, status)
            call set_buffer(saved, st)
        end if

        if (.not. status%ok()) return
        call display_rmail_summary_maybe(status)
    end subroutine select_rmail_summary

    !> Like `pop_to_buffer', but with `split_width_threshold' set to
    !> none: Rmail does not expect horizontal splitting.  Port of
    !> rmail.el:rmail-pop-to-buffer.
    subroutine pop_to_rmail_buffer(buffer, status)
        use window_mod, only: pop_to_buffer, split_width_threshold
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer :: saved

        saved = split_width_threshold
        split_width_threshold = huge(saved)
        call pop_to_buffer(buffer, status)
        split_width_threshold = saved
    end subroutine pop_to_rmail_buffer

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine execute_rmail(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call read_file_name("Run rmail on RMAIL file: ", file, status)
            if (status%ok()) call rmail(status, file)
        else
            call rmail(status)
        end if
    end subroutine execute_rmail

    recursive subroutine execute_rmail_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Run rmail on RMAIL file: ", file, status)
        if (status%ok()) call rmail_input(file, status)
    end subroutine execute_rmail_input

    recursive subroutine execute_get_rmail_new_mail(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call read_file_name("Get new mail from file: ", file, status)
            if (status%ok()) call get_rmail_new_mail(status, file)
        else
            call get_rmail_new_mail(status)
        end if
    end subroutine execute_get_rmail_new_mail

    recursive subroutine execute_show_rmail_message(status)
        type(status_type), intent(out) :: status

        call show_rmail_message(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_show_rmail_message

    recursive subroutine execute_next_rmail_message(status)
        type(status_type), intent(out) :: status

        call next_rmail_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_next_rmail_message

    recursive subroutine execute_previous_rmail_message(status)
        type(status_type), intent(out) :: status

        call previous_rmail_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_previous_rmail_message

    recursive subroutine execute_next_rmail_undeleted_message(status)
        type(status_type), intent(out) :: status

        call next_rmail_undeleted_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_next_rmail_undeleted_message

    recursive subroutine execute_previous_rmail_undeleted_message(status)
        type(status_type), intent(out) :: status

        call previous_rmail_undeleted_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_previous_rmail_undeleted_message

    recursive subroutine execute_search_rmail(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer(int64) :: n

        call read_rmail_search(.false., regular_expression, n, status)
        if (status%ok()) call search_rmail(regular_expression, status, n)
    end subroutine execute_search_rmail

    recursive subroutine execute_search_rmail_backwards(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer(int64) :: n

        call read_rmail_search(.true., regular_expression, n, status)
        if (status%ok()) call search_rmail_backwards(regular_expression, status, n)
    end subroutine execute_search_rmail_backwards

    recursive subroutine execute_delete_rmail_message_forward(status)
        type(status_type), intent(out) :: status

        call delete_rmail_message_forward(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_delete_rmail_message_forward

    recursive subroutine execute_delete_rmail_message_backward(status)
        type(status_type), intent(out) :: status

        call delete_rmail_message_backward(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_delete_rmail_message_backward

    recursive subroutine execute_undelete_previous_rmail_message(status)
        type(status_type), intent(out) :: status

        call undelete_previous_rmail_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_undelete_previous_rmail_message

    recursive subroutine execute_expunge_rmail_only(status)
        type(status_type), intent(out) :: status

        call expunge_rmail_only(status)
    end subroutine execute_expunge_rmail_only

    recursive subroutine execute_expunge_rmail(status)
        type(status_type), intent(out) :: status

        call expunge_rmail(status)
    end subroutine execute_expunge_rmail

    recursive subroutine execute_toggle_rmail_header(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call toggle_rmail_header(status)
        else
            call toggle_rmail_header(status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_toggle_rmail_header

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    ! The faces of rmail.el.  Port of the defface forms of rmail.el.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => rmail_highlight
        call f%define("rmail_highlight", "Face to use for highlighting the most important header fields."//LF// &
                      "The variable `rmail_highlighted_headers' specifies which headers.")
        call f%specification_entry(display_when(), face_attributes(inherit="highlight"))
        f => rmail_header_name
        call f%define("rmail_header_name", "Face to use for highlighting the header names."//LF// &
                      "The variable `rmail_font_lock_keywords' specifies which headers"//LF//"get highlighted.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_function_name_face"))
    end subroutine define_faces

    ! The default values of the options that have none yet.  Port of the
    ! defcustom and defvar forms of rmail.el.
    subroutine set_default_values()
        character(len=*), parameter :: NUL = achar(0), BS = achar(8), CR = achar(13), DEL = achar(127)
        character(len=:), allocatable :: time_zone_regular_expression

        if (.not. allocated(rmail_file_name)) rmail_file_name = "~/RMAIL"

        if (.not. allocated(rmail_spool_directory)) then
            if (file_exists("/var/mail")) then
                ! SVR4 and recent BSD are said to use this.
                rmail_spool_directory = "/var/mail/"
            else if (file_exists("/var/spool/mail")) then
                ! Many GNU/Linux systems use this name.
                rmail_spool_directory = "/var/spool/mail/"
            else
                rmail_spool_directory = "/usr/spool/mail/"
            end if
        end if

        if (.not. allocated(rmail_ignored_headers)) then
            rmail_ignored_headers = "^via:\|^mail-from:\|^origin:\|^references:\|^sender:"// &
                                    "\|^status:\|^received:\|^x400-originator:\|^x400-recipients:"// &
                                    "\|^x400-received:\|^x400-mts-identifier:\|^x400-content-type:"// &
                                    "\|^\(resent-\|\)message-id:\|^summary-line:\|^resent-date:"// &
                                    "\|^nntp-posting-host:\|^path:\|^x-char.*:\|^x-face:\|^face:"// &
                                    "\|^x-mailer:\|^delivered-to:\|^lines:"// &
                                    "\|^content-transfer-encoding:\|^x-coding-system:"// &
                                    "\|^return-path:\|^errors-to:\|^return-receipt-to:"// &
                                    "\|^precedence:\|^mime-version:"// &
                                    "\|^list-owner:\|^list-help:\|^list-post:\|^list-subscribe:"// &
                                    "\|^list-id:\|^list-unsubscribe:\|^list-archive:"// &
                                    "\|^content-length:\|^nntp-posting-date:\|^user-agent"// &
                                    "\|^importance:\|^envelope-to:\|^delivery-date\|^openpgp:"// &
                                    "\|^mbox-line:\|^cancel-lock:"// &
                                    "\|^DomainKey-Signature:\|^dkim-signature:"// &
                                    "\|^ARC-.*:"// &
                                    "\|^Received-SPF:"// &
                                    "\|^Authentication-Results:"// &
                                    "\|^resent-face:\|^resent-x.*:\|^resent-organization:\|^resent-openpgp:"// &
                                    "\|^x-.*:"
        end if

        if (.not. allocated(rmail_nonignored_headers)) rmail_nonignored_headers = "^x-spam-status:"

        if (.not. allocated(rmail_retry_ignored_headers)) then
            rmail_retry_ignored_headers = "^x-authentication-warning:\|^x-detected-operating-system:\|"// &
                                          "^x-spam[-a-z]*:\|^arc-.*:\|"// &
                                          "^content-type:\|^content-transfer-encoding:\|"// &
                                          "^mime-version:\|^message-id:\|^x-google-smtp-source:\|"// &
                                          "^x-received:\|^received-spf:\|"// &
                                          "^authentication-results:\|^dkim-signature:"
        end if

        if (.not. allocated(rmail_highlighted_headers)) rmail_highlighted_headers = "^From:\|^Subject:"
        if (.not. allocated(rmail_secondary_file_directory)) rmail_secondary_file_directory = "~/"
        if (.not. allocated(rmail_secondary_file_regular_expression)) rmail_secondary_file_regular_expression = "\.xmail\'"
        if (.not. allocated(rmail_reply_prefix)) rmail_reply_prefix = "Re: "
        if (.not. allocated(rmail_default_file)) rmail_default_file = "~/xmail"
        if (.not. allocated(rmail_default_body_file)) rmail_default_body_file = "~/mailout"

        if (.not. allocated(rmail_mime_character_set_pattern)) then
            rmail_mime_character_set_pattern = "^content-type:[ "//TAB//"]*text/plain;"// &
                                               "\(?:[ "//TAB//LF//"]*\(?:format\|delsp\)=""?[-a-z0-9]+""?;\)*"// &
                                               "[ "//TAB//LF//"]*charset=""?\([^ "//TAB//LF//""";]+\)""?"
        end if

        time_zone_regular_expression = "\([A-Z]?[A-Z]?[A-Z][A-Z]\( DST\)?"// &
                                       "\|[-+]?[0-9][0-9][0-9][0-9]"// &
                                       "\|"// &
                                       "\) *"
        rmail_unix_mail_delimiter = "From "// &
                                    ! The leading phrase of the sender, then anything goes.
                                    "\([^"//NUL//"-"//BS//LF//"-"//CR//DEL//"].*\)? "// &
                                    ! The time the message was sent: day of the week, month,
                                    ! day of month, time of day.
                                    "\([^"//NUL//"-"//CR//" "//DEL//"]+\) +"// &
                                    "\([^"//NUL//"-"//CR//" "//DEL//"]+\) +"// &
                                    "\([0-3]?[0-9]\) +"// &
                                    "\([0-2][0-9]:[0-5][0-9]\(:[0-6][0-9]\)?\) *"// &
                                    ! Perhaps a time zone, specified by an abbreviation, or by a
                                    ! numeric offset.
                                    time_zone_regular_expression// &
                                    ! The year.
                                    " \([0-9][0-9]+\) *"// &
                                    ! On some systems the time zone can appear after the year, too.
                                    time_zone_regular_expression// &
                                    ! Old uucp cruft.
                                    "\(remote from .*\)?"// &
                                    LF
    end subroutine set_default_values

    !> Define Rmail mode, its keymap and commands.  Not a port: the
    !> top-level forms of rmail.el.
    subroutine initialize_rmail()
        type(status_type) :: st
        type(handle_type) :: map
        character(len=1) :: digit
        integer :: d

        call set_default_values()
        if (len_trim(rmail_highlight%name) == 0) call define_faces()
        if (.not. associated(major_modes%text_mode%execute)) call initialize_text_mode()
        call major_modes%rmail_mode%define("rmail_mode", "RMAIL", rmail_mode, rmail_mode_hook)
        ! Rmail mode is suitable only for specially formatted data.
        major_modes%rmail_mode%special = .true.
        rmail_mode_map = major_modes%rmail_mode%map
        map = rmail_mode_map
        ! (suppress-keymap map)
        call keymap_set(map, "<remap> <self_insert_command>", "undefined", st)
        call keymap_set(map, "-", "negative_argument", st)

        do d = 0, 9
            write (digit, "(i1)") d
            call keymap_set(map, digit, "digit_argument", st)
        end do

        call keymap_set(map, "a", "add_rmail_label", st)
        call keymap_set(map, "b", "bury_rmail", st)
        call keymap_set(map, "d", "delete_rmail_message_forward", st)
        call keymap_set(map, "C-d", "delete_rmail_message_backward", st)
        call keymap_set(map, "e", "edit_rmail_current_message", st)
        call keymap_set(map, "g", "get_rmail_new_mail", st)
        call keymap_set(map, "h", "rmail_summary", st)
        call keymap_set(map, "i", "rmail_input", st)
        call keymap_set(map, "j", "show_rmail_message", st)
        call keymap_set(map, "k", "remove_rmail_label", st)
        call keymap_set(map, "l", "rmail_summary_by_labels", st)
        call keymap_set(map, "C-M-h", "rmail_summary", st)
        call keymap_set(map, "C-M-l", "rmail_summary_by_labels", st)
        call keymap_set(map, "C-M-r", "rmail_summary_by_recipients", st)
        call keymap_set(map, "C-M-f", "rmail_summary_by_senders", st)
        call keymap_set(map, "n", "next_rmail_undeleted_message", st)
        call keymap_set(map, "M-n", "next_rmail_message", st)
        call keymap_set(map, "C-M-n", "next_rmail_labeled_message", st)
        call keymap_set(map, "o", "output_rmail_message", st)
        call keymap_set(map, "C-o", "output_rmail_message_as_seen", st)
        call keymap_set(map, "p", "previous_rmail_undeleted_message", st)
        call keymap_set(map, "M-p", "previous_rmail_message", st)
        call keymap_set(map, "C-M-p", "previous_rmail_labeled_message", st)
        call keymap_set(map, "q", "quit_rmail", st)
        call keymap_set(map, "s", "expunge_and_save_rmail", st)
        call keymap_set(map, "M-s", "search_rmail", st)
        call keymap_set(map, "t", "toggle_rmail_header", st)
        call keymap_set(map, "u", "undelete_previous_rmail_message", st)
        call keymap_set(map, "w", "output_rmail_message_body_to_file", st)
        call keymap_set(map, "C-c C-w", "widen_rmail", st)
        call keymap_set(map, "x", "expunge_rmail", st)
        call keymap_set(map, ".", "beginning_of_rmail_message", st)
        call keymap_set(map, "/", "end_of_rmail_message", st)
        call keymap_set(map, "<", "show_first_rmail_message", st)
        call keymap_set(map, ">", "show_last_rmail_message", st)
        call keymap_set(map, "SPC", "scroll_up_command", st)
        call keymap_set(map, "S-SPC", "scroll_down_command", st)
        call keymap_set(map, "DEL", "scroll_down_command", st)
        call keymap_set(map, "?", "describe_mode", st)
        commands%rmail%execute => execute_rmail
        commands%rmail_mode%execute => rmail_mode
        commands%rmail_input%execute => execute_rmail_input
        commands%get_rmail_new_mail%execute => execute_get_rmail_new_mail
        commands%show_rmail_message%execute => execute_show_rmail_message
        commands%next_rmail_message%execute => execute_next_rmail_message
        commands%previous_rmail_message%execute => execute_previous_rmail_message
        commands%next_rmail_undeleted_message%execute => execute_next_rmail_undeleted_message
        commands%previous_rmail_undeleted_message%execute => execute_previous_rmail_undeleted_message
        commands%show_first_rmail_message%execute => show_first_rmail_message
        commands%show_last_rmail_message%execute => show_last_rmail_message
        commands%search_rmail%execute => execute_search_rmail
        commands%search_rmail_backwards%execute => execute_search_rmail_backwards
        commands%delete_rmail_message%execute => delete_rmail_message
        commands%delete_rmail_message_forward%execute => execute_delete_rmail_message_forward
        commands%delete_rmail_message_backward%execute => execute_delete_rmail_message_backward
        commands%undelete_previous_rmail_message%execute => execute_undelete_previous_rmail_message
        commands%expunge_rmail_only%execute => execute_expunge_rmail_only
        commands%expunge_rmail%execute => execute_expunge_rmail
        commands%expunge_and_save_rmail%execute => expunge_and_save_rmail
        commands%quit_rmail%execute => quit_rmail
        commands%bury_rmail%execute => bury_rmail
        commands%toggle_rmail_header%execute => execute_toggle_rmail_header
        commands%beginning_of_rmail_message%execute => beginning_of_rmail_message
        commands%end_of_rmail_message%execute => end_of_rmail_message
        commands%widen_rmail%execute => widen_rmail
        ! FIXME: Don't do it globally!!
        call after_save_hook%prepend(update_rmail_after_save)
    end subroutine initialize_rmail
end module rmail_mod
