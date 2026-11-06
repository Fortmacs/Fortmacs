! rmail_summary_mod.f90 -- the summary of an Rmail file: one line per message.
! Ported from GNU Emacs 31.1 lisp/mail/rmailsum.el (in part: see
! docs/status/rmail_summary_mod.md).
!
! Copyright (C) 1985, 1993-1996, 2000-2026 Free Software Foundation,
! Inc.
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

!> The summary of an Rmail file: \[rmail_summary] (h) shows a line for
!> each message (its number, whether it is deleted or unseen, the date,
!> the sender, the number of lines, the labels and the subject) in
!> Rmail Summary mode, whose commands act on the message of the line;
!> \[rmail_summary_by_labels], \[rmail_summary_by_recipients] and
!> \[rmail_summary_by_senders] show only some messages.  Moving in the
!> summary shows the message of the line in the Rmail buffer's window.
!> See docs/status/rmail_summary_mod.md for what is deferred.
module rmail_summary_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, prefix_argument_type, PREFIX_NONE, &
                                         PREFIX_NUMBER, PREFIX_MINUS
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use hooks_mod, only: hook_type
    use string_type_mod, only: string_type, append_string
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_name, close_buffer, &
                          get_or_make_buffer, generate_new_buffer_name, set_buffer_modified, buffer_serial_live, erase_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_end_of_buffer, &
                                     is_beginning_of_buffer, buffer_substring, &
                                     buffer_substring_no_properties, delete_region, narrow_to_region, widen, &
                                     line_end_position, line_beginning_position, save_restriction_begin, &
                                     save_restriction_end, save_excursion_begin, save_excursion_end, restriction_type, &
                                     excursion_type, user_login_name, character_after
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, end_of_line, beginning_of_line
    use syntax_mod, only: skip_characters_forward, skip_characters_backward
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, &
                          looking_at, match_beginning, match_end, &
                          string_match, regular_expression_quote, case_fold_search, replace_match, SEARCH_NOERROR, SEARCH_MOVE
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use keymap_mod, only: keymap_set, use_local_map, make_sparse_keymap, set_keymap_parent
    use modes_mod, only: major_modes
    use keyboard_mod, only: post_command_hook
    use display_engine_mod, only: message
    use window_mod, only: get_buffer_window, select_window, split_window, next_window, frame_first_window, &
                          MINIBUF_ACTIVE, MINIBUF_NO, &
                          delete_other_windows, delete_window, window_buffer, set_window_buffer, other_buffer, &
                          is_position_visible_in_window, scroll_other_window, scroll_other_window_down, recenter, &
                          window_body_height, switch_to_buffer, bury_buffer, pop_up_windows
    use frame_mod, only: selected_window
    use minibuffer_mod, only: read_file_name
    use minibuffer_reading_mod, only: read_string
    use interval_tree_mod, only: make_overlay, overlay_put, move_overlay, overlay_get
    use value_mod, only: value_type
    use faces_mod, only: highlight
    use text_mode_mod, only: text_mode_map, text_mode_syntax_table
    use simple_mod, only: count_lines, beginning_of_buffer, end_of_buffer
    use files_mod, only: save_buffer
    use startup_mod, only: user_mail_address_value
    use incremental_search_mod, only: incremental_search_active
    use mail_utilities_mod, only: get_mail_field, strip_mail_quoted_names, mail_comma_list_regular_expression
    use rmail_mod, only: rmail_states, rmail_state_index, rmail_buffer, set_rmail_buffer, rmail_message_beginning, &
                         rmail_message_end, is_rmail_message_deleted, get_rmail_header, has_rmail_message_labels, &
                         apply_in_rmail_message_begin, apply_in_rmail_message_end, rmail_message_scope_type, &
                         set_rmail_bad_format_error, &
                         show_rmail_message, set_rmail_attribute, is_rmail_message_unseen, delete_rmail_message, &
                         undelete_previous_rmail_message, is_rmail_expunge_confirmed, expunge_rmail_only, &
                         expunge_and_save_rmail, get_rmail_new_mail, rmail, quit_rmail, search_rmail, toggle_rmail_header, &
                         select_rmail_summary, pop_to_rmail_buffer, rmail_summary_window_size, &
                         rmail_user_mail_address_regular_expression, rmail_last_multi_labels, RMAIL_ATTRIBUTE_HEADER, &
                         RMAIL_KEYWORD_HEADER, RMAIL_UNSEEN_ATTRIBUTE_INDEX, goto_rmail_summary_message_function, &
                         mark_rmail_summary_deleted_function, mark_rmail_summary_undeleted_function, &
                         update_rmail_summary_line_function, update_rmail_summary_function, rmail_summary_function, &
                         disable_rmail_summary_function, enable_rmail_summary_function, read_rmail_search
    use rmail_keywords_mod, only: append_rmail_label, read_rmail_label, add_rmail_label, set_rmail_label, &
                                  next_rmail_labeled_message, previous_rmail_labeled_message
    use rmail_edit_mod, only: edit_rmail_current_message, cease_rmail_edit, abort_rmail_edit
    use rmail_output_mod, only: output_rmail_message, output_rmail_message_as_seen, output_rmail_message_body_to_file, &
                                read_rmail_output_file_name
    implicit none(type, external)
    private

    public :: rmail_summary, rmail_summary_by_labels, rmail_summary_by_recipients, rmail_summary_by_senders, &
              update_rmail_summary, goto_rmail_summary_message, mark_rmail_summary_deleted, mark_rmail_summary_undeleted, &
              update_rmail_summary_line, rmail_summary_mode, strip_rmail_summary_recipient_quoted_names, &
              strip_rmail_summary_sender, summarize_rmail_header, initialize_rmail_summary

    !> The kinds of summary `rmail_summary_redo' remakes: of all messages,
    !> by labels, by recipients, by senders.  Not a port.
    integer, parameter :: SUMMARY_ALL = 1, SUMMARY_LABELS = 2, SUMMARY_RECIPIENTS = 3, SUMMARY_SENDERS = 4

    abstract interface
        !> A function formatting the contents of a header field for a
        !> summary line (`rmail_summary_sender_function',
        !> `rmail_summary_recipient_function'): TEXT is the field's raw
        !> contents, SHOWN what to show.  Not a port.
        subroutine rmail_summary_field_procedure(text, shown)
            implicit none(type, external)
            character(len=*), intent(in) :: text
            character(len=:), allocatable, intent(out) :: shown
        end subroutine rmail_summary_field_procedure
    end interface

    !> True means Rmail summary scroll commands move between messages.
    !> That is, after `scroll_rmail_summary_message_up' reaches the end of a
    !> message, it moves to the next message; and similarly for
    !> `scroll_rmail_summary_message_down'.  Port of rmailsum.el
    !> rmail-summary-scroll-between-messages.
    logical, public :: rmail_summary_scroll_between_messages = .true.

    !> True means Rmail summary should show the number of lines in each message.
    !> Setting this option to .false. might speed up the generation of summaries.
    !> Port of rmailsum.el rmail-summary-line-count-flag.
    logical, public :: rmail_summary_line_count_flag = .true.

    !> Width of the sender/recipient field in Rmail summary lines.  Port of
    !> rmailsum.el rmail-summary-address-width.
    integer, public :: rmail_summary_address_width = 25

    !> Function used to format the sender field in Rmail summary lines.
    !> The function is called with the raw contents of the From: field as a
    !> string, and should return a string.  Port of rmailsum.el
    !> rmail-summary-sender-function (its rmail-summary-name-or-address
    !> waits for mail-parse.el).
    procedure(rmail_summary_field_procedure), pointer, public :: rmail_summary_sender_function => null()

    !> Function used to format the recipient field in Rmail summary lines.
    !> The function is called with the raw contents of the To: field as a
    !> string, and should return a string.  When extracting names, fall back to
    !> email addresses if no name can be extracted.  Port of rmailsum.el
    !> rmail-summary-recipient-function (its rmail-summary-name-or-address
    !> and rmail-summary-recipient-names wait for mail-parse.el).
    procedure(rmail_summary_field_procedure), pointer, public :: rmail_summary_recipient_function => null()

    !> True means progressively narrow the set of messages produced by summary.
    !> This enables you to apply the summary criteria on top one another,
    !> thus progressively narrowing the selection of the messages produced
    !> by each summary criteria.
    !> For example, applying `rmail_summary_by_senders' on top
    !> of `rmail_summary_by_topic' produces a summary of messages
    !> with the specified Subjects that were sent from specified
    !> sending addresses.
    !> This way, the user can apply one summary on top of another,
    !> and keep narrowing the resulting list of messages.  Port of
    !> rmailsum.el rmail-summary-progressively-narrow.
    logical, public :: rmail_summary_progressively_narrow = .false.

    !> Message number to start summarizing at.  Port of rmailsum.el
    !> rmail-summary-starting-message.
    integer(int64), public :: rmail_summary_starting_message = 1

    !> Function to decode a Rmail summary line, which it receives as TEXT
    !> and gives back decoded; when null, lines are shown undecoded.
    !> Port of rmailsum.el rmail-summary-line-decoder (its default,
    !> rfc2047-decode-string, waits for rfc2047.el: until then it is null).
    procedure(rmail_summary_field_procedure), pointer, public :: rmail_summary_line_decoder => null()

    !> Run after Rmail Summary mode is set up.  Port of the hook
    !> define-derived-mode makes for rmail-summary-mode.
    type(hook_type), target, public :: rmail_summary_mode_hook = hook_type(name="rmail_summary_mode_hook")

    !> The keymap of Rmail Summary mode.  Port of rmailsum.el
    !> rmail-summary-mode-map.
    type(handle_type), public :: rmail_summary_mode_map

    !> The keymap of a summary buffer while a message is edited.  Port of
    !> rmailsum.el rmail-summary-edit-map.
    type(handle_type), public :: rmail_summary_edit_map

    ! What a summary buffer keeps (its buffer-local variables upstream):
    ! how to remake it (rmail-summary-redo), the messages it shows
    ! (rmail-summary-currently-displayed-msgs), its number of lines
    ! (its rmail-total-messages) and the highlighting overlay.
    type :: summary_state_type
        integer(int64) :: serial = 0
        integer :: redo_kind = 0
        character(len=:), allocatable :: redo_text
        logical :: redo_primary_only = .false.
        logical, allocatable :: displayed(:)
        integer(int64) :: total_messages = 0
        type(handle_type) :: overlay
    end type summary_state_type

    type(summary_state_type), allocatable :: summary_states(:)

    ! The position of the line whose `unseen' attribute an incremental
    ! search removed, to restore (rmail-summary-put-back-unseen; 0: none).
    integer(int64) :: put_back_unseen = 0

    ! The number of summary lines made so far (rmail-new-summary-line-count).
    integer(int64) :: new_summary_line_count = 0

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    ! The index of the current buffer's summary state, made when MAKE (0
    ! when there is none).  Not a port.
    integer function summary_state_index(make) result(k)
        logical, intent(in) :: make
        type(summary_state_type) :: entry

        k = 0
        if (current == 0) return
        if (.not. allocated(summary_states)) allocate (summary_states(0))
        k = findloc(summary_states%serial, buffers(current)%serial, dim=1)
        if (k > 0 .or. .not. make) return

        do k = 1, size(summary_states)
            if (.not. buffer_serial_live(summary_states(k)%serial)) exit
        end do

        if (k > size(summary_states)) summary_states = [summary_states, entry]
        summary_states(k) = entry
        summary_states(k)%serial = buffers(current)%serial
    end function summary_state_index

    ! The index of the state of the current buffer's Rmail buffer (0 if
    ! none).  Not a port.
    integer function rmail_index() result(k)
        k = rmail_state_index(rmail_buffer())
    end function rmail_index

    ! ------------------------------------------------------------------
    ! Formatting fields
    ! ------------------------------------------------------------------

    !> Strip quoted names from TEXT, a From field (`strip_mail_quoted_names'):
    !> the default of `rmail_summary_sender_function'.  Not a port.
    subroutine strip_rmail_summary_sender(text, shown)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: shown

        shown = strip_mail_quoted_names(text)
    end subroutine strip_rmail_summary_sender

    !> Strip quoted names from the first line of RECIPIENT field.
    !> Applies `strip_mail_quoted_names' to the first physical line of the
    !> header field value.  Port of
    !> rmailsum.el:rmail-summary-recipient-strip-quoted-names.
    subroutine strip_rmail_summary_recipient_quoted_names(recipient, shown)
        character(len=*), intent(in) :: recipient
        character(len=:), allocatable, intent(out) :: shown
        character(len=:), allocatable :: first
        integer :: k

        first = recipient
        k = index(first, LF)
        if (k > 0) first = first(1:k - 1)
        k = verify(first, " "//TAB, back=.true.)
        shown = strip_mail_quoted_names(first(1:k))
    end subroutine strip_rmail_summary_recipient_quoted_names

    ! ------------------------------------------------------------------
    ! Making summaries
    ! ------------------------------------------------------------------

    !> Remake the current summary as it was made.  Port of
    !> rmailsum.el:rmail-update-summary.
    recursive subroutine update_rmail_summary(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        if (n < 0) return
        k = summary_state_index(.false.)
        if (k == 0) return

        select case (summary_states(k)%redo_kind)
        case (SUMMARY_ALL)
            call rmail_summary(status)
        case (SUMMARY_LABELS)
            call rmail_summary_by_labels(summary_states(k)%redo_text, status)
        case (SUMMARY_RECIPIENTS)
            call rmail_summary_by_recipients(summary_states(k)%redo_text, status, summary_states(k)%redo_primary_only)
        case (SUMMARY_SENDERS)
            call rmail_summary_by_senders(summary_states(k)%redo_text, status)
        case default
            continue
        end select
    end subroutine update_rmail_summary

    ! `update_rmail_summary' as the revert function of summary buffers.
    ! Not a port.
    recursive subroutine revert_summary(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status

        status = status_type()
        if (ignore_auto .and. noconfirm) continue
        call update_rmail_summary(0_int64, status)
    end subroutine revert_summary

    ! Set the array of the messages the summary shows from its lines.
    ! Port of rmailsum.el:rmail-summary-populate-displayed-messages.
    subroutine populate_is_rmail_summary_displayed_messages()
        type(handle_type) :: saved, summary
        type(status_type) :: st
        integer(int64) :: total, number, moved
        integer :: k, s

        k = rmail_index()
        if (k == 0) return
        total = rmail_states(k)%total_messages
        summary = rmail_states(k)%summary_buffer
        saved = current_buffer()
        call set_buffer(summary, st)
        s = summary_state_index(.true.)
        if (allocated(summary_states(s)%displayed)) deallocate (summary_states(s)%displayed)
        allocate (summary_states(s)%displayed(0:total))
        summary_states(s)%displayed = .false.
        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            number = line_number_at_point()
            if (number >= 0 .and. number <= total) summary_states(s)%displayed(number) = .true.
            moved = forward_line(1_int64)
        end do

        call set_buffer(saved, st)
    end subroutine populate_is_rmail_summary_displayed_messages

    ! The number at the start of the line at point (string-to-number of
    ! the line: 0 if none).  Not a port.
    integer(int64) function line_number_at_point() result(number)
        character(len=:), allocatable :: line
        type(status_type) :: st
        integer :: i, j, ios

        number = 0
        call buffer_substring_no_properties(line_beginning_position(1_int64), line_end_position(1_int64), line, st)
        i = verify(line, " "//TAB)
        if (i == 0) return
        j = i
        if (line(j:j) == "-" .or. line(j:j) == "+") j = j + 1

        do while (j <= len(line))
            if (scan(line(j:j), "0123456789") == 0) exit
            j = j + 1
        end do

        if (j == i) return
        read (line(i:j - 1), *, iostat=ios) number
        if (ios /= 0) number = 0
    end function line_number_at_point

    ! Whether the summary shows message MESSAGE (when it progressively
    ! narrows).  Not a port.
    logical function is_displayed(message)
        integer(int64), intent(in) :: message
        type(handle_type) :: saved, summary
        type(status_type) :: st
        integer :: k, s

        is_displayed = .false.
        k = rmail_index()
        if (k == 0) return
        summary = rmail_states(k)%summary_buffer
        if (.not. is_buffer_live(summary)) return
        saved = current_buffer()
        call set_buffer(summary, st)
        s = summary_state_index(.false.)

        if (s > 0) then
            if (allocated(summary_states(s)%displayed)) then
                if (message >= 0 .and. message <= ubound(summary_states(s)%displayed, 1)) then
                    is_displayed = summary_states(s)%displayed(message)
                end if
            end if
        end if

        call set_buffer(saved, st)
    end function is_displayed

    ! Whether the summary exists, from the Rmail or the summary buffer.
    ! Port of rmailsum.el:rmail-summary--exists-1.
    logical function summary_exists_1()
        integer :: k

        summary_exists_1 = .false.
        k = rmail_index()
        if (k > 0) summary_exists_1 = is_buffer_live(rmail_states(k)%summary_buffer)
    end function summary_exists_1

    !> Display a summary of all messages, one line per message.  Port of
    !> rmailsum.el:rmail-summary.
    recursive subroutine rmail_summary(status)
        type(status_type), intent(out) :: status

        call make_rmail_new_summary("All", SUMMARY_ALL, "", .false., status)
    end subroutine rmail_summary

    ! `rmail_summary' as `rmail_summary_function'.  Not a port.
    recursive subroutine make_rmail_summary_for_rmail(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        status = status_type()
        if (n < 0) return
        call rmail_summary(status)
    end subroutine make_rmail_summary_for_rmail

    !> Display a summary of all messages with one or more LABELS.
    !> LABELS should be a string containing the desired labels, separated by commas.
    !> Port of rmailsum.el:rmail-summary-by-labels.
    recursive subroutine rmail_summary_by_labels(labels, status)
        character(len=*), intent(in) :: labels
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: wanted

        wanted = labels

        if (len(wanted) == 0) then
            if (.not. allocated(rmail_last_multi_labels)) then
                call set_error(status, GENERIC_ERROR_CODE, "No label specified")
                return
            end if

            wanted = rmail_last_multi_labels
        end if

        rmail_last_multi_labels = wanted
        if (rmail_summary_progressively_narrow .and. summary_exists_1()) call populate_is_rmail_summary_displayed_messages()
        call make_rmail_new_summary("labels "//wanted, SUMMARY_LABELS, wanted, .false., status)
    end subroutine rmail_summary_by_labels

    !> Display a summary of all messages with the given RECIPIENTS.
    !> Normally checks the To, From and Cc fields of headers;
    !> but if PRIMARY_ONLY is true (prefix arg given),
    !>  only look in the To and From fields.
    !> RECIPIENTS is a regular expression.  Port of
    !> rmailsum.el:rmail-summary-by-recipients.
    recursive subroutine rmail_summary_by_recipients(recipients, status, primary_only)
        character(len=*), intent(in) :: recipients
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: primary_only
        logical :: primary

        primary = .false.
        if (present(primary_only)) primary = primary_only
        if (rmail_summary_progressively_narrow .and. summary_exists_1()) call populate_is_rmail_summary_displayed_messages()
        call make_rmail_new_summary("recipients "//recipients, SUMMARY_RECIPIENTS, recipients, primary, status)
    end subroutine rmail_summary_by_recipients

    !> Display a summary of all messages whose "From" field matches SENDERS.
    !> SENDERS is a regular expression.  The default for SENDERS matches the
    !> sender of the current message.  Port of
    !> rmailsum.el:rmail-summary-by-senders.
    recursive subroutine rmail_summary_by_senders(senders, status)
        character(len=*), intent(in) :: senders
        type(status_type), intent(out) :: status

        if (rmail_summary_progressively_narrow .and. summary_exists_1()) call populate_is_rmail_summary_displayed_messages()
        call make_rmail_new_summary("senders "//senders, SUMMARY_SENDERS, senders, .false., status)
    end subroutine rmail_summary_by_senders

    ! Whether message MESSAGE goes in a summary of kind KIND with TEXT and
    ! PRIMARY_ONLY.  Port of rmailsum.el:rmail-message-labels-p's use,
    ! rmailsum.el:rmail-message-recipients-p (with
    ! rmailsum.el:rmail-message-recipients-p-1) and
    ! rmailsum.el:rmail-message-senders-p, and of the progressive
    ! narrowing lambdas.
    logical function is_summarized(message, kind, text, primary_only) result(included)
        integer(int64), intent(in) :: message
        integer, intent(in) :: kind
        character(len=*), intent(in) :: text
        logical, intent(in) :: primary_only
        type(rmail_message_scope_type) :: scope
        character(len=:), allocatable :: value
        type(status_type) :: st
        integer(int64) :: found

        included = .true.
        if (kind == SUMMARY_ALL) return

        if (rmail_summary_progressively_narrow .and. summary_exists_1()) then
            included = is_displayed(message)
            if (.not. included) return
        end if

        select case (kind)
        case (SUMMARY_LABELS)
            included = has_rmail_message_labels(message, " \("//mail_comma_list_regular_expression(text)//"\)\(,\|\'\)")
        case (SUMMARY_RECIPIENTS)
            included = .false.
            call apply_in_rmail_message_begin(scope, st, message)

            if (scope%active) then
                ! mail-fetch-field does not care where it starts from.
                call search_forward(LF//LF, found, st, noerror=SEARCH_NOERROR)
                call narrow_to_region(point_minimum(), point(), st)
                included = matches("To") .or. matches("From")
                if (.not. included .and. .not. primary_only) included = matches("Cc")
            end if

            call apply_in_rmail_message_end(scope)
        case (SUMMARY_SENDERS)
            call get_rmail_header("From", value, st, message)
            if (.not. allocated(value)) value = ""
            included = string_match(text, value, st) > 0
        case default
            continue
        end select

    contains

        ! Whether TEXT matches the header field NAME ("" when absent).
        logical function matches(name)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: field
            type(status_type) :: st2

            call get_mail_field(name, field, st2)
            if (.not. allocated(field)) field = ""
            matches = string_match(text, field, st2) > 0
        end function matches
    end function is_summarized

    ! Create a summary of selected messages.
    ! DESCRIPTION makes part of the mode line of the summary buffer.
    ! KIND, TEXT and PRIMARY_ONLY are what to put in the summary's redo
    ! information and which messages it includes (all for SUMMARY_ALL).
    ! Port of rmailsum.el:rmail-new-summary (the I/O menu waits for the
    ! menu bar).
    recursive subroutine make_rmail_new_summary(description, kind, text, primary_only, status)
        character(len=*), intent(in) :: description, text
        integer, intent(in) :: kind
        logical, intent(in) :: primary_only
        type(status_type), intent(out) :: status
        type(handle_type) :: summary, window, saved
        type(status_type) :: st
        integer(int64) :: mesg
        logical :: was_in_summary, found
        integer :: k

        call message("Computing summary lines...")

        if (.not. is_buffer_live(rmail_buffer())) then
            call set_error(status, GENERIC_ERROR_CODE, "No RMAIL buffer found")
            return
        end if

        was_in_summary = buffers(current)%major_mode == "rmail_summary_mode"
        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        k = rmail_state_index(rmail_buffer())
        call make_rmail_new_summary_1(description, kind, text, primary_only, summary, status)
        if (.not. status%ok()) return
        rmail_states(k)%summary_buffer = summary
        mesg = rmail_states(k)%current_message

        ! Now display the summary buffer and go to the right place in it.
        if (.not. was_in_summary) then
            if (is_one_window() .and. pop_up_windows) then
                ! If there is just one window, put the summary on the top.
                call split_window(selected_window(), rmail_summary_window_size, rmail_summary_window_size > 0, &
                                  .false., window, status)
                if (.not. status%ok()) return
                call select_window(next_window(frame_first_window(), MINIBUF_ACTIVE, st), status)
                if (.not. status%ok()) return
                call pop_to_rmail_buffer(summary, status)
                if (.not. status%ok()) return
                ! If pop-to-buffer did not use that window, delete that
                ! window.
                window = window_buffer(frame_first_window(), st)
                if (window%index /= summary%index) call delete_other_windows(selected_window(), status)
            else
                call pop_to_rmail_buffer(summary, status)
            end if

            if (.not. status%ok()) return
            call set_buffer(rmail_buffer(), status)
            ! This is how rmail makes the summary buffer reappear.
            call select_rmail_summary(n=0_int64, status=status)
            if (.not. status%ok()) return
            call set_buffer(summary, status)
            if (.not. status%ok()) return
        else
            call set_buffer(saved, status)
            if (.not. status%ok()) return
        end if

        call goto_rmail_summary_message(status, mesg, .true., .true., found)
        if (.not. status%ok()) return
        call message("Computing summary lines...done")
    end subroutine make_rmail_new_summary

    ! Whether the selected window is the only one of the frame (but the
    ! minibuffer).  Port of window.el:one-window-p (NOMINI absent).
    logical function is_one_window()
        type(status_type) :: st
        type(handle_type) :: next, this

        this = selected_window()
        next = next_window(this, MINIBUF_ACTIVE, st)
        is_one_window = next%index == this%index
    end function is_one_window

    ! Filter messages to obtain summary lines; SUMMARY is the summary
    ! buffer, made by invoking the filter of KIND on each message.  The
    ! current buffer must be a Rmail buffer either containing a collection
    ! of mbox formatted messages or displaying a single message.  Port of
    ! rmailsum.el:rmail-new-summary-1 (the description in the mode line
    ! waits for buffer-local minor-mode-alist entries).
    recursive subroutine make_rmail_new_summary_1(description, kind, text, primary_only, summary, status)
        character(len=*), intent(in) :: description, text
        integer, intent(in) :: kind
        logical, intent(in) :: primary_only
        type(handle_type), intent(out) :: summary
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: lines(:)
        type(handle_type) :: main_buffer
        character(len=:), allocatable :: line
        character(len=60) :: progress
        integer(int64) :: msgnum, total, count
        logical :: saved_read_only
        integer :: k, s, i

        new_summary_line_count = 0
        call get_or_make_rmail_summary_buffer(summary, status)
        if (.not. status%ok()) return
        main_buffer = current_buffer()
        k = rmail_state_index(main_buffer)
        msgnum = rmail_summary_starting_message
        total = rmail_states(k)%total_messages
        allocate (lines(0))

        ! Scan the messages, getting their summary strings and putting the
        ! list of them in LINES.
        do while (total >= msgnum)
            ! First test whether to include this message.
            if (is_summarized(msgnum, kind, text, primary_only)) then
                call get_rmail_summary(msgnum, line, status)
                if (.not. status%ok()) return
                call append_string(lines, line)
            end if

            msgnum = msgnum + 1

            ! Provide a periodic User progress message.
            if (new_summary_line_count /= 0 .and. mod(new_summary_line_count, 10_int64) == 0) then
                write (progress, '("Computing summary lines...", i0)') new_summary_line_count
                call message(trim(progress))
            end if
        end do

        ! Temporarily, while summary buffer is unfinished, we "don't have"
        ! a summary.
        rmail_states(k)%summary_buffer = null_handle()
        call set_buffer(summary, status)
        if (.not. status%ok()) return
        ! Set up the summary buffer's contents.
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call erase_buffer(status)
        count = 0

        do i = 1, size(lines)
            if (status%ok()) call insert(lines(i)%text, status)
            count = count + 1
        end do

        if (.not. status%ok()) return
        call goto_character(point_minimum())
        ! Set up the rest of its state and local variables.
        buffers(current)%read_only = .true.
        call rmail_summary_mode(status)
        if (.not. status%ok()) return
        call set_rmail_buffer(main_buffer)
        s = summary_state_index(.true.)
        summary_states(s)%redo_kind = kind
        summary_states(s)%redo_text = text
        summary_states(s)%redo_primary_only = primary_only
        summary_states(s)%total_messages = count
        if (len(description) == 0) continue
        call set_buffer(main_buffer, status)
        if (saved_read_only) continue
    end subroutine make_rmail_new_summary_1

    ! The Rmail summary buffer, made if necessary with undo disabled.
    ! Port of rmailsum.el:rmail-get-create-summary-buffer.
    subroutine get_or_make_rmail_summary_buffer(summary, status)
        type(handle_type), intent(out) :: summary
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        integer :: k

        status = status_type()
        k = current_state_here()

        if (is_buffer_live(rmail_states(k)%summary_buffer)) then
            summary = rmail_states(k)%summary_buffer
            return
        end if

        call buffer_name(current_buffer(), name, status)
        if (.not. status%ok()) return
        call get_or_make_buffer(generate_new_buffer_name(name//"-summary"), summary, status)
        if (.not. status%ok()) return
        buffers(summary%index)%undo_enabled = .false.
    end subroutine get_or_make_rmail_summary_buffer

    ! The state of the current buffer as an Rmail buffer.  Not a port.
    integer function current_state_here() result(k)
        k = rmail_state_index(current_buffer())
    end function current_state_here

    ! The summary line LINE for message MSGNUM, made and cached if it has
    ! none yet.  The mbox buffer must be current when you call this
    ! function even if its text is swapped.  Port of
    ! rmailsum.el:rmail-get-summary.
    subroutine get_rmail_summary(msgnum, line, status)
        integer(int64), intent(in) :: msgnum
        character(len=:), allocatable, intent(out) :: line
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = rmail_state_index(rmail_buffer())

        if (allocated(rmail_states(k)%summaries(msgnum)%text)) then
            line = rmail_states(k)%summaries(msgnum)%text
            return
        end if

        ! Register a summary line for MSGNUM.
        new_summary_line_count = new_summary_line_count + 1
        call create_rmail_summary_line(msgnum, line, status)
        if (.not. status%ok()) return
        ! Cache the summary line for use during this Rmail session.
        rmail_states(k)%summaries(msgnum)%text = line
    end subroutine get_rmail_summary

    ! The summary line LINE of message MSGNUM.  The mbox buffer must be
    ! current when you call this function even if its text is swapped.
    ! Port of rmailsum.el:rmail-create-summary-line.
    subroutine create_rmail_summary_line(msgnum, line, status)
        integer(int64), intent(in) :: msgnum
        character(len=:), allocatable, intent(out) :: line
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(status_type) :: st
        integer(int64) :: beginning, end, lines, found
        logical :: deleted, unseen
        integer :: k

        k = rmail_state_index(rmail_buffer())
        beginning = rmail_message_beginning(msgnum)
        end = rmail_message_end(msgnum)
        deleted = is_rmail_message_deleted(msgnum)
        unseen = .false.
        lines = -1
        saved = current_buffer()
        ! Switch to the buffer that has the whole mbox text.
        if (rmail_states(k)%swapped) call set_buffer(rmail_states(k)%view_buffer, status)
        ! Now we can compute the line count.
        if (rmail_summary_line_count_flag) lines = count_lines(beginning, end)
        ! Narrow to the message header.
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call widen()
        call goto_character(beginning)
        call search_forward(LF//LF, found, status, bound=end, noerror=SEARCH_NOERROR)

        if (status%ok() .and. found > 0) then
            call narrow_to_region(beginning, point(), status)
            ! Replace rmail-message-unseen-p.
            call goto_character(beginning)
            call search_forward(RMAIL_ATTRIBUTE_HEADER//": ", found, st, noerror=SEARCH_NOERROR)
            if (found > 0) unseen = looking_at("......U", st)
            ! Generate a status line from the message.
            call create_rmail_summary(msgnum, deleted, unseen, lines, line, status)
        else if (status%ok()) then
            call set_rmail_bad_format_error(status, msgnum)
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
        call set_buffer(saved, st)
    end subroutine create_rmail_summary_line

    ! A string with the current message labels, in curly braces ("" when
    ! there are none).  The current buffer must already be narrowed to the
    ! message headers.  Port of rmailsum.el:rmail-get-summary-labels.
    function get_rmail_summary_labels() result(text)
        character(len=:), allocatable :: text
        character(len=:), allocatable :: labels, made
        type(status_type) :: st
        integer :: start, comma

        text = ""
        call get_mail_field(RMAIL_KEYWORD_HEADER, labels, st)
        if (.not. allocated(labels)) return
        if (len(labels) == 0) return
        ! Remember them so that read_rmail_label can offer completion.
        start = 1

        do
            comma = index(labels(start:), ", ")

            if (comma == 0) then
                if (start <= len(labels)) call append_rmail_label(labels(start:), made)
                exit
            end if

            if (comma > 1) call append_rmail_label(labels(start:start + comma - 2), made)
            start = start + comma + 1
        end do

        text = "{ "//labels//" } "
    end function get_rmail_summary_labels

    ! The summary line LINE of message MSGNUM.  The current buffer should
    ! already be narrowed to the header for that message.  DELETED is true
    ! if this message is marked deleted, UNSEEN if it is marked unseen,
    ! LINES is the number of lines in the message (or -1 not to show it).
    ! Port of rmailsum.el:rmail-create-summary.
    subroutine create_rmail_summary(msgnum, deleted, unseen, lines, line, status)
        integer(int64), intent(in) :: msgnum, lines
        logical, intent(in) :: deleted, unseen
        character(len=:), allocatable, intent(out) :: line
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: basic_start, basic_end, labels, decoded, count_text
        character(len=20) :: linecount, prefix
        character :: state

        call goto_character(point_minimum())
        call summarize_rmail_header(basic_start, basic_end, status)
        if (.not. status%ok()) return
        labels = get_rmail_summary_labels()

        if (lines < 0) then
            linecount = ""
        else if (lines <= 9) then
            write (linecount, '("   [", i0, "]")') lines
        else if (lines <= 99) then
            write (linecount, '("  [", i0, "]")') lines
        else if (lines <= 999) then
            write (linecount, '(" [", i0, "]")') lines
        else if (lines <= 9999) then
            write (linecount, '("  [", i0, "k]")') lines/1000
        else if (lines <= 99999) then
            write (linecount, '(" [", i0, "k]")') lines/1000
        else
            write (linecount, '("[", i0, "k]")') lines/1000
        end if

        count_text = trim(linecount)
        if (lines < 0) count_text = " "

        if (deleted) then
            state = "D"
        else if (unseen) then
            state = "-"
        else
            state = " "
        end if

        write (prefix, "(i0)") msgnum
        line = repeat(" ", max(0, 5 - len_trim(prefix)))//trim(prefix)//state//" "//basic_start//count_text//" "// &
               labels//basic_end

        if (associated(rmail_summary_line_decoder)) then
            call rmail_summary_line_decoder(line, decoded)
            line = decoded
        end if
    end subroutine create_rmail_summary

    !> The message summary BASIC_START, BASIC_END (the first and second
    !> parts of the summary) based on the message headers.  The current
    !> buffer must already be narrowed to the message headers for the
    !> message being processed.  Port of rmailsum.el:rmail-header-summary
    !> (decoding RFC 2047 words in the address waits for rfc2047.el).
    subroutine summarize_rmail_header(basic_start, basic_end, status)
        character(len=:), allocatable, intent(out) :: basic_start, basic_end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: date, from_raw, from, from_display, to, subject, more
        type(excursion_type) :: excursion
        type(status_type) :: st
        character(len=20) :: text
        integer(int64) :: found, beginning, position, moved
        integer :: width, after, before, length, mch, lo, newline, day
        logical :: saved_fold

        status = status_type()
        call goto_character(point_minimum())
        call save_excursion_begin(excursion)
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call regular_expression_search_forward("^Date:", found, st, noerror=SEARCH_NOERROR)

        if (found == 0) then
            date = "      "
        else
            ! Match month names case-insensitively.
            call regular_expression_search_forward("\([^0-9:]\)\([0-3]?[0-9]\)\([- "//TAB &
                                                   //"_]+\)\([adfjmnos][aceopu][bcglnprtvy]\)", &
                                                   found, st, bound=line_end_position(1_int64), noerror=SEARCH_NOERROR)

            if (found > 0) then
                day = int(string_number(match_text(2)))
                write (text, '(i2, "-", a3)') day, match_text(4)
                date = text(1:6)
            else
                call regular_expression_search_forward("\([^a-z]\)\([adfjmnos][acepou][bcglnprtvy]\)\([-a-z "//TAB &
                                                       //"_]*\)\([0-9][0-9]?\)", &
                                                       found, st, bound=line_end_position(1_int64), noerror=SEARCH_NOERROR)

                if (found > 0) then
                    day = int(string_number(match_text(4)))
                    write (text, '(i2, "-", a3)') day, match_text(2)
                    date = text(1:6)
                else
                    case_fold_search%value = saved_fold
                    call regular_expression_search_forward("\(19\|20\)\([0-9][0-9]\)-\([01][0-9]\)-\([0-3][0-9]\)", found, st, &
                                                           bound=line_end_position(1_int64), noerror=SEARCH_NOERROR)
                    case_fold_search%value = .true.

                    if (found > 0) then
                        date = right_justify(match_text(2), 2)//right_justify(match_text(3), 2)//right_justify(match_text(4), 2)
                    else
                        date = "??????"
                    end if
                end if
            end if
        end if

        call save_excursion_end(excursion)
        call save_excursion_begin(excursion)
        call regular_expression_search_forward("^From:[ "//TAB//"]*", found, st, noerror=SEARCH_NOERROR)

        if (found > 0) then
            beginning = point() - 1

            ! Get all the lines of the From field so that we get a whole
            ! comment if there is one, so that mail-strip-quoted-names can
            ! discard it.
            do
                moved = forward_line(1_int64)
                if (.not. looking_at("[ "//TAB//"]", st)) exit
            end do

            ! Back up over newline, then trailing spaces or tabs.
            call goto_character(point() - 1)
            moved = skip_characters_backward(" "//TAB, st)
            call buffer_substring(beginning, point(), from_raw, st)
            ! FROM is used for self-detection and should be independent of
            ! display customization via `rmail_summary_sender_function'.
            from = strip_mail_quoted_names(from_raw)

            if (associated(rmail_summary_sender_function)) then
                call rmail_summary_sender_function(from_raw, from_display)
            else
                call strip_rmail_summary_sender(from_raw, from_display)
            end if
        end if

        width = rmail_summary_address_width
        after = min(11, width)
        before = width - after

        ! If there are multiple lines in FROM or FROM-DISPLAY, discard up
        ! to the last newline in them.
        if (allocated(from)) then
            newline = index(from, LF, back=.true.)
            if (newline > 0) from = from(newline + 1:)
        end if

        if (allocated(from_display)) then
            newline = index(from_display, LF, back=.true.)
            if (newline > 0) from_display = from_display(newline + 1:)
        end if

        if (is_this_user(from)) then
            ! No From field, or it's this user.
            call goto_character(point_minimum())
            call regular_expression_search_forward("^To:[ "//TAB//"]*", found, st, noerror=SEARCH_NOERROR)

            if (found > 0) then
                call get_mail_field("To", to, st)
                if (.not. allocated(to)) to = ""

                if (associated(rmail_summary_recipient_function)) then
                    call rmail_summary_recipient_function(to, more)
                else
                    call strip_rmail_summary_recipient_quoted_names(to, more)
                end if

                from_display = "to: "//more
            end if
        end if

        call save_excursion_end(excursion)

        if (.not. allocated(from_display)) then
            from_display = repeat(" ", width)
        else
            ! We cannot tolerate any leftover newlines in From, as that
            ! disrupts the rmail-summary display.
            call replace_newlines(from_display)
            length = len(from_display)
            mch = scan(from_display, "@%")

            if (mch == 0 .or. length <= width) then
                from_display = right_justify(from_display(max(1, length - width + 1):), width)
            else
                ! Upstream's MCH is 0-based.
                mch = mch - 1

                if (mch - before < 0) then
                    lo = 0
                else if (length < mch + after) then
                    lo = length - width
                else
                    lo = mch - before
                end if

                from_display = right_justify(from_display(lo + 1:min(length, lo + width)), width)
            end if
        end if

        basic_start = date//"  "//from_display
        call goto_character(point_minimum())
        call regular_expression_search_forward("^Subject:", found, st, noerror=SEARCH_NOERROR)

        if (found > 0) then
            moved = skip_characters_forward(" "//TAB, st)
            position = point()
            moved = forward_line(1_int64)
            call buffer_substring(position, point() - 1, subject, st)

            do while (looking_at("[ "//TAB//"]", st))
                call buffer_substring(match_end(0, st), line_end_position(1_int64), more, st)
                subject = subject//" "//more
                moved = forward_line(1_int64)
            end do
        else
            call regular_expression_search_forward("[\n][\n]+", found, st, noerror=SEARCH_NOERROR)
            position = point()
            call end_of_line(1_int64)
            call buffer_substring(position, point(), subject, st)
        end if

        case_fold_search%value = saved_fold
        basic_end = subject//LF

    contains

        ! The text matched by subexpression N.
        function match_text(n) result(matched)
            integer, intent(in) :: n
            character(len=:), allocatable :: matched
            type(status_type) :: st2

            call buffer_substring(match_beginning(n, st2), match_end(n, st2), matched, st2)
        end function match_text

        ! TEXT right-justified in WIDE characters ("%Ns").
        function right_justify(text, wide) result(justified)
            character(len=*), intent(in) :: text
            integer, intent(in) :: wide
            character(len=:), allocatable :: justified

            justified = repeat(" ", max(0, wide - len(text)))//text
        end function right_justify

        ! Replace runs of newlines in TEXT by a space.
        subroutine replace_newlines(text)
            character(len=:), allocatable, intent(inout) :: text
            integer :: k, j

            k = index(text, LF)

            do while (k > 0)
                j = k

                do while (j < len(text))
                    if (text(j + 1:j + 1) /= LF) exit
                    j = j + 1
                end do

                text = text(1:k - 1)//" "//text(j + 1:)
                k = index(text, LF)
            end do
        end subroutine replace_newlines

        ! Whether FROM is absent or this user's address.
        logical function is_this_user(from)
            character(len=:), allocatable, intent(in) :: from
            character(len=:), allocatable :: regular_expression, address
            type(status_type) :: st2

            is_this_user = .true.
            if (.not. allocated(from)) return

            if (allocated(rmail_user_mail_address_regular_expression)) then
                regular_expression = rmail_user_mail_address_regular_expression
            else
                address = user_mail_address_value()
                regular_expression = "^\("//regular_expression_quote(user_login_name())//"\($\|@\)\|" &
                                     //regular_expression_quote(address)//"\>\)"
            end if

            is_this_user = string_match(regular_expression, from, st2) > 0
        end function is_this_user
    end subroutine summarize_rmail_header

    ! The number at the start of TEXT (string-to-number: 0 if none).
    ! Not a port.
    integer(int64) function string_number(text) result(number)
        character(len=*), intent(in) :: text
        integer :: ios

        number = 0
        if (len_trim(text) == 0) return
        read (text, *, iostat=ios) number
        if (ios /= 0) number = 0
    end function string_number

    ! ------------------------------------------------------------------
    ! Simple motion in a summary buffer
    ! ------------------------------------------------------------------

    ! Show the Rmail buffer in a window (display-buffer rmail-buffer).
    ! Not a port.
    subroutine display_rmail_buffer(status)
        use window_mod, only: display_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        call display_buffer(rmail_buffer(), window, status)
    end subroutine display_rmail_buffer

    !> Move NUMBER lines in the summary, deleted messages or not.  Port of
    !> rmailsum.el:rmail-summary-next-all.
    recursive subroutine next_rmail_summary_all(number, status)
        integer(int64), intent(in) :: number
        type(status_type), intent(out) :: status
        integer(int64) :: moved

        moved = forward_line(number)
        ! It doesn't look nice to move forward past the last message line.
        if (is_end_of_buffer() .and. number > 0) moved = forward_line(-1_int64)
        call display_rmail_buffer(status)
    end subroutine next_rmail_summary_all

    !> Move back NUMBER lines in the summary, deleted messages or not.
    !> Port of rmailsum.el:rmail-summary-previous-all.
    recursive subroutine previous_rmail_summary_all(number, status)
        integer(int64), intent(in) :: number
        type(status_type), intent(out) :: status

        call next_rmail_summary_all(-number, status)
    end subroutine previous_rmail_summary_all

    !> Display next non-deleted msg from rmail file.
    !> With optional prefix argument NUMBER, moves forward this number of non-deleted
    !> messages, or backward if NUMBER is negative.  Port of
    !> rmailsum.el:rmail-summary-next-msg.
    recursive subroutine next_rmail_summary_message(number, status)
        integer(int64), intent(in) :: number
        type(status_type), intent(out) :: status
        integer(int64) :: count, found
        logical :: non_deleted_message_found

        call beginning_of_line(1_int64)
        if (number > 0) call end_of_line(1_int64)
        count = abs(number)
        non_deleted_message_found = .false.

        do while (count > 0)
            if (number > 0) then
                call regular_expression_search_forward("^.....[^D]", found, status, noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_backward("^.....[^D]", found, status, noerror=SEARCH_NOERROR)
            end if

            non_deleted_message_found = found > 0 .or. non_deleted_message_found
            if (.not. non_deleted_message_found) exit
            count = count - 1
        end do

        call beginning_of_line(1_int64)
        call display_rmail_buffer(status)
    end subroutine next_rmail_summary_message

    !> Display previous non-deleted msg from rmail file.
    !> With optional prefix argument NUMBER, moves backward this number of
    !> non-deleted messages.  Port of rmailsum.el:rmail-summary-previous-msg.
    recursive subroutine previous_rmail_summary_message(number, status)
        integer(int64), intent(in) :: number
        type(status_type), intent(out) :: status

        call next_rmail_summary_message(-number, status)
    end subroutine previous_rmail_summary_message

    !> Show next message with LABELS.  Defaults to last labels used.
    !> With prefix argument N moves forward N messages with these labels.
    !> Port of rmailsum.el:rmail-summary-next-labeled-message.
    recursive subroutine next_rmail_summary_labeled_message(n, labels, status)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: labels
        type(status_type), intent(out) :: status

        call move_labeled_in_rmail_buffer(n, labels, status)
    end subroutine next_rmail_summary_labeled_message

    !> Show previous message with LABELS.  Defaults to last labels used.
    !> With prefix argument N moves backward N messages with these labels.
    !> Port of rmailsum.el:rmail-summary-previous-labeled-message.
    recursive subroutine previous_rmail_summary_labeled_message(n, labels, status)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: labels
        type(status_type), intent(out) :: status

        call move_labeled_in_rmail_buffer(-n, labels, status)
    end subroutine previous_rmail_summary_labeled_message

    ! Move N messages with LABELS in the Rmail buffer, then to its line.
    ! Not a port: the bodies of the two commands above.
    recursive subroutine move_labeled_in_rmail_buffer(n, labels, status)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: labels
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: message
        logical :: found
        integer :: k

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        call next_rmail_labeled_message(n, labels, status)
        k = rmail_index()
        message = rmail_states(k)%current_message
        call set_buffer(saved, st)
        if (.not. status%ok()) return
        call goto_rmail_summary_message(status, message, found=found)
    end subroutine move_labeled_in_rmail_buffer

    ! ------------------------------------------------------------------
    ! Deleting
    ! ------------------------------------------------------------------

    !> Delete this message and move to next nondeleted one.
    !> Deleted messages stay in the file until the \[expunge_rmail] command is given.
    !> A prefix argument serves as a repeat count;
    !> a negative argument means to delete and move backward.  Port of
    !> rmailsum.el:rmail-summary-delete-forward.
    recursive subroutine delete_rmail_summary_message_forward(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: n, deleted_message, moved
        logical :: backward, found, at_deleted
        integer :: k

        n = 1
        if (present(count)) n = count
        backward = n < 0

        do while (n /= 0)
            ! Don't waste time counting down without doing anything if we
            ! are at the beginning and trying to go backward.
            if (backward .and. is_beginning_of_buffer()) n = -1
            call goto_rmail_summary_message(status, found=found)
            if (.not. status%ok()) return
            saved = current_buffer()
            call set_buffer(rmail_buffer(), status)
            if (.not. status%ok()) return
            k = rmail_index()
            deleted_message = rmail_states(k)%current_message
            call delete_rmail_message(status)
            call set_buffer(saved, st)
            if (.not. status%ok()) return
            call mark_rmail_summary_deleted(deleted_message, status)
            if (.not. status%ok()) return

            do
                if (backward) then
                    if (is_beginning_of_buffer()) exit
                else
                    if (is_end_of_buffer()) exit
                end if

                at_deleted = line_looking_at(" *[0-9]+D")
                if (.not. at_deleted) exit

                if (backward) then
                    moved = forward_line(-1_int64)
                else
                    moved = forward_line(1_int64)
                end if
            end do

            if (n > 0) then
                n = n - 1
            else
                n = n + 1
            end if

            ! It looks ugly to move to the empty line at end of buffer.
            ! And don't waste time after hitting the end.
            if (is_end_of_buffer() .and. .not. backward) then
                n = 0
                moved = forward_line(-1_int64)
            end if
        end do
    end subroutine delete_rmail_summary_message_forward

    ! Whether the text from the beginning of the line matches REGULAR_EXPRESSION.
    ! Not a port.
    logical function line_looking_at(regular_expression) result(matched)
        character(len=*), intent(in) :: regular_expression
        type(excursion_type) :: saved
        type(status_type) :: st

        call save_excursion_begin(saved)
        call beginning_of_line(1_int64)
        matched = looking_at(regular_expression, st)
        call save_excursion_end(saved)
    end function line_looking_at

    !> Delete this message and move to previous nondeleted one.
    !> Deleted messages stay in the file until the \[expunge_rmail] command is given.
    !> A prefix argument serves as a repeat count;
    !> a negative argument means to delete and move forward.  Port of
    !> rmailsum.el:rmail-summary-delete-backward.
    recursive subroutine delete_rmail_summary_message_backward(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call delete_rmail_summary_message_forward(status, -n)
    end subroutine delete_rmail_summary_message_backward

    !> Mark message N (that of the line at point if 0) deleted in the
    !> summary.  Port of rmailsum.el:rmail-summary-mark-deleted.
    recursive subroutine mark_rmail_summary_deleted(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call mark_deleted(n, .false., status)
    end subroutine mark_rmail_summary_deleted

    !> Mark message N undeleted in the summary.  Port of
    !> rmailsum.el:rmail-summary-mark-undeleted.
    recursive subroutine mark_rmail_summary_undeleted(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call mark_deleted(n, .true., status)
    end subroutine mark_rmail_summary_undeleted

    ! Mark message N (that of the line at point if 0) deleted in the
    ! summary, or undeleted if UNDELETE.  Port of
    ! rmailsum.el:rmail-summary-mark-deleted.
    recursive subroutine mark_deleted(n, undelete, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: undelete
        type(status_type), intent(out) :: status
        type(value_type) :: face
        type(status_type) :: st
        integer(int64) :: moved
        logical :: found, saved_read_only
        integer :: s, k

        status = status_type()

        ! Since third arg is .true., this only alters summary, not the
        ! Rmail buffer.
        if (n /= 0 .and. n /= rmail_summary_message_number()) then
            call goto_rmail_summary_message(status, n, .true., .true., found)
            if (.not. status%ok()) return
        end if

        s = summary_state_index(.false.)
        face = value_type()
        if (s > 0) face = overlay_get(summary_states(s)%overlay, "face")

        if (.not. is_end_of_buffer() .and. allocated(face%a)) then
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call beginning_of_line(1_int64)
            moved = skip_characters_forward(" ", st)
            moved = skip_characters_forward("0-9", st)

            if (undelete) then
                if (looking_at("D", st)) then
                    call delete_region(point(), point() + 1, status)
                    if (status%ok()) call insert(" ", status)
                end if
            else
                call delete_region(point(), point() + 1, status)
                if (status%ok()) call insert("D", status)
            end if

            buffers(current)%read_only = saved_read_only

            ! Discard cached new summary line.
            if (n /= 0) then
                k = rmail_index()
                if (allocated(rmail_states(k)%summaries(n)%text)) deallocate (rmail_states(k)%summaries(n)%text)
            end if
        end if

        call beginning_of_line(1_int64)
    end subroutine mark_deleted

    !> Update the summary line for message N.  Port of
    !> rmailsum.el:rmail-summary-update-line.
    recursive subroutine update_rmail_summary_line(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: line
        type(handle_type) :: saved
        type(value_type) :: high
        type(status_type) :: st
        integer(int64) :: start, end, moved
        logical :: found, saved_read_only, highlighted
        integer :: k, s

        call goto_rmail_summary_message(status, n, .true., .true., found)
        if (.not. status%ok() .or. .not. found) return
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        start = line_beginning_position(1_int64)
        end = line_beginning_position(2_int64)
        ! Whether the highlighting overlay is on this line.
        s = summary_state_index(.false.)
        highlighted = .false.

        if (s > 0) then
            high = overlay_get(summary_states(s)%overlay, "rmail_summary")
            highlighted = allocated(high%a) .and. overlay_on_line(summary_states(s)%overlay, start, end)
        end if

        call delete_region(start, end, status)

        if (status%ok()) then
            saved = current_buffer()
            call set_buffer(rmail_buffer(), status)
            k = rmail_index()
            if (status%ok()) call create_rmail_summary_line(n, line, status)
            if (status%ok()) rmail_states(k)%summaries(n)%text = line
            call set_buffer(saved, st)
            if (status%ok()) call insert(line, status)
        end if

        buffers(current)%read_only = saved_read_only
        if (.not. status%ok()) return

        if (highlighted) then
            moved = forward_line(-1_int64)
            call update_rmail_summary_highlight(.false., status)
        end if

    contains

        ! Whether OVERLAY overlaps the text from START to END.
        logical function overlay_on_line(overlay, start, end)
            use interval_tree_mod, only: overlay_start, overlay_end
            type(handle_type), intent(in) :: overlay
            integer(int64), intent(in) :: start, end

            overlay_on_line = overlay_start(overlay) < end .and. overlay_end(overlay) >= start
            if (overlay_start(overlay) == 0) overlay_on_line = .false.
        end function overlay_on_line
    end subroutine update_rmail_summary_line

    ! Whether message N (that of the line at point if 0) is marked
    ! deleted in the summary.  Port of rmailsum.el:rmail-summary-deleted-p.
    recursive logical function is_summary_message_deleted(n) result(deleted)
        integer(int64), intent(in) :: n
        type(excursion_type) :: saved
        type(status_type) :: st
        integer(int64) :: moved
        logical :: found

        call save_excursion_begin(saved)
        if (n /= 0) call goto_rmail_summary_message(st, n, .false., .true., found)
        moved = skip_characters_forward(" ", st)
        moved = skip_characters_forward("0-9", st)
        deleted = looking_at("D", st)
        call save_excursion_end(saved)
    end function is_summary_message_deleted

    !> Undelete current message.
    !> Optional prefix ARGUMENT means undelete ARGUMENT previous messages.  Port of
    !> rmailsum.el:rmail-summary-undelete.
    recursive subroutine undelete_rmail_summary(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, found
        logical :: saved_read_only, ok
        integer :: k

        if (argument /= 1) then
            call undelete_rmail_summary_many(status, argument)
            return
        end if

        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        original_point = point()
        call end_of_line(1_int64)
        call regular_expression_search_backward("\(^ *[0-9]*\)\(D\)", found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return

        if (found > 0) then
            call replace_match("\1 ", status)
            buffers(current)%read_only = saved_read_only
            if (.not. status%ok()) return
            call goto_rmail_summary_message(status, found=ok)
            if (.not. status%ok()) return
            call pop_to_rmail_buffer(rmail_buffer(), status)
            if (.not. status%ok()) return
            k = rmail_index()

            if (is_rmail_message_deleted(rmail_states(k)%current_message)) then
                call undelete_previous_rmail_message(1_int64, status)
                if (.not. status%ok()) return
            end if

            call pop_to_rmail_buffer(rmail_states(k)%summary_buffer, status)
        else
            buffers(current)%read_only = saved_read_only
            call goto_character(original_point)
        end if
    end subroutine undelete_rmail_summary

    !> Undelete all deleted msgs, optional prefix arg N means undelete N
    !> prev msgs.  Port of rmailsum.el:rmail-summary-undelete-many.
    recursive subroutine undelete_rmail_summary_many(status, n)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: count, i, deleted_message, moved, total
        logical :: found
        integer :: k

        status = status_type()

        if (present(n)) then
            count = n

            do while (count > 0 .and. .not. is_end_of_buffer())
                call goto_rmail_summary_message(status, found=found)
                if (.not. status%ok()) return

                if (is_summary_message_deleted(0_int64)) then
                    call undelete_in_rmail(deleted_message)
                    if (.not. status%ok()) return
                    call mark_rmail_summary_undeleted(deleted_message, status)
                    if (.not. status%ok()) return
                end if

                do while (.not. is_end_of_buffer())
                    if (.not. line_looking_at(" *[0-9]+ ")) exit
                    moved = forward_line(1_int64)
                end do

                count = count - 1
            end do
        else
            call goto_rmail_summary_message(status, 1_int64, found=found)
            if (.not. status%ok()) return
            k = summary_state_index(.false.)
            total = 0
            if (k > 0) total = summary_states(k)%total_messages

            do i = 1, total
                call goto_rmail_summary_message(status, found=found)
                if (.not. status%ok()) return

                if (is_summary_message_deleted(0_int64)) then
                    call undelete_in_rmail(deleted_message)
                    if (.not. status%ok()) return
                    call mark_rmail_summary_undeleted(deleted_message, status)
                    if (.not. status%ok()) return
                end if

                if (.not. is_end_of_buffer()) moved = forward_line(1_int64)
            end do
        end if

        ! It looks ugly to move to the empty line at end of buffer.
        if (is_end_of_buffer()) moved = forward_line(-1_int64)

    contains

        ! Undelete in the Rmail buffer; MESSAGE is its current message.
        subroutine undelete_in_rmail(message_number)
            integer(int64), intent(out) :: message_number
            integer :: j

            message_number = 0
            saved = current_buffer()
            call set_buffer(rmail_buffer(), status)
            if (.not. status%ok()) return
            call undelete_previous_rmail_message(1_int64, status)
            j = rmail_index()
            message_number = rmail_states(j)%current_message
            call set_buffer(saved, st)
        end subroutine undelete_in_rmail
    end subroutine undelete_rmail_summary_many

    ! ------------------------------------------------------------------
    ! Rmail Summary mode
    ! ------------------------------------------------------------------

    !> Rmail Summary Mode is invoked from Rmail Mode by using \<rmail_mode_map>\[rmail_summary].
    !> As commands are issued in the summary buffer, they are applied to the
    !> corresponding mail messages in the rmail buffer.
    !>
    !> All normal editing commands are turned off.
    !> Instead, nearly all the Rmail mode commands are available,
    !> though many of them move only among the messages in the summary.
    !>
    !> These additional commands exist:
    !>
    !> \[undelete_rmail_summary_many]        Undelete all or prefix arg deleted messages.
    !> \[wipe_rmail_summary] Delete the summary and go to the Rmail buffer.
    !>
    !> Commands for sorting the summary:
    !>
    !> \[rmail_summary_sort_by_date] Sort by date.
    !> \[rmail_summary_sort_by_subject] Sort by subject.
    !> \[rmail_summary_sort_by_author] Sort by author.
    !> \[rmail_summary_sort_by_recipient] Sort by recipient.
    !> \[rmail_summary_sort_by_correspondent] Sort by correspondent.
    !> \[rmail_summary_sort_by_lines] Sort by lines.
    !> \[rmail_summary_sort_by_labels] Sort by labels.
    !> Port of rmailsum.el:rmail-summary-mode (truncate-lines and the
    !> font lock keywords wait for those features).
    recursive subroutine rmail_summary_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%rmail_summary_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .true.
        buffers(current)%syntax_table = text_mode_syntax_table
        call enable_rmail_summary(0_int64, status)
        if (.not. status%ok()) return
        call major_modes%rmail_summary_mode%end(saved, status)
    end subroutine rmail_summary_mode

    !> Summary features need to be disabled during edit mode.  Port of
    !> rmailsum.el:rmail-summary-disable.
    recursive subroutine disable_rmail_summary(n, status)
        use files_mod, only: revert_buffer_function
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        status = status_type()
        if (n < 0) return
        call use_local_map(text_mode_map, status)
        call post_command_hook%remove(update_rmail_from_summary, local=.true.)
        call revert_buffer_function%set_local()
    end subroutine disable_rmail_summary

    !> Enable the summary's features again.  Port of
    !> rmailsum.el:rmail-summary-enable.
    recursive subroutine enable_rmail_summary(n, status)
        use files_mod, only: revert_buffer_function
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        status = status_type()
        if (n < 0) return
        call use_local_map(rmail_summary_mode_map, status)
        call post_command_hook%prepend(update_rmail_from_summary, local=.true.)
        call revert_buffer_function%set_local(revert_summary)
    end subroutine enable_rmail_summary

    ! Remove the unseen mark from the current message N, update the
    ! summary array.  NOMOVE true means we are already at the right
    ! column.  UNSEEN true means mark the message as unseen.  Port of
    ! rmailsum.el:rmail-summary-mark-seen.
    subroutine mark_rmail_summary_seen(n, status, nomove, unseen)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nomove, unseen
        type(excursion_type) :: saved
        character(len=:), allocatable :: line
        character :: wanted, replacement
        type(status_type) :: st
        integer(int64) :: moved
        integer(int32) :: c
        logical :: exists, saved_read_only, mark_unseen
        integer :: k

        status = status_type()
        mark_unseen = .false.
        if (present(unseen)) mark_unseen = unseen
        call save_excursion_begin(saved)

        if (.not. present(nomove)) then
            call beginning_of_line(1_int64)
            moved = skip_characters_forward(" ", st)
            moved = skip_characters_forward("0-9", st)
        else if (.not. nomove) then
            call beginning_of_line(1_int64)
            moved = skip_characters_forward(" ", st)
            moved = skip_characters_forward("0-9", st)
        end if

        wanted = "-"
        replacement = " "

        if (mark_unseen) then
            wanted = " "
            replacement = "-"
        end if

        call character_after(c, exists)

        if (exists .and. c == iachar(wanted)) then
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call delete_region(point(), point() + 1, status)
            if (status%ok()) call insert(replacement, status)
            buffers(current)%read_only = saved_read_only
            call buffer_substring_no_properties(line_beginning_position(1_int64), line_beginning_position(2_int64), line, st)
            k = rmail_index()

            if (k > 0 .and. n >= 1) then
                if (n <= size(rmail_states(k)%summaries)) rmail_states(k)%summaries(n)%text = line
            end if
        end if

        call save_excursion_end(saved)
    end subroutine mark_rmail_summary_seen

    ! Show in Rmail the message described by the summary line that point
    ! is on, but only if the Rmail buffer is already visible.  This is a
    ! post-command-hook in summary buffers.  Port of
    ! rmailsum.el:rmail-summary-rmail-update (linum-update is not ported).
    recursive subroutine update_rmail_from_summary(status)
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        type(handle_type) :: window, owin, rmail_buffer_handle, summary_buffer
        character(len=:), allocatable :: digits
        type(status_type) :: st
        integer(int64) :: message_number, moved, start, here
        logical :: saved_read_only
        integer :: k, ios

        status = status_type()
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call save_excursion_begin(saved)

        body: block
            ! If at end of buffer, pretend we are on the last text line.
            if (is_end_of_buffer()) moved = forward_line(-1_int64)
            call beginning_of_line(1_int64)
            moved = skip_characters_forward(" ", st)
            ! If the summary is empty, don't do anything.
            if (is_end_of_buffer()) exit body
            start = point()
            moved = skip_characters_forward("0-9", st)
            call buffer_substring_no_properties(start, point(), digits, st)
            message_number = 0

            if (len(digits) > 0) then
                read (digits, *, iostat=ios) message_number
                if (ios /= 0) message_number = 0
            end if

            ! Always leave `unseen' removed if we get out of isearch mode.
            ! Don't let a subsequent isearch restore that `unseen'.
            if (.not. incremental_search_active) put_back_unseen = 0
            k = rmail_index()
            if (k == 0) exit body
            rmail_buffer_handle = rmail_buffer()
            summary_buffer = current_buffer()

            if (rmail_states(k)%current_message /= message_number) then
                window = get_buffer_window(rmail_buffer_handle)
                owin = selected_window()

                if (incremental_search_active) then
                    ! If we first saw the previous message in this search,
                    ! and we have gone to a different message while
                    ! searching, put back `unseen' on the former one.
                    if (put_back_unseen /= 0) then
                        call set_rmail_attribute(RMAIL_UNSEEN_ATTRIBUTE_INDEX, .true., status, &
                                                 rmail_states(k)%current_message)
                        if (.not. status%ok()) exit body
                        here = point()
                        call goto_character(put_back_unseen)
                        call mark_rmail_summary_seen(rmail_states(k)%current_message, status, .true., .true.)
                        call goto_character(here)
                    end if

                    ! Arrange to do that later, for the new current
                    ! message, if it still has `unseen'.
                    put_back_unseen = 0
                    if (is_rmail_message_unseen(message_number)) put_back_unseen = point()
                else
                    put_back_unseen = 0
                end if

                ! Go to the desired message.
                rmail_states(k)%current_message = message_number
                ! Update the summary to show the message has been seen.
                call mark_rmail_summary_seen(message_number, status, .true.)
                if (.not. status%ok()) exit body

                if (window%index /= 0) then
                    ! Using save-window-excursion would cause the new value
                    ! of point to get lost.
                    call select_window(window, status)
                    if (status%ok()) call show_rmail_message(status, message_number, .true.)
                    call select_window(owin, st)
                else if (is_buffer_live(rmail_buffer_handle)) then
                    call set_buffer(rmail_buffer_handle, status)
                    if (status%ok()) call show_rmail_message(status, message_number, .true.)
                end if

                call set_buffer(summary_buffer, st)
                if (.not. status%ok()) exit body
            end if

            call update_rmail_summary_highlight(.false., status)
        end block body

        call save_excursion_end(saved)
        buffers(current)%read_only = saved_read_only
    end subroutine update_rmail_from_summary

    !> Save the buffer associated with this RMAIL summary.  Port of
    !> rmailsum.el:rmail-summary-save-buffer.
    recursive subroutine save_rmail_summary_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call save_buffer(1_int64, .false., status)
        call set_buffer(saved, st)
    end subroutine save_rmail_summary_buffer

    ! The number of the message of the line at point.  Port of
    ! rmailsum.el:rmail-summary-msg-number.
    integer(int64) function rmail_summary_message_number() result(number)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring_no_properties(line_beginning_position(1_int64), &
                                            min(point_maximum(), line_beginning_position(1_int64) + 6), text, st)
        number = string_number(text)
    end function rmail_summary_message_number

    !> Go to message N in the summary buffer and the Rmail buffer.
    !> If N is absent, use the message corresponding to point in the summary
    !> and move to that message in the Rmail buffer.
    !>
    !> If NOWARN, don't say anything if N is out of range.
    !> If SKIP_RMAIL, don't do anything to the Rmail buffer.
    !> FOUND is true if message N was found.  Port of
    !> rmailsum.el:rmail-summary-goto-msg.
    recursive subroutine goto_rmail_summary_message(status, n, nowarn, skip_rmail, found)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        logical, intent(in), optional :: nowarn, skip_rmail
        logical, intent(out), optional :: found
        type(handle_type) :: output_buffer, mail_buffer, selwin
        type(status_type) :: st
        character(len=20) :: pattern
        character(len=40) :: text
        integer(int64) :: original_point, curmsg, total, number, at, moved
        logical :: message_not_found, quiet, skip, sub_found
        integer :: k

        status = status_type()
        quiet = .false.
        if (present(nowarn)) quiet = nowarn
        skip = .false.
        if (present(skip_rmail)) skip = skip_rmail
        if (is_end_of_buffer()) moved = forward_line(-1_int64)
        call beginning_of_line(1_int64)
        output_buffer = current_buffer()
        mail_buffer = rmail_buffer()
        original_point = point()
        message_not_found = .false.
        curmsg = rmail_summary_message_number()
        k = rmail_index()
        total = 0
        if (k > 0) total = rmail_states(k)%total_messages
        number = curmsg

        if (present(n)) then
            number = n

            if (number < 1) then
                call message("No preceding message")
                number = 1
            end if

            if (number > total .and. total > 0) then
                call message("No following message")
                call goto_character(point_maximum())
                call goto_rmail_summary_message(status, nowarn=nowarn, skip_rmail=skip_rmail, found=sub_found)
            end if

            call goto_character(point_minimum())
            write (pattern, "(i5)") number
            if (number > 99999) write (pattern, "(i0)") number
            call regular_expression_search_forward("^"//trim(pattern)//"[^0-9]", at, st, noerror=SEARCH_NOERROR)

            if (at == 0) then
                if (.not. quiet) then
                    write (text, '("Message ", i0, " not found")') number
                    call message(trim(text))
                end if

                number = curmsg
                message_not_found = .true.
                call goto_character(original_point)
            end if
        end if

        ! N can be 0 now, along with CURMSG, if the summary is empty.
        if (number /= 0) then
            call mark_rmail_summary_seen(number, status)
            if (.not. status%ok()) return
            call update_rmail_summary_highlight(message_not_found, status)
            if (.not. status%ok()) return
            call beginning_of_line(1_int64)

            if (.not. skip) then
                selwin = selected_window()
                call pop_to_rmail_buffer(mail_buffer, status)
                if (status%ok()) call show_rmail_message(status, number)
                call select_window(selwin, st)
                ! The actions above can alter the current buffer.
                ! Preserve it.
                call set_buffer(output_buffer, st)
            end if
        end if

        if (present(found)) found = .not. message_not_found
    end subroutine goto_rmail_summary_message

    ! `goto_rmail_summary_message' with NOWARN and SKIP_RMAIL, as Rmail
    ! calls it.  Not a port.
    recursive subroutine goto_message_for_rmail(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical :: found

        call goto_rmail_summary_message(status, n, .true., .true., found)
    end subroutine goto_message_for_rmail

    ! Update the highlighted line in an rmail summary buffer.  That should
    ! be current.  We highlight the line point is on.  If NOT_FOUND is
    ! true, we turn off highlighting.  Port of
    ! rmailsum.el:rmail-summary-update-highlight.
    subroutine update_rmail_summary_highlight(not_found, status)
        logical, intent(in) :: not_found
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        type(status_type) :: st
        integer(int64) :: beginning, moved
        integer :: s

        status = status_type()
        s = summary_state_index(.true.)

        ! Make sure we have an overlay to use.
        if (summary_states(s)%overlay%index == 0) then
            call make_overlay(summary_states(s)%overlay, point(), point(), status)
            if (.not. status%ok()) return
            call overlay_put(summary_states(s)%overlay, "rmail_summary", value_type(.true.), status)
            if (.not. status%ok()) return
        end if

        ! If this message is in the summary, use the overlay to highlight
        ! it.  Otherwise, don't highlight anything.
        if (not_found) then
            call overlay_put(summary_states(s)%overlay, "face", value_type(), status)
        else
            call save_excursion_begin(saved)
            call beginning_of_line(1_int64)
            moved = skip_characters_forward(" ", st)
            beginning = point()
            call save_excursion_end(saved)
            call move_overlay(summary_states(s)%overlay, beginning, line_end_position(1_int64), status)
            if (status%ok()) call overlay_put(summary_states(s)%overlay, "face", value_type(name=trim(highlight%name)), &
                                              status)
        end if
    end subroutine update_rmail_summary_highlight

    !> Scroll the Rmail window forward.
    !> If the Rmail window is displaying the end of a message,
    !> advance to the next message.  Port of
    !> rmailsum.el:rmail-summary-scroll-msg-up.
    recursive subroutine scroll_rmail_summary_message_up(dist, status)
        type(prefix_argument_type), intent(in) :: dist
        type(status_type), intent(out) :: status

        call scroll_message(dist, .true., status)
    end subroutine scroll_rmail_summary_message_up

    !> Scroll the Rmail window backward.
    !> If the Rmail window is now displaying the beginning of a message,
    !> move to the previous message.  Port of
    !> rmailsum.el:rmail-summary-scroll-msg-down.
    recursive subroutine scroll_rmail_summary_message_down(dist, status)
        type(prefix_argument_type), intent(in) :: dist
        type(status_type), intent(out) :: status

        call scroll_message(dist, .false., status)
    end subroutine scroll_rmail_summary_message_down

    ! The bodies of the two commands above, UP for the first.  Port of
    ! rmailsum.el:rmail-summary-scroll-msg-up and
    ! rmailsum.el:rmail-summary-scroll-msg-down (other-window-scroll-buffer
    ! is not ported: the Rmail window is the next window).
    recursive subroutine scroll_message(dist, up, status)
        type(prefix_argument_type), intent(in) :: dist
        logical, intent(in) :: up
        type(status_type), intent(out) :: status
        type(handle_type) :: rmail_window, summary_window, saved_buffer
        type(status_type) :: st
        integer(int64) :: count, edge
        logical :: visible

        if (dist%kind == PREFIX_MINUS) then
            call scroll_message(prefix_argument_type(PREFIX_NONE, 1_int64),.not. up, status)
            return
        end if

        rmail_window = get_buffer_window(rmail_buffer())

        if (rmail_window%index == 0) then
            ! If it isn't visible at all, show the beginning.
            call show_rmail_summary_message(.false., status)
            return
        end if

        summary_window = selected_window()
        saved_buffer = current_buffer()
        call select_window(rmail_window, status)
        if (.not. status%ok()) return

        ! Is END_OF_BUFFER (BEGINNING_OF_BUFFER) visible in the buffer?
        if (up) then
            edge = point_maximum()
        else
            edge = point_minimum()
        end if

        visible = is_position_visible_in_window(edge, rmail_window)
        call select_window(summary_window, status)
        call set_buffer(saved_buffer, st)
        if (.not. status%ok()) return
        count = 1
        if (dist%kind /= PREFIX_NONE) count = prefix_numeric_value(dist)

        if (visible) then
            if (.not. rmail_summary_scroll_between_messages) then
                if (up) then
                    call set_error(status, GENERIC_ERROR_CODE, "End of buffer")
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Beginning of buffer")
                end if
            else if (up) then
                call next_rmail_summary_message(count, status)
            else
                call previous_rmail_summary_message(count, status)
            end if
        else if (up) then
            call scroll_other_window(dist, status)
        else
            call scroll_other_window_down(dist, status)
        end if
    end subroutine scroll_message

    !> Show current message from the beginning.  Port of
    !> rmailsum.el:rmail-summary-beginning-of-message.
    recursive subroutine beginning_of_rmail_summary_message(status)
        type(status_type), intent(out) :: status

        call show_rmail_summary_message(.false., status)
    end subroutine beginning_of_rmail_summary_message

    !> Show bottom of current message.  Port of
    !> rmailsum.el:rmail-summary-end-of-message.
    recursive subroutine end_of_rmail_summary_message(status)
        type(status_type), intent(out) :: status

        call show_rmail_summary_message(.true., status)
    end subroutine end_of_rmail_summary_message

    ! Show current mail message, at its beginning (after the header) or,
    ! with AT_END, its end.  Port of rmailsum.el:rmail-summary-show-message.
    recursive subroutine show_rmail_summary_message(at_end, status)
        logical, intent(in) :: at_end
        type(status_type), intent(out) :: status
        type(handle_type) :: window, buffer, summary
        type(status_type) :: st
        integer(int64) :: found
        integer :: height, k

        buffer = rmail_buffer()
        k = rmail_index()
        summary = rmail_states(k)%summary_buffer

        if (is_one_window()) then
            call split_window(selected_window(), rmail_summary_window_size, rmail_summary_window_size > 0, .false., &
                              window, status)
            if (.not. status%ok()) return
            call select_window(frame_first_window(), status)
            if (.not. status%ok()) return
            call pop_to_rmail_buffer(buffer, status)
            if (.not. status%ok()) return
            ! If pop-to-buffer did not use that window, delete that
            ! window.
            window = window_buffer(next_window(frame_first_window(), MINIBUF_ACTIVE, st), st)
            if (window%index /= buffer%index) call delete_other_windows(selected_window(), status)
        else
            call pop_to_rmail_buffer(buffer, status)
        end if

        if (.not. status%ok()) return

        if (at_end) then
            call goto_character(point_maximum())
            height = window_body_height(selected_window(), status)
            if (status%ok()) call recenter(prefix_argument_type(PREFIX_NUMBER, int(height - 1, int64)), .false., status)
        else
            call goto_character(point_minimum())
            call search_forward(LF//LF, found, status)
        end if

        if (.not. status%ok()) return
        call pop_to_rmail_buffer(summary, status)
    end subroutine show_rmail_summary_message

    !> Bury the Rmail buffer and the Rmail summary buffer.  Port of
    !> rmailsum.el:rmail-summary-bury.
    recursive subroutine bury_rmail_summary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer_to_bury, window, other
        character(len=:), allocatable :: name

        buffer_to_bury = current_buffer()

        do
            window = get_buffer_window(rmail_buffer())
            if (window%index == 0) exit
            call set_window_buffer(window, other_buffer(rmail_buffer()), status)
            if (.not. status%ok()) return
        end do

        call bury_buffer(status, rmail_buffer())
        if (.not. status%ok()) return
        other = other_buffer(buffer_to_bury)
        call buffer_name(other, name, status)
        if (status%ok()) call switch_to_buffer(name, status)
        if (status%ok()) call bury_buffer(status, buffer_to_bury)
    end subroutine bury_rmail_summary

    !> Quit out of Rmail and Rmail summary.  Port of
    !> rmailsum.el:rmail-summary-quit.
    recursive subroutine quit_rmail_summary(status)
        type(status_type), intent(out) :: status

        call wipe_rmail_summary(status)
        if (status%ok()) call quit_rmail(status)
    end subroutine quit_rmail_summary

    !> Kill and wipe away Rmail summary, remaining within Rmail.  Port of
    !> rmailsum.el:rmail-summary-wipe.
    recursive subroutine wipe_rmail_summary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: local_rmail_buffer, next, this
        type(status_type) :: st
        integer :: k

        k = rmail_index()
        local_rmail_buffer = rmail_buffer()
        if (k > 0) rmail_states(k)%summary_buffer = null_handle()
        call close_buffer(current_buffer(), status)
        if (.not. status%ok()) return
        this = selected_window()
        next = next_window(this, MINIBUF_NO, st)
        if (next%index /= this%index) call delete_window(this, status)
        if (.not. status%ok()) return
        call pop_to_rmail_buffer(local_rmail_buffer, status)
    end subroutine wipe_rmail_summary

    !> Actually erase all deleted messages and recompute summary headers.
    !> Port of rmailsum.el:rmail-summary-expunge.
    recursive subroutine expunge_rmail_summary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st
        logical :: confirmed

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        call is_rmail_expunge_confirmed(confirmed, status)
        if (status%ok() .and. confirmed) call expunge_rmail_only(status)
        call set_buffer(saved, st)
        if (.not. status%ok()) return
        call update_rmail_summary(0_int64, status)
    end subroutine expunge_rmail_summary

    !> Expunge and save RMAIL file.  Port of
    !> rmailsum.el:rmail-summary-expunge-and-save.
    recursive subroutine expunge_and_save_rmail_summary(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st

        saved = current_buffer()
        call expunge_and_save_rmail(status)
        call set_buffer(saved, st)
        if (.not. status%ok()) return
        call update_rmail_summary(0_int64, status)
        call set_buffer_modified(.false.)
    end subroutine expunge_and_save_rmail_summary

    !> Get new mail and recompute summary headers.
    !>
    !> Optionally you can specify the file to get new mail from.  In this case,
    !> the file of new mail is not changed or deleted.  Noninteractively, you can
    !> pass the inbox file name as an argument.  Interactively, a prefix
    !> argument says to read a file name and use that file as the inbox.
    !> Port of rmailsum.el:rmail-summary-get-new-mail.
    recursive subroutine get_rmail_summary_new_mail(status, file_name)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_name
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: message
        logical :: found
        integer :: k

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        call get_rmail_new_mail(status, file_name)
        k = rmail_index()
        message = rmail_states(k)%current_message
        call set_buffer(saved, st)
        if (.not. status%ok()) return
        if (message /= 0) call goto_rmail_summary_message(status, message, found=found)
    end subroutine get_rmail_summary_new_mail

    !> Run Rmail on file FILENAME.  Port of rmailsum.el:rmail-summary-input.
    recursive subroutine rmail_summary_input(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call pop_to_rmail_buffer(rmail_buffer(), status)
        if (status%ok()) call rmail(status, filename)
    end subroutine rmail_summary_input

    !> Show first message in Rmail file from summary buffer.  Port of
    !> rmailsum.el:rmail-summary-first-message.
    recursive subroutine show_first_rmail_summary_message(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call beginning_of_buffer(prefix_argument_type(PREFIX_NONE, 1_int64))
    end subroutine show_first_rmail_summary_message

    !> Show last message in Rmail file from summary buffer.  Port of
    !> rmailsum.el:rmail-summary-last-message.
    recursive subroutine show_last_rmail_summary_message(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved

        status = status_type()
        call end_of_buffer(prefix_argument_type(PREFIX_NONE, 1_int64))
        moved = forward_line(-1_int64)
    end subroutine show_last_rmail_summary_message

    !> Edit the contents of this message.  Port of
    !> rmailsum.el:rmail-summary-edit-current-message.
    recursive subroutine edit_rmail_summary_current_message(status)
        type(status_type), intent(out) :: status

        call pop_to_rmail_buffer(rmail_buffer(), status)
        if (status%ok()) call edit_rmail_current_message(status)
        if (status%ok()) call use_local_map(rmail_summary_edit_map, status)
    end subroutine edit_rmail_summary_current_message

    !> Finish editing message, then go back to Rmail summary buffer.
    !> Port of rmailsum.el:rmail-summary-cease-edit.
    recursive subroutine cease_rmail_summary_edit(status)
        type(status_type), intent(out) :: status
        integer :: k

        call cease_rmail_edit(status)
        if (.not. status%ok()) return
        k = rmail_index()
        call pop_to_rmail_buffer(rmail_states(k)%summary_buffer, status)
    end subroutine cease_rmail_summary_edit

    !> Abort edit of current message; restore original contents.
    !> Go back to summary buffer.  Port of
    !> rmailsum.el:rmail-summary-abort-edit.
    recursive subroutine abort_rmail_summary_edit(status)
        type(status_type), intent(out) :: status
        integer :: k

        call abort_rmail_edit(status)
        if (.not. status%ok()) return
        k = rmail_index()
        call pop_to_rmail_buffer(rmail_states(k)%summary_buffer, status)
    end subroutine abort_rmail_summary_edit

    !> Show message containing next match for REGULAR_EXPRESSION.
    !> Prefix argument gives repeat count; negative argument means search
    !> backwards (through earlier messages).
    !> Interactively, empty argument means use same regexp used last time.
    !> Port of rmailsum.el:rmail-summary-search.
    recursive subroutine search_rmail_summary(regular_expression, status, n)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        type(handle_type) :: buffer, selwin
        type(status_type) :: st

        buffer = current_buffer()
        selwin = selected_window()
        call pop_to_rmail_buffer(rmail_buffer(), status)
        if (status%ok()) call search_rmail(regular_expression, status, n)
        call select_window(selwin, st)
        call set_buffer(buffer, st)
    end subroutine search_rmail_summary

    !> Show message containing next match for REGULAR_EXPRESSION.
    !> Prefix argument gives repeat count; negative argument means search
    !> backwards (through earlier messages).
    !> Interactively, empty argument means use same regexp used last time.
    !> Port of rmailsum.el:rmail-summary-search-backward.
    recursive subroutine search_rmail_summary_backward(regular_expression, status, n)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: count

        count = 1
        if (present(n)) count = n
        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call search_rmail(regular_expression, status, -count)
        call set_buffer(saved, st)
    end subroutine search_rmail_summary_backward

    !> Show original message header if pruned header currently shown, or
    !> vice versa.  Port of rmailsum.el:rmail-summary-toggle-header.
    recursive subroutine toggle_rmail_summary_header(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved, window, owin
        type(status_type) :: st

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call toggle_rmail_header(status)
        call set_buffer(saved, st)
        if (.not. status%ok()) return
        window = get_buffer_window(rmail_buffer())

        if (window%index /= 0) then
            owin = selected_window()
            call select_window(window, status)
            if (status%ok()) call goto_character(point_minimum())
            call select_window(owin, st)
            call set_buffer(saved, st)
        end if
    end subroutine toggle_rmail_summary_header

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine execute_rmail_summary_by_labels(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: labels

        call read_string("Labels to summarize by: ", labels, status)
        if (status%ok()) call rmail_summary_by_labels(labels, status)
    end subroutine execute_rmail_summary_by_labels

    recursive subroutine execute_rmail_summary_by_recipients(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: recipients
        logical :: primary_only

        call read_string("Recipients to summarize by: ", recipients, status)
        primary_only = current_prefix_argument%kind /= PREFIX_NONE
        if (status%ok()) call rmail_summary_by_recipients(recipients, status, primary_only)
    end subroutine execute_rmail_summary_by_recipients

    recursive subroutine execute_rmail_summary_by_senders(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: def, sender, prompt, senders

        call get_rmail_header("From", def, status)
        if (.not. status%ok()) return
        if (.not. allocated(def)) def = ""
        ! We quote the default argument, because if it contains regexp
        ! special characters (eg "?"), it can fail to match itself.
        sender = regular_expression_quote(def)
        prompt = "Senders to summarize by (regexp, default this message's sender): "
        call read_string(prompt, senders, status, default=sender)
        if (status%ok()) call rmail_summary_by_senders(senders, status)
    end subroutine execute_rmail_summary_by_senders

    recursive subroutine execute_next_rmail_summary_all(status)
        type(status_type), intent(out) :: status

        call next_rmail_summary_all(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_next_rmail_summary_all

    recursive subroutine execute_previous_rmail_summary_all(status)
        type(status_type), intent(out) :: status

        call previous_rmail_summary_all(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_previous_rmail_summary_all

    recursive subroutine execute_next_rmail_summary_message(status)
        type(status_type), intent(out) :: status

        call next_rmail_summary_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_next_rmail_summary_message

    recursive subroutine execute_previous_rmail_summary_message(status)
        type(status_type), intent(out) :: status

        call previous_rmail_summary_message(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_previous_rmail_summary_message

    recursive subroutine execute_next_rmail_summary_labeled_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: labels
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_argument)
        call read_string("Move to next msg with labels: ", labels, status)
        if (status%ok()) call next_rmail_summary_labeled_message(n, labels, status)
    end subroutine execute_next_rmail_summary_labeled_message

    recursive subroutine execute_previous_rmail_summary_labeled_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: labels
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_argument)
        call read_string("Move to previous msg with labels: ", labels, status)
        if (status%ok()) call previous_rmail_summary_labeled_message(n, labels, status)
    end subroutine execute_previous_rmail_summary_labeled_message

    recursive subroutine execute_delete_rmail_summary_message_forward(status)
        type(status_type), intent(out) :: status

        call delete_rmail_summary_message_forward(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_delete_rmail_summary_message_forward

    recursive subroutine execute_delete_rmail_summary_message_backward(status)
        type(status_type), intent(out) :: status

        call delete_rmail_summary_message_backward(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_delete_rmail_summary_message_backward

    recursive subroutine execute_undelete_rmail_summary(status)
        type(status_type), intent(out) :: status

        call undelete_rmail_summary(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_undelete_rmail_summary

    recursive subroutine execute_undelete_rmail_summary_many(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call undelete_rmail_summary_many(status)
        else
            call undelete_rmail_summary_many(status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_undelete_rmail_summary_many

    recursive subroutine execute_goto_rmail_summary_message(status)
        type(status_type), intent(out) :: status
        logical :: found

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call goto_rmail_summary_message(status, found=found)
        else
            call goto_rmail_summary_message(status, prefix_numeric_value(current_prefix_argument), found=found)
        end if
    end subroutine execute_goto_rmail_summary_message

    recursive subroutine execute_scroll_rmail_summary_message_up(status)
        type(status_type), intent(out) :: status

        call scroll_rmail_summary_message_up(current_prefix_argument, status)
    end subroutine execute_scroll_rmail_summary_message_up

    recursive subroutine execute_scroll_rmail_summary_message_down(status)
        type(status_type), intent(out) :: status

        call scroll_rmail_summary_message_down(current_prefix_argument, status)
    end subroutine execute_scroll_rmail_summary_message_down

    recursive subroutine execute_get_rmail_summary_new_mail(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call read_file_name("Get new mail from file: ", file, status)
            if (status%ok()) call get_rmail_summary_new_mail(status, file)
        else
            call get_rmail_summary_new_mail(status)
        end if
    end subroutine execute_get_rmail_summary_new_mail

    recursive subroutine execute_rmail_summary_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Run rmail on RMAIL file: ", file, status)
        if (status%ok()) call rmail_summary_input(file, status)
    end subroutine execute_rmail_summary_input

    recursive subroutine execute_search_rmail_summary(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer(int64) :: n

        call read_rmail_search(.false., regular_expression, n, status)
        if (status%ok()) call search_rmail_summary(regular_expression, status, n)
    end subroutine execute_search_rmail_summary

    recursive subroutine execute_search_rmail_summary_backward(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer(int64) :: n

        call read_rmail_search(.true., regular_expression, n, status)
        if (status%ok()) call search_rmail_summary_backward(regular_expression, status, n)
    end subroutine execute_search_rmail_summary_backward

    !> Add LABEL to labels associated with current Rmail message.
    !> Completion is performed over known labels when reading.  Port of
    !> rmailsum.el:rmail-summary-add-label.
    recursive subroutine execute_add_rmail_summary_label(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: label
        type(handle_type) :: saved
        type(status_type) :: st

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call read_rmail_label("Add label", label, status)
        if (status%ok()) call add_rmail_label(label, status)
        call set_buffer(saved, st)
    end subroutine execute_add_rmail_summary_label

    !> Remove LABEL from labels associated with current Rmail message.
    !> Completion is performed over known labels when reading.  Port of
    !> rmailsum.el:rmail-summary-kill-label.
    recursive subroutine execute_remove_rmail_summary_label(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: label
        type(handle_type) :: saved
        type(status_type) :: st

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call read_rmail_label("Kill label", label, status)
        if (status%ok()) call set_rmail_label(label, .false., status)
        call set_buffer(saved, st)
    end subroutine execute_remove_rmail_summary_label

    ! Output N messages from the summary to FILE_NAME with output_rmail_message
    ! (or, if AS_SEEN, output_rmail_message_as_seen), moving the summary along.
    ! Port of rmailsum.el:rmail-summary-output and
    ! rmailsum.el:rmail-summary-output-as-seen.
    recursive subroutine output_from_summary(file_name, n, as_seen, status)
        use rmail_mod, only: rmail_delete_after_output
        character(len=*), intent(in) :: file_name
        integer(int64), intent(in) :: n
        logical, intent(in) :: as_seen
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: i, previous_message, current_message
        logical :: found, saved_delete
        integer :: k

        i = 0
        previous_message = -1

        do while (i < n)
            call goto_rmail_summary_message(status, found=found)
            if (.not. status%ok()) return
            k = rmail_index()
            if (rmail_states(k)%current_message == previous_message) exit
            previous_message = rmail_states(k)%current_message
            i = i + 1
            saved = current_buffer()
            call set_buffer(rmail_buffer(), status)
            if (.not. status%ok()) return
            saved_delete = rmail_delete_after_output
            rmail_delete_after_output = .false.
            current_message = rmail_states(k)%current_message

            if (as_seen) then
                call output_rmail_message_as_seen(file_name, status, 1_int64)
            else
                call output_rmail_message(file_name, status, 1_int64)
            end if

            rmail_delete_after_output = saved_delete
            call set_buffer(saved, st)
            if (.not. status%ok()) return

            ! rmail-output sometimes moves to the next message; undo that.
            if (.not. as_seen .and. current_message /= rmail_summary_message_number()) then
                call goto_rmail_summary_message(status, current_message, found=found)
                if (.not. status%ok()) return
            end if

            if (rmail_delete_after_output) then
                call delete_rmail_summary_message_forward(status)
            else if (i < n) then
                call next_rmail_summary_message(1_int64, status)
            end if

            if (.not. status%ok()) return
        end do
    end subroutine output_from_summary

    !> Append this message to mail file FILE_NAME.
    !> This works with both mbox format and Babyl format files,
    !> outputting in the appropriate format for each.
    !> The default file name comes from `rmail_default_file',
    !> which is updated to the name you use in this command.
    !>
    !> A prefix argument N says to output that many consecutive messages
    !> from those in the summary, starting with the current one.
    !> Deleted messages are skipped and don't count.
    !> When called from a program, N may be omitted and defaults to 1.
    !>
    !> This command always outputs the complete message header,
    !> even if the header display is currently pruned.  Port of
    !> rmailsum.el:rmail-summary-output.
    recursive subroutine execute_output_rmail_summary_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_argument)
        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call read_rmail_output_file_name(file, status)
        call set_buffer(saved, st)
        if (status%ok()) call output_from_summary(file, n, .false., status)
    end subroutine execute_output_rmail_summary_message

    !> Append this message to mbox file named FILE_NAME.
    !> A prefix argument N says to output that many consecutive messages,
    !> from the summary, starting with the current one.
    !> Deleted messages are skipped and don't count.
    !> When called from a program, N may be omitted and defaults to 1.
    !>
    !> This outputs the message header as you see it (or would see it)
    !> displayed in Rmail.
    !>
    !> The default file name comes from `rmail_default_file',
    !> which is updated to the name you use in this command.  Port of
    !> rmailsum.el:rmail-summary-output-as-seen.
    recursive subroutine execute_output_rmail_summary_message_as_seen(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_argument)
        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (status%ok()) call read_rmail_output_file_name(file, status)
        call set_buffer(saved, st)
        if (status%ok()) call output_from_summary(file, n, .true., status)
    end subroutine execute_output_rmail_summary_message_as_seen

    !> Write this message body to the file FILE_NAME.
    !> FILE_NAME defaults, interactively, from the Subject field of the
    !> message.  Port of rmailsum.el:rmail-summary-output-body.
    recursive subroutine execute_output_rmail_summary_message_body(status)
        use rmail_mod, only: rmail_delete_after_output
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st
        logical :: saved_delete

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        saved_delete = rmail_delete_after_output
        rmail_delete_after_output = .false.
        if (status%ok()) call commands%output_rmail_message_body_to_file%execute(status)
        rmail_delete_after_output = saved_delete
        call set_buffer(saved, st)
        if (.not. status%ok()) return
        if (rmail_delete_after_output) call delete_rmail_summary_message_forward(status)
    end subroutine execute_output_rmail_summary_message_body

    !> Define Rmail Summary mode, its keymap and commands.  Not a port: the
    !> top-level forms of rmailsum.el.
    subroutine initialize_rmail_summary()
        type(status_type) :: st
        type(handle_type) :: map

        rmail_summary_sender_function => strip_rmail_summary_sender
        rmail_summary_recipient_function => strip_rmail_summary_recipient_quoted_names
        call major_modes%rmail_summary_mode%define("rmail_summary_mode", "RMAIL Summary", rmail_summary_mode, &
                                                   rmail_summary_mode_hook, parent=major_modes%special_mode)
        ! Rmail Summary mode is suitable only for specially formatted data.
        major_modes%rmail_summary_mode%special = .true.
        rmail_summary_mode_map = major_modes%rmail_summary_mode%map
        map = rmail_summary_mode_map
        call keymap_set(map, "a", "add_rmail_summary_label", st)
        call keymap_set(map, "b", "bury_rmail_summary", st)
        call keymap_set(map, "d", "delete_rmail_summary_message_forward", st)
        call keymap_set(map, "C-d", "delete_rmail_summary_message_backward", st)
        call keymap_set(map, "e", "edit_rmail_summary_current_message", st)
        call keymap_set(map, "g", "get_rmail_summary_new_mail", st)
        call keymap_set(map, "h", "rmail_summary", st)
        call keymap_set(map, "i", "rmail_summary_input", st)
        call keymap_set(map, "j", "goto_rmail_summary_message", st)
        call keymap_set(map, "RET", "goto_rmail_summary_message", st)
        call keymap_set(map, "k", "remove_rmail_summary_label", st)
        call keymap_set(map, "l", "rmail_summary_by_labels", st)
        call keymap_set(map, "C-M-h", "rmail_summary", st)
        call keymap_set(map, "C-M-l", "rmail_summary_by_labels", st)
        call keymap_set(map, "C-M-r", "rmail_summary_by_recipients", st)
        ! `f' for "from".
        call keymap_set(map, "C-M-f", "rmail_summary_by_senders", st)
        call keymap_set(map, "n", "next_rmail_summary_message", st)
        call keymap_set(map, "M-n", "next_rmail_summary_all", st)
        call keymap_set(map, "C-M-n", "next_rmail_summary_labeled_message", st)
        call keymap_set(map, "o", "output_rmail_summary_message", st)
        call keymap_set(map, "C-o", "output_rmail_summary_message_as_seen", st)
        call keymap_set(map, "p", "previous_rmail_summary_message", st)
        call keymap_set(map, "M-p", "previous_rmail_summary_all", st)
        call keymap_set(map, "C-M-p", "previous_rmail_summary_labeled_message", st)
        call keymap_set(map, "q", "quit_rmail_summary", st)
        call keymap_set(map, "Q", "wipe_rmail_summary", st)
        call keymap_set(map, "s", "expunge_and_save_rmail_summary", st)
        call keymap_set(map, "M-s", "search_rmail_summary", st)
        call keymap_set(map, "t", "toggle_rmail_summary_header", st)
        call keymap_set(map, "u", "undelete_rmail_summary", st)
        call keymap_set(map, "M-u", "undelete_rmail_summary_many", st)
        call keymap_set(map, "x", "expunge_rmail_summary", st)
        call keymap_set(map, "w", "output_rmail_summary_message_body", st)
        call keymap_set(map, ".", "beginning_of_rmail_summary_message", st)
        call keymap_set(map, "/", "end_of_rmail_summary_message", st)
        call keymap_set(map, "<", "show_first_rmail_summary_message", st)
        call keymap_set(map, ">", "show_last_rmail_summary_message", st)
        call keymap_set(map, "SPC", "scroll_rmail_summary_message_up", st)
        call keymap_set(map, "S-SPC", "scroll_rmail_summary_message_down", st)
        call keymap_set(map, "DEL", "scroll_rmail_summary_message_down", st)
        call keymap_set(map, "?", "describe_mode", st)
        call keymap_set(map, "C-x C-s", "save_rmail_summary_buffer", st)
        call make_sparse_keymap(rmail_summary_edit_map)
        call set_keymap_parent(rmail_summary_edit_map, text_mode_map, st)
        call keymap_set(rmail_summary_edit_map, "C-c C-c", "cease_rmail_edit", st)
        call keymap_set(rmail_summary_edit_map, "C-c C-]", "abort_rmail_edit", st)
        commands%rmail_summary%execute => rmail_summary
        commands%rmail_summary_mode%execute => rmail_summary_mode
        commands%rmail_summary_by_labels%execute => execute_rmail_summary_by_labels
        commands%rmail_summary_by_recipients%execute => execute_rmail_summary_by_recipients
        commands%rmail_summary_by_senders%execute => execute_rmail_summary_by_senders
        commands%next_rmail_summary_all%execute => execute_next_rmail_summary_all
        commands%previous_rmail_summary_all%execute => execute_previous_rmail_summary_all
        commands%next_rmail_summary_message%execute => execute_next_rmail_summary_message
        commands%previous_rmail_summary_message%execute => execute_previous_rmail_summary_message
        commands%next_rmail_summary_labeled_message%execute => execute_next_rmail_summary_labeled_message
        commands%previous_rmail_summary_labeled_message%execute => execute_previous_rmail_summary_labeled_message
        commands%delete_rmail_summary_message_forward%execute => execute_delete_rmail_summary_message_forward
        commands%delete_rmail_summary_message_backward%execute => execute_delete_rmail_summary_message_backward
        commands%undelete_rmail_summary%execute => execute_undelete_rmail_summary
        commands%undelete_rmail_summary_many%execute => execute_undelete_rmail_summary_many
        commands%save_rmail_summary_buffer%execute => save_rmail_summary_buffer
        commands%goto_rmail_summary_message%execute => execute_goto_rmail_summary_message
        commands%scroll_rmail_summary_message_up%execute => execute_scroll_rmail_summary_message_up
        commands%scroll_rmail_summary_message_down%execute => execute_scroll_rmail_summary_message_down
        commands%beginning_of_rmail_summary_message%execute => beginning_of_rmail_summary_message
        commands%end_of_rmail_summary_message%execute => end_of_rmail_summary_message
        commands%bury_rmail_summary%execute => bury_rmail_summary
        commands%quit_rmail_summary%execute => quit_rmail_summary
        commands%wipe_rmail_summary%execute => wipe_rmail_summary
        commands%expunge_rmail_summary%execute => expunge_rmail_summary
        commands%expunge_and_save_rmail_summary%execute => expunge_and_save_rmail_summary
        commands%get_rmail_summary_new_mail%execute => execute_get_rmail_summary_new_mail
        commands%rmail_summary_input%execute => execute_rmail_summary_input
        commands%show_first_rmail_summary_message%execute => show_first_rmail_summary_message
        commands%show_last_rmail_summary_message%execute => show_last_rmail_summary_message
        commands%edit_rmail_summary_current_message%execute => edit_rmail_summary_current_message
        commands%cease_rmail_summary_edit%execute => cease_rmail_summary_edit
        commands%abort_rmail_summary_edit%execute => abort_rmail_summary_edit
        commands%search_rmail_summary%execute => execute_search_rmail_summary
        commands%search_rmail_summary_backward%execute => execute_search_rmail_summary_backward
        commands%toggle_rmail_summary_header%execute => toggle_rmail_summary_header
        commands%add_rmail_summary_label%execute => execute_add_rmail_summary_label
        commands%remove_rmail_summary_label%execute => execute_remove_rmail_summary_label
        commands%output_rmail_summary_message%execute => execute_output_rmail_summary_message
        commands%output_rmail_summary_message_as_seen%execute => execute_output_rmail_summary_message_as_seen
        commands%output_rmail_summary_message_body%execute => execute_output_rmail_summary_message_body
        goto_rmail_summary_message_function => goto_message_for_rmail
        mark_rmail_summary_deleted_function => mark_rmail_summary_deleted
        mark_rmail_summary_undeleted_function => mark_rmail_summary_undeleted
        update_rmail_summary_line_function => update_rmail_summary_line
        update_rmail_summary_function => update_rmail_summary
        rmail_summary_function => make_rmail_summary_for_rmail
        disable_rmail_summary_function => disable_rmail_summary
        enable_rmail_summary_function => enable_rmail_summary
    end subroutine initialize_rmail_summary
end module rmail_summary_mod
