! rmail_keywords_mod.f90 -- labels (keywords) of Rmail messages.
! Ported from GNU Emacs 31.1 lisp/mail/rmailkwd.el.
!
! Copyright (C) 1985, 1988, 1994, 2001-2026 Free Software Foundation,
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

!> Labels of Rmail messages: \[rmail_add_label] (a) and
!> \[rmail_remove_label] (k) add and remove one label of the current
!> message, in its X-RMAIL-KEYWORDS header field (the attribute names,
!> such as "deleted", set the attribute instead), and
!> \[rmail_next_labeled_message] (C-M-n) and
!> \[rmail_previous_labeled_message] (C-M-p) move to messages with given
!> labels.  The labels seen so far are offered for completion.
module rmail_keywords_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_arg, prefix_numeric_value
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type
    use string_type_mod, only: string_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer
    use search_mod, only: string_match, case_fold_search
    use character_mod, only: downcase_ascii
    use value_mod, only: value_type, make_list
    use minibuffer_mod, only: completing_read
    use minibuffer_reading_mod, only: read_string
    use minibuffer_reading_tables_mod, only: completion_table_type, TABLE_LIST
    use replace_mod, only: format_prompt
    use mail_utilities_mod, only: mail_comma_list_regexp
    use rmail_mod, only: rmail_buffer, rmail_state_index, rmail_states, rmail_get_keywords, rmail_get_labels, &
                         rmail_set_attribute, rmail_set_header, rmail_display_labels, rmail_summary_exists, &
                         rmail_select_summary, rmail_summary_update_line_function, rmail_maybe_set_message_counters, &
                         rmail_show_message_1, RMAIL_ATTRIBUTE_NAMES, RMAIL_KEYWORD_HEADER, rmail_last_label, &
                         rmail_last_multi_labels
    implicit none(type, external)
    private

    public :: rmail_add_label, rmail_remove_label, read_rmail_label, rmail_set_label, rmail_next_labeled_message, &
              rmail_previous_labeled_message, append_rmail_label, init_rmail_keywords

    !> The labels used by Rmail, all in lower case, starting with the
    !> attribute names; `read_rmail_label' offers them for completion.
    !> Port of rmailkwd.el:rmail-label-obarray (a name table).
    type(string_type), allocatable, public :: rmail_labels(:)

contains

    !> Add the downcased label S to `rmail_labels' unless it is there;
    !> LABEL is it.  Port of rmailkwd.el:rmail-make-label.
    subroutine append_rmail_label(s, label)
        character(len=*), intent(in) :: s
        character(len=:), allocatable, intent(out) :: label
        type(string_type), allocatable :: grown(:)
        integer :: i, n

        label = downcase_ascii(s)
        if (.not. allocated(rmail_labels)) allocate (rmail_labels(0))

        do i = 1, size(rmail_labels)
            if (rmail_labels(i)%text == label) return
        end do

        n = size(rmail_labels)
        allocate (grown(n + 1))
        grown(1:n) = rmail_labels
        grown(n + 1)%text = label
        call move_alloc(grown, rmail_labels)
    end subroutine append_rmail_label

    !> Add LABEL to labels associated with current RMAIL message.
    !> Completes (see `read_rmail_label') over known labels when reading.
    !> Only one label is allowed.  Port of rmailkwd.el:rmail-add-label.
    recursive subroutine rmail_add_label(label, status)
        character(len=*), intent(in) :: label
        type(status_type), intent(out) :: status

        call rmail_set_label(label, .true., status)
    end subroutine rmail_add_label

    !> Remove LABEL from labels associated with current RMAIL message.
    !> Completes (see `read_rmail_label') over known labels when reading.
    !> Only one label is allowed.  Port of rmailkwd.el:rmail-kill-label.
    recursive subroutine rmail_remove_label(label, status)
        character(len=*), intent(in) :: label
        type(status_type), intent(out) :: status

        call rmail_set_label(label, .false., status)
    end subroutine rmail_remove_label

    !> Read a label with completion, prompting with PROMPT.
    !> Completions are chosen from `rmail_labels'.  The default
    !> is `rmail_last_label', if that is allocated.  Updates `rmail_last_label'
    !> according to the choice made, and returns it in LABEL.  Port of
    !> rmailkwd.el:rmail-read-label.
    recursive subroutine read_rmail_label(prompt, label, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: label
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old, result, made
        type(value_type), allocatable :: names(:)
        type(completion_table_type) :: table
        integer :: i, start, comma

        ! If the summary exists, we've already read all the existing
        ! labels.  If not, read the ones in this message.
        if (buffers(current)%major_mode /= "rmail_summary_mode" .and. .not. rmail_summary_exists()) then
            call rmail_get_keywords(old, status)
            if (.not. status%ok()) return

            if (allocated(old)) then
                start = 1

                do
                    comma = index(old(start:), ", ")

                    if (comma == 0) then
                        if (start <= len(old)) call append_rmail_label(old(start:), made)
                        exit
                    end if

                    if (comma > 1) call append_rmail_label(old(start:start + comma - 2), made)
                    start = start + comma + 1
                end do
            end if
        end if

        call append_rmail_attribute_labels()
        allocate (names(size(rmail_labels)))

        do i = 1, size(rmail_labels)
            names(i) = value_type(text=rmail_labels(i)%text)
        end do

        table%kind = TABLE_LIST
        table%list = make_list(names)

        if (allocated(rmail_last_label)) then
            call completing_read(format_prompt(prompt, rmail_last_label), table, result, status)
        else
            call completing_read(format_prompt(prompt), table, result, status)
        end if

        if (.not. status%ok()) return

        if (len(result) == 0) then
            label = ""
            if (allocated(rmail_last_label)) label = rmail_last_label
        else
            call append_rmail_label(result, made)
            rmail_last_label = made
            label = made
        end if
    end subroutine read_rmail_label

    ! Initialize `rmail_labels' with the standard labels.  Port of the
    ! top-level mapc of rmailkwd.el.
    subroutine append_rmail_attribute_labels()
        character(len=:), allocatable :: made
        integer :: i

        if (allocated(rmail_labels)) then
            if (size(rmail_labels) > 0) return
        end if

        do i = 1, size(RMAIL_ATTRIBUTE_NAMES)
            call append_rmail_label(trim(RMAIL_ATTRIBUTE_NAMES(i)), made)
        end do
    end subroutine append_rmail_attribute_labels

    !> Set LABEL as present or absent according to STATE in message MSG
    !> (the current message if absent).  Port of rmailkwd.el:rmail-set-label.
    recursive subroutine rmail_set_label(label, state, status, msg)
        character(len=*), intent(in) :: label
        logical, intent(in) :: state
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: msg
        character(len=:), allocatable :: header, before, after, padded
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: n, match_start, match_end
        integer :: k, attribute_index
        logical :: present_now

        if (index(label, ",") > 0) then
            call set_error(status, FM_GENERIC_ERROR, "More than one label specified")
            return
        end if

        saved = current_buffer()
        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        call rmail_maybe_set_message_counters()
        k = rmail_state_index(rmail_buffer())
        n = rmail_states(k)%current_message
        if (present(msg)) n = msg

        if (n == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No message")
            call set_buffer(saved, st)
            return
        end if

        ! Force recalculation of summary for this message.
        if (allocated(rmail_states(k)%summaries(n)%text)) deallocate (rmail_states(k)%summaries(n)%text)
        ! Is this label an attribute?
        attribute_index = findloc(RMAIL_ATTRIBUTE_NAMES == label, .true., dim=1)

        if (attribute_index > 0) then
            ! If so, set it as an attribute.
            call rmail_set_attribute(attribute_index - 1, state, status, n)
        else
            ! Is this keyword already present in msg's keyword list?
            call rmail_get_keywords(header, status, n)
            if (.not. status%ok()) return
            if (.not. allocated(header)) header = ""
            padded = ", "//header//","

            ! string-match with `case_fold_search' (ASCII letters).
            if (case_fold_search%value) then
                match_start = index(downcase_ascii(padded), downcase_ascii(", "//label//","))
            else
                match_start = index(padded, ", "//label//",")
            end if

            present_now = match_start > 0

            ! If current state is not correct, either add it or delete it.
            if (present_now .neqv. state) then
                if (state) then
                    ! Add this keyword at the end.
                    if (len(header) > 0) then
                        call rmail_set_header(RMAIL_KEYWORD_HEADER, status, n, header//", "//label)
                    else
                        call rmail_set_header(RMAIL_KEYWORD_HEADER, status, n, label)
                    end if
                else
                    ! Delete this keyword: the match of ", LABEL," in
                    ! ", HEADER," starts at MATCH_START (0-based in the
                    ! padded text: MATCH_START - 1).
                    match_end = match_start + len(label) + 3
                    before = header(1:max(0_int64, match_start - 1 - 2))
                    after = header(min(int(len(header), int64), match_end - 1 - 1) + 1:)

                    if (len(before) == 0) then
                        ! If before and after both empty, delete the header.
                        if (len(after) == 0) then
                            call rmail_set_header(RMAIL_KEYWORD_HEADER, status, n)
                        else
                            call rmail_set_header(RMAIL_KEYWORD_HEADER, status, n, after)
                        end if
                    else if (len(after) == 0) then
                        call rmail_set_header(RMAIL_KEYWORD_HEADER, status, n, before)
                    else
                        call rmail_set_header(RMAIL_KEYWORD_HEADER, status, n, before//", "//after)
                    end if
                end if
            end if
        end if

        if (.not. status%ok()) return

        if (rmail_summary_exists()) then
            call rmail_select_summary(rmail_summary_update_line_function, n, status)
            if (.not. status%ok()) return
        end if

        if (n == rmail_states(k)%current_message) call rmail_display_labels(status)
        if (status%ok()) call set_buffer(saved, status)
    end subroutine rmail_set_label

    !> Show previous message with one of the labels LABELS.
    !> LABELS should be a comma-separated list of label names.
    !> If LABELS is empty, the last set of labels specified is used.
    !> With prefix argument N moves backward N messages with these labels.
    !> Port of rmailkwd.el:rmail-previous-labeled-message.
    recursive subroutine rmail_previous_labeled_message(n, labels, status)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: labels
        type(status_type), intent(out) :: status

        call rmail_next_labeled_message(-n, labels, status)
    end subroutine rmail_previous_labeled_message

    !> Show next message with one of the labels LABELS.
    !> LABELS should be a comma-separated list of label names.
    !> If LABELS is empty, the last set of labels specified is used.
    !> With prefix argument N moves forward N messages with these labels.
    !> Port of rmailkwd.el:rmail-next-labeled-message.
    recursive subroutine rmail_next_labeled_message(n, labels, status)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: labels
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: wanted, regexp, text, blurb
        character(len=200) :: line
        integer(int64) :: count, last_win, at
        integer :: k

        if (len(labels) == 0) then
            if (.not. allocated(rmail_last_multi_labels)) then
                call set_error(status, FM_GENERIC_ERROR, "No labels to find have been specified previously")
                return
            end if

            wanted = rmail_last_multi_labels
        else
            wanted = labels
        end if

        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        rmail_last_multi_labels = wanted
        call rmail_maybe_set_message_counters()
        k = rmail_state_index(rmail_buffer())
        count = n
        last_win = rmail_states(k)%current_message
        at = rmail_states(k)%current_message
        regexp = " \("//mail_comma_list_regexp(wanted)//"\)\(,\|\'\)"

        do while (count > 0 .and. at < rmail_states(k)%total_messages)
            at = at + 1
            call rmail_get_labels(text, status, at)
            if (.not. status%ok()) return

            if (string_match(regexp, text, status) > 0) then
                last_win = at
                count = count - 1
            end if
        end do

        do while (count < 0 .and. at > 1)
            at = at - 1
            call rmail_get_labels(text, status, at)
            if (.not. status%ok()) return

            if (string_match(regexp, text, status) > 0) then
                last_win = at
                count = count + 1
            end if
        end do

        if (count < 0) then
            write (line, '("No previous message with labels ", a)') wanted
            call set_error(status, FM_GENERIC_ERROR, trim(line))
        else if (count > 0) then
            write (line, '("No following message with labels ", a)') wanted
            call set_error(status, FM_GENERIC_ERROR, trim(line))
        else
            call rmail_show_message_1(blurb, status, last_win)
        end if
    end subroutine rmail_next_labeled_message

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_rmail_add_label(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: label

        call read_rmail_label("Add label", label, status)
        if (status%ok()) call rmail_add_label(label, status)
    end subroutine cmd_rmail_add_label

    recursive subroutine cmd_rmail_remove_label(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: label

        call read_rmail_label("Remove label", label, status)
        if (status%ok()) call rmail_remove_label(label, status)
    end subroutine cmd_rmail_remove_label

    recursive subroutine cmd_rmail_next_labeled_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: labels
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_arg)
        call read_string("Move to next msg with labels: ", labels, status)
        if (status%ok()) call rmail_next_labeled_message(n, labels, status)
    end subroutine cmd_rmail_next_labeled_message

    recursive subroutine cmd_rmail_previous_labeled_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: labels
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_arg)
        call read_string("Move to previous msg with labels: ", labels, status)
        if (status%ok()) call rmail_previous_labeled_message(n, labels, status)
    end subroutine cmd_rmail_previous_labeled_message

    !> Define the commands of rmailkwd.el.  Not a port.
    subroutine init_rmail_keywords()
        call append_rmail_attribute_labels()
        commands%rmail_add_label%execute => cmd_rmail_add_label
        commands%rmail_remove_label%execute => cmd_rmail_remove_label
        commands%rmail_next_labeled_message%execute => cmd_rmail_next_labeled_message
        commands%rmail_previous_labeled_message%execute => cmd_rmail_previous_labeled_message
    end subroutine init_rmail_keywords
end module rmail_keywords_mod
