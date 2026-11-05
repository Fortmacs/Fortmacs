! rmail_edit_mod.f90 -- "RMAIL edit mode": editing the current Rmail message.
! Ported from GNU Emacs 31.1 lisp/mail/rmailedit.el.
!
! Copyright (C) 1985, 1994, 2001-2026 Free Software Foundation, Inc.
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

!> Editing the current Rmail message: \[edit_rmail_current_message] (e)
!> puts the Rmail buffer, which shows the message, in Rmail Edit mode
!> (Text mode's commands); \[cease_rmail_edit] (C-c C-c) puts the edited
!> message back into the message collection, encoded as the message
!> says, applying the changes to its header fields, and
!> \[abort_rmail_edit] (C-c C-]) restores the message.  See
!> docs/status/rmail_edit_mod.md for what is deferred.
module rmail_edit_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type, null_handle
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_serial_live
    use editing_functions_mod, only: point, goto_char, point_min, point_max, buffer_substring, &
                                     buffer_substring_no_properties, delete_region, narrow_to_region, widen, is_eob, &
                                     save_restriction_begin, save_restriction_end, save_excursion_begin, save_excursion_end, &
                                     restriction_type, excursion_type
    use insert_delete_mod, only: insert, insert_before_markers
    use editing_commands_mod, only: forward_line, beginning_of_line
    use search_mod, only: re_search_forward, search_forward, looking_at, match_beginning, match_end, case_fold_search, &
                          SEARCH_NOERROR, SEARCH_MOVE
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use keymap_mod, only: keymap_set, use_local_map, make_sparse_keymap, set_keymap_parent
    use modes_mod, only: major_modes
    use basic_subprograms_mod, only: execute_mode_hooks, delay_mode_hooks_begin, delay_mode_hooks_end
    use data_mod, only: clear_all_local_variables
    use text_mode_mod, only: text_mode_map
    use display_engine_mod, only: message
    use help_mod, only: substitute_command_keys
    use coding_mod, only: buffer_file_coding_system, coding_system_base, coding_system_equal, coding_system_mime_charset, &
                          encode_coding_string
    use multilingual_environment_mod, only: coding_system_from_name
    use file_input_output_mod, only: write_region_annotate_functions, select_safe_coding_system_function
    use character_mod, only: str_to_multibyte, downcase_ascii
    use mail_utilities_mod, only: get_mail_field, quote_mail_printable_region
    use utility_functions_mod, only: base64_encode_region
    use rmail_mod, only: rmail_states, rmail_state_index, rmail_buffer, set_up_rmail_mode_1, set_rmail_variables, &
                         modify_rmail_format, is_rmail_message_pruned, rmail_summary_exists, disable_rmail_summary_function, &
                         enable_rmail_summary_function, update_rmail_summary_line_function, rmail_message_beginning, &
                         rmail_message_end, swap_rmail_buffers_maybe, is_rmail_text, get_rmail_coding_system, &
                         ensure_rmail_blank_line, set_rmail_attribute, show_rmail_message, toggle_rmail_header, &
                         highlight_rmail_headers, annotate_rmail_write_region, rmail_mode_hook, rmail_mime_charset_pattern, &
                         rmail_mbox_format, RMAIL_MBOX_FORMAT_MBOXO, RMAIL_EDITED_ATTRIBUTE_INDEX
    implicit none(type, external)
    private

    public :: rmail_edit_mode, edit_rmail_current_message, cease_rmail_edit, abort_rmail_edit, get_rmail_edit_headers, &
              diff_rmail_edit_headers, update_rmail_edit_headers, init_rmail_edit

    !> List of functions to call when editing an RMAIL message.  Port of
    !> rmailedit.el rmail-edit-mode-hook.
    type(hook_type), target, public :: rmail_edit_mode_hook = hook_type(name="rmail_edit_mode_hook")

    !> The keymap of Rmail Edit mode.  Port of rmailedit.el rmail-edit-map.
    type(handle_type), public :: rmail_edit_map

    !> A header field of a message: its name (`invalid' when the text is
    !> not valid as a header field: then `number' counts them, from 1),
    !> its entire text with the final newline and, when wanted, markers
    !> at its two ends, as upstream puts them (`first' and `second').
    !> Upstream's elements of rmailedit.el:rmail-edit-headers-alist.
    type, public :: rmail_header_field_type
        character(len=:), allocatable :: name
        logical :: invalid = .false.
        integer :: number = 0
        character(len=:), allocatable :: text
        type(handle_type) :: first, second
    end type rmail_header_field_type

    !> A header field inserted while editing (NEW), after the field AFTER
    !> (`at_start' when it goes at the beginning).  Upstream's
    !> (AFTER-WHAT INSERT-WHAT).
    type, public :: rmail_header_insertion_type
        logical :: at_start = .false.
        type(rmail_header_field_type) :: after, new
    end type rmail_header_insertion_type

    !> The differences between two headers: the fields inserted, deleted
    !> (of the old) and changed (old and new).  Upstream's (INSERTED
    !> DELETED CHANGED).
    type, public :: rmail_header_diff_type
        type(rmail_header_insertion_type), allocatable :: inserted(:)
        type(rmail_header_field_type), allocatable :: deleted(:)
        type(rmail_header_field_type), allocatable :: changed_old(:), changed_new(:)
    end type rmail_header_diff_type

    ! What an Rmail buffer being edited keeps (permanent buffer-local
    ! variables upstream: rmail-old-text, rmail-old-pruned,
    ! rmail-old-headers), by serial number.
    type :: edit_state_type
        integer(int64) :: serial = 0
        character(len=:), allocatable :: old_text
        logical :: old_pruned = .false.
        type(rmail_header_field_type), allocatable :: old_headers(:)
    end type edit_state_type

    type(edit_state_type), allocatable :: edit_states(:)

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    ! The index of the current buffer's edit state, made when MAKE.
    ! Not a port.
    integer function edit_state_index(make) result(k)
        logical, intent(in) :: make
        type(edit_state_type) :: entry

        k = 0
        if (current == 0) return
        if (.not. allocated(edit_states)) allocate (edit_states(0))
        k = findloc(edit_states%serial, buffers(current)%serial, dim=1)
        if (k > 0 .or. .not. make) return

        do k = 1, size(edit_states)
            if (.not. buffer_serial_live(edit_states(k)%serial)) exit
        end do

        if (k > size(edit_states)) edit_states = [edit_states, entry]
        edit_states(k) = entry
        edit_states(k)%serial = buffers(current)%serial
    end function edit_state_index

    !> Major mode for editing the contents of an Rmail message.
    !> The editing commands are the same as in Text mode, together with
    !> two commands to return to regular Rmail:
    !>   *  \[abort_rmail_edit] cancels any changes and returns to Rmail
    !>   *  \[cease_rmail_edit] makes them permanent.
    !> This function runs the hooks `text_mode_hook' and `rmail_edit_mode_hook'.
    !> \{rmail_edit_map}
    !> Port of rmailedit.el:rmail-edit-mode.
    recursive subroutine rmail_edit_mode(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(status_type) :: st
        logical :: delayed, swapped
        integer :: k

        status = status_type()

        if (rmail_summary_exists()) then
            k = rmail_state_index(rmail_buffer())
            saved = current_buffer()
            call set_buffer(rmail_states(k)%summary_buffer, status)

            if (status%ok() .and. associated(disable_rmail_summary_function)) then
                call disable_rmail_summary_function(0_int64, status)
            end if

            call set_buffer(saved, st)
            if (.not. status%ok()) return
        end if

        ! Prevent change-major-mode-hook from unswapping the buffers.
        k = rmail_state_index(current_buffer())
        swapped = .false.

        if (k > 0) then
            swapped = rmail_states(k)%swapped
            rmail_states(k)%swapped = .false.
        end if

        call delay_mode_hooks_begin(delayed)
        call major_modes%text_mode%execute(status)
        call delay_mode_hooks_end(delayed)
        if (k > 0) rmail_states(k)%swapped = swapped
        if (.not. status%ok()) return
        call use_local_map(rmail_edit_map, status)
        if (.not. status%ok()) return
        buffers(current)%major_mode = "rmail_edit_mode"
        buffers(current)%mode_name = "RMAIL Edit"
        ! If someone uses C-x C-s, don't clobber the rmail file.
        call write_region_annotate_functions%prepend(annotate_rmail_write_region, local=.true.)
        call execute_mode_hooks(status, rmail_edit_mode_hook)
    end subroutine rmail_edit_mode

    !> Edit the contents of this message.  Port of
    !> rmailedit.el:rmail-edit-current-message (the MIME state waits for
    !> rmailmm.el).
    recursive subroutine edit_rmail_current_message(status)
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction
        logical :: pruned
        integer :: k, e

        k = rmail_state_index(rmail_buffer())

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No messages in this buffer")
            return
        end if

        if (rmail_states(k)%total_messages == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No messages in this buffer")
            return
        end if

        call modify_rmail_format(status)
        if (.not. status%ok()) return
        pruned = is_rmail_message_pruned()
        e = edit_state_index(.true.)
        edit_states(e)%old_pruned = pruned
        call rmail_edit_mode(status)
        if (.not. status%ok()) return
        e = edit_state_index(.true.)
        call save_restriction_begin(restriction)
        call widen()
        call buffer_substring(point_min(), point_max(), edit_states(e)%old_text, status)
        call save_restriction_end(restriction)
        if (.not. status%ok()) return
        call get_rmail_edit_headers(edit_states(e)%old_headers, status, widen_first=.true.)
        if (.not. status%ok()) return
        buffers(current)%read_only = .false.
        buffers(current)%undo_enabled = .true.
        buffers(current)%undo_count = 0
        ! Whether the buffer is initially marked as modified or not depends
        ! on whether or not the underlying rmail buffer was so marked.
        call message(substitute_command_keys("Editing: Type \[cease_rmail_edit] to return to Rmail, "// &
                                             "\[abort_rmail_edit] to abort"))
    end subroutine edit_rmail_current_message

    !> Finish editing message; switch back to Rmail proper.
    !> If ABORT, this is the result of aborting an edit.  Port of
    !> rmailedit.el:rmail-cease-edit (restoring a MIME state waits for
    !> rmailmm.el).
    recursive subroutine cease_rmail_edit(status, abort)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: abort
        type(rmail_header_field_type), allocatable :: new_headers(:), old_headers(:)
        type(rmail_header_diff_type) :: diff
        type(restriction_type) :: restriction
        type(handle_type) :: saved, headers_end, start, end, view
        type(status_type) :: st
        character(len=:), allocatable :: old, edited_coding, fromline, new_coding, mime_charset, old_coding, &
                                         content_type, character_coding, coding_system, chosen, text, encoded, &
                                         old_content_type, header
        integer(int64) :: found, limit, mime_beginning, mime_end, beginning_position, headers_end_position, length, &
                          message_beginning, message_end, moved
        logical :: aborting, pruned, saved_fold, is_text_message, saved_inhibit, changed, swapped
        integer :: k, e, i

        aborting = .false.
        if (present(abort)) aborting = abort
        k = rmail_state_index(rmail_buffer())

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "There is no Rmail buffer")
            return
        end if

        if (rmail_summary_exists()) then
            saved = current_buffer()
            call set_buffer(rmail_states(k)%summary_buffer, status)
            if (status%ok() .and. associated(enable_rmail_summary_function)) call enable_rmail_summary_function(0_int64, status)
            call set_buffer(saved, st)
            if (.not. status%ok()) return
        end if

        call widen()
        call goto_char(point_min())
        ! This is far from ideal.  The edit may have inadvertently removed
        ! the blank line at the end of the headers, but there are almost
        ! certainly other blank lines.
        call search_forward(LF//LF, found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return

        if (found == 0) then
            call set_error(status, FM_GENERIC_ERROR, "There must be a blank line at the end of the headers")
            return
        end if

        e = edit_state_index(.true.)
        ! Disguise any "From " lines so they don't start a new message.
        call goto_char(point_min())
        ! This tries to skip the mbox From.
        if (.not. edit_states(e)%old_pruned) moved = forward_line(1_int64)

        ! When editing a non-MIME message, rmail-show-message-1 has
        ! unescaped ^>*From lines according to rmail-mbox-format.  We are
        ! editing the message as it was displayed, and need to put the
        ! escapes when done.
        if (rmail_mbox_format%choice == RMAIL_MBOX_FORMAT_MBOXO) then
            fromline = "^From "
        else
            fromline = "^>*From "
        end if

        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        do
            call re_search_forward(fromline, found, status, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. status%ok()) exit
            call beginning_of_line(1_int64)
            call insert(">", status)
            if (.not. status%ok()) exit
            moved = forward_line(1_int64)
        end do

        case_fold_search%value = saved_fold
        if (.not. status%ok()) return
        old = ""
        if (allocated(edit_states(e)%old_text)) old = edit_states(e)%old_text
        pruned = edit_states(e)%old_pruned
        if (allocated(edit_states(e)%old_headers)) old_headers = edit_states(e)%old_headers
        ! People who know what they are doing might have modified the
        ! buffer's encoding if editing the message included inserting
        ! characters that were unencodable by the original message's
        ! encoding.  Make note of the new encoding and use it for encoding
        ! the edited message.
        edited_coding = "undecided"
        if (allocated(buffer_file_coding_system%value)) edited_coding = buffer_file_coding_system%value

        ! Make sure `edited_coding' can safely encode the edited message.
        if (associated(select_safe_coding_system_function)) then
            call select_safe_coding_system_function(point_min(), point_max(), edited_coding, .false., "", chosen, status)
            if (.not. status%ok()) return
            if (len(chosen) > 0) edited_coding = chosen
        end if

        ! Go back to Rmail mode, but carefully.
        swapped = rmail_states(k)%swapped
        rmail_states(k)%swapped = .false.
        call clear_all_local_variables()
        call set_up_rmail_mode_1(status)
        rmail_states(k)%swapped = swapped
        if (.not. status%ok()) return
        call set_rmail_variables()
        ! If text has really changed, mark message as edited.
        call buffer_substring(point_min(), point_max(), text, status)
        if (.not. status%ok()) return
        changed = .not. (len(old) == point_max() - point_min() .and. old == text)

        if (changed) then
            call goto_char(point_min())
            call search_forward(LF//LF, found, status)
            if (.not. status%ok()) return
            ! First character of body.
            call copy_marker_at(point(), headers_end, .false., st)
            call save_restriction_begin(restriction)

            header_block: block
                call narrow_to_region(point_min(), point(), status)
                if (.not. status%ok()) exit header_block
                saved_inhibit = buffers(current)%read_only
                buffers(current)%read_only = .false.
                ! If they changed the message's encoding, rewrite the
                ! charset= header for them, so that subsequent
                ! rmail-show-message decodes it correctly.
                new_coding = coding_system_base(edited_coding)
                mime_charset = coding_system_mime_charset(new_coding)

                if (len(mime_charset) == 0) then
                    if (coding_system_equal(new_coding, "undecided")) then
                        mime_charset = "us-ascii"
                    else
                        mime_charset = new_coding
                    end if
                end if

                ! If there's no content-type in the edited headers, look for
                ! one in the original headers and add it to the edited
                ! headers.
                call get_mail_field("Content-Type", content_type, status)
                if (.not. status%ok()) exit header_block

                if (.not. allocated(content_type)) then
                    message_beginning = rmail_message_beginning(rmail_states(k)%current_message)
                    message_end = rmail_message_end(rmail_states(k)%current_message)
                    call find_old_content_type(rmail_states(k)%view_buffer, message_beginning, message_end, &
                                               old_content_type, status)
                    if (.not. status%ok()) exit header_block

                    if (len(old_content_type) > 0) then
                        call marker_position(headers_end, headers_end_position, st)
                        call save_excursion_begin_here()
                        ! First char of body, then add header before second
                        ! newline.
                        call goto_char(headers_end_position - 1)
                        call insert(old_content_type, status)
                        call restore_excursion_here()
                        ! Add it to the old headers as though it had been
                        ! there originally, to avoid an extra copy.
                        header = old_content_type(1:min(len(old_content_type), len("content-type")))

                        if (.not. has_field_named(old_headers, header)) then
                            call prepend_field(old_headers, header, old_content_type)
                        end if
                    end if
                end if

                call goto_char(point_min())
                old_coding = ""
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                call re_search_forward(rmail_mime_charset_pattern, found, status, noerror=SEARCH_MOVE)
                case_fold_search%value = saved_fold
                if (.not. status%ok()) exit header_block

                if (found > 0) then
                    mime_beginning = match_beginning(1, st)
                    mime_end = match_end(1, st)
                    call buffer_substring_no_properties(mime_beginning, mime_end, text, st)
                    old_coding = coding_system_from_name(text)
                    if (allocated(content_type)) deallocate (content_type)
                else
                    call get_mail_field("Content-Type", content_type, status)
                    if (.not. status%ok()) exit header_block
                end if

                if (allocated(content_type)) then
                    ! No match for the charset pattern, but there was some
                    ! other Content-Type.  We should not insert another.
                    continue
                else if (aborting) then
                    ! Don't insert anything if aborting.
                    continue
                else if (len(old_coding) == 0) then
                    ! If there was no charset= spec, insert one.
                    call goto_char(point() - 1)
                    call insert("Content-type: text/plain; charset="//mime_charset//LF, status)
                else if (.not. coding_system_equal(coding_system_base(old_coding), new_coding)) then
                    call goto_char(mime_end)
                    call delete_region(mime_beginning, mime_end, status)
                    if (status%ok()) call insert(mime_charset, status)
                end if

                buffers(current)%read_only = saved_inhibit
            end block header_block

            call save_restriction_end(restriction)
            if (.not. status%ok()) return
            call get_rmail_edit_headers(new_headers, status, widen_first=.true.)
            if (.not. status%ok()) return
            call swap_rmail_buffers_maybe(status)
            if (.not. status%ok()) return
            call narrow_to_region(rmail_message_beginning(rmail_states(k)%current_message), &
                                  rmail_message_end(rmail_states(k)%current_message), status)
            if (.not. status%ok()) return
            call goto_char(point_min())
            call search_forward(LF//LF, limit, status)
            if (.not. status%ok()) return
            ! All 3 of the functions we call below assume the buffer was
            ! narrowed to just the headers of the message.
            call save_restriction_begin(restriction)
            call narrow_to_region(point_min(), limit, status)
            call get_mail_field("content-transfer-encoding", character_coding, st)
            is_text_message = is_rmail_text()

            if (len(edited_coding) > 0 .and. .not. coding_system_equal(coding_system_base(edited_coding), "undecided")) then
                coding_system = edited_coding
            else
                coding_system = get_rmail_coding_system()
            end if

            call save_restriction_end(restriction)
            if (.not. status%ok()) return
            if (allocated(character_coding)) character_coding = downcase_ascii(character_coding)
            call goto_char(limit)
            saved_inhibit = buffers(current)%read_only
            buffers(current)%read_only = .false.
            ! The new body will be between these two markers.
            call copy_marker_at(point(), start, .false., st)
            call copy_marker_at(point(), end, .true., st)
            view = rmail_states(k)%view_buffer
            saved = current_buffer()
            call marker_position(headers_end, headers_end_position, st)
            call set_buffer(view, status)

            if (status%ok()) then
                call buffer_substring_no_properties(headers_end_position, point_max(), text, status)
                if (status%ok()) call encode_coding_string(text, coding_system, encoded, status)
            end if

            call set_buffer(saved, st)
            if (status%ok()) call insert(str_to_multibyte(encoded), status)
            ! Apply to the mbox buffer any changes in header fields that
            ! the user made while editing in the view buffer.
            if (status%ok()) call diff_rmail_edit_headers(old_headers, new_headers, diff)
            if (status%ok()) call update_rmail_edit_headers(diff, status)

            ! Re-apply content-transfer-encoding, if any, on the message
            ! body.
            if (status%ok() .and. allocated(character_coding)) then
                call marker_position(start, beginning_position, st)
                call marker_position(end, message_end, st)

                if (character_coding == "quoted-printable") then
                    call quote_mail_printable_region(beginning_position, message_end, status)
                else if (character_coding == "base64" .and. is_text_message) then
                    call base64_encode_region(beginning_position, message_end, length, status)
                end if
            end if

            if (status%ok()) then
                ! After encoding, make sure buffer ends with a blank line so
                ! as not to run this message together with the following
                ! one.
                call marker_position(end, message_end, st)
                call goto_char(message_end)
                call ensure_rmail_blank_line(status)
                ! Delete previous body.  This must be after all insertions
                ! at the end, so the marker for the beginning of the next
                ! message isn't messed up.
                call marker_position(end, message_end, st)
                if (status%ok()) call delete_region(message_end, point_max(), status)
            end if

            buffers(current)%read_only = saved_inhibit
            call destroy_marker(start, st)
            call destroy_marker(end, st)
            call destroy_marker(headers_end, st)
            if (.not. status%ok()) return
            call set_rmail_attribute(RMAIL_EDITED_ATTRIBUTE_INDEX, .true., status)
            if (.not. status%ok()) return

            if (rmail_summary_exists() .and. associated(update_rmail_summary_line_function)) then
                saved = current_buffer()
                call set_buffer(rmail_states(k)%summary_buffer, status)
                if (status%ok()) call update_rmail_summary_line_function(rmail_states(k)%current_message, status)
                call set_buffer(saved, st)
                if (.not. status%ok()) return
            end if
        end if

        call show_rmail_message(status)
        if (.not. status%ok()) return

        if (pruned) then
            call toggle_rmail_header(status, 1_int64)
        else
            call toggle_rmail_header(status, 0_int64)
        end if

        if (.not. status%ok()) return
        call rmail_mode_hook%execute(status)
        i = 0

    contains

        ! save-excursion around the insertion of the old Content-Type.
        subroutine save_excursion_begin_here()
            call copy_marker_at(point(), saved, .false., st)
        end subroutine save_excursion_begin_here

        subroutine restore_excursion_here()
            integer(int64) :: pos

            call marker_position(saved, pos, st)
            call goto_char(pos)
            call destroy_marker(saved, st)
        end subroutine restore_excursion_here
    end subroutine cease_rmail_edit

    ! The Content-Type field of the message from MESSAGE_BEGINNING to
    ! MESSAGE_END of BUFFER, the message collection, with its newline (""
    ! if it has none).  Port of the search in the original headers of
    ! rmailedit.el:rmail-cease-edit.
    subroutine find_old_content_type(buffer, message_beginning, message_end, field, status)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: message_beginning, message_end
        character(len=:), allocatable, intent(out) :: field
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        type(restriction_type) :: restriction
        type(excursion_type) :: excursion
        type(status_type) :: st
        integer(int64) :: limit, found, beginning, moved
        logical :: saved_fold

        field = ""
        saved = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_region(message_beginning, message_end, status)

        if (status%ok()) then
            call goto_char(point_min())
            call search_forward(LF//LF, limit, status)
        end if

        if (status%ok()) then
            call narrow_to_region(point_min(), limit, status)
            call goto_char(point_min())
            saved_fold = case_fold_search%value
            case_fold_search%value = .true.
            call re_search_forward("^content-type:", found, status, bound=limit, noerror=SEARCH_NOERROR)
            case_fold_search%value = saved_fold

            if (found > 0 .and. status%ok()) then
                beginning = match_beginning(0, st)
                moved = forward_line(1_int64)
                call buffer_substring(beginning, point(), field, status)
            end if
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
        call set_buffer(saved, st)
    end subroutine find_old_content_type

    ! Whether a field of FIELDS is named NAME.  Not a port: upstream's
    ! (assoc NAME rmail-old-headers).
    logical function has_field_named(fields, name)
        type(rmail_header_field_type), allocatable, intent(in) :: fields(:)
        character(len=*), intent(in) :: name
        integer :: i

        has_field_named = .false.
        if (.not. allocated(fields)) return

        do i = 1, size(fields)
            if (fields(i)%invalid) cycle
            if (fields(i)%name == name) has_field_named = .true.
        end do
    end function has_field_named

    ! Add the field NAME with TEXT at the front of FIELDS.  Not a port.
    subroutine prepend_field(fields, name, text)
        type(rmail_header_field_type), allocatable, intent(inout) :: fields(:)
        character(len=*), intent(in) :: name, text
        type(rmail_header_field_type), allocatable :: grown(:)
        integer :: n

        n = 0
        if (allocated(fields)) n = size(fields)
        allocate (grown(n + 1))
        grown(1)%name = name
        grown(1)%text = text
        if (n > 0) grown(2:) = fields
        call move_alloc(grown, fields)
    end subroutine prepend_field

    !> Abort edit of current message; restore original contents.  Port of
    !> rmailedit.el:rmail-abort-edit.
    recursive subroutine abort_rmail_edit(status)
        type(status_type), intent(out) :: status
        integer :: e

        call widen()
        call delete_region(point_min(), point_max(), status)
        if (.not. status%ok()) return
        e = edit_state_index(.true.)
        if (allocated(edit_states(e)%old_text)) call insert(edit_states(e)%old_text, status)
        if (.not. status%ok()) return
        call cease_rmail_edit(status, .true.)
        if (.not. status%ok()) return
        call highlight_rmail_headers(status)
    end subroutine abort_rmail_edit

    !> The header fields of the message in the current buffer, in FIELDS.
    !> Each has the name of the header field and its entire text with a
    !> final newline.  If part of the text is not valid as a header field,
    !> the field is `invalid' and numbered by consecutive integers.
    !> If WIDEN_FIRST is true, operate on the entire buffer.
    !> If MARKERS is true, the fields have markers at their beginning
    !> (`first') and end (`second').
    !> Port of rmailedit.el:rmail-edit-headers-alist.
    subroutine get_rmail_edit_headers(fields, status, widen_first, markers)
        type(rmail_header_field_type), allocatable, intent(out) :: fields(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: widen_first, markers
        type(rmail_header_field_type), allocatable :: grown(:)
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        type(status_type) :: st
        integer(int64) :: start, found, moved
        integer :: n, no_good_header_count
        logical :: matched, with_markers

        allocate (fields(0))
        no_good_header_count = 1
        with_markers = .false.
        if (present(markers)) with_markers = markers
        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)

        if (present(widen_first)) then
            if (widen_first) call widen()
        end if

        call goto_char(point_min())
        call search_forward(LF//LF, found, status)

        if (status%ok()) then
            call narrow_to_region(point_min(), point() - 1, status)
            call goto_char(point_min())

            do while (.not. is_eob() .and. status%ok())
                start = point()
                n = size(fields)
                allocate (grown(n + 1))
                grown(1:n) = fields
                call move_alloc(grown, fields)
                ! Match the name.
                matched = looking_at("[ "//TAB//"]*\([^:"//LF//" "//TAB//"]\(\|[^:"//LF//"]*[^:"//LF//" "//TAB// &
                                     "]\)\)[ "//TAB//"]*:", status)

                if (matched) then
                    call buffer_substring_no_properties(match_beginning(1, st), match_end(1, st), fields(n + 1)%name, st)
                else
                    fields(n + 1)%name = ""
                    fields(n + 1)%invalid = .true.
                    fields(n + 1)%number = no_good_header_count
                    no_good_header_count = no_good_header_count + 1
                end if

                moved = forward_line(1_int64)

                do while (looking_at("[ "//TAB//"]", status))
                    moved = forward_line(1_int64)
                end do

                call buffer_substring_no_properties(start, point(), fields(n + 1)%text, st)

                if (with_markers) then
                    call copy_marker_at(start, fields(n + 1)%first, .false., st)
                    call copy_marker_at(point(), fields(n + 1)%second, .false., st)
                end if
            end do
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine get_rmail_edit_headers

    ! Whether fields A and B have the same name (assoc's test).  Not a port.
    logical function is_same_name(a, b)
        type(rmail_header_field_type), intent(in) :: a, b

        if (a%invalid .or. b%invalid) then
            is_same_name = a%invalid .and. b%invalid .and. a%number == b%number
        else
            is_same_name = a%name == b%name
        end if
    end function is_same_name

    !> Compare OLD_HEADERS and NEW_HEADERS and give the field differences
    !> in DIFF: the inserted fields, each with the field (of NEW_HEADERS)
    !> to insert it after or none to insert it at the beginning; the
    !> deleted fields (of OLD_HEADERS); and the changed fields, old and
    !> new.  Port of rmailedit.el:rmail-edit-diff-headers.
    subroutine diff_rmail_edit_headers(old_headers, new_headers, diff)
        type(rmail_header_field_type), allocatable, intent(in) :: old_headers(:), new_headers(:)
        type(rmail_header_diff_type), intent(out) :: diff
        logical, allocatable :: spoken(:)
        integer :: i, j, k, n

        allocate (diff%inserted(0), diff%deleted(0), diff%changed_old(0), diff%changed_new(0))
        n = 0
        if (allocated(new_headers)) n = size(new_headers)
        allocate (spoken(n))
        spoken = .false.

        if (allocated(old_headers)) then
            do i = 1, size(old_headers)
                ! The new counterpart: the first field of that name not yet
                ! spoken for.
                k = 0

                do j = 1, n
                    if (spoken(j)) cycle

                    if (is_same_name(old_headers(i), new_headers(j))) then
                        k = j
                        exit
                    end if
                end do

                if (k == 0) then
                    ! If it's in OLD-HEADERS and has no new counterpart, it
                    ! is a deletion.
                    call append_field(diff%deleted, old_headers(i))
                else
                    ! If it has a new counterpart, maybe it was changed.
                    if (old_headers(i)%text /= new_headers(k)%text) then
                        call append_field(diff%changed_old, old_headers(i))
                        call append_field(diff%changed_new, new_headers(k))
                    end if

                    ! Remove the new counterpart, since it has been spoken
                    ! for.
                    spoken(k) = .true.
                end if
            end do
        end if

        ! Look at the new headers with no old counterpart.
        do j = 1, n
            if (spoken(j)) cycle
            ! Mark each one as an insertion.  Record the previous new
            ! header (that of the last equal field in buffer order), to
            ! insert it after that.
            k = 0

            do i = n, 1, -1
                if (new_headers(i)%text == new_headers(j)%text .and. is_same_name(new_headers(i), new_headers(j))) then
                    k = i
                    exit
                end if
            end do

            call append_insertion(diff%inserted, k, new_headers, new_headers(j))
        end do

    contains

        ! Add FIELD at the end of LIST.
        subroutine append_field(list, field)
            type(rmail_header_field_type), allocatable, intent(inout) :: list(:)
            type(rmail_header_field_type), intent(in) :: field
            type(rmail_header_field_type), allocatable :: grown(:)
            integer :: m

            m = size(list)
            allocate (grown(m + 1))
            grown(1:m) = list
            grown(m + 1) = field
            call move_alloc(grown, list)
        end subroutine append_field

        ! Add the insertion of NEW after the field before field AT of
        ! FIELDS (at the beginning when AT is 0 or 1).
        subroutine append_insertion(list, at, fields, new)
            type(rmail_header_insertion_type), allocatable, intent(inout) :: list(:)
            integer, intent(in) :: at
            type(rmail_header_field_type), intent(in) :: fields(:)
            type(rmail_header_field_type), intent(in) :: new
            type(rmail_header_insertion_type), allocatable :: grown(:)
            integer :: m

            m = size(list)
            allocate (grown(m + 1))
            grown(1:m) = list
            grown(m + 1)%new = new
            grown(m + 1)%at_start = at <= 1
            if (at > 1) grown(m + 1)%after = fields(at - 1)
            call move_alloc(grown, list)
        end subroutine append_insertion
    end subroutine diff_rmail_edit_headers

    !> Edit the mail headers in the buffer based on DIFF, given by
    !> `diff_rmail_edit_headers'.  Port of
    !> rmailedit.el:rmail-edit-update-headers.
    subroutine update_rmail_edit_headers(diff, status)
        type(rmail_header_diff_type), intent(in) :: diff
        type(status_type), intent(out) :: status
        type(rmail_header_field_type), allocatable :: buffer_headers(:)
        type(rmail_header_field_type) :: entry
        type(status_type) :: st
        integer(int64) :: end, first, second
        integer :: i, m

        call get_rmail_edit_headers(buffer_headers, status, markers=.true.)
        if (.not. status%ok()) return

        ! Change all the fields scheduled for being changed.
        do i = 1, size(diff%changed_old)
            m = find_text(diff%changed_old(i)%text)
            if (m == 0) cycle
            call marker_position(buffer_headers(m)%second, end, st)
            call goto_char(end)
            ! Insert the new, then delete the old.  That avoids collapsing
            ! markers.
            call insert_before_markers(collection_text(diff%changed_new(i)%text), status)
            if (.not. status%ok()) return
            call marker_position(buffer_headers(m)%first, first, st)
            call delete_region(first, end, status)
            if (.not. status%ok()) return
            ! Remove the old field from BUF-HEADERS, then update it to show
            ! the changed field (its markers the other way round, as
            ! upstream makes them).
            call destroy_marker(buffer_headers(m)%first, st)
            call destroy_marker(buffer_headers(m)%second, st)
            buffer_headers = [buffer_headers(:m - 1), buffer_headers(m + 1:)]
            entry%text = diff%changed_new(i)%text
            entry%invalid = .false.
            entry%name = ""
            call copy_marker_at(point(), entry%first, .false., st)
            call copy_marker_at(point() - len(collection_text(entry%text), int64), entry%second, .false., st)
            buffer_headers = [entry, buffer_headers]
        end do

        ! Delete all the fields scheduled for deletion.  We do deletion
        ! after changes because when two fields look alike and get
        ! replaced by one, the first of them is considered changed and the
        ! second is considered deleted.
        do i = 1, size(diff%deleted)
            m = find_text(diff%deleted(i)%text)
            if (m == 0) cycle
            call marker_position(buffer_headers(m)%first, first, st)
            call marker_position(buffer_headers(m)%second, second, st)
            call delete_region(min(first, second), max(first, second), status)
            if (.not. status%ok()) return
        end do

        ! Insert all the fields scheduled for insertion.
        do i = 1, size(diff%inserted)
            m = 0
            if (.not. diff%inserted(i)%at_start) m = find_text(diff%inserted(i)%after%text)

            if (m > 0) then
                call marker_position(buffer_headers(m)%second, second, st)
                call goto_char(second)
            else
                call goto_char(point_min())
            end if

            call insert(collection_text(diff%inserted(i)%new%text), status)
            if (.not. status%ok()) return
            ! Update BUF-HEADERS to show the inserted field.
            entry%text = diff%inserted(i)%new%text
            call copy_marker_at(point() - len(collection_text(entry%text), int64), entry%first, .false., st)
            call copy_marker_at(point(), entry%second, .false., st)
            buffer_headers = [entry, buffer_headers]
        end do

        ! Disconnect the markers.
        do i = 1, size(buffer_headers)
            call destroy_marker(buffer_headers(i)%first, st)
            call destroy_marker(buffer_headers(i)%second, st)
        end do

    contains

        ! The first field of BUFFER_HEADERS whose text is TEXT (0 if none).
        integer function find_text(text) result(found)
            character(len=*), intent(in) :: text

            do found = 1, size(buffer_headers)
                if (buffer_headers(found)%text == text) return
            end do

            found = 0
        end function find_text
    end subroutine update_rmail_edit_headers

    ! TEXT, header text of the edited message, as the message collection
    ! gets it: upstream inserts it into a unibyte buffer, which keeps the
    ! low byte of each character; here each such byte is an eight-bit
    ! character of the (multibyte) collection.  Not a port.
    function collection_text(text) result(bytes)
        use character_mod, only: string_char_and_length
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: bytes
        character(len=:), allocatable :: raw
        integer(int32) :: c, length
        integer(int64) :: i
        integer :: n

        allocate (character(len=len(text)) :: raw)
        i = 1
        n = 0

        do while (i <= len(text))
            call string_char_and_length(text, i, c, length)
            n = n + 1
            raw(n:n) = achar(iand(c, 255))
            i = i + max(length, 1)
        end do

        bytes = str_to_multibyte(raw(1:n))
    end function collection_text

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_cease_rmail_edit(status)
        type(status_type), intent(out) :: status

        call cease_rmail_edit(status)
    end subroutine cmd_cease_rmail_edit

    !> Define Rmail Edit mode's keymap and commands.  Not a port: the
    !> top-level forms of rmailedit.el.
    subroutine init_rmail_edit()
        type(status_type) :: st

        call make_sparse_keymap(rmail_edit_map)
        call set_keymap_parent(rmail_edit_map, text_mode_map, st)
        call keymap_set(rmail_edit_map, "C-c C-c", "cease_rmail_edit", st)
        call keymap_set(rmail_edit_map, "C-c C-]", "abort_rmail_edit", st)
        commands%edit_rmail_current_message%execute => edit_rmail_current_message
        commands%cease_rmail_edit%execute => cmd_cease_rmail_edit
        commands%abort_rmail_edit%execute => abort_rmail_edit
    end subroutine init_rmail_edit
end module rmail_edit_mod
