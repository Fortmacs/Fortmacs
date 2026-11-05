! rmail_output_mod.f90 -- writing Rmail messages to files.
! Ported from GNU Emacs 31.1 lisp/mail/rmailout.el (in part: see
! docs/status/rmail_output_mod.md).
!
! Copyright (C) 1985, 1987, 1993-1994, 2001-2026 Free Software
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

!> Writing Rmail messages to files: \[output_rmail_message] (o) appends the
!> current message, with its complete header, to an mbox file (or to the
!> buffer visiting it, which, when it is an Rmail buffer, learns about
!> the new message), \[output_rmail_message_as_seen] (C-o) appends it as it is
!> shown, and \[output_rmail_message_body_to_file] (w) writes its body to a
!> file.  See docs/status/rmail_output_mod.md for what is deferred.
module rmail_output_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_arg, prefix_numeric_value
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR, FM_VOID_FUNCTION
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, get_file_buffer, close_buffer, &
                          get_or_make_buffer, generate_new_buffer_name
    use editing_functions_mod, only: point, goto_char, point_min, point_max, narrow_to_region, widen, buffer_size, &
                                     insert_buffer_substring, buffer_substring_no_properties, save_restriction_begin, &
                                     save_restriction_end, save_excursion_begin, save_excursion_end, restriction_type, &
                                     excursion_type, line_beginning_position
    use editing_commands_mod, only: beginning_of_line
    use search_mod, only: re_search_forward, search_forward, looking_at, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use utility_functions_mod, only: read_yes_or_no
    use basic_subprograms_mod, only: read_y_or_n
    use file_input_output_mod, only: expand_file_name, file_exists, file_name_directory, file_name_nondirectory, &
                                     is_file_readable, is_file_directory, write_region
    use files_mod, only: abbreviate_file_name
    use coding_mod, only: coding_system_for_write
    use display_engine_mod, only: message
    use minibuffer_mod, only: read_file_name
    use replace_mod, only: format_prompt
    use mail_utilities_mod, only: get_mail_field, is_mail_file_babyl
    use rmail_mod, only: rmail_states, rmail_state_index, rmail_buffer, rmail_message_beginning, rmail_message_end, &
                         is_rmail_message_deleted, set_rmail_attribute, delete_rmail_message, delete_rmail_message_forward, &
                         next_rmail_message, next_rmail_undeleted_message, swap_rmail_buffers_maybe, modify_rmail_format, &
                         set_rmail_message_counters_maybe, ensure_rmail_blank_line, count_rmail_new_messages, &
                         has_rmail_summary, select_rmail_summary, update_rmail_summary_function, show_rmail_message_1, &
                         rmail_default_file, rmail_default_body_file, rmail_delete_after_output, rmail_enable_mime, &
                         RMAIL_DELETED_ATTRIBUTE_INDEX, RMAIL_FILED_ATTRIBUTE_INDEX
    implicit none(type, external)
    private

    public :: output_rmail_message, output_rmail_message_as_seen, output_rmail_message_body_to_file, read_rmail_output_file_name, &
              delete_rmail_unwanted_fields, output_rmail_message_as_mbox, init_rmail_output

    !> If true, do coding system decoding when outputting message as Babyl.
    !> Port of rmailout.el rmail-output-decode-coding.
    logical, public :: rmail_output_decode_coding = .false.

    !> Regexp describing fields to exclude when outputting a message to a file.
    !> The function `delete_rmail_unwanted_fields' uses this, ignoring case.
    !> Port of rmailout.el rmail-fields-not-to-output.
    character(len=:), allocatable, public :: rmail_fields_not_to_output

    !> True means reset the "deleted" flag when outputting a message to a
    !> file.  Port of rmailout.el rmail-output-reset-deleted-flag.
    logical, public :: rmail_output_reset_deleted_flag = .false.

    character(len=*), parameter :: LF = achar(10)

contains

    !> Read the file name to use for `output_rmail_message'.
    !> Set `rmail_default_file' to this name as well as returning it in
    !> FILE.  Port of rmailout.el:rmail-output-read-file-name
    !> (`rmail_output_file_alist' is Deferred).
    recursive subroutine read_rmail_output_file_name(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default_file, read, read_file

        ! If no suggestion, use same file as last time.
        default_file = rmail_default_file
        call read_file_name(format_prompt("Output message to mail file", file_name_nondirectory(default_file)), read, &
                            status, dir=file_name_directory(default_file), &
                            default_filename=abbreviate_file_name(default_file))
        if (.not. status%ok()) return
        call expand_file_name(read, read_file, status, file_name_directory(default_file))
        if (.not. status%ok()) return

        if (is_file_directory(read_file)) then
            call expand_file_name(file_name_nondirectory(default_file), file, status, read_file)
        else
            call expand_file_name(read_file, file, status, file_name_directory(default_file))
        end if

        if (status%ok()) rmail_default_file = file
    end subroutine read_rmail_output_file_name

    !> Delete all headers matching `rmail_fields_not_to_output'.
    !> Retains headers matching the regexp PRESERVE (none if empty).
    !> Ignores case.  The buffer should be narrowed to just the header.
    !> Port of rmailout.el:rmail-delete-unwanted-fields.
    subroutine delete_rmail_unwanted_fields(preserve, status)
        use editing_functions_mod, only: delete_region
        character(len=*), intent(in) :: preserve
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        integer(int64) :: found
        logical :: saved_fold, kept

        status = status_type()
        if (.not. allocated(rmail_fields_not_to_output)) return
        call save_excursion_begin(saved)
        call goto_char(point_min())
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.

        do
            call re_search_forward(rmail_fields_not_to_output, found, status, noerror=SEARCH_NOERROR)
            if (found == 0 .or. .not. status%ok()) exit
            call beginning_of_line(1_int64)
            kept = .false.
            if (len(preserve) > 0) kept = looking_at(preserve, status)
            if (.not. kept) call delete_region(point(), line_beginning_position(2_int64), status)
            if (.not. status%ok()) exit
        end do

        case_fold_search%value = saved_fold
        call save_excursion_end(saved)
    end subroutine delete_rmail_unwanted_fields

    !> Convert the current buffer's text to mbox and output to FILE_NAME.
    !> Alters the current buffer's text, so it should be a temporary buffer.
    !> If a buffer is visiting FILE_NAME, adds the text to that buffer
    !> rather than saving the file directly.  If the buffer is an Rmail buffer,
    !> updates it accordingly.  If no buffer is visiting FILE_NAME, appends
    !> the text directly to FILE_NAME, and displays a "Wrote file" message
    !> unless NOMSG is true.
    !> AS_SEEN is true if we are copying the message "as seen".
    !> Port of rmailout.el:rmail-output-as-mbox (encrypted files, NAME.gpg,
    !> are Deferred: they are written as other files; and a message without
    !> a "From " line needs mail-mbox-from, which waits for time-date.el's
    !> date-to-time: a `void_function' error condition occurs).
    recursive subroutine output_rmail_message_as_mbox(file_name, nomsg, status, as_seen)
        character(len=*), intent(in) :: file_name
        logical, intent(in) :: nomsg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: as_seen
        character(len=:), allocatable :: wrote, saved_coding
        type(handle_type) :: buffer, temporary
        integer(int64) :: found
        logical :: saved_fold, seen, coding_set, saved_read_only
        integer :: k

        seen = .false.
        if (present(as_seen)) seen = as_seen
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call goto_char(point_min())
        ! Preserve the Mail-From and MIME-Version fields even if they have
        ! been pruned.
        call search_forward(LF//LF, found, status, noerror=SEARCH_MOVE)
        call narrow_to_region(point_min(), point(), status)

        if (rmail_enable_mime) then
            call delete_rmail_unwanted_fields("Mail-From", status)
        else
            call delete_rmail_unwanted_fields("Mail-From\|MIME-Version\|Content-type", status)
        end if

        if (.not. status%ok()) then
            case_fold_search%value = saved_fold
            return
        end if

        call goto_char(point_min())

        if (.not. looking_at("From ", status)) then
            case_fold_search%value = saved_fold
            call set_error(status, FM_VOID_FUNCTION, "mail-mbox-from")
            return
        end if

        call widen()
        ! Make sure message ends with blank line.
        call goto_char(point_max())
        call ensure_rmail_blank_line(status)
        if (.not. status%ok()) return
        call goto_char(point_min())
        case_fold_search%value = saved_fold
        buffer = get_file_buffer(file_name)
        temporary = current_buffer()

        if (.not. is_buffer_live(buffer)) then
            coding_set = allocated(coding_system_for_write%value)
            if (coding_set) saved_coding = coding_system_for_write%value
            coding_system_for_write%value = "raw-text-unix"
            ! FIXME should ensure existing file ends with a blank line.
            call write_region(file_name, .false., status, point_min(), point_max(), wrote, append=.true.)

            if (coding_set) then
                coding_system_for_write%value = saved_coding
            else
                deallocate (coding_system_for_write%value)
            end if

            if (status%ok() .and. .not. nomsg) call message(wrote)
            return
        end if

        if (buffer%index == temporary%index) then
            call set_error(status, FM_GENERIC_ERROR, "Can't output message to same file it's already in")
            return
        end if

        ! File has been visited, in buffer BUFFER.
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        k = rmail_state_index(buffer)

        if (k > 0 .and. seen) then
            call set_error(status, FM_GENERIC_ERROR, "Can't output ""as seen"" to a visited Rmail file")
            return
        end if

        if (k > 0) then
            call output_rmail_message_to_rmail_buffer(temporary, rmail_states(k)%current_message, status)
        else
            ! Output file not in Rmail mode => just insert at the end.
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call widen()
            call goto_char(point_max())
            call insert_buffer_substring(temporary, status)
            buffers(current)%read_only = saved_read_only
        end if
    end subroutine output_rmail_message_as_mbox

    ! Copy message in TEMBUF into the current Rmail buffer.
    ! Do what is necessary to make Rmail know about the new message, then
    ! display message number MSG.  Port of
    ! rmailout.el:rmail-output-to-rmail-buffer (Auto Save mode is not
    ! ported).
    recursive subroutine output_rmail_message_to_rmail_buffer(tembuf, msg, status)
        type(handle_type), intent(in) :: tembuf
        integer(int64), intent(in) :: msg
        type(status_type), intent(out) :: status
        type(excursion_type) :: saved
        character(len=:), allocatable :: blurb
        logical :: saved_read_only

        call save_excursion_begin(saved)

        body: block
            call swap_rmail_buffers_maybe(status)
            if (.not. status%ok()) exit body
            call modify_rmail_format(status)
            if (.not. status%ok()) exit body
            call set_rmail_message_counters_maybe()
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            ! Insert the new message after the last old message.
            call widen()

            if (buffer_size() /= 0) then
                ! Make sure the last old message ends with a blank line.
                call goto_char(point_max())
                call ensure_rmail_blank_line(status)
                if (.not. status%ok()) exit body
                ! Insert the new message at the end.
                call narrow_to_region(point_max(), point_max(), status)
            end if

            call insert_buffer_substring(tembuf, status)
            buffers(current)%read_only = saved_read_only
            if (.not. status%ok()) exit body
            call count_rmail_new_messages(.true.)

            if (has_rmail_summary()) then
                call select_rmail_summary(update_rmail_summary_function, 0_int64, status)
                if (.not. status%ok()) exit body
            end if

            call show_rmail_message_1(blurb, status, msg)
        end block body

        call save_excursion_end(saved)
    end subroutine output_rmail_message_to_rmail_buffer

    !> Append this message to mail file FILE_NAME.
    !> Writes mbox format, unless FILE_NAME exists and is Babyl format, in which
    !> case it writes Babyl.
    !>
    !> Interactively, the default file name comes from `rmail_default_file',
    !> which is updated to the name you use in this command.  In all uses, if
    !> FILE_NAME is not absolute, it is expanded with the directory part of
    !> `rmail_default_file'.
    !>
    !> If a buffer is visiting FILE_NAME, adds the text to that buffer
    !> rather than saving the file directly.  If the buffer is an Rmail
    !> buffer, updates it accordingly.
    !>
    !> This command always outputs the complete message header, even if
    !> the header display is currently pruned.
    !>
    !> If `rmail_output_reset_deleted_flag' is true, the message's
    !> deleted flag is reset in the message appended to the destination
    !> file.  Otherwise, the appended message will remain marked as
    !> deleted if it was deleted before invoking this command.
    !>
    !> Optional prefix argument COUNT (default 1) says to output that
    !> many consecutive messages, starting with the current one (ignoring
    !> deleted messages, unless `rmail_output_reset_deleted_flag' is
    !> true).  If `rmail_delete_after_output' is true, deletes
    !> messages after output.
    !>
    !> The optional third argument NOATTRIBUTE, if true, says not to
    !> set the `filed' attribute, and not to display a "Wrote file"
    !> message (if writing a file directly).
    !>
    !> Set the optional fourth argument NOT_RMAIL true if you call this
    !> from a non-Rmail buffer.  In this case, COUNT is ignored.
    !> Port of rmailout.el:rmail-output (Babyl files wait for
    !> rmail-output-as-babyl: a `void_function' error condition occurs).
    recursive subroutine output_rmail_message(file_name, status, count, noattribute, not_rmail)
        character(len=*), intent(in) :: file_name
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(in), optional :: noattribute, not_rmail
        character(len=:), allocatable :: file
        type(handle_type) :: buffer, source
        logical :: babyl_format, create, nomsg, reset, next_message, outside
        integer(int64) :: n, orig_count, beginning, end, message_number
        integer :: k

        n = 1
        if (present(count)) n = count
        nomsg = .false.
        if (present(noattribute)) nomsg = noattribute
        outside = .false.
        if (present(not_rmail)) outside = not_rmail
        call expand_file_name(file_name, file, status, file_name_directory(rmail_default_file))
        if (.not. status%ok()) return
        ! Warn about creating new file.
        call confirm_output_file(file, status)
        if (.not. status%ok()) return
        babyl_format = .false.
        if (is_file_readable(file)) babyl_format = is_mail_file_babyl(file, status)
        if (.not. status%ok()) return
        buffer = get_file_buffer(file)
        ! If a babyl file is visited in a buffer, is it visited as babyl or
        ! as mbox?
        if (babyl_format .and. is_buffer_live(buffer)) babyl_format = is_babyl_buffer(buffer)

        if (babyl_format) then
            call set_error(status, FM_VOID_FUNCTION, "rmail-output-as-babyl")
            return
        end if

        if (outside) then
            source = current_buffer()
            call output_in_temporary_buffer(source, point_min(), point_max(), file, nomsg, .false., status)
            return
        end if

        ! Called from an Rmail buffer.
        if (.not. is_buffer_live(rmail_buffer())) then
            call set_error(status, FM_GENERIC_ERROR, "There is no Rmail buffer")
            return
        end if

        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        k = rmail_state_index(rmail_buffer())

        if (rmail_states(k)%total_messages == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No messages to output")
            return
        end if

        orig_count = n

        do while (n > 0)
            reset = .false.

            if (rmail_output_reset_deleted_flag .and. is_rmail_message_deleted(rmail_states(k)%current_message)) then
                call set_rmail_attribute(RMAIL_DELETED_ATTRIBUTE_INDEX, .false., status)
                if (.not. status%ok()) return
                reset = .true.
            end if

            beginning = rmail_message_beginning(rmail_states(k)%current_message)
            end = rmail_message_end(rmail_states(k)%current_message)
            ! It is ok to go to a different buffer.
            source = current_buffer()
            if (rmail_states(k)%swapped) source = rmail_states(k)%view_buffer
            call output_in_temporary_buffer(source, beginning, end, file, nomsg, .false., status)
            ! Make sure we undo our messing with the DELETED attribute.
            if (reset) call set_rmail_attribute(RMAIL_DELETED_ATTRIBUTE_INDEX, .true., status)
            if (.not. status%ok()) return

            ! Mark message as "filed".
            if (.not. nomsg) then
                call set_rmail_attribute(RMAIL_FILED_ATTRIBUTE_INDEX, .true., status)
                if (.not. status%ok()) return
            end if

            n = n - 1
            next_message = .false.

            if (rmail_output_reset_deleted_flag) then
                if (rmail_delete_after_output) call delete_rmail_message(status)
                if (.not. status%ok()) return

                if (n >= 0) then
                    message_number = rmail_states(k)%current_message
                    call next_rmail_message(1_int64, status)
                    if (.not. status%ok()) return
                    next_message = rmail_states(k)%current_message == message_number + 1
                end if
            else if (rmail_delete_after_output) then
                call delete_rmail_message_forward(status, shown=next_message)
            else if (n > 0) then
                call next_rmail_undeleted_message(1_int64, status, next_message)
            end if

            if (.not. status%ok()) return

            if (n > 0 .and. .not. next_message) then
                call set_appended_error(orig_count - n, status)
                return
            end if
        end do

    contains

        ! Whether BUFFER's text starts as a Babyl file.
        logical function is_babyl_buffer(buffer)
            type(handle_type), intent(in) :: buffer
            type(handle_type) :: saved
            type(restriction_type) :: restriction
            type(excursion_type) :: excursion
            type(status_type) :: st

            saved = current_buffer()
            call set_buffer(buffer, st)
            call save_excursion_begin(excursion)
            call save_restriction_begin(restriction)
            call widen()
            call goto_char(point_min())
            is_babyl_buffer = looking_at("BABYL OPTIONS:", st)
            call save_restriction_end(restriction)
            call save_excursion_end(excursion)
            call set_buffer(saved, st)
        end function is_babyl_buffer
    end subroutine output_rmail_message

    ! The error "Only N message(s) appended" in STATUS.  Not a port.
    subroutine set_appended_error(num_appended, status)
        integer(int64), intent(in) :: num_appended
        type(status_type), intent(out) :: status
        character(len=100) :: text

        if (num_appended == 1) then
            write (text, '("Only ", i0, " message appended")') num_appended
        else
            write (text, '("Only ", i0, " messages appended")') num_appended
        end if

        call set_error(status, FM_GENERIC_ERROR, trim(text))
    end subroutine set_appended_error

    ! Ask whether to create FILE if no buffer visits it and it does not
    ! exist; an error condition occurs if not.  Port of the check of
    ! rmailout.el:rmail-output and rmailout.el:rmail-output-as-seen.
    recursive subroutine confirm_output_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical :: create

        status = status_type()
        if (is_buffer_live(get_file_buffer(file))) return
        if (file_exists(file)) return
        call read_yes_or_no(""""//file//""" does not exist, create it? ", create, status)
        if (status%ok() .and. .not. create) call set_error(status, FM_GENERIC_ERROR, "Output file does not exist")
    end subroutine confirm_output_file

    ! Insert the text from BEGINNING to END of buffer CURRENT, widened, into
    ! a temporary buffer and output it to FILE as mbox.  Port of the
    ! with-temp-buffer forms of rmailout.el:rmail-output and
    ! rmailout.el:rmail-output-as-seen.
    recursive subroutine output_in_temporary_buffer(source, beginning, end, file, nomsg, as_seen, status)
        type(handle_type), intent(in) :: source
        integer(int64), intent(in) :: beginning, end
        character(len=*), intent(in) :: file
        logical, intent(in) :: nomsg, as_seen
        type(status_type), intent(out) :: status
        type(handle_type) :: saved, temporary
        type(restriction_type) :: restriction
        type(status_type) :: st

        saved = current_buffer()
        call set_buffer(source, status)
        if (.not. status%ok()) return
        call save_restriction_begin(restriction)
        call widen()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)

        if (status%ok()) then
            call set_buffer(temporary, status)
            if (status%ok()) call insert_buffer_substring(source, status, beginning, end)
            if (status%ok()) call output_rmail_message_as_mbox(file, nomsg, status, as_seen)
            call set_buffer(source, st)
            call close_buffer(temporary, st)
        end if

        call save_restriction_end(restriction)
        call set_buffer(saved, st)
    end subroutine output_in_temporary_buffer

    !> Append this message to mbox file named FILE_NAME.
    !> The details are as for `output_rmail_message', except that:
    !>   i) the header is output as currently seen
    !>  ii) this function cannot write to Babyl files
    !> iii) an Rmail buffer cannot be visiting FILE_NAME
    !>
    !> Note that if NOT_RMAIL is true, there is no difference between this
    !> function and `output_rmail_message'.  This argument may be removed in future,
    !> so you should call `output_rmail_message' directly in that case.
    !> Port of rmailout.el:rmail-output-as-seen.
    recursive subroutine output_rmail_message_as_seen(file_name, status, count, noattribute, not_rmail)
        character(len=*), intent(in) :: file_name
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(in), optional :: noattribute, not_rmail
        character(len=:), allocatable :: file
        type(handle_type) :: source
        integer(int64) :: n, orig_count, beginning, end
        logical :: nomsg, next_message, babyl
        integer :: k

        if (present(not_rmail)) then
            if (not_rmail) then
                call output_rmail_message(file_name, status, count, noattribute, not_rmail)
                return
            end if
        end if

        n = 1
        if (present(count)) n = count
        call expand_file_name(file_name, file, status, file_name_directory(rmail_default_file))
        if (.not. status%ok()) return
        ! Warn about creating new file.
        call confirm_output_file(file, status)
        if (.not. status%ok()) return
        babyl = .false.
        if (is_file_readable(file)) babyl = is_mail_file_babyl(file, status)

        if (babyl) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot output `as seen' to a Babyl file")
            return
        end if

        nomsg = .false.
        if (present(noattribute)) nomsg = noattribute

        if (.not. is_buffer_live(rmail_buffer())) then
            call set_error(status, FM_GENERIC_ERROR, "There is no Rmail buffer")
            return
        end if

        call set_buffer(rmail_buffer(), status)
        if (.not. status%ok()) return
        k = rmail_state_index(rmail_buffer())

        if (rmail_states(k)%total_messages == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No messages to output")
            return
        end if

        orig_count = n
        source = current_buffer()

        do while (n > 0)
            ! If operating from whole-mbox buffer, get message bounds.
            if (rmail_states(k)%swapped) then
                ! If operating from the view buffer, get the bounds.
                beginning = 1
                end = buffers(source%index)%text%z
            else
                beginning = rmail_message_beginning(rmail_states(k)%current_message)
                end = rmail_message_end(rmail_states(k)%current_message)
            end if

            call output_in_temporary_buffer(source, beginning, end, file, nomsg, .true., status)
            if (.not. status%ok()) return

            ! Mark message as "filed".
            if (.not. nomsg) then
                call set_rmail_attribute(RMAIL_FILED_ATTRIBUTE_INDEX, .true., status)
                if (.not. status%ok()) return
            end if

            n = n - 1
            next_message = .false.

            if (rmail_delete_after_output) then
                call delete_rmail_message_forward(status, shown=next_message)
            else if (n > 0) then
                call next_rmail_undeleted_message(1_int64, status, next_message)
            end if

            if (.not. status%ok()) return

            if (n > 0 .and. .not. next_message) then
                call set_appended_error(orig_count - n, status)
                return
            end if
        end do
    end subroutine output_rmail_message_as_seen

    !> Write this message body to the file FILE_NAME.
    !> Interactively, the default file name comes from either the message
    !> "Subject" header, or from `rmail_default_body_file'.  Updates the value
    !> of `rmail_default_body_file' accordingly.  In all uses, if FILE_NAME
    !> is not absolute, it is expanded with the directory part of
    !> `rmail_default_body_file'.
    !>
    !> Note that this overwrites FILE_NAME (after confirmation), rather
    !> than appending to it.  Deletes the message after writing if
    !> `rmail_delete_after_output' is true.  Port of
    !> rmailout.el:rmail-output-body-to-file.
    recursive subroutine output_rmail_message_body_to_file(file_name, status)
        character(len=*), intent(in) :: file_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(excursion_type) :: saved
        integer(int64) :: found
        logical :: overwrite
        integer :: k

        call expand_file_name(file_name, file, status, file_name_directory(rmail_default_body_file))
        if (.not. status%ok()) return
        k = rmail_state_index(rmail_buffer())

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No message to output")
            return
        end if

        if (rmail_states(k)%current_message == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No message to output")
            return
        end if

        call save_excursion_begin(saved)
        call goto_char(point_min())
        call search_forward(LF//LF, found, status)

        if (status%ok() .and. file_exists(file)) then
            call read_y_or_n("File "//file//" exists; overwrite? ", overwrite, status)
            if (status%ok() .and. .not. overwrite) call set_error(status, FM_GENERIC_ERROR, "Operation aborted")
        end if

        if (status%ok()) call write_region(file, .false., status, point(), point_max())
        call save_excursion_end(saved)
        if (.not. status%ok()) return
        if (rmail_delete_after_output) call delete_rmail_message_forward(status)
    end subroutine output_rmail_message_body_to_file

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_output_rmail_message(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_arg)
        call read_rmail_output_file_name(file, status)
        if (status%ok()) call output_rmail_message(file, status, n)
    end subroutine cmd_output_rmail_message

    recursive subroutine cmd_output_rmail_message_as_seen(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        integer(int64) :: n

        n = prefix_numeric_value(current_prefix_arg)
        call read_rmail_output_file_name(file, status)
        if (status%ok()) call output_rmail_message_as_seen(file, status, n)
    end subroutine cmd_output_rmail_message_as_seen

    recursive subroutine cmd_output_rmail_message_body_to_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default_file, file
        integer :: i

        call get_mail_field("Subject", default_file, status)
        if (.not. status%ok()) return
        if (.not. allocated(default_file)) default_file = rmail_default_body_file

        do i = 1, len(default_file)
            if (default_file(i:i) == ":" .or. default_file(i:i) == " ") default_file(i:i) = "-"
        end do

        call read_file_name("Output message body to file: ", file, status, dir=file_name_directory(default_file), &
                            default_filename=default_file, initial_input=default_file)
        if (.not. status%ok()) return
        rmail_default_body_file = file
        call output_rmail_message_body_to_file(file, status)
    end subroutine cmd_output_rmail_message_body_to_file

    !> Define the commands of rmailout.el.  Not a port.
    subroutine init_rmail_output()
        commands%output_rmail_message%execute => cmd_output_rmail_message
        commands%output_rmail_message_as_seen%execute => cmd_output_rmail_message_as_seen
        commands%output_rmail_message_body_to_file%execute => cmd_output_rmail_message_body_to_file
    end subroutine init_rmail_output
end module rmail_output_mod
