! rmail_mime_mod.f90 -- MIME decoding and display stuff for Rmail.
! Ported from GNU Emacs 31.1 lisp/mail/rmailmm.el (in part: see
! docs/status/rmail_mime_mod.md).
!
! Copyright (C) 2006-2026 Free Software Foundation, Inc.
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

!> MIME messages in Rmail, with two operation modes.
!>
!> (1) When `rmail_enable_mime' is true (the default), `show_rmail_mime'
!> (the value of `rmail_show_mime_function') shows a MIME message directly
!> in Rmail's view buffer: `parse_rmail_mime' parses the message into MIME
!> entities (`process_rmail_mime', `process_rmail_mime_multipart'), and
!> `insert_rmail_mime' inserts them, the text parts decoded (base64 and
!> quoted-printable, then the charset), multipart/alternative showing the
!> best part, and the other parts as tag lines with a button that shows or
!> hides the part (\[toggle_rmail_mime_hidden]) and a button that saves an
!> attachment.  \[toggle_rmail_mime] toggles between this and the raw
!> message.
!>
!> (2) When `rmail_enable_mime' is false, \[toggle_rmail_mime] shows a
!> MIME message in a new buffer "*RMAIL*" (`show_rmail_mime_buffer',
!> `handle_rmail_mime' and the handlers of
!> `rmail_mime_media_type_handlers').
!>
!> Each entity is a record of a registry; the text that shows it carries
!> its index as the text property `rmail_mime_entity'.  What is not ported
!> yet (images, rendering HTML with shr, the actions after saving an
!> attachment, forwarding and resending) is listed in
!> docs/status/rmail_mime_mod.md.
module rmail_mime_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_rint
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE
    use character_mod, only: downcase_ascii, string_as_unibyte
    use handle_mod, only: handle_type, null_handle
    use hooks_mod, only: hook_type
    use value_mod, only: value_type, vtag, xfixnum, list, make_buffer_reference, make_list, V_INTEGER
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name, erase_buffer, is_buffer_live, is_buffer_modified, &
                          restore_buffer_modified, set_buffer_modified
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, narrow_to_region, widen, &
                                     character_after, is_end_of_buffer, is_beginning_of_line, buffer_string, &
                                     buffer_substring_no_properties, delete_region, insert_buffer_substring, &
                                     save_excursion_begin, save_excursion_end, save_restriction_begin, &
                                     save_restriction_end, excursion_type, restriction_type
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line
    use syntax_mod, only: skip_characters_forward
    use search_mod, only: search_forward, regular_expression_search_forward, looking_at, match_beginning, match_end, &
                          string_match, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use text_properties_mod, only: get_text_property, put_text_property, add_text_properties, &
                                   next_single_property_change, previous_single_property_change
    use coding_mod, only: is_coding_system, coding_system_base, coding_system_change_end_of_line_conversion, &
                          decode_coding_region, last_coding_system_used, coding_system_for_read, &
                          coding_system_for_write, END_OF_LINE_DOS
    use multilingual_environment_mod, only: coding_system_from_name, set_buffer_file_coding_system
    use utility_functions_mod, only: base64_decode_region
    use quoted_printable_mod, only: decode_quoted_printable_region
    use rfc2047_mod, only: decode_rfc2047_region
    use rfc2231_mod, only: mime_header_value_type, parse_rfc2231_quoted_printable_string, get_rfc2231_value
    use mime_utilities_mod, only: mime_character_set_overrides, mime_character_set_coding_type
    use mail_utilities_mod, only: get_mail_field
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use button_mod, only: button_kind_type, button_type, insert_button, next_button, button_at, button_start, &
                          button_end
    use basic_subprograms_mod, only: read_y_or_n
    use minibuffer_mod, only: read_file_name
    use file_input_output_mod, only: expand_file_name, is_file_directory, file_name_nondirectory, &
                                     file_name_as_directory, write_region
    use files_mod, only: temporary_file_directory, executable_find
    use execute_process_mod, only: execute_process_region
    use display_engine_mod, only: message
    use keymap_mod, only: keymap_set
    use modes_mod, only: major_modes
    use simple_mod, only: fundamental_mode
    use view_mod, only: view_buffer
    use rmail_mod, only: rmail_enable_mime, rmail_show_mime_function, rmail_search_mime_message_function, &
                         rmail_mode_map, rmail_states, rmail_state_index, rmail_buffer, is_rmail_buffers_swapped, &
                         is_rmail_mime_message, copy_rmail_headers, highlight_rmail_headers, show_rmail_message, &
                         toggle_rmail_header, rmail_message_beginning, rmail_message_end, apply_in_rmail_message_begin, &
                         apply_in_rmail_message_end, rmail_message_scope_type, RMAIL_MIME_ENTITY_PROPERTY
    implicit none(type, external)
    private

    public :: toggle_rmail_mime, toggle_rmail_mime_hidden, toggle_rmail_mime_raw, rmail_mime_mode, show_rmail_mime, &
              show_rmail_mime_buffer, parse_rmail_mime, insert_rmail_mime, search_rmail_mime_message, &
              handle_rmail_mime, handle_rmail_mime_text, handle_rmail_mime_bulk, handle_rmail_mime_multipart, &
              render_rmail_mime_html_lynx, initialize_rmail_mime
    public :: rmail_mime_handler_procedure, rmail_mime_render_html_procedure

    abstract interface
        !> A handler of `rmail_mime_media_type_handlers': handle the current
        !> buffer, narrowed to a MIME part's body, as a part of
        !> CONTENT_TYPE, CONTENT_DISPOSITION and CONTENT_TRANSFER_ENCODING
        !> (in lower case, "" for none).  HANDLED true means the part is
        !> handled.  Not a port.
        recursive subroutine rmail_mime_handler_procedure(content_type, content_disposition, &
                                                          content_transfer_encoding, handled, status)
            import :: mime_header_value_type, status_type
            implicit none(type, external)
            type(mime_header_value_type), intent(in) :: content_type, content_disposition
            character(len=*), intent(in) :: content_transfer_encoding
            logical, intent(out) :: handled
            type(status_type), intent(out) :: status
        end subroutine rmail_mime_handler_procedure

        !> Convert the HTML in SOURCE to text in the current buffer.  Not
        !> a port.
        recursive subroutine rmail_mime_render_html_procedure(source, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: source
            type(status_type), intent(out) :: status
        end subroutine rmail_mime_render_html_procedure

        ! Insert MIME entity ENTITY according to its display (an entity's
        ! handler).
        recursive subroutine insert_procedure(entity, status)
            import :: status_type
            implicit none(type, external)
            integer, intent(in) :: entity
            type(status_type), intent(out) :: status
        end subroutine insert_procedure
    end interface

    !> The longest regular expression of `rmail_mime_media_type_handlers' and
    !> `rmail_mime_attachment_directories'.
    integer, parameter, public :: RMAIL_MIME_REGULAR_EXPRESSION_MAXIMUM = 64

    !> The longest directory of `rmail_mime_attachment_directories'.
    integer, parameter, public :: RMAIL_MIME_DIRECTORY_MAXIMUM = 256

    !> A content type and the function that handles it.  Not a port: an
    !> element of rmail-mime-media-type-handlers-alist.
    type, public :: rmail_mime_media_type_handler_type
        !> A regular expression matching a content type.
        character(len=RMAIL_MIME_REGULAR_EXPRESSION_MAXIMUM) :: regular_expression = ""
        !> The function that handles it.
        procedure(rmail_mime_handler_procedure), pointer, nopass :: handler => null()
    end type rmail_mime_media_type_handler_type

    !> A content type and a directory to save its attachments into.  Not a
    !> port: a directory of an element of rmail-mime-attachment-dirs-alist.
    type, public :: rmail_mime_attachment_directory_type
        !> A regular expression matching a content type.
        character(len=RMAIL_MIME_REGULAR_EXPRESSION_MAXIMUM) :: content_type = ""
        !> The directory.
        character(len=RMAIL_MIME_DIRECTORY_MAXIMUM) :: directory = ""
    end type rmail_mime_attachment_directory_type

    !> Functions to handle various content types.
    !> These are records of a regular expression matching a content type
    !> and the function to call; the first record whose regular expression
    !> matches is used, and the following ones are tried, in order of
    !> decreasing preference, until one handles the part.
    !> Note that this only applies to items with an inline Content-Disposition,
    !> all others are handled by `handle_rmail_mime_bulk'.
    !> Note also that this array is ignored when the variable
    !> `rmail_enable_mime' is true.  Port of rmailmm.el
    !> rmail-mime-media-type-handlers-alist.
    type(rmail_mime_media_type_handler_type), allocatable, public :: rmail_mime_media_type_handlers(:)

    !> Default directories to save attachments of various types into.
    !> These are records of a regular expression matching a content type
    !> and a directory, in order of decreasing preference.
    !> The first directory that exists, of the first records whose regular
    !> expression matches, is used.  Port of rmailmm.el
    !> rmail-mime-attachment-dirs-alist.
    type(rmail_mime_attachment_directory_type), allocatable, public :: rmail_mime_attachment_directories(:)

    !> Function to convert HTML to text.
    !> Called with buffer containing HTML extracted from message in a
    !> temporary buffer.  Converts to text in current buffer.  If null,
    !> display HTML source.  Port of rmailmm.el
    !> rmail-mime-render-html-function (`render_rmail_mime_html_lynx' when
    !> lynx is installed; shr is Deferred).
    procedure(rmail_mime_render_html_procedure), pointer, public :: rmail_mime_render_html_function => null()

    !> If true, default to showing HTML part rather than text part
    !> when both are available.  Port of rmailmm.el rmail-mime-prefer-html
    !> (.true. when `rmail_mime_render_html_function' is set).
    logical, public :: rmail_mime_prefer_html = .false.

    !> Hook run after entering `rmail_mime_mode'.  Port of rmailmm.el
    !> rmail-mime-mode-hook (made by define-derived-mode).
    type(hook_type), target, public :: rmail_mime_mode_hook = hook_type(name="rmail_mime_mode_hook")

    !> The button type of the buttons that save an attachment.  Port of
    !> rmailmm.el's button type rmail-mime-save.
    type(button_kind_type), target, public :: rmail_mime_save_button

    !> The button type of the buttons that show or hide a MIME part.  Port
    !> of rmailmm.el's button type rmail-mime-toggle.
    type(button_kind_type), target, public :: rmail_mime_toggle_button

    !> How a part of an entity is displayed: not displayed, displayed by
    !> the decoded presentation form, or displayed by the raw MIME data
    !> (for the header and body only).
    integer, parameter, public :: RMAIL_MIME_DISPLAY_HIDDEN = 0, RMAIL_MIME_DISPLAY_DECODED = 1, &
                                  RMAIL_MIME_DISPLAY_RAW = 2

    ! The kinds of attached data (the TYPE of a tag line's BULK-DATA):
    ! unknown, text and HTML (no image type is supported).
    integer, parameter :: BULK_NONE = 0, BULK_TEXT = 1, BULK_HTML = 2

    ! How an entity is displayed.  Port of rmailmm.el's rmail-mime-display
    ! structure (made by rmail-mime--make-display, copied by assignment).
    type :: display_type
        integer :: header = RMAIL_MIME_DISPLAY_HIDDEN
        logical :: tagline = .false.
        integer :: body = RMAIL_MIME_DISPLAY_HIDDEN
    end type display_type

    ! The region of an entity's header or body in the mbox buffer
    ! (markers; null when the body is DATA instead) and how it is
    ! displayed by default.  Port of rmailmm.el's [BEG END DISPLAY-FLAG].
    type :: region_type
        type(handle_type) :: beginning, end
        character(len=:), allocatable :: data
        integer :: display = RMAIL_MIME_DISPLAY_HIDDEN
    end type region_type

    ! A MIME entity.  Port of rmailmm.el's rmail-mime-entity structure:
    ! TYPE, DISPOSITION and TRANSFER-ENCODING ("" for none), DISPLAY
    ! (CURRENT and NEW), HEADER and BODY, TAGLINE (TAG, the size and kind
    ! of the attached data and DISPLAY-FLAG), CHILDREN (indices), HANDLER
    ! and TRUNCATED; and the file name and directory its Save button
    ! offers, which upstream keeps on the button.
    type :: entity_type
        logical :: used = .false.
        type(handle_type) :: owner
        type(mime_header_value_type) :: content_type, content_disposition
        character(len=:), allocatable :: transfer_encoding
        type(display_type) :: current, new
        type(region_type) :: header, body
        character(len=:), allocatable :: tag
        real(real64) :: size = 0
        integer :: bulk = BULK_NONE
        logical :: tagline = .false.
        integer, allocatable :: children(:)
        procedure(insert_procedure), pointer, nopass :: handler => null()
        logical :: truncated = .false.
        character(len=:), allocatable :: filename, directory
    end type entity_type

    ! The MIME entities.
    type(entity_type), allocatable :: entities(:)

    ! The buffer whose entities new entities belong to; they are destroyed
    ! when it shows the next message.
    type(handle_type) :: owner

    ! Buffer containing the mbox data, and buffer showing a message.  Port
    ! of rmailmm.el rmail-mime-mbox-buffer and rmail-mime-view-buffer.
    type(handle_type) :: mime_mbox_buffer, mime_view_buffer

    ! The first coding system used for decoding a MIME entity ("" for
    ! none), and whether it came from a header.  Port of rmailmm.el
    ! rmail-mime-coding-system (a cons for a header's).
    character(len=:), allocatable :: mime_coding_system
    logical :: mime_coding_system_from_header = .false.

    ! True inside `search_rmail_mime_message' to suppress expensive
    ! operations such as HTML decoding.  Port of rmailmm.el
    ! rmail-mime-searching.
    logical :: searching = .false.

    character(len=*), parameter :: RMAIL_MIME_HIDDEN_PROPERTY = "rmail_mime_hidden", FACE_PROPERTY = "face", &
                                   FONT_LOCK_FACE_PROPERTY = "font_lock_face"
    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    ! ------------------------------------------------------------------
    ! The entities
    ! ------------------------------------------------------------------

    ! Make a new entity E, belonging to `owner'.  Not a port.
    subroutine make_entity(e)
        integer, intent(out) :: e
        type(entity_type), allocatable :: grown(:)
        type(entity_type) :: fresh

        if (.not. allocated(entities)) allocate (entities(16))
        e = findloc(entities%used, .false., dim=1)

        if (e == 0) then
            allocate (grown(2*size(entities)))
            grown(1:size(entities)) = entities
            e = size(entities) + 1
            call move_alloc(grown, entities)
        end if

        entities(e) = fresh
        entities(e)%used = .true.
        entities(e)%owner = owner
        entities(e)%transfer_encoding = ""
        entities(e)%tag = ""
        allocate (entities(e)%children(0))
    end subroutine make_entity

    ! Destroy the entities of buffer BUFFER, with their markers.
    ! Not a port: upstream's entities are garbage collected.
    subroutine destroy_entities(buffer)
        type(handle_type), intent(in) :: buffer
        type(entity_type) :: fresh
        type(status_type) :: st
        integer :: e

        if (.not. allocated(entities)) return

        do e = 1, size(entities)
            if (.not. entities(e)%used) cycle
            if (entities(e)%owner%index /= buffer%index .or. entities(e)%owner%generation /= buffer%generation) cycle
            call destroy_region(entities(e)%header)
            call destroy_region(entities(e)%body)
            entities(e) = fresh
        end do

    contains

        subroutine destroy_region(region)
            type(region_type), intent(inout) :: region

            if (region%beginning%index /= 0) call destroy_marker(region%beginning, st)
            if (region%end%index /= 0) call destroy_marker(region%end, st)
        end subroutine destroy_region
    end subroutine destroy_entities

    ! The entity whose text is at POSITION (its `rmail_mime_entity'
    ! property), 0 for none.  Not a port.
    integer function entity_at(position) result(e)
        integer(int64), intent(in) :: position
        type(value_type) :: v
        type(status_type) :: st

        e = 0
        if (position < point_minimum() .or. position > point_maximum()) return
        call get_text_property(position, value_type(name=RMAIL_MIME_ENTITY_PROPERTY), v, st)
        if (.not. st%ok() .or. vtag(v) /= V_INTEGER) return
        e = int(xfixnum(v))
        if (.not. allocated(entities)) e = 0
        if (e < 1 .or. e > size(entities)) e = 0

        if (e > 0) then
            if (.not. entities(e)%used) e = 0
        end if
    end function entity_at

    ! Give the text from BEGINNING to END the property of entity E.
    ! Not a port: (put-text-property BEG END 'rmail-mime-entity ENTITY).
    subroutine put_entity(beginning, end, e, status)
        integer(int64), intent(in) :: beginning, end
        integer, intent(in) :: e
        type(status_type), intent(out) :: status

        call put_text_property(beginning, end, value_type(name=RMAIL_MIME_ENTITY_PROPERTY), value_type(int(e, int64)), &
                               status)
    end subroutine put_entity

    ! The position of marker M.  Not a port.
    integer(int64) function position_of(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, position, st)
    end function position_of

    ! A content type or disposition whose name is NAME, with no parameters.
    ! Not a port: upstream's '("text/plain") and the like.
    function plain_header(name) result(header)
        character(len=*), intent(in) :: name
        type(mime_header_value_type) :: header

        header%name = name
        allocate (header%parameters(0))
    end function plain_header

    ! The name of HEADER, "" for none.  Not a port: (car HEADER).
    function name_of(header) result(name)
        type(mime_header_value_type), intent(in) :: header
        character(len=:), allocatable :: name

        name = ""
        if (allocated(header%name)) name = header%name
    end function name_of

    ! HEADER as text, for an error message.  Not a port.
    function header_text(header) result(text)
        type(mime_header_value_type), intent(in) :: header
        character(len=:), allocatable :: text
        integer :: k

        text = name_of(header)
        if (.not. allocated(header%parameters)) return

        do k = 1, size(header%parameters)
            text = text//"; "//header%parameters(k)%attribute//"="//header%parameters(k)%value
        end do
    end function header_text

    ! Insert the text from BEGINNING to END of the mbox buffer into the
    ! current buffer, a unibyte buffer, as bytes.  Not a port: upstream's
    ! insert-buffer-substring from its unibyte mbox buffer.
    subroutine insert_mbox_bytes(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        type(handle_type) :: here
        logical :: multibyte

        here = current_buffer()
        call set_buffer(mime_mbox_buffer, status)
        if (.not. status%ok()) return
        multibyte = buffers(current)%multibyte
        call buffer_substring_no_properties(beginning, end, text, status)
        call set_buffer(here, status)
        if (.not. status%ok()) return
        if (multibyte) text = string_as_unibyte(text)
        call insert(text, status)
    end subroutine insert_mbox_bytes

    ! Make a temporary buffer current (with-temp-buffer), unibyte unless
    ! MULTIBYTE; OLD is the buffer to go back to.  Not a port.
    subroutine temporary_buffer_begin(old, temporary, multibyte, status)
        type(handle_type), intent(out) :: old, temporary
        logical, intent(in) :: multibyte
        type(status_type), intent(out) :: status

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)
        if (.not. status%ok()) return
        buffers(current)%multibyte = multibyte
        buffers(current)%undo_enabled = .false.
    end subroutine temporary_buffer_begin

    ! Close the temporary buffer TEMPORARY and make OLD current again.
    ! Not a port.
    subroutine temporary_buffer_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call set_buffer(old, st)
        call close_buffer(temporary, st)
    end subroutine temporary_buffer_end

    ! Decode the region from BEGINNING to the marker END by the transfer
    ! encoding ENCODING (base64, errors ignored, or quoted-printable).
    ! Not a port: the cond the insert functions of rmailmm.el share.
    subroutine decode_transfer_encoding(encoding, beginning, end, status)
        character(len=*), intent(in) :: encoding
        integer(int64), intent(in) :: beginning
        type(handle_type), intent(in) :: end
        type(status_type), intent(out) :: status
        integer(int64) :: length
        type(status_type) :: st

        status = status_type()

        if (encoding == "base64") then
            call base64_decode_region(beginning, position_of(end), length, st)
        else if (encoding == "quoted-printable") then
            call decode_quoted_printable_region(beginning, position_of(end), status)
        end if

        call goto_character(position_of(end))
    end subroutine decode_transfer_encoding

    ! Delete the N characters after point.  Not a port: (delete-char N).
    subroutine delete_forward(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call delete_region(point(), point() + n, status)
    end subroutine delete_forward

    ! The prompt PROMPT with DEFAULT.  Not a port: minibuffer.el's
    ! format-prompt with the default `minibuffer_default_prompt_format'.
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt//" (default "//default//"): "
    end function format_prompt

    ! ------------------------------------------------------------------
    ! Buttons
    ! ------------------------------------------------------------------

    !> Save the attachment of the MIME entity of BUTTON (its DATA).
    !> Port of rmailmm.el:rmail-mime-save (`rmail-mime-save-action' is
    !> Deferred: nothing is done after saving, its default).
    recursive subroutine save_rmail_mime(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, directory, given, original, saved_coding, expanded
        type(handle_type) :: old, temporary, end_marker
        logical :: answer, had_coding
        integer :: e, k
        type(status_type) :: st

        status = status_type()
        if (vtag(data) /= V_INTEGER) return
        e = int(xfixnum(data))
        if (.not. allocated(entities)) return
        if (e < 1 .or. e > size(entities)) return
        if (.not. entities(e)%used) return
        k = rmail_state_index(rmail_buffer())
        if (k > 0) mime_mbox_buffer = rmail_states(k)%view_buffer
        filename = entities(e)%filename
        directory = entities(e)%directory
        original = filename

        if (.not. allocated(entities(e)%body%data) .and. entities(e)%truncated) then
            call read_y_or_n("This entity is truncated; save anyway? ", answer, status)
            if (.not. status%ok()) return

            if (.not. answer) then
                call set_error(status, GENERIC_ERROR_CODE, "Aborted")
                return
            end if
        end if

        call expand_file_name(filename, expanded, status, directory)
        if (.not. status%ok()) return
        call read_file_name(format_prompt("Save as", filename), given, status, directory=directory, &
                            default_filename=expanded)
        if (.not. status%ok()) return
        call expand_file_name(given, filename, status, directory)
        if (.not. status%ok()) return

        ! If arg is just a directory, use the default file name, but in
        ! that directory (copied from write-file).
        if (is_file_directory(filename)) then
            call expand_file_name(file_name_nondirectory(original), filename, status, file_name_as_directory(filename))
            if (.not. status%ok()) return
        end if

        call temporary_buffer_begin(old, temporary, .false., status)
        if (.not. status%ok()) return

        body: block
            if (allocated(entities(e)%body%data)) then
                call insert(entities(e)%body%data, status)
                if (.not. status%ok()) exit body
            else
                ! DATA is a MIME-entity object.
                call insert_mbox_bytes(position_of(entities(e)%body%beginning), position_of(entities(e)%body%end), &
                                       status)
                if (.not. status%ok()) exit body
                call copy_marker_at(point_maximum(), end_marker, .true., status)
                if (.not. status%ok()) exit body
                call decode_transfer_encoding(entities(e)%transfer_encoding, point_minimum(), end_marker, status)
                call destroy_marker(end_marker, st)
                if (.not. status%ok()) exit body
            end if

            had_coding = allocated(coding_system_for_write%value)
            if (had_coding) saved_coding = coding_system_for_write%value
            coding_system_for_write = "no-conversion"
            call write_region(filename, .false., status, start=point_minimum(), end=point_maximum())

            if (had_coding) then
                coding_system_for_write = saved_coding
            else
                call coding_system_for_write%clear()
            end if
        end block body

        call temporary_buffer_end(old, temporary)
    end subroutine save_rmail_mime

    !> Return the region [INDEX HEADER TAGLINE BODY END] of a MIME-entity
    !> at POSITION in SEGMENT.  ENTITY is the MIME-entity at POSITION (0 to
    !> look it up).  The elements are:
    !>   INDEX: index into SEGMENT indicating where POSITION is (1..3)
    !>   HEADER: the position of the beginning of a header
    !>   TAGLINE: the position of the beginning of a tag line, including
    !>            the newline that precedes it
    !>   BODY: the position of the beginning of a body
    !>   END: the position of the end of the entity.
    !> Port of rmailmm.el:rmail-mime-entity-segment.
    subroutine get_rmail_mime_entity_segment(position, entity, segment)
        integer(int64), intent(in) :: position
        integer, intent(in) :: entity
        integer(int64), intent(out) :: segment(5)
        type(excursion_type) :: excursion
        type(display_type) :: current_display
        type(status_type) :: st
        character(len=:), allocatable :: tag, tag2
        integer(int64) :: beginning, tagline_beginning, body_beginning, end, place, found_at, moved, next
        integer :: e, e2
        logical :: found

        call save_excursion_begin(excursion)
        e = entity
        if (e == 0) e = entity_at(position)

        if (e == 0) then
            segment = [1_int64, point(), point(), point(), point()]
            call save_excursion_end(excursion)
            return
        end if

        current_display = entities(e)%current
        beginning = position

        if (position > point_minimum()) then
            if (entity_at(position - 1) == e) then
                call previous_single_property_change(position, value_type(name=RMAIL_MIME_ENTITY_PROPERTY), beginning, &
                                                     found, st, limit=point_minimum())
            end if
        end if

        place = 1
        call goto_character(beginning)
        ! If the header is displayed, get past it to the tagline.
        if (current_display%header /= RMAIL_MIME_DISPLAY_HIDDEN) call search_forward(LF//LF, found_at, st, &
                                                                                     noerror=SEARCH_NOERROR)
        tagline_beginning = point()
        if (position >= tagline_beginning) place = 2
        ! If the tagline is displayed, get past it to the body.  The next
        ! forward_line call must be in sync with how
        ! `insert_rmail_mime_tagline' formats the tagline.  The body begins
        ! after the empty line that ends the tagline.
        if (current_display%tagline) moved = forward_line(3_int64)
        body_beginning = point()
        if (position >= body_beginning) place = 3

        ! If the body is displayed, find its end.
        if (current_display%body /= RMAIL_MIME_DISPLAY_HIDDEN) then
            tag = entities(e)%tag
            call next_single_property_change(beginning, value_type(name=RMAIL_MIME_ENTITY_PROPERTY), end, found, st, &
                                             limit=point_maximum())

            ! `tag' is either an empty string or "/n" where n is the number
            ! of the part of the multipart MIME message.  The loop below
            ! finds the next location whose `rmail_mime_entity' property
            ! specifies a tag of a different value.
            do while (end < point_maximum())
                e2 = entity_at(end)
                if (e2 == 0) exit
                tag2 = entities(e2)%tag
                if (len(tag2) == 0) exit
                if (index(tag2, tag) /= 1) exit
                call next_single_property_change(end, value_type(name=RMAIL_MIME_ENTITY_PROPERTY), next, found, st, &
                                                 limit=point_maximum())
                end = next
            end do
        else
            end = body_beginning
        end if

        segment = [place, beginning, tagline_beginning, body_beginning, end]
        call save_excursion_end(excursion)
    end subroutine get_rmail_mime_entity_segment

    !> Make MIME-entity ENTITY display in the default way.  Port of
    !> rmailmm.el:rmail-mime-shown-mode.
    recursive subroutine set_rmail_mime_shown_mode(entity)
        integer, intent(in) :: entity
        integer :: k

        entities(entity)%new%header = entities(entity)%header%display
        entities(entity)%new%tagline = entities(entity)%tagline
        entities(entity)%new%body = entities(entity)%body%display

        do k = 1, size(entities(entity)%children)
            call set_rmail_mime_shown_mode(entities(entity)%children(k))
        end do
    end subroutine set_rmail_mime_shown_mode

    !> Make MIME-entity ENTITY display in hidden mode.  Port of
    !> rmailmm.el:rmail-mime-hidden-mode.
    recursive subroutine set_rmail_mime_hidden_mode(entity)
        integer, intent(in) :: entity
        integer :: k

        entities(entity)%new = display_type(RMAIL_MIME_DISPLAY_HIDDEN, .true., RMAIL_MIME_DISPLAY_HIDDEN)

        do k = 1, size(entities(entity)%children)
            call set_rmail_mime_hidden_mode(entities(entity)%children(k))
        end do
    end subroutine set_rmail_mime_hidden_mode

    !> Make MIME-entity ENTITY display in raw mode.  Port of
    !> rmailmm.el:rmail-mime-raw-mode.
    recursive subroutine set_rmail_mime_raw_mode(entity)
        integer, intent(in) :: entity
        integer :: k

        entities(entity)%new = display_type(RMAIL_MIME_DISPLAY_RAW, .false., RMAIL_MIME_DISPLAY_RAW)

        do k = 1, size(entities(entity)%children)
            call set_rmail_mime_raw_mode(entities(entity)%children(k))
        end do
    end subroutine set_rmail_mime_raw_mode

    !> Toggle on and off the raw display mode of MIME-entity at point.
    !> With optional argument STATE, force the specified display mode.
    !> Use `RMAIL_MIME_DISPLAY_RAW' for raw mode, and
    !> `RMAIL_MIME_DISPLAY_DECODED' for decoded mode.  Port of
    !> rmailmm.el:rmail-mime-toggle-raw.
    recursive subroutine toggle_rmail_mime_raw(status, state)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: state
        integer(int64) :: position, segment(5)
        type(excursion_type) :: excursion
        logical :: modified, raw, saved_read_only
        integer :: e

        status = status_type()
        position = point()
        if (is_end_of_buffer()) position = point_maximum() - 1
        e = entity_at(position)
        if (e == 0) return
        call get_rmail_mime_entity_segment(position, e, segment)

        if (present(state)) then
            raw = state == RMAIL_MIME_DISPLAY_RAW
        else
            raw = entities(e)%current%header /= RMAIL_MIME_DISPLAY_RAW
        end if

        if (raw) then
            ! Enter the raw mode.
            call set_rmail_mime_raw_mode(e)
        else
            ! Enter the shown mode.
            call set_rmail_mime_shown_mode(e)
            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) return
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call save_excursion_begin(excursion)
            call goto_character(segment(2))
            call insert_rmail_mime(e, status)
            call restore_buffer_modified(modified)
            call save_excursion_end(excursion)
            buffers(current)%read_only = saved_read_only
        end if
    end subroutine toggle_rmail_mime_raw

    !> Hide or show the body of the MIME-entity at point.  Port of
    !> rmailmm.el:rmail-mime-toggle-hidden.
    recursive subroutine toggle_rmail_mime_hidden(status)
        type(status_type), intent(out) :: status
        integer(int64) :: position, segment(5)
        type(excursion_type) :: excursion
        type(handle_type) :: saved_mbox, saved_view
        logical :: modified, answer, saved_read_only
        integer :: e, k

        status = status_type()
        if (.not. is_rmail_mime_message()) return
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        saved_mbox = mime_mbox_buffer
        saved_view = mime_view_buffer
        mime_mbox_buffer = rmail_states(k)%view_buffer
        mime_view_buffer = current_buffer()
        position = point()
        if (is_end_of_buffer()) position = point_maximum() - 1
        e = entity_at(position)

        body: block
            if (e == 0) exit body
            call get_rmail_mime_entity_segment(position, e, segment)

            if (entities(e)%current%body /= RMAIL_MIME_DISPLAY_HIDDEN) then
                ! Enter the hidden mode.  If point is in the body part, move
                ! it to the tagline (or the header if tagline is not
                ! displayed).
                if (segment(1) == 3) call goto_character(segment(3))
                call set_rmail_mime_hidden_mode(e)
                ! If the current entity is the topmost one, display the
                ! header.
                if (segment(2) == point_minimum()) entities(e)%new%header = RMAIL_MIME_DISPLAY_DECODED
            else
                ! Query as a warning before showing if truncated.
                if (entities(e)%truncated) then
                    call read_y_or_n("This entity is truncated; show anyway? ", answer, status)
                    if (.not. status%ok()) exit body

                    if (.not. answer) then
                        call set_error(status, GENERIC_ERROR_CODE, "Aborted")
                        exit body
                    end if
                end if

                ! Enter the shown mode.
                call set_rmail_mime_shown_mode(e)
                ! Force this body shown.
                entities(e)%new%body = RMAIL_MIME_DISPLAY_DECODED
            end if

            call is_buffer_modified(current_buffer(), modified, status)
            if (.not. status%ok()) exit body
            mime_mbox_buffer = rmail_states(k)%view_buffer
            mime_view_buffer = rmail_buffer()
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            call save_excursion_begin(excursion)
            call goto_character(segment(2))
            call insert_rmail_mime(e, status)
            call restore_buffer_modified(modified)
            call save_excursion_end(excursion)
            buffers(current)%read_only = saved_read_only
        end block body

        mime_mbox_buffer = saved_mbox
        mime_view_buffer = saved_view
    end subroutine toggle_rmail_mime_hidden

    ! ------------------------------------------------------------------
    ! Handlers
    ! ------------------------------------------------------------------

    !> Insert a tag line for MIME-entity ENTITY.
    !> FILENAME and SIZE_TEXT, if present, add the Save button of an
    !> attachment and its size to the tag line.  Port of
    !> rmailmm.el:rmail-mime-insert-tagline (the items of its one caller
    !> with items, `insert_rmail_mime_bulk').
    recursive subroutine insert_rmail_mime_tagline(entity, status, filename, size_text)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: filename, size_text
        type(button_type) :: button
        character(len=:), allocatable :: tag, label

        ! Precede the tagline by an empty line to make it a separate
        ! paragraph, so that it is aligned to the left margin of the window
        ! even if preceded by a right-to-left paragraph.
        call insert(LF//"[", status)
        if (.not. status%ok()) return
        tag = entities(entity)%tag

        if (len(tag) > 0) then
            call insert(tag(2:)//":", status)
            if (.not. status%ok()) return
        end if

        call insert(name_of(entities(entity)%content_type)//" ", status)
        if (.not. status%ok()) return
        label = "Show"
        if (entities(entity)%new%body /= RMAIL_MIME_DISPLAY_HIDDEN) label = "Hide"
        call insert_button(label, button, status, kind=rmail_mime_toggle_button, help_echo="mouse-2, RET: Toggle show/hide")
        if (.not. status%ok()) return

        if (present(filename)) then
            call insert(" Save:", status)
            if (.not. status%ok()) return
            call insert_button(filename, button, status, kind=rmail_mime_save_button, &
                               help_echo="mouse-2, RET: Save attachment", button_data=value_type(int(entity, int64)))
            if (.not. status%ok()) return
        end if

        if (present(size_text)) then
            call insert(size_text, status)
            if (.not. status%ok()) return
        end if

        ! Follow the tagline by an empty line to make it a separate
        ! paragraph, so that the paragraph direction of the following text
        ! is determined based on that text.
        call insert("]"//LF//LF, status)
    end subroutine insert_rmail_mime_tagline

    !> Update the current tag line for MIME-entity ENTITY.  Port of
    !> rmailmm.el:rmail-mime-update-tagline.
    recursive subroutine update_rmail_mime_tagline(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        type(button_type) :: button
        type(excursion_type) :: excursion
        character(len=:), allocatable :: label
        logical :: modified, found, saved_read_only
        integer(int64) :: moved

        status = status_type()
        call is_buffer_modified(current_buffer(), modified, status)
        if (.not. status%ok()) return
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        ! If we are going to show the body, the new button label is
        ! "Hide".  Otherwise, it's "Show".
        label = "Show"
        if (entities(entity)%new%body /= RMAIL_MIME_DISPLAY_HIDDEN) label = "Hide"

        body: block
            call next_button(point(), button, found)
            if (.not. found) exit body
            ! Go to the second character of the button "Show" or "Hide".
            call goto_character(button_start(button) + 1)
            call button_at(point(), button, found)
            if (.not. found) exit body
            call save_excursion_begin(excursion)
            call insert(label, status)
            if (status%ok()) call delete_region(point(), button_end(button), status)
            call save_excursion_end(excursion)
            if (.not. status%ok()) exit body
            call delete_region(button_start(button), point(), status)
            if (.not. status%ok()) exit body
            call put_entity(point(), button_end(button), entity, status)
            if (.not. status%ok()) exit body
            call restore_buffer_modified(modified)
            ! The following call to forward_line must be in sync with how
            ! insert_rmail_mime_tagline formats the tagline.
            moved = forward_line(2_int64)
        end block body

        buffers(current)%read_only = saved_read_only
    end subroutine update_rmail_mime_tagline

    !> Decode and insert a MIME-entity header, from BEGINNING to END of
    !> the mbox buffer, in the current buffer.  Port of
    !> rmailmm.el:rmail-mime-insert-header.
    recursive subroutine insert_rmail_mime_header(beginning, status)
        integer(int64), intent(in) :: beginning
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction
        type(excursion_type) :: excursion
        type(handle_type) :: here
        integer(int64) :: position
        type(status_type) :: st

        position = point()
        call last_coding_system_used%clear()
        call save_restriction_begin(restriction)

        body: block
            call narrow_to_region(position, position, status)
            if (.not. status%ok()) exit body
            here = current_buffer()
            call set_buffer(mime_mbox_buffer, status)
            if (.not. status%ok()) exit body
            call save_excursion_begin(excursion)
            call goto_character(beginning)
            call copy_rmail_headers(mime_view_buffer, point(), status)
            call save_excursion_end(excursion)
            call set_buffer(here, st)
            if (.not. status%ok()) exit body
            call decode_rfc2047_region(position, point(), status)
            if (.not. status%ok()) exit body

            if (allocated(last_coding_system_used%value) .and. len(mime_coding_system) == 0) then
                mime_coding_system = last_coding_system_used%value
                mime_coding_system_from_header = .true.
            end if

            call goto_character(point_minimum())
            call highlight_rmail_headers(status)
            if (.not. status%ok()) exit body
            call goto_character(point_maximum())
            call insert(LF, status)
        end block body

        call save_restriction_end(restriction)
    end subroutine insert_rmail_mime_header

    !> Return the last coding system used to decode the header from
    !> BEGINNING to END of the mbox buffer in CODING_SYSTEM.  Port of
    !> rmailmm.el:rmail-mime-find-header-encoding.
    recursive subroutine find_rmail_mime_header_encoding(beginning, coding_system, status)
        integer(int64), intent(in) :: beginning
        character(len=:), allocatable, intent(out) :: coding_system
        type(status_type), intent(out) :: status
        type(handle_type) :: old, temporary
        type(excursion_type) :: excursion

        coding_system = ""
        call temporary_buffer_begin(old, temporary, .true., status)
        if (.not. status%ok()) return

        body: block
            call set_buffer(mime_mbox_buffer, status)
            if (.not. status%ok()) exit body
            call last_coding_system_used%clear()
            call save_excursion_begin(excursion)
            call goto_character(beginning)
            call copy_rmail_headers(temporary, point(), status)
            call save_excursion_end(excursion)
            call set_buffer(temporary, status)
            if (.not. status%ok()) exit body
            call decode_rfc2047_region(point_minimum(), point_maximum(), status)
            if (.not. status%ok()) exit body
            if (allocated(last_coding_system_used%value)) coding_system = last_coding_system_used%value
        end block body

        call temporary_buffer_end(old, temporary)
    end subroutine find_rmail_mime_header_encoding

    !> Handle the current buffer as a plain text MIME part.  Port of
    !> rmailmm.el:rmail-mime-text-handler.
    recursive subroutine handle_rmail_mime_text(content_type, content_disposition, content_transfer_encoding, handled, status)
        type(mime_header_value_type), intent(in) :: content_type, content_disposition
        character(len=*), intent(in) :: content_transfer_encoding
        logical, intent(out) :: handled
        type(status_type), intent(out) :: status
        integer :: e

        call make_entity(e)
        entities(e)%content_type = content_type
        entities(e)%content_disposition = content_disposition
        entities(e)%transfer_encoding = content_transfer_encoding
        entities(e)%new%body = RMAIL_MIME_DISPLAY_DECODED
        entities(e)%tagline = .true.
        entities(e)%handler => insert_rmail_mime_text
        call insert_rmail_mime_text(e, status)
        handled = .true.
    end subroutine handle_rmail_mime_text

    !> Decode and insert the text body of MIME-entity ENTITY.  Port of
    !> rmailmm.el:rmail-mime-insert-decoded-text.
    recursive subroutine insert_rmail_mime_decoded_text(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: character_set, coding_system
        type(handle_type) :: end_marker
        integer(int64) :: position, inserted
        type(status_type) :: st

        call get_rfc2231_value(entities(entity)%content_type, "charset", character_set)
        coding_system = ""
        if (allocated(character_set)) coding_system = coding_system_from_name(character_set)

        if (len(coding_system) == 0) then
            coding_system = "undecided"
        else if (.not. is_coding_system(coding_system)) then
            coding_system = "undecided"
        end if

        position = point()

        if (allocated(entities(entity)%body%data)) then
            call insert(entities(entity)%body%data, status)
            if (.not. status%ok()) return
            call copy_marker_at(point(), end_marker, .true., status)
            if (.not. status%ok()) return
        else
            call insert_buffer_substring(mime_mbox_buffer, status, position_of(entities(entity)%body%beginning), &
                                         position_of(entities(entity)%body%end))
            if (.not. status%ok()) return
            call copy_marker_at(point(), end_marker, .true., status)
            if (.not. status%ok()) return
            call decode_transfer_encoding(entities(entity)%transfer_encoding, position, end_marker, status)

            if (.not. status%ok()) then
                call destroy_marker(end_marker, st)
                return
            end if
        end if

        ! If the text is empty, we don't have anything to decode.
        if (position /= point()) then
            ! Use -dos decoding, to remove ^M characters left from base64
            ! or rogue qp-encoded text.
            call decode_coding_region(position, point(), coding_system_change_end_of_line_conversion(coding_system, &
                                                                                                     END_OF_LINE_DOS), &
                                      inserted, status)
            call goto_character(position_of(end_marker))
        end if

        call destroy_marker(end_marker, st)
        if (.not. status%ok()) return

        if (len(mime_coding_system) == 0 .or. mime_coding_system_from_header) then
            if (coding_system_base(coding_system) /= "us-ascii") then
                mime_coding_system = coding_system
                mime_coding_system_from_header = .false.
            end if
        end if

        if (.not. is_beginning_of_line()) call insert(LF, status)
    end subroutine insert_rmail_mime_decoded_text

    ! Do the header and tagline steps of inserting entity E, whose segment
    ! is SEGMENT; FILENAME and SIZE_TEXT are the items of the tagline.
    ! Not a port: the code rmail-mime-insert-text, rmail-mime-insert-bulk
    ! and rmail-mime-insert-multipart share.
    recursive subroutine insert_header_and_tagline(e, segment, status, filename, size_text)
        integer, intent(in) :: e
        integer(int64), intent(in) :: segment(5)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: filename, size_text
        type(display_type) :: current_display, new_display

        status = status_type()
        current_display = entities(e)%current
        new_display = entities(e)%new

        ! header
        if (current_display%header == new_display%header) then
            call goto_character(segment(3))
        else
            if (current_display%header /= RMAIL_MIME_DISPLAY_HIDDEN) call delete_forward(segment(3) - segment(2), status)
            if (.not. status%ok()) return

            if (new_display%header /= RMAIL_MIME_DISPLAY_HIDDEN) then
                call insert_rmail_mime_header(position_of(entities(e)%header%beginning), status)
                if (.not. status%ok()) return
            end if
        end if

        ! tagline
        if (current_display%tagline .eqv. new_display%tagline) then
            if (.not. current_display%tagline .or. current_display%body == new_display%body) then
                call goto_character(point() + segment(4) - segment(3))
            else
                call update_rmail_mime_tagline(e, status)
            end if
        else
            if (current_display%tagline) call delete_forward(segment(4) - segment(3), status)
            if (.not. status%ok()) return
            if (new_display%tagline) call insert_rmail_mime_tagline(e, status, filename, size_text)
        end if
    end subroutine insert_header_and_tagline

    !> Presentation handler for a plain text MIME entity.  Port of
    !> rmailmm.el:rmail-mime-insert-text.
    recursive subroutine insert_rmail_mime_text(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, segment(5)
        type(display_type) :: current_display, new_display

        beginning = point()
        call get_rmail_mime_entity_segment(point(), entity, segment)

        if (entities(entity)%body%beginning%index == 0) then
            entities(entity)%body%data = buffer_string()
            call delete_region(point_minimum(), point_maximum(), status)
            if (.not. status%ok()) return
        end if

        call insert_header_and_tagline(entity, segment, status)
        if (.not. status%ok()) return
        current_display = entities(entity)%current
        new_display = entities(entity)%new

        ! body
        if (current_display%body == new_display%body) then
            call goto_character(point() + segment(5) - segment(4))
        else
            if (current_display%body /= RMAIL_MIME_DISPLAY_HIDDEN) call delete_forward(segment(5) - segment(4), status)
            if (.not. status%ok()) return
            if (new_display%body /= RMAIL_MIME_DISPLAY_HIDDEN) call insert_rmail_mime_decoded_text(entity, status)
            if (.not. status%ok()) return
        end if

        call put_entity(beginning, point(), entity, status)
    end subroutine insert_rmail_mime_text

    !> Decode, render, and insert html from MIME-entity ENTITY.  Port of
    !> rmailmm.el:rmail-mime-insert-html.
    recursive subroutine insert_rmail_mime_html(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: character_set, coding_system
        type(handle_type) :: old, temporary, end_marker
        type(excursion_type) :: excursion
        integer(int64) :: found, inserted, start
        logical :: saved_fold
        type(status_type) :: st

        call get_rfc2231_value(entities(entity)%content_type, "charset", character_set)
        coding_system = ""
        if (allocated(character_set)) coding_system = coding_system_from_name(character_set)

        if (len(coding_system) == 0) then
            coding_system = "undecided"
        else if (.not. is_coding_system(coding_system)) then
            coding_system = "undecided"
        end if

        call temporary_buffer_begin(old, temporary, .false., status)
        if (.not. status%ok()) return

        body: block
            call insert_mbox_bytes(position_of(entities(entity)%body%beginning), position_of(entities(entity)%body%end), &
                                   status)
            if (.not. status%ok()) exit body
            call copy_marker_at(point(), end_marker, .true., status)
            if (.not. status%ok()) exit body
            call decode_transfer_encoding(entities(entity)%transfer_encoding, point_minimum(), end_marker, status)
            call destroy_marker(end_marker, st)
            if (.not. status%ok()) exit body

            ! Some broken MUAs state the charset only in the HTML <head>,
            ! so if we don't have a non-trivial coding-system at this
            ! point, make one last attempt to find it there.
            if (coding_system == "undecided") then
                call save_excursion_begin(excursion)
                call goto_character(point_minimum())
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                call regular_expression_search_forward("^<html><head><meta[^;]*; charset=\([-a-zA-Z0-9]+\)", found, st, &
                                                       noerror=SEARCH_NOERROR)
                case_fold_search%value = saved_fold

                if (found > 0) then
                    coding_system = coding_system_from_name(matched_text())

                    if (len(coding_system) == 0) then
                        coding_system = "undecided"
                    else if (.not. is_coding_system(coding_system)) then
                        coding_system = "undecided"
                    end if
                end if

                ! Finally, let them manually force decoding if they know it.
                if (coding_system == "undecided" .and. allocated(coding_system_for_read%value)) then
                    coding_system = coding_system_for_read%value
                end if

                call save_excursion_end(excursion)
            end if

            ! Use -dos decoding, to remove ^M characters left from base64 or
            ! rogue qp-encoded text.
            call decode_coding_region(point_minimum(), point(), coding_system_change_end_of_line_conversion(coding_system, &
                                                                                                            END_OF_LINE_DOS), &
                                      inserted, status)
            if (.not. status%ok()) exit body

            if (len(mime_coding_system) == 0 .or. mime_coding_system_from_header) then
                if (coding_system_base(coding_system) /= "us-ascii") then
                    mime_coding_system = coding_system
                    mime_coding_system_from_header = .false.
                end if
            end if

            ! Convert html in temporary buffer to text and insert in original
            ! buffer.
            call set_buffer(old, status)
            if (.not. status%ok()) exit body
            start = point()

            if (associated(rmail_mime_render_html_function)) then
                call rmail_mime_render_html_function(temporary, status)
            else
                call insert_buffer_substring(temporary, status)
            end if

            if (.not. status%ok()) exit body
            call fix_rmail_mime_inserted_faces(start, status)
        end block body

        call temporary_buffer_end(old, temporary)

    contains

        ! The text of the first group of the last match.
        function matched_text() result(text)
            character(len=:), allocatable :: text
            type(status_type) :: st2

            call buffer_substring_no_properties(match_beginning(1, st2), match_end(1, st2), text, st2)
            if (.not. allocated(text)) text = ""
        end function matched_text
    end subroutine insert_rmail_mime_html

    !> Convert the HTML in SOURCE to text in the current buffer with lynx.
    !> Port of rmailmm.el:rmail-mime-render-html-lynx.
    recursive subroutine render_rmail_mime_html_lynx(source, status)
        type(handle_type), intent(in) :: source
        type(status_type), intent(out) :: status
        type(handle_type) :: destination
        type(value_type) :: result

        destination = current_buffer()
        call set_buffer(source, status)
        if (.not. status%ok()) return
        call execute_process_region("lynx", result, status, start=point_minimum(), end=point_maximum(), &
                                    destination=make_buffer_reference(destination), &
                                    arguments=list(value_type(text="-stdin"), value_type(text="-dump"), &
                                                   value_type(text="-force_html"), value_type(text="-dont_wrap_pre"), &
                                                   value_type(text="-width=70")))
        call set_buffer(destination, status)
    end subroutine render_rmail_mime_html_lynx

    !> Put `font_lock_face' properties matching `face' properties on text
    !> inserted, e.g., by a renderer, in text from START to point.  Port of
    !> rmailmm.el:rmail-mime-fix-inserted-faces.
    subroutine fix_rmail_mime_inserted_faces(start, status)
        integer(int64), intent(in) :: start
        type(status_type), intent(out) :: status
        type(value_type) :: face
        integer(int64) :: position, next
        logical :: found

        status = status_type()
        position = start

        do while (position < point())
            call get_text_property(position, value_type(name=FACE_PROPERTY), face, status)
            if (.not. status%ok()) return
            call next_single_property_change(position, value_type(name=FACE_PROPERTY), next, found, status, limit=point())
            if (.not. status%ok()) return
            ! Anything to do?
            if (allocated(face%a)) call put_text_property(position, next, value_type(name=FONT_LOCK_FACE_PROPERTY), face, &
                                                          status)
            if (.not. status%ok()) return
            position = next
        end do
    end subroutine fix_rmail_mime_inserted_faces

    !> Hide or show the body of the MIME-entity associated with BUTTON.
    !> Port of rmailmm.el:rmail-mime-toggle-button.
    recursive subroutine toggle_rmail_mime_button(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion

        call save_excursion_begin(excursion)
        call goto_character(button_start(button))
        call toggle_rmail_mime_hidden(status)
        call save_excursion_end(excursion)
    end subroutine toggle_rmail_mime_button

    !> Handle the current buffer as an attachment to download.  Port of
    !> rmailmm.el:rmail-mime-bulk-handler (images are Deferred).
    recursive subroutine handle_rmail_mime_bulk(content_type, content_disposition, content_transfer_encoding, handled, status)
        type(mime_header_value_type), intent(in) :: content_type, content_disposition
        character(len=*), intent(in) :: content_transfer_encoding
        logical, intent(out) :: handled
        type(status_type), intent(out) :: status
        integer :: e

        call make_entity(e)
        entities(e)%content_type = content_type
        entities(e)%content_disposition = content_disposition
        entities(e)%transfer_encoding = content_transfer_encoding
        entities(e)%new%tagline = .true.
        entities(e)%tagline = .true.
        entities(e)%handler => insert_rmail_mime_bulk
        call insert_rmail_mime_bulk(e, status)
        ! Like upstream's, whose value is that of put-text-property.
        handled = .false.
    end subroutine handle_rmail_mime_bulk

    !> Setup the information about the attachment object for MIME-entity
    !> ENTITY.  TO_SHOW is true if and only if the attachment object should
    !> be shown directly.  Port of rmailmm.el:rmail-mime-set-bulk-data (no
    !> image type is supported, as in an Emacs built without images, so no
    !> attachment is shown directly).
    subroutine set_rmail_mime_bulk_data(entity, to_show)
        integer, intent(in) :: entity
        logical, intent(out) :: to_show
        character(len=:), allocatable :: content_type, size_text
        real(real64) :: data_size
        integer :: iostat

        to_show = .false.
        content_type = name_of(entities(entity)%content_type)
        call get_rfc2231_value(entities(entity)%content_disposition, "size", size_text)

        if (allocated(size_text)) then
            data_size = 0
            read (size_text, *, iostat=iostat) data_size
            if (iostat /= 0) data_size = 0
        else if (allocated(entities(entity)%body%data)) then
            data_size = len(entities(entity)%body%data)
        else
            ! Rough estimation of the size.
            data_size = position_of(entities(entity)%body%end) - position_of(entities(entity)%body%beginning)

            if (entities(entity)%transfer_encoding == "base64") then
                ! https://en.wikipedia.org/wiki/Base64#MIME
                data_size = data_size*0.73_real64
            else if (entities(entity)%transfer_encoding == "quoted-printable") then
                ! Assume most of the text is ASCII...
                data_size = real((int(data_size, int64)*5)/7, real64)
            end if
        end if

        if (index(content_type, "text/html") > 0) then
            entities(entity)%bulk = BULK_HTML
        else if (index(content_type, "text/") > 0) then
            entities(entity)%bulk = BULK_TEXT
        else
            entities(entity)%bulk = BULK_NONE
        end if

        entities(entity)%size = data_size
    end subroutine set_rmail_mime_bulk_data

    !> Presentation handler for an attachment MIME entity.  Port of
    !> rmailmm.el:rmail-mime-insert-bulk (images are Deferred).
    recursive subroutine insert_rmail_mime_bulk(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        character(len=*), parameter :: UNITS(4) = ["B ", "kB", "MB", "GB"]
        character(len=:), allocatable :: content_type, directory, filename, size_text, value
        character(len=40) :: number
        integer(int64) :: beginning, segment(5)
        type(display_type) :: current_display, new_display
        real(real64) :: data_size
        logical :: to_show
        integer :: k, unit

        content_type = name_of(entities(entity)%content_type)
        ! Find the default directory for this media type.
        directory = "~"

        if (allocated(rmail_mime_attachment_directories)) then
            do k = 1, size(rmail_mime_attachment_directories)
                if (string_match(trim(rmail_mime_attachment_directories(k)%content_type), content_type, status) == 0) cycle
                if (.not. status%ok()) return

                if (is_file_directory(trim(rmail_mime_attachment_directories(k)%directory))) then
                    directory = trim(rmail_mime_attachment_directories(k)%directory)
                    exit
                end if
            end do
        end if

        call get_rfc2231_value(entities(entity)%content_type, "name", value)
        if (.not. allocated(value)) call get_rfc2231_value(entities(entity)%content_disposition, "filename", value)

        if (allocated(value)) then
            filename = value
        else
            filename = "noname"
        end if

        call get_rmail_mime_entity_segment(point(), entity, segment)

        if (entities(entity)%body%beginning%index /= 0) then
            data_size = entities(entity)%size
        else
            if (.not. allocated(entities(entity)%body%data)) then
                entities(entity)%body%data = buffer_string()

                if (buffers(current)%multibyte) then
                    call set_error(status, GENERIC_ERROR_CODE, "Assertion failed: the data of an attachment is unibyte")
                    return
                end if

                call set_rmail_mime_bulk_data(entity, to_show)
                call delete_region(point_minimum(), point_maximum(), status)
                if (.not. status%ok()) return
            end if

            data_size = len(entities(entity)%body%data)
        end if

        unit = 1

        ! Cribbed from gnus-agent-expire-done-message.
        do while (data_size > 1024.0_real64 .and. unit < size(UNITS))
            data_size = data_size/1024.0_real64
            unit = unit + 1
        end do

        write (number, "(i0)") int(ieee_rint(data_size), int64)
        size_text = " ("//trim(number)//trim(UNITS(unit))//")"
        entities(entity)%filename = filename
        entities(entity)%directory = file_name_as_directory(directory)
        beginning = point()
        call insert_header_and_tagline(entity, segment, status, filename, size_text)
        if (.not. status%ok()) return
        current_display = entities(entity)%current
        new_display = entities(entity)%new

        ! body
        if (current_display%body == new_display%body) then
            call goto_character(point() + segment(5) - segment(4))
        else
            if (current_display%body /= RMAIL_MIME_DISPLAY_HIDDEN) call delete_forward(segment(5) - segment(4), status)
            if (.not. status%ok()) return

            if (new_display%body /= RMAIL_MIME_DISPLAY_HIDDEN) then
                ! Render HTML if display single message, but if searching
                ! don't render but just search HTML itself.  As we don't
                ! know how to display other bodies, just insert them as a
                ! text.
                if (entities(entity)%bulk == BULK_HTML .and. .not. searching) then
                    call insert_rmail_mime_html(entity, status)
                else
                    call insert_rmail_mime_decoded_text(entity, status)
                end if

                if (.not. status%ok()) return
            end if
        end if

        call put_entity(beginning, point(), entity, status)
    end subroutine insert_rmail_mime_bulk

    !> Handle the current buffer as a multipart MIME body.
    !> The current buffer should be narrowed to the body.  CONTENT_TYPE,
    !> CONTENT_DISPOSITION, and CONTENT_TRANSFER_ENCODING are the values
    !> of the respective parsed headers.  See `handle_rmail_mime' for their
    !> format.  Port of rmailmm.el:rmail-mime-multipart-handler.
    recursive subroutine handle_rmail_mime_multipart(content_type, content_disposition, content_transfer_encoding, &
                                                     handled, status)
        type(mime_header_value_type), intent(in) :: content_type, content_disposition
        character(len=*), intent(in) :: content_transfer_encoding
        logical, intent(out) :: handled
        type(status_type), intent(out) :: status
        integer, allocatable :: children(:)

        call process_rmail_mime_multipart(content_type, content_disposition, content_transfer_encoding, children, status)
        handled = .true.
    end subroutine handle_rmail_mime_multipart

    !> Process the current buffer as a multipart MIME body.
    !>
    !> If PARSE_TAG is absent, modify the current buffer directly for
    !> showing the MIME body.
    !>
    !> Otherwise, PARSE_TAG is a string indicating the depth and index
    !> number of the entity.  In this case, parse the current buffer and
    !> return the MIME entities in CHILDREN.
    !>
    !> The other arguments are the same as `handle_rmail_mime_multipart'.
    !> Port of rmailmm.el:rmail-mime-process-multipart.
    recursive subroutine process_rmail_mime_multipart(content_type, content_disposition, content_transfer_encoding, &
                                                      children, status, parse_tag)
        type(mime_header_value_type), intent(in) :: content_type, content_disposition
        character(len=*), intent(in) :: content_transfer_encoding
        integer, allocatable, intent(out) :: children(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: parse_tag
        character(len=:), allocatable :: boundary, subtype, child_tag
        type(mime_header_value_type) :: child_type
        type(restriction_type) :: restriction
        type(excursion_type) :: excursion
        type(handle_type) :: next
        integer(int64) :: beginning, end, found, moved
        integer :: part_index, child, slash, best, best_priority, priority, k
        logical :: truncated, last, has_next, matched, has_child_type
        character(len=:), allocatable :: child_kind, disposition
        character(len=12) :: number
        type(status_type) :: st

        status = status_type()
        allocate (children(0))
        call get_rfc2231_value(content_type, "boundary", boundary)
        subtype = ""
        slash = index(name_of(content_type), "/")
        if (slash > 0) subtype = name_of(content_type)
        if (slash > 0) subtype = subtype(slash + 1:)
        if (index(subtype, "/") > 0) subtype = subtype(1:index(subtype, "/") - 1)
        part_index = 0

        if (.not. allocated(boundary)) then
            call set_rmail_mime_boundary_error("No boundary defined", content_type, content_disposition, &
                                               content_transfer_encoding, status)
            return
        end if

        boundary = LF//"--"//boundary
        ! Hide the body before the first bodypart.
        call goto_character(point_minimum())
        call search_forward(boundary, found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return

        if (found > 0) then
            matched = looking_at("[ "//TAB//"]*"//LF, status)
            if (.not. status%ok()) return

            if (matched) then
                if (present(parse_tag)) then
                    call narrow_to_region(match_end(0, st), point_maximum(), status)
                else
                    call delete_region(point_minimum(), match_end(0, st), status)
                end if

                if (.not. status%ok()) return
            end if
        end if

        ! Change content-type to the proper default one for the children.
        has_child_type = .true.

        if (index(subtype, "mixed") > 0) then
            child_type = plain_header("text/plain")
        else if (index(subtype, "digest") > 0) then
            child_type = plain_header("message/rfc822")
        else
            has_child_type = .false.
        end if

        ! Loop over all body parts, where beginning points at the beginning
        ! of the part and end points at the end of the part.  next points
        ! at the beginning of the next part.  The current point is just
        ! after the boundary tag.
        beginning = point_minimum()
        last = .false.

        do
            call search_forward(boundary, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return

            if (found > 0) then
                truncated = .false.
                end = match_beginning(0, st)
            else
                ! If the boundary does not appear at all, the message was
                ! truncated.  Handle the rest of the truncated message (if
                ! it isn't empty) by pretending that the boundary appears
                ! at the end of the message.  We use `last' to distinguish
                ! this from the more likely situation of there being an
                ! epilogue after the last boundary, which should be
                ! ignored.  See rmailmm-test-multipart-handler for an
                ! example, and also bug#10101.
                if (last) exit
                call save_excursion_begin(excursion)
                moved = skip_characters_forward(LF, st)
                found = point()
                call save_excursion_end(excursion)
                if (point_maximum() <= found) exit
                truncated = .true.
                end = point_maximum()
            end if

            ! If this is the last boundary according to RFC 2046, hide the
            ! epilogue, else hide the boundary only.  Use a marker for
            ! `next' because `show_rmail_mime_buffer' may change the
            ! buffer.
            has_next = .true.

            if (looking_at("--[ "//TAB//"]*$", status)) then
                call copy_marker_at(point_maximum(), next, .false., status)
                last = .true.
            else if (looking_at("[ "//TAB//"]*"//LF, status)) then
                call copy_marker_at(match_end(0, st), next, .true., status)
            else if (truncated) then
                ! We're handling what's left of a truncated message.
                call copy_marker_at(point_maximum(), next, .false., status)
            else
                ! This line may be a boundary of nested multipart, so we
                ! just skip it.
                has_next = .false.
            end if

            if (.not. status%ok()) return

            if (has_next) then
                part_index = part_index + 1
                ! Handle the part.
                call save_restriction_begin(restriction)

                if (present(parse_tag)) then
                    call narrow_to_region(beginning, end, status)

                    if (status%ok()) then
                        write (number, "(i0)") part_index
                        child_tag = parse_tag//"/"//trim(number)

                        if (has_child_type) then
                            call process_rmail_mime(.false., child, status, child_tag, child_type, content_disposition)
                        else
                            call process_rmail_mime(.false., child, status, child_tag, &
                                                    default_content_disposition=content_disposition)
                        end if
                    end if

                    if (status%ok()) then
                        ! Display a tagline.
                        entities(child)%tagline = .true.
                        entities(child)%new%tagline = .true.
                        entities(child)%truncated = truncated
                        children = [children, child]
                    end if
                else
                    call delete_region(end, position_of(next), status)
                    if (status%ok()) call narrow_to_region(beginning, end, status)
                    if (status%ok()) call show_rmail_mime_buffer(status)
                end if

                call save_restriction_end(restriction)
                beginning = position_of(next)
                call destroy_marker(next, st)
                if (.not. status%ok()) return
                call goto_character(beginning)
            end if
        end do

        if (.not. present(parse_tag)) return

        if (index(subtype, "alternative") > 0) then
            ! Find the best entity to show, and hide all the others.
            ! If rmail_mime_prefer_html is set, html is best, then plain.
            ! If not, plain is best, then html.
            ! Then comes any other text part.
            ! If thereto of the same type, earlier entities in the message
            ! (later in the reverse list) are preferred.
            best = 0
            best_priority = 0

            do k = 1, size(children)
                disposition = name_of(entities(children(k))%content_disposition)

                if (.not. allocated(entities(children(k))%content_disposition%name)) then
                    disposition = name_of(content_disposition)
                end if

                if (disposition /= "inline") cycle
                child_kind = name_of(entities(children(k))%content_type)
                ! Consider all inline text parts.
                if (index(child_kind, "text/") == 0) cycle

                if (index(child_kind, "text/html") > 0) then
                    priority = merge(1, 2, rmail_mime_prefer_html)
                else if (index(child_kind, "text/plain") > 0) then
                    priority = merge(2, 1, rmail_mime_prefer_html)
                else
                    priority = 3
                end if

                if (best == 0 .or. priority <= best_priority) then
                    best = children(k)
                    best_priority = priority
                end if
            end do

            do k = 1, size(children)
                if (children(k) == best) cycle
                entities(children(k))%body%display = RMAIL_MIME_DISPLAY_HIDDEN
                call set_rmail_mime_hidden_mode(children(k))
            end do
        end if
    end subroutine process_rmail_mime_multipart

    !> Presentation handler for a multipart MIME entity.  Port of
    !> rmailmm.el:rmail-mime-insert-multipart.
    recursive subroutine insert_rmail_mime_multipart(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, segment(5)
        type(display_type) :: current_display, new_display
        integer, allocatable :: children(:)
        integer :: k

        beginning = point()
        call get_rmail_mime_entity_segment(point(), entity, segment)
        call insert_header_and_tagline(entity, segment, status)
        if (.not. status%ok()) return
        call put_entity(beginning, point(), entity, status)
        if (.not. status%ok()) return
        current_display = entities(entity)%current
        new_display = entities(entity)%new

        ! body
        if (current_display%body == new_display%body) then
            call goto_character(point() + segment(5) - segment(4))
        else
            children = entities(entity)%children

            do k = 1, size(children)
                call insert_rmail_mime(children(k), status)
                if (.not. status%ok()) return
            end do
        end if
    end subroutine insert_rmail_mime_multipart

    ! ------------------------------------------------------------------
    ! Main code
    ! ------------------------------------------------------------------

    !> Handle the current buffer as a MIME part.
    !> The current buffer should be narrowed to the respective body, and
    !> point should be at the beginning of the body.
    !>
    !> CONTENT_TYPE, CONTENT_DISPOSITION, and CONTENT_TRANSFER_ENCODING
    !> are the values of the respective parsed headers.  The latter should
    !> be lower-case ("" for none).  The parsed headers for CONTENT_TYPE
    !> and CONTENT_DISPOSITION are values of `mime_header_value_type': the
    !> value and its parameters, each an attribute and its value.
    !>
    !> Consider the following header, for example:
    !>
    !> Content-Type: multipart/mixed;
    !>         boundary="----=_NextPart_000_0104_01C617E4.BDEC4C40"
    !>
    !> The parsed header value has the name "multipart/mixed" and the
    !> parameter "boundary" whose value is
    !> "----=_NextPart_000_0104_01C617E4.BDEC4C40".  Port of
    !> rmailmm.el:rmail-mime-handle.
    recursive subroutine handle_rmail_mime(content_type, content_disposition, content_transfer_encoding, status)
        type(mime_header_value_type), intent(in) :: content_type, content_disposition
        character(len=*), intent(in) :: content_transfer_encoding
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: encoding
        type(restriction_type) :: restriction
        integer(int64) :: length
        logical :: stop
        integer :: k, e
        type(status_type) :: st

        status = status_type()
        encoding = content_transfer_encoding

        ! Handle the content transfer encodings we know.  Unknown transfer
        ! encodings will be passed on to the various handlers.
        if (encoding == "base64") then
            call base64_decode_region(point(), point_maximum(), length, st)
            if (st%ok()) encoding = ""
        else if (encoding == "quoted-printable") then
            call decode_quoted_printable_region(point(), point_maximum(), status)
            if (.not. status%ok()) return
            encoding = ""
        else if (encoding == "8bit") then
            ! It just means there's no decoding to do.
            encoding = ""
        end if

        ! Inline stuff requires work.  Attachments are handled by the bulk
        ! handler.
        if (name_of(content_disposition) == "inline") then
            stop = .false.

            if (allocated(rmail_mime_media_type_handlers)) then
                do k = 1, size(rmail_mime_media_type_handlers)
                    if (stop) exit
                    if (.not. associated(rmail_mime_media_type_handlers(k)%handler)) cycle
                    if (string_match(trim(rmail_mime_media_type_handlers(k)%regular_expression), name_of(content_type), &
                                     status) == 0) cycle
                    if (.not. status%ok()) return
                    call rmail_mime_media_type_handlers(k)%handler(content_type, content_disposition, encoding, stop, status)
                    if (.not. status%ok()) return
                end do
            end if
        else
            ! Everything else is an attachment.
            call handle_rmail_mime_bulk(content_type, content_disposition, encoding, stop, status)
            if (.not. status%ok()) return
        end if

        call save_restriction_begin(restriction)
        call widen()

        if (point() > point_minimum()) then
            e = entity_at(point() - 1)
            if (e > 0) entities(e)%current = entities(e)%new
        end if

        call save_restriction_end(restriction)
    end subroutine handle_rmail_mime

    !> Handle the current buffer as a MIME message.
    !> If SHOW_HEADERS is true, then the headers of the current part
    !> will shown as usual for a MIME message.  The headers are also
    !> shown for the content type message/rfc822.  This function will be
    !> called recursively if multiple parts are available.
    !>
    !> The current buffer must contain a single message.  It will be
    !> modified.  Port of rmailmm.el:rmail-mime-show.
    recursive subroutine show_rmail_mime_buffer(status, show_headers)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: show_headers
        integer :: e
        logical :: show

        show = .false.
        if (present(show_headers)) show = show_headers
        call process_rmail_mime(show, e, status)
    end subroutine show_rmail_mime_buffer

    !> Process the current buffer, a message or a part of one: with
    !> PARSE_TAG absent, modify it for showing (`show_rmail_mime_buffer');
    !> with PARSE_TAG present (the depth and index number of the entity,
    !> "" for the message), parse it into the MIME entity ENTITY.
    !> DEFAULT_CONTENT_TYPE and DEFAULT_CONTENT_DISPOSITION are the values
    !> for a part without those headers.  Port of
    !> rmailmm.el:rmail-mime-process.
    recursive subroutine process_rmail_mime(show_headers, entity, status, parse_tag, default_content_type, &
                                            default_content_disposition)
        logical, intent(in) :: show_headers
        integer, intent(out) :: entity
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: parse_tag
        type(mime_header_value_type), intent(in), optional :: default_content_type, default_content_disposition
        character(len=:), allocatable :: type_field, encoding_field, disposition_field, content_transfer_encoding
        type(mime_header_value_type) :: content_type, content_disposition
        type(restriction_type) :: restriction
        integer(int64) :: end, found
        integer, allocatable :: children(:)
        integer :: child, e
        logical :: is_inline, has_character, to_show
        integer :: c

        status = status_type()
        entity = 0
        ! `point_minimum' returns the beginning and `end' points at the end
        ! of the headers.
        end = point_minimum()
        call goto_character(point_minimum())
        call character_after(c, has_character)

        ! If we're showing a part without headers, then it will start with
        ! a newline.
        if (has_character .and. c == iachar(LF)) then
            end = point() + 1
        else
            call search_forward(LF//LF, found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return

            if (found > 0) then
                end = point()
                call save_restriction_begin(restriction)
                call narrow_to_region(point_minimum(), end, status)
                if (status%ok()) call get_mail_field("Content-Type", type_field, status)
                if (status%ok()) call get_mail_field("Content-Transfer-Encoding", encoding_field, status)
                if (status%ok()) call get_mail_field("Content-Disposition", disposition_field, status)
                call save_restriction_end(restriction)
                if (.not. status%ok()) return
            end if
        end if

        ! Per RFC 2045, C-T-E is case insensitive (bug#5070), but the
        ! others are not completely so.
        content_transfer_encoding = ""
        if (allocated(encoding_field)) content_transfer_encoding = downcase_ascii(encoding_field)

        if (allocated(type_field)) then
            call parse_rfc2231_quoted_printable_string(type_field, content_type, status)
            if (.not. status%ok()) return
        else if (present(default_content_type)) then
            content_type = default_content_type
        else
            content_type = plain_header("text/plain")
        end if

        ! A content type with no value cannot be matched.
        if (.not. allocated(content_type%name)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "Wrong type argument: expected character(len=*)")
            return
        end if

        if (allocated(disposition_field)) then
            call parse_rfc2231_quoted_printable_string(disposition_field, content_disposition, status)
            if (.not. status%ok()) return
        else if (present(default_content_disposition)) then
            ! If none specified, we are free to choose what we deem suitable
            ! according to RFC 2183.  We like inline.
            content_disposition = default_content_disposition
        else
            content_disposition = plain_header("inline")
        end if

        ! Unrecognized disposition types are to be treated like attachment
        ! according to RFC 2183.
        if (name_of(content_disposition) /= "inline" .and. name_of(content_disposition) /= "attachment") then
            content_disposition = plain_header("attachment")
        end if

        if (present(parse_tag)) then
            is_inline = name_of(content_disposition) == "inline"
            call make_entity(e)
            entities(e)%content_type = content_type
            entities(e)%content_disposition = content_disposition
            entities(e)%transfer_encoding = content_transfer_encoding
            call copy_marker_at(point_minimum(), entities(e)%header%beginning, .false., status)
            if (status%ok()) call copy_marker_at(end, entities(e)%header%end, .false., status)
            if (status%ok()) call copy_marker_at(end, entities(e)%body%beginning, .false., status)
            if (status%ok()) call copy_marker_at(point_maximum(), entities(e)%body%end, .false., status)
            if (.not. status%ok()) return
            entities(e)%tag = parse_tag
            entities(e)%tagline = .true.
            if (is_inline) entities(e)%body%display = RMAIL_MIME_DISPLAY_DECODED
            entities(e)%new = display_type(entities(e)%header%display, entities(e)%tagline, entities(e)%body%display)

            if (string_match("multipart/.*", name_of(content_type), status) > 0) then
                call save_restriction_begin(restriction)
                call narrow_to_region(end - 1, point_maximum(), status)

                if (status%ok()) then
                    ! Top level of message: don't show tagline.
                    if (len(parse_tag) == 0) then
                        entities(e)%new%tagline = .false.
                        entities(e)%tagline = .false.
                    end if

                    call process_rmail_mime_multipart(content_type, content_disposition, content_transfer_encoding, &
                                                      children, status, parse_tag)
                    if (status%ok()) entities(e)%children = children
                    entities(e)%handler => insert_rmail_mime_multipart
                end if

                call save_restriction_end(restriction)
            else if (string_match("message/rfc822", name_of(content_type), status) > 0) then
                call save_restriction_begin(restriction)
                call narrow_to_region(end, point_maximum(), status)
                if (status%ok()) call process_rmail_mime(.true., child, status, parse_tag, plain_header("text/plain"), &
                                                         plain_header("inline"))

                if (status%ok()) then
                    ! Show header of the child.
                    entities(child)%new%header = RMAIL_MIME_DISPLAY_DECODED
                    entities(child)%header%display = RMAIL_MIME_DISPLAY_DECODED
                    ! Hide tagline of the child.
                    entities(child)%new%tagline = .false.
                    entities(child)%tagline = .false.
                    entities(e)%children = [child]
                    entities(e)%handler => insert_rmail_mime_multipart
                end if

                call save_restriction_end(restriction)
            else if (is_inline .and. string_match("text/html", name_of(content_type), status) > 0) then
                ! Display tagline, so part can be detached.
                entities(e)%new%tagline = .true.
                entities(e)%tagline = .true.
                ! Display body also.
                entities(e)%new%body = RMAIL_MIME_DISPLAY_DECODED
                entities(e)%body%display = RMAIL_MIME_DISPLAY_DECODED
                entities(e)%handler => insert_rmail_mime_bulk
            else if (is_inline .and. string_match("text/", name_of(content_type), status) > 0) then
                ! Inline non-HTML text: don't need a tagline.
                entities(e)%new%tagline = .false.
                entities(e)%tagline = .false.
                entities(e)%handler => insert_rmail_mime_text
            else
                ! Force hidden mode.
                entities(e)%new%tagline = .true.
                entities(e)%tagline = .true.
                entities(e)%new%body = RMAIL_MIME_DISPLAY_HIDDEN
                entities(e)%body%display = RMAIL_MIME_DISPLAY_HIDDEN
                entities(e)%handler => insert_rmail_mime_bulk
            end if

            if (.not. status%ok()) return

            if (associated(entities(e)%handler, insert_rmail_mime_bulk)) then
                call set_rmail_mime_bulk_data(e, to_show)

                ! Show the body.
                if (to_show) then
                    entities(e)%new%body = RMAIL_MIME_DISPLAY_DECODED
                    entities(e)%body%display = RMAIL_MIME_DISPLAY_DECODED
                end if
            end if

            entity = e
            return
        end if

        ! Hide headers and handle the part.
        call make_entity(e)
        entities(e)%content_type = content_type
        entities(e)%content_disposition = content_disposition
        entities(e)%transfer_encoding = content_transfer_encoding
        entities(e)%current = display_type(RMAIL_MIME_DISPLAY_RAW, .false., RMAIL_MIME_DISPLAY_RAW)
        entities(e)%new = entities(e)%current
        entities(e)%header%display = RMAIL_MIME_DISPLAY_RAW
        entities(e)%body%display = RMAIL_MIME_DISPLAY_RAW
        call put_entity(point_minimum(), point_maximum(), e, status)
        if (.not. status%ok()) return
        call save_restriction_begin(restriction)

        if (name_of(content_type) == "message/rfc822") then
            call narrow_to_region(end, point_maximum(), status)
        else if (.not. show_headers) then
            call delete_region(point_minimum(), end, status)
        end if

        if (status%ok()) call handle_rmail_mime(content_type, content_disposition, content_transfer_encoding, status)
        call save_restriction_end(restriction)
    end subroutine process_rmail_mime

    !> Parse the current Rmail message as a MIME message.
    !> ENTITY is the MIME entity of the message.
    !> If an error occurs, ENTITY is 0 and ERROR_MESSAGE the error message.
    !> Port of rmailmm.el:rmail-mime-parse.
    recursive subroutine parse_rmail_mime(entity, error_message)
        integer, intent(out) :: entity
        character(len=:), allocatable, intent(out) :: error_message
        type(handle_type) :: saved_mbox, here
        type(excursion_type) :: excursion
        type(status_type) :: status, st
        integer :: k

        entity = 0
        saved_mbox = mime_mbox_buffer
        here = current_buffer()
        mime_mbox_buffer = here

        if (is_rmail_buffers_swapped()) then
            k = rmail_state_index(rmail_buffer())
            if (k > 0) mime_mbox_buffer = rmail_states(k)%view_buffer
        end if

        call set_buffer(mime_mbox_buffer, status)

        if (status%ok()) then
            call save_excursion_begin(excursion)
            call goto_character(point_minimum())
            call process_rmail_mime(.true., entity, status, "", plain_header("text/plain"), plain_header("inline"))

            if (status%ok()) then
                ! Show header.
                entities(entity)%new%header = RMAIL_MIME_DISPLAY_DECODED
                entities(entity)%header%display = RMAIL_MIME_DISPLAY_DECODED
            end if

            call save_excursion_end(excursion)
        end if

        call set_buffer(here, st)
        mime_mbox_buffer = saved_mbox

        if (.not. status%ok()) then
            entity = 0
            error_message = format_error(status)
        end if
    end subroutine parse_rmail_mime

    !> Insert a MIME-entity ENTITY in the current buffer.
    !>
    !> This function will be called recursively if multiple parts are
    !> available.  Port of rmailmm.el:rmail-mime-insert.
    recursive subroutine insert_rmail_mime(entity, status)
        integer, intent(in) :: entity
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, segment(5)
        type(display_type) :: current_display, new_display
        integer, allocatable :: children(:)
        integer :: k

        status = status_type()
        current_display = entities(entity)%current
        new_display = entities(entity)%new

        if (new_display%header /= RMAIL_MIME_DISPLAY_RAW) then
            ! Not a raw-mode.  Each handler should handle it.
            if (associated(entities(entity)%handler)) call entities(entity)%handler(entity, status)
        else
            beginning = point()
            call get_rmail_mime_entity_segment(point(), entity, segment)

            ! header
            if (current_display%header == new_display%header) then
                call goto_character(segment(3))
            else
                if (current_display%header /= RMAIL_MIME_DISPLAY_HIDDEN) call delete_forward(segment(3) - segment(2), status)
                if (status%ok()) call insert_buffer_substring(mime_mbox_buffer, status, &
                                                              position_of(entities(entity)%header%beginning), &
                                                              position_of(entities(entity)%header%end))
            end if

            ! tagline
            if (status%ok() .and. current_display%tagline) call delete_forward(segment(4) - segment(3), status)
            if (.not. status%ok()) return
            ! body
            children = entities(entity)%children

            if (size(children) > 0) then
                call put_entity(beginning, point(), entity, status)
                if (.not. status%ok()) return

                do k = 1, size(children)
                    call insert_rmail_mime(children(k), status)
                    if (.not. status%ok()) return
                end do
            else
                if (current_display%body == new_display%body) then
                    call goto_character(point() + segment(5) - segment(4))
                else
                    if (current_display%body /= RMAIL_MIME_DISPLAY_HIDDEN) then
                        call delete_forward(segment(5) - segment(4), status)
                    end if

                    if (.not. status%ok()) return

                    if (allocated(entities(entity)%body%data)) then
                        call insert(entities(entity)%body%data, status)
                    else
                        call insert_buffer_substring(mime_mbox_buffer, status, position_of(entities(entity)%body%beginning), &
                                                     position_of(entities(entity)%body%end))
                    end if

                    if (.not. status%ok()) return
                    if (.not. is_beginning_of_line()) call insert(LF, status)
                    if (.not. status%ok()) return
                end if

                call put_entity(beginning, point(), entity, status)
            end if
        end if

        if (.not. status%ok()) return
        entities(entity)%current = new_display
    end subroutine insert_rmail_mime

    !> Major mode used in `toggle_rmail_mime' buffers.  Port of
    !> rmailmm.el:rmail-mime-mode (its font lock with
    !> rmail-font-lock-keywords waits for Rmail's).
    recursive subroutine rmail_mime_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%rmail_mime_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%rmail_mime_mode%end(saved, status)
    end subroutine rmail_mime_mode

    !> Toggle the display of a MIME message.
    !>
    !> The actual behavior depends on the value of `rmail_enable_mime'.
    !>
    !> If `rmail_enable_mime' is true (the default), this command toggles
    !> the display of a MIME message between decoded presentation form and
    !> raw data.  The optional argument STATE forces a particular display
    !> state, rather than toggling: `RMAIL_MIME_DISPLAY_RAW' forces raw
    !> mode, `RMAIL_MIME_DISPLAY_DECODED' forces decoded mode.
    !>
    !> If `rmail_enable_mime' is false, this creates a temporary "*RMAIL*"
    !> buffer holding a decoded copy of the message.  Inline content-types
    !> are handled according to `rmail_mime_media_type_handlers'.
    !> By default, this displays text and multipart messages, and offers to
    !> download attachments as specified by
    !> `rmail_mime_attachment_directories'.
    !> The argument STATE has no effect in this case.  Port of
    !> rmailmm.el:rmail-mime (its prefix argument, which it ignores, is left
    !> out).
    recursive subroutine toggle_rmail_mime(status, state)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: state
        type(rmail_message_scope_type) :: scope
        type(handle_type) :: here, buffer, rmail, saved_mbox, saved_view
        type(value_type) :: hidden_value
        character(len=:), allocatable :: data
        logical :: hidden, desired_hidden, saved_enable, saved_read_only, changed
        integer(int64) :: message_number
        integer :: k
        type(status_type) :: st

        status = status_type()

        if (rmail_enable_mime) then
            here = current_buffer()
            rmail = rmail_buffer()
            call set_buffer(rmail, status)
            if (.not. status%ok()) return
            call get_text_property(point_minimum(), value_type(name=RMAIL_MIME_HIDDEN_PROPERTY), hidden_value, st)
            hidden = allocated(hidden_value%a)

            if (is_rmail_mime_message() .or. hidden) then
                if (present(state)) then
                    desired_hidden = state == RMAIL_MIME_DISPLAY_RAW
                else
                    desired_hidden = .not. hidden
                end if

                if (hidden .neqv. desired_hidden) then
                    k = rmail_state_index(rmail)
                    ! A copy: showing the message changes the state.
                    message_number = rmail_states(k)%current_message

                    if (.not. desired_hidden) then
                        call show_rmail_message(status, message_number)
                    else
                        saved_enable = rmail_enable_mime
                        rmail_enable_mime = .false.
                        call show_rmail_message(status, message_number)
                        rmail_enable_mime = saved_enable

                        if (status%ok()) then
                            saved_read_only = buffers(current)%read_only
                            buffers(current)%read_only = .false.
                            call add_text_properties(point_minimum(), point_maximum(), &
                                                     list(value_type(name=RMAIL_MIME_HIDDEN_PROPERTY), value_type(.true.)), &
                                                     changed, status)
                            buffers(current)%read_only = saved_read_only
                        end if
                    end if
                end if
            else
                call message("Not a MIME message, just toggling headers")
                call toggle_rmail_header(status)
            end if

            call set_buffer(here, st)
            return
        end if

        call apply_in_rmail_message_begin(scope, status)
        if (.not. status%ok()) return
        if (scope%active) data = buffer_string()
        call apply_in_rmail_message_end(scope)
        if (.not. allocated(data)) data = ""
        k = rmail_state_index(rmail_buffer())
        call get_or_make_buffer("*RMAIL*", buffer, status)
        if (.not. status%ok()) return
        saved_mbox = mime_mbox_buffer
        saved_view = mime_view_buffer
        if (k > 0) mime_mbox_buffer = rmail_states(k)%view_buffer
        mime_view_buffer = buffer
        call destroy_entities(buffer)
        owner = buffer
        call set_buffer(buffer, status)

        body: block
            if (.not. status%ok()) exit body
            buffers(current)%undo_enabled = .false.
            saved_read_only = buffers(current)%read_only
            buffers(current)%read_only = .false.
            ! Decoding the message in fundamental mode for speed, only
            ! switching to rmail_mime_mode at the end for display.  E.g.
            ! decode_quoted_printable_region gets very slow otherwise
            ! (Bug#4993).
            call fundamental_mode(status)
            if (status%ok()) call erase_buffer(status)
            if (status%ok()) call insert(data, status)
            if (status%ok()) call show_rmail_mime_buffer(status, .true.)
            if (status%ok()) call rmail_mime_mode(status)
            buffers(current)%read_only = saved_read_only
            if (.not. status%ok()) exit body
            call set_buffer_modified(.false.)
            call view_buffer(buffer, status)
        end block body

        mime_mbox_buffer = saved_mbox
        mime_view_buffer = saved_view
    end subroutine toggle_rmail_mime

    !> Report MESSAGE with more information on the main MIME components,
    !> CONTENT_TYPE, CONTENT_DISPOSITION and CONTENT_TRANSFER_ENCODING, as
    !> an error condition in STATUS.  Port of
    !> rmailmm.el:rmail-mm-get-boundary-error-message.
    subroutine set_rmail_mime_boundary_error(message_text, content_type, content_disposition, &
                                             content_transfer_encoding, status)
        character(len=*), intent(in) :: message_text
        type(mime_header_value_type), intent(in) :: content_type, content_disposition
        character(len=*), intent(in) :: content_transfer_encoding
        type(status_type), intent(inout) :: status

        call set_error(status, GENERIC_ERROR_CODE, message_text//"; type: "//header_text(content_type)// &
                       "; disposition: "//header_text(content_disposition)//"; encoding: "//content_transfer_encoding)
    end subroutine set_rmail_mime_boundary_error

    !> Function to use for the value of `rmail_show_mime_function'.
    !> Port of rmailmm.el:rmail-show-mime.
    recursive subroutine show_rmail_mime(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: error_message, coding_system
        type(handle_type) :: here, saved_mbox, saved_view, saved_owner
        type(mime_character_set_coding_type), allocatable :: saved_overrides(:)
        integer(int64) :: region(3), found, moved
        logical :: saved_read_only, had_overrides
        integer :: e, k
        type(status_type) :: st

        status = status_type()
        here = current_buffer()
        k = rmail_state_index(rmail_buffer())
        if (k == 0) return
        saved_mbox = mime_mbox_buffer
        saved_view = mime_view_buffer
        saved_owner = owner
        owner = rmail_buffer()
        call destroy_entities(owner)
        call parse_rmail_mime(e, error_message)
        mime_mbox_buffer = rmail_buffer()
        mime_view_buffer = rmail_states(k)%view_buffer
        mime_coding_system = ""
        mime_coding_system_from_header = .false.

        ! If ENTITY is 0, an error message describes the error.
        if (e > 0) then
            call set_buffer(mime_view_buffer, status)

            if (status%ok()) then
                call erase_buffer(status)
                ! This is for catching an error in the internal MIME
                ! decoding (e.g. incorrect BASE64 form) that may occur in
                ! insert_rmail_mime.
                if (status%ok()) call insert_rmail_mime(e, status)

                if (status%ok()) then
                    if (mime_coding_system_from_header) then
                        ! Decoding is done by decode_rfc2047_region only
                        ! for a header.  But, as the used coding system may
                        ! have been overridden by
                        ! mime_character_set_overrides, we can't trust it.
                        ! So, here we try the decoding again with
                        ! mime_character_set_overrides empty.
                        had_overrides = allocated(mime_character_set_overrides)
                        if (had_overrides) call move_alloc(mime_character_set_overrides, saved_overrides)
                        allocate (mime_character_set_overrides(0))
                        call find_rmail_mime_header_encoding(position_of(entities(e)%header%beginning), coding_system, status)
                        deallocate (mime_character_set_overrides)
                        if (had_overrides) call move_alloc(saved_overrides, mime_character_set_overrides)
                        mime_coding_system = coding_system
                    end if

                    if (status%ok()) then
                        if (len(mime_coding_system) > 0) then
                            call set_buffer_file_coding_system(coding_system_base(mime_coding_system), .true., .true., &
                                                               .false., status)
                        else
                            call set_buffer_file_coding_system("undecided", .true., .true., .false., status)
                        end if
                    end if
                end if

                if (.not. status%ok()) then
                    e = 0
                    error_message = format_error(status)
                    status = status_type()
                end if
            end if

            call set_buffer(here, st)
        end if

        ! Re-check ENTITY.  It may be set to 0 by an error.
        if (e == 0 .and. status%ok()) then
            ! Decoding failed.  ERROR_MESSAGE is an error message.  Insert
            ! the original message body as is, and show warning.
            call set_buffer(mime_mbox_buffer, status)

            if (status%ok()) then
                call goto_character(point_minimum())
                call regular_expression_search_forward("^$", found, st, noerror=SEARCH_NOERROR)
                moved = forward_line(1_int64)
                region = [point_minimum(), point(), point_maximum()]
                call set_buffer(mime_view_buffer, status)
            end if

            if (status%ok()) then
                saved_read_only = buffers(current)%read_only
                buffers(current)%read_only = .false.
                call erase_buffer(status)
                if (status%ok()) call insert_rmail_mime_header(region(1), status)
                if (status%ok()) call insert_buffer_substring(mime_mbox_buffer, status, region(2), region(3))
                buffers(current)%read_only = saved_read_only
            end if

            call set_buffer(here, st)
            if (status%ok()) call set_buffer_file_coding_system("no-conversion", .true., .true., .false., status)
            if (status%ok()) call message("MIME decoding failed: "//error_message)
        end if

        mime_mbox_buffer = saved_mbox
        mime_view_buffer = saved_view
        owner = saved_owner
    end subroutine show_rmail_mime

    !> Search message number MESSAGE_NUMBER of the current Rmail buffer for
    !> REGULAR_EXPRESSION: first its header, decoded, then its body,
    !> decoded when it is not plain text in US-ASCII.  FOUND is true if it
    !> matches.  The function of `rmail_search_mime_message_function'.
    !> Port of rmailmm.el:rmail-search-mime-message.
    recursive subroutine search_rmail_mime_message(message_number, regular_expression, found, status)
        integer(int64), intent(in) :: message_number
        character(len=*), intent(in) :: regular_expression
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(restriction_type) :: restriction
        type(excursion_type) :: excursion
        type(handle_type) :: old, temporary, saved_mbox, saved_view, saved_owner, search_owner
        character(len=:), allocatable :: error_message, character_set
        integer(int64) :: header_end, at
        logical :: saved_searching, decode_body
        integer :: e, k
        type(status_type) :: st

        status = status_type()
        found = .false.
        k = rmail_state_index(rmail_buffer())
        call save_restriction_begin(restriction)
        saved_mbox = mime_mbox_buffer
        saved_view = mime_view_buffer
        saved_owner = owner
        saved_searching = searching

        body: block
            call narrow_to_region(rmail_message_beginning(message_number), rmail_message_end(message_number), status)
            if (.not. status%ok()) exit body
            ! Mark inside search.
            searching = .true.
            mime_mbox_buffer = current_buffer()
            if (k > 0) mime_view_buffer = rmail_states(k)%view_buffer
            call save_excursion_begin(excursion)
            call regular_expression_search_forward("^$", at, st, noerror=SEARCH_MOVE)
            header_end = point()
            call save_excursion_end(excursion)
            ! The entities of the search belong to a buffer of their own.
            call get_or_make_buffer(generate_new_buffer_name(" *rmail-mime-search*"), search_owner, status)
            if (.not. status%ok()) exit body
            owner = search_owner
            call parse_rmail_mime(e, error_message)
            ! At first, just search the headers.
            call temporary_buffer_begin(old, temporary, .true., status)
            if (.not. status%ok()) exit body
            call insert_buffer_substring(mime_mbox_buffer, status, point_minimum_of(mime_mbox_buffer), header_end)
            if (status%ok()) call decode_rfc2047_region(point_minimum(), point(), status)
            call goto_character(point_minimum())
            if (status%ok()) call regular_expression_search_forward(regular_expression, at, status, noerror=SEARCH_NOERROR)
            found = at > 0
            call temporary_buffer_end(old, temporary)
            if (.not. status%ok() .or. found) exit body
            ! Next, search the body.
            decode_body = .false.

            if (e > 0) then
                if (string_match("text/.*", name_of(entities(e)%content_type), status) == 0) then
                    decode_body = .true.
                else
                    call get_rfc2231_value(entities(e)%content_type, "charset", character_set)
                    if (allocated(character_set)) decode_body = downcase_ascii(character_set) /= "us-ascii"
                end if
            end if

            if (decode_body) then
                ! Search the decoded MIME message.
                call temporary_buffer_begin(old, temporary, .true., status)
                if (.not. status%ok()) exit body
                call insert_rmail_mime(e, status)
                call goto_character(point_minimum())
                if (status%ok()) call regular_expression_search_forward(regular_expression, at, status, &
                                                                        noerror=SEARCH_NOERROR)
                found = at > 0
                call temporary_buffer_end(old, temporary)
            else
                ! Search the body without decoding.
                call goto_character(header_end)
                call regular_expression_search_forward(regular_expression, at, status, noerror=SEARCH_NOERROR)
                found = at > 0
            end if
        end block body

        if (is_buffer_live(search_owner)) then
            call destroy_entities(search_owner)
            call close_buffer(search_owner, st)
        end if

        searching = saved_searching
        mime_mbox_buffer = saved_mbox
        mime_view_buffer = saved_view
        owner = saved_owner
        call save_restriction_end(restriction)

    contains

        ! The beginning of the accessible portion of BUFFER.
        integer(int64) function point_minimum_of(buffer) result(position)
            type(handle_type), intent(in) :: buffer

            position = buffers(buffer%index)%beginning_of_accessible_portion
        end function point_minimum_of
    end subroutine search_rmail_mime_message

    recursive subroutine execute_toggle_rmail_mime(status)
        type(status_type), intent(out) :: status

        call toggle_rmail_mime(status)
    end subroutine execute_toggle_rmail_mime

    !> Set up Rmail's MIME support: the defaults of the user options, the
    !> button types, RMIME mode, the keys of Rmail mode and Rmail's MIME
    !> functions.  Port of rmailmm.el's top-level forms.
    subroutine initialize_rmail_mime()
        type(status_type) :: st
        character(len=:), allocatable :: lynx

        if (.not. allocated(rmail_mime_media_type_handlers)) then
            allocate (rmail_mime_media_type_handlers(4))
            rmail_mime_media_type_handlers(1)%regular_expression = "multipart/.*"
            rmail_mime_media_type_handlers(1)%handler => handle_rmail_mime_multipart
            rmail_mime_media_type_handlers(2)%regular_expression = "text/.*"
            rmail_mime_media_type_handlers(2)%handler => handle_rmail_mime_text
            rmail_mime_media_type_handlers(3)%regular_expression = "text/\(x-\)?patch"
            rmail_mime_media_type_handlers(3)%handler => handle_rmail_mime_bulk
            rmail_mime_media_type_handlers(4)%regular_expression = "\(image\|audio\|video\|application\)/.*"
            rmail_mime_media_type_handlers(4)%handler => handle_rmail_mime_bulk
        end if

        if (.not. allocated(rmail_mime_attachment_directories)) then
            allocate (rmail_mime_attachment_directories(5))
            rmail_mime_attachment_directories(1) = rmail_mime_attachment_directory_type("text/.*", "~/Documents")
            rmail_mime_attachment_directories(2) = rmail_mime_attachment_directory_type("image/.*", "~/Pictures")
            rmail_mime_attachment_directories(3) = rmail_mime_attachment_directory_type(".*", "~/Desktop")
            rmail_mime_attachment_directories(4) = rmail_mime_attachment_directory_type(".*", "~")
            rmail_mime_attachment_directories(5) = rmail_mime_attachment_directory_type(".*", temporary_file_directory())
        end if

        if (.not. associated(rmail_mime_render_html_function)) then
            call executable_find("lynx", lynx)
            if (len(lynx) > 0) rmail_mime_render_html_function => render_rmail_mime_html_lynx
        end if

        ! Default to preferring HTML parts, but only if we have a renderer.
        rmail_mime_prefer_html = associated(rmail_mime_render_html_function)
        call rmail_mime_save_button%define("rmail_mime_save") ! fortran-names: keep (the button type's name)
        rmail_mime_save_button%action => save_rmail_mime
        call rmail_mime_toggle_button%define("rmail_mime_toggle")
        rmail_mime_toggle_button%action => toggle_rmail_mime_button
        call major_modes%rmail_mime_mode%define("rmail_mime_mode", "RMIME", rmail_mime_mode, rmail_mime_mode_hook)
        commands%rmail_mime_mode%execute => rmail_mime_mode
        commands%toggle_rmail_mime%execute => execute_toggle_rmail_mime
        commands%toggle_rmail_mime_hidden%execute => toggle_rmail_mime_hidden
        call keymap_set(rmail_mode_map, "TAB", "forward_button", st)
        call keymap_set(rmail_mode_map, "<backtab>", "backward_button", st)
        call keymap_set(rmail_mode_map, "RET", "toggle_rmail_mime_hidden", st)
        rmail_show_mime_function => show_rmail_mime
        rmail_search_mime_message_function => search_rmail_mime_message
        if (.not. allocated(mime_coding_system)) mime_coding_system = ""
    end subroutine initialize_rmail_mime
end module rmail_mime_mod
