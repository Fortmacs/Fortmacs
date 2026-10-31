! buff_menu_fm.f90 -- interface for viewing and manipulating buffers.
! Ported from GNU Emacs 31.1 lisp/buff-menu.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> The Buffer Menu is used to view, edit, delete, or change attributes
!> of buffers.  The entry points are `C-x C-b' (`list_buffers') and
!> `M-x buffer_menu'.
!>
!> The Buffer Menu is a Tabulated List: an entry's ID is the serial
!> number of its buffer (upstream the buffer object itself), so a killed
!> buffer is never mistaken for a new one.
!>
!> Deferred: groups (`Buffer-menu-group-by', `Buffer-menu-group-sort-by',
!> `Buffer-menu-group-by-mode', `Buffer-menu-group-by-root',
!> `Buffer-menu-group-sort-alphabetically' and the Outline minor mode
!> commands on group headings: Tabulated List groups and Outline mode are
!> not ported), the fake header line for `buffer_menu_use_header_line'
!> false (Tabulated List's), `Buffer-menu-isearch-buffers' and
!> `Buffer-menu-isearch-buffers-regexp' (misearch.el), and
!> `Buffer-menu-visit-tags-table' (etags.el), the menu, the mouse command
!> `Buffer-menu-mouse-select', the face alias `Buffer-menu-buffer', the
!> warning of a buffer that fails to save in `buffer_menu_execute'
!> (warnings.el; the error is ignored, as upstream after warning), the
!> obsolete `Buffer-menu-sort', the :advertised-binding of
!> `buffer_menu_delete' and `buffer_menu_this_window'.
module buff_menu_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_QUIT
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nilp, fixnump, xfixnum
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes, minor_modes
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, buffer_live_p, buffer_handle, &
                         buffer_list, frame_buffer_list, buffer_serial_live, kill_buffer, buffer_modified_p, &
                         restore_buffer_modified_p
    use editfns_fm, only: point, goto_char, point_min, point_max, eobp, bobp, buffer_size, following_char
    use cmds_fm, only: forward_line, beginning_of_line
    use search_fm, only: re_search_forward
    use keyboard_fm, only: read_event
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use xdisp_fm, only: message, mode_line_process_text, cursor_in_echo_area
    use dispnew_fm, only: ding
    use help_fm, only: substitute_command_keys
    use files_fm, only: save_buffer, abbreviate_file_name, file_size_human_readable, buffer_stale_function, &
                        BUFFER_STALE_FAST, revert_buffer
    use shell_fm, only: list_buffers_directory
    use frame_fm, only: selected_frame, selected_window, frame_lines
    use window_fm, only: windows, window_text_cols, switch_to_buffer, switch_to_buffer_other_window, display_buffer, &
                         display_action_table_type, delete_other_windows, split_window, other_window, bury_buffer, other_buffer
    use view_fm, only: view_buffer, view_buffer_other_window
    use replace_fm, only: multi_occur, occur_read_primary_args
    use keymap_fm, only: keymap_set
    use tabulated_list_fm, only: tabulated_list_column_type, tabulated_list_desc_type, tabulated_list_entry_type, &
                                 set_tabulated_list_format, set_tabulated_list_entries, &
                                 set_tabulated_list_use_header_line, tabulated_list_get_id, tabulated_list_get_entry, &
                                 tabulated_list_set_col, tabulated_list_delete_entry, tabulated_list_print, &
                                 tabulated_list_print_entry, tabulated_list_init_header, tabulated_list_revert_hook
    use faces_fm, only: named_face_type, display_when, face_attrs
    implicit none(type, external)
    private

    public :: buffer_menu_mode, buffer_menu, buffer_menu_other_window, list_buffers, list_buffers_noselect, &
              buffer_menu_toggle_files_only, buffer_menu_toggle_internal, buffer_menu_buffer, buffer_menu_mark, &
              buffer_menu_unmark, buffer_menu_unmark_all_buffers, buffer_menu_unmark_all, buffer_menu_backup_unmark, &
              buffer_menu_delete, buffer_menu_delete_backwards, buffer_menu_save, buffer_menu_not_modified, &
              buffer_menu_execute, buffer_menu_select, buffer_menu_marked_buffers, buffer_menu_multi_occur, &
              buffer_menu_1_window, buffer_menu_this_window, buffer_menu_other_window_command, &
              buffer_menu_switch_other_window, buffer_menu_2_window, buffer_menu_toggle_read_only, buffer_menu_bury, &
              buffer_menu_view, buffer_menu_view_other_window, buffer_menu_dynamic_name_width, &
              tabulated_list_entry_size_greater, syms_of_buff_menu

    abstract interface
        !> A `buffer_menu_name_width_function': the width of the name column
        !> for the BUFFERS listed.  Not a port.
        integer function buffer_menu_name_width_proc(bufs)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: bufs(:)
        end function buffer_menu_name_width_proc

        !> A `Buffer-menu-filter-predicate': whether to list BUFFER.
        !> Not a port.
        logical function buffer_menu_filter_proc(buffer)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
        end function buffer_menu_filter_proc

        !> A `Buffer-menu-buffer-list' function: the buffers to list.
        !> Not a port.
        subroutine buffer_menu_buffer_list_proc(bufs)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), allocatable, intent(out) :: bufs(:)
        end subroutine buffer_menu_buffer_list_proc
    end interface

    public :: buffer_menu_name_width_proc, buffer_menu_filter_proc, buffer_menu_buffer_list_proc

    !> The mark character for marked buffers.  Port of buff-menu.el
    !> Buffer-menu-marker-char.
    character, public :: buffer_menu_marker_char = ">"

    !> Character used to flag buffers for deletion.  Port of buff-menu.el
    !> Buffer-menu-del-char.
    character, public :: buffer_menu_del_char = "D"

    !> If true, use the header line to display Buffer Menu column titles.
    !> Port of buff-menu.el Buffer-menu-use-header-line.
    logical, public :: buffer_menu_use_header_line = .true.

    !> Face for buffer names in the Buffer Menu.  Port of buff-menu.el
    !> buffer-menu-buffer (the face `buffer_menu_buffer').
    type(named_face_type), target, public :: buffer_menu_buffer_face

    !> Width of buffer name column in the Buffer Menu, used while
    !> `buffer_menu_name_width_function' is null.  Port of buff-menu.el
    !> Buffer-menu-name-width (a number).
    integer, public :: buffer_menu_name_width = 19

    !> Function returning the width of the buffer name column in the
    !> Buffer Menu: it is called with the list of buffers and should
    !> return a number.  When it is null, `buffer_menu_name_width' is used
    !> directly.  Port of buff-menu.el Buffer-menu-name-width (a function;
    !> the default `buffer_menu_dynamic_name_width').
    procedure(buffer_menu_name_width_proc), pointer, public :: buffer_menu_name_width_function => null()

    !> Width of buffer size column in the Buffer Menu.  Port of
    !> buff-menu.el Buffer-menu-size-width.
    integer, public :: buffer_menu_size_width = 7

    !> Width of mode name column in the Buffer Menu.  Port of buff-menu.el
    !> Buffer-menu-mode-width.
    integer, public :: buffer_menu_mode_width = 16

    !> If true, the Buffer Menu uses the selected frame's buffer list.
    !> Buffers that were never selected in that frame are listed at the end.
    !> If the value is false, the Buffer Menu uses the global buffer list.
    !> This variable matters if the Buffer Menu is sorted by visited order,
    !> as it is by default.  Port of buff-menu.el
    !> Buffer-menu-use-frame-buffer-list.
    logical, public :: buffer_menu_use_frame_buffer_list = .true.

    !> If true, show buffer sizes in human-readable format.
    !> That means to use `file_size_human_readable' (which see) to format the
    !> buffer sizes in the buffer size column.  Port of buff-menu.el
    !> Buffer-menu-human-readable-sizes.
    logical, public :: buffer_menu_human_readable_sizes = .false.

    !> Hook run when entering Buffer Menu mode.
    type(hook_type), target, public :: buffer_menu_mode_hook = hook_type(name="buffer_menu_mode_hook")

    !> Local keymap for `buffer_menu_mode' buffers.  Port of buff-menu.el
    !> Buffer-menu-mode-map.
    type(handle_type), public :: buffer_menu_mode_map

    ! The buffer-local variables of a Buffer Menu buffer:
    ! Buffer-menu-files-only (true if it lists only file buffers; set by
    ! the prefix argument to `buffer_menu' and related commands),
    ! Buffer-menu-show-internal (true if it lists internal buffers, those
    ! whose names start with a space), Buffer-menu-filter-predicate (a
    ! function to filter out buffers in the buffer list: buffers that
    ! don't satisfy it are skipped) and Buffer-menu-buffer-list (the list
    ! of buffers, with HAS_LIST, or a function returning them).
    type :: local_type
        integer(int64) :: serial = 0
        logical :: files_only = .false.
        logical :: show_internal = .false.
        procedure(buffer_menu_filter_proc), pointer, nopass :: filter_predicate => null()
        logical :: has_list = .false.
        type(handle_type), allocatable :: list(:)
        procedure(buffer_menu_buffer_list_proc), pointer, nopass :: list_function => null()
    end type local_type

    type(local_type), allocatable :: locals(:)

contains

    ! The index of the current buffer's entry in `locals', made if MAKE
    ! (0 if there is none).  Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(local_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) return
        end do

        k = 0
        if (.not. make) return

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) then
                locals(k) = local_type(serial=buffers(cur)%serial)
                return
            end if
        end do

        n = size(locals)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = locals(i)
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, locals)
        k = n + 1
    end function local_index

    ! The ID of buffer slot BI's entries: its serial number.  Not a port.
    function buffer_id(bi) result(id)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: id
        character(len=24) :: s

        write (s, "(i0)") buffers(bi)%serial
        id = trim(s)
    end function buffer_id

    ! The live buffer an entry ID names (the null handle for none).
    ! Not a port.
    function id_buffer(id) result(h)
        character(len=*), intent(in) :: id
        type(handle_type) :: h
        integer(int64) :: serial, k
        integer :: ios

        h = null_handle()
        if (len(id) == 0) return
        read (id, *, iostat=ios) serial
        if (ios /= 0) return

        do k = 1, size(buffers, kind=int64)
            if (buffers(k)%serial /= serial) cycle
            if (buffer_live_p(buffer_handle(k))) h = buffer_handle(k)
            return
        end do
    end function id_buffer

    ! Whether the entry ID names a buffer, live or killed (non-nil
    ! tabulated-list-get-id).  Not a port.
    logical function has_id(id)
        character(len=*), intent(in) :: id

        has_id = len(id) > 0
    end function has_id

    !> Return a name column width based on the current window width.
    !> The width will never exceed the actual width of the buffer names,
    !> but will never be narrower than 19 characters.  Port of
    !> buff-menu.el:Buffer-menu--dynamic-name-width.
    integer function buffer_menu_dynamic_name_width(bufs) result(width)
        type(handle_type), intent(in) :: bufs(:)
        integer :: k, longest, cols

        longest = 0

        do k = 1, size(bufs)
            if (buffer_live_p(bufs(k))) longest = max(longest, len(buffers(bufs(k)%index)%name))
        end do

        cols = 80

        if (selected_frame%selected_window%index /= 0) then
            cols = window_text_cols(windows(selected_frame%selected_window%index))
        end if

        ! This gives 19 on an 80 column window, and take up
        ! proportionally more space as the window widens.
        width = max(19, min(int(real(cols, real64)/4.2_real64), longest))
    end function buffer_menu_dynamic_name_width

    !> Major mode for Buffer Menu buffers.
    !> The Buffer Menu is invoked by the commands \[list_buffers],
    !> \[buffer_menu], and \[buffer_menu_other_window].
    !> See `buffer_menu' for a description of its contents.
    !>
    !> In Buffer Menu mode, the following commands are defined:
    !> \<buffer_menu_mode_map>
    !> \[quit_window]    Remove the Buffer Menu from the display.
    !> \[buffer_menu_this_window]    Select current line's buffer in place of the buffer menu.
    !> \[buffer_menu_other_window_command]    Select that buffer in another window,
    !>      so the Buffer Menu remains visible in its window.
    !> \[buffer_menu_view]    Select current line's buffer, in `view_mode'.
    !> \[buffer_menu_view_other_window]    Select that buffer in another window, in `view_mode'.
    !> \[buffer_menu_switch_other_window]  Make another window display that buffer.
    !> \[buffer_menu_mark]    Mark current line's buffer to be displayed.
    !> \[buffer_menu_select]    Select current line's buffer.
    !>      Also show buffers marked with "m", in other windows.
    !> \[buffer_menu_1_window]    Select that buffer in full-frame window.
    !> \[buffer_menu_2_window]    Select that buffer in one window, together with the
    !>      buffer selected before this one in another window.
    !> \[buffer_menu_multi_occur]    Show lines matching regexp in the marked buffers.
    !> \[buffer_menu_not_modified]    Clear modified-flag on that buffer.
    !> \[buffer_menu_save]    Mark that buffer to be saved, and move down.
    !> \[buffer_menu_delete]    Mark that buffer to be deleted, and move down.
    !> \[buffer_menu_delete_backwards]  Mark that buffer to be deleted, and move up.
    !> \[buffer_menu_execute]    Delete or save marked buffers.
    !> \[buffer_menu_unmark]    Remove all marks from current line.
    !>      With prefix argument, also move up one line.
    !> \[buffer_menu_unmark_all_buffers]    Remove a particular mark from all lines.
    !> \[buffer_menu_unmark_all]    Remove all marks from all lines.
    !> \[buffer_menu_backup_unmark]  Back up a line and remove marks.
    !> \[buffer_menu_toggle_read_only]    Toggle read-only status of buffer on this line.
    !> \[revert_buffer]    Update the list of buffers.
    !> \[buffer_menu_toggle_files_only]    Toggle whether the menu displays only file buffers.
    !> \[buffer_menu_bury]    Bury the buffer listed on this line.
    !>
    !> Port of buff-menu.el:Buffer-menu-mode.
    recursive subroutine buffer_menu_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%buffer_menu_mode%begin(saved, status)
        if (.not. status%ok()) return
        call buffer_stale_function%set_local(stale_fast)
        call tabulated_list_revert_hook%add(refresh_hook, local=.true.)
        call major_modes%buffer_menu_mode%finish(saved, status)
    end subroutine buffer_menu_mode

    ! Not a port.
    recursive integer function stale_fast(noconfirm) result(stale)
        logical, intent(in) :: noconfirm

        if (noconfirm) continue
        stale = BUFFER_STALE_FAST
    end function stale_fast

    ! Not a port.
    recursive subroutine refresh_hook(status)
        type(status_type), intent(out) :: status

        call list_buffers_refresh(status)
    end subroutine refresh_hook

    ! Port of buff-menu.el:buffer-menu--display-help.
    recursive subroutine display_help()
        call message(substitute_command_keys("Commands: " &
                                             //"\<buffer_menu_mode_map>" &
                                             //"\[buffer_menu_delete], " &
                                             //"\[buffer_menu_save], " &
                                             //"\[buffer_menu_execute], " &
                                             //"\[buffer_menu_unmark]; " &
                                             //"\[buffer_menu_this_window], " &
                                             //"\[buffer_menu_other_window_command], " &
                                             //"\[buffer_menu_1_window], " &
                                             //"\[buffer_menu_2_window], " &
                                             //"\[buffer_menu_mark], " &
                                             //"\[buffer_menu_select]; " &
                                             //"\[buffer_menu_not_modified], " &
                                             //"\[buffer_menu_toggle_read_only]; " &
                                             //"\[quit_window] to quit; \[describe_mode] for help"))
    end subroutine display_help

    ! FILES-ONLY from the raw prefix argument (nil if not ARG).  Not a port.
    logical function prefix_files_only() result(files_only)
        files_only = .not. nilp(current_prefix_arg)
        if (files_only) files_only = prefix_numeric_value(current_prefix_arg) >= 0
    end function prefix_files_only

    !> Switch to the Buffer Menu.
    !> By default, the Buffer Menu lists all buffers except those whose
    !> names start with a space (which are for internal use).  With
    !> prefix argument ARG, show only buffers that are visiting files.
    !>
    !> In the Buffer Menu, the first column (denoted "C") shows "."
    !> for the buffer from which you came, ">" for buffers you mark to
    !> be displayed, and "D" for those you mark for deletion.
    !>
    !> The "R" column has a "%" if the buffer is read-only.
    !> The "M" column has a "*" if it is modified, or "S" if you
    !> have marked it for saving.
    !>
    !> The remaining columns show the buffer name, the buffer size in
    !> characters, its major mode, and the visited file name (if any).
    !>
    !> See `buffer_menu_mode' for the keybindings available the Buffer
    !> Menu.
    !>
    !> The width of the various columns can be customized by changing
    !> the `buffer_menu_name_width', `buffer_menu_size_width' and
    !> `buffer_menu_mode_width' variables.  Port of
    !> buff-menu.el:buffer-menu.
    recursive subroutine buffer_menu(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        call list_buffers_noselect(buf, status, files_only=prefix_files_only())
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buf%index)%name, status)
        if (.not. status%ok()) return
        call display_help()
    end subroutine buffer_menu

    !> Display the Buffer Menu in another window.
    !> See `buffer_menu' for a description of the Buffer Menu.
    !>
    !> By default, all buffers are listed except those whose names start
    !> with a space (which are for internal use).  With prefix argument
    !> ARG, show only buffers that are visiting files.  Port of
    !> buff-menu.el:buffer-menu-other-window.
    recursive subroutine buffer_menu_other_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        call list_buffers_noselect(buf, status, files_only=prefix_files_only())
        if (.not. status%ok()) return
        call switch_to_buffer_other_window(buf, status)
        if (.not. status%ok()) return
        call display_help()
    end subroutine buffer_menu_other_window

    !> Display a list of existing buffers.
    !> The list is displayed in a buffer named "*Buffer List*".
    !> See `buffer_menu' for a description of the Buffer Menu.
    !>
    !> By default, all buffers are listed except those whose names start
    !> with a space (which are for internal use).  With prefix argument
    !> ARG, show only buffers that are visiting files.  Port of
    !> buff-menu.el:list-buffers.
    recursive subroutine list_buffers(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, window

        call list_buffers_noselect(buf, status, files_only=prefix_files_only())
        if (.not. status%ok()) return
        call display_buffer(buf, window, status)
    end subroutine list_buffers

    ! Port of buff-menu.el:Buffer-menu--selection-message.
    subroutine selection_message()
        integer :: k

        k = local_index(.true.)

        if (locals(k)%files_only) then
            call message("Showing only file-visiting buffers.")
        else if (locals(k)%show_internal) then
            call message("Showing all buffers.")
        else
            call message("Showing all buffers except internal ones.")
        end if
    end subroutine selection_message

    !> Toggle whether the current `buffer_menu' displays only file buffers.
    !> With a positive ARG, display only file buffers.  With zero or
    !> negative ARG, display other buffers as well.  Port of
    !> buff-menu.el:Buffer-menu-toggle-files-only.
    recursive subroutine buffer_menu_toggle_files_only(status)
        type(status_type), intent(out) :: status
        integer :: k

        k = local_index(.true.)

        if (nilp(current_prefix_arg)) then
            locals(k)%files_only = .not. locals(k)%files_only
        else
            locals(k)%files_only = prefix_numeric_value(current_prefix_arg) > 0
        end if

        call selection_message()
        call revert_buffer(status)
    end subroutine buffer_menu_toggle_files_only

    !> Toggle whether the current `buffer_menu' displays internal buffers.
    !> With a positive ARG, don't show internal buffers.  With zero or
    !> negative ARG, display internal buffers as well.  Port of
    !> buff-menu.el:Buffer-menu-toggle-internal.
    recursive subroutine buffer_menu_toggle_internal(status)
        type(status_type), intent(out) :: status
        integer :: k

        k = local_index(.true.)

        if (nilp(current_prefix_arg)) then
            locals(k)%show_internal = .not. locals(k)%show_internal
        else
            locals(k)%show_internal = prefix_numeric_value(current_prefix_arg) > 0
        end if

        call selection_message()
        call revert_buffer(status)
    end subroutine buffer_menu_toggle_internal

    !> Return the buffer described by the current Buffer Menu line.
    !> If there is no buffer here, return the null handle if
    !> ERROR_IF_NON_EXISTENT_P is false or omitted, and an error condition occurs in
    !> STATUS otherwise.  Port of buff-menu.el:Buffer-menu-buffer.
    function buffer_menu_buffer(status, error_if_non_existent_p) result(buffer)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: error_if_non_existent_p
        type(handle_type) :: buffer
        character(len=:), allocatable :: id
        logical :: signal

        status = status_type()
        signal = .false.
        if (present(error_if_non_existent_p)) signal = error_if_non_existent_p
        id = tabulated_list_get_id()
        buffer = id_buffer(id)

        if (.not. has_id(id)) then
            if (signal) call set_error(status, FM_GENERIC_ERROR, "No buffer on this line")
        else if (buffer%index == 0) then
            if (signal) call set_error(status, FM_GENERIC_ERROR, "This buffer has been killed")
        end if
    end function buffer_menu_buffer

    ! Port of buff-menu.el:Buffer-menu-beginning.
    subroutine buffer_menu_beginning()
        integer(int64) :: shortage

        call goto_char(point_min())
        if (.not. buffer_menu_use_header_line) shortage = forward_line()
    end subroutine buffer_menu_beginning

    ! (tabulated-list-set-col COL TEXT t).  Not a port.
    recursive subroutine set_col(col, text, status)
        integer, intent(in) :: col
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(tabulated_list_desc_type) :: desc

        desc%text = text
        desc%face = ""
        call tabulated_list_set_col(desc, status, col=col, change_entry_data=.true.)
    end subroutine set_col

    !> Mark the Buffer menu entry at point for later display.
    !> It will be displayed by the \<buffer_menu_mode_map>\[buffer_menu_select] command.
    !> Port of buff-menu.el:Buffer-menu-mark.
    recursive subroutine buffer_menu_mark(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        status = status_type()

        if (has_id(tabulated_list_get_id())) then
            call set_col(1, buffer_menu_marker_char, status)
            if (.not. status%ok()) return
        end if

        shortage = forward_line()
    end subroutine buffer_menu_mark

    !> Cancel all requested operations on buffer on this line and move
    !> down.  Optional BACKUP (the prefix argument) means move up.  Port of
    !> buff-menu.el:Buffer-menu-unmark.
    recursive subroutine buffer_menu_unmark(status, backup)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: backup
        integer(int64) :: shortage, n

        status = status_type()
        n = 1

        if (present(backup)) then
            if (backup) n = -1
        end if

        if (has_id(tabulated_list_get_id())) then
            call unmark(char(13), status)
            if (.not. status%ok()) return
        end if

        shortage = forward_line(n)
    end subroutine buffer_menu_unmark

    recursive subroutine cmd_buffer_menu_unmark(status)
        type(status_type), intent(out) :: status

        call buffer_menu_unmark(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_buffer_menu_unmark

    !> Cancel a requested operation on all buffers.
    !> MARK is the character to flag the operation on the buffers.
    !> When called interactively prompt for MARK;  RET remove all marks.
    !> Port of buff-menu.el:Buffer-menu-unmark-all-buffers.
    recursive subroutine buffer_menu_unmark_all_buffers(mark, status)
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        integer(int64) :: saved, shortage

        status = status_type()
        saved = point()
        call goto_char(point_min())

        do while (.not. eobp())
            call unmark(mark, status)
            if (.not. status%ok()) exit
            shortage = forward_line()
        end do

        call goto_char(saved)
    end subroutine buffer_menu_unmark_all_buffers

    recursive subroutine cmd_buffer_menu_unmark_all_buffers(status)
        type(status_type), intent(out) :: status
        integer(int64) :: c

        call read_char("Remove marks (RET means all):", c, status)
        if (.not. status%ok()) return

        if (c > 127) then
            ! (A mark is an ASCII character; others match none.)
            call buffer_menu_unmark_all_buffers(char(0), status)
        else
            call buffer_menu_unmark_all_buffers(achar(c), status)
        end if
    end subroutine cmd_buffer_menu_unmark_all_buffers

    !> Cancel all requested operations on buffers.  Port of
    !> buff-menu.el:Buffer-menu-unmark-all.
    recursive subroutine buffer_menu_unmark_all(status)
        type(status_type), intent(out) :: status

        call buffer_menu_unmark_all_buffers(char(13), status)
    end subroutine buffer_menu_unmark_all

    !> Move up and cancel all requested operations on buffer on line
    !> above.  Port of buff-menu.el:Buffer-menu-backup-unmark.
    recursive subroutine buffer_menu_backup_unmark(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        status = status_type()
        shortage = forward_line(-1_int64)

        do while (.not. has_id(tabulated_list_get_id()) .and. .not. bobp())
            shortage = forward_line(-1_int64)
        end do

        if (has_id(tabulated_list_get_id())) call unmark(char(13), status)
    end subroutine buffer_menu_backup_unmark

    ! Remove MARK in current entry.
    ! If MARK is RET remove all marks.  Port of
    ! buff-menu.el:Buffer-menu--unmark.
    recursive subroutine unmark(mark, status)
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        type(tabulated_list_entry_type) :: entry
        type(handle_type) :: buf
        type(status_type) :: st
        logical :: found, modified
        integer :: col
        integer, parameter :: cols(2) = [1, 3]
        integer :: j

        status = status_type()
        call tabulated_list_get_entry(entry, found)
        if (.not. found) return

        ! A mark could appear in column 0 or 2.
        do j = 1, 2
            col = cols(j)

            if (mark == char(13) .or. first_char_equal(mark, entry%cols(col)%text)) then
                call set_col(col, " ", status)
                if (.not. status%ok()) return
            end if
        end do

        ! Reset modified mark in column 2.
        buf = buffer_menu_buffer(st)

        if (buf%index /= 0) then
            call buffer_modified_p(buf, modified, st)
            if (modified .and. entry%cols(3)%text == " ") call set_col(3, "*", status)
        end if
    end subroutine unmark

    ! (char-equal MARK (string-to-char TEXT)), case folding as
    ! case-fold-search does.  Not a port.
    logical function first_char_equal(mark, text) result(equal)
        use casefiddle_fm, only: downcase_char
        character, intent(in) :: mark
        character(len=*), intent(in) :: text
        integer(int32) :: a, b

        a = 0
        if (len(text) > 0) a = iachar(text(1:1))
        b = iachar(mark)
        equal = downcase_char(a) == downcase_char(b)
    end function first_char_equal

    !> Mark the buffer on this Buffer Menu buffer line for deletion.
    !> A subsequent \<buffer_menu_mode_map>\[buffer_menu_execute] command
    !> will delete it.
    !>
    !> If prefix argument ARG is true, it specifies the number of
    !> buffers to delete; a negative ARG means to delete backwards.
    !> Port of buff-menu.el:Buffer-menu-delete.
    recursive subroutine buffer_menu_delete(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: n, shortage

        status = status_type()
        n = 1
        if (present(arg)) n = arg
        if (n == 0) n = 1

        do while (n > 0)
            buf = buffer_menu_buffer(st)

            if (buf%index /= 0) then
                call set_col(1, buffer_menu_del_char, status)
                if (.not. status%ok()) return
            end if

            shortage = forward_line(1_int64)
            n = n - 1
        end do

        do while (n < 0)
            buf = buffer_menu_buffer(st)

            if (buf%index /= 0) then
                call set_col(1, buffer_menu_del_char, status)
                if (.not. status%ok()) return
            end if

            shortage = forward_line(-1_int64)
            n = n + 1
        end do
    end subroutine buffer_menu_delete

    recursive subroutine cmd_buffer_menu_delete(status)
        type(status_type), intent(out) :: status

        call buffer_menu_delete(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_buffer_menu_delete

    !> Mark the buffer on this Buffer Menu line for deletion, and move up.
    !> A subsequent \<buffer_menu_mode_map>\[buffer_menu_execute] command
    !> will delete the marked buffer.  Prefix ARG
    !>  means move that many lines.  Port of
    !> buff-menu.el:Buffer-menu-delete-backwards.
    recursive subroutine buffer_menu_delete_backwards(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n

        n = 1
        if (present(arg)) n = arg
        call buffer_menu_delete(status, -n)
    end subroutine buffer_menu_delete_backwards

    recursive subroutine cmd_buffer_menu_delete_backwards(status)
        type(status_type), intent(out) :: status

        call buffer_menu_delete_backwards(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_buffer_menu_delete_backwards

    !> Mark the buffer on this Buffer Menu line for saving.
    !> A subsequent \<buffer_menu_mode_map>\[buffer_menu_execute]
    !> command will save it.  Port of buff-menu.el:Buffer-menu-save.
    recursive subroutine buffer_menu_save(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: shortage

        status = status_type()

        if (has_id(tabulated_list_get_id())) then
            buf = buffer_menu_buffer(st)

            if (buf%index /= 0) then
                call set_col(3, "S", status)
                if (.not. status%ok()) return
            end if
        end if

        shortage = forward_line()
    end subroutine buffer_menu_save

    !> Mark the buffer on this line as unmodified (no changes to save).
    !> If ARG is true (interactively, with a prefix argument), mark
    !> it as modified.  Port of buff-menu.el:Buffer-menu-not-modified.
    recursive subroutine buffer_menu_not_modified(status, arg)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        type(handle_type) :: buf, old
        type(status_type) :: st
        logical :: modified

        modified = .false.
        if (present(arg)) modified = arg
        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call restore_buffer_modified_p(modified)
        call set_buffer(old, st)

        if (modified) then
            call set_col(3, "*", status)
        else
            call set_col(3, " ", status)
        end if
    end subroutine buffer_menu_not_modified

    recursive subroutine cmd_buffer_menu_not_modified(status)
        type(status_type), intent(out) :: status

        call buffer_menu_not_modified(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_buffer_menu_not_modified

    !> Save and/or delete marked buffers in the Buffer Menu.
    !> Buffers marked with \<buffer_menu_mode_map>\[buffer_menu_save] are saved.
    !> Buffers marked with \<buffer_menu_mode_map>\[buffer_menu_delete] are deleted.
    !> Port of buff-menu.el:Buffer-menu-execute.
    recursive subroutine buffer_menu_execute(status)
        type(status_type), intent(out) :: status
        type(tabulated_list_entry_type) :: entry, deleted
        type(handle_type) :: buffer, menu
        type(status_type) :: st
        integer(int64) :: saved, shortage
        logical :: found, delete, killed

        status = status_type()
        menu = current_buffer()
        saved = point()
        call buffer_menu_beginning()

        do while (.not. eobp())
            buffer = id_buffer(tabulated_list_get_id())
            call tabulated_list_get_entry(entry, found)

            if (.not. found) then
                shortage = forward_line(1_int64)
            else if (buffer%index == 0) then
                call tabulated_list_delete_entry(deleted, found, status)
                if (.not. status%ok()) exit
            else
                delete = following_char() == iachar("D")

                if (entry%cols(3)%text == "S") then
                    call set_buffer(buffer, st)
                    call save_buffer(1_int64, .false., st)
                    call set_buffer(menu, status)
                    if (.not. status%ok()) exit

                    if (st%ok()) then
                        call set_col(3, " ", status)
                        if (.not. status%ok()) exit
                    end if
                end if

                killed = .false.

                if (delete .and. buffer%index /= menu%index) then
                    call kill_buffer(buffer, st)
                    killed = st%ok() .and. .not. buffer_live_p(buffer)
                    call set_buffer(menu, status)
                    if (.not. status%ok()) exit
                end if

                if (killed) then
                    call tabulated_list_delete_entry(deleted, found, status)
                    if (.not. status%ok()) exit
                else
                    shortage = forward_line(1_int64)
                end if
            end if
        end do

        call goto_char(min(saved, point_max()))
    end subroutine buffer_menu_execute

    !> Select this line's buffer; also, display buffers marked with `>'.
    !> You can mark buffers with the \<buffer_menu_mode_map>\[buffer_menu_mark] command.
    !>
    !> This command deletes and replaces all the previously existing windows
    !> in the selected frame, and will remove any marks.  Port of
    !> buff-menu.el:Buffer-menu-select.
    recursive subroutine buffer_menu_select(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: this_buffer, menu_buffer, new
        type(handle_type), allocatable :: marked(:), others(:)
        integer :: height, k, n

        this_buffer = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        menu_buffer = current_buffer()
        call buffer_menu_marked_buffers(marked, status, unmark=.true.)
        if (.not. status%ok()) return
        allocate (others(size(marked)))
        n = 0

        do k = 1, size(marked)
            if (marked(k)%index == this_buffer%index) cycle
            n = n + 1
            others(n) = marked(k)
        end do

        height = (frame_lines(selected_frame) - 1)/(1 + n)
        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(this_buffer%index)%name, status)
        if (.not. status%ok()) return

        if (menu_buffer%index /= this_buffer%index) then
            call bury_buffer(status, menu_buffer)
            if (.not. status%ok()) return
        end if

        do k = 1, n
            call split_window(selected_window(), height, .true., .false., new, status)
            if (.not. status%ok()) return
            call other_window(1_int64, status)
            if (.not. status%ok()) return
            call switch_to_buffer(buffers(others(k)%index)%name, status)
            if (.not. status%ok()) return
        end do

        ! Back to the beginning!
        call other_window(1_int64, status)
    end subroutine buffer_menu_select

    !> Return in BUFS the list of buffers marked with `buffer_menu_mark'.
    !> If UNMARK is true, unmark them.  Port of
    !> buff-menu.el:Buffer-menu-marked-buffers.
    recursive subroutine buffer_menu_marked_buffers(bufs, status, unmark)
        type(handle_type), allocatable, intent(out) :: bufs(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: unmark
        type(handle_type), allocatable :: grown(:)
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: saved, found
        logical :: clear

        clear = .false.
        if (present(unmark)) clear = unmark
        allocate (bufs(0))
        saved = point()
        call buffer_menu_beginning()

        do
            call re_search_forward("^>", found, status, noerror=1)
            if (.not. status%ok()) exit
            if (found == 0) exit
            buffer = buffer_menu_buffer(st)

            if (buffer%index /= 0 .and. clear) then
                call set_col(1, " ", status)
                if (.not. status%ok()) exit
            end if

            if (buffer%index /= 0) then
                allocate (grown(size(bufs) + 1))
                grown(1:size(bufs)) = bufs
                grown(size(grown)) = buffer
                call move_alloc(grown, bufs)
            end if
        end do

        call goto_char(saved)
    end subroutine buffer_menu_marked_buffers

    !> Show all lines in marked buffers containing a match for a regexp.
    !> Port of buff-menu.el:Buffer-menu-multi-occur.
    recursive subroutine buffer_menu_multi_occur(regexp, status, nlines)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: nlines
        type(handle_type), allocatable :: bufs(:)

        call buffer_menu_marked_buffers(bufs, status)
        if (.not. status%ok()) return
        call multi_occur(bufs, regexp, status, nlines)
    end subroutine buffer_menu_multi_occur

    recursive subroutine cmd_buffer_menu_multi_occur(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, collect
        logical :: has_nlines, has_collect
        integer(int64) :: nlines

        call occur_read_primary_args(regexp, has_nlines, nlines, has_collect, collect, status)
        if (.not. status%ok()) return

        if (has_nlines) then
            call buffer_menu_multi_occur(regexp, status, nlines)
        else
            call buffer_menu_multi_occur(regexp, status)
        end if
    end subroutine cmd_buffer_menu_multi_occur

    !> Select this line's buffer, alone, in full frame.  Port of
    !> buff-menu.el:Buffer-menu-1-window.
    recursive subroutine buffer_menu_1_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buf%index)%name, status)
        if (.not. status%ok()) return
        call bury_buffer(status, other_buffer(current_buffer()))
        if (.not. status%ok()) return
        call delete_other_windows(selected_window(), status)
    end subroutine buffer_menu_1_window

    !> Select this line's buffer in this window.  Port of
    !> buff-menu.el:Buffer-menu-this-window.
    recursive subroutine buffer_menu_this_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buf%index)%name, status)
    end subroutine buffer_menu_this_window

    !> Select this line's buffer in other window, leaving buffer menu
    !> visible.  Port of buff-menu.el:Buffer-menu-other-window (renamed: its
    !> name differs only in case from `buffer_menu_other_window').
    recursive subroutine buffer_menu_other_window_command(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        ! (display-buffer-overriding-action inhibit-same-window: as
        ! switch-to-buffer-other-window's.)
        call switch_to_buffer_other_window(buf, status)
    end subroutine buffer_menu_other_window_command

    !> Make the other window select this line's buffer.
    !> The current window remains selected.  Port of
    !> buff-menu.el:Buffer-menu-switch-other-window.
    recursive subroutine buffer_menu_switch_other_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, window

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        call display_buffer(buf, window, status, table=display_action_table_type(inhibit_same_window=.true.))
    end subroutine buffer_menu_switch_other_window

    !> Select this line's buffer, with previous buffer in second window.
    !> Port of buff-menu.el:Buffer-menu-2-window.
    recursive subroutine buffer_menu_2_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buff, menu, other

        buff = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        menu = current_buffer()
        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        other = other_buffer(current_buffer())
        call switch_to_buffer(buffers(other%index)%name, status)
        if (.not. status%ok()) return
        call switch_to_buffer_other_window(buff, status)
        if (.not. status%ok()) return
        call bury_buffer(status, menu)
    end subroutine buffer_menu_2_window

    !> Toggle read-only status of buffer on this line.
    !> This behaves like invoking \[read_only_mode] in that buffer.
    !> Port of buff-menu.el:Buffer-menu-toggle-read-only.
    recursive subroutine buffer_menu_toggle_read_only(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st
        logical :: read_only

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call minor_modes%read_only_mode%toggle(status)
        read_only = buffers(cur)%read_only
        call set_buffer(old, st)
        if (.not. status%ok()) return

        if (read_only) then
            call set_col(2, "%", status)
        else
            call set_col(2, " ", status)
        end if
    end subroutine buffer_menu_toggle_read_only

    !> Bury the buffer listed on this line.  Port of
    !> buff-menu.el:Buffer-menu-bury.
    recursive subroutine buffer_menu_bury(status)
        type(status_type), intent(out) :: status
        type(tabulated_list_entry_type) :: elt
        type(handle_type) :: buffer
        character(len=:), allocatable :: id
        integer(int64) :: saved
        logical :: found

        status = status_type()
        id = tabulated_list_get_id()
        buffer = id_buffer(id)

        if (.not. has_id(id)) then
            continue
        else if (buffer%index /= 0) then
            call bury_buffer(status, buffer)
            if (.not. status%ok()) return
            saved = point()
            call tabulated_list_delete_entry(elt, found, status)

            if (status%ok()) then
                call goto_char(point_max())
                call tabulated_list_print_entry(elt, status)
            end if

            call goto_char(min(saved, point_max()))
            if (.not. status%ok()) return
            call message("Buffer buried.")
        else
            call tabulated_list_delete_entry(elt, found, status)
            if (.not. status%ok()) return
            call message("Buffer is dead; removing from list.")
        end if
    end subroutine buffer_menu_bury

    !> View this line's buffer in View mode.  Port of
    !> buff-menu.el:Buffer-menu-view.
    recursive subroutine buffer_menu_view(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        call view_buffer(buf, status)
    end subroutine buffer_menu_view

    !> View this line's buffer in View mode in another window.  Port of
    !> buff-menu.el:Buffer-menu-view-other-window.
    recursive subroutine buffer_menu_view_other_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        buf = buffer_menu_buffer(status, .true.)
        if (.not. status%ok()) return
        call view_buffer_other_window(buf, status)
    end subroutine buffer_menu_view_other_window

    ! ------------------------------------------------------------------
    ! Functions for populating the Buffer Menu.
    ! ------------------------------------------------------------------

    !> Create and return in BUFFER a Buffer Menu buffer.
    !> This is called by `buffer_menu' and others as a subroutine.
    !>
    !> If FILES_ONLY is true, show only file-visiting buffers.
    !> If BUFFER_LIST is present, it is the list of buffers to list and no
    !> others; or BUFFER_LIST_FUNCTION returns them.
    !> If FILTER_PREDICATE is present, it should be a function
    !> that filters out buffers from the list of buffers.  Port of
    !> buff-menu.el:list-buffers-noselect.
    recursive subroutine list_buffers_noselect(buffer, status, files_only, buffer_list, buffer_list_function, &
                                               filter_predicate)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: files_only
        type(handle_type), intent(in), optional :: buffer_list(:)
        procedure(buffer_menu_buffer_list_proc), optional :: buffer_list_function
        procedure(buffer_menu_filter_proc), optional :: filter_predicate
        type(handle_type) :: old_buffer
        type(status_type) :: st
        integer :: k

        old_buffer = current_buffer()
        call get_buffer_create("*Buffer List*", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call buffer_menu_mode(status)

        if (status%ok()) then
            k = local_index(.true.)
            locals(k)%files_only = .false.
            if (present(files_only)) locals(k)%files_only = files_only
            locals(k)%has_list = present(buffer_list)
            if (allocated(locals(k)%list)) deallocate (locals(k)%list)
            if (present(buffer_list)) locals(k)%list = buffer_list
            locals(k)%list_function => null()
            if (present(buffer_list_function)) locals(k)%list_function => buffer_list_function
            locals(k)%filter_predicate => null()
            if (present(filter_predicate)) locals(k)%filter_predicate => filter_predicate
            call list_buffers_refresh(status, old_buffer)
            if (status%ok()) call tabulated_list_print(status)
        end if

        call set_buffer(old_buffer, st)
    end subroutine list_buffers_noselect

    ! Port of buff-menu.el:list-buffers--refresh: set up the current
    ! buffer's format and entries.  OLD_BUFFER is the buffer from which
    ! the menu was called (the null handle for none).
    recursive subroutine list_buffers_refresh(status, old_buffer)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: old_buffer
        type(handle_type), allocatable :: marked(:), candidates(:), listed(:)
        type(tabulated_list_entry_type), allocatable :: entries(:)
        type(tabulated_list_column_type) :: format(7)
        type(handle_type) :: menu, old
        procedure(buffer_menu_filter_proc), pointer :: filter
        character(len=:), allocatable :: name, file, size_text
        character(len=24) :: s
        logical :: show_non_file, show_internal, explicit, modified, live
        integer :: k, j, n, name_width
        integer(int64) :: bi
        type(status_type) :: st

        old = null_handle()
        if (present(old_buffer)) old = old_buffer
        call buffer_menu_marked_buffers(marked, status)
        if (.not. status%ok()) return
        menu = current_buffer()
        k = local_index(.true.)
        show_non_file = .not. locals(k)%files_only
        show_internal = locals(k)%show_internal
        filter => locals(k)%filter_predicate
        ! Collect info for each buffer we're interested in.
        explicit = .true.

        if (associated(locals(k)%list_function)) then
            call locals(k)%list_function(candidates)
        else if (locals(k)%has_list) then
            candidates = locals(k)%list
        else
            explicit = .false.

            if (buffer_menu_use_frame_buffer_list) then
                candidates = frame_buffer_list()
            else
                candidates = buffer_list()
            end if
        end if

        allocate (entries(size(candidates)), listed(size(candidates)))
        n = 0

        do j = 1, size(candidates)
            if (.not. buffer_live_p(candidates(j))) cycle
            bi = candidates(j)%index
            call set_buffer(candidates(j), st)
            name = buffers(bi)%name
            file = ""
            if (allocated(buffers(bi)%filename)) file = buffers(bi)%filename
            live = .true.

            if (.not. explicit) then
                live = .false.

                if (show_internal .or. len(file) > 0) then
                    live = .true.
                else if (len(name) > 0) then
                    live = name(1:1) /= " "
                else
                    live = .true.
                end if

                if (bi == menu%index) live = .false.
                if (.not. (len(file) > 0 .or. show_non_file)) live = .false.
                if (live .and. associated(filter)) live = filter(candidates(j))
            end if

            if (live) then
                n = n + 1
                listed(n) = candidates(j)

                associate (e => entries(n))
                    e%id = buffer_id(bi)
                    allocate (e%cols(7))

                    do k = 1, 7
                        e%cols(k)%face = ""
                    end do

                    if (bi == old%index) then
                        e%cols(1)%text = "."
                    else if (any_index(marked, bi)) then
                        e%cols(1)%text = ">"
                    else
                        e%cols(1)%text = " "
                    end if

                    e%cols(2)%text = " "
                    if (buffers(bi)%read_only) e%cols(2)%text = "%"
                    call buffer_modified_p(candidates(j), modified, st)
                    e%cols(3)%text = " "
                    if (modified) e%cols(3)%text = "*"
                    ! (Buffer-menu--pretty-name.)
                    e%cols(4)%text = name
                    e%cols(4)%face = "buffer_menu_buffer"

                    if (buffer_menu_human_readable_sizes) then
                        size_text = file_size_human_readable(real(buffer_size(), real64))
                    else
                        write (s, "(i0)") buffer_size()
                        size_text = trim(s)
                    end if

                    e%cols(5)%text = size_text
                    ! (format-mode-line mode-name: "Fundamental" until a mode sets it.)
                    e%cols(6)%text = "Fundamental"
                    if (allocated(buffers(bi)%mode_name)) e%cols(6)%text = buffers(bi)%mode_name
                    e%cols(6)%text = e%cols(6)%text//mode_line_process_text(bi)
                    e%cols(7)%text = pretty_file_name(file)
                end associate
            end if
        end do

        call set_buffer(menu, status)
        if (.not. status%ok()) return

        if (associated(buffer_menu_name_width_function)) then
            name_width = buffer_menu_name_width_function(listed(1:n))
        else
            name_width = buffer_menu_name_width
        end if

        format(1) = column("C", 1, 0)
        format(2) = column("R", 1, 0)
        format(3) = column("M", 1, 1)
        format(4) = column("Buffer", name_width, 1)
        format(5) = column("Size", buffer_menu_size_width, 1)
        format(5)%predicate => tabulated_list_entry_size_greater
        format(5)%right_align = .true.
        format(6) = column("Mode", buffer_menu_mode_width, 1)
        format(7) = column("File", 1, 1)
        call set_tabulated_list_format(format)
        call set_tabulated_list_use_header_line(buffer_menu_use_header_line)
        call set_tabulated_list_entries(entries(1:n))
        call tabulated_list_init_header()
    end subroutine list_buffers_refresh

    ! A sortable column NAME, WIDTH wide, PAD_RIGHT spaces after it.
    ! Not a port.
    function column(name, width, pad_right) result(c)
        character(len=*), intent(in) :: name
        integer, intent(in) :: width, pad_right
        type(tabulated_list_column_type) :: c

        c%name = name
        c%width = width
        c%sort = .true.
        c%pad_right = pad_right
    end function column

    ! Not a port.
    logical function any_index(bufs, bi) result(found)
        type(handle_type), intent(in) :: bufs(:)
        integer(int64), intent(in) :: bi
        integer :: k

        found = .false.

        do k = 1, size(bufs)
            if (bufs(k)%index == bi) found = .true.
        end do
    end function any_index

    !> Whether entry A's size is greater than entry B's: the sort
    !> predicate of the Size column.  Port of
    !> buff-menu.el:tabulated-list-entry-size->.
    logical function tabulated_list_entry_size_greater(a, b) result(greater)
        type(tabulated_list_entry_type), intent(in) :: a, b

        greater = string_to_number(a%cols(5)%text) > string_to_number(b%cols(5)%text)
    end function tabulated_list_entry_size_greater

    ! string-to-number: the number at the start of TEXT (after spaces and
    ! tabs; 0 if there is none).  Not a port.
    real(real64) function string_to_number(text) result(x)
        character(len=*), intent(in) :: text
        integer :: k, e, ios

        x = 0
        k = 1

        do while (k <= len(text))
            if (text(k:k) /= " " .and. text(k:k) /= char(9)) exit
            k = k + 1
        end do

        e = k

        if (e <= len(text)) then
            if (text(e:e) == "-" .or. text(e:e) == "+") e = e + 1
        end if

        do while (e <= len(text))
            if (index("0123456789.", text(e:e)) == 0) exit
            e = e + 1
        end do

        if (e > k) read (text(k:e - 1), *, iostat=ios) x
        if (e > k .and. ios /= 0) x = 0
    end function string_to_number

    ! Port of buff-menu.el:Buffer-menu--pretty-file-name, in the current
    ! buffer.
    function pretty_file_name(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text

        if (len(file) > 0) then
            text = abbreviate_file_name(file)
        else if (allocated(list_buffers_directory%value)) then
            if (len(list_buffers_directory%value) > 0) then
                text = abbreviate_file_name(list_buffers_directory%value)
            else
                text = ""
            end if
        else
            text = ""
        end if
    end function pretty_file_name

    ! Read a character in the echo area after PROMPT (read-char): C-g
    ! quits, other non-characters are an error.  Not a port.
    subroutine read_char(prompt, c, status)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: c
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have

        c = 0
        if (len(prompt) > 0) call message(prompt)
        cursor_in_echo_area = .true.
        call read_event(ev, have, status)
        cursor_in_echo_area = .false.
        if (.not. status%ok()) return

        if (.not. have) then
            call set_error(status, FM_QUIT)
        else if (.not. fixnump(ev)) then
            call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
        else if (xfixnum(ev) == 7) then
            call set_error(status, FM_QUIT)
        else if (xfixnum(ev) < 0 .or. xfixnum(ev) > int(z'3FFFFF', int64)) then
            call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
        else
            c = xfixnum(ev)
        end if
    end subroutine read_char

    !> Define Buffer Menu mode, its keymap and commands, and C-x C-b.
    !> Not a port.
    subroutine syms_of_buff_menu()
        type(status_type) :: st

        if (.not. associated(buffer_menu_name_width_function)) then
            buffer_menu_name_width_function => buffer_menu_dynamic_name_width
        end if

        call buffer_menu_buffer_face%define("buffer_menu_buffer", "Face for buffer names in the Buffer Menu.")
        call buffer_menu_buffer_face%spec_entry(display_when(), face_attrs(weight="bold"))
        call major_modes%buffer_menu_mode%define("buffer_menu_mode", "Buffer Menu", buffer_menu_mode, &
                                                 buffer_menu_mode_hook, parent=major_modes%tabulated_list_mode)
        buffer_menu_mode_map = major_modes%buffer_menu_mode%map
        commands%buffer_menu_mode%run => buffer_menu_mode
        commands%buffer_menu%run => buffer_menu
        commands%buffer_menu_other_window%run => buffer_menu_other_window
        commands%list_buffers%run => list_buffers
        commands%buffer_menu_toggle_files_only%run => buffer_menu_toggle_files_only
        commands%buffer_menu_toggle_internal%run => buffer_menu_toggle_internal
        commands%buffer_menu_mark%run => buffer_menu_mark
        commands%buffer_menu_unmark%run => cmd_buffer_menu_unmark
        commands%buffer_menu_unmark_all_buffers%run => cmd_buffer_menu_unmark_all_buffers
        commands%buffer_menu_unmark_all%run => buffer_menu_unmark_all
        commands%buffer_menu_backup_unmark%run => buffer_menu_backup_unmark
        commands%buffer_menu_delete%run => cmd_buffer_menu_delete
        commands%buffer_menu_delete_backwards%run => cmd_buffer_menu_delete_backwards
        commands%buffer_menu_save%run => buffer_menu_save
        commands%buffer_menu_not_modified%run => cmd_buffer_menu_not_modified
        commands%buffer_menu_execute%run => buffer_menu_execute
        commands%buffer_menu_select%run => buffer_menu_select
        commands%buffer_menu_multi_occur%run => cmd_buffer_menu_multi_occur
        commands%buffer_menu_1_window%run => buffer_menu_1_window
        commands%buffer_menu_this_window%run => buffer_menu_this_window
        commands%buffer_menu_other_window_command%run => buffer_menu_other_window_command
        commands%buffer_menu_switch_other_window%run => buffer_menu_switch_other_window
        commands%buffer_menu_2_window%run => buffer_menu_2_window
        commands%buffer_menu_toggle_read_only%run => buffer_menu_toggle_read_only
        commands%buffer_menu_bury%run => buffer_menu_bury
        commands%buffer_menu_view%run => buffer_menu_view
        commands%buffer_menu_view_other_window%run => buffer_menu_view_other_window
        call keymap_set(buffer_menu_mode_map, "v", "buffer_menu_select", st)
        call keymap_set(buffer_menu_mode_map, "2", "buffer_menu_2_window", st)
        call keymap_set(buffer_menu_mode_map, "1", "buffer_menu_1_window", st)
        call keymap_set(buffer_menu_mode_map, "f", "buffer_menu_this_window", st)
        call keymap_set(buffer_menu_mode_map, "e", "buffer_menu_this_window", st)
        call keymap_set(buffer_menu_mode_map, "C-m", "buffer_menu_this_window", st)
        call keymap_set(buffer_menu_mode_map, "o", "buffer_menu_other_window_command", st)
        call keymap_set(buffer_menu_mode_map, "C-o", "buffer_menu_switch_other_window", st)
        call keymap_set(buffer_menu_mode_map, "s", "buffer_menu_save", st)
        call keymap_set(buffer_menu_mode_map, "d", "buffer_menu_delete", st)
        call keymap_set(buffer_menu_mode_map, "k", "buffer_menu_delete", st)
        call keymap_set(buffer_menu_mode_map, "C-k", "buffer_menu_delete", st)
        call keymap_set(buffer_menu_mode_map, "C-d", "buffer_menu_delete_backwards", st)
        call keymap_set(buffer_menu_mode_map, "x", "buffer_menu_execute", st)
        call keymap_set(buffer_menu_mode_map, "SPC", "next_line", st)
        call keymap_set(buffer_menu_mode_map, "DEL", "buffer_menu_backup_unmark", st)
        call keymap_set(buffer_menu_mode_map, "~", "buffer_menu_not_modified", st)
        call keymap_set(buffer_menu_mode_map, "u", "buffer_menu_unmark", st)
        call keymap_set(buffer_menu_mode_map, "M-DEL", "buffer_menu_unmark_all_buffers", st)
        call keymap_set(buffer_menu_mode_map, "U", "buffer_menu_unmark_all", st)
        call keymap_set(buffer_menu_mode_map, "m", "buffer_menu_mark", st)
        call keymap_set(buffer_menu_mode_map, "%", "buffer_menu_toggle_read_only", st)
        call keymap_set(buffer_menu_mode_map, "b", "buffer_menu_bury", st)
        call keymap_set(buffer_menu_mode_map, "V", "buffer_menu_view", st)
        call keymap_set(buffer_menu_mode_map, "O", "buffer_menu_view_other_window", st)
        call keymap_set(buffer_menu_mode_map, "T", "buffer_menu_toggle_files_only", st)
        call keymap_set(buffer_menu_mode_map, "I", "buffer_menu_toggle_internal", st)
        call keymap_set(buffer_menu_mode_map, "M-s a C-o", "buffer_menu_multi_occur", st)
    end subroutine syms_of_buff_menu
end module buff_menu_fm
