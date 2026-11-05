! version_control_directory_mod.f90 -- directory status display under VC.
! Ported from GNU Emacs 31.1 lisp/vc/vc-dir.el.
!
! Copyright (C) 2007-2026 Free Software Foundation, Inc.
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

!> VC-Dir (C-x v d): the VC status of the "interesting" files in and
!> below a directory, as an ewoc of `vc_directory_fileinfo_type' entries the
!> backend's `directory_status_files' computes asynchronously.  The files can
!> be marked, and the VC commands act on the marked files (or the file at
!> point).
!>
!> Deferred: `vc_directory_search', `vc_directory_query_replace_regular_expression',
!> `vc_directory_isearch' and `vc_directory_isearch_regular_expression' (fileloop.el and
!> multi-isearch), the menu, the tool bar, the mouse maps,
!> `vc_directory_clean_files' (`map-y-or-n-p' over files), the keys of the
!> commands not ported yet (pull, push, incoming and outgoing logs,
!> branches, unintegrated changes), the outgoing count header
!> (`vc_directory_show_outgoing_count'), `vc_directory_close_query' (needing
!> `close_buffer_query_functions'), the backend's VC-Dir minor mode
!> (vc-dir-git-mode), the `display-state' of files being committed
!> asynchronously, desktop and bookmark support, project buffers and
!> directory-local variables.
module version_control_directory_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type
    use utility_functions_mod, only: is_string_less
    use data_mod, only: symbol_value, set, append_locals_clearer, choice_setting_type
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_serial_live, erase_buffer, &
                          get_or_make_buffer, generate_new_buffer_name, close_buffer, deactivate_mark_variable
    use commands_mod, only: commands
    use modes_mod, only: major_modes
    use derived_mod, only: is_derived_mode
    use editing_functions_mod, only: point, goto_character, point_minimum, is_end_of_line, region_beginning, region_end
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: beginning_of_line, forward_character
    use text_properties_mod, only: put_text_property
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use simple_mod, only: is_use_region, line_number_at_position
    use basic_subprograms_mod, only: read_y_or_n, string_matches
    use display_engine_mod, only: message, clear_mode_line_process
    use faces_mod, only: named_face_type, display_when, face_attributes
    use keymap_mod, only: keymap_set, make_sparse_keymap
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_name_as_directory, directory_file_name, &
                                     is_file_directory, file_exists
    use files_mod, only: abbreviate_file_name, make_file_buffer, find_file, find_file_other_window, &
                         find_file_noselect, revert_buffer_function, file_truename, is_file_in_directory, file_relative_name
    use process_mod, only: get_buffer_process, delete_process
    use fortmacs_widget_for_object_collections_mod, only: ewoc_type, ewoc_part_type, make_ewoc, ewoc_data, ewoc_set_data, &
                                                          ewoc_enter_last, ewoc_enter_before, ewoc_next, ewoc_previous, ewoc_nth, &
                                                          ewoc_map, ewoc_delete, ewoc_filter, ewoc_locate, ewoc_invalidate, &
                                                          ewoc_goto_previous, ewoc_goto_next, ewoc_goto_node, ewoc_collect, &
                                                          ewoc_set_hf_parts
    use version_control_hooks_mod, only: vc_backends, vc_directory_entry_type, vc_directory_fileinfo_type, vc_directory_buffers, &
                                         vc_directory_resynch_file, vc_state, vc_state_name, vc_backend_name, vc_file_set_backend, &
                                         VC_STATE_UNKNOWN, VC_UP_TO_DATE, VC_IGNORED, VC_UNREGISTERED, VC_EDITED, VC_ADDED, &
                                         VC_REMOVED, VC_MISSING, VC_CONFLICT, VC_NEEDS_UPDATE, VC_UNLOCKED_CHANGES, &
                                         VC_BACKEND_NONE
    use version_control_dispatcher_mod, only: vc_setup_buffer, set_vc_parent_buffer, vc_parent_buffer, &
                                              vc_set_mode_line_busy_indicator
    use version_control_dispatcher_mod, only: vc_directory_refresh_function
    use version_control_mod, only: vc_fileset_type, vc_responsible_backend, vc_root_directory, vc_only_files_state_and_model, &
                                   vc_ignore, vc_ignore_base_directory, vc_delete_file, vc_buffer_synchronize_fileset, &
                                   vc_next_action, set_vc_buffer_overriding_fileset
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: vc_directory, vc_directory_root, vc_directory_mode, vc_directory_update, vc_directory_refresh, &
              vc_directory_refresh_files, &
              vc_directory_next_line, vc_directory_previous_line, vc_directory_next_directory, vc_directory_previous_directory, &
              vc_directory_mark, vc_directory_mark_file, vc_directory_mark_all_files, vc_directory_mark_by_regular_expression, &
              vc_directory_mark_files, &
              vc_directory_mark_state_files, vc_directory_mark_registered_files, vc_directory_mark_unregistered_files, &
              vc_directory_unmark, &
              vc_directory_unmark_file, vc_directory_unmark_file_up, vc_directory_unmark_all_files, vc_directory_toggle_mark_file, &
              vc_directory_root_next_action, vc_directory_delete_file, vc_directory_find_file, &
              vc_directory_find_file_other_window, &
              vc_directory_display_file, vc_directory_view_file, vc_directory_ignore, vc_directory_current_file, &
              vc_directory_marked_files, &
              vc_directory_marked_only_files_and_states, vc_directory_child_files_and_states, vc_directory_resynch_file_impl, &
              vc_directory_show_fileentry, vc_directory_hide_state, vc_directory_remove_line, vc_directory_deduce_fileset, &
              vc_directory_backend, &
              vc_directory_busy, vc_directory_stop_directory_status_process, vc_directory_headers, vc_default_directory_printer, &
              vc_default_directory_extra_headers, vc_directory_ewoc, vc_directory_info, vc_directory_node_directory, &
              initialize_vc_directory, &
              insert_with_face

    !> Values of `vc_directory_allow_mass_mark_changes' besides .true. and .false.
    integer, parameter, public :: VC_DIRECTORY_ALLOW_MASS_MARK_CHANGES_ASK = 2

    !> Values of `vc_directory_auto_hide_up_to_date' besides .true. and .false.
    integer, parameter, public :: VC_DIRECTORY_AUTO_HIDE_UP_TO_DATE_REVERT = 2

    !> Normal hook run by `vc_directory_mode'.
    !> See `execute_mode_hooks'.  Port of vc-dir.el vc-dir-mode-hook.
    type(hook_type), target, public :: vc_directory_mode_hook = hook_type(name="vc_directory_mode_hook")

    !> Hook run after a refresh of the VC-Dir buffer completes (the hook
    !> `vc_directory_refresh' runs).
    type(hook_type), target, public :: vc_directory_refresh_hook = hook_type(name="vc_directory_refresh_hook")

    !> If true, VC-Dir commands may mark or unmark many items at once.
    !>
    !> When a directory in VC-Dir is marked, then for most VCS, this means that
    !> all files within it are implicitly marked as well.
    !> For consistency, the mark and unmark commands
    !> (principally \<vc_directory_mode_map>\[vc_directory_mark] and \[vc_directory_unmark]) will
    !> not explicitly mark or unmark entries if doing so would result in a
    !> situation where both a directory and a file or directory within it are
    !> both marked.
    !>
    !> With the default value of this variable, VC_DIRECTORY_ALLOW_MASS_MARK_CHANGES_ASK, if you
    !> attempt to mark or unmark a particular item and doing so consistent
    !> with these restrictions would require other items to be marked or
    !> unmarked too, Fortmacs will prompt you to confirm that you do mean
    !> for the other items to be marked or unmarked.
    !>
    !> If this variable is .false., the commands will refuse to
    !> do anything if they would need to mark or unmark other entries too.
    !> If this variable is .true., the commands will always
    !> proceed to mark and unmark other entries, without asking.  Port of
    !> vc-dir.el vc-dir-allow-mass-mark-changes.
    type(choice_setting_type), public :: vc_directory_allow_mass_mark_changes = &
                                         choice_setting_type(name="vc_directory_allow_mass_mark_changes", &
                                                             choice=VC_DIRECTORY_ALLOW_MASS_MARK_CHANGES_ASK)

    !> Whether VC-Dir auto-removes `up-to-date'/`ignored' files from display.
    !>
    !> If the value is .false., files shown in the VC-Dir buffer
    !> will remain on display if they become `up-to-date' or `ignored'.
    !> If the value is .true., files are automatically removed
    !> from display when they become `up-to-date' or `ignored'.
    !> If the value is VC_DIRECTORY_AUTO_HIDE_UP_TO_DATE_REVERT, any displayed files that
    !> are `up-to-date' or `ignored' are removed from display
    !> by \<vc_directory_mode_map>\[revert_buffer], but they are not automatically removed
    !> when they become `up-to-date' or `ignored'.  That is,
    !> refreshing the VC-Dir buffer hides `up-to-date' and `ignored'
    !> files when the value is VC_DIRECTORY_AUTO_HIDE_UP_TO_DATE_REVERT.
    !>
    !> VC-Dir never shows `up-to-date' and `ignored' files when the
    !> directory is first displayed.
    !>
    !> You can still use `vc_directory_show_fileentry' to manually add an entry for
    !> an `up-to-date' or `ignored' file.  Port of vc-dir.el
    !> vc-dir-auto-hide-up-to-date.
    type(choice_setting_type), public :: vc_directory_auto_hide_up_to_date = &
                                         choice_setting_type(name="vc_directory_auto_hide_up_to_date")

    !> If true, first offer to save relevant buffers when refreshing VC-Dir.
    !> Port of vc-dir.el vc-dir-save-some-buffers-on-revert.
    logical, public :: vc_directory_save_some_buffers_on_revert = .false.

    !> The keymap of `vc_directory_mode'.
    type(handle_type), public :: vc_directory_mode_map

    !> The faces of `vc_directory_mode' (vc-dir.el's vc-dir-header and the rest).
    type(named_face_type), target, public :: vc_directory_header, vc_directory_header_value, vc_directory_header_urgent_value, &
                                             vc_directory_directory, vc_directory_file, vc_directory_mark_indicator, &
                                             vc_directory_status_warning, vc_directory_status_edited, &
                                             vc_directory_status_up_to_date, &
                                             vc_directory_status_ignored

    ! The file information of the ewocs, by the data of their nodes.
    type(vc_directory_fileinfo_type), allocatable :: infos(:)
    logical, allocatable :: info_live(:)

    ! A VC-Dir buffer's `vc_ewoc', `vc_directory_process_buffer' and
    ! `vc_directory_backend', by serial number.
    type :: local_type
        integer(int64) :: serial = 0
        logical :: has_ewoc = .false.
        type(ewoc_type) :: ewoc
        type(handle_type) :: process_buffer
        integer :: backend = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! The backend `vc_directory_mode' gives the buffer (use-vc-backend,
    ! saved and restored by `vc_directory').
    integer :: use_vc_backend = 0

    ! `vc_directory_allow_mass_mark_changes' as bound by `vc_directory_mark_unmark'.
    logical :: mass_mark_bound_false = .false.

    ! The arguments of the `ewoc_map' and `ewoc_collect' tests
    ! (closures upstream).
    integer :: map_state = 0
    logical :: map_unmark = .false.
    character(len=:), allocatable :: map_regular_expression
    type(string_type), allocatable :: map_files(:)
    integer, allocatable :: map_states(:)

    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values and the file information
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
    subroutine clear_vc_directory_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = local_type()
    end subroutine clear_vc_directory_locals

    !> The current buffer's `vc_ewoc' (HAS .false. for none).  Not a port.
    subroutine vc_directory_ewoc(ewoc, has)
        type(ewoc_type), intent(out) :: ewoc
        logical, intent(out) :: has
        integer :: k

        has = .false.
        k = local_index()

        if (k > 0) then
            has = locals(k)%has_ewoc
            ewoc = locals(k)%ewoc
        end if
    end subroutine vc_directory_ewoc

    ! The current buffer's ewoc (a stale handle if none).  Not a port.
    function ew() result(ewoc)
        type(ewoc_type) :: ewoc
        logical :: has

        call vc_directory_ewoc(ewoc, has)
    end function ew

    !> The current buffer's `vc_directory_backend': the backend used by the
    !> current *vc-dir* buffer (0 for none).  Not a port.
    integer function vc_directory_backend()
        integer :: k

        vc_directory_backend = 0
        k = local_index()
        if (k > 0) vc_directory_backend = locals(k)%backend
    end function vc_directory_backend

    ! The current buffer's `vc_directory_process_buffer'.  Not a port.
    function process_buffer() result(h)
        type(handle_type) :: h
        integer :: k

        h = null_handle()
        k = local_index()
        if (k > 0) h = locals(k)%process_buffer
    end function process_buffer

    ! A new file information (vc-dir-create-fileinfo) from ENTRY, for a
    ! directory DIRECTORY if given: its data for the ewoc.  Not a port.
    integer(int64) function make_info(entry, directory) result(id)
        type(vc_directory_entry_type), intent(in) :: entry
        character(len=*), intent(in), optional :: directory
        type(vc_directory_fileinfo_type), allocatable :: grown(:)
        logical, allocatable :: grown_live(:)
        integer :: k, n

        if (.not. allocated(infos)) then
            allocate (infos(16))
            allocate (info_live(16))
            info_live = .false.
        end if

        id = 0

        do k = 1, size(infos)
            if (.not. info_live(k)) then
                id = k
                exit
            end if
        end do

        if (id == 0) then
            n = size(infos)
            allocate (grown(2*n))
            allocate (grown_live(2*n))
            grown_live = .false.

            do k = 1, n
                grown(k) = infos(k)
            end do

            grown_live(1:n) = info_live
            call move_alloc(grown, infos)
            call move_alloc(grown_live, info_live)
            id = n + 1
        end if

        infos(id) = vc_directory_fileinfo_type()
        infos(id)%entry = entry
        if (present(directory)) infos(id)%directory = directory
        info_live(id) = .true.
    end function make_info

    ! A directory entry for NAME (vc-dir-create-fileinfo NAME nil nil nil
    ! DIRECTORY).  Not a port.
    integer(int64) function create_directory_info(name, directory) result(id)
        character(len=*), intent(in) :: name, directory
        type(vc_directory_entry_type) :: entry

        entry%name = name
        id = make_info(entry, directory)
    end function create_directory_info

    !> The file information of the ewoc's node NODE in the current buffer.
    !> Not a port.
    function vc_directory_info(node) result(info)
        integer, intent(in) :: node
        type(vc_directory_fileinfo_type) :: info
        integer(int64) :: id

        id = ewoc_data(ew(), node)
        if (id > 0) info = infos(id)
    end function vc_directory_info

    ! The data of NODE.  Not a port.
    integer(int64) function did(node)
        integer, intent(in) :: node

        did = ewoc_data(ew(), node)
    end function did

    ! Delete NODES from the ewoc and forget their information.  Not a port.
    subroutine delete_nodes(nodes)
        integer, intent(in) :: nodes(:)
        integer(int64) :: id
        integer :: k

        do k = 1, size(nodes)
            id = did(nodes(k))
            if (id > 0) info_live(id) = .false.
        end do

        call ewoc_delete(ew(), nodes)
    end subroutine delete_nodes

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> Insert TEXT with the face FACE (propertize).  Not a port.
    subroutine insert_with_face(text, face)
        character(len=*), intent(in) :: text, face
        integer(int64) :: beginning
        type(status_type) :: st

        beginning = point()
        call insert(text, st)
        if (len(face) > 0 .and. point() > beginning) call put_text_property(beginning, point(), value_type(name="face"), &
                                                                            value_type(name=face), st)
    end subroutine insert_with_face

    ! (expand-file-name NAME): relative to the current buffer's directory.
    ! Not a port.
    function expand(name) result(full)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: full
        type(status_type) :: st

        call expand_file_name(name, full, st, buffers(current)%directory)
    end function expand

    ! (file-name-directory (directory-file-name (expand-file-name NAME))).
    ! Not a port.
    function entry_directory(name) result(directory)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: directory

        directory = file_name_directory(directory_file_name(expand(name)))
    end function entry_directory

    ! (let ((inhibit-read-only t)) ...).  Not a port.
    subroutine bind_inhibit_read_only(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = value_type()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
    end subroutine bind_inhibit_read_only

    ! Not a port.
    subroutine restore_inhibit_read_only(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine restore_inhibit_read_only

    !> Whether `vc_directory_allow_mass_mark_changes' allows mass mark changes,
    !> asking or not (it is off while bound by `vc_directory_mark_unmark').
    !> Not a port.
    logical function is_mass_mark_allowed()
        is_mass_mark_allowed = .not. (mass_mark_bound_false .or. vc_directory_allow_mass_mark_changes%is_off())
    end function is_mass_mark_allowed

    !> Whether `vc_directory_allow_mass_mark_changes' says to ask first.
    !> Not a port.
    logical function is_mass_mark_asked()
        is_mass_mark_asked = .not. mass_mark_bound_false .and. &
                             vc_directory_allow_mass_mark_changes%choice == VC_DIRECTORY_ALLOW_MASS_MARK_CHANGES_ASK
    end function is_mass_mark_asked

    ! ------------------------------------------------------------------
    ! The buffer
    ! ------------------------------------------------------------------

    ! Used to keep the cursor on the file name column.  Port of
    ! vc-dir.el:vc-dir-move-to-goal-column.
    subroutine vc_directory_move_to_goal_column()
        type(status_type) :: st
        integer(int64) :: k

        call beginning_of_line()

        if (.not. is_end_of_line()) then
            ! Must be in sync with vc-default-dir-printer.
            do k = 1, 25
                call forward_character(st, 1_int64)
                if (.not. st%ok()) exit
            end do
        end if
    end subroutine vc_directory_move_to_goal_column

    ! Find a buffer named BNAME showing DIRECTORY, or create a new one.  Port of
    ! vc-dir.el:vc-dir-prepare-status-buffer.
    recursive subroutine vc_directory_prepare_status_buffer(bname, directory_in, backend, buffer, status, create_new)
        character(len=*), intent(in) :: bname, directory_in
        integer, intent(in) :: backend
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: create_new
        character(len=:), allocatable :: directory, full
        type(handle_type) :: old
        type(status_type) :: st
        logical :: new
        integer :: k

        status = status_type()
        new = .false.
        if (present(create_new)) new = create_new
        call expand_file_name(directory_in, full, status, buffers(current)%directory)
        if (.not. status%ok()) return
        directory = file_name_as_directory(full)
        buffer = null_handle()
        old = current_buffer()

        ! Look for another buffer name BNAME visiting the same directory.
        if (.not. new .and. allocated(vc_directory_buffers)) then
            do k = 1, size(vc_directory_buffers)
                if (.not. is_buffer_live(vc_directory_buffers(k))) cycle
                call set_buffer(vc_directory_buffers(k), st)

                if (is_derived_mode("vc_directory_mode") .and. vc_directory_backend() == backend .and. &
                    buffers(current)%directory == directory) then
                    buffer = vc_directory_buffers(k)
                    exit
                end if
            end do
        end if

        call set_buffer(old, st)
        if (buffer%index /= 0) return
        ! Create a new buffer named BNAME.
        ! We pass a filename to `create-file-buffer' because it is what
        ! the function expects, and also what uniquify needs (if active)
        call expand_file_name(bname, full, status, directory)
        if (.not. status%ok()) return
        call make_file_buffer(full, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, st)
        buffers(current)%directory = directory
        call vc_setup_buffer(buffer, status)
        ! Reset the vc-parent-buffer-name so that it does not appear
        ! in the mode-line.
        call set_vc_parent_buffer(vc_parent_buffer(), "")
        call set_buffer(old, st)
    end subroutine vc_directory_prepare_status_buffer

    !> Compute the directory for NODE.  Port of
    !> vc-dir.el:vc-dir-node-directory.
    function vc_directory_node_directory(node) result(directory)
        integer, intent(in) :: node
        character(len=:), allocatable :: directory
        integer(int64) :: id

        id = did(node)
        directory = ""
        if (id == 0) return

        ! If it's a directory node, get it from the node.
        if (allocated(infos(id)%directory)) then
            directory = infos(id)%directory
        else
            ! Otherwise compute it from the file name.
            directory = entry_directory(infos(id)%entry%name)
        end if
    end function vc_directory_node_directory

    ! Whether STATE is one of `vc_directory__up_to_date_states'.  Not a port.
    logical function up_to_date_state(state)
        integer, intent(in) :: state

        up_to_date_state = state == VC_UP_TO_DATE .or. state == VC_IGNORED
    end function up_to_date_state

    !> Update BUFFER's VC-Dir ewoc from ENTRIES.
    !> This has the effect of adding ENTRIES to the VC-Dir buffer BUFFER.
    !> If optional argument NOINSERT is true, update ewoc nodes, but don't
    !> add elements of ENTRIES to the buffer that aren't already in the ewoc.
    !> Also update some VC file properties from ENTRIES.  Port of
    !> vc-dir.el:vc-dir-update.
    recursive subroutine vc_directory_update(entries_in, buffer, noinsert)
        type(vc_directory_entry_type), intent(in) :: entries_in(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in), optional :: noinsert
        type(vc_directory_entry_type), allocatable :: entries(:)
        type(string_type), allocatable :: directories(:)
        integer, allocatable :: order(:), to_remove(:)
        character(len=:), allocatable :: dotname, entryfile, entrydir, nodedir, nodefile, rd, previous_directory, lastdir
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        type(ewoc_type) :: e
        integer(int64) :: id
        logical :: noins, has
        integer :: n, k, j, i, node, previous_node, nremove, crt, first, previous, next, b

        noins = .false.
        if (present(noinsert)) noins = noinsert
        old = current_buffer()
        call set_buffer(buffer, st)
        call vc_directory_ewoc(e, has)

        if (.not. has) then
            call set_buffer(old, st)
            return
        end if

        ! Insert the entries sorted by name into the ewoc.
        ! We assume the ewoc is sorted too, which should be the
        ! case if we always add entries with vc-dir-update.
        n = size(entries_in)
        allocate (directories(n), order(n))

        do k = 1, n
            directories(k)%text = entry_directory(entries_in(k)%name)
            order(k) = k
        end do

        ! Sort: first files and then subdirectories.
        do k = 2, n
            j = order(k)
            i = k - 1

            do while (i >= 1)
                if (.not. before(j, order(i))) exit
                order(i + 1) = order(i)
                i = i - 1
            end do

            order(i + 1) = j
        end do

        allocate (entries(n))

        do k = 1, n
            entries(k) = entries_in(order(k))
        end do

        ! Insert directory entries in the right places.
        k = 1
        node = ewoc_nth(e, 0)
        allocate (to_remove(0))
        dotname = file_relative_name(buffers(current)%directory, buffers(current)%directory)

        ! Insert . if it is not present.
        if (node == 0) then
            node = ewoc_enter_last(e, create_directory_info(dotname, buffers(current)%directory))
            node = ewoc_nth(e, 0)
        end if

        do while (k <= n .and. node /= 0)
            entryfile = entries(k)%name
            entrydir = entry_directory(entryfile)
            nodedir = vc_directory_node_directory(node)

            if (is_string_less(nodedir, entrydir)) then
                ! First try to find the directory.
                node = ewoc_next(e, node)
            else if (nodedir == entrydir) then
                ! Found the directory, find the place for the file name.
                nodefile = infos(did(node))%entry%name

                if (nodefile == dotname) then
                    node = ewoc_next(e, node)
                else if (is_string_less(nodefile, entryfile)) then
                    node = ewoc_next(e, node)
                else if (nodefile == entryfile) then
                    id = did(node)

                    if (entries(k)%state == VC_STATE_UNKNOWN .or. &
                        (.not. vc_directory_auto_hide_up_to_date%is_off() .and. &
                         vc_directory_auto_hide_up_to_date%choice /= VC_DIRECTORY_AUTO_HIDE_UP_TO_DATE_REVERT .and. &
                         up_to_date_state(entries(k)%state))) then
                        to_remove = [node, to_remove]
                    else
                        infos(id)%entry%state = entries(k)%state
                        ! (vc--file-getinheprop nodefile 'display-state): only
                        ! asynchronous checkins set it (deferred).
                        if (allocated(infos(id)%display_state)) deallocate (infos(id)%display_state)
                        infos(id)%entry%has_extra = entries(k)%has_extra
                        infos(id)%entry%old_permission = entries(k)%old_permission
                        infos(id)%entry%new_permission = entries(k)%new_permission
                        infos(id)%entry%rename_state = entries(k)%rename_state
                        if (allocated(entries(k)%original_name)) infos(id)%entry%original_name = entries(k)%original_name
                        infos(id)%needs_update = .false.
                        call ewoc_invalidate(e, [node])
                    end if

                    k = k + 1
                    node = ewoc_next(e, node)
                else
                    if (.not. noins) node = enter_before_keep(node, make_info(entries(k)))
                    k = k + 1
                end if
            else
                if (.not. noins) then
                    ! We might need to insert a directory node if the
                    ! previous node was in a different directory.
                    rd = file_relative_name(entrydir, buffers(current)%directory)
                    previous_node = ewoc_previous(e, node)
                    previous_directory = ""
                    if (previous_node /= 0) previous_directory = vc_directory_node_directory(previous_node)
                    if (entrydir /= previous_directory) node = enter_before_keep(node, create_directory_info(rd, entrydir))
                    ! Now insert the node itself.
                    node = enter_before_keep(node, make_info(entries(k)))
                end if

                k = k + 1
            end if
        end do

        ! We're past the last node, all remaining entries go to the end.
        if (node == 0 .and. .not. noins) then
            lastdir = vc_directory_node_directory(ewoc_nth(e, -1))

            do j = k, n
                entrydir = entry_directory(entries(j)%name)

                ! Insert a directory node if needed.
                if (lastdir /= entrydir) then
                    lastdir = entrydir
                    rd = file_relative_name(entrydir, buffers(current)%directory)
                    i = ewoc_enter_last(e, create_directory_info(rd, entrydir))
                end if

                ! Now insert the node itself.
                i = ewoc_enter_last(e, make_info(entries(j)))
            end do
        end if

        nremove = size(to_remove)

        if (nremove > 0) then
            call bind_inhibit_read_only(saved)
            crt = ewoc_nth(e, -1)
            first = ewoc_nth(e, 0)
            i = 1

            do while (crt /= first .and. crt /= 0)
                previous = ewoc_previous(e, crt)

                if (i <= nremove) then
                    if (crt == to_remove(i)) then
                        ! Remove the entries we queued up for removal.
                        call delete_nodes([crt])
                        i = i + 1
                        crt = previous
                        cycle
                    end if
                end if

                ! Remove directories which (now) have no child files.
                if (allocated(infos(did(crt))%directory)) then
                    next = ewoc_next(e, crt)
                    has = next == 0
                    if (.not. has) has = allocated(infos(did(next))%directory)
                    if (has) call delete_nodes([crt])
                end if

                crt = previous
            end do

            call restore_inhibit_read_only(saved)
        end if

        ! Update VC file properties.
        b = vc_directory_backend()

        do j = k, n
            if (entries(j)%state == VC_UNREGISTERED) then
                call vc_file_set_backend(entries(j)%name, VC_BACKEND_NONE)
            else
                call vc_file_set_backend(entries(j)%name, b)
            end if
        end do

        call set_buffer(old, st)

    contains

        ! The sort test of the entries: by directory, then name.
        logical function before(a, b)
            integer, intent(in) :: a, b

            if (is_string_less(directories(a)%text, directories(b)%text)) then
                before = .true.
            else if (directories(a)%text /= directories(b)%text) then
                before = .false.
            else
                before = is_string_less(entries_in(a)%name, entries_in(b)%name)
            end if
        end function before

        ! (ewoc-enter-before vc-ewoc NODE DATA), keeping NODE.
        integer function enter_before_keep(node, data) result(same)
            integer, intent(in) :: node
            integer(int64), intent(in) :: data
            integer :: new

            new = ewoc_enter_before(e, node, data)
            same = node
        end function enter_before_keep
    end subroutine vc_directory_update

    !> Whether a status update process is running.  Port of
    !> vc-dir.el:vc-dir-busy.
    logical function vc_directory_busy()
        type(handle_type) :: buffer, process

        buffer = process_buffer()
        vc_directory_busy = is_buffer_live(buffer)

        if (vc_directory_busy) then
            process = get_buffer_process(buffer)
            vc_directory_busy = process%index /= 0
        end if
    end function vc_directory_busy

    !> Stop the temporary buffer and associated process.  Port of
    !> vc-dir.el:vc-dir-kill-dir-status-process.
    recursive subroutine vc_directory_stop_directory_status_process(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, process
        integer :: k

        status = status_type()
        buffer = process_buffer()
        if (.not. is_buffer_live(buffer)) return
        process = get_buffer_process(buffer)
        if (process%index /= 0) call delete_process(process, status)
        call make_local_entry(k)
        locals(k)%process_buffer = null_handle()
        call clear_mode_line_process()
    end subroutine vc_directory_stop_directory_status_process

    ! ------------------------------------------------------------------
    ! Motion
    ! ------------------------------------------------------------------

    !> Go to the next line.
    !> With prefix argument ARGUMENT, move that many lines.  Port of
    !> vc-dir.el:vc-dir-next-line.
    recursive subroutine vc_directory_next_line(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        integer :: node

        node = ewoc_goto_next(ew(), argument, status)
        if (.not. status%ok()) return
        call vc_directory_move_to_goal_column()
    end subroutine vc_directory_next_line

    !> Go to the previous line.
    !> With prefix argument ARGUMENT, move that many lines.  Port of
    !> vc-dir.el:vc-dir-previous-line.
    recursive subroutine vc_directory_previous_line(argument)
        integer, intent(in) :: argument
        integer :: node

        node = ewoc_goto_previous(ew(), argument)
        call vc_directory_move_to_goal_column()
    end subroutine vc_directory_previous_line

    !> Go to the next directory.  Port of vc-dir.el:vc-dir-next-directory.
    recursive subroutine vc_directory_next_directory()
        integer(int64) :: original
        integer :: next

        original = point()

        do
            next = ewoc_next(ew(), ewoc_locate(ew()))

            if (next == 0) then
                call goto_character(original)
                return
            end if

            call ewoc_goto_node(ew(), next)
            call vc_directory_move_to_goal_column()
            if (allocated(infos(did(next))%directory)) return
        end do
    end subroutine vc_directory_next_directory

    !> Go to the previous directory.  Port of
    !> vc-dir.el:vc-dir-previous-directory.
    recursive subroutine vc_directory_previous_directory()
        integer(int64) :: original
        integer :: previous

        original = point()

        do
            previous = ewoc_previous(ew(), ewoc_locate(ew()))

            if (previous == 0) then
                call goto_character(original)
                return
            end if

            call ewoc_goto_node(ew(), previous)
            call vc_directory_move_to_goal_column()
            if (allocated(infos(did(previous))%directory)) return
        end do
    end subroutine vc_directory_previous_directory

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    ! Call MARK_UNMARK_FUNCTION on each line of the active region, or once.
    ! Port of vc-dir.el:vc-dir-mark-unmark.
    recursive subroutine vc_directory_mark_unmark(fn, status)
        interface
            recursive subroutine fn(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine fn
        end interface
        type(status_type), intent(out) :: status
        integer(int64) :: lastl, processed_line, original_point, position
        type(status_type) :: st
        logical :: saved

        status = status_type()

        if (is_use_region()) then
            processed_line = -1
            call region_end(position, st)
            lastl = line_number_at_position(position)
            original_point = point()
            call region_beginning(position, st)
            call goto_character(position)

            do while (line_number_at_position() <= lastl)
                ! We make sure to not get stuck processing the
                ! same line in an infinite loop.
                if (processed_line == line_number_at_position()) exit
                processed_line = line_number_at_position()
                ! Avoid any prompting.
                saved = mass_mark_bound_false
                mass_mark_bound_false = .true.
                call fn(st)
                mass_mark_bound_false = saved
                ! `vc-dir-mark-file' signals an error if we try marking
                ! a directory containing marked files in its tree, or a
                ! file in a marked directory tree.  Just continue.
                if (.not. st%ok()) call vc_directory_next_line(1, st)
            end do

            call goto_character(original_point)
        else
            call fn(status)
        end if
    end subroutine vc_directory_mark_unmark

    ! Return the parent node of ARGUMENT.
    ! If IF_MARKED, return the nearest marked parent.  Port of
    ! vc-dir.el:vc-dir--parent.
    integer function vc_directory_parent(argument, if_marked) result(found)
        integer, intent(in) :: argument
        logical, intent(in) :: if_marked
        character(len=:), allocatable :: argdir, directory
        integer :: crt

        argdir = vc_directory_node_directory(argument)
        crt = argument
        found = 0

        ! Go through the predecessors, checking if any directory that is
        ! a parent is marked.
        do while (found == 0)
            crt = ewoc_previous(ew(), crt)
            if (crt == 0) exit
            directory = vc_directory_node_directory(crt)

            if (allocated(infos(did(crt))%directory) .and. index(argdir, directory) == 1) then
                if (.not. if_marked .or. infos(did(crt))%marked) found = crt
            end if
        end do
    end function vc_directory_parent

    ! Return a list of children of ARGUMENT.  If ONLY_MARKED, only those marked.
    ! Port of vc-dir.el:vc-dir--children.
    subroutine vc_directory_children(argument, only_marked, found)
        integer, intent(in) :: argument
        logical, intent(in) :: only_marked
        integer, allocatable, intent(out) :: found(:)
        character(len=:), allocatable :: argdir
        integer :: crt

        argdir = vc_directory_node_directory(argument)
        allocate (found(0))
        crt = argument

        do
            crt = ewoc_next(ew(), crt)
            if (crt == 0) exit
            ! We are done, we got to an entry that is not a child of `argument'.
            if (index(vc_directory_node_directory(crt), argdir) /= 1) exit
            if (.not. only_marked .or. infos(did(crt))%marked) found = [crt, found]
        end do
    end subroutine vc_directory_children

    !> Mark ARGUMENT (0 for the current file) and move to the next line.  Port of
    !> vc-dir.el:vc-dir-mark-file.
    recursive subroutine vc_directory_mark_file(status, argument)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: argument
        integer, allocatable :: to_inval(:), children(:)
        character(len=:), allocatable :: name
        integer(int64) :: id
        integer :: crt, parent, k
        logical :: yes

        status = status_type()
        crt = 0
        if (present(argument)) crt = argument
        if (crt == 0) crt = ewoc_locate(ew())
        if (crt == 0) return
        id = did(crt)
        to_inval = [crt]
        ! We do not allow a state in which a directory is marked and also
        ! some of its files are marked.  If the user's intent is apparent,
        ! offer to adjust things for them so that they can proceed.
        allocate (children(0))
        if (allocated(infos(id)%directory)) call vc_directory_children(crt, .true., children)
        name = infos(id)%entry%name

        if (size(children) > 0) then
            ! The user wants to mark a directory where some of its children
            ! are already marked.  Although the user's intent is clear, by
            ! default we still ask them before unmarking in order to avoid
            ! accidental erasure of complex patterns of marks.
            yes = is_mass_mark_allowed()

            if (yes .and. is_mass_mark_asked()) then
                call read_y_or_n("Replace marks on subitems with marking `"//name//"' itself?", yes, status)
                if (.not. status%ok()) return
            end if

            if (.not. yes) then
                call set_error(status, USER_ERROR_CODE, "`"//name//"' is already marked")
                return
            end if

            do k = 1, size(children)
                infos(did(children(k)))%marked = .false.
                to_inval = [children(k), to_inval]
            end do
        else
            parent = vc_directory_parent(crt, .true.)

            if (parent /= 0) then
                name = infos(did(parent))%entry%name
                ! The user seems to want to mark an entry whose directory is
                ! already marked.  As the file is already implicitly marked for
                ! most VCS, they may not really intend this.
                yes = is_mass_mark_allowed()

                if (yes .and. is_mass_mark_asked()) then
                    call read_y_or_n("`"//name//"' is already marked; unmark it?", yes, status)
                    if (.not. status%ok()) return
                end if

                if (.not. yes) then
                    call set_error(status, USER_ERROR_CODE, "`"//name//"' is already marked")
                    return
                end if

                infos(did(parent))%marked = .false.
                to_inval = [parent, to_inval]
            end if
        end if

        infos(id)%marked = .true.
        call ewoc_invalidate(ew(), to_inval)
        ! (mouse-event-p last-command-event): mouse events are not
        ! ported.
        if (.not. present(argument)) call vc_directory_next_line(1, status)
    end subroutine vc_directory_mark_file

    ! Not a port.
    recursive subroutine mark_file_current(status)
        type(status_type), intent(out) :: status

        call vc_directory_mark_file(status)
    end subroutine mark_file_current

    !> Mark the current file or all files in the region.
    !> If the region is active, mark all the files in the region.
    !> Otherwise mark the file on the current line and move to the next
    !> line.  Port of vc-dir.el:vc-dir-mark.
    recursive subroutine vc_directory_mark(status)
        type(status_type), intent(out) :: status

        call vc_directory_mark_unmark(mark_file_current, status)
    end subroutine vc_directory_mark

    ! Not a port.
    recursive logical function directory_marked(data)
        integer(int64), intent(in) :: data

        directory_marked = allocated(infos(data)%directory) .and. infos(data)%marked
    end function directory_marked

    ! Not a port.
    recursive logical function mark_unmarked_file(data)
        integer(int64), intent(in) :: data

        mark_unmarked_file = .false.
        if (allocated(infos(data)%directory) .or. infos(data)%marked) return
        infos(data)%marked = .true.
        mark_unmarked_file = .true.
    end function mark_unmarked_file

    !> Mark all files with the same state as the current one.
    !> With prefix argument ARGUMENT, mark all files (not directories).
    !> If the current entry is a directory, mark all child files.
    !>
    !> The commands operate on files that are on the same state.
    !> This command is intended to make it easy to select all files that
    !> share the same state.  Port of vc-dir.el:vc-dir-mark-all-files.
    recursive subroutine vc_directory_mark_all_files(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: marked_directories(:)
        integer :: crt, state
        integer(int64) :: id

        status = status_type()

        if (argument) then
            ! Mark all files.
            ! First check that no directory is marked, we can't mark
            ! files in that case.
            call ewoc_collect(ew(), directory_marked, marked_directories)

            if (size(marked_directories) > 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Cannot mark all files, directory `"// &
                               infos(marked_directories(1))%entry%name//"' marked")
                return
            end if

            call ewoc_map(mark_unmarked_file, ew())
        else
            crt = ewoc_locate(ew())
            if (crt == 0) return
            id = did(crt)

            if (allocated(infos(id)%directory)) then
                ! It's a directory, mark child files.
                do
                    crt = ewoc_next(ew(), crt)
                    if (crt == 0) exit
                    if (allocated(infos(did(crt))%directory)) exit
                    infos(did(crt))%marked = .true.
                    call ewoc_invalidate(ew(), [crt])
                end do
            else
                ! It's a file
                state = infos(id)%entry%state
                crt = ewoc_nth(ew(), 0)

                do while (crt /= 0)
                    id = did(crt)

                    if (.not. infos(id)%marked .and. infos(id)%entry%state == state .and. &
                        .not. allocated(infos(id)%directory)) then
                        call vc_directory_mark_file(status, crt)
                        if (.not. status%ok()) return
                    end if

                    crt = ewoc_next(ew(), crt)
                end do
            end if
        end if
    end subroutine vc_directory_mark_all_files

    ! Not a port.
    recursive logical function mark_by_regular_expression(data)
        integer(int64), intent(in) :: data
        type(status_type) :: st

        mark_by_regular_expression = .false.
        if (allocated(infos(data)%directory)) return
        if ((.not. map_unmark) .neqv. (.not. infos(data)%marked)) return

        ! We don't want to match on the part of the file
        ! that's above the current directory.
        if (.not. string_matches(map_regular_expression, file_relative_name(infos(data)%entry%name, buffers(current)%directory), &
                                 st)) then
            return
        end if

        infos(data)%marked = .not. map_unmark
        mark_by_regular_expression = .true.
    end function mark_by_regular_expression

    !> Mark all files that match REGULAR_EXPRESSION.
    !> If UNMARK (interactively, the prefix), unmark instead.  Port of
    !> vc-dir.el:vc-dir-mark-by-regexp.
    recursive subroutine vc_directory_mark_by_regular_expression(regular_expression, unmark)
        character(len=*), intent(in) :: regular_expression
        logical, intent(in), optional :: unmark

        map_regular_expression = regular_expression
        map_unmark = .false.
        if (present(unmark)) map_unmark = unmark
        call ewoc_map(mark_by_regular_expression, ew())
    end subroutine vc_directory_mark_by_regular_expression

    ! Not a port.
    recursive logical function mark_listed_file(data)
        integer(int64), intent(in) :: data
        character(len=:), allocatable :: name
        integer :: k

        mark_listed_file = .false.
        name = expand(infos(data)%entry%name)

        do k = 1, size(map_files)
            if (map_files(k)%text == name) then
                infos(data)%marked = .true.
                mark_listed_file = .true.
                return
            end if
        end do
    end function mark_listed_file

    !> Mark files specified by file names in the argument MARK_FILES.
    !> MARK_FILES should be a list of absolute filenames.
    !> Directories must have trailing slashes.  Port of
    !> vc-dir.el:vc-dir-mark-files.
    recursive subroutine vc_directory_mark_files(mark_files)
        type(string_type), intent(in) :: mark_files(:)
        type(string_type), allocatable :: sorted(:)
        type(string_type) :: temporary
        integer :: k, j, n

        ! Filter out subitems that would be implicitly marked.
        sorted = mark_files

        do k = 2, size(sorted)
            temporary = sorted(k)
            j = k - 1

            do while (j >= 1)
                if (.not. is_string_less(temporary%text, sorted(j)%text)) exit
                sorted(j + 1) = sorted(j)
                j = j - 1
            end do

            sorted(j + 1) = temporary
        end do

        n = 0
        if (allocated(map_files)) deallocate (map_files)
        allocate (map_files(size(sorted)))
        k = 1

        do while (k <= size(sorted))
            n = n + 1
            map_files(n) = sorted(k)
            j = k + 1

            if (len(sorted(k)%text) > 0) then
                if (sorted(k)%text(len(sorted(k)%text):) == "/") then
                    do while (j <= size(sorted))
                        if (index(sorted(j)%text, sorted(k)%text) /= 1) exit
                        j = j + 1
                    end do
                end if
            end if

            k = j
        end do

        map_files = map_files(1:n)
        call ewoc_map(mark_listed_file, ew())
    end subroutine vc_directory_mark_files

    ! Not a port.
    recursive logical function mark_state(data)
        integer(int64), intent(in) :: data

        mark_state = .false.
        if (.not. any(map_states == infos(data)%entry%state)) return
        infos(data)%marked = .true.
        mark_state = .true.
    end function mark_state

    !> Mark files that are in the state specified by the list in STATES.
    !> Port of vc-dir.el:vc-dir-mark-state-files.
    recursive subroutine vc_directory_mark_state_files(states)
        integer, intent(in) :: states(:)

        map_states = states
        call ewoc_map(mark_state, ew())
    end subroutine vc_directory_mark_state_files

    !> Mark files that are in one of registered states: edited, added or
    !> removed.  Port of vc-dir.el:vc-dir-mark-registered-files.
    recursive subroutine vc_directory_mark_registered_files(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call vc_directory_mark_state_files([VC_EDITED, VC_ADDED, VC_REMOVED])
    end subroutine vc_directory_mark_registered_files

    !> Mark files that are in unregistered state.  Port of
    !> vc-dir.el:vc-dir-mark-unregistered-files.
    recursive subroutine vc_directory_mark_unregistered_files(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call vc_directory_mark_state_files([VC_UNREGISTERED])
    end subroutine vc_directory_mark_unregistered_files

    !> Unmark the current file and move to the next line.  Port of
    !> vc-dir.el:vc-dir-unmark-file.
    recursive subroutine vc_directory_unmark_file(status)
        type(status_type), intent(out) :: status
        integer, allocatable :: to_inval(:), all_children(:), subtree(:), children(:)
        integer(int64) :: id
        integer :: crt, parent, k
        logical :: yes

        status = status_type()
        crt = ewoc_locate(ew())
        if (crt == 0) return
        id = did(crt)
        allocate (to_inval(0))

        if (infos(id)%marked) then
            infos(id)%marked = .false.
            to_inval = [crt, to_inval]
        else
            ! The current item is not explicitly marked, but its containing
            ! directory is marked.  So this item is implicitly marked, for
            ! most VCS.  Offer to change that.
            parent = vc_directory_parent(crt, .true.)
            allocate (all_children(0))
            if (parent /= 0) call vc_directory_children(parent, .false., all_children)

            if (parent /= 0 .and. size(all_children) > 0) then
                yes = is_mass_mark_allowed()

                if (yes .and. is_mass_mark_asked()) then
                    call read_y_or_n("Replace mark on `"//infos(id)%entry%name// &
                                     "' with marks on all subitems but this one?", yes, status)
                    if (.not. status%ok()) return
                end if

                if (yes) then
                    if (allocated(infos(id)%directory)) then
                        call vc_directory_children(crt, .false., children)
                        subtree = [crt, children]
                    else
                        subtree = [crt, vc_directory_parent(crt, .false.)]
                    end if

                    infos(did(parent))%marked = .false.
                    to_inval = [parent, to_inval]

                    do k = 1, size(all_children)
                        infos(did(all_children(k)))%marked = .not. any(subtree == all_children(k))
                        to_inval = [all_children(k), to_inval]
                    end do
                end if
            else if (allocated(infos(id)%directory)) then
                ! The current item is a directory that's not marked, implicitly
                ! or explicitly, but it has marked items below it.
                ! Offer to unmark those.
                call vc_directory_children(crt, .true., children)

                if (size(children) > 0 .and. is_mass_mark_allowed()) then
                    yes = .true.

                    if (is_mass_mark_asked()) then
                        call read_y_or_n("Unmark all items within `"//infos(id)%entry%name//"'?", yes, status)
                        if (.not. status%ok()) return
                    end if

                    if (yes) then
                        do k = 1, size(children)
                            infos(did(children(k)))%marked = .false.
                            to_inval = [children(k), to_inval]
                        end do
                    end if
                end if
            end if
        end if

        if (size(to_inval) > 0) call ewoc_invalidate(ew(), to_inval)
        call vc_directory_next_line(1, status)
    end subroutine vc_directory_unmark_file

    !> Unmark the current file or all files in the region.
    !> If the region is active, unmark all the files in the region.
    !> Otherwise unmark the file on the current line and move to the next
    !> line.  Port of vc-dir.el:vc-dir-unmark.
    recursive subroutine vc_directory_unmark(status)
        type(status_type), intent(out) :: status

        call vc_directory_mark_unmark(vc_directory_unmark_file, status)
    end subroutine vc_directory_unmark

    !> Move to the previous line and unmark the file.  Port of
    !> vc-dir.el:vc-dir-unmark-file-up.
    recursive subroutine vc_directory_unmark_file_up(status)
        type(status_type), intent(out) :: status
        integer :: previous

        status = status_type()
        ! If we're on the first line, we won't move up, but we will still
        ! remove the mark.  This seems a bit odd but it is what buffer-menu
        ! does.
        previous = ewoc_goto_previous(ew(), 1)
        if (previous == 0) return
        infos(did(previous))%marked = .false.
        call ewoc_invalidate(ew(), [previous])
        call vc_directory_move_to_goal_column()
    end subroutine vc_directory_unmark_file_up

    ! Not a port.
    recursive logical function unmark_entry(data)
        integer(int64), intent(in) :: data

        unmark_entry = .false.
        if (.not. infos(data)%marked) return
        infos(data)%marked = .false.
        unmark_entry = .true.
    end function unmark_entry

    ! Not a port.
    recursive logical function unmark_state(data)
        integer(int64), intent(in) :: data

        unmark_state = .false.
        if (.not. infos(data)%marked .or. infos(data)%entry%state /= map_state) return
        infos(data)%marked = .false.
        unmark_state = .true.
    end function unmark_state

    !> Unmark all files with the same state as the current one.
    !> With prefix argument ARGUMENT, unmark all files.
    !> If the current entry is a directory, unmark all the child files.
    !>
    !> The commands operate on files that are on the same state.
    !> This command is intended to make it easy to deselect all files
    !> that share the same state.  Port of vc-dir.el:vc-dir-unmark-all-files.
    recursive subroutine vc_directory_unmark_all_files(argument)
        logical, intent(in) :: argument
        integer :: crt

        if (argument) then
            call ewoc_map(unmark_entry, ew())
        else
            crt = ewoc_locate(ew())
            if (crt == 0) return

            if (allocated(infos(did(crt))%directory)) then
                ! It's a directory, unmark child files.
                do
                    crt = ewoc_next(ew(), crt)
                    if (crt == 0) exit

                    if (.not. allocated(infos(did(crt))%directory)) then
                        infos(did(crt))%marked = .false.
                        call ewoc_invalidate(ew(), [crt])
                    end if
                end do
            else
                ! It's a file
                map_state = infos(did(crt))%entry%state
                call ewoc_map(unmark_state, ew())
            end if
        end if
    end subroutine vc_directory_unmark_all_files

    !> Toggle the mark of the current file.  Port of
    !> vc-dir.el:vc-dir-toggle-mark-file.
    recursive subroutine vc_directory_toggle_mark_file(status)
        type(status_type), intent(out) :: status
        integer :: crt

        status = status_type()
        crt = ewoc_locate(ew())
        if (crt == 0) return

        if (infos(did(crt))%marked) then
            call vc_directory_unmark_file(status)
        else
            call vc_directory_mark_file(status)
        end if
    end subroutine vc_directory_toggle_mark_file

    ! ------------------------------------------------------------------
    ! Commands on the files
    ! ------------------------------------------------------------------

    !> Like `vc_next_action' but for all files shown in the VC-Dir buffer.
    !> This command ignores VC-Dir marks and the position of point.
    !> When the VC-Dir's root directory is the repository root (as it usually
    !> is), this command is useful to check in all local changes at once.
    !> Port of vc-dir.el:vc-dir-root-next-action.
    recursive subroutine vc_directory_root_next_action(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        integer, allocatable :: states(:)
        type(vc_fileset_type) :: fs
        integer :: crt
        integer(int64) :: id

        allocate (files(0), states(0))
        crt = ewoc_nth(ew(), 0)

        do while (crt /= 0)
            id = did(crt)

            if (.not. allocated(infos(id)%directory)) then
                call append_file_state(files, states, expand(infos(id)%entry%name), infos(id)%entry%state)
            end if

            crt = ewoc_next(ew(), crt)
        end do

        fs%backend = vc_directory_backend()
        call append_string(fs%files, buffers(current)%directory)
        call vc_directory_only_files_state_and_model(files, states, fs, status)
        if (.not. status%ok()) return
        call set_vc_buffer_overriding_fileset(fs)
        call vc_next_action(.false., status)
        call set_vc_buffer_overriding_fileset()
    end subroutine vc_directory_root_next_action

    !> Delete the marked files, or the current file if no marks.
    !> The files will also be marked as deleted in the version control
    !> system.  Port of vc-dir.el:vc-dir-delete-file.
    recursive subroutine vc_directory_delete_file(status)
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs

        call vc_directory_deduce_fileset(fs, status, .true.)
        if (.not. status%ok()) return

        if (.not. allocated(fs%only_files)) then
            call set_error(status, USER_ERROR_CODE, "Nothing to delete here")
            return
        end if

        if (size(fs%only_files) == 0) then
            call set_error(status, USER_ERROR_CODE, "Nothing to delete here")
            return
        end if

        call vc_delete_file(fs%only_files, status)
    end subroutine vc_directory_delete_file

    !> Find the file on the current line.  Port of vc-dir.el:vc-dir-find-file.
    recursive subroutine vc_directory_find_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call vc_directory_current_file(file, status)
        if (status%ok()) call find_file(file, status)
    end subroutine vc_directory_find_file

    !> Find the file on the current line, in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of vc-dir.el:vc-dir-find-file-other-window.
    recursive subroutine vc_directory_find_file_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call vc_directory_current_file(file, status)
        if (status%ok()) call find_file_other_window(file, status)
    end subroutine vc_directory_find_file_other_window

    !> Display the file on the current line, in another window.  Port of
    !> vc-dir.el:vc-dir-display-file.
    recursive subroutine vc_directory_display_file(status)
        use window_mod, only: display_buffer, display_action_parameters_type
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: buffer, window
        type(display_action_parameters_type) :: table

        call vc_directory_current_file(file, status)
        if (.not. status%ok()) return
        call find_file_noselect(file, buffer, status)
        if (.not. status%ok()) return
        ! (display-buffer ... t): not in the selected window.
        table%inhibit_same_window = .true.
        call display_buffer(buffer, window, status, table=table)
    end subroutine vc_directory_display_file

    !> Examine a file on the current line in view mode.  Port of
    !> vc-dir.el:vc-dir-view-file.
    recursive subroutine vc_directory_view_file(status)
        use view_mod, only: view_file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call vc_directory_current_file(file, status)
        if (status%ok()) call view_file(file, status)
    end subroutine vc_directory_view_file

    ! Not a port.
    recursive logical function ignore_marked(data)
        integer(int64), intent(in) :: data
        type(status_type) :: st

        ignore_marked = .false.
        if (.not. infos(data)%marked) return
        call vc_ignore(infos(data)%entry%name, st)
        ignore_marked = .true.
    end function ignore_marked

    !> Ignore the current file.
    !> If a prefix argument is given, ignore all marked files.  Port of
    !> vc-dir.el:vc-dir-ignore.
    recursive subroutine vc_directory_ignore(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: relative_directory, file

        status = status_type()

        if (argument) then
            call ewoc_map(ignore_marked, ew())
        else
            relative_directory = vc_ignore_base_directory(status)
            if (.not. status%ok()) return
            call vc_directory_current_file(file, status)
            if (.not. status%ok()) return
            call vc_ignore(file_relative_name(file, relative_directory), status, relative_directory)
        end if
    end subroutine vc_directory_ignore

    !> The file at point (an error if there is none).  Port of
    !> vc-dir.el:vc-dir-current-file.
    recursive subroutine vc_directory_current_file(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        integer :: node

        status = status_type()
        file = ""
        node = ewoc_locate(ew())

        if (node == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No file available")
            return
        end if

        file = expand(infos(did(node))%entry%name)
    end subroutine vc_directory_current_file

    ! Not a port.
    recursive logical function is_marked(data)
        integer(int64), intent(in) :: data

        is_marked = infos(data)%marked
    end function is_marked

    ! Not a port.
    recursive logical function needs_update(data)
        integer(int64), intent(in) :: data

        needs_update = infos(data)%needs_update
    end function needs_update

    !> Return the list of marked files.  Port of vc-dir.el:vc-dir-marked-files.
    subroutine vc_directory_marked_files(files)
        type(string_type), allocatable, intent(out) :: files(:)
        integer(int64), allocatable :: ids(:)
        integer :: k

        call ewoc_collect(ew(), is_marked, ids)
        allocate (files(size(ids)))

        do k = 1, size(ids)
            files(k)%text = expand(infos(ids(k))%entry%name)
        end do
    end subroutine vc_directory_marked_files

    !> Return the FILE and STATE of each marked file.
    !> For marked directories return the FILE and STATE of the
    !> child files.  Port of vc-dir.el:vc-dir-marked-only-files-and-states.
    subroutine vc_directory_marked_only_files_and_states(files, states)
        type(string_type), allocatable, intent(out) :: files(:)
        integer, allocatable, intent(out) :: states(:)
        character(len=:), allocatable :: directory
        integer(int64) :: id, data
        integer :: crt

        allocate (files(0), states(0))
        crt = ewoc_nth(ew(), 0)

        do while (crt /= 0)
            id = did(crt)

            if (infos(id)%marked) then
                ! FIXME: use vc-dir-child-files-and-states here instead of duplicating it.
                if (allocated(infos(id)%directory)) then
                    directory = infos(id)%directory

                    do
                        crt = ewoc_next(ew(), crt)
                        if (crt == 0) exit
                        data = did(crt)
                        if (index(vc_directory_node_directory(crt), directory) /= 1) exit

                        if (.not. allocated(infos(data)%directory)) then
                            call append_file_state(files, states, expand(infos(data)%entry%name), infos(data)%entry%state)
                        end if
                    end do
                else
                    call append_file_state(files, states, expand(infos(id)%entry%name), infos(id)%entry%state)
                    crt = ewoc_next(ew(), crt)
                end if
            else
                crt = ewoc_next(ew(), crt)
            end if
        end do
    end subroutine vc_directory_marked_only_files_and_states

    ! Append (FILE . STATE).  Not a port.
    subroutine append_file_state(files, states, file, state)
        type(string_type), allocatable, intent(inout) :: files(:)
        integer, allocatable, intent(inout) :: states(:)
        character(len=*), intent(in) :: file
        integer, intent(in) :: state

        call append_string(files, file)
        states = [states, state]
    end subroutine append_file_state

    !> Return the state of one or more files for the current entry.
    !> If the entry is a directory, return the list of states of its child
    !> files, where each file is described by two values, FILE and STATE.
    !> If the entry is a file, return its FILE and STATE for
    !> that file.  Port of vc-dir.el:vc-dir-child-files-and-states.
    subroutine vc_directory_child_files_and_states(files, states)
        type(string_type), allocatable, intent(out) :: files(:)
        integer, allocatable, intent(out) :: states(:)
        character(len=:), allocatable :: directory
        integer(int64) :: id, data
        integer :: crt

        allocate (files(0), states(0))
        crt = ewoc_locate(ew())
        if (crt == 0) return
        id = did(crt)

        if (allocated(infos(id)%directory)) then
            directory = infos(id)%directory

            do
                crt = ewoc_next(ew(), crt)
                if (crt == 0) exit
                data = did(crt)
                if (index(vc_directory_node_directory(crt), directory) /= 1) exit

                if (.not. allocated(infos(data)%directory)) then
                    call append_file_state(files, states, expand(infos(data)%entry%name), infos(data)%entry%state)
                end if
            end do
        else
            call append_file_state(files, states, expand(infos(id)%entry%name), infos(id)%entry%state)
        end if
    end subroutine vc_directory_child_files_and_states

    ! Compute state of FILE_NAME known to live inside DEF_DIRECTORY.  Port of
    ! vc-dir.el:vc-dir-recompute-file-state.
    recursive subroutine vc_directory_recompute_file_state(file_name, def_directory, entry)
        character(len=*), intent(in) :: file_name, def_directory
        type(vc_directory_entry_type), intent(out) :: entry
        character(len=:), allocatable :: saved
        integer :: b

        entry%name = file_relative_name(file_name, def_directory)
        b = vc_directory_backend()
        if (b == 0) return
        saved = buffers(current)%directory
        buffers(current)%directory = def_directory
        if (associated(vc_backends(b)%state)) entry%state = vc_backends(b)%state(entry%name)

        if (associated(vc_backends(b)%status_fileinfo_extra)) then
            call vc_backends(b)%status_fileinfo_extra(entry%name, entry)
        end if

        buffers(current)%directory = saved

        ! Ensure we return a nil state if the file does not exist and is
        ! not tracked so that it disappears from VC-Dir (bug#81191).
        if (entry%state == VC_UP_TO_DATE .and. .not. file_exists(file_name)) then
            entry%state = VC_STATE_UNKNOWN
            entry%has_extra = .false.
        end if
    end subroutine vc_directory_recompute_file_state

    ! Give a DIRECTORY_NAME string return the list of all child files shown in
    ! the current *vc-dir* buffer.  Port of
    ! vc-dir.el:vc-dir-find-child-files.
    subroutine vc_directory_find_child_files(directory_name, children)
        character(len=*), intent(in) :: directory_name
        type(string_type), allocatable, intent(out) :: children(:)
        integer, allocatable :: states(:)
        integer :: crt

        allocate (children(0), states(0))
        crt = ewoc_nth(ew(), 0)

        ! Find DIRECTORY
        do while (crt /= 0)
            if (index(vc_directory_node_directory(crt), directory_name) == 1) exit
            crt = ewoc_next(ew(), crt)
        end do

        do while (crt /= 0)
            if (index(vc_directory_node_directory(crt), directory_name) /= 1) exit

            if (.not. allocated(infos(did(crt))%directory)) then
                ! (push ...)
                call append_file_state(children, states, expand(infos(did(crt))%entry%name), 0)
            end if

            crt = ewoc_next(ew(), crt)
        end do

        children = children(size(children):1:-1)
    end subroutine vc_directory_find_child_files

    ! Update the entries for all the child files of DIRECTORY_NAME shown in
    ! the current *vc-dir* buffer.  Port of
    ! vc-dir.el:vc-dir-resync-directory-files.
    recursive subroutine vc_directory_resync_directory_files(directory_name)
        character(len=*), intent(in) :: directory_name
        type(string_type), allocatable :: files(:)
        type(vc_directory_entry_type), allocatable :: fileentries(:)
        integer :: k, n

        call vc_directory_find_child_files(directory_name, files)
        n = size(files)
        if (n == 0) return
        allocate (fileentries(n))

        do k = 1, n
            ! (push ...): the last first.
            call vc_directory_recompute_file_state(files(k)%text, buffers(current)%directory, fileentries(n + 1 - k))
        end do

        call vc_directory_update(fileentries, current_buffer())
    end subroutine vc_directory_resync_directory_files

    !> Update the entries for FILE_NAME in any directory buffers that list it.
    !> Port of vc-dir.el:vc-dir-resynch-file (the `vc_directory_resynch_file' of
    !> version_control_hooks_mod).
    recursive subroutine vc_directory_resynch_file_impl(file_name)
        character(len=*), intent(in) :: file_name
        character(len=:), allocatable :: file, ddir, truedir
        type(handle_type), allocatable :: keep(:)
        type(handle_type) :: old, status_buffer
        type(vc_directory_entry_type) :: complete_state(1)
        type(ewoc_part_type), allocatable :: header(:)
        type(ewoc_part_type) :: none(0)
        type(status_type) :: st
        integer :: k, n

        call file_truename(file_name, file, st)
        if (.not. st%ok()) file = file_name
        if (.not. allocated(vc_directory_buffers)) return
        old = current_buffer()
        allocate (keep(size(vc_directory_buffers)))
        n = 0

        ! look for a vc-dir buffer that might show this file.
        do k = 1, size(vc_directory_buffers)
            status_buffer = vc_directory_buffers(k)
            if (.not. is_buffer_live(status_buffer)) cycle
            call set_buffer(status_buffer, st)
            if (.not. is_derived_mode("vc_directory_mode")) cycle
            n = n + 1
            keep(n) = status_buffer
            ! The actual contents of this VC-Dir buffer,
            ! which is what we care about here, is always
            ! relative to the toplevel value.
            call expand_file_name(buffers(current)%directory, ddir, st)
            if (.not. is_file_in_directory(file, ddir)) cycle

            if (is_file_directory(file)) then
                call vc_directory_resync_directory_files(file)
                call vc_directory_headers(vc_directory_backend(), ddir, header)
                call ewoc_set_hf_parts(ew(), header, none)
            else
                ! Pass two truenames (bug#80803, bug#80967).
                call file_truename(ddir, truedir, st)
                if (.not. st%ok()) truedir = ddir
                call vc_directory_recompute_file_state(file, truedir, complete_state(1))
                call vc_directory_update(complete_state, status_buffer, &
                                         complete_state(1)%state == VC_STATE_UNKNOWN .or. &
                                         complete_state(1)%state == VC_UP_TO_DATE)
            end if
        end do

        call set_buffer(old, st)
        ! Remove out-of-date entries from vc-dir-buffers.
        vc_directory_buffers = keep(1:n)
    end subroutine vc_directory_resynch_file_impl

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for VC directory buffers.
    !> Marking/Unmarking key bindings and actions: \<vc_directory_mode_map>
    !> \[vc_directory_mark] - mark a file/directory
    !>   - if the region is active, mark all the files in region.
    !>     Restrictions: - a file cannot be marked if any parent directory is marked
    !>                   - a directory cannot be marked if any child file or
    !>                     directory is marked
    !> \[vc_directory_unmark] - unmark a file/directory
    !>   - if the region is active, unmark all the files in region.
    !> \[vc_directory_mark_all_files] - if the cursor is on a file: mark all the files with the same state as
    !>       the current file
    !>   - if the cursor is on a directory: mark all child files
    !>   - with a prefix argument: mark all files
    !> \[vc_directory_unmark_all_files] - if the cursor is on a file: unmark all the files with the same state
    !>       as the current file
    !>   - if the cursor is on a directory: unmark all child files
    !>   - with a prefix argument: unmark all files
    !>
    !> VC commands
    !> VC commands in the \[vc_prefix_map] prefix can be used.
    !> VC commands act on the marked entries.  If nothing is marked, VC
    !> commands act on the current entry.
    !>
    !> \{vc_directory_mode_map}  Port of vc-dir.el:vc-dir-mode (Search & Replace
    !> are deferred).
    recursive subroutine vc_directory_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved_mode
        integer :: k

        if (.not. associated(major_modes%vc_directory_mode%execute)) call initialize_vc_directory()
        call major_modes%vc_directory_mode%begin(saved_mode, status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        locals(k)%backend = use_vc_backend
        buffers(current)%read_only = .false.
        call erase_buffer(status)

        if (status%ok()) then
            locals(k)%process_buffer = null_handle()
            locals(k)%ewoc = make_ewoc(vc_directory_printer)
            locals(k)%has_ewoc = .true.
            call revert_buffer_function%set_local(vc_directory_revert_buffer_function)
            ! (setq list-buffers-directory ...): Buffer Menu is not ported.
            call append_vc_directory_buffer(current_buffer())
            call vc_directory_refresh(status)
        end if

        buffers(current)%read_only = .true.
        if (.not. status%ok()) return
        call major_modes%vc_directory_mode%end(saved_mode, status)
    end subroutine vc_directory_mode

    ! (add-to-list 'vc-dir-buffers BUFFER).  Not a port.
    subroutine append_vc_directory_buffer(buffer)
        type(handle_type), intent(in) :: buffer

        if (.not. allocated(vc_directory_buffers)) allocate (vc_directory_buffers(0))
        if (any(vc_directory_buffers%index == buffer%index .and. vc_directory_buffers%generation == buffer%generation)) return
        vc_directory_buffers = [buffer, vc_directory_buffers]
    end subroutine append_vc_directory_buffer

    !> Display the headers in the *VC-Dir* buffer.
    !> It calls the `directory_extra_headers' backend method to display backend
    !> specific headers.  Port of vc-dir.el:vc-dir-headers (the outgoing
    !> count is deferred).
    recursive subroutine vc_directory_headers(backend, directory, parts)
        integer, intent(in) :: backend
        character(len=*), intent(in) :: directory
        type(ewoc_part_type), allocatable, intent(out) :: parts(:)
        type(ewoc_part_type), allocatable :: extra(:)
        integer :: k, n

        if (associated(vc_backends(backend)%directory_extra_headers)) then
            call vc_backends(backend)%directory_extra_headers(directory, extra)
        else
            call vc_default_directory_extra_headers(directory, extra)
        end if

        n = size(extra)
        allocate (parts(5 + n))
        parts(1)%text = "VC backend : "
        parts(1)%face = "vc_directory_header"
        parts(2)%text = vc_backend_name(backend)//LF
        parts(2)%face = "vc_directory_header_value"
        parts(3)%text = "Working dir: "
        parts(3)%face = "vc_directory_header"
        parts(4)%text = abbreviate_file_name(directory)//LF
        parts(4)%face = "vc_directory_header_value"

        do k = 1, n
            parts(4 + k) = extra(k)
        end do

        parts(5 + n)%text = LF
        parts(5 + n)%face = ""
    end subroutine vc_directory_headers

    ! The update function of `vc_directory_refresh_files': add ENTRIES and,
    ! when complete, remove the entries not updated at all and the
    ! directories left without children.  Not a port.
    recursive subroutine refresh_files_update(entries, buffer, more_to_come)
        type(vc_directory_entry_type), intent(in) :: entries(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: more_to_come
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        integer :: crt, first, previous, next
        logical :: empty

        old = current_buffer()
        call set_buffer(buffer, st)
        call vc_directory_update(entries, buffer)

        if (.not. more_to_come) then
            call clear_mode_line_process()
            ! Remove the ones that haven't been updated at all.
            ! Those not-updated are those whose state is nil because the
            ! file/dir doesn't exist and isn't versioned.
            call ewoc_filter(ew(), not_needs_update)
            ! One more pass to remove directory entries with no children.
            call bind_inhibit_read_only(saved)
            crt = ewoc_nth(ew(), -1)
            first = ewoc_nth(ew(), 0)

            do while (crt /= first .and. crt /= 0)
                previous = ewoc_previous(ew(), crt)

                if (allocated(infos(did(crt))%directory)) then
                    next = ewoc_next(ew(), crt)
                    empty = next == 0
                    if (.not. empty) empty = allocated(infos(did(next))%directory)
                    if (empty) call delete_nodes([crt])
                end if

                crt = previous
            end do

            call restore_inhibit_read_only(saved)
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine refresh_files_update

    ! The test of `ewoc_filter' in `refresh_files_update'.  Not a port.
    recursive logical function not_needs_update(data)
        integer(int64), intent(in) :: data

        ! The state for directory entries might
        ! have been changed to 'up-to-date,
        ! reset it, otherwise it will be removed when doing 'x'
        ! next time.
        ! FIXME: There should be a more elegant way to do this.
        if (allocated(infos(data)%directory) .and. infos(data)%entry%state == VC_UP_TO_DATE) then
            infos(data)%entry%state = VC_STATE_UNKNOWN
        end if

        not_needs_update = .not. infos(data)%needs_update
        if (.not. not_needs_update) info_live(data) = .false.
    end function not_needs_update

    ! The temporary buffer for the status process, made if needed.  Not a port.
    recursive subroutine ensure_process_buffer(backend, buffer)
        integer, intent(in) :: backend
        type(handle_type), intent(out) :: buffer
        type(status_type) :: st
        integer :: k

        call make_local_entry(k)

        if (.not. is_buffer_live(locals(k)%process_buffer)) then
            call get_or_make_buffer(generate_new_buffer_name(" *VC-"//vc_backend_name(backend)//"* tmp status"), &
                                    locals(k)%process_buffer, st)
        end if

        buffer = locals(k)%process_buffer
    end subroutine ensure_process_buffer

    !> Refresh some FILES in the *VC-Dir* buffer.  Port of
    !> vc-dir.el:vc-dir-refresh-files.
    recursive subroutine vc_directory_refresh_files(files, status)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: def_directory
        type(handle_type) :: buffer, pbuf
        type(status_type) :: st
        integer :: backend

        status = status_type()
        def_directory = buffers(current)%directory
        backend = vc_directory_backend()
        call vc_set_mode_line_busy_indicator()
        ! Call the `dir-status-files' backend function.
        ! `dir-status-files' is supposed to be asynchronous.
        ! It should compute the results, and then call the function
        ! passed as an argument in order to update the vc-dir buffer
        ! with the results.
        call ensure_process_buffer(backend, pbuf)
        buffer = current_buffer()
        call set_buffer(pbuf, st)
        buffers(current)%directory = def_directory
        call erase_buffer(st)

        if (associated(vc_backends(backend)%directory_status_files)) then
            call vc_backends(backend)%directory_status_files(def_directory, files, refresh_files_update, buffer, status)
        end if

        call set_buffer(buffer, st)
    end subroutine vc_directory_refresh_files

    ! The `revert_buffer_function' of VC-Dir buffers.  Port of
    ! vc-dir.el:vc-dir-revert-buffer-function.
    recursive subroutine vc_directory_revert_buffer_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status

        if (ignore_auto .and. noconfirm .and. .false.) return
        call vc_directory_refresh(status)
        if (.not. status%ok()) return
        if (.not. vc_directory_auto_hide_up_to_date%is_off()) call vc_directory_hide_state()
    end subroutine vc_directory_revert_buffer_function

    ! The update function of `vc_directory_refresh'.  Not a port.
    recursive subroutine refresh_update(entries, buffer, more_to_come)
        type(vc_directory_entry_type), intent(in) :: entries(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: more_to_come
        type(string_type), allocatable :: names(:)
        integer(int64), allocatable :: remaining(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k

        old = current_buffer()
        call set_buffer(buffer, st)
        call vc_directory_update(entries, buffer)

        if (.not. more_to_come) then
            call ewoc_collect(ew(), needs_update, remaining)

            if (size(remaining) > 0) then
                allocate (names(size(remaining)))

                do k = 1, size(remaining)
                    names(k)%text = infos(remaining(k))%entry%name
                end do

                call vc_directory_refresh_files(names, st)
            else
                call clear_mode_line_process()
                call vc_directory_refresh_hook%execute(st)
            end if
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine refresh_update

    ! Not a port.
    recursive logical function set_needs_update(data)
        integer(int64), intent(in) :: data

        if (.not. allocated(infos(data)%directory)) infos(data)%needs_update = .true.
        set_needs_update = .false.
    end function set_needs_update

    !> Refresh the contents of the *VC-Dir* buffer.
    !> An error condition occurs if another update process is in progress.  Port of
    !> vc-dir.el:vc-dir-refresh.
    recursive subroutine vc_directory_refresh(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: def_directory
        type(ewoc_part_type), allocatable :: header(:)
        type(ewoc_part_type) :: none(0)
        type(vc_fileset_type) :: fs
        type(string_type) :: nofiles(0)
        type(handle_type) :: buffer, pbuf
        type(status_type) :: st
        integer :: backend

        status = status_type()

        if (vc_directory_busy()) then
            call set_error(status, GENERIC_ERROR_CODE, "Another update process is in progress, cannot run two at a time")
            return
        end if

        def_directory = buffers(current)%directory
        backend = vc_directory_backend()

        if (vc_directory_save_some_buffers_on_revert) then
            fs%backend = backend
            call append_string(fs%files, def_directory)
            call vc_buffer_synchronize_fileset(fs, status, .true.)
            if (.not. status%ok()) return
        end if

        call vc_set_mode_line_busy_indicator()
        ! Create a buffer that can be used by `dir-status' and call
        ! `dir-status' with this buffer as the current buffer.  Use
        ! `vc-dir-process-buffer' to remember this buffer, so that
        ! it can be used later to kill the update process in case it
        ! takes too long.
        call ensure_process_buffer(backend, pbuf)
        ! set the needs-update flag on all non-directory entries
        call ewoc_map(set_needs_update, ew())
        ! Bzr has serious locking problems, so setup the headers first (this is
        ! synchronous) rather than doing it while dir-status is running.
        call vc_directory_headers(backend, def_directory, header)
        call ewoc_set_hf_parts(ew(), header, none)
        buffer = current_buffer()
        call set_buffer(pbuf, st)
        buffers(current)%directory = def_directory
        call erase_buffer(st)

        if (associated(vc_backends(backend)%directory_status_files)) then
            call vc_backends(backend)%directory_status_files(def_directory, nofiles, refresh_update, buffer, status)
        end if

        call set_buffer(buffer, st)
    end subroutine vc_directory_refresh

    !> Insert an entry for a specific file into the current *VC-Dir* listing.
    !> This is typically used if the file is up-to-date (or has been added
    !> outside of VC) and one wants to do some operation on it.  Port of
    !> vc-dir.el:vc-dir-show-fileentry.
    recursive subroutine vc_directory_show_fileentry(file)
        character(len=*), intent(in) :: file
        type(vc_directory_entry_type) :: entry(1)

        entry(1)%name = file_relative_name(file, buffers(current)%directory)
        entry(1)%state = vc_state(expand(file))
        call vc_directory_update(entry, current_buffer())
    end subroutine vc_directory_show_fileentry

    !> Hide items that are in STATE from display.
    !> See `vc_state' for valid values of STATE.
    !>
    !> If STATE is absent, hide both `up-to-date' and `ignored' items.
    !>
    !> Interactively, if `current_prefix_argument' is true, set STATE to
    !> state of item at point, if any.  Port of vc-dir.el:vc-dir-hide-state.
    recursive subroutine vc_directory_hide_state(state)
        integer, intent(in), optional :: state
        type(value_type) :: saved
        integer(int64) :: id
        integer :: crt, first, next, previous
        logical :: remove

        if (present(state)) then
            call message("Hiding items in state """//vc_state_name(state)//"""")
        else
            call message("Hiding up-to-date and ignored items")
        end if

        crt = ewoc_nth(ew(), -1)
        first = ewoc_nth(ew(), 0)

        ! Go over from the last item to the first and remove the
        ! up-to-date files and directories with no child files.
        do while (crt /= first .and. crt /= 0)
            id = did(crt)
            next = ewoc_next(ew(), crt)
            previous = ewoc_previous(ew(), crt)
            ! ewoc-delete does not work without this...
            call bind_inhibit_read_only(saved)
            remove = .false.

            if (allocated(infos(id)%directory)) then
                ! Remove directories with no child files.
                ! Nothing follows this directory.
                remove = next == 0
                ! Next item is a directory.
                if (.not. remove) remove = allocated(infos(did(next))%directory)
            end if

            if (.not. remove) then
                ! Remove files in specified STATE.  STATE can be a
                ! symbol, a user-name, or nil.
                if (present(state)) then
                    remove = infos(id)%entry%state == state
                else
                    remove = up_to_date_state(infos(id)%entry%state)
                end if
            end if

            if (remove) call delete_nodes([crt])
            call restore_inhibit_read_only(saved)
            crt = previous
        end do
    end subroutine vc_directory_hide_state

    !> Remove the current line from display.  Port of
    !> vc-dir.el:vc-dir-kill-line.
    recursive subroutine vc_directory_remove_line(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        integer :: crt

        status = status_type()
        crt = ewoc_locate(ew())
        if (crt == 0) return
        call bind_inhibit_read_only(saved)
        call delete_nodes([crt])
        call restore_inhibit_read_only(saved)
    end subroutine vc_directory_remove_line

    ! The pretty-printer of the ewoc.  Port of vc-dir.el:vc-dir-printer.
    recursive subroutine vc_directory_printer(data)
        integer(int64), intent(in) :: data
        integer :: b

        b = vc_directory_backend()

        if (b > 0) then
            if (associated(vc_backends(b)%directory_printer)) then
                call vc_backends(b)%directory_printer(infos(data))
                return
            end if
        end if

        call vc_default_directory_printer(infos(data))
    end subroutine vc_directory_printer

    ! Call `vc_only_files_state_and_model' as appropriate for VC-Dir buffers.
    ! Offer to call `vc_directory_hide_up_to_date' if that might be useful.
    ! If we did, remove up-to-date items from ONLY_FILES_LIST before passing
    ! to `vc_only_files_state_and_model'.
    !
    ! There's usually no action to be taken on `up-to-date' or `ignored'
    ! files, but a new user may include these in their VC-Dir fileset without
    ! realizing it.  To avoid the user having to know in advance that what
    ! they must do is invoke \<vc_directory_mode_map>\[vc_directory_hide_up_to_date]
    ! before \[vc_next_action], offer to invoke `vc_directory_hide_up_to_date'
    ! for them, and also filter ONLY_FILES_LIST so as not to include entries
    ! in those states.  Only do this if there are both up-to-date and
    ! non-up-to-date files in ONLY_FILES_LIST (in case we add a VC next action
    ! for `up-to-date' and/or `ignored' files at some point).  Port of
    ! vc-dir.el:vc-dir--only-files-state-and-model.
    recursive subroutine vc_directory_only_files_state_and_model(files, states, fs, status)
        type(string_type), intent(in) :: files(:)
        integer, intent(in) :: states(:)
        type(vc_fileset_type), intent(inout) :: fs
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: other(:)
        integer, allocatable :: other_states(:)
        integer :: k, up_to_date_count
        logical :: yes

        status = status_type()
        allocate (other(0), other_states(0))
        up_to_date_count = 0

        do k = 1, size(files)
            if (up_to_date_state(states(k))) then
                up_to_date_count = up_to_date_count + 1
            else
                call append_file_state(other, other_states, files(k)%text, states(k))
            end if
        end do

        yes = .false.

        if (up_to_date_count > 0 .and. size(other) > 0) then
            call read_y_or_n("Clear up-to-date items before proceeding?", yes, status)
            if (.not. status%ok()) return
        end if

        if (yes) then
            call vc_directory_hide_state()
            call vc_only_files_state_and_model(other, other_states, vc_directory_backend(), fs, status)
        else
            call vc_only_files_state_and_model(files, states, vc_directory_backend(), fs, status)
        end if
    end subroutine vc_directory_only_files_state_and_model

    !> The fileset of a VC-Dir buffer: the marked files, or the file at
    !> point.  With STATE_MODEL_ONLY_FILES, also their states and model.
    !> Port of vc-dir.el:vc-dir-deduce-fileset.
    recursive subroutine vc_directory_deduce_fileset(fs, status, state_model_only_files)
        type(vc_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: state_model_only_files
        type(string_type), allocatable :: marked(:), only_files(:)
        integer, allocatable :: states(:)
        character(len=:), allocatable :: file
        logical :: smo

        status = status_type()
        smo = .false.
        if (present(state_model_only_files)) smo = state_model_only_files
        call vc_directory_marked_files(marked)

        if (size(marked) > 0) then
            fs%files = marked
            if (smo) call vc_directory_marked_only_files_and_states(only_files, states)
        else
            call vc_directory_current_file(file, status)
            if (.not. status%ok()) return
            call append_string(fs%files, file)
            if (smo) call vc_directory_child_files_and_states(only_files, states)
        end if

        fs%backend = vc_directory_backend()
        if (smo) call vc_directory_only_files_state_and_model(only_files, states, fs, status)
    end subroutine vc_directory_deduce_fileset

    !> Run `vc_directory' in the repository root directory without prompt.
    !> If the default directory of the current buffer is
    !> not under version control, prompt for a directory.  Port of
    !> vc-dir.el:vc-dir-root.
    recursive subroutine vc_directory_root(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root
        logical :: found

        call vc_root_directory(root, found, status)
        if (.not. status%ok()) return

        if (found) then
            call vc_directory(root, status)
        else
            call execute_vc_directory(status)
        end if
    end subroutine vc_directory_root

    !> Show the VC status for "interesting" files in and below DIRECTORY.
    !> This allows you to mark files and perform VC operations on them.
    !> The list omits files which are up to date, with no changes in your copy
    !> or the repository, if there is nothing in particular to say about them.
    !>
    !> Preparing the list of file status takes time; when the buffer
    !> first appears, it has only the first few lines of summary information.
    !> The file lines appear later.
    !>
    !> Optional second argument BACKEND specifies the VC backend to use.
    !> Interactively, a prefix argument means to ask for the backend.
    !>
    !> These are the commands available for use in the file status buffer:
    !>
    !> \{vc_directory_mode_map}  Port of vc-dir.el:vc-dir.
    recursive subroutine vc_directory(directory, status, backend)
        use window_mod, only: pop_to_buffer
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: backend
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer :: b, saved

        b = 0
        if (present(backend)) b = backend

        if (b == 0) then
            b = vc_responsible_backend(directory, status)
            if (.not. status%ok()) return
        end if

        ! (let ((pop-up-windows)) ...): based on cvs-examine; bug#6204.
        call vc_directory_prepare_status_buffer("*vc-dir*", directory, b, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)

        if (is_derived_mode("vc_directory_mode")) then
            call vc_directory_refresh(status)
        else
            ! FIXME: find a better way to pass the backend to `vc-dir-mode'.
            saved = use_vc_backend
            use_vc_backend = b
            call vc_directory_mode(status)
            use_vc_backend = saved
            ! (Activate the backend-specific minor mode, if any: deferred.)
        end if

        call set_buffer(old, st)
        if (.not. status%ok()) return
        call pop_to_buffer(buffer, status)
    end subroutine vc_directory

    !> The default `directory_extra_headers': a reminder to add backend specific
    !> headers.  Port of vc-dir.el:vc-default-dir-extra-headers.
    recursive subroutine vc_default_directory_extra_headers(directory, parts)
        character(len=*), intent(in) :: directory
        type(ewoc_part_type), allocatable, intent(out) :: parts(:)

        ! Be loud by default to remind people to add code to display
        ! backend specific headers.
        ! XXX: change this to return nil before the release.
        allocate (parts(2))
        parts(1)%text = "Extra      : "
        parts(1)%face = "vc_directory_header"
        parts(2)%text = "Please add backend specific headers here.  It's easy!"
        if (len(directory) < 0) parts(2)%text = ""
        parts(2)%face = "vc_directory_status_warning"
    end subroutine vc_default_directory_extra_headers

    !> Pretty print FILEENTRY.  Port of vc-dir.el:vc-default-dir-printer
    !> (without the mouse maps and help echoes).
    recursive subroutine vc_default_directory_printer(fileentry)
        type(vc_directory_fileinfo_type), intent(in) :: fileentry
        character(len=:), allocatable :: display_state, face
        character(len=20) :: padded
        logical :: isdir
        integer :: state

        ! If you change the layout here, change vc-dir-move-to-goal-column.
        ! VC backends can implement backend specific versions of this
        ! function.  Changes here might need to be reflected in the
        ! vc-BACKEND-dir-printer functions.
        isdir = allocated(fileentry%directory)
        state = fileentry%entry%state

        if (isdir) then
            display_state = ""
        else if (allocated(fileentry%display_state)) then
            display_state = fileentry%display_state
        else
            display_state = vc_state_name(state)
        end if

        if (fileentry%marked) then
            call insert_with_face("*", "vc_directory_mark_indicator")
        else
            call insert_with_face(" ", "vc_directory_mark_indicator")
        end if

        call insert_with_face("   ", "")

        if (display_state == "up-to-date") then
            face = "vc_directory_status_up_to_date"
        else if (display_state == "missing" .or. display_state == "conflict" .or. display_state == "needs-update" &
                 .or. display_state == "unlocked-changes" .or. display_state == "committing") then
            face = "vc_directory_status_warning"
        else if (display_state == "ignored") then
            face = "vc_directory_status_ignored"
        else
            face = "vc_directory_status_edited"
        end if

        if (len(display_state) >= 20) then
            call insert_with_face(display_state, face)
        else
            padded = display_state
            call insert_with_face(padded, face)
        end if

        call insert_with_face(" ", "")

        if (isdir) then
            call insert_with_face(fileentry%entry%name, "vc_directory_directory")
        else
            call insert_with_face(fileentry%entry%name, "vc_directory_file")
        end if
    end subroutine vc_default_directory_printer

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! Not a port.
    integer function count_argument()
        count_argument = int(prefix_numeric_value(current_prefix_argument))
    end function count_argument

    recursive subroutine execute_vc_directory(status)
        use minibuffer_mod, only: read_file_name
        use minibuffer_mod, only: REQUIRE_MATCH_YES
        use version_control_hooks_mod, only: vc_backend_named
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, directory, truename, name
        logical :: found
        integer :: b

        call vc_root_directory(root, found, status)
        if (.not. status%ok()) return
        if (.not. found) root = buffers(current)%directory
        ! When you hit C-x v d in a visited VC file,
        ! the *vc-dir* buffer visits the directory under its truename;
        ! therefore it makes sense to always do that.
        ! Otherwise if you do C-x v d -> C-x C-f -> C-x v d
        ! you may get a new *vc-dir* buffer, different from the original
        call read_file_name("VC status for directory: ", directory, status, directory=root, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call file_truename(directory, truename, status)
        if (.not. status%ok()) return
        b = 0

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call read_backend(name, status)
            if (.not. status%ok()) return
            b = vc_backend_named(name)
        end if

        call vc_directory(truename, status, b)
    end subroutine execute_vc_directory

    ! (completing-read "Use VC backend: " vc-handled-backends nil t).
    ! Not a port.
    recursive subroutine read_backend(name, status)
        use minibuffer_reading_mod, only: completion_list_table
        use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
        use value_mod, only: make_list, value_type
        use version_control_hooks_mod, only: vc_handled_backends
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        integer :: k

        allocate (names(size(vc_handled_backends)))

        do k = 1, size(vc_handled_backends)
            names(k) = value_type(text=trim(vc_handled_backends(k)))
        end do

        call completing_read("Use VC backend: ", completion_list_table(make_list(names)), name, status, &
                             require_match=REQUIRE_MATCH_YES)
    end subroutine read_backend

    recursive subroutine execute_vc_directory_next_line(status)
        type(status_type), intent(out) :: status

        call vc_directory_next_line(count_argument(), status)
    end subroutine execute_vc_directory_next_line

    recursive subroutine execute_vc_directory_previous_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call vc_directory_previous_line(count_argument())
    end subroutine execute_vc_directory_previous_line

    recursive subroutine execute_vc_directory_next_directory(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call vc_directory_next_directory()
    end subroutine execute_vc_directory_next_directory

    recursive subroutine execute_vc_directory_previous_directory(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call vc_directory_previous_directory()
    end subroutine execute_vc_directory_previous_directory

    recursive subroutine execute_vc_directory_mark_all_files(status)
        type(status_type), intent(out) :: status

        call vc_directory_mark_all_files(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_vc_directory_mark_all_files

    recursive subroutine execute_vc_directory_unmark_all_files(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call vc_directory_unmark_all_files(current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_vc_directory_unmark_all_files

    recursive subroutine execute_vc_directory_mark_by_regular_expression(status)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression

        call read_string("Mark files matching: ", regular_expression, status)
        if (status%ok()) call vc_directory_mark_by_regular_expression(regular_expression, &
                                                                      current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_vc_directory_mark_by_regular_expression

    recursive subroutine execute_vc_directory_ignore(status)
        type(status_type), intent(out) :: status

        call vc_directory_ignore(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_vc_directory_ignore

    recursive subroutine execute_vc_directory_hide_up_to_date(status)
        type(status_type), intent(out) :: status
        integer :: node

        status = status_type()

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            ! Command is prefixed.  Infer STATE from point.
            node = ewoc_locate(ew())

            if (node /= 0) then
                call vc_directory_hide_state(infos(did(node))%entry%state)
                return
            end if
        end if

        call vc_directory_hide_state()
    end subroutine execute_vc_directory_hide_up_to_date

    recursive subroutine execute_vc_directory_show_fileentry(status)
        use minibuffer_mod, only: read_file_name
        use minibuffer_mod, only: REQUIRE_MATCH_YES
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Show file: ", file, status, mustmatch=REQUIRE_MATCH_YES)
        if (status%ok()) call vc_directory_show_fileentry(file)
    end subroutine execute_vc_directory_show_fileentry

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => vc_directory_header
        call f%define("vc_directory_header", "Face for headers in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_type_face"))
        f => vc_directory_header_value
        call f%define("vc_directory_header_value", "Face for header values in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_variable_name_face"))
        f => vc_directory_header_urgent_value
        call f%define("vc_directory_header_urgent_value", "Face for urgent header values in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_warning_face"))
        f => vc_directory_directory
        call f%define("vc_directory_directory", "Face for directories in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_comment_delimiter_face"))
        f => vc_directory_file
        call f%define("vc_directory_file", "Face for files in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_function_name_face"))
        f => vc_directory_mark_indicator
        call f%define("vc_directory_mark_indicator", "Face for mark indicators in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_type_face"))
        f => vc_directory_status_warning
        call f%define("vc_directory_status_warning", "Face for warning status in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_warning_face"))
        f => vc_directory_status_edited
        call f%define("vc_directory_status_edited", "Face for edited status in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_variable_name_face"))
        f => vc_directory_status_up_to_date
        call f%define("vc_directory_status_up_to_date", "Face for up-to-date status in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_builtin_face"))
        f => vc_directory_status_ignored
        call f%define("vc_directory_status_ignored", "Face for ignored or empty values in VC-Dir buffers.")
        call f%specification_entry(display_when(), face_attributes(inherit="shadow"))
    end subroutine define_faces

    !> Define VC-Dir mode, its keymap and faces, and register the commands.
    !> Not a port.
    subroutine initialize_vc_directory()
        use value_mod, only: make_keymap_reference, value_type
        type(handle_type) :: regular_expression_map, mark_map
        type(status_type) :: st

        vc_directory_resynch_file => vc_directory_resynch_file_impl
        vc_directory_refresh_function => vc_directory_refresh
        commands%vc_directory%execute => execute_vc_directory
        commands%vc_directory_root%execute => vc_directory_root
        if (associated(major_modes%vc_directory_mode%execute)) return
        call define_faces()
        call major_modes%vc_directory_mode%define("vc_directory_mode", "VC dir", vc_directory_mode, vc_directory_mode_hook, &
                                                  parent=major_modes%special_mode)
        vc_directory_mode_map = major_modes%vc_directory_mode%map
        commands%vc_directory_mode%execute => vc_directory_mode
        commands%vc_directory_mark%execute => vc_directory_mark
        commands%vc_directory_delete_file%execute => vc_directory_delete_file
        commands%vc_directory_mark_all_files%execute => execute_vc_directory_mark_all_files
        commands%vc_directory_unmark%execute => vc_directory_unmark
        commands%vc_directory_unmark_all_files%execute => execute_vc_directory_unmark_all_files
        commands%vc_directory_unmark_file_up%execute => vc_directory_unmark_file_up
        commands%vc_directory_next_line%execute => execute_vc_directory_next_line
        commands%vc_directory_previous_line%execute => execute_vc_directory_previous_line
        commands%vc_directory_next_directory%execute => execute_vc_directory_next_directory
        commands%vc_directory_previous_directory%execute => execute_vc_directory_previous_directory
        commands%vc_directory_find_file%execute => vc_directory_find_file
        commands%vc_directory_find_file_other_window%execute => vc_directory_find_file_other_window
        commands%vc_directory_display_file%execute => vc_directory_display_file
        commands%vc_directory_view_file%execute => vc_directory_view_file
        commands%vc_directory_stop_directory_status_process%execute => vc_directory_stop_directory_status_process
        commands%vc_directory_hide_up_to_date%execute => execute_vc_directory_hide_up_to_date
        commands%vc_directory_remove_line%execute => vc_directory_remove_line
        commands%vc_directory_ignore%execute => execute_vc_directory_ignore
        commands%vc_directory_root_next_action%execute => vc_directory_root_next_action
        commands%vc_directory_mark_by_regular_expression%execute => execute_vc_directory_mark_by_regular_expression
        commands%vc_directory_mark_registered_files%execute => vc_directory_mark_registered_files
        commands%vc_directory_mark_unregistered_files%execute => vc_directory_mark_unregistered_files
        commands%vc_directory_refresh%execute => vc_directory_refresh
        commands%vc_directory_show_fileentry%execute => execute_vc_directory_show_fileentry
        ! VC commands
        call keymap_set(vc_directory_mode_map, "v", "vc_next_action", st)    ! C-x v v
        call keymap_set(vc_directory_mode_map, "=", "vc_diff", st)           ! C-x v =
        call keymap_set(vc_directory_mode_map, "D", "vc_root_diff", st)      ! C-x v D
        call keymap_set(vc_directory_mode_map, "i", "vc_register", st)       ! C-x v i
        call keymap_set(vc_directory_mode_map, "+", "vc_pull", st)           ! C-x v +
        call keymap_set(vc_directory_mode_map, "P", "vc_push", st)           ! C-x v P
        call keymap_set(vc_directory_mode_map, "l", "vc_print_log", st)      ! C-x v l
        call keymap_set(vc_directory_mode_map, "L", "vc_print_root_log", st) ! C-x v L
        call keymap_set(vc_directory_mode_map, "I", "vc_root_log_incoming", st) ! C-x v I
        call keymap_set(vc_directory_mode_map, "O", "vc_root_log_outgoing", st) ! C-x v O
        ! Marking.
        call keymap_set(vc_directory_mode_map, "m", "vc_directory_mark", st)
        call keymap_set(vc_directory_mode_map, "d", "vc_directory_delete_file", st)
        call keymap_set(vc_directory_mode_map, "M", "vc_directory_mark_all_files", st)
        call keymap_set(vc_directory_mode_map, "u", "vc_directory_unmark", st)
        call keymap_set(vc_directory_mode_map, "U", "vc_directory_unmark_all_files", st)
        call keymap_set(vc_directory_mode_map, "DEL", "vc_directory_unmark_file_up", st)
        call keymap_set(vc_directory_mode_map, "M-DEL", "vc_directory_unmark_all_files", st)
        ! Movement.
        call keymap_set(vc_directory_mode_map, "n", "vc_directory_next_line", st)
        call keymap_set(vc_directory_mode_map, "SPC", "vc_directory_next_line", st)
        call keymap_set(vc_directory_mode_map, "TAB", "vc_directory_next_directory", st)
        call keymap_set(vc_directory_mode_map, "p", "vc_directory_previous_line", st)
        call keymap_set(vc_directory_mode_map, "S-SPC", "vc_directory_previous_line", st)
        call keymap_set(vc_directory_mode_map, "<backtab>", "vc_directory_previous_directory", st)
        ! Rebind paragraph-movement commands.
        call keymap_set(vc_directory_mode_map, "M-}", "vc_directory_next_directory", st)
        call keymap_set(vc_directory_mode_map, "M-{", "vc_directory_previous_directory", st)
        call keymap_set(vc_directory_mode_map, "C-<down>", "vc_directory_next_directory", st)
        call keymap_set(vc_directory_mode_map, "C-<up>", "vc_directory_previous_directory", st)
        ! The remainder.
        call keymap_set(vc_directory_mode_map, "f", "vc_directory_find_file", st)
        call keymap_set(vc_directory_mode_map, "e", "vc_directory_find_file", st) ! dired-mode compatibility
        call keymap_set(vc_directory_mode_map, "RET", "vc_directory_find_file", st)
        call keymap_set(vc_directory_mode_map, "o", "vc_directory_find_file_other_window", st)
        call keymap_set(vc_directory_mode_map, "C-o", "vc_directory_display_file", st)
        call keymap_set(vc_directory_mode_map, "C-c C-c", "vc_directory_stop_directory_status_process", st)
        call keymap_set(vc_directory_mode_map, "x", "vc_directory_hide_up_to_date", st)
        call keymap_set(vc_directory_mode_map, "C-k", "vc_directory_remove_line", st)
        call keymap_set(vc_directory_mode_map, "G", "vc_directory_ignore", st)
        call keymap_set(vc_directory_mode_map, "@", "vc_revert", st)
        call keymap_set(vc_directory_mode_map, "E L", "vc_root_log_outgoing", st)
        call keymap_set(vc_directory_mode_map, "V", "vc_directory_root_next_action", st)
        call keymap_set(vc_directory_mode_map, "b c", "vc_create_branch", st)
        call keymap_set(vc_directory_mode_map, "b l", "vc_print_fileset_branch_log", st)
        call keymap_set(vc_directory_mode_map, "b L", "vc_print_root_branch_log", st)
        call keymap_set(vc_directory_mode_map, "b s", "vc_switch_branch", st)
        call make_sparse_keymap(regular_expression_map)
        call keymap_set(vc_directory_mode_map, "%", make_keymap_reference(regular_expression_map), st)
        call keymap_set(regular_expression_map, "m", "vc_directory_mark_by_regular_expression", st)
        call make_sparse_keymap(mark_map)
        call keymap_set(vc_directory_mode_map, "*", make_keymap_reference(mark_map), st)
        call keymap_set(mark_map, "%", "vc_directory_mark_by_regular_expression", st)
        call keymap_set(mark_map, "r", "vc_directory_mark_registered_files", st)
        call append_locals_clearer(clear_vc_directory_locals)
    end subroutine initialize_vc_directory
end module version_control_directory_mod
