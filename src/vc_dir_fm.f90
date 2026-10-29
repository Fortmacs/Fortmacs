! vc_dir_fm.f90 -- directory status display under VC.
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
!> below a directory, as an ewoc of `vc_dir_fileinfo_type' entries the
!> backend's `dir_status_files' computes asynchronously.  The files can
!> be marked, and the VC commands act on the marked files (or the file at
!> point).
!>
!> Deferred: `vc_dir_search', `vc_dir_query_replace_regexp',
!> `vc_dir_isearch' and `vc_dir_isearch_regexp' (fileloop.el and
!> multi-isearch), the menu, the tool bar, the mouse maps,
!> `vc_dir_clean_files' (`map-y-or-n-p' over files), the keys of the
!> commands not ported yet (pull, push, incoming and outgoing logs,
!> branches, unintegrated changes), the outgoing count header
!> (`vc_dir_show_outgoing_count'), `vc_dir_kill_query' (needing
!> `kill_buffer_query_functions'), the backend's VC-Dir minor mode
!> (vc-dir-git-mode), the `display-state' of files being committed
!> asynchronously, desktop and bookmark support, project buffers and
!> directory-local variables.
module vc_dir_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, t_value, nil, nilp
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set, add_local_variables_killer
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, buffer_live_p, buffer_serial_live, erase_buffer, &
                         get_buffer_create, generate_new_buffer_name, kill_buffer, deactivate_mark_variable
    use commands_fm, only: commands
    use modes_fm, only: major_modes
    use derived_fm, only: derived_mode_p
    use editfns_fm, only: point, goto_char, point_min, eolp, region_beginning, region_end
    use insdel_fm, only: insert
    use cmds_fm, only: beginning_of_line, forward_char
    use textprop_fm, only: put_text_property
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use simple_fm, only: use_region_p, line_number_at_pos
    use subr_fm, only: y_or_n_p, string_match_p
    use xdisp_fm, only: message, clear_mode_line_process
    use faces_fm, only: named_face_type, display_when, face_attrs
    use keymap_fm, only: keymap_set, make_sparse_keymap
    use fileio_fm, only: expand_file_name, file_name_directory, file_name_as_directory, directory_file_name, &
                         file_directory_p, file_exists_p
    use files_fm, only: abbreviate_file_name, create_file_buffer, find_file, find_file_other_window, &
                        find_file_noselect, revert_buffer_function, file_truename, file_in_directory_p, file_relative_name
    use process_fm, only: get_buffer_process, delete_process
    use ewoc_fm, only: ewoc_type, ewoc_part_type, ewoc_create, ewoc_data, ewoc_set_data, ewoc_enter_last, &
                       ewoc_enter_before, ewoc_next, ewoc_prev, ewoc_nth, ewoc_map, ewoc_delete, ewoc_filter, &
                       ewoc_locate, ewoc_invalidate, ewoc_goto_prev, ewoc_goto_next, ewoc_goto_node, ewoc_collect, &
                       ewoc_set_hf_parts
    use vc_hooks_fm, only: vc_string_type, vc_backends, vc_dir_entry_type, vc_dir_fileinfo_type, vc_dir_buffers, &
                           vc_dir_resynch_file, vc_state, vc_state_name, vc_backend_name, vc_file_set_backend, &
                           VC_STATE_NIL, VC_UP_TO_DATE, VC_IGNORED, VC_UNREGISTERED, VC_EDITED, VC_ADDED, VC_REMOVED, &
                           VC_MISSING, VC_CONFLICT, VC_NEEDS_UPDATE, VC_UNLOCKED_CHANGES, VC_BACKEND_NONE
    use vc_dispatcher_fm, only: vc_setup_buffer, set_vc_parent_buffer, vc_parent_buffer, vc_set_mode_line_busy_indicator
    use vc_fm, only: vc_fileset_type, vc_responsible_backend, vc_root_dir, vc_only_files_state_and_model, vc_ignore, &
                     vc_ignore_base_dir, vc_delete_file, vc_buffer_sync_fileset, vc_next_action, set_vc_buffer_overriding_fileset
    implicit none(type, external)
    private

    public :: vc_dir, vc_dir_root, vc_dir_mode, vc_dir_update, vc_dir_refresh, vc_dir_refresh_files, &
              vc_dir_next_line, vc_dir_previous_line, vc_dir_next_directory, vc_dir_previous_directory, &
              vc_dir_mark, vc_dir_mark_file, vc_dir_mark_all_files, vc_dir_mark_by_regexp, vc_dir_mark_files, &
              vc_dir_mark_state_files, vc_dir_mark_registered_files, vc_dir_mark_unregistered_files, vc_dir_unmark, &
              vc_dir_unmark_file, vc_dir_unmark_file_up, vc_dir_unmark_all_files, vc_dir_toggle_mark_file, &
              vc_dir_root_next_action, vc_dir_delete_file, vc_dir_find_file, vc_dir_find_file_other_window, &
              vc_dir_display_file, vc_dir_view_file, vc_dir_ignore, vc_dir_current_file, vc_dir_marked_files, &
              vc_dir_marked_only_files_and_states, vc_dir_child_files_and_states, vc_dir_resynch_file_impl, &
              vc_dir_show_fileentry, vc_dir_hide_state, vc_dir_kill_line, vc_dir_deduce_fileset, vc_dir_backend, &
              vc_dir_busy, vc_dir_kill_dir_status_process, vc_dir_headers, vc_default_dir_printer, &
              vc_default_dir_extra_headers, vc_dir_ewoc, vc_dir_info, vc_dir_node_directory, syms_of_vc_dir, &
              insert_with_face

    !> Values of `vc_dir_allow_mass_mark_changes'.
    integer, parameter, public :: VC_DIR_MASS_MARK_NIL = 0, VC_DIR_MASS_MARK_ASK = 1, VC_DIR_MASS_MARK_T = 2

    !> Values of `vc_dir_auto_hide_up_to_date'.
    integer, parameter, public :: VC_DIR_AUTO_HIDE_NIL = 0, VC_DIR_AUTO_HIDE_T = 1, VC_DIR_AUTO_HIDE_REVERT = 2

    !> Normal hook run by `vc_dir_mode'.
    !> See `run_mode_hooks'.  Port of vc-dir.el vc-dir-mode-hook.
    type(hook_type), target, public :: vc_dir_mode_hook = hook_type(name="vc_dir_mode_hook")

    !> Hook run after a refresh of the VC-Dir buffer completes (the hook
    !> `vc_dir_refresh' runs).
    type(hook_type), target, public :: vc_dir_refresh_hook = hook_type(name="vc_dir_refresh_hook")

    !> If true, VC-Dir commands may mark or unmark many items at once.
    !>
    !> When a directory in VC-Dir is marked, then for most VCS, this means that
    !> all files within it are implicitly marked as well.
    !> For consistency, the mark and unmark commands
    !> (principally \<vc_dir_mode_map>\[vc_dir_mark] and \[vc_dir_unmark]) will
    !> not explicitly mark or unmark entries if doing so would result in a
    !> situation where both a directory and a file or directory within it are
    !> both marked.
    !>
    !> With the default value of this variable, VC_DIR_MASS_MARK_ASK, if you
    !> attempt to mark or unmark a particular item and doing so consistent
    !> with these restrictions would require other items to be marked or
    !> unmarked too, Fortmacs will prompt you to confirm that you do mean
    !> for the other items to be marked or unmarked.
    !>
    !> If this variable is VC_DIR_MASS_MARK_NIL, the commands will refuse to
    !> do anything if they would need to mark or unmark other entries too.
    !> If this variable is VC_DIR_MASS_MARK_T, the commands will always
    !> proceed to mark and unmark other entries, without asking.  Port of
    !> vc-dir.el vc-dir-allow-mass-mark-changes.
    integer, public :: vc_dir_allow_mass_mark_changes = VC_DIR_MASS_MARK_ASK

    !> Whether VC-Dir auto-removes `up-to-date'/`ignored' files from display.
    !>
    !> If the value is VC_DIR_AUTO_HIDE_NIL, files shown in the VC-Dir buffer
    !> will remain on display if they become `up-to-date' or `ignored'.
    !> If the value is VC_DIR_AUTO_HIDE_T, files are automatically removed
    !> from display when they become `up-to-date' or `ignored'.
    !> If the value is VC_DIR_AUTO_HIDE_REVERT, any displayed files that
    !> are `up-to-date' or `ignored' are removed from display
    !> by \<vc_dir_mode_map>\[revert_buffer], but they are not automatically removed
    !> when they become `up-to-date' or `ignored'.  That is,
    !> refreshing the VC-Dir buffer hides `up-to-date' and `ignored'
    !> files when the value is VC_DIR_AUTO_HIDE_REVERT.
    !>
    !> VC-Dir never shows `up-to-date' and `ignored' files when the
    !> directory is first displayed.
    !>
    !> You can still use `vc_dir_show_fileentry' to manually add an entry for
    !> an `up-to-date' or `ignored' file.  Port of vc-dir.el
    !> vc-dir-auto-hide-up-to-date.
    integer, public :: vc_dir_auto_hide_up_to_date = VC_DIR_AUTO_HIDE_NIL

    !> If true, first offer to save relevant buffers when refreshing VC-Dir.
    !> Port of vc-dir.el vc-dir-save-some-buffers-on-revert.
    logical, public :: vc_dir_save_some_buffers_on_revert = .false.

    !> The keymap of `vc_dir_mode'.
    type(handle_type), public :: vc_dir_mode_map

    type(named_face_type), target, public :: vc_dir_header, vc_dir_header_value, vc_dir_header_urgent_value, &
                                             vc_dir_directory, vc_dir_file, vc_dir_mark_indicator, &
                                             vc_dir_status_warning, vc_dir_status_edited, vc_dir_status_up_to_date, &
                                             vc_dir_status_ignored

    ! The file information of the ewocs, by the data of their nodes.
    type(vc_dir_fileinfo_type), allocatable :: infos(:)
    logical, allocatable :: info_live(:)

    ! A VC-Dir buffer's `vc_ewoc', `vc_dir_process_buffer' and
    ! `vc_dir_backend', by serial number.
    type :: local_type
        integer(int64) :: serial = 0
        logical :: has_ewoc = .false.
        type(ewoc_type) :: ewoc
        type(handle_type) :: process_buffer
        integer :: backend = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! The backend `vc_dir_mode' gives the buffer (use-vc-backend,
    ! dynamically bound by `vc_dir').
    integer :: use_vc_backend = 0

    ! `vc_dir_allow_mass_mark_changes' as bound by `vc_dir_mark_unmark'.
    logical :: mass_mark_bound_nil = .false.

    ! The arguments of the `ewoc_map' and `ewoc_collect' predicates
    ! (closures upstream).
    integer :: map_state = 0
    logical :: map_unmark = .false.
    character(len=:), allocatable :: map_regexp
    type(vc_string_type), allocatable :: map_files(:)
    integer, allocatable :: map_states(:)

    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values and the file information
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals', made if MAKE
    ! (0 if none).
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

    ! kill-all-local-variables: forget the current buffer's values.
    subroutine kill_vc_dir_locals()
        integer :: k
        k = local_index(.false.)
        if (k > 0) locals(k) = local_type()
    end subroutine kill_vc_dir_locals

    !> The current buffer's `vc_ewoc' (HAS false for nil).
    subroutine vc_dir_ewoc(ewoc, has)
        type(ewoc_type), intent(out) :: ewoc
        logical, intent(out) :: has
        integer :: k
        has = .false.
        k = local_index(.false.)
        if (k > 0) then
            has = locals(k)%has_ewoc
            ewoc = locals(k)%ewoc
        end if
    end subroutine vc_dir_ewoc

    ! The current buffer's ewoc (a stale handle if none).
    function ew() result(ewoc)
        type(ewoc_type) :: ewoc
        logical :: has
        call vc_dir_ewoc(ewoc, has)
    end function ew

    !> The current buffer's `vc_dir_backend': the backend used by the
    !> current *vc-dir* buffer (0 for nil).
    integer function vc_dir_backend()
        integer :: k
        vc_dir_backend = 0
        k = local_index(.false.)
        if (k > 0) vc_dir_backend = locals(k)%backend
    end function vc_dir_backend

    ! The current buffer's `vc_dir_process_buffer'.
    function process_buffer() result(h)
        type(handle_type) :: h
        integer :: k
        h = null_handle()
        k = local_index(.false.)
        if (k > 0) h = locals(k)%process_buffer
    end function process_buffer

    ! A new file information (vc-dir-create-fileinfo) from ENTRY, for a
    ! directory DIRECTORY if given: its data for the ewoc.
    integer(int64) function new_info(entry, directory) result(id)
        type(vc_dir_entry_type), intent(in) :: entry
        character(len=*), intent(in), optional :: directory
        type(vc_dir_fileinfo_type), allocatable :: grown(:)
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
        infos(id) = vc_dir_fileinfo_type()
        infos(id)%entry = entry
        if (present(directory)) infos(id)%directory = directory
        info_live(id) = .true.
    end function new_info

    ! A directory entry for NAME (vc-dir-create-fileinfo NAME nil nil nil
    ! DIRECTORY).
    integer(int64) function new_dir_info(name, directory) result(id)
        character(len=*), intent(in) :: name, directory
        type(vc_dir_entry_type) :: entry
        entry%name = name
        id = new_info(entry, directory)
    end function new_dir_info

    !> The file information of the ewoc's node NODE in the current buffer.
    function vc_dir_info(node) result(info)
        integer, intent(in) :: node
        type(vc_dir_fileinfo_type) :: info
        integer(int64) :: id
        id = ewoc_data(ew(), node)
        if (id > 0) info = infos(id)
    end function vc_dir_info

    ! The data of NODE.
    integer(int64) function did(node)
        integer, intent(in) :: node
        did = ewoc_data(ew(), node)
    end function did

    ! Delete NODES from the ewoc and forget their information.
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

    !> Insert TEXT with the face FACE (propertize).
    subroutine insert_with_face(text, face)
        character(len=*), intent(in) :: text, face
        integer(int64) :: beg
        type(status_type) :: st
        beg = point()
        call insert(text, st)
        if (len(face) > 0 .and. point() > beg) call put_text_property(beg, point(), sym("face"), sym(face), st)
    end subroutine insert_with_face

    ! (string< A B): A sorts before B (a prefix before the longer string).
    logical function string_lessp(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k
        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                string_lessp = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do
        string_lessp = len(a) < len(b)
    end function string_lessp

    ! (string-prefix-p PREFIX STRING).
    logical function string_prefix_p(prefix, string)
        character(len=*), intent(in) :: prefix, string
        string_prefix_p = .false.
        if (len(prefix) > len(string)) return
        string_prefix_p = string(1:len(prefix)) == prefix
    end function string_prefix_p

    ! (expand-file-name NAME): relative to the current buffer's directory.
    function expand(name) result(full)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: full
        type(status_type) :: st
        call expand_file_name(name, full, st, buffers(cur)%directory)
    end function expand

    ! (file-name-directory (directory-file-name (expand-file-name NAME))).
    function entry_directory(name) result(dir)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: dir
        dir = file_name_directory(directory_file_name(expand(name)))
    end function entry_directory

    ! (let ((inhibit-read-only t)) ...).
    subroutine bind_inhibit_read_only(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st
        call symbol_value(sym("inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = nil()
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine bind_inhibit_read_only

    subroutine restore_inhibit_read_only(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine restore_inhibit_read_only

    ! The value of `vc_dir_allow_mass_mark_changes' (nil while bound by
    ! `vc_dir_mark_unmark').
    integer function mass_mark()
        mass_mark = vc_dir_allow_mass_mark_changes
        if (mass_mark_bound_nil) mass_mark = VC_DIR_MASS_MARK_NIL
    end function mass_mark

    ! ------------------------------------------------------------------
    ! The buffer
    ! ------------------------------------------------------------------

    ! Used to keep the cursor on the file name column.  Port of
    ! vc-dir.el:vc-dir-move-to-goal-column.
    subroutine vc_dir_move_to_goal_column()
        type(status_type) :: st
        integer(int64) :: k
        call beginning_of_line()
        if (.not. eolp()) then
            ! Must be in sync with vc-default-dir-printer.
            do k = 1, 25
                call forward_char(st, 1_int64)
                if (.not. st%ok()) exit
            end do
        end if
    end subroutine vc_dir_move_to_goal_column

    ! Find a buffer named BNAME showing DIR, or create a new one.  Port of
    ! vc-dir.el:vc-dir-prepare-status-buffer.
    recursive subroutine vc_dir_prepare_status_buffer(bname, dir_in, backend, buf, status, create_new)
        character(len=*), intent(in) :: bname, dir_in
        integer, intent(in) :: backend
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: create_new
        character(len=:), allocatable :: dir, full
        type(handle_type) :: old
        type(status_type) :: st
        logical :: new
        integer :: k
        status = status_type()
        new = .false.
        if (present(create_new)) new = create_new
        call expand_file_name(dir_in, full, status, buffers(cur)%directory)
        if (.not. status%ok()) return
        dir = file_name_as_directory(full)
        buf = null_handle()
        old = current_buffer()
        ! Look for another buffer name BNAME visiting the same directory.
        if (.not. new .and. allocated(vc_dir_buffers)) then
            do k = 1, size(vc_dir_buffers)
                if (.not. buffer_live_p(vc_dir_buffers(k))) cycle
                call set_buffer(vc_dir_buffers(k), st)
                if (derived_mode_p("vc_dir_mode") .and. vc_dir_backend() == backend .and. &
                    buffers(cur)%directory == dir) then
                    buf = vc_dir_buffers(k)
                    exit
                end if
            end do
        end if
        call set_buffer(old, st)
        if (buf%index /= 0) return
        ! Create a new buffer named BNAME.
        ! We pass a filename to `create-file-buffer' because it is what
        ! the function expects, and also what uniquify needs (if active)
        call expand_file_name(bname, full, status, dir)
        if (.not. status%ok()) return
        call create_file_buffer(full, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, st)
        buffers(cur)%directory = dir
        call vc_setup_buffer(buf, status)
        ! Reset the vc-parent-buffer-name so that it does not appear
        ! in the mode-line.
        call set_vc_parent_buffer(vc_parent_buffer(), "")
        call set_buffer(old, st)
    end subroutine vc_dir_prepare_status_buffer

    !> Compute the directory for NODE.  Port of
    !> vc-dir.el:vc-dir-node-directory.
    function vc_dir_node_directory(node) result(dir)
        integer, intent(in) :: node
        character(len=:), allocatable :: dir
        integer(int64) :: id
        id = did(node)
        dir = ""
        if (id == 0) return
        ! If it's a directory node, get it from the node.
        if (allocated(infos(id)%directory)) then
            dir = infos(id)%directory
        else
            ! Otherwise compute it from the file name.
            dir = entry_directory(infos(id)%entry%name)
        end if
    end function vc_dir_node_directory

    ! Whether STATE is one of `vc_dir__up_to_date_states'.
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
    recursive subroutine vc_dir_update(entries_in, buffer, noinsert)
        type(vc_dir_entry_type), intent(in) :: entries_in(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in), optional :: noinsert
        type(vc_dir_entry_type), allocatable :: entries(:)
        type(vc_string_type), allocatable :: dirs(:)
        integer, allocatable :: order(:), to_remove(:)
        character(len=:), allocatable :: dotname, entryfile, entrydir, nodedir, nodefile, rd, prev_dir, lastdir
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        type(ewoc_type) :: e
        integer(int64) :: id
        logical :: noins, has
        integer :: n, k, j, i, node, prev_node, nremove, crt, first, prev, next, b
        noins = .false.
        if (present(noinsert)) noins = noinsert
        old = current_buffer()
        call set_buffer(buffer, st)
        call vc_dir_ewoc(e, has)
        if (.not. has) then
            call set_buffer(old, st)
            return
        end if
        ! Insert the entries sorted by name into the ewoc.
        ! We assume the ewoc is sorted too, which should be the
        ! case if we always add entries with vc-dir-update.
        n = size(entries_in)
        allocate (dirs(n), order(n))
        do k = 1, n
            dirs(k)%s = entry_directory(entries_in(k)%name)
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
        dotname = file_relative_name(buffers(cur)%directory, buffers(cur)%directory)
        ! Insert . if it is not present.
        if (node == 0) then
            node = ewoc_enter_last(e, new_dir_info(dotname, buffers(cur)%directory))
            node = ewoc_nth(e, 0)
        end if
        do while (k <= n .and. node /= 0)
            entryfile = entries(k)%name
            entrydir = entry_directory(entryfile)
            nodedir = vc_dir_node_directory(node)
            if (string_lessp(nodedir, entrydir)) then
                ! First try to find the directory.
                node = ewoc_next(e, node)
            else if (nodedir == entrydir) then
                ! Found the directory, find the place for the file name.
                nodefile = infos(did(node))%entry%name
                if (nodefile == dotname) then
                    node = ewoc_next(e, node)
                else if (string_lessp(nodefile, entryfile)) then
                    node = ewoc_next(e, node)
                else if (nodefile == entryfile) then
                    id = did(node)
                    if (entries(k)%state == VC_STATE_NIL .or. &
                        (vc_dir_auto_hide_up_to_date /= VC_DIR_AUTO_HIDE_NIL .and. &
                         vc_dir_auto_hide_up_to_date /= VC_DIR_AUTO_HIDE_REVERT .and. &
                         up_to_date_state(entries(k)%state))) then
                        to_remove = [node, to_remove]
                    else
                        infos(id)%entry%state = entries(k)%state
                        ! (vc--file-getinheprop nodefile 'display-state): only
                        ! asynchronous checkins set it (deferred).
                        if (allocated(infos(id)%display_state)) deallocate (infos(id)%display_state)
                        infos(id)%entry%has_extra = entries(k)%has_extra
                        infos(id)%entry%old_perm = entries(k)%old_perm
                        infos(id)%entry%new_perm = entries(k)%new_perm
                        infos(id)%entry%rename_state = entries(k)%rename_state
                        if (allocated(entries(k)%orig_name)) infos(id)%entry%orig_name = entries(k)%orig_name
                        infos(id)%needs_update = .false.
                        call ewoc_invalidate(e, [node])
                    end if
                    k = k + 1
                    node = ewoc_next(e, node)
                else
                    if (.not. noins) node = enter_before_keep(node, new_info(entries(k)))
                    k = k + 1
                end if
            else
                if (.not. noins) then
                    ! We might need to insert a directory node if the
                    ! previous node was in a different directory.
                    rd = file_relative_name(entrydir, buffers(cur)%directory)
                    prev_node = ewoc_prev(e, node)
                    prev_dir = ""
                    if (prev_node /= 0) prev_dir = vc_dir_node_directory(prev_node)
                    if (entrydir /= prev_dir) node = enter_before_keep(node, new_dir_info(rd, entrydir))
                    ! Now insert the node itself.
                    node = enter_before_keep(node, new_info(entries(k)))
                end if
                k = k + 1
            end if
        end do
        ! We're past the last node, all remaining entries go to the end.
        if (node == 0 .and. .not. noins) then
            lastdir = vc_dir_node_directory(ewoc_nth(e, -1))
            do j = k, n
                entrydir = entry_directory(entries(j)%name)
                ! Insert a directory node if needed.
                if (lastdir /= entrydir) then
                    lastdir = entrydir
                    rd = file_relative_name(entrydir, buffers(cur)%directory)
                    i = ewoc_enter_last(e, new_dir_info(rd, entrydir))
                end if
                ! Now insert the node itself.
                i = ewoc_enter_last(e, new_info(entries(j)))
            end do
        end if
        nremove = size(to_remove)
        if (nremove > 0) then
            call bind_inhibit_read_only(saved)
            crt = ewoc_nth(e, -1)
            first = ewoc_nth(e, 0)
            i = 1
            do while (crt /= first .and. crt /= 0)
                prev = ewoc_prev(e, crt)
                if (i <= nremove) then
                    if (crt == to_remove(i)) then
                        ! Remove the entries we queued up for removal.
                        call delete_nodes([crt])
                        i = i + 1
                        crt = prev
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
                crt = prev
            end do
            call restore_inhibit_read_only(saved)
        end if
        ! Update VC file properties.
        b = vc_dir_backend()
        do j = k, n
            if (entries(j)%state == VC_UNREGISTERED) then
                call vc_file_set_backend(entries(j)%name, VC_BACKEND_NONE)
            else
                call vc_file_set_backend(entries(j)%name, b)
            end if
        end do
        call set_buffer(old, st)
    contains
        ! The sort predicate of the entries: by directory, then name.
        logical function before(a, b)
            integer, intent(in) :: a, b
            if (string_lessp(dirs(a)%s, dirs(b)%s)) then
                before = .true.
            else if (dirs(a)%s /= dirs(b)%s) then
                before = .false.
            else
                before = string_lessp(entries_in(a)%name, entries_in(b)%name)
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
    end subroutine vc_dir_update

    !> Whether a status update process is running.  Port of
    !> vc-dir.el:vc-dir-busy.
    logical function vc_dir_busy()
        type(handle_type) :: buf, proc
        buf = process_buffer()
        vc_dir_busy = buffer_live_p(buf)
        if (vc_dir_busy) then
            proc = get_buffer_process(buf)
            vc_dir_busy = proc%index /= 0
        end if
    end function vc_dir_busy

    !> Kill the temporary buffer and associated process.  Port of
    !> vc-dir.el:vc-dir-kill-dir-status-process.
    recursive subroutine vc_dir_kill_dir_status_process(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, proc
        integer :: k
        status = status_type()
        buf = process_buffer()
        if (.not. buffer_live_p(buf)) return
        proc = get_buffer_process(buf)
        if (proc%index /= 0) call delete_process(proc, status)
        k = local_index(.true.)
        locals(k)%process_buffer = null_handle()
        call clear_mode_line_process()
    end subroutine vc_dir_kill_dir_status_process

    ! ------------------------------------------------------------------
    ! Motion
    ! ------------------------------------------------------------------

    !> Go to the next line.
    !> With prefix argument ARG, move that many lines.  Port of
    !> vc-dir.el:vc-dir-next-line.
    recursive subroutine vc_dir_next_line(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: node
        node = ewoc_goto_next(ew(), arg, status)
        if (.not. status%ok()) return
        call vc_dir_move_to_goal_column()
    end subroutine vc_dir_next_line

    !> Go to the previous line.
    !> With prefix argument ARG, move that many lines.  Port of
    !> vc-dir.el:vc-dir-previous-line.
    recursive subroutine vc_dir_previous_line(arg)
        integer, intent(in) :: arg
        integer :: node
        node = ewoc_goto_prev(ew(), arg)
        call vc_dir_move_to_goal_column()
    end subroutine vc_dir_previous_line

    !> Go to the next directory.  Port of vc-dir.el:vc-dir-next-directory.
    recursive subroutine vc_dir_next_directory()
        integer(int64) :: orig
        integer :: next
        orig = point()
        do
            next = ewoc_next(ew(), ewoc_locate(ew()))
            if (next == 0) then
                call goto_char(orig)
                return
            end if
            call ewoc_goto_node(ew(), next)
            call vc_dir_move_to_goal_column()
            if (allocated(infos(did(next))%directory)) return
        end do
    end subroutine vc_dir_next_directory

    !> Go to the previous directory.  Port of
    !> vc-dir.el:vc-dir-previous-directory.
    recursive subroutine vc_dir_previous_directory()
        integer(int64) :: orig
        integer :: prev
        orig = point()
        do
            prev = ewoc_prev(ew(), ewoc_locate(ew()))
            if (prev == 0) then
                call goto_char(orig)
                return
            end if
            call ewoc_goto_node(ew(), prev)
            call vc_dir_move_to_goal_column()
            if (allocated(infos(did(prev))%directory)) return
        end do
    end subroutine vc_dir_previous_directory

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    ! Call MARK_UNMARK_FUNCTION on each line of the active region, or once.
    ! Port of vc-dir.el:vc-dir-mark-unmark.
    recursive subroutine vc_dir_mark_unmark(fn, status)
        interface
            recursive subroutine fn(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine fn
        end interface
        type(status_type), intent(out) :: status
        integer(int64) :: lastl, processed_line, opoint, pos
        type(status_type) :: st
        logical :: saved
        status = status_type()
        if (use_region_p()) then
            processed_line = -1
            call region_end(pos, st)
            lastl = line_number_at_pos(pos)
            opoint = point()
            call region_beginning(pos, st)
            call goto_char(pos)
            do while (line_number_at_pos() <= lastl)
                ! We make sure to not get stuck processing the
                ! same line in an infinite loop.
                if (processed_line == line_number_at_pos()) exit
                processed_line = line_number_at_pos()
                ! Avoid any prompting.
                saved = mass_mark_bound_nil
                mass_mark_bound_nil = .true.
                call fn(st)
                mass_mark_bound_nil = saved
                ! `vc-dir-mark-file' signals an error if we try marking
                ! a directory containing marked files in its tree, or a
                ! file in a marked directory tree.  Just continue.
                if (.not. st%ok()) call vc_dir_next_line(1, st)
            end do
            call goto_char(opoint)
        else
            call fn(status)
        end if
    end subroutine vc_dir_mark_unmark

    ! Return the parent node of ARG.
    ! If IF_MARKED, return the nearest marked parent.  Port of
    ! vc-dir.el:vc-dir--parent.
    integer function vc_dir_parent(arg, if_marked) result(found)
        integer, intent(in) :: arg
        logical, intent(in) :: if_marked
        character(len=:), allocatable :: argdir, dir
        integer :: crt
        argdir = vc_dir_node_directory(arg)
        crt = arg
        found = 0
        ! Go through the predecessors, checking if any directory that is
        ! a parent is marked.
        do while (found == 0)
            crt = ewoc_prev(ew(), crt)
            if (crt == 0) exit
            dir = vc_dir_node_directory(crt)
            if (allocated(infos(did(crt))%directory) .and. string_prefix_p(dir, argdir)) then
                if (.not. if_marked .or. infos(did(crt))%marked) found = crt
            end if
        end do
    end function vc_dir_parent

    ! Return a list of children of ARG.  If ONLY_MARKED, only those marked.
    ! Port of vc-dir.el:vc-dir--children.
    subroutine vc_dir_children(arg, only_marked, found)
        integer, intent(in) :: arg
        logical, intent(in) :: only_marked
        integer, allocatable, intent(out) :: found(:)
        character(len=:), allocatable :: argdir
        integer :: crt
        argdir = vc_dir_node_directory(arg)
        allocate (found(0))
        crt = arg
        do
            crt = ewoc_next(ew(), crt)
            if (crt == 0) exit
            ! We are done, we got to an entry that is not a child of `arg'.
            if (.not. string_prefix_p(argdir, vc_dir_node_directory(crt))) exit
            if (.not. only_marked .or. infos(did(crt))%marked) found = [crt, found]
        end do
    end subroutine vc_dir_children

    !> Mark ARG (0 for the current file) and move to the next line.  Port
    !> of vc-dir.el:vc-dir-mark-file.
    recursive subroutine vc_dir_mark_file(status, arg)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: arg
        integer, allocatable :: to_inval(:), children(:)
        character(len=:), allocatable :: name
        integer(int64) :: id
        integer :: crt, parent, k
        logical :: yes
        status = status_type()
        crt = 0
        if (present(arg)) crt = arg
        if (crt == 0) crt = ewoc_locate(ew())
        if (crt == 0) return
        id = did(crt)
        to_inval = [crt]
        ! We do not allow a state in which a directory is marked and also
        ! some of its files are marked.  If the user's intent is apparent,
        ! offer to adjust things for them so that they can proceed.
        allocate (children(0))
        if (allocated(infos(id)%directory)) call vc_dir_children(crt, .true., children)
        name = infos(id)%entry%name
        if (size(children) > 0) then
            ! The user wants to mark a directory where some of its children
            ! are already marked.  Although the user's intent is clear, by
            ! default we still ask them before unmarking in order to avoid
            ! accidental erasure of complex patterns of marks.
            yes = mass_mark() /= VC_DIR_MASS_MARK_NIL
            if (yes .and. mass_mark() == VC_DIR_MASS_MARK_ASK) then
                call y_or_n_p("Replace marks on subitems with marking `"//name//"' itself?", yes, status)
                if (.not. status%ok()) return
            end if
            if (.not. yes) then
                call set_error(status, FM_USER_ERROR, "`"//name//"' is already marked")
                return
            end if
            do k = 1, size(children)
                infos(did(children(k)))%marked = .false.
                to_inval = [children(k), to_inval]
            end do
        else
            parent = vc_dir_parent(crt, .true.)
            if (parent /= 0) then
                name = infos(did(parent))%entry%name
                ! The user seems to want to mark an entry whose directory is
                ! already marked.  As the file is already implicitly marked for
                ! most VCS, they may not really intend this.
                yes = mass_mark() /= VC_DIR_MASS_MARK_NIL
                if (yes .and. mass_mark() == VC_DIR_MASS_MARK_ASK) then
                    call y_or_n_p("`"//name//"' is already marked; unmark it?", yes, status)
                    if (.not. status%ok()) return
                end if
                if (.not. yes) then
                    call set_error(status, FM_USER_ERROR, "`"//name//"' is already marked")
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
        if (.not. present(arg)) call vc_dir_next_line(1, status)
    end subroutine vc_dir_mark_file

    recursive subroutine mark_file_current(status)
        type(status_type), intent(out) :: status
        call vc_dir_mark_file(status)
    end subroutine mark_file_current

    !> Mark the current file or all files in the region.
    !> If the region is active, mark all the files in the region.
    !> Otherwise mark the file on the current line and move to the next
    !> line.  Port of vc-dir.el:vc-dir-mark.
    recursive subroutine vc_dir_mark(status)
        type(status_type), intent(out) :: status
        call vc_dir_mark_unmark(mark_file_current, status)
    end subroutine vc_dir_mark

    recursive logical function directory_marked(data)
        integer(int64), intent(in) :: data
        directory_marked = allocated(infos(data)%directory) .and. infos(data)%marked
    end function directory_marked

    recursive logical function mark_unmarked_file(data)
        integer(int64), intent(in) :: data
        mark_unmarked_file = .false.
        if (allocated(infos(data)%directory) .or. infos(data)%marked) return
        infos(data)%marked = .true.
        mark_unmarked_file = .true.
    end function mark_unmarked_file

    !> Mark all files with the same state as the current one.
    !> With prefix argument ARG, mark all files (not directories).
    !> If the current entry is a directory, mark all child files.
    !>
    !> The commands operate on files that are on the same state.
    !> This command is intended to make it easy to select all files that
    !> share the same state.  Port of vc-dir.el:vc-dir-mark-all-files.
    recursive subroutine vc_dir_mark_all_files(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: marked_dirs(:)
        integer :: crt, state
        integer(int64) :: id
        status = status_type()
        if (arg) then
            ! Mark all files.
            ! First check that no directory is marked, we can't mark
            ! files in that case.
            call ewoc_collect(ew(), directory_marked, marked_dirs)
            if (size(marked_dirs) > 0) then
                call set_error(status, FM_GENERIC_ERROR, "Cannot mark all files, directory `"// &
                               infos(marked_dirs(1))%entry%name//"' marked")
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
                        call vc_dir_mark_file(status, crt)
                        if (.not. status%ok()) return
                    end if
                    crt = ewoc_next(ew(), crt)
                end do
            end if
        end if
    end subroutine vc_dir_mark_all_files

    recursive logical function mark_by_regexp_fn(data)
        integer(int64), intent(in) :: data
        type(status_type) :: st
        mark_by_regexp_fn = .false.
        if (allocated(infos(data)%directory)) return
        if ((.not. map_unmark) .neqv. (.not. infos(data)%marked)) return
        ! We don't want to match on the part of the file
        ! that's above the current directory.
        if (string_match_p(map_regexp, file_relative_name(infos(data)%entry%name, buffers(cur)%directory), st) == 0) then
            return
        end if
        infos(data)%marked = .not. map_unmark
        mark_by_regexp_fn = .true.
    end function mark_by_regexp_fn

    !> Mark all files that match REGEXP.
    !> If UNMARK (interactively, the prefix), unmark instead.  Port of
    !> vc-dir.el:vc-dir-mark-by-regexp.
    recursive subroutine vc_dir_mark_by_regexp(regexp, unmark)
        character(len=*), intent(in) :: regexp
        logical, intent(in), optional :: unmark
        map_regexp = regexp
        map_unmark = .false.
        if (present(unmark)) map_unmark = unmark
        call ewoc_map(mark_by_regexp_fn, ew())
    end subroutine vc_dir_mark_by_regexp

    recursive logical function mark_files_fn(data)
        integer(int64), intent(in) :: data
        character(len=:), allocatable :: name
        integer :: k
        mark_files_fn = .false.
        name = expand(infos(data)%entry%name)
        do k = 1, size(map_files)
            if (map_files(k)%s == name) then
                infos(data)%marked = .true.
                mark_files_fn = .true.
                return
            end if
        end do
    end function mark_files_fn

    !> Mark files specified by file names in the argument MARK_FILES.
    !> MARK_FILES should be a list of absolute filenames.
    !> Directories must have trailing slashes.  Port of
    !> vc-dir.el:vc-dir-mark-files.
    recursive subroutine vc_dir_mark_files(mark_files)
        type(vc_string_type), intent(in) :: mark_files(:)
        type(vc_string_type), allocatable :: sorted(:)
        type(vc_string_type) :: tmp
        integer :: k, j, n
        ! Filter out subitems that would be implicitly marked.
        sorted = mark_files
        do k = 2, size(sorted)
            tmp = sorted(k)
            j = k - 1
            do while (j >= 1)
                if (.not. string_lessp(tmp%s, sorted(j)%s)) exit
                sorted(j + 1) = sorted(j)
                j = j - 1
            end do
            sorted(j + 1) = tmp
        end do
        n = 0
        if (allocated(map_files)) deallocate (map_files)
        allocate (map_files(size(sorted)))
        k = 1
        do while (k <= size(sorted))
            n = n + 1
            map_files(n) = sorted(k)
            j = k + 1
            if (len(sorted(k)%s) > 0) then
                if (sorted(k)%s(len(sorted(k)%s):) == "/") then
                    do while (j <= size(sorted))
                        if (.not. string_prefix_p(sorted(k)%s, sorted(j)%s)) exit
                        j = j + 1
                    end do
                end if
            end if
            k = j
        end do
        map_files = map_files(1:n)
        call ewoc_map(mark_files_fn, ew())
    end subroutine vc_dir_mark_files

    recursive logical function mark_state_fn(data)
        integer(int64), intent(in) :: data
        mark_state_fn = .false.
        if (.not. any(map_states == infos(data)%entry%state)) return
        infos(data)%marked = .true.
        mark_state_fn = .true.
    end function mark_state_fn

    !> Mark files that are in the state specified by the list in STATES.
    !> Port of vc-dir.el:vc-dir-mark-state-files.
    recursive subroutine vc_dir_mark_state_files(states)
        integer, intent(in) :: states(:)
        map_states = states
        call ewoc_map(mark_state_fn, ew())
    end subroutine vc_dir_mark_state_files

    !> Mark files that are in one of registered states: edited, added or
    !> removed.  Port of vc-dir.el:vc-dir-mark-registered-files.
    recursive subroutine vc_dir_mark_registered_files(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_dir_mark_state_files([VC_EDITED, VC_ADDED, VC_REMOVED])
    end subroutine vc_dir_mark_registered_files

    !> Mark files that are in unregistered state.  Port of
    !> vc-dir.el:vc-dir-mark-unregistered-files.
    recursive subroutine vc_dir_mark_unregistered_files(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_dir_mark_state_files([VC_UNREGISTERED])
    end subroutine vc_dir_mark_unregistered_files

    !> Unmark the current file and move to the next line.  Port of
    !> vc-dir.el:vc-dir-unmark-file.
    recursive subroutine vc_dir_unmark_file(status)
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
            parent = vc_dir_parent(crt, .true.)
            allocate (all_children(0))
            if (parent /= 0) call vc_dir_children(parent, .false., all_children)
            if (parent /= 0 .and. size(all_children) > 0) then
                yes = mass_mark() /= VC_DIR_MASS_MARK_NIL
                if (yes .and. mass_mark() == VC_DIR_MASS_MARK_ASK) then
                    call y_or_n_p("Replace mark on `"//infos(id)%entry%name// &
                                  "' with marks on all subitems but this one?", yes, status)
                    if (.not. status%ok()) return
                end if
                if (yes) then
                    if (allocated(infos(id)%directory)) then
                        call vc_dir_children(crt, .false., children)
                        subtree = [crt, children]
                    else
                        subtree = [crt, vc_dir_parent(crt, .false.)]
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
                call vc_dir_children(crt, .true., children)
                if (size(children) > 0 .and. mass_mark() /= VC_DIR_MASS_MARK_NIL) then
                    yes = .true.
                    if (mass_mark() == VC_DIR_MASS_MARK_ASK) then
                        call y_or_n_p("Unmark all items within `"//infos(id)%entry%name//"'?", yes, status)
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
        call vc_dir_next_line(1, status)
    end subroutine vc_dir_unmark_file

    !> Unmark the current file or all files in the region.
    !> If the region is active, unmark all the files in the region.
    !> Otherwise unmark the file on the current line and move to the next
    !> line.  Port of vc-dir.el:vc-dir-unmark.
    recursive subroutine vc_dir_unmark(status)
        type(status_type), intent(out) :: status
        call vc_dir_mark_unmark(vc_dir_unmark_file, status)
    end subroutine vc_dir_unmark

    !> Move to the previous line and unmark the file.  Port of
    !> vc-dir.el:vc-dir-unmark-file-up.
    recursive subroutine vc_dir_unmark_file_up(status)
        type(status_type), intent(out) :: status
        integer :: prev
        status = status_type()
        ! If we're on the first line, we won't move up, but we will still
        ! remove the mark.  This seems a bit odd but it is what buffer-menu
        ! does.
        prev = ewoc_goto_prev(ew(), 1)
        if (prev == 0) return
        infos(did(prev))%marked = .false.
        call ewoc_invalidate(ew(), [prev])
        call vc_dir_move_to_goal_column()
    end subroutine vc_dir_unmark_file_up

    recursive logical function unmark_fn(data)
        integer(int64), intent(in) :: data
        unmark_fn = .false.
        if (.not. infos(data)%marked) return
        infos(data)%marked = .false.
        unmark_fn = .true.
    end function unmark_fn

    recursive logical function unmark_state_fn(data)
        integer(int64), intent(in) :: data
        unmark_state_fn = .false.
        if (.not. infos(data)%marked .or. infos(data)%entry%state /= map_state) return
        infos(data)%marked = .false.
        unmark_state_fn = .true.
    end function unmark_state_fn

    !> Unmark all files with the same state as the current one.
    !> With prefix argument ARG, unmark all files.
    !> If the current entry is a directory, unmark all the child files.
    !>
    !> The commands operate on files that are on the same state.
    !> This command is intended to make it easy to deselect all files
    !> that share the same state.  Port of vc-dir.el:vc-dir-unmark-all-files.
    recursive subroutine vc_dir_unmark_all_files(arg)
        logical, intent(in) :: arg
        integer :: crt
        if (arg) then
            call ewoc_map(unmark_fn, ew())
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
                call ewoc_map(unmark_state_fn, ew())
            end if
        end if
    end subroutine vc_dir_unmark_all_files

    !> Toggle the mark of the current file.  Port of
    !> vc-dir.el:vc-dir-toggle-mark-file.
    recursive subroutine vc_dir_toggle_mark_file(status)
        type(status_type), intent(out) :: status
        integer :: crt
        status = status_type()
        crt = ewoc_locate(ew())
        if (crt == 0) return
        if (infos(did(crt))%marked) then
            call vc_dir_unmark_file(status)
        else
            call vc_dir_mark_file(status)
        end if
    end subroutine vc_dir_toggle_mark_file

    ! ------------------------------------------------------------------
    ! Commands on the files
    ! ------------------------------------------------------------------

    !> Like `vc_next_action' but for all files shown in the VC-Dir buffer.
    !> This command ignores VC-Dir marks and the position of point.
    !> When the VC-Dir's root directory is the repository root (as it usually
    !> is), this command is useful to check in all local changes at once.
    !> Port of vc-dir.el:vc-dir-root-next-action.
    recursive subroutine vc_dir_root_next_action(status)
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: files(:)
        integer, allocatable :: states(:)
        type(vc_fileset_type) :: fs
        integer :: crt, n
        integer(int64) :: id
        allocate (files(0), states(0))
        crt = ewoc_nth(ew(), 0)
        n = 0
        do while (crt /= 0)
            id = did(crt)
            if (.not. allocated(infos(id)%directory)) then
                call push_file(expand(infos(id)%entry%name), infos(id)%entry%state)
            end if
            crt = ewoc_next(ew(), crt)
        end do
        fs%backend = vc_dir_backend()
        allocate (fs%files(1))
        fs%files(1)%s = buffers(cur)%directory
        call vc_dir_only_files_state_and_model(files, states, fs, status)
        if (.not. status%ok()) return
        call set_vc_buffer_overriding_fileset(fs)
        call vc_next_action(.false., status)
        call set_vc_buffer_overriding_fileset()
    contains
        subroutine push_file(name, state)
            character(len=*), intent(in) :: name
            integer, intent(in) :: state
            type(vc_string_type), allocatable :: grown(:)
            integer :: k
            n = size(files)
            allocate (grown(n + 1))
            do k = 1, n
                call move_alloc(files(k)%s, grown(k)%s)
            end do
            grown(n + 1)%s = name
            call move_alloc(grown, files)
            states = [states, state]
        end subroutine push_file
    end subroutine vc_dir_root_next_action

    !> Delete the marked files, or the current file if no marks.
    !> The files will also be marked as deleted in the version control
    !> system.  Port of vc-dir.el:vc-dir-delete-file.
    recursive subroutine vc_dir_delete_file(status)
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs
        call vc_dir_deduce_fileset(fs, status, .true.)
        if (.not. status%ok()) return
        if (.not. allocated(fs%only_files)) then
            call set_error(status, FM_USER_ERROR, "Nothing to delete here")
            return
        end if
        if (size(fs%only_files) == 0) then
            call set_error(status, FM_USER_ERROR, "Nothing to delete here")
            return
        end if
        call vc_delete_file(fs%only_files, status)
    end subroutine vc_dir_delete_file

    !> Find the file on the current line.  Port of vc-dir.el:vc-dir-find-file.
    recursive subroutine vc_dir_find_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call vc_dir_current_file(file, status)
        if (status%ok()) call find_file(file, status)
    end subroutine vc_dir_find_file

    !> Find the file on the current line, in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of vc-dir.el:vc-dir-find-file-other-window.
    recursive subroutine vc_dir_find_file_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call vc_dir_current_file(file, status)
        if (status%ok()) call find_file_other_window(file, status)
    end subroutine vc_dir_find_file_other_window

    !> Display the file on the current line, in another window.  Port of
    !> vc-dir.el:vc-dir-display-file.
    recursive subroutine vc_dir_display_file(status)
        use window_fm, only: display_buffer, display_alist_type
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: buf, window
        type(display_alist_type) :: alist
        call vc_dir_current_file(file, status)
        if (.not. status%ok()) return
        call find_file_noselect(file, buf, status)
        if (.not. status%ok()) return
        ! (display-buffer ... t): not in the selected window.
        alist%inhibit_same_window = .true.
        call display_buffer(buf, window, status, alist=alist)
    end subroutine vc_dir_display_file

    !> Examine a file on the current line in view mode.  Port of
    !> vc-dir.el:vc-dir-view-file.
    recursive subroutine vc_dir_view_file(status)
        use view_fm, only: view_file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call vc_dir_current_file(file, status)
        if (status%ok()) call view_file(file, status)
    end subroutine vc_dir_view_file

    recursive logical function ignore_marked_fn(data)
        integer(int64), intent(in) :: data
        type(status_type) :: st
        ignore_marked_fn = .false.
        if (.not. infos(data)%marked) return
        call vc_ignore(infos(data)%entry%name, st)
        ignore_marked_fn = .true.
    end function ignore_marked_fn

    !> Ignore the current file.
    !> If a prefix argument is given, ignore all marked files.  Port of
    !> vc-dir.el:vc-dir-ignore.
    recursive subroutine vc_dir_ignore(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rel_dir, file
        status = status_type()
        if (arg) then
            call ewoc_map(ignore_marked_fn, ew())
        else
            rel_dir = vc_ignore_base_dir(status)
            if (.not. status%ok()) return
            call vc_dir_current_file(file, status)
            if (.not. status%ok()) return
            call vc_ignore(file_relative_name(file, rel_dir), status, rel_dir)
        end if
    end subroutine vc_dir_ignore

    !> The file at point (an error if there is none).  Port of
    !> vc-dir.el:vc-dir-current-file.
    recursive subroutine vc_dir_current_file(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        integer :: node
        status = status_type()
        file = ""
        node = ewoc_locate(ew())
        if (node == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No file available")
            return
        end if
        file = expand(infos(did(node))%entry%name)
    end subroutine vc_dir_current_file

    recursive logical function marked_fn(data)
        integer(int64), intent(in) :: data
        marked_fn = infos(data)%marked
    end function marked_fn

    recursive logical function needs_update_fn(data)
        integer(int64), intent(in) :: data
        needs_update_fn = infos(data)%needs_update
    end function needs_update_fn

    !> Return the list of marked files.  Port of vc-dir.el:vc-dir-marked-files.
    subroutine vc_dir_marked_files(files)
        type(vc_string_type), allocatable, intent(out) :: files(:)
        integer(int64), allocatable :: ids(:)
        integer :: k
        call ewoc_collect(ew(), marked_fn, ids)
        allocate (files(size(ids)))
        do k = 1, size(ids)
            files(k)%s = expand(infos(ids(k))%entry%name)
        end do
    end subroutine vc_dir_marked_files

    !> Return the list of (FILE . STATE) for the marked files.
    !> For marked directories return the corresponding pairs for the
    !> child files.  Port of vc-dir.el:vc-dir-marked-only-files-and-states.
    subroutine vc_dir_marked_only_files_and_states(files, states)
        type(vc_string_type), allocatable, intent(out) :: files(:)
        integer, allocatable, intent(out) :: states(:)
        character(len=:), allocatable :: dir
        integer(int64) :: id, data
        integer :: crt
        allocate (files(0), states(0))
        crt = ewoc_nth(ew(), 0)
        do while (crt /= 0)
            id = did(crt)
            if (infos(id)%marked) then
                ! FIXME: use vc-dir-child-files-and-states here instead of duplicating it.
                if (allocated(infos(id)%directory)) then
                    dir = infos(id)%directory
                    do
                        crt = ewoc_next(ew(), crt)
                        if (crt == 0) exit
                        data = did(crt)
                        if (.not. string_prefix_p(dir, vc_dir_node_directory(crt))) exit
                        if (.not. allocated(infos(data)%directory)) then
                            call push_pair(files, states, expand(infos(data)%entry%name), infos(data)%entry%state)
                        end if
                    end do
                else
                    call push_pair(files, states, expand(infos(id)%entry%name), infos(id)%entry%state)
                    crt = ewoc_next(ew(), crt)
                end if
            else
                crt = ewoc_next(ew(), crt)
            end if
        end do
    end subroutine vc_dir_marked_only_files_and_states

    ! Append (FILE . STATE).
    subroutine push_pair(files, states, file, state)
        type(vc_string_type), allocatable, intent(inout) :: files(:)
        integer, allocatable, intent(inout) :: states(:)
        character(len=*), intent(in) :: file
        integer, intent(in) :: state
        type(vc_string_type), allocatable :: grown(:)
        integer :: k, n
        n = size(files)
        allocate (grown(n + 1))
        do k = 1, n
            call move_alloc(files(k)%s, grown(k)%s)
        end do
        grown(n + 1)%s = file
        call move_alloc(grown, files)
        states = [states, state]
    end subroutine push_pair

    !> Return the state of one or more files for the current entry.
    !> If the entry is a directory, return the list of states of its child
    !> files, where each file is described by a pair (FILE . STATE).
    !> If the entry is a file, return a single pair (FILE . STATE) for
    !> that file.  Port of vc-dir.el:vc-dir-child-files-and-states.
    subroutine vc_dir_child_files_and_states(files, states)
        type(vc_string_type), allocatable, intent(out) :: files(:)
        integer, allocatable, intent(out) :: states(:)
        character(len=:), allocatable :: dir
        integer(int64) :: id, data
        integer :: crt
        allocate (files(0), states(0))
        crt = ewoc_locate(ew())
        if (crt == 0) return
        id = did(crt)
        if (allocated(infos(id)%directory)) then
            dir = infos(id)%directory
            do
                crt = ewoc_next(ew(), crt)
                if (crt == 0) exit
                data = did(crt)
                if (.not. string_prefix_p(dir, vc_dir_node_directory(crt))) exit
                if (.not. allocated(infos(data)%directory)) then
                    call push_pair(files, states, expand(infos(data)%entry%name), infos(data)%entry%state)
                end if
            end do
        else
            call push_pair(files, states, expand(infos(id)%entry%name), infos(id)%entry%state)
        end if
    end subroutine vc_dir_child_files_and_states

    ! Compute state of FNAME known to live inside DEF_DIR.  Port of
    ! vc-dir.el:vc-dir-recompute-file-state.
    recursive subroutine vc_dir_recompute_file_state(fname, def_dir, entry)
        character(len=*), intent(in) :: fname, def_dir
        type(vc_dir_entry_type), intent(out) :: entry
        character(len=:), allocatable :: saved
        integer :: b
        entry%name = file_relative_name(fname, def_dir)
        b = vc_dir_backend()
        if (b == 0) return
        saved = buffers(cur)%directory
        buffers(cur)%directory = def_dir
        if (associated(vc_backends(b)%state)) entry%state = vc_backends(b)%state(entry%name)
        if (associated(vc_backends(b)%status_fileinfo_extra)) then
            call vc_backends(b)%status_fileinfo_extra(entry%name, entry)
        end if
        buffers(cur)%directory = saved
        ! Ensure we return a nil state if the file does not exist and is
        ! not tracked so that it disappears from VC-Dir (bug#81191).
        if (entry%state == VC_UP_TO_DATE .and. .not. file_exists_p(fname)) then
            entry%state = VC_STATE_NIL
            entry%has_extra = .false.
        end if
    end subroutine vc_dir_recompute_file_state

    ! Give a DIRNAME string return the list of all child files shown in
    ! the current *vc-dir* buffer.  Port of
    ! vc-dir.el:vc-dir-find-child-files.
    subroutine vc_dir_find_child_files(dirname, children)
        character(len=*), intent(in) :: dirname
        type(vc_string_type), allocatable, intent(out) :: children(:)
        integer, allocatable :: states(:)
        integer :: crt
        allocate (children(0), states(0))
        crt = ewoc_nth(ew(), 0)
        ! Find DIR
        do while (crt /= 0)
            if (string_prefix_p(dirname, vc_dir_node_directory(crt))) exit
            crt = ewoc_next(ew(), crt)
        end do
        do while (crt /= 0)
            if (.not. string_prefix_p(dirname, vc_dir_node_directory(crt))) exit
            if (.not. allocated(infos(did(crt))%directory)) then
                ! (push ...)
                call push_pair(children, states, expand(infos(did(crt))%entry%name), 0)
            end if
            crt = ewoc_next(ew(), crt)
        end do
        children = children(size(children):1:-1)
    end subroutine vc_dir_find_child_files

    ! Update the entries for all the child files of DIRNAME shown in
    ! the current *vc-dir* buffer.  Port of
    ! vc-dir.el:vc-dir-resync-directory-files.
    recursive subroutine vc_dir_resync_directory_files(dirname)
        character(len=*), intent(in) :: dirname
        type(vc_string_type), allocatable :: files(:)
        type(vc_dir_entry_type), allocatable :: fileentries(:)
        integer :: k, n
        call vc_dir_find_child_files(dirname, files)
        n = size(files)
        if (n == 0) return
        allocate (fileentries(n))
        do k = 1, n
            ! (push ...): the last first.
            call vc_dir_recompute_file_state(files(k)%s, buffers(cur)%directory, fileentries(n + 1 - k))
        end do
        call vc_dir_update(fileentries, current_buffer())
    end subroutine vc_dir_resync_directory_files

    !> Update the entries for FNAME in any directory buffers that list it.
    !> Port of vc-dir.el:vc-dir-resynch-file (the `vc_dir_resynch_file' of
    !> vc_hooks_fm).
    recursive subroutine vc_dir_resynch_file_impl(fname)
        character(len=*), intent(in) :: fname
        character(len=:), allocatable :: file, ddir, truedir
        type(handle_type), allocatable :: keep(:)
        type(handle_type) :: old, status_buf
        type(vc_dir_entry_type) :: complete_state(1)
        type(ewoc_part_type), allocatable :: header(:)
        type(ewoc_part_type) :: none(0)
        type(status_type) :: st
        integer :: k, n
        call file_truename(fname, file, st)
        if (.not. st%ok()) file = fname
        if (.not. allocated(vc_dir_buffers)) return
        old = current_buffer()
        allocate (keep(size(vc_dir_buffers)))
        n = 0
        ! look for a vc-dir buffer that might show this file.
        do k = 1, size(vc_dir_buffers)
            status_buf = vc_dir_buffers(k)
            if (.not. buffer_live_p(status_buf)) cycle
            call set_buffer(status_buf, st)
            if (.not. derived_mode_p("vc_dir_mode")) cycle
            n = n + 1
            keep(n) = status_buf
            ! The actual contents of this VC-Dir buffer,
            ! which is what we care about here, is always
            ! relative to the toplevel value.
            call expand_file_name(buffers(cur)%directory, ddir, st)
            if (.not. file_in_directory_p(file, ddir)) cycle
            if (file_directory_p(file)) then
                call vc_dir_resync_directory_files(file)
                call vc_dir_headers(vc_dir_backend(), ddir, header)
                call ewoc_set_hf_parts(ew(), header, none)
            else
                ! Pass two truenames (bug#80803, bug#80967).
                call file_truename(ddir, truedir, st)
                if (.not. st%ok()) truedir = ddir
                call vc_dir_recompute_file_state(file, truedir, complete_state(1))
                call vc_dir_update(complete_state, status_buf, &
                                   complete_state(1)%state == VC_STATE_NIL .or. &
                                   complete_state(1)%state == VC_UP_TO_DATE)
            end if
        end do
        call set_buffer(old, st)
        ! Remove out-of-date entries from vc-dir-buffers.
        vc_dir_buffers = keep(1:n)
    end subroutine vc_dir_resynch_file_impl

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for VC directory buffers.
    !> Marking/Unmarking key bindings and actions: \<vc_dir_mode_map>
    !> \[vc_dir_mark] - mark a file/directory
    !>   - if the region is active, mark all the files in region.
    !>     Restrictions: - a file cannot be marked if any parent directory is marked
    !>                   - a directory cannot be marked if any child file or
    !>                     directory is marked
    !> \[vc_dir_unmark] - unmark a file/directory
    !>   - if the region is active, unmark all the files in region.
    !> \[vc_dir_mark_all_files] - if the cursor is on a file: mark all the files with the same state as
    !>       the current file
    !>   - if the cursor is on a directory: mark all child files
    !>   - with a prefix argument: mark all files
    !> \[vc_dir_unmark_all_files] - if the cursor is on a file: unmark all the files with the same state
    !>       as the current file
    !>   - if the cursor is on a directory: unmark all child files
    !>   - with a prefix argument: unmark all files
    !>
    !> VC commands
    !> VC commands in the \[vc_prefix_map] prefix can be used.
    !> VC commands act on the marked entries.  If nothing is marked, VC
    !> commands act on the current entry.
    !>
    !> \{vc_dir_mode_map}  Port of vc-dir.el:vc-dir-mode (Search & Replace
    !> are deferred).
    recursive subroutine vc_dir_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved_mode
        integer :: k
        if (.not. associated(major_modes%vc_dir_mode%run)) call syms_of_vc_dir()
        call major_modes%vc_dir_mode%begin(saved_mode, status)
        if (.not. status%ok()) return
        k = local_index(.true.)
        locals(k)%backend = use_vc_backend
        buffers(cur)%read_only = .false.
        call erase_buffer(status)
        if (status%ok()) then
            locals(k)%process_buffer = null_handle()
            locals(k)%ewoc = ewoc_create(vc_dir_printer)
            locals(k)%has_ewoc = .true.
            call revert_buffer_function%set_local(vc_dir_revert_buffer_function)
            ! (setq list-buffers-directory ...): Buffer Menu is not ported.
            call add_vc_dir_buffer(current_buffer())
            call vc_dir_refresh(status)
        end if
        buffers(cur)%read_only = .true.
        if (.not. status%ok()) return
        call major_modes%vc_dir_mode%finish(saved_mode, status)
    end subroutine vc_dir_mode

    ! (add-to-list 'vc-dir-buffers BUF).
    subroutine add_vc_dir_buffer(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(vc_dir_buffers)) allocate (vc_dir_buffers(0))
        do k = 1, size(vc_dir_buffers)
            if (vc_dir_buffers(k)%index == buf%index .and. vc_dir_buffers(k)%generation == buf%generation) return
        end do
        n = size(vc_dir_buffers)
        allocate (grown(n + 1))
        grown(1) = buf
        if (n > 0) grown(2:) = vc_dir_buffers
        call move_alloc(grown, vc_dir_buffers)
    end subroutine add_vc_dir_buffer

    !> Display the headers in the *VC-Dir* buffer.
    !> It calls the `dir_extra_headers' backend method to display backend
    !> specific headers.  Port of vc-dir.el:vc-dir-headers (the outgoing
    !> count is deferred).
    recursive subroutine vc_dir_headers(backend, dir, parts)
        integer, intent(in) :: backend
        character(len=*), intent(in) :: dir
        type(ewoc_part_type), allocatable, intent(out) :: parts(:)
        type(ewoc_part_type), allocatable :: extra(:)
        integer :: k, n
        if (associated(vc_backends(backend)%dir_extra_headers)) then
            call vc_backends(backend)%dir_extra_headers(dir, extra)
        else
            call vc_default_dir_extra_headers(dir, extra)
        end if
        n = size(extra)
        allocate (parts(5 + n))
        parts(1)%text = "VC backend : "
        parts(1)%face = "vc_dir_header"
        parts(2)%text = vc_backend_name(backend)//LF
        parts(2)%face = "vc_dir_header_value"
        parts(3)%text = "Working dir: "
        parts(3)%face = "vc_dir_header"
        parts(4)%text = abbreviate_file_name(dir)//LF
        parts(4)%face = "vc_dir_header_value"
        do k = 1, n
            parts(4 + k) = extra(k)
        end do
        parts(5 + n)%text = LF
        parts(5 + n)%face = ""
    end subroutine vc_dir_headers

    ! The update function of `vc_dir_refresh_files': add ENTRIES and,
    ! when complete, remove the entries not updated at all and the
    ! directories left without children.
    recursive subroutine refresh_files_update(entries, buffer, more_to_come)
        type(vc_dir_entry_type), intent(in) :: entries(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: more_to_come
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        integer :: crt, first, prev, next
        logical :: empty
        old = current_buffer()
        call set_buffer(buffer, st)
        call vc_dir_update(entries, buffer)
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
                prev = ewoc_prev(ew(), crt)
                if (allocated(infos(did(crt))%directory)) then
                    next = ewoc_next(ew(), crt)
                    empty = next == 0
                    if (.not. empty) empty = allocated(infos(did(next))%directory)
                    if (empty) call delete_nodes([crt])
                end if
                crt = prev
            end do
            call restore_inhibit_read_only(saved)
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine refresh_files_update

    ! The predicate of `ewoc_filter' in `refresh_files_update'.
    recursive logical function not_needs_update(data)
        integer(int64), intent(in) :: data
        ! The state for directory entries might
        ! have been changed to 'up-to-date,
        ! reset it, otherwise it will be removed when doing 'x'
        ! next time.
        ! FIXME: There should be a more elegant way to do this.
        if (allocated(infos(data)%directory) .and. infos(data)%entry%state == VC_UP_TO_DATE) then
            infos(data)%entry%state = VC_STATE_NIL
        end if
        not_needs_update = .not. infos(data)%needs_update
        if (.not. not_needs_update) info_live(data) = .false.
    end function not_needs_update

    ! The temporary buffer for the status process, made if needed.
    recursive subroutine ensure_process_buffer(backend, buf)
        integer, intent(in) :: backend
        type(handle_type), intent(out) :: buf
        type(status_type) :: st
        integer :: k
        k = local_index(.true.)
        if (.not. buffer_live_p(locals(k)%process_buffer)) then
            call get_buffer_create(generate_new_buffer_name(" *VC-"//vc_backend_name(backend)//"* tmp status"), &
                                   locals(k)%process_buffer, st)
        end if
        buf = locals(k)%process_buffer
    end subroutine ensure_process_buffer

    !> Refresh some FILES in the *VC-Dir* buffer.  Port of
    !> vc-dir.el:vc-dir-refresh-files.
    recursive subroutine vc_dir_refresh_files(files, status)
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: def_dir
        type(handle_type) :: buffer, pbuf
        type(status_type) :: st
        integer :: backend
        status = status_type()
        def_dir = buffers(cur)%directory
        backend = vc_dir_backend()
        call vc_set_mode_line_busy_indicator()
        ! Call the `dir-status-files' backend function.
        ! `dir-status-files' is supposed to be asynchronous.
        ! It should compute the results, and then call the function
        ! passed as an argument in order to update the vc-dir buffer
        ! with the results.
        call ensure_process_buffer(backend, pbuf)
        buffer = current_buffer()
        call set_buffer(pbuf, st)
        buffers(cur)%directory = def_dir
        call erase_buffer(st)
        if (associated(vc_backends(backend)%dir_status_files)) then
            call vc_backends(backend)%dir_status_files(def_dir, files, refresh_files_update, buffer, status)
        end if
        call set_buffer(buffer, st)
    end subroutine vc_dir_refresh_files

    ! The `revert_buffer_function' of VC-Dir buffers.  Port of
    ! vc-dir.el:vc-dir-revert-buffer-function.
    recursive subroutine vc_dir_revert_buffer_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        if (ignore_auto .and. noconfirm .and. .false.) return
        call vc_dir_refresh(status)
        if (.not. status%ok()) return
        if (vc_dir_auto_hide_up_to_date /= VC_DIR_AUTO_HIDE_NIL) call vc_dir_hide_state()
    end subroutine vc_dir_revert_buffer_function

    ! The update function of `vc_dir_refresh'.
    recursive subroutine refresh_update(entries, buffer, more_to_come)
        type(vc_dir_entry_type), intent(in) :: entries(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: more_to_come
        type(vc_string_type), allocatable :: names(:)
        integer(int64), allocatable :: remaining(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k
        old = current_buffer()
        call set_buffer(buffer, st)
        call vc_dir_update(entries, buffer)
        if (.not. more_to_come) then
            call ewoc_collect(ew(), needs_update_fn, remaining)
            if (size(remaining) > 0) then
                allocate (names(size(remaining)))
                do k = 1, size(remaining)
                    names(k)%s = infos(remaining(k))%entry%name
                end do
                call vc_dir_refresh_files(names, st)
            else
                call clear_mode_line_process()
                call vc_dir_refresh_hook%run(st)
            end if
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine refresh_update

    recursive logical function set_needs_update(data)
        integer(int64), intent(in) :: data
        if (.not. allocated(infos(data)%directory)) infos(data)%needs_update = .true.
        set_needs_update = .false.
    end function set_needs_update

    !> Refresh the contents of the *VC-Dir* buffer.
    !> Throw an error if another update process is in progress.  Port of
    !> vc-dir.el:vc-dir-refresh.
    recursive subroutine vc_dir_refresh(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: def_dir
        type(ewoc_part_type), allocatable :: header(:)
        type(ewoc_part_type) :: none(0)
        type(vc_fileset_type) :: fs
        type(vc_string_type) :: nofiles(0)
        type(handle_type) :: buffer, pbuf
        type(status_type) :: st
        integer :: backend
        status = status_type()
        if (vc_dir_busy()) then
            call set_error(status, FM_GENERIC_ERROR, "Another update process is in progress, cannot run two at a time")
            return
        end if
        def_dir = buffers(cur)%directory
        backend = vc_dir_backend()
        if (vc_dir_save_some_buffers_on_revert) then
            fs%backend = backend
            allocate (fs%files(1))
            fs%files(1)%s = def_dir
            call vc_buffer_sync_fileset(fs, status, .true.)
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
        call vc_dir_headers(backend, def_dir, header)
        call ewoc_set_hf_parts(ew(), header, none)
        buffer = current_buffer()
        call set_buffer(pbuf, st)
        buffers(cur)%directory = def_dir
        call erase_buffer(st)
        if (associated(vc_backends(backend)%dir_status_files)) then
            call vc_backends(backend)%dir_status_files(def_dir, nofiles, refresh_update, buffer, status)
        end if
        call set_buffer(buffer, st)
    end subroutine vc_dir_refresh

    !> Insert an entry for a specific file into the current *VC-Dir* listing.
    !> This is typically used if the file is up-to-date (or has been added
    !> outside of VC) and one wants to do some operation on it.  Port of
    !> vc-dir.el:vc-dir-show-fileentry.
    recursive subroutine vc_dir_show_fileentry(file)
        character(len=*), intent(in) :: file
        type(vc_dir_entry_type) :: entry(1)
        entry(1)%name = file_relative_name(file, buffers(cur)%directory)
        entry(1)%state = vc_state(expand(file))
        call vc_dir_update(entry, current_buffer())
    end subroutine vc_dir_show_fileentry

    !> Hide items that are in STATE from display.
    !> See `vc_state' for valid values of STATE.
    !>
    !> If STATE is absent (nil), hide both `up-to-date' and `ignored' items.
    !>
    !> Interactively, if `current_prefix_arg' is true, set STATE to
    !> state of item at point, if any.  Port of vc-dir.el:vc-dir-hide-state.
    recursive subroutine vc_dir_hide_state(state)
        integer, intent(in), optional :: state
        type(value_type) :: saved
        integer(int64) :: id
        integer :: crt, first, next, prev
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
            prev = ewoc_prev(ew(), crt)
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
            crt = prev
        end do
    end subroutine vc_dir_hide_state

    !> Remove the current line from display.  Port of
    !> vc-dir.el:vc-dir-kill-line.
    recursive subroutine vc_dir_kill_line(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        integer :: crt
        status = status_type()
        crt = ewoc_locate(ew())
        if (crt == 0) return
        call bind_inhibit_read_only(saved)
        call delete_nodes([crt])
        call restore_inhibit_read_only(saved)
    end subroutine vc_dir_kill_line

    ! The pretty-printer of the ewoc.  Port of vc-dir.el:vc-dir-printer.
    recursive subroutine vc_dir_printer(data)
        integer(int64), intent(in) :: data
        integer :: b
        b = vc_dir_backend()
        if (b > 0) then
            if (associated(vc_backends(b)%dir_printer)) then
                call vc_backends(b)%dir_printer(infos(data))
                return
            end if
        end if
        call vc_default_dir_printer(infos(data))
    end subroutine vc_dir_printer

    ! Call `vc_only_files_state_and_model' as appropriate for VC-Dir buffers.
    ! Offer to call `vc_dir_hide_up_to_date' if that might be useful.
    ! If we did, remove up-to-date items from ONLY_FILES_LIST before passing
    ! to `vc_only_files_state_and_model'.
    !
    ! There's usually no action to be taken on `up-to-date' or `ignored'
    ! files, but a new user may include these in their VC-Dir fileset without
    ! realizing it.  To avoid the user having to know in advance that what
    ! they must do is invoke \<vc_dir_mode_map>\[vc_dir_hide_up_to_date]
    ! before \[vc_next_action], offer to invoke `vc_dir_hide_up_to_date'
    ! for them, and also filter ONLY_FILES_LIST so as not to include entries
    ! in those states.  Only do this if there are both up-to-date and
    ! non-up-to-date files in ONLY_FILES_LIST (in case we add a VC next action
    ! for `up-to-date' and/or `ignored' files at some point).  Port of
    ! vc-dir.el:vc-dir--only-files-state-and-model.
    recursive subroutine vc_dir_only_files_state_and_model(files, states, fs, status)
        type(vc_string_type), intent(in) :: files(:)
        integer, intent(in) :: states(:)
        type(vc_fileset_type), intent(inout) :: fs
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: other(:)
        integer, allocatable :: other_states(:)
        integer :: k, n_up
        logical :: yes
        status = status_type()
        allocate (other(0), other_states(0))
        n_up = 0
        do k = 1, size(files)
            if (up_to_date_state(states(k))) then
                n_up = n_up + 1
            else
                call push_pair(other, other_states, files(k)%s, states(k))
            end if
        end do
        yes = .false.
        if (n_up > 0 .and. size(other) > 0) then
            call y_or_n_p("Clear up-to-date items before proceeding?", yes, status)
            if (.not. status%ok()) return
        end if
        if (yes) then
            call vc_dir_hide_state()
            call vc_only_files_state_and_model(other, other_states, vc_dir_backend(), fs, status)
        else
            call vc_only_files_state_and_model(files, states, vc_dir_backend(), fs, status)
        end if
    end subroutine vc_dir_only_files_state_and_model

    !> The fileset of a VC-Dir buffer: the marked files, or the file at
    !> point.  With STATE_MODEL_ONLY_FILES, also their states and model.
    !> Port of vc-dir.el:vc-dir-deduce-fileset.
    recursive subroutine vc_dir_deduce_fileset(fs, status, state_model_only_files)
        type(vc_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: state_model_only_files
        type(vc_string_type), allocatable :: marked(:), only_files(:)
        integer, allocatable :: states(:)
        character(len=:), allocatable :: file
        logical :: smo
        status = status_type()
        smo = .false.
        if (present(state_model_only_files)) smo = state_model_only_files
        call vc_dir_marked_files(marked)
        if (size(marked) > 0) then
            fs%files = marked
            if (smo) call vc_dir_marked_only_files_and_states(only_files, states)
        else
            call vc_dir_current_file(file, status)
            if (.not. status%ok()) return
            allocate (fs%files(1))
            fs%files(1)%s = file
            if (smo) call vc_dir_child_files_and_states(only_files, states)
        end if
        fs%backend = vc_dir_backend()
        if (smo) call vc_dir_only_files_state_and_model(only_files, states, fs, status)
    end subroutine vc_dir_deduce_fileset

    !> Run `vc_dir' in the repository root directory without prompt.
    !> If the default directory of the current buffer is
    !> not under version control, prompt for a directory.  Port of
    !> vc-dir.el:vc-dir-root.
    recursive subroutine vc_dir_root(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root
        logical :: found
        call vc_root_dir(root, found, status)
        if (.not. status%ok()) return
        if (found) then
            call vc_dir(root, status)
        else
            call cmd_vc_dir(status)
        end if
    end subroutine vc_dir_root

    !> Show the VC status for "interesting" files in and below DIR.
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
    !> \{vc_dir_mode_map}  Port of vc-dir.el:vc-dir.
    recursive subroutine vc_dir(dir, status, backend)
        use window_fm, only: pop_to_buffer
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: backend
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer :: b, saved
        b = 0
        if (present(backend)) b = backend
        if (b == 0) then
            b = vc_responsible_backend(dir, status)
            if (.not. status%ok()) return
        end if
        ! (let ((pop-up-windows)) ...): based on cvs-examine; bug#6204.
        call vc_dir_prepare_status_buffer("*vc-dir*", dir, b, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)
        if (derived_mode_p("vc_dir_mode")) then
            call vc_dir_refresh(status)
        else
            ! FIXME: find a better way to pass the backend to `vc-dir-mode'.
            saved = use_vc_backend
            use_vc_backend = b
            call vc_dir_mode(status)
            use_vc_backend = saved
            ! (Activate the backend-specific minor mode, if any: deferred.)
        end if
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call pop_to_buffer(buffer, status)
    end subroutine vc_dir

    !> The default `dir_extra_headers': a reminder to add backend specific
    !> headers.  Port of vc-dir.el:vc-default-dir-extra-headers.
    recursive subroutine vc_default_dir_extra_headers(dir, parts)
        character(len=*), intent(in) :: dir
        type(ewoc_part_type), allocatable, intent(out) :: parts(:)
        ! Be loud by default to remind people to add code to display
        ! backend specific headers.
        ! XXX: change this to return nil before the release.
        allocate (parts(2))
        parts(1)%text = "Extra      : "
        parts(1)%face = "vc_dir_header"
        parts(2)%text = "Please add backend specific headers here.  It's easy!"
        if (len(dir) < 0) parts(2)%text = ""
        parts(2)%face = "vc_dir_status_warning"
    end subroutine vc_default_dir_extra_headers

    !> Pretty print FILEENTRY.  Port of vc-dir.el:vc-default-dir-printer
    !> (without the mouse maps and help echoes).
    recursive subroutine vc_default_dir_printer(fileentry)
        type(vc_dir_fileinfo_type), intent(in) :: fileentry
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
            call insert_with_face("*", "vc_dir_mark_indicator")
        else
            call insert_with_face(" ", "vc_dir_mark_indicator")
        end if
        call insert_with_face("   ", "")
        if (display_state == "up-to-date") then
            face = "vc_dir_status_up_to_date"
        else if (display_state == "missing" .or. display_state == "conflict" .or. display_state == "needs-update" &
                 .or. display_state == "unlocked-changes" .or. display_state == "committing") then
            face = "vc_dir_status_warning"
        else if (display_state == "ignored") then
            face = "vc_dir_status_ignored"
        else
            face = "vc_dir_status_edited"
        end if
        if (len(display_state) >= 20) then
            call insert_with_face(display_state, face)
        else
            padded = display_state
            call insert_with_face(padded, face)
        end if
        call insert_with_face(" ", "")
        if (isdir) then
            call insert_with_face(fileentry%entry%name, "vc_dir_directory")
        else
            call insert_with_face(fileentry%entry%name, "vc_dir_file")
        end if
    end subroutine vc_default_dir_printer

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    integer function count_arg()
        count_arg = int(prefix_numeric_value(current_prefix_arg))
    end function count_arg

    recursive subroutine cmd_vc_dir(status)
        use minibuffer_fm, only: read_file_name
        use minibuffer_fm, only: REQUIRE_MATCH_T
        use vc_hooks_fm, only: vc_backend_named
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, dir, truename, name
        logical :: found
        integer :: b
        call vc_root_dir(root, found, status)
        if (.not. status%ok()) return
        if (.not. found) root = buffers(cur)%directory
        ! When you hit C-x v d in a visited VC file,
        ! the *vc-dir* buffer visits the directory under its truename;
        ! therefore it makes sense to always do that.
        ! Otherwise if you do C-x v d -> C-x C-f -> C-x v d
        ! you may get a new *vc-dir* buffer, different from the original
        call read_file_name("VC status for directory: ", dir, status, dir=root, mustmatch=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        call file_truename(dir, truename, status)
        if (.not. status%ok()) return
        b = 0
        if (.not. nilp(current_prefix_arg)) then
            call read_backend(name, status)
            if (.not. status%ok()) return
            b = vc_backend_named(name)
        end if
        call vc_dir(truename, status, b)
    end subroutine cmd_vc_dir

    ! (completing-read "Use VC backend: " vc-handled-backends nil t).
    recursive subroutine read_backend(name, status)
        use minibuf_fm, only: completion_list_table
        use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
        use lisp_fm, only: make_string, make_list
        use vc_hooks_fm, only: vc_handled_backends
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        integer :: k
        allocate (names(size(vc_handled_backends)))
        do k = 1, size(vc_handled_backends)
            names(k) = make_string(trim(vc_handled_backends(k)))
        end do
        call completing_read("Use VC backend: ", completion_list_table(make_list(names)), name, status, &
                             require_match=REQUIRE_MATCH_T)
    end subroutine read_backend

    recursive subroutine cmd_vc_dir_next_line(status)
        type(status_type), intent(out) :: status
        call vc_dir_next_line(count_arg(), status)
    end subroutine cmd_vc_dir_next_line

    recursive subroutine cmd_vc_dir_previous_line(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_dir_previous_line(count_arg())
    end subroutine cmd_vc_dir_previous_line

    recursive subroutine cmd_vc_dir_next_directory(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_dir_next_directory()
    end subroutine cmd_vc_dir_next_directory

    recursive subroutine cmd_vc_dir_previous_directory(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_dir_previous_directory()
    end subroutine cmd_vc_dir_previous_directory

    recursive subroutine cmd_vc_dir_mark_all_files(status)
        type(status_type), intent(out) :: status
        call vc_dir_mark_all_files(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_vc_dir_mark_all_files

    recursive subroutine cmd_vc_dir_unmark_all_files(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_dir_unmark_all_files(.not. nilp(current_prefix_arg))
    end subroutine cmd_vc_dir_unmark_all_files

    recursive subroutine cmd_vc_dir_mark_by_regexp(status)
        use minibuf_fm, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        call read_string("Mark files matching: ", regexp, status)
        if (status%ok()) call vc_dir_mark_by_regexp(regexp,.not. nilp(current_prefix_arg))
    end subroutine cmd_vc_dir_mark_by_regexp

    recursive subroutine cmd_vc_dir_ignore(status)
        type(status_type), intent(out) :: status
        call vc_dir_ignore(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_vc_dir_ignore

    recursive subroutine cmd_vc_dir_hide_up_to_date(status)
        type(status_type), intent(out) :: status
        integer :: node
        status = status_type()
        if (.not. nilp(current_prefix_arg)) then
            ! Command is prefixed.  Infer STATE from point.
            node = ewoc_locate(ew())
            if (node /= 0) then
                call vc_dir_hide_state(infos(did(node))%entry%state)
                return
            end if
        end if
        call vc_dir_hide_state()
    end subroutine cmd_vc_dir_hide_up_to_date

    recursive subroutine cmd_vc_dir_show_fileentry(status)
        use minibuffer_fm, only: read_file_name
        use minibuffer_fm, only: REQUIRE_MATCH_T
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call read_file_name("Show file: ", file, status, mustmatch=REQUIRE_MATCH_T)
        if (status%ok()) call vc_dir_show_fileentry(file)
    end subroutine cmd_vc_dir_show_fileentry

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    subroutine define_faces()
        type(named_face_type), pointer :: f
        f => vc_dir_header
        call f%define("vc_dir_header", "Face for headers in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_type_face"))
        f => vc_dir_header_value
        call f%define("vc_dir_header_value", "Face for header values in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => vc_dir_header_urgent_value
        call f%define("vc_dir_header_urgent_value", "Face for urgent header values in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_warning_face"))
        f => vc_dir_directory
        call f%define("vc_dir_directory", "Face for directories in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_comment_delimiter_face"))
        f => vc_dir_file
        call f%define("vc_dir_file", "Face for files in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => vc_dir_mark_indicator
        call f%define("vc_dir_mark_indicator", "Face for mark indicators in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_type_face"))
        f => vc_dir_status_warning
        call f%define("vc_dir_status_warning", "Face for warning status in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_warning_face"))
        f => vc_dir_status_edited
        call f%define("vc_dir_status_edited", "Face for edited status in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => vc_dir_status_up_to_date
        call f%define("vc_dir_status_up_to_date", "Face for up-to-date status in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_builtin_face"))
        f => vc_dir_status_ignored
        call f%define("vc_dir_status_ignored", "Face for ignored or empty values in VC-Dir buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="shadow"))
    end subroutine define_faces

    !> Define VC-Dir mode, its keymap and faces, and register the commands.
    subroutine syms_of_vc_dir()
        use lisp_fm, only: make_keymap_ref
        type(handle_type) :: regexp_map, mark_map
        type(status_type) :: st
        vc_dir_resynch_file => vc_dir_resynch_file_impl
        commands%vc_dir%run => cmd_vc_dir
        commands%vc_dir_root%run => vc_dir_root
        if (associated(major_modes%vc_dir_mode%run)) return
        call define_faces()
        call major_modes%vc_dir_mode%define("vc_dir_mode", "VC dir", vc_dir_mode, vc_dir_mode_hook, &
                                            parent=major_modes%special_mode)
        vc_dir_mode_map = major_modes%vc_dir_mode%map
        commands%vc_dir_mode%run => vc_dir_mode
        commands%vc_dir_mark%run => vc_dir_mark
        commands%vc_dir_delete_file%run => vc_dir_delete_file
        commands%vc_dir_mark_all_files%run => cmd_vc_dir_mark_all_files
        commands%vc_dir_unmark%run => vc_dir_unmark
        commands%vc_dir_unmark_all_files%run => cmd_vc_dir_unmark_all_files
        commands%vc_dir_unmark_file_up%run => vc_dir_unmark_file_up
        commands%vc_dir_next_line%run => cmd_vc_dir_next_line
        commands%vc_dir_previous_line%run => cmd_vc_dir_previous_line
        commands%vc_dir_next_directory%run => cmd_vc_dir_next_directory
        commands%vc_dir_previous_directory%run => cmd_vc_dir_previous_directory
        commands%vc_dir_find_file%run => vc_dir_find_file
        commands%vc_dir_find_file_other_window%run => vc_dir_find_file_other_window
        commands%vc_dir_display_file%run => vc_dir_display_file
        commands%vc_dir_view_file%run => vc_dir_view_file
        commands%vc_dir_kill_dir_status_process%run => vc_dir_kill_dir_status_process
        commands%vc_dir_hide_up_to_date%run => cmd_vc_dir_hide_up_to_date
        commands%vc_dir_kill_line%run => vc_dir_kill_line
        commands%vc_dir_ignore%run => cmd_vc_dir_ignore
        commands%vc_dir_root_next_action%run => vc_dir_root_next_action
        commands%vc_dir_mark_by_regexp%run => cmd_vc_dir_mark_by_regexp
        commands%vc_dir_mark_registered_files%run => vc_dir_mark_registered_files
        commands%vc_dir_mark_unregistered_files%run => vc_dir_mark_unregistered_files
        commands%vc_dir_refresh%run => vc_dir_refresh
        commands%vc_dir_show_fileentry%run => cmd_vc_dir_show_fileentry
        ! VC commands
        call keymap_set(vc_dir_mode_map, "v", "vc_next_action", st)    ! C-x v v
        call keymap_set(vc_dir_mode_map, "=", "vc_diff", st)           ! C-x v =
        call keymap_set(vc_dir_mode_map, "D", "vc_root_diff", st)      ! C-x v D
        call keymap_set(vc_dir_mode_map, "i", "vc_register", st)       ! C-x v i
        call keymap_set(vc_dir_mode_map, "l", "vc_print_log", st)      ! C-x v l
        call keymap_set(vc_dir_mode_map, "L", "vc_print_root_log", st) ! C-x v L
        ! Marking.
        call keymap_set(vc_dir_mode_map, "m", "vc_dir_mark", st)
        call keymap_set(vc_dir_mode_map, "d", "vc_dir_delete_file", st)
        call keymap_set(vc_dir_mode_map, "M", "vc_dir_mark_all_files", st)
        call keymap_set(vc_dir_mode_map, "u", "vc_dir_unmark", st)
        call keymap_set(vc_dir_mode_map, "U", "vc_dir_unmark_all_files", st)
        call keymap_set(vc_dir_mode_map, "DEL", "vc_dir_unmark_file_up", st)
        call keymap_set(vc_dir_mode_map, "M-DEL", "vc_dir_unmark_all_files", st)
        ! Movement.
        call keymap_set(vc_dir_mode_map, "n", "vc_dir_next_line", st)
        call keymap_set(vc_dir_mode_map, "SPC", "vc_dir_next_line", st)
        call keymap_set(vc_dir_mode_map, "TAB", "vc_dir_next_directory", st)
        call keymap_set(vc_dir_mode_map, "p", "vc_dir_previous_line", st)
        call keymap_set(vc_dir_mode_map, "S-SPC", "vc_dir_previous_line", st)
        call keymap_set(vc_dir_mode_map, "<backtab>", "vc_dir_previous_directory", st)
        ! Rebind paragraph-movement commands.
        call keymap_set(vc_dir_mode_map, "M-}", "vc_dir_next_directory", st)
        call keymap_set(vc_dir_mode_map, "M-{", "vc_dir_previous_directory", st)
        call keymap_set(vc_dir_mode_map, "C-<down>", "vc_dir_next_directory", st)
        call keymap_set(vc_dir_mode_map, "C-<up>", "vc_dir_previous_directory", st)
        ! The remainder.
        call keymap_set(vc_dir_mode_map, "f", "vc_dir_find_file", st)
        call keymap_set(vc_dir_mode_map, "e", "vc_dir_find_file", st) ! dired-mode compatibility
        call keymap_set(vc_dir_mode_map, "RET", "vc_dir_find_file", st)
        call keymap_set(vc_dir_mode_map, "o", "vc_dir_find_file_other_window", st)
        call keymap_set(vc_dir_mode_map, "C-o", "vc_dir_display_file", st)
        call keymap_set(vc_dir_mode_map, "C-c C-c", "vc_dir_kill_dir_status_process", st)
        call keymap_set(vc_dir_mode_map, "x", "vc_dir_hide_up_to_date", st)
        call keymap_set(vc_dir_mode_map, "C-k", "vc_dir_kill_line", st)
        call keymap_set(vc_dir_mode_map, "G", "vc_dir_ignore", st)
        call keymap_set(vc_dir_mode_map, "@", "vc_revert", st)
        call keymap_set(vc_dir_mode_map, "V", "vc_dir_root_next_action", st)
        call make_sparse_keymap(regexp_map)
        call keymap_set(vc_dir_mode_map, "%", make_keymap_ref(regexp_map), st)
        call keymap_set(regexp_map, "m", "vc_dir_mark_by_regexp", st)
        call make_sparse_keymap(mark_map)
        call keymap_set(vc_dir_mode_map, "*", make_keymap_ref(mark_map), st)
        call keymap_set(mark_map, "%", "vc_dir_mark_by_regexp", st)
        call keymap_set(mark_map, "r", "vc_dir_mark_registered_files", st)
        call add_local_variables_killer(kill_vc_dir_locals)
    end subroutine syms_of_vc_dir
end module vc_dir_fm
