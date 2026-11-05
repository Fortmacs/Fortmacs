! ediff_init_mod.f90 -- Ediff's variables, faces and hooks.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-init.el (in part: the
! comparison of two and three buffers on a terminal frame).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> The state of Ediff sessions and what they share: the user options,
!> hooks and faces of ediff-init.el, the buffer types (A, B, C and the
!> ancestor) as named constants, and one `ediff_session_type' per
!> control buffer in `ediff_sessions', holding what upstream keeps in
!> the control buffer's local variables (`ediff_buffer_a', the
!> difference vectors, the current difference, the windows, ...).
!>
!> The arrays of a session indexed by buffer type (`buffers', `windows',
!> `differences', the bounds and the current-difference overlays) have
!> room for all four types, so merge jobs (buffer C the merge buffer, the
!> ancestor) and session groups (`meta_buffer', ediff_multiple_mod) fit.
module ediff_init_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, FM_USER_ERROR
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, command_type, choice_type
    use data_mod, only: choice_setting_type
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, current_buffer, is_buffer_live, buffer_serial_live, close_buffer
    use hooks_mod, only: hook_type
    use faces_mod, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use itree_mod, only: overlay_put, overlay_buffer, move_overlay, delete_overlay, is_overlay
    use frame_mod, only: selected_frame
    use callint_mod, only: this_command
    use fileio_mod, only: expand_file_name, file_name_directory, is_file_directory
    use files_mod, only: abbreviate_file_name
    implicit none(type, external)
    private

    public :: ediff_session_index, has_ediff_face_support, is_ediff_buffer_job, ediff_char_to_buffer_type, &
              ediff_buffer_type_name, set_ediff_overlay_face, move_ediff_overlay, ediff_background_face, &
              paint_ediff_background_regions, check_ediff_control_buffer, close_ediff_buffer_carefully, &
              make_ediff_session_entry, drop_ediff_session, init_ediff_init, is_ediff_ancestor_metajob, &
              is_ediff_collect_diffs_metajob, is_ediff_merge_metajob, is_ediff_metajob3, is_ediff_comparison_metajob3, &
              abbreviate_ediff_job_name, strip_ediff_last_dir

    ! ------------------------------------------------------------------
    ! Buffer types
    ! ------------------------------------------------------------------

    !> The buffer types of an Ediff session: buffer A, buffer B, buffer C
    !> (the third variant, or the merge buffer) and the ancestor buffer.
    !> Upstream's symbols A, B, C and Ancestor.
    integer, parameter, public :: EDIFF_BUFFER_TYPE_A = 1, EDIFF_BUFFER_TYPE_B = 2, EDIFF_BUFFER_TYPE_C = 3, &
                                  EDIFF_BUFFER_TYPE_ANCESTOR = 4

    !> The highlighting styles of a session (`ediff_highlighting_style'):
    !> faces, ASCII flags, or none (on a dumb terminal only).
    integer, parameter, public :: EDIFF_HIGHLIGHTING_STYLE_FACE = 1, EDIFF_HIGHLIGHTING_STYLE_ASCII = 2, &
                                  EDIFF_HIGHLIGHTING_STYLE_NONE = 3

    !> The state of a merge region (upstream's state-of-merge): how its
    !> text in buffer C, the merge buffer, was made: by the default
    !> variant (default-A, default-B, combined) or because only one
    !> variant differs from the ancestor (prefer-A, prefer-B); none outside
    !> merge jobs.  An internal enumeration: ediff_merg_mod maps the
    !> default variant option's constants to it.
    integer, parameter, public :: EDIFF_STATE_OF_MERGE_NONE = 0, EDIFF_STATE_OF_MERGE_DEFAULT_A = 1, &
                                  EDIFF_STATE_OF_MERGE_DEFAULT_B = 2, EDIFF_STATE_OF_MERGE_COMBINED = 3, &
                                  EDIFF_STATE_OF_MERGE_PREFER_A = 4, EDIFF_STATE_OF_MERGE_PREFER_B = 5

    ! ------------------------------------------------------------------
    ! Error messages
    ! ------------------------------------------------------------------

    !> Port of ediff-init.el ediff-KILLED-VITAL-BUFFER.
    character(len=*), parameter, public :: EDIFF_KILLED_VITAL_BUFFER = &
                                           "You have killed a vital Ediff buffer---you must leave Ediff now!"
    !> Port of ediff-init.el ediff-NO-DIFFERENCES.
    character(len=*), parameter, public :: EDIFF_NO_DIFFERENCES = &
                                           "Sorry, comparison of identical variants is not what I am made for..."

    ! ------------------------------------------------------------------
    ! Sessions
    ! ------------------------------------------------------------------

    abstract interface
        !> A function that splits the selected window for Ediff
        !> (`ediff_split_window_function'): NEW is the window it made.
        !> Not a port.
        subroutine ediff_split_window_procedure(new, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine ediff_split_window_procedure
    end interface

    public :: ediff_split_window_procedure

    !> A difference region in one buffer: upstream's difference record
    !> [overlay fine-diff-vector no-fine-diffs-flag state-of-diff], the
    !> fine differences not yet ported.  `state_of_diff' says, in 3-way
    !> jobs, which buffer differs from the other two ("=diff(A)"), "" if
    !> none.
    type, public :: ediff_difference_type
        type(handle_type) :: overlay
        character(len=12) :: state_of_diff = ""
    end type ediff_difference_type

    !> An old difference region a copy replaced (an entry of upstream's
    !> `ediff-killed-diffs-alist'): difference N
    !> (counted from 0) of buffer type BUFFER_TYPE held TEXT.
    type, public :: ediff_replaced_region_type
        integer :: n = -1
        integer :: buffer_type = 0
        character(len=:), allocatable :: text
    end type ediff_replaced_region_type

    !> The state of one merge region (an entry of upstream's
    !> `ediff-state-of-merge'): how buffer C's text was made
    !> (EDIFF_STATE_OF_MERGE_*), and whether the ancestor's region is
    !> empty.
    type, public :: ediff_merge_state_type
        integer :: state_of_merge = EDIFF_STATE_OF_MERGE_NONE
        logical :: state_of_ancestor = .false.
    end type ediff_merge_state_type

    !> One Ediff session, upstream's local variables of the control
    !> buffer.  Differences are counted from 0, as upstream counts them
    !> internally (the user sees N + 1); -1 is before the first one.
    type, public :: ediff_session_type
        !> The control buffer's serial number (0 for a free entry).
        integer(int64) :: serial = 0
        !> `ediff_control_buffer'.
        type(handle_type) :: control_buffer
        !> `ediff_buffer_a', `ediff_buffer_b', `ediff_buffer_c' and
        !> `ediff_ancestor_buffer', by buffer type.
        type(handle_type) :: buffers(4)
        !> `ediff_window_a', `_b', `_c', `_ancestor', by buffer type, and
        !> `ediff_control_window'.
        type(handle_type) :: windows(4), control_window
        !> The command that started the session (upstream's `ediff-job-name'
        !> as commands%ediff_files, commands%ediff_buffers3, ...).
        type(command_type), pointer :: job_name => null()
        !> The classification of the job (`ediff_3way_comparison_job',
        !> `ediff_merge_job', ...), set by `set_up_ediff'.
        logical :: three_way_comparison_job = .false., merge_job = .false., merge_with_ancestor_job = .false., &
                   three_way_job = .false., diff3_job = .false., narrow_job = .false., windows_job = .false., &
                   word_mode_job = .false.
        !> `ediff_word_mode': difference regions are words.
        logical :: word_mode = .false.
        !> The difference regions, (buffer type, difference + 1)
        !> (upstream's `ediff-difference-vector-A' and the others).
        type(ediff_difference_type), allocatable :: differences(:, :)
        !> `ediff_number_of_differences' and `ediff_current_difference'.
        integer :: number_of_differences = 0, current_difference = -1
        !> `ediff_current_diff_overlay_a' and the others.
        type(handle_type) :: current_diff_overlays(4)
        !> `ediff_wide_bounds', `ediff_narrow_bounds' and
        !> `ediff_visible_bounds': overlays by buffer type (A, B, C).
        type(handle_type) :: wide_bounds(3), narrow_bounds(3), visible_bounds(3)
        !> `ediff_diff_buffer' and `ediff_error_buffer'.
        type(handle_type) :: diff_buffer, error_buffer
        !> `ediff_meta_buffer': the session group this session belongs to
        !> (the null handle for none), and `ediff_meta_session_number', its
        !> number in that group.
        type(handle_type) :: meta_buffer
        integer :: meta_session_number = 0
        !> The regions copies replaced (upstream's `ediff-killed-diffs-alist').
        type(ediff_replaced_region_type), allocatable :: replaced_regions(:)
        !> `ediff_highlighting_style' (EDIFF_HIGHLIGHTING_STYLE_*).
        integer :: highlighting_style = EDIFF_HIGHLIGHTING_STYLE_ASCII
        !> The session's own values of `ediff_use_faces',
        !> `ediff_highlight_all_diffs', `ediff_use_long_help_message' and
        !> `ediff_keep_variants'.
        logical :: use_faces = .true., highlight_all_diffs = .true., use_long_help_message = .false., &
                   keep_variants = .true.
        !> The session's own `ediff_split_window_function'.
        procedure(ediff_split_window_procedure), pointer, nopass :: split_window_function => null()
        !> `ediff_ignore_case'.
        logical :: ignore_case = .false.
        !> `ediff_help_message', `ediff_long_help_message' and
        !> `ediff_brief_help_message'.
        character(len=:), allocatable :: help_message, long_help_message, brief_help_message
        !> `ediff_control_buffer_suffix' and `ediff_control_buffer_number'.
        character(len=:), allocatable :: control_buffer_suffix
        integer :: control_buffer_number = 0
        !> `ediff_window_config_saved'.
        character(len=:), allocatable :: window_config_saved
        !> `ediff_mode_map', the control buffer's own keymap.
        type(handle_type) :: keymap
        !> The state of each merge region (upstream's `ediff-state-of-merge';
        !> merge jobs only).
        type(ediff_merge_state_type), allocatable :: merge_states(:)
        !> The session's own `ediff_default_variant' (its state, with
        !> ediff_merg_mod's EDIFF_DEFAULT_VARIANT_* constants).
        type(choice_type) :: default_variant
        !> `ediff_merge_window_share': the share of the frame occupied by
        !> the merge window (buffer C).
        real :: merge_window_share = 0.45
        !> `ediff_merge_store_file': the file where the result of the
        !> merge is to be saved (unallocated for none).
        character(len=:), allocatable :: merge_store_file
        !> The session's own `ediff_autostore_merges' (its state).
        type(choice_type) :: autostore_merges
        !> The session's own `ediff_show_clashes_only' and
        !> `ediff_skip_merge_regions_that_differ_from_default'.
        logical :: show_clashes_only = .false., skip_merge_regions_that_differ_from_default = .false.
        !> The value `ediff_show_ancestor' had before
        !> `toggle_ediff_show_ancestor' changed it, restored on quitting
        !> (upstream's `ediff--show-ancestor-orig'; .false. also stands for
        !> none saved, as upstream's empty value does).
        logical :: show_ancestor_original = .false.
        !> `ediff_custom_diff_buffer': the output of
        !> `ediff_custom_diff_program'.
        type(handle_type) :: custom_diff_buffer
    end type ediff_session_type

    !> The Ediff sessions, one per control buffer (entries of closed
    !> control buffers are reused).
    type(ediff_session_type), allocatable, target, public :: ediff_sessions(:)

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> If true, Ediff will think that it is running on a display that
    !> supports faces.
    !> This is provided as a temporary relief for users of face-capable
    !> displays that Ediff doesn't know about.  Port of ediff-init.el
    !> ediff-force-faces.
    logical, public :: ediff_force_faces = .false.

    !> Flag placed before highlighted block of differences at beginning of a line.
    !> Port of ediff-init.el ediff-before-flag-bol.
    character(len=:), allocatable, public :: ediff_before_flag_bol
    !> Flag placed after highlighted block of differences that ends at end of line.
    !> Port of ediff-init.el ediff-after-flag-eol.
    character(len=:), allocatable, public :: ediff_after_flag_eol
    !> Flag placed before highlighted block of differences that starts mid-line.
    !> Port of ediff-init.el ediff-before-flag-mol.
    character(len=:), allocatable, public :: ediff_before_flag_mol
    !> Flag placed after a highlighted block of differences, if block ends in mid-line.
    !> Port of ediff-init.el ediff-after-flag-mol.
    character(len=:), allocatable, public :: ediff_after_flag_mol

    !> If true, differences are highlighted using faces, if device
    !> supports faces.
    !> If .false., differences are highlighted using ASCII flags,
    !> ediff-before-flag and ediff-after-flag.  On a non-window system,
    !> differences are always highlighted using ASCII flags.  Port of
    !> ediff-init.el ediff-use-faces (each session takes its own value
    !> when it starts).
    logical, public :: ediff_use_faces = .true.

    !> .false. means prompt to remove unmodified buffers A/B/C at session end.
    !> Supplying a prefix argument to the quit command \`q' temporarily
    !> reverses the meaning of this variable.  Port of ediff-init.el
    !> ediff-keep-variants.
    logical, public :: ediff_keep_variants = .true.

    !> If .false., only the selected differences are highlighted.
    !> Otherwise, all difference regions are highlighted, but the selected
    !> region is shown in brighter colors.  Port of ediff-init.el
    !> ediff-highlight-all-diffs (each session takes its own value when it
    !> starts).
    logical, public :: ediff_highlight_all_diffs = .true.

    !> Coding system for read to use when running the diff program as a
    !> subprocess.
    !> In most cases, the default will do.  Port of ediff-init.el
    !> ediff-coding-system-for-read.
    character(len=:), allocatable, public :: ediff_coding_system_for_read

    !> Coding system for write to use when writing out difference regions.
    !> This is used when writing to temp files in buffer jobs and when
    !> Ediff needs to find fine differences.  Port of ediff-init.el
    !> ediff-coding-system-for-write.
    character(len=:), allocatable, public :: ediff_coding_system_for_write

    !> A true value makes all buffers read-only at startup.
    !> If .false., only the buffers whose file is read-only and
    !> ancestor buffers are made read-only.  Port of ediff-init.el
    !> ediff-make-buffers-readonly-at-startup.
    logical, public :: ediff_make_buffers_readonly_at_startup = .false.

    !> If true, show ancestor buffer in 3way merges and refine it.
    !> Port of ediff-init.el ediff-show-ancestor (the refinement is
    !> Deferred with fine differences, see docs/status/ediff_diff_mod.md).
    logical, public :: ediff_show_ancestor = .true.

    !> Values of `ediff_autostore_merges' besides .true. and .false.
    integer, parameter, public :: EDIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY = 2

    !> Save the results of merge jobs automatically.
    !> With value .false., don't save automatically.  With value .true.,
    !> always save.  EDIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY (the default)
    !> means save automatically only if the merge job is part of a group
    !> of jobs, such as `ediff_merge_directory' or
    !> `ediff_merge_directory_revisions'.  Port of ediff-init.el
    !> ediff-autostore-merges (each session takes its own value when it
    !> starts).
    type(choice_setting_type), public :: ediff_autostore_merges = &
                                         choice_setting_type(name="ediff_autostore_merges", &
                                                             choice=EDIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY)

    !> If .true., C-h invokes Fortmacs help in the control buffer.
    !> If .false., C-h jumps to the previous difference instead.  Port of
    !> ediff-init.el ediff-no-emacs-help-in-control-buffer (named
    !> positively, the value inverted).
    logical, public :: ediff_fortmacs_help_in_control_buffer = .true.

    !> Mode for Ediff temporary files.  Port of ediff-init.el
    !> ediff-temp-file-mode.
    integer, public :: ediff_temp_file_mode = int(o"600")

    !> Prefix to be attached to saved merge buffers.  Port of ediff-init.el
    !> ediff-merge-filename-prefix.
    character(len=:), allocatable, public :: ediff_merge_filename_prefix

    ! ------------------------------------------------------------------
    ! Hooks
    ! ------------------------------------------------------------------

    !> Hooks to run before Ediff begins to set up windows and buffers.
    !> This hook can be used to save the previous window config, which
    !> can be restored on `quit_ediff'.  Port of ediff-init.el
    !> ediff-before-setup-hook.
    type(hook_type), target, public :: ediff_before_setup_hook = hook_type(name="ediff_before_setup_hook")
    !> Hooks to run before Ediff sets its window configuration.
    !> This hook is run every time when Ediff arranges its windows.
    !> This happens each time Ediff detects that the windows were messed
    !> up by the user.  Port of ediff-init.el ediff-before-setup-windows-hook.
    type(hook_type), target, public :: ediff_before_setup_windows_hook = &
                                       hook_type(name="ediff_before_setup_windows_hook")
    !> Hooks to run after Ediff sets its window configuration.
    !> This can be used to set up control window or icon in a desired
    !> place.  Port of ediff-init.el ediff-after-setup-windows-hook.
    type(hook_type), target, public :: ediff_after_setup_windows_hook = &
                                       hook_type(name="ediff_after_setup_windows_hook")
    !> Hooks to run in the control buffer after Ediff has been set up and
    !> is ready.  Port of ediff-init.el ediff-startup-hook.
    type(hook_type), target, public :: ediff_startup_hook = hook_type(name="ediff_startup_hook")
    !> Hooks to run after a difference has been selected.  Port of
    !> ediff-init.el ediff-select-hook.
    type(hook_type), target, public :: ediff_select_hook = hook_type(name="ediff_select_hook")
    !> Hooks to run after a difference has been unselected.  Port of
    !> ediff-init.el ediff-unselect-hook.
    type(hook_type), target, public :: ediff_unselect_hook = hook_type(name="ediff_unselect_hook")
    !> Hooks run after buffers A, B, and C are set up.
    !> For each buffer, the hooks are run with that buffer made current.
    !> Port of ediff-init.el ediff-prepare-buffer-hook.
    type(hook_type), target, public :: ediff_prepare_buffer_hook = hook_type(name="ediff_prepare_buffer_hook")
    !> Hook run just after `ediff_mode' is set up in the control buffer.
    !> This is done before any windows or frames are created.  One can use
    !> it to set local variables that determine how the display looks
    !> like.  Port of ediff-init.el ediff-mode-hook.
    type(hook_type), target, public :: ediff_mode_hook = hook_type(name="ediff_mode_hook")
    !> Hook run just after the default bindings in Ediff keymap are set
    !> up.  Port of ediff-init.el ediff-keymap-setup-hook.
    type(hook_type), target, public :: ediff_keymap_setup_hook = hook_type(name="ediff_keymap_setup_hook")
    !> Hooks run after preparing the help message.  Port of ediff-init.el
    !> ediff-display-help-hook.
    type(hook_type), target, public :: ediff_display_help_hook = hook_type(name="ediff_display_help_hook")
    !> Hooks to run in the Ediff control buffer after finishing Ediff.
    !> Port of ediff-init.el ediff-quit-hook (its default function,
    !> `clean_up_ediff_mess', is added by ediff_util_mod).
    type(hook_type), target, public :: ediff_quit_hook = hook_type(name="ediff_quit_hook")
    !> Hooks to run on exiting Ediff, before killing the control and
    !> variant buffers.  Port of ediff-init.el ediff-cleanup-hook.
    type(hook_type), target, public :: ediff_cleanup_hook = hook_type(name="ediff_cleanup_hook")

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Face for highlighting the selected difference in buffer A.
    type(named_face_type), target, public :: ediff_current_diff_a
    !> Face for highlighting the selected difference in buffer B.
    type(named_face_type), target, public :: ediff_current_diff_b
    !> Face for highlighting the selected difference in buffer C.
    type(named_face_type), target, public :: ediff_current_diff_c
    !> Face for highlighting the selected difference in buffer Ancestor.
    type(named_face_type), target, public :: ediff_current_diff_ancestor
    !> Face for highlighting the refinement of the selected diff in buffer A.
    type(named_face_type), target, public :: ediff_fine_diff_a
    !> Face for highlighting the refinement of the selected diff in buffer B.
    type(named_face_type), target, public :: ediff_fine_diff_b
    !> Face for highlighting the refinement of the selected diff in buffer C.
    type(named_face_type), target, public :: ediff_fine_diff_c
    !> Face for highlighting refinement of the selected diff in the ancestor buffer.
    type(named_face_type), target, public :: ediff_fine_diff_ancestor
    !> Face for highlighting even-numbered non-current differences in buffer A.
    type(named_face_type), target, public :: ediff_even_diff_a
    !> Face for highlighting even-numbered non-current differences in buffer B.
    type(named_face_type), target, public :: ediff_even_diff_b
    !> Face for highlighting even-numbered non-current differences in buffer C.
    type(named_face_type), target, public :: ediff_even_diff_c
    !> Face for highlighting even-numbered non-current differences in ancestor buffer.
    type(named_face_type), target, public :: ediff_even_diff_ancestor
    !> Face for highlighting odd-numbered non-current differences in buffer A.
    type(named_face_type), target, public :: ediff_odd_diff_a
    !> Face for highlighting odd-numbered non-current differences in buffer B.
    type(named_face_type), target, public :: ediff_odd_diff_b
    !> Face for highlighting odd-numbered non-current differences in buffer C.
    type(named_face_type), target, public :: ediff_odd_diff_c
    !> Face for highlighting odd-numbered non-current differences in ancestor buffer.
    type(named_face_type), target, public :: ediff_odd_diff_ancestor

    !> The faces' names by buffer type (`ediff-current-diff-face-alist'
    !> and the others).
    character(len=*), parameter, public :: EDIFF_CURRENT_DIFF_FACES(4) = &
                                           [character(len=27) :: "ediff_current_diff_a", "ediff_current_diff_b", &
                                                                  "ediff_current_diff_c", "ediff_current_diff_ancestor"]
    character(len=*), parameter :: EVEN_DIFF_FACES(4) = &
                                   [character(len=24) :: "ediff_even_diff_a", "ediff_even_diff_b", "ediff_even_diff_c", &
                                                          "ediff_even_diff_ancestor"]
    character(len=*), parameter :: ODD_DIFF_FACES(4) = &
                                   [character(len=23) :: "ediff_odd_diff_a", "ediff_odd_diff_b", "ediff_odd_diff_c", &
                                                          "ediff_odd_diff_ancestor"]

contains

    !> The index in `ediff_sessions' of the session whose control buffer
    !> is BUFFER (the current buffer if absent), 0 if it is none.
    !> Not a port: upstream's session variables are the control buffer's.
    integer function ediff_session_index(buffer) result(k)
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: serial

        k = 0
        if (.not. allocated(ediff_sessions)) return

        if (present(buffer)) then
            if (.not. is_buffer_live(buffer)) return
            serial = buffers(buffer%index)%serial
        else
            if (current == 0) return
            serial = buffers(current)%serial
        end if

        k = findloc(ediff_sessions%serial, serial, dim=1)
    end function ediff_session_index

    !> Find or make the session entry of control buffer BUFFER, reusing
    !> the entry of a closed control buffer; K is its index.  Not a port.
    subroutine make_ediff_session_entry(buffer, k)
        type(handle_type), intent(in) :: buffer
        integer, intent(out) :: k
        type(ediff_session_type) :: entry

        k = ediff_session_index(buffer)
        if (k > 0) return
        if (.not. allocated(ediff_sessions)) allocate (ediff_sessions(0))

        do k = 1, size(ediff_sessions)
            if (.not. buffer_serial_live(ediff_sessions(k)%serial)) exit
        end do

        if (k > size(ediff_sessions)) ediff_sessions = [ediff_sessions, entry]
        ediff_sessions(k) = entry
        ediff_sessions(k)%serial = buffers(buffer%index)%serial
        ediff_sessions(k)%control_buffer = buffer
    end subroutine make_ediff_session_entry

    !> Forget session K (its control buffer is about to close).
    !> Not a port.
    subroutine drop_ediff_session(k)
        integer, intent(in) :: k
        type(ediff_session_type) :: entry

        if (k < 1 .or. k > size(ediff_sessions)) return
        ediff_sessions(k) = entry
    end subroutine drop_ediff_session

    !> Whether the display supports faces: a graphic display, a color
    !> terminal, or `ediff_force_faces'.  Port of
    !> ediff-init.el:ediff-has-face-support-p.
    logical function has_ediff_face_support()
        has_ediff_face_support = associated(selected_frame%rif) .or. ediff_force_faces .or. &
                                 selected_frame%tty%tn_max_colors > 0
    end function has_ediff_face_support

    !> Whether the job JOB_NAME compares or merges buffers (its name has
    !> "buffer" in it upstream).  Not a port: ediff-util.el and
    !> ediff-diff.el test (string-match "buffer" (symbol-name ediff-job-name)).
    logical function is_ediff_buffer_job(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_ediff_buffer_job = associated(job_name, commands%ediff_buffers) .or. associated(job_name, commands%ediff_buffers3) &
                              .or. associated(job_name, commands%ediff_merge_buffers) .or. &
                              associated(job_name, commands%ediff_merge_buffers_with_ancestor)
    end function is_ediff_buffer_job

    !> The buffer type a key names: a or A is buffer A, b or B buffer B,
    !> c or C buffer C; 0 for another character.  Port of
    !> ediff-init.el:ediff-char-to-buftype.
    pure integer function ediff_char_to_buffer_type(c) result(buffer_type)
        character, intent(in) :: c

        select case (c)
        case ("a", "A")
            buffer_type = EDIFF_BUFFER_TYPE_A
        case ("b", "B")
            buffer_type = EDIFF_BUFFER_TYPE_B
        case ("c", "C")
            buffer_type = EDIFF_BUFFER_TYPE_C
        case default
            buffer_type = 0
        end select
    end function ediff_char_to_buffer_type

    !> The name of BUFFER_TYPE as messages show it: A, B, C or Ancestor.
    !> Not a port: upstream prints the symbol.
    pure function ediff_buffer_type_name(buffer_type) result(name)
        integer, intent(in) :: buffer_type
        character(len=:), allocatable :: name

        select case (buffer_type)
        case (EDIFF_BUFFER_TYPE_A)
            name = "A"
        case (EDIFF_BUFFER_TYPE_B)
            name = "B"
        case (EDIFF_BUFFER_TYPE_C)
            name = "C"
        case default
            name = "Ancestor"
        end select
    end function ediff_buffer_type_name

    !> Give OVERLAY the face named FACE ("" for none).  Port of
    !> ediff-init.el:ediff-set-overlay-face (the `help-echo' text is
    !> deferred with the mouse).
    subroutine set_ediff_overlay_face(overlay, face, status)
        type(handle_type), intent(in) :: overlay
        character(len=*), intent(in) :: face
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. is_buffer_live(overlay_buffer(overlay))) then
            call delete_overlay(overlay)
            return
        end if

        if (len(face) == 0) then
            call overlay_put(overlay, "face", value_type(), status)
        else
            call overlay_put(overlay, "face", value_type(name=face), status)
        end if
    end subroutine set_ediff_overlay_face

    !> If OVERLAY's buffer exists, move OVERLAY to BEGINNING-END; delete it
    !> otherwise.  Port of ediff-init.el:ediff-move-overlay.
    subroutine move_ediff_overlay(overlay, beginning, end, status)
        type(handle_type), intent(in) :: overlay
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_overlay(overlay)) return

        if (is_buffer_live(overlay_buffer(overlay))) then
            call move_overlay(overlay, beginning, end, status)
        else
            call delete_overlay(overlay)
        end if
    end subroutine move_ediff_overlay

    !> The face for non-current difference DIFF_NUM (counted from 0) of
    !> BUFFER_TYPE: the even face when the number the user sees is even.
    !> Port of ediff-init.el:ediff-background-face.
    pure function ediff_background_face(buffer_type, diff_num) result(face)
        integer, intent(in) :: buffer_type, diff_num
        character(len=:), allocatable :: face

        ! The value of diff_num is always 1- the one that user sees.
        ! This is why even face is used when diff_num is odd.
        if (modulo(diff_num, 2) == 1) then
            face = trim(EVEN_DIFF_FACES(buffer_type))
        else
            face = trim(ODD_DIFF_FACES(buffer_type))
        end if
    end function ediff_background_face

    !> Activate faces on the difference regions of session K in all
    !> buffers, or with UNHIGHLIGHT remove them.  Port of
    !> ediff-init.el:ediff-paint-background-regions with
    !> ediff-paint-background-regions-in-one-buffer.
    subroutine paint_ediff_background_regions(k, unhighlight, status)
        integer, intent(in) :: k
        logical, intent(in) :: unhighlight
        type(status_type), intent(out) :: status
        integer :: buffer_type, n

        status = status_type()
        if (.not. allocated(ediff_sessions(k)%differences)) return

        do buffer_type = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_ANCESTOR
            do n = 1, size(ediff_sessions(k)%differences, 2)
                associate (overlay => ediff_sessions(k)%differences(buffer_type, n)%overlay)
                    ! Only if overlay is alive.
                    if (.not. is_buffer_live(overlay_buffer(overlay))) cycle

                    if (unhighlight) then
                        call set_ediff_overlay_face(overlay, "", status)
                    else
                        call set_ediff_overlay_face(overlay, ediff_background_face(buffer_type, n - 1), status)
                    end if
                end associate

                if (.not. status%ok()) return
            end do
        end do
    end subroutine paint_ediff_background_regions

    !> Give in K the current buffer's session; a `user_error' if the
    !> current buffer is no Ediff control buffer.  Port of
    !> ediff-init.el:ediff-barf-if-not-control-buffer with
    !> ediff-in-control-buffer-p (the meta buffer case waits for
    !> ediff-mult.el).
    subroutine check_ediff_control_buffer(k, status)
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        status = status_type()
        k = ediff_session_index()
        if (k > 0) return
        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        call set_error(status, FM_USER_ERROR, name//": This command runs in Ediff Control Buffer only!")
    end subroutine check_ediff_control_buffer

    !> Kill buffer BUFFER if it exists.  Port of
    !> ediff-init.el:ediff-kill-buffer-carefully.
    recursive subroutine close_ediff_buffer_carefully(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_buffer_live(buffer)) call close_buffer(buffer, status)
    end subroutine close_ediff_buffer_carefully

    !> Whether session group job JOB_NAME merges with ancestors.  Port of
    !> ediff-init.el:ediff-ancestor-metajob (the file group job is
    !> Deferred).
    logical function is_ediff_ancestor_metajob(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_ediff_ancestor_metajob = associated(job_name, commands%ediff_merge_directories_with_ancestor)
    end function is_ediff_ancestor_metajob

    !> Whether session group job JOB_NAME can collect the custom diffs of
    !> its sessions into a multifile patch.  Port of
    !> ediff-init.el:ediff-collect-diffs-metajob (the revision jobs are
    !> Deferred).
    logical function is_ediff_collect_diffs_metajob(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_ediff_collect_diffs_metajob = associated(job_name, commands%ediff_directories) .or. &
                                         associated(job_name, commands%ediff_merge_directories) .or. &
                                         associated(job_name, commands%ediff_merge_directories_with_ancestor)
    end function is_ediff_collect_diffs_metajob

    !> Whether session group job JOB_NAME merges.  Port of
    !> ediff-init.el:ediff-merge-metajob (the revision and file group jobs
    !> are Deferred).
    logical function is_ediff_merge_metajob(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_ediff_merge_metajob = associated(job_name, commands%ediff_merge_directories) .or. &
                                 associated(job_name, commands%ediff_merge_directories_with_ancestor)
    end function is_ediff_merge_metajob

    !> Whether the sessions of session group job JOB_NAME involve three
    !> files.  Port of ediff-init.el:ediff-metajob3 (the file group jobs
    !> are Deferred).
    logical function is_ediff_metajob3(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_ediff_metajob3 = associated(job_name, commands%ediff_merge_directories_with_ancestor) .or. &
                            associated(job_name, commands%ediff_directories3)
    end function is_ediff_metajob3

    !> Whether session group job JOB_NAME compares three files.  Port of
    !> ediff-init.el:ediff-comparison-metajob3 (the file group job is
    !> Deferred).
    logical function is_ediff_comparison_metajob3(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_ediff_comparison_metajob3 = associated(job_name, commands%ediff_directories3)
    end function is_ediff_comparison_metajob3

    !> What the session group and registry buffers call job JOB_NAME
    !> ("Compare two directories"), "" for a job not ported.  Port of
    !> ediff-init.el:ediff-abbrev-jobname (the jobs it names by
    !> capitalizing the words of their names are spelled out).
    function abbreviate_ediff_job_name(job_name) result(text)
        type(command_type), pointer, intent(in) :: job_name
        character(len=:), allocatable :: text

        if (associated(job_name, commands%ediff_directories)) then
            text = "Compare two directories"
        else if (associated(job_name, commands%ediff_files)) then
            text = "Compare two files"
        else if (associated(job_name, commands%ediff_buffers)) then
            text = "Compare two buffers"
        else if (associated(job_name, commands%ediff_directories3)) then
            text = "Compare three directories"
        else if (associated(job_name, commands%ediff_files3)) then
            text = "Compare three files"
        else if (associated(job_name, commands%ediff_buffers3)) then
            text = "Compare three buffers"
        else if (associated(job_name, commands%ediff_merge_files)) then
            text = "Merge Files"
        else if (associated(job_name, commands%ediff_merge_buffers)) then
            text = "Merge Buffers"
        else if (associated(job_name, commands%ediff_merge_files_with_ancestor)) then
            text = "Merge Files With Ancestor"
        else if (associated(job_name, commands%ediff_merge_buffers_with_ancestor)) then
            text = "Merge Buffers With Ancestor"
        else if (associated(job_name, commands%ediff_merge_directories)) then
            text = "Merge Directories"
        else if (associated(job_name, commands%ediff_merge_directories_with_ancestor)) then
            text = "Merge Directories With Ancestor"
        else
            text = ""
        end if
    end function abbreviate_ediff_job_name

    !> The parent directory of DIR (`default_directory' if DIR is "");
    !> does nothing to "/".  If DIR is a regular file, strip the file AND
    !> the last dir.  Port of ediff-init.el:ediff-strip-last-dir.
    subroutine strip_ediff_last_dir(dir_in, result, status)
        character(len=*), intent(in) :: dir_in
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer :: pos

        result = ""
        dir = dir_in

        if (len(dir) == 0) then
            if (current > 0) then
                if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
            end if
        end if

        call expand_file_name(dir, result, status)
        if (.not. status%ok()) return
        dir = result
        if (.not. is_file_directory(dir)) dir = file_name_directory(dir)
        pos = len(dir)

        if (pos > 1) then
            if (dir(pos:pos) == "/") dir = dir(1:pos - 1)
        end if

        result = abbreviate_file_name(file_name_directory(dir))
    end subroutine strip_ediff_last_dir

    !> Define the faces of ediff-init.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => ediff_current_diff_a
        call f%define("ediff_current_diff_a", "Face for highlighting the selected difference in buffer A.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#ffdddd", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#553333", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="firebrick", background="pale green", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="blue3", background="yellow3", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true., extend=.true.))
        f => ediff_current_diff_b
        call f%define("ediff_current_diff_b", "Face for highlighting the selected difference in buffer B.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#ddffdd", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#335533", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="DarkOrchid", background="Yellow", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="magenta3", background="yellow3", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true., extend=.true.))
        f => ediff_current_diff_c
        call f%define("ediff_current_diff_c", "Face for highlighting the selected difference in buffer C.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#ffffaa", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#888833", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Navy", background="Pink", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="cyan3", background="yellow3", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true., extend=.true.))
        f => ediff_current_diff_ancestor
        call f%define("ediff_current_diff_ancestor", "Face for highlighting the selected difference in buffer Ancestor.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#cfdeee", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#004151", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#cfdeee", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#004151", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="black", background="magenta3", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true., extend=.true.))
        f => ediff_fine_diff_a
        call f%define("ediff_fine_diff_a", "Face for highlighting the refinement of the selected diff in buffer A.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#ffbbbb"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#882222"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Navy", background="sky blue"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="white", background="sky blue", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(underline=.true.))
        f => ediff_fine_diff_b
        call f%define("ediff_fine_diff_b", "Face for highlighting the refinement of the selected diff in buffer B.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#aaffaa"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#228822"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Black", background="cyan"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), face_attrs(foreground="magenta3", background="cyan3"))
        call f%spec_entry(display_when(), face_attrs(underline=.true.))
        f => ediff_fine_diff_c
        call f%define("ediff_fine_diff_c", "Face for highlighting the refinement of the selected diff in buffer C.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#ffff55"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#aaaa22"))
        call f%spec_entry(display_when(types=["pc"]), face_attrs(foreground="white", background="Turquoise"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Black", background="Turquoise"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="yellow3", background="Turquoise", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(underline=.true.))
        f => ediff_fine_diff_ancestor
        call f%define("ediff_fine_diff_ancestor", &
                      "Face for highlighting refinement of the selected diff in the ancestor buffer."//achar(10)// &
                      "At present, this face is not used and no fine differences are"//achar(10)// &
                      "computed for the ancestor buffer.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#00c5c0"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#009591"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="#00c5c0"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(background="#009591"))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), face_attrs(foreground="red3", background="green"))
        call f%spec_entry(display_when(), face_attrs(underline=.true.))
        call define_background_faces()
    end subroutine define_faces

    !> Define the even and odd faces of ediff-init.el.  Not a port.
    subroutine define_background_faces()
        type(named_face_type), pointer :: f

        f => ediff_even_diff_a
        call f%define("ediff_even_diff_a", "Face for highlighting even-numbered non-current differences in buffer A.")
        call f%spec_entry(display_when(types=["pc"]), &
                          face_attrs(foreground="green3", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dark grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Black", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="red3", background="light grey", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_even_diff_b
        call f%define("ediff_even_diff_b", "Face for highlighting even-numbered non-current differences in buffer B.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dim grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="White", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="blue3", background="Grey", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_even_diff_c
        call f%define("ediff_even_diff_c", "Face for highlighting even-numbered non-current differences in buffer C.")
        call f%spec_entry(display_when(types=["pc"]), &
                          face_attrs(foreground="yellow3", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dark grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Black", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="yellow3", background="light grey", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_even_diff_ancestor
        call f%define("ediff_even_diff_ancestor", &
                      "Face for highlighting even-numbered non-current differences in ancestor buffer.")
        call f%spec_entry(display_when(types=["pc"]), &
                          face_attrs(foreground="cyan3", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dim grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="White", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="cyan3", background="light grey", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_odd_diff_a
        call f%define("ediff_odd_diff_a", "Face for highlighting odd-numbered non-current differences in buffer A.")
        call f%spec_entry(display_when(types=["pc"]), &
                          face_attrs(foreground="green3", background="gray40", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dim grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="White", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="red3", background="black", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_odd_diff_b
        call f%define("ediff_odd_diff_b", "Face for highlighting odd-numbered non-current differences in buffer B.")
        call f%spec_entry(display_when(types=["pc"]), &
                          face_attrs(foreground="White", background="gray40", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dark grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="Black", background="light grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="cyan3", background="black", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_odd_diff_c
        call f%define("ediff_odd_diff_c", "Face for highlighting odd-numbered non-current differences in buffer C.")
        call f%spec_entry(display_when(types=["pc"]), &
                          face_attrs(foreground="yellow3", background="gray40", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(distant_foreground="Black", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88, backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(distant_foreground="White", background="dim grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="White", background="Grey", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="yellow3", background="black", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
        f => ediff_odd_diff_ancestor
        call f%define("ediff_odd_diff_ancestor", &
                      "Face for highlighting odd-numbered non-current differences in ancestor buffer.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=88), &
                          face_attrs(background="gray40", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], min_colors=16), &
                          face_attrs(foreground="cyan3", background="gray40", extend=.true.))
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                          face_attrs(foreground="green3", background="black", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(slant="italic", extend=.true.))
    end subroutine define_background_faces

    !> Define the faces and give the string options their defaults.
    !> Not a port.
    subroutine init_ediff_init()
        if (len_trim(ediff_current_diff_a%name) > 0) return
        call define_faces()
        if (.not. allocated(ediff_before_flag_bol)) ediff_before_flag_bol = "->>"
        if (.not. allocated(ediff_after_flag_eol)) ediff_after_flag_eol = "<<-"
        if (.not. allocated(ediff_before_flag_mol)) ediff_before_flag_mol = "->>"
        if (.not. allocated(ediff_after_flag_mol)) ediff_after_flag_mol = "<<-"
        if (.not. allocated(ediff_coding_system_for_read)) ediff_coding_system_for_read = "raw-text"
        if (.not. allocated(ediff_coding_system_for_write)) ediff_coding_system_for_write = "emacs-internal"
        if (.not. allocated(ediff_merge_filename_prefix)) ediff_merge_filename_prefix = "merge_"
    end subroutine init_ediff_init
end module ediff_init_mod
