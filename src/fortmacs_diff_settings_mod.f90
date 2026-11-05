! fortmacs_diff_settings_mod.f90 -- Ediff's variables, faces and hooks.
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
!> ancestor) as named constants, and one `fortmacs_diff_session_type' per
!> control buffer in `fortmacs_diff_sessions', holding what upstream keeps in
!> the control buffer's local variables (`fortmacs_diff_buffer_a', the
!> difference vectors, the current difference, the windows, ...).
!>
!> The arrays of a session indexed by buffer type (`buffers', `windows',
!> `differences', the bounds and the current-difference overlays) have
!> room for all four types, so merge jobs (buffer C the merge buffer, the
!> ancestor) and session groups (`meta_buffer', fortmacs_diff_multiple_mod) fit.
module fortmacs_diff_settings_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, command_type, choice_type
    use data_mod, only: choice_setting_type
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, current_buffer, is_buffer_live, buffer_serial_live, close_buffer, &
                          header_line_format_type
    use hooks_mod, only: hook_type
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use interval_tree_mod, only: overlay_put, overlay_buffer, move_overlay, delete_overlay, is_overlay
    use frame_mod, only: selected_frame
    use execute_interactively_mod, only: this_command
    use file_input_output_mod, only: expand_file_name, file_name_directory, is_file_directory
    use files_mod, only: abbreviate_file_name
    implicit none(type, external)
    private

    public :: fortmacs_diff_session_index, has_fortmacs_diff_face_support, is_fortmacs_diff_buffer_job, &
              fortmacs_diff_character_to_buffer_type, fortmacs_diff_buffer_type_name, set_fortmacs_diff_overlay_face, &
              move_fortmacs_diff_overlay, fortmacs_diff_background_face, check_fortmacs_diff_control_buffer, &
              close_fortmacs_diff_buffer_carefully, make_fortmacs_diff_session_entry, drop_fortmacs_diff_session, &
              initialize_fortmacs_diff_initialize, is_fortmacs_diff_ancestor_metajob, is_fortmacs_diff_collect_diffs_metajob, &
              is_fortmacs_diff_merge_metajob, is_fortmacs_diff_metajob3, is_fortmacs_diff_comparison_metajob3, &
              abbreviate_fortmacs_diff_job_name, strip_fortmacs_diff_last_directory

    ! ------------------------------------------------------------------
    ! Buffer types
    ! ------------------------------------------------------------------

    !> The buffer types of an Ediff session: buffer A, buffer B, buffer C
    !> (the third variant, or the merge buffer) and the ancestor buffer.
    !> Upstream's symbols A, B, C and Ancestor.
    integer, parameter, public :: FORTMACS_DIFF_BUFFER_TYPE_A = 1, FORTMACS_DIFF_BUFFER_TYPE_B = 2, &
                                  FORTMACS_DIFF_BUFFER_TYPE_C = 3, &
                                  FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR = 4

    !> The FLAG values of `unselect_and_select_difference' of
    !> `fortmacs_diff_session_type': only select, or only unselect.
    integer, parameter, public :: FORTMACS_DIFF_SELECT_ONLY = 1, FORTMACS_DIFF_UNSELECT_ONLY = -1

    !> The highlighting styles of a session (`fortmacs_diff_highlighting_style'):
    !> faces, ASCII flags, or none (on a dumb terminal only).
    integer, parameter, public :: FORTMACS_DIFF_HIGHLIGHTING_STYLE_FACE = 1, FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII = 2, &
                                  FORTMACS_DIFF_HIGHLIGHTING_STYLE_NONE = 3

    !> The state of a merge region (upstream's state-of-merge): how its
    !> text in buffer C, the merge buffer, was made: by the default
    !> variant (default-A, default-B, combined) or because only one
    !> variant differs from the ancestor (prefer-A, prefer-B); none outside
    !> merge jobs.  An internal enumeration: fortmacs_diff_merging_mod maps the
    !> default variant option's constants to it.
    integer, parameter, public :: FORTMACS_DIFF_STATE_OF_MERGE_NONE = 0, FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_A = 1, &
                                  FORTMACS_DIFF_STATE_OF_MERGE_DEFAULT_B = 2, FORTMACS_DIFF_STATE_OF_MERGE_COMBINED = 3, &
                                  FORTMACS_DIFF_STATE_OF_MERGE_PREFER_A = 4, FORTMACS_DIFF_STATE_OF_MERGE_PREFER_B = 5

    ! ------------------------------------------------------------------
    ! Error messages
    ! ------------------------------------------------------------------

    !> Port of ediff-init.el ediff-KILLED-VITAL-BUFFER.
    character(len=*), parameter, public :: FORTMACS_DIFF_KILLED_VITAL_BUFFER = &
                                           "You have killed a vital Ediff buffer---you must leave Ediff now!"
    !> Port of ediff-init.el ediff-NO-DIFFERENCES.
    character(len=*), parameter, public :: FORTMACS_DIFF_NO_DIFFERENCES = &
                                           "Sorry, comparison of identical variants is not what I am made for..."

    ! ------------------------------------------------------------------
    ! Sessions
    ! ------------------------------------------------------------------

    abstract interface
        !> A function that splits the selected window for Ediff
        !> (`fortmacs_diff_split_window_function'): NEW is the window it made.
        !> Not a port.
        subroutine fortmacs_diff_split_window_procedure(new, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine fortmacs_diff_split_window_procedure
    end interface

    public :: fortmacs_diff_split_window_procedure

    !> A difference region in one buffer: upstream's difference record
    !> [overlay fine-diff-vector no-fine-diffs-flag state-of-diff], the
    !> fine differences not yet ported.  `state_of_diff' says, in 3-way
    !> jobs, which buffer differs from the other two ("=diff(A)"), "" if
    !> none.
    type, public :: fortmacs_diff_difference_type
        type(handle_type) :: overlay
        character(len=12) :: state_of_diff = ""
    end type fortmacs_diff_difference_type

    !> An old difference region a copy replaced (an entry of upstream's
    !> `ediff-killed-diffs-alist'): difference N
    !> (counted from 0) of buffer type BUFFER_TYPE held TEXT.
    type, public :: fortmacs_diff_replaced_region_type
        integer :: n = -1
        integer :: buffer_type = 0
        character(len=:), allocatable :: text
    end type fortmacs_diff_replaced_region_type

    !> The state of one merge region (an entry of upstream's
    !> `ediff-state-of-merge'): how buffer C's text was made
    !> (FORTMACS_DIFF_STATE_OF_MERGE_*), and whether the ancestor's region is
    !> empty.
    type, public :: fortmacs_diff_merge_state_type
        integer :: state_of_merge = FORTMACS_DIFF_STATE_OF_MERGE_NONE
        logical :: state_of_ancestor = .false.
    end type fortmacs_diff_merge_state_type

    !> A difference region as diff reports it, in all the buffers of a
    !> job: upstream's vector [a-beg a-end b-beg b-end c-beg c-end anc-beg
    !> anc-end state-of-diff state-of-merge state-of-ancestor], positions by
    !> buffer type (0 for a buffer the job does not have).
    !> `state_of_diff' is the buffer type that differs from the other two
    !> (3-way jobs), 0 if none; `state_of_merge' how the region of buffer C
    !> is made in merge jobs (FORTMACS_DIFF_STATE_OF_MERGE_*); `state_of_ancestor'
    !> whether the ancestor's region is empty.
    type :: fortmacs_diff_diff_record_type
        integer(int64) :: beginning(4) = 0, end(4) = 0
        integer :: state_of_diff = 0
        integer :: state_of_merge = FORTMACS_DIFF_STATE_OF_MERGE_NONE
        logical :: state_of_ancestor = .false.
    end type fortmacs_diff_diff_record_type
    !> One Ediff session, upstream's local variables of the control
    !> buffer.  Differences are counted from 0, as upstream counts them
    !> internally (the user sees N + 1); -1 is before the first one.
    !>
    !> The type-bound procedures act on one session and are called on its
    !> entry of `fortmacs_diff_sessions', the session index (panel 14):
    !> `call fortmacs_diff_sessions(session_index)%quit(.false., status)'.
    !> That array grows, and may move, when a session starts, which a hook
    !> can do; so a subroutine that can get there finds its entry again by
    !> the serial number on entry (`call self%get_index(session_index, status)',
    !> a `user_error' if the entry is gone) and works on
    !> `fortmacs_diff_sessions(session_index)', never on SELF after that.  A
    !> function, which has no side effects, and a subroutine that calls
    !> nothing that can run user code read SELF.  The procedures are in the
    !> submodules named for their upstream file: fortmacs_diff_settings_diff_mod,
    !> _merging_mod, _utilities_mod, _window_mod and _help_mod.
    type, public :: fortmacs_diff_session_type
        !> The control buffer's serial number (0 for a free entry).
        integer(int64) :: serial = 0
        !> `fortmacs_diff_control_buffer'.
        type(handle_type) :: control_buffer
        !> `fortmacs_diff_buffer_a', `fortmacs_diff_buffer_b', `fortmacs_diff_buffer_c' and
        !> `fortmacs_diff_ancestor_buffer', by buffer type.
        type(handle_type) :: buffers(4)
        !> `fortmacs_diff_window_a', `_b', `_c', `_ancestor', by buffer type, and
        !> `fortmacs_diff_control_window'.
        type(handle_type) :: windows(4), control_window
        !> The command that started the session (upstream's `ediff-job-name'
        !> as commands%fortmacs_diff_files, commands%fortmacs_diff_buffers3, ...).
        type(command_type), pointer :: job_name => null()
        !> The classification of the job (`fortmacs_diff_3way_comparison_job',
        !> `fortmacs_diff_merge_job', ...), set by `set_up_fortmacs_diff'.
        logical :: three_way_comparison_job = .false., merge_job = .false., merge_with_ancestor_job = .false., &
                   three_way_job = .false., diff3_job = .false., narrow_job = .false., windows_job = .false., &
                   word_mode_job = .false.
        !> `fortmacs_diff_word_mode': difference regions are words.
        logical :: word_mode = .false.
        !> The difference regions, (buffer type, difference + 1)
        !> (upstream's `ediff-difference-vector-A' and the others).
        type(fortmacs_diff_difference_type), allocatable :: differences(:, :)
        !> `fortmacs_diff_number_of_differences' and `fortmacs_diff_current_difference'.
        integer :: number_of_differences = 0, current_difference = -1
        !> `fortmacs_diff_current_diff_overlay_a' and the others.
        type(handle_type) :: current_diff_overlays(4)
        !> `fortmacs_diff_wide_bounds', `fortmacs_diff_narrow_bounds' and
        !> `fortmacs_diff_visible_bounds': overlays by buffer type (A, B, C).
        type(handle_type) :: wide_bounds(3), narrow_bounds(3), visible_bounds(3)
        !> `fortmacs_diff_diff_buffer' and `fortmacs_diff_error_buffer'.
        type(handle_type) :: diff_buffer, error_buffer
        !> `fortmacs_diff_meta_buffer': the session group this session belongs to
        !> (the null handle for none), and `fortmacs_diff_meta_session_number', its
        !> number in that group.
        type(handle_type) :: meta_buffer
        integer :: meta_session_number = 0
        !> The regions copies replaced (upstream's `ediff-killed-diffs-alist').
        type(fortmacs_diff_replaced_region_type), allocatable :: replaced_regions(:)
        !> `fortmacs_diff_highlighting_style' (FORTMACS_DIFF_HIGHLIGHTING_STYLE_*).
        integer :: highlighting_style = FORTMACS_DIFF_HIGHLIGHTING_STYLE_ASCII
        !> The session's own values of `fortmacs_diff_use_faces',
        !> `fortmacs_diff_highlight_all_diffs', `fortmacs_diff_use_long_help_message' and
        !> `fortmacs_diff_keep_variants'.
        logical :: use_faces = .true., highlight_all_diffs = .true., use_long_help_message = .false., &
                   keep_variants = .true.
        !> The session's own `fortmacs_diff_split_window_function'.
        procedure(fortmacs_diff_split_window_procedure), pointer, nopass :: split_window_function => null()
        !> `fortmacs_diff_ignore_case'.
        logical :: ignore_case = .false.
        !> `fortmacs_diff_help_message', `fortmacs_diff_long_help_message' and
        !> `fortmacs_diff_brief_help_message'.
        character(len=:), allocatable :: help_message, long_help_message, brief_help_message
        !> `fortmacs_diff_control_buffer_suffix' and `fortmacs_diff_control_buffer_number'.
        character(len=:), allocatable :: control_buffer_suffix
        integer :: control_buffer_number = 0
        !> `fortmacs_diff_window_configuration_saved'.
        character(len=:), allocatable :: window_configuration_saved
        !> `fortmacs_diff_mode_map', the control buffer's own keymap.
        type(handle_type) :: keymap
        !> The state of each merge region (upstream's `ediff-state-of-merge';
        !> merge jobs only).
        type(fortmacs_diff_merge_state_type), allocatable :: merge_states(:)
        !> The session's own `fortmacs_diff_default_variant' (its state, with
        !> fortmacs_diff_merging_mod's FORTMACS_DIFF_DEFAULT_VARIANT_* constants).
        type(choice_type) :: default_variant
        !> `fortmacs_diff_merge_window_share': the share of the frame occupied by
        !> the merge window (buffer C).
        real :: merge_window_share = 0.45
        !> `fortmacs_diff_merge_store_file': the file where the result of the
        !> merge is to be saved (unallocated for none).
        character(len=:), allocatable :: merge_store_file
        !> The session's own `fortmacs_diff_autostore_merges' (its state).
        type(choice_type) :: autostore_merges
        !> The session's own `fortmacs_diff_show_clashes_only' and
        !> `fortmacs_diff_skip_merge_regions_that_differ_from_default'.
        logical :: show_clashes_only = .false., skip_merge_regions_that_differ_from_default = .false.
        !> The value `fortmacs_diff_show_ancestor' had before
        !> `toggle_fortmacs_diff_show_ancestor' changed it, restored on quitting
        !> (upstream's `ediff--show-ancestor-orig'; .false. also stands for
        !> none saved, as upstream's empty value does).
        logical :: show_ancestor_original = .false.
        !> `fortmacs_diff_custom_diff_buffer': the output of
        !> `fortmacs_diff_custom_diff_program'.
        type(handle_type) :: custom_diff_buffer
    contains
        !> Find the session's entry of `fortmacs_diff_sessions' again (this module).
        procedure, private :: get_index => get_session_index
        !> Activate or remove the faces of the difference regions.
        procedure :: paint_background_regions
        !> Generate the difference regions and overlays for two files.
        procedure :: set_up_difference_regions
        !> Generate the difference regions and overlays for three files.
        procedure :: set_up_difference_regions3
        !> ediff-diff.el's other session procedures (fortmacs_diff_settings_diff_mod).
        procedure, private :: make_diff2_buffer, prepare_error_list, extract_differences, extract_differences3, &
            get_narrow_start, make_difference_overlays, set_difference_overlays_in_one_buffer
        !> Set the state of every difference in buffers A, B and C.
        procedure :: set_all_difference_states
        !> Merge the differences from a difference on into buffer C.
        procedure :: merge_differences
        !> Combine a difference region of buffers A and B into buffer C.
        procedure :: combine_differences
        !> Whether n and p skip merge region DIFFERENCE (in SKIP).
        procedure :: check_region_skip
        !> ediff-merg.el's other session procedures (fortmacs_diff_settings_merging_mod).
        procedure, private :: get_state_of_merge, is_merge_region_non_clash, get_combined_region, set_difference_state, &
            check_region_changed
        !> Whether a difference number is a difference of the session.
        procedure :: is_valid_difference
        !> The message of an invalid difference number.
        procedure :: get_bad_difference_message
        !> Set the visited file name and auto-saving of the merge buffer.
        procedure :: arrange_autosave_in_merge_jobs
        !> Set up the selected window for the control buffer.
        procedure :: set_up_control_buffer
        !> Bring the current difference into view in all the windows.
        procedure :: recenter => recenter_session
        !> The difference closest to a position in a buffer.
        procedure :: get_difference_at_point
        !> Copy a difference region from one buffer to another.
        procedure :: copy_difference
        !> Restore the last replaced region of a difference.
        procedure :: pop_difference
        !> Perform the quit operations of the session.
        procedure :: quit
        !> Unselect the current difference and select another.
        procedure :: unselect_and_select_difference
        !> Where a difference begins or ends in a buffer.
        procedure :: get_difference_position
        !> Delete the difference overlays.
        procedure :: clear_differences
        !> Make the current difference overlay of a buffer.
        procedure :: make_current_difference_overlay
        !> Narrow buffers A, B (and C) to the visible bounds.
        procedure :: make_region_visible
        !> A `user_error' unless the session has a live ancestor buffer.
        procedure :: check_ancestor_exists
        !> Save the merge buffer and maybe close it.
        procedure :: save_and_delete_merge
        !> Compute the custom diff output of buffers A and B.
        procedure :: compute_custom_diffs
        !> ediff-util.el's other session procedures (fortmacs_diff_settings_utilities_mod).
        procedure, private :: recenter_one_window, save_difference_region, get_replaced_region_index, test_save_region, &
            dispose_of_variants, select_difference, unselect_difference, highlight_difference_in_one_buffer, &
            unhighlight_difference_in_one_buffer, unhighlight_differences_totally, place_flags_in_buffer, &
            get_region_contents, restore_highlighting
        !> Revise the control buffer's mode line.
        procedure :: refresh_mode_lines
        !> What the saved window configuration records of the session.
        procedure :: get_window_configuration
        !> ediff-wind.el's other session procedure (fortmacs_diff_settings_window_mod).
        procedure, private :: make_wide_control_buffer_id
        !> Compose the help message.
        procedure :: set_help_message
        !> ediff-help.el's other session procedures (fortmacs_diff_settings_help_mod).
        procedure, private :: get_help_message_line_length, indent_help_message
    end type fortmacs_diff_session_type

    interface
        !> In SKIP, whether n and p (`next_fortmacs_diff_difference',
        !> `previous_fortmacs_diff_difference') skip merge region DIFFERENCE of the
        !> session: with the session's `fortmacs_diff_show_clashes_only', a region
        !> without a clash between the ancestor and one of the variants;
        !> with its `fortmacs_diff_skip_merge_regions_that_differ_from_default', a
        !> region that differs from the current default (the user has decided
        !> how to merge it).  The other tests of ediff-next-difference (the
        !> regular expression filters, the regions that differ only in white
        !> space) are Deferred (docs/status/fortmacs_diff_utilities_mod.md).
        !> In fortmacs_diff_settings_merging_mod.  Port of
        !> ediff-merg.el:ediff-merge-region-is-non-clash-to-skip and
        !> ediff-merg.el:ediff-skip-merge-region-if-changed-from-default-p.
        recursive module subroutine check_region_skip(self, difference, skip, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            logical, intent(out) :: skip
            type(status_type), intent(out) :: status
        end subroutine check_region_skip

        !> Generate the difference regions and overlays of the session for the
        !> two files FILE_A and FILE_B.  Port of
        !> ediff-diff.el:ediff-setup-diff-regions.
        recursive module subroutine set_up_difference_regions(self, file_a, file_b, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            character(len=*), intent(in) :: file_a, file_b
            type(status_type), intent(out) :: status
        end subroutine set_up_difference_regions

        !> Run the diff program on FILE1 and FILE2 and put the output in
        !> DIFF_BUFFER; CODE is the size of DIFF_BUFFER, or 1 (an error exit
        !> code) when a file is missing.  Port of
        !> ediff-diff.el:ediff-make-diff2-buffer.
        recursive module subroutine make_diff2_buffer(self, diff_buffer, file1, file2, code, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(handle_type), intent(in) :: diff_buffer
            character(len=*), intent(in) :: file1, file2
            integer, intent(out) :: code
            type(status_type), intent(out) :: status
        end subroutine make_diff2_buffer

        !> Copy the output in DIFF_BUFFER that does not match OK_REGULAR_EXPRESSION into
        !> the session's error buffer; if there is any, show it, kill the
        !> control buffer and return with a `user_error'.  Port of
        !> ediff-diff.el:ediff-prepare-error-list (the output is shown
        !> undecoded).
        recursive module subroutine prepare_error_list(self, ok_regular_expression, diff_buffer, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            character(len=*), intent(in) :: ok_regular_expression
            type(handle_type), intent(in) :: diff_buffer
            type(status_type), intent(out) :: status
        end subroutine prepare_error_list

        !> Parse the normal diff output in DIFF_BUFFER into RECORDS, positions
        !> in buffers A and B (and C when live) of the session.  Port of
        !> ediff-diff.el:ediff-extract-diffs (WORD-MODE and BOUNDS other than
        !> the narrow bounds are deferred with those jobs).
        recursive module subroutine extract_differences(self, diff_buffer, records, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(handle_type), intent(in) :: diff_buffer
            type(fortmacs_diff_diff_record_type), allocatable, intent(out) :: records(:)
            type(status_type), intent(out) :: status
        end subroutine extract_differences

        !> Where the narrow bounds of the session begin in BUFFER_TYPE.
        !> Not a port: (ediff-overlay-start
        !> (ediff-get-value-according-to-buffer-type BUFFER-TYPE
        !> ediff-narrow-bounds)).
        module integer(int64) function get_narrow_start(self, buffer_type) result(position)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            integer, intent(in) :: buffer_type
        end function get_narrow_start

        !> Make an overlay for each difference region of RECORDS in each buffer
        !> of the session, and record them in its difference vectors.  Port of
        !> ediff-diff.el:ediff-convert-diffs-to-overlays.
        recursive module subroutine make_difference_overlays(self, records, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(fortmacs_diff_diff_record_type), intent(in) :: records(:)
            type(status_type), intent(out) :: status
        end subroutine make_difference_overlays

        !> Make the overlays of RECORDS in the BUFFER_TYPE buffer of the session.
        !> Port of ediff-diff.el:ediff-set-diff-overlays-in-one-buffer (word
        !> numbers are deferred).
        recursive module subroutine set_difference_overlays_in_one_buffer(self, buffer_type, records, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type
            type(fortmacs_diff_diff_record_type), intent(in) :: records(:)
            type(status_type), intent(out) :: status
        end subroutine set_difference_overlays_in_one_buffer

        !> Parse the diff3 output in DIFF_BUFFER into RECORDS for the session.
        !> THREE_WAY_COMP: it is a 3-way comparison; else it is merging with an
        !> ancestor, in which case buffer C's contents is identical to buffer
        !> A's or B's, depending on `fortmacs_diff_default_variant', and the second
        !> file of diff3 is the ancestor.  Port of
        !> ediff-diff.el:ediff-extract-diffs3 (word mode is Deferred with its
        !> jobs, see docs/status/fortmacs_diff_diff_mod.md).
        recursive module subroutine extract_differences3(self, diff_buffer, three_way_comp, records, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(handle_type), intent(in) :: diff_buffer
            logical, intent(in) :: three_way_comp
            type(fortmacs_diff_diff_record_type), allocatable, intent(out) :: records(:)
            type(status_type), intent(out) :: status
        end subroutine extract_differences3

        !> Generate the difference regions and overlays of the session for three
        !> files: FILE_C is the third file to compare (the ancestor's file in
        !> merges).  Port of ediff-diff.el:ediff-setup-diff-regions3.
        recursive module subroutine set_up_difference_regions3(self, file_a, file_b, file_c, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            character(len=*), intent(in) :: file_a, file_b, file_c
            type(status_type), intent(out) :: status
        end subroutine set_up_difference_regions3

        !> The state of merge region DIFFERENCE (counted from 0) of the session
        !> (FORTMACS_DIFF_STATE_OF_MERGE_*), none outside merge jobs.  Port of
        !> ediff-init.el:ediff-get-state-of-merge.
        module integer function get_state_of_merge(self, difference) result(state)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            integer, intent(in) :: difference
        end function get_state_of_merge

        !> Whether there is no clash between the ancestor and one of the
        !> variants in region DIFFERENCE of the session (only one variant changed it);
        !> .true. if it is not a merge job.  Port of
        !> ediff-merg.el:ediff-merge-region-is-non-clash.
        module logical function is_merge_region_non_clash(self, difference)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            integer, intent(in) :: difference
        end function is_merge_region_non_clash

        !> The combination of difference region DIFFERENCE of buffers A and B (and the
        !> ancestor) of the session by `fortmacs_diff_combination_pattern', in TEXT.  A
        !> region that cannot be read (the ancestor's in a merge without one)
        !> is left out with its delimiter.  Port of
        !> ediff-merg.el:ediff-get-combined-region.
        recursive module subroutine get_combined_region(self, difference, text, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            character(len=:), allocatable, intent(out) :: text
            type(status_type), intent(out) :: status
        end subroutine get_combined_region

        !> Set the state of every difference of the session in buffers A, B and
        !> C.  Port of ediff-merg.el:ediff-set-state-of-all-diffs-in-all-buffers.
        recursive module subroutine set_all_difference_states(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine set_all_difference_states

        !> Set the state of difference DIFFERENCE of the session in buffers A, B and C
        !> ("=diff(B)" in A when A's region equals B's, and so on; "=diff(A+B)"
        !> in C when C holds the combination).  Port of
        !> ediff-merg.el:ediff-set-state-of-diff-in-all-buffers.
        recursive module subroutine set_difference_state(self, difference, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            type(status_type), intent(out) :: status
        end subroutine set_difference_state

        !> Go over all diffs of the session starting with DIFF_NUMBER and copy
        !> regions into buffer C according to the state of the difference.
        !> Since `copy_difference' refuses to copy identical diff regions,
        !> there is no need to optimize this any further.
        !>
        !> If REMERGING, change state of merge in all diffs starting with
        !> DIFF_NUMBER, except those where the state is prefer-* or where it is
        !> default-* or combined but the buffer C region appears to be modified
        !> since last set by default.  Port of ediff-merg.el:ediff-do-merge.
        recursive module subroutine merge_differences(self, diff_number, status, remerging)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: diff_number
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: remerging
        end subroutine merge_differences

        !> Combine diff region DIFFERENCE of buffers A and B and place the
        !> combination in C (DIFFERENCE counted from 0, as Ediff counts internally).
        !> Combining is done according to the specifications in variable
        !> `fortmacs_diff_combination_pattern'.  Unless BATCH_INVOCATION, the old region
        !> is saved and the session goes to the difference.  Port of
        !> ediff-merg.el:ediff-combine-diffs (the command's N, the user's
        !> number, is read by the command).
        recursive module subroutine combine_differences(self, difference, status, batch_invocation)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: batch_invocation
        end subroutine combine_differences

        !> Give in CHANGED whether the non-preferred merge region DIFFERENCE of the
        !> session has been modified since originally set.  This affects only the
        !> regions that are marked as default-A/B or combined.  If PREFERS_TOO,
        !> then look at the regions marked as prefer-A/B as well.  Port of
        !> ediff-merg.el:ediff-merge-changed-from-default-p.
        recursive module subroutine check_region_changed(self, difference, changed, status, prefers_too)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            logical, intent(out) :: changed
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: prefers_too
        end subroutine check_region_changed

        !> Whether DIFFERENCE (the current difference if absent) is a difference of
        !> the session.  Port of ediff-init.el:ediff-valid-difference-p.
        module logical function is_valid_difference(self, difference)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            integer, intent(in), optional :: difference
        end function is_valid_difference

        !> The message of an invalid difference number DIFFERENCE (counted from 0) of
        !> the session.  Port of ediff-init.el ediff-BAD-DIFF-NUMBER.
        module function get_bad_difference_message(self, difference) result(text)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            integer, intent(in) :: difference
            character(len=:), allocatable :: text
        end function get_bad_difference_message

        !> Set the visited file name and auto-saving of the merge buffer of
        !> the session: with MERGE_BUFFER_FILE, the merge is stored there (and
        !> `fortmacs_diff_autostore_merges' becomes .true. for the session).  Runs in
        !> the control buffer.  Port of
        !> ediff-util.el:ediff-arrange-autosave-in-merge-jobs (`buffer-offer-save'
        !> and `auto-save-mode' are Deferred, see docs/status/fortmacs_diff_utilities_mod.md).
        recursive module subroutine arrange_autosave_in_merge_jobs(self, status, merge_buffer_file)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: merge_buffer_file
        end subroutine arrange_autosave_in_merge_jobs

        !> Set up the selected window for control buffer CONTROL_BUFFER of
        !> the session: show the help message, fit the window, mark the help
        !> text and remember the window configuration.  Port of
        !> ediff-util.el:ediff-setup-control-buffer.
        recursive module subroutine set_up_control_buffer(self, control_buffer, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(handle_type), intent(in) :: control_buffer
            type(status_type), intent(out) :: status
        end subroutine set_up_control_buffer

        !> The body of `recenter_fortmacs_diff' for the session, current in its control
        !> buffer.  Port of ediff-util.el:ediff-recenter.
        recursive module subroutine recenter_session(self, no_rehighlight, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            logical, intent(in) :: no_rehighlight
            type(status_type), intent(out) :: status
        end subroutine recenter_session

        !> Show the current difference of the session in the window of
        !> BUFFER_TYPE, without changing the selected window.  Port of
        !> ediff-util.el:ediff-recenter-one-window.
        recursive module subroutine recenter_one_window(self, buffer_type, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type
            type(status_type), intent(out) :: status
        end subroutine recenter_one_window

        !> The difference of the session closest to POSITION (point if absent) in
        !> BUFFER_TYPE, as the user counts them (from 1).  WHICH_DIFF 1
        !> finds the difference after POSITION, -1 the one before; 0 the nearest.
        !> Port of ediff-util.el:ediff-diff-at-point (WHICH-DIFF `after',
        !> `before' or absent).
        recursive module subroutine get_difference_at_point(self, buffer_type, diff_no, status, position, which_diff)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type
            integer, intent(out) :: diff_no
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: position
            integer, intent(in), optional :: which_diff
        end subroutine get_difference_at_point

        !> Copy difference DIFFERENCE of the session from FROM_TYPE (or REGION_TO_COPY,
        !> when given, with FROM_TYPE 0) to TO_TYPE, saving the old region
        !> unless BATCH_INVOCATION.  Port of ediff-util.el:ediff-copy-diff.
        recursive module subroutine copy_difference(self, difference, from_type, to_type, status, batch_invocation, region_to_copy)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference, from_type, to_type
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: batch_invocation
            character(len=*), intent(in), optional :: region_to_copy
        end subroutine copy_difference

        !> Save TEXT, the old difference DIFFERENCE of BUFFER_TYPE in the session (in the
        !> session's `replaced_regions'), and give in MESSG what was
        !> saved.  Port of ediff-util.el:ediff-save-diff-region.
        module subroutine save_difference_region(self, difference, buffer_type, text, messg)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference, buffer_type
            character(len=*), intent(in) :: text
            character(len=:), allocatable, intent(out) :: messg
        end subroutine save_difference_region

        !> The index of the replaced region of difference DIFFERENCE of BUFFER_TYPE in
        !> the session, 0 if none.  Not a port: the lookup in
        !> ediff-killed-diffs-alist that ediff-util.el does inline.
        module integer function get_replaced_region_index(self, difference, buffer_type) result(j)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            integer, intent(in) :: difference, buffer_type
        end function get_replaced_region_index

        !> Whether difference DIFFERENCE of BUFFER_TYPE may be saved: if it was saved
        !> before, ask; "Quit" if the user declines.  Port of
        !> ediff-util.el:ediff-test-save-region.
        recursive module subroutine test_save_region(self, difference, buffer_type, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference, buffer_type
            type(status_type), intent(out) :: status
        end subroutine test_save_region

        !> Pop last killed diff region DIFFERENCE from buffer BUFFER_TYPE.  Port of
        !> ediff-util.el:ediff-pop-diff.
        recursive module subroutine pop_difference(self, difference, buffer_type, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference, buffer_type
            type(status_type), intent(out) :: status
        end subroutine pop_difference

        !> Perform the quit operations of the session.  Port of
        !> ediff-util.el:ediff-really-quit.
        recursive module subroutine quit(self, reverse_default_keep_variants, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            logical, intent(in) :: reverse_default_keep_variants
            type(status_type), intent(out) :: status
        end subroutine quit

        !> Kill buffers A, B, and, possibly, C, if these buffers aren't
        !> modified; ASK asks the user about each unmodified buffer, unless
        !> KEEP_VARIANTS, in which case buffers are never killed.  Port of
        !> ediff-util.el:ediff-janitor.
        recursive module subroutine dispose_of_variants(self, ask, keep_variants, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            logical, intent(in) :: ask, keep_variants
            type(status_type), intent(out) :: status
        end subroutine dispose_of_variants

        !> Select difference DIFFERENCE of the session: highlight it and run
        !> `fortmacs_diff_select_hook'.  Port of ediff-util.el:ediff-select-difference
        !> (fine differences are deferred).
        recursive module subroutine select_difference(self, difference, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            type(status_type), intent(out) :: status
        end subroutine select_difference

        !> Unselect difference DIFFERENCE of the session by removing the ASCII flags or
        !> the highlighting.  Port of ediff-util.el:ediff-unselect-difference.
        recursive module subroutine unselect_difference(self, difference, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            type(status_type), intent(out) :: status
        end subroutine unselect_difference

        !> Unselect the current difference of the session and select DIFFERENCE, or
        !> with FLAG 1 only select DIFFERENCE, with FLAG -1 only unselect; recenter
        !> unless NO_RECENTER.  Port of
        !> ediff-util.el:ediff-unselect-and-select-difference (FLAG
        !> `select-only' or `unselect-only').
        recursive module subroutine unselect_and_select_difference(self, difference, status, flag, no_recenter)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: flag
            logical, intent(in), optional :: no_recenter
        end subroutine unselect_and_select_difference

        !> Highlight difference DIFFERENCE of the session in BUFFER_TYPE with the current
        !> difference overlay.  Port of
        !> ediff-util.el:ediff-highlight-diff-in-one-buffer.
        recursive module subroutine highlight_difference_in_one_buffer(self, difference, buffer_type, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference, buffer_type
            type(status_type), intent(out) :: status
        end subroutine highlight_difference_in_one_buffer

        !> Remove the current difference overlay of the session in BUFFER_TYPE
        !> and give the difference its background face again.  Port of
        !> ediff-util.el:ediff-unhighlight-diff-in-one-buffer.
        recursive module subroutine unhighlight_difference_in_one_buffer(self, buffer_type, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type
            type(status_type), intent(out) :: status
        end subroutine unhighlight_difference_in_one_buffer

        !> Delete the highlighting overlays of the session and restore the faces
        !> to their original form.  Port of
        !> ediff-util.el:ediff-unhighlight-diffs-totally with
        !> ediff-unhighlight-diffs-totally-in-one-buffer.
        recursive module subroutine unhighlight_differences_totally(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine unhighlight_differences_totally

        !> Put the ASCII flags around difference DIFFERENCE of the session in BUFFER_TYPE.
        !> Port of ediff-util.el:ediff-place-flags-in-buffer with
        !> ediff-place-flags-in-buffer1.
        recursive module subroutine place_flags_in_buffer(self, buffer_type, difference, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type, difference
            type(status_type), intent(out) :: status
        end subroutine place_flags_in_buffer

        !> The position where difference DIFFERENCE (the current one if absent) of
        !> the session begins in BUFFER_TYPE, or with AT_END ends.  An error
        !> condition occurs for an invalid DIFFERENCE or a killed buffer.  Port of
        !> ediff-util.el:ediff-get-diff-posn (AT_END for POSITION `end').
        recursive module subroutine get_difference_position(self, buffer_type, at_end, position, status, difference)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type
            logical, intent(in) :: at_end
            integer(int64), intent(out) :: position
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: difference
        end subroutine get_difference_position

        !> The text of difference DIFFERENCE of the session in BUFFER_TYPE (from START
        !> to END when given).  Port of ediff-util.el:ediff-get-region-contents.
        recursive module subroutine get_region_contents(self, difference, buffer_type, text, status, start, end)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: difference, buffer_type
            character(len=:), allocatable, intent(out) :: text
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: start, end
        end subroutine get_region_contents

        !> Restore highlighting to what it should be according to
        !> `fortmacs_diff_use_faces', the highlighting style and
        !> `fortmacs_diff_highlight_all_diffs'.  Port of
        !> ediff-util.el:ediff-restore-highlighting.
        recursive module subroutine restore_highlighting(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine restore_highlighting

        !> Delete the difference overlays of the session so they won't slow down
        !> future editing operations.  Port of
        !> ediff-util.el:ediff-clear-diff-vector (for the vectors of all the
        !> buffers; fine differences are deferred).
        module subroutine clear_differences(self)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
        end subroutine clear_differences

        !> Make the current difference overlay of the session in BUFFER_TYPE,
        !> when the display has faces.  Port of
        !> ediff-util.el:ediff-make-current-diff-overlay.
        recursive module subroutine make_current_difference_overlay(self, buffer_type, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            integer, intent(in) :: buffer_type
            type(status_type), intent(out) :: status
        end subroutine make_current_difference_overlay

        !> Narrow buffers A, B (and C) of the session to the visible bounds.
        !> Port of ediff-util.el:ediff-visible-region.
        recursive module subroutine make_region_visible(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine make_region_visible

        !> A `user_error' unless the session has a live ancestor buffer.  Port of
        !> ediff-util.el:ediff--check-ancestor-exists.
        module subroutine check_ancestor_exists(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine check_ancestor_exists

        !> `maybe_save_and_delete_fortmacs_diff_merge' for the session: if
        !> SAVE_AND_CONTINUE, save merge buffer and continue (as if
        !> `fortmacs_diff_autostore_merges' were .true., without offering to close
        !> the buffer).  Port of ediff-util.el:ediff-maybe-save-and-delete-merge.
        recursive module subroutine save_and_delete_merge(self, save_and_continue, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            logical, intent(in) :: save_and_continue
            type(status_type), intent(out) :: status
        end subroutine save_and_delete_merge

        !> Compute the custom diff output of buffers A and B of the session into
        !> its custom diff buffer, in Diff mode: `fortmacs_diff_custom_diff_program'
        !> with `fortmacs_diff_custom_diff_options' on their files (temporary copies
        !> when a buffer visits no existing file).  Port of
        !> ediff-util.el:ediff-compute-custom-diffs-maybe (remote files are not
        !> ported).
        recursive module subroutine compute_custom_diffs(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine compute_custom_diffs

        !> Revise the mode line of the session's control buffer to show which
        !> difference is selected.  Port of ediff-wind.el:ediff-refresh-mode-lines
        !> (the mode lines of buffers A, B and C, which show the state of the
        !> difference, wait for `mode_line_format'; the control buffer's mode
        !> line keeps the standard form around its identification).
        recursive module subroutine refresh_mode_lines(self)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
        end subroutine refresh_mode_lines

        !> The control buffer's identification in the mode line: its name and
        !> where the session stands ("diff 2 of 5").  Port of
        !> ediff-wind.el:ediff-make-wide-control-buffer-id.
        module function make_wide_control_buffer_id(self) result(id)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            type(header_line_format_type) :: id
        end function make_wide_control_buffer_id

        !> What `fortmacs_diff_window_configuration_saved' records of the session: its windows,
        !> split function and display.  Not a port: the string
        !> (format "%S%S%S%S%S%S%S%S" ...) of ediff-setup-control-buffer and
        !> ediff-keep-window-config.
        module function get_window_configuration(self) result(configuration)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
            character(len=:), allocatable :: configuration
        end function get_window_configuration

        !> The length of the first line of the help message in the current
        !> buffer (the second one with the long message).  Port of
        !> ediff-help.el:ediff-help-message-line-length.
        module integer function get_help_message_line_length(self) result(n)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(in) :: self
        end function get_help_message_line_length

        !> Center the help message in the control window: indent each line of
        !> the current buffer by half the room the window leaves.  Port of
        !> ediff-help.el:ediff-indent-help-message.
        module subroutine indent_help_message(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine indent_help_message

        !> Compose the help message of the session: the long one for its kind
        !> of job, the brief one, and the one to show
        !> (`fortmacs_diff_use_long_help_message'); then run `fortmacs_diff_display_help_hook'.
        !> Port of ediff-help.el:ediff-set-help-message (the user's message
        !> functions and the word-mode and narrowing messages are deferred
        !> with those jobs).
        recursive module subroutine set_help_message(self, status)
            implicit none(type, external)
            class(fortmacs_diff_session_type), intent(inout), target :: self
            type(status_type), intent(out) :: status
        end subroutine set_help_message
    end interface

    !> The Ediff sessions, one per control buffer (entries of closed
    !> control buffers are reused).
    type(fortmacs_diff_session_type), allocatable, target, public :: fortmacs_diff_sessions(:)

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> If true, Ediff will think that it is running on a display that
    !> supports faces.
    !> This is provided as a temporary relief for users of face-capable
    !> displays that Ediff doesn't know about.  Port of ediff-init.el
    !> ediff-force-faces.
    logical, public :: fortmacs_diff_force_faces = .false.

    !> Flag placed before highlighted block of differences at beginning of a line.
    !> Port of ediff-init.el ediff-before-flag-bol.
    character(len=:), allocatable, public :: fortmacs_diff_before_flag_beginning_of_line
    !> Flag placed after highlighted block of differences that ends at end of line.
    !> Port of ediff-init.el ediff-after-flag-eol.
    character(len=:), allocatable, public :: fortmacs_diff_after_flag_end_of_line
    !> Flag placed before highlighted block of differences that starts mid-line.
    !> Port of ediff-init.el ediff-before-flag-mol.
    character(len=:), allocatable, public :: fortmacs_diff_before_flag_mol
    !> Flag placed after a highlighted block of differences, if block ends in mid-line.
    !> Port of ediff-init.el ediff-after-flag-mol.
    character(len=:), allocatable, public :: fortmacs_diff_after_flag_mol

    !> If true, differences are highlighted using faces, if device
    !> supports faces.
    !> If .false., differences are highlighted using ASCII flags,
    !> ediff-before-flag and ediff-after-flag.  On a non-window system,
    !> differences are always highlighted using ASCII flags.  Port of
    !> ediff-init.el ediff-use-faces (each session takes its own value
    !> when it starts).
    logical, public :: fortmacs_diff_use_faces = .true.

    !> .false. means prompt to remove unmodified buffers A/B/C at session end.
    !> Supplying a prefix argument to the quit command \`q' temporarily
    !> reverses the meaning of this variable.  Port of ediff-init.el
    !> ediff-keep-variants.
    logical, public :: fortmacs_diff_keep_variants = .true.

    !> If .false., only the selected differences are highlighted.
    !> Otherwise, all difference regions are highlighted, but the selected
    !> region is shown in brighter colors.  Port of ediff-init.el
    !> ediff-highlight-all-diffs (each session takes its own value when it
    !> starts).
    logical, public :: fortmacs_diff_highlight_all_diffs = .true.

    !> Coding system for read to use when running the diff program as a
    !> subprocess.
    !> In most cases, the default will do.  Port of ediff-init.el
    !> ediff-coding-system-for-read.
    character(len=:), allocatable, public :: fortmacs_diff_coding_system_for_read

    !> Coding system for write to use when writing out difference regions.
    !> This is used when writing to temp files in buffer jobs and when
    !> Ediff needs to find fine differences.  Port of ediff-init.el
    !> ediff-coding-system-for-write.
    character(len=:), allocatable, public :: fortmacs_diff_coding_system_for_write

    !> A true value makes all buffers read-only at startup.
    !> If .false., only the buffers whose file is read-only and
    !> ancestor buffers are made read-only.  Port of ediff-init.el
    !> ediff-make-buffers-readonly-at-startup.
    logical, public :: fortmacs_diff_make_buffers_readonly_at_startup = .false.

    !> If true, show ancestor buffer in 3way merges and refine it.
    !> Port of ediff-init.el ediff-show-ancestor (the refinement is
    !> Deferred with fine differences, see docs/status/fortmacs_diff_diff_mod.md).
    logical, public :: fortmacs_diff_show_ancestor = .true.

    !> Values of `fortmacs_diff_autostore_merges' besides .true. and .false.
    integer, parameter, public :: FORTMACS_DIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY = 2

    !> Save the results of merge jobs automatically.
    !> With value .false., don't save automatically.  With value .true.,
    !> always save.  FORTMACS_DIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY (the default)
    !> means save automatically only if the merge job is part of a group
    !> of jobs, such as `fortmacs_diff_merge_directory' or
    !> `fortmacs_diff_merge_directory_revisions'.  Port of ediff-init.el
    !> ediff-autostore-merges (each session takes its own value when it
    !> starts).
    type(choice_setting_type), public :: fortmacs_diff_autostore_merges = &
                                         choice_setting_type(name="fortmacs_diff_autostore_merges", &
                                                             choice=FORTMACS_DIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY)

    !> If .true., C-h invokes Fortmacs help in the control buffer.
    !> If .false., C-h jumps to the previous difference instead.  Port of
    !> ediff-init.el ediff-no-emacs-help-in-control-buffer (named
    !> positively, the value inverted).
    logical, public :: fortmacs_diff_fortmacs_help_in_control_buffer = .true.

    !> Mode for Ediff temporary files.  Port of ediff-init.el
    !> ediff-temp-file-mode.
    integer, public :: fortmacs_diff_temporary_file_mode = int(o"600")

    !> Prefix to be attached to saved merge buffers.  Port of ediff-init.el
    !> ediff-merge-filename-prefix.
    character(len=:), allocatable, public :: fortmacs_diff_merge_filename_prefix

    ! ------------------------------------------------------------------
    ! Hooks
    ! ------------------------------------------------------------------

    !> Hooks to run before Ediff begins to set up windows and buffers.
    !> This hook can be used to save the previous window config, which
    !> can be restored on `quit_fortmacs_diff'.  Port of ediff-init.el
    !> ediff-before-setup-hook.
    type(hook_type), target, public :: fortmacs_diff_before_setup_hook = hook_type(name="fortmacs_diff_before_setup_hook")
    !> Hooks to run before Ediff sets its window configuration.
    !> This hook is run every time when Ediff arranges its windows.
    !> This happens each time Ediff detects that the windows were messed
    !> up by the user.  Port of ediff-init.el ediff-before-setup-windows-hook.
    type(hook_type), target, public :: fortmacs_diff_before_setup_windows_hook = &
                                       hook_type(name="fortmacs_diff_before_setup_windows_hook")
    !> Hooks to run after Ediff sets its window configuration.
    !> This can be used to set up control window or icon in a desired
    !> place.  Port of ediff-init.el ediff-after-setup-windows-hook.
    type(hook_type), target, public :: fortmacs_diff_after_setup_windows_hook = &
                                       hook_type(name="fortmacs_diff_after_setup_windows_hook")
    !> Hooks to run in the control buffer after Ediff has been set up and
    !> is ready.  Port of ediff-init.el ediff-startup-hook.
    type(hook_type), target, public :: fortmacs_diff_startup_hook = hook_type(name="fortmacs_diff_startup_hook")
    !> Hooks to run after a difference has been selected.  Port of
    !> ediff-init.el ediff-select-hook.
    type(hook_type), target, public :: fortmacs_diff_select_hook = hook_type(name="fortmacs_diff_select_hook")
    !> Hooks to run after a difference has been unselected.  Port of
    !> ediff-init.el ediff-unselect-hook.
    type(hook_type), target, public :: fortmacs_diff_unselect_hook = hook_type(name="fortmacs_diff_unselect_hook")
    !> Hooks run after buffers A, B, and C are set up.
    !> For each buffer, the hooks are run with that buffer made current.
    !> Port of ediff-init.el ediff-prepare-buffer-hook.
    type(hook_type), target, public :: fortmacs_diff_prepare_buffer_hook = hook_type(name="fortmacs_diff_prepare_buffer_hook")
    !> Hook run just after `fortmacs_diff_mode' is set up in the control buffer.
    !> This is done before any windows or frames are created.  One can use
    !> it to set local variables that determine how the display looks
    !> like.  Port of ediff-init.el ediff-mode-hook.
    type(hook_type), target, public :: fortmacs_diff_mode_hook = hook_type(name="fortmacs_diff_mode_hook")
    !> Hook run just after the default bindings in Ediff keymap are set
    !> up.  Port of ediff-init.el ediff-keymap-setup-hook.
    type(hook_type), target, public :: fortmacs_diff_keymap_setup_hook = hook_type(name="fortmacs_diff_keymap_setup_hook")
    !> Hooks run after preparing the help message.  Port of ediff-init.el
    !> ediff-display-help-hook.
    type(hook_type), target, public :: fortmacs_diff_display_help_hook = hook_type(name="fortmacs_diff_display_help_hook")
    !> Hooks to run in the Ediff control buffer after finishing Ediff.
    !> Port of ediff-init.el ediff-quit-hook (its default function,
    !> `clean_up_fortmacs_diff_mess', is added by fortmacs_diff_utilities_mod).
    type(hook_type), target, public :: fortmacs_diff_quit_hook = hook_type(name="fortmacs_diff_quit_hook")
    !> Hooks to run on exiting Ediff, before killing the control and
    !> variant buffers.  Port of ediff-init.el ediff-cleanup-hook.
    type(hook_type), target, public :: fortmacs_diff_cleanup_hook = hook_type(name="fortmacs_diff_cleanup_hook")

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Face for highlighting the selected difference in buffer A.
    type(named_face_type), target, public :: fortmacs_diff_current_diff_a
    !> Face for highlighting the selected difference in buffer B.
    type(named_face_type), target, public :: fortmacs_diff_current_diff_b
    !> Face for highlighting the selected difference in buffer C.
    type(named_face_type), target, public :: fortmacs_diff_current_diff_c
    !> Face for highlighting the selected difference in buffer Ancestor.
    type(named_face_type), target, public :: fortmacs_diff_current_diff_ancestor
    !> Face for highlighting the refinement of the selected diff in buffer A.
    type(named_face_type), target, public :: fortmacs_diff_fine_diff_a
    !> Face for highlighting the refinement of the selected diff in buffer B.
    type(named_face_type), target, public :: fortmacs_diff_fine_diff_b
    !> Face for highlighting the refinement of the selected diff in buffer C.
    type(named_face_type), target, public :: fortmacs_diff_fine_diff_c
    !> Face for highlighting refinement of the selected diff in the ancestor buffer.
    type(named_face_type), target, public :: fortmacs_diff_fine_diff_ancestor
    !> Face for highlighting even-numbered non-current differences in buffer A.
    type(named_face_type), target, public :: fortmacs_diff_even_diff_a
    !> Face for highlighting even-numbered non-current differences in buffer B.
    type(named_face_type), target, public :: fortmacs_diff_even_diff_b
    !> Face for highlighting even-numbered non-current differences in buffer C.
    type(named_face_type), target, public :: fortmacs_diff_even_diff_c
    !> Face for highlighting even-numbered non-current differences in ancestor buffer.
    type(named_face_type), target, public :: fortmacs_diff_even_diff_ancestor
    !> Face for highlighting odd-numbered non-current differences in buffer A.
    type(named_face_type), target, public :: fortmacs_diff_odd_diff_a
    !> Face for highlighting odd-numbered non-current differences in buffer B.
    type(named_face_type), target, public :: fortmacs_diff_odd_diff_b
    !> Face for highlighting odd-numbered non-current differences in buffer C.
    type(named_face_type), target, public :: fortmacs_diff_odd_diff_c
    !> Face for highlighting odd-numbered non-current differences in ancestor buffer.
    type(named_face_type), target, public :: fortmacs_diff_odd_diff_ancestor

    !> The faces' names by buffer type (`ediff-current-diff-face-alist'
    !> and the others).
    character(len=*), parameter, public :: FORTMACS_DIFF_CURRENT_DIFF_FACES(4) = &
                                           [character(len=35) :: "fortmacs_diff_current_diff_a", "fortmacs_diff_current_diff_b", &
                                                                  "fortmacs_diff_current_diff_c", &
                                                                  "fortmacs_diff_current_diff_ancestor"]
    character(len=*), parameter :: EVEN_DIFF_FACES(4) = &
                                   [character(len=32) :: "fortmacs_diff_even_diff_a", "fortmacs_diff_even_diff_b", &
                                                          "fortmacs_diff_even_diff_c", &
                                                          "fortmacs_diff_even_diff_ancestor"]
    character(len=*), parameter :: ODD_DIFF_FACES(4) = &
                                   [character(len=31) :: "fortmacs_diff_odd_diff_a", "fortmacs_diff_odd_diff_b", &
                                                          "fortmacs_diff_odd_diff_c", &
                                                          "fortmacs_diff_odd_diff_ancestor"]

contains

    !> The index in `fortmacs_diff_sessions' of the session whose control buffer
    !> is BUFFER (the current buffer if absent), 0 if it is none.
    !> Not a port: upstream's session variables are the control buffer's.
    integer function fortmacs_diff_session_index(buffer) result(session_index)
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: serial

        session_index = 0
        if (.not. allocated(fortmacs_diff_sessions)) return

        if (present(buffer)) then
            if (.not. is_buffer_live(buffer)) return
            serial = buffers(buffer%index)%serial
        else
            if (current == 0) return
            serial = buffers(current)%serial
        end if

        session_index = findloc(fortmacs_diff_sessions%serial, serial, dim=1)
    end function fortmacs_diff_session_index

    !> Find or make the session entry of control buffer BUFFER, reusing
    !> the entry of a closed control buffer; SESSION_INDEX is its index.  Not a port.
    subroutine make_fortmacs_diff_session_entry(buffer, session_index)
        type(handle_type), intent(in) :: buffer
        integer, intent(out) :: session_index
        type(fortmacs_diff_session_type) :: entry

        session_index = fortmacs_diff_session_index(buffer)
        if (session_index > 0) return
        if (.not. allocated(fortmacs_diff_sessions)) allocate (fortmacs_diff_sessions(0))

        do session_index = 1, size(fortmacs_diff_sessions)
            if (.not. buffer_serial_live(fortmacs_diff_sessions(session_index)%serial)) exit
        end do

        if (session_index > size(fortmacs_diff_sessions)) fortmacs_diff_sessions = [fortmacs_diff_sessions, entry]
        fortmacs_diff_sessions(session_index) = entry
        fortmacs_diff_sessions(session_index)%serial = buffers(buffer%index)%serial
        fortmacs_diff_sessions(session_index)%control_buffer = buffer
    end subroutine make_fortmacs_diff_session_entry

    !> Forget session SESSION_INDEX (its control buffer is about to close).
    !> Not a port.
    subroutine drop_fortmacs_diff_session(session_index)
        integer, intent(in) :: session_index
        type(fortmacs_diff_session_type) :: entry

        if (session_index < 1 .or. session_index > size(fortmacs_diff_sessions)) return
        fortmacs_diff_sessions(session_index) = entry
    end subroutine drop_fortmacs_diff_session

    !> Whether the display supports faces: a graphic display, a color
    !> terminal, or `fortmacs_diff_force_faces'.  Port of
    !> ediff-init.el:ediff-has-face-support-p.
    logical function has_fortmacs_diff_face_support()
        has_fortmacs_diff_face_support = associated(selected_frame%redisplay_interface) .or. fortmacs_diff_force_faces .or. &
                                         selected_frame%terminal%tn_maximum_colors > 0
    end function has_fortmacs_diff_face_support

    !> Whether the job JOB_NAME compares or merges buffers (its name has
    !> "buffer" in it upstream).  Not a port: ediff-util.el and
    !> ediff-diff.el test (string-match "buffer" (symbol-name ediff-job-name)).
    logical function is_fortmacs_diff_buffer_job(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_fortmacs_diff_buffer_job = associated(job_name, commands%fortmacs_diff_buffers) .or. associated(job_name, &
                                                                                                  commands%fortmacs_diff_buffers3) &
                                      .or. associated(job_name, commands%fortmacs_diff_merge_buffers) .or. &
                                      associated(job_name, commands%fortmacs_diff_merge_buffers_with_ancestor)
    end function is_fortmacs_diff_buffer_job

    !> The buffer type a key names: a or A is buffer A, b or B buffer B,
    !> c or C buffer C; 0 for another character.  Port of
    !> ediff-init.el:ediff-char-to-buftype.
    pure integer function fortmacs_diff_character_to_buffer_type(c) result(buffer_type)
        character, intent(in) :: c

        select case (c)
        case ("a", "A")
            buffer_type = FORTMACS_DIFF_BUFFER_TYPE_A
        case ("b", "B")
            buffer_type = FORTMACS_DIFF_BUFFER_TYPE_B
        case ("c", "C")
            buffer_type = FORTMACS_DIFF_BUFFER_TYPE_C
        case default
            buffer_type = 0
        end select
    end function fortmacs_diff_character_to_buffer_type

    !> The name of BUFFER_TYPE as messages show it: A, B, C or Ancestor.
    !> Not a port: upstream prints the symbol.
    pure function fortmacs_diff_buffer_type_name(buffer_type) result(name)
        integer, intent(in) :: buffer_type
        character(len=:), allocatable :: name

        select case (buffer_type)
        case (FORTMACS_DIFF_BUFFER_TYPE_A)
            name = "A"
        case (FORTMACS_DIFF_BUFFER_TYPE_B)
            name = "B"
        case (FORTMACS_DIFF_BUFFER_TYPE_C)
            name = "C"
        case default
            name = "Ancestor"
        end select
    end function fortmacs_diff_buffer_type_name

    !> Give OVERLAY the face named FACE ("" for none).  Port of
    !> ediff-init.el:ediff-set-overlay-face (the `help-echo' text is
    !> deferred with the mouse).
    subroutine set_fortmacs_diff_overlay_face(overlay, face, status)
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
    end subroutine set_fortmacs_diff_overlay_face

    !> If OVERLAY's buffer exists, move OVERLAY to BEGINNING-END; delete it
    !> otherwise.  Port of ediff-init.el:ediff-move-overlay.
    subroutine move_fortmacs_diff_overlay(overlay, beginning, end, status)
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
    end subroutine move_fortmacs_diff_overlay

    !> The face for non-current difference DIFF_NUMBER (counted from 0) of
    !> BUFFER_TYPE: the even face when the number the user sees is even.
    !> Port of ediff-init.el:ediff-background-face.
    pure function fortmacs_diff_background_face(buffer_type, diff_number) result(face)
        integer, intent(in) :: buffer_type, diff_number
        character(len=:), allocatable :: face

        ! The value of diff_number is always 1- the one that user sees.
        ! This is why even face is used when diff_number is odd.
        if (modulo(diff_number, 2) == 1) then
            face = trim(EVEN_DIFF_FACES(buffer_type))
        else
            face = trim(ODD_DIFF_FACES(buffer_type))
        end if
    end function fortmacs_diff_background_face

    !> Activate faces on the difference regions of the session in all
    !> buffers, or with UNHIGHLIGHT remove them.  Port of
    !> ediff-init.el:ediff-paint-background-regions with
    !> ediff-paint-background-regions-in-one-buffer.
    subroutine paint_background_regions(self, unhighlight, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        logical, intent(in) :: unhighlight
        type(status_type), intent(out) :: status
        integer :: buffer_type, n
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        if (.not. allocated(fortmacs_diff_sessions(session_index)%differences)) return

        do buffer_type = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR
            do n = 1, size(fortmacs_diff_sessions(session_index)%differences, 2)
                associate (overlay => fortmacs_diff_sessions(session_index)%differences(buffer_type, n)%overlay)
                    ! Only if overlay is alive.
                    if (.not. is_buffer_live(overlay_buffer(overlay))) cycle

                    if (unhighlight) then
                        call set_fortmacs_diff_overlay_face(overlay, "", status)
                    else
                        call set_fortmacs_diff_overlay_face(overlay, fortmacs_diff_background_face(buffer_type, n - 1), status)
                    end if
                end associate

                if (.not. status%ok()) return
            end do
        end do
    end subroutine paint_background_regions

    !> Give in SESSION_INDEX the current buffer's session; a `user_error' if the
    !> current buffer is no Ediff control buffer.  Port of
    !> ediff-init.el:ediff-barf-if-not-control-buffer with
    !> ediff-in-control-buffer-p (the meta buffer case waits for
    !> ediff-mult.el).
    subroutine check_fortmacs_diff_control_buffer(session_index, status)
        integer, intent(out) :: session_index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        status = status_type()
        session_index = fortmacs_diff_session_index()
        if (session_index > 0) return
        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        call set_error(status, USER_ERROR_CODE, name//": This command runs in Ediff Control Buffer only!")
    end subroutine check_fortmacs_diff_control_buffer

    !> Give in SESSION_INDEX the entry of `fortmacs_diff_sessions' that is the
    !> session, found again by its serial number (the array may have grown
    !> and moved since the caller took the entry); a `user_error' if the
    !> session has no entry (its control buffer was closed).  Not a port:
    !> upstream's session variables are the control buffer's, as in
    !> ediff-init.el:ediff-barf-if-not-control-buffer.
    subroutine get_session_index(self, session_index, status)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer, intent(out) :: session_index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        status = status_type()
        session_index = 0

        if (allocated(fortmacs_diff_sessions) .and. self%serial /= 0) then
            session_index = findloc(fortmacs_diff_sessions%serial, self%serial, dim=1)
        end if

        if (session_index > 0) return
        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        call set_error(status, USER_ERROR_CODE, name//": This command runs in Ediff Control Buffer only!")
    end subroutine get_session_index

    !> Kill buffer BUFFER if it exists.  Port of
    !> ediff-init.el:ediff-kill-buffer-carefully.
    recursive subroutine close_fortmacs_diff_buffer_carefully(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_buffer_live(buffer)) call close_buffer(buffer, status)
    end subroutine close_fortmacs_diff_buffer_carefully

    !> Whether session group job JOB_NAME merges with ancestors.  Port of
    !> ediff-init.el:ediff-ancestor-metajob (the file group job is
    !> Deferred).
    logical function is_fortmacs_diff_ancestor_metajob(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_fortmacs_diff_ancestor_metajob = associated(job_name, commands%fortmacs_diff_merge_directories_with_ancestor)
    end function is_fortmacs_diff_ancestor_metajob

    !> Whether session group job JOB_NAME can collect the custom diffs of
    !> its sessions into a multifile patch.  Port of
    !> ediff-init.el:ediff-collect-diffs-metajob (the revision jobs are
    !> Deferred).
    logical function is_fortmacs_diff_collect_diffs_metajob(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_fortmacs_diff_collect_diffs_metajob = associated(job_name, commands%fortmacs_diff_directories) .or. &
                                                 associated(job_name, commands%fortmacs_diff_merge_directories) .or. &
                                                 associated(job_name, commands%fortmacs_diff_merge_directories_with_ancestor)
    end function is_fortmacs_diff_collect_diffs_metajob

    !> Whether session group job JOB_NAME merges.  Port of
    !> ediff-init.el:ediff-merge-metajob (the revision and file group jobs
    !> are Deferred).
    logical function is_fortmacs_diff_merge_metajob(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_fortmacs_diff_merge_metajob = associated(job_name, commands%fortmacs_diff_merge_directories) .or. &
                                         associated(job_name, commands%fortmacs_diff_merge_directories_with_ancestor)
    end function is_fortmacs_diff_merge_metajob

    !> Whether the sessions of session group job JOB_NAME involve three
    !> files.  Port of ediff-init.el:ediff-metajob3 (the file group jobs
    !> are Deferred).
    logical function is_fortmacs_diff_metajob3(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_fortmacs_diff_metajob3 = associated(job_name, commands%fortmacs_diff_merge_directories_with_ancestor) .or. &
                                    associated(job_name, commands%fortmacs_diff_directories3)
    end function is_fortmacs_diff_metajob3

    !> Whether session group job JOB_NAME compares three files.  Port of
    !> ediff-init.el:ediff-comparison-metajob3 (the file group job is
    !> Deferred).
    logical function is_fortmacs_diff_comparison_metajob3(job_name)
        type(command_type), pointer, intent(in) :: job_name

        is_fortmacs_diff_comparison_metajob3 = associated(job_name, commands%fortmacs_diff_directories3)
    end function is_fortmacs_diff_comparison_metajob3

    !> What the session group and registry buffers call job JOB_NAME
    !> ("Compare two directories"), "" for a job not ported.  Port of
    !> ediff-init.el:ediff-abbrev-jobname (the jobs it names by
    !> capitalizing the words of their names are spelled out).
    function abbreviate_fortmacs_diff_job_name(job_name) result(text)
        type(command_type), pointer, intent(in) :: job_name
        character(len=:), allocatable :: text

        if (associated(job_name, commands%fortmacs_diff_directories)) then
            text = "Compare two directories"
        else if (associated(job_name, commands%fortmacs_diff_files)) then
            text = "Compare two files"
        else if (associated(job_name, commands%fortmacs_diff_buffers)) then
            text = "Compare two buffers"
        else if (associated(job_name, commands%fortmacs_diff_directories3)) then
            text = "Compare three directories"
        else if (associated(job_name, commands%fortmacs_diff_files3)) then
            text = "Compare three files"
        else if (associated(job_name, commands%fortmacs_diff_buffers3)) then
            text = "Compare three buffers"
        else if (associated(job_name, commands%fortmacs_diff_merge_files)) then
            text = "Merge Files"
        else if (associated(job_name, commands%fortmacs_diff_merge_buffers)) then
            text = "Merge Buffers"
        else if (associated(job_name, commands%fortmacs_diff_merge_files_with_ancestor)) then
            text = "Merge Files With Ancestor"
        else if (associated(job_name, commands%fortmacs_diff_merge_buffers_with_ancestor)) then
            text = "Merge Buffers With Ancestor"
        else if (associated(job_name, commands%fortmacs_diff_merge_directories)) then
            text = "Merge Directories"
        else if (associated(job_name, commands%fortmacs_diff_merge_directories_with_ancestor)) then
            text = "Merge Directories With Ancestor"
        else
            text = ""
        end if
    end function abbreviate_fortmacs_diff_job_name

    !> The parent directory of DIRECTORY_IN (`default_directory' if it is
    !> ""); does nothing to "/".  If DIRECTORY_IN is a regular file, strip
    !> the file AND the last directory.  Port of
    !> ediff-init.el:ediff-strip-last-dir.
    subroutine strip_fortmacs_diff_last_directory(directory_in, result, status)
        character(len=*), intent(in) :: directory_in
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory
        integer :: position

        result = ""
        directory = directory_in

        if (len(directory) == 0) then
            if (current > 0) then
                if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
            end if
        end if

        call expand_file_name(directory, result, status)
        if (.not. status%ok()) return
        directory = result
        if (.not. is_file_directory(directory)) directory = file_name_directory(directory)
        position = len(directory)

        if (position > 1) then
            if (directory(position:position) == "/") directory = directory(1:position - 1)
        end if

        result = abbreviate_file_name(file_name_directory(directory))
    end subroutine strip_fortmacs_diff_last_directory

    !> Define the faces of ediff-init.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => fortmacs_diff_current_diff_a
        call f%define("fortmacs_diff_current_diff_a", "Face for highlighting the selected difference in buffer A.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#ffdddd", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#553333", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="firebrick", background="pale green", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="blue3", background="yellow3", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true., extend=.true.))
        f => fortmacs_diff_current_diff_b
        call f%define("fortmacs_diff_current_diff_b", "Face for highlighting the selected difference in buffer B.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#ddffdd", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#335533", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="DarkOrchid", background="Yellow", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="magenta3", background="yellow3", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true., extend=.true.))
        f => fortmacs_diff_current_diff_c
        call f%define("fortmacs_diff_current_diff_c", "Face for highlighting the selected difference in buffer C.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#ffffaa", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#888833", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Navy", background="Pink", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="cyan3", background="yellow3", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true., extend=.true.))
        f => fortmacs_diff_current_diff_ancestor
        call f%define("fortmacs_diff_current_diff_ancestor", "Face for highlighting the selected difference in buffer Ancestor.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#cfdeee", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#004151", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#cfdeee", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#004151", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="black", background="magenta3", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true., extend=.true.))
        f => fortmacs_diff_fine_diff_a
        call f%define("fortmacs_diff_fine_diff_a", "Face for highlighting the refinement of the selected diff in buffer A.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#ffbbbb"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#882222"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Navy", background="sky blue"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="white", background="sky blue", weight="bold"))
        call f%specification_entry(display_when(), face_attributes(underline=.true.))
        f => fortmacs_diff_fine_diff_b
        call f%define("fortmacs_diff_fine_diff_b", "Face for highlighting the refinement of the selected diff in buffer B.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#aaffaa"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#228822"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Black", background="cyan"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), face_attributes(foreground="magenta3", &
                                                                                             background="cyan3"))
        call f%specification_entry(display_when(), face_attributes(underline=.true.))
        f => fortmacs_diff_fine_diff_c
        call f%define("fortmacs_diff_fine_diff_c", "Face for highlighting the refinement of the selected diff in buffer C.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#ffff55"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#aaaa22"))
        call f%specification_entry(display_when(types=["pc"]), face_attributes(foreground="white", background="Turquoise"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Black", background="Turquoise"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="yellow3", background="Turquoise", weight="bold"))
        call f%specification_entry(display_when(), face_attributes(underline=.true.))
        f => fortmacs_diff_fine_diff_ancestor
        call f%define("fortmacs_diff_fine_diff_ancestor", &
                      "Face for highlighting refinement of the selected diff in the ancestor buffer."//achar(10)// &
                      "At present, this face is not used and no fine differences are"//achar(10)// &
                      "computed for the ancestor buffer.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#00c5c0"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#009591"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="#00c5c0"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(background="#009591"))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), face_attributes(foreground="red3", background="green"))
        call f%specification_entry(display_when(), face_attributes(underline=.true.))
        call define_background_faces()
    end subroutine define_faces

    !> Define the even and odd faces of ediff-init.el.  Not a port.
    subroutine define_background_faces()
        type(named_face_type), pointer :: f

        f => fortmacs_diff_even_diff_a
        call f%define("fortmacs_diff_even_diff_a", "Face for highlighting even-numbered non-current differences in buffer A.")
        call f%specification_entry(display_when(types=["pc"]), &
                                   face_attributes(foreground="green3", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dark grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Black", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="red3", background="light grey", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_even_diff_b
        call f%define("fortmacs_diff_even_diff_b", "Face for highlighting even-numbered non-current differences in buffer B.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dim grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="White", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="blue3", background="Grey", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_even_diff_c
        call f%define("fortmacs_diff_even_diff_c", "Face for highlighting even-numbered non-current differences in buffer C.")
        call f%specification_entry(display_when(types=["pc"]), &
                                   face_attributes(foreground="yellow3", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dark grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Black", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="yellow3", background="light grey", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_even_diff_ancestor
        call f%define("fortmacs_diff_even_diff_ancestor", &
                      "Face for highlighting even-numbered non-current differences in ancestor buffer.")
        call f%specification_entry(display_when(types=["pc"]), &
                                   face_attributes(foreground="cyan3", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dim grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="White", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="cyan3", background="light grey", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_odd_diff_a
        call f%define("fortmacs_diff_odd_diff_a", "Face for highlighting odd-numbered non-current differences in buffer A.")
        call f%specification_entry(display_when(types=["pc"]), &
                                   face_attributes(foreground="green3", background="gray40", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dim grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="White", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="red3", background="black", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_odd_diff_b
        call f%define("fortmacs_diff_odd_diff_b", "Face for highlighting odd-numbered non-current differences in buffer B.")
        call f%specification_entry(display_when(types=["pc"]), &
                                   face_attributes(foreground="White", background="gray40", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dark grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="Black", background="light grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="cyan3", background="black", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_odd_diff_c
        call f%define("fortmacs_diff_odd_diff_c", "Face for highlighting odd-numbered non-current differences in buffer C.")
        call f%specification_entry(display_when(types=["pc"]), &
                                   face_attributes(foreground="yellow3", background="gray40", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(distant_foreground="Black", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                backgrounds=[FACE_BACKGROUND_DARK]), &
                                   face_attributes(distant_foreground="White", background="dim grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="White", background="Grey", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="yellow3", background="black", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
        f => fortmacs_diff_odd_diff_ancestor
        call f%define("fortmacs_diff_odd_diff_ancestor", &
                      "Face for highlighting odd-numbered non-current differences in ancestor buffer.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88), &
                                   face_attributes(background="gray40", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                   face_attributes(foreground="cyan3", background="gray40", extend=.true.))
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR]), &
                                   face_attributes(foreground="green3", background="black", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(slant="italic", extend=.true.))
    end subroutine define_background_faces

    !> Define the faces and give the string options their defaults.
    !> Not a port.
    subroutine initialize_fortmacs_diff_initialize()
        if (len_trim(fortmacs_diff_current_diff_a%name) > 0) return
        call define_faces()
        if (.not. allocated(fortmacs_diff_before_flag_beginning_of_line)) fortmacs_diff_before_flag_beginning_of_line = "->>"
        if (.not. allocated(fortmacs_diff_after_flag_end_of_line)) fortmacs_diff_after_flag_end_of_line = "<<-"
        if (.not. allocated(fortmacs_diff_before_flag_mol)) fortmacs_diff_before_flag_mol = "->>"
        if (.not. allocated(fortmacs_diff_after_flag_mol)) fortmacs_diff_after_flag_mol = "<<-"
        if (.not. allocated(fortmacs_diff_coding_system_for_read)) fortmacs_diff_coding_system_for_read = "raw-text"
        if (.not. allocated(fortmacs_diff_coding_system_for_write)) fortmacs_diff_coding_system_for_write = "emacs-internal"
        if (.not. allocated(fortmacs_diff_merge_filename_prefix)) fortmacs_diff_merge_filename_prefix = "merge_"
    end subroutine initialize_fortmacs_diff_initialize
end module fortmacs_diff_settings_mod
