! log_view_mod.f90 -- major mode for browsing revision log histories.
! Ported from GNU Emacs 31.1 lisp/vc/log-view.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> Log View mode, the major mode of VERSION_CONTROL's log buffers: moving between
!> log messages (n, p, TAB, <backtab>) and files, marking entries
!> (m, u, U), expanding short entries (RET), diffs of an entry or a
!> region (d, =, D, M-RET, M-n, M-p), visiting (f) or annotating (a) a
!> revision and copying revision IDs (w).
!>
!> The buffer-local variables `log_view_file_regular_expression', `log_view_message_regular_expression',
!> `log_view_per_file_logs', `log_view_expanded_log_entry_function',
!> `log_view_version_control_fileset' and `log_view_version_control_backend' are set with
!> `set_log_view_file_regular_expression' and its siblings, as `set_local' does.
!>
!> Deferred: `log_view_modify_change_comment' (e, needing the backend's
!> `modify-change-comment'), `log_view_cherry_pick' (C), `log_view_revert_or_delete_revisions' (R),
!> `log_view_uncommit_revisions_from_end' (x),
!> `log_view_delete_revisions_from_end' (X), `log_view_revert_revisions'
!> and `log_view_delete_revisions' (needing `vc--pick-or-revert' and
!> working trees), PCL-CVS linkage (`log_view_minor_wrap',
!> `cvs-pcl-cvs-dirchange-re'), `log_view_extract_comment',
!> `beginning_of_subprogram_function' and `end_of_subprogram_function' (lisp.el's
!> subprogram motion is not ported), restoring marks after `revert_buffer'
!> (`log_view__restore_marks'), the menu, `project-find-matching-buffer-function'
!> and directory-local variables.
module log_view_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, xstring, vtag, V_NULL
    use data_mod, only: symbol_value, set, append_locals_clearer, value_eq
    use hooks_mod, only: hook_type
    use buffer_local_setting_mod, only: buffer_local_setting_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, buffer_serial_live, is_buffer_live, deactivate_mark_variable
    use commands_mod, only: commands
    use modes_mod, only: major_modes
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_buffer, &
                                     is_end_of_buffer, is_beginning_of_line, line_beginning_position, delete_region, &
                                     region_beginning, region_end
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line
    use search_mod, only: looking_at, search_regular_expression_forward => regular_expression_search_forward, &
                          search_regular_expression_backward => regular_expression_search_backward, match_beginning, match_end, &
                          SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string_no_properties, looking_back
    use text_properties_mod, only: get_text_property, put_text_property, next_single_property_change, get_character_property, &
                                   get_character_property_and_overlay
    use interval_tree_mod, only: make_overlay, overlay_put, delete_overlay, overlay_get, overlay_end
    use easy_minor_mode_mod, only: easy_mmode_next, easy_mmode_previous
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use simple_mod, only: is_use_region, copy_to_clipboard_history, line_number_at_position, special_mode
    use syntax_mod, only: skip_syntax_backward
    use display_engine_mod, only: message
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, FONT_LOCK_OVERRIDE_APPEND
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight
    use keymap_mod, only: keymap_set
    use file_input_output_mod, only: is_file_directory
    use version_control_hooks_mod, only: version_control_backends
    use version_control_mod, only: version_control_fileset_type, version_control_diff_internal, version_control_find_revision, &
                                   version_control_root_directory, version_control_allow_asynchronous_diff
    use string_type_mod, only: string_type, prepend_string, append_string
    implicit none(type, external)
    private

    public :: log_view_entry_procedure, log_view_mode, log_view_message_next, log_view_message_previous, log_view_file_next, &
              log_view_file_previous, log_view_goto_revision, log_view_message_and_diff_next, log_view_message_and_diff_previous, &
              log_view_current_file, log_view_current_entry, is_log_view_inside_comment, log_view_current_tag, &
              log_view_toggle_mark_entry, log_view_mark_entry, log_view_unmark_entry, log_view_unmark_all_entries, &
              log_view_get_marked, log_view_toggle_entry_display, log_view_display_entry_and_diff, &
              log_view_beginning_of_entry, log_view_end_of_entry, log_view_find_revision, log_view_diff, &
              log_view_diff_changeset, log_view_diff_common, log_view_copy_revision, log_view_annotate_version, &
              log_view_version_control_annotate_procedure, &
              log_view_file_regular_expression, log_view_message_regular_expression, log_view_per_file_logs, &
              log_view_version_control_fileset, &
              log_view_version_control_backend, &
              set_log_view_file_regular_expression, set_log_view_message_regular_expression, set_log_view_per_file_logs, &
              set_log_view_version_control_fileset, &
              set_log_view_version_control_backend, &
              set_log_view_font_lock_keywords, log_view_font_lock_keywords, initialize_log_view

    abstract interface
        !> `log_view_expanded_log_entry_function': return in TEXT the detailed
        !> description of the Log View entry REVISION (FOUND .false. for none).
        !> Not a port.
        recursive subroutine log_view_entry_procedure(revision, text, found)
            implicit none(type, external)
            character(len=*), intent(in) :: revision
            character(len=:), allocatable, intent(out) :: text
            logical, intent(out) :: found
        end subroutine log_view_entry_procedure

        !> vc-annotate.el's `version_control_annotate' of revision REVISION of FILE with
        !> BACKEND (no display mode, buffer or line).  Not a port.
        recursive subroutine log_view_version_control_annotate_procedure(file, revision, backend, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file, revision
            integer, intent(in) :: backend
            type(status_type), intent(out) :: status
        end subroutine log_view_version_control_annotate_procedure
    end interface

    !> vc-annotate.el's `version_control_annotate' for `log_view_annotate_version',
    !> associated once VC-Annotate is loaded (version_control_annotate_mod).
    procedure(log_view_version_control_annotate_procedure), pointer, public :: log_view_version_control_annotate => null()

    !> Hook run at the end of `log_view_mode'.
    type(hook_type), target, public :: log_view_mode_hook = hook_type(name="log_view_mode_hook")

    !> Face for the file header line in `log_view_mode'.
    type(named_face_type), target, public :: log_view_file_face
    !> Face for the message header line in `log_view_mode'.
    type(named_face_type), target, public :: log_view_message_face
    !> Face for the commit body in `log_view_mode'.
    type(named_face_type), target, public :: log_view_commit_body

    !> The keymap of `log_view_mode'.
    type(handle_type), public :: log_view_mode_map

    !> Regexp matching the text identifying the file (the default value of
    !> `log_view_file_regular_expression').
    !> The match group number 1 should match the file name itself.
    character(len=*), parameter, public :: LOG_VIEW_FILE_REGULAR_EXPRESSION_DEFAULT = &
                                           "^\(?:Working file: \(?1:.+\)" &       ! RCS and CONCURRENT_VERSIONS_SYSTEM.
                                           //"\|\(?:SCCS/s\.\|Changes to \)\(?1:.+\):" & ! SCCS and Darcs.
                                           //"\)"//achar(10)  ! Include the \n for font-lock reasons.

    !> Regexp matching the text identifying a revision (the default value
    !> of `log_view_message_regular_expression').
    !> The match group number 1 should match the revision number itself.
    character(len=*), parameter, public :: LOG_VIEW_MESSAGE_REGULAR_EXPRESSION_DEFAULT = &
                                           ! RCS and CONCURRENT_VERSIONS_SYSTEM.
                                           "^\(?:revision \(?1:[.0-9]+\)\(?:"//achar(9)//".*\)?" &
                                           //"\|r\(?1:[0-9]+\) | .* | .*" &                         ! Subversion.
                                           //"\|D \(?1:[.0-9]+\) .*" &                              ! SCCS.
                                           ! Darcs doesn't have revision names.  VC-darcs uses patch names
                                           ! instead.  Darcs patch names are hashcodes, which do not appear
                                           ! in the log output :-(, but darcs accepts any prefix of the log
                                           ! message as a patch name, so we match the first line of the log
                                           ! message.
                                           ! First loosely match the date format.
                                           //"\|[^ "//achar(10)//"].*[^0-9"//achar(10)//"][0-9][0-9]:[0-9][0-9][^0-9" &
                                           //achar(10)//"].*[^ "//achar(10)//"]" &
                                           ! Email of user and finally Msg, used as revision name.
                                           //"  .*@.*"//achar(10)//"\(?:  \* \(?1:.*\)\)?" &
                                           //"\)$"

    ! "^cvs[.ex]* [a-z]+: Logging \\(.+\\)$"
    character(len=*), parameter :: LOG_VIEW_DIRECTORY_REGULAR_EXPRESSION = "^cvs[.ex]* [a-z]+: Logging \(.+\)$"

    !> One buffer's own value of
    !> `log_view_expanded_log_entry_function'.
    type :: log_view_expanded_log_entry_entry_type
        procedure(log_view_entry_procedure), pointer, nopass :: procedure => null()
    end type log_view_expanded_log_entry_entry_type

    !> The type of
    !> `log_view_expanded_log_entry_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: log_view_expanded_log_entry_setting_type
        procedure(log_view_entry_procedure), pointer, nopass :: procedure => null()
        type(log_view_expanded_log_entry_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_log_view_expanded_log_entry_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_log_view_expanded_log_entry_current
        procedure :: append_local_entry => append_log_view_expanded_log_entry_entry
        procedure :: clear_local_entry => clear_log_view_expanded_log_entry_entry
    end type log_view_expanded_log_entry_setting_type

    !> Function returning the detailed description of a Log View entry.
    !> It is called by the command `log_view_toggle_entry_display' with
    !> one argument, the revision ID of the current entry, and gives its
    !> long description in TEXT (FOUND .false. for none).  Port of
    !> log-view.el log-view-expanded-log-entry-function (`%procedure' is
    !> the global value, null for none; `%set_local' gives a buffer its
    !> own).
    type(log_view_expanded_log_entry_setting_type), public :: log_view_expanded_log_entry_function

    ! The buffer-local values of a Log View buffer, by serial number:
    ! log-view-file-re, log-view-message-re, log-view-per-file-logs,
    ! log-view-font-lock-keywords, log-view-vc-fileset and
    ! log-view-vc-backend.
    type :: local_type
        integer(int64) :: serial = 0
        character(len=:), allocatable :: file_regular_expression, message_regular_expression
        logical :: has_per_file_logs = .false.
        logical :: per_file_logs = .true.
        logical :: has_keywords = .false.
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: has_fileset = .false.
        type(string_type), allocatable :: fileset(:)
        integer :: backend = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
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
    subroutine clear_log_view_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = local_type()
        call log_view_expanded_log_entry_function%clear_local()
    end subroutine clear_log_view_locals

    !> The current buffer's `log_view_file_regular_expression': the regexp matching the
    !> text identifying the file.  The match group number 1 should match
    !> the file name itself.  Not a port.
    function log_view_file_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression
        integer :: k

        regular_expression = LOG_VIEW_FILE_REGULAR_EXPRESSION_DEFAULT
        k = local_index()

        if (k > 0) then
            if (allocated(locals(k)%file_regular_expression)) regular_expression = locals(k)%file_regular_expression
        end if
    end function log_view_file_regular_expression

    !> The current buffer's `log_view_message_regular_expression': the regexp matching the
    !> text identifying a revision.  The match group number 1 should match
    !> the revision number itself.  Not a port.
    function log_view_message_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression
        integer :: k

        regular_expression = LOG_VIEW_MESSAGE_REGULAR_EXPRESSION_DEFAULT
        k = local_index()

        if (k > 0) then
            if (allocated(locals(k)%message_regular_expression)) regular_expression = locals(k)%message_regular_expression
        end if
    end function log_view_message_regular_expression

    !> The current buffer's `log_view_per_file_logs': true if the logs are
    !> shown one file at a time.  Not a port.
    logical function log_view_per_file_logs()
        integer :: k

        log_view_per_file_logs = .true.
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_per_file_logs) log_view_per_file_logs = locals(k)%per_file_logs
        end if
    end function log_view_per_file_logs

    !> The current buffer's `log_view_version_control_fileset': the VERSION_CONTROL fileset
    !> corresponding to the current log (HAS .false. for none).  Not a port.
    subroutine log_view_version_control_fileset(fileset, has)
        type(string_type), allocatable, intent(out) :: fileset(:)
        logical, intent(out) :: has
        integer :: k

        has = .false.
        k = local_index()
        if (k > 0) has = locals(k)%has_fileset

        if (has) then
            fileset = locals(k)%fileset
        else
            allocate (fileset(0))
        end if
    end subroutine log_view_version_control_fileset

    !> The current buffer's `log_view_version_control_backend': the VERSION_CONTROL backend that
    !> created the current log (0 for none).  Not a port.
    integer function log_view_version_control_backend() result(b)
        integer :: k

        b = 0
        k = local_index()
        if (k > 0) b = locals(k)%backend
    end function log_view_version_control_backend

    !> The current buffer's `log_view_font_lock_keywords' (the default
    !> highlights the file and message lines when not set).  Not a port.
    subroutine log_view_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        type(font_lock_highlight_type) :: h(1)
        integer :: k

        k = local_index()

        if (k > 0) then
            if (locals(k)%has_keywords) then
                keywords = locals(k)%keywords
                return
            end if
        end if

        ! We use `eval' so as to use the buffer-local value of
        ! log-view-file-re and log-view-message-re, if applicable.
        ! ((1 (if (boundp 'cvs-filename-face) cvs-filename-face)): PCL-CVS
        ! is not ported.)
        allocate (keywords(2))
        h(1) = font_lock_highlight(0, face="log_view_file", override=FONT_LOCK_OVERRIDE_APPEND)
        keywords(1) = font_lock_keyword(log_view_file_regular_expression(), highlights=h)
        keywords(2) = font_lock_keyword(log_view_message_regular_expression(), face="log_view_message")
    end subroutine log_view_font_lock_keywords

    !> Give the current buffer its own `log_view_file_regular_expression', REGULAR_EXPRESSION (set_local).  Not a port.
    subroutine set_log_view_file_regular_expression(regular_expression)
        character(len=*), intent(in) :: regular_expression
        integer :: k

        call make_local_entry(k)
        locals(k)%file_regular_expression = regular_expression
    end subroutine set_log_view_file_regular_expression

    !> Give the current buffer its own `log_view_message_regular_expression', REGULAR_EXPRESSION (set_local).  Not a port.
    subroutine set_log_view_message_regular_expression(regular_expression)
        character(len=*), intent(in) :: regular_expression
        integer :: k

        call make_local_entry(k)
        locals(k)%message_regular_expression = regular_expression
    end subroutine set_log_view_message_regular_expression

    !> Give the current buffer its own `log_view_per_file_logs', FLAG (set_local).  Not a port.
    subroutine set_log_view_per_file_logs(flag)
        logical, intent(in) :: flag
        integer :: k

        call make_local_entry(k)
        locals(k)%has_per_file_logs = .true.
        locals(k)%per_file_logs = flag
    end subroutine set_log_view_per_file_logs

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `log_view_expanded_log_entry_function'.  Not a port.
    subroutine set_log_view_expanded_log_entry_local(self, local_procedure)
        class(log_view_expanded_log_entry_setting_type), intent(inout) :: self
        procedure(log_view_entry_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_log_view_expanded_log_entry_local

    !> The current buffer's value of
    !> `log_view_expanded_log_entry_function':
    !> its own, else the global one.  Not a port.
    function get_log_view_expanded_log_entry_current(self) result(current_procedure)
        class(log_view_expanded_log_entry_setting_type), intent(in) :: self
        procedure(log_view_entry_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_log_view_expanded_log_entry_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_log_view_expanded_log_entry_entry(self)
        class(log_view_expanded_log_entry_setting_type), intent(inout) :: self
        type(log_view_expanded_log_entry_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_log_view_expanded_log_entry_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_log_view_expanded_log_entry_entry(self, k)
        class(log_view_expanded_log_entry_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_log_view_expanded_log_entry_entry

    !> Give the current buffer its own `log_view_version_control_fileset', FILESET (set_local).  Not a port.
    subroutine set_log_view_version_control_fileset(fileset)
        type(string_type), intent(in) :: fileset(:)
        integer :: k

        call make_local_entry(k)
        locals(k)%has_fileset = .true.
        locals(k)%fileset = fileset
    end subroutine set_log_view_version_control_fileset

    !> Give the current buffer its own `log_view_version_control_backend', B (set_local).  Not a port.
    subroutine set_log_view_version_control_backend(b)
        integer, intent(in) :: b
        integer :: k

        call make_local_entry(k)
        locals(k)%backend = b
    end subroutine set_log_view_version_control_backend

    !> Give the current buffer its own `log_view_font_lock_keywords', KEYWORDS (set_local).  Not a port.
    subroutine set_log_view_font_lock_keywords(keywords)
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        integer :: k

        call make_local_entry(k)
        locals(k)%has_keywords = .true.
        locals(k)%keywords = keywords
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
    end subroutine set_log_view_font_lock_keywords

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for browsing CONCURRENT_VERSIONS_SYSTEM log output.  Port of
    !> log-view.el:log-view-mode.
    recursive subroutine log_view_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved

        if (.not. associated(major_modes%log_view_mode%execute)) call initialize_log_view()
        call major_modes%log_view_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .true.
        call log_view_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call major_modes%log_view_mode%end(saved, status)
    end subroutine log_view_mode

    ! ------------------------------------------------------------------
    ! Navigation
    ! ------------------------------------------------------------------

    !> Go to the next COUNT'th log message.
    !>
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-next (easy-mmode-define-navigation).
    recursive subroutine log_view_message_next(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(log_view_message_regular_expression(), "log message", n, status)
    end subroutine log_view_message_next

    !> Go to the previous COUNT'th log message.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-prev.
    recursive subroutine log_view_message_previous(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_previous(log_view_message_regular_expression(), "log message", n, status)
    end subroutine log_view_message_previous

    !> Go to the next COUNT'th file.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-file-next.
    recursive subroutine log_view_file_next(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(log_view_file_regular_expression(), "file", n, status)
    end subroutine log_view_file_next

    !> Go to the previous COUNT'th file.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-file-prev.
    recursive subroutine log_view_file_previous(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_previous(log_view_file_regular_expression(), "file", n, status)
    end subroutine log_view_file_previous

    !> Go to revision REVISION (FOUND false if there is none).  Port of
    !> log-view.el:log-view-goto-rev.
    recursive subroutine log_view_goto_revision(revision, found)
        character(len=*), intent(in) :: revision
        logical, intent(out) :: found
        character(len=:), allocatable :: tag
        logical :: has
        type(status_type) :: st

        call goto_character(point_minimum())
        found = .false.

        do
            call log_view_current_tag(tag, has)

            if (has) then
                if (tag == revision) exit
            end if

            ! (ignore-errors ...)
            call log_view_message_next(st)
            if (.not. st%ok()) return
        end do

        found = .true.
    end subroutine log_view_goto_revision

    !> Go to next COUNT'th entry, expand it if possible, and show its diff.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-and-diff-next.
    recursive subroutine log_view_message_and_diff_next(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status

        call log_view_message_next(status, count)
        if (status%ok()) call log_view_display_entry_and_diff(status)
    end subroutine log_view_message_and_diff_next

    !> Go to previous COUNT'th entry, expand it if possible, and show its diff.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-and-diff-prev.
    recursive subroutine log_view_message_and_diff_previous(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status

        call log_view_message_next(status, -count)
        if (status%ok()) call log_view_display_entry_and_diff(status)
    end subroutine log_view_message_and_diff_previous

    ! ------------------------------------------------------------------
    ! Linkage to PCL-CVS (mostly copied from cvs-status.el)
    ! ------------------------------------------------------------------

    !> Return the current file.  Port of log-view.el:log-view-current-file
    !> (without PCL-CVS's directory changes).
    recursive subroutine log_view_current_file(file, status)
        use file_input_output_mod, only: expand_file_name
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, cvsdir, directory, regular_expression
        integer(int64) :: original_point, shortage, found
        type(status_type) :: st

        status = status_type()
        original_point = point()
        shortage = forward_line(1_int64)
        regular_expression = log_view_file_regular_expression()
        call search_regular_expression_backward(regular_expression, found, st, noerror=SEARCH_NOERROR)
        if (found == 0) call search_regular_expression_forward(regular_expression, found, st, noerror=SEARCH_NOERROR)

        if (found == 0) then
            call goto_character(original_point)
            call set_error(status, GENERIC_ERROR_CODE, "Unable to determine the current file")
            return
        end if

        name = match_string_no_properties(1)
        call search_regular_expression_backward(LOG_VIEW_DIRECTORY_REGULAR_EXPRESSION, found, st, noerror=SEARCH_NOERROR)
        directory = ""

        if (found /= 0) then
            cvsdir = match_string_no_properties(1)
            call expand_file_name(cvsdir, directory, st, "")
        end if

        call expand_file_name(name, file, st, directory)
        call goto_character(original_point)
    end subroutine log_view_current_file

    !> Return the position and revision tag of the Log View entry at POSITION.
    !> This is (BEGINNING, TAG), where BEGINNING is a buffer position and TAG is a
    !> string.  If POSITION is absent, it defaults to point.
    !> If there is no entry at POSITION, FOUND is false.
    !>
    !> If optional arg MOVE is true, move point to BEGINNING if found.
    !> Otherwise, don't move point.  Port of
    !> log-view.el:log-view-current-entry.
    recursive subroutine log_view_current_entry(beginning, tag, found, position, move)
        integer(int64), intent(out) :: beginning
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: position
        logical, intent(in), optional :: move
        integer(int64) :: original_point, shortage, p
        logical :: looping, separator
        character(len=:), allocatable :: regular_expression
        type(status_type) :: st

        found = .false.
        beginning = 0
        tag = ""
        original_point = point()
        if (present(position)) call goto_character(position)
        shortage = forward_line(0_int64)
        ! Treat "---" separator lines as part of the following revision.
        separator = looking_at("-\{20,\}$", st)

        if (separator) then
            shortage = forward_line(2_int64)
        else
            shortage = forward_line(1_int64)
        end if

        regular_expression = log_view_message_regular_expression()
        looping = .true.
        p = 0

        do while (looping)
            call search_regular_expression_backward(regular_expression, p, st, noerror=SEARCH_MOVE)
            looping = .false.
            if (p /= 0) looping = is_log_view_inside_comment(point())
        end do

        if (p /= 0) then
            found = .true.
            beginning = p
            tag = match_string_no_properties(1)
        end if

        call goto_character(original_point)

        if (present(move)) then
            if (move .and. found) call goto_character(beginning)
        end if
    end subroutine log_view_current_entry

    !> Return true if POSITION lies inside an expanded log entry.  Port of
    !> log-view.el:log-view-inside-comment-p.
    logical function is_log_view_inside_comment(position)
        integer(int64), intent(in) :: position
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(position, value_type(name="log_view_comment"), v, st)
        is_log_view_inside_comment = .false.
        if (st%ok()) is_log_view_inside_comment = value_eq(v, value_type(.true.))
    end function is_log_view_inside_comment

    !> Return the revision tag (a string) of the Log View entry at POSITION.
    !> if POSITION is omitted, it defaults to point (HAS .false. for none).
    !> Port of log-view.el:log-view-current-tag.
    recursive subroutine log_view_current_tag(tag, has, position)
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: has
        integer(int64), intent(in), optional :: position
        integer(int64) :: beginning

        call log_view_current_entry(beginning, tag, has, position)
    end subroutine log_view_current_tag

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    !> Toggle the marked state for the log entry at point.
    !> See `log_view_mark_entry'.  Port of
    !> log-view.el:log-view-toggle-mark-entry.
    recursive subroutine log_view_toggle_mark_entry(status)
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, beginning
        character(len=:), allocatable :: tag
        logical :: found

        status = status_type()
        original_point = point()
        call log_view_current_entry(beginning, tag, found)

        if (found) then
            if (vtag(get_character_property(beginning, value_type(name="log_view_self"))) /= V_NULL) then
                call log_view_unmark_entry(1_int64, status)
            else
                call log_view_mark_entry(1_int64, status)
            end if
        end if

        call goto_character(original_point)
    end subroutine log_view_toggle_mark_entry

    ! Call `log_view__mark_entry' (MARK) or `log_view__unmark_entry' on
    ! each line of an active region or ARGUMENT times.  The last line of an
    ! active region is excluded in the case that the region ends right at
    ! the beginning of the line, or after only non-word characters.
    ! Port of log-view.el:log-view--mark-unmark.
    recursive subroutine mark_unmark(mark, argument, status, beginning, end, restore)
        logical, intent(in) :: mark
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        !> The table of `log_view__restore_marks' (marks the entries whose
        !> tags are in it, moves past the others).
        character(len=*), intent(in), optional :: restore(:)
        integer(int64) :: lastl, n, processed_line, original_point, moved
        integer(int64) :: k

        status = status_type()

        if (present(beginning) .neqv. present(end)) then
            call set_error(status, GENERIC_ERROR_CODE, "log-view--mark-unmark called with invalid arguments")
            return
        end if

        if (present(beginning)) then
            processed_line = -1
            ! Exclude the region's last line if the region ends right
            ! at the beginning of that line or almost at the beginning.
            ! This is like the `file' value of `dired-mark-region'.
            ! We don't want to include the last line unless the region
            ! visually includes that revision.
            original_point = point()
            call goto_character(end)
            moved = skip_syntax_backward("^w")

            if (is_beginning_of_line()) then
                lastl = line_number_at_position() - 1
            else
                lastl = line_number_at_position()
            end if

            call goto_character(beginning)

            do
                n = line_number_at_position()
                ! Make sure we don't get stuck processing the
                ! same line infinitely.
                if (.not. (n <= lastl)) exit
                if (n == processed_line) exit
                processed_line = n
                call toggle_entry()
                if (.not. status%ok()) exit
            end do

            call goto_character(original_point)
            deactivate_mark_variable = .true.
        else
            do k = 1, argument
                call toggle_entry()
                if (.not. status%ok()) return
            end do
        end if

    contains

        recursive subroutine toggle_entry()
            character(len=:), allocatable :: tag
            logical :: has

            if (present(restore)) then
                call log_view_current_tag(tag, has)

                if (has) then
                    if (any(restore == tag)) then
                        call mark_entry(status)
                        return
                    end if
                end if

                call log_view_message_next(status)
            else if (mark) then
                call mark_entry(status)
            else
                call unmark_entry(status)
            end if
        end subroutine toggle_entry
    end subroutine mark_unmark

    !> Mark the log entry at point.
    !> If the region is active in Transient Mark mode, mark all entries.
    !> When called with a prefix argument, mark that many log entries.
    !> When called from a program, mark ARGUMENT entries or all entries lying
    !> between BEGINNING and END.  If BEGINNING and END are supplied, ARGUMENT is ignored.
    !>
    !> When entries are marked, some commands that usually operate on the entry
    !> at point will instead operate on all marked entries.
    !> Use \[log_view_unmark_entry] to unmark an entry.
    !>
    !> Programs can use `log_view_get_marked' to obtain a list of all
    !> marked revisions.  Port of log-view.el:log-view-mark-entry.
    recursive subroutine log_view_mark_entry(argument, status, beginning, end)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end

        call mark_unmark(.true., argument, status, beginning, end)
    end subroutine log_view_mark_entry

    ! Mark the log entry at point.  Port of log-view.el:log-view--mark-entry.
    recursive subroutine mark_entry(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end, original_point
        character(len=:), allocatable :: tag
        type(value_type) :: expanded
        type(handle_type) :: ov
        logical :: found
        type(status_type) :: st

        status = status_type()
        call log_view_current_entry(beginning, tag, found)

        if (found) then
            original_point = point()
            call goto_character(beginning)

            if (vtag(get_character_property(beginning, value_type(name="log_view_self"))) == V_NULL) then
                call get_text_property(beginning, value_type(name="log_view_entry_expanded"), expanded, st)

                if (allocated(expanded%a)) then
                    call next_single_property_change(beginning, value_type(name="log_view_comment"), end, found, st)
                    if (.not. found) end = point_maximum()
                else
                    call log_view_end_of_entry()
                    end = point()
                end if

                call make_overlay(ov, beginning, end, st)
                call overlay_put(ov, "face", value_type(name="log_view_file"), st)
                ! This is used to check if the overlay is present.
                call overlay_put(ov, "log_view_self", value_type(.true.), st)
                call overlay_put(ov, "log_view_marked", value_type(text=tag), st)
            end if

            call goto_character(original_point)
        end if

        call log_view_message_next(status, 1_int64)
    end subroutine mark_entry

    !> Unmark the log entry at point.
    !> If the region is active in Transient Mark mode, unmark all entries.
    !> When called with a prefix argument, unmark that many log entries.
    !> When called from a program, mark ARGUMENT entries or all entries lying
    !> between BEGINNING and END.  If BEGINNING and END are supplied, ARGUMENT is ignored.
    !>
    !> See `log_view_mark_entry'.  Port of log-view.el:log-view-unmark-entry.
    recursive subroutine log_view_unmark_entry(argument, status, beginning, end)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end

        call mark_unmark(.false., argument, status, beginning, end)
    end subroutine log_view_unmark_entry

    ! Unmark the log entry at point.  Port of
    ! log-view.el:log-view--unmark-entry.
    recursive subroutine unmark_entry(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning
        character(len=:), allocatable :: tag
        type(value_type) :: v
        type(handle_type) :: ov
        logical :: found

        call log_view_current_entry(beginning, tag, found)

        if (found) then
            call get_character_property_and_overlay(beginning, value_type(name="log_view_self"), v, ov)
            if (allocated(v%a) .and. ov%index /= 0) call delete_overlay(ov)
        end if

        call log_view_message_next(status, 1_int64)
    end subroutine unmark_entry

    !> Unmark all marked log entries in this buffer.  Port of
    !> log-view.el:log-view-unmark-all-entries.
    recursive subroutine log_view_unmark_all_entries(status)
        type(status_type), intent(out) :: status

        call mark_unmark(.false., 0_int64, status, point_minimum(), point_maximum())
    end subroutine log_view_unmark_all_entries

    !> Return the list of tags for the marked log entries.  Port of
    !> log-view.el:log-view-get-marked.
    subroutine log_view_get_marked(marked)
        type(string_type), allocatable, intent(out) :: marked(:)
        type(value_type) :: v, tag
        type(handle_type) :: ov
        integer(int64) :: position, next
        logical :: found
        type(status_type) :: st

        allocate (marked(0))
        position = point_minimum()

        do
            call next_single_property_change(position, value_type(name="face"), next, found, st)
            if (.not. found) exit
            position = next
            call get_character_property_and_overlay(position, value_type(name="log_view_self"), v, ov)

            if (allocated(v%a) .and. ov%index /= 0) then
                tag = overlay_get(ov, "log_view_marked")
                ! (push ...): the last found comes first.
                call prepend_string(marked, xstring(tag))
                position = overlay_end(ov)
            end if
        end do
    end subroutine log_view_get_marked

    ! ------------------------------------------------------------------
    ! Expanding entries
    ! ------------------------------------------------------------------

    !> If possible, expand the current Log View entry.
    !> This calls `log_view_expanded_log_entry_function' to do the work.
    !> See also `log_view_display_entry_and_diff'.  Port of
    !> log-view.el:log-view-toggle-entry-display.
    recursive subroutine log_view_toggle_entry_display(status)
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, beginning, position, last
        character(len=:), allocatable :: tag, long_entry
        type(value_type) :: expanded, saved, properties(4)
        logical :: found, has, changed
        procedure(log_view_entry_procedure), pointer :: expand

        status = status_type()
        ! Don't do anything unless `log_view_expanded_log_entry_function'
        ! is defined in this mode.
        expand => log_view_expanded_log_entry_function%get_current()
        if (.not. associated(expand)) return
        original_point = point()
        call log_view_current_entry(beginning, tag, found, move=.true.)
        if (.not. found) return
        call bind_inhibit_read_only(saved)
        call get_text_property(beginning, value_type(name="log_view_entry_expanded"), expanded, status)

        if (allocated(expanded%a)) then
            ! If the entry is expanded, collapse it.
            call next_single_property_change(beginning, value_type(name="log_view_comment"), position, found, status)
            has = found
            if (has) has = is_log_view_inside_comment(position)

            if (.not. has) then
                call set_error(status, GENERIC_ERROR_CODE, "Broken markup in `log_view_toggle_entry_display'")
            else
                call next_single_property_change(position, value_type(name="log_view_comment"), last, found, status)
                if (.not. found) last = point_maximum()
                call delete_region(position, last, status)
                if (status%ok()) call put_text_property(beginning, beginning + 1, value_type(name="log_view_entry_expanded"), &
                                                        value_type(), &
                                                        status)
                if (original_point < position) call goto_character(original_point)
            end if
        else
            ! Otherwise, expand the entry.
            call expand(tag, long_entry, has)

            if (has) then
                call put_text_property(beginning, beginning + 1, value_type(name="log_view_entry_expanded"), &
                                       value_type(.true.), status)
                call log_view_end_of_entry()
                beginning = point()
                if (status%ok()) call insert(long_entry//LF, status)
                properties(1) = value_type(name="font_lock_face")
                properties(2) = value_type(name="log_view_commit_body")
                properties(3) = value_type(name="log_view_comment")
                properties(4) = value_type(.true.)
                if (status%ok()) call add_properties(beginning, point(), properties, changed, status)
                call goto_character(original_point)
            end if
        end if

        call restore_inhibit_read_only(saved)
        deactivate_mark_variable = .false.
    end subroutine log_view_toggle_entry_display

    ! (add-text-properties BEGINNING END PROPERTIES).  Not a port.
    recursive subroutine add_properties(beginning, end, properties, changed, status)
        use value_mod, only: make_list, value_type
        use text_properties_mod, only: add_text_properties
        integer(int64), intent(in) :: beginning, end
        type(value_type), intent(in) :: properties(:)
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status

        call add_text_properties(beginning, end, make_list(properties), changed, status)
    end subroutine add_properties

    !> Expand current Log View entry, if possible, and also display its diff.
    !> If the current Log View entry is already expanded, only display its
    !> diff; this command never collapses entries.
    !> In contrast with `log_view_diff_changeset', the window displaying the
    !> diff is not selected.
    !> See also `log_view_toggle_entry_display'.  Port of
    !> log-view.el:log-view-display-entry-and-diff.
    recursive subroutine log_view_display_entry_and_diff(status)
        use frame_mod, only: selected_window
        use window_mod, only: select_window
        type(status_type), intent(out) :: status
        integer(int64) :: beginning
        character(len=:), allocatable :: tag
        type(value_type) :: expanded
        type(handle_type) :: sw
        logical :: found
        type(status_type) :: st

        status = status_type()
        call log_view_current_entry(beginning, tag, found)

        if (found) then
            call get_text_property(beginning, value_type(name="log_view_entry_expanded"), expanded, st)
            if (.not. allocated(expanded%a)) call log_view_toggle_entry_display(status)
            if (.not. status%ok()) return
        end if

        sw = selected_window()
        call log_view_diff(point(), point(), status)
        call select_window(sw, st)
    end subroutine log_view_display_entry_and_diff

    ! ------------------------------------------------------------------
    ! Subprogram motion
    ! ------------------------------------------------------------------

    !> Move backward to the beginning of a Log View entry.
    !> With ARGUMENT, do it that many times.  Negative ARGUMENT means move forward
    !> to the beginning of the ARGth following entry.
    !>
    !> This is Log View mode's default `beginning_of_subprogram_function'.
    !> It assumes that a log entry starts with a line matching
    !> `log_view_message_regular_expression'.  FOUND is the result.  Port of
    !> log-view.el:log-view-beginning-of-defun.
    recursive subroutine log_view_beginning_of_entry(found, argument)
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: argument
        integer(int64) :: n, beginning, shortage
        character(len=:), allocatable :: tag
        logical :: has

        n = 1
        if (present(argument)) n = argument
        found = .true.

        if (n < 0) then
            ! In log view, the end of one subprogram is the beginning of the
            ! next, so punting to log-view-end-of-defun is safe in this
            ! context.
            call log_view_end_of_entry(-n)
            return
        end if

        do while (n > 0)
            n = n - 1
            call log_view_current_entry(beginning, tag, has)

            if (.not. has) then
                call goto_character(point_minimum())
                found = .false.
            else if (beginning >= point()) then
                if (.not. is_beginning_of_buffer()) then
                    shortage = forward_line(-1_int64)
                    n = n + 1
                end if

                found = .false.
            else
                call goto_character(beginning)
                found = .true.
            end if
        end do
    end subroutine log_view_beginning_of_entry

    ! Move forward to the next Log View entry.  Port of
    ! log-view.el:log-view-end-of-defun-1.
    recursive subroutine end_of_subprogram_1()
        character(len=:), allocatable :: regular_expression
        integer(int64) :: found, shortage
        logical :: looping
        type(status_type) :: st

        regular_expression = log_view_message_regular_expression()
        looping = .true.
        if (looking_at(regular_expression, st)) call goto_character(match_end(0, st))

        do while (looping)
            call search_regular_expression_forward(regular_expression, found, st, noerror=SEARCH_MOVE)

            if (found /= 0) then
                if (.not. is_log_view_inside_comment(point())) then
                    looping = .false.
                    call goto_character(match_beginning(0, st))
                end if
            else if (looking_back("Show 4X entries    Show unlimited entries", st, line_beginning_position())) then
                ! Don't advance past the end buttons inserted by
                ! `vc-print-log-setup-buttons'.
                looping = .false.
                shortage = forward_line(-1_int64)
            else if (is_end_of_buffer()) then
                ! There are no buttons if we've turned on unlimited entries.
                looping = .false.
            end if
        end do
    end subroutine end_of_subprogram_1

    !> Move forward to the next Log View entry.
    !> Works like `end_of_subprogram'.  Port of log-view.el:log-view-end-of-defun.
    recursive subroutine log_view_end_of_entry(argument)
        integer(int64), intent(in), optional :: argument
        integer(int64) :: n, k
        logical :: found

        n = 1
        if (present(argument)) n = argument

        if (n < 0) then
            call log_view_beginning_of_entry(found, -n)
        else
            do k = 1, n
                call end_of_subprogram_1()
            end do
        end if
    end subroutine log_view_end_of_entry

    ! ------------------------------------------------------------------
    ! Revisions
    ! ------------------------------------------------------------------

    ! (car log-view-vc-fileset) for the commands that need one file: a
    ! user error if the buffer shows several.  Not a port.
    recursive subroutine single_file(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: fileset(:)
        logical :: has, several

        status = status_type()
        call log_view_version_control_fileset(fileset, has)
        file = ""
        if (size(fileset) > 0) file = fileset(1)%text

        if (.not. log_view_per_file_logs()) then
            several = size(fileset) > 1 .or. size(fileset) == 0
            if (.not. several) several = is_file_directory(file)
            if (several) call set_error(status, USER_ERROR_CODE, &
                                        "Multiple files shown in this buffer, cannot use this command here")
        end if
    end subroutine single_file

    !> Visit the version at POSITION.
    !> If called interactively, visit the version at point.  Port of
    !> log-view.el:log-view-find-revision.
    recursive subroutine log_view_find_revision(position, status)
        use window_mod, only: switch_to_buffer
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, tag
        type(handle_type) :: buffer
        integer(int64) :: original_point
        logical :: has

        call single_file(file, status)
        if (.not. status%ok()) return
        original_point = point()
        call goto_character(position)
        if (log_view_per_file_logs()) call log_view_current_file(file, status)

        if (status%ok()) then
            call log_view_current_tag(tag, has)
            call version_control_find_revision(file, tag, buffer, status, log_view_version_control_backend())
        end if

        call goto_character(original_point)
        if (status%ok()) call switch_to_buffer(buffers(buffer%index)%name, status)
    end subroutine log_view_find_revision

    !> Annotate the version at POSITION.
    !> If called interactively, annotate the version at point.  Port of
    !> log-view.el:log-view-annotate-version.
    recursive subroutine log_view_annotate_version(position, status)
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, tag
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: original_point
        logical :: has

        call single_file(file, status)
        if (.not. status%ok()) return
        old = current_buffer()
        original_point = point()
        call goto_character(position)
        if (log_view_per_file_logs()) call log_view_current_file(file, status)

        if (status%ok()) then
            call log_view_current_tag(tag, has)

            if (.not. associated(log_view_version_control_annotate)) then
                call set_error(status, GENERIC_ERROR_CODE, "VC-Annotate is not loaded")
            else
                call log_view_version_control_annotate(file, tag, log_view_version_control_backend(), status)
            end if
        end if

        ! (save-excursion ...)
        if (is_buffer_live(old)) call set_buffer(old, st)
        call goto_character(original_point)
    end subroutine log_view_annotate_version

    ! ------------------------------------------------------------------
    ! Diff
    ! ------------------------------------------------------------------

    !> Get the diff between two revisions.
    !> If the region is inactive or the mark is on the revision at
    !> point, get the diff between the revision at point and its
    !> previous revision.  Otherwise, get the diff between the revisions
    !> where the region starts and ends.
    !>
    !> Unlike `log_view_diff_changeset', this function only shows the
    !> part of the changeset which affected the currently considered
    !> file(s).  Port of log-view.el:log-view-diff.
    recursive subroutine log_view_diff(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        call log_view_diff_common(beginning, end, status)
    end subroutine log_view_diff

    !> Get the diff between two revisions.
    !> If the region is inactive or the mark is on the revision at
    !> point, get the diff between the revision at point and its
    !> previous revision.  Otherwise, get the diff between the revisions
    !> where the region starts and ends.
    !>
    !> Unlike `log_view_diff' this function shows the whole changeset,
    !> including changes affecting other files than the currently
    !> considered file(s).  Port of log-view.el:log-view-diff-changeset.
    recursive subroutine log_view_diff_changeset(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, saved
        logical :: found
        integer :: b

        status = status_type()
        b = log_view_version_control_backend()

        if (b > 0) then
            ! (eq (vc-call-backend ... 'revision-granularity) 'file): every
            ! ported backend has repository granularity.
            continue
        end if

        call version_control_root_directory(root, found, status)
        if (.not. status%ok()) return
        saved = buffers(current)%directory
        if (found) buffers(current)%directory = root
        call log_view_diff_common(beginning, end, status, .true.)
        buffers(current)%directory = saved
    end subroutine log_view_diff_changeset

    !> The diff between the revisions at BEGINNING and END (the one before when
    !> they are the same), of the current file(s), or with WHOLE_CHANGESET
    !> of all files.  Port of log-view.el:log-view-diff-common.
    recursive subroutine log_view_diff_common(beginning, end, status, whole_changeset)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: whole_changeset
        character(len=:), allocatable :: to, fr, previous_value, file
        type(version_control_fileset_type) :: fs
        integer(int64) :: fr_beginning, original_point, last
        logical :: has_to, has_fr, whole, changes, has, previous
        type(string_type), allocatable :: fileset(:)

        status = status_type()
        whole = .false.
        if (present(whole_changeset)) whole = whole_changeset
        call log_view_current_tag(to, has_to, beginning)
        call log_view_current_entry(fr_beginning, fr, has_fr, end)
        ! When TO and FR are the same, or when point is on a line after
        ! the last entry, look at the previous revision.
        previous = has_to .eqv. has_fr
        if (previous .and. has_to) previous = fr == to

        if (.not. previous) then
            original_point = point()
            call goto_character(end)
            call log_view_end_of_entry()
            last = point()
            call goto_character(original_point)
            previous = end >= last
        end if

        fs%backend = log_view_version_control_backend()

        if (previous) then
            has = .false.

            if (fs%backend > 0) then
                if (associated(version_control_backends(fs%backend)%previous_revision)) then
                    call version_control_backends(fs%backend)%previous_revision("", fr, previous_value, has)
                end if
            end if

            if (has) then
                fr = previous_value
            else
                fr = ""
            end if
        end if

        ! The value passed here should follow what
        ! `vc-deduce-fileset' returns.  If we want to see the
        ! diff for all the files in the changeset, pass FALSE for
        ! the file list.
        if (whole) then
            allocate (fs%files(0))
        else if (log_view_per_file_logs()) then
            call log_view_current_file(file, status)
            if (.not. status%ok()) return
            call append_string(fs%files, file)
        else
            call log_view_version_control_fileset(fileset, has)
            fs%files = fileset
        end if

        if (len(to) == 0) then
            if (len(fr) == 0) then
                call version_control_diff_internal(version_control_allow_asynchronous_diff, fs, changes, status)
            else
                call version_control_diff_internal(version_control_allow_asynchronous_diff, fs, changes, status, rev1=fr)
            end if
        else if (len(fr) == 0) then
            call version_control_diff_internal(version_control_allow_asynchronous_diff, fs, changes, status, rev2=to)
        else
            call version_control_diff_internal(version_control_allow_asynchronous_diff, fs, changes, status, fr, to)
        end if
    end subroutine log_view_diff_common

    !> Copy the ID of the revision at point to the clipboard history.
    !> If there are marked revisions, copy the IDs of those, separated by
    !> spaces.  Port of log-view.el:log-view-copy-revision-as-kill.
    recursive subroutine log_view_copy_revision(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: revisions(:)
        character(len=:), allocatable :: found, revision
        logical :: has
        integer :: k

        status = status_type()
        call log_view_get_marked(revisions)

        if (size(revisions) > 1) then
            found = revisions(1)%text

            do k = 2, size(revisions)
                found = found//" "//revisions(k)%text
            end do

            call copy_to_clipboard_history(found)
            call message(found)
        else
            if (size(revisions) == 1) then
                revision = revisions(1)%text
                has = .true.
            else
                call log_view_current_tag(revision, has)
            end if

            if (has) then
                call copy_to_clipboard_history(revision)
                call message(revision)
            else
                call set_error(status, USER_ERROR_CODE, "No revision at point")
            end if
        end if
    end subroutine log_view_copy_revision

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! (let ((inhibit-read-only t)) ...): bind it, keeping the old value
    ! in SAVED.  Not a port.
    subroutine bind_inhibit_read_only(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = value_type()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
    end subroutine bind_inhibit_read_only

    ! The end of (let ((inhibit-read-only t)) ...): restore SAVED.
    ! Not a port.
    subroutine restore_inhibit_read_only(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine restore_inhibit_read_only

    ! (interactive "p"): the numeric prefix argument.  Not a port.
    integer(int64) function count_argument()
        count_argument = prefix_numeric_value(current_prefix_argument)
    end function count_argument

    ! (if (use-region-p) (region-beginning) (point)) and the end.
    ! Not a port.
    subroutine region_arguments(beginning, end)
        integer(int64), intent(out) :: beginning, end
        type(status_type) :: st

        beginning = point()
        end = point()

        if (is_use_region()) then
            call region_beginning(beginning, st)
            call region_end(end, st)
        end if
    end subroutine region_arguments

    ! (interactive "p\nR"): the count and the active region, if any.
    ! Not a port.
    recursive subroutine mark_command(mark, status)
        logical, intent(in) :: mark
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end
        type(status_type) :: st

        if (is_use_region()) then
            call region_beginning(beginning, st)
            call region_end(end, st)
            call mark_unmark(mark, count_argument(), status, beginning, end)
        else
            call mark_unmark(mark, count_argument(), status)
        end if
    end subroutine mark_command

    recursive subroutine execute_log_view_message_next(status)
        type(status_type), intent(out) :: status

        call log_view_message_next(status, count_argument())
    end subroutine execute_log_view_message_next

    recursive subroutine execute_log_view_message_previous(status)
        type(status_type), intent(out) :: status

        call log_view_message_previous(status, count_argument())
    end subroutine execute_log_view_message_previous

    recursive subroutine execute_log_view_file_next(status)
        type(status_type), intent(out) :: status

        call log_view_file_next(status, count_argument())
    end subroutine execute_log_view_file_next

    recursive subroutine execute_log_view_file_previous(status)
        type(status_type), intent(out) :: status

        call log_view_file_previous(status, count_argument())
    end subroutine execute_log_view_file_previous

    recursive subroutine execute_log_view_message_and_diff_next(status)
        type(status_type), intent(out) :: status

        call log_view_message_and_diff_next(count_argument(), status)
    end subroutine execute_log_view_message_and_diff_next

    recursive subroutine execute_log_view_message_and_diff_previous(status)
        type(status_type), intent(out) :: status

        call log_view_message_and_diff_previous(count_argument(), status)
    end subroutine execute_log_view_message_and_diff_previous

    recursive subroutine execute_log_view_mark_entry(status)
        type(status_type), intent(out) :: status

        call mark_command(.true., status)
    end subroutine execute_log_view_mark_entry

    recursive subroutine execute_log_view_unmark_entry(status)
        type(status_type), intent(out) :: status

        call mark_command(.false., status)
    end subroutine execute_log_view_unmark_entry

    recursive subroutine execute_log_view_find_revision(status)
        type(status_type), intent(out) :: status

        call log_view_find_revision(point(), status)
    end subroutine execute_log_view_find_revision

    recursive subroutine execute_log_view_annotate_version(status)
        type(status_type), intent(out) :: status

        call log_view_annotate_version(point(), status)
    end subroutine execute_log_view_annotate_version

    recursive subroutine execute_log_view_diff(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call region_arguments(beginning, end)
        call log_view_diff(beginning, end, status)
    end subroutine execute_log_view_diff

    recursive subroutine execute_log_view_diff_changeset(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call region_arguments(beginning, end)
        call log_view_diff_changeset(beginning, end, status)
    end subroutine execute_log_view_diff_changeset

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    ! Define the faces.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => log_view_file_face
        call f%define("log_view_file", "Face for the file header line in `log_view_mode'.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="grey70", weight="bold", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(weight="bold", extend=.true.))
        f => log_view_message_face
        call f%define("log_view_message", "Face for the message header line in `log_view_mode'.")
        call f%specification_entry(display_when(classes=[FACE_CLASS_COLOR], backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                   face_attributes(background="grey85", extend=.true.))
        call f%specification_entry(display_when(), face_attributes(weight="bold", extend=.true.))
        f => log_view_commit_body
        call f%define("log_view_commit_body", "Face for the commit body in `log_view_mode'.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_comment_face"))
    end subroutine define_faces

    !> Define Log View mode, its keymap and faces, and register the
    !> commands.  Not a port.
    subroutine initialize_log_view()
        type(status_type) :: st

        if (associated(major_modes%log_view_mode%execute)) return
        call define_faces()
        call major_modes%log_view_mode%define("log_view_mode", "Log-View", log_view_mode, log_view_mode_hook, &
                                              parent=major_modes%special_mode)
        log_view_mode_map = major_modes%log_view_mode%map
        commands%log_view_mode%execute => log_view_mode
        commands%log_view_message_next%execute => execute_log_view_message_next
        commands%log_view_message_previous%execute => execute_log_view_message_previous
        commands%log_view_file_next%execute => execute_log_view_file_next
        commands%log_view_file_previous%execute => execute_log_view_file_previous
        commands%log_view_message_and_diff_next%execute => execute_log_view_message_and_diff_next
        commands%log_view_message_and_diff_previous%execute => execute_log_view_message_and_diff_previous
        commands%log_view_toggle_mark_entry%execute => log_view_toggle_mark_entry
        commands%log_view_mark_entry%execute => execute_log_view_mark_entry
        commands%log_view_unmark_entry%execute => execute_log_view_unmark_entry
        commands%log_view_unmark_all_entries%execute => log_view_unmark_all_entries
        commands%log_view_toggle_entry_display%execute => log_view_toggle_entry_display
        commands%log_view_display_entry_and_diff%execute => log_view_display_entry_and_diff
        commands%log_view_find_revision%execute => execute_log_view_find_revision
        commands%log_view_annotate_version%execute => execute_log_view_annotate_version
        commands%log_view_diff%execute => execute_log_view_diff
        commands%log_view_diff_changeset%execute => execute_log_view_diff_changeset
        commands%log_view_copy_revision%execute => log_view_copy_revision
        call keymap_set(log_view_mode_map, "RET", "log_view_toggle_entry_display", st)
        call keymap_set(log_view_mode_map, "M-RET", "log_view_display_entry_and_diff", st)
        call keymap_set(log_view_mode_map, "m", "log_view_mark_entry", st)
        call keymap_set(log_view_mode_map, "u", "log_view_unmark_entry", st)
        call keymap_set(log_view_mode_map, "U", "log_view_unmark_all_entries", st)
        call keymap_set(log_view_mode_map, "d", "log_view_diff", st)
        call keymap_set(log_view_mode_map, "=", "log_view_diff", st)
        call keymap_set(log_view_mode_map, "D", "log_view_diff_changeset", st)
        call keymap_set(log_view_mode_map, "f", "log_view_find_revision", st)
        call keymap_set(log_view_mode_map, "a", "log_view_annotate_version", st)
        call keymap_set(log_view_mode_map, "n", "log_view_message_next", st)
        call keymap_set(log_view_mode_map, "p", "log_view_message_previous", st)
        call keymap_set(log_view_mode_map, "M-n", "log_view_message_and_diff_next", st)
        call keymap_set(log_view_mode_map, "M-p", "log_view_message_and_diff_previous", st)
        call keymap_set(log_view_mode_map, "w", "log_view_copy_revision", st)
        call keymap_set(log_view_mode_map, "TAB", "log_view_message_next", st)
        call keymap_set(log_view_mode_map, "<backtab>", "log_view_message_previous", st)
        call append_locals_clearer(clear_log_view_locals)
    end subroutine initialize_log_view
end module log_view_mod
