! log_view_fm.f90 -- major mode for browsing revision log histories.
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

!> Log View mode, the major mode of VC's log buffers: moving between
!> log messages (n, p, TAB, <backtab>) and files, marking entries
!> (m, u, U), expanding short entries (RET), diffs of an entry or a
!> region (d, =, D, M-RET, M-n, M-p), visiting a revision (f) and
!> copying revision IDs (w).
!>
!> The buffer-local variables `log_view_file_re', `log_view_message_re',
!> `log_view_per_file_logs', `log_view_expanded_log_entry_function',
!> `log_view_vc_fileset' and `log_view_vc_backend' are set with
!> `set_log_view_file_re' and its siblings, as `set_local' does.
!>
!> Deferred: `log_view_modify_change_comment' (e, needing the backend's
!> `modify-change-comment'), `log_view_annotate_version' (a, needing
!> `vc-annotate'), `log_view_cherry_pick' (C), `log_view_revert_or_delete_revisions' (R),
!> `log_view_uncommit_revisions_from_end' (x),
!> `log_view_delete_revisions_from_end' (X), `log_view_revert_revisions'
!> and `log_view_delete_revisions' (needing `vc--pick-or-revert' and
!> working trees), PCL-CVS linkage (`log_view_minor_wrap',
!> `cvs-pcl-cvs-dirchange-re'), `log_view_extract_comment',
!> `beginning_of_subprogram_function' and `end_of_subprogram_function' (lisp.el's
!> subprogram motion is not ported), restoring marks after `revert_buffer'
!> (`log_view__restore_marks'), the menu, `project-find-matching-buffer-function'
!> and directory-local variables.
module log_view_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, t_value, make_string, xstring, vtag, V_NULL
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set, add_local_variables_killer, value_eq
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, buffer_serial_live, is_buffer_live, deactivate_mark_variable
    use commands_fm, only: commands
    use modes_fm, only: major_modes
    use editfns_fm, only: point, goto_char, point_min, point_max, is_bob, is_eob, is_bol, line_beginning_position, &
                          delete_region, region_beginning, region_end
    use insdel_fm, only: insert
    use cmds_fm, only: forward_line
    use search_fm, only: looking_at, search_re_forward => re_search_forward, search_re_backward => re_search_backward, &
                         match_beginning, match_end, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string_no_properties, looking_back
    use textprop_fm, only: get_text_property, put_text_property, next_single_property_change, get_char_property, &
                           get_char_property_and_overlay
    use itree_fm, only: make_overlay, overlay_put, delete_overlay, overlay_get, overlay_end
    use easy_mmode_fm, only: easy_mmode_next, easy_mmode_prev
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use simple_fm, only: is_use_region, kill_new, line_number_at_pos, special_mode
    use syntax_fm, only: skip_syntax_backward
    use xdisp_fm, only: message
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, FONT_LOCK_OVERRIDE_APPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight
    use keymap_fm, only: keymap_set
    use fileio_fm, only: is_file_directory
    use vc_hooks_fm, only: vc_string_type, vc_backends
    use vc_fm, only: vc_fileset_type, vc_diff_internal, vc_find_revision, vc_root_dir, vc_allow_async_diff
    implicit none(type, external)
    private

    public :: log_view_entry_proc, log_view_mode, log_view_msg_next, log_view_msg_prev, log_view_file_next, &
              log_view_file_prev, log_view_goto_rev, log_view_msg_and_diff_next, log_view_msg_and_diff_prev, &
              log_view_current_file, log_view_current_entry, is_log_view_inside_comment, log_view_current_tag, &
              log_view_toggle_mark_entry, log_view_mark_entry, log_view_unmark_entry, log_view_unmark_all_entries, &
              log_view_get_marked, log_view_toggle_entry_display, log_view_display_entry_and_diff, &
              log_view_beginning_of_entry, log_view_end_of_entry, log_view_find_revision, log_view_diff, &
              log_view_diff_changeset, log_view_diff_common, log_view_copy_revision_as_kill, &
              log_view_file_re, log_view_message_re, log_view_per_file_logs, log_view_vc_fileset, log_view_vc_backend, &
              set_log_view_file_re, set_log_view_message_re, set_log_view_per_file_logs, &
              set_log_view_expanded_log_entry_function, set_log_view_vc_fileset, set_log_view_vc_backend, &
              set_log_view_font_lock_keywords, log_view_font_lock_keywords, syms_of_log_view

    abstract interface
        !> `log_view_expanded_log_entry_function': return in TEXT the detailed
        !> description of the Log View entry REVISION (FOUND .false. for none).
        !> Not a port.
        recursive subroutine log_view_entry_proc(revision, text, found)
            implicit none(type, external)
            character(len=*), intent(in) :: revision
            character(len=:), allocatable, intent(out) :: text
            logical, intent(out) :: found
        end subroutine log_view_entry_proc
    end interface

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
    !> `log_view_file_re').
    !> The match group number 1 should match the file name itself.
    character(len=*), parameter, public :: LOG_VIEW_FILE_RE_DEFAULT = &
                                           "^\(?:Working file: \(?1:.+\)" &       ! RCS and CVS.
                                           //"\|\(?:SCCS/s\.\|Changes to \)\(?1:.+\):" & ! SCCS and Darcs.
                                           //"\)"//achar(10)  ! Include the \n for font-lock reasons.

    !> Regexp matching the text identifying a revision (the default value
    !> of `log_view_message_re').
    !> The match group number 1 should match the revision number itself.
    character(len=*), parameter, public :: LOG_VIEW_MESSAGE_RE_DEFAULT = &
                                           "^\(?:revision \(?1:[.0-9]+\)\(?:"//achar(9)//".*\)?" & ! RCS and CVS.
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
    character(len=*), parameter :: LOG_VIEW_DIR_RE = "^cvs[.ex]* [a-z]+: Logging \(.+\)$"

    ! The buffer-local values of a Log View buffer, by serial number:
    ! log-view-file-re, log-view-message-re, log-view-per-file-logs,
    ! log-view-expanded-log-entry-function, log-view-font-lock-keywords,
    ! log-view-vc-fileset and log-view-vc-backend.
    type :: local_type
        integer(int64) :: serial = 0
        character(len=:), allocatable :: file_re, message_re
        logical :: has_per_file_logs = .false.
        logical :: per_file_logs = .true.
        procedure(log_view_entry_proc), pointer, nopass :: expanded => null()
        logical :: has_keywords = .false.
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: has_fileset = .false.
        type(vc_string_type), allocatable :: fileset(:)
        integer :: backend = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals', made if MAKE
    ! (0 if none).  Not a port.
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
    ! Not a port.
    subroutine kill_log_view_locals()
        integer :: k

        k = local_index(.false.)
        if (k > 0) locals(k) = local_type()
    end subroutine kill_log_view_locals

    !> The current buffer's `log_view_file_re': the regexp matching the
    !> text identifying the file.  The match group number 1 should match
    !> the file name itself.  Not a port.
    function log_view_file_re() result(re)
        character(len=:), allocatable :: re
        integer :: k

        re = LOG_VIEW_FILE_RE_DEFAULT
        k = local_index(.false.)

        if (k > 0) then
            if (allocated(locals(k)%file_re)) re = locals(k)%file_re
        end if
    end function log_view_file_re

    !> The current buffer's `log_view_message_re': the regexp matching the
    !> text identifying a revision.  The match group number 1 should match
    !> the revision number itself.  Not a port.
    function log_view_message_re() result(re)
        character(len=:), allocatable :: re
        integer :: k

        re = LOG_VIEW_MESSAGE_RE_DEFAULT
        k = local_index(.false.)

        if (k > 0) then
            if (allocated(locals(k)%message_re)) re = locals(k)%message_re
        end if
    end function log_view_message_re

    !> The current buffer's `log_view_per_file_logs': true if the logs are
    !> shown one file at a time.  Not a port.
    logical function log_view_per_file_logs()
        integer :: k

        log_view_per_file_logs = .true.
        k = local_index(.false.)

        if (k > 0) then
            if (locals(k)%has_per_file_logs) log_view_per_file_logs = locals(k)%per_file_logs
        end if
    end function log_view_per_file_logs

    !> The current buffer's `log_view_vc_fileset': the VC fileset
    !> corresponding to the current log (HAS .false. for none).  Not a port.
    subroutine log_view_vc_fileset(fileset, has)
        type(vc_string_type), allocatable, intent(out) :: fileset(:)
        logical, intent(out) :: has
        integer :: k

        has = .false.
        k = local_index(.false.)
        if (k > 0) has = locals(k)%has_fileset

        if (has) then
            fileset = locals(k)%fileset
        else
            allocate (fileset(0))
        end if
    end subroutine log_view_vc_fileset

    !> The current buffer's `log_view_vc_backend': the VC backend that
    !> created the current log (0 for none).  Not a port.
    integer function log_view_vc_backend() result(b)
        integer :: k

        b = 0
        k = local_index(.false.)
        if (k > 0) b = locals(k)%backend
    end function log_view_vc_backend

    !> The current buffer's `log_view_font_lock_keywords' (the default
    !> highlights the file and message lines when not set).  Not a port.
    subroutine log_view_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        type(font_lock_highlight_type) :: h(1)
        integer :: k

        k = local_index(.false.)

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
        keywords(1) = font_lock_keyword(log_view_file_re(), highlights=h)
        keywords(2) = font_lock_keyword(log_view_message_re(), face="log_view_message")
    end subroutine log_view_font_lock_keywords

    !> Give the current buffer its own `log_view_file_re', RE (set_local).  Not a port.
    subroutine set_log_view_file_re(re)
        character(len=*), intent(in) :: re
        integer :: k

        k = local_index(.true.)
        locals(k)%file_re = re
    end subroutine set_log_view_file_re

    !> Give the current buffer its own `log_view_message_re', RE (set_local).  Not a port.
    subroutine set_log_view_message_re(re)
        character(len=*), intent(in) :: re
        integer :: k

        k = local_index(.true.)
        locals(k)%message_re = re
    end subroutine set_log_view_message_re

    !> Give the current buffer its own `log_view_per_file_logs', FLAG (set_local).  Not a port.
    subroutine set_log_view_per_file_logs(flag)
        logical, intent(in) :: flag
        integer :: k

        k = local_index(.true.)
        locals(k)%has_per_file_logs = .true.
        locals(k)%per_file_logs = flag
    end subroutine set_log_view_per_file_logs

    !> Give the current buffer its own `log_view_expanded_log_entry_function', FN (set_local).  Not a port.
    subroutine set_log_view_expanded_log_entry_function(fn)
        procedure(log_view_entry_proc) :: fn
        integer :: k

        k = local_index(.true.)
        locals(k)%expanded => fn
    end subroutine set_log_view_expanded_log_entry_function

    !> Give the current buffer its own `log_view_vc_fileset', FILESET (set_local).  Not a port.
    subroutine set_log_view_vc_fileset(fileset)
        type(vc_string_type), intent(in) :: fileset(:)
        integer :: k

        k = local_index(.true.)
        locals(k)%has_fileset = .true.
        locals(k)%fileset = fileset
    end subroutine set_log_view_vc_fileset

    !> Give the current buffer its own `log_view_vc_backend', B (set_local).  Not a port.
    subroutine set_log_view_vc_backend(b)
        integer, intent(in) :: b
        integer :: k

        k = local_index(.true.)
        locals(k)%backend = b
    end subroutine set_log_view_vc_backend

    !> Give the current buffer its own `log_view_font_lock_keywords', KEYWORDS (set_local).  Not a port.
    subroutine set_log_view_font_lock_keywords(keywords)
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        integer :: k

        k = local_index(.true.)
        locals(k)%has_keywords = .true.
        locals(k)%keywords = keywords
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
    end subroutine set_log_view_font_lock_keywords

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for browsing CVS log output.  Port of
    !> log-view.el:log-view-mode.
    recursive subroutine log_view_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved

        if (.not. associated(major_modes%log_view_mode%run)) call syms_of_log_view()
        call major_modes%log_view_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(cur)%read_only = .true.
        call log_view_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call major_modes%log_view_mode%finish(saved, status)
    end subroutine log_view_mode

    ! ------------------------------------------------------------------
    ! Navigation
    ! ------------------------------------------------------------------

    !> Go to the next COUNT'th log message.
    !>
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-next (easy-mmode-define-navigation).
    recursive subroutine log_view_msg_next(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(log_view_message_re(), "log message", n, status)
    end subroutine log_view_msg_next

    !> Go to the previous COUNT'th log message.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-prev.
    recursive subroutine log_view_msg_prev(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_prev(log_view_message_re(), "log message", n, status)
    end subroutine log_view_msg_prev

    !> Go to the next COUNT'th file.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-file-next.
    recursive subroutine log_view_file_next(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(log_view_file_re(), "file", n, status)
    end subroutine log_view_file_next

    !> Go to the previous COUNT'th file.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-file-prev.
    recursive subroutine log_view_file_prev(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_prev(log_view_file_re(), "file", n, status)
    end subroutine log_view_file_prev

    !> Go to revision REV (FOUND false if there is none).  Port of
    !> log-view.el:log-view-goto-rev.
    recursive subroutine log_view_goto_rev(rev, found)
        character(len=*), intent(in) :: rev
        logical, intent(out) :: found
        character(len=:), allocatable :: tag
        logical :: has
        type(status_type) :: st

        call goto_char(point_min())
        found = .false.

        do
            call log_view_current_tag(tag, has)

            if (has) then
                if (tag == rev) exit
            end if

            ! (ignore-errors ...)
            call log_view_msg_next(st)
            if (.not. st%ok()) return
        end do

        found = .true.
    end subroutine log_view_goto_rev

    !> Go to next COUNT'th entry, expand it if possible, and show its diff.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-and-diff-next.
    recursive subroutine log_view_msg_and_diff_next(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status

        call log_view_msg_next(status, count)
        if (status%ok()) call log_view_display_entry_and_diff(status)
    end subroutine log_view_msg_and_diff_next

    !> Go to previous COUNT'th entry, expand it if possible, and show its diff.
    !> Interactively, COUNT is the numeric prefix argument, and defaults to 1.
    !> Port of log-view.el:log-view-msg-and-diff-prev.
    recursive subroutine log_view_msg_and_diff_prev(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status

        call log_view_msg_next(status, -count)
        if (status%ok()) call log_view_display_entry_and_diff(status)
    end subroutine log_view_msg_and_diff_prev

    ! ------------------------------------------------------------------
    ! Linkage to PCL-CVS (mostly copied from cvs-status.el)
    ! ------------------------------------------------------------------

    !> Return the current file.  Port of log-view.el:log-view-current-file
    !> (without PCL-CVS's directory changes).
    recursive subroutine log_view_current_file(file, status)
        use fileio_fm, only: expand_file_name
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, cvsdir, dir, re
        integer(int64) :: opoint, shortage, found
        type(status_type) :: st

        status = status_type()
        opoint = point()
        shortage = forward_line(1_int64)
        re = log_view_file_re()
        call search_re_backward(re, found, st, noerror=SEARCH_NOERROR)
        if (found == 0) call search_re_forward(re, found, st, noerror=SEARCH_NOERROR)

        if (found == 0) then
            call goto_char(opoint)
            call set_error(status, FM_GENERIC_ERROR, "Unable to determine the current file")
            return
        end if

        name = match_string_no_properties(1)
        call search_re_backward(LOG_VIEW_DIR_RE, found, st, noerror=SEARCH_NOERROR)
        dir = ""

        if (found /= 0) then
            cvsdir = match_string_no_properties(1)
            call expand_file_name(cvsdir, dir, st, "")
        end if

        call expand_file_name(name, file, st, dir)
        call goto_char(opoint)
    end subroutine log_view_current_file

    !> Return the position and revision tag of the Log View entry at POS.
    !> This is (BEG, TAG), where BEG is a buffer position and TAG is a
    !> string.  If POS is absent, it defaults to point.
    !> If there is no entry at POS, FOUND is false.
    !>
    !> If optional arg MOVE is true, move point to BEG if found.
    !> Otherwise, don't move point.  Port of
    !> log-view.el:log-view-current-entry.
    recursive subroutine log_view_current_entry(beg, tag, found, pos, move)
        integer(int64), intent(out) :: beg
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: pos
        logical, intent(in), optional :: move
        integer(int64) :: opoint, shortage, p
        logical :: looping, separator
        character(len=:), allocatable :: re
        type(status_type) :: st

        found = .false.
        beg = 0
        tag = ""
        opoint = point()
        if (present(pos)) call goto_char(pos)
        shortage = forward_line(0_int64)
        ! Treat "---" separator lines as part of the following revision.
        separator = looking_at("-\{20,\}$", st)

        if (separator) then
            shortage = forward_line(2_int64)
        else
            shortage = forward_line(1_int64)
        end if

        re = log_view_message_re()
        looping = .true.
        p = 0

        do while (looping)
            call search_re_backward(re, p, st, noerror=SEARCH_MOVE)
            looping = .false.
            if (p /= 0) looping = is_log_view_inside_comment(point())
        end do

        if (p /= 0) then
            found = .true.
            beg = p
            tag = match_string_no_properties(1)
        end if

        call goto_char(opoint)

        if (present(move)) then
            if (move .and. found) call goto_char(beg)
        end if
    end subroutine log_view_current_entry

    !> Return true if POS lies inside an expanded log entry.  Port of
    !> log-view.el:log-view-inside-comment-p.
    logical function is_log_view_inside_comment(pos)
        integer(int64), intent(in) :: pos
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(pos, sym("log_view_comment"), v, st)
        is_log_view_inside_comment = .false.
        if (st%ok()) is_log_view_inside_comment = value_eq(v, t_value())
    end function is_log_view_inside_comment

    !> Return the revision tag (a string) of the Log View entry at POS.
    !> if POS is omitted, it defaults to point (HAS .false. for none).
    !> Port of log-view.el:log-view-current-tag.
    recursive subroutine log_view_current_tag(tag, has, pos)
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: has
        integer(int64), intent(in), optional :: pos
        integer(int64) :: beg

        call log_view_current_entry(beg, tag, has, pos)
    end subroutine log_view_current_tag

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    !> Toggle the marked state for the log entry at point.
    !> See `log_view_mark_entry'.  Port of
    !> log-view.el:log-view-toggle-mark-entry.
    recursive subroutine log_view_toggle_mark_entry(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, beg
        character(len=:), allocatable :: tag
        logical :: found

        status = status_type()
        opoint = point()
        call log_view_current_entry(beg, tag, found)

        if (found) then
            if (vtag(get_char_property(beg, sym("log_view_self"))) /= V_NULL) then
                call log_view_unmark_entry(1_int64, status)
            else
                call log_view_mark_entry(1_int64, status)
            end if
        end if

        call goto_char(opoint)
    end subroutine log_view_toggle_mark_entry

    ! Call `log_view__mark_entry' (MARK) or `log_view__unmark_entry' on
    ! each line of an active region or ARG times.  The last line of an
    ! active region is excluded in the case that the region ends right at
    ! the beginning of the line, or after only non-word characters.
    ! Port of log-view.el:log-view--mark-unmark.
    recursive subroutine mark_unmark(mark, arg, status, beg, end, restore)
        logical, intent(in) :: mark
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        !> The table of `log_view__restore_marks' (marks the entries whose
        !> tags are in it, moves past the others).
        character(len=*), intent(in), optional :: restore(:)
        integer(int64) :: lastl, n, processed_line, opoint, moved
        integer(int64) :: k

        status = status_type()

        if (present(beg) .neqv. present(end)) then
            call set_error(status, FM_GENERIC_ERROR, "log-view--mark-unmark called with invalid arguments")
            return
        end if

        if (present(beg)) then
            processed_line = -1
            ! Exclude the region's last line if the region ends right
            ! at the beginning of that line or almost at the beginning.
            ! This is like the `file' value of `dired-mark-region'.
            ! We don't want to include the last line unless the region
            ! visually includes that revision.
            opoint = point()
            call goto_char(end)
            moved = skip_syntax_backward("^w")

            if (is_bol()) then
                lastl = line_number_at_pos() - 1
            else
                lastl = line_number_at_pos()
            end if

            call goto_char(beg)

            do
                n = line_number_at_pos()
                ! Make sure we don't get stuck processing the
                ! same line infinitely.
                if (.not. (n <= lastl)) exit
                if (n == processed_line) exit
                processed_line = n
                call toggle_entry()
                if (.not. status%ok()) exit
            end do

            call goto_char(opoint)
            deactivate_mark_variable = .true.
        else
            do k = 1, arg
                call toggle_entry()
                if (.not. status%ok()) return
            end do
        end if

    contains

        recursive subroutine toggle_entry()
            character(len=:), allocatable :: tag
            logical :: has
            integer :: i

            if (present(restore)) then
                call log_view_current_tag(tag, has)

                if (has) then
                    do i = 1, size(restore)
                        if (restore(i) == tag) then
                            call mark_entry(status)
                            return
                        end if
                    end do
                end if

                call log_view_msg_next(status)
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
    !> When called from a program, mark ARG entries or all entries lying
    !> between BEG and END.  If BEG and END are supplied, ARG is ignored.
    !>
    !> When entries are marked, some commands that usually operate on the entry
    !> at point will instead operate on all marked entries.
    !> Use \[log_view_unmark_entry] to unmark an entry.
    !>
    !> Programs can use `log_view_get_marked' to obtain a list of all
    !> marked revisions.  Port of log-view.el:log-view-mark-entry.
    recursive subroutine log_view_mark_entry(arg, status, beg, end)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end

        call mark_unmark(.true., arg, status, beg, end)
    end subroutine log_view_mark_entry

    ! Mark the log entry at point.  Port of log-view.el:log-view--mark-entry.
    recursive subroutine mark_entry(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end, opoint
        character(len=:), allocatable :: tag
        type(value_type) :: expanded
        type(handle_type) :: ov
        logical :: found
        type(status_type) :: st

        status = status_type()
        call log_view_current_entry(beg, tag, found)

        if (found) then
            opoint = point()
            call goto_char(beg)

            if (vtag(get_char_property(beg, sym("log_view_self"))) == V_NULL) then
                call get_text_property(beg, sym("log_view_entry_expanded"), expanded, st)

                if (allocated(expanded%a)) then
                    call next_single_property_change(beg, sym("log_view_comment"), end, found, st)
                    if (.not. found) end = point_max()
                else
                    call log_view_end_of_entry()
                    end = point()
                end if

                call make_overlay(ov, beg, end, st)
                call overlay_put(ov, "face", sym("log_view_file"), st)
                ! This is used to check if the overlay is present.
                call overlay_put(ov, "log_view_self", t_value(), st)
                call overlay_put(ov, "log_view_marked", make_string(tag), st)
            end if

            call goto_char(opoint)
        end if

        call log_view_msg_next(status, 1_int64)
    end subroutine mark_entry

    !> Unmark the log entry at point.
    !> If the region is active in Transient Mark mode, unmark all entries.
    !> When called with a prefix argument, unmark that many log entries.
    !> When called from a program, mark ARG entries or all entries lying
    !> between BEG and END.  If BEG and END are supplied, ARG is ignored.
    !>
    !> See `log_view_mark_entry'.  Port of log-view.el:log-view-unmark-entry.
    recursive subroutine log_view_unmark_entry(arg, status, beg, end)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end

        call mark_unmark(.false., arg, status, beg, end)
    end subroutine log_view_unmark_entry

    ! Unmark the log entry at point.  Port of
    ! log-view.el:log-view--unmark-entry.
    recursive subroutine unmark_entry(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        character(len=:), allocatable :: tag
        type(value_type) :: v
        type(handle_type) :: ov
        logical :: found

        call log_view_current_entry(beg, tag, found)

        if (found) then
            call get_char_property_and_overlay(beg, sym("log_view_self"), v, ov)
            if (allocated(v%a) .and. ov%index /= 0) call delete_overlay(ov)
        end if

        call log_view_msg_next(status, 1_int64)
    end subroutine unmark_entry

    !> Unmark all marked log entries in this buffer.  Port of
    !> log-view.el:log-view-unmark-all-entries.
    recursive subroutine log_view_unmark_all_entries(status)
        type(status_type), intent(out) :: status

        call mark_unmark(.false., 0_int64, status, point_min(), point_max())
    end subroutine log_view_unmark_all_entries

    !> Return the list of tags for the marked log entries.  Port of
    !> log-view.el:log-view-get-marked.
    subroutine log_view_get_marked(marked)
        type(vc_string_type), allocatable, intent(out) :: marked(:)
        type(vc_string_type), allocatable :: grown(:)
        type(value_type) :: v, tag
        type(handle_type) :: ov
        integer(int64) :: pos, next
        logical :: found
        type(status_type) :: st
        integer :: n

        allocate (marked(0))
        pos = point_min()

        do
            call next_single_property_change(pos, sym("face"), next, found, st)
            if (.not. found) exit
            pos = next
            call get_char_property_and_overlay(pos, sym("log_view_self"), v, ov)

            if (allocated(v%a) .and. ov%index /= 0) then
                tag = overlay_get(ov, "log_view_marked")
                ! (push ...): the last found comes first.
                n = size(marked)
                allocate (grown(n + 1))
                grown(1)%s = xstring(tag)
                if (n > 0) grown(2:) = marked
                call move_alloc(grown, marked)
                pos = overlay_end(ov)
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
        integer(int64) :: opoint, beg, pos, last
        character(len=:), allocatable :: tag, long_entry
        type(value_type) :: expanded, saved, props(4)
        logical :: found, has, changed
        integer :: k

        status = status_type()
        ! Don't do anything unless `log_view_expanded_log_entry_function'
        ! is defined in this mode.
        k = local_index(.false.)
        if (k == 0) return
        if (.not. associated(locals(k)%expanded)) return
        opoint = point()
        call log_view_current_entry(beg, tag, found, move=.true.)
        if (.not. found) return
        call bind_inhibit_read_only(saved)
        call get_text_property(beg, sym("log_view_entry_expanded"), expanded, status)

        if (allocated(expanded%a)) then
            ! If the entry is expanded, collapse it.
            call next_single_property_change(beg, sym("log_view_comment"), pos, found, status)
            has = found
            if (has) has = is_log_view_inside_comment(pos)

            if (.not. has) then
                call set_error(status, FM_GENERIC_ERROR, "Broken markup in `log_view_toggle_entry_display'")
            else
                call next_single_property_change(pos, sym("log_view_comment"), last, found, status)
                if (.not. found) last = point_max()
                call delete_region(pos, last, status)
                if (status%ok()) call put_text_property(beg, beg + 1, sym("log_view_entry_expanded"), value_type(), status)
                if (opoint < pos) call goto_char(opoint)
            end if
        else
            ! Otherwise, expand the entry.
            call locals(k)%expanded(tag, long_entry, has)

            if (has) then
                call put_text_property(beg, beg + 1, sym("log_view_entry_expanded"), t_value(), status)
                call log_view_end_of_entry()
                beg = point()
                if (status%ok()) call insert(long_entry//LF, status)
                props(1) = sym("font_lock_face")
                props(2) = sym("log_view_commit_body")
                props(3) = sym("log_view_comment")
                props(4) = t_value()
                if (status%ok()) call add_properties(beg, point(), props, changed, status)
                call goto_char(opoint)
            end if
        end if

        call restore_inhibit_read_only(saved)
        deactivate_mark_variable = .false.
    end subroutine log_view_toggle_entry_display

    ! (add-text-properties BEG END PROPS).  Not a port.
    recursive subroutine add_properties(beg, end, props, changed, status)
        use lisp_fm, only: make_list
        use textprop_fm, only: add_text_properties
        integer(int64), intent(in) :: beg, end
        type(value_type), intent(in) :: props(:)
        logical, intent(out) :: changed
        type(status_type), intent(out) :: status

        call add_text_properties(beg, end, make_list(props), changed, status)
    end subroutine add_properties

    !> Expand current Log View entry, if possible, and also display its diff.
    !> If the current Log View entry is already expanded, only display its
    !> diff; this command never collapses entries.
    !> In contrast with `log_view_diff_changeset', the window displaying the
    !> diff is not selected.
    !> See also `log_view_toggle_entry_display'.  Port of
    !> log-view.el:log-view-display-entry-and-diff.
    recursive subroutine log_view_display_entry_and_diff(status)
        use frame_fm, only: selected_window
        use window_fm, only: select_window
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        character(len=:), allocatable :: tag
        type(value_type) :: expanded
        type(handle_type) :: sw
        logical :: found
        type(status_type) :: st

        status = status_type()
        call log_view_current_entry(beg, tag, found)

        if (found) then
            call get_text_property(beg, sym("log_view_entry_expanded"), expanded, st)
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
    !> With ARG, do it that many times.  Negative ARG means move forward
    !> to the beginning of the ARGth following entry.
    !>
    !> This is Log View mode's default `beginning_of_subprogram_function'.
    !> It assumes that a log entry starts with a line matching
    !> `log_view_message_re'.  FOUND is the result.  Port of
    !> log-view.el:log-view-beginning-of-defun.
    recursive subroutine log_view_beginning_of_entry(found, arg)
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n, beg, shortage
        character(len=:), allocatable :: tag
        logical :: has

        n = 1
        if (present(arg)) n = arg
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
            call log_view_current_entry(beg, tag, has)

            if (.not. has) then
                call goto_char(point_min())
                found = .false.
            else if (beg >= point()) then
                if (.not. is_bob()) then
                    shortage = forward_line(-1_int64)
                    n = n + 1
                end if

                found = .false.
            else
                call goto_char(beg)
                found = .true.
            end if
        end do
    end subroutine log_view_beginning_of_entry

    ! Move forward to the next Log View entry.  Port of
    ! log-view.el:log-view-end-of-defun-1.
    recursive subroutine end_of_subprogram_1()
        character(len=:), allocatable :: re
        integer(int64) :: found, shortage
        logical :: looping
        type(status_type) :: st

        re = log_view_message_re()
        looping = .true.
        if (looking_at(re, st)) call goto_char(match_end(0, st))

        do while (looping)
            call search_re_forward(re, found, st, noerror=SEARCH_MOVE)

            if (found /= 0) then
                if (.not. is_log_view_inside_comment(point())) then
                    looping = .false.
                    call goto_char(match_beginning(0, st))
                end if
            else if (looking_back("Show 4X entries    Show unlimited entries", st, line_beginning_position())) then
                ! Don't advance past the end buttons inserted by
                ! `vc-print-log-setup-buttons'.
                looping = .false.
                shortage = forward_line(-1_int64)
            else if (is_eob()) then
                ! There are no buttons if we've turned on unlimited entries.
                looping = .false.
            end if
        end do
    end subroutine end_of_subprogram_1

    !> Move forward to the next Log View entry.
    !> Works like `end_of_subprogram'.  Port of log-view.el:log-view-end-of-defun.
    recursive subroutine log_view_end_of_entry(arg)
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n, k
        logical :: found

        n = 1
        if (present(arg)) n = arg

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
        type(vc_string_type), allocatable :: fileset(:)
        logical :: has, several

        status = status_type()
        call log_view_vc_fileset(fileset, has)
        file = ""
        if (size(fileset) > 0) file = fileset(1)%s

        if (.not. log_view_per_file_logs()) then
            several = size(fileset) > 1 .or. size(fileset) == 0
            if (.not. several) several = is_file_directory(file)
            if (several) call set_error(status, FM_USER_ERROR, &
                                        "Multiple files shown in this buffer, cannot use this command here")
        end if
    end subroutine single_file

    !> Visit the version at POS.
    !> If called interactively, visit the version at point.  Port of
    !> log-view.el:log-view-find-revision.
    recursive subroutine log_view_find_revision(pos, status)
        use window_fm, only: switch_to_buffer
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, tag
        type(handle_type) :: buf
        integer(int64) :: opoint
        logical :: has

        call single_file(file, status)
        if (.not. status%ok()) return
        opoint = point()
        call goto_char(pos)
        if (log_view_per_file_logs()) call log_view_current_file(file, status)

        if (status%ok()) then
            call log_view_current_tag(tag, has)
            call vc_find_revision(file, tag, buf, status, log_view_vc_backend())
        end if

        call goto_char(opoint)
        if (status%ok()) call switch_to_buffer(buffers(buf%index)%name, status)
    end subroutine log_view_find_revision

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
    recursive subroutine log_view_diff(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        call log_view_diff_common(beg, end, status)
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
    recursive subroutine log_view_diff_changeset(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, saved
        logical :: found
        integer :: b

        status = status_type()
        b = log_view_vc_backend()

        if (b > 0) then
            ! (eq (vc-call-backend ... 'revision-granularity) 'file): every
            ! ported backend has repository granularity.
            continue
        end if

        call vc_root_dir(root, found, status)
        if (.not. status%ok()) return
        saved = buffers(cur)%directory
        if (found) buffers(cur)%directory = root
        call log_view_diff_common(beg, end, status, .true.)
        buffers(cur)%directory = saved
    end subroutine log_view_diff_changeset

    !> The diff between the revisions at BEG and END (the one before when
    !> they are the same), of the current file(s), or with WHOLE_CHANGESET
    !> of all files.  Port of log-view.el:log-view-diff-common.
    recursive subroutine log_view_diff_common(beg, end, status, whole_changeset)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: whole_changeset
        character(len=:), allocatable :: to, fr, prev, file
        type(vc_fileset_type) :: fs
        integer(int64) :: fr_beg, opoint, last
        logical :: has_to, has_fr, whole, changes, has, previous
        type(vc_string_type), allocatable :: fileset(:)

        status = status_type()
        whole = .false.
        if (present(whole_changeset)) whole = whole_changeset
        call log_view_current_tag(to, has_to, beg)
        call log_view_current_entry(fr_beg, fr, has_fr, end)
        ! When TO and FR are the same, or when point is on a line after
        ! the last entry, look at the previous revision.
        previous = has_to .eqv. has_fr
        if (previous .and. has_to) previous = fr == to

        if (.not. previous) then
            opoint = point()
            call goto_char(end)
            call log_view_end_of_entry()
            last = point()
            call goto_char(opoint)
            previous = end >= last
        end if

        fs%backend = log_view_vc_backend()

        if (previous) then
            has = .false.

            if (fs%backend > 0) then
                if (associated(vc_backends(fs%backend)%previous_revision)) then
                    call vc_backends(fs%backend)%previous_revision("", fr, prev, has)
                end if
            end if

            if (has) then
                fr = prev
            else
                fr = ""
            end if
        end if

        ! The value passed here should follow what
        ! `vc-deduce-fileset' returns.  If we want to see the
        ! diff for all the files in the changeset, pass NIL for
        ! the file list.
        if (whole) then
            allocate (fs%files(0))
        else if (log_view_per_file_logs()) then
            call log_view_current_file(file, status)
            if (.not. status%ok()) return
            allocate (fs%files(1))
            fs%files(1)%s = file
        else
            call log_view_vc_fileset(fileset, has)
            fs%files = fileset
        end if

        if (len(to) == 0) then
            if (len(fr) == 0) then
                call vc_diff_internal(vc_allow_async_diff, fs, changes, status)
            else
                call vc_diff_internal(vc_allow_async_diff, fs, changes, status, rev1=fr)
            end if
        else if (len(fr) == 0) then
            call vc_diff_internal(vc_allow_async_diff, fs, changes, status, rev2=to)
        else
            call vc_diff_internal(vc_allow_async_diff, fs, changes, status, fr, to)
        end if
    end subroutine log_view_diff_common

    !> Copy the ID of the revision at point to the kill ring.
    !> If there are marked revisions, copy the IDs of those, separated by
    !> spaces.  Port of log-view.el:log-view-copy-revision-as-kill.
    recursive subroutine log_view_copy_revision_as_kill(status)
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: revisions(:)
        character(len=:), allocatable :: found, rev
        logical :: has
        integer :: k

        status = status_type()
        call log_view_get_marked(revisions)

        if (size(revisions) > 1) then
            found = revisions(1)%s

            do k = 2, size(revisions)
                found = found//" "//revisions(k)%s
            end do

            call kill_new(found)
            call message(found)
        else
            if (size(revisions) == 1) then
                rev = revisions(1)%s
                has = .true.
            else
                call log_view_current_tag(rev, has)
            end if

            if (has) then
                call kill_new(rev)
                call message(rev)
            else
                call set_error(status, FM_USER_ERROR, "No revision at point")
            end if
        end if
    end subroutine log_view_copy_revision_as_kill

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! (let ((inhibit-read-only t)) ...): bind it, keeping the old value
    ! in SAVED.  Not a port.
    subroutine bind_inhibit_read_only(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        call symbol_value(sym("inhibit_read_only"), saved, st)
        if (.not. st%ok()) saved = value_type()
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine bind_inhibit_read_only

    ! The end of (let ((inhibit-read-only t)) ...): restore SAVED.
    ! Not a port.
    subroutine restore_inhibit_read_only(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(sym("inhibit_read_only"), saved, st)
    end subroutine restore_inhibit_read_only

    ! (interactive "p"): the numeric prefix argument.  Not a port.
    integer(int64) function count_arg()
        count_arg = prefix_numeric_value(current_prefix_arg)
    end function count_arg

    ! (if (use-region-p) (region-beginning) (point)) and the end.
    ! Not a port.
    subroutine region_args(beg, end)
        integer(int64), intent(out) :: beg, end
        type(status_type) :: st

        beg = point()
        end = point()

        if (is_use_region()) then
            call region_beginning(beg, st)
            call region_end(end, st)
        end if
    end subroutine region_args

    ! (interactive "p\nR"): the count and the active region, if any.
    ! Not a port.
    recursive subroutine mark_command(mark, status)
        logical, intent(in) :: mark
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        type(status_type) :: st

        if (is_use_region()) then
            call region_beginning(beg, st)
            call region_end(end, st)
            call mark_unmark(mark, count_arg(), status, beg, end)
        else
            call mark_unmark(mark, count_arg(), status)
        end if
    end subroutine mark_command

    recursive subroutine cmd_log_view_msg_next(status)
        type(status_type), intent(out) :: status

        call log_view_msg_next(status, count_arg())
    end subroutine cmd_log_view_msg_next

    recursive subroutine cmd_log_view_msg_prev(status)
        type(status_type), intent(out) :: status

        call log_view_msg_prev(status, count_arg())
    end subroutine cmd_log_view_msg_prev

    recursive subroutine cmd_log_view_file_next(status)
        type(status_type), intent(out) :: status

        call log_view_file_next(status, count_arg())
    end subroutine cmd_log_view_file_next

    recursive subroutine cmd_log_view_file_prev(status)
        type(status_type), intent(out) :: status

        call log_view_file_prev(status, count_arg())
    end subroutine cmd_log_view_file_prev

    recursive subroutine cmd_log_view_msg_and_diff_next(status)
        type(status_type), intent(out) :: status

        call log_view_msg_and_diff_next(count_arg(), status)
    end subroutine cmd_log_view_msg_and_diff_next

    recursive subroutine cmd_log_view_msg_and_diff_prev(status)
        type(status_type), intent(out) :: status

        call log_view_msg_and_diff_prev(count_arg(), status)
    end subroutine cmd_log_view_msg_and_diff_prev

    recursive subroutine cmd_log_view_mark_entry(status)
        type(status_type), intent(out) :: status

        call mark_command(.true., status)
    end subroutine cmd_log_view_mark_entry

    recursive subroutine cmd_log_view_unmark_entry(status)
        type(status_type), intent(out) :: status

        call mark_command(.false., status)
    end subroutine cmd_log_view_unmark_entry

    recursive subroutine cmd_log_view_find_revision(status)
        type(status_type), intent(out) :: status

        call log_view_find_revision(point(), status)
    end subroutine cmd_log_view_find_revision

    recursive subroutine cmd_log_view_diff(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call region_args(beg, end)
        call log_view_diff(beg, end, status)
    end subroutine cmd_log_view_diff

    recursive subroutine cmd_log_view_diff_changeset(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call region_args(beg, end)
        call log_view_diff_changeset(beg, end, status)
    end subroutine cmd_log_view_diff_changeset

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    ! Define the faces.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => log_view_file_face
        call f%define("log_view_file", "Face for the file header line in `log_view_mode'.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="grey70", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(weight="bold", extend=.true.))
        f => log_view_message_face
        call f%define("log_view_message", "Face for the message header line in `log_view_mode'.")
        call f%spec_entry(display_when(classes=[FACE_CLASS_COLOR], backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(background="grey85", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(weight="bold", extend=.true.))
        f => log_view_commit_body
        call f%define("log_view_commit_body", "Face for the commit body in `log_view_mode'.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_comment_face"))
    end subroutine define_faces

    !> Define Log View mode, its keymap and faces, and register the
    !> commands.  Not a port.
    subroutine syms_of_log_view()
        type(status_type) :: st

        if (associated(major_modes%log_view_mode%run)) return
        call define_faces()
        call major_modes%log_view_mode%define("log_view_mode", "Log-View", log_view_mode, log_view_mode_hook, &
                                              parent=major_modes%special_mode)
        log_view_mode_map = major_modes%log_view_mode%map
        commands%log_view_mode%run => log_view_mode
        commands%log_view_msg_next%run => cmd_log_view_msg_next
        commands%log_view_msg_prev%run => cmd_log_view_msg_prev
        commands%log_view_file_next%run => cmd_log_view_file_next
        commands%log_view_file_prev%run => cmd_log_view_file_prev
        commands%log_view_msg_and_diff_next%run => cmd_log_view_msg_and_diff_next
        commands%log_view_msg_and_diff_prev%run => cmd_log_view_msg_and_diff_prev
        commands%log_view_toggle_mark_entry%run => log_view_toggle_mark_entry
        commands%log_view_mark_entry%run => cmd_log_view_mark_entry
        commands%log_view_unmark_entry%run => cmd_log_view_unmark_entry
        commands%log_view_unmark_all_entries%run => log_view_unmark_all_entries
        commands%log_view_toggle_entry_display%run => log_view_toggle_entry_display
        commands%log_view_display_entry_and_diff%run => log_view_display_entry_and_diff
        commands%log_view_find_revision%run => cmd_log_view_find_revision
        commands%log_view_diff%run => cmd_log_view_diff
        commands%log_view_diff_changeset%run => cmd_log_view_diff_changeset
        commands%log_view_copy_revision_as_kill%run => log_view_copy_revision_as_kill
        call keymap_set(log_view_mode_map, "RET", "log_view_toggle_entry_display", st)
        call keymap_set(log_view_mode_map, "M-RET", "log_view_display_entry_and_diff", st)
        call keymap_set(log_view_mode_map, "m", "log_view_mark_entry", st)
        call keymap_set(log_view_mode_map, "u", "log_view_unmark_entry", st)
        call keymap_set(log_view_mode_map, "U", "log_view_unmark_all_entries", st)
        call keymap_set(log_view_mode_map, "d", "log_view_diff", st)
        call keymap_set(log_view_mode_map, "=", "log_view_diff", st)
        call keymap_set(log_view_mode_map, "D", "log_view_diff_changeset", st)
        call keymap_set(log_view_mode_map, "f", "log_view_find_revision", st)
        call keymap_set(log_view_mode_map, "n", "log_view_msg_next", st)
        call keymap_set(log_view_mode_map, "p", "log_view_msg_prev", st)
        call keymap_set(log_view_mode_map, "M-n", "log_view_msg_and_diff_next", st)
        call keymap_set(log_view_mode_map, "M-p", "log_view_msg_and_diff_prev", st)
        call keymap_set(log_view_mode_map, "w", "log_view_copy_revision_as_kill", st)
        call keymap_set(log_view_mode_map, "TAB", "log_view_msg_next", st)
        call keymap_set(log_view_mode_map, "<backtab>", "log_view_msg_prev", st)
        call add_local_variables_killer(kill_log_view_locals)
    end subroutine syms_of_log_view
end module log_view_fm
