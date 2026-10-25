! log_edit_fm.f90 -- Major mode for editing VC commit log messages.
! Ported from GNU Emacs 31.1 lisp/vc/log-edit.el.
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

!> Log Edit mode: the buffer in which a commit message is written
!> (`log_edit', `log_edit_mode', \[log_edit_done] to commit), the
!> commit comment history (`log_edit_comment_ring' with M-p, M-n, M-r and
!> M-s), the list of files to be committed (`log_edit_show_files'), the
!> diff (`log_edit_show_diff'), the message template and its headers
!> (`log_edit_set_header', `log_edit_extract_headers'), and log messages
!> taken from ChangeLog entries (`log_edit_insert_changelog').
!>
!> The variables that upstream's `log-edit' takes as PARAMS (the
!> function listing the files, the diff function, the VC backend) are
!> arguments of `log_edit' here; the caller's own buffer-local values
!> are set by its SET_PARAMS procedure.
!>
!> Deferred: `log_edit_generate_changelog_from_diff' (C-c C-w, needing
!> `diff_add_log_current_defuns'), `log_edit_add_to_changelog' and
!> `log_edit_comment_to_change_log' (needing `add_change_log_entry'),
!> `log_edit_fill_entry' as the `fill_paragraph_function' (needing
!> `change_log_read_defuns'), `log_edit_done_strip_cvs_lines' (needing
!> `flush_lines'), the BUFFER argument of `log_edit' (PCL-CVS) and so
!> `log_edit_parent_buffer', `log_edit_maybe_show_diff' (needing
!> `display_buffer_overriding_action'), `log_edit_diff_patch' (patches),
!> the menu and the tool bar,
!> `font_lock_extra_managed_props' and `jit_lock_contextually' as
!> buffer-local values, the help-echo texts of the GNU-style keywords,
!> and `hack_dir_local_variables_non_file_buffer' (directory-local
!> variables).
module log_edit_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, format_error
    use handle_fm, only: handle_type, null_handle
    use commands_fm, only: commands
    use lisp_fm, only: command_proc
    use lisp_fm, only: value_type, t_value
    use lread_fm, only: sym
    use data_fm, only: value_eq, add_local_variables_killer
    use eval_fm, only: hook_type
    use ring_fm, only: ring_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer, get_buffer_create, buffer_live_p, &
                         erase_buffer, kill_buffer, buffer_serial_live, generate_new_buffer_name
    use editfns_fm, only: point, goto_char, point_min, point_max, bolp, eobp, bobp, buffer_substring, buffer_string, &
                          delete_region, char_after, char_before, narrow_to_region, widen, line_end_position, &
                          insert_buffer_substring
    use cmds_fm, only: forward_line, end_of_line, forward_char, beginning_of_line
    use insdel_fm, only: insert
    use search_fm, only: looking_at, search_re_forward => re_search_forward, search_re_backward => re_search_backward, &
                         search_fwd => search_forward, search_bwd => search_backward, &
                         match_beginning, match_end, replace_match, string_match, regexp_quote, set_match_data, &
                         case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string, y_or_n_p, match_substitute_replacement
    use syntax_fm, only: skip_syntax_backward, skip_chars_forward
    use textprop_fm, only: put_text_property
    use fileio_fm, only: expand_file_name, file_name_directory, file_name_nondirectory, file_exists_p, &
                         file_readable_p, insert_file_contents
    use files_fm, only: file_relative_name, kill_buffer_hook
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use keyboard_fm, only: last_command
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use minibuf_fm, only: read_string
    use help_fm, only: substitute_command_keys
    use simple_fm, only: rfc822_goto_eoh, push_mark, special_mode, delete_horizontal_space, do_auto_fill, &
                         normal_auto_fill_function
    use indent_fm, only: current_indentation, indent_rigidly
    use fill_fm, only: string_width
    use indent_fm, only: current_column
    use buffer_fm, only: fill_column, get_file_buffer
    use window_fm, only: display_alist_type, shrink_window_if_larger_than_buffer, DISPLAY_ACTION_BELOW_SELECTED, &
                         display_buffer_mark_dedicated, DEDICATED_STRONG, display_buffer, get_buffer_window, &
                         bury_buffer, quit_windows_on, delete_window, select_window
    use frame_fm, only: selected_window
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use callproc_fm, only: call_process
    use derived_fm, only: derived_mode_p
    use modes_fm, only: major_modes
    use keymap_fm, only: keymap_set
    use faces_fm, only: named_face_type, display_when, face_attrs
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            FONT_LOCK_OVERRIDE_T
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use pcvs_util_fm, only: cvs_string_type, cvs_insert_strings
    use add_log_fm, only: find_change_log, add_log_find_changelog_buffer, change_log_mode, change_log_default_name, &
                          add_log_full_name, add_log_mailing_address, add_log_time_format, &
                          add_log_dont_create_changelog_file
    use editfns_fm, only: user_full_name
    use startup_fm, only: user_mail_address_value
    use page_fm, only: forward_page
    implicit none(type, external)
    private

    public :: log_edit_string_type, log_edit_header_type, log_edit_listfun_proc, log_edit_header_function
    public :: log_edit, log_edit_mode, log_edit_files, log_edit_done, log_edit_kill_buffer, log_edit_empty_buffer_p, &
              log_edit_set_header, log_edit_toggle_header, log_edit_extract_headers, log_edit_toggle_amend, &
              log_edit_remember_comment, log_edit_show_files, log_edit_show_diff, log_edit_insert_changelog, &
              log_edit_insert_message_template, log_edit_previous_comment, log_edit_next_comment, &
              log_edit_comment_search_backward, log_edit_comment_search_forward, log_edit_set_common_indentation, &
              log_edit_font_lock_keywords, log_edit_vc_backend, log_edit_diff_fileset, &
              syms_of_log_edit

    !> A string of a list (files, command line arguments).
    type :: log_edit_string_type
        character(len=:), allocatable :: s
    end type log_edit_string_type

    !> Values of `log_edit_confirm'.
    integer, parameter, public :: LOG_EDIT_CONFIRM_NIL = 0, LOG_EDIT_CONFIRM_T = 1, LOG_EDIT_CONFIRM_CHANGED = 2

    !> Values of `log_edit_require_final_newline'.
    integer, parameter, public :: LOG_EDIT_FINAL_NEWLINE_NIL = 0, LOG_EDIT_FINAL_NEWLINE_T = 1, &
                                  LOG_EDIT_FINAL_NEWLINE_ASK = 2

    !> Kinds of the entries of HEADERS for `log_edit_extract_headers': a
    !> command line option given the value (CMDARG), a function of the
    !> value, or the summary (t).
    integer, parameter, public :: LOG_EDIT_HEADER_CMDARG = 0, LOG_EDIT_HEADER_FN = 1, &
                                  LOG_EDIT_HEADER_SUMMARY = 2

    abstract interface
        !> `log_edit_listfun': the list of files that are concerned by the
        !> current operation (using relative names).
        recursive subroutine log_edit_listfun_proc(files, status)
            import :: log_edit_string_type, status_type
            implicit none(type, external)
            type(log_edit_string_type), allocatable, intent(out) :: files(:)
            type(status_type), intent(out) :: status
        end subroutine log_edit_listfun_proc

        !> The FUNCTION of a header for `log_edit_extract_headers': the
        !> strings to append to the arguments for the header's VALUE.
        subroutine log_edit_header_function(value, args)
            import :: log_edit_string_type
            implicit none(type, external)
            character(len=*), intent(in) :: value
            type(log_edit_string_type), allocatable, intent(out) :: args(:)
        end subroutine log_edit_header_function

        !> The LAST-MSG-FN of `log_edit_toggle_amend'.
        recursive subroutine log_edit_last_msg_proc(msg, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: msg
            type(status_type), intent(out) :: status
        end subroutine log_edit_last_msg_proc
    end interface
    public :: log_edit_last_msg_proc

    !> One entry of HEADERS for `log_edit_extract_headers': (HEADER .
    !> CMDARG), (HEADER . FUNCTION) or (HEADER . t).
    type :: log_edit_header_type
        character(len=:), allocatable :: name
        integer :: kind = LOG_EDIT_HEADER_CMDARG
        character(len=:), allocatable :: cmdarg
        procedure(log_edit_header_function), pointer, nopass :: fn => null()
    end type log_edit_header_type

    !> If true, `log_edit_done' will request confirmation.
    !> If LOG_EDIT_CONFIRM_CHANGED, only request confirmation if the list
    !> of files has changed since the beginning of the `log_edit' session.
    !> Port of log-edit.el log-edit-confirm.
    integer, public :: log_edit_confirm = LOG_EDIT_CONFIRM_CHANGED

    !> If true, don't hide the buffer after `log_edit_done'.  Port of
    !> log-edit.el log-edit-keep-buffer.
    logical, public :: log_edit_keep_buffer = .false.

    !> Enforce a newline at the end of commit log messages.
    !> Enforce it silently if LOG_EDIT_FINAL_NEWLINE_T, query if
    !> LOG_EDIT_FINAL_NEWLINE_ASK and don't do anything if
    !> LOG_EDIT_FINAL_NEWLINE_NIL.  Port of log-edit.el
    !> log-edit-require-final-newline.
    integer, public :: log_edit_require_final_newline = LOG_EDIT_FINAL_NEWLINE_T

    !> True means `log_edit' should invert the meaning of its SETUP arg.
    !> If SETUP is forced, this variable has no effect.  Port of
    !> log-edit.el log-edit-setup-invert.
    logical, public :: log_edit_setup_invert = .false.

    !> True means `log_edit' may add the `Author:' header.
    !> This applies when its SETUP argument is true.  Port of log-edit.el
    !> log-edit-setup-add-author.
    logical, public :: log_edit_setup_add_author = .false.

    !> Hook run at the end of `log_edit'.  Port of log-edit.el
    !> log-edit-hook (its functions are set by `syms_of_log_edit').
    type(hook_type), target, public :: log_edit_hook = hook_type(name="log_edit_hook")

    !> Hook run when entering `log_edit_mode'.  Port of log-edit.el
    !> log-edit-mode-hook.
    type(hook_type), target, public :: log_edit_mode_hook = hook_type(name="log_edit_mode_hook")

    !> Hook run before doing the actual commit.
    !> This hook can be used to cleanup the message, enforce various
    !> conventions, or to allow recording the message in some other database,
    !> such as a bug-tracking system.  The list of files about to be committed
    !> can be obtained from `log_edit_files'.  Port of log-edit.el
    !> log-edit-done-hook.
    type(hook_type), target, public :: log_edit_done_hook = hook_type(name="log_edit_done_hook")

    !> If true, remove file name from single-file log entries.  Port of
    !> log-edit.el log-edit-strip-single-file-name.
    logical, public :: log_edit_strip_single_file_name = .false.

    !> If true, include full ChangeLog paragraphs in the log.
    !> This may be set in the "local variables" section of a ChangeLog, to
    !> indicate the policy for that ChangeLog.
    !>
    !> A ChangeLog paragraph is a bunch of log text containing no blank lines;
    !> a paragraph usually describes a set of changes with a single purpose,
    !> but perhaps spanning several functions in several files.  Changes in
    !> different paragraphs are unrelated.
    !>
    !> You could argue that the log entry for a file should contain the
    !> full ChangeLog paragraph mentioning the change to the file, even though
    !> it may mention other files, because that gives you the full context you
    !> need to understand the change.  This is the behavior you get when this
    !> variable is set to true.
    !>
    !> On the other hand, you could argue that the log entry for a change
    !> should contain only the text for the changes which occurred in that
    !> file, because the log is per-file.  This is the behavior you get
    !> when this variable is set to false.  Port of log-edit.el
    !> log-edit-changelog-full-paragraphs.
    logical, public :: log_edit_changelog_full_paragraphs = .true.

    !> Minimum indentation to use in `log_edit_set_common_indentation'.
    !> Port of log-edit.el log-edit-common-indent.
    integer, public :: log_edit_common_indent = 0

    !> If true, highlight common failures to follow VC commit log conventions.
    !> The conventions checked are those described in the GNU coding standards
    !> document.  Port of log-edit.el log-edit-font-lock-gnu-style.
    logical, public :: log_edit_font_lock_gnu_style = .false.

    !> True means rewrite (tiny change).  Port of log-edit.el
    !> log-edit-rewrite-tiny-change.
    logical, public :: log_edit_rewrite_tiny_change = .true.

    !> Rule to rewrite bug numbers into Fixes: headers in commit log messages.
    !> The value should be of the form (REGEXP . REPLACEMENT)
    !> where REGEXP should match the expression referring to a bug number
    !> in the text, and REPLACEMENT is an expression to pass to `replace_match'
    !> to build the Fixes: header.  Port of log-edit.el
    !> log-edit-rewrite-fixes (unallocated for nil).
    character(len=:), allocatable, public :: log_edit_rewrite_fixes_regexp, log_edit_rewrite_fixes_replacement

    !> The name of the buffer listing the files to be committed.  Port of
    !> log-edit.el log-edit-files-buf.
    character(len=*), parameter, public :: LOG_EDIT_FILES_BUF = "*log-edit-files*"

    !> Maximum number of saved commit comments in `log_edit_comment_ring'.
    !> Port of log-edit.el log-edit-maximum-comment-ring-size.
    integer, parameter, public :: LOG_EDIT_MAXIMUM_COMMENT_RING_SIZE = 32

    !> The commit log comments.  Port of log-edit.el log-edit-comment-ring.
    type(ring_type), public :: log_edit_comment_ring

    ! log-edit-comment-ring-index (its default value; Log Edit buffers
    ! have their own) and log-edit-last-comment-match.
    logical :: ring_index_set = .false.
    integer :: ring_index = 0
    character(len=:), allocatable :: last_comment_match

    !> Regular expression matching the header field in `log_edit_mode'.
    !> The first subexpression is the actual text of the field.  Port of
    !> log-edit.el log-edit-header-contents-regexp.
    character(len=*), parameter, public :: LOG_EDIT_HEADER_CONTENTS_REGEXP = "[ "//achar(9)//"]*\(.*\(" &
                                                                          //achar(10)//"[ "//achar(9)//"].*\)*\)" &
                                                                          //achar(10)//"?"

    !> Face for the summary in `log_edit_mode' buffers.  Port of
    !> log-edit.el log-edit-summary.
    type(named_face_type), target, public :: log_edit_summary
    !> Face for the separator line in `log_edit_mode' buffers.
    type(named_face_type), target, public :: log_edit_headers_separator
    !> Face for the headers in `log_edit_mode' buffers.
    type(named_face_type), target, public :: log_edit_header
    !> Face for unknown headers in `log_edit_mode' buffers.
    type(named_face_type), target, public :: log_edit_unknown_header

    !> Keymap of `log_edit_mode'.  Port of log-edit.el log-edit-mode-map.
    type(handle_type), public :: log_edit_mode_map

    ! A Log Edit buffer's own values of log-edit-callback,
    ! log-edit-listfun, log-edit-diff-function, log-edit-vc-backend,
    ! log-edit-initial-files and log-edit-comment-ring-index, by
    ! serial number.
    type :: local_type
        integer(int64) :: serial = 0
        procedure(command_proc), pointer, nopass :: callback => null()
        procedure(log_edit_listfun_proc), pointer, nopass :: listfun => null()
        procedure(command_proc), pointer, nopass :: diff_function => null()
        integer :: vc_backend = 0
        logical :: has_initial_files = .false.
        type(log_edit_string_type), allocatable :: initial_files(:)
        logical :: has_ring_index = .false.
        logical :: ring_index_set = .false.
        integer :: ring_index = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! log-edit-author: the author found by `log_edit_changelog_ours_p'
    ! (dynamically bound by `log_edit_insert_changelog_entries').
    logical :: author_bound = .false.
    logical :: has_author = .false.
    character(len=:), allocatable :: author_name
    logical :: author_tiny = .false.

    ! log-edit-changelog-use-first (dynamically bound).
    logical :: changelog_use_first = .false.

    ! The ChangeLog entries for a file (see `log_edit_changelog_entries'):
    ! LOGBUFFER (null if none) and the bounds of each entry in it.
    type :: changelog_entries_type
        type(handle_type) :: buffer
        integer :: n = 0
        integer(int64), allocatable :: beg(:), end(:)
    end type changelog_entries_type

    interface
        !> Display diffs for the files to be committed.  Port of
        !> log-edit.el:log-edit-diff-fileset.  (In log_edit_vc_fm.)
        recursive module subroutine log_edit_diff_fileset(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine log_edit_diff_fileset

        !> Provide help for the `log_edit_mode_map'.  Port of
        !> log-edit.el:log-edit-mode-help.  (In log_edit_vc_fm.)
        recursive module subroutine log_edit_mode_help(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine log_edit_mode_help
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
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
    subroutine kill_log_edit_locals()
        integer :: k
        k = local_index(.false.)
        if (k > 0) locals(k) = local_type()
    end subroutine kill_log_edit_locals

    !> The current buffer's `log_edit_vc_backend' (a VC backend index, 0
    !> for nil): the VC fileset backend corresponding to the current log.
    integer function log_edit_vc_backend() result(b)
        integer :: k
        b = 0
        k = local_index(.false.)
        if (k > 0) b = locals(k)%vc_backend
    end function log_edit_vc_backend

    ! The current value of log-edit-comment-ring-index (SET false for nil).
    subroutine get_ring_index(set, index)
        logical, intent(out) :: set
        integer, intent(out) :: index
        integer :: k
        k = local_index(.false.)
        if (k > 0) then
            if (locals(k)%has_ring_index) then
                set = locals(k)%ring_index_set
                index = locals(k)%ring_index
                return
            end if
        end if
        set = ring_index_set
        index = ring_index
    end subroutine get_ring_index

    ! Set log-edit-comment-ring-index.
    subroutine set_ring_index(index)
        integer, intent(in) :: index
        integer :: k
        k = local_index(.false.)
        if (k > 0) then
            if (locals(k)%has_ring_index) then
                locals(k)%ring_index_set = .true.
                locals(k)%ring_index = index
                return
            end if
        end if
        ring_index_set = .true.
        ring_index = index
    end subroutine set_ring_index

    ! A marker at POS (save-excursion and save-restriction keep
    ! positions in markers).
    function marker_at(pos, insertion_type) result(m)
        integer(int64), intent(in) :: pos
        logical, intent(in), optional :: insertion_type
        type(handle_type) :: m
        type(status_type) :: st
        logical :: it
        it = .false.
        if (present(insertion_type)) it = insertion_type
        call copy_marker_at(pos, m, it, st)
    end function marker_at

    ! The position of marker M.
    integer(int64) function mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call marker_position(m, mpos, st)
    end function mpos

    ! The buffer visiting FILE, or the null handle (files.el
    ! find-buffer-visiting without truenames).
    function find_buffer_visiting(file) result(h)
        character(len=*), intent(in) :: file
        type(handle_type) :: h
        character(len=:), allocatable :: expanded
        type(status_type) :: st
        h = null_handle()
        call expand_file_name(file, expanded, st, buffers(cur)%directory)
        if (st%ok()) h = get_file_buffer(expanded)
    end function find_buffer_visiting

    ! Format PROMPT with DEFAULT: "PROMPT (default DEFAULT): ", or
    ! "PROMPT: " when DEFAULT is empty.  Port of minibuffer.el:format-prompt.
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full
        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function format_prompt

    ! The position of marker M, which is then freed.
    integer(int64) function release(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call marker_position(m, release, st)
        call free_marker(m, st)
    end function release

    ! The searches, telling whether they found a match (FOUND).
    recursive subroutine re_search_forward(regexp, found, status, bound, noerror)
        character(len=*), intent(in) :: regexp
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        integer, intent(in), optional :: noerror
        integer(int64) :: pos
        call search_re_forward(regexp, pos, status, bound=bound, noerror=noerror)
        found = pos /= 0 .and. status%ok()
    end subroutine re_search_forward

    recursive subroutine re_search_backward(regexp, found, status, bound, noerror)
        character(len=*), intent(in) :: regexp
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        integer, intent(in), optional :: noerror
        integer(int64) :: pos
        call search_re_backward(regexp, pos, status, bound=bound, noerror=noerror)
        found = pos /= 0 .and. status%ok()
    end subroutine re_search_backward

    recursive subroutine search_forward(string, found, status, noerror)
        character(len=*), intent(in) :: string
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: noerror
        integer(int64) :: pos
        call search_fwd(string, pos, status, noerror=noerror)
        found = pos /= 0 .and. status%ok()
    end subroutine search_forward

    recursive subroutine search_backward(string, found, status, noerror)
        character(len=*), intent(in) :: string
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: noerror
        integer(int64) :: pos
        call search_bwd(string, pos, status, noerror=noerror)
        found = pos /= 0 .and. status%ok()
    end subroutine search_backward

    ! ------------------------------------------------------------------
    ! The comment ring
    ! ------------------------------------------------------------------

    !> Return the comment whose index is STRIDE elements away from the current one.
    !> This accesses `log_edit_comment_ring', which stores commit log comments,
    !> i.e. descriptions of changes done by commits.
    !> LEN is the length of `log_edit_comment_ring'.  Port of
    !> log-edit.el:log-edit-new-comment-index.
    integer function log_edit_new_comment_index(stride, len) result(index)
        integer, intent(in) :: stride, len
        logical :: set
        integer :: current
        call get_ring_index(set, current)
        if (set) then
            index = modulo(current + stride, len)
        else if (stride > 0) then
            ! Initialize the index on the first use of this command
            ! so that the first M-p gets index 0, and the first M-n gets
            ! index -1.
            index = modulo(stride - 1, len)
        else
            index = modulo(stride, len)
        end if
    end function log_edit_new_comment_index

    !> Cycle backwards through VC commit comment history.
    !> With a numeric prefix ARG, go back ARG comments.  Port of
    !> log-edit.el:log-edit-previous-comment.
    recursive subroutine log_edit_previous_comment(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: item
        character(len=16) :: num
        integer :: ring_len, index
        status = status_type()
        ring_len = log_edit_comment_ring%count()
        if (ring_len <= 0) then
            call message("Empty comment ring")
            call ding()
        else
            ! Don't use `erase-buffer' because we don't want to `widen'.
            call delete_region(point_min(), point_max(), status)
            if (.not. status%ok()) return
            index = log_edit_new_comment_index(arg, ring_len)
            call set_ring_index(index)
            write (num, "(i0)") index + 1
            call message("Comment "//trim(num))
            call log_edit_comment_ring%ref(index, item, status)
            if (.not. status%ok()) return
            call insert(item, status)
        end if
    end subroutine log_edit_previous_comment

    !> Cycle forwards through VC commit comment history.
    !> With a numeric prefix ARG, go forward ARG comments.  Port of
    !> log-edit.el:log-edit-next-comment.
    recursive subroutine log_edit_next_comment(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        call log_edit_previous_comment(-arg, status)
    end subroutine log_edit_next_comment

    !> Search backwards through VC commit comment history for a match of STR.
    !> If the optional argument STRIDE is present, that is a step-width to use
    !> when going through the comment ring, `log_edit_comment_ring'.  Port of
    !> log-edit.el:log-edit-comment-search-backward.
    recursive subroutine log_edit_comment_search_backward(str_in, status, stride)
        character(len=*), intent(in) :: str_in
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: stride
        character(len=:), allocatable :: str, item
        integer :: step, ring_len, n
        status = status_type()
        step = 1
        if (present(stride)) step = stride
        if (.not. allocated(last_comment_match)) last_comment_match = ""
        if (len_trim(str_in) == 0 .and. len(str_in) == 0) then
            str = last_comment_match
        else
            str = str_in
            last_comment_match = str_in
        end if
        str = regexp_quote(str)
        ring_len = log_edit_comment_ring%count()
        n = log_edit_new_comment_index(step, ring_len)
        do
            if (n >= ring_len .or. n < 0) then
                call set_error(status, FM_GENERIC_ERROR, "Not found")
                return
            end if
            call log_edit_comment_ring%ref(n, item, status)
            if (.not. status%ok()) return
            if (string_match(str, item, status) > 0) exit
            if (.not. status%ok()) return
            n = n + step
        end do
        call set_ring_index(n)
        call log_edit_previous_comment(0, status)
    end subroutine log_edit_comment_search_backward

    !> Search forwards through VC commit comment history for a match of STR.
    !> Port of log-edit.el:log-edit-comment-search-forward.
    recursive subroutine log_edit_comment_search_forward(str, status)
        character(len=*), intent(in) :: str
        type(status_type), intent(out) :: status
        call log_edit_comment_search_backward(str, status, -1)
    end subroutine log_edit_comment_search_forward

    ! ------------------------------------------------------------------
    ! Font Lock
    ! ------------------------------------------------------------------

    ! Port of log-edit.el:log-edit-match-to-eoh.
    subroutine log_edit_match_to_eoh(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: start
        found = .false.
        if (limit < 0) continue
        start = point()
        call rfc822_goto_eoh(status)
        if (.not. status%ok()) return
        ! Typical situation: some temporary change causes the header to be
        ! incorrect, so EOH comes earlier than intended: the last lines of the
        ! intended headers are now not considered part of the header any more,
        ! so they don't have the multiline property set.  When the change is
        ! completed and the header has its correct shape again, the lack of the
        ! multiline property means we won't rehighlight the last lines of
        ! the header.
        if (point() < start) return
        ! Here we disregard LIMIT so that we may extend the area again.
        call set_match_data([start, point()])
        found = .true.
    end subroutine log_edit_match_to_eoh

    !> Go to the end of the headers.  Port of log-edit.el:log-edit-goto-eoh.
    subroutine log_edit_goto_eoh(status)
        type(status_type), intent(out) :: status
        logical :: found
        call goto_char(point_min())
        call re_search_forward("^\([^[:alpha:]]\|[[:alnum:]-]+[^[:alnum:]-]\)", found, status, noerror=SEARCH_MOVE)
        if (found) call goto_char(match_beginning(0, status))
    end subroutine log_edit_goto_eoh

    ! Port of log-edit.el:log-edit--match-first-line.
    subroutine log_edit_match_first_line(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: start, here, moved
        logical :: summary
        found = .false.
        start = point()
        call log_edit_goto_eoh(status)
        if (.not. status%ok()) return
        moved = skip_chars_forward(achar(10), status)
        if (.not. (start < line_end_position())) return
        if (.not. (point() < limit)) return
        here = point()
        call re_search_backward("^Summary:[ "//achar(9)//"]*[^ "//achar(9)//achar(10)//"]", summary, status, &
                                noerror=SEARCH_NOERROR)
        call goto_char(here)
        if (summary) return
        if (.not. looking_at(".+", status)) return
        call goto_char(match_end(0, status))
        call put_text_property(point_min(), point(), sym("jit_lock_defer_multiline"), t_value(), status)
        found = .true.
    end subroutine log_edit_match_first_line

    ! The face of a header's name: (if (assoc-string (match-string 2)
    ! log-edit-headers-alist t) 'log-edit-header 'log-edit-unknown-header).
    subroutine header_name_face(face)
        character(len=:), allocatable, intent(out) :: face
        if (known_header(match_string(2))) then
            face = "log_edit_header"
        else
            face = "log_edit_unknown_header"
        end if
    end subroutine header_name_face

    ! The face of a header's contents: (or (cdr (assoc-string ...))
    ! 'log-edit-header).
    subroutine header_contents_face(face)
        character(len=:), allocatable, intent(out) :: face
        face = header_alist_face(match_string(2))
        if (len(face) == 0) face = "log_edit_header"
    end subroutine header_contents_face

    ! Whether NAME is in log-edit-headers-alist (ignoring case):
    ! ("Summary" . log-edit-summary) ("Fixes") ("Author").
    logical function known_header(name)
        character(len=*), intent(in) :: name
        known_header = string_equal_ci(name, "Summary") .or. string_equal_ci(name, "Fixes") .or. &
                       string_equal_ci(name, "Author")
    end function known_header

    ! The face log-edit-headers-alist gives NAME ("" for none).
    function header_alist_face(name) result(face)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: face
        face = ""
        if (string_equal_ci(name, "Summary")) face = "log_edit_summary"
    end function header_alist_face

    ! Whether A and B are equal ignoring the case of ASCII letters.
    logical function string_equal_ci(a, b)
        character(len=*), intent(in) :: a, b
        integer :: i
        string_equal_ci = .false.
        if (len(a) /= len(b)) return
        do i = 1, len(a)
            if (lower(a(i:i)) /= lower(b(i:i))) return
        end do
        string_equal_ci = .true.
    end function string_equal_ci

    ! C in lower case (ASCII).
    character function lower(c)
        character, intent(in) :: c
        lower = c
        if (c >= "A" .and. c <= "Z") lower = achar(iachar(c) + 32)
    end function lower

    ! (progn (goto-char (match-beginning 0)) (match-end 0)).
    subroutine headers_pre_form(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st
        call goto_char(match_beginning(0, st))
        limit = match_end(0, st)
    end subroutine headers_pre_form

    ! (and (not (bound-and-true-p git-commit-mode))
    !      (progn (goto-char (match-end 0)) (1+ (match-end 0)))).
    subroutine separator_pre_form(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st
        call goto_char(match_end(0, st))
        limit = match_end(0, st) + 1
    end subroutine separator_pre_form

    ! The face of ("^(" (0 ...)) in log-edit-font-lock-gnu-keywords.
    subroutine gnu_punctuate_face(face)
        use syntax_fm, only: char_syntax
        character(len=:), allocatable, intent(out) :: face
        type(status_type) :: st
        integer(int64) :: beg
        integer :: c
        logical :: found
        face = ""
        beg = max(point_min(), match_beginning(0, st) - 2)
        call put_text_property(beg, match_end(0, st), sym("font_lock_multiline"), t_value(), st)
        call char_after(c, found, beg)
        if (found) then
            if (char_syntax(c) == "w") face = "font_lock_warning_face"
        end if
    end subroutine gnu_punctuate_face

    !> The keywords of Log Edit mode: log-edit-font-lock-keywords, with
    !> log-edit-font-lock-gnu-keywords when `log_edit_font_lock_gnu_style'.
    !> Port of log-edit.el:log-edit-font-lock-keywords.
    subroutine log_edit_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        type(font_lock_highlight_type) :: inner(2), sep(1), anchors(2), first(1), h(1)
        integer :: n
        n = 2
        if (log_edit_font_lock_gnu_style) n = 7
        allocate (keywords(n))
        ! Copied/inspired by message-font-lock-keywords.
        inner(1) = font_lock_highlight(1, face_function=header_name_face, laxmatch=.true.)
        ! From `log-edit-header-contents-regexp':
        inner(2) = font_lock_highlight(3, face_function=header_contents_face, laxmatch=.true.)
        anchors(1) = font_lock_anchored(inner, regexp="^\(\([[:alpha:]-]+\):\)"//LOG_EDIT_HEADER_CONTENTS_REGEXP, &
                                        pre_match_form=headers_pre_form)
        sep(1) = font_lock_highlight(0, face="log_edit_headers_separator", &
                                     true_properties=[character(len=28) :: "display_line_numbers_disable", &
                                                      "rear_nonsticky"])
        anchors(2) = font_lock_anchored(sep, regexp="^"//achar(10), pre_match_form=separator_pre_form)
        keywords(1) = font_lock_keyword(highlights=anchors, matcher=log_edit_match_to_eoh)
        first(1) = font_lock_highlight(0, face="log_edit_summary")
        keywords(2) = font_lock_keyword(highlights=first, matcher=log_edit_match_first_line)
        if (.not. log_edit_font_lock_gnu_style) return
        ! Use
        !   * foo.el (bla, bli)
        !   (blo, blu): Toto.
        ! Rather than
        !   * foo.el (bla, bli,
        !   blo, blu): Toto.
        h(1) = font_lock_highlight(1, face="font_lock_warning_face", override=FONT_LOCK_OVERRIDE_T)
        keywords(3) = font_lock_keyword(regexp="^[ "//achar(9)//"]*\(?:\* .*\)?\(([^"//achar(10)//")]*,\s-*\)$", &
                                        highlights=h)
        ! Change Log entries should use present tense.
        keywords(4) = font_lock_keyword(regexp="):[ "//achar(9)//achar(10)//"]*[[:alpha:]]+\(ed\)\>", highlights=h)
        ! Change log entries start with a capital letter.
        h(1) = font_lock_highlight(0, face="font_lock_warning_face", override=FONT_LOCK_OVERRIDE_T)
        keywords(5) = font_lock_keyword(regexp="): [a-z]", highlights=h)
        h(1) = font_lock_highlight(1, face="font_lock_warning_face", override=FONT_LOCK_OVERRIDE_T)
        keywords(6) = font_lock_keyword(regexp="[^[:upper:]]\(\. [[:upper:]]\)", highlights=h)
        h(1) = font_lock_highlight(0, face_function=gnu_punctuate_face, override=FONT_LOCK_OVERRIDE_T)
        keywords(7) = font_lock_keyword(regexp="^(", highlights=h)
    end subroutine log_edit_font_lock_keywords

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Setup a buffer to enter a VC commit log message.
    !> The buffer is put in mode MODE, or `log_edit_mode' if MODE is absent.
    !> If SETUP is true, erase the buffer and run `log_edit_hook'.
    !> Set mark and point around the entire contents of the buffer, so
    !> that it is easy to kill the contents of the buffer with
    !> \[kill_region].  Once the user is done editing the message, he
    !> or she is expected to invoke the command \[log_edit_done] (`log_edit_done'),
    !> which will call CALLBACK, a function to do the actual commit.
    !>
    !> LISTFUN, if present, is the function taking no arguments that
    !> returns the list of files that are concerned by the current operation
    !> (using relative names); DIFF_FUNCTION the function taking no
    !> arguments that displays a diff of the files concerned by the current
    !> operation, VC_BACKEND the VC backend of the files, and SET_PARAMS a
    !> procedure giving the caller's other buffer-local variables their
    !> values (PARAMS).
    !>
    !> This function uses the current buffer.  SETUP_FORCE is the SETUP
    !> `force', on which `log_edit_setup_invert' has no effect.  Port of
    !> log-edit.el:log-edit (no BUFFER argument).
    recursive subroutine log_edit(callback, status, setup, setup_force, listfun, diff_function, vc_backend, &
                                  set_params, mode)
        procedure(command_proc) :: callback
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: setup, setup_force
        procedure(log_edit_listfun_proc), optional :: listfun
        procedure(command_proc), optional :: diff_function, set_params, mode
        integer, intent(in), optional :: vc_backend
        logical :: do_setup
        integer :: k
        do_setup = .false.
        if (present(setup)) do_setup = setup
        if (present(setup_force)) then
            if (setup_force) do_setup = .true.
        end if
        if (log_edit_setup_invert .and. .not. opt(setup_force)) do_setup = .not. do_setup
        if (present(mode)) then
            call mode(status)
        else
            call log_edit_mode(status)
        end if
        if (.not. status%ok()) return
        k = local_index(.true.)
        locals(k)%callback => callback
        if (present(listfun)) locals(k)%listfun => listfun
        if (present(diff_function)) locals(k)%diff_function => diff_function
        if (present(vc_backend)) locals(k)%vc_backend = vc_backend
        if (present(set_params)) then
            call set_params(status)
            if (.not. status%ok()) return
        end if
        k = local_index(.true.)
        call log_edit_files(locals(k)%initial_files)
        locals(k)%has_initial_files = .true.
        if (do_setup) then
            call erase_buffer(status)
            if (.not. status%ok()) return
            call log_edit_hook%run(status)
            if (.not. status%ok()) return
        end if
        call push_mark(point_max())
        ! `vc-start-logentry' already emits a message; avoid a duplicate,
        ! and ensure we don't emit one at all in the case of doing the
        ! action immediately.
        if (.not. present(mode)) then
            call message(substitute_command_keys("Press \<log_edit_mode_map>\[log_edit_done] when you are done editing."))
        end if
    contains
        logical function opt(x)
            logical, intent(in), optional :: x
            opt = .false.
            if (present(x)) opt = x
        end function opt
    end subroutine log_edit

    !> Major mode for editing version-control (VC) commit log messages.
    !> When done editing the log entry, type \[log_edit_done], which will
    !> trigger the actual commit of the file(s).
    !> Several other handy support commands are provided, and the package
    !> from which this is used might also provide additional commands (under
    !> the \[vc_prefix_map] prefix for VC commands, for example).
    !>
    !> \{log_edit_mode_map}
    !>
    !> Port of log-edit.el:log-edit-mode (see the module documentation for
    !> what is deferred).
    recursive subroutine log_edit_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved
        integer :: k
        if (.not. associated(major_modes%log_edit_mode%run)) call syms_of_log_edit()
        call major_modes%log_edit_mode%begin(saved, status)
        if (.not. status%ok()) return
        call log_edit_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call normal_auto_fill_function%set_local(log_edit_do_auto_fill)
        k = local_index(.true.)
        locals(k)%has_ring_index = .true.
        locals(k)%ring_index_set = ring_index_set
        locals(k)%ring_index = ring_index
        call kill_buffer_hook%add(log_edit_remember_comment_hook, local=.true.)
        call major_modes%log_edit_mode%finish(saved, status)
    end subroutine log_edit_mode

    !> Like `do_auto_fill', but don't fill in Log Edit headers.  Port of
    !> log-edit.el:log-edit-do-auto-fill.
    recursive subroutine log_edit_do_auto_fill(status)
        type(status_type), intent(out) :: status
        integer(int64) :: here, eoh
        status = status_type()
        here = point()
        call rfc822_goto_eoh(status)
        eoh = point()
        call goto_char(here)
        if (.not. status%ok()) return
        if (.not. (eoh > here)) call do_auto_fill(status)
    end subroutine log_edit_do_auto_fill

    !> Hide the buffer BUF (`log_edit_files_buf' if absent).  Port of
    !> log-edit.el:log-edit-hide-buf.
    recursive subroutine log_edit_hide_buf(buf_in)
        type(handle_type), intent(in), optional :: buf_in
        type(handle_type) :: buf, win, sw
        type(status_type) :: st
        if (present(buf_in)) then
            buf = buf_in
        else
            buf = get_buffer(LOG_EDIT_FILES_BUF)
        end if
        if (.not. buffer_live_p(buf)) return
        ! FIXME: Should use something like `quit-windows-on' here, but
        ! that function never deletes this buffer's window because it
        ! was created using `cvs-pop-to-buffer-same-frame'.
        sw = selected_window()
        win = get_buffer_window(buf)
        if (win%index /= 0) call delete_window(win, st)
        call bury_buffer(st, buf)
        call select_window(sw, st)
    end subroutine log_edit_hide_buf

    !> Remember COMMENT (the buffer's text if absent) in
    !> `log_edit_comment_ring', unless it is the latest comment there.
    !> Port of log-edit.el:log-edit-remember-comment.
    recursive subroutine log_edit_remember_comment(comment)
        character(len=*), intent(in), optional :: comment
        character(len=:), allocatable :: c, latest
        type(status_type) :: st
        if (present(comment)) then
            c = comment
        else
            c = buffer_string()
        end if
        if (.not. log_edit_comment_ring%empty_p()) then
            call log_edit_comment_ring%ref(0, latest, st)
            if (latest == c) return
        end if
        call log_edit_comment_ring%insert(c)
    end subroutine log_edit_remember_comment

    ! log-edit-remember-comment on `kill_buffer_hook'.
    recursive subroutine log_edit_remember_comment_hook(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call log_edit_remember_comment()
    end subroutine log_edit_remember_comment_hook

    !> Finish editing the VC commit log message, and commit the files.
    !> If you want to abort the commit, simply delete the buffer.  Port of
    !> log-edit.el:log-edit-done.
    recursive subroutine log_edit_done(status)
        type(status_type), intent(out) :: status
        type(log_edit_string_type), allocatable :: files(:)
        type(handle_type) :: win
        integer(int64) :: beg, moved
        integer :: c, k
        logical :: found, add, yes, same
        status = status_type()
        ! Clean up empty headers.
        call goto_char(point_min())
        do while (looking_at("^[a-z]*:"//LOG_EDIT_HEADER_CONTENTS_REGEXP, status))
            beg = match_beginning(0, status)
            call goto_char(match_end(0, status))
            if (string_match("\`[ "//achar(10)//achar(9)//"]*\'", match_string(1), status) > 0) then
                call delete_region(beg, point(), status)
                if (.not. status%ok()) return
            end if
        end do
        if (.not. status%ok()) return
        ! Get rid of leading empty lines.
        call goto_char(point_min())
        if (looking_at("\([ "//achar(9)//"]*"//achar(10)//"\)+", status)) then
            call delete_region(match_beginning(0, status), match_end(0, status), status)
            if (.not. status%ok()) return
        end if
        ! Get rid of trailing empty lines
        call goto_char(point_max())
        moved = skip_syntax_backward(" ")
        call char_after(c, found)
        if (found) then
            if (c == 10) call forward_char(status, 1_int64)
        end if
        call delete_region(point(), point_max(), status)
        if (.not. status%ok()) return
        ! Check for final newline
        if (point_max() > point_min()) then
            call char_before(c, found, point_max())
            if (c /= 10) then
                add = log_edit_require_final_newline == LOG_EDIT_FINAL_NEWLINE_T
                if (.not. add .and. log_edit_require_final_newline /= LOG_EDIT_FINAL_NEWLINE_NIL) then
                    call y_or_n_p("Buffer "//buffers(cur)%name//" does not end in newline.  Add one? ", add, status)
                    if (.not. status%ok()) return
                end if
                if (add) then
                    beg = point()
                    call goto_char(point_max())
                    call insert(achar(10), status)
                    call goto_char(beg)
                    if (.not. status%ok()) return
                end if
            end if
        end if
        call log_edit_remember_comment()
        win = get_buffer_window(get_buffer(LOG_EDIT_FILES_BUF))
        yes = .true.
        if (log_edit_confirm /= LOG_EDIT_CONFIRM_NIL) then
            same = .false.
            if (log_edit_confirm == LOG_EDIT_CONFIRM_CHANGED) then
                call log_edit_files(files)
                k = local_index(.false.)
                if (k > 0) then
                    if (locals(k)%has_initial_files) same = same_files(files, locals(k)%initial_files)
                end if
            end if
            if (.not. same) then
                call log_edit_show_files(status)
                if (.not. status%ok()) return
                call y_or_n_p("Really commit? ", yes, status)
                if (.not. status%ok()) return
            end if
        end if
        if (.not. yes) then
            if (win%index == 0) call log_edit_hide_buf()
            call message("Oh, well!  Later maybe?")
            return
        end if
        call log_edit_done_hook%run(status)
        if (.not. status%ok()) return
        call log_edit_hide_buf()
        k = local_index(.false.)
        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No log operation is pending")
            return
        end if
        if (.not. associated(locals(k)%callback)) then
            call set_error(status, FM_GENERIC_ERROR, "No log operation is pending")
            return
        end if
        call locals(k)%callback(status)
    contains
        logical function same_files(a, b)
            type(log_edit_string_type), intent(in) :: a(:), b(:)
            integer :: i
            same_files = .false.
            if (size(a) /= size(b)) return
            do i = 1, size(a)
                if (a(i)%s /= b(i)%s) return
            end do
            same_files = .true.
        end function same_files
    end subroutine log_edit_done

    !> Kill the current VC commit log buffer.
    !> This command saves the contents of the log buffer in the VC commit
    !> comment history, see `log_edit_comment_ring', and hides `log_edit_files_buf'.
    !> Port of log-edit.el:log-edit-kill-buffer.
    recursive subroutine log_edit_kill_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        call log_edit_hide_buf()
        buf = current_buffer()
        call quit_windows_on(buf, .false., status)
        if (.not. status%ok()) return
        call kill_buffer(buf, status)
    end subroutine log_edit_kill_buffer

    !> Return the list of files that are about to be committed.  Port of
    !> log-edit.el:log-edit-files (errors are ignored).
    recursive subroutine log_edit_files(files)
        type(log_edit_string_type), allocatable, intent(out) :: files(:)
        type(status_type) :: st
        integer :: k
        k = local_index(.false.)
        if (k > 0) then
            if (associated(locals(k)%listfun)) then
                call locals(k)%listfun(files, st)
                if (st%ok() .and. allocated(files)) return
            end if
        end if
        if (allocated(files)) deallocate (files)
        allocate (files(0))
    end subroutine log_edit_files

    !> (Un)Indent the current buffer rigidly to `log_edit_common_indent'.
    !> Port of log-edit.el:log-edit-set-common-indentation.
    recursive subroutine log_edit_set_common_indentation(status)
        type(status_type), intent(out) :: status
        integer(int64) :: common, here, shortage
        here = point()
        common = point_max()
        call rfc822_goto_eoh(status)
        if (.not. status%ok()) return
        do while (point() < point_max())
            if (.not. looking_at("^[ "//achar(9)//"]*$", status)) common = min(common, current_indentation())
            shortage = forward_line(1_int64)
        end do
        call rfc822_goto_eoh(status)
        if (status%ok()) call indent_rigidly(point(), point_max(), log_edit_common_indent - common, status)
        call goto_char(here)
    end subroutine log_edit_set_common_indentation

    !> Show diff for the changes to be committed.  Port of
    !> log-edit.el:log-edit-show-diff.
    recursive subroutine log_edit_show_diff(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        k = local_index(.false.)
        if (k > 0) then
            if (associated(locals(k)%diff_function)) then
                call locals(k)%diff_function(status)
                return
            end if
        end if
        call set_error(status, FM_GENERIC_ERROR, "Diff functionality has not been set up")
    end subroutine log_edit_show_diff

    ! The current buffer's log-edit-diff-function, for
    ! `log_edit_maybe_show_diff' (in log_edit_vc_fm).
    recursive subroutine call_diff_function(status)
        type(status_type), intent(out) :: status
        call log_edit_show_diff(status)
    end subroutine call_diff_function

    !> Show the list of files to be committed.  Port of
    !> log-edit.el:log-edit-show-files.
    recursive subroutine log_edit_show_files(status)
        type(status_type), intent(out) :: status
        type(log_edit_string_type), allocatable :: files(:)
        type(cvs_string_type), allocatable :: strings(:)
        type(handle_type) :: buf, old, win
        type(display_alist_type) :: alist
        type(status_type) :: st
        integer :: k, saved_dedicated
        call log_edit_files(files)
        call get_buffer_create(LOG_EDIT_FILES_BUF, buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call log_edit_hide_buf(buf)
        buffers(cur)%read_only = .false.
        call erase_buffer(status)
        if (status%ok()) then
            allocate (strings(size(files)))
            do k = 1, size(files)
                strings(k)%s = files(k)%s
            end do
            call cvs_insert_strings(strings, status)
        end if
        if (status%ok()) call special_mode(status)
        call goto_char(point_min())
        if (status%ok()) then
            alist%inhibit_same_window = .true.
            alist%window_height => shrink_window
            ! (dedicated . t)
            saved_dedicated = display_buffer_mark_dedicated
            display_buffer_mark_dedicated = DEDICATED_STRONG
            call display_buffer(buf, win, status, [DISPLAY_ACTION_BELOW_SELECTED], alist)
            display_buffer_mark_dedicated = saved_dedicated
        end if
        call set_buffer(old, st)
    end subroutine log_edit_show_files

    ! (window-height . shrink-window-if-larger-than-buffer).
    subroutine shrink_window(window)
        type(handle_type), intent(in) :: window
        call shrink_window_if_larger_than_buffer(window)
    end subroutine shrink_window

    !> Return true if the buffer is "empty".  Port of
    !> log-edit.el:log-edit-empty-buffer-p.
    logical function log_edit_empty_buffer_p() result(empty)
        integer(int64) :: here
        type(status_type) :: st
        empty = point_min() == point_max()
        if (empty) return
        here = point()
        call goto_char(point_min())
        do while (looking_at("^\([a-zA-Z]+: ?\)?$", st))
            if (forward_line(1_int64) /= 0) exit
        end do
        empty = eobp()
        call goto_char(here)
    end function log_edit_empty_buffer_p

    ! Port of log-edit.el:log-edit--make-header-line: the line, with the
    ! extent of the header name (for its field property) in NAME_END.
    function make_header_line(header, value) result(line)
        character(len=*), intent(in) :: header
        character(len=*), intent(in), optional :: value
        character(len=:), allocatable :: line
        line = header//": "
        if (present(value)) line = line//value
        line = line//achar(10)
    end function make_header_line

    ! Insert the header line of HEADER with VALUE, making \[move_beginning_of_line]
    ! work like it does in other buffers with header names (the
    ! `field' property header and `rear_nonsticky' on the name).
    recursive subroutine insert_header_line(header, status, value)
        character(len=*), intent(in) :: header
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: value
        integer(int64) :: start
        start = point()
        call insert(make_header_line(header, value), status)
        if (.not. status%ok()) return
        call put_text_property(start, start + len(header) + 2, sym("field"), sym("header"), status)
        if (.not. status%ok()) return
        call put_text_property(start, start + len(header) + 2, sym("rear_nonsticky"), t_value(), status)
    end subroutine insert_header_line

    !> Insert the default VC commit log template with Summary and Author.
    !> INTERACTIVE is (called-interactively-p 'interactive).  Port of
    !> log-edit.el:log-edit-insert-message-template.
    recursive subroutine log_edit_insert_message_template(status, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        logical :: inter
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        if (.not. (inter .or. log_edit_empty_buffer_p())) return
        ! Put Author first because then the user can immediately yank in a
        ! multiline log message, or use \\`C-c C-w' (probably because they
        ! know it will generate exactly one line), without thereby pushing
        ! Author out of the header and into the log message body.
        ! (Also note that `log-edit-set-header' inserts all other headers
        ! before Summary.)
        if (log_edit_setup_add_author) then
            call insert_header_line("Author", status)
            if (.not. status%ok()) return
        end if
        call insert_header_line("Summary", status)
        if (.not. status%ok()) return
        call insert(achar(10), status)
        if (.not. status%ok()) return
        call end_of_line(-1_int64)
    end subroutine log_edit_insert_message_template

    ! log-edit-insert-message-template on `log_edit_hook'.
    recursive subroutine hook_insert_message_template(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_message_template(status)
    end subroutine hook_insert_message_template

    !> Insert the commit log template specified by the CVS administrator, if any.
    !> This simply uses the local CVS/Template file.  Port of
    !> log-edit.el:log-edit-insert-cvs-template.
    recursive subroutine log_edit_insert_cvs_template(status, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        character(len=:), allocatable :: template
        integer(int64) :: inserted
        logical :: inter
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        if (.not. (inter .or. log_edit_empty_buffer_p())) return
        ! Should the template take precedence over an empty Summary:,
        ! ie should we first erase the buffer?
        call expand_file_name("CVS/Template", template, status, buffers(cur)%directory)
        if (.not. status%ok()) return
        if (file_readable_p(template)) then
            call goto_char(point_max())
            call insert_file_contents(template, .false., inserted, status)
        end if
    end subroutine log_edit_insert_cvs_template

    ! log-edit-insert-cvs-template on `log_edit_hook'.
    recursive subroutine hook_insert_cvs_template(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_cvs_template(status)
    end subroutine hook_insert_cvs_template

    !> Insert the RCS commit log template from the CVS repository.
    !> This contacts the repository to get the rcstemplate file and
    !> can thus take some time.  Port of
    !> log-edit.el:log-edit-insert-cvs-rcstemplate.
    recursive subroutine log_edit_insert_cvs_rcstemplate(status, interactive)
        use lisp_fm, only: make_string, make_list, nil
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        character(len=:), allocatable :: root
        type(value_type) :: result, args(3)
        type(status_type) :: st
        logical :: inter
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        if (.not. (inter .or. log_edit_empty_buffer_p())) return
        call expand_file_name("CVS/Root", root, status, buffers(cur)%directory)
        if (.not. status%ok()) return
        if (file_readable_p(root)) then
            call goto_char(point_max())
            ! Ignore the stderr stuff, even if it's an error.
            args(1) = make_string("checkout")
            args(2) = make_string("-p")
            args(3) = make_string("CVSROOT/rcstemplate")
            call call_process("cvs", result, st, destination=t_value(), args=make_list(args))
        end if
    end subroutine log_edit_insert_cvs_rcstemplate

    !> Insert the list of files that are to be committed.  Port of
    !> log-edit.el:log-edit-insert-filenames.
    recursive subroutine log_edit_insert_filenames(status)
        type(status_type), intent(out) :: status
        type(log_edit_string_type), allocatable :: files(:)
        character(len=:), allocatable :: text
        integer :: k
        call log_edit_files(files)
        text = "Affected files:  "//achar(10)
        do k = 1, size(files)
            if (k > 1) text = text//"  "//achar(10)
            text = text//files(k)%s
        end do
        call insert(text, status)
    end subroutine log_edit_insert_filenames

    !> Insert the list of files that have no ChangeLog message.  Port of
    !> log-edit.el:log-edit-insert-filenames-without-changelog.
    recursive subroutine log_edit_insert_filenames_without_changelog(status)
        type(status_type), intent(out) :: status
        type(log_edit_string_type), allocatable :: files(:)
        type(changelog_entries_type) :: entries
        character(len=:), allocatable :: text
        integer :: k
        status = status_type()
        call log_edit_files(files)
        text = ""
        do k = 1, size(files)
            call log_edit_changelog_entries(files(k)%s, entries, status)
            if (.not. status%ok()) return
            if (entries%n > 0) cycle
            if (file_name_nondirectory(files(k)%s) == "ChangeLog") cycle
            if (len(text) > 0) text = text//", "
            text = text//files(k)%s
        end do
        if (len(text) > 0) then
            call goto_char(point_max())
            call insert(text//": ", status)
        end if
    end subroutine log_edit_insert_filenames_without_changelog

    ! Port of log-edit.el:log-edit-add-field.
    recursive subroutine log_edit_add_field(field, value, status)
        character(len=*), intent(in) :: field, value
        type(status_type), intent(out) :: status
        integer(int64) :: here
        logical :: found, nl
        call rfc822_goto_eoh(status)
        if (.not. status%ok()) return
        here = point()
        call re_search_backward("^"//field//":\([ "//achar(9)//"]*\)$", found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return
        if (found) then
            call goto_char(here)
            call replace_match(" "//value, status, fixedcase=.true., literal=.true., subexp=1)
        else
            call goto_char(here)
            nl = looking_at(achar(10), status)
            if (nl) then
                call insert(field//": "//value//achar(10), status)
            else
                call insert(field//": "//value//achar(10)//achar(10), status)
            end if
        end if
    end subroutine log_edit_add_field

    !> Insert a VC commit log message by looking at the ChangeLog.
    !> The idea is to write your ChangeLog entries first, and then use this
    !> command to commit your changes with that log.
    !>
    !> To select default log text, this command:
    !> - finds the ChangeLog entries for the files to be checked in;
    !> - verifies that the top entry in the ChangeLog is on the current date
    !>     and by the current user; if not, it doesn't provide any default text;
    !> - searches the ChangeLog entry for paragraphs containing the names of
    !>     the files to be checked in; and finally
    !> - uses those paragraphs as the log text.
    !>
    !> If the optional prefix arg USE_FIRST is given (via \[universal_argument]),
    !> or if the command is repeated, use the first log entry regardless of user
    !> name or time.  Port of log-edit.el:log-edit-insert-changelog.
    recursive subroutine log_edit_insert_changelog(status, use_first)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: use_first
        type(log_edit_string_type), allocatable :: files(:)
        character(len=:), allocatable :: fixes
        integer(int64) :: here, eoh, start, end, shortage, moved
        logical :: found, saved_use_first, use
        type(status_type) :: st
        status = status_type()
        here = point()
        call rfc822_goto_eoh(status)
        if (.not. status%ok()) return
        eoh = point()
        call goto_char(here)
        if (point() <= eoh) then
            call goto_char(eoh)
            if (looking_at(achar(10), status)) call forward_char(status, 1_int64)
        end if
        use = .false.
        if (present(use_first)) use = use_first
        if (value_eq(last_command, sym("log_edit_insert_changelog"))) use = .true.
        saved_use_first = changelog_use_first
        changelog_use_first = use
        call log_edit_files(files)
        call log_edit_insert_changelog_entries(files, status)
        changelog_use_first = saved_use_first
        if (status%ok()) call log_edit_set_common_indentation(status)
        ! Add an Author: field if appropriate.
        if (status%ok() .and. has_author) then
            call log_edit_add_field("Author", author_name, status)
            if (status%ok()) call log_edit_add_field("Summary", "", status)
        end if
        ! Add a Fixes: field if applicable.
        if (status%ok() .and. allocated(log_edit_rewrite_fixes_regexp) .and. &
            allocated(log_edit_rewrite_fixes_replacement)) then
            call rfc822_goto_eoh(status)
            if (status%ok()) call re_search_forward(log_edit_rewrite_fixes_regexp, found, status, noerror=SEARCH_NOERROR)
            if (status%ok() .and. found) then
                start = match_beginning(0, status)
                end = match_end(0, status)
                fixes = match_substitute_replacement(log_edit_rewrite_fixes_replacement, status)
                if (status%ok()) call delete_region(start, end, status)
                if (status%ok()) call log_edit_add_field("Fixes", fixes, status)
            end if
        end if
        if (status%ok() .and. log_edit_strip_single_file_name) then
            call rfc822_goto_eoh(status)
            if (looking_at(achar(10), st)) call forward_char(st, 1_int64)
            if (looking_at("\*\s-+", st)) then
                start = point()
                shortage = forward_line(1_int64)
                call re_search_forward("^\*\s-+", found, st, noerror=SEARCH_NOERROR)
                if (.not. found) then
                    call goto_char(start)
                    moved = skip_chars_forward("^():", st)
                    moved = skip_chars_forward(": ", st)
                    call delete_region(start, point(), status)
                end if
            end if
        end if
        ! FIXME also add "Co-authored-by" when appropriate.
        ! Bzr accepts multiple --author arguments, others (?) don't.
        if (status%ok() .and. log_edit_rewrite_tiny_change .and. has_author .and. author_tiny) then
            call goto_char(point_max())
            call insert(achar(10)//"Copyright-paperwork-exempt: yes"//achar(10), status)
        end if
        call goto_char(here)
    end subroutine log_edit_insert_changelog

    ! ------------------------------------------------------------------
    ! Functions for getting commit message from ChangeLog a file...
    ! Courtesy Jim Blandy
    ! ------------------------------------------------------------------

    !> Narrow to the top page of the current buffer, which visits a ChangeLog file.
    !> Actually, the narrowed region doesn't include the date line.
    !> A "page" in a ChangeLog file is the area between two dates.  Port of
    !> log-edit.el:log-edit-narrow-changelog.
    recursive subroutine log_edit_narrow_changelog(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, shortage
        status = status_type()
        if (.not. derived_mode_p_exact("change_log_mode")) then
            call set_error(status, FM_GENERIC_ERROR, "log-edit-narrow-changelog: Current buffer isn't a ChangeLog")
            return
        end if
        call goto_char(point_min())
        ! Skip date line and subsequent blank lines.
        shortage = forward_line(1_int64)
        if (looking_at("[ "//achar(9)//achar(10)//"]*"//achar(10), status)) call goto_char(match_end(0, status))
        start = point()
        call forward_page(status, 1_int64)
        if (.not. status%ok()) return
        call narrow_to_region(start, point(), status)
        call goto_char(point_min())
    contains
        ! (eq major-mode MODE).
        logical function derived_mode_p_exact(mode)
            character(len=*), intent(in) :: mode
            derived_mode_p_exact = .false.
            if (allocated(buffers(cur)%major_mode)) derived_mode_p_exact = buffers(cur)%major_mode == mode
        end function derived_mode_p_exact
    end subroutine log_edit_narrow_changelog

    !> Return the bounds of the ChangeLog paragraph containing point.
    !> If we are between paragraphs, return the previous paragraph.  Port of
    !> log-edit.el:log-edit-changelog-paragraph.
    subroutine log_edit_changelog_paragraph(beg, end)
        integer(int64), intent(out) :: beg, end
        type(status_type) :: st
        integer(int64) :: moved
        logical :: found
        call beginning_of_line()
        if (looking_at("^[ "//achar(9)//"]*$", st)) moved = skip_chars_backward_ws()
        call re_search_backward("^[ "//achar(9)//"]*"//achar(10), found, st, noerror=SEARCH_MOVE)
        if (found) call goto_char(match_end(0, st))
        beg = point()
        call re_search_forward("^[ "//achar(9)//achar(10)//"]*$", found, st, noerror=SEARCH_NOERROR)
        if (found) then
            end = match_beginning(0, st)
        else
            end = point_max()
        end if
    end subroutine log_edit_changelog_paragraph

    ! (skip-chars-backward " \t\n" (point-min)).
    integer(int64) function skip_chars_backward_ws() result(moved)
        use syntax_fm, only: skip_chars_backward
        type(status_type) :: st
        moved = skip_chars_backward(" "//achar(9)//achar(10), st, point_min())
    end function skip_chars_backward_ws

    !> Return the bounds of the ChangeLog subparagraph containing point.
    !> A subparagraph is a block of non-blank lines beginning with an asterisk.
    !> If we are between sub-paragraphs, return the previous subparagraph.
    !> Port of log-edit.el:log-edit-changelog-subparagraph.
    subroutine log_edit_changelog_subparagraph(beg, end)
        integer(int64), intent(out) :: beg, end
        type(status_type) :: st
        integer(int64) :: shortage
        logical :: found
        call end_of_line(1_int64)
        call search_backward("*", found, st, noerror=SEARCH_NOERROR)
        if (found) then
            call beginning_of_line()
            beg = point()
            shortage = forward_line(1_int64)
            call re_search_forward("^[ "//achar(9)//"]*["//achar(10)//"*]", found, st, noerror=SEARCH_NOERROR)
            if (found) then
                end = match_beginning(0, st)
            else
                end = point_max()
            end if
        else
            beg = point()
            end = point()
        end if
    end subroutine log_edit_changelog_subparagraph

    !> Return the bounds of the ChangeLog entry containing point.
    !> The variable `log_edit_changelog_full_paragraphs' determines whether an
    !> "entry" is a paragraph or a subparagraph; see its documentation string
    !> for more details.  Port of log-edit.el:log-edit-changelog-entry.
    subroutine log_edit_changelog_entry(beg, end)
        integer(int64), intent(out) :: beg, end
        integer(int64) :: here
        here = point()
        if (log_edit_changelog_full_paragraphs) then
            call log_edit_changelog_paragraph(beg, end)
        else
            call log_edit_changelog_subparagraph(beg, end)
        end if
        call goto_char(here)
    end subroutine log_edit_changelog_entry

    !> See if ChangeLog entry at point is for the current user, today.
    !> Return true if it is.  Port of log-edit.el:log-edit-changelog-ours-p.
    recursive logical function log_edit_changelog_ours_p() result(ours)
        character(len=:), allocatable :: name, mail, time, author
        type(status_type) :: st
        logical :: tiny
        ! Code adapted from add-change-log-entry.
        if (allocated(add_log_full_name)) then
            name = add_log_full_name
        else
            name = user_full_name()
        end if
        if (allocated(add_log_mailing_address)) then
            mail = add_log_mailing_address
        else
            mail = user_mail_address_value()
        end if
        time = add_log_time_format()
        if (.not. changelog_use_first) then
            ours = looking_at(regexp_quote(time//"  "//name//"  <"//mail//">"), st)
            return
        end if
        ! Check the author, to potentially add it as a "Author: " header.
        ! FIXME This accumulates multiple authors, but only when there
        ! are multiple ChangeLog files.  It should also check for
        ! multiple authors in each individual entry.
        ours = .false.
        if (.not. looking_at("[^ "//achar(9)//"]", st)) return
        if (author_bound) then
            if (.not. looking_at(".+  .+  <"//regexp_quote(mail)//">", st)) then
                if (looking_at(".+  \(.+  <.+>\) *\((tiny change)\)?", st)) then
                    author = replace_double_spaces(match_string(1))
                    tiny = match_beginning(2, st) > 0
                    if (.not. (has_author .and. index_in(author_name, author))) then
                        if (.not. has_author) then
                            has_author = .true.
                            author_name = author
                            author_tiny = tiny
                        else
                            author_name = author_name//", "//author
                            if (tiny .and. .not. author_tiny) author_tiny = .true.
                        end if
                    end if
                end if
            end if
        end if
        ours = .true.
    contains
        ! (string-replace "  " " " S).
        function replace_double_spaces(s) result(r)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: r
            integer :: i
            r = ""
            i = 1
            do while (i <= len(s))
                if (i < len(s)) then
                    if (s(i:i + 1) == "  ") then
                        r = r//" "
                        i = i + 2
                        cycle
                    end if
                end if
                r = r//s(i:i)
                i = i + 1
            end do
        end function replace_double_spaces

        ! (string-match (regexp-quote NEEDLE) HAYSTACK).
        logical function index_in(haystack, needle)
            character(len=*), intent(in) :: haystack, needle
            index_in = index(haystack, needle) > 0
        end function index_in
    end function log_edit_changelog_ours_p

    !> Return the ChangeLog entries for FILE, and the ChangeLog they came from.
    !> The return value looks like this:
    !>   (LOGBUFFER (ENTRYSTART ENTRYEND) ...)
    !> where LOGBUFFER is the name of the ChangeLog buffer, and each
    !> (ENTRYSTART . ENTRYEND) pair is a buffer region.  Port of
    !> log-edit.el:log-edit-changelog-entries.
    recursive subroutine log_edit_changelog_entries(file, entries, status)
        character(len=*), intent(in) :: file
        type(changelog_entries_type), intent(out) :: entries
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: changelog_file_name, saved_dir, expanded, pattern
        type(handle_type) :: visiting, old, buf
        type(status_type) :: st
        integer(int64) :: pos, eb, ee, saved_begv, saved_zv
        logical :: found
        status = status_type()
        allocate (entries%beg(0), entries%end(0))
        entries%buffer = null_handle()
        call expand_file_name(file, expanded, status, buffers(cur)%directory)
        if (.not. status%ok()) return
        visiting = find_buffer_visiting(file)
        ! If there is a buffer visiting FILE, and it has a local
        ! value for `change-log-default-name', use that.
        if (buffer_live_p(visiting)) then
            old = current_buffer()
            call set_buffer(visiting, st)
            if (change_log_default_name%local_p() .and. allocated(change_log_default_name%value)) &
                changelog_file_name = change_log_default_name%value
            call set_buffer(old, st)
        end if
        if (.not. allocated(changelog_file_name)) then
            ! `find-change-log' uses `change-log-default-name' if set
            ! and sets it before exiting, so we need to work around
            ! that memoizing which is undesired here.
            saved_dir = buffers(cur)%directory
            buffers(cur)%directory = file_name_directory(expanded)
            call change_log_default_name%clear()
            changelog_file_name = find_change_log()
            buffers(cur)%directory = saved_dir
        end if
        if (.not. (buffer_live_p(find_buffer_visiting(changelog_file_name)) .or. file_exists_p(changelog_file_name) &
                   .or. add_log_dont_create_changelog_file)) return
        call add_log_find_changelog_buffer(changelog_file_name, buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        entries%buffer = buf
        body: block
            if (buffers(cur)%major_mode /= "change_log_mode") then
                call change_log_mode(status)
                if (.not. status%ok()) exit body
            end if
            call goto_char(point_min())
            if (looking_at("\s-*"//achar(10), status)) call goto_char(match_end(0, status))
            if (.not. log_edit_changelog_ours_p()) exit body
            saved_begv = point_min()
            saved_zv = point_max()
            call log_edit_narrow_changelog(status)
            if (.not. status%ok()) exit body
            call goto_char(point_min())
            pattern = log_edit_changelog_pattern(file, changelog_file_name)
            pos = point()
            do while (.not. eobp())
                call re_search_forward(pattern, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. .not. found) exit
                call log_edit_changelog_entry(eb, ee)
                ! This is not relevant, actually.
                if (.not. (ee < max(pos + 1, point()))) then
                    entries%beg = [eb, entries%beg]
                    entries%end = [ee, entries%end]
                    entries%n = entries%n + 1
                end if
                ! Make sure we make progress.
                pos = max(pos + 1, ee)
                call goto_char(pos)
            end do
            call widen()
            call narrow_to_region(saved_begv, saved_zv, st)
        end block body
        call set_buffer(old, st)
    end subroutine log_edit_changelog_entries

    ! Port of log-edit.el:log-edit-changelog--pattern (in the ChangeLog
    ! buffer).
    function log_edit_changelog_pattern(file, changelog_file_name) result(pattern)
        character(len=*), intent(in) :: file, changelog_file_name
        character(len=:), allocatable :: pattern
        integer(int64) :: here
        logical :: found
        type(status_type) :: st
        if (file(len(file):len(file)) == "/") then
            ! Match any files inside this directory.
            pattern = "^"//achar(9)//"\* "
            if (file /= "./") pattern = pattern//file
            return
        end if
        ! Search for the name of FILE relative to the ChangeLog.  If that
        ! doesn't occur anywhere, they're not using full relative
        ! filenames in the ChangeLog, so just look for FILE; we'll accept
        ! some false positives.
        pattern = file_relative_name(file, file_name_directory(changelog_file_name))
        ! FIXME: When can the above return an empty string?
        found = .false.
        if (len(pattern) > 0) then
            here = point()
            call search_forward(pattern, found, st, noerror=SEARCH_NOERROR)
            call goto_char(here)
        end if
        if (.not. found) pattern = file_name_nondirectory(file)
        pattern = "\(^\|[^[:alnum:]]\)"//regexp_quote(pattern)//"\($\|[^[:alnum:]]\)"
    end function log_edit_changelog_pattern

    !> Insert the text from ChangeLog BUFFER between BEG and END.
    !> Rename relative filenames in the ChangeLog entry with FILES.
    !> FILES are supposed to name the same files whose relative filenames
    !> are to be replaced, and their names relative to the directory of
    !> BUFFER are expected to match the relative file names in the ChangeLog
    !> entry.  Port of log-edit.el:log-edit-changelog-insert-entries.
    recursive subroutine log_edit_changelog_insert_entries(buffer, beg, end, files, status)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: beg, end
        type(log_edit_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: log_name
        type(handle_type) :: bound
        integer(int64) :: opoint
        logical :: saved_case, found
        integer :: k
        opoint = point()
        if (buffer_live_p(buffer)) then
            if (allocated(buffers(buffer%index)%filename)) log_name = buffers(buffer%index)%filename
        end if
        saved_case = case_fold_search%value
        case_fold_search = .false.
        call insert_buffer_substring(buffer, status, beg, end)
        if (.not. status%ok()) then
            case_fold_search = saved_case
            return
        end if
        bound = marker_at(point())
        if (allocated(log_name)) then
            do k = 1, size(files)
                ! FIXME: f can be a directory, a (possibly indirect) parent
                ! of the ChangeLog file.
                call goto_char(opoint)
                call re_search_forward("\(^\|[ "//achar(9)//"]\)\("// &
                                       file_relative_name(files(k)%s, file_name_directory(log_name))// &
                                       "\)[, :"//achar(10)//"]", found, status, bound=mpos(bound), &
                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit
                if (found) call replace_match(files(k)%s, status, fixedcase=.true., literal=.true., subexp=2)
                if (.not. status%ok()) exit
            end do
        end if
        ! Eliminate tabs at the beginning of the line.
        if (status%ok()) then
            call goto_char(opoint)
            do
                call re_search_forward("^\("//achar(9)//"+\)", found, status, bound=mpos(bound), &
                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. .not. found) exit
                call replace_match("", status)
                if (.not. status%ok()) exit
            end do
        end if
        call goto_char(release(bound))
        case_fold_search = saved_case
    end subroutine log_edit_changelog_insert_entries

    !> Given a list of files FILES, insert the ChangeLog entries for them.
    !> Sets the author found (log-edit-author, the result).  Port of
    !> log-edit.el:log-edit-insert-changelog-entries.
    recursive subroutine log_edit_insert_changelog_entries(files, status)
        type(log_edit_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        ! A log entry: ((LOGBUFFER ENTRYSTART ENTRYEND) FILE1 FILE2...).
        type :: log_entry_type
            type(handle_type) :: buffer
            integer(int64) :: beg = 0, end = 0
            type(log_edit_string_type), allocatable :: files(:)
        end type log_entry_type
        type(log_entry_type), allocatable :: log_entries(:), grown(:)
        type(changelog_entries_type) :: entries
        logical :: saved_bound
        integer :: i, j, k, n, m
        status = status_type()
        allocate (log_entries(0))
        saved_bound = author_bound
        author_bound = .true.
        has_author = .false.
        author_tiny = .false.
        ! Note that any ChangeLog entry can apply to more than one file.
        ! Here we construct a log-entries list with elements of the form
        !   ((LOGBUFFER ENTRYSTART ENTRYEND) FILE1 FILE2...)
        do i = 1, size(files)
            call log_edit_changelog_entries(files(i)%s, entries, status)
            if (.not. status%ok()) exit
            do j = 1, entries%n
                k = 0
                do m = 1, size(log_entries)
                    if (log_entries(m)%buffer%index == entries%buffer%index .and. &
                        log_entries(m)%buffer%generation == entries%buffer%generation .and. &
                        log_entries(m)%beg == entries%beg(j) .and. log_entries(m)%end == entries%end(j)) k = m
                end do
                if (k > 0) then
                    n = size(log_entries(k)%files)
                    call append_file(log_entries(k)%files, files(i)%s)
                else
                    ! (push ...): the newest first, as reversed below.
                    n = size(log_entries)
                    allocate (grown(n + 1))
                    grown(1)%buffer = entries%buffer
                    grown(1)%beg = entries%beg(j)
                    grown(1)%end = entries%end(j)
                    allocate (grown(1)%files(0))
                    call append_file(grown(1)%files, files(i)%s)
                    do m = 1, n
                        grown(m + 1) = log_entries(m)
                    end do
                    call move_alloc(grown, log_entries)
                end if
            end do
        end do
        ! Now map over log-entries, and extract the strings.
        if (status%ok()) then
            do k = size(log_entries), 1, -1
                call log_edit_changelog_insert_entries(log_entries(k)%buffer, log_entries(k)%beg, log_entries(k)%end, &
                                                       log_entries(k)%files, status)
                if (.not. status%ok()) exit
                call insert(achar(10), status)
                if (.not. status%ok()) exit
            end do
        end if
        ! No newline after the last entry.
        if (status%ok() .and. size(log_entries) > 0) call delete_region(point() - 1, point(), status)
        author_bound = saved_bound
    contains
        subroutine append_file(list, f)
            type(log_edit_string_type), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: f
            type(log_edit_string_type), allocatable :: more(:)
            integer :: p
            allocate (more(size(list) + 1))
            do p = 1, size(list)
                more(p) = list(p)
            end do
            more(size(list) + 1)%s = f
            call move_alloc(more, list)
        end subroutine append_file
    end subroutine log_edit_insert_changelog_entries

    ! ------------------------------------------------------------------
    ! Headers
    ! ------------------------------------------------------------------

    !> Toggle a boolean-type header in the current buffer.
    !> See `log_edit_set_header' for details.  Port of
    !> log-edit.el:log-edit-toggle-header.
    recursive logical function log_edit_toggle_header(header, value, status) result(val)
        character(len=*), intent(in) :: header, value
        type(status_type), intent(out) :: status
        val = log_edit_set_header(header, value, status, toggle=.true.)
    end function log_edit_toggle_header

    !> Set the value of HEADER to VALUE in the current buffer.
    !> If TOGGLE is true, and the value of HEADER already is VALUE,
    !> clear it.  Make sure there is an empty line after the headers.
    !> Return true if toggled on (or TOGGLE is absent), otherwise false.
    !> Port of log-edit.el:log-edit-set-header.
    recursive logical function log_edit_set_header(header, value, status, toggle) result(val)
        character(len=*), intent(in) :: header, value
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: toggle
        type(handle_type) :: here, rbeg, rend
        type(status_type) :: st
        logical :: found, tog, narrowed
        tog = .false.
        if (present(toggle)) tog = toggle
        val = .true.
        found = .false.
        ! (save-excursion (save-restriction ...)).
        here = marker_at(point())
        narrowed = buffers(cur)%begv /= 1 .or. buffers(cur)%zv /= buffers(cur)%text%z
        rbeg = marker_at(buffers(cur)%begv)
        rend = marker_at(buffers(cur)%zv, .true.)
        call rfc822_goto_eoh(status)
        if (status%ok()) call narrow_to_region(point_min(), point(), status)
        if (status%ok()) then
            call goto_char(point_min())
            call re_search_forward("^"//header//":"//LOG_EDIT_HEADER_CONTENTS_REGEXP, found, status, &
                                   noerror=SEARCH_NOERROR)
        end if
        if (status%ok()) then
            if (found) then
                val = .not. (tog .and. match_string(1) == value)
                if (val) then
                    call replace_match(make_header_line(header, value), status, fixedcase=.true., literal=.true.)
                else
                    call replace_match("", status, fixedcase=.true., literal=.true., subexp=1)
                end if
            else
                call insert_header_line(header, status, value)
            end if
        end if
        call widen()
        if (narrowed) then
            call narrow_to_region(release(rbeg), release(rend), st)
        else
            call free_marker(rbeg, st)
            call free_marker(rend, st)
        end if
        if (status%ok()) call rfc822_goto_eoh(status)
        if (status%ok()) call delete_horizontal_space(.false., status)
        if (status%ok()) then
            if (.not. looking_at(achar(10), status)) call insert(achar(10), status)
        end if
        call goto_char(release(here))
    end function log_edit_set_header

    !> Extract headers from VC commit COMMENT to form command line arguments.
    !> HEADERS should be an alist with elements (HEADER . CMDARG)
    !> or (HEADER . FUNCTION) associating headers to command line
    !> options and the result is then a list of the form (MSG ARGUMENTS...)
    !> where MSG is the remaining text from COMMENT.
    !> FUNCTION should be a function of one argument that takes the
    !> header value and returns the list of strings to be appended to
    !> ARGUMENTS.  CMDARG will be added to ARGUMENTS followed by the
    !> header value.  If "Summary" is not in HEADERS, then the
    !> "Summary" header is extracted anyway and put back as the first
    !> line of MSG.  Port of log-edit.el:log-edit-extract-headers.
    recursive subroutine log_edit_extract_headers(headers, comment, msg, args, status)
        type(log_edit_header_type), intent(in) :: headers(:)
        character(len=*), intent(in) :: comment
        character(len=:), allocatable, intent(out) :: msg
        type(log_edit_string_type), allocatable, intent(out) :: args(:)
        type(status_type), intent(out) :: status
        type(log_edit_header_type), allocatable :: all(:)
        type(log_edit_string_type), allocatable :: res(:), more(:)
        character(len=:), allocatable :: summary, txt
        type(handle_type) :: old, temp
        type(status_type) :: st
        logical :: has_summary, found, saved_case
        integer :: k
        status = status_type()
        allocate (res(0))
        has_summary = .false.
        do k = 1, size(headers)
            if (headers(k)%name == "Summary") has_summary = .true.
        end do
        if (has_summary) then
            all = headers
        else
            allocate (all(size(headers) + 1))
            all(1)%name = "Summary"
            all(1)%kind = LOG_EDIT_HEADER_SUMMARY
            do k = 1, size(headers)
                all(k + 1) = headers(k)
            end do
        end if
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, status, inhibit_buffer_hooks=.true.)
        if (.not. status%ok()) return
        call set_buffer(temp, status)
        if (.not. status%ok()) return
        call erase_buffer(status)
        saved_case = case_fold_search%value
        body: block
            call insert(comment, status)
            if (.not. status%ok()) exit body
            call rfc822_goto_eoh(status)
            if (.not. status%ok()) exit body
            call narrow_to_region(point_min(), point(), status)
            case_fold_search = .true.
            do k = 1, size(all)
                call goto_char(point_min())
                do
                    call re_search_forward("^"//all(k)%name//":"//LOG_EDIT_HEADER_CONTENTS_REGEXP, found, status, &
                                           noerror=SEARCH_NOERROR)
                    if (.not. status%ok()) exit body
                    if (.not. found) exit
                    txt = match_string(1)
                    call replace_match("", status, fixedcase=.true., literal=.true.)
                    if (.not. status%ok()) exit body
                    select case (all(k)%kind)
                    case (LOG_EDIT_HEADER_SUMMARY)
                        summary = txt
                    case (LOG_EDIT_HEADER_FN)
                        call all(k)%fn(txt, more)
                        res = [res, more]
                        deallocate (more)
                    case default
                        allocate (more(2))
                        more(1)%s = cmdarg_of(all(k))
                        more(2)%s = txt
                        res = [res, more]
                        deallocate (more)
                    end select
                end do
            end do
            ! Remove header separator if the header is empty.
            call widen()
            call goto_char(point_min())
            if (looking_at("\([ "//achar(9)//"]*"//achar(10)//"\)+", status)) then
                call delete_region(match_beginning(0, status), match_end(0, status), status)
                if (.not. status%ok()) exit body
            end if
            if (allocated(summary)) then
                call insert(summary//achar(10)//achar(10), status)
                if (.not. status%ok()) exit body
            end if
            msg = buffer_string()
        end block body
        case_fold_search = saved_case
        call set_buffer(old, st)
        call kill_buffer(temp, st)
        if (.not. allocated(msg)) msg = ""
        call move_alloc(res, args)
    contains
        function cmdarg_of(h) result(s)
            type(log_edit_header_type), intent(in) :: h
            character(len=:), allocatable :: s
            if (allocated(h%cmdarg)) then
                s = h%cmdarg
            else
                s = h%name
            end if
        end function cmdarg_of
    end subroutine log_edit_extract_headers

    !> Toggle the Amend header; if toggled on, insert the message of the
    !> commit to amend (LAST_MSG_FN) and make its first line the Summary.
    !> Port of log-edit.el:log-edit--toggle-amend.
    recursive subroutine log_edit_toggle_amend(last_msg_fn, status)
        procedure(log_edit_last_msg_proc) :: last_msg_fn
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: msg, summary
        type(handle_type) :: here
        integer(int64) :: pt, moved, shortage
        logical :: on
        on = log_edit_toggle_header("Amend", "yes", status)
        if (.not. status%ok() .or. .not. on) return
        call goto_char(point_max())
        if (.not. bolp()) call insert(achar(10), status)
        if (.not. status%ok()) return
        call last_msg_fn(msg, status)
        if (.not. status%ok()) return
        call insert(msg, status)
        if (.not. status%ok()) return
        ! (save-excursion ...)
        here = marker_at(point())
        body: block
            call rfc822_goto_eoh(status)
            if (.not. status%ok()) exit body
            shortage = forward_line(1_int64)
            pt = point()
            if (forward_line(1_int64) /= 0) exit body
            if (.not. looking_at(achar(10)//"\|\'", status)) exit body
            if (bolp()) then
                call buffer_substring(pt, point() - 1, summary, status)
            else
                call buffer_substring(pt, point(), summary, status)
            end if
            if (.not. status%ok()) exit body
            moved = skip_chars_forward(" "//achar(10), status)
            call delete_region(pt, point(), status)
            if (.not. status%ok()) exit body
            on = log_edit_set_header("Summary", summary, status)
        end block body
        call goto_char(release(here))
    end subroutine log_edit_toggle_amend

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_log_edit_done(status)
        type(status_type), intent(out) :: status
        call log_edit_done(status)
    end subroutine cmd_log_edit_done

    recursive subroutine cmd_log_edit_insert_changelog(status)
        use lisp_fm, only: nilp
        type(status_type), intent(out) :: status
        call log_edit_insert_changelog(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_log_edit_insert_changelog

    recursive subroutine cmd_log_edit_show_diff(status)
        type(status_type), intent(out) :: status
        call log_edit_show_diff(status)
    end subroutine cmd_log_edit_show_diff

    recursive subroutine cmd_log_edit_show_files(status)
        type(status_type), intent(out) :: status
        call log_edit_show_files(status)
    end subroutine cmd_log_edit_show_files

    recursive subroutine cmd_log_edit_kill_buffer(status)
        type(status_type), intent(out) :: status
        call log_edit_kill_buffer(status)
    end subroutine cmd_log_edit_kill_buffer

    recursive subroutine cmd_log_edit_previous_comment(status)
        type(status_type), intent(out) :: status
        if (read_only(status)) return
        call log_edit_previous_comment(int(prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_log_edit_previous_comment

    recursive subroutine cmd_log_edit_next_comment(status)
        type(status_type), intent(out) :: status
        if (read_only(status)) return
        call log_edit_next_comment(int(prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_log_edit_next_comment

    ! The `*' of an interactive spec: signal buffer_read_only if the
    ! buffer is read-only.
    logical function read_only(status)
        use error_fm, only: FM_BUFFER_READ_ONLY
        type(status_type), intent(out) :: status
        status = status_type()
        read_only = buffers(cur)%read_only
        if (read_only) call set_error(status, FM_BUFFER_READ_ONLY, buffers(cur)%name)
    end function read_only

    ! The interactive argument of the comment searches.
    recursive subroutine read_comment_substring(str, status)
        character(len=:), allocatable, intent(out) :: str
        type(status_type), intent(out) :: status
        if (.not. allocated(last_comment_match)) last_comment_match = ""
        call read_string(format_prompt("Comment substring", last_comment_match), str, status, &
                         default=last_comment_match)
    end subroutine read_comment_substring

    recursive subroutine cmd_log_edit_comment_search_backward(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: str
        call read_comment_substring(str, status)
        if (status%ok()) call log_edit_comment_search_backward(str, status)
    end subroutine cmd_log_edit_comment_search_backward

    recursive subroutine cmd_log_edit_comment_search_forward(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: str
        call read_comment_substring(str, status)
        if (status%ok()) call log_edit_comment_search_forward(str, status)
    end subroutine cmd_log_edit_comment_search_forward

    recursive subroutine cmd_log_edit_mode_help(status)
        type(status_type), intent(out) :: status
        call log_edit_mode_help(status)
    end subroutine cmd_log_edit_mode_help

    recursive subroutine cmd_log_edit_insert_message_template(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_message_template(status, interactive=.true.)
    end subroutine cmd_log_edit_insert_message_template

    recursive subroutine cmd_log_edit_insert_cvs_template(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_cvs_template(status, interactive=.true.)
    end subroutine cmd_log_edit_insert_cvs_template

    recursive subroutine cmd_log_edit_insert_cvs_rcstemplate(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_cvs_rcstemplate(status, interactive=.true.)
    end subroutine cmd_log_edit_insert_cvs_rcstemplate

    recursive subroutine cmd_log_edit_insert_filenames(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_filenames(status)
    end subroutine cmd_log_edit_insert_filenames

    recursive subroutine cmd_log_edit_insert_filenames_without_changelog(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_filenames_without_changelog(status)
    end subroutine cmd_log_edit_insert_filenames_without_changelog

    recursive subroutine cmd_log_edit_diff_fileset(status)
        type(status_type), intent(out) :: status
        call log_edit_diff_fileset(status)
    end subroutine cmd_log_edit_diff_fileset

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of log-edit.el.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        f => log_edit_summary
        call f%define("log_edit_summary", "Face for the summary in `log_edit_mode' buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => log_edit_headers_separator
        call f%define("log_edit_headers_separator", "Face for the separator line in `log_edit_mode' buffers.")
        call f%spec_entry(display_when(), face_attrs(relative_height=0.1, inverse_video=.true., extend=.true.))
        f => log_edit_header
        call f%define("log_edit_header", "Face for the headers in `log_edit_mode' buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_keyword_face"))
        f => log_edit_unknown_header
        call f%define("log_edit_unknown_header", "Face for unknown headers in `log_edit_mode' buffers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_comment_face"))
    end subroutine define_faces

    !> Define Log Edit mode, its keymap, faces and hooks, and register the
    !> commands.
    subroutine syms_of_log_edit()
        type(status_type) :: st
        call define_faces()
        if (log_edit_comment_ring%size() == 0) call log_edit_comment_ring%make(LOG_EDIT_MAXIMUM_COMMENT_RING_SIZE)
        if (.not. allocated(last_comment_match)) last_comment_match = ""
        call major_modes%log_edit_mode%define("log_edit_mode", "Log-Edit", log_edit_mode, log_edit_mode_hook, &
                                              parent=major_modes%text_mode)
        log_edit_mode_map = major_modes%log_edit_mode%map
        commands%log_edit_mode%run => log_edit_mode
        commands%log_edit_done%run => cmd_log_edit_done
        commands%log_edit_insert_changelog%run => cmd_log_edit_insert_changelog
        commands%log_edit_show_diff%run => cmd_log_edit_show_diff
        commands%log_edit_show_files%run => cmd_log_edit_show_files
        commands%log_edit_kill_buffer%run => cmd_log_edit_kill_buffer
        commands%log_edit_next_comment%run => cmd_log_edit_next_comment
        commands%log_edit_previous_comment%run => cmd_log_edit_previous_comment
        commands%log_edit_comment_search_backward%run => cmd_log_edit_comment_search_backward
        commands%log_edit_comment_search_forward%run => cmd_log_edit_comment_search_forward
        commands%log_edit_mode_help%run => cmd_log_edit_mode_help
        commands%log_edit_insert_message_template%run => cmd_log_edit_insert_message_template
        commands%log_edit_insert_cvs_template%run => cmd_log_edit_insert_cvs_template
        commands%log_edit_insert_cvs_rcstemplate%run => cmd_log_edit_insert_cvs_rcstemplate
        commands%log_edit_insert_filenames%run => cmd_log_edit_insert_filenames
        commands%log_edit_insert_filenames_without_changelog%run => cmd_log_edit_insert_filenames_without_changelog
        commands%log_edit_diff_fileset%run => cmd_log_edit_diff_fileset
        call keymap_set(log_edit_mode_map, "C-c C-c", "log_edit_done", st)
        call keymap_set(log_edit_mode_map, "C-c C-a", "log_edit_insert_changelog", st)
        call keymap_set(log_edit_mode_map, "C-c C-d", "log_edit_show_diff", st)
        call keymap_set(log_edit_mode_map, "C-c C-f", "log_edit_show_files", st)
        call keymap_set(log_edit_mode_map, "C-c C-k", "log_edit_kill_buffer", st)
        call keymap_set(log_edit_mode_map, "M-n", "log_edit_next_comment", st)
        call keymap_set(log_edit_mode_map, "M-p", "log_edit_previous_comment", st)
        call keymap_set(log_edit_mode_map, "M-r", "log_edit_comment_search_backward", st)
        call keymap_set(log_edit_mode_map, "M-s", "log_edit_comment_search_forward", st)
        call keymap_set(log_edit_mode_map, "C-c ?", "log_edit_mode_help", st)
        if (log_edit_hook%empty()) then
            call log_edit_hook%add(log_edit_show_files_hook)
            call log_edit_hook%add(log_edit_insert_changelog_hook)
            call log_edit_hook%add(hook_insert_cvs_template)
            call log_edit_hook%add(hook_insert_message_template)
        end if
        call add_local_variables_killer(kill_log_edit_locals)
    end subroutine syms_of_log_edit

    ! log-edit-insert-changelog on `log_edit_hook'.
    recursive subroutine log_edit_insert_changelog_hook(status)
        type(status_type), intent(out) :: status
        call log_edit_insert_changelog(status)
    end subroutine log_edit_insert_changelog_hook

    ! log-edit-show-files on `log_edit_hook'.
    recursive subroutine log_edit_show_files_hook(status)
        type(status_type), intent(out) :: status
        call log_edit_show_files(status)
    end subroutine log_edit_show_files_hook
end module log_edit_fm
