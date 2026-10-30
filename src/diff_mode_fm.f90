! diff_mode_fm.f90 -- a mode for viewing/editing context diffs.
! Ported from GNU Emacs 31.1 lisp/vc/diff-mode.el (and the navigation
! of lisp/emacs-lisp/easy-mmode.el's easy-mmode-define-navigation).
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> Diff mode: unified, context and (to a lesser extent) normal diffs.
!> Moving by hunks and files (n, p, N, P, TAB), killing hunks and files
!> (k, K), splitting a unified hunk (C-c C-s), restricting the view
!> (C-c C-n), finding the source of a hunk and jumping there (RET, o,
!> C-c C-c), applying, testing and reverting hunks (C-c C-a, C-c C-t,
!> C-c M-u, C-c RET a), fixing up hunk headers, `next_error' through the
!> hunks, the faces and font-lock keywords of the diff text, and the
!> short keys of read-only Diff buffers (`diff_mode_read_only').  The
!> hunk text and source-location parts are in the submodule
!> diff_mode_apply_fm.
!>
!> Deferred: hunk refinement (`diff_refine', C-c C-b), syntax highlighting
!> and prettifying in hunks, converting between unified and context
!> diffs and reversing them (C-c C-u, C-c C-d, C-c C-r), keeping hunk
!> headers up to date on the fly (`diff_update_on_the_fly' and the
!> write-contents hook), `diff_minor_mode', Ediff (C-c C-e, A), the
!> add-log support (C-x 4 A), ignoring whitespace in and refreshing hunks
!> (C-c C-w, C-c C-l), `diff_kill_ring_save' (w), Whitespace mode's
!> settings (`diff_setup_whitespace'), outline and imenu support, the
!> menu, mouse bindings, `undo_ignore_read_only', VC revisions of the old
!> file, `diff_wiggle', `next_error_follow_minor_mode' (C-c C-f) and
!> `vc_next_action' (v).  `diff_read_only_map' is
!> `diff_mode_read_only_map' with `special_mode_map' as its parent
!> (upstream composes the two).
module diff_mode_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, fixnump, xfixnum
    use lread_fm, only: sym
    use data_fm, only: logical_setting, forward_setting, add_local_variables_killer
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer
    use data_fm, only: symbol_value, set
    use lisp_fm, only: t_value, make_keymap_ref
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, bolp, buffer_substring, &
                          line_beginning_position, line_end_position, narrow_to_region, widen, buffer_narrowed_p, &
                          char_after, delete_region
    use cmds_fm, only: forward_line, beginning_of_line
    use insdel_fm, only: insert
    use search_fm, only: looking_at, re_search_forward, re_search_backward, match_beginning, match_end, replace_match, &
                         SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use simple_fm, only: kill_region, special_mode_map, set_next_error_function, read_only_mode_hook, use_region_p
    use editfns_fm, only: region_beginning, region_end
    use fileio_fm, only: file_regular_p, file_exists_p, expand_file_name, file_name_directory, file_name_nondirectory
    use keymap_fm, only: make_sparse_keymap, set_keymap_parent, keymap_set, add_minor_mode_map, keymap_live_p, &
                         free_keymap
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                        FACE_BACKGROUND_DARK
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight
    use modes_fm, only: major_modes
    use easy_mmode_fm, only: easy_mmode_next, easy_mmode_prev
    implicit none(type, external)
    private

    public :: syms_of_diff_mode, diff_mode, diff_hunk_next, diff_hunk_prev, diff_file_next, diff_file_prev, &
              diff_beginning_of_hunk, diff_end_of_hunk, diff_beginning_of_file, diff_end_of_file, &
              diff_bounds_of_hunk, diff_bounds_of_file, diff_hunk_kill, diff_file_kill, diff_split_hunk, &
              diff_fixup_modifs, diff_restrict_view, diff_hunk_file_names, diff_find_file_name, &
              diff_setup_buffer_type, diff_hunk_text, diff_find_source_location, diff_apply_hunk, diff_test_hunk, &
              diff_goto_source, diff_apply_buffer, diff_revert_and_kill_hunk, diff_sanity_check_hunk, &
              diff_beginning_of_file_and_junk, diff_delete_other_hunks, diff_kill_junk, diff_next_error, &
              diff_merge_strings, set_diff_default_directory, diff_source_location_type, diff_text_type, &
              diff_buffer_type, diff_name_type, set_diff_vc_backend, diff_vc_backend

    !> If true, `diff_mode' buffers default to being read-only.  Port of
    !> diff-mode.el diff-default-read-only.
    logical, public :: diff_default_read_only = .false.

    !> True means `diff_goto_source' jumps to the old file.
    !> Else, it jumps to the new file.  Port of diff-mode.el
    !> diff-jump-to-old-file.
    logical, public :: diff_jump_to_old_file = .false.

    !> True means `diff_apply_hunk' will move to the next hunk after
    !> applying.  Port of diff-mode.el diff-advance-after-apply-hunk.
    logical, public :: diff_advance_after_apply_hunk = .true.

    !> If true, `diff_revert_and_kill_hunk' will ask for confirmation.
    !> Port of diff-mode.el diff-ask-before-revert-and-kill-hunk.
    logical, public :: diff_ask_before_revert_and_kill_hunk = .true.

    !> If true, empty lines are valid in unified diffs.
    !> Some versions of diff replace all-blank context lines in unified
    !> format with empty lines.  This makes the format less robust, but is
    !> tolerated.  Port of diff-mode.el diff-valid-unified-empty-line.
    logical, public :: diff_valid_unified_empty_line = .true.

    !> Run after setting up the `diff_mode' major mode.  Port of
    !> diff-mode.el diff-mode-hook.
    type(hook_type), target, public :: diff_mode_hook = hook_type(name="diff_mode_hook")

    !> True when read-only diff buffer uses short keys.  Port of
    !> diff-mode.el diff-mode-read-only (buffer-local).
    type(logical_setting), target, public :: diff_mode_read_only = logical_setting(name="diff_mode_read_only")

    !> Bindings for read-only `diff_mode' buffers.  See also
    !> `diff_mode_read_only_map'.  Port of diff-mode.el
    !> diff-mode-shared-map.
    type(handle_type), public :: diff_mode_shared_map

    !> Additional bindings for read-only `diff_mode' buffers.  Port of
    !> diff-mode.el diff-mode-read-only-map.
    type(handle_type), public :: diff_mode_read_only_map

    !> Keymap for `diff_mode'.  See also `diff_mode_shared_map'.  Port of
    !> diff-mode.el diff-mode-map.
    type(handle_type), public :: diff_mode_map

    !> Additional bindings for read-only `diff_mode' buffers.  Port of
    !> diff-mode.el diff-read-only-map.
    type(handle_type), public :: diff_read_only_map

    !> `diff_mode' face inherited by hunk and index header faces.
    type(named_face_type), target, public :: diff_header
    !> `diff_mode' face used to highlight file header lines.
    type(named_face_type), target, public :: diff_file_header
    !> `diff_mode' face used to highlight index header lines.
    type(named_face_type), target, public :: diff_index
    !> `diff_mode' face used to highlight hunk header lines.
    type(named_face_type), target, public :: diff_hunk_header
    !> `diff_mode' face used to highlight removed lines.
    type(named_face_type), target, public :: diff_removed
    !> `diff_mode' face used to highlight added lines.
    type(named_face_type), target, public :: diff_added
    !> `diff_mode' face used to highlight changed lines.
    type(named_face_type), target, public :: diff_changed_unspecified
    !> `diff_mode' face used to highlight changed lines.
    type(named_face_type), target, public :: diff_changed
    !> `diff_mode' face used to highlight indicator of removed lines (-, <).
    type(named_face_type), target, public :: diff_indicator_removed
    !> `diff_mode' face used to highlight indicator of added lines (+, >).
    type(named_face_type), target, public :: diff_indicator_added
    !> `diff_mode' face used to highlight indicator of changed lines.
    type(named_face_type), target, public :: diff_indicator_changed
    !> `diff_mode' face used to highlight function names produced by
    !> "diff -p".
    type(named_face_type), target, public :: diff_function
    !> `diff_mode' face used to highlight context and other
    !> side-information.
    type(named_face_type), target, public :: diff_context
    !> `diff_mode' face used to highlight nonexistent files in recursive
    !> diffs.
    type(named_face_type), target, public :: diff_nonexistent
    !> `diff_mode' face for error messages from diff.
    type(named_face_type), target, public :: diff_error

    !> Kinds of diff buffer (`diff_buffer_type'): a plain diff, Git's,
    !> Mercurial's.
    integer, parameter, public :: DIFF_BUFFER_PLAIN = 0, DIFF_BUFFER_GIT = 1, DIFF_BUFFER_HG = 2

    !> Hunk styles (diff-hunk-style).
    integer, parameter, public :: DIFF_STYLE_NONE = 0, DIFF_STYLE_UNIFIED = 1, DIFF_STYLE_CONTEXT = 2, &
                                  DIFF_STYLE_NORMAL = 3

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    !> Port of diff-mode.el diff-hunk-header-re-unified.
    character(len=*), parameter, public :: diff_hunk_header_re_unified = &
                                           "^@@ -\([0-9]+\)\(?:,\([0-9]+\)\)? \+\([0-9]+\)\(?:,\([0-9]+\)\)? @@"
    !> Port of diff-mode.el diff-context-mid-hunk-header-re.
    character(len=*), parameter, public :: diff_context_mid_hunk_header_re = &
                                           "--- \([0-9]+\)\(?:,\([0-9]+\)\)? ----$"
    !> Port of diff-mode.el diff-hunk-header-re.
    character(len=*), parameter, public :: diff_hunk_header_re = "^\(?:"//diff_hunk_header_re_unified// &
                                           ".*\|\*\{15\}.*"//LF//"\*\*\* .+ \*\*\*\*\|[0-9]+\(,[0-9]+\)?[acd][0-9]+"// &
                                           "\(,[0-9]+\)?\)$"
    !> Port of diff-mode.el diff-file-header-re.
    character(len=*), parameter, public :: diff_file_header_re = "^\(--- .+"//LF//"\+\+\+ \|\*\*\* .+"//LF// &
                                           "--- \|[^-+!<>0-9@* "//LF//"]\).+"//LF// &
                                           diff_hunk_header_re(2:)
    !> Port of diff-mode.el diff-separator-re.
    character(len=*), parameter :: diff_separator_re = "^--+ ?$"
    !> Port of diff-mode.el diff-file-junk-re.
    character(len=*), parameter :: diff_file_junk_re = "Index: \|Prereq: \|=\{20,\}\|"// &
                                   "diff \|index \|\(?:deleted file\|new\(?: file\)?\|old\) mode\|=== modified file"

    !> A string in an array.
    type, public :: diff_name_type
        character(len=:), allocatable :: s
    end type diff_name_type

    !> An entry of diff-remembered-files-alist: the file names a hunk
    !> mentions and the file to use for them.
    type :: remembered_type
        type(diff_name_type), allocatable :: names(:)
        character(len=:), allocatable :: file
    end type remembered_type

    !> A Diff buffer's own variables, by buffer serial number:
    !> diff-buffer-type, diff-default-directory, diff-remembered-defdir and
    !> diff-remembered-files-alist, diff-apply-hunk-to-backup-file,
    !> diff-narrowed-to (0 nil, 1 hunk, 2 file).
    type :: diff_locals_type
        integer(int64) :: serial = 0
        integer :: kind = DIFF_BUFFER_PLAIN
        character(len=:), allocatable :: default_directory, remembered_defdir
        type(remembered_type), allocatable :: remembered(:)
        logical :: apply_to_backup = .false.
        integer :: narrowed_to = 0
        !> `diff_vc_backend' (the name of the VC backend that produced
        !> the diff, unallocated for nil) and `diff_vc_revisions'.
        character(len=:), allocatable :: vc_backend, vc_rev1, vc_rev2
    end type diff_locals_type

    type(diff_locals_type), allocatable :: diff_buffers(:)

    !> A text of a hunk (`diff_hunk_text'): the text and the offset in it.
    type, public :: diff_text_type
        character(len=:), allocatable :: text
        integer(int64) :: offset = 0
    end type diff_text_type

    !> What `diff_find_source_location' finds: the source buffer, the
    !> line offset of the text in it (`found' false for nil), the bounds
    !> of the text, the variant found (SRC) and the other (DST), and
    !> whether they were switched (the patch is already applied).
    type, public :: diff_source_location_type
        type(handle_type) :: buf
        logical :: found = .false.
        integer(int64) :: line_offset = 0
        integer(int64) :: beg = 0, end = 0
        type(diff_text_type) :: src, dst
        logical :: switched = .false.
    end type diff_source_location_type

    interface
        !> The current buffer's entry, made if need be.  Not a port.
        module integer function entry_index() result(k)
            implicit none(type, external)
        end function entry_index

        !> (let ((inhibit-read-only t)) ...): bind it, keeping the old value
        !> in SAVED.  Not a port.
        module subroutine bind_inhibit_read_only(saved)
            implicit none(type, external)
            type(value_type), intent(out) :: saved
        end subroutine bind_inhibit_read_only

        !> The end of the binding of `bind_inhibit_read_only'.  Not a port.
        module subroutine restore_inhibit_read_only(saved)
            implicit none(type, external)
            type(value_type), intent(in) :: saved
        end subroutine restore_inhibit_read_only

        !> Whether the regexp RE matches at point (keeping the match data).
        !> Not a port.
        module logical function at(re)
            implicit none(type, external)
            character(len=*), intent(in) :: re
        end function at

        !> Search forward for RE (no error); 0 if not found.  Not a port.
        module integer(int64) function search_fwd(re, bound, count, move) result(found)
            implicit none(type, external)
            character(len=*), intent(in) :: re
            integer(int64), intent(in), optional :: bound, count
            logical, intent(in), optional :: move
        end function search_fwd

        !> Search backward for RE (no error); 0 if not found.  Not a port.
        module integer(int64) function search_bwd(re, bound) result(found)
            implicit none(type, external)
            character(len=*), intent(in) :: re
            integer(int64), intent(in), optional :: bound
        end function search_bwd

        !> The beginning and end of match group N (0 if it did not match).
        !> Not a port.
        module integer(int64) function mb(n)
            implicit none(type, external)
            integer, intent(in) :: n
        end function mb

        ! Not a port.
        module integer(int64) function me(n)
            implicit none(type, external)
            integer, intent(in) :: n
        end function me

        !> The character after point (-1 at the end).  Not a port.
        module integer(int32) function char_at_point() result(c)
            implicit none(type, external)
        end function char_at_point

        !> The value of the decimal number S (0 if none: string-to-number).
        !> Not a port.
        module integer(int64) function string_to_number(s) result(n)
            implicit none(type, external)
            character(len=*), intent(in) :: s
        end function string_to_number

        !> N as a decimal string.  Not a port.
        module function number_to_string(n) result(s)
            implicit none(type, external)
            integer(int64), intent(in) :: n
            character(len=:), allocatable :: s
        end function number_to_string

        !> Return the literal source text from HUNK as (TEXT . OFFSET).
        !> If DESTP is false, TEXT is the source, otherwise the destination
        !> text.  CHAR_OFFSET is a char-offset in HUNK (negative for nil),
        !> and OFFSET is the corresponding char-offset in TEXT.  Port of
        !> diff-mode.el:diff-hunk-text.  (In diff_mode_apply_fm.)
        module subroutine diff_hunk_text(hunk, destp, char_offset, res, status)
            implicit none(type, external)
            character(len=*), intent(in) :: hunk
            logical, intent(in) :: destp
            integer(int64), intent(in) :: char_offset
            type(diff_text_type), intent(out) :: res
            type(status_type), intent(out) :: status
        end subroutine diff_hunk_text

        !> Find current diff location within the source file.  Port of
        !> diff-mode.el:diff-find-source-location.  (In diff_mode_apply_fm.)
        module subroutine diff_find_source_location(loc, status, other_file, reverse, noprompt, other_arg)
            implicit none(type, external)
            type(diff_source_location_type), intent(out) :: loc
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: other_file, reverse, noprompt
            integer(int64), intent(in), optional :: other_arg
        end subroutine diff_find_source_location

        !> Apply the current hunk to its source file and go to the next
        !> hunk.  Port of diff-mode.el:diff-apply-hunk.  (In
        !> diff_mode_apply_fm.)
        module subroutine diff_apply_hunk(status, reverse, beg, end)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: reverse
            integer(int64), intent(in), optional :: beg, end
        end subroutine diff_apply_hunk

        !> See whether it's possible to apply the current hunk.  With
        !> REVERSE, try to reverse the hunk.  Port of
        !> diff-mode.el:diff-test-hunk.  (In diff_mode_apply_fm.)
        module subroutine diff_test_hunk(status, reverse)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: reverse
        end subroutine diff_test_hunk

        !> Jump to the corresponding source line.  Port of
        !> diff-mode.el:diff-goto-source.  (In diff_mode_apply_fm.)
        module subroutine diff_goto_source(status, other_file, other_arg)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: other_file
            integer(int64), intent(in), optional :: other_arg
        end subroutine diff_goto_source

        !> Apply the diff in the entire diff buffer (or the hunks from
        !> BEG to END): FAILURES gets the number of failed hunks.  Port of
        !> diff-mode.el:diff-apply-buffer.  (In diff_mode_apply_fm.)
        module subroutine diff_apply_buffer(failures, status, beg, end, reverse, test, no_save)
            implicit none(type, external)
            integer, intent(out) :: failures
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: beg, end
            logical, intent(in), optional :: reverse, test, no_save
        end subroutine diff_apply_buffer

        !> Check the hunk at point, offering to fix it.  Port of
        !> diff-mode.el:diff-sanity-check-hunk.  (In diff_mode_apply_fm.)
        module subroutine diff_sanity_check_hunk(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine diff_sanity_check_hunk
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer state
    ! ------------------------------------------------------------------

    !> kill-all-local-variables: forget the current buffer's entry.
    !> Not a port.
    subroutine kill_diff_locals()
        logical, allocatable :: keep(:)
        integer :: k

        if (.not. allocated(diff_buffers) .or. cur == 0) return
        allocate (keep(size(diff_buffers)))

        do k = 1, size(diff_buffers)
            keep(k) = diff_buffers(k)%serial /= buffers(cur)%serial
        end do

        diff_buffers = pack(diff_buffers, keep)
    end subroutine kill_diff_locals

    !> The current buffer's diff-buffer-type.  Not a port.
    integer function buffer_kind()
        buffer_kind = diff_buffers(entry_index())%kind
    end function buffer_kind

    !> Set the current buffer's diff-default-directory.  Not a port.
    subroutine set_diff_default_directory(dir)
        character(len=*), intent(in) :: dir

        diff_buffers(entry_index())%default_directory = dir
    end subroutine set_diff_default_directory

    ! ------------------------------------------------------------------
    ! Hunks and files
    ! ------------------------------------------------------------------

    !> The style of the hunk whose header is at point (moving past the
    !> header), else STYLE.  Port of diff-mode.el:diff-hunk-style.
    integer function diff_hunk_style(style) result(s)
        integer, intent(in) :: style
        integer(int32) :: c

        s = style

        if (at(diff_hunk_header_re)) then
            c = char_at_point()

            if (c == iachar("@")) then
                s = DIFF_STYLE_UNIFIED
            else if (c == iachar("*")) then
                s = DIFF_STYLE_CONTEXT
            else
                s = DIFF_STYLE_NONE
            end if

            call goto_char(me(0))
        end if
    end function diff_hunk_style

    !> Return previous line if it has patch separator as produced by git.
    !> Port of diff-mode.el:diff-prev-line-if-patch-separator.
    integer(int64) function diff_prev_line_if_patch_separator() result(pos)
        integer(int64) :: old_point, shortage

        pos = point()
        if (buffer_kind() /= DIFF_BUFFER_GIT) return
        old_point = point()
        shortage = forward_line(-1_int64)

        if (at("^-- $")) then
            pos = point()
        else
            pos = old_point
        end if

        call goto_char(old_point)
    end function diff_prev_line_if_patch_separator

    !> Advance to the end of the current hunk, and return its position.
    !> Port of diff-mode.el:diff-end-of-hunk.
    recursive integer(int64) function diff_end_of_hunk(style, donttrustheader) result(end)
        integer, intent(in), optional :: style
        logical, intent(in), optional :: donttrustheader
        integer(int64) :: nold, nnew, endold, endnew, saved, found
        integer :: sty
        logical :: trust
        character(len=:), allocatable :: re

        sty = DIFF_STYLE_NONE
        if (present(style)) sty = style
        trust = .true.
        if (present(donttrustheader)) trust = .not. donttrustheader
        end = 0

        if (at(diff_hunk_header_re)) then
            sty = diff_hunk_style(sty)
            ! (diff-hunk-style left the match data of the header.)
            call goto_char(me(0))

            if (trust .and. me(2) > 0) then
                nold = 1
                nnew = 1
                if (len(match_string(2)) > 0) nold = string_to_number(match_string(2))
                if (len(match_string(4)) > 0) nnew = string_to_number(match_string(4))
                saved = point()

                if (diff_valid_unified_empty_line) then
                    found = search_fwd("^[- "//LF//"]", count=nold)
                else
                    found = search_fwd("^[- ]", count=nold)
                end if

                if (at(".*"//LF//"\\")) then
                    endold = line_beginning_position(3_int64)
                else
                    endold = line_beginning_position(2_int64)
                end if

                call goto_char(saved)

                if (diff_valid_unified_empty_line) then
                    found = search_fwd("^[+ "//LF//"]", count=nnew)
                else
                    found = search_fwd("^[+ ]", count=nnew)
                end if

                if (at(".*"//LF//"\\")) then
                    endnew = line_beginning_position(3_int64)
                else
                    endnew = line_beginning_position(2_int64)
                end if

                call goto_char(saved)
                end = max(endold, endnew)
            end if
        end if

        if (end == 0) then
            select case (sty)
            case (DIFF_STYLE_UNIFIED)
                if (diff_valid_unified_empty_line) then
                    re = "^[^-+# \\"//LF//"]\|"//diff_file_header_re
                else
                    re = "^[^-+# \\]\|"//diff_file_header_re
                end if
            case (DIFF_STYLE_CONTEXT)
                if (diff_valid_unified_empty_line) then
                    re = "^[^-+#! "//LF//"\\]"
                else
                    re = "^[^-+#! \\]"
                end if
            case (DIFF_STYLE_NORMAL)
                re = "^[^<>#\\]"
            case default
                re = "^[^-+#!<> \\]"
            end select

            found = search_fwd(re)
            if (found /= 0) end = mb(0)

            if (diff_valid_unified_empty_line) then
                if (end /= 0) then
                    call goto_char(end)
                else
                    call goto_char(point_max())
                end if

                do while (point() - 2 >= point_min())
                    if (char_before_pos(point() - 1) /= 10) exit
                    call goto_char(point() - 1)
                    end = point()
                end do
            end if

            if (end /= 0) call goto_char(end)
            end = diff_prev_line_if_patch_separator()
        end if

        if (end == 0) end = point_max()
        call goto_char(end)

    contains

        integer(int32) function char_before_pos(pos) result(c)
            integer(int64), intent(in) :: pos
            logical :: found

            call char_after(c, found, pos - 1)
            if (.not. found) c = -1
        end function char_before_pos
    end function diff_end_of_hunk

    !> Return true if point is inside a diff header: the position of its
    !> start, else 0.  Port of diff-mode.el:diff--at-diff-header-p.
    integer(int64) function diff_at_diff_header_p() result(pos)
        integer(int64) :: orig, shortage, found

        pos = 0
        orig = point()
        call beginning_of_line()

        if (at(diff_hunk_header_re)) then
            ! Hunk header.
            pos = point()
        else
            shortage = forward_line(-1_int64)
            found = search_fwd(diff_file_header_re, bound=line_end_position(4_int64))

            if (found /= 0) then
                ! File header.
                call beginning_of_line()
                pos = point()
            else
                call goto_char(orig)
                call beginning_of_line()

                if (at(diff_file_junk_re)) then
                    ! Git diff junk.
                    do while (at(diff_file_junk_re) .and. .not. bobp())
                        shortage = forward_line(-1_int64)
                    end do

                    found = search_fwd(diff_file_header_re)
                    call beginning_of_line()
                    pos = point()
                end if
            end if
        end if

        call goto_char(orig)
    end function diff_at_diff_header_p

    !> Move back to the previous hunk beginning, and return its position.
    !> If point is in a file header rather than a hunk, advance to the
    !> next hunk if TRY_HARDER is true; otherwise signal an error.
    !> Port of diff-mode.el:diff-beginning-of-hunk.
    recursive integer(int64) function diff_beginning_of_hunk(status, try_harder) result(pos)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: try_harder
        integer(int64) :: header
        logical :: harder

        status = status_type()
        harder = .false.
        if (present(try_harder)) harder = try_harder
        call beginning_of_line()
        pos = point()
        ! At hunk header.
        if (at(diff_hunk_header_re)) return
        header = diff_at_diff_header_p()

        if (header /= 0) then
            ! At junk diff header.
            if (harder) then
                call goto_char(header)
                pos = point()
                return
            end if

            call set_error(status, FM_GENERIC_ERROR, "Can't find the beginning of the hunk")
            return
        end if

        ! In the middle of a hunk.
        pos = search_bwd(diff_hunk_header_re)
        if (pos /= 0) return

        ! At first hunk header.
        if (search_fwd(diff_hunk_header_re) /= 0) then
            call beginning_of_line()
            pos = point()
            return
        end if

        call set_error(status, FM_GENERIC_ERROR, "Can't find the beginning of the hunk")
    end function diff_beginning_of_hunk

    !> Whether the hunk at point is a unified one.  Port of
    !> diff-mode.el:diff-unified-hunk-p.
    logical function diff_unified_hunk_p()
        integer(int64) :: saved, pos
        type(status_type) :: st

        saved = point()
        pos = diff_beginning_of_hunk(st)
        diff_unified_hunk_p = st%ok() .and. at("^@@")
        call goto_char(saved)
    end function diff_unified_hunk_p

    !> Move to the beginning of the file header of point's file.  Port of
    !> diff-mode.el:diff-beginning-of-file.
    recursive subroutine diff_beginning_of_file(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, res, shortage

        status = status_type()
        call beginning_of_line()
        if (at(diff_file_header_re)) return
        start = point()
        shortage = forward_line(3_int64)
        res = search_bwd(diff_file_header_re)

        if (res /= 0) then
            if (point() <= start) return
            res = search_bwd(diff_file_header_re)
            if (res /= 0) return
        end if

        call goto_char(start)
        call set_error(status, FM_GENERIC_ERROR, "Can't find the beginning of the file")
    end subroutine diff_beginning_of_file

    !> Move to the end of point's file.  Port of
    !> diff-mode.el:diff-end-of-file.
    recursive integer(int64) function diff_end_of_file() result(pos)
        integer(int64) :: found

        found = search_fwd("^[-+#!<>0-9@* \\]")
        found = search_fwd("^[^-+#!<>0-9@* \\]\|"//diff_file_header_re, move=.true.)

        if (found /= 0 .and. mb(1) > 0) then
            call goto_char(mb(1))
        else
            call beginning_of_line()
        end if

        pos = point()
    end function diff_end_of_file

    !> diff-end-of-hunk as the ENDFUN of the hunk navigation.  Not a port.
    recursive integer(int64) function end_of_hunk_fun() result(pos)
        pos = diff_end_of_hunk()
    end function end_of_hunk_fun

    !> diff-restrict-view as the NARROWFUN of the hunk navigation.
    !> Not a port.
    recursive subroutine restrict_view_fun(status)
        type(status_type), intent(out) :: status

        call diff_restrict_view(status)
    end subroutine restrict_view_fun

    !> Go to the next COUNT'th hunk.
    !> Interactively, COUNT is the prefix numeric argument, and defaults
    !> to 1.  Port of diff-mode.el:diff-hunk-next (from
    !> easy-mmode-define-navigation).
    recursive subroutine diff_hunk_next(status, count, interactive)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(in), optional :: interactive
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(diff_hunk_header_re, "hunk", n, status, end_of_hunk_fun, restrict_view_fun, &
                             interactive)
    end subroutine diff_hunk_next

    !> Go to the previous COUNT'th hunk.
    !> Interactively, COUNT is the prefix numeric argument, and defaults
    !> to 1.  Port of diff-mode.el:diff-hunk-prev (from
    !> easy-mmode-define-navigation).
    recursive subroutine diff_hunk_prev(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_prev(diff_hunk_header_re, "hunk", n, status, end_of_hunk_fun, restrict_view_fun)
    end subroutine diff_hunk_prev

    !> diff-end-of-file as the ENDFUN of the file navigation.  Not a port.
    recursive integer(int64) function end_of_file_fun() result(pos)
        pos = diff_end_of_file()
    end function end_of_file_fun

    !> Go to the next COUNT'th file.
    !> Interactively, COUNT is the prefix numeric argument, and defaults
    !> to 1.  Port of diff-mode.el:diff-file-next (from
    !> easy-mmode-define-navigation).
    recursive subroutine diff_file_next(status, count, interactive)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        logical, intent(in), optional :: interactive
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_next(diff_file_header_re, "file", n, status, end_of_file_fun, interactive=interactive)
    end subroutine diff_file_next

    !> Go to the previous COUNT'th file.
    !> Interactively, COUNT is the prefix numeric argument, and defaults
    !> to 1.  Port of diff-mode.el:diff-file-prev (from
    !> easy-mmode-define-navigation).
    recursive subroutine diff_file_prev(status, count)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n

        n = 1
        if (present(count)) n = count
        call easy_mmode_prev(diff_file_header_re, "file", n, status, end_of_file_fun)
    end subroutine diff_file_prev

    !> Return the bounds of the diff hunk at point.
    !> The return value is (BEG END), which are the hunk's start and end
    !> positions.  Signal an error if no hunk is found.  If point is in a
    !> file header, return the bounds of the next hunk.  Port of
    !> diff-mode.el:diff-bounds-of-hunk.
    recursive subroutine diff_bounds_of_hunk(beg, end, status)
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        pos = point()
        beg = diff_beginning_of_hunk(status, .true.)

        if (.not. status%ok()) then
            call goto_char(pos)
            return
        end if

        end = diff_end_of_hunk()

        if (end < pos) then
            if (search_fwd(diff_hunk_header_re) /= 0) then
                beg = mb(0)
                end = diff_end_of_hunk()
            else
                call set_error(status, FM_GENERIC_ERROR, "No hunk found")
            end if
        end if

        call goto_char(pos)
    end subroutine diff_bounds_of_hunk

    !> Return the bounds of the file segment at point.
    !> The return value is (BEG END), which are the segment's start and
    !> end positions.  Port of diff-mode.el:diff-bounds-of-file.
    recursive subroutine diff_bounds_of_file(beg, end, status)
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        pos = point()
        call diff_beginning_of_file_and_junk(status)

        if (.not. status%ok()) then
            call goto_char(pos)
            return
        end if

        beg = point()
        end = diff_end_of_file()

        do while (at("^"//LF))
            call goto_char(point() + 1)
        end do

        end = point()
        call goto_char(pos)
        if (pos > end) call set_error(status, FM_GENERIC_ERROR, "Not inside a file diff")
    end subroutine diff_bounds_of_file

    !> Restrict the view to the current hunk.
    !> If FILE is true (the prefix ARG is given), restrict the view to the
    !> current file instead.  Port of diff-mode.el:diff-restrict-view.
    recursive subroutine diff_restrict_view(status, file)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: file
        integer(int64) :: beg, end
        logical :: whole_file

        whole_file = .false.
        if (present(file)) whole_file = file

        if (whole_file) then
            call diff_bounds_of_file(beg, end, status)
        else
            call diff_bounds_of_hunk(beg, end, status)
        end if

        if (.not. status%ok()) return
        call narrow_to_region(beg, end, status)

        if (whole_file) then
            diff_buffers(entry_index())%narrowed_to = 2
        else
            diff_buffers(entry_index())%narrowed_to = 1
        end if
    end subroutine diff_restrict_view

    !> Whether the buffer has hunks.  Port of
    !> diff-mode.el:diff--some-hunks-p.
    logical function diff_some_hunks_p()
        integer(int64) :: saved

        saved = point()
        call goto_char(point_min())
        diff_some_hunks_p = search_fwd(diff_hunk_header_re) /= 0
        call goto_char(saved)
    end function diff_some_hunks_p

    !> Kill the hunk at point.
    !> When killing the last hunk left for a file, kill the file header
    !> too.  Interactively, if the region is active, kill all hunks that
    !> the region overlaps.
    !>
    !> When called from programs with BEG and END, kill all hunks
    !> overlapped by the region from BEG to END as though called
    !> interactively with an active region delimited by BEG and END.
    !> Port of diff-mode.el:diff-hunk-kill.
    recursive subroutine diff_hunk_kill(status, beg, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end

        if (present(beg) .neqv. present(end)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid call to `diff_hunk_kill'")
            return
        end if

        call diff_revert_kill_hunks(status, .false., beg, end)
    end subroutine diff_hunk_kill

    !> Delete hunks other than the current one.
    !> Interactively, if the region is active, delete all hunks that the
    !> region overlaps; otherwise delete all hunks except the current one.
    !> Port of diff-mode.el:diff-delete-other-hunks.
    recursive subroutine diff_delete_other_hunks(status, beg, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        integer(int64) :: b, e, saved, x
        type(value_type) :: saved_inhibit

        status = status_type()

        if (buffer_narrowed_p()) then
            call set_error(status, FM_USER_ERROR, "Command is not safe in a narrowed buffer")
            return
        end if

        if (present(beg) .neqv. present(end)) then
            call set_error(status, FM_GENERIC_ERROR, "Require exactly zero or two arguments")
            return
        end if

        saved = point()
        call bind_inhibit_read_only(saved_inhibit)

        if (present(beg)) then
            call goto_char(beg)
            call diff_bounds_of_hunk(b, x, status)

            if (status%ok()) then
                call goto_char(end)
                call diff_bounds_of_hunk(x, e, status)
            end if
        else
            call diff_bounds_of_hunk(b, e, status)
        end if

        if (status%ok()) then
            call delete_region(e, point_max(), status)
            call goto_char(b)
            call diff_beginning_of_file(status)
            if (status%ok()) call diff_hunk_next(status)
            if (status%ok()) call delete_region(point(), b, status)
            if (status%ok()) call diff_beginning_of_file_and_junk(status)
            if (status%ok()) call delete_region(point_min(), point(), status)
        end if

        call restore_inhibit_read_only(saved_inhibit)
        call goto_char(min(saved, point_max()))
    end subroutine diff_delete_other_hunks

    !> Go to the beginning of file-related diff-info.
    !> This is like `diff_beginning_of_file' except it tries to skip back
    !> over leading data such as "Index: ..." and such.  Port of
    !> diff-mode.el:diff-beginning-of-file-and-junk.
    recursive subroutine diff_beginning_of_file_and_junk(status)
        type(status_type), intent(out) :: status
        integer(int64) :: orig, start, prevfile, nextfile, prevhunk, previndex, shortage, saved
        type(status_type) :: err, st
        logical :: have_prev

        status = status_type()
        orig = point()
        call beginning_of_line()

        do while (at(diff_file_junk_re))
            shortage = forward_line(1_int64)
            if (eobp()) exit
        end do

        start = point()
        call diff_beginning_of_file(err)
        have_prev = err%ok()
        prevfile = point()
        call goto_char(start)
        nextfile = 0
        call diff_file_next(st)
        if (st%ok()) nextfile = point()
        call goto_char(start)
        prevhunk = 0
        if (have_prev) call goto_char(prevfile)
        call diff_hunk_prev(st)
        if (st%ok()) prevhunk = point()
        call goto_char(start)
        saved = point()
        ! In case we're looking at "Index:".
        shortage = forward_line(1_int64)
        previndex = 0

        if (prevhunk > 0) then
            previndex = search_bwd("^Index: ", bound=prevhunk)
        else
            previndex = search_bwd("^Index: ")
        end if

        call goto_char(saved)

        if (nextfile > 0) then
            if (.not. have_prev) then
                prevfile = nextfile
                have_prev = .true.
            else if (previndex > 0 .and. previndex > prevfile) then
                prevfile = nextfile
            end if
        end if

        if (previndex > 0 .and. have_prev .and. previndex < prevfile) prevfile = previndex

        if (have_prev .and. prevfile <= start) then
            call goto_char(prevfile)

            do
                saved = point()

                if (forward_line(-1_int64) /= 0) then
                    call goto_char(saved)
                    exit
                end if

                if (.not. at(diff_file_junk_re)) then
                    call goto_char(saved)
                    exit
                end if
            end do
        else
            call goto_char(orig)
            status = err
            if (status%ok()) call set_error(status, FM_GENERIC_ERROR, "Can't find the beginning of the file")
        end if
    end subroutine diff_beginning_of_file_and_junk

    !> Kill current file's hunks.
    !> When called from programs with DELETE true, delete them, instead.
    !> Port of diff-mode.el:diff-file-kill.
    recursive subroutine diff_file_kill(status, delete)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delete
        integer(int64) :: beg, end, pos
        type(value_type) :: saved
        type(status_type) :: st

        if (.not. diff_some_hunks_p()) then
            call set_error(status, FM_GENERIC_ERROR, "No hunks")
            return
        end if

        pos = diff_beginning_of_hunk(status, .true.)
        if (.not. status%ok()) return
        call diff_bounds_of_file(beg, end, status)
        if (.not. status%ok()) return
        call bind_inhibit_read_only(saved)

        if (present(delete)) then
            if (delete) then
                call delete_region(beg, end, status)
            else
                call kill_region(beg, end, status)
            end if
        else
            call kill_region(beg, end, status)
        end if

        call restore_inhibit_read_only(saved)
        pos = diff_beginning_of_hunk(st, .true.)
    end subroutine diff_file_kill

    !> Kill spurious empty diffs.  Port of diff-mode.el:diff-kill-junk.
    recursive subroutine diff_kill_junk(status)
        type(status_type), intent(out) :: status
        integer(int64) :: saved, b, e
        type(value_type) :: saved_inhibit

        status = status_type()
        saved = point()
        call bind_inhibit_read_only(saved_inhibit)
        call goto_char(point_min())

        do while (search_fwd("^\(Index: .*"//LF//"\)\([^-+!* <>].*"//LF//"\)*?\(\(Index:\) \|"// &
                             diff_file_header_re//"\)") /= 0)
            if (me(4) > 0) then
                b = mb(0)
            else
                b = me(1)
            end if

            e = mb(3)
            call delete_region(b, e, status)
            call beginning_of_line()
        end do

        call restore_inhibit_read_only(saved_inhibit)
        call goto_char(min(saved, point_max()))
    end subroutine diff_kill_junk

    !> The number of matches of RE from START to END.  Port of
    !> diff-mode.el:diff-count-matches.
    integer(int64) function diff_count_matches(re, start, end) result(n)
        character(len=*), intent(in) :: re
        integer(int64), intent(in) :: start, end
        integer(int64) :: saved

        saved = point()
        n = 0
        call goto_char(start)

        do while (search_fwd(re, bound=end) /= 0)
            n = n + 1
        end do

        call goto_char(saved)
    end function diff_count_matches

    !> Split the current (unified diff) hunk at point into two hunks.
    !> Port of diff-mode.el:diff-split-hunk.
    recursive subroutine diff_split_hunk(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pos, start, start1, start2, newstart1, newstart2, shortage
        type(value_type) :: saved

        call beginning_of_line()
        pos = point()
        start = diff_beginning_of_hunk(status)
        if (.not. status%ok()) return

        if (.not. at(diff_hunk_header_re_unified)) then
            call set_error(status, FM_GENERIC_ERROR, "diff_split_hunk only works on unified context diffs")
            return
        end if

        start1 = string_to_number(match_string(1))
        start2 = string_to_number(match_string(3))
        shortage = forward_line(1_int64)
        newstart1 = start1 + diff_count_matches("^[- "//TAB//"]", point(), pos)
        newstart2 = start2 + diff_count_matches("^[+ "//TAB//"]", point(), pos)
        call bind_inhibit_read_only(saved)
        call goto_char(pos)
        call insert("@@ -"//number_to_string(newstart1)//",1 +"//number_to_string(newstart2)//",1 @@"//LF, status)
        if (status%ok()) call diff_fixup_modifs(start, pos, status)
        call restore_inhibit_read_only(saved)
    end subroutine diff_split_hunk

    !> Fixup the hunk headers (in case the buffer was modified).
    !> START and END are either taken from the region (if a prefix arg is
    !> given) or else cover the whole buffer.  Port of
    !> diff-mode.el:diff-fixup-modifs.
    recursive subroutine diff_fixup_modifs(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: plus, minus, space, bang, saved, x
        integer(int32) :: c
        type(value_type) :: saved_inhibit
        character(len=:), allocatable :: old1, old2, new1, new2, new, old

        status = status_type()
        saved = point()
        call bind_inhibit_read_only(saved_inhibit)
        call goto_char(end)
        x = diff_end_of_hunk(donttrustheader=.true.)
        plus = 0
        minus = 0
        space = 0
        bang = 0

        do while (forward_line(-1_int64) == 0)
            if (point() < start) exit

            if (.not. at(diff_hunk_header_re_unified//"\|[-*][-*][-*] [0-9,]+ [-*][-*][-*][-*]$"// &
                         "\|--- .+"//LF//"\+\+\+ ")) then
                c = char_at_point()

                select case (c)
                case (32)
                    space = space + 1
                case (43)
                    plus = plus + 1
                case (45)
                    ! In git format-patch "^-- $" signifies the end of the
                    ! patch.
                    if (.not. (buffer_kind() == DIFF_BUFFER_GIT .and. at("^-- $"))) minus = minus + 1
                case (33)
                    bang = bang + 1
                case (92, 35)
                    continue
                case (10)
                    if (diff_valid_unified_empty_line) then
                        space = space + 1
                    else
                        call reset()
                    end if
                case default
                    call reset()
                end select
            else
                if (at(diff_hunk_header_re_unified)) then
                    old1 = match_string(2)
                    old2 = match_string(4)
                    new1 = number_to_string(space + minus)
                    new2 = number_to_string(space + plus)

                    if (me(4) > 0) then
                        if (new2 /= old2 .or. len(new2) /= len(old2)) then
                            call replace_match(new2, status, fixedcase=.true., literal=.true., subexp=4)
                        end if
                    else
                        call goto_char(me(3))
                        call insert(","//new2, status)
                    end if

                    ! (The header line again: the match data moved.)
                    call beginning_of_line()

                    if (at(diff_hunk_header_re_unified)) then
                        if (me(2) > 0) then
                            if (new1 /= old1 .or. len(new1) /= len(old1)) then
                                call replace_match(new1, status, fixedcase=.true., literal=.true., subexp=2)
                            end if
                        else
                            call goto_char(me(1))
                            call insert(","//new1, status)
                        end if
                    end if
                else if (at(diff_context_mid_hunk_header_re)) then
                    if (space + bang + plus > 0) then
                        old1 = match_string(1)
                        old2 = match_string(2)
                        new = number_to_string(space + bang + plus - 1 + string_to_number(old1))

                        if (new /= old2 .or. len(new) /= len(old2)) then
                            call replace_match(new, status, fixedcase=.true., literal=.true., subexp=2)
                        end if
                    end if
                else if (at("\*\*\* \([0-9]+\),\(-?[0-9]*\) \*\*\*\*$")) then
                    if (space + bang + minus > 0) then
                        old = match_string(1)
                        new = zero_padded(space + bang + minus - 1 + string_to_number(old), len(old))

                        if (new /= old .or. len(new) /= len(old)) then
                            call replace_match(new, status, fixedcase=.true., literal=.true., subexp=2)
                        end if
                    end if
                end if

                call reset()
            end if
        end do

        call restore_inhibit_read_only(saved_inhibit)
        call goto_char(min(saved, point_max()))

    contains

        subroutine reset()
            space = 0
            plus = 0
            minus = 0
            bang = 0
        end subroutine reset

        !> N in at least WIDTH digits, zero-padded (format "%0Nd").
        function zero_padded(n, width) result(s)
            integer(int64), intent(in) :: n
            integer, intent(in) :: width
            character(len=:), allocatable :: s

            s = number_to_string(n)
            if (len(s) < width .and. n >= 0) s = repeat("0", width - len(s))//s
        end function zero_padded
    end subroutine diff_fixup_modifs

    !> Workhorse routine for killing hunks, after possibly reverting them.
    !> If BEG and END are absent, kill the hunk at point.
    !> Otherwise kill all hunks overlapped by region delimited by BEG and
    !> END.  When killing a hunk that's the only one remaining for its
    !> file, kill the file header too.  If REVERTP is true, reverse-apply
    !> hunks before killing them.  Port of
    !> diff-mode.el:diff--revert-kill-hunks.
    recursive subroutine diff_revert_kill_hunks(status, revertp, beg, end)
        type(status_type), intent(out) :: status
        logical, intent(in) :: revertp
        integer(int64), intent(in), optional :: beg, end
        integer(int64) :: b, e, x, pos, hunk_beg, hunk_end, file_beg, file_end
        integer :: failures
        logical :: file_bounds, whole_file
        type(value_type) :: saved
        type(status_type) :: st

        if (.not. diff_some_hunks_p()) then
            call set_error(status, FM_GENERIC_ERROR, "No hunks")
            return
        end if

        status = status_type()

        if (present(beg)) then
            pos = point()
            call goto_char(beg)
            call diff_bounds_of_hunk(b, x, status)
            if (.not. status%ok()) return
            e = end
            call goto_char(end)

            if (.not. at(diff_hunk_header_re)) then
                call diff_bounds_of_hunk(x, e, status)
                if (.not. status%ok()) return
            end if

            call goto_char(pos)
        else
            call diff_bounds_of_hunk(b, e, status)
            if (.not. status%ok()) return
        end if

        if (revertp) then
            call diff_apply_buffer(failures, status, b, e, reverse=.true.)
            if (.not. status%ok()) return
            if (failures /= 0) return
        end if

        call goto_char(e)
        pos = diff_at_diff_header_p()
        if (pos /= 0) call goto_char(pos)
        e = point()
        call bind_inhibit_read_only(saved)
        call goto_char(b)

        do
            call diff_bounds_of_hunk(hunk_beg, hunk_end, status)
            if (.not. status%ok()) exit
            call diff_bounds_of_file(file_beg, file_end, st)
            file_bounds = st%ok()
            whole_file = .false.

            if (file_bounds) then
                call goto_char(file_beg)
                call diff_hunk_next(st)

                if (point() == hunk_beg) then
                    call goto_char(hunk_end)

                    do while (at("^"//LF))
                        call goto_char(point() + 1)
                    end do

                    whole_file = point() == file_end
                end if
            end if

            if (whole_file) then
                call kill_region(file_beg, file_end, status)
                call goto_char(file_beg)
                ! (The text from FILE_BEG to FILE_END went.)
                if (e > file_beg) e = max(file_beg, e - (file_end - file_beg))
            else
                call kill_region(hunk_beg, hunk_end, status)
                call goto_char(hunk_beg)
                if (e > hunk_beg) e = max(hunk_beg, e - (hunk_end - hunk_beg))
            end if

            if (.not. status%ok()) exit
            x = diff_beginning_of_hunk(st, .true.)
            if (point() < b .or. point() == e) exit
        end do

        call restore_inhibit_read_only(saved)
    end subroutine diff_revert_kill_hunks

    !> Reverse-apply and then kill the hunk at point.  Save changed buffer.
    !> Interactively, if the region is active, reverse-apply and kill all
    !> hunks that the region overlaps.
    !>
    !> This command is useful in buffers generated by \[vc_diff] and
    !> \[vc_root_diff], especially when preparing to commit the patch with
    !> \[vc_next_action].  You can use \[diff_hunk_kill] to temporarily
    !> remove changes that you intend to include in a separate commit or
    !> commits, and you can use this command to permanently drop changes
    !> you didn't intend, or no longer want.
    !>
    !> This is a destructive operation, so by default, this command asks
    !> you to confirm you really want to reverse-apply and kill the hunk.
    !> You can customize `diff_ask_before_revert_and_kill_hunk' to control
    !> that.  Port of diff-mode.el:diff-revert-and-kill-hunk.
    recursive subroutine diff_revert_and_kill_hunk(status, beg, end)
        use subr_fm, only: y_or_n_p
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        logical :: yes

        status = status_type()

        if (present(beg) .neqv. present(end)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid call to `diff_revert_and_kill_hunk'")
            return
        end if

        yes = .true.

        if (diff_ask_before_revert_and_kill_hunk) then
            call y_or_n_p("Really reverse-apply and kill hunk(s)?", yes, status)
            if (.not. status%ok()) return
        end if

        if (yes) call diff_revert_kill_hunks(status, .true., beg, end)
    end subroutine diff_revert_and_kill_hunk

    ! ------------------------------------------------------------------
    ! File names
    ! ------------------------------------------------------------------

    !> FILE without its first directory (nil, unallocated, if it has
    !> none).  Port of diff-mode.el:diff-filename-drop-dir.
    subroutine diff_filename_drop_dir(file, rest)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rest
        integer :: k

        k = scan(file, "/\")
        if (k > 0) rest = file(k + 1:)
    end subroutine diff_filename_drop_dir

    !> Merge the diff between ANCESTOR and FROM into TO.
    !> Returns the merged string if successful (MERGED true) or nil
    !> otherwise.  The strings are assumed not to contain any "\n" (i.e.
    !> end of line).  If ANCESTOR = FROM, returns TO.  If ANCESTOR = TO,
    !> returns FROM.  The heuristic is simplistic and only really works for
    !> cases like (diff_merge_strings "b/foo" "b/bar" "/a/c/foo").  Port of
    !> diff-mode.el:diff-merge-strings.
    subroutine diff_merge_strings(ancestor, from, to, result, merged)
        use search_fm, only: string_match
        use fns_fm, only: substring
        character(len=*), intent(in) :: ancestor, from, to
        character(len=:), allocatable, intent(out) :: result
        logical, intent(out) :: merged
        character(len=:), allocatable :: str
        integer(int64) :: idx, b5, e5, b6, e6
        type(status_type) :: st

        merged = .false.
        str = ancestor//LF//from//LF//to
        idx = string_match("\`\(.*?\)\(.*\)\(.*\)"//LF//"\1\(.*\)\3"//LF//"\(.*\(\2\).*\)\'", str, st)
        if (idx == 0 .or. .not. st%ok()) return
        if (match_string(5, str) /= to .or. len(match_string(5, str)) /= len(to)) return
        b5 = match_beginning(5, st)
        e5 = match_end(5, st)
        b6 = match_beginning(6, st)
        e6 = match_end(6, st)
        result = substring(str, b5 - 1, b6 - 1)//match_string(4, str)//substring(str, e6 - 1, e5 - 1)
        merged = .true.
    end subroutine diff_merge_strings

    !> Give the list of file names textually mentioned for the current
    !> hunk.  Port of diff-mode.el:diff-hunk-file-names.
    recursive subroutine diff_hunk_file_names(names, old)
        type(diff_name_type), allocatable, intent(out) :: names(:)
        logical, intent(in), optional :: old
        integer(int64) :: orig, saved, limit, found, shortage
        type(status_type) :: st
        type(diff_name_type), allocatable :: header_files(:)
        logical :: o
        integer :: k

        o = .false.
        if (present(old)) o = old
        allocate (names(0))
        orig = point()

        if (.not. at(diff_file_header_re)) then
            call diff_beginning_of_file(st)
            if (.not. st%ok()) found = search_fwd(diff_file_header_re)
        end if

        limit = point()
        call diff_hunk_prev(st)

        if (st%ok()) then
            limit = point()
        else
            limit = point_min()
        end if

        call goto_char(orig)

        if (.not. at(diff_file_header_re)) then
            call diff_beginning_of_file(st)
            if (.not. st%ok()) found = search_fwd(diff_file_header_re)
        end if

        allocate (header_files(0))

        if (at("[-*][-*][-*] \([^"//TAB//LF//"]+\).*"//LF//"[-+][-+][-+] \([^"//TAB//LF//"]+\)")) then
            if (o) then
                call push(header_files, match_string(1))
                call push(header_files, match_string(2))
            else
                call push(header_files, match_string(2))
                call push(header_files, match_string(1))
            end if
        else
            shortage = forward_line(1_int64)
        end if

        if (.not. o) then
            saved = point()
            if (search_bwd("^Index: \(.+\)", bound=limit) /= 0) call push(names, match_string(1))
            call goto_char(saved)
        end if

        do k = 1, size(header_files)
            call push(names, header_files(k)%s)
        end do

        if (search_bwd("^diff ") /= 0) then
            if (at("^diff \(-[^ "//TAB//LF//"L]+ +\)*\(-L +\S-+ +\)*\(\S-+\)\( +\(\S-+\)\)?")) then
                if (o) then
                    if (mb(3) > 0) call push(names, match_string(3))
                    if (mb(4) > 0) call push(names, match_string(4))
                else
                    if (mb(5) > 0) call push(names, match_string(5))
                    if (mb(3) > 0) call push(names, match_string(3))
                end if
            end if
        end if

        call goto_char(orig)

    contains

        subroutine push(list, s)
            type(diff_name_type), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: s
            type(diff_name_type), allocatable :: grown(:)
            integer :: n

            n = size(list)
            allocate (grown(n + 1))
            grown(1:n) = list
            grown(n + 1)%s = s
            call move_alloc(grown, list)
        end subroutine push
    end subroutine diff_hunk_file_names

    !> Whether the lists of names A and B are `equal'.  Not a port.
    logical function same_names(a, b)
        type(diff_name_type), intent(in) :: a(:), b(:)
        integer :: k

        same_names = size(a) == size(b)
        if (.not. same_names) return

        do k = 1, size(a)
            if (a(k)%s /= b(k)%s .or. len(a(k)%s) /= len(b(k)%s)) then
                same_names = .false.
                return
            end if
        end do
    end function same_names

    !> Return the file corresponding to the current patch (FOUND false
    !> for none).  OLD true means that we want the old file.  NOPROMPT
    !> true means to prefer returning nil than to prompt the user.
    !> Port of diff-mode.el:diff-find-file-name (no cvs-pcl-cvs).
    recursive subroutine diff_find_file_name(file, found, status, old, noprompt)
        use minibuffer_fm, only: read_file_name, REQUIRE_MATCH_NIL, REQUIRE_MATCH_T
        character(len=:), allocatable, intent(out) :: file
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: old, noprompt
        type(diff_name_type), allocatable :: fs(:), files(:), other(:)
        character(len=:), allocatable :: dir, newfile, rest, prompt_file, answer
        integer(int64) :: saved, savedb, savedz, x
        integer :: k, j, ent
        logical :: o, quiet, ok, creation
        type(status_type) :: st

        status = status_type()
        found = .false.
        o = .false.
        if (present(old)) o = old
        quiet = .false.
        if (present(noprompt)) quiet = noprompt
        dir = ""
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        ent = entry_index()
        ok = .false.
        if (allocated(diff_buffers(ent)%remembered_defdir)) ok = diff_buffers(ent)%remembered_defdir == dir

        if (.not. ok) then
            diff_buffers(ent)%remembered_defdir = dir
            if (allocated(diff_buffers(ent)%remembered)) deallocate (diff_buffers(ent)%remembered)
        end if

        if (.not. allocated(diff_buffers(ent)%remembered)) allocate (diff_buffers(ent)%remembered(0))
        saved = point()
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%zv
        call widen()

        if (.not. at(diff_file_header_re)) then
            call diff_beginning_of_file(st)
            if (.not. st%ok()) x = search_fwd(diff_file_header_re)
        end if

        call diff_hunk_file_names(fs, o)

        if (buffer_kind() == DIFF_BUFFER_GIT .or. buffer_kind() == DIFF_BUFFER_HG) then
            do k = 1, size(fs)
                if (len(fs(k)%s) >= 2) then
                    if (index("icoawib", fs(k)%s(1:1)) > 0 .and. fs(k)%s(2:2) == "/") fs(k)%s = fs(k)%s(3:)
                end if
            end do
        end if

        ! (cdr (assoc fs diff-remembered-files-alist))
        do k = 1, size(diff_buffers(ent)%remembered)
            if (same_names(diff_buffers(ent)%remembered(k)%names, fs)) then
                file = diff_buffers(ent)%remembered(k)%file
                found = .true.
                call done()
                return
            end if
        end do

        do k = 1, size(diff_buffers(ent)%remembered)
            associate (rf => diff_buffers(ent)%remembered(k))
                if (size(rf%names) == 0 .or. size(fs) == 0) cycle
                call diff_merge_strings(rf%names(1)%s, fs(1)%s, rf%file, newfile, ok)

                if (ok) then
                    if (file_exists_p(newfile)) then
                        file = newfile
                        found = .true.
                        call done()
                        return
                    end if
                end if
            end associate
        end do

        ! Look for each file name, dropping a directory each time.
        files = fs

        do while (size(files) > 0)
            do k = 1, size(files)
                if (file_regular_p(files(k)%s)) then
                    file = files(k)%s
                    found = .true.
                    call done()
                    return
                end if
            end do

            if (buffer_kind() == DIFF_BUFFER_GIT .or. buffer_kind() == DIFF_BUFFER_HG) then
                if (.not. o .and. size(files) >= 2) then
                    if (files(2)%s == "/dev/null") exit
                end if
            end if

            allocate (other(size(files)))
            j = 0

            do k = 1, size(files)
                call diff_filename_drop_dir(files(k)%s, rest)

                if (allocated(rest)) then
                    j = j + 1
                    call move_alloc(rest, other(j)%s)
                end if
            end do

            files = other(1:j)
            deallocate (other)
        end do

        ! A .rej file's original.
        if (allocated(buffers(cur)%filename)) then
            j = len(buffers(cur)%filename)

            if (j > 4) then
                if (buffers(cur)%filename(j - 3:) == ".rej") then
                    if (file_exists_p(buffers(cur)%filename(:j - 4))) then
                        file = buffers(cur)%filename(:j - 4)
                        found = .true.
                        call done()
                        return
                    end if
                end if
            end if
        end if

        if (.not. quiet) then
            prompt_file = ""
            if (size(fs) > 0) prompt_file = fs(1)%s
            call diff_hunk_file_names(other,.not. o)
            creation = .false.
            if (size(other) > 0) creation = other(1)%s == "/dev/null"
            call expand_file_name(prompt_file, newfile, st)
            call done()
            call read_file_name("Use file "//newfile//": ", answer, status, dir=file_name_directory(newfile), &
                                default_filename=newfile, mustmatch=merge(REQUIRE_MATCH_NIL, REQUIRE_MATCH_T, creation), &
                                initial=file_name_nondirectory(newfile))
            if (.not. status%ok()) return
            if (.not. creation .or. file_exists_p(answer)) call remember(entry_index(), fs, answer)
            file = answer
            found = .true.
            return
        end if

        call done()

    contains

        !> Push (FS . FILE) onto entry K's diff-remembered-files-alist.
        subroutine remember(k, names, file)
            integer, intent(in) :: k
            type(diff_name_type), intent(in) :: names(:)
            character(len=*), intent(in) :: file
            type(remembered_type), allocatable :: grown(:)
            integer :: n

            n = size(diff_buffers(k)%remembered)
            allocate (grown(n + 1))
            grown(2:) = diff_buffers(k)%remembered
            grown(1)%names = names
            grown(1)%file = file
            call move_alloc(grown, diff_buffers(k)%remembered)
        end subroutine remember

        !> Restore point and the narrowing.
        subroutine done()
            type(status_type) :: st2

            call narrow_to_region(savedb, savedz, st2)
            call goto_char(saved)
        end subroutine done
    end subroutine diff_find_file_name

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Try to guess the `diff_buffer_type' from content of current Diff
    !> mode buffer.  (`outline_regexp' is deferred.)  Port of
    !> diff-mode.el:diff-setup-buffer-type.
    subroutine diff_setup_buffer_type()
        integer(int64) :: saved
        integer :: k

        saved = point()
        call goto_char(point_min())
        k = entry_index()

        if (search_fwd("^diff --git") /= 0) then
            diff_buffers(k)%kind = DIFF_BUFFER_GIT
        else if (search_fwd("^diff -r ") /= 0) then
            diff_buffers(k)%kind = DIFF_BUFFER_HG
        else
            diff_buffers(k)%kind = DIFF_BUFFER_PLAIN
        end if

        call goto_char(saved)
    end subroutine diff_setup_buffer_type

    !> Set the current buffer's `diff_vc_backend' to BACKEND (the VC
    !> backend name; absent for nil) and `diff_vc_revisions' to (REV1
    !> REV2).  Port of the setq-local of diff-mode.el diff-vc-backend and
    !> diff-vc-revisions.
    subroutine set_diff_vc_backend(backend, rev1, rev2)
        character(len=*), intent(in), optional :: backend, rev1, rev2
        integer :: k

        k = entry_index()
        if (allocated(diff_buffers(k)%vc_backend)) deallocate (diff_buffers(k)%vc_backend)
        if (allocated(diff_buffers(k)%vc_rev1)) deallocate (diff_buffers(k)%vc_rev1)
        if (allocated(diff_buffers(k)%vc_rev2)) deallocate (diff_buffers(k)%vc_rev2)
        if (present(backend)) diff_buffers(k)%vc_backend = backend
        if (present(rev1)) diff_buffers(k)%vc_rev1 = rev1
        if (present(rev2)) diff_buffers(k)%vc_rev2 = rev2
    end subroutine set_diff_vc_backend

    !> The current buffer's `diff_vc_backend' ("" for nil).  Not a port.
    function diff_vc_backend() result(name)
        character(len=:), allocatable :: name
        integer :: k

        name = ""
        k = entry_index()
        if (allocated(diff_buffers(k)%vc_backend)) name = diff_buffers(k)%vc_backend
    end function diff_vc_backend

    !> The current buffer's `diff_buffer_type': DIFF_BUFFER_PLAIN (nil),
    !> DIFF_BUFFER_GIT or DIFF_BUFFER_HG.  Port of the buffer-local
    !> diff-mode.el:diff-buffer-type.
    integer function diff_buffer_type() result(kind)
        integer :: k

        kind = DIFF_BUFFER_PLAIN
        k = entry_index()
        if (k > 0) kind = diff_buffers(k)%kind
    end function diff_buffer_type

    !> The `next_error_function' of Diff buffers: go to the source of the
    !> N'th next hunk (from the start with RESET).  Port of
    !> diff-mode.el:diff-next-error.
    recursive subroutine diff_next_error(n, reset, status)
        use window_fm, only: pop_to_buffer
        integer(int64), intent(in) :: n
        logical, intent(in) :: reset
        type(status_type), intent(out) :: status

        call pop_to_buffer(current_buffer(), status)
        if (.not. status%ok()) return
        if (reset) call goto_char(point_min())
        call diff_hunk_next(status, n)
        if (.not. status%ok()) return
        call diff_goto_source(status)
    end subroutine diff_next_error

    !> `read_only_mode_hook' in Diff buffers: diff-mode-read-only follows
    !> buffer-read-only.  Not a port.
    recursive subroutine track_read_only(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call diff_mode_read_only%set_local(buffers(cur)%read_only)
    end subroutine track_read_only

    !> Major mode for viewing/editing context diffs.
    !> Supports unified and context diffs as well as, to a lesser extent,
    !> diffs in the old "normal" format.  (Unified diffs have become the
    !> standard, most commonly encountered format.)
    !>
    !> \{diff_mode_map}
    !> In read-only buffers the following bindings are also available:
    !> \{diff_read_only_map}
    !>
    !> Port of diff-mode.el:diff-mode.
    recursive subroutine diff_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved

        if (.not. associated(major_modes%diff_mode%run)) call syms_of_diff_mode()
        call major_modes%diff_mode%begin(saved, status)
        if (.not. status%ok()) return
        call diff_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call set_next_error_function(diff_next_error)
        if (diff_default_read_only) buffers(cur)%read_only = .true.
        if (buffers(cur)%read_only) call diff_mode_read_only%set_local(.true.)
        call read_only_mode_hook%add(track_read_only, local=.true.)
        call diff_setup_buffer_type()
        call major_modes%diff_mode%finish(saved, status)
    end subroutine diff_mode

    ! ------------------------------------------------------------------
    ! Font lock
    ! ------------------------------------------------------------------

    !> The start of the hunks of a Git patch (point-min if there is no
    !> "diff --git" line).  Port of diff-mode.el:diff--git-preamble-end
    !> (computed each time, no overlay cache).
    integer(int64) function diff_git_preamble_end() result(pos)
        integer(int64) :: saved, found, shortage

        saved = point()
        call goto_char(point_min())
        found = search_fwd("^diff --git .+ .+$", move=.true.)
        shortage = forward_line(2_int64)
        pos = point()
        if (found == 0) pos = point_min()
        call goto_char(saved)
    end function diff_git_preamble_end

    !> The start of the email signature of a Git patch (point-max if
    !> none).  Port of diff-mode.el:diff--git-footer-start (no overlay
    !> cache).
    integer(int64) function diff_git_footer_start() result(pos)
        integer(int64) :: saved, found

        saved = point()
        call goto_char(point_max())
        found = search_bwd(LF//"-- $")

        if (found == 0) then
            pos = point_max()
        else
            pos = mb(0) + 1
        end if

        call goto_char(saved)
    end function diff_git_footer_start

    !> Fontify added/removed lines from point to LIMIT using REGEXP.
    !> If this is a Git patch, don't fontify lines before the first hunk,
    !> or in the email signature at the end.  Port of
    !> diff-mode.el:diff--indicator-matcher-helper.
    subroutine indicator_matcher(limit, regexp, found)
        integer(int64), intent(in) :: limit
        character(len=*), intent(in) :: regexp
        logical, intent(out) :: found
        integer(int64) :: preamble_end, footer_start, beg, end, lim

        found = .false.
        lim = limit

        if (buffer_kind() == DIFF_BUFFER_GIT) then
            preamble_end = diff_git_preamble_end()
            footer_start = diff_git_footer_start()
            beg = point()
            end = limit

            if (end <= preamble_end .or. beg >= footer_start) then
                return
            else if (beg < preamble_end) then
                call goto_char(preamble_end)
            else if (end > footer_start) then
                lim = footer_start
            end if
        end if

        found = search_fwd(regexp, bound=lim) /= 0
    end subroutine indicator_matcher

    !> Port of diff-mode.el:diff--indicator-added-matcher.
    subroutine indicator_added_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call indicator_matcher(limit, "^\([+>]\)\(.*"//LF//"\)", found)
    end subroutine indicator_added_matcher

    !> Port of diff-mode.el:diff--indicator-removed-matcher.
    subroutine indicator_removed_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call indicator_matcher(limit, "^\([<-]\)\(.*"//LF//"\)", found)
    end subroutine indicator_removed_matcher

    !> The face of the "!" line of a context diff at point: added after
    !> the hunk's middle header, else removed (diff-use-changed-face is
    !> false: `diff_changed' is not distinct).  INDICATOR asks for the
    !> indicator face.  Not a port.
    subroutine bang_face(face, indicator)
        character(len=:), allocatable, intent(out) :: face
        logical, intent(in) :: indicator
        integer(int64) :: saved, limit, mb0, me0, found
        type(status_type) :: st

        face = ""
        saved = point()
        ! (save-match-data)
        mb0 = mb(0)
        me0 = me(0)
        limit = diff_beginning_of_hunk(st)
        call goto_char(saved)

        if (st%ok() .and. limit < point()) then
            found = search_bwd(diff_context_mid_hunk_header_re, bound=limit)

            if (found /= 0) then
                face = merge("diff_indicator_added", "diff_added          ", indicator)
            else
                face = merge("diff_indicator_removed", "diff_removed          ", indicator)
            end if

            face = trim(face)
        end if

        call goto_char(saved)
        call restore_match(mb0, me0)
    end subroutine bang_face

    !> Put the match data back to [MB0, ME0) of the "!" line.  Not a port.
    subroutine restore_match(mb0, me0)
        integer(int64), intent(in) :: mb0, me0
        integer(int64) :: saved, found

        saved = point()
        call goto_char(mb0)
        found = search_fwd("^\(!\)\(.*"//LF//"\)", bound=me0)
        call goto_char(saved)
    end subroutine restore_match

    ! Not a port.
    subroutine bang_indicator_face(face)
        character(len=:), allocatable, intent(out) :: face

        call bang_face(face, .true.)
    end subroutine bang_indicator_face

    ! Not a port.
    subroutine bang_line_face(face)
        character(len=:), allocatable, intent(out) :: face

        call bang_face(face, .false.)
    end subroutine bang_line_face

    !> The file name of a header line gets `diff_file_header' unless it
    !> is the "*** ... ****"/"--- ... ----" of a context hunk.  Not a port.
    subroutine file_header_name_face(face)
        character(len=:), allocatable, intent(out) :: face

        face = ""
        if (me(3) == 0) face = "diff_file_header"
    end subroutine file_header_name_face

    !> Port of diff-mode.el diff-font-lock-keywords (no syntax,
    !> prettifying or refinement).
    subroutine diff_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        ! (Highlights go through these variables: gfortran leaks a
        ! function result inside an array constructor of this type.)
        type(font_lock_highlight_type) :: h1(1), h2(2)

        allocate (keywords(20))
        h2(1) = font_lock_highlight(1, face="diff_hunk_header")
        h2(2) = font_lock_highlight(6, face="diff_function")
        keywords(1) = font_lock_keyword("\("//diff_hunk_header_re_unified//"\)\(.*\)$", h2)
        ! context
        h2(1) = font_lock_highlight(1, face="diff_hunk_header")
        h2(2) = font_lock_highlight(2, face="diff_function")
        keywords(2) = font_lock_keyword("^\(\*\{15\}\)\(.*\)$", h2)
        keywords(3) = font_lock_keyword("^\*\*\* .+ \*\*\*\*", face="diff_hunk_header")
        keywords(4) = font_lock_keyword(diff_context_mid_hunk_header_re, face="diff_hunk_header")
        ! normal
        keywords(5) = font_lock_keyword("^[0-9,]+[acd][0-9,]+$", face="diff_hunk_header")
        keywords(6) = font_lock_keyword("^---$", face="diff_hunk_header")
        h2(1) = font_lock_highlight(0, face="diff_header")
        h2(2) = font_lock_highlight(2, face_function=file_header_name_face, override=FONT_LOCK_OVERRIDE_PREPEND)
        keywords(7) = font_lock_keyword("^\(---\|\+\+\+\|\*\*\*\) \([^"//TAB//LF//"]+?\)\(?:"//TAB// &
                                        ".*\| \(\*\*\*\*\|----\)\)?"//LF, h2)
        h2(1) = font_lock_highlight(1, face="diff_indicator_removed")
        h2(2) = font_lock_highlight(2, face="diff_removed")
        keywords(8) = font_lock_keyword(matcher=indicator_removed_matcher, highlights=h2)
        h2(1) = font_lock_highlight(1, face="diff_indicator_added")
        h2(2) = font_lock_highlight(2, face="diff_added")
        keywords(9) = font_lock_keyword(matcher=indicator_added_matcher, highlights=h2)
        h2(1) = font_lock_highlight(1, face_function=bang_indicator_face)
        h2(2) = font_lock_highlight(2, face_function=bang_line_face)
        keywords(10) = font_lock_keyword("^\(!\)\(.*"//LF//"\)", h2)
        h2(1) = font_lock_highlight(0, face="diff_header")
        h2(2) = font_lock_highlight(1, face="diff_index", override=FONT_LOCK_OVERRIDE_PREPEND)
        keywords(11) = font_lock_keyword("^\(?:Index\|revno\): \(.+\).*"//LF, h2)
        keywords(12) = font_lock_keyword("^\(?:index .*\.\.\|diff \).*"//LF, face="diff_header")
        keywords(13) = font_lock_keyword("^\(?:new\|deleted\) file mode .*"//LF, face="diff_header")
        keywords(14) = font_lock_keyword("^Only in .*"//LF, face="diff_nonexistent")
        keywords(15) = font_lock_keyword("^Binary files .* differ"//LF, face="diff_file_header")
        h2(1) = font_lock_highlight(1, face="font_lock_comment_delimiter_face")
        h2(2) = font_lock_highlight(2, face="font_lock_comment_face")
        keywords(16) = font_lock_keyword("^\(#\)\(.*\)", h2)
        h1(1) = font_lock_highlight(0, face="diff_error")
        keywords(17) = font_lock_keyword("^diff: .*", h1)
        h1(1) = font_lock_highlight(0, face="diff_context")
        keywords(18) = font_lock_keyword("^[^-=+*!<>#].*"//LF, h1)
        keywords = keywords(1:18)
    end subroutine diff_font_lock_keywords

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The region if it is active (interactive "R"), else absent.
    !> Not a port.
    subroutine region_args(have, beg, end)
        logical, intent(out) :: have
        integer(int64), intent(out) :: beg, end
        type(status_type) :: st

        have = use_region_p()
        beg = 0
        end = 0

        if (have) then
            call region_beginning(beg, st)
            call region_end(end, st)
        end if
    end subroutine region_args

    !> The prefix numeric value (interactive "p").  Not a port.
    integer(int64) function p_arg()
        p_arg = prefix_numeric_value(current_prefix_arg)
    end function p_arg

    recursive subroutine cmd_diff_hunk_next(status)
        type(status_type), intent(out) :: status

        call diff_hunk_next(status, p_arg(), interactive=.true.)
    end subroutine cmd_diff_hunk_next

    recursive subroutine cmd_diff_hunk_prev(status)
        type(status_type), intent(out) :: status

        call diff_hunk_prev(status, p_arg())
    end subroutine cmd_diff_hunk_prev

    recursive subroutine cmd_diff_file_next(status)
        type(status_type), intent(out) :: status

        call diff_file_next(status, p_arg(), interactive=.true.)
    end subroutine cmd_diff_file_next

    recursive subroutine cmd_diff_file_prev(status)
        type(status_type), intent(out) :: status

        call diff_file_prev(status, p_arg())
    end subroutine cmd_diff_file_prev

    recursive subroutine cmd_diff_hunk_kill(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        logical :: have

        call region_args(have, beg, end)

        if (have) then
            call diff_hunk_kill(status, beg, end)
        else
            call diff_hunk_kill(status)
        end if
    end subroutine cmd_diff_hunk_kill

    recursive subroutine cmd_diff_file_kill(status)
        type(status_type), intent(out) :: status

        call diff_file_kill(status)
    end subroutine cmd_diff_file_kill

    recursive subroutine cmd_diff_delete_other_hunks(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        logical :: have

        call region_args(have, beg, end)

        if (have) then
            call diff_delete_other_hunks(status, beg, end)
        else
            call diff_delete_other_hunks(status)
        end if
    end subroutine cmd_diff_delete_other_hunks

    recursive subroutine cmd_diff_restrict_view(status)
        type(status_type), intent(out) :: status

        call diff_restrict_view(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_diff_restrict_view

    recursive subroutine cmd_diff_split_hunk(status)
        type(status_type), intent(out) :: status

        call diff_split_hunk(status)
    end subroutine cmd_diff_split_hunk

    recursive subroutine cmd_diff_kill_junk(status)
        type(status_type), intent(out) :: status

        call diff_kill_junk(status)
    end subroutine cmd_diff_kill_junk

    recursive subroutine cmd_diff_fixup_modifs(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        if (.not. nilp(current_prefix_arg) .or. use_region_p()) then
            call region_beginning(beg, status)
            if (status%ok()) call region_end(end, status)
            if (status%ok()) call diff_fixup_modifs(beg, end, status)
        else
            call diff_fixup_modifs(point_min(), point_max(), status)
        end if
    end subroutine cmd_diff_fixup_modifs

    recursive subroutine cmd_diff_goto_source(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call diff_goto_source(status)
        else
            call diff_goto_source(status, .true., p_arg())
        end if
    end subroutine cmd_diff_goto_source

    recursive subroutine cmd_diff_apply_hunk(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        logical :: have

        call region_args(have, beg, end)

        if (have) then
            call diff_apply_hunk(status,.not. nilp(current_prefix_arg), beg, end)
        else
            call diff_apply_hunk(status,.not. nilp(current_prefix_arg))
        end if
    end subroutine cmd_diff_apply_hunk

    recursive subroutine cmd_diff_test_hunk(status)
        type(status_type), intent(out) :: status

        call diff_test_hunk(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_diff_test_hunk

    recursive subroutine cmd_diff_revert_and_kill_hunk(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        logical :: have

        call region_args(have, beg, end)

        if (have) then
            call diff_revert_and_kill_hunk(status, beg, end)
        else
            call diff_revert_and_kill_hunk(status)
        end if
    end subroutine cmd_diff_revert_and_kill_hunk

    recursive subroutine cmd_diff_apply_buffer(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end
        integer :: failures
        logical :: have

        call region_args(have, beg, end)

        if (have) then
            call diff_apply_buffer(failures, status, beg, end,.not. nilp(current_prefix_arg))
        else
            call diff_apply_buffer(failures, status, reverse=.not. nilp(current_prefix_arg))
        end if
    end subroutine cmd_diff_apply_buffer

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of diff-mode.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, L = FACE_BACKGROUND_LIGHT, D = FACE_BACKGROUND_DARK

        f => diff_header
        call f%define("diff_header", "`diff_mode' face inherited by hunk and index header faces.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="grey85", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="grey45", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="blue1", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(weight="bold", extend=.true.))
        f => diff_file_header
        call f%define("diff_file_header", "`diff_mode' face used to highlight file header lines.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="grey75", weight="bold", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="grey60", weight="bold", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="cyan", weight="bold", extend=.true.))
        call f%spec_entry(display_when(), face_attrs(weight="bold", extend=.true.))
        f => diff_index
        call f%define("diff_index", "`diff_mode' face used to highlight index header lines.")
        call f%spec_entry(display_when(), face_attrs(inherit="diff_file_header"))
        f => diff_hunk_header
        call f%define("diff_hunk_header", "`diff_mode' face used to highlight hunk header lines.")
        call f%spec_entry(display_when(), face_attrs(inherit="diff_header"))
        f => diff_removed
        call f%define("diff_removed", "`diff_mode' face used to highlight removed lines.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="diff_changed"))
        call f%spec_entry(display_when(classes=[C], min_colors=257, backgrounds=[L]), &
                          face_attrs(background="#ffeeee", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="#ffdddd", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="#553333", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="red", extend=.true.))
        f => diff_added
        call f%define("diff_added", "`diff_mode' face used to highlight added lines.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="diff_changed"))
        call f%spec_entry(display_when(classes=[C], min_colors=257, backgrounds=[L]), &
                          face_attrs(background="#eeffee", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="#ddffdd", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="#335533", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="green", extend=.true.))
        f => diff_changed_unspecified
        call f%define("diff_changed_unspecified", "`diff_mode' face used to highlight changed lines.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="diff_changed"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[L]), &
                          face_attrs(background="#ffffaa", extend=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[D]), &
                          face_attrs(background="#888833", extend=.true.))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="yellow", extend=.true.))
        f => diff_changed
        call f%define("diff_changed", "`diff_mode' face used to highlight changed lines.")
        call f%spec_entry(display_when(), face_attrs())
        f => diff_indicator_removed
        call f%define("diff_indicator_removed", "`diff_mode' face used to highlight indicator of removed lines (-, <).")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="diff_removed"))
        call f%spec_entry(display_when(classes=[C], min_colors=88), face_attrs(foreground="#aa2222"))
        f => diff_indicator_added
        call f%define("diff_indicator_added", "`diff_mode' face used to highlight indicator of added lines (+, >).")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="diff_added"))
        call f%spec_entry(display_when(classes=[C], min_colors=88), face_attrs(foreground="#22aa22"))
        f => diff_indicator_changed
        call f%define("diff_indicator_changed", "`diff_mode' face used to highlight indicator of changed lines.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="diff_changed"))
        call f%spec_entry(display_when(classes=[C], min_colors=88), face_attrs(foreground="#aaaa22"))
        f => diff_function
        call f%define("diff_function", "`diff_mode' face used to highlight function names produced by ""diff -p"".")
        call f%spec_entry(display_when(), face_attrs(inherit="diff_header"))
        f => diff_context
        call f%define("diff_context", "`diff_mode' face used to highlight context and other side-information.")
        call f%spec_entry(display_when(), face_attrs(extend=.true.))
        f => diff_nonexistent
        call f%define("diff_nonexistent", "`diff_mode' face used to highlight nonexistent files in recursive diffs.")
        call f%spec_entry(display_when(), face_attrs(inherit="diff_file_header"))
        f => diff_error
        call f%define("diff_error", "`diff_mode' face for error messages from diff.")
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="red", background="black", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold"))
    end subroutine define_faces

    !> Define Diff mode, its keymaps and faces, and register the commands.
    !> Not a port.
    subroutine syms_of_diff_mode()
        type(status_type) :: st

        call define_faces()
        call major_modes%diff_mode%define("diff_mode", "Diff", diff_mode, diff_mode_hook)
        major_modes%diff_mode%special = .true.
        diff_mode_map = major_modes%diff_mode%map
        commands%diff_mode%run => diff_mode
        commands%diff_hunk_next%run => cmd_diff_hunk_next
        commands%diff_hunk_prev%run => cmd_diff_hunk_prev
        commands%diff_file_next%run => cmd_diff_file_next
        commands%diff_file_prev%run => cmd_diff_file_prev
        commands%diff_hunk_kill%run => cmd_diff_hunk_kill
        commands%diff_file_kill%run => cmd_diff_file_kill
        commands%diff_delete_other_hunks%run => cmd_diff_delete_other_hunks
        commands%diff_restrict_view%run => cmd_diff_restrict_view
        commands%diff_restrict_view%disabled = .true.
        commands%diff_split_hunk%run => cmd_diff_split_hunk
        commands%diff_kill_junk%run => cmd_diff_kill_junk
        commands%diff_fixup_modifs%run => cmd_diff_fixup_modifs
        commands%diff_goto_source%run => cmd_diff_goto_source
        commands%diff_apply_hunk%run => cmd_diff_apply_hunk
        commands%diff_test_hunk%run => cmd_diff_test_hunk
        commands%diff_revert_and_kill_hunk%run => cmd_diff_revert_and_kill_hunk
        commands%diff_apply_buffer%run => cmd_diff_apply_buffer
        ! diff-mode-shared-map.
        if (keymap_live_p(diff_mode_shared_map)) call free_keymap(diff_mode_shared_map, st)
        call make_sparse_keymap(diff_mode_shared_map)
        call keymap_set(diff_mode_shared_map, "n", "diff_hunk_next", st)
        call keymap_set(diff_mode_shared_map, "N", "diff_file_next", st)
        call keymap_set(diff_mode_shared_map, "p", "diff_hunk_prev", st)
        call keymap_set(diff_mode_shared_map, "P", "diff_file_prev", st)
        call keymap_set(diff_mode_shared_map, "TAB", "diff_hunk_next", st)
        call keymap_set(diff_mode_shared_map, "<backtab>", "diff_hunk_prev", st)
        call keymap_set(diff_mode_shared_map, "k", "diff_hunk_kill", st)
        call keymap_set(diff_mode_shared_map, "K", "diff_file_kill", st)
        ! From compilation-minor-mode.
        call keymap_set(diff_mode_shared_map, "}", "diff_file_next", st)
        call keymap_set(diff_mode_shared_map, "{", "diff_file_prev", st)
        call keymap_set(diff_mode_shared_map, "RET", "diff_goto_source", st)
        ! other-window
        call keymap_set(diff_mode_shared_map, "o", "diff_goto_source", st)
        ! diff-mode-read-only-map.
        if (keymap_live_p(diff_mode_read_only_map)) call free_keymap(diff_mode_read_only_map, st)
        call make_sparse_keymap(diff_mode_read_only_map)
        call keymap_set(diff_mode_read_only_map, "W", "widen", st)
        call keymap_set(diff_mode_read_only_map, "r", "diff_restrict_view", st)
        call keymap_set(diff_mode_read_only_map, "s", "diff_split_hunk", st)
        call keymap_set(diff_mode_read_only_map, "u", "diff_revert_and_kill_hunk", st)
        call keymap_set(diff_mode_read_only_map, "@", "diff_revert_and_kill_hunk", st)
        call set_keymap_parent(diff_mode_read_only_map, special_mode_map, st)
        ! diff-mode-map.
        call keymap_set(diff_mode_map, "ESC", make_keymap_ref(diff_mode_shared_map), st)
        call keymap_set(diff_mode_map, "C-c C-c", "diff_goto_source", st)
        call keymap_set(diff_mode_map, "C-c C-a", "diff_apply_hunk", st)
        call keymap_set(diff_mode_map, "C-c M-u", "diff_revert_and_kill_hunk", st)
        call keymap_set(diff_mode_map, "C-c C-m a", "diff_apply_buffer", st)
        call keymap_set(diff_mode_map, "C-c C-m n", "diff_delete_other_hunks", st)
        call keymap_set(diff_mode_map, "C-c C-n", "diff_restrict_view", st)
        call keymap_set(diff_mode_map, "C-c C-s", "diff_split_hunk", st)
        call keymap_set(diff_mode_map, "C-c C-t", "diff_test_hunk", st)
        ! diff-read-only-map (composed of diff-mode-read-only-map and
        ! special-mode-map), active while diff-mode-read-only.
        if (keymap_live_p(diff_read_only_map)) call free_keymap(diff_read_only_map, st)
        call make_sparse_keymap(diff_read_only_map)
        call set_keymap_parent(diff_read_only_map, diff_mode_read_only_map, st)
        call forward_setting(diff_mode_read_only, local_if_set=.true.)
        call add_minor_mode_map("diff_mode_read_only", diff_read_only_map)
        call add_local_variables_killer(kill_diff_locals)
    end subroutine syms_of_diff_mode
end module diff_mode_fm
