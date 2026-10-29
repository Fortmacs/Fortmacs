! replace_fm.f90 -- replace commands.
! Ported from GNU Emacs 31.1 lisp/replace.el.
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

!> Query replace: `query_replace' (M-%), `query_replace_regexp' (C-M-%),
!> `map_query_replace_regexp', `replace_string', `replace_regexp' and
!> their engine `perform_replace', with the `query_replace_map' answers
!> (act, skip, backup, undo, edit, ...).
!>
!> Differences from upstream, for want of Lisp and of display features:
!>
!> - REPLACEMENTS as a (FUNCTION . DATA) cons is a Fortran function of
!>   the replacement count (`replacement_function'); `\,' (a Lisp
!>   expression) in an interactive replacement is an error, while `\#'
!>   (the replacement count) works;
!> - the FROM and TO of a history entry are split at the separator text
!>   itself (upstream marks the separator with a text property);
!> - no highlighting (query-replace-highlight, lazy highlight), no
!>   invisible-text or isearch-filter-predicate skipping, no character
!>   folding, no noncontiguous regions, no multi-buffer answers and no
!>   `diff' answer yet.
!>
!> Occur: `occur' (M-s o; `list_matching_lines') lists the lines matching
!> a regexp in *Occur*, in Occur mode, whose lines visit the matches (RET,
!> o, C-o, n, p, M-n, M-p, `next_error'); `multi_occur' and
!> `multi_occur_in_matching_buffers' search several buffers;
!> `occur_edit_mode' (e) applies edits to the original lines.  With C-u,
!> occur collects the matches instead.  The keep-lines family:
!> `keep_lines', `flush_lines', `kill_matching_lines',
!> `copy_matching_lines' and `how_many'.
!>
!> Occur differences: the targets of a line (upstream lists of markers in
!> the `occur-target' property) are kept with the Occur buffer, the
!> property holding their index; a region searched is markers, not an
!> overlay.  Deferred: the overlay arrow, the highlighting of the visited
!> occurrence (`next_error_highlight'), mouse events (mouse-2,
!> `occur_word_at_mouse', `occur_symbol_at_mouse', the context menu), the
!> menu, C-c C-f (`next_error_follow_minor_mode'), l
!> (`recenter_current_error'), c (`clone_buffer'), the coding system of
!> the buffers searched, `isearch_occur' and the isearch descriptions of
!> regexps, `read_regexp_case_fold_search'.
module replace_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, FM_QUIT, FM_USER_ERROR, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, make_string, make_list, stringp, xstring, nilp, list_elements, &
                       list_length, list_elt
    use lread_fm, only: sym
    use fns_fm, only: schars
    use data_fm, only: value_eq, symbol_value, set
    use buffer_fm, only: barf_if_buffer_read_only
    use marker_fm, only: make_marker, set_marker, marker_position, free_marker
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, buffer_substring, &
                          region_beginning, region_end
    use cmds_fm, only: forward_char
    use textprop_fm, only: text_property_not_all
    use undo_fm, only: undo_boundary
    use search_fm, only: case_fold_search, search_spaces_regexp, re_search_forward, re_search_backward, &
                         looking_at, match_beginning, match_end, match_data, set_match_data, replace_match, &
                         regexp_quote, string_match, SEARCH_NOERROR
    use subr_fm, only: read_key, add_to_history, defvar_nil, history_length, query_replace_answer, match_string, &
                       looking_back, match_substitute_replacement, find_tag_default, find_tag_default_as_regexp, &
                       find_tag_default_as_symbol_regexp, &
                       ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, ANSWER_ACT_AND_EXIT, ANSWER_AUTOMATIC, &
                       ANSWER_RECENTER, ANSWER_HELP, ANSWER_QUIT, ANSWER_EDIT_REPLACEMENT, &
                       ANSWER_EDIT_REPLACEMENT_EXACT_CASE, ANSWER_ACT_AND_SHOW, ANSWER_EDIT, &
                       ANSWER_DELETE_AND_EDIT, ANSWER_BACKUP, ANSWER_UNDO, ANSWER_UNDO_ALL, ANSWER_DIFF, &
                       ANSWER_SCROLL_UP, ANSWER_SCROLL_DOWN
    use isearch_fm, only: search_upper_case, UPPER_CASE_NIL, search_whitespace_regexp, &
                          isearch_no_upper_case_p, word_search_regexp
    use minibuf_fm, only: read_from_minibuffer, read_string, history_add_new_input, minibuffer_string_type
    use keyboard_fm, only: this_command, unread_command_events, sit_for, recursive_edit_1
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use simple_fm, only: push_mark, use_region_p, deactivate_mark
    use window_fm, only: recenter_top_bottom, scroll_up, scroll_down
    use help_fm, only: with_help_window
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use handle_fm, only: null_handle
    use lisp_fm, only: t_value, consp, fixnump, xfixnum, make_fixnum, list
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, get_buffer, kill_buffer, &
                         buffer_live_p, buffer_list, erase_buffer, rename_buffer, generate_new_buffer_name, &
                         restore_buffer_modified_p
    use marker_fm, only: copy_marker_at, marker_buffer
    use insdel_fm, only: insert, after_change_functions
    use editfns_fm, only: bolp, eolp, line_beginning_position, line_end_position, delete_region
    use cmds_fm, only: forward_line
    use indent_fm, only: current_column, move_to_column
    use textprop_fm, only: put_text_property, get_text_property, add_text_properties, add_face_text_property, &
                           text_properties_at, next_property_change, next_single_property_change, &
                           previous_single_property_change, propertize
    use search_fm, only: search_forward, SEARCH_MOVE
    use keyboard_fm, only: last_command
    use simple_fm, only: kill_region, copy_region_as_kill, line_number_at_pos, count_lines, set_next_error_function, &
                         next_error_found, next_error_last_buffer, fundamental_mode
    use window_fm, only: windows, display_buffer, pop_to_buffer, switch_to_buffer_other_window, select_window, &
                         get_buffer_window, set_window_point, window_start, window_text_cols, recenter, &
                         display_alist_type
    use frame_fm, only: selected_frame
    use help_fm, only: substitute_command_keys
    use minibuffer_fm, only: read_buffer, REQUIRE_MATCH_T

    use files_fm, only: kill_buffer_hook, revert_buffer_function
    use eval_fm, only: hook_type
    use derived_fm, only: derived_mode_p
    use modes_fm, only: major_modes
    use keymap_fm, only: keymap_set, set_keymap_parent
    use text_mode_fm, only: text_mode_map
    use font_core_fm, only: font_lock_mode_enabled, turn_on_font_lock
    use font_lock_fm, only: font_lock_ensure
    use regexp_opt_fm, only: regexp_opt_depth
    use xfaces_fm, only: face_differs_from_default_p
    use faces_fm, only: named_face_type, display_when, face_attrs, init_faces, FACE_CLASS_COLOR, FACE_CLASS_MONO, &
                        FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    implicit none(type, external)
    private

    public :: perform_replace, replace_search, replace_match_maybe_edit, replace_quote, flush_lines
    public :: query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, replace_regexp
    public :: query_replace_descr, query_replace_read_from, query_replace_read_to, query_replace_read_args, &
              query_replace_compile_replacement, replace_expand_count, syms_of_replace
    public :: replacement_function, read_regexp, read_regexp_suggestions, query_replace_read_from_suggestions, &
              format_prompt
    public :: keep_lines, kill_matching_lines, copy_matching_lines, how_many, occur, occur_in_region, multi_occur, &
              occur_read_primary_args, &
              multi_occur_in_matching_buffers, occur_rename_buffer, occur_mode, occur_edit_mode, occur_cease_edit, &
              occur_mode_goto_occurrence, occur_mode_goto_occurrence_other_window, occur_mode_display_occurrence, &
              occur_next, occur_prev, occur_next_error, occur_revert_function, occur_after_change_function, &
              occur_mode_find_occurrence, occur_add_target, syms_of_occur

    !> Face used to highlight matches permanently.  Port of replace.el
    !> match.
    type(named_face_type), target, public :: match

    !> A FROM_STRING and TO_STRING pair of `query_replace_defaults'.
    type, public :: replace_pair_type
        character(len=:), allocatable :: from, to
    end type replace_pair_type

    abstract interface
        !> A function computing each replacement string from the number of
        !> replacements done so far: the (FUNCTION . DATA) form of
        !> `perform_replace''s REPLACEMENTS.
        function replacement_function(count) result(text)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: count
            character(len=:), allocatable :: text
        end function replacement_function
    end interface

    !> True means `query_replace' should preserve case in replacements.
    !> Port of replace.el case-replace.
    logical, public :: case_replace = .true.

    !> True means `query_replace' matches a sequence of whitespace chars.
    !> When you enter a space or spaces in the strings to be replaced,
    !> it will match any sequence matched by the regexp
    !> `search_whitespace_regexp'.  Port of replace.el
    !> replace-lax-whitespace.
    logical, public :: replace_lax_whitespace = .false.

    !> True means `query_replace_regexp' matches a sequence of whitespace
    !> chars.  When you enter a space or spaces in the regexps to be
    !> replaced, it will match any sequence matched by the regexp
    !> `search_whitespace_regexp'.  Port of replace.el
    !> replace-regexp-lax-whitespace.
    logical, public :: replace_regexp_lax_whitespace = .false.

    !> Default values of FROM_STRING and TO_STRING for `query_replace'.
    !> This is an array of FROM_STRING and TO_STRING pairs, or unallocated
    !> if there are no default values.  Port of replace.el
    !> query-replace-defaults (most recent first).
    type(replace_pair_type), allocatable, public :: query_replace_defaults(:)

    !> String that separates FROM and TO in the history of replacement
    !> pairs.  When unallocated, the pair will not be added to the
    !> history (same behavior as in Emacs 24.5).  Port of replace.el
    !> query-replace-from-to-separator.
    character(len=:), allocatable, public :: query_replace_from_to_separator

    !> History list to use for the FROM argument of `query_replace'
    !> commands.  The value of this variable should be the name of
    !> a variable that holds a history list for the strings
    !> or patterns to be replaced.  Port of replace.el
    !> query-replace-from-history-variable.
    character(len=:), allocatable, public :: query_replace_from_history_variable

    !> History list to use for the TO argument of `query_replace'
    !> commands.  The value of this variable should be the name of
    !> a variable that holds a history list for replacement
    !> strings or patterns.  Port of replace.el
    !> query-replace-to-history-variable.
    character(len=:), allocatable, public :: query_replace_to_history_variable

    !> True means `query_replace' and friends ignore read-only matches.
    !> Port of replace.el query-replace-skip-read-only.
    logical, public :: query_replace_skip_read_only = .false.

    !> True means show substituted replacement text in the minibuffer.
    !> This variable affects only `query_replace_regexp'.  Port of
    !> replace.el query-replace-show-replacement.
    logical, public :: query_replace_show_replacement = .true.

    !> Number of replacements done so far.
    !> See `replace_regexp'.  Port of replace.el replace-count.
    integer(int64), public :: replace_count = 0

    character(len=*), parameter :: LF = achar(10)

    !> Help message while in `query_replace'.  Port of replace.el
    !> query-replace-help (keys substituted; no multi-buffer answers).
    character(len=*), parameter :: query_replace_help = &
                                   "Type SPC or y to replace one match, Delete or n to skip to next,"//LF// &
                                   "RET or q to exit, Period to replace one match and exit,"//LF// &
                                   ", to replace but not move point immediately,"//LF// &
                                   "! to replace all remaining matches in this buffer with no more questions,"//LF// &
                                   "C-r to enter recursive edit (C-M-c to get out again),"//LF// &
                                   "C-w to delete match and then enter recursive edit,"//LF// &
                                   "^ to move point back to previous match,"//LF// &
                                   "u to undo previous replacement,"//LF// &
                                   "U to undo all replacements,"//LF// &
                                   "e to edit the replacement string."//LF// &
                                   "E to edit the replacement string with exact case."//LF// &
                                   "C-l to clear the screen, redisplay, and offer same replacement again,"//LF// &
                                   "Any other character exits the interactive replacement loop, and is then"//LF// &
                                   "re-executed as a normal key sequence."

    ! The values of perform-replace's match-again: nil, t (a match is
    ! possible at point) and match data.
    integer, parameter :: AGAIN_NIL = 0, AGAIN_T = 1, AGAIN_DATA = 2

    ! An element of perform-replace's stack, (POINT REPLACED MATCH-DATA
    ! SEARCH-STR NEXT-REPLACE NEXT-REPLACEMENT MATCH-AGAIN), as
    ! replace--push-stack makes it.
    type :: stack_entry_type
        integer(int64) :: point = 0
        logical :: replaced = .false.
        integer(int64), allocatable :: data(:)
        character(len=:), allocatable :: search_str, next_replace, next_replacement
        integer :: again = AGAIN_NIL
        integer(int64), allocatable :: again_match(:)
    end type stack_entry_type

    ! ------------------------------------------------------------------
    ! Occur
    ! ------------------------------------------------------------------

    !> A buffer `occur' searches, or the region of one (upstream an
    !> overlay with the property `occur--orig-point'): BUFFER, and with
    !> REGION the markers START and END and the point ORIG_POINT occur was
    !> called at.
    type, public :: occur_source_type
        type(handle_type) :: buffer
        logical :: region = .false.
        type(handle_type) :: start, end
        integer(int64) :: orig_point = 0
    end type occur_source_type

    ! An `occur-target' value: the markers BEG and END of each match on a
    ! line, in the buffer searched.  The `occur_target' text property of
    ! an Occur buffer holds the index of one in that buffer's targets.
    type :: occur_target_type
        type(handle_type), allocatable :: beg(:), end(:)
    end type occur_target_type

    ! What an Occur buffer keeps: occur-revert-arguments (REGEXP, NLINES
    ! with HAS_NLINES, or the string COLLECT, and the SOURCES; permanently
    ! buffer-local) and the targets of its lines.
    type :: occur_buffer_type
        type(handle_type) :: buffer
        logical :: has_revert = .false.
        character(len=:), allocatable :: regexp, collect
        logical :: has_nlines = .false.
        integer(int64) :: nlines = 0
        type(occur_source_type), allocatable :: sources(:)
        type(occur_target_type), allocatable :: targets(:)
        integer :: ntargets = 0
        ! grep-edit-original-mode-map and occur's Occur-Edit keymap.
        type(handle_type) :: original_map
    end type occur_buffer_type

    type(occur_buffer_type), allocatable :: occur_buffers(:)

    !> History of regexp for occur's collect operation.  Port of
    !> replace.el occur-collect-regexp-history (a history list, named
    !> "occur_collect_regexp_history").
    character(len=*), parameter :: OCCUR_COLLECT_HISTORY = "occur_collect_regexp_history"

    !> Default number of context lines included around
    !> `list_matching_lines' matches.  A negative number means to include
    !> that many lines before the match.  A positive number means to
    !> include that many lines both before and after.  Port of replace.el
    !> list-matching-lines-default-context-lines.
    integer, public :: list_matching_lines_default_context_lines = 0

    !> Face used by \[list_matching_lines] to show the text that matches.
    !> If the value is "", don't highlight the matching portions
    !> specially.  Port of replace.el list-matching-lines-face.
    character(len=:), allocatable, public :: list_matching_lines_face

    !> Face used by \[list_matching_lines] to show the names of buffers.
    !> If the value is "", don't highlight the buffer names specially.
    !> Port of replace.el list-matching-lines-buffer-name-face.
    character(len=:), allocatable, public :: list_matching_lines_buffer_name_face

    !> Face used by \[list_matching_lines] to highlight the current line.
    !> Port of replace.el list-matching-lines-current-line-face.
    character(len=:), allocatable, public :: list_matching_lines_current_line_face

    !> If true, \[list_matching_lines] shows the current line highlighted.
    !> The current line for this purpose is the line of the original buffer
    !> which was current when \[list_matching_lines] was invoked.
    !> Point in the `*Occur*' buffer will be set right after such line when
    !> there are matches after it.  Port of replace.el
    !> list-matching-lines-jump-to-current-line.
    logical, public :: list_matching_lines_jump_to_current_line = .false.

    !> Face used by \[list_matching_lines] to show the prefix column.
    !> The prefix column is the part of display that precedes the actual
    !> contents of the line; it normally shows the line number.  (For
    !> multiline matches, the prefix column shows the line number for the
    !> first line and whitespace for the rest of the lines.)
    !> If this face will display the same as the default face, the prefix
    !> column will not be highlighted specially.  Port of replace.el
    !> list-matching-lines-prefix-face.
    character(len=:), allocatable, public :: list_matching_lines_prefix_face

    !> Text properties to discard when copying lines to the *Occur* buffer.
    !> The value should be a list of text properties to discard, or
    !> `occur_excluded_properties_all' true to discard all text
    !> properties (the value t).  Port of replace.el
    !> occur-excluded-properties.
    character(len=24), allocatable, public :: occur_excluded_properties(:)

    !> True for the value t of `occur_excluded_properties': discard all
    !> text properties.
    logical, public :: occur_excluded_properties_all = .false.

    !> Hook run when entering Occur mode.  Port of replace.el
    !> occur-mode-hook.
    type(hook_type), target, public :: occur_mode_hook = hook_type(name="occur_mode_hook")

    !> Hook run when entering Occur-Edit mode.  Port of replace.el
    !> occur-edit-mode-hook.
    type(hook_type), target, public :: occur_edit_mode_hook = hook_type(name="occur_edit_mode_hook")

    !> Hook run by Occur when there are any matches.  Port of replace.el
    !> occur-hook.
    type(hook_type), target, public :: occur_hook = hook_type(name="occur_hook")

    !> Hook run by Occur after locating an occurrence.
    !> This will be called with the cursor position at the occurrence.  An
    !> application for this is to reveal context in an outline mode when
    !> the occurrence is hidden.  Port of replace.el
    !> occur-mode-find-occurrence-hook.
    type(hook_type), target, public :: occur_mode_find_occurrence_hook = &
                                       hook_type(name="occur_mode_find_occurrence_hook")

    !> Keymap for `occur_mode'.  Port of replace.el occur-mode-map.
    type(handle_type), public :: occur_mode_map

    !> Keymap for `occur_edit_mode'.  Port of replace.el
    !> occur-edit-mode-map.
    type(handle_type), public :: occur_edit_mode_map

    ! occur--final-pos: where point goes in the *Occur* buffer (0 for
    ! nil).
    integer(int64) :: occur_final_pos = 0

contains

    !> The decimal representation of `n'.
    function number_to_string(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function number_to_string

    !> STRING with control characters shown as ^C and DEL as ^?, as the
    !> prompts display it.  Port of replace.el:query-replace-descr (which
    !> uses display properties).
    function query_replace_descr(string) result(descr)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: descr
        integer :: i, c
        descr = ""
        do i = 1, len(string)
            c = iachar(string(i:i))
            if (c < 32) then
                descr = descr//"^"//achar(c + 64)
            else if (c == 127) then
                descr = descr//"^?"
            else
                descr = descr//string(i:i)
            end if
        end do
    end function query_replace_descr

    !> Quote a replacement string.
    !> This just doubles all backslashes in REPLACEMENT and
    !> returns the resulting string.  Port of replace.el:replace-quote
    !> (REPLACEMENT is a string).
    pure function replace_quote(replacement) result(quoted)
        character(len=*), intent(in) :: replacement
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = ""
        do i = 1, len(replacement)
            if (replacement(i:i) == "\") then
                quoted = quoted//"\\"
            else
                quoted = quoted//replacement(i:i)
            end if
        end do
    end function replace_quote

    !> The position of the first backslash of `string' at or after
    !> `from', not itself escaped, that is followed by one of `chars'; or
    !> 0.  (What the regexp \(\`\|[^\]\)\(\\\\\)*\\[CHARS] finds.)
    integer function find_directive(string, from, chars) result(at)
        character(len=*), intent(in) :: string, chars
        integer, intent(in) :: from
        integer :: i
        at = 0
        i = from
        do while (i < len(string))
            if (string(i:i) == "\") then
                if (index(chars, string(i + 1:i + 1)) > 0) then
                    at = i
                    return
                end if
                ! Skip the escaped character.
                i = i + 2
            else
                i = i + 1
            end if
        end do
    end function find_directive

    !> Maybe convert a regexp replacement TO for `perform_replace'.
    !> REGEXP-FLAG non-nil means TO is a regexp.  `counted' is true when TO
    !> contains `\#', which stands for the number of replacements done so
    !> far (`replace_expand_count' substitutes it).  `\,' (a Lisp
    !> expression) is an error: there is no Lisp.  Port of
    !> replace.el:query-replace-compile-replacement.
    subroutine query_replace_compile_replacement(to, regexp_flag, counted, status)
        character(len=*), intent(in) :: to
        logical, intent(in) :: regexp_flag
        logical, intent(out) :: counted
        type(status_type), intent(out) :: status
        integer :: at
        status = status_type()
        counted = .false.
        if (.not. regexp_flag) return
        at = find_directive(to, 1, ",#")
        do while (at > 0)
            if (to(at + 1:at + 1) == ",") then
                call set_error(status, FM_USER_ERROR, "\, in a replacement is not applicable in Fortmacs")
                return
            end if
            counted = .true.
            at = find_directive(to, at + 2, ",#")
        end do
    end subroutine query_replace_compile_replacement

    !> TO with each `\#' replaced by COUNT, as the compiled replacement
    !> (concat ... (number-to-string replace-count) ...) computes it.
    function replace_expand_count(to, count) result(text)
        character(len=*), intent(in) :: to
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: text
        integer :: at, from
        text = ""
        from = 1
        at = find_directive(to, 1, "#")
        do while (at > 0)
            text = text//to(from:at - 1)//number_to_string(count)
            from = at + 2
            at = find_directive(to, from, "#")
        end do
        text = text//to(from:)
    end function replace_expand_count

    !> Prompt with " (default DEFAULT)" if DEFAULT is given, then ": ".
    !> Port of minibuffer.el:format-prompt.
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: full
        full = prompt
        if (present(default)) full = full//" (default "//default//")"
        full = full//": "
    end function format_prompt

    !> The first string of the list in variable `name', if any.
    subroutine history_head(name, head, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: head
        logical, intent(out) :: found
        type(value_type) :: list, first
        type(status_type) :: status
        head = ""
        found = .false.
        call symbol_value(sym(name), list, status)
        if (.not. status%ok()) return
        if (list_length(list) == 0) return
        first = list_elt(list, 1_int64)
        if (.not. stringp(first)) return
        head = xstring(first)
        found = .true.
    end subroutine history_head

    !> Add (FROM . TO) to `query_replace_defaults'.  Port of the
    !> (add-to-history 'query-replace-defaults (cons from to) nil t)
    !> calls.
    subroutine push_defaults(from, to)
        character(len=*), intent(in) :: from, to
        type(replace_pair_type), allocatable :: longer(:)
        integer :: n, k
        n = 0
        if (allocated(query_replace_defaults)) n = size(query_replace_defaults)
        allocate (longer(min(n + 1, max(history_length, 0))))
        if (size(longer) == 0) then
            call move_alloc(longer, query_replace_defaults)
            return
        end if
        longer(1)%from = from
        longer(1)%to = to
        do k = 2, size(longer)
            longer(k) = query_replace_defaults(k - 1)
        end do
        call move_alloc(longer, query_replace_defaults)
    end subroutine push_defaults

    !> Append `s' to the list `list' (allocated, `n' used) unless absent.
    subroutine push_string(list, n, s)
        type(minibuffer_string_type), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: s
        type(minibuffer_string_type), allocatable :: grown(:)
        integer :: k
        if (n == size(list)) then
            allocate (grown(max(4, 2*n)))
            do k = 1, n
                call move_alloc(list(k)%s, grown(k)%s)
            end do
            call move_alloc(grown, list)
        end if
        n = n + 1
        list(n)%s = s
    end subroutine push_string

    !> The region's text, if `use_region_p'.
    subroutine region_text(text, found)
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        integer(int64) :: b, e
        type(status_type) :: st
        found = use_region_p()
        text = ""
        if (.not. found) return
        call region_beginning(b, st)
        call region_end(e, st)
        call buffer_substring(b, e, text, st)
    end subroutine region_text

    !> Return a list of standard suggestions for `query_replace_read_from'.
    !> By default, the list includes the active region, the identifier
    !> (a.k.a. "tag") at point, the last isearch string, and the last
    !> replacement regexp.  `query_replace_read_from' appends the list
    !> returned by this function to the end of values available via M-n.
    !> Port of replace.el:query-replace-read-from-suggestions.
    subroutine query_replace_read_from_suggestions(list, n)
        type(minibuffer_string_type), allocatable, intent(out) :: list(:)
        integer, intent(out) :: n
        character(len=:), allocatable :: s
        logical :: found
        allocate (list(4))
        n = 0
        call region_text(s, found)
        if (found) call push_string(list, n, s)
        call find_tag_default(s, found)
        if (found) call push_string(list, n, s)
        call history_head("search_ring", s, found)
        if (found) call push_string(list, n, s)
        call history_head(query_replace_from_history_variable, s, found)
        if (found) call push_string(list, n, s)
    end subroutine query_replace_read_from_suggestions

    !> Return a list of standard suggestions for `read_regexp'.
    !> By default, the list includes the active region, the identifier
    !> (a.k.a. "tag") at point, the last isearch regexp, the last isearch
    !> string, and the last replacement regexp.  `read_regexp' appends the
    !> list returned by this function to the end of values available via
    !> M-n.  Port of replace.el:read-regexp-suggestions (nil elements are
    !> left out, as read-regexp drops them).
    subroutine read_regexp_suggestions(list, n)
        type(minibuffer_string_type), allocatable, intent(out) :: list(:)
        integer, intent(out) :: n
        character(len=:), allocatable :: s
        logical :: found
        allocate (list(6))
        n = 0
        call region_text(s, found)
        if (found) call push_string(list, n, s)
        call find_tag_default_as_regexp(s, found)
        if (found) call push_string(list, n, s)
        call find_tag_default_as_symbol_regexp(s, found)
        if (found) call push_string(list, n, s)
        call history_head("regexp_search_ring", s, found)
        if (found) call push_string(list, n, s)
        call history_head("search_ring", s, found)
        call push_string(list, n, regexp_quote(s))
        call history_head(query_replace_from_history_variable, s, found)
        if (found) call push_string(list, n, s)
    end subroutine read_regexp_suggestions

    !> Read and return a regular expression as a string.
    !> Prompt with the string PROMPT.  If PROMPT ends in ":" (followed by
    !> optional whitespace), use it as is.  Otherwise, add ": " to the end,
    !> possibly preceded by the default result (see below).
    !>
    !> The optional argument DEFAULTS is a list of default values: the
    !> first is the default when the user enters empty input, and the
    !> rest come before `read_regexp_suggestions' in the values M-n
    !> offers.
    !>
    !> The optional argument HISTORY is a symbol to use for the history list.
    !> If nil, use `regexp_history'.  Port of replace.el:read-regexp
    !> (DEFAULTS a list of strings; no read-regexp-defaults-function,
    !> read-regexp-map or case-fold toggle).
    recursive subroutine read_regexp(prompt, result, status, defaults, history)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), intent(in), optional :: defaults(:)
        character(len=*), intent(in), optional :: history
        type(minibuffer_string_type), allocatable :: more(:), suggestions(:)
        character(len=:), allocatable :: full, input, histvar
        integer :: n, nmore, k, j
        logical :: saved_add, have_default, dup
        histvar = "regexp_history"
        if (present(history)) histvar = history
        have_default = .false.
        if (present(defaults)) have_default = size(defaults) > 0
        call read_regexp_suggestions(more, nmore)
        ! (delete-dups (delq nil (delete "" suggestions)))
        allocate (suggestions(4))
        n = 0
        if (present(defaults)) then
            do k = 1, size(defaults)
                call add_suggestion(defaults(k)%s)
            end do
        end if
        do k = 1, nmore
            call add_suggestion(more(k)%s)
        end do
        ! The prompt
        full = prompt
        if (.not. ends_in_colon(prompt)) then
            full = prompt
            if (have_default) then
                if (len(defaults(1)%s) > 0) full = full//" (default "//query_replace_descr(defaults(1)%s)//")"
            end if
            full = full//": "
        end if
        ! Do not automatically add default to the history for empty input.
        saved_add = history_add_new_input
        history_add_new_input = .false.
        call read_from_minibuffer(full, input, status, history=histvar, defaults=suggestions(1:n))
        history_add_new_input = saved_add
        if (.not. status%ok()) return
        ! Return the default value when the user enters empty input.
        result = input
        if (len(input) == 0) then
            if (.not. have_default) return
            result = defaults(1)%s
        end if
        call add_to_history(histvar, result)
    contains

        subroutine add_suggestion(s)
            character(len=*), intent(in) :: s
            if (len(s) == 0) return
            dup = .false.
            do j = 1, n
                if (suggestions(j)%s == s .and. len(suggestions(j)%s) == len(s)) dup = .true.
            end do
            if (.not. dup) call push_string(suggestions, n, s)
        end subroutine add_suggestion

        ! (string-match-p ":[ \t]*\\'" prompt)
        logical function ends_in_colon(p)
            character(len=*), intent(in) :: p
            integer :: i
            ends_in_colon = .false.
            do i = len(p), 1, -1
                if (p(i:i) == " " .or. p(i:i) == achar(9)) cycle
                ends_in_colon = p(i:i) == ":"
                return
            end do
        end function ends_in_colon
    end subroutine read_regexp

    !> Query and return the FROM argument of a `query_replace' operation.
    !> Prompt with PROMPT.  REGEXP-FLAG non-nil means the response should
    !> be a regexp.  The return value can also be a pair (FROM . TO)
    !> indicating that the user wants to replace FROM with TO: then
    !> `have_to' is true, and `counted' says whether TO uses `\#'.  Port
    !> of replace.el:query-replace-read-from.
    recursive subroutine query_replace_read_from(prompt, regexp_flag, from, to, have_to, counted, status)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: regexp_flag
        character(len=:), allocatable, intent(out) :: from, to
        logical, intent(out) :: have_to, counted
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: full_prompt
        type(value_type) :: saved_history, from_history
        type(value_type), allocatable :: older(:), entries(:)
        type(minibuffer_string_type), allocatable :: suggestions(:)
        type(status_type) :: st
        integer(int64) :: saved_point
        logical :: saved_add, have_separator
        integer :: k, n, at, nsuggestions
        to = ""
        have_to = .false.
        counted = .false.
        have_separator = allocated(query_replace_from_to_separator)
        ! The minibuffer history: the default pairs, then the FROM history.
        call symbol_value(sym("minibuffer_history"), saved_history, st)
        if (.not. st%ok()) saved_history = nil()
        call symbol_value(sym(query_replace_from_history_variable), from_history, st)
        if (.not. st%ok()) from_history = nil()
        call list_elements(from_history, older)
        n = 0
        if (have_separator .and. allocated(query_replace_defaults)) n = size(query_replace_defaults)
        allocate (entries(n + size(older)))
        do k = 1, n
            entries(k) = make_string(query_replace_defaults(k)%from//query_replace_from_to_separator// &
                                     query_replace_defaults(k)%to)
        end do
        if (size(older) > 0) entries(n + 1:) = older
        call set(sym("minibuffer_history"), make_list(entries), st)
        if (n > 0) then
            full_prompt = format_prompt(prompt, query_replace_descr(query_replace_defaults(1)%from)// &
                                        query_replace_from_to_separator// &
                                        query_replace_descr(query_replace_defaults(1)%to))
        else if (allocated(query_replace_defaults)) then
            if (size(query_replace_defaults) > 0) then
                full_prompt = format_prompt(prompt, query_replace_descr(query_replace_defaults(1)%from)//" -> "// &
                                            query_replace_descr(query_replace_defaults(1)%to))
            else
                full_prompt = format_prompt(prompt)
            end if
        else
            full_prompt = format_prompt(prompt)
        end if
        ! The save-excursion here is in case the user marks and copies a
        ! region in order to specify the minibuffer input.
        saved_point = point()
        if (regexp_flag) then
            call read_regexp(full_prompt, from, status, history="minibuffer_history")
        else
            call query_replace_read_from_suggestions(suggestions, nsuggestions)
            saved_add = history_add_new_input
            history_add_new_input = .false.
            call read_from_minibuffer(full_prompt, from, status, history="minibuffer_history", &
                                      defaults=suggestions(1:nsuggestions))
            history_add_new_input = saved_add
        end if
        call goto_char(saved_point)
        call set(sym("minibuffer_history"), saved_history, st)
        if (.not. status%ok()) return
        if (len(from) == 0 .and. allocated(query_replace_defaults)) then
            if (size(query_replace_defaults) > 0) then
                to = query_replace_defaults(1)%to
                from = query_replace_defaults(1)%from
                have_to = .true.
                call query_replace_compile_replacement(to, regexp_flag, counted, status)
                return
            end if
        end if
        if (have_separator .and. len(query_replace_from_to_separator) > 0) then
            at = index(from, query_replace_from_to_separator)
            if (at > 0) then
                to = from(at + len(query_replace_from_to_separator):)
                from = from(1:at - 1)
                have_to = .true.
            end if
        end if
        call add_to_history(query_replace_from_history_variable, from, keep_all=.true.)
        ! Warn if user types \n or \t, but don't reject the input.
        if (regexp_flag) then
            at = find_directive(from, 1, "nt")
            if (at > 0) then
                if (from(at + 1:at + 1) == "n") then
                    call message(format_message("Note: `\n' here doesn't match a newline; to do that, type C-q C-j instead"))
                else
                    call message(format_message("Note: `\t' here doesn't match a tab; to do that, just type TAB"))
                end if
                call sit_for(2000)
            end if
        end if
        if (.not. have_to) return
        call add_to_history(query_replace_to_history_variable, to, keep_all=.true.)
        call push_defaults(from, to)
        call query_replace_compile_replacement(to, regexp_flag, counted, status)
    end subroutine query_replace_read_from

    !> Query and return the TO argument of a `query_replace' operation.
    !> Prompt with PROMPT.  REGEXP-FLAG non-nil means the response
    !> should a regexp.  `counted' says whether TO uses `\#'.  Port of
    !> replace.el:query-replace-read-to.
    recursive subroutine query_replace_read_to(from, prompt, regexp_flag, to, counted, status)
        character(len=*), intent(in) :: from, prompt
        logical, intent(in) :: regexp_flag
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: counted
        type(status_type), intent(out) :: status
        logical :: saved_add
        counted = .false.
        saved_add = history_add_new_input
        history_add_new_input = .false.
        call read_from_minibuffer(prompt//" "//query_replace_descr(from)//" with: ", to, status, &
                                  history=query_replace_to_history_variable, default=from)
        history_add_new_input = saved_add
        if (.not. status%ok()) return
        call add_to_history(query_replace_to_history_variable, to, keep_all=.true.)
        call push_defaults(from, to)
        call query_replace_compile_replacement(to, regexp_flag, counted, status)
    end subroutine query_replace_read_to

    !> Read the arguments of a query-replace command: FROM, TO, whether to
    !> replace whole words only (DELIMITED: a prefix argument other than
    !> `-') and whether to go BACKWARD (the prefix argument `-').  Unless
    !> NOERROR, a read-only buffer is an error.  Port of
    !> replace.el:query-replace-read-args.
    recursive subroutine query_replace_read_args(prompt, regexp_flag, from, to, delimited, backward, counted, &
                                                 status, noerror)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: regexp_flag
        character(len=:), allocatable, intent(out) :: from, to
        logical, intent(out) :: delimited, backward, counted
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        logical :: have_to
        delimited = .false.
        backward = .false.
        counted = .false.
        to = ""
        from = ""
        status = status_type()
        if (.not. present_true(noerror)) then
            call barf_if_buffer_read_only(status)
            if (.not. status%ok()) return
        end if
        delimited = .not. nilp(current_prefix_arg) .and. .not. value_eq(current_prefix_arg, sym("-"))
        backward = value_eq(current_prefix_arg, sym("-"))
        call query_replace_read_from(prompt, regexp_flag, from, to, have_to, counted, status)
        if (.not. status%ok()) return
        if (.not. have_to) call query_replace_read_to(from, prompt, regexp_flag, to, counted, status)
    end subroutine query_replace_read_args

    logical function present_true(x)
        logical, intent(in), optional :: x
        present_true = .false.
        if (present(x)) present_true = x
    end function present_true

    !> Search for the next occurrence of SEARCH-STRING to replace.  Port
    !> of replace.el:replace-search with isearch-search-fun-default
    !> (non-incremental: no lax word search).
    logical function replace_search(search_string, limit, regexp_flag, delimited_flag, case_fold, status, &
                                    backward) result(found)
        character(len=*), intent(in) :: search_string
        integer(int64), intent(in) :: limit
        logical, intent(in) :: regexp_flag, delimited_flag, case_fold
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: regexp, saved_spaces
        integer(int64) :: pos
        logical :: had_spaces, saved_fold
        if (delimited_flag) then
            regexp = word_search_regexp(search_string, .false.)
        else if (regexp_flag) then
            regexp = search_string
        else
            regexp = regexp_quote(search_string)
        end if
        had_spaces = allocated(search_spaces_regexp)
        if (had_spaces) saved_spaces = search_spaces_regexp
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        if (merge(replace_regexp_lax_whitespace, replace_lax_whitespace, regexp_flag) .and. &
            allocated(search_whitespace_regexp)) search_spaces_regexp = search_whitespace_regexp
        saved_fold = case_fold_search%value
        case_fold_search = case_fold
        if (present_true(backward)) then
            if (limit > 0) then
                call re_search_backward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
            else
                call re_search_backward(regexp, pos, status, noerror=SEARCH_NOERROR)
            end if
        else
            if (limit > 0) then
                call re_search_forward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(regexp, pos, status, noerror=SEARCH_NOERROR)
            end if
        end if
        case_fold_search = saved_fold
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        if (had_spaces) search_spaces_regexp = saved_spaces
        found = status%ok() .and. pos > 0
    end function replace_search

    !> Make a replacement with `replace_match', editing `\?'.
    !> FIXEDCASE, LITERAL are passed to `replace_match' (which see).
    !> After possibly editing it (if `\?' is present), NEWTEXT is also
    !> passed to `replace_match'.  If NOEDIT is true, no check for `\?'
    !> is made (to save time).
    !> MATCH-DATA is used for the replacement, and is a data structure
    !> as returned from the `match_data' function.  BACKWARD is
    !> used to reverse the replacement direction.
    !>
    !> The return value is non-nil if there has been no `\?' or NOEDIT was
    !> passed in.  If LITERAL is set, no checking is done, anyway.  Port
    !> of replace.el:replace-match-maybe-edit (the edited text is read
    !> with point at its end).
    recursive logical function replace_match_maybe_edit(newtext, fixedcase, literal, noedit, data, status, &
                                                        backward) result(unedited)
        character(len=*), intent(in) :: newtext
        logical, intent(in) :: fixedcase, literal, noedit
        integer(int64), intent(in) :: data(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: text, edited
        integer :: at
        unedited = noedit
        text = newtext
        status = status_type()
        if (.not. (literal .or. noedit)) then
            unedited = .true.
            at = find_directive(text, 1, "?")
            do while (at > 0)
                call read_string("Edit replacement string: ", edited, status, &
                                 initial=text(1:at - 1)//text(at + 2:), position=1 + schars(text(1:at - 1)))
                if (.not. status%ok()) return
                text = edited
                unedited = .false.
                at = find_directive(text, 1, "?")
            end do
        end if
        call set_match_data(data)
        call replace_match(text, status, fixedcase, literal)
        if (.not. status%ok()) return
        ! `replace_match' leaves point at the end of the replacement text,
        ! so move point to the beginning when replacing backward.
        if (present_true(backward)) call goto_char(data(1))
    end function replace_match_maybe_edit

    !> Push an element on perform-replace's stack.  Port of
    !> replace.el:replace--push-stack.
    subroutine push_stack(stack, nstack, replaced, search_str, next_replace, next_replacement, again, again_match)
        type(stack_entry_type), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: nstack
        logical, intent(in) :: replaced
        character(len=:), allocatable, intent(in) :: search_str, next_replace
        character(len=*), intent(in) :: next_replacement
        integer, intent(in) :: again
        integer(int64), intent(in) :: again_match(:)
        type(stack_entry_type), allocatable :: grown(:)
        type(status_type) :: status
        integer :: k
        if (.not. allocated(stack)) allocate (stack(16))
        if (nstack == size(stack)) then
            allocate (grown(2*size(stack)))
            do k = 1, nstack
                grown(k) = stack(k)
            end do
            call move_alloc(grown, stack)
        end if
        nstack = nstack + 1
        stack(nstack)%point = point()
        stack(nstack)%replaced = replaced
        if (replaced) then
            ! If the replacement has already happened, all we need is the
            ! current match start and end.
            stack(nstack)%data = [match_beginning(0, status), match_end(0, status)]
        else
            stack(nstack)%data = match_data()
        end if
        stack(nstack)%search_str = ""
        if (allocated(search_str)) stack(nstack)%search_str = search_str
        stack(nstack)%next_replace = ""
        if (allocated(next_replace)) stack(nstack)%next_replace = next_replace
        stack(nstack)%next_replacement = next_replacement
        stack(nstack)%again = again
        stack(nstack)%again_match = again_match
    end subroutine push_stack

    !> The match data after `looking_at(regexp)' at `pos', point left
    !> where it was (save-excursion).
    function match_data_at(pos, regexp) result(data)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: regexp
        integer(int64), allocatable :: data(:)
        integer(int64) :: opoint
        type(status_type) :: status
        logical :: matched
        opoint = point()
        call goto_char(pos)
        matched = looking_at(regexp, status)
        data = match_data()
        call goto_char(opoint)
    end function match_data_at

    !> Subroutine of `query_replace'.  Its complexity handles interactive
    !> queries.  Don't use this in your own program unless you want to
    !> query and set the mark just as `query_replace' does.  Instead,
    !> write a simple loop like this:
    !>
    !>   do
    !>       call re_search_forward("foo[ \t]+bar", found, status, noerror=SEARCH_NOERROR)
    !>       if (found == 0) exit
    !>       call replace_match("foobar", status)
    !>   end do
    !>
    !> which will run faster and probably do exactly what you want.  Please
    !> see the documentation of `replace_match' to find out how to simulate
    !> `case_replace'.
    !>
    !> This function returns nil if there were no matches to make, or
    !> the user canceled the call.
    !>
    !> REPLACEMENTS is either a string, a list of strings, or a cons cell
    !> containing a function and its first argument.  The function is
    !> called to generate each replacement like this:
    !>   (funcall (car replacements) (cdr replacements) replace-count)
    !> It must return a string.
    !>
    !> In Fortmacs, REPLACEMENTS is the string `replacements', unless
    !> `replacement_list' (a list of strings, each used REPEAT-COUNT times
    !> in turn) or `replacement_fn' (called with the replacement count) is
    !> given; `counted' means the string uses `\#' (see
    !> `query_replace_compile_replacement').  START and END give the
    !> region to operate on.  `result' is the return value.  Port of
    !> replace.el:perform-replace (no MAP, no noncontiguous regions).
    recursive subroutine perform_replace(from_string, replacements, query_flag, regexp_flag, delimited_flag, &
                                         status, repeat_count, start, end, backward, replacement_list, &
                                         replacement_fn, counted, result)
        character(len=*), intent(in) :: from_string, replacements
        logical, intent(in) :: query_flag, regexp_flag, delimited_flag
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: repeat_count, start, end
        logical, intent(in), optional :: backward, counted
        type(value_type), intent(in), optional :: replacement_list
        procedure(replacement_function), optional :: replacement_fn
        logical, intent(out), optional :: result
        type(stack_entry_type), allocatable :: stack(:)
        type(stack_entry_type) :: elt
        type(value_type), allocatable :: items(:)
        type(value_type) :: key
        type(handle_type) :: limit_marker, opos
        type(status_type) :: st
        integer(int64), allocatable :: real_data(:), again_match(:)
        integer(int64) :: limit, opoint, skip_read_only_count, rotate_count, repeat, ro_pos
        character(len=:), allocatable :: next_replacement, search_string_replaced, next_replacement_replaced, &
                                         presentation, msg_prefix, last_replacement, undo_search, help_text, &
                                         edited
        logical :: fold, saved_fold, nocasify, literal, noedit, keep_going, last_was_undo, &
                   last_was_act_and_show, update_stack, nonempty_match, have_limit, back, found, done, &
                   replaced, querying, use_items, use_fn, is_counted, unread, matched, ro_found
        integer :: again, def, nstack, item, stack_idx, stack_len, num_replacements
        status = status_type()
        back = present_true(backward)
        is_counted = present_true(counted)
        querying = query_flag
        if (present(result)) result = .false.
        fold = case_fold_search%value
        if (case_fold_search%value .and. search_upper_case /= UPPER_CASE_NIL) then
            fold = isearch_no_upper_case_p(from_string, regexp_flag)
        end if
        nocasify = .not. (case_replace .and. fold)
        literal = .not. regexp_flag
        noedit = .false.
        keep_going = .true.
        nstack = 0
        last_was_undo = .false.
        last_was_act_and_show = .false.
        update_stack = .true.
        replace_count = 0
        skip_read_only_count = 0
        again = AGAIN_T
        allocate (again_match(0), real_data(0))
        unread = .false.
        if (querying) then
            msg_prefix = "Query replacing "
            if (back) msg_prefix = msg_prefix//"backward "
            if (delimited_flag) msg_prefix = msg_prefix//"word "
            if (regexp_flag) msg_prefix = msg_prefix//"regexp "
        end if
        ! If region is active, in Transient Mark mode, operate on region.
        limit = 0
        have_limit = .false.
        if (present(start) .and. present(end)) then
            have_limit = .true.
            call make_marker(limit_marker)
            if (back) then
                call set_marker(limit_marker, min(start, end), st)
                call goto_char(max(start, end))
            else
                call set_marker(limit_marker, max(start, end), st)
                call goto_char(min(start, end))
            end if
            call deactivate_mark()
        end if
        next_replacement = replacements
        use_items = .false.
        use_fn = present(replacement_fn)
        if (.not. use_fn .and. present(replacement_list)) then
            call list_elements(replacement_list, items)
            use_items = size(items) > 0
            ! This is a hand-made `iterator'.
            repeat = 1
            if (present(repeat_count)) repeat = max(1_int64, repeat_count)
            rotate_count = repeat
            item = 1
        end if
        call push_mark()
        call undo_boundary()
        saved_fold = case_fold_search%value
        case_fold_search = fold
        ! Loop finding occurrences that perhaps should be replaced.
        main: do while (keep_going)
            if (have_limit) call marker_position(limit_marker, limit, st)
            if (back) then
                if (bobp()) exit main
                if (have_limit .and. point() <= limit) exit main
            else
                if (eobp()) exit main
                if (have_limit .and. point() >= limit) exit main
            end if
            ! Use the next match if it is already known; otherwise, search
            ! for a match after moving forward one char if progress is
            ! required.
            select case (again)
            case (AGAIN_DATA)
                if (back) then
                    call goto_char(again_match(1))
                else
                    call goto_char(again_match(2))
                end if
                real_data = again_match
                found = .true.
            case (AGAIN_T)
                ! MATCH-AGAIN non-nil means accept an adjacent match.
                found = replace_search(from_string, merge(limit, 0_int64, have_limit), regexp_flag, &
                                       delimited_flag, fold, status, back)
                if (.not. status%ok()) exit main
                if (found) real_data = match_data()
            case default
                found = .false.
                if (back) then
                    if (point() - 1 > point_min()) found = .not. have_limit .or. point() - 1 > limit
                else
                    if (point() + 1 < point_max()) found = .not. have_limit .or. point() + 1 < limit
                end if
                if (found) then
                    ! If not accepting adjacent matches, move one char to
                    ! the right before searching again.  Undo the motion if
                    ! the search fails.
                    opoint = point()
                    call forward_char(st, merge(-1_int64, 1_int64, back))
                    found = replace_search(from_string, merge(limit, 0_int64, have_limit), regexp_flag, &
                                           delimited_flag, fold, status, back)
                    if (.not. status%ok()) exit main
                    if (found) then
                        real_data = match_data()
                    else
                        call goto_char(opoint)
                    end if
                end if
            end select
            if (.not. found) exit main
            ! Record whether the match is nonempty, to avoid an infinite
            ! loop repeatedly matching the same empty string.
            nonempty_match = real_data(1) /= real_data(2)
            ! If the match is empty, record that the next one can't be
            ! adjacent.  Otherwise, if matching a regular expression, do
            ! the next match now, since the replacement for this match may
            ! affect whether the next match is adjacent to this one.  If
            ! that match is empty, don't use it.
            again = AGAIN_NIL
            if (nonempty_match) then
                if (.not. regexp_flag) then
                    again = AGAIN_T
                else
                    if (back) then
                        matched = looking_back(from_string, st)
                    else
                        matched = looking_at(from_string, st)
                    end if
                    if (matched) then
                        again_match = match_data()
                        if (again_match(1) /= again_match(2)) again = AGAIN_DATA
                    end if
                end if
            end if
            ! Optionally ignore matches that have a read-only property.
            if (query_replace_skip_read_only) then
                call text_property_not_all(real_data(1), real_data(2), sym("read_only"), nil(), ro_pos, &
                                           ro_found, st)
                if (ro_found) then
                    skip_read_only_count = skip_read_only_count + 1
                    cycle main
                end if
            end if
            ! Calculate the replacement string, if necessary.
            if (use_fn) then
                call set_match_data(real_data)
                next_replacement = replacement_fn(replace_count)
            else if (use_items) then
                ! replace-loop-through-replacements
                if (rotate_count == replace_count) then
                    rotate_count = replace_count + repeat
                    item = item + 1
                    if (item > size(items)) item = 1
                end if
                next_replacement = xstring(items(item))
            else if (is_counted) then
                next_replacement = replace_expand_count(replacements, replace_count)
            end if
            if (.not. querying) then
                call act(next_replacement, nocasify)
                if (.not. status%ok()) exit main
                replace_count = replace_count + 1
                cycle main
            end if
            call undo_boundary()
            done = .false.
            replaced = .false.
            ! Loop reading commands until one of them sets done, which
            ! means it has finished handling this occurrence.  Any command
            ! that sets `done' should leave behind proper match data for the
            ! stack.  Commands not setting `done' need to adjust
            ! `real_match_data'.
            do while (.not. done)
                call set_match_data(real_data)
                ! Obtain the matched groups: needed only when regexp-flag
                ! non nil.
                if (last_was_undo .and. regexp_flag) then
                    last_was_undo = .false.
                    real_data = match_data_at(real_data(1), from_string)
                end if
                ! Matched string and next-replacement-replaced stored in
                ! stack.
                call buffer_substring(real_data(1), real_data(2), search_string_replaced, st)
                call set_match_data(real_data)
                presentation = match_substitute_replacement(next_replacement, st, nocasify, literal)
                if (.not. st%ok()) presentation = next_replacement
                ! (query-replace-descr only adds display properties: the
                ! text stays the replacement.)
                next_replacement_replaced = presentation
                if (.not. query_replace_show_replacement) presentation = next_replacement
                call read_key(msg_prefix//query_replace_descr(from_string)//" with "// &
                              query_replace_descr(presentation)//": (? for help) ", key, status)
                if (.not. status%ok()) exit main
                ! Necessary in case something happens during read-event
                ! that clobbers the match data.
                call set_match_data(real_data)
                def = query_replace_answer(key)
                select case (def)
                case (ANSWER_HELP)
                    help_text = "Query replacing "
                    if (back) help_text = help_text//"backward "
                    if (delimited_flag) help_text = help_text//"word "
                    if (regexp_flag) help_text = help_text//"regexp "
                    call with_help_window(help_text//from_string//" with "//next_replacement//"."//LF//LF// &
                                          query_replace_help, st)
                case (ANSWER_EXIT)
                    keep_going = .false.
                    done = .true.
                case (ANSWER_BACKUP)
                    if (nstack > 0) then
                        elt = stack(nstack)
                        nstack = nstack - 1
                        call goto_char(elt%point)
                        replaced = elt%replaced
                        real_data = elt%data
                        next_replacement = elt%next_replacement
                        again = elt%again
                        again_match = elt%again_match
                    else
                        call message("No previous match")
                        call ding()
                        call sit_for(1000)
                    end if
                case (ANSWER_UNDO, ANSWER_UNDO_ALL)
                    if (nstack == 0) then
                        call message("Nothing to undo")
                        call ding()
                        call sit_for(1000)
                    else
                        stack_idx = 0
                        stack_len = nstack
                        num_replacements = 0
                        do while (stack_idx < stack_len .and. nstack > 0 .and. &
                                  (.not. replaced .or. last_was_act_and_show))
                            elt = stack(nstack - stack_idx)
                            stack_idx = stack_idx + 1
                            replaced = elt%replaced
                            ! Bind swapped values (search-string <--> replacement)
                            if (replaced) then
                                undo_search = elt%next_replace
                                last_replacement = elt%search_str
                            else
                                undo_search = elt%search_str
                                last_replacement = elt%next_replace
                            end if
                            search_string_replaced = undo_search
                            next_replacement_replaced = last_replacement
                            last_was_act_and_show = .false.
                            if (stack_idx == stack_len .and. .not. replaced .and. num_replacements == 0) then
                                call message("Nothing to undo")
                                call ding()
                                call sit_for(1000)
                            end if
                            if (replaced) then
                                nstack = nstack - stack_idx
                                call goto_char(elt%point)
                                call set_match_data(elt%data)
                                ! We must quote the string (Bug#37073)
                                real_data = match_data_at(elt%data(1), regexp_quote(undo_search))
                                ! Undo must preserve case (Bug#31073).
                                call act(last_replacement, .true.)
                                if (.not. status%ok()) exit main
                                replace_count = replace_count - 1
                                if (regexp_flag) then
                                    real_data = match_data_at(match_beginning(0, st), last_replacement)
                                else
                                    real_data = match_data_at(match_beginning(0, st), regexp_quote(last_replacement))
                                end if
                                if (regexp_flag) next_replacement = elt%next_replace
                                ! Set replaced nil to keep in loop
                                if (def == ANSWER_UNDO_ALL) then
                                    replaced = .false.
                                    stack_len = stack_len - stack_idx
                                    stack_idx = 0
                                    num_replacements = num_replacements + 1
                                end if
                            end if
                        end do
                        if (def == ANSWER_UNDO_ALL .and. num_replacements /= 0) then
                            if (num_replacements == 1) then
                                call message("Undid 1 replacement")
                            else
                                call message("Undid "//number_to_string(int(num_replacements, int64))// &
                                             " replacements")
                            end if
                            call ding()
                            call sit_for(1000)
                        end if
                        replaced = .false.
                        last_was_undo = .true.
                        last_was_act_and_show = .false.
                    end if
                case (ANSWER_ACT)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                    end if
                    done = .true.
                    replaced = .true.
                    update_stack = .not. last_was_act_and_show
                case (ANSWER_ACT_AND_EXIT)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                    end if
                    keep_going = .false.
                    done = .true.
                    replaced = .true.
                case (ANSWER_ACT_AND_SHOW)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                        real_data = match_data()
                        replaced = .true.
                        last_was_act_and_show = .true.
                        call push_stack(stack, nstack, replaced, search_string_replaced, &
                                        next_replacement_replaced, next_replacement, again, again_match)
                    end if
                case (ANSWER_AUTOMATIC)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                    end if
                    done = .true.
                    querying = .false.
                    replaced = .true.
                case (ANSWER_SKIP)
                    done = .true.
                    update_stack = .not. last_was_act_and_show
                case (ANSWER_RECENTER)
                    call recenter_top_bottom(nil(), st)
                case (ANSWER_EDIT)
                    opoint = point()
                    call make_marker(opos)
                    call set_marker(opos, opoint, st)
                    call goto_char(match_beginning(0, st))
                    ! save-excursion
                    opoint = point()
                    call recursive_edit_1(status)
                    if (.not. status%ok()) then
                        call free_marker(opos, st)
                        exit main
                    end if
                    call goto_char(opoint)
                    call marker_position(opos, opoint, st)
                    call goto_char(opoint)
                    call free_marker(opos, st)
                    ! Before we make the replacement, decide whether the
                    ! search string can match again just after this match.
                    if (regexp_flag .and. nonempty_match) then
                        again = AGAIN_NIL
                        if (looking_at(from_string, st)) then
                            again = AGAIN_DATA
                            again_match = match_data()
                        end if
                    end if
                case (ANSWER_EDIT_REPLACEMENT, ANSWER_EDIT_REPLACEMENT_EXACT_CASE)
                    if (def == ANSWER_EDIT_REPLACEMENT_EXACT_CASE) then
                        call read_string("Edit replacement string (exact case): ", edited, status, &
                                         initial=next_replacement)
                    else
                        call read_string("Edit replacement string: ", edited, status, initial=next_replacement)
                    end if
                    if (.not. status%ok()) exit main
                    next_replacement = edited
                    noedit = .false.
                    if (replaced) then
                        call set_match_data(real_data)
                    else
                        call act(next_replacement, def == ANSWER_EDIT_REPLACEMENT_EXACT_CASE .or. nocasify)
                        if (.not. status%ok()) exit main
                        replaced = .true.
                    end if
                    next_replacement_replaced = next_replacement
                    done = .true.
                case (ANSWER_DELETE_AND_EDIT)
                    call replace_match("", status, .true., .true.)
                    if (.not. status%ok()) exit main
                    call adjust_markers(real_data(1), real_data(2))
                    real_data = match_data()
                    opoint = point()
                    call recursive_edit_1(status)
                    if (.not. status%ok()) exit main
                    call goto_char(opoint)
                    replaced = .true.
                case (ANSWER_DIFF)
                    call message("The diff answer needs diff_mode, which is not ported yet")
                    call ding()
                    call sit_for(1000)
                case (ANSWER_SCROLL_UP, ANSWER_SCROLL_DOWN)
                    ! (commandp def t): (call-interactively def).
                    if (def == ANSWER_SCROLL_UP) then
                        call scroll_up(nil(), status)
                    else
                        call scroll_down(nil(), status)
                    end if
                    if (.not. status%ok()) exit main
                case (ANSWER_QUIT)
                    call set_error(status, FM_QUIT)
                    exit main
                case default
                    ! Any other key (and exit-prefix) exits, and is then
                    ! re-executed as a normal key sequence.  (The commands
                    ! scroll-other-window and scroll-other-window-down are
                    ! not ported: Deferred.)
                    this_command = sym("mode_exited")
                    keep_going = .false.
                    call unread_command_events([key])
                    unread = .true.
                    done = .true.
                end select
            end do
            ! Record previous position for ^ when we move on.
            if (update_stack) call push_stack(stack, nstack, replaced, search_string_replaced, &
                                              next_replacement_replaced, next_replacement, again, again_match)
            if (allocated(next_replacement_replaced)) deallocate (next_replacement_replaced)
            if (allocated(search_string_replaced)) deallocate (search_string_replaced)
            last_was_act_and_show = .false.
        end do main
        case_fold_search = saved_fold
        if (have_limit) call free_marker(limit_marker, st)
        if (.not. status%ok()) return
        if (.not. unread) then
            help_text = "Replaced "//number_to_string(replace_count)//" occurrence"
            if (replace_count /= 1) help_text = help_text//"s"
            if (skip_read_only_count > 0) then
                help_text = help_text//" (skipped "//number_to_string(skip_read_only_count)//" read-only)"
            end if
            call message(help_text)
        end if
        if (present(result)) result = keep_going .and. nstack > 0

    contains

        ! (setq noedit (replace-match-maybe-edit TEXT FIXEDCASE literal noedit
        ! real-match-data backward)), then move the positions that are
        ! markers upstream (match-again and the match data of the stack).
        recursive subroutine act(text, fixedcase)
            character(len=*), intent(in) :: text
            logical, intent(in) :: fixedcase
            integer(int64) :: from, to
            from = real_data(1)
            to = real_data(2)
            noedit = replace_match_maybe_edit(text, fixedcase, literal, noedit, real_data, status, back)
            if (status%ok()) call adjust_markers(from, to)
        end subroutine act

        ! Adjust the stored positions for the replacement of [FROM, TO)
        ! by the current match, as replace_range adjusts markers.
        subroutine adjust_markers(from, to)
            integer(int64), intent(in) :: from, to
            type(status_type) :: s
            integer(int64) :: delta
            integer :: k
            delta = match_end(0, s) - match_beginning(0, s) - (to - from)
            call adjust(again_match, from, to, delta)
            do k = 1, nstack
                if (.not. stack(k)%replaced) call adjust(stack(k)%data, from, to, delta)
            end do
        end subroutine adjust_markers

        ! Move POSITIONS as markers move when [FROM, TO) is replaced by
        ! text DELTA characters longer.
        pure subroutine adjust(positions, from, to, delta)
            integer(int64), intent(inout) :: positions(:)
            integer(int64), intent(in) :: from, to, delta
            integer :: i
            do i = 1, size(positions)
                if (positions(i) <= 0) cycle
                if (positions(i) >= to) then
                    positions(i) = positions(i) + delta
                else if (positions(i) > from) then
                    positions(i) = from
                end if
            end do
        end subroutine adjust
    end subroutine perform_replace

    ! ------------------------------------------------------------------
    ! The commands
    ! ------------------------------------------------------------------

    !> Replace some occurrences of FROM_STRING with TO_STRING.
    !> As each match is found, the user must type a character saying
    !> what to do with it.  Type SPC or `y' to replace the match,
    !> DEL or `n' to skip and go to the next match.  For more directions,
    !> type C-h at that time.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> In interactive use, the prefix arg (true DELIMITED in
    !> non-interactive use), means replace only matches surrounded by
    !> word boundaries.  A negative prefix arg means replace backward.
    !>
    !> Use M-n to pull the last incremental search string to the minibuffer
    !> that reads FROM_STRING, or invoke replacements from incremental
    !> search with a key sequence like `C-s C-s M-%' to use its current
    !> search string as the string to replace.
    !>
    !> Matching is independent of case if both `case_fold_search'
    !> and `search_upper_case' are true (not UPPER_CASE_NIL) and FROM_STRING has no
    !> uppercase letters; if `search_upper_case' is UPPER_CASE_NIL, then whether
    !> matching ignores case depends on `case_fold_search' regardless
    !> of whether there are uppercase letters in FROM_STRING.
    !> Replacement transfers the case pattern of the old text to the
    !> new text, if both `case_fold_search' and `case_replace' are
    !> true and FROM_STRING has no uppercase letters.
    !> (Transferring the case pattern means that if the old text
    !> matched is all caps, or all of its words are capitalized, then its
    !> replacement is respectively upcased or capitalized.  For more
    !> details about this, see `replace_match'.)
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_lax_whitespace' is true, a space or spaces in the string
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> To customize possible responses, change the bindings in
    !> `query_replace_map'.
    !>
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> Arguments FROM_STRING, TO_STRING, DELIMITED, START, END and BACKWARD
    !> are passed to `perform_replace' (which see).  Port of
    !> replace.el:query-replace.
    recursive subroutine query_replace(from_string, to_string, status, delimited, start, end, backward)
        character(len=*), intent(in) :: from_string, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward
        integer(int64), intent(in), optional :: start, end
        call perform_replace(from_string, to_string, .true., .false., present_true(delimited), status, &
                             start=start, end=end, backward=backward)
    end subroutine query_replace

    !> Replace some things after point matching REGEXP with TO_STRING.
    !> As each match is found, the user must type a character saying
    !> what to do with it.  Type SPC or `y' to replace the match,
    !> DEL or `n' to skip and go to the next match.  For more directions,
    !> type C-h at that time.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> When invoked interactively, matching a newline with `\n' will not work;
    !> use `C-q C-j' instead.  To match a tab character (`\t'), just press `TAB'.
    !>
    !> Matching is independent of case if both `case_fold_search'
    !> and `search_upper_case' are true (not UPPER_CASE_NIL) and REGEXP has no uppercase
    !> letters; if `search_upper_case' is UPPER_CASE_NIL, then whether matching
    !> ignores case depends on `case_fold_search' regardless of whether
    !> there are uppercase letters in REGEXP.
    !> Replacement transfers the case pattern of the old text to the new
    !> text, if both `case_fold_search' and `case_replace' are true
    !> and REGEXP has no uppercase letters.  (Transferring the case pattern
    !> means that if the old text matched is all caps, or all of its words
    !> are capitalized, then its replacement is respectively upcased or
    !> capitalized.  For more details about this, see `replace_match'.)
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_regexp_lax_whitespace' is true, a space or spaces in the regexp
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> Third arg DELIMITED (prefix arg if interactive), if true, means replace
    !> only matches surrounded by word boundaries.  A negative prefix arg means
    !> replace backward.
    !>
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> In TO_STRING, `\&' stands for whatever matched the whole of REGEXP,
    !> and `\N' (where N is a digit) stands for whatever matched
    !> the Nth `\(...\)' (1-based) in REGEXP.  The `\(...\)' groups are
    !> counted from 1.
    !> `\?' lets you edit the replacement text in the minibuffer
    !> at the given position for each replacement.
    !>
    !> In interactive calls, `\#' in the replacement text stands for the
    !> number of replacements done so far (starting with zero); `counted'
    !> says that TO_STRING uses it.
    !>
    !> Arguments REGEXP, TO_STRING, DELIMITED, START, END, and BACKWARD
    !> are passed to `perform_replace' (which see).  Port of
    !> replace.el:query-replace-regexp (`\,' followed by an expression to
    !> evaluate is not applicable: there is no run-time evaluation).
    recursive subroutine query_replace_regexp(regexp, to_string, status, delimited, start, end, backward, counted)
        character(len=*), intent(in) :: regexp, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward, counted
        integer(int64), intent(in), optional :: start, end
        call perform_replace(regexp, to_string, .true., .true., present_true(delimited), status, &
                             start=start, end=end, backward=backward, counted=counted)
    end subroutine query_replace_regexp

    !> Replace some matches for REGEXP with various strings, in rotation.
    !> The second argument TO_STRINGS contains the replacement strings,
    !> separated by spaces.  This command works like
    !> `query_replace_regexp' except that each successive replacement
    !> uses the next successive replacement string, wrapping around from
    !> the last such string to the first.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> As each match is found, the user must type a character saying
    !> what to do with it.  Type SPC or `y' to replace the match,
    !> DEL or `n' to skip and go to the next match.  For more directions,
    !> type C-h at that time.
    !>
    !> A prefix argument N says to use each replacement string N times
    !> before rotating to the next.
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> Arguments REGEXP, START, and END are passed to `perform_replace'
    !> (which see).  Port of replace.el:map-query-replace-regexp.
    recursive subroutine map_query_replace_regexp(regexp, to_strings, status, n, start, end)
        character(len=*), intent(in) :: regexp, to_strings
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n, start, end
        type(value_type), allocatable :: items(:), grown(:)
        character(len=:), allocatable :: rest
        integer :: at
        allocate (items(0))
        rest = to_strings
        do while (len(rest) /= 0)
            at = index(rest, " ")
            allocate (grown(size(items) + 1))
            grown(1:size(items)) = items
            if (at > 0) then
                grown(size(grown)) = make_string(rest(1:at - 1))
                rest = rest(at + 1:)
            else
                grown(size(grown)) = make_string(rest)
                rest = ""
            end if
            call move_alloc(grown, items)
        end do
        call perform_replace(regexp, "", .true., .true., .false., status, repeat_count=n, start=start, end=end, &
                             replacement_list=make_list(items))
    end subroutine map_query_replace_regexp

    !> Replace occurrences of FROM_STRING with TO_STRING.
    !> Preserve case in each match if `case_replace' and `case_fold_search'
    !> are true and FROM_STRING has no uppercase letters.
    !> (Preserving case means that if the string matched is all caps, or capitalized,
    !> then its replacement is upcased or capitalized.)
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_lax_whitespace' is true, a space or spaces in the string
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> Third arg DELIMITED (prefix arg if interactive), if true, means replace
    !> only matches surrounded by word boundaries.  A negative prefix arg means
    !> replace backward.
    !>
    !> Operates on the region between START and END (if both are absent, from point
    !> to the end of the buffer).  Interactively, if Transient Mark mode is
    !> enabled and the mark is active, operates on the contents of the region;
    !> otherwise from point to the end of the buffer's accessible portion.
    !>
    !> Argument BACKWARD is passed to `perform_replace' (which see).
    !>
    !> This function is usually the wrong thing to use in a program.
    !> What you probably want is a loop like this:
    !>   do
    !>       call search_forward(FROM_STRING, found, status, noerror=SEARCH_NOERROR)
    !>       if (found == 0) exit
    !>       call replace_match(TO_STRING, status, literal=.true.)
    !>   end do
    !> which will run faster and will not set the mark or print anything.
    !> (You may need a more complex loop if FROM_STRING can match the null string
    !> and TO_STRING is also null.)  Port of replace.el:replace-string.
    recursive subroutine replace_string(from_string, to_string, status, delimited, start, end, backward)
        character(len=*), intent(in) :: from_string, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward
        integer(int64), intent(in), optional :: start, end
        call perform_replace(from_string, to_string, .false., .false., present_true(delimited), status, &
                             start=start, end=end, backward=backward)
    end subroutine replace_string

    !> Replace things after point matching REGEXP with TO_STRING.
    !> Preserve case in each match if `case_replace' and `case_fold_search'
    !> are true and REGEXP has no uppercase letters.
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_regexp_lax_whitespace' is true, a space or spaces in the regexp
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> Third arg DELIMITED (prefix arg if interactive), if true, means replace
    !> only matches surrounded by word boundaries.  A negative prefix arg means
    !> replace backward.
    !>
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> Argument BACKWARD is passed to `perform_replace' (which see).
    !>
    !> In TO_STRING, `\&' stands for whatever matched the whole of REGEXP,
    !> and `\N' (where N is a digit) stands for whatever matched
    !> the Nth `\(...\)' (1-based) in REGEXP.
    !> `\?' lets you edit the replacement text in the minibuffer
    !> at the given position for each replacement.
    !>
    !> In interactive calls, `\#' in the replacement text stands for
    !> `replace_count', the number of replacements occurred so far,
    !> starting from zero; `counted' says that TO_STRING uses it.
    !> `replacement_fn', if given, computes each replacement instead.
    !>
    !> This function is usually the wrong thing to use in a program.
    !> What you probably want is a loop like this:
    !>   do
    !>       call re_search_forward(REGEXP, found, status, noerror=SEARCH_NOERROR)
    !>       if (found == 0) exit
    !>       call replace_match(TO_STRING, status)
    !>   end do
    !> which will run faster and will not set the mark or print anything.
    !> Port of replace.el:replace-regexp.
    recursive subroutine replace_regexp(regexp, to_string, status, delimited, start, end, backward, counted, &
                                        replacement_fn)
        character(len=*), intent(in) :: regexp, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward, counted
        integer(int64), intent(in), optional :: start, end
        procedure(replacement_function), optional :: replacement_fn
        call perform_replace(regexp, to_string, .false., .true., present_true(delimited), status, &
                             start=start, end=end, backward=backward, counted=counted, &
                             replacement_fn=replacement_fn)
    end subroutine replace_regexp

    !> Delete lines containing matches for REGEXP.
    !> When called from a program (and usually when called interactively as
    !> well, see below), applies to the part of the buffer after point.
    !> The line point is in is deleted if and only if it contains a
    !> match for regexp starting after point.
    !>
    !> If REGEXP contains upper case characters (excluding those preceded by `\')
    !> and `search_upper_case' is true, the matching is case-sensitive.
    !>
    !> Second and third arg RSTART and REND specify the region to operate on.
    !> Lines partially contained in this region are deleted if and only if
    !> they contain a match entirely contained in it.
    !>
    !> Interactively, in Transient Mark mode when the mark is active, operate
    !> on the contents of the region.  Otherwise, operate from point to the
    !> end of (the accessible portion of) the buffer.  When calling this function
    !> from a program, you can pretend that it was called interactively by passing
    !> a true INTERACTIVE argument.
    !>
    !> If a match is split across lines, all the lines it lies in are deleted.
    !> They are deleted _before_ looking for the next match.  Hence, a match
    !> starting on the same line at which another match ended is ignored.
    !>
    !> Return the number of deleted matching lines.  When called interactively,
    !> also print the number.
    !>
    !> If you want to not just delete the lines, but also add them to
    !> the kill ring, use the \[kill_matching_lines] command instead.
    !> Port of replace.el:flush-lines (the command's argument reading is
    !> Deferred).
    recursive subroutine flush_lines(regexp, count, status, rstart, rend, interactive)
        use cmds_fm, only: forward_line
        use editfns_fm, only: delete_region
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in), optional :: interactive
        type(handle_type) :: end_marker, opoint
        integer(int64) :: limit, found, beg, shortage
        logical :: inter, saved_fold
        type(status_type) :: st
        count = 0
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        call make_marker(end_marker)
        if (present(rstart) .and. present(rend)) then
            call goto_char(min(rstart, rend))
            call set_marker(end_marker, max(rstart, rend), st)
        else if (inter .and. use_region_p()) then
            call region_end(limit, st)
            call set_marker(end_marker, limit, st)
            call region_beginning(limit, st)
            call goto_char(limit)
        else
            call set_marker(end_marker, point_max(), st)
        end if
        saved_fold = case_fold_search%value
        if (case_fold_search%value .and. search_upper_case /= UPPER_CASE_NIL) then
            case_fold_search%value = isearch_no_upper_case_p(regexp, .true.)
        end if
        call make_marker(opoint)
        call set_marker(opoint, point(), st)
        do
            call marker_position(end_marker, limit, st)
            if (point() >= limit) exit
            call re_search_forward(regexp, found, status, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit
            call goto_char(match_beginning(0, st))
            shortage = forward_line(0_int64)
            beg = point()
            call goto_char(match_end(0, st))
            shortage = forward_line(1_int64)
            call delete_region(beg, point(), status)
            if (.not. status%ok()) exit
            count = count + 1
        end do
        case_fold_search%value = saved_fold
        call marker_position(opoint, beg, st)
        call goto_char(beg)
        call free_marker(opoint, st)
        call free_marker(end_marker, st)
        if (.not. status%ok()) return
        if (inter) then
            if (count == 1) then
                call message("Deleted 1 matching line")
            else
                call message("Deleted "//number_to_string(count)//" matching lines")
            end if
        end if
    end subroutine flush_lines

    !> The interactive prompt of a replace command: BASE, then " backward"
    !> or " word" for the prefix argument, then WHAT and " in region".
    function command_prompt(base, what) result(prompt)
        character(len=*), intent(in) :: base, what
        character(len=:), allocatable :: prompt
        prompt = base
        if (.not. nilp(current_prefix_arg)) then
            if (value_eq(current_prefix_arg, sym("-"))) then
                prompt = prompt//" backward"
            else
                prompt = prompt//" word"
            end if
        end if
        prompt = prompt//what
        if (use_region_p()) prompt = prompt//" in region"
    end function command_prompt

    ! The interactive commands: read the arguments, then operate on the
    ! region when it is in use.
    recursive subroutine run_command(base, what, regexp_flag, query, status)
        character(len=*), intent(in) :: base, what
        logical, intent(in) :: regexp_flag, query
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        logical :: delimited, backward, counted
        integer(int64) :: b, e
        type(status_type) :: st
        call query_replace_read_args(command_prompt(base, what), regexp_flag, from, to, delimited, backward, &
                                     counted, status)
        if (.not. status%ok()) return
        if (use_region_p()) then
            call region_beginning(b, st)
            call region_end(e, st)
            call perform_replace(from, to, query, regexp_flag, delimited, status, start=b, end=e, &
                                 backward=backward, counted=counted)
        else
            call perform_replace(from, to, query, regexp_flag, delimited, status, backward=backward, &
                                 counted=counted)
        end if
    end subroutine run_command

    recursive subroutine cmd_query_replace(status)
        type(status_type), intent(out) :: status
        call run_command("Query replace", "", .false., .true., status)
    end subroutine cmd_query_replace

    recursive subroutine cmd_query_replace_regexp(status)
        type(status_type), intent(out) :: status
        call run_command("Query replace", " regexp", .true., .true., status)
    end subroutine cmd_query_replace_regexp

    recursive subroutine cmd_replace_string(status)
        type(status_type), intent(out) :: status
        call run_command("Replace", " string", .false., .false., status)
    end subroutine cmd_replace_string

    recursive subroutine cmd_replace_regexp(status)
        type(status_type), intent(out) :: status
        call run_command("Replace", " regexp", .true., .false., status)
    end subroutine cmd_replace_regexp

    recursive subroutine cmd_map_query_replace_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        integer(int64) :: b, e
        type(status_type) :: st
        call read_regexp("Map query replace (regexp): ", from, status, history=query_replace_from_history_variable)
        if (.not. status%ok()) return
        call read_from_minibuffer("Query replace "//query_replace_descr(from)//" with (space-separated strings): ", &
                                  to, status, history=query_replace_to_history_variable, default=from)
        if (.not. status%ok()) return
        if (use_region_p()) then
            call region_beginning(b, st)
            call region_end(e, st)
            if (nilp(current_prefix_arg)) then
                call map_query_replace_regexp(from, to, status, start=b, end=e)
            else
                call map_query_replace_regexp(from, to, status, n=prefix_numeric_value(current_prefix_arg), &
                                              start=b, end=e)
            end if
        else if (nilp(current_prefix_arg)) then
            call map_query_replace_regexp(from, to, status)
        else
            call map_query_replace_regexp(from, to, status, n=prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_map_query_replace_regexp

    !> Give the variables their defaults and register the commands.
    subroutine syms_of_replace()
        integer, parameter :: C = FACE_CLASS_COLOR, M = FACE_CLASS_MONO, LIGHT = FACE_BACKGROUND_LIGHT, &
                              DARK = FACE_BACKGROUND_DARK
        type(status_type) :: st
        type(value_type) :: history
        ! (faces.el is loaded before replace.el.)
        call init_faces()
        call match%define("match", "Face used to highlight matches permanently.")
        call match%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                              face_attrs(background="khaki1"))
        call match%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                              face_attrs(background="RoyalBlue3"))
        call match%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[LIGHT]), &
                              face_attrs(background="yellow", foreground="black"))
        call match%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[DARK]), &
                              face_attrs(background="blue", foreground="white"))
        call match%spec_entry(display_when(types=[character(len=3) :: "tty"], classes=[M]), &
                              face_attrs(inverse_video=.true.))
        call match%spec_entry(display_when(), face_attrs(background="gray"))
        query_replace_from_to_separator = " → "
        query_replace_from_history_variable = "query_replace_history"
        call defvar_nil("query_replace_history")
        call defvar_nil("regexp_history")
        query_replace_to_history_variable = "query_replace_history"
        ! Default history list for `query_replace' commands.
        call symbol_value(sym("query_replace_history"), history, st)
        if (.not. st%ok()) call set(sym("query_replace_history"), nil(), st)
        commands%query_replace%run => cmd_query_replace
        commands%query_replace_regexp%run => cmd_query_replace_regexp
        commands%map_query_replace_regexp%run => cmd_map_query_replace_regexp
        commands%replace_string%run => cmd_replace_string
        commands%replace_regexp%run => cmd_replace_regexp
    end subroutine syms_of_replace
    ! ------------------------------------------------------------------
    ! keep-lines and friends
    ! ------------------------------------------------------------------

    !> The regexp argument of `keep_lines' and friends: read with PROMPT.
    !> Port of replace.el:keep-lines-read-args.
    recursive subroutine keep_lines_read_args(prompt, regexp, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: regexp
        type(status_type), intent(out) :: status
        call read_regexp(prompt, regexp, status)
    end subroutine keep_lines_read_args

    !> The case-fold-search of the keep-lines commands for REGEXP.
    logical function lines_case_fold(regexp) result(fold)
        character(len=*), intent(in) :: regexp
        fold = case_fold_search%value
        if (case_fold_search%value .and. search_upper_case /= UPPER_CASE_NIL) then
            fold = isearch_no_upper_case_p(regexp, .true.)
        end if
    end function lines_case_fold

    !> Delete all lines except those containing matches for REGEXP.
    !> A match split across lines preserves all the lines it lies in.
    !> When called from a program (and usually interactively as well, see
    !> below) applies to all lines starting after point.
    !>
    !> If REGEXP contains upper case characters (excluding those preceded
    !> by `\') and `search_upper_case' is true, the matching is
    !> case-sensitive.
    !>
    !> Second and third arg RSTART and REND specify the region to operate
    !> on.  This command operates on (the accessible part of) all lines
    !> whose accessible part is entirely contained in the region determined
    !> by RSTART and REND.  (A newline ending a line counts as part of that
    !> line.)  If RSTART is given, REND also has to be given.
    !>
    !> Interactively, in Transient Mark mode when the mark is active,
    !> operate on all lines whose accessible part is entirely contained in
    !> the region.  Otherwise, the command applies to all lines starting
    !> after point.  When calling this function from a program, you can
    !> pretend that it was called interactively by passing a true
    !> INTERACTIVE argument.
    !>
    !> This function starts looking for the next match from the end of the
    !> previous match.  Hence, it ignores matches that overlap a previously
    !> found match.  Port of replace.el:keep-lines.
    recursive subroutine keep_lines(regexp, status, rstart, rend, interactive)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in), optional :: interactive
        type(handle_type) :: rend_m
        type(status_type) :: st
        integer(int64) :: start, end, limit, found, opoint, pos, shortage
        logical :: inter, saved_fold
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        call make_marker(rend_m)
        if (present(rstart) .and. present(rend)) then
            call goto_char(min(rstart, rend))
            opoint = point()
            call goto_char(max(rstart, rend))
            if (.not. (bolp() .or. eobp())) shortage = forward_line(0_int64)
            call set_marker(rend_m, point(), st)
            call goto_char(opoint)
        else if (inter .and. use_region_p()) then
            call region_end(pos, st)
            call goto_char(pos)
            if (.not. (bolp() .or. eobp())) shortage = forward_line(0_int64)
            call set_marker(rend_m, point(), st)
            call region_beginning(pos, st)
            call goto_char(pos)
        else
            call set_marker(rend_m, point_max(), st)
        end if
        opoint = point()
        if (.not. bolp()) shortage = forward_line(1_int64)
        start = point()
        saved_fold = case_fold_search%value
        case_fold_search = lines_case_fold(regexp)
        do
            call marker_position(rend_m, limit, st)
            if (point() >= limit) exit
            ! Start is first char not preserved by previous match.
            call re_search_forward(regexp, found, status, bound=limit, noerror=SEARCH_MOVE)
            if (.not. status%ok()) exit
            if (found == 0) then
                call delete_region(start, limit, status)
            else
                pos = point()
                call goto_char(match_beginning(0, st))
                shortage = forward_line(0_int64)
                end = point()
                call goto_char(pos)
                ! Now end is first char preserved by the new match.
                if (start < end) call delete_region(start, end, status)
            end if
            if (.not. status%ok()) exit
            pos = point()
            shortage = forward_line(1_int64)
            start = point()
            call goto_char(pos)
            ! If the match was empty, avoid matching again at same place.
            call marker_position(rend_m, limit, st)
            if (point() < limit .and. match_beginning(0, st) == match_end(0, st)) call goto_char(point() + 1)
        end do
        case_fold_search = saved_fold
        call goto_char(min(opoint, point_max()))
        call free_marker(rend_m, st)
    end subroutine keep_lines

    !> The command: read the regexp, then `keep_lines' (interactively).
    !> The interactive form of replace.el:keep-lines.
    !>
    !> Delete all lines except those containing matches for a regexp.
    recursive subroutine cmd_keep_lines(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call keep_lines_read_args("Keep lines containing match for regexp", regexp, status)
        if (status%ok()) call keep_lines(regexp, status, interactive=.true.)
    end subroutine cmd_keep_lines

    !> The command: read the regexp, then `flush_lines' (interactively).
    !> The interactive form of replace.el:flush-lines.
    !>
    !> Delete lines containing matches for a regexp.
    recursive subroutine cmd_flush_lines(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer(int64) :: count
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call keep_lines_read_args("Flush lines containing match for regexp", regexp, status)
        if (status%ok()) call flush_lines(regexp, count, status, interactive=.true.)
    end subroutine cmd_flush_lines

    !> The start and end marker of the keep-lines commands: RSTART and
    !> REND, the region interactively, else point to the end.
    subroutine lines_bounds(rend_m, rstart, rend, inter)
        type(handle_type), intent(out) :: rend_m
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in) :: inter
        type(status_type) :: st
        integer(int64) :: pos
        call make_marker(rend_m)
        if (present(rstart) .and. present(rend)) then
            call goto_char(min(rstart, rend))
            call set_marker(rend_m, max(rstart, rend), st)
        else if (inter .and. use_region_p()) then
            call region_end(pos, st)
            call set_marker(rend_m, pos, st)
            call region_beginning(pos, st)
            call goto_char(pos)
        else
            call set_marker(rend_m, point_max(), st)
        end if
    end subroutine lines_bounds

    !> Kill (COPY false) or copy (COPY true) the lines containing matches
    !> for REGEXP: the body of `kill_matching_lines' and
    !> `copy_matching_lines'.
    recursive subroutine kill_or_copy_matching_lines(regexp, copy, count, status, rstart, rend, inter)
        character(len=*), intent(in) :: regexp
        logical, intent(in) :: copy
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in) :: inter
        type(handle_type) :: rend_m, opoint
        type(status_type) :: st
        integer(int64) :: limit, found, beg, shortage
        logical :: saved_fold
        count = 0
        status = status_type()
        call lines_bounds(rend_m, rstart, rend, inter)
        saved_fold = case_fold_search%value
        case_fold_search = lines_case_fold(regexp)
        call make_marker(opoint)
        call set_marker(opoint, point(), st)
        do
            call marker_position(rend_m, limit, st)
            if (point() >= limit) exit
            call re_search_forward(regexp, found, status, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit
            if (count /= 0) last_command = sym("kill_region")
            call goto_char(match_beginning(0, st))
            shortage = forward_line(0_int64)
            beg = point()
            call goto_char(match_end(0, st))
            shortage = forward_line(1_int64)
            if (copy) then
                call copy_region_as_kill(beg, point(), status)
            else
                call kill_region(beg, point(), status)
            end if
            if (.not. status%ok()) exit
            count = count + 1
        end do
        case_fold_search = saved_fold
        call marker_position(opoint, beg, st)
        call goto_char(beg)
        call free_marker(opoint, st)
        call free_marker(rend_m, st)
    end subroutine kill_or_copy_matching_lines

    !> Kill lines containing matches for REGEXP.
    !>
    !> When called from a program (and usually when called interactively
    !> as well, see below), applies to the part of the buffer after point.
    !> The line point is in is killed if and only if it contains a match
    !> for REGEXP starting after point.
    !>
    !> If REGEXP contains upper case characters (excluding those preceded
    !> by `\') and `search_upper_case' is true, the matching is
    !> case-sensitive.
    !>
    !> Second and third args RSTART and REND specify the region to operate
    !> on.  Lines partially contained in this region are killed if and
    !> only if they contain a match entirely contained in the region.
    !>
    !> Interactively, in Transient Mark mode when the mark is active,
    !> operate on the contents of the region.  Otherwise, operate from
    !> point to the end of (the accessible portion of) the buffer.
    !>
    !> If a match is split across lines, all the lines it lies in are
    !> killed.  They are killed _before_ looking for the next match.
    !> Hence, a match starting on the same line at which another match
    !> ended is ignored.
    !>
    !> Return the number of killed matching lines.  When called
    !> interactively, also print the number.
    !>
    !> If you merely want to delete the lines, without adding them to the
    !> kill ring, the \[delete_matching_lines] command is faster.  Port of
    !> replace.el:kill-matching-lines.
    recursive subroutine kill_matching_lines(regexp, count, status, rstart, rend, interactive)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in), optional :: interactive
        logical :: inter
        inter = .false.
        if (present(interactive)) inter = interactive
        call kill_or_copy_matching_lines(regexp, .false., count, status, rstart, rend, inter)
        if (status%ok() .and. inter) call message(plural(count, "Killed", "matching line"))
    end subroutine kill_matching_lines

    !> Copy lines containing matches for REGEXP to the kill ring.
    !>
    !> When called from a program (and usually when called interactively
    !> as well, see below), applies to the part of the buffer after point.
    !> The line point is in is copied if and only if it contains a match
    !> for REGEXP starting after point.
    !>
    !> If REGEXP contains upper case characters (excluding those preceded
    !> by `\') and `search_upper_case' is true, the matching is
    !> case-sensitive.
    !>
    !> Second and third args RSTART and REND specify the region to operate
    !> on.  Lines partially contained in this region are copied if and
    !> only if they contain a match entirely contained in the region.
    !>
    !> Interactively, in Transient Mark mode when the mark is active,
    !> operate on the contents of the region.  Otherwise, operate from
    !> point to the end of (the accessible portion of) the buffer.
    !>
    !> If a match is split across lines, all the lines it lies in are
    !> copied.
    !>
    !> Return the number of copied matching lines.  When called
    !> interactively, also print the number.  Port of
    !> replace.el:copy-matching-lines.
    recursive subroutine copy_matching_lines(regexp, count, status, rstart, rend, interactive)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in), optional :: interactive
        logical :: inter
        inter = .false.
        if (present(interactive)) inter = interactive
        call kill_or_copy_matching_lines(regexp, .true., count, status, rstart, rend, inter)
        if (status%ok() .and. inter) call message(plural(count, "Copied", "matching line"))
    end subroutine copy_matching_lines

    !> "VERB N NOUN" or "VERB N NOUNs" (ngettext).
    function plural(n, verb, noun) result(text)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: verb, noun
        character(len=:), allocatable :: text
        text = verb//" "//number_to_string(n)//" "//noun
        if (n /= 1) text = text//"s"
    end function plural

    !> The command: read the regexp, then `kill_matching_lines'.  The
    !> interactive form of replace.el:kill-matching-lines.
    !>
    !> Kill lines containing matches for a regexp.
    recursive subroutine cmd_kill_matching_lines(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer(int64) :: count
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call keep_lines_read_args("Kill lines containing match for regexp", regexp, status)
        if (status%ok()) call kill_matching_lines(regexp, count, status, interactive=.true.)
    end subroutine cmd_kill_matching_lines

    !> The command: read the regexp, then `copy_matching_lines'.  The
    !> interactive form of replace.el:copy-matching-lines.
    !>
    !> Copy lines containing matches for a regexp to the kill ring.
    recursive subroutine cmd_copy_matching_lines(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer(int64) :: count
        call keep_lines_read_args("Copy lines containing match for regexp", regexp, status)
        if (status%ok()) call copy_matching_lines(regexp, count, status, interactive=.true.)
    end subroutine cmd_copy_matching_lines

    !> Print and return number of matches for REGEXP following point.
    !> When called from a program and INTERACTIVE is absent or false, just
    !> return the number, do not print it; if INTERACTIVE is true, the
    !> function behaves in all respects as if it had been called
    !> interactively.
    !>
    !> If REGEXP contains upper case characters (excluding those preceded
    !> by `\') and `search_upper_case' is true, the matching is
    !> case-sensitive.
    !>
    !> Second and third arg RSTART and REND specify the region to operate
    !> on.
    !>
    !> Interactively, in Transient Mark mode when the mark is active,
    !> operate on the contents of the region.  Otherwise, operate from
    !> point to the end of (the accessible portion of) the buffer.
    !>
    !> This function starts looking for the next match from the end of the
    !> previous match.  Hence, it ignores matches that overlap a previously
    !> found match.  Port of replace.el:how-many.
    recursive subroutine how_many(regexp, count, status, rstart, rend, interactive)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in), optional :: interactive
        type(status_type) :: st
        integer(int64) :: limit, found, opoint, pos
        logical :: inter, saved_fold
        count = 0
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        opoint = point()
        if (present(rstart)) then
            if (present(rend)) then
                call goto_char(min(rstart, rend))
                limit = max(rstart, rend)
            else
                call goto_char(rstart)
                limit = point_max()
            end if
        else if (inter .and. use_region_p()) then
            call region_end(limit, st)
            call region_beginning(pos, st)
            call goto_char(pos)
        else
            limit = point_max()
        end if
        saved_fold = case_fold_search%value
        case_fold_search = lines_case_fold(regexp)
        do while (point() < limit)
            call re_search_forward(regexp, found, status, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit
            ! Ensure forward progress on zero-length matches like "^$".
            if (match_beginning(0, st) == match_end(0, st) .and. .not. eobp()) call goto_char(point() + 1)
            count = count + 1
        end do
        case_fold_search = saved_fold
        call goto_char(opoint)
        if (status%ok() .and. inter) then
            if (count == 1) then
                call message("1 occurrence")
            else
                call message(number_to_string(count)//" occurrences")
            end if
        end if
    end subroutine how_many

    !> The command: read the regexp, then `how_many'.  The interactive
    !> form of replace.el:how-many.
    !>
    !> Print the number of matches for a regexp following point.
    recursive subroutine cmd_how_many(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer(int64) :: count
        call keep_lines_read_args("How many matches for regexp", regexp, status)
        if (status%ok()) call how_many(regexp, count, status, interactive=.true.)
    end subroutine cmd_how_many

    ! ------------------------------------------------------------------
    ! Occur buffers
    ! ------------------------------------------------------------------

    logical function same_buffer_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_buffer_handle = a%index == b%index .and. a%generation == b%generation
    end function same_buffer_handle

    !> The entry of buffer H in `occur_buffers' (0 for none), made with
    !> CREATE (in the slot of a dead buffer if there is one).
    integer function occur_entry(h, create) result(k)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: create
        type(occur_buffer_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(occur_buffers)) allocate (occur_buffers(0))
        do k = 1, size(occur_buffers)
            if (same_buffer_handle(occur_buffers(k)%buffer, h)) return
        end do
        k = 0
        if (.not. create) return
        do k = 1, size(occur_buffers)
            if (.not. buffer_live_p(occur_buffers(k)%buffer)) then
                call occur_reset_entry(k)
                occur_buffers(k)%buffer = h
                return
            end if
        end do
        n = size(occur_buffers)
        allocate (grown(n + 1))
        do k = 1, n
            grown(k) = occur_buffers(k)
        end do
        grown(n + 1)%buffer = h
        call move_alloc(grown, occur_buffers)
        k = n + 1
    end function occur_entry

    !> Free the targets of entry K.
    subroutine occur_free_targets(k)
        integer, intent(in) :: k
        type(status_type) :: st
        integer :: i, j
        if (.not. allocated(occur_buffers(k)%targets)) return
        do i = 1, occur_buffers(k)%ntargets
            if (.not. allocated(occur_buffers(k)%targets(i)%beg)) cycle
            do j = 1, size(occur_buffers(k)%targets(i)%beg)
                call free_marker(occur_buffers(k)%targets(i)%beg(j), st)
                call free_marker(occur_buffers(k)%targets(i)%end(j), st)
            end do
        end do
        deallocate (occur_buffers(k)%targets)
        occur_buffers(k)%ntargets = 0
    end subroutine occur_free_targets

    !> Free the region markers of SOURCES.
    subroutine occur_free_sources(sources)
        type(occur_source_type), intent(inout) :: sources(:)
        type(status_type) :: st
        integer :: i
        do i = 1, size(sources)
            if (.not. sources(i)%region) cycle
            if (sources(i)%start%index /= 0) call free_marker(sources(i)%start, st)
            if (sources(i)%end%index /= 0) call free_marker(sources(i)%end, st)
        end do
    end subroutine occur_free_sources

    !> Free what entry K holds and make it unused.
    subroutine occur_reset_entry(k)
        integer, intent(in) :: k
        type(occur_buffer_type) :: empty
        call occur_free_targets(k)
        if (allocated(occur_buffers(k)%sources)) call occur_free_sources(occur_buffers(k)%sources)
        occur_buffers(k) = empty
    end subroutine occur_reset_entry

    !> Forget the revert arguments and targets of the buffer being killed
    !> (kill-buffer-hook).  Port of replace.el:occur--garbage-collect-revert-args.
    recursive subroutine occur_garbage_collect_revert_args(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        k = occur_entry(current_buffer(), .false.)
        if (k /= 0) call occur_reset_entry(k)
    end subroutine occur_garbage_collect_revert_args

    !> Add a target, the matches BEG(i) to END(i) (markers given to the
    !> current buffer's targets), to the current buffer's targets; its
    !> index is the value of the `occur_target' text property.
    integer function occur_add_target(beg, end) result(idx)
        type(handle_type), intent(in) :: beg(:), end(:)
        type(occur_target_type), allocatable :: grown(:)
        integer :: k, n, i
        k = occur_entry(current_buffer(), .true.)
        if (.not. allocated(occur_buffers(k)%targets)) allocate (occur_buffers(k)%targets(16))
        n = occur_buffers(k)%ntargets
        if (n == size(occur_buffers(k)%targets)) then
            allocate (grown(2*n))
            do i = 1, n
                call move_alloc(occur_buffers(k)%targets(i)%beg, grown(i)%beg)
                call move_alloc(occur_buffers(k)%targets(i)%end, grown(i)%end)
            end do
            call move_alloc(grown, occur_buffers(k)%targets)
        end if
        n = n + 1
        occur_buffers(k)%targets(n)%beg = beg
        occur_buffers(k)%targets(n)%end = end
        occur_buffers(k)%ntargets = n
        idx = n
    end function occur_add_target

    ! ------------------------------------------------------------------
    ! Occur mode
    ! ------------------------------------------------------------------

    !> Major mode for output from \[occur].
    !> \<occur_mode_map>Move point to one of the items in this buffer,
    !> then use \[occur_mode_goto_occurrence] to go to the occurrence that
    !> the item refers to.  Alternatively, click \[occur_mode_mouse_goto]
    !> on an item to go to it.
    !>
    !> \{occur_mode_map}  Port of replace.el:occur-mode.
    recursive subroutine occur_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        call major_modes%occur_mode%begin(saved, status)
        if (.not. status%ok()) return
        call occur_mode_body()
        call major_modes%occur_mode%finish(saved, status)
    end subroutine occur_mode

    !> The body of `occur_mode' (also run by `occur_edit_mode' as its
    !> parent).
    subroutine occur_mode_body()
        buffers(cur)%read_only = .true.
        call revert_buffer_function%set_local(occur_revert_function)
        call kill_buffer_hook%add(occur_garbage_collect_revert_args, local=.true.)
        call set_next_error_function(occur_next_error)
    end subroutine occur_mode_body

    !> Major mode for editing *Occur* buffers.
    !> In this mode, changes to the *Occur* buffer are also applied to the
    !> originating buffer.
    !>
    !> To return to ordinary Occur mode, use \[occur_cease_edit].  Port of
    !> replace.el:occur-edit-mode.
    recursive subroutine occur_edit_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        call major_modes%occur_edit_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(cur)%read_only = .false.
        call after_change_functions%add(occur_after_change_function, local=.true.)
        call message(substitute_command_keys("Editing: Type \[occur_cease_edit] to return to Occur mode."))
        call major_modes%occur_edit_mode%finish(saved, status)
    end subroutine occur_edit_mode

    !> Switch from Occur Edit mode to Occur mode.  Port of
    !> replace.el:occur-cease-edit.
    recursive subroutine occur_cease_edit(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (derived_mode_p(major_modes%occur_edit_mode)) then
            call occur_mode(status)
            if (status%ok()) call message("Switching to Occur mode.")
        end if
    end subroutine occur_cease_edit

    !> The `occur_target' at POS of the current buffer: its index in the
    !> buffer's targets (0 for none).
    integer function target_at(pos) result(idx)
        integer(int64), intent(in) :: pos
        type(value_type) :: v
        type(status_type) :: st
        integer :: k
        idx = 0
        call get_text_property(pos, sym("occur_target"), v, st)
        if (.not. fixnump(v)) return
        k = occur_entry(current_buffer(), .false.)
        if (k == 0) return
        idx = int(xfixnum(v))
        if (idx < 1 .or. idx > occur_buffers(k)%ntargets) idx = 0
    end function target_at

    !> The first marker of the `occur_target' of the line at point, and
    !> its buffer; errors for none or for a killed buffer.  Port of
    !> replace.el:occur-mode--find-occurrences and occur--targets-start.
    subroutine occur_mode_find_occurrences(marker, buffer, status)
        type(handle_type), intent(out) :: marker, buffer
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: idx, k
        status = status_type()
        idx = target_at(point())
        if (idx == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No occurrence on this line")
            return
        end if
        k = occur_entry(current_buffer(), .false.)
        marker = occur_buffers(k)%targets(idx)%beg(1)
        call marker_buffer(marker, buffer, st)
        if (.not. buffer_live_p(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer for this occurrence was killed")
        end if
    end subroutine occur_mode_find_occurrences

    !> Return a marker to the first match of the line at point.  Port of
    !> replace.el:occur-mode-find-occurrence.
    subroutine occur_mode_find_occurrence(marker, status)
        type(handle_type), intent(out) :: marker
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        call occur_mode_find_occurrences(marker, buffer, status)
    end subroutine occur_mode_find_occurrence

    !> Go to the occurrence on the current line.  Port of
    !> replace.el:occur-mode-goto-occurrence (the overlay arrow, the
    !> highlighting of the occurrence and mouse events are Deferred).
    recursive subroutine occur_mode_goto_occurrence(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, buffer
        type(status_type) :: st
        integer(int64) :: pos
        call occur_mode_find_occurrences(marker, buffer, status)
        if (.not. status%ok()) return
        call pop_to_buffer(buffer, status)
        if (.not. status%ok()) return
        call marker_position(marker, pos, st)
        call goto_char(pos)
        call occur_mode_find_occurrence_hook%run(status)
    end subroutine occur_mode_goto_occurrence

    !> Go to the occurrence the current line describes, in another window.
    !> If this command needs to split the current window, it by default
    !> obeys the user options `split_height_threshold' and
    !> `split_width_threshold', when it decides whether to split the window
    !> horizontally or vertically.  Port of
    !> replace.el:occur-mode-goto-occurrence-other-window.
    recursive subroutine occur_mode_goto_occurrence_other_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, buffer, occur_buf
        type(status_type) :: st
        integer(int64) :: pos
        occur_buf = current_buffer()
        call occur_mode_find_occurrences(marker, buffer, status)
        if (.not. status%ok()) return
        call switch_to_buffer_other_window(buffer, status)
        if (.not. status%ok()) return
        call marker_position(marker, pos, st)
        call goto_char(pos)
        call next_error_found(status, occur_buf, current_buffer())
        if (status%ok()) call occur_mode_find_occurrence_hook%run(status)
    end subroutine occur_mode_goto_occurrence_other_window

    !> Display in another window the occurrence the current line describes.
    !> Port of replace.el:occur-mode-display-occurrence.
    recursive subroutine occur_mode_display_occurrence(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, buffer, occur_buf, window, old_window
        type(display_alist_type) :: alist
        type(status_type) :: st
        integer(int64) :: pos
        occur_buf = current_buffer()
        call occur_mode_find_occurrences(marker, buffer, status)
        if (.not. status%ok()) return
        alist%inhibit_same_window = .true.
        call display_buffer(buffer, window, status, alist=alist)
        if (.not. status%ok()) return
        if (window%index == 0) return
        ! This is the way to set point in the proper window.
        old_window = selected_frame%selected_window
        call select_window(window, status)
        if (.not. status%ok()) return
        call marker_position(marker, pos, st)
        call goto_char(pos)
        call next_error_found(status, occur_buf, current_buffer())
        if (status%ok()) call occur_mode_find_occurrence_hook%run(status)
        call select_window(old_window, st)
    end subroutine occur_mode_display_occurrence

    !> Move N times to the next (FORWARD) or previous match, MESSAGE the
    !> error when there is none.  Port of replace.el:occur-find-match.
    subroutine occur_find_match(n, forward, msg, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: forward
        character(len=*), intent(in) :: msg
        type(status_type), intent(out) :: status
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: r, r1, i
        logical :: found
        status = status_type()
        do i = 1, n
            call search(point(), r, found)
            if (found) then
                call get_text_property(r, sym("occur_match"), v, st)
                if (.not. nilp(v)) then
                    r1 = r
                    call search(r1, r, found)
                end if
            end if
            if (.not. found) then
                call set_error(status, FM_USER_ERROR, msg)
                return
            end if
            call goto_char(r)
        end do
    contains

        subroutine search(from, to, ok)
            integer(int64), intent(in) :: from
            integer(int64), intent(out) :: to
            logical, intent(out) :: ok
            type(status_type) :: s
            if (forward) then
                call next_single_property_change(from, sym("occur_match"), to, ok, s)
            else
                call previous_single_property_change(from, sym("occur_match"), to, ok, s)
            end if
            if (.not. s%ok()) ok = .false.
        end subroutine search
    end subroutine occur_find_match

    !> Move to the Nth (default 1) next match in an Occur mode buffer.
    !> Port of replace.el:occur-next.
    recursive subroutine occur_next(status)
        type(status_type), intent(out) :: status
        call occur_find_match(prefix_numeric_value(current_prefix_arg), .true., "No more matches", status)
    end subroutine occur_next

    !> Move to the Nth (default 1) previous match in an Occur mode buffer.
    !> Port of replace.el:occur-prev.
    recursive subroutine occur_prev(status)
        type(status_type), intent(out) :: status
        call occur_find_match(prefix_numeric_value(current_prefix_arg), .false., "No earlier matches", status)
    end subroutine occur_prev

    !> Move to the ARGPth (default 1) next match in an Occur mode buffer.
    !> RESET true means rewind to the first match.
    !> This is a compatibility function for \[next_error] invocations.
    !> Port of replace.el:occur-next-error.
    recursive subroutine occur_next_error(n, reset, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: reset
        type(status_type), intent(out) :: status
        type(handle_type) :: win
        type(status_type) :: st
        if (reset) then
            call goto_char(point_min())
        else if (n < 0) then
            call goto_char(line_beginning_position())
        else if (n > 0) then
            call goto_char(line_end_position())
        end if
        call occur_find_match(abs(n), n >= 0, "No more matches", status)
        if (.not. status%ok()) return
        ! In case the *Occur* buffer is visible in a nonselected window.
        win = get_buffer_window(current_buffer())
        if (win%index /= 0) call set_window_point(windows(win%index), point(), st)
        call occur_mode_goto_occurrence(status)
    end subroutine occur_next_error

    !> Handle `revert_buffer' for Occur mode buffers.  Port of
    !> replace.el:occur-revert-function.
    recursive subroutine occur_revert_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(occur_source_type), allocatable :: sources(:)
        character(len=:), allocatable :: regexp, collect, name
        logical :: has_nlines, has_collect
        integer(int64) :: nlines
        integer :: k
        status = status_type()
        if (ignore_auto .or. noconfirm) continue
        k = occur_entry(current_buffer(), .false.)
        if (k == 0) return
        if (.not. occur_buffers(k)%has_revert) return
        regexp = occur_buffers(k)%regexp
        has_nlines = occur_buffers(k)%has_nlines
        nlines = occur_buffers(k)%nlines
        has_collect = allocated(occur_buffers(k)%collect)
        if (has_collect) collect = occur_buffers(k)%collect
        sources = occur_buffers(k)%sources
        name = buffers(cur)%name
        if (has_collect) then
            call occur_1(regexp, has_nlines, nlines, sources, status, name, collect)
        else
            call occur_1(regexp, has_nlines, nlines, sources, status, name)
        end if
    end subroutine occur_revert_function

    !> Apply a change in an Occur-Edit buffer to the line of the
    !> originating buffer.  Port of replace.el:occur-after-change-function.
    recursive subroutine occur_after_change_function(beg, end, old_len, status)
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status
        type(value_type) :: prefix_beg, prefix_end
        type(handle_type) :: m, buf, win, old_window, occur_buf
        type(display_alist_type) :: alist
        type(status_type) :: st
        character(len=:), allocatable :: text, buf_str
        integer(int64) :: opoint, line_beg, line_end, col, line, pos, beg_pos, end_pos, prefix_len, suffix_len, &
                          found, reached
        logical :: readonly, ok
        integer :: idx, k
        status = status_type()
        opoint = point()
        call goto_char(beg)
        line_beg = line_beginning_position()
        idx = target_at(line_beg)
        if (idx == 0) then
            call goto_char(opoint)
            return
        end if
        k = occur_entry(current_buffer(), .false.)
        m = occur_buffers(k)%targets(idx)%beg(1)
        call marker_buffer(m, buf, st)
        call get_text_property(line_beg, sym("occur_prefix"), prefix_beg, st)
        call get_text_property(end, sym("occur_prefix"), prefix_end, st)
        if (nilp(prefix_beg) .or. .not. nilp(prefix_end) .or. .not. buffer_live_p(buf)) then
            call goto_char(opoint)
            return
        end if
        if (old_len == 0) then
            ! Apply occur-target property to inserted (e.g. yanked) text.
            call put_text_property(beg, end, sym("occur_target"), make_fixnum(int(idx, int64)), st)
            ! Did we insert a newline?  Occur Edit mode can't create new
            ! Occur entries; just discard everything after the newline.
            pos = point()
            call search_forward(LF, found, st, bound=end, noerror=SEARCH_NOERROR)
            if (found /= 0) call delete_region(point() - 1, end, st)
            call goto_char(pos)
        end if
        line = line_number_at_pos() - line_number_at_pos(window_start(windows(selected_frame%selected_window%index)))
        readonly = buffers(buf%index)%read_only
        occur_buf = current_buffer()
        win = get_buffer_window(buf)
        if (win%index == 0) then
            alist%inhibit_same_window = .true.
            call display_buffer(buf, win, st, alist=alist)
            call set_buffer(occur_buf, st)
        end if
        ! (save-excursion: the display may have moved point.)
        call goto_char(beg)
        line_end = line_end_position()
        call next_single_property_change(line_beg, sym("occur_prefix"), pos, ok, st, line_end)
        if (.not. ok) pos = line_end
        call goto_char(pos)
        col = point() - line_beg
        call buffer_substring(point(), line_end, text, st)
        call goto_char(opoint)
        if (win%index == 0) return
        old_window = selected_frame%selected_window
        call select_window(win, st)
        call marker_position(m, pos, st)
        call goto_char(pos)
        call recenter(make_fixnum(line), .false., st)
        if (readonly) then
            call message("Buffer `"//buffers(buf%index)%name//"' is read only.")
        else
            ! Replace the line, but make the change as small as possible by
            ! shrink-wrapping.  That way, we avoid disturbing markers
            ! unnecessarily.
            beg_pos = line_beginning_position()
            end_pos = line_end_position()
            call buffer_substring(beg_pos, end_pos, buf_str, st)
            prefix_len = common_prefix(buf_str, text)
            suffix_len = common_prefix(reverse_string(buf_str(prefix_len + 1:)), reverse_string(text(prefix_len + 1:)))
            beg_pos = beg_pos + chars_of(buf_str(:prefix_len))
            end_pos = end_pos - chars_of(buf_str(len(buf_str) - suffix_len + 1:))
            text = text(prefix_len + 1:len(text) - suffix_len)
            call delete_region(beg_pos, end_pos, st)
            call goto_char(beg_pos)
            call insert(text, st)
        end if
        call move_to_column(col, reached, .false., st)
        call select_window(old_window, st)
        call set_buffer(occur_buf, st)
    contains

        !> The length in bytes of the common prefix of S1 and S2, whole
        !> characters.
        integer function common_prefix(s1, s2) result(n)
            character(len=*), intent(in) :: s1, s2
            integer :: i
            n = 0
            do i = 1, min(len(s1), len(s2))
                if (s1(i:i) /= s2(i:i)) exit
                n = i
            end do
            ! Back to a character boundary.
            do while (n > 0 .and. n < len(s1))
                if (iand(iachar(s1(n + 1:n + 1)), 192) /= 128) exit
                n = n - 1
            end do
        end function common_prefix

        !> S with its bytes reversed.  (The common suffix is then counted
        !> in bytes; common_prefix keeps it on character boundaries of the
        !> reversed bytes, which are the starts of UTF-8 sequences read
        !> backwards.)
        function reverse_string(s) result(r)
            character(len=*), intent(in) :: s
            character(len=len(s)) :: r
            integer :: i
            do i = 1, len(s)
                r(i:i) = s(len(s) - i + 1:len(s) - i + 1)
            end do
        end function reverse_string

        integer(int64) function chars_of(s) result(c)
            character(len=*), intent(in) :: s
            integer :: i
            c = 0
            do i = 1, len(s)
                if (iand(iachar(s(i:i)), 192) /= 128) c = c + 1
            end do
        end function chars_of
    end subroutine occur_after_change_function

    ! ------------------------------------------------------------------
    ! occur
    ! ------------------------------------------------------------------

    !> The arguments of `occur' read interactively: the REGEXP, and the
    !> COLLECT regexp with C-u (HAS_COLLECT), else NLINES from a numeric
    !> prefix (HAS_NLINES).  Port of replace.el:occur-read-primary-args.
    recursive subroutine occur_read_primary_args(regexp, has_nlines, nlines, has_collect, collect, status)
        character(len=:), allocatable, intent(out) :: regexp, collect
        logical, intent(out) :: has_nlines, has_collect
        integer(int64), intent(out) :: nlines
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), allocatable :: defaults(:)
        type(status_type) :: st
        character(len=:), allocatable :: head, default
        logical :: found
        integer :: depth
        has_collect = consp(current_prefix_arg)
        has_nlines = .false.
        nlines = 0
        ! 'regexp-history-last.
        call history_head("regexp_history", head, found)
        if (found) then
            allocate (defaults(1))
            defaults(1)%s = head
        else
            allocate (defaults(0))
        end if
        if (has_collect) then
            call read_regexp("Collect strings matching regexp", regexp, status, defaults)
        else
            call read_regexp("List lines matching regexp", regexp, status, defaults)
        end if
        if (.not. status%ok()) return
        if (has_collect) then
            ! Perform collect operation.
            depth = regexp_opt_depth(regexp, st)
            if (depth == 0) then
                ! No subexpression so collect the entire match.
                collect = "\&"
            else
                ! Get the regexp for collection pattern.
                call history_head(OCCUR_COLLECT_HISTORY, default, found)
                if (.not. found) default = "\1"
                deallocate (defaults)
                allocate (defaults(1))
                defaults(1)%s = default
                call read_regexp(format_prompt("Regexp to collect", default), collect, status, defaults, &
                                 OCCUR_COLLECT_HISTORY)
            end if
        else if (.not. nilp(current_prefix_arg)) then
            ! Otherwise normal occur takes numerical prefix argument.
            has_nlines = .true.
            nlines = prefix_numeric_value(current_prefix_arg)
        end if
    end subroutine occur_read_primary_args

    !> Show all lines in the current buffer containing a match for REGEXP.
    !> If a match spreads across multiple lines, all those lines are
    !> shown.
    !>
    !> Each match is extended to include complete lines.  Only
    !> non-overlapping matches are considered.  (Note that extending
    !> matches to complete lines could cause some of the matches to
    !> overlap; if so, they will not be shown as separate matches.)
    !>
    !> Each line is displayed with NLINES lines before and after, or
    !> -NLINES before if NLINES is negative.
    !> NLINES defaults to `list_matching_lines_default_context_lines'.
    !> Interactively it is the prefix arg.
    !>
    !> `occur_in_region' restricts the search to a region (the REGION
    !> argument); otherwise search the entire buffer.
    !>
    !> The lines are shown in a buffer named `*Occur*'.
    !> That buffer can serve as a menu for finding any of the matches for
    !> REGEXP in the current buffer.
    !> \<occur_mode_map>\[describe_mode] in that buffer will explain how.
    !>
    !> Matches for REGEXP are shown in the face determined by the variable
    !> `list_matching_lines_face'.
    !> Names of buffers with matched lines are shown in the face determined
    !> by the variable `list_matching_lines_buffer_name_face'.
    !> The line numbers of the matching lines are shown in the face
    !> determined by the variable `list_matching_lines_prefix_face'.
    !>
    !> If `list_matching_lines_jump_to_current_line' is true, then the line
    !> in the current buffer which was current when the command was invoked
    !> will be shown in the `*Occur*' buffer highlighted with the
    !> `list_matching_lines_current_line_face', with point at the end of
    !> that line.  (If the current line doesn't match REGEXP, it will
    !> nonetheless be inserted into the `*Occur*' buffer between the 2
    !> closest lines that do match REGEXP.)
    !>
    !> If REGEXP contains upper case characters (excluding those preceded
    !> by `\') and `search_upper_case' is true, the matching is
    !> case-sensitive.
    !>
    !> When COLLECT is given (or when the function is called interactively
    !> with prefix argument without a number (\[universal_argument] alone
    !> as prefix)) the matching strings are collected into the `*Occur*'
    !> buffer by using COLLECT as a replacement regexp.  COLLECT may
    !> contain \& and \N which convention follows `replace_match'.
    !> For example, providing "defun\s +\(\S +\)" for REGEXP and "\1" for
    !> COLLECT collects all the function names in a Lisp program.  When
    !> there is no parenthesized subexpressions in REGEXP the entire match
    !> is collected.  In any case the searched buffer is not modified.
    !> Port of replace.el:occur (NLINES as a string is COLLECT).
    recursive subroutine occur(regexp, status, nlines, collect)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: nlines
        character(len=*), intent(in), optional :: collect
        type(occur_source_type) :: sources(1)
        integer(int64) :: n
        n = 0
        if (present(nlines)) n = nlines
        sources(1)%buffer = current_buffer()
        if (present(collect)) then
            call occur_1(regexp, present(nlines), n, sources, status, collect=collect)
        else
            call occur_1(regexp, present(nlines), n, sources, status)
        end if
    end subroutine occur

    !> `occur' restricted to the region from START to END (upstream the
    !> REGION argument, a list of (START . END)).
    recursive subroutine occur_in_region(regexp, start, end, status, nlines, collect)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: nlines
        character(len=*), intent(in), optional :: collect
        type(occur_source_type) :: sources(1)
        type(status_type) :: st
        integer(int64) :: n
        n = 0
        if (present(nlines)) n = nlines
        sources(1)%buffer = current_buffer()
        sources(1)%region = .true.
        call copy_marker_at(max(start, point_min()), sources(1)%start, .false., st)
        call copy_marker_at(min(end, point_max()), sources(1)%end, .false., st)
        sources(1)%orig_point = point()
        if (present(collect)) then
            call occur_1(regexp, present(nlines), n, sources, status, collect=collect)
        else
            call occur_1(regexp, present(nlines), n, sources, status)
        end if
    end subroutine occur_in_region

    !> The command: read the arguments, then `occur' (on the region if it
    !> is active).  The interactive form of replace.el:occur.
    !>
    !> Show all lines in the current buffer containing a match for a
    !> regexp.
    recursive subroutine cmd_occur(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, collect
        logical :: has_nlines, has_collect
        integer(int64) :: nlines, b, e
        type(status_type) :: st
        call occur_read_primary_args(regexp, has_nlines, nlines, has_collect, collect, status)
        if (.not. status%ok()) return
        if (use_region_p()) then
            call region_beginning(b, st)
            call region_end(e, st)
            if (has_collect) then
                call occur_in_region(regexp, b, e, status, collect=collect)
            else if (has_nlines) then
                call occur_in_region(regexp, b, e, status, nlines)
            else
                call occur_in_region(regexp, b, e, status)
            end if
        else if (has_collect) then
            call occur(regexp, status, collect=collect)
        else if (has_nlines) then
            call occur(regexp, status, nlines)
        else
            call occur(regexp, status)
        end if
    end subroutine cmd_occur

    !> Show all lines in buffers BUFS containing a match for REGEXP.
    !> Optional argument NLINES specifies the number of context lines to
    !> show with each match, see `list_matching_lines_default_context_lines'.
    !> This function acts on multiple buffers; otherwise, it is exactly
    !> like `occur'.  When you invoke this command interactively, you must
    !> specify the buffer names that you want, one by one.
    !> See also `multi_occur_in_matching_buffers'.  Port of
    !> replace.el:multi-occur.
    recursive subroutine multi_occur(bufs, regexp, status, nlines, collect)
        type(handle_type), intent(in) :: bufs(:)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: nlines
        character(len=*), intent(in), optional :: collect
        type(occur_source_type), allocatable :: sources(:)
        integer(int64) :: n
        integer :: i
        n = 0
        if (present(nlines)) n = nlines
        allocate (sources(size(bufs)))
        do i = 1, size(bufs)
            sources(i)%buffer = bufs(i)
        end do
        if (present(collect)) then
            call occur_1(regexp, present(nlines), n, sources, status, collect=collect)
        else
            call occur_1(regexp, present(nlines), n, sources, status)
        end if
    end subroutine multi_occur

    !> The command: read the buffers one by one, then the arguments of
    !> `occur', then `multi_occur'.  The interactive form of
    !> replace.el:multi-occur.
    !>
    !> Show all lines in buffers containing a match for a regexp.
    recursive subroutine cmd_multi_occur(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: bufs(:)
        character(len=64), allocatable :: names(:)
        character(len=:), allocatable :: name, regexp, collect
        logical :: has_nlines, has_collect, dup
        integer(int64) :: nlines
        integer :: i
        call read_buffer("First buffer to search: ", name, status, buffers(cur)%name, REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        names = [character(len=64) :: name]
        do
            call read_buffer("Next buffer to search (RET to end): ", name, status, require_match=REQUIRE_MATCH_T)
            if (.not. status%ok()) return
            if (len(name) == 0) exit
            dup = .false.
            do i = 1, size(names)
                if (trim(names(i)) == name) dup = .true.
            end do
            if (.not. dup) names = [character(len=64) :: names, name]
        end do
        allocate (bufs(size(names)))
        do i = 1, size(names)
            bufs(i) = get_buffer(trim(names(i)))
        end do
        call occur_read_primary_args(regexp, has_nlines, nlines, has_collect, collect, status)
        if (.not. status%ok()) return
        if (has_collect) then
            call multi_occur(bufs, regexp, status, collect=collect)
        else if (has_nlines) then
            call multi_occur(bufs, regexp, status, nlines)
        else
            call multi_occur(bufs, regexp, status)
        end if
    end subroutine cmd_multi_occur

    !> Show all lines matching REGEXP in buffers specified by BUFREGEXP.
    !> Normally BUFREGEXP matches against each buffer's visited file name,
    !> but ALLBUFS true (interactively, if you specify a prefix argument),
    !> it matches against the buffer name and includes also buffers that
    !> don't visit files.
    !> See also `multi_occur'.  Port of
    !> replace.el:multi-occur-in-matching-buffers.
    recursive subroutine multi_occur_in_matching_buffers(bufregexp, regexp, status, allbufs, nlines, collect)
        character(len=*), intent(in) :: bufregexp, regexp
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: allbufs
        integer(int64), intent(in), optional :: nlines
        character(len=*), intent(in), optional :: collect
        type(handle_type), allocatable :: all(:), chosen(:)
        type(status_type) :: st
        logical :: by_name, keep
        integer :: i, n
        status = status_type()
        by_name = .false.
        if (present(allbufs)) by_name = allbufs
        all = buffer_list()
        allocate (chosen(size(all)))
        n = 0
        do i = 1, size(all)
            if (by_name) then
                keep = string_match(bufregexp, buffers(all(i)%index)%name, st) /= 0
            else
                keep = .false.
                if (allocated(buffers(all(i)%index)%filename)) then
                    keep = string_match(bufregexp, buffers(all(i)%index)%filename, st) /= 0
                end if
            end if
            if (keep) then
                n = n + 1
                chosen(n) = all(i)
            end if
        end do
        if (present(collect)) then
            call multi_occur(chosen(:n), regexp, status, collect=collect)
        else
            ! (occur-1 regexp nil ...): NLINES is not passed.
            if (present(nlines)) continue
            call multi_occur(chosen(:n), regexp, status)
        end if
    end subroutine multi_occur_in_matching_buffers

    !> The command: read the buffer regexp, then the arguments of `occur',
    !> then `multi_occur_in_matching_buffers'.  The interactive form of
    !> replace.el:multi-occur-in-matching-buffers.
    !>
    !> Show all lines matching a regexp in buffers specified by a regexp.
    recursive subroutine cmd_multi_occur_in_matching_buffers(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bufregexp, regexp, collect, default
        logical :: has_nlines, has_collect, found
        integer(int64) :: nlines
        call history_head("regexp_history", default, found)
        if (.not. nilp(current_prefix_arg)) then
            call read_regexp("List lines in buffers whose names match regexp: ", bufregexp, status)
        else
            call read_regexp("List lines in buffers whose filenames match regexp: ", bufregexp, status)
        end if
        if (.not. status%ok()) return
        if (len(bufregexp) == 0 .and. found) bufregexp = default
        call occur_read_primary_args(regexp, has_nlines, nlines, has_collect, collect, status)
        if (.not. status%ok()) return
        if (has_collect) then
            call multi_occur_in_matching_buffers(bufregexp, regexp, status,.not. nilp(current_prefix_arg), &
                                                 collect=collect)
        else
            call multi_occur_in_matching_buffers(bufregexp, regexp, status,.not. nilp(current_prefix_arg))
        end if
    end subroutine cmd_multi_occur_in_matching_buffers

    !> Rename the current *Occur* buffer to *Occur: original-buffer-name*.
    !> Here `original-buffer-name' is the buffer name where Occur was
    !> originally run.  If UNIQUE_P is true (interactively, the prefix
    !> argument), or called non-interactively with INTERACTIVE_P false,
    !> the renaming will not clobber the existing buffer(s) of that name,
    !> but will use `generate_new_buffer_name' instead.
    !> You can add this to `occur_hook' if you always want a separate
    !> *Occur* buffer for each buffer where you invoke `occur'.  Port of
    !> replace.el:occur-rename-buffer.
    recursive subroutine occur_rename_buffer(status, unique_p, interactive_p)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: unique_p, interactive_p
        type(handle_type) :: old, buf
        type(status_type) :: st
        character(len=:), allocatable :: name, actual
        logical :: unique, inter
        integer :: k, i
        status = status_type()
        unique = .false.
        if (present(unique_p)) unique = unique_p
        inter = .false.
        if (present(interactive_p)) inter = interactive_p
        old = current_buffer()
        if (buffers(cur)%major_mode == "occur_mode") then
            buf = current_buffer()
        else
            buf = get_buffer("*Occur*")
        end if
        if (.not. buffer_live_p(buf)) return
        call set_buffer(buf, st)
        name = "*Occur: "
        k = occur_entry(buf, .false.)
        if (k /= 0) then
            if (allocated(occur_buffers(k)%sources)) then
                do i = 1, size(occur_buffers(k)%sources)
                    if (i > 1) name = name//"/"
                    if (buffer_live_p(occur_buffers(k)%sources(i)%buffer)) then
                        name = name//buffers(occur_buffers(k)%sources(i)%buffer%index)%name
                    end if
                end do
            end if
        end if
        name = name//"*"
        call rename_buffer(name, unique .or. .not. inter, actual, status)
        call set_buffer(old, st)
    end subroutine occur_rename_buffer

    !> The command: `occur_rename_buffer' with the prefix argument as
    !> UNIQUE_P.  The interactive form of replace.el:occur-rename-buffer.
    !>
    !> Rename the current *Occur* buffer to *Occur: original-buffer-name*.
    recursive subroutine cmd_occur_rename_buffer(status)
        type(status_type), intent(out) :: status
        call occur_rename_buffer(status,.not. nilp(current_prefix_arg), .true.)
    end subroutine cmd_occur_rename_buffer

    !> The description of REGEXP in Occur's headers: its control
    !> characters get `display' properties when the header is inserted
    !> (`descr_display'), as query-replace-descr gives them.  Port of
    !> replace.el:occur-regexp-descr (the isearch descriptions are
    !> Deferred).
    function occur_regexp_descr(regexp) result(descr)
        character(len=*), intent(in) :: regexp
        character(len=:), allocatable :: descr
        descr = " for """//regexp//""""
    end function occur_regexp_descr

    !> Show the control characters from B to E of the current buffer as
    !> ^C in the face `escape_glyph' (the display properties of
    !> query-replace-descr).
    subroutine descr_display(b, e)
        integer(int64), intent(in) :: b, e
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: p
        integer :: i, c
        call buffer_substring(b, e, text, st)
        p = b
        do i = 1, len(text)
            c = iachar(text(i:i))
            if (iand(c, 192) == 128) cycle
            if (c < 32) then
                call put_prop(p, p + 1, "display", propertize(make_string("^"//achar(c + 64)), &
                                                              list(sym("face"), sym("escape_glyph"))))
            else if (c == 127) then
                call put_prop(p, p + 1, "display", propertize(make_string("^?"), &
                                                              list(sym("face"), sym("escape_glyph"))))
            end if
            p = p + 1
        end do
    end subroutine descr_display

    !> The live sources of SOURCES (buffers alive, regions whose buffer is
    !> alive).
    function active_sources(sources) result(active)
        type(occur_source_type), intent(in) :: sources(:)
        type(occur_source_type), allocatable :: active(:)
        logical :: live(size(sources))
        integer :: i
        do i = 1, size(sources)
            live(i) = buffer_live_p(sources(i)%buffer)
        end do
        active = pack(sources, live)
    end function active_sources

    !> Run occur for REGEXP on SOURCES with NLINES (HAS_NLINES; else the
    !> default) context lines, or COLLECT, in the buffer BUF_NAME
    !> (*Occur*).  Port of replace.el:occur-1.
    recursive subroutine occur_1(regexp, has_nlines, nlines, sources, status, buf_name, collect)
        character(len=*), intent(in) :: regexp
        logical, intent(in) :: has_nlines
        integer(int64), intent(in) :: nlines
        type(occur_source_type), intent(in) :: sources(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buf_name, collect
        type(occur_source_type), allocatable :: active(:)
        type(handle_type) :: occur_buf, old, win
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: name, dir, actual, str, text
        integer(int64) :: count, n, found, limit
        logical :: saved_undo, fold, same, saved_fold
        integer :: i, k, bufcount, diff
        status = status_type()
        if (len(regexp) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Occur doesn't work with the empty regexp")
            return
        end if
        name = "*Occur*"
        if (present(buf_name)) name = buf_name
        active = active_sources(sources)
        old = current_buffer()
        dir = ""
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        ! Handle the case where one of the buffers we're searching is the
        ! output buffer.  Just rename it.
        do i = 1, size(active)
            if (buffers(active(i)%buffer%index)%name == name) then
                call set_buffer(active(i)%buffer, st)
                call rename_buffer(generate_new_buffer_name(name), .false., actual, st)
                call set_buffer(old, st)
                exit
            end if
        end do
        ! Now find or create the output buffer.  If we just renamed that
        ! buffer, we will make a new one here.
        call get_buffer_create(name, occur_buf, status)
        if (.not. status%ok()) return
        call set_buffer(occur_buf, st)
        ! Make the default-directory of the *Occur* buffer match that of the
        ! buffer where the occurrences come from.
        buffers(cur)%directory = dir
        if (present(collect)) then
            ! This is for collect operation.
            call fundamental_mode(status)
        else
            call occur_mode(status)
        end if
        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        ! Don't generate undo entries for creation of the initial contents.
        saved_undo = buffers(cur)%undo_enabled
        buffers(cur)%undo_enabled = .false.
        occur_final_pos = 0
        call erase_buffer(st)
        k = occur_entry(occur_buf, .true.)
        call occur_free_targets(k)
        count = 0
        if (present(collect)) then
            ! Treat nlines as a regexp to collect.
            do i = 1, size(active)
                call set_buffer(active(i)%buffer, st)
                n = point()
                if (active(i)%region) then
                    call marker_position(active(i)%start, limit, st)
                    call goto_char(limit)
                    call marker_position(active(i)%end, limit, st)
                else
                    call goto_char(point_min())
                    limit = point_max()
                end if
                do
                    call re_search_forward(regexp, found, st, bound=limit, noerror=SEARCH_NOERROR)
                    if (.not. st%ok() .or. found == 0) exit
                    ! Insert the replacement regexp.
                    str = match_substitute_replacement(collect, st)
                    if (.not. st%ok()) exit
                    call set_buffer(occur_buf, st)
                    call insert(str, st)
                    count = count + 1
                    if (current_column() /= 0) call insert(LF, st)
                    call set_buffer(active(i)%buffer, st)
                end do
                call goto_char(n)
            end do
            call set_buffer(occur_buf, st)
        else
            ! Perform normal occur.
            if (has_nlines) then
                n = nlines
            else
                n = list_matching_lines_default_context_lines
            end if
            call set_buffer(old, st)
            fold = case_fold_search%value
            if (case_fold_search%value .and. search_upper_case /= UPPER_CASE_NIL) then
                fold = isearch_no_upper_case_p(regexp, .true.)
            end if
            call set_buffer(occur_buf, st)
            saved_fold = .false.
            if (len(list_matching_lines_prefix_face) > 0) then
                saved_fold = face_differs_from_default_p(list_matching_lines_prefix_face)
            end if
            if (saved_fold) then
                call occur_engine(regexp, active, occur_buf, n, fold, list_matching_lines_buffer_name_face, &
                                  list_matching_lines_prefix_face, list_matching_lines_face, &
                                  .not. occur_excluded_properties_all, count)
            else
                call occur_engine(regexp, active, occur_buf, n, fold, list_matching_lines_buffer_name_face, "", &
                                  list_matching_lines_face,.not. occur_excluded_properties_all, count)
            end if
            call set_buffer(occur_buf, st)
        end if
        bufcount = size(active)
        diff = size(sources) - bufcount
        text = "Searched "//number_to_string(int(bufcount, int64))//" buffer"
        if (bufcount /= 1) text = text//"s"
        if (diff /= 0) text = text//" ("//number_to_string(int(diff, int64))//" killed)"
        text = text//"; "
        if (count == 0) then
            text = text//"no"
        else
            text = text//number_to_string(count)
        end if
        text = text//" match"
        if (count /= 1) text = text//"es"
        ! Don't display regexp if with remaining text it is longer than
        ! window-width.
        if (len(regexp) + 42 <= window_text_cols(windows(selected_frame%selected_window%index))) then
            text = text//" for """//query_replace_descr(regexp)//""""
        end if
        call message(text)
        k = occur_entry(occur_buf, .true.)
        same = .false.
        if (allocated(occur_buffers(k)%sources)) same = same_sources(occur_buffers(k)%sources, sources)
        if (.not. same .and. allocated(occur_buffers(k)%sources)) call occur_free_sources(occur_buffers(k)%sources)
        occur_buffers(k)%has_revert = .true.
        occur_buffers(k)%regexp = regexp
        occur_buffers(k)%has_nlines = has_nlines
        occur_buffers(k)%nlines = nlines
        if (present(collect)) then
            occur_buffers(k)%collect = collect
        else if (allocated(occur_buffers(k)%collect)) then
            deallocate (occur_buffers(k)%collect)
        end if
        occur_buffers(k)%sources = sources
        buffers(cur)%undo_enabled = saved_undo
        call set(sym("inhibit_read_only"), inhibit, st)
        if (count == 0) then
            call kill_buffer(occur_buf, st)
        else
            call display_buffer(occur_buf, win, st)
            if (occur_final_pos /= 0) then
                win = get_buffer_window(occur_buf)
                if (win%index /= 0) call set_window_point(windows(win%index), occur_final_pos, st)
            end if
            next_error_last_buffer = occur_buf
            buffers(cur)%read_only = .true.
            call restore_buffer_modified_p(.false.)
            call occur_hook%run(status)
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    contains

        logical function same_sources(a, b)
            type(occur_source_type), intent(in) :: a(:), b(:)
            integer :: j
            same_sources = size(a) == size(b)
            if (.not. same_sources) return
            do j = 1, size(a)
                if (.not. same_buffer_handle(a(j)%buffer, b(j)%buffer)) same_sources = .false.
                if (a(j)%region .neqv. b(j)%region) same_sources = .false.
                if (a(j)%start%index /= b(j)%start%index) same_sources = .false.
            end do
        end function same_sources
    end subroutine occur_1

    ! ------------------------------------------------------------------
    ! The engine
    ! ------------------------------------------------------------------

    !> Insert TEXT in the current buffer at point; START and END get where
    !> it went.
    subroutine out_insert(text, start, end)
        character(len=*), intent(in) :: text
        integer(int64), intent(out) :: start, end
        type(status_type) :: st
        start = point()
        call insert(text, st)
        end = point()
    end subroutine out_insert

    !> Put PROP with VALUE on START..END of the current buffer.
    subroutine put_prop(start, end, prop, value)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: prop
        type(value_type), intent(in) :: value
        type(status_type) :: st
        if (end > start) call put_text_property(start, end, sym(prop), value, st)
    end subroutine put_prop

    !> Insert the text from SBEG to SEND of buffer SRC in the current
    !> buffer at point, with its text properties when KEEP_PROPS (but
    !> `occur_excluded_properties'): occur-engine-line and its insertion.
    !> START and END get where it went.
    subroutine insert_source_line(src, sbeg, send, keep_props, start, end)
        type(handle_type), intent(in) :: src
        integer(int64), intent(in) :: sbeg, send
        logical, intent(in) :: keep_props
        integer(int64), intent(out) :: start, end
        type(handle_type) :: out
        type(status_type) :: st
        type(value_type) :: plist
        type(value_type), allocatable :: elts(:), kept(:)
        character(len=:), allocatable :: text
        integer(int64) :: p, q, off
        logical :: found, changed
        integer :: i, n
        out = current_buffer()
        call set_buffer(src, st)
        if (keep_props .and. font_lock_mode_enabled()) call font_lock_ensure(st, sbeg, send)
        call buffer_substring(sbeg, send, text, st)
        call set_buffer(out, st)
        call out_insert(text, start, end)
        if (.not. keep_props) return
        p = sbeg
        do while (p < send)
            call set_buffer(src, st)
            call text_properties_at(p, plist, st)
            call next_property_change(p, q, found, st, send)
            if (.not. found .or. q > send) q = send
            call list_elements(plist, elts)
            allocate (kept(size(elts)))
            n = 0
            do i = 1, size(elts) - 1, 2
                if (excluded(elts(i))) cycle
                kept(n + 1) = elts(i)
                kept(n + 2) = elts(i + 1)
                n = n + 2
            end do
            call set_buffer(out, st)
            off = p - sbeg
            if (n > 0) call add_text_properties(start + off, start + off + (q - p), make_list(kept(:n)), changed, st)
            deallocate (kept)
            p = q
        end do
        call set_buffer(out, st)
    contains

        logical function excluded(name)
            type(value_type), intent(in) :: name
            integer :: j
            excluded = .false.
            if (.not. allocated(occur_excluded_properties)) return
            do j = 1, size(occur_excluded_properties)
                if (value_eq(name, sym(trim(occur_excluded_properties(j))))) excluded = .true.
            end do
        end function excluded
    end subroutine insert_source_line

    !> Insert a context line (occur-engine-add-prefix): the prefix
    !> "       :" in PREFIX_FACE, the line SBEG..SEND of SRC (with the
    !> current-line face when CURRENT) and a newline.
    subroutine insert_context_line(src, sbeg, send, keep_props, prefix_face, current)
        type(handle_type), intent(in) :: src
        integer(int64), intent(in) :: sbeg, send
        logical, intent(in) :: keep_props, current
        character(len=*), intent(in) :: prefix_face
        integer(int64) :: b, e
        type(status_type) :: st
        call out_insert("       :", b, e)
        if (len(prefix_face) > 0) call put_prop(b, e, "font_lock_face", sym(prefix_face))
        call insert_source_line(src, sbeg, send, keep_props, b, e)
        if (current) call mark_current_line(b, e)
        call insert(LF, st)
    end subroutine insert_context_line

    !> Give B..E the current-line face and the `current_line' property.
    subroutine mark_current_line(b, e)
        integer(int64), intent(in) :: b, e
        type(status_type) :: st
        if (e <= b) return
        call add_face_text_property(b, e, sym(list_matching_lines_current_line_face), st)
        call put_prop(b, e, "current_line", t_value())
    end subroutine mark_current_line

    !> The lines (their start and end positions) COUNT lines forward from
    !> PT (backward for a negative COUNT), the line of PT first, in the
    !> current buffer.  Port of replace.el:occur-accumulate-lines.
    subroutine occur_accumulate_lines(count_in, pt, begs, ends)
        integer(int64), intent(in) :: count_in, pt
        integer(int64), allocatable, intent(out) :: begs(:), ends(:)
        integer(int64) :: opoint, count, shortage
        logical :: forwardp, moved
        opoint = point()
        call goto_char(pt)
        count = count_in
        forwardp = count > 0
        allocate (begs(0), ends(0))
        moved = .false.
        do
            if (count == 0) exit
            if (forwardp) then
                if (eobp()) exit
            else
                if (bobp() .and. .not. moved) exit
            end if
            if (forwardp) then
                count = count - 1
            else
                count = count + 1
            end if
            begs = [begs, line_beginning_position()]
            ends = [ends, line_end_position()]
            if (forwardp) then
                shortage = forward_line(1_int64)
            else
                shortage = forward_line(-1_int64)
            end if
            moved = shortage == 0
        end do
        call goto_char(opoint)
    end subroutine occur_accumulate_lines

    !> Search REGEXP in SOURCES and insert the matches in OUT_BUF; COUNT
    !> is the number of matches.  Port of replace.el:occur-engine (the
    !> coding system of the first buffer that binds it locally is Deferred).
    recursive subroutine occur_engine(regexp, sources, out_buf, nlines_in, case_fold, title_face, prefix_face, &
                                      match_face, keep_props, global_matches)
        character(len=*), intent(in) :: regexp
        type(occur_source_type), intent(in) :: sources(:)
        type(handle_type), intent(in) :: out_buf
        integer(int64), intent(in) :: nlines_in
        logical, intent(in) :: case_fold, keep_props
        character(len=*), intent(in) :: title_face, prefix_face, match_face
        integer(int64), intent(out) :: global_matches
        type(handle_type) :: src
        type(handle_type), allocatable :: mbeg(:), mend(:)
        type(status_type) :: st
        integer(int64), allocatable :: match_starts(:), match_ends(:), before_b(:), before_e(:), after_b(:), &
                                       after_e(:), prev_after_b(:), prev_after_e(:), acc_b(:), acc_e(:)
        logical, allocatable :: prev_after_cur(:), before_cur(:), after_cur(:)
        character(len=:), allocatable :: curstring, header
        integer(int64) :: global_lines, lines, matches, headerpt, orig_line, limit, start_line, curr_line, &
                          prev_line, origpt, begpt, endpt, found, slen, mstart, mb, me, hb, he, nlines, &
                          shortage, opoint, x
        logical :: multi_occur_p, orig_line_shown_p, has_prev_line, saved_fold, separator, current_match
        integer :: s, tidx, i, j
        global_lines = 0
        global_matches = 0
        nlines = nlines_in
        multi_occur_p = size(sources) > 1
        do s = 1, size(sources)
            src = sources(s)%buffer
            if (.not. buffer_live_p(src)) cycle
            lines = 0
            matches = 0
            call set_buffer(out_buf, st)
            headerpt = point()
            call set_buffer(src, st)
            if (sources(s)%region) then
                orig_line = line_number_at_pos(sources(s)%orig_point)
            else
                orig_line = line_number_at_pos()
            end if
            saved_fold = case_fold_search%value
            case_fold_search = case_fold
            opoint = point()
            ! Begin searching in the buffer.
            if (sources(s)%region) then
                call marker_position(sources(s)%start, x, st)
                call goto_char(x)
            else
                call goto_char(point_min())
            end if
            shortage = forward_line(0_int64)
            if (sources(s)%region) then
                call marker_position(sources(s)%end, limit, st)
            else
                limit = point_max()
            end if
            start_line = line_number_at_pos()
            curr_line = start_line
            orig_line_shown_p = .false.
            has_prev_line = .false.
            prev_line = 0
            allocate (prev_after_b(0), prev_after_e(0), prev_after_cur(0))
            do while (point() < limit)
                origpt = point()
                call re_search_forward(regexp, found, st, bound=limit, noerror=SEARCH_NOERROR)
                endpt = found
                if (found /= 0) then
                    lines = lines + 1
                    ! Get beginning of first match line and end of the last.
                    call goto_char(match_beginning(0, st))
                    begpt = line_beginning_position()
                    call goto_char(endpt)
                    endpt = line_end_position()
                    ! Sum line numbers up to the first match line.
                    curr_line = curr_line + count_lines(origpt, begpt)
                    call buffer_substring(begpt, endpt, curstring, st)
                    slen = endpt - begpt
                    ! Count empty lines that don't use next loop (Bug#22062).
                    if (slen == 0) matches = matches + 1
                    current_match = .false.
                    if (list_matching_lines_jump_to_current_line .and. .not. multi_occur_p) then
                        if (curr_line == orig_line) current_match = .true.
                        ! Shown either here or will be shown by
                        ! occur-context-lines.
                        if (orig_line >= curr_line - nlines .and. orig_line <= curr_line + nlines) then
                            orig_line_shown_p = .true.
                        end if
                    end if
                    ! Highlight the matches.
                    allocate (match_starts(0), match_ends(0), mbeg(0), mend(0))
                    mstart = 0
                    do while (mstart < slen)
                        if (string_match(regexp, curstring, st, mstart + 1) == 0) exit
                        mb = match_beginning(0, st) - 1
                        me = match_end(0, st) - 1
                        match_starts = [match_starts, mb]
                        match_ends = [match_ends, me]
                        mbeg = [mbeg, null_handle()]
                        mend = [mend, null_handle()]
                        call copy_marker_at(begpt + mb, mbeg(size(mbeg)), .false., st)
                        call copy_marker_at(begpt + me, mend(size(mend)), .false., st)
                        matches = matches + 1
                        ! Avoid infloop (Bug#7593).
                        if (mstart == me) then
                            mstart = mstart + 1
                        else
                            mstart = me
                        end if
                    end do
                    call set_buffer(out_buf, st)
                    tidx = occur_add_target(mbeg, mend)
                    ! Context lines.
                    separator = .false.
                    if (nlines /= 0) then
                        call set_buffer(src, st)
                        call occur_accumulate_lines(-(1 + abs(nlines)), begpt, acc_b, acc_e)
                        ! (nreverse (cdr ...)).
                        before_b = acc_b(size(acc_b):2:-1)
                        before_e = acc_e(size(acc_e):2:-1)
                        call occur_accumulate_lines(1 + nlines, endpt, acc_b, acc_e)
                        after_b = acc_b(2:)
                        after_e = acc_e(2:)
                        allocate (before_cur(size(before_b)), after_cur(size(after_b)))
                        before_cur = .false.
                        after_cur = .false.
                        if (list_matching_lines_jump_to_current_line .and. .not. multi_occur_p) then
                            if (orig_line >= curr_line - nlines .and. orig_line < curr_line) then
                                j = int(size(before_b) - (curr_line - orig_line)) + 1
                                if (j >= 1 .and. j <= size(before_b)) before_cur(j) = .true.
                            end if
                            if (orig_line <= curr_line + nlines .and. orig_line > curr_line) then
                                j = int(orig_line - curr_line)
                                if (j >= 1 .and. j <= size(after_b)) after_cur(j) = .true.
                            end if
                        end if
                        ! Combine after-lines of the previous match with
                        ! before-lines of the current match.
                        if (size(prev_after_b) > 0) then
                            ! Don't overlap prev after-lines with current
                            ! before-lines.
                            if (prev_line + size(prev_after_b) >= curr_line - size(before_b)) then
                                j = int(max(0_int64, min(int(size(prev_after_b), int64), &
                                                         curr_line - prev_line - size(before_b) - 1)))
                                prev_after_b = prev_after_b(:j)
                                prev_after_e = prev_after_e(:j)
                                prev_after_cur = prev_after_cur(:j)
                            else
                                ! Separate non-overlapping context lines with
                                ! a dashed line.
                                separator = .true.
                            end if
                        end if
                        if (has_prev_line) then
                            ! Don't overlap current before-lines with
                            ! previous match line.
                            if (curr_line - size(before_b) <= prev_line) then
                                j = int(size(before_b) - (curr_line - prev_line - 1))
                                j = max(0, min(j, size(before_b)))
                                before_b = before_b(j + 1:)
                                before_e = before_e(j + 1:)
                                before_cur = before_cur(j + 1:)
                            else if (nlines <= 0) then
                                ! Separate non-overlapping before-context
                                ! lines.
                                separator = .true.
                            end if
                        end if
                    end if
                    ! orig-line-str.
                    call set_buffer(out_buf, st)
                    if (list_matching_lines_jump_to_current_line .and. .not. orig_line_shown_p .and. &
                        curr_line > orig_line) then
                        orig_line_shown_p = .true.
                        call insert_orig_line()
                    end if
                    ! Actually insert the match display data.
                    if (nlines /= 0) then
                        do i = 1, size(prev_after_b)
                            call insert_context_line(src, prev_after_b(i), prev_after_e(i), keep_props, prefix_face, &
                                                     prev_after_cur(i))
                        end do
                        if (separator) then
                            call out_insert("-------"//LF, hb, he)
                            if (len(prefix_face) > 0) call put_prop(hb, he, "font_lock_face", sym(prefix_face))
                        end if
                        do i = 1, size(before_b)
                            call insert_context_line(src, before_b(i), before_e(i), keep_props, prefix_face, &
                                                     before_cur(i))
                        end do
                    end if
                    call insert_match(src, begpt, endpt, curstring, curr_line, tidx, match_starts, match_ends, &
                                      current_match, keep_props, prefix_face, match_face)
                    if (nlines > 0) then
                        prev_after_b = after_b
                        prev_after_e = after_e
                        prev_after_cur = after_cur
                    else if (nlines /= 0) then
                        deallocate (prev_after_b, prev_after_e, prev_after_cur)
                        allocate (prev_after_b(0), prev_after_e(0), prev_after_cur(0))
                    end if
                    if (allocated(before_cur)) deallocate (before_cur)
                    if (allocated(after_cur)) deallocate (after_cur)
                    deallocate (match_starts, match_ends, mbeg, mend)
                    call set_buffer(src, st)
                    call goto_char(endpt)
                end if
                if (found /= 0) then
                    ! Sum line numbers between first and last match lines.
                    curr_line = curr_line + count_lines(begpt, endpt)
                    ! Add 1 for empty last match line since count-lines
                    ! returns one line less.
                    if (bolp() .and. eolp()) curr_line = curr_line + 1
                    ! On to the next match...
                    shortage = forward_line(1_int64)
                else
                    call goto_char(point_max())
                end if
                prev_line = curr_line - 1
                has_prev_line = .true.
            end do
            ! Flush remaining context after-lines.
            call set_buffer(out_buf, st)
            do i = 1, size(prev_after_b)
                call insert_context_line(src, prev_after_b(i), prev_after_e(i), keep_props, prefix_face, prev_after_cur(i))
            end do
            deallocate (prev_after_b, prev_after_e, prev_after_cur)
            if (list_matching_lines_jump_to_current_line .and. .not. orig_line_shown_p) then
                orig_line_shown_p = .true.
                call insert_orig_line()
            end if
            call set_buffer(src, st)
            call goto_char(opoint)
            case_fold_search = saved_fold
            ! Is the count zero?
            if (lines /= 0) then
                global_lines = global_lines + lines
                global_matches = global_matches + matches
                call set_buffer(out_buf, st)
                call goto_char(headerpt)
                header = number_to_string(matches)//" match"
                if (matches /= 1) header = header//"es"
                ! Don't display the same number of lines and matches in
                ! case of 1 match per line.
                if (lines /= matches) then
                    header = header//" in "//number_to_string(lines)//" line"
                    if (lines /= 1) header = header//"s"
                end if
                ! Don't display regexp for multi-buffer.
                if (.not. size(sources) > 1) header = header//occur_regexp_descr(regexp)
                header = header//" in buffer: "//buffers(src%index)%name
                if (sources(s)%region) then
                    call marker_position(sources(s)%start, mb, st)
                    call marker_position(sources(s)%end, me, st)
                    header = header//" within region: "//number_to_string(mb)//"-"//number_to_string(me)
                end if
                call out_insert(header//LF, hb, he)
                call descr_display(hb, he - 1)
                call put_prop(hb, he, "read_only", t_value())
                if (len(title_face) > 0) call add_face_text_property(hb, he, sym(title_face), st)
                if (list_matching_lines_jump_to_current_line .and. .not. multi_occur_p) then
                    call goto_char(point_max())
                    call previous_single_property_change(point(), sym("current_line"), x, current_match, st)
                    if (.not. current_match) x = point_max()
                    occur_final_pos = x
                    call goto_char(x)
                else
                    call goto_char(point_min())
                end if
            end if
        end do
        ! Display total match count and regexp for multi-buffer.
        call set_buffer(out_buf, st)
        if (global_lines /= 0 .and. size(sources) > 1) then
            call goto_char(point_min())
            header = number_to_string(global_matches)//" match"
            if (global_matches /= 1) header = header//"es"
            if (global_lines /= global_matches) then
                header = header//" in "//number_to_string(global_lines)//" line"
                if (global_lines /= 1) header = header//"s"
            end if
            header = header//" total"//occur_regexp_descr(regexp)//":"//LF
            call out_insert(header, hb, he)
            call descr_display(hb, he - 1)
            if (len(title_face) > 0) call add_face_text_property(hb, he, sym(title_face), st)
            call goto_char(point_min())
        end if
    contains

        !> Insert the line of the original buffer that was current
        !> (orig-line-str), highlighted, with its prefix.
        subroutine insert_orig_line()
            integer(int64) :: lb, le, here, sh
            call set_buffer(src, st)
            here = point()
            call goto_char(point_min())
            sh = forward_line(orig_line - 1)
            lb = line_beginning_position()
            le = line_end_position()
            call goto_char(here)
            call set_buffer(out_buf, st)
            call insert_context_line(src, lb, le, keep_props, prefix_face, .true.)
        end subroutine insert_orig_line
    end subroutine occur_engine

    !> Insert the display of a match: the prefix with the line number
    !> CURR_LINE, the lines BEGPT..ENDPT of SRC (CURSTRING), the other lines
    !> of a multi-line match after a prefix "       :", and a newline;
    !> with the target TIDX, the matches MATCH_STARTS..MATCH_ENDS (0-based
    !> offsets in CURSTRING) in MATCH_FACE, the current-line face when
    !> CURRENT.
    subroutine insert_match(src, begpt, endpt, curstring, curr_line, tidx, match_starts, match_ends, current, &
                            keep_props, prefix_face, match_face)
        type(handle_type), intent(in) :: src
        integer(int64), intent(in) :: begpt, endpt, curr_line
        character(len=*), intent(in) :: curstring, prefix_face, match_face
        integer, intent(in) :: tidx
        integer(int64), intent(in) :: match_starts(:), match_ends(:)
        logical, intent(in) :: current, keep_props
        type(status_type) :: st
        type(value_type) :: target
        integer(int64), allocatable :: seg_out(:), seg_cur(:), seg_len(:)
        integer(int64) :: pb, pe, b, e, sbeg, cur_off, nl_pos, a, z, i, k
        character(len=16) :: buf
        integer :: nseg
        target = make_fixnum(int(tidx, int64))
        ! Using 7 digits aligns tabs properly.
        write (buf, "(i7, ':')") curr_line
        call out_insert(trim(buf), pb, pe)
        if (len(prefix_face) > 0) call put_prop(pb, pe, "font_lock_face", sym(prefix_face))
        call put_prop(pb, pe, "occur_prefix", t_value())
        ! Allow insertion of text at the end of the prefix (for Occur Edit
        ! mode).
        call put_prop(pb, pe, "front_sticky", t_value())
        call put_prop(pb, pe, "rear_nonsticky", t_value())
        call put_prop(pb, pe, "read_only", t_value())
        call put_prop(pb, pe, "occur_target", target)
        call put_prop(pb, pe, "follow_link", t_value())
        call put_prop(pb, pe, "help_echo", make_string("mouse-2: go to this occurrence"))
        ! Add mouse face in one section to ensure the prefix and the string
        ! get a contiguous highlight.
        call put_prop(pb, pe, "mouse_face", sym("highlight"))
        ! The lines of the match; each newline becomes "\n       :".
        allocate (seg_out(0), seg_cur(0), seg_len(0))
        sbeg = begpt
        cur_off = 0
        nseg = 0
        do
            nl_pos = index(curstring(byte_of(cur_off) + 1:), LF)
            if (nl_pos == 0) then
                call segment(endpt)
                exit
            end if
            call segment(sbeg + chars(curstring(byte_of(cur_off) + 1:byte_of(cur_off) + nl_pos - 1)))
            call out_insert(LF//"       :", b, e)
            if (len(prefix_face) > 0) call put_prop(b, e, "font_lock_face", sym(prefix_face))
            call put_prop(b, e, "occur_target", target)
            cur_off = cur_off + seg_len(nseg) + 1
            sbeg = sbeg + seg_len(nseg) + 1
        end do
        if (current) then
            do k = 1, nseg
                call mark_current_line(seg_out(k), seg_out(k) + seg_len(k))
            end do
        end if
        do i = 1, size(match_starts)
            do k = 1, nseg
                a = max(match_starts(i), seg_cur(k))
                z = min(match_ends(i), seg_cur(k) + seg_len(k))
                if (z <= a) cycle
                call put_prop(seg_out(k) + (a - seg_cur(k)), seg_out(k) + (z - seg_cur(k)), "occur_match", t_value())
                ! Add `match-face' to faces copied from the buffer.
                if (len(match_face) > 0) then
                    call add_face_text_property(seg_out(k) + (a - seg_cur(k)), seg_out(k) + (z - seg_cur(k)), &
                                                sym(match_face), st)
                end if
            end do
        end do
        ! Add markers at eol, but no mouse props.
        call out_insert(LF, b, e)
        call put_prop(b, e, "occur_target", target)
    contains

        !> Insert the source text from SBEG to SEND as the next segment.
        subroutine segment(send)
            integer(int64), intent(in) :: send
            integer(int64) :: sb, se
            call insert_source_line(src, sbeg, send, keep_props, sb, se)
            call put_prop(sb, se, "occur_target", target)
            call put_prop(sb, se, "follow_link", t_value())
            call put_prop(sb, se, "help_echo", make_string("mouse-2: go to this occurrence"))
            call put_prop(sb, se, "mouse_face", sym("highlight"))
            seg_out = [seg_out, sb]
            seg_cur = [seg_cur, cur_off]
            seg_len = [seg_len, se - sb]
            nseg = nseg + 1
        end subroutine segment

        !> The byte offset in CURSTRING of the character offset C.
        integer function byte_of(c) result(off)
            integer(int64), intent(in) :: c
            integer(int64) :: n
            integer :: j
            off = 0
            n = 0
            do j = 1, len(curstring)
                if (iand(iachar(curstring(j:j)), 192) /= 128) then
                    if (n == c) return
                    n = n + 1
                end if
                off = j
            end do
        end function byte_of

        integer(int64) function chars(s2) result(c)
            character(len=*), intent(in) :: s2
            integer :: j
            c = 0
            do j = 1, len(s2)
                if (iand(iachar(s2(j:j)), 192) /= 128) c = c + 1
            end do
        end function chars
    end subroutine insert_match

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define Occur mode and Occur-Edit mode, give the Occur variables
    !> their defaults and register the commands (after Text mode, whose map
    !> Occur-Edit mode's inherits).
    subroutine syms_of_occur()
        type(status_type) :: st
        list_matching_lines_face = "match"
        list_matching_lines_buffer_name_face = "underline"
        list_matching_lines_current_line_face = "lazy_highlight"
        list_matching_lines_prefix_face = "shadow"
        if (.not. allocated(occur_excluded_properties)) then
            occur_excluded_properties = [character(len=24) :: "read_only", "invisible", "intangible", "field", &
                                         "mouse_face", "help_echo", "local_map", "keymap", "yank_handler", &
                                         "follow_link"]
        end if
        call defvar_nil(OCCUR_COLLECT_HISTORY)
        call set(sym(OCCUR_COLLECT_HISTORY), list(make_string("\1")), st)
        call occur_mode_hook%add(turn_on_font_lock_hook)
        call major_modes%occur_mode%define("occur_mode", "Occur", occur_mode, occur_mode_hook, &
                                           parent=major_modes%special_mode)
        occur_mode_map = major_modes%occur_mode%map
        call keymap_set(occur_mode_map, "C-c C-c", "occur_mode_goto_occurrence", st)
        call keymap_set(occur_mode_map, "e", "occur_edit_mode", st)
        call keymap_set(occur_mode_map, "C-m", "occur_mode_goto_occurrence", st)
        call keymap_set(occur_mode_map, "o", "occur_mode_goto_occurrence_other_window", st)
        call keymap_set(occur_mode_map, "C-o", "occur_mode_display_occurrence", st)
        call keymap_set(occur_mode_map, "n", "next_error_no_select", st)
        call keymap_set(occur_mode_map, "p", "previous_error_no_select", st)
        call keymap_set(occur_mode_map, "M-n", "occur_next", st)
        call keymap_set(occur_mode_map, "M-p", "occur_prev", st)
        call keymap_set(occur_mode_map, "r", "occur_rename_buffer", st)
        call major_modes%occur_edit_mode%define("occur_edit_mode", "Occur-Edit", occur_edit_mode, &
                                                occur_edit_mode_hook, parent=major_modes%occur_mode)
        occur_edit_mode_map = major_modes%occur_edit_mode%map
        call set_keymap_parent(occur_edit_mode_map, text_mode_map, st)
        call keymap_set(occur_edit_mode_map, "C-c C-c", "occur_cease_edit", st)
        call keymap_set(occur_edit_mode_map, "C-o", "occur_mode_display_occurrence", st)
        commands%keep_lines%run => cmd_keep_lines
        commands%delete_non_matching_lines%run => cmd_keep_lines
        commands%flush_lines%run => cmd_flush_lines
        commands%delete_matching_lines%run => cmd_flush_lines
        commands%kill_matching_lines%run => cmd_kill_matching_lines
        commands%copy_matching_lines%run => cmd_copy_matching_lines
        commands%how_many%run => cmd_how_many
        commands%count_matches%run => cmd_how_many
        commands%occur%run => cmd_occur
        commands%list_matching_lines%run => cmd_occur
        commands%multi_occur%run => cmd_multi_occur
        commands%multi_occur_in_matching_buffers%run => cmd_multi_occur_in_matching_buffers
        commands%occur_rename_buffer%run => cmd_occur_rename_buffer
        commands%occur_mode%run => occur_mode
        commands%occur_edit_mode%run => occur_edit_mode
        commands%occur_cease_edit%run => occur_cease_edit
        commands%occur_mode_goto_occurrence%run => occur_mode_goto_occurrence
        commands%occur_mode_mouse_goto%run => occur_mode_goto_occurrence
        commands%occur_mode_goto_occurrence_other_window%run => occur_mode_goto_occurrence_other_window
        commands%occur_mode_display_occurrence%run => occur_mode_display_occurrence
        commands%occur_next%run => occur_next
        commands%occur_prev%run => occur_prev
    end subroutine syms_of_occur

    !> turn-on-font-lock as a function of `occur_mode_hook'.
    recursive subroutine turn_on_font_lock_hook(status)
        type(status_type), intent(out) :: status
        call turn_on_font_lock(status)
    end subroutine turn_on_font_lock_hook

end module replace_fm
