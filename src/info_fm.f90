! info_fm.f90 -- Info package for Fortmacs.
! Ported from GNU Emacs 31.1 lisp/info.el.
!
! Copyright (C) 1985-1986, 1992-2026 Free Software Foundation, Inc.
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

!> The Info documentation browser (`info', C-h i): finding Info files
!> (with their compressed variants) in `info_directory_list', tag tables
!> and indirect (split) files, the directory node merged from the `dir'
!> files, and Info mode with its node commands: `info_next' (n),
!> `info_prev' (p), `info_up' (u, ^), `info_menu' (m), `info_follow_reference'
!> (f), `info_goto_node' (g), `info_directory' (d), `info_top_node' (t,
!> <), `info_final_node' (>), `info_forward_node' (]), `info_backward_node'
!> ([), `info_history_back' (l), `info_history_forward' (r), the scrolling
!> commands (SPC, DEL, S-SPC), the reference motion commands (TAB, M-TAB,
!> <backtab>), `info_follow_nearest_node' (RET), `info_nth_menu_item'
!> (1 to 9) and `info_copy_current_node_name' (w, c).
!>
!> The buffer-local variables `info_current_file', `info_current_node'
!> and `info_current_subfile' are read with the functions of those names
!> (unallocated for nil); the variables `info_history' and
!> `info_history_forward' (the stacks of nodes visited) are
!> `info_history_variable' and `info_history_forward_variable', as the
!> commands have their names.
!>
!> Nodes are fontified (`info_fontify_node'), with the header line and
!> breadcrumbs; T and L show the table of contents and the nodes
!> visited. `info_search' (s, S), the indexes (`info_index' i,
!> `info_index_next' ,, `info_virtual_index' I, `info_apropos') and
!> `info_goto_emacs_command_node' and `info_goto_emacs_key_command_node'
!> (C-h F, C-h K) are ported, with Isearch across nodes and
!> `info_display_manual' (C-h R).
!>
!> Deferred: `info_refill_paragraphs', `info_finder',
!> `info_goto_node_web' (G, needing browse-url), following URLs,
!> `info_standalone', `clone_buffer' (M-n), images (Fortmacs displays none, so their text
!> is shown), the mouse, menu bar, tool bar and context menu commands,
!> bookmarks, desktop saving, speedbar, active nodes
!> (`info_enable_active_nodes'), the display table for terminals without
!> UTF-8, and the Info files of the source tree (`source_directory').
module info_fm
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_USER_ERROR, FM_SEARCH_FAILED, &
                        FM_USER_SEARCH_FAILED, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type
    use lisp_fm, only: command_type, value_type, nil, t_value, nilp, make_string, make_list, list, make_fixnum, &
                       stringp, consp, &
                       fixnump, xstring, xfixnum, list_length, list_elt, make_symbol_ref, symbols, xsymbol, symbolp, &
                       list_elements
    use lread_fm, only: sym, intern
    use data_fm, only: logical_setting, set, add_local_variables_killer, symbol_value
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, buffer_live_p, buffer_serial_live, get_buffer_create, &
                         generate_new_buffer_name, kill_buffer, erase_buffer, restore_buffer_modified_p, get_buffer, &
                         inhibit_read_only, transient_mark_mode, header_line_format_type, buffer_list
    use marker_fm, only: make_marker, free_marker, marker_buffer, marker_position, set_marker, &
                         detach_marker, set_marker_insertion_type, byte_to_char, char_to_byte
    use editfns_fm, only: point, point_min, point_max, goto_char, bobp, eobp, bolp, eolp, buffer_size, widen, &
                          narrow_to_region, buffer_substring, insert_buffer_substring, delete_region, &
                          delete_and_extract_region, line_beginning_position, line_end_position
    use insdel_fm, only: insert
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line
    use search_fm, only: looking_at, string_match, search_forward, search_backward, re_search_forward, &
                         re_search_backward, match_beginning, match_end, replace_match, regexp_quote, &
                         string_index_to_offset, case_fold_search, search_spaces_regexp, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string_no_properties, looking_back, y_or_n_p
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, copy_syntax_table, modify_syntax_entry, &
                         syntax_table_live_p, skip_syntax_backward
    use casefiddle_fm, only: downcase_string, capitalize_string
    use textprop_fm, only: get_text_property, put_text_property, get_char_property, &
                           next_single_char_property_change, previous_single_char_property_change, &
                           next_single_property_change, remove_text_properties, propertized_concat, &
                           string_put_text_property, text_property_not_all
    use character_fm, only: multibyte_chars_in_text
    use itree_fm, only: make_overlay, overlay_put
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                        FACE_BACKGROUND_DARK
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_mode_enabled, turn_on_font_lock
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight
    use fill_fm, only: string_width
    use fileio_fm, only: file_exists_p, file_directory_p, file_name_directory, file_name_nondirectory, &
                         file_name_as_directory, directory_file_name, file_name_absolute_p, expand_file_name, &
                         substitute_in_file_name, insert_file_contents, file_attributes, file_stat_type
    use files_fm, only: file_truename, file_name_sans_extension, kill_buffer_hook, revert_buffer_function
    use dired_fm, only: directory_files
    use env_fm, only: getenv
    use config_fm, only: configure_info_directory
    use coding_fm, only: inhibit_null_byte_detection, buffer_file_coding_system
    use callproc_fm, only: call_process
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, find_command, commandp
    use keyboard_fm, only: this_command_keys, read_event, unread_command_events, read_key_sequence_vs, sit_for
    use isearch_fm, only: isearch_active, isearch_forward, isearch_wrapped, isearch_regexp, isearch_nonincremental, &
                          isearch_lax_whitespace, isearch_regexp_lax_whitespace, search_whitespace_regexp, &
                          isearch_search_proc, isearch_pop_proc, isearch_search_fun_default, isearch_mode_hook, &
                          isearch_mode_end_hook, isearch_search_fun_function, isearch_wrap_function, &
                          isearch_push_state_function, isearch_filter_predicate, set_isearch_wrapped
    use keymap_fm, only: keymap_set, where_is_internal, key_description, key_binding
    use commands_fm, only: commands
    use modes_fm, only: major_modes
    use derived_fm, only: derived_mode_p
    use xdisp_fm, only: message, tab_width
    use indent_fm, only: current_column, move_to_column
    use simple_fm, only: kill_new, count_lines, pos_visible_in_window_p, deactivate_mark, line_number_at_pos
    use window_fm, only: windows, display_buffer, select_window, get_buffer_window, window_start, set_window_start, &
                         scroll_up, scroll_down, recenter, display_alist_type, DISPLAY_ACTION_SAME_WINDOW, &
                         window_configuration_type, current_window_configuration, set_window_configuration, &
                         free_window_configuration, delete_other_windows, switch_to_buffer, bury_buffer, pop_to_buffer
    use frame_fm, only: selected_window, selected_frame, frame_window_p
    use dispnew_fm, only: ding
    use minibuf_fm, only: completion_ignore_case, try_completion, all_completions, test_completion, &
                          completion_table_type, completion_list_table, completion_function_table, &
                          completion_predicate, completion_obarray_table, read_string, COMPLETE_TRY, COMPLETE_ALL, &
                          COMPLETE_TEST, COMPLETE_BOUNDARIES
    use minibuffer_fm, only: completing_read, read_file_name, REQUIRE_MATCH_T
    use text_mode_fm, only: text_mode_syntax_table, text_mode_abbrev_table, syms_of_text_mode
    use shell_fm, only: list_buffers_directory
    use help_fm, only: substitute_command_keys, documentation_hook
    use help_mode_fm, only: help_mode
    implicit none(type, external)
    private

    public :: info_mode, info, info_other_window, info_pop_to_buffer, info_emacs_manual, info_emacs_bug, &
              info_initialize, info_find_file, info_find_node, info_on_current_buffer, info_goto_node, &
              info_url_for_node, info_next, info_prev, info_up, info_history_back, info_history_forward, &
              info_directory, info_follow_reference, info_menu, info_nth_menu_item, info_top_node, &
              info_final_node, info_forward_node, info_backward_node, info_next_menu_item, info_last_menu_item, &
              info_next_preorder, info_last_preorder, info_scroll_up, info_scroll_down, info_next_reference, &
              info_prev_reference, info_index_nodes, info_index_node, info_follow_nearest_node, &
              info_copy_current_node_name, info_extract_pointer, info_extract_menu_node_name, info_select_node, &
              info_current_file, info_current_node, info_current_subfile, info_insert_file_contents, &
              info_build_node_completions, info_undefined, info_help, info_summary, info_kill_buffer, &
              info_toc, info_history, info_fontify_node, syms_of_info, &
              info_search, info_search_backward, info_search_next, info_search_case_sensitively, &
              info_index, info_index_next, info_virtual_index, info_apropos, &
              info_goto_emacs_command_node, info_goto_emacs_key_command_node, info_history_variable, &
              info_display_manual, set_info_history_variable

    !> A node visited: the Info FILE (as `info_current_file'), the NODE name
    !> and the position POS of point in it.  An element of the stacks
    !> `info_history_variable' and `info_history_forward_variable'.
    type, public :: info_history_entry_type
        character(len=:), allocatable :: file
        character(len=:), allocatable :: node
        integer(int64) :: pos = 0
    end type info_history_entry_type

    !> A node visited, without a position: an element of
    !> `info_history_list'.
    type, public :: info_node_ref_type
        character(len=:), allocatable :: file
        character(len=:), allocatable :: node
    end type info_node_ref_type

    !> An element of `info_suffix_list': a file name SUFFIX and the
    !> command that decodes such a file (PROGRAM empty for none), given
    !> the file as standard input with the arguments ARGS.
    type, public :: info_suffix_type
        character(len=12) :: suffix = ""
        character(len=12) :: program = ""
        character(len=4) :: args = ""
    end type info_suffix_type

    !> An element of `info_streamline_headings': the headings of the
    !> directory node matching the regexp RE are merged into NAME.
    type, public :: info_heading_type
        character(len=64) :: re = ""
        character(len=32) :: name = ""
    end type info_heading_type

    ! A string, as an element of an array of strings of any lengths.
    type :: string_type
        character(len=:), allocatable :: s
    end type string_type

    ! An element of the table of contents of an Info file (an element of
    ! Info-toc-nodes): the node NAME, its PARENT from the Up pointer, the
    ! SECTION name in the Top node where it is placed, and its CHILDREN
    ! from the node menu.
    type :: toc_node_type
        character(len=:), allocatable :: name
        logical :: has_parent = .false., has_section = .false.
        character(len=:), allocatable :: parent, section
        type(string_type), allocatable :: children(:)
    end type toc_node_type

    abstract interface
        !> The find-file operation of a virtual Info file: the file name
        !> `info_find_file' returns for FILENAME.  Not a port.
        recursive subroutine info_virtual_find_file_proc(filename, result, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine info_virtual_find_file_proc

        !> The find-node operation of a virtual Info file: insert the text
        !> of node NODENAME of FILENAME in the current (empty) Info buffer.
        !> Not a port.
        recursive subroutine info_virtual_find_node_proc(filename, nodename, no_going_back, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: filename, nodename
            logical, intent(in) :: no_going_back
            type(status_type), intent(out) :: status
        end subroutine info_virtual_find_node_proc

        !> The toc-nodes operation of a virtual Info file: the node list
        !> of FILENAME with parent-children information (as
        !> `info_toc_nodes' gives).  Not a port.
        recursive subroutine info_virtual_toc_nodes_proc(filename, nodes)
            import :: toc_node_type
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(toc_node_type), allocatable, intent(out) :: nodes(:)
        end subroutine info_virtual_toc_nodes_proc
    end interface

    !> A virtual Info file (an element of `info_virtual_files') or node
    !> (of `info_virtual_nodes'): file or node names matching the regexp
    !> NAME are handled by FIND_FILE, FIND_NODE and TOC_NODES instead of
    !> being read from disk.  SLOW marks the files that are expensive to
    !> compute.
    type, public :: info_virtual_file_type
        character(len=64) :: name = ""
        procedure(info_virtual_find_file_proc), pointer, nopass :: find_file => null()
        procedure(info_virtual_find_node_proc), pointer, nopass :: find_node => null()
        procedure(info_virtual_toc_nodes_proc), pointer, nopass :: toc_nodes => null()
        logical :: slow = .false.
    end type info_virtual_file_type

    ! ------------------------------------------------------------------
    ! User options and variables
    ! ------------------------------------------------------------------

    !> True means don't record intermediate Info nodes to the history.
    !> Intermediate Info nodes are nodes visited by Info internally in the
    !> process of searching the node to display.  Intermediate nodes are
    !> not presented to the user.  Port of info.el
    !> Info-history-skip-intermediate-nodes.
    type(logical_setting), public :: info_history_skip_intermediate_nodes = &
                                     logical_setting(name="info_history_skip_intermediate_nodes", value=.true.)

    !> Default list of directories to search for Info documentation files.
    !> They are searched in the order they are given in the list.
    !> Therefore, the directory of Info files that come with Fortmacs
    !> normally should come last (so that local files override standard
    !> ones), unless Fortmacs is installed into a non-standard directory.
    !> In the latter case, the directory of Info files that come with
    !> Fortmacs should be first in this list.
    !>
    !> Once Info is started, the list of directories to search comes from
    !> the variable `info_directory_list'.
    !>
    !> This variable is used as the default for initializing
    !> `info_directory_list' when Info is started, unless the environment
    !> variable INFOPATH is set.  Port of info.el Info-default-directory-list.
    character(len=:), allocatable, public :: info_default_directory_list(:)

    !> List of directories to search for Info documentation files.
    !> If unallocated, meaning not yet initialized, Info uses the
    !> environment variable INFOPATH to initialize it, or
    !> `info_default_directory_list' if there is no INFOPATH variable in
    !> the environment, or the concatenation of the two if INFOPATH ends
    !> with a `path_separator'.
    !>
    !> When `info_directory_list' is initialized from the value of
    !> `info_default_directory_list', and Fortmacs is installed in one of
    !> the standard directories, the directory of Info files that come with
    !> Fortmacs is put last (so that local Info files override standard
    !> ones).
    !>
    !> When `info_directory_list' is initialized from the value of
    !> `info_default_directory_list', and Fortmacs is not installed in one
    !> of the standard directories, the first element of the resulting list
    !> is the directory where Fortmacs installs the Info files that come
    !> with it.  This is so that Fortmacs's own manual, which suits the
    !> version of Fortmacs you are using, will always be found first.  This
    !> is useful when you install an experimental version of Fortmacs
    !> without removing the standard installation.
    !>
    !> If you want to override the order of directories in
    !> `info_default_directory_list', set INFOPATH in the environment.
    !> Port of info.el Info-directory-list.
    character(len=:), allocatable, public :: info_directory_list(:)

    !> List of additional directories to search for Info documentation
    !> files.  These directories are searched after those in
    !> `info_directory_list'.  Port of info.el Info-additional-directory-list.
    character(len=:), allocatable, public :: info_additional_directory_list(:)

    !> If true, \<info_mode_map>\[info_scroll_up] in a menu visits the
    !> first subnode of that menu.  If false, \[info_scroll_up] scrolls
    !> the menu as long as it is not all visible and then visits the next
    !> node (if any) (in this case, the menu's subnodes are skipped).  Port
    !> of info.el Info-scroll-prefer-subnodes.
    type(logical_setting), public :: info_scroll_prefer_subnodes = &
                                     logical_setting(name="info_scroll_prefer_subnodes", value=.false.)

    !> Hook run when activating Info Mode.  Port of info.el Info-mode-hook
    !> (its default, `turn_on_font_lock', is Deferred with the
    !> fontification).
    type(hook_type), target, public :: info_mode_hook = hook_type(name="info_mode_hook")

    !> Hook run when an Info node is selected as the current node.  Port
    !> of info.el Info-selection-hook.
    type(hook_type), target, public :: info_selection_hook = hook_type(name="info_selection_hook")

    !> List of all Info nodes user has visited, the most recent first.
    !> Each element of the list is a `info_node_ref_type' (FILE NODE).
    !> Port of info.el Info-history-list.
    type(info_node_ref_type), allocatable, public :: info_history_list(:)

    !> List of file name suffixes and associated decoding commands.  Each
    !> entry has a SUFFIX and the PROGRAM to which the file is given as
    !> standard input, with the arguments ARGS.  If PROGRAM is empty, no
    !> decoding is done.  Because the suffixes are tried in order, the
    !> empty string should be last in the list.  Port of info.el
    !> Info-suffix-list.
    type(info_suffix_type), public :: info_suffix_list(36) = [ &
                                      info_suffix_type(".info.Z", "uncompress"), &
                                      info_suffix_type(".info.Y", "unyabba"), &
                                      info_suffix_type(".info.gz", "gunzip"), &
                                      info_suffix_type(".info.z", "gunzip"), &
                                      info_suffix_type(".info.bz2", "bzip2", "-dc"), &
                                      info_suffix_type(".info.xz", "unxz"), &
                                      info_suffix_type(".info.zst", "zstd", "-dc"), &
                                      info_suffix_type(".info.lz", "lzip", "-dc"), &
                                      info_suffix_type(".info", ""), &
                                      info_suffix_type("-info.Z", "uncompress"), &
                                      info_suffix_type("-info.Y", "unyabba"), &
                                      info_suffix_type("-info.gz", "gunzip"), &
                                      info_suffix_type("-info.bz2", "bzip2", "-dc"), &
                                      info_suffix_type("-info.z", "gunzip"), &
                                      info_suffix_type("-info.xz", "unxz"), &
                                      info_suffix_type("-info.zst", "zstd", "-dc"), &
                                      info_suffix_type("-info.lz", "lzip", "-dc"), &
                                      info_suffix_type("-info", ""), &
                                      info_suffix_type("/index.Z", "uncompress"), &
                                      info_suffix_type("/index.Y", "unyabba"), &
                                      info_suffix_type("/index.gz", "gunzip"), &
                                      info_suffix_type("/index.z", "gunzip"), &
                                      info_suffix_type("/index.bz2", "bzip2", "-dc"), &
                                      info_suffix_type("/index.xz", "unxz"), &
                                      info_suffix_type("/index.zst", "zstd", "-dc"), &
                                      info_suffix_type("/index.lz", "lzip", "-dc"), &
                                      info_suffix_type("/index", ""), &
                                      info_suffix_type(".Z", "uncompress"), &
                                      info_suffix_type(".Y", "unyabba"), &
                                      info_suffix_type(".gz", "gunzip"), &
                                      info_suffix_type(".z", "gunzip"), &
                                      info_suffix_type(".bz2", "bzip2", "-dc"), &
                                      info_suffix_type(".xz", "unxz"), &
                                      info_suffix_type(".zst", "zstd", "-dc"), &
                                      info_suffix_type(".lz", "lzip", "-dc"), &
                                      info_suffix_type("", "")]

    !> List of elements (RE NAME) to merge headings matching RE to NAME.
    !> Port of info.el Info-streamline-headings.
    type(info_heading_type), public :: info_streamline_headings(5) = [ &
                                       info_heading_type("Emacs", "Emacs"), &
                                       info_heading_type("Software development\|Programming", "Software development"), &
                                       info_heading_type("Compression\|Data Compression", "Compression"), &
                                       info_heading_type("Libraries", "Libraries"), &
                                       info_heading_type("Network applications\|World Wide Web\|Net Utilities", &
                                                         "Network applications")]

    !> List of definitions of virtual Info files.  Each element names a
    !> class of virtual Info file names by a regexp (NAME), chosen
    !> carefully not to cause file name clashes with existing file names,
    !> and the procedures handling their operations (FIND_FILE and
    !> FIND_NODE).  Port of info.el Info-virtual-files.
    type(info_virtual_file_type), allocatable, public :: info_virtual_files(:)

    !> List of definitions of virtual Info nodes.  Each element names a
    !> class of virtual Info node names by a regexp (NAME), chosen
    !> carefully not to cause node name clashes with existing node names,
    !> and the procedure handling their find-node operation (FIND_NODE).
    !> Port of info.el Info-virtual-nodes.
    type(info_virtual_file_type), allocatable, public :: info_virtual_nodes(:)

    !> The syntax table of `info_mode'.  Port of info.el Info-mode-syntax-table.
    type(handle_type), public :: info_mode_syntax_table

    !> The keymap of `info_mode'.  Port of info.el Info-mode-map.
    type(handle_type), public :: info_mode_map

    ! The value of Info-point-loc: the point location within a selected
    ! node.  If a string, the point is moved to the proper occurrence of
    ! the name of the followed cross reference within a selected node.  If
    ! a number, the point is moved to the corresponding line.
    integer, parameter :: LOC_NONE = 0, LOC_LINE = 1, LOC_NAME = 2
    integer :: point_loc_kind = LOC_NONE
    integer(int64) :: point_loc_line = 0
    character(len=:), allocatable :: point_loc_name

    ! The caches: Info-file-attributes (the modification times of the
    ! Info files read), Info-index-nodes (the index node names of visited
    ! Info files), Info-file-supports-index-cookies-list and
    ! Info-file-completions.
    type :: file_time_type
        character(len=:), allocatable :: name
        integer(int64) :: sec = 0
        integer(int32) :: nsec = 0
    end type file_time_type
    type(file_time_type), allocatable :: file_times(:)

    type :: index_nodes_type
        character(len=:), allocatable :: file
        type(string_type), allocatable :: nodes(:)
    end type index_nodes_type
    type(index_nodes_type), allocatable :: index_nodes_cache(:)

    type :: cookies_type
        character(len=:), allocatable :: file
        logical :: supports = .false.
    end type cookies_type
    type(cookies_type), allocatable :: cookies_cache(:)

    type :: completions_type
        character(len=:), allocatable :: file
        type(string_type), allocatable :: nodes(:)
    end type completions_type
    type(completions_type), allocatable :: file_completions(:)

    ! An index entry matching a topic (an element of
    ! Info-index-alternatives, Info-virtual-index-nodes and
    ! Info-apropos-nodes): the ENTRY text, with the part from MATCH_BEG to
    ! MATCH_END (characters) matching the topic, the node NODENAME it
    ! points to, the INDEX_NODE it was found in (the manual for
    ! `info_apropos') and the LINE in the node (0 for none).
    type :: index_match_type
        character(len=:), allocatable :: entry, nodename, index_node
        integer(int64) :: line = 0
        integer(int64) :: match_beg = 0, match_end = 0
    end type index_match_type

    ! The buffer-local values of an Info buffer, by serial number:
    ! Info-history, Info-history-forward, Info-current-file,
    ! Info-current-subfile, Info-current-node, Info-tag-table-marker,
    ! Info-tag-table-buffer, Info-current-file-completions,
    ! Info-file-supports-index-cookies, Info-current-node-virtual, the
    ! cache of the merged dir file (Info-dir-contents,
    ! Info-dir-contents-directory, Info-dir-file-attributes,
    ! Info-dir-file-name) and Info-complete-cache.
    type :: local_type
        integer(int64) :: serial = 0
        type(info_history_entry_type), allocatable :: history(:)
        type(info_history_entry_type), allocatable :: history_forward(:)
        character(len=:), allocatable :: current_file
        character(len=:), allocatable :: current_subfile
        character(len=:), allocatable :: current_node
        type(handle_type) :: tag_table_marker
        type(handle_type) :: tag_table_buffer
        logical :: has_completions = .false.
        type(string_type), allocatable :: completions(:)
        logical :: supports_index_cookies = .false.
        logical :: current_node_virtual = .false.
        character(len=:), allocatable :: dir_contents
        character(len=:), allocatable :: dir_contents_directory
        type(file_time_type), allocatable :: dir_file_attributes(:)
        character(len=:), allocatable :: dir_file_name
        logical :: has_complete_cache = .false.
        character(len=:), allocatable :: cache_file, cache_node, cache_string, cache_nodes
        type(string_type), allocatable :: cache_completions(:)
        ! The header-line-format of Info mode, (:eval (get-text-property
        ! (point-min) 'header-line)), and the header with its faces.
        logical :: header_from_property = .false.
        type(header_line_format_type) :: header
        ! Info-index-alternatives and Info--current-index-alternative.
        type(index_match_type), allocatable :: index_alternatives(:)
        integer(int64) :: current_index_alternative = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! A menu of a dir file being merged into the directory node: the
    ! NAME of its node and its text in BUFFER from BEG to END.
    type :: dir_node_type
        character(len=:), allocatable :: name
        type(handle_type) :: buffer
        integer(int64) :: beg = 0, end = 0
    end type dir_node_type

    ! Info-complete-menu-buffer: the buffer whose menu
    ! `complete_menu_item' completes.
    type(handle_type) :: complete_menu_buffer

    ! Info-read-node-completion-table: the node names of the current Info
    ! file while `read_node_name' reads one.
    type(string_type), allocatable :: read_node_completion_table(:)

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), US = achar(31), DEL = achar(127), &
                                   FF = achar(12), NUL = achar(0), BS = achar(8)

    !> Regexp that matches a menu entry name up to but not including the
    !> colon.  Because of ambiguities, this should be concatenated with
    !> something like `:' and `info_following_node_name_re'.  Port of
    !> info.el Info-menu-entry-name-re.
    character(len=*), parameter, public :: INFO_MENU_ENTRY_NAME_RE = "\(?:[^:]\|:[^:,.;() "//TAB//LF//"]\)*"

    !> Values of `info_fontify_maximum_menu_size' other than a number.
    integer, parameter, public :: INFO_FONTIFY_MAXIMUM_MENU_SIZE_NIL = -1, INFO_FONTIFY_MAXIMUM_MENU_SIZE_T = -2

    !> Maximum size of menu to fontify if `font_lock_mode' is true.
    !> Set to INFO_FONTIFY_MAXIMUM_MENU_SIZE_NIL to disable node
    !> fontification; set to INFO_FONTIFY_MAXIMUM_MENU_SIZE_T for no
    !> limit.  Port of info.el Info-fontify-maximum-menu-size.
    integer, public :: info_fontify_maximum_menu_size = 400000

    !> True to fontify references to visited nodes in `info_xref_visited'
    !> face.  Port of info.el Info-fontify-visited-nodes.
    type(logical_setting), public :: info_fontify_visited_nodes = &
                                     logical_setting(name="info_fontify_visited_nodes", value=.true.)

    !> True means to put the beginning-of-node links in an Emacs
    !> header-line.  A header-line does not scroll with the rest of the
    !> buffer.  Port of info.el Info-use-header-line.
    type(logical_setting), public :: info_use_header_line = &
                                     logical_setting(name="info_use_header_line", value=.true.)

    !> Values of `info_hide_note_references'.
    integer, parameter, public :: INFO_HIDE_NOTE_REFERENCES_NIL = 0, INFO_HIDE_NOTE_REFERENCES_T = 1, &
                                  INFO_HIDE_NOTE_REFERENCES_HIDE = 2, INFO_HIDE_NOTE_REFERENCES_TAG = 3

    !> If true, hide the tag and section reference in *note and * menu
    !> items.  If the value is INFO_HIDE_NOTE_REFERENCES_T, the default,
    !> replace "*note" with "see".  If the value is
    !> INFO_HIDE_NOTE_REFERENCES_HIDE, remove "*note" without replacing it
    !> with anything.  If the value is INFO_HIDE_NOTE_REFERENCES_TAG, the
    !> reference section is still shown.  INFO_HIDE_NOTE_REFERENCES_NIL
    !> completely disables this feature, leaving the original "*note"
    !> visible.  (`info_refill_paragraphs' is Deferred.)  Port of info.el
    !> Info-hide-note-references.
    integer, public :: info_hide_note_references = INFO_HIDE_NOTE_REFERENCES_T

    !> Depth of breadcrumbs to display.  0 means do not display
    !> breadcrumbs.  Port of info.el Info-breadcrumbs-depth.
    integer, public :: info_breadcrumbs_depth = 4

    !> Face for Info node names.  Port of info.el info-node.
    type(named_face_type), target, public :: info_node_face
    !> Face for info titles at level 1.  Port of info.el info-title-1.
    type(named_face_type), target, public :: info_title_1_face
    !> Face for info titles at level 2.  Port of info.el info-title-2.
    type(named_face_type), target, public :: info_title_2_face
    !> Face for info titles at level 3.  Port of info.el info-title-3.
    type(named_face_type), target, public :: info_title_3_face
    !> Face for info titles at level 4.  Port of info.el info-title-4.
    type(named_face_type), target, public :: info_title_4_face
    !> Face for headers in Info menus.  Port of info.el info-menu-header.
    type(named_face_type), target, public :: info_menu_header_face
    !> Face used to emphasize `*' in an Info menu.  The face is assigned
    !> to the third, sixth, and ninth `*' for easier orientation.  See
    !> `info_nth_menu_item'.  Port of info.el info-menu-star.
    type(named_face_type), target, public :: info_menu_star_face
    !> Face for unvisited Info cross-references.  Port of info.el info-xref.
    type(named_face_type), target, public :: info_xref_face
    !> Face for visited Info cross-references.  Port of info.el
    !> info-xref-visited.
    type(named_face_type), target, public :: info_xref_visited_face
    !> Face for Info cross-references in a node header.  Port of info.el
    !> info-header-xref.
    type(named_face_type), target, public :: info_header_xref_face
    !> Face for Info nodes in a node header.  Port of info.el
    !> info-header-node.
    type(named_face_type), target, public :: info_header_node_face
    !> Face used to highlight matches in an index entry.  Port of info.el
    !> info-index-match.
    type(named_face_type), target, public :: info_index_match_face
    !> Face used for quoted elements.  Port of info.el Info-quoted.
    type(named_face_type), target, public :: info_quoted_face

    ! A cached search of `info_virtual_index': the matches of TOPIC in
    ! FILE.
    type :: virtual_index_type
        character(len=:), allocatable :: file, topic
        type(index_match_type), allocatable :: matches(:)
    end type virtual_index_type

    ! Info-virtual-index-nodes, the most recent first.
    type(virtual_index_type), allocatable :: virtual_index_nodes(:)

    ! A cached search of `info_apropos': the NODENAME that holds the search
    ! result, the search STRING and its MATCHES (HAS_MATCHES false for
    ! nil).
    type :: apropos_node_type
        character(len=:), allocatable :: nodename, string
        logical :: has_matches = .false.
        type(index_match_type), allocatable :: matches(:)
    end type apropos_node_type

    ! Info-apropos-nodes, the most recent first.
    type(apropos_node_type), allocatable :: apropos_nodes(:)

    !> Info file name of the virtual manual for matches of `info_apropos'.
    !> Port of info.el Info-apropos-file.
    character(len=*), parameter, public :: INFO_APROPOS_FILE = "*Apropos*"

    !> If true, regular expression to match a sequence of whitespace
    !> chars.  This applies to Info search for regular expressions.  You
    !> might want to use something like "[ \t\r\n]+" instead.  Don't add
    !> any capturing groups into this value; that can change the numbering
    !> of existing capture groups in unexpected ways.  (Unallocated for
    !> nil.)  Port of info.el Info-search-whitespace-regexp.
    character(len=:), allocatable, public :: info_search_whitespace_regexp

    !> If true, isearch in Info searches through multiple nodes.
    !> Before leaving the initial Info node, where isearch was started,
    !> it fails once with the error message [end of node], and with
    !> subsequent C-s/C-r continues through other nodes without failing
    !> with this error message in other nodes.  When isearch fails for
    !> the rest of the manual, it displays the error message [end of
    !> manual], wraps around the whole manual and restarts the search
    !> from the top/final node depending on search direction.
    !>
    !> Setting this option to false restores the default isearch behavior
    !> with wrapping around the current Info node.  Port of info.el
    !> Info-isearch-search.
    type(logical_setting), public :: info_isearch_search = logical_setting(name="info_isearch_search", value=.true.)

    ! info.el Info-isearch-initial-node (unallocated for nil),
    ! Info-isearch-initial-history and Info-isearch-initial-history-list.
    character(len=:), allocatable :: isearch_initial_node
    type(info_history_entry_type), allocatable :: isearch_initial_history(:)
    type(info_node_ref_type), allocatable :: isearch_initial_history_list(:)

    ! The value of `case_fold_search' from previous `info_search' command
    ! (Info-search-case-fold).
    logical :: info_search_case_fold = .false.

    !> Warn when wrapping to the beginning/end when displaying index
    !> alternatives.  Port of info.el Info-warn-on-index-alternatives-wrap.
    type(logical_setting), public :: info_warn_on_index_alternatives_wrap = &
                                     logical_setting(name="info_warn_on_index_alternatives_wrap", value=.true.)

    !> An element of `info_file_list_for_emacs': the PREFIX of command
    !> names and the Info FILE that documents them.
    type, public :: info_file_prefix_type
        character(len=16) :: prefix = ""
        character(len=12) :: file = ""
    end type info_file_prefix_type

    !> List of Info files that describe Emacs commands.  An element gives
    !> a name PREFIX and the FILE to look in for the commands whose names
    !> start with it (followed by "_" or nothing).  Port of info.el
    !> Info-file-list-for-emacs.
    type(info_file_prefix_type), public :: info_file_list_for_emacs(54) = [ &
                                           info_file_prefix_type("ediff", "ediff"), &
                                           info_file_prefix_type("eudc", "eudc"), &
                                           info_file_prefix_type("forms", "forms"), &
                                           info_file_prefix_type("gnus", "gnus"), &
                                           info_file_prefix_type("info", "info"), &
                                           info_file_prefix_type("Info", "info"), &
                                           info_file_prefix_type("mh", "mh-e"), &
                                           info_file_prefix_type("sc", "sc"), &
                                           info_file_prefix_type("message", "message"), &
                                           info_file_prefix_type("dired", "dired-x"), &
                                           info_file_prefix_type("viper", "viper"), &
                                           info_file_prefix_type("idlwave", "idlwave"), &
                                           info_file_prefix_type("c", "ccmode"), &
                                           info_file_prefix_type("c++", "ccmode"), &
                                           info_file_prefix_type("objc", "ccmode"), &
                                           info_file_prefix_type("java", "ccmode"), &
                                           info_file_prefix_type("idl", "ccmode"), &
                                           info_file_prefix_type("pike", "ccmode"), &
                                           info_file_prefix_type("skeleton", "autotype"), &
                                           info_file_prefix_type("auto_insert", "autotype"), &
                                           info_file_prefix_type("copyright", "autotype"), &
                                           info_file_prefix_type("executable", "autotype"), &
                                           info_file_prefix_type("tempo", "autotype"), &
                                           info_file_prefix_type("hippie_expand", "autotype"), &
                                           info_file_prefix_type("cvs", "pcl-cvs"), &
                                           info_file_prefix_type("ada", "ada-mode"), &
                                           info_file_prefix_type("calc", "calc"), &
                                           info_file_prefix_type("calcAlg", "calc"), &
                                           info_file_prefix_type("calcDigit", "calc"), &
                                           info_file_prefix_type("calcVar", "calc"), &
                                           info_file_prefix_type("ebrowse", "ebrowse"), &
                                           info_file_prefix_type("eshell", "eshell"), &
                                           info_file_prefix_type("cl", "cl"), &
                                           info_file_prefix_type("reftex", "reftex"), &
                                           info_file_prefix_type("speedbar", "speedbar"), &
                                           info_file_prefix_type("widget", "widget"), &
                                           info_file_prefix_type("woman", "woman"), &
                                           info_file_prefix_type("mail_header", "emacs-mime"), &
                                           info_file_prefix_type("mail_content", "emacs-mime"), &
                                           info_file_prefix_type("mail_encode", "emacs-mime"), &
                                           info_file_prefix_type("mail_decode", "emacs-mime"), &
                                           info_file_prefix_type("rfc2045", "emacs-mime"), &
                                           info_file_prefix_type("rfc2231", "emacs-mime"), &
                                           info_file_prefix_type("rfc2047", "emacs-mime"), &
                                           info_file_prefix_type("rfc2045", "emacs-mime"), &
                                           info_file_prefix_type("rfc1843", "emacs-mime"), &
                                           info_file_prefix_type("ietf_drums", "emacs-mime"), &
                                           info_file_prefix_type("quoted_printable", "emacs-mime"), &
                                           info_file_prefix_type("binhex", "emacs-mime"), &
                                           info_file_prefix_type("uudecode", "emacs-mime"), &
                                           info_file_prefix_type("mailcap", "emacs-mime"), &
                                           info_file_prefix_type("mm", "emacs-mime"), &
                                           info_file_prefix_type("mml", "emacs-mime"), &
                                           info_file_prefix_type("tramp", "tramp")]

    ! Info-complete-nodes: the other nodes whose menus `complete_menu_item'
    ! completes in (the index nodes, for `info_index').
    type(string_type), allocatable :: complete_nodes(:)

    ! The cached table of contents of FILE.
    type :: toc_file_type
        character(len=:), allocatable :: file
        type(toc_node_type), allocatable :: nodes(:)
    end type toc_file_type
    type(toc_file_type), allocatable :: toc_cache(:)

    abstract interface
        !> A URL-specification function of `info_url_alist': the URL of
        !> node NODE (URL-encoded as ENCODED) of manual MANUAL (a subroutine:
        !> gfortran frees a pointer component to a function with a
        !> deferred-length result).  Not a port.
        subroutine info_url_proc(manual, node, encoded, url)
            implicit none(type, external)
            character(len=*), intent(in) :: manual, node, encoded
            character(len=:), allocatable, intent(out) :: url
        end subroutine info_url_proc
    end interface

    !> An element of `info_url_alist': the names of one or more MANUALS
    !> and their URL specification, either the string URL or the function
    !> FN.
    type, public :: info_url_type
        character(len=16) :: manuals(80) = ""
        character(len=80) :: url = ""
        procedure(info_url_proc), pointer, nopass :: fn => null()
    end type info_url_type

    !> List telling `info_mode' where manuals are accessible online.
    !>
    !> Each element of this list has the names of one or more MANUALS and
    !> a URL specification: a string URL in which the substring "%m" will
    !> be expanded to the manual-name and "%n" to the node-name.  "%e" will
    !> expand to the URL-encoded node-name, including the `.html'
    !> extension; in case of the Top node, it will expand to the empty
    !> string.  (The URL-encoding of the node-name mimics GNU Texinfo, as
    !> documented at Info node `(texinfo)HTML Xref Node Name Expansion'.)
    !> Alternatively, the URL specification can be a function FN which is
    !> given manual-name, node-name and URL-encoded node-name as arguments,
    !> and is expected to return the corresponding URL as a string.
    !>
    !> This variable particularly affects the command
    !> `info_goto_node_web', which see.
    !>
    !> The default value of this variable refers to the official,
    !> HTTPS-accessible HTML-representations of all manuals that Emacs
    !> includes.  These URLs refer to the most recently released version
    !> of Emacs, disregarding the version of the running Fortmacs.  In
    !> other words, the content of your local Info node and the associated
    !> online node may differ.  The resource represented by the generated
    !> URL may even be not found by the gnu.org server.  Port of info.el
    !> Info-url-alist.
    type(info_url_type), allocatable, public :: info_url_alist(:)

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
    subroutine kill_info_locals()
        integer :: k

        k = local_index(.false.)
        if (k > 0) locals(k) = local_type()
    end subroutine kill_info_locals

    !> The current buffer's `info_current_file': the Info file that Info
    !> is now looking at (unallocated for nil).  This is the name that was
    !> specified in Info, not the actual file name.  It doesn't contain
    !> directory names or file name extensions added by Info.  Port of
    !> info.el Info-current-file.
    function info_current_file() result(file)
        character(len=:), allocatable :: file
        integer :: k

        k = local_index(.false.)

        if (k > 0) then
            if (allocated(locals(k)%current_file)) file = locals(k)%current_file
        end if
    end function info_current_file

    !> The current buffer's `info_current_subfile': the Info subfile that
    !> is actually in the *info* buffer now (unallocated if the current
    !> Info file is not split into subfiles).  Port of info.el
    !> Info-current-subfile.
    function info_current_subfile() result(file)
        character(len=:), allocatable :: file
        integer :: k

        k = local_index(.false.)

        if (k > 0) then
            if (allocated(locals(k)%current_subfile)) file = locals(k)%current_subfile
        end if
    end function info_current_subfile

    !> The current buffer's `info_current_node': the name of the node that
    !> Info is now looking at (unallocated for nil).  Port of info.el
    !> Info-current-node.
    function info_current_node() result(node)
        character(len=:), allocatable :: node
        integer :: k

        k = local_index(.false.)

        if (k > 0) then
            if (allocated(locals(k)%current_node)) node = locals(k)%current_node
        end if
    end function info_current_node

    !> The current buffer's `info_history_variable': the stack of Info
    !> nodes the user has visited, the most recent first.  Each element of
    !> the stack is a list (FILENAME NODENAME BUFFERPOS).  Port of info.el
    !> Info-history (the variable).
    subroutine info_history_variable(history)
        type(info_history_entry_type), allocatable, intent(out) :: history(:)
        integer :: k

        k = local_index(.false.)
        allocate (history(0))

        if (k > 0) then
            if (allocated(locals(k)%history)) history = locals(k)%history
        end if
    end subroutine info_history_variable

    !> Set the current buffer's `info_history_variable' to HISTORY.
    !> Not a port.
    subroutine set_info_history_variable(history)
        type(info_history_entry_type), intent(in) :: history(:)
        integer :: k

        k = here()
        locals(k)%history = history
    end subroutine set_info_history_variable

    !> The current buffer's `info_history_forward_variable': the stack of
    !> Info nodes the user has visited with the `info_history_back'
    !> command, the most recent first.  Port of info.el
    !> Info-history-forward (the variable).
    subroutine info_history_forward_variable(history)
        type(info_history_entry_type), allocatable, intent(out) :: history(:)
        integer :: k

        k = local_index(.false.)
        allocate (history(0))

        if (k > 0) then
            if (allocated(locals(k)%history_forward)) history = locals(k)%history_forward
        end if
    end subroutine info_history_forward_variable

    ! The entry of the current buffer, made if needed.  Not a port.
    integer function here() result(k)
        k = local_index(.true.)
        if (.not. allocated(locals(k)%history)) allocate (locals(k)%history(0))
        if (.not. allocated(locals(k)%history_forward)) allocate (locals(k)%history_forward(0))
    end function here

    ! (setq Info-current-file FILE); an empty FILE with NIL true for nil.
    ! Not a port.
    subroutine set_current_file(file, clear)
        character(len=*), intent(in) :: file
        logical, intent(in), optional :: clear
        integer :: k

        k = here()

        if (present(clear)) then
            if (clear) then
                if (allocated(locals(k)%current_file)) deallocate (locals(k)%current_file)
                return
            end if
        end if

        locals(k)%current_file = file
    end subroutine set_current_file

    ! (setq Info-current-node NODE), or nil if absent.  Not a port.
    subroutine set_current_node(node)
        character(len=*), intent(in), optional :: node
        integer :: k

        k = here()

        if (present(node)) then
            locals(k)%current_node = node
        else if (allocated(locals(k)%current_node)) then
            deallocate (locals(k)%current_node)
        end if
    end subroutine set_current_node

    ! (setq Info-current-subfile FILE), or nil if absent.  Not a port.
    subroutine set_current_subfile(file)
        character(len=*), intent(in), optional :: file
        integer :: k

        k = here()

        if (present(file)) then
            locals(k)%current_subfile = file
        else if (allocated(locals(k)%current_subfile)) then
            deallocate (locals(k)%current_subfile)
        end if
    end subroutine set_current_subfile

    ! Whether Info-current-file is non-nil.  Not a port.
    logical function has_current_file()
        integer :: k

        k = local_index(.false.)
        has_current_file = .false.
        if (k > 0) has_current_file = allocated(locals(k)%current_file)
    end function has_current_file

    ! Whether Info-current-node is non-nil.  Not a port.
    logical function has_current_node()
        integer :: k

        k = local_index(.false.)
        has_current_node = .false.
        if (k > 0) has_current_node = allocated(locals(k)%current_node)
    end function has_current_node

    ! Info-current-file, "" for nil.  Not a port.
    function current_file_or_empty() result(file)
        character(len=:), allocatable :: file

        file = info_current_file()
        if (.not. allocated(file)) file = ""
    end function current_file_or_empty

    ! Info-current-node, "" for nil.  Not a port.
    function current_node_or_empty() result(node)
        character(len=:), allocatable :: node

        node = info_current_node()
        if (.not. allocated(node)) node = ""
    end function current_node_or_empty

    ! (push (list FILE NODE POS) STACK).  Not a port.
    subroutine push_history(stack, file, node, pos)
        type(info_history_entry_type), allocatable, intent(inout) :: stack(:)
        character(len=*), intent(in) :: file, node
        integer(int64), intent(in) :: pos
        type(info_history_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(stack)) allocate (stack(0))
        n = size(stack)
        allocate (grown(n + 1))
        grown(1)%file = file
        grown(1)%node = node
        grown(1)%pos = pos

        do i = 1, n
            call move_alloc(stack(i)%file, grown(i + 1)%file)
            call move_alloc(stack(i)%node, grown(i + 1)%node)
            grown(i + 1)%pos = stack(i)%pos
        end do

        call move_alloc(grown, stack)
    end subroutine push_history

    ! (setq STACK (cdr STACK)).  Not a port.
    subroutine pop_history(stack)
        type(info_history_entry_type), allocatable, intent(inout) :: stack(:)
        type(info_history_entry_type), allocatable :: rest(:)
        integer :: i, n

        n = size(stack)
        if (n == 0) return
        allocate (rest(n - 1))

        do i = 2, n
            call move_alloc(stack(i)%file, rest(i - 1)%file)
            call move_alloc(stack(i)%node, rest(i - 1)%node)
            rest(i - 1)%pos = stack(i)%pos
        end do

        call move_alloc(rest, stack)
    end subroutine pop_history

    ! Add a new unique history item (FILE NODE) to the front of
    ! `info_history_list'.
    ! Port of info.el:Info-find-node-2 and info.el:Info-select-node, in
    ! part.
    subroutine note_history_list(file, node)
        character(len=*), intent(in) :: file, node
        type(info_node_ref_type), allocatable :: grown(:)
        integer :: i, n, m

        if (.not. allocated(info_history_list)) allocate (info_history_list(0))
        n = size(info_history_list)
        allocate (grown(n + 1))
        grown(1)%file = file
        grown(1)%node = node
        m = 1

        do i = 1, n
            if (info_history_list(i)%file == file .and. info_history_list(i)%node == node) cycle
            m = m + 1
            call move_alloc(info_history_list(i)%file, grown(m)%file)
            call move_alloc(info_history_list(i)%node, grown(m)%node)
        end do

        info_history_list = grown(1:m)
    end subroutine note_history_list

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Signal a user-error with MESSAGE.  Not a port.
    subroutine user_error(status, message)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: message

        call set_error(status, FM_USER_ERROR, message)
    end subroutine user_error

    ! Signal an error with MESSAGE.  Not a port.
    subroutine plain_error(status, message)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: message

        call set_error(status, FM_GENERIC_ERROR, message)
    end subroutine plain_error

    ! Whether the last search or match succeeded for subexpression N.
    ! Not a port.
    logical function matched_group(n, status)
        integer, intent(in) :: n
        type(status_type), intent(inout) :: status

        matched_group = match_beginning(n, status) > 0
    end function matched_group

    ! (string-match RE S) as a logical, without changing the match data
    ! unless MODIFY.  Not a port.
    logical function smatch(re, s, modify)
        character(len=*), intent(in) :: re, s
        logical, intent(in), optional :: modify
        type(status_type) :: st
        logical :: inhibit

        inhibit = .true.
        if (present(modify)) inhibit = .not. modify
        smatch = string_match(re, s, st, inhibit_modify=inhibit) > 0
    end function smatch

    ! (replace-regexp-in-string "[ \t\n]+" " " S): each run of white
    ! space as one space.  Port of info.el:info--node-canonicalize-whitespace.
    function canonicalize_whitespace(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i
        logical :: in_space

        t = ""
        in_space = .false.

        do i = 1, len(s)
            if (s(i:i) == " " .or. s(i:i) == TAB .or. s(i:i) == LF) then
                if (.not. in_space) t = t//" "
                in_space = .true.
            else
                t = t//s(i:i)
                in_space = .false.
            end if
        end do
    end function canonicalize_whitespace

    ! (string-replace FROM TO S).  Not a port.
    function replace_all(from, to, s) result(t)
        character(len=*), intent(in) :: from, to, s
        character(len=:), allocatable :: t
        integer :: i, j

        t = ""
        i = 1

        do
            j = index(s(i:), from)
            if (j == 0 .or. len(from) == 0) exit
            t = t//s(i:i + j - 2)//to
            i = i + j - 1 + len(from)
        end do

        t = t//s(i:)
    end function replace_all

    ! S without its trailing white space (the "\\s +\\'" trims).
    ! Not a port.
    function trim_trailing_space(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: n

        n = len(s)

        do while (n > 0)
            if (s(n:n) /= " " .and. s(n:n) /= TAB .and. s(n:n) /= LF .and. s(n:n) /= achar(13) .and. &
                s(n:n) /= FF) exit
            n = n - 1
        end do

        t = s(1:n)
    end function trim_trailing_space

    ! (downcase S).  Not a port.
    function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t

        t = downcase_string(s)
    end function lower

    ! Point, saved for save-excursion.  Not a port.
    integer(int64) function save_point()
        save_point = point()
    end function save_point

    ! Go to the beginning of the accessible text.  Not a port.
    subroutine goto_min()
        call goto_char(point_min())
    end subroutine goto_min

    ! (forward-line N), ignoring the shortage.  Not a port.
    subroutine fwd_line(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage

        shortage = forward_line(n)
    end subroutine fwd_line

    ! Search forward for STRING without error; FOUND is true if found.
    ! Not a port.
    logical function sfwd(string, bound, move)
        character(len=*), intent(in) :: string
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: move
        integer(int64) :: found
        type(status_type) :: st
        integer :: mode

        mode = SEARCH_NOERROR

        if (present(move)) then
            if (move) mode = SEARCH_MOVE
        end if

        call search_forward(string, found, st, bound=bound, noerror=mode)
        sfwd = found /= 0 .and. st%ok()
    end function sfwd

    ! Search backward for STRING without error.  Not a port.
    logical function sbwd(string, bound, move)
        character(len=*), intent(in) :: string
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: move
        integer(int64) :: found
        type(status_type) :: st
        integer :: mode

        mode = SEARCH_NOERROR

        if (present(move)) then
            if (move) mode = SEARCH_MOVE
        end if

        call search_backward(string, found, st, bound=bound, noerror=mode)
        sbwd = found /= 0 .and. st%ok()
    end function sbwd

    ! Search forward for regexp RE without error.  Not a port.
    logical function refwd(re, bound, move)
        character(len=*), intent(in) :: re
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: move
        integer(int64) :: found
        type(status_type) :: st
        integer :: mode

        mode = SEARCH_NOERROR

        if (present(move)) then
            if (move) mode = SEARCH_MOVE
        end if

        call re_search_forward(re, found, st, bound=bound, noerror=mode)
        refwd = found /= 0 .and. st%ok()
    end function refwd

    ! Search backward for regexp RE without error.  Not a port.
    logical function rebwd(re, bound, move)
        character(len=*), intent(in) :: re
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: move
        integer(int64) :: found
        type(status_type) :: st
        integer :: mode

        mode = SEARCH_NOERROR

        if (present(move)) then
            if (move) mode = SEARCH_MOVE
        end if

        call re_search_backward(re, found, st, bound=bound, noerror=mode)
        rebwd = found /= 0 .and. st%ok()
    end function rebwd

    ! (looking-at RE).  Not a port.
    logical function looking(re)
        character(len=*), intent(in) :: re
        type(status_type) :: st

        looking = looking_at(re, st)
        if (.not. st%ok()) looking = .false.
    end function looking

    ! (match-beginning N), 0 if the group did not match.  Not a port.
    integer(int64) function mbeg(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mbeg = match_beginning(n, st)
    end function mbeg

    ! (match-end N), 0 if the group did not match.  Not a port.
    integer(int64) function mend(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mend = match_end(n, st)
    end function mend

    ! The buffer text from START to END, without properties.  Not a port.
    function text(start, end) result(s)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(start, end, s, st)
        if (.not. st%ok()) s = ""
    end function text

    ! (window-start) of the selected window.  Not a port.
    integer(int64) function sel_window_start()
        type(handle_type) :: w

        w = selected_window()
        sel_window_start = window_start(windows(w%index))
    end function sel_window_start

    ! (set-window-start (selected-window) POS).  Not a port.
    subroutine set_sel_window_start(pos, status)
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        type(handle_type) :: w

        w = selected_window()
        call set_window_start(windows(w%index), pos, status)
    end subroutine set_sel_window_start

    ! (pos-visible-in-window-p POS) in the selected window.  Not a port.
    logical function visible(pos)
        integer(int64), intent(in) :: pos
        type(handle_type) :: w

        w = selected_window()
        visible = pos_visible_in_window_p(pos, windows(w%index))
    end function visible

    ! Run the case-fold-search binding: set it to FOLD and return the old
    ! value.  Not a port.
    logical function bind_case_fold(fold) result(old)
        logical, intent(in) :: fold

        old = case_fold_search%value
        case_fold_search%value = fold
    end function bind_case_fold

    ! Append S to the list LIST.  Not a port.
    subroutine add_string(list, s)
        type(string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(string_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(list(i)%s, grown(i)%s)
        end do

        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine add_string

    ! Whether LIST has the string S (with case ignored if FOLD).
    ! Not a port.
    logical function has_string(list, s, fold)
        type(string_type), intent(in) :: list(:)
        character(len=*), intent(in) :: s
        logical, intent(in), optional :: fold
        integer :: i
        logical :: f

        f = .false.
        if (present(fold)) f = fold
        has_string = .false.

        do i = 1, size(list)
            if (f) then
                if (lower(list(i)%s) == lower(s)) has_string = .true.
            else
                if (list(i)%s == s) has_string = .true.
            end if

            if (has_string) return
        end do
    end function has_string

    ! The directory names of a setting such as `info_directory_list'
    ! (trimmed), empty if it is unallocated.  Not a port.
    subroutine setting_dirs(setting, list)
        character(len=:), intent(in), allocatable :: setting(:)
        type(string_type), allocatable, intent(out) :: list(:)
        integer :: i

        allocate (list(0))
        if (.not. allocated(setting)) return

        do i = 1, size(setting)
            call add_string(list, trim(setting(i)))
        end do
    end subroutine setting_dirs

    ! LIST as an array for a setting such as `info_directory_list'.
    ! Not a port.
    subroutine list_to_setting(list, setting)
        type(string_type), intent(in) :: list(:)
        character(len=:), allocatable, intent(out) :: setting(:)
        integer :: i, n

        n = 1

        do i = 1, size(list)
            n = max(n, len(list(i)%s))
        end do

        allocate (character(len=n) :: setting(size(list)))

        do i = 1, size(list)
            setting(i) = list(i)%s
        end do
    end subroutine list_to_setting

    ! The directories in which Info looks for files and dir files:
    ! `info_directory_list' then `info_additional_directory_list'.
    ! Not a port.
    subroutine search_dirs(list)
        type(string_type), allocatable, intent(out) :: list(:)
        type(string_type), allocatable :: more(:)
        integer :: i

        call setting_dirs(info_directory_list, list)
        call setting_dirs(info_additional_directory_list, more)

        do i = 1, size(more)
            call add_string(list, more(i)%s)
        end do
    end subroutine search_dirs

    ! Return a copy of DIRS with all non-existent directories removed.
    ! Port of files.el:prune-directory-list (without KEEP and REJECT).
    subroutine prune_directory_list(dirs, pruned)
        type(string_type), intent(in) :: dirs(:)
        type(string_type), allocatable, intent(out) :: pruned(:)
        integer :: i

        allocate (pruned(0))

        do i = 1, size(dirs)
            if (file_directory_p(dirs(i)%s)) call add_string(pruned, dirs(i)%s)
        end do
    end subroutine prune_directory_list

    ! (expand-file-name NAME DIR).  Not a port.
    function expand(name, dir) result(full)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: dir
        character(len=:), allocatable :: full
        type(status_type) :: st

        if (present(dir)) then
            call expand_file_name(name, full, st, default_directory=dir)
        else
            call expand_file_name(name, full, st)
        end if

        if (.not. st%ok()) full = name
    end function expand

    ! ------------------------------------------------------------------
    ! Finding and reading Info files
    ! ------------------------------------------------------------------

    ! Whether FILENAME exists and is not a directory.  Port of
    ! info.el:info-file-exists-p.
    logical function info_file_exists_p(filename)
        character(len=*), intent(in) :: filename

        info_file_exists_p = file_exists_p(filename) .and. .not. file_directory_p(filename)
    end function info_file_exists_p

    !> Insert the contents of an Info file in the current buffer.
    !> Do the right thing if the file has been compressed or zipped.
    !> Port of info.el:info-insert-file-contents (the decoding command reads
    !> the file itself as its standard input, which comes to the same as
    !> piping the literal contents through it).
    recursive subroutine info_insert_file_contents(filename, status, visit)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: visit
        character(len=:), allocatable :: fullname, dir
        type(value_type) :: result, args
        type(file_stat_type) :: stat
        integer(int64) :: inserted
        integer :: tail
        logical :: found, done, v, null_detection

        status = status_type()
        v = .false.
        if (present(visit)) v = visit
        tail = 0

        if (info_file_exists_p(filename)) then
            ! FILENAME exists--see if that name contains a suffix.
            ! If so, set DECODE accordingly.
            do tail = 1, size(info_suffix_list)
                if (ends_with(filename, trim(info_suffix_list(tail)%suffix))) exit
            end do

            fullname = filename
        else
            ! Try adding suffixes to FILENAME and see if we can find something.
            done = .false.

            do tail = 1, size(info_suffix_list)
                fullname = filename//trim(info_suffix_list(tail)%suffix)

                if (info_file_exists_p(fullname)) then
                    done = .true.
                    exit
                end if
            end do

            if (.not. done) then
                call plain_error(status, "Can't find "//filename//" or any compressed version of it")
                return
            end if
        end if

        ! (jka-compr, which would conflict, is not ported.)
        null_detection = inhibit_null_byte_detection%value
        ! Index nodes include null bytes.
        inhibit_null_byte_detection%value = .true.

        if (tail <= size(info_suffix_list)) then
            if (len_trim(info_suffix_list(tail)%program) > 0) then
                dir = file_name_directory(fullname)
                if (len(dir) == 0) dir = buffers(cur)%directory

                if (len_trim(info_suffix_list(tail)%args) > 0) then
                    args = list(make_string(trim(info_suffix_list(tail)%args)))
                else
                    args = nil()
                end if

                call call_process(trim(info_suffix_list(tail)%program), result, status, infile=fullname, &
                                  destination=t_value(), args=args)
                inhibit_null_byte_detection%value = null_detection
                if (.not. status%ok()) return
                call note_file_time(fullname)
                return
            end if
        end if

        call insert_file_contents(fullname, v, inserted, status)
        inhibit_null_byte_detection%value = null_detection
        if (.not. status%ok()) return
        call note_file_time(fullname)

    contains

        ! Clear the caches of modified Info files and record the
        ! modification time of FULL (`Info-file-attributes').
        subroutine note_file_time(full)
            character(len=*), intent(in) :: full
            type(file_time_type), allocatable :: kept(:)
            integer :: i, m
            logical :: newer

            call file_attributes(full, stat, found)
            if (.not. allocated(file_times)) allocate (file_times(0))
            newer = .false.

            do i = 1, size(file_times)
                if (file_times(i)%name == full .and. found) then
                    newer = file_times(i)%sec < stat%mtime_sec .or. &
                            (file_times(i)%sec == stat%mtime_sec .and. file_times(i)%nsec < stat%mtime_nsec)
                end if
            end do

            if (newer) then
                if (has_current_file()) then
                    call forget_file_caches(info_current_file())
                else
                    call forget_file_caches(filename)
                end if
            end if

            ! Add new modtime to `Info-file-attributes'.
            allocate (kept(size(file_times) + 1))
            kept(1)%name = full
            kept(1)%sec = stat%mtime_sec
            kept(1)%nsec = stat%mtime_nsec
            m = 1

            do i = 1, size(file_times)
                if (file_times(i)%name == full) cycle
                m = m + 1
                kept(m) = file_times(i)
            end do

            file_times = kept(1:m)
        end subroutine note_file_time
    end subroutine info_insert_file_contents

    ! Remove FILE from the cache of index nodes (Info-index-nodes); the
    ! table of contents cache (Info-toc-nodes) comes with the table of
    ! contents.  Not a port.
    subroutine forget_file_caches(file)
        character(len=*), intent(in) :: file
        type(index_nodes_type), allocatable :: kept(:)
        integer :: i, m

        if (.not. allocated(index_nodes_cache)) return
        allocate (kept(size(index_nodes_cache)))
        m = 0

        do i = 1, size(index_nodes_cache)
            if (index_nodes_cache(i)%file == file) cycle
            m = m + 1
            kept(m) = index_nodes_cache(i)
        end do

        index_nodes_cache = kept(1:m)
    end subroutine forget_file_caches

    ! Whether S ends with SUFFIX ("" matches anything).  Not a port.
    logical function ends_with(s, suffix)
        character(len=*), intent(in) :: s, suffix

        ends_with = .false.
        if (len(suffix) > len(s)) return
        ends_with = s(len(s) - len(suffix) + 1:) == suffix
    end function ends_with

    ! Return true if FILE supports Info index cookies.  Info index
    ! cookies were first introduced in 4.7, and all later makeinfo
    ! versions output them in index nodes, so we can rely solely on the
    ! makeinfo version.  This function caches the information in
    ! `Info-file-supports-index-cookies-list'.  The current buffer must
    ! hold the beginning of FILE.  Port of
    ! info.el:Info-file-supports-index-cookies.
    logical function file_supports_index_cookies(file) result(found)
        character(len=*), intent(in) :: file
        type(cookies_type), allocatable :: grown(:)
        character(len=:), allocatable :: version
        integer(int64) :: opoint
        integer :: i, n

        if (.not. allocated(cookies_cache)) allocate (cookies_cache(0))

        do i = 1, size(cookies_cache)
            if (cookies_cache(i)%file == file) then
                found = cookies_cache(i)%supports
                return
            end if
        end do

        found = .false.

        ! Skip virtual Info files.
        if (.not. info_virtual_file_p(file)) then
            opoint = point()
            call goto_min()

            if (refwd("\(?:makeinfo\|texi2any\)[ "//LF//"]version[ "//LF//"]\([0-9]+.[0-9]+\)", &
                      line_beginning_position(4_int64))) then
                version = match_string_no_properties(1)
                found = .not. version_less(version, "4.7")
            end if

            call goto_char(opoint)
        end if

        n = size(cookies_cache)
        allocate (grown(n + 1))
        grown(1)%file = file
        grown(1)%supports = found

        do i = 1, n
            grown(i + 1) = cookies_cache(i)
        end do

        call move_alloc(grown, cookies_cache)
    end function file_supports_index_cookies

    ! (version< A B) for versions made of numbers and dots.  Not a port.
    logical function version_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: i, j, na, nb, va, vb

        i = 1
        j = 1
        version_less = .false.

        do
            call next_number(a, i, va, na)
            call next_number(b, j, vb, nb)
            if (na == 0 .and. nb == 0) return

            if (va /= vb) then
                version_less = va < vb
                return
            end if
        end do

    contains

        ! The number at POS of S (0 if none, NDIGITS 0), skipping the dot
        ! after it.
        subroutine next_number(s, pos, v, ndigits)
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos
            integer, intent(out) :: v, ndigits

            v = 0
            ndigits = 0

            do while (pos <= len(s))
                if (s(pos:pos) < "0" .or. s(pos:pos) > "9") exit
                v = v*10 + iachar(s(pos:pos)) - iachar("0")
                ndigits = ndigits + 1
                pos = pos + 1
            end do

            if (pos <= len(s)) pos = pos + 1
        end subroutine next_number
    end function version_less

    ! Compute a directory list suitable for Info.  Port of
    ! info.el:Info--default-directory-list.
    subroutine default_directory_list(dirs)
        type(string_type), allocatable, intent(out) :: dirs(:)
        type(string_type), allocatable :: candidates(:), pruned(:), standard(:)
        character(len=:), allocatable :: config_dir
        character(len=*), parameter :: prefixes(3) = [character(len=11) :: "/usr/local/", "/usr/", "/opt/"]
        character(len=*), parameter :: suffixes(2) = [character(len=6) :: "share/", ""]
        type(string_type), allocatable :: pfx(:)
        integer :: i, j

        if (allocated(configure_info_directory)) then
            config_dir = file_name_as_directory(configure_info_directory)
        else
            config_dir = "/usr/local/share/info/"
        end if

        ! Directory trees in which to look for info subdirectories.
        allocate (candidates(0))

        do i = 1, size(prefixes)
            call add_string(candidates, trim(prefixes(i)))
        end do

        call prune_directory_list(candidates, pfx)
        allocate (standard(0))

        do i = 1, size(pfx)
            ! Subdirectories in each directory tree that may contain info
            ! directories.
            deallocate (candidates)
            allocate (candidates(0))

            do j = 1, size(suffixes)
                call add_string(candidates, pfx(i)%s//trim(suffixes(j))//"info/")
            end do

            call prune_directory_list(candidates, pruned)

            do j = 1, size(pruned)
                call add_string(standard, pruned(j)%s)
            end do
        end do

        ! If $(prefix)/share/info is not one of the standard info
        ! directories, they are probably installing an experimental
        ! version of Fortmacs, so make sure that experimental version's Info
        ! files override the ones in standard directories.
        allocate (dirs(0))

        if (has_string(standard, config_dir)) then
            do i = 1, size(standard)
                call add_string(dirs, standard(i)%s)
            end do

            call add_string(dirs, config_dir)
        else
            call add_string(dirs, config_dir)

            do i = 1, size(standard)
                call add_string(dirs, standard(i)%s)
            end do
        end if
    end subroutine default_directory_list

    ! `info_default_directory_list' followed by
    ! `default_directory_list'.  Port of info.el:Info-default-dirs (the
    ! Info files of the source tree, `source_directory', are Deferred).
    subroutine default_dirs(dirs)
        type(string_type), allocatable, intent(out) :: dirs(:)
        type(string_type), allocatable :: more(:)
        integer :: i

        call setting_dirs(info_default_directory_list, dirs)
        call default_directory_list(more)

        do i = 1, size(more)
            call add_string(dirs, more(i)%s)
        end do
    end subroutine default_dirs

    !> Initialize `info_directory_list', if that hasn't been done yet.
    !> Port of info.el:info-initialize.
    subroutine info_initialize()
        type(string_type), allocatable :: dirs(:), defaults(:), pruned(:)
        character(len=:), allocatable :: path
        logical :: found
        integer :: i, start

        if (allocated(info_directory_list)) then
            if (size(info_directory_list) > 0) return
        end if

        call getenv("INFOPATH", path, found)
        allocate (dirs(0))

        if (found) then
            if (ends_with(path, ":") .and. len(path) > 0) then
                call split_path(path(1:len(path) - 1))
                call default_dirs(defaults)

                do i = 1, size(defaults)
                    call add_string(dirs, defaults(i)%s)
                end do
            else
                call split_path(path)
            end if
        else
            call default_dirs(dirs)
        end if

        call prune_directory_list(dirs, pruned)
        call list_to_setting(pruned, info_directory_list)

    contains

        ! Add the parts of P between colons to DIRS ((split-string P ":")).
        subroutine split_path(p)
            character(len=*), intent(in) :: p

            start = 1

            do i = 1, len(p)
                if (p(i:i) == ":") then
                    call add_string(dirs, p(start:i - 1))
                    start = i + 1
                end if
            end do

            call add_string(dirs, p(start:))
        end subroutine split_path
    end subroutine info_initialize

    ! Whether Info file FILENAME is virtual.  Port of
    ! info.el:Info-virtual-file-p.
    logical function info_virtual_file_p(filename)
        character(len=*), intent(in) :: filename

        info_virtual_file_p = virtual_file_index(filename) > 0
    end function info_virtual_file_p

    ! The index in `info_virtual_files' of the virtual file FILENAME
    ! belongs to (0 if none).  Port of info.el:Info-virtual-fun, in part.
    integer function virtual_file_index(filename) result(k)
        character(len=*), intent(in) :: filename

        if (.not. allocated(info_virtual_files)) call init_virtual_files()

        do k = 1, size(info_virtual_files)
            if (smatch(trim(info_virtual_files(k)%name), filename)) return
        end do

        k = 0
    end function virtual_file_index

    ! The procedure of the find-node operation of the virtual file
    ! FILENAME or else of the virtual node NODENAME (null if neither is
    ! virtual).  Port of info.el:Info-virtual-fun, in part.
    function virtual_find_node(filename, nodename) result(fn)
        character(len=*), intent(in) :: filename, nodename
        procedure(info_virtual_find_node_proc), pointer :: fn
        integer :: k

        fn => null()
        k = virtual_file_index(filename)
        if (k > 0) fn => info_virtual_files(k)%find_node
        if (associated(fn)) return

        do k = 1, size(info_virtual_nodes)
            if (smatch(trim(info_virtual_nodes(k)%name), nodename)) then
                fn => info_virtual_nodes(k)%find_node
                if (associated(fn)) return
            end if
        end do
    end function virtual_find_node

    !> Return expanded FILENAME, or FILENAME itself if it is a virtual
    !> file such as "dir".  If NOERROR is true and the file is not found,
    !> return unallocated (no error).
    !>
    !> If NO_POP_TO_DIR, don't try to pop to the info buffer if we can't
    !> find a node.  Port of info.el:Info-find-file.
    recursive subroutine info_find_file(filename, result, status, noerror, no_pop_to_dir)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror, no_pop_to_dir
        type(string_type), allocatable :: dirs(:)
        character(len=:), allocatable :: name, temp, temp_downcase, found
        type(status_type) :: st
        integer :: d, s, k

        status = status_type()
        call info_initialize()
        ! Convert filename to lower case if not found as specified.
        ! Expand it.
        k = virtual_file_index(filename)

        if (k > 0) then
            if (associated(info_virtual_files(k)%find_file)) then
                call info_virtual_files(k)%find_file(filename, result, status)
                return
            end if
        end if

        name = substitute_in_file_name(filename)

        if (smatch("^\./", name)) then
            ! If specified name starts with `./' then just try current
            ! directory.
            allocate (dirs(1))
            dirs(1)%s = "./"
        else if (file_name_absolute_p(name)) then
            ! No point in searching for an absolute file name.
            allocate (dirs(1))
            dirs(1)%s = ""
        else
            call search_dirs(dirs)
        end if

        ! (installation-directory is nil for Fortmacs.)
        ! Search the directory list for file FILENAME.
        do d = 1, size(dirs)
            if (len(dirs(d)%s) > 0) then
                temp = expand(name, dirs(d)%s)
                temp_downcase = expand(lower(name), dirs(d)%s)
            else
                temp = expand(name)
                temp_downcase = expand(lower(name))
            end if

            ! Try several variants of specified name.
            do s = 1, size(info_suffix_list)
                if (info_file_exists_p(temp//trim(info_suffix_list(s)%suffix))) then
                    found = temp
                    exit
                else if (info_file_exists_p(temp_downcase//trim(info_suffix_list(s)%suffix))) then
                    found = temp_downcase
                    exit
                end if
            end do

            if (allocated(found)) exit
        end do

        if (allocated(found)) then
            result = found
            return
        end if

        if (present(noerror)) then
            if (noerror) return
        end if

        ! If there is no previous Info file, go to the directory.
        if (.not. has_current_file()) then
            k = 0

            if (present(no_pop_to_dir)) then
                if (no_pop_to_dir) k = 1
            end if

            if (k == 0) call info_directory(st)
        end if

        call user_error(status, "Info file `"//name//"' does not exist; consider installing it")
    end subroutine info_find_file

    ! The buffer position of the 0-based file position BYTE of the
    ! (widened) buffer.  Port of mule-util.el:filepos-to-bufferpos for
    ! UTF-8 and single-byte files with Unix line ends, which is what Info
    ! files are.
    integer(int64) function filepos_to_bufferpos(byte) result(pos)
        integer(int64), intent(in) :: byte
        integer(int64) :: b

        b = min(max(1_int64, 1 + byte), buffers(cur)%text%z_byte)
        pos = byte_to_char(b)
    end function filepos_to_bufferpos

    ! The 0-based file position of buffer position POS.  Port of
    ! mule-util.el:bufferpos-to-filepos (as `filepos_to_bufferpos').
    integer(int64) function bufferpos_to_filepos(pos) result(byte)
        integer(int64), intent(in) :: pos

        byte = char_to_byte(pos) - 1
    end function bufferpos_to_filepos

    !> Like `info' but show the Info buffer in another window.  Port of
    !> info.el:info-other-window.
    recursive subroutine info_other_window(status, file_or_node, buffer)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_or_node, buffer

        call info_pop_to_buffer(status, file_or_node, buffer, other_window=.true.)
    end subroutine info_other_window

    !> Put Info node FILE_OR_NODE in specified buffer and display it.
    !> Optional argument FILE_OR_NODE is as for `info'.
    !>
    !> If the optional argument BUFFER_NAME is present, use that string as
    !> the name of the buffer, creating it if it does not exist.
    !> Otherwise, use a buffer with the name `*info*', creating it if it
    !> does not exist.  (BUFFER_HANDLE gives the buffer itself instead.)
    !>
    !> Optional argument OTHER_WINDOW false means to prefer the selected
    !> window.  OTHER_WINDOW true means to prefer another window.  Select
    !> the window used, if it has been made.  Port of
    !> info.el:info-pop-to-buffer.
    recursive subroutine info_pop_to_buffer(status, file_or_node, buffer_name, other_window, buffer_handle)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_or_node, buffer_name
        logical, intent(in), optional :: other_window
        type(handle_type), intent(in), optional :: buffer_handle
        type(handle_type) :: buffer, old, window
        type(display_alist_type) :: alist
        type(info_history_entry_type), allocatable :: history(:)
        type(status_type) :: st
        logical :: other

        status = status_type()
        other = .false.
        if (present(other_window)) other = other_window

        if (present(buffer_handle)) then
            buffer = buffer_handle
        else if (present(buffer_name)) then
            call get_buffer_create(buffer_name, buffer, status)
        else
            call get_buffer_create("*info*", buffer, status)
        end if

        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        if (.not. derived_mode_p(major_modes%info_mode)) call info_mode(status)

        if (status%ok()) then
            if (present(file_or_node)) then
                ! If argument already contains parentheses, don't add
                ! another set since the argument will then be parsed
                ! improperly.  This also has the added benefit of allowing
                ! node names to be included following the parenthesized
                ! filename.
                if (smatch("(.*)", file_or_node)) then
                    call info_goto_node(file_or_node, status)
                else
                    call info_goto_node("("//file_or_node//")", status)
                end if
            else
                call info_history_variable(history)
                ! If we just created the Info buffer, go to the directory.
                if (buffer_size() == 0 .and. size(history) == 0) call info_directory(status)
            end if
        end if

        if (buffer_live_p(old)) call set_buffer(old, st)
        if (.not. status%ok()) return

        if (other) then
            alist%inhibit_same_window = .true.
            call display_buffer(buffer, window, status, alist=alist)
        else
            call display_buffer(buffer, window, status, actions=[DISPLAY_ACTION_SAME_WINDOW])
        end if

        if (.not. status%ok()) return
        if (window%index /= 0) call select_window(window, status)
    end subroutine info_pop_to_buffer

    !> Enter Info, the documentation browser.
    !> Optional argument FILE_OR_NODE specifies the file to examine;
    !> the default is the top-level directory of Info.
    !> Called from a program, FILE_OR_NODE may specify an Info node of the
    !> form "(FILENAME)NODENAME".
    !> Optional argument BUFFER specifies the Info buffer name;
    !> the default buffer name is *info*.  If BUFFER exists,
    !> just switch to BUFFER.  Otherwise, create a new buffer
    !> with the top-level Info directory.
    !>
    !> In interactive use, a non-numeric prefix argument directs
    !> this command to read a file name from the minibuffer.
    !>
    !> A numeric prefix argument of N selects an Info buffer named
    !> "*info*<N>".
    !>
    !> The search path for Info files is in the variable
    !> `info_directory_list'.  The top-level Info directory is made by
    !> combining all the files named `dir' in all the directories in that
    !> path.
    !>
    !> See a list of available Info commands in `info_mode'.  Port of
    !> info.el:info.
    recursive subroutine info(status, file_or_node, buffer)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_or_node, buffer

        if (present(file_or_node)) then
            ! Info node names don't contain newlines, so allow for easier
            ! use of names that might have been wrapped (in emails, etc.).
            call info_pop_to_buffer(status, replace_all(LF, " ", file_or_node), buffer)
        else
            call info_pop_to_buffer(status, buffer_name=buffer)
        end if
    end subroutine info

    ! The interactive arguments of `info' and `info_other_window': with a
    ! non-numeric prefix argument a file name read in the minibuffer, with
    ! a numeric one the buffer *info*<N>.  HAS_FILE and HAS_BUFFER say
    ! which are given.
    ! Port of info.el:info, in part (its interactive spec).
    recursive subroutine info_interactive(file, has_file, buffer, has_buffer, status)
        character(len=:), allocatable, intent(out) :: file, buffer
        logical, intent(out) :: has_file, has_buffer
        type(status_type), intent(out) :: status
        character(len=24) :: buf

        status = status_type()
        has_file = .false.
        has_buffer = .false.
        if (nilp(current_prefix_arg)) return

        if (fixnump(current_prefix_arg)) then
            write (buf, "(i0)") xfixnum(current_prefix_arg)
            buffer = "*info*<"//trim(buf)//">"
            has_buffer = .true.
        else
            call read_file_name("Info file name: ", file, status, mustmatch=REQUIRE_MATCH_T)
            if (.not. status%ok()) return
            has_file = .true.
        end if
    end subroutine info_interactive

    recursive subroutine cmd_info(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, buffer
        logical :: has_file, has_buffer

        call info_interactive(file, has_file, buffer, has_buffer, status)
        if (.not. status%ok()) return

        if (has_file) then
            call info(status, file)
        else if (has_buffer) then
            call info(status, buffer=buffer)
        else
            call info(status)
        end if
    end subroutine cmd_info

    recursive subroutine cmd_info_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, buffer
        logical :: has_file, has_buffer

        call info_interactive(file, has_file, buffer, has_buffer, status)
        if (.not. status%ok()) return

        if (has_file) then
            call info_other_window(status, file)
        else if (has_buffer) then
            call info_other_window(status, buffer=buffer)
        else
            call info_other_window(status)
        end if
    end subroutine cmd_info_other_window

    !> Display the Emacs manual in Info mode.  Port of
    !> info.el:info-emacs-manual.
    recursive subroutine info_emacs_manual(status)
        type(status_type), intent(out) :: status

        call info(status, "emacs")
    end subroutine info_emacs_manual

    !> Display the "Reporting Bugs" section of the Emacs manual in Info
    !> mode.  Port of info.el:info-emacs-bug.
    recursive subroutine info_emacs_bug(status)
        type(status_type), intent(out) :: status

        call info(status, "(emacs)Bugs")
    end subroutine info_emacs_bug

    ! See if the accessible portion of the buffer begins with a node
    ! delimiter, and the node header line which follows matches REGEXP.
    ! Typically, this test will be followed by a loop that examines the
    ! rest of the buffer with (search-forward "\n\^_"), and it's a pity
    ! to have the overhead of this special test inside the loop.
    !
    ! This function changes match-data, but supposedly the caller might
    ! want to use the results of re-search-backward.
    !
    ! The return value is the value of point at the beginning of matching
    ! REGEXP, if the function succeeds, 0 otherwise.  Port of
    ! info.el:Info-node-at-bob-matching.
    integer(int64) function node_at_bob_matching(regexp) result(pos)
        character(len=*), intent(in) :: regexp
        integer(int64) :: beg

        pos = 0
        if (.not. bobp()) return
        if (.not. looking(US)) return
        call fwd_line(1_int64)
        beg = point()
        call fwd_line(1_int64)
        if (rebwd(regexp, beg)) pos = point()
    end function node_at_bob_matching

    !> Go to an Info node specified as separate FILENAME and NODENAME.
    !> NO_GOING_BACK is true if recovering from an error in this
    !> function; it says do not attempt further (recursive) error
    !> recovery.
    !>
    !> This function first looks for a case-sensitive match for NODENAME;
    !> if none is found it then tries a case-insensitive match (unless
    !> STRICT_CASE is true).
    !>
    !> If NOERROR, inhibit error messages when we can't find the node.
    !> FILENAME absent stands for nil, the current Info file.  Port of
    !> info.el:Info-find-node.
    recursive subroutine info_find_node(filename, nodename, status, no_going_back, strict_case, noerror)
        character(len=*), intent(in), optional :: filename
        character(len=*), intent(in) :: nodename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_going_back, strict_case, noerror
        character(len=:), allocatable :: node, file
        logical :: back, has_file
        integer :: k

        status = status_type()
        call info_initialize()
        node = canonicalize_whitespace(nodename)
        has_file = present(filename)

        if (has_file) then
            call info_find_file(filename, file, status, noerror)
            if (.not. status%ok()) return
            has_file = allocated(file)
        end if

        ! Go into Info buffer.
        if (.not. derived_mode_p(major_modes%info_mode)) then
            if (has_file) then
                call info_pop_to_buffer(status, file)
            else
                call info_pop_to_buffer(status)
            end if

            if (.not. status%ok()) return
        end if

        back = .false.
        if (present(no_going_back)) back = no_going_back

        ! Record the node we are leaving, if we were in one.
        if (.not. back .and. has_current_file()) then
            k = here()
            call push_history(locals(k)%history, info_current_file(), current_node_or_empty(), point())
        end if

        if (has_file) then
            call info_find_node_2(file, node, status, no_going_back, strict_case)
        else
            call info_find_node_2(nodename=node, status=status, no_going_back=no_going_back, strict_case=strict_case)
        end if
    end subroutine info_find_node

    ! If the current Info file has a tag table, record its location for
    ! NODENAME.
    !
    ! This creates a tag-table buffer, sets `Info-tag-table-buffer' to
    ! name that buffer, and records the buffer and the tag table in the
    ! marker `Info-tag-table-buffer'.  If the Info file has no tag table,
    ! or if NODENAME is "*", the function sets the marker to nil to
    ! indicate the tag table is not available/relevant.
    !
    ! The function assumes that the Info buffer is widened, and does not
    ! preserve point.  Port of info.el:Info--record-tag-table.
    recursive subroutine record_tag_table(nodename, status)
        character(len=*), intent(in) :: nodename
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, tagbuf
        integer(int64) :: pos, mend0
        integer :: k

        status = status_type()
        k = here()
        if (locals(k)%tag_table_marker%index == 0) call make_marker(locals(k)%tag_table_marker)
        call goto_char(point_max())
        call fwd_line(-8_int64)

        if (nodename /= "*" .and. sfwd(US//LF//"End tag table"//LF)) then
            ! We have a tag table.  Find its beginning.
            ! Is this an indirect file?
            if (.not. sbwd(LF//"Tag table:"//LF)) then
                call set_error(status, FM_SEARCH_FAILED, LF//"Tag table:"//LF)
                return
            end if

            pos = point()
            call fwd_line(2_int64)

            if (looking("(Indirect)"//LF)) then
                mend0 = mend(0)
                ! It is indirect.  Copy it to another buffer and record
                ! that the tag table is in that buffer.
                buf = current_buffer()
                tagbuf = locals(k)%tag_table_buffer

                if (.not. buffer_live_p(tagbuf)) then
                    call get_buffer_create(generate_new_buffer_name(" *info tag table*"), tagbuf, status)
                    if (.not. status%ok()) return
                end if

                locals(k)%tag_table_buffer = tagbuf
                call set_buffer(tagbuf, status)
                if (.not. status%ok()) return
                buffers(cur)%undo_enabled = .false.
                call case_fold_search%set_local(.true.)
                call erase_buffer(status)
                if (status%ok()) call insert_buffer_substring(buf, status)
                call set_buffer(buf, status)
                if (.not. status%ok()) return
                call set_marker(locals(k)%tag_table_marker, mend0, status, tagbuf)
            else
                call set_marker(locals(k)%tag_table_marker, pos, status, current_buffer())
            end if
        else
            call set_marker_nowhere(locals(k)%tag_table_marker)
        end if
    end subroutine record_tag_table

    ! Make marker M point nowhere ((set-marker M nil)).  Not a port.
    subroutine set_marker_nowhere(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call detach_marker(m, st)
    end subroutine set_marker_nowhere

    !> Use Info mode to browse the current Info buffer.
    !> With a prefix arg, this queries for the node name to visit first;
    !> otherwise, that defaults to `Top'.  Port of
    !> info.el:Info-on-current-buffer.
    recursive subroutine info_on_current_buffer(status, nodename)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: nodename
        character(len=:), allocatable :: node

        node = "Top"
        if (present(nodename)) node = nodename
        call info_initialize()
        call info_mode(status)
        if (.not. status%ok()) return

        if (allocated(buffers(cur)%filename)) then
            call set_current_file(buffers(cur)%filename)
        else
            ! If called on a non-file buffer, make a fake file name.
            call set_current_file(buffers(cur)%directory//buffers(cur)%name)
        end if

        call record_tag_table(node, status)
        if (.not. status%ok()) return
        call info_find_node_2(nodename=node, status=status)
    end subroutine info_on_current_buffer

    recursive subroutine cmd_info_on_current_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node
        type(string_type), allocatable :: completions(:)

        if (.not. nilp(current_prefix_arg)) then
            call build_node_completions(completions, status)
            if (.not. status%ok()) return
            call completing_read("Node name: ", completion_list_table(strings_value(completions)), node, status, &
                                 require_match=REQUIRE_MATCH_T, initial="Top")
            if (.not. status%ok()) return
            call info_on_current_buffer(status, node)
        else
            call info_on_current_buffer(status)
        end if
    end subroutine cmd_info_on_current_buffer

    ! LIST as a Lisp list of strings (a completion table).  Not a port.
    function strings_value(list) result(v)
        type(string_type), intent(in) :: list(:)
        type(value_type) :: v
        type(value_type), allocatable :: elems(:)
        integer :: i

        allocate (elems(size(list)))

        do i = 1, size(list)
            elems(i) = make_string(list(i)%s)
        end do

        v = make_list(elems)
    end function strings_value

    ! Go to an Info node FILENAME and NODENAME, re-reading disk contents.
    ! When *info* is already displaying FILENAME and NODENAME, the window
    ! position is preserved, if possible.  Port of
    ! info.el:Info-revert-find-node.
    recursive subroutine revert_find_node(filename, nodename, status)
        character(len=*), intent(in) :: filename, nodename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old_filename, old_nodename, new_file, new_node
        integer(int64) :: pline, wline, new_pos, reached, pcolumn
        integer :: k
        logical :: window_selected, has_new

        status = status_type()

        if (.not. derived_mode_p(major_modes%info_mode)) then
            call info_pop_to_buffer(status, filename)
            if (.not. status%ok()) return
        end if

        old_filename = current_file_or_empty()
        old_nodename = current_node_or_empty()
        window_selected = same_buffer(selected_window(), get_buffer_window(current_buffer()))
        pcolumn = current_column()
        pline = count_lines(point_min(), line_beginning_position(1_int64))
        wline = count_lines(point_min(), sel_window_start())
        has_new = has_current_file()

        if (has_new) then
            new_file = info_current_file()
            new_node = current_node_or_empty()
            new_pos = point()
        end if

        ! When `Info-current-file' is nil, `Info-find-node-2' rereads the
        ! file.
        call set_current_file("", clear=.true.)
        call info_find_node(filename, nodename, status)
        if (.not. status%ok()) return

        if (old_filename == current_file_or_empty() .and. old_nodename == current_node_or_empty()) then
            ! note goto-line is no good, we want to measure from point-min
            if (window_selected) then
                call goto_min()
                call fwd_line(wline)
                call set_sel_window_start(point(), status)
            end if

            call goto_min()
            call fwd_line(pline)
            call move_to_column(pcolumn, reached, .false., status)
        else if (has_new) then
            ! only add to the history when coming from a different
            ! file+node
            k = here()
            call push_history(locals(k)%history, new_file, new_node, new_pos)
        end if
    end subroutine revert_find_node

    ! Whether handles A and B are the same object.  Not a port.
    logical function same_buffer(a, b)
        type(handle_type), intent(in) :: a, b

        same_buffer = a%index == b%index .and. a%generation == b%generation
    end function same_buffer

    ! The `revert_buffer_function' of Info buffers.  Port of
    ! info.el:Info-revert-buffer-function.
    recursive subroutine info_revert_buffer_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        logical :: yes

        status = status_type()
        if (ignore_auto) continue
        yes = noconfirm

        if (.not. yes) then
            call y_or_n_p("Revert info buffer? ", yes, status)
            if (.not. status%ok()) return
        end if

        if (yes) then
            call revert_find_node(current_file_or_empty(), current_node_or_empty(), status)
            if (.not. status%ok()) return
            call message("Reverted "//current_file_or_empty())
        end if
    end subroutine info_revert_buffer_function

    ! Find a node in a tag table.
    ! MARKER specifies the buffer and position to start searching at.
    ! REGEXP is a regular expression matching nodes or references.  Its
    ! first group should match `Node:' or `Ref:'.
    ! CASE_FOLD true means search for a case-insensitive match.
    ! If a match was found, FOUND is true, ANCHOR true if a `Ref:' was
    ! matched, POS the file position where the match was found, and
    ! IN_INFO_MODE whether the buffer of the match is in Info mode.
    ! Port of info.el:Info-find-in-tag-table-1.
    recursive subroutine find_in_tag_table_1(marker, regexp, case_fold, found, anchor, pos, in_info_mode)
        type(handle_type), intent(in) :: marker
        character(len=*), intent(in) :: regexp
        logical, intent(in) :: case_fold
        logical, intent(out) :: found, anchor, in_info_mode
        integer(int64), intent(out) :: pos
        type(handle_type) :: old, buf
        type(status_type) :: st
        integer(int64) :: mpos
        logical :: fold

        found = .false.
        anchor = .false.
        in_info_mode = .false.
        pos = 0
        old = current_buffer()
        call marker_buffer(marker, buf, st)
        call marker_position(marker, mpos, st)
        if (.not. st%ok()) return
        call set_buffer(buf, st)
        fold = bind_case_fold(case_fold)
        call goto_char(mpos)
        ! Search tag table
        call beginning_of_line(1_int64)

        if (refwd(regexp)) then
            found = .true.
            anchor = match_string_no_properties(1) == "Ref:"
            pos = read_number()
            in_info_mode = .false.
            if (allocated(buffers(cur)%major_mode)) in_info_mode = buffers(cur)%major_mode == "info_mode"
        end if

        fold = bind_case_fold(fold)
        call set_buffer(old, st)
    end subroutine find_in_tag_table_1

    ! (read (current-buffer)) for a number: skip white space and read the
    ! integer after point, leaving point after it.  Not a port.
    integer(int64) function read_number() result(n)
        type(status_type) :: st
        integer(int64) :: moved, start
        character(len=:), allocatable :: digits
        integer :: ios

        moved = skip_chars_forward(" "//TAB//LF, st)
        start = point()
        moved = skip_chars_forward("0-9", st)
        digits = text(start, point())
        n = 0

        if (len(digits) > 0) then
            read (digits, *, iostat=ios) n
            if (ios /= 0) n = 0
        end if
    end function read_number

    ! Find a node in a tag table.  Tries a case-sensitive match first,
    ! then a case-insensitive match (unless STRICT_CASE is true).
    ! Port of info.el:Info-find-in-tag-table.
    recursive subroutine find_in_tag_table(marker, regexp, strict_case, found, anchor, pos, in_info_mode)
        type(handle_type), intent(in) :: marker
        character(len=*), intent(in) :: regexp
        logical, intent(in) :: strict_case
        logical, intent(out) :: found, anchor, in_info_mode
        integer(int64), intent(out) :: pos

        call find_in_tag_table_1(marker, regexp, .false., found, anchor, pos, in_info_mode)

        if (.not. strict_case .and. .not. found) then
            call find_in_tag_table_1(marker, regexp, .true., found, anchor, pos, in_info_mode)
        end if
    end subroutine find_in_tag_table

    ! Find a node or anchor in the current buffer.
    ! REGEXP is a regular expression matching nodes or references.  Its
    ! first group should match `Node:' or `Ref:'.
    ! CASE_FOLD true means search for a case-insensitive match.
    ! Value is the position at which a match was found, or 0 if not
    ! found.  Port of info.el:Info-find-node-in-buffer-1.
    integer(int64) function find_node_in_buffer_1(regexp, case_fold) result(found)
        character(len=*), intent(in) :: regexp
        logical, intent(in) :: case_fold
        integer(int64) :: opoint, beg
        logical :: fold

        fold = bind_case_fold(case_fold)
        found = 0
        opoint = point()

        if (node_at_bob_matching(regexp) > 0) then
            found = point()
        else
            do while (found == 0)
                if (.not. sfwd(LF//US)) exit
                call fwd_line(1_int64)
                beg = point()
                call fwd_line(1_int64)
                if (rebwd(regexp, beg)) found = line_beginning_position(1_int64)
            end do
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)
    end function find_node_in_buffer_1

    ! Find a node or anchor in the current buffer.  Looks for a
    ! case-sensitive match first.  If none is found, a case-insensitive
    ! match is tried (unless STRICT_CASE is true).  Port of
    ! info.el:Info-find-node-in-buffer.
    integer(int64) function find_node_in_buffer(regexp, strict_case) result(found)
        character(len=*), intent(in) :: regexp
        logical, intent(in) :: strict_case

        found = find_node_in_buffer_1(regexp, .false.)
        if (found == 0 .and. .not. strict_case) found = find_node_in_buffer_1(regexp, .true.)
    end function find_node_in_buffer

    ! (let ((inhibit-read-only t)) ...): bind it, returning the old value.
    ! Not a port.
    function bind_inhibit_read_only() result(saved)
        type(value_type) :: saved
        type(status_type) :: st

        saved = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
    end function bind_inhibit_read_only

    ! The end of (let ((inhibit-read-only t)) ...).  Not a port.
    subroutine unbind_inhibit_read_only(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(sym("inhibit_read_only"), saved, st)
    end subroutine unbind_inhibit_read_only

    ! Go to node NODENAME of FILENAME (absent for the current Info file)
    ! in the current Info buffer, reading the file if necessary.  Port of
    ! info.el:Info-find-node-2.
    recursive subroutine info_find_node_2(filename, nodename, status, no_going_back, strict_case)
        character(len=*), intent(in), optional :: filename
        character(len=*), intent(in) :: nodename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_going_back, strict_case
        type(info_history_entry_type) :: hist
        type(status_type) :: st
        logical :: back, strict, fold
        integer :: k

        status = status_type()
        back = .false.
        if (present(no_going_back)) back = no_going_back
        strict = .false.
        if (present(strict_case)) strict = strict_case
        buffers(cur)%undo_enabled = .false.

        if (.not. derived_mode_p(major_modes%info_mode)) then
            call info_mode(status)
            if (.not. status%ok()) return
        end if

        call widen()
        call set_current_node()
        fold = bind_case_fold(.true.)
        call find_node_body(status)
        fold = bind_case_fold(fold)

        ! If we did not finish finding the specified node,
        ! go back to the previous one or to the Top node.
        if (.not. (has_current_node() .or. back)) then
            k = here()

            if (size(locals(k)%history) > 0) then
                hist = locals(k)%history(1)
                call pop_history(locals(k)%history)
                call info_find_node(hist%file, hist%node, st, .true., .true.)
                call goto_char(hist%pos)
            else
                call info_find_node(current_file_or_empty(), "Top", st, .true., .true.)
            end if
        end if

    contains

        ! The body of the unwind-protect.
        recursive subroutine find_node_body(status)
            type(status_type), intent(out) :: status
            character(len=:), allocatable :: file, regexp
            type(value_type) :: saved
            integer(int64) :: guesspos, anchorpos, filepos, pos, line
            logical :: found, anchor, in_info_mode, has_file, done
            procedure(info_virtual_find_node_proc), pointer :: vfn

            status = status_type()
            has_file = present(filename)

            if (has_file) then
                file = filename
            else
                file = current_file_or_empty()
            end if

            anchorpos = 0
            k = here()
            vfn => virtual_find_node(file, nodename)

            if (associated(vfn)) then
                buffers(cur)%read_only = .false.
                call set_current_file(file)
                call set_current_subfile()
                locals(k)%has_completions = .false.
                if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
                call erase_buffer(status)
                if (.not. status%ok()) return
                call vfn(file, nodename, back, status)
                if (.not. status%ok()) return
                k = here()
                call set_marker_nowhere(locals(k)%tag_table_marker)
                buffers(cur)%read_only = .true.
                call restore_buffer_modified_p(.false.)
                locals(k)%current_node_virtual = .true.
            else if (locals(k)%current_node_virtual .or. &
                     (has_file .and. .not. (current_file_or_empty() == file .and. has_current_file()))) then
                ! Switch files if necessary
                saved = bind_inhibit_read_only()

                if (locals(k)%current_node_virtual) then
                    ! When moving from a virtual node.
                    locals(k)%current_node_virtual = .false.
                end if

                call set_current_file("", clear=.true.)
                call set_current_subfile()
                locals(k)%has_completions = .false.
                if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
                call erase_buffer(status)

                if (status%ok()) then
                    ! Erase any memory of the previous coding-system, so that
                    ! info-insert-file-contents sets the buffer's encoding to
                    ! what the Info file specifies.
                    call buffer_file_coding_system%set_local("undecided")
                    call info_insert_file_contents(file, status)
                end if

                call unbind_inhibit_read_only(saved)
                if (.not. status%ok()) return
                buffers(cur)%directory = file_name_directory(file)
                call restore_buffer_modified_p(.false.)
                locals(k)%supports_index_cookies = file_supports_index_cookies(file)
                ! See whether file has a tag table.  Record the location if yes.
                call record_tag_table(nodename, status)
                if (.not. status%ok()) return
                call set_current_file(file)
            end if

            if (nodename == "*") then
                call set_current_node(nodename)
                call info_set_mode_line()
                return
            end if

            ! Possibilities:
            !
            ! 1. Anchor found in tag table
            ! 2. Anchor *not* in tag table
            !
            ! 3. Node found in tag table
            ! 4. Node *not* found in tag table, but found in file
            ! 5. Node *not* in tag table, and *not* in file
            !
            ! *Or* the same, but in an indirect subfile.
            ! Search file for a suitable node.
            guesspos = point_min()
            regexp = "\(Node:\|Ref:\) *\("//regexp_quote(nodename)//"\) *[,"//TAB//LF//DEL//"]"
            done = .false.
            ! First, search a tag table, if any
            k = here()

            if (marker_points(locals(k)%tag_table_marker)) then
                call find_in_tag_table(locals(k)%tag_table_marker, regexp, strict, found, anchor, filepos, &
                                       in_info_mode)

                if (found) then
                    ! If this is an indirect file, determine which file
                    ! really holds this node and read it in.
                    if (.not. in_info_mode) then
                        ! Note that the current buffer must be the *info*
                        ! buffer on entry to Info-read-subfile.
                        pos = filepos
                        call read_subfile_at(pos, filepos, status)
                        if (.not. status%ok()) return
                    end if

                    guesspos = filepos_to_bufferpos(filepos)

                    ! Handle anchor
                    if (anchor) then
                        anchorpos = guesspos
                        call goto_char(anchorpos)
                        done = .true.
                    end if
                end if
            end if

            if (.not. done) then
                ! Else we may have a node, which we search for:
                call goto_char(max(point_min(), guesspos - 1000))
                ! Now search from our advised position (or from beg of
                ! buffer) to find the actual node.  First, check whether
                ! the node is right where we are, in case the buffer begins
                ! with a node.
                pos = find_node_in_buffer(regexp, strict)

                if (pos > 0) then
                    call goto_char(pos)
                    done = .true.
                end if
            end if

            if (.not. done) then
                ! If the Texinfo source had an @ifnottex block of text
                ! before the Top node, makeinfo 5.0 and 5.1 mistakenly
                ! omitted that block's size from the starting position of
                ! the 1st subfile, which makes GUESSPOS overshoot the
                ! correct position by the length of that text.  So we try
                ! again with a larger slop.
                call goto_char(max(point_min(), guesspos - 10000))
                pos = find_node_in_buffer(regexp, strict)

                if (pos > 0) then
                    call goto_char(pos)
                    done = .true.
                end if
            end if

            if (.not. done) then
                if (smatch("\([^.]+\)\.", nodename, .true.)) then
                    block
                        character(len=:), allocatable :: part
                        integer :: saved_kind
                        part = nodename(1:string_index_to_offset(nodename, mend(1)))
                        ! (let (Info-point-loc) ...)
                        saved_kind = point_loc_kind
                        point_loc_kind = LOC_NONE

                        if (has_file) then
                            call info_find_node_2(filename, part, status, no_going_back)
                        else
                            call info_find_node_2(nodename=part, status=status, no_going_back=no_going_back)
                        end if

                        point_loc_kind = saved_kind
                    end block

                    if (.not. status%ok()) return
                    call widen()
                    return
                end if

                ! No such anchor in tag table or node in tag table or file
                call user_error(status, "No such node or anchor: "//nodename)
                return
            end if

            call info_select_node(status)
            if (.not. status%ok()) return
            call goto_min()
            ! Skip the header line or breadcrumbs, unless
            ! 'scroll-conservatively' is set to a large enough value
            ! which could cause us scroll the display to leave that line
            ! outside of the window.  The shortest node is 5 lines, thus 6
            ! is the threshold value.  (scroll-conservatively is not
            ! ported: it is always 0.)
            call fwd_line(1_int64)

            if (anchorpos > 0) then
                ! Add anchors to the history too
                call note_history_list(current_file_or_empty(), nodename)
                call goto_char(anchorpos)
            else if (point_loc_kind == LOC_LINE) then
                line = point_loc_line
                call fwd_line(line - 2)
                point_loc_kind = LOC_NONE
            else if (point_loc_kind == LOC_NAME) then
                call find_index_name(point_loc_name)
                point_loc_kind = LOC_NONE
            end if
        end subroutine find_node_body
    end subroutine info_find_node_2

    ! Whether marker M points somewhere ((marker-position M) non-nil).
    ! Not a port.
    logical function marker_points(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        integer(int64) :: pos

        marker_points = .false.
        if (m%index == 0) return
        call marker_position(m, pos, st)
        marker_points = st%ok()
    end function marker_points

    ! Move point to the place within the current node where NAME is
    ! defined.  FOUND is true if found (else point goes to the beginning
    ! of the node).  Port of info.el:Info-find-index-name.
    recursive subroutine find_index_name(name, found)
        character(len=*), intent(in) :: name
        logical, intent(out), optional :: found
        logical :: fold, ok

        fold = bind_case_fold(.true.)
        ok = refwd("[a-zA-Z]+: "//regexp_quote(name)//"\( \|$\)")
        ! Find a function definition with a return type.
        if (.not. ok) ok = refwd("[a-zA-Z]+: [a-zA-Z0-9_ *&]+ "//regexp_quote(name)//"\( \|$\)")
        if (.not. ok) ok = sfwd("['`‘]"//name//"['’]")

        if (.not. ok) then
            if (smatch("\`.*\( (.*)\)\'", name, .true.)) then
                ok = sfwd("['`‘]"//name(1:string_index_to_offset(name, mbeg(1)))//"['’]")
            end if
        end if

        if (.not. ok) ok = sfwd(name)

        ! Try again without the " <1>" makeinfo can append
        if (.not. ok) then
            if (smatch("\`\(.*\) <[0-9]+>\'", name, .true.)) then
                call find_index_name(match_string_no_properties(1, name), ok)
            end if
        end if

        if (ok) then
            call beginning_of_line(1_int64)
        else
            call goto_min()
        end if

        if (present(found)) found = ok
        fold = bind_case_fold(fold)
    end subroutine find_index_name

    ! Construct the Info directory node by merging the files named `dir'
    ! from various directories.  Set the *info* buffer's
    ! default-directory to the first directory we actually get any text
    ! from.  Port of info.el:Info-insert-dir.
    recursive subroutine info_insert_dir(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: dirs(:), dirs_done(:)
        type(file_time_type), allocatable :: dir_file_attrs(:)
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: main, buffer, newbuf
        type(file_stat_type) :: stat
        type(string_type), allocatable :: menu_items(:)
        character(len=:), allocatable :: truename, file, nodename, contents_dir
        character(len=*), parameter :: variants(5) = [character(len=8) :: "dir", "DIR", "dir.info", "DIR.INFO", &
                                                                                                                        "dir.gz"]
        type(dir_node_type), allocatable :: nodes(:), this_nodes(:)
        type(status_type) :: st
        integer(int64) :: inserted, beg, end, menu_end
        integer :: i, j, k, v
        logical :: found, problems, fold, unchanged

        status = status_type()
        k = here()
        main = current_buffer()

        if (allocated(locals(k)%dir_contents) .and. allocated(locals(k)%dir_file_attributes)) then
            ! Verify that none of the files we used has changed since we
            ! used it.
            unchanged = .true.

            do i = 1, size(locals(k)%dir_file_attributes)
                call file_truename(locals(k)%dir_file_attributes(i)%name, truename, st)
                call file_attributes(truename, stat, found)

                if (.not. found) then
                    unchanged = .false.
                else if (stat%mtime_sec /= locals(k)%dir_file_attributes(i)%sec .or. &
                         stat%mtime_nsec /= locals(k)%dir_file_attributes(i)%nsec) then
                    unchanged = .false.
                end if
            end do

            if (unchanged) then
                call insert(locals(k)%dir_contents, status)
                call goto_min()
                buffers(cur)%directory = locals(k)%dir_contents_directory
                return
            end if
        end if

        call search_dirs(dirs)
        allocate (dir_file_attrs(0), bufs(0), dirs_done(0))
        ! Bind this in case the user sets it to nil.
        fold = bind_case_fold(.true.)
        ! This is set true if we find a problem in some input files.
        problems = .false.

        ! Search the directory list for the directory file.
        do i = 1, size(dirs)
            call file_truename(expand(dirs(i)%s), truename, st)
            if (.not. st%ok()) truename = expand(dirs(i)%s)

            if (.not. (has_string(dirs_done, truename) .or. has_string(dirs_done, directory_file_name(truename)))) then
                ! Try several variants of specified name.
                ! Try upcasing, appending `.info', or both.
                found = .false.

                do v = 1, size(variants)
                    file = expand(trim(variants(v)), truename)
                    call file_attributes(file, stat, found)
                    ! Shouldn't really happen, but sometimes does, eg on
                    ! Debian systems with buggy packages; so may as well
                    ! try it.
                    if (found) exit
                end do

                call add_string(dirs_done, truename)
                call add_string(dirs_done, directory_file_name(truename))

                if (found) then
                    call get_buffer_create(generate_new_buffer_name(" info dir"), newbuf, st)
                    call set_buffer(newbuf, st)
                    if (size(bufs) == 0) call message("Composing main Info directory...")
                    call insert_file_contents(file, .false., inserted, st)

                    if (st%ok()) then
                        ! (Info-dir-file-name is set in the buffer read.)
                        k = here()
                        locals(k)%dir_file_name = file
                        bufs = [newbuf, bufs]
                        call prepend_time(dir_file_attrs, file, stat%mtime_sec, stat%mtime_nsec)
                    else
                        call kill_buffer(newbuf, st)
                    end if

                    call set_buffer(main, st)
                end if
            end if

            if (i == size(dirs)) then
                k = here_in(main)
                locals(k)%dir_contents_directory = file_name_as_directory(dirs(i)%s)
            end if
        end do

        call set_buffer(main, st)

        if (size(bufs) == 0) then
            fold = bind_case_fold(fold)
            call plain_error(status, "Can't find the Info directory node")
            return
        end if

        ! Distinguish the dir file that comes with Fortmacs from all the
        ! others.  Yes, that is really what this is supposed to do.  The
        ! definition of `info_directory_list' puts it first on that list
        ! and so last in `buffers' at this point.
        buffer = bufs(size(bufs))
        ! Insert the entire original dir file as a start; note that we've
        ! already saved its default directory to use as the default
        ! directory for the whole concatenation.
        beg = point()
        call insert_buffer_substring(buffer, status)
        if (.not. status%ok()) return
        call goto_char(beg)
        ! Look at each of the other buffers one by one.
        allocate (nodes(0))

        do i = 1, size(bufs) - 1
            allocate (this_nodes(0))
            ! In each, find all the menus.
            call set_buffer(bufs(i), st)
            call goto_min()

            ! Find each menu, and add an elt to NODES for it.
            do while (refwd("^\* Menu:"))
                do
                    if (forward_line(1_int64) /= 0) exit
                    if (.not. eolp()) exit
                end do

                beg = point()
                if (.not. rebwd("^"//US)) exit
                if (.not. sfwd("Node: ")) exit
                nodename = following_node_name()
                if (.not. sfwd(LF//US, move=.true.)) continue
                call beginning_of_line(1_int64)
                end = point()
                call prepend_node(this_nodes, nodename, bufs(i), beg, end)
            end do

            found = .false.

            do j = 1, size(this_nodes)
                if (lower(this_nodes(j)%name) == "top") found = .true.
            end do

            if (found) then
                nodes = [this_nodes, nodes]
            else
                problems = .true.
                call message("No `top' node in "//locals(here_in(bufs(i)))%dir_file_name)
            end if

            deallocate (this_nodes)
        end do

        call set_buffer(main, st)

        ! Add to the main menu a menu item for each other node.
        if (.not. refwd("^\* Menu:")) then
            fold = bind_case_fold(fold)
            call set_error(status, FM_SEARCH_FAILED, "^\* Menu:")
            return
        end if

        call fwd_line(1_int64)
        allocate (menu_items(1))
        menu_items(1)%s = "top"
        beg = point()
        menu_end = point_max()
        if (sfwd(US)) menu_end = point()
        call goto_char(beg)

        do i = 1, size(nodes)
            nodename = nodes(i)%name
            beg = point()

            if (.not. has_string(menu_items, lower(nodename))) then
                if (.not. refwd("^\* +"//regexp_quote(nodename)//"::", menu_end)) then
                    call insert("* "//nodename//"::"//LF, st)
                    menu_end = menu_end + len("* "//nodename//"::"//LF)
                    call add_string(menu_items, nodename)
                end if
            end if

            call goto_char(beg)
        end do

        ! Now take each node of each of the other buffers and merge it into
        ! the main buffer.
        do i = 1, size(nodes)
            nodename = nodes(i)%name
            call goto_min()

            ! Find the like-named node in the main buffer.
            if (refwd("^"//US//".*"//LF//".*Node: "//regexp_quote(nodename)//"[,"//LF//TAB//"]")) then
                found = sfwd(LF//US, move=.true.)
                call beginning_of_line(1_int64)
                call insert(LF, st)
            else
                ! If none exists, add one.
                call goto_char(point_max())
                call insert(US//LF//"File: dir"//TAB//"Node: "//nodename//LF//LF//"* Menu:"//LF//LF, st)
            end if

            ! Merge the text from the other buffer's menu into the menu in
            ! the like-named node in the main buffer.
            call insert_buffer_substring(nodes(i)%buffer, st, nodes(i)%beg, nodes(i)%end)
        end do

        call dir_remove_duplicates()

        ! Kill all the buffers we just made, including the special one
        ! excised.
        do i = 1, size(bufs)
            call kill_buffer(bufs(i), st)
        end do

        call set_buffer(main, st)
        call goto_min()

        if (problems) then
            call message("Composing main Info directory...problems encountered, see `*Messages*'")
        else
            call message("Composing main Info directory...done")
        end if

        k = here()
        locals(k)%dir_contents = text(point_min(), point_max())
        locals(k)%dir_file_attributes = dir_file_attrs
        fold = bind_case_fold(fold)
        contents_dir = locals(k)%dir_contents_directory
        buffers(cur)%directory = contents_dir

    contains

        ! (push (cons FILE ATTRS) DIR-FILE-ATTRS).
        subroutine prepend_time(list, name, sec, nsec)
            type(file_time_type), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: sec
            integer(int32), intent(in) :: nsec
            type(file_time_type), allocatable :: grown(:)
            integer :: m

            allocate (grown(size(list) + 1))
            grown(1)%name = name
            grown(1)%sec = sec
            grown(1)%nsec = nsec

            do m = 1, size(list)
                grown(m + 1) = list(m)
            end do

            call move_alloc(grown, list)
        end subroutine prepend_time

        ! (push (list NODENAME BUFFER BEG END) THIS-BUFFER-NODES).
        subroutine prepend_node(list, name, b, beg, end)
            type(dir_node_type), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: name
            type(handle_type), intent(in) :: b
            integer(int64), intent(in) :: beg, end
            type(dir_node_type), allocatable :: grown(:)
            integer :: m

            allocate (grown(size(list) + 1))
            grown(1)%name = name
            grown(1)%buffer = b
            grown(1)%beg = beg
            grown(1)%end = end

            do m = 1, size(list)
                grown(m + 1) = list(m)
            end do

            call move_alloc(grown, list)
        end subroutine prepend_node
    end subroutine info_insert_dir

    ! The entry in `locals' of buffer B, made if needed.  Not a port.
    integer function here_in(b) result(k)
        type(handle_type), intent(in) :: b
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(b, st)
        k = here()
        call set_buffer(old, st)
    end function here_in

    ! Remove duplicate headings and entries from the merged directory
    ! node.  Port of info.el:Info-dir-remove-duplicates.
    subroutine dir_remove_duplicates()
        type(string_type), allocatable :: entries(:), seen(:)
        character(len=:), allocatable :: name, re, entry, x
        type(handle_type) :: saved, rb, re_m
        type(status_type) :: st
        integer(int64) :: limit, start, p0, p1
        integer :: i
        logical :: has_limit, has_re

        call goto_min()

        ! Remove duplicate headings in the same menu.
        do while (sfwd(LF//"* Menu:"))
            p0 = point()
            has_limit = sfwd(LF//US)
            limit = point()
            call goto_char(p0)

            ! Look for the next heading to unify.
            do while (refwd_limit("^\(\w.*\)"//LF//"\*"))
                name = match_string_no_properties(1)
                start = mbeg(0)
                has_re = .false.

                ! Check whether this heading should be streamlined.
                do i = 1, size(info_streamline_headings)
                    if (smatch(trim(info_streamline_headings(i)%re), name)) then
                        name = trim(info_streamline_headings(i)%name)
                        re = trim(info_streamline_headings(i)%re)
                        has_re = .true.
                    end if
                end do

                if (has_re) call replace_match(name, st, fixedcase=.true., literal=.true., subexp=1)

                if (refwd_limit("^[^* "//LF//TAB//"]")) then
                    call goto_char(mbeg(0))
                else
                    call goto_char(limit_or_max())
                end if

                ! Look for other headings of the same category and merge
                ! them.
                allocate (entries(0))
                saved = point_marker_here()

                do while (refwd_limit("^\(\w.*\)"//LF//"\*"))
                    if (heading_matches(match_string_no_properties(1))) then
                        p0 = mbeg(0)
                        call fwd_line(0_int64)
                        ! Delete redundant heading.
                        call delete_region(p0, point(), st)
                        ! Push the entries onto `text'.
                        p0 = point()

                        if (refwd("^[^* "//LF//TAB//"]")) then
                            p1 = mbeg(0)
                        else
                            p1 = limit_or_max()
                        end if

                        call delete_and_extract_region(p0, p1, entry, st)
                        call add_string(entries, entry)
                        call fwd_line(0_int64)
                    end if
                end do

                call goto_marker(saved)

                ! Insert the entries just found.
                do while (line_beginning_position(0_int64) == point() - 1)
                    call goto_char(point() - 1)
                end do

                do i = 1, size(entries)
                    call insert(entries(i)%s, st)

                    do while (line_beginning_position(0_int64) == point() - 1)
                        call delete_region(point() - 1, point(), st)
                    end do
                end do

                deallocate (entries)
                ! Now remove duplicate entries under the same heading.
                allocate (seen(0))
                call save_restriction(rb, re_m)
                call narrow_to_region(start, point(), st)
                call goto_min()

                do while (refwd("^\* \([^:"//LF//"]+:[^."//LF//"]+.\)", move=.true.))
                    ! Fold case straight away; `member-ignore-case' here
                    ! wasteful.
                    x = lower(match_string_no_properties(1))

                    if (has_string(seen, x)) then
                        p0 = mbeg(0)

                        if (refwd("^[^ "//TAB//"]", move=.true.)) then
                            call goto_char(mbeg(0))
                            p1 = point()
                        else
                            p1 = point_max()
                        end if

                        call delete_region(p0, p1, st)
                    else
                        call add_string(seen, x)
                    end if
                end do

                call restore_restriction(rb, re_m)
                deallocate (seen)
            end do
        end do

    contains

        ! (re-search-forward RE limit t), LIMIT being the end of the menu's
        ! node when found (nil, the end of the buffer, otherwise).
        logical function refwd_limit(re)
            character(len=*), intent(in) :: re

            if (has_limit) then
                refwd_limit = refwd(re, limit)
            else
                refwd_limit = refwd(re)
            end if
        end function refwd_limit

        ! (or limit (point-max)).
        integer(int64) function limit_or_max()
            if (has_limit) then
                limit_or_max = limit
            else
                limit_or_max = point_max()
            end if
        end function limit_or_max

        ! Whether the heading H is of the category being merged.
        logical function heading_matches(h)
            character(len=*), intent(in) :: h

            if (has_re) then
                heading_matches = smatch(re, h)
            else
                heading_matches = name == h
            end if
        end function heading_matches
    end subroutine dir_remove_duplicates

    ! Save the restriction (save-restriction) in markers B and E.
    ! Not a port.
    subroutine save_restriction(b, e)
        type(handle_type), intent(out) :: b, e
        type(status_type) :: st

        call make_marker(b)
        call set_marker(b, point_min(), st, current_buffer())
        call make_marker(e)
        call set_marker(e, point_max(), st, current_buffer())
        call set_marker_insertion_type(e, .true., st)
    end subroutine save_restriction

    ! Restore the restriction saved in B and E and free them.  Not a port.
    subroutine restore_restriction(b, e)
        type(handle_type), intent(inout) :: b, e
        type(status_type) :: st
        integer(int64) :: pb, pe

        call marker_position(b, pb, st)
        call marker_position(e, pe, st)
        call widen()
        call narrow_to_region(pb, pe, st)
        call free_marker(b, st)
        call free_marker(e, st)
    end subroutine restore_restriction

    ! A marker at point, for save-excursion.  Not a port.
    function point_marker_here() result(m)
        type(handle_type) :: m
        type(status_type) :: st

        call make_marker(m)
        call set_marker(m, point(), st, current_buffer())
    end function point_marker_here

    ! Go to marker M and free it (the end of save-excursion).  Not a port.
    subroutine goto_marker(m)
        type(handle_type), intent(inout) :: m
        type(status_type) :: st
        integer(int64) :: pos

        call marker_position(m, pos, st)
        if (st%ok()) call goto_char(pos)
        call free_marker(m, st)
    end subroutine goto_marker

    ! Read in the subfile holding the 0-based byte position NODEPOS of
    ! the Info file as a whole (not relative to a subfile), and return in
    ! RESULT the byte offset corresponding to NODEPOS from the beginning
    ! of the subfile.  On entry the current buffer must be the *info*
    ! buffer, not the info tags buffer.  Port of info.el:Info-read-subfile
    ! (a numeric NODEPOS).
    recursive subroutine read_subfile_at(nodepos, result, status)
        integer(int64), intent(in) :: nodepos
        integer(int64), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: lastfilename, thisfilename
        type(handle_type) :: info_buf, tagbuf
        type(status_type) :: st
        integer(int64) :: lastfilepos, thisfilepos, beg
        integer :: k

        status = status_type()
        result = 0
        lastfilepos = 0
        lastfilename = ""
        k = here()
        info_buf = current_buffer()
        call marker_buffer(locals(k)%tag_table_marker, tagbuf, st)
        call set_buffer(tagbuf, st)
        call goto_min()

        if (.not. looking(US)) then
            if (.not. sfwd(LF//US)) then
                call set_buffer(info_buf, st)
                call set_error(status, FM_SEARCH_FAILED, LF//US)
                return
            end if
        end if

        call fwd_line(2_int64)

        do while (.not. looking(US) .and. .not. eobp())
            if (.not. eolp()) then
                beg = point()
                if (.not. sfwd(": ")) exit
                thisfilename = text(beg, point() - 2)
                thisfilepos = read_number()
                ! read in version 19 stops at the end of number.
                ! Advance to the next line.
                call fwd_line(1_int64)
                if (thisfilepos > nodepos) exit
                lastfilename = thisfilename
                lastfilepos = thisfilepos
            else
                call fwd_line(1_int64)
            end if
        end do

        call set_buffer(info_buf, st)
        call read_subfile_named(lastfilename, status)
        if (.not. status%ok()) return
        ! Our caller ('Info-find-node-2') wants the (zero-based) byte
        ! offset corresponding to NODEPOS, from the beginning of the
        ! subfile.  This is especially important if NODEPOS is for an
        ! anchor reference, because for those the position is all we have.
        result = nodepos - lastfilepos + bufferpos_to_filepos(point())
    end subroutine read_subfile_at

    ! Read in the subfile named NAME if it is not current, and leave
    ! point after the summary segment.  Port of info.el:Info-read-subfile,
    ! in part.
    recursive subroutine read_subfile_named(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        character(len=:), allocatable :: subfile

        status = status_type()
        ! Assume previous buffer is in Info-mode.
        subfile = info_current_subfile()
        if (.not. allocated(subfile)) subfile = achar(0)

        if (subfile /= name) then
            saved = bind_inhibit_read_only()
            if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
            call widen()
            call erase_buffer(status)
            if (status%ok()) call info_insert_file_contents(name, status)
            call unbind_inhibit_read_only(saved)
            if (.not. status%ok()) return
            call restore_buffer_modified_p(.false.)
            call set_current_subfile(name)
        end if

        ! Widen in case we are in the same subfile as before.
        call widen()
        call goto_min()

        ! Skip the summary segment for `Info-search'.
        if (looking(US)) then
            call goto_char(point() + 1)
        else if (.not. sfwd(LF//US)) then
            call set_error(status, FM_SEARCH_FAILED, LF//US)
        end if
    end subroutine read_subfile_named

    ! The description of FILE for `list_buffers_directory'.  Port of
    ! info.el:Info-node-description.
    function node_description(file) result(d)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: d

        select case (file)
        case ("dir")
            d = "*Info Directory*"
        case default
            d = "("//file_name_nondirectory(file)//") "//current_node_or_empty()
        end select
    end function node_description

    ! Display images in current node.  Fortmacs displays no images, so
    ! this shows the alternative text if provided, or otherwise a clue
    ! that there's meant to be a picture (the text-only display).  Port of
    ! info.el:Info-display-images-node, in part.
    subroutine display_images_node()
        type(value_type) :: saved
        type(status_type) :: st
        character(len=:), allocatable :: params, replacement, src, alt, txt
        integer(int64) :: opoint, start
        logical :: fold, has_src, has_alt, has_text

        opoint = point()
        saved = bind_inhibit_read_only()
        fold = bind_case_fold(.true.)
        call goto_min()

        do while (refwd("\("//NUL//BS//"[[]image\(\(?:[^"//BS//"]\|[^"//NUL//"]+"//BS//"\)*\)"//NUL//BS//"[]]\)"))
            start = mbeg(1)
            params = match_string_no_properties(2)
            call parameter_value(params, "src", src, has_src)
            call parameter_value(params, "alt", alt, has_alt)
            call parameter_value(params, "text", txt, has_text)
            ! text-only display, show alternative text if provided, or
            ! otherwise a clue that there's meant to be a picture
            call delete_region(start, point(), st)

            if (has_text) then
                replacement = txt
            else if (has_alt) then
                replacement = alt
            else if (has_src) then
                replacement = "[image:"//src//"]"
            else
                replacement = "[image]"
            end if

            call insert(replacement, st)
        end do

        fold = bind_case_fold(fold)
        call unbind_inhibit_read_only(saved)
        call goto_char(min(opoint, point_max()))
        call restore_buffer_modified_p(.false.)
    end subroutine display_images_node

    ! The value of KEY in PARAMETER_STRING, a whitespace separated list of
    ! KEY=VALUE pairs (FOUND false if absent; case is ignored in KEY, as
    ! assoc-string does).  If VALUE contains whitespace or double quotes,
    ! it must be quoted in double quotes and any double quotes or
    ! backslashes must be escaped.  Port of
    ! info.el:Info-split-parameter-string and Info-unescape-quotes.
    subroutine parameter_value(parameter_string, key, value, found)
        character(len=*), intent(in) :: parameter_string, key
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        type(status_type) :: st
        character(len=:), allocatable :: k, v
        integer(int64) :: start, idx

        found = .false.
        value = ""
        start = 1

        do
            idx = string_match("\s *\([^=]+\)=\(?:\([^""[:space:]]+\)\|\(?:""\(\(?:[^\""]\|\\[\""]\)*\)""\)\)", &
                               parameter_string, st, start)
            if (idx == 0 .or. .not. st%ok()) exit
            start = mend(0)
            k = match_string_no_properties(1, parameter_string)

            if (mbeg(2) > 0) then
                v = match_string_no_properties(2, parameter_string)
            else
                v = unescape_quotes(match_string_no_properties(3, parameter_string))
            end if

            ! (The alist is built by pushing, so the last pair wins.)
            if (lower(k) == lower(key)) then
                value = v
                found = .true.
            end if
        end do
    end subroutine parameter_value

    ! Unescape double quotes and backslashes in VALUE.  Port of
    ! info.el:Info-unescape-quotes.
    function unescape_quotes(value) result(unquote)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: unquote
        integer :: i

        unquote = ""
        i = 1

        do while (i <= len(value))
            if (value(i:i) == "\" .and. i < len(value)) then
                if (value(i + 1:i + 1) == "\" .or. value(i + 1:i + 1) == '"') i = i + 1
            end if

            unquote = unquote//value(i:i)
            i = i + 1
        end do
    end function unescape_quotes

    ! Texinfo 4.7 adds cookies of the form ^@^H[NAME CONTENTS ^@^H].
    ! Hide any construct of the general form ^@[^@-^_][ ...  ^@[^@-^_]],
    ! including one optional trailing newline.  Port of
    ! info.el:Info-hide-cookies-node.
    subroutine hide_cookies_node()
        type(value_type) :: saved, prop
        type(status_type) :: st
        integer(int64) :: opoint, start
        logical :: fold

        opoint = point()
        saved = bind_inhibit_read_only()
        fold = bind_case_fold(.true.)
        call goto_min()

        do while (refwd("\("//NUL//"["//NUL//"-"//achar(31)//"][[][^"//NUL//"]*"//NUL//"["//NUL//"-"//achar(31) &
                        //"][]]"//LF//"?\)"))
            start = mbeg(1)
            call get_text_property(start, sym("invisible"), prop, st)

            if (nilp(prop)) then
                call get_text_property(start, sym("display"), prop, st)
                if (nilp(prop)) call put_text_property(start, point(), sym("invisible"), t_value(), st)
            end if
        end do

        fold = bind_case_fold(fold)
        call unbind_inhibit_read_only(saved)
        call goto_char(opoint)
        call restore_buffer_modified_p(.false.)
    end subroutine hide_cookies_node

    !> Select the Info node that point is in.  Port of
    !> info.el:Info-select-node.
    recursive subroutine info_select_node(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, start, finish
        type(status_type) :: st
        logical :: fold

        status = status_type()
        ! Bind this in case the user sets it to nil.
        fold = bind_case_fold(.true.)
        opoint = point()

        ! Find beginning of node.
        if (sbwd(LF//US, move=.true.)) then
            call fwd_line(2_int64)
        else if (looking(US)) then
            call fwd_line(1_int64)
        else
            fold = bind_case_fold(fold)
            call set_error(status, FM_SEARCH_FAILED, LF//US)
            return
        end if

        ! Get nodename spelled as it is in the node.
        if (.not. refwd("Node:[ "//TAB//"]*")) then
            fold = bind_case_fold(fold)
            call set_error(status, FM_SEARCH_FAILED, "Node:[ "//TAB//"]*")
            return
        end if

        start = point()
        finish = skip_chars_forward("^,"//TAB//LF, st)
        call set_current_node(text(start, point()))
        call info_set_mode_line()
        ! Find the end of it, and narrow.
        call beginning_of_line(1_int64)
        start = point()

        ! Narrow to the node contents (active expressions, "execute:",
        ! are Deferred with `info_enable_active_nodes').
        if (refwd(LF//"["//US//FF//"]")) then
            finish = point() - 1
        else
            finish = point_max()
        end if

        call narrow_to_region(start, finish, st)
        ! Add a new unique history item to full history list
        call note_history_list(current_file_or_empty(), current_node_or_empty())

        k_forward: block
            integer :: k
            k = here()
            deallocate (locals(k)%history_forward)
            allocate (locals(k)%history_forward(0))
        end block k_forward

        if (info_fontify_maximum_menu_size /= INFO_FONTIFY_MAXIMUM_MENU_SIZE_NIL) call info_fontify_node()
        call update_header_line()
        call list_buffers_directory%set_local(node_description(current_file_or_empty()))
        call display_images_node()
        call hide_cookies_node()
        call goto_char(min(max(opoint, point_min()), point_max()))
        fold = bind_case_fold(fold)
        call info_selection_hook%run(status)
    end subroutine info_select_node

    ! Set the buffer identification of the mode line: the buffer name,
    ! the manual's name in parentheses and the node name.  Port of
    ! info.el:Info-set-mode-line (the keymap and help-echo on the node
    ! name are Deferred with the mouse).
    subroutine info_set_mode_line()
        character(len=:), allocatable :: file, manual, node
        type(header_line_format_type) :: id
        integer :: i

        file = info_current_file()

        if (allocated(file)) then
            ! Remove trailing ".info" and ".info.gz", etc.
            manual = file_name_nondirectory(file)
            i = index(manual, ".")
            if (i > 0) manual = manual(1:i - 1)
            manual = replace_all("%", "%%", manual)
        else
            manual = "*nil*"
        end if

        call id%add("%b", "mode_line_buffer_id")
        call id%add(" ("//manual//") ")
        node = info_current_node()

        if (allocated(node)) then
            call id%add(replace_all("%", "%%", node), "mode_line_buffer_id")
        else
            call id%add("")
        end if

        buffers(cur)%mode_line_buffer_identification = id
    end subroutine info_set_mode_line

    !> Go to Info node named NODENAME.  Give just NODENAME or
    !> (FILENAME)NODENAME.  If NODENAME is of the form (FILENAME)NODENAME,
    !> the node is in the Info file FILENAME; otherwise, NODENAME should be
    !> in the current Info file (or one of its sub-files).
    !> Completion is available for node names in the current Info file as
    !> well as in the Info file FILENAME after the closing parenthesis in
    !> (FILENAME).
    !> Empty NODENAME in (FILENAME) defaults to the Top node.
    !> If FORK is true (interactively with a prefix arg), show the node in
    !> a new Info buffer (Deferred: `clone_buffer' is not ported).
    !>
    !> This function first looks for a case-sensitive match for the node
    !> part of NODENAME; if none is found it then tries a case-insensitive
    !> match (unless STRICT_CASE is true).  Port of info.el:Info-goto-node.
    recursive subroutine info_goto_node(nodename, status, strict_case)
        character(len=*), intent(in) :: nodename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: strict_case
        character(len=:), allocatable :: filename, node
        type(status_type) :: st
        integer(int64) :: idx

        status = status_type()
        call info_initialize()
        idx = string_match("\s *\((\s *\([^"//TAB//")]*\)\s *)\s *\|\)\(.*\)", nodename, st)

        if (mbeg(1) == mend(1)) then
            filename = ""
        else
            filename = match_string_no_properties(2, nodename)
        end if

        node = match_string_no_properties(3, nodename)
        filename = trim_trailing_space(filename)
        node = trim_trailing_space(node)
        if (transient_mark_mode%value) call deactivate_mark_now()
        if (len(node) == 0) node = "Top"

        if (len(filename) == 0) then
            call info_find_node(nodename=node, status=status, strict_case=strict_case)
        else
            call info_find_node(filename, node, status, strict_case=strict_case)
        end if
    end subroutine info_goto_node

    ! (deactivate-mark).  Not a port.
    subroutine deactivate_mark_now()
        call deactivate_mark()
    end subroutine deactivate_mark_now

    recursive subroutine cmd_info_goto_node(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node

        call read_node_name("Go to node: ", node, status)
        if (.not. status%ok()) return
        call info_goto_node(node, status)
    end subroutine cmd_info_goto_node

    !> Return the URL corresponding to NODE.
    !>
    !> NODE should be a string of the form "(manual)Node".
    !>
    !> The correspondence between Info manuals and their Web URLs is
    !> established by `info_url_alist', which see.  Port of
    !> info.el:Info-url-for-node.
    function info_url_for_node(node, status) result(url)
        character(len=*), intent(in) :: node
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: url, manual, name, encoded
        integer :: i, j

        status = status_type()
        url = ""
        if (.not. allocated(info_url_alist)) call init_url_alist()

        ! GNU Texinfo skips whitespaces and newlines between the closing
        ! parenthesis and the node-name, i.e. space, tab, line feed and
        ! carriage return.
        if (.not. smatch("\`(\(.+\))[ "//TAB//LF//achar(13)//"]*\(.+\)\'", node, .true.)) then
            call plain_error(status, "Invalid node-name "//node)
            return
        end if

        manual = match_string_no_properties(1, node)
        name = match_string_no_properties(2, node)

        do i = 1, size(info_url_alist)
            do j = 1, size(info_url_alist(i)%manuals)
                if (len_trim(info_url_alist(i)%manuals(j)) == 0) cycle

                if (lower(trim(info_url_alist(i)%manuals(j))) == lower(manual)) then
                    ! Reproduce GNU Texinfo's way of URL-encoding.
                    ! (info "(texinfo) HTML Xref Node Name Expansion")
                    if (name == "Top") then
                        encoded = ""
                    else
                        encoded = url_hexify(replace_all(" ", "-", texinfo_escape(name)))//".html"
                    end if

                    if (associated(info_url_alist(i)%fn)) then
                        call info_url_alist(i)%fn(manual, name, encoded, url)
                    else
                        url = format_url(trim(info_url_alist(i)%url))
                    end if

                    return
                end if
            end do
        end do

        call plain_error(status, "No URL-specification associated with manual-name `"//manual//"'")

    contains

        ! (format-spec SPEC '((?m . MANUAL) (?n . NAME) (?e . ENCODED))).
        function format_url(spec) result(u)
            character(len=*), intent(in) :: spec
            character(len=:), allocatable :: u
            integer :: p

            u = ""
            p = 1

            do while (p <= len(spec))
                if (spec(p:p) == "%" .and. p < len(spec)) then
                    select case (spec(p + 1:p + 1))
                    case ("m")
                        u = u//manual
                    case ("n")
                        u = u//name
                    case ("e")
                        u = u//encoded
                    case default
                        u = u//spec(p + 1:p + 1)
                    end select

                    p = p + 2
                else
                    u = u//spec(p:p)
                    p = p + 1
                end if
            end do
        end function format_url
    end function info_url_for_node

    ! The characters of S that GNU Texinfo escapes in HTML file names
    ! written as _00XX (control characters and the ASCII punctuation).
    ! Port of info.el:Info-url-for-node, in part.
    function texinfo_escape(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        character(len=2) :: hex
        integer :: i, ch

        t = ""

        do i = 1, len(s)
            ch = iachar(s(i:i))

            if (ch < 32 .or. (ch >= 33 .and. ch <= 47) .or. (ch >= 58 .and. ch <= 64) .or. &
                (ch >= 91 .and. ch <= 96) .or. (ch >= 123 .and. ch <= 127)) then
                write (hex, "(z2.2)") ch
                t = t//"_00"//lower(adjustl(hex))
            else
                t = t//s(i:i)
            end if
        end do
    end function texinfo_escape

    ! (url-hexify-string S): every byte but the unreserved characters
    ! (letters, digits, "-", "_", "." and "~") as %XX.
    ! Port of url-util.el:url-hexify-string (without ALLOWED-CHARS).
    function url_hexify(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        character(len=2) :: hex
        integer :: i

        t = ""

        do i = 1, len(s)
            select case (s(i:i))
            case ("a":"z", "A":"Z", "0":"9", "-", "_", ".", "~")
                t = t//s(i:i)
            case default
                write (hex, "(z2.2)") iachar(s(i:i))
                t = t//"%"//hex
            end select
        end do
    end function url_hexify

    ! The default value of `info_url_alist'.  Not a port.
    subroutine init_url_alist()
        allocate (info_url_alist(1))
        info_url_alist(1)%manuals(1:69) = [character(len=16) :: &
                                           "auth", "autotype", "bovine", "calc", "ccmode", "cl", "dbus", "dired-x", &
                                           "ebrowse", "ede", "ediff", "edt", "efaq", "efaq-w32", "eglot", "eieio", &
                                           "eintr", "elisp", "emacs", "emacs-gnutls", "emacs-mime", "epa", "erc", &
                                           "ert", "eshell", "eudc", "eww", "flymake", "forms", "gnus", &
                                           "htmlfontify", "idlwave", "ido", "info", "mairix-el", "message", &
                                           "mh-e", "modus-themes", "newsticker", "nxml-mode", "octave-mode", &
                                           "org", "pcl-cvs", "pgg", "rcirc", "reftex", "remember", "sasl", "sc", &
                                           "semantic", "ses", "sieve", "smtpmail", "speedbar", "srecode", &
                                           "todo-mode", "tramp", "transient", "url", "use-package", "vhdl-mode", &
                                           "viper", "vtable", "widget", "wisent", "woman", "", "", ""]
        info_url_alist(1)%url = "https://www.gnu.org/software/emacs/manual/html_node/%m/%e"
    end subroutine init_url_alist

    ! Complete STRING in the list LIST for ACTION ((complete-with-action
    ! ACTION LIST STRING PRED)).  Not a port.
    recursive subroutine complete_with_action(action, list, string, result, status, pred)
        integer, intent(in) :: action
        type(string_type), intent(in) :: list(:)
        character(len=*), intent(in) :: string
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        type(completion_table_type) :: table

        status = status_type()
        table = completion_list_table(strings_value(list))

        select case (action)
        case (COMPLETE_TRY)
            call try_completion(string, table, result, status, pred)
        case (COMPLETE_ALL)
            call all_completions(string, table, result, status, pred)
        case (COMPLETE_TEST)
            result = nil()
            if (test_completion(string, table, pred)) result = t_value()
        case default
            result = nil()
        end select
    end subroutine complete_with_action

    ! Return the Info file names (without suffixes) in DIRS for the
    ! FILENAME part of a node named "(FILENAME)NODENAME": the files with a
    ! standard suffix, excluding subfiles of split Info files and backup
    ! files.  Port of info.el:Info-read-node-name-2, in part.
    subroutine info_file_names(dirs, names)
        type(string_type), intent(in) :: dirs(:)
        type(string_type), allocatable, intent(out) :: names(:)
        type(value_type) :: files
        type(status_type) :: st
        character(len=:), allocatable :: file, suffix_re
        type(string_type), allocatable :: suffixes(:)
        integer :: i
        integer(int64) :: idx, j

        allocate (names(0), suffixes(0))

        do i = 1, size(info_suffix_list)
            if (len_trim(info_suffix_list(i)%suffix) > 0) call add_string(suffixes, trim(info_suffix_list(i)%suffix))
        end do

        ! (concat (regexp-opt suffixes t) "\\'")
        suffix_re = "\("

        do i = 1, size(suffixes)
            if (i > 1) suffix_re = suffix_re//"\|"
            suffix_re = suffix_re//regexp_quote(suffixes(i)%s)
        end do

        suffix_re = suffix_re//"\)\'"

        do i = 1, size(dirs)
            if (.not. file_directory_p(dirs(i)%s)) cycle
            call directory_files(dirs(i)%s, files, st)
            if (.not. st%ok()) cycle

            do j = 1, list_length(files)
                file = xstring(list_elt(files, j))
                ! If the file name has a standard suffix, include it
                ! (without the suffix).
                idx = string_match(suffix_re, file, st)

                if (idx > 0 .and. .not. smatch("\.info-[0-9]+", file) .and. .not. smatch("~\'", file)) then
                    if (.not. has_string(names, file(1:string_index_to_offset(file, idx)))) then
                        call add_string(names, file(1:string_index_to_offset(file, idx)))
                    end if
                end if
            end do
        end do
    end subroutine info_file_names

    ! Internal function used to complete Info node names.  See
    ! `completing_read' for a description of arguments and usage.
    ! Port of info.el:Info-read-node-name-1 (with Info-read-node-name-2).
    recursive subroutine read_node_name_1(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(string_type), allocatable :: dirs(:), names(:), nodes(:)
        character(len=:), allocatable :: file0, file1, nodename, found
        type(value_type) :: comp, comp2
        type(status_type) :: st
        integer(int64) :: idx

        status = status_type()
        result = nil()

        if (action == COMPLETE_BOUNDARIES) then
            if (present(suffix)) continue
            return
        end if

        if (smatch("\`([^)]*\'", string)) then
            ! First complete embedded file names.
            call setting_dirs(info_directory_list, dirs)
            call info_file_names(dirs, names)

            select case (action)
            case (COMPLETE_TRY)
                ! (completion-table-with-terminator ")" ...) in the
                ! context "(".
                call complete_with_action(COMPLETE_TRY, names, string(2:), comp, status, pred)
                if (.not. status%ok()) return

                if (is_t(comp)) then
                    result = make_string("("//string(2:)//")")
                else if (stringp(comp)) then
                    call complete_with_action(COMPLETE_TRY, names, xstring(comp), comp2, status, pred)

                    if (len(xstring(comp)) > 0 .and. is_t(comp2)) then
                        result = make_string("("//xstring(comp)//")")
                    else
                        result = make_string("("//xstring(comp))
                    end if
                else
                    result = comp
                end if
            case default
                call complete_with_action(action, names, string(2:), result, status, pred)
            end select
        else if (smatch("\`(\([^)]+\))", string, .true.)) then
            ! If a file name was given, complete nodes in the file.
            file0 = match_string_no_properties(0, string)
            file1 = match_string_no_properties(1, string)
            idx = mend(0)
            nodename = string(string_index_to_offset(string, idx) + 1:)

            if (len(nodename) == 0 .and. action == COMPLETE_TEST) then
                ! Empty node name is permitted that means "Top".
                result = t_value()
                return
            end if

            allocate (nodes(0))
            call info_find_file(file1, found, st, noerror=.true., no_pop_to_dir=.true.)
            if (allocated(found)) call build_node_completions(nodes, st, found)
            call complete_with_action(action, nodes, nodename, result, status, pred)
            if (action == COMPLETE_TRY .and. stringp(result)) result = make_string(file0//xstring(result))
        else
            ! Otherwise use Info-read-node-completion-table.
            call complete_with_action(action, read_node_completion_table, string, result, status, pred)
        end if
    end subroutine read_node_name_1

    ! Whether V is t.  Not a port.
    logical function is_t(v)
        type(value_type), intent(in) :: v

        is_t = .not. nilp(v) .and. .not. stringp(v) .and. .not. consp(v)
    end function is_t

    ! Read an Info node name with completion, prompting with PROMPT.
    ! A node name can have the form "NODENAME", referring to a node in the
    ! current Info file, or "(FILENAME)NODENAME", referring to a node in
    ! FILENAME.  "(FILENAME)" is a short format to go to the Top node in
    ! FILENAME.  Port of info.el:Info-read-node-name.
    recursive subroutine read_node_name(prompt, nodename, status, default)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: nodename
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default
        logical :: saved_ignore

        status = status_type()
        call build_node_completions(read_node_completion_table, status)
        if (.not. status%ok()) return
        saved_ignore = completion_ignore_case%value

        do
            completion_ignore_case%value = .true.
            call completing_read(prompt, completion_function_table(read_node_name_1), nodename, status, &
                                 require_match=REQUIRE_MATCH_T, history="info_minibuf_history", default=default)
            completion_ignore_case%value = saved_ignore
            if (.not. status%ok()) return
            if (len(nodename) > 0) exit
        end do
    end subroutine read_node_name

    ! The node names of FILENAME (absent: the current Info file) for
    ! completion, "*" first.  Port of info.el:Info-build-node-completions.
    recursive subroutine build_node_completions(compl, status, filename)
        type(string_type), allocatable, intent(out) :: compl(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: filename
        type(completions_type), allocatable :: grown(:)
        type(handle_type) :: old, temp
        type(status_type) :: st
        integer :: i, k

        status = status_type()

        if (present(filename)) then
            if (.not. allocated(file_completions)) allocate (file_completions(0))

            do i = 1, size(file_completions)
                if (file_completions(i)%file == filename) then
                    compl = file_completions(i)%nodes
                    return
                end if
            end do

            allocate (compl(0))
            ! (with-temp-buffer (Info-mode) ...)
            old = current_buffer()
            call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st, inhibit_buffer_hooks=.true.)
            call set_buffer(temp, st)
            call info_mode(st)
            if (st%ok()) call info_goto_node("("//filename//")Top", st)

            ! `Info-goto-node' signals a `user-error' when there are no
            ! nodes in the file in question (for instance, if it's not
            ! actually an Info file).
            if (st%ok()) then
                call build_node_completions_1(compl, status)
                allocate (grown(size(file_completions) + 1))
                grown(1)%file = filename
                grown(1)%nodes = compl

                do i = 1, size(file_completions)
                    grown(i + 1) = file_completions(i)
                end do

                call move_alloc(grown, file_completions)
            end if

            call info_kill_buffer(st)
            call set_buffer(old, st)
            call kill_buffer(temp, st)
            return
        end if

        k = here()

        if (locals(k)%has_completions) then
            compl = locals(k)%completions
        else
            call build_node_completions_1(compl, status)
        end if
    end subroutine build_node_completions

    !> The node names of the current Info file for completion, "*" first:
    !> the names in the tag table or in the node headers.  Port of
    !> info.el:Info-build-node-completions.
    recursive subroutine info_build_node_completions(compl, status)
        type(string_type), allocatable, intent(out) :: compl(:)
        type(status_type), intent(out) :: status

        call build_node_completions(compl, status)
    end subroutine info_build_node_completions

    ! Collect the node names of the current Info file.  Port of
    ! info.el:Info-build-node-completions-1.
    recursive subroutine build_node_completions_1(compl, status)
        type(string_type), allocatable, intent(out) :: compl(:)
        type(status_type), intent(out) :: status
        character(len=*), parameter :: node_regexp = "Node: *\([^,"//LF//"]*\) *[,"//LF//TAB//"]"
        type(string_type), allocatable :: found(:)
        type(handle_type) :: old, tagbuf, rb, re_m
        type(status_type) :: st
        integer(int64) :: opoint, mpos, beg
        integer :: k
        logical :: fold

        status = status_type()
        allocate (found(0))
        ! Bind this in case the user sets it to nil.
        fold = bind_case_fold(.true.)
        old = current_buffer()
        opoint = point()
        k = here()

        if (locals(k)%tag_table_marker%index == 0) then
            fold = bind_case_fold(fold)
            call plain_error(status, "No Info tags found")
            return
        end if

        call marker_buffer(locals(k)%tag_table_marker, tagbuf, st)

        if (tagbuf%index /= 0) then
            call marker_position(locals(k)%tag_table_marker, mpos, st)
            call set_buffer(tagbuf, st)
            call save_restriction(rb, re_m)
            call widen()
            call goto_char(mpos)

            do while (refwd(LF//"\(Node\|Ref\): \(.*\)"//DEL))
                call add_string(found, match_string_no_properties(2))
            end do

            call restore_restriction(rb, re_m)
            call set_buffer(old, st)
        else
            call save_restriction(rb, re_m)
            call widen()
            call goto_min()
            ! If the buffer begins with a node header, process that first.
            if (node_at_bob_matching(node_regexp) > 0) call add_string(found, match_string_no_properties(1))

            ! Now for the rest of the nodes.
            do while (sfwd(LF//US))
                call fwd_line(1_int64)
                beg = point()
                call fwd_line(1_int64)
                if (rebwd(node_regexp, beg)) call add_string(found, match_string_no_properties(1))
            end do

            call restore_restriction(rb, re_m)
        end if

        call goto_char(opoint)
        allocate (compl(1))
        compl(1)%s = "*"

        do k = 1, size(found)
            call add_string(compl, found(k)%s)
        end do

        k = here()
        locals(k)%has_completions = .true.
        locals(k)%completions = compl
        fold = bind_case_fold(fold)
    end subroutine build_node_completions_1

    ! If this node has been visited, restore the point value when we
    ! left.  Port of info.el:Info-restore-point.
    subroutine restore_point(hl)
        type(info_history_entry_type), intent(in) :: hl(:)
        integer :: i

        do i = 1, size(hl)
            if (hl(i)%file == current_file_or_empty() .and. hl(i)%node == current_node_or_empty()) then
                call goto_char(hl(i)%pos)
                return
            end if
        end do
    end subroutine restore_point

    ! ------------------------------------------------------------------
    ! Moving between nodes
    ! ------------------------------------------------------------------

    !> Extract the value of the node-pointer named NAME.
    !> If there is none, use ERRORNAME in the error message (NAME if
    !> absent); if NOERROR is true, just return unallocated.  Port of
    !> info.el:Info-extract-pointer (ERRORNAME t is NOERROR).
    recursive subroutine info_extract_pointer(name, value, status, errorname, noerror)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: errorname
        logical, intent(in), optional :: noerror
        integer(int64) :: opoint, bound
        logical :: fold, quiet

        status = status_type()
        quiet = .false.
        if (present(noerror)) quiet = noerror
        ! Bind this in case the user sets it to nil.
        fold = bind_case_fold(.true.)
        opoint = point()
        call goto_min()
        bound = point()
        call fwd_line(1_int64)

        if (rebwd(name//":"//following_node_name_re(), bound)) then
            value = match_string_no_properties(1)
        else if (.not. quiet) then
            if (present(errorname)) then
                call user_error(status, "Node has no "//capitalize_string(errorname))
            else
                call user_error(status, "Node has no "//capitalize_string(name))
            end if
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)
    end subroutine info_extract_pointer

    ! Return a regexp matching a node name.
    ! ALLOWEDCHARS, if present, goes within [...] to make a regexp saying
    ! which chars may appear in the node name.
    ! Submatch 1 is the complete node name.
    ! Submatch 2 if non-nil is the parenthesized file name part of the
    ! node name.
    ! Submatch 3 is the local part of the node name.
    ! End of submatch 0, 1, and 3 are the same, so you can safely concat.
    ! Port of info.el:Info-following-node-name-re.
    function following_node_name_re(allowedchars) result(re)
        character(len=*), intent(in), optional :: allowedchars
        character(len=:), allocatable :: re, allowed

        if (present(allowedchars)) then
            allowed = allowedchars
        else
            allowed = "^,"//TAB//LF
        end if

        re = "[ "//TAB//LF//"]*" & ! Skip leading space.
             //"\(\(([^)]+)\)?" & ! Node name can start with a file name.
             //"\(["//allowed//"]*" & ! Any number of allowed chars.
             //"["//allowed//" ]" & ! The last char can't be a space.
             //"\|\)\)" ! Allow empty node names.
    end function following_node_name_re

    ! The node name after point (unallocated if none).  Port of
    ! info.el:Info-following-node-name.
    function following_node_name() result(name)
        character(len=:), allocatable :: name

        if (looking(following_node_name_re())) then
            name = match_string_no_properties(1)
        else
            name = ""
        end if
    end function following_node_name

    !> Go to the "next" node, staying on the same hierarchical level.
    !> This command doesn't descend into sub-nodes, like
    !> \<info_mode_map>\[info_forward_node] does.  Port of info.el:Info-next.
    recursive subroutine info_next(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node

        call ensure_info_buffer(status)
        if (.not. status%ok()) return
        call info_extract_pointer("next", node, status)
        if (.not. status%ok()) return
        call info_goto_node(node, status)
    end subroutine info_next

    !> Go to the "previous" node, staying on the same hierarchical level.
    !> This command doesn't go up to the parent node, like
    !> \<info_mode_map>\[info_backward_node] does.  Port of
    !> info.el:Info-prev.
    recursive subroutine info_prev(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node

        call ensure_info_buffer(status)
        if (.not. status%ok()) return
        call info_extract_pointer("prev[ious]*", node, status, "previous")
        if (.not. status%ok()) return
        call info_goto_node(node, status)
    end subroutine info_prev

    ! (or (derived-mode-p 'Info-mode) (info-pop-to-buffer)): the commands
    ! that move between nodes work in the *info* buffer when called
    ! elsewhere.  (save-window-excursion is not needed: the window
    ! configuration only changes when another buffer is shown.) Not a port.
    recursive subroutine ensure_info_buffer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. derived_mode_p(major_modes%info_mode)) call info_pop_to_buffer(status)
    end subroutine ensure_info_buffer

    !> Go to the superior node of this node.
    !> If SAME_FILE is true, do not move to a different Info file.
    !> Port of info.el:Info-up.
    recursive subroutine info_up(status, same_file)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: same_file
        character(len=:), allocatable :: old_node, old_file, node, re
        type(info_history_entry_type), allocatable :: history(:)
        integer(int64) :: p
        logical :: same, found

        call ensure_info_buffer(status)
        if (.not. status%ok()) return
        same = .false.
        if (present(same_file)) same = same_file
        old_node = current_node_or_empty()
        old_file = info_current_file()
        call info_extract_pointer("up", node, status)
        if (.not. status%ok()) return

        if (same .and. smatch("^(", node)) then
            call plain_error(status, "Up node is in another Info file")
            return
        end if

        call info_goto_node(node, status)
        if (.not. status%ok()) return
        p = point()
        call goto_min()
        found = .false.

        if (allocated(old_file)) then
            if (sfwd(LF//"* Menu:")) then
                if (old_node == "Top") then
                    re = LF//"\*[^:]+: +("//file_name_nondirectory(old_file)//")"
                else
                    re = LF//"\* +\("//regexp_quote(old_node)//":\|[^:]+: +"//regexp_quote(old_node)//"\)"
                end if

                found = refwd(re)
            end if
        end if

        if (found) then
            call beginning_of_line(1_int64)
            if (looking("^\* ")) call goto_char(point() + 2)
        else
            call goto_char(p)
            call info_history_variable(history)
            call restore_point(history)
        end if
        ! (scroll-conservatively, which would recenter, is not ported.)
    end subroutine info_up

    !> Go back in the history to the last node visited.  Port of
    !> info.el:Info-history-back.
    recursive subroutine info_history_back(status)
        type(status_type), intent(out) :: status
        type(info_history_entry_type), allocatable :: history_forward(:)
        type(info_history_entry_type) :: last
        integer :: k

        status = status_type()
        k = here()

        if (size(locals(k)%history) == 0) then
            call user_error(status, "This is the first Info node you looked at")
            return
        end if

        history_forward = locals(k)%history_forward
        call push_history(history_forward, current_file_or_empty(), current_node_or_empty(), point())
        last = locals(k)%history(1)
        call pop_history(locals(k)%history)
        call info_find_node(last%file, last%node, status)
        if (.not. status%ok()) return
        k = here()
        call pop_history(locals(k)%history)
        locals(k)%history_forward = history_forward
        call goto_char(last%pos)
    end subroutine info_history_back

    !> Go forward in the history of visited nodes.  Port of
    !> info.el:Info-history-forward.
    recursive subroutine info_history_forward(status)
        type(status_type), intent(out) :: status
        type(info_history_entry_type), allocatable :: history_forward(:)
        type(info_history_entry_type) :: next
        integer :: k

        status = status_type()
        k = here()

        if (size(locals(k)%history_forward) == 0) then
            call user_error(status, "This is the last Info node you looked at")
            return
        end if

        history_forward = locals(k)%history_forward
        next = history_forward(1)
        call pop_history(history_forward)
        call info_find_node(next%file, next%node, status)
        if (.not. status%ok()) return
        k = here()
        locals(k)%history_forward = history_forward
        call goto_char(next%pos)
    end subroutine info_history_forward

    ! The virtual Info files defined so far: the directory.  Not a port.
    subroutine init_virtual_files()
        allocate (info_virtual_files(3))
        info_virtual_files(1)%name = "\`dir\'"
        info_virtual_files(1)%find_file => directory_find_file
        info_virtual_files(1)%find_node => directory_find_node
        info_virtual_files(1)%toc_nodes => top_toc_nodes
        info_virtual_files(2)%name = "\`\*History\*\'"
        info_virtual_files(2)%find_file => directory_find_file
        info_virtual_files(2)%find_node => history_find_node
        info_virtual_files(2)%toc_nodes => top_toc_nodes
        info_virtual_files(3)%name = "\`\*Apropos\*\'"
        info_virtual_files(3)%find_file => directory_find_file
        info_virtual_files(3)%find_node => apropos_find_node
        info_virtual_files(3)%toc_nodes => apropos_toc_nodes
        info_virtual_files(3)%slow = .true.
        if (.not. allocated(virtual_index_nodes)) allocate (virtual_index_nodes(0))
        if (.not. allocated(apropos_nodes)) allocate (apropos_nodes(0))
        if (.not. allocated(info_search_whitespace_regexp)) info_search_whitespace_regexp = "\s-+"
        allocate (info_virtual_nodes(2))
        info_virtual_nodes(1)%name = "\`\*TOC\*\'"
        info_virtual_nodes(1)%find_node => toc_find_node
        info_virtual_nodes(2)%name = "\`\*Index.*\*\'"
        info_virtual_nodes(2)%find_node => virtual_index_find_node
        info_virtual_nodes(2)%slow = .true.
    end subroutine init_virtual_files

    ! The table of contents of the directory and history files: a Top
    ! node without parent, section or children.  Port of
    ! info.el:Info-directory-toc-nodes and Info-history-toc-nodes.
    recursive subroutine top_toc_nodes(filename, nodes)
        character(len=*), intent(in) :: filename
        type(toc_node_type), allocatable, intent(out) :: nodes(:)

        if (len(filename) < 0) continue
        allocate (nodes(1))
        nodes(1)%name = "Top"
        allocate (nodes(1)%children(0))
    end subroutine top_toc_nodes

    ! Directory-specific implementation of `info_find_file'.  Port of
    ! info.el:Info-directory-find-file.
    recursive subroutine directory_find_file(filename, result, status)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        status = status_type()
        result = filename
    end subroutine directory_find_file

    ! Directory-specific implementation of `Info-find-node-2'.  Port of
    ! info.el:Info-directory-find-node.
    recursive subroutine directory_find_node(filename, nodename, no_going_back, status)
        character(len=*), intent(in) :: filename, nodename
        logical, intent(in) :: no_going_back
        type(status_type), intent(out) :: status

        if (no_going_back .or. len(filename) < 0 .or. len(nodename) < 0) continue
        call info_insert_dir(status)
    end subroutine directory_find_node

    !> Go to the Info directory node.  Port of info.el:Info-directory.
    recursive subroutine info_directory(status)
        type(status_type), intent(out) :: status

        call info_find_node("dir", "top", status)
    end subroutine info_directory

    !> Follow cross reference named FOOTNOTENAME to the node it refers to.
    !> FOOTNOTENAME may be an abbreviation of the reference name.  (FORK,
    !> showing the node in a new Info buffer, is Deferred with
    !> `clone_buffer'.)  Port of info.el:Info-follow-reference.
    recursive subroutine info_follow_reference(footnotename, status)
        character(len=*), intent(in) :: footnotename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: str, target
        integer(int64) :: opoint, next_ref, prev_ref, bol, p
        type(status_type) :: st
        logical :: fold, has_next, has_prev, ok
        integer :: i

        status = status_type()
        fold = bind_case_fold(.true.)
        str = "\*note "//regexp_quote(footnotename)
        ! Each space as any white space.
        i = 1

        do
            p = index(str(i:), " ")
            if (p == 0) exit
            i = i + int(p) - 1
            str = str(1:i - 1)//"[ "//TAB//LF//"]+"//str(i + 1:)
            i = i + 6
        end do

        opoint = point()

        ! Move point to the beginning of reference if point is on
        ! reference
        if (.not. looking("\*note[ "//LF//TAB//"]+")) then
            p = point()
            bol = skip_chars_backward(" "//LF//TAB, st)
            bol = line_beginning_position(1_int64)
            call goto_char(p)

            if (looking_back("\*note[ "//LF//TAB//"]+", st, bol)) then
                call goto_char(mbeg(0))
            else
                call goto_char(point() + 5) ! skip a possible *note
                ok = rebwd("\*note[ "//LF//TAB//"]+")
                if (ok) ok = looking(str)
                call goto_char(p)

                if (ok) then
                    if (point() <= mend(0)) call goto_char(mbeg(0))
                end if
            end if
        end if

        ! Go to the reference closest to point
        p = point()
        has_next = refwd(str)
        if (has_next) next_ref = mbeg(0) + 5
        call goto_char(p)
        has_prev = rebwd(str)
        if (has_prev) prev_ref = mbeg(0) + 5
        call goto_char(p)

        if (has_next .and. has_prev) then
            if (abs(next_ref - point()) < abs(prev_ref - point())) then
                call goto_char(next_ref)
            else
                call goto_char(prev_ref)
            end if
        else if (has_next) then
            call goto_char(next_ref)
        else if (has_prev) then
            call goto_char(prev_ref)
        else
            call goto_char(opoint)
            fold = bind_case_fold(fold)
            call user_error(status, "No cross-reference named "//footnotename)
            return
        end if

        call info_extract_menu_node_name(target, multi_line=.true.)
        call goto_char(opoint)
        fold = bind_case_fold(fold)
        target = canonicalize_whitespace(target)
        call info_goto_node(target, status)
    end subroutine info_follow_reference

    recursive subroutine cmd_info_follow_reference(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: completions(:)
        character(len=:), allocatable :: str, default, alt_default, input
        integer(int64) :: start_point, bol, eol, opoint
        logical :: fold, saved_ignore, has_default, has_alt

        status = status_type()
        fold = bind_case_fold(.true.)
        allocate (completions(0))
        has_default = .false.
        has_alt = .false.
        start_point = point()
        opoint = point()
        ! Store end and beginning of line.
        eol = line_end_position(1_int64)
        bol = line_beginning_position(1_int64)
        call goto_min()

        do while (refwd("\*note[ "//LF//TAB//"]+\([^:]*\):"))
            str = canonicalize_whitespace(match_string_no_properties(1))

            ! See if this one should be the default.
            if (.not. has_default .and. mbeg(0) <= start_point .and. start_point <= point()) then
                has_default = .true.
                default = str
            end if

            ! See if this one should be the alternate default.
            if (.not. has_alt .and. bol <= mbeg(0) .and. point() <= eol) then
                has_alt = .true.
                alt_default = str
            end if

            ! Don't add this string if it's a duplicate.
            if (.not. has_string(completions, str, .true.)) call add_string(completions, str)
        end do

        call goto_char(opoint)
        fold = bind_case_fold(fold)

        ! If no good default was found, try an alternate.
        if (.not. has_default .and. has_alt) then
            default = alt_default
            has_default = .true.
        end if

        ! If only one cross-reference found, then make it default.
        if (size(completions) == 1) then
            default = completions(1)%s
            has_default = .true.
        end if

        if (size(completions) == 0) then
            call user_error(status, "No cross-references in this node")
            return
        end if

        saved_ignore = completion_ignore_case%value
        completion_ignore_case%value = .true.

        if (has_default) then
            call completing_read(format_prompt("Follow reference named", default), &
                                 completion_list_table(strings_value(completions)), input, status, &
                                 require_match=REQUIRE_MATCH_T)
        else
            call completing_read("Follow reference named: ", completion_list_table(strings_value(completions)), &
                                 input, status, require_match=REQUIRE_MATCH_T)
        end if

        completion_ignore_case%value = saved_ignore
        if (.not. status%ok()) return

        if (len(input) == 0) then
            if (.not. has_default) then
                call plain_error(status, "No reference was specified")
                return
            end if

            input = default
        end if

        call info_follow_reference(input, status)
    end subroutine cmd_info_follow_reference

    ! (format-prompt PROMPT DEFAULT): "PROMPT (default DEFAULT): ".
    ! Not a port.
    function format_prompt(prompt, default) result(p)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: p

        p = prompt//" (default "//default//"): "
    end function format_prompt

    !> The node name of the menu entry or cross reference after point
    !> (after skipping white space): its node part, or its name for an
    !> entry `NAME::'.  MULTI_LINE allows the node name to span lines (in
    !> cross references); in an INDEX_NODE, the entry's line number or
    !> name becomes the place to go to in the node.  Port of
    !> info.el:Info-extract-menu-node-name.
    subroutine info_extract_menu_node_name(name, multi_line, index_node)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(in), optional :: multi_line, index_node
        character(len=:), allocatable :: allowed, re, digits
        type(status_type) :: st
        integer(int64) :: moved
        logical :: multi, idx

        multi = .false.
        if (present(multi_line)) multi = multi_line
        idx = .false.
        if (present(index_node)) idx = index_node
        moved = skip_chars_forward(" "//TAB//LF, st)

        if (idx) then
            allowed = "^,"//TAB//LF
        else if (multi) then
            allowed = "^.,"//TAB
        else
            allowed = "^.,"//TAB//LF
        end if

        re = INFO_MENU_ENTRY_NAME_RE//":\(:\|"//following_node_name_re(allowed)//"\)"
        if (idx) re = re//"\.\(?:["//" "//TAB//LF//"]+(line +\([0-9]+\))\)?"
        name = ""
        if (.not. looking(re)) return

        if (idx) then
            if (mbeg(5) > 0) then
                point_loc_kind = LOC_LINE
                digits = match_string_no_properties(5)
                read (digits, *) point_loc_line
            else
                point_loc_kind = LOC_NAME
                point_loc_name = text(mbeg(0), mbeg(1) - 1)
            end if
        end if

        if (len(match_string_no_properties(2)) > 0) then
            name = canonicalize_whitespace(match_string_no_properties(2))
        else
            ! If the node name is the menu entry name (using `entry::').
            name = canonicalize_whitespace(text(mbeg(0), mbeg(1) - 1))
        end if
    end subroutine info_extract_menu_node_name

    ! Complete STRING among the menu items of the node of
    ! `complete_menu_buffer'.  Port of info.el:Info-complete-menu-item
    ! (`Info-complete-next-re' and `Info-complete-nodes' come with the
    ! index commands, which use them).
    recursive subroutine complete_menu_item(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(string_type), allocatable :: completions(:)
        character(len=:), allocatable :: pattern, orignode
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: opoint
        integer :: k, j
        logical :: fold, cached

        status = status_type()
        result = nil()
        if (present(suffix)) continue
        ! Note that `Info-complete-menu-buffer' could be current already,
        ! so we want to save point.
        old = current_buffer()
        call set_buffer(complete_menu_buffer, st)
        opoint = point()
        fold = bind_case_fold(.true.)
        call goto_min()

        if (.not. sfwd(LF//"* Menu:")) then
            call user_error(status, "No menu in this node")
        else
            select case (action)
            case (COMPLETE_BOUNDARIES)
                result = nil()
            case (COMPLETE_TEST)
                if (refwd(LF//"\* +"//regexp_quote(string)//":")) result = t_value()
            case default
                pattern = LF//"\* +\("//regexp_quote(string)//INFO_MENU_ENTRY_NAME_RE//"\):"//node_spec_re()
                k = here()
                ! Check the cache.
                cached = .false.

                if (locals(k)%has_complete_cache) then
                    cached = locals(k)%cache_file == current_file_or_empty() .and. &
                             locals(k)%cache_node == current_node_or_empty()
                    if (cached) cached = string_prefix_ignore_case(locals(k)%cache_string, string)
                end if

                if (cached) cached = locals(k)%cache_nodes == nodes_key()

                if (cached) then
                    ! We can reuse the previous list.
                    completions = locals(k)%cache_completions
                else
                    ! The cache can't be used.
                    allocate (completions(0))
                    orignode = current_node_or_empty()
                    j = 1

                    do
                        do while (refwd(pattern))
                            if (.not. has_string(completions, match_string_no_properties(1))) then
                                call add_string(completions, match_string_no_properties(1))
                            end if
                        end do

                        ! Check subsequent nodes if applicable.
                        if (.not. allocated(complete_nodes)) exit
                        j = j + 1
                        if (j > size(complete_nodes)) exit
                        call info_goto_node(complete_nodes(j)%s, st)
                        if (.not. st%ok()) exit
                        call goto_min()
                    end do

                    ! Go back to the start node (for the next completion).
                    if (current_node_or_empty() /= orignode) call info_goto_node(orignode, st)
                    k = here()
                    locals(k)%cache_nodes = nodes_key()
                    ! Update the cache.
                    locals(k)%has_complete_cache = .true.
                    locals(k)%cache_file = current_file_or_empty()
                    locals(k)%cache_node = current_node_or_empty()
                    locals(k)%cache_string = string
                    locals(k)%cache_completions = completions
                end if

                call complete_with_action(action, completions, string, result, status, pred)
            end select
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)
        call set_buffer(old, st)

    contains

        ! Info-complete-nodes as one string, for the cache.
        function nodes_key() result(key)
            character(len=:), allocatable :: key
            integer :: m

            key = ""
            if (.not. allocated(complete_nodes)) return

            do m = 1, size(complete_nodes)
                key = key//complete_nodes(m)%s//achar(0)
            end do
        end function nodes_key
    end subroutine complete_menu_item

    ! Whether PREFIX is a prefix of S, ignoring case ((string-prefix-p
    ! PREFIX S t)).  Not a port.
    logical function string_prefix_ignore_case(prefix, s)
        character(len=*), intent(in) :: prefix, s

        string_prefix_ignore_case = .false.
        if (len(prefix) > len(s)) return
        string_prefix_ignore_case = lower(prefix) == lower(s(1:len(prefix)))
    end function string_prefix_ignore_case

    ! Regexp to match the text after a : until the terminating `.'.
    ! Port of info.el:Info-node-spec-re.
    function node_spec_re() result(re)
        character(len=:), allocatable :: re

        re = following_node_name_re("^.,:")//"[,:.]"
    end function node_spec_re

    !> Go to the node pointed to by the menu item named (or abbreviated)
    !> MENU_ITEM.  The menu item should one of those listed in the current
    !> node's menu.  Completion is allowed, and the default menu item is
    !> the one point is on.  (FORK, showing the node in a new Info buffer,
    !> is Deferred with `clone_buffer'.)  Port of info.el:Info-menu.
    recursive subroutine info_menu(menu_item, status)
        character(len=*), intent(in) :: menu_item
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node

        ! there is a problem here in that if several menu items have the
        ! same name you can only go to the node of the first with this
        ! command.
        call extract_menu_item(menu_item, node, status)
        if (.not. status%ok()) return
        call info_goto_node(node, status)
    end subroutine info_menu

    recursive subroutine cmd_info_menu(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, item
        integer(int64) :: p, beg, opoint
        logical :: fold, has_default, saved_ignore

        status = status_type()
        ! If point is within a menu item, use that item as the default
        has_default = .false.
        p = point()
        opoint = point()
        fold = bind_case_fold(.true.)
        call goto_min()

        if (.not. sfwd(LF//"* menu:")) then
            call goto_char(opoint)
            fold = bind_case_fold(fold)
            call user_error(status, "No menu in this node")
            return
        end if

        beg = point()

        if (point() < p) then
            call goto_char(p)
            call end_of_line(1_int64)

            if (rebwd(LF//"\* +\("//INFO_MENU_ENTRY_NAME_RE//"\):", beg)) then
                default = match_string_no_properties(1)
                has_default = .true.
            end if
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)
        complete_menu_buffer = current_buffer()
        saved_ignore = completion_ignore_case%value

        do
            completion_ignore_case%value = .true.

            if (has_default) then
                call completing_read(format_prompt("Menu item", default), completion_function_table(complete_menu_item), &
                                     item, status, require_match=REQUIRE_MATCH_T, default=default)
            else
                call completing_read("Menu item: ", completion_function_table(complete_menu_item), item, status, &
                                     require_match=REQUIRE_MATCH_T)
            end if

            completion_ignore_case%value = saved_ignore
            if (.not. status%ok()) return
            if (len(item) > 0) exit
        end do

        call info_menu(item, status)
    end subroutine cmd_info_menu

    ! The node of the menu item MENU_ITEM of the current node.  Port of
    ! info.el:Info-extract-menu-item.
    recursive subroutine extract_menu_item(menu_item, node, status)
        character(len=*), intent(in) :: menu_item
        character(len=:), allocatable, intent(out) :: node
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: item
        integer(int64) :: opoint
        logical :: fold, is_index

        status = status_type()
        node = ""
        item = regexp_quote(menu_item)
        fold = bind_case_fold(.true.)
        opoint = point()
        call goto_min()

        if (.not. sfwd(LF//"* menu:")) then
            call user_error(status, "No menu in this node")
        else if (.not. (refwd(LF//"\* +"//item//":") .or. refwd(LF//"\* +"//item))) then
            call user_error(status, "No such item in menu")
        else
            call beginning_of_line(1_int64)
            call goto_char(point() + 2)
            is_index = info_index_node()
            call info_extract_menu_node_name(node, index_node=is_index)
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)
    end subroutine extract_menu_item

    ! The node of menu item COUNT of the current node (the last item if
    ! COUNT is 0).  With NO_DETAIL, don't look in the detailed node
    ! listing.  Port of info.el:Info-extract-menu-counting.
    recursive subroutine extract_menu_counting(count, node, status, no_detail)
        integer, intent(in) :: count
        character(len=:), allocatable, intent(out) :: node
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_detail
        integer(int64) :: opoint, bound, found
        logical :: fold, is_index

        status = status_type()
        node = ""
        fold = bind_case_fold(.true.)
        opoint = point()
        bound = point_max()

        if (present(no_detail)) then
            if (no_detail) then
                if (refwd("^[ "//TAB//"-]*The Detailed Node Listing")) then
                    bound = mbeg(0)
                end if
            end if
        end if

        call goto_min()

        if (.not. sfwd(LF//"* menu:", bound)) then
            call user_error(status, "No menu in this node")
        else
            if (count > 0) then
                call search_forward(LF//"* ", found, status, bound=bound, noerror=SEARCH_NOERROR, count=int(count, int64))

                if (found == 0) then
                    status = status_type()
                    call plain_error(status, "Too few items in menu")
                end if
            else
                do while (sfwd(LF//"* ", bound))
                end do
            end if

            if (status%ok()) then
                is_index = info_index_node()
                call info_extract_menu_node_name(node, index_node=is_index)
            end if
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)
    end subroutine extract_menu_counting

    !> Go to the node of the Nth menu item.  N is the digit argument used
    !> to invoke this command.  Port of info.el:Info-nth-menu-item.
    recursive subroutine info_nth_menu_item(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node

        call extract_menu_counting(n, node, status)
        if (.not. status%ok()) return
        call info_goto_node(node, status)
    end subroutine info_nth_menu_item

    recursive subroutine cmd_info_nth_menu_item(status)
        type(status_type), intent(out) :: status
        integer :: key

        key = last_command_char()
        call info_nth_menu_item(key - iachar("0"), status)
    end subroutine cmd_info_nth_menu_item

    ! The last character of the keys that invoked the command
    ! ((aref (this-command-keys) (1- (length (this-command-keys))))).
    ! Not a port.
    integer function last_command_char() result(c)
        integer :: n

        c = 0
        if (.not. allocated(this_command_keys)) return
        n = size(this_command_keys)

        if (n > 0) then
            if (fixnump(this_command_keys(n))) c = int(xfixnum(this_command_keys(n)))
        end if
    end function last_command_char

    !> Go to the Top node of this file.  Port of info.el:Info-top-node.
    recursive subroutine info_top_node(status)
        type(status_type), intent(out) :: status

        call info_goto_node("Top", status)
    end subroutine info_top_node

    !> Go to the final node in this file.  Port of info.el:Info-final-node.
    recursive subroutine info_final_node(status)
        type(status_type), intent(out) :: status
        type(info_history_entry_type), allocatable :: saved(:)
        character(len=:), allocatable :: node
        logical :: fold, moved
        integer :: k

        call info_goto_node("Top", status)
        if (.not. status%ok()) return
        ! (let ((Info-history nil) ...): the nodes visited on the way are
        ! not recorded.
        k = here()
        saved = locals(k)%history
        deallocate (locals(k)%history)
        allocate (locals(k)%history(0))
        fold = bind_case_fold(.true.)
        ! Go to the last node in the menu of Top.  But don't delve into
        ! detailed node listings.
        call extract_menu_counting(0, node, status, .true.)
        if (status%ok()) call info_goto_node(node, status)

        if (status%ok()) then
            ! If the last node in the menu is not last in pointer
            ! structure, move forward (but not down- or upward - see
            ! bug#1116) until we can't go any farther.
            do
                call info_forward_node(status, .true., .true., .true., moved)
                if (.not. status%ok() .or. .not. moved) exit
            end do
        end if

        if (status%ok()) then
            ! Then keep moving down to last subnode, unless we reach an
            ! index.
            do while (.not. info_index_node() .and. menu_after_point())
                call extract_menu_counting(0, node, status)
                if (status%ok()) call info_goto_node(node, status)
                if (.not. status%ok()) exit
            end do
        end if

        fold = bind_case_fold(fold)
        k = here()
        locals(k)%history = saved
    end subroutine info_final_node

    ! (save-excursion (search-forward "\n* Menu:" nil t)).  Not a port.
    logical function menu_after_point()
        integer(int64) :: opoint

        opoint = point()
        menu_after_point = sfwd(LF//"* Menu:")
        call goto_char(opoint)
    end function menu_after_point

    !> Go forward one node, considering all nodes as forming one sequence.
    !> Interactively, if the current node has sub-nodes, descend into the
    !> first sub-node; otherwise go to the "next" node, if it exists, else
    !> go "up" to the parent node.  When called from Fortran, NOT_DOWN
    !> true means don't descend into sub-nodes, NOT_UP true means don't go
    !> to parent nodes, and NO_ERROR true means don't signal a user-error
    !> if there's no node to go to (MOVED says whether it went to one).
    !> Port of info.el:Info-forward-node.
    recursive subroutine info_forward_node(status, not_down, not_up, no_error, moved)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_down, not_up, no_error
        logical, intent(out), optional :: moved
        character(len=:), allocatable :: node, up, old_node
        type(info_history_entry_type), allocatable :: old_history(:)
        integer(int64) :: opoint
        logical :: down, upward, quiet, fold, has_menu, has_next, has_up, success
        type(status_type) :: st
        integer :: k

        status = status_type()
        down = .true.
        if (present(not_down)) down = .not. not_down
        upward = .true.
        if (present(not_up)) upward = .not. not_up
        quiet = .false.
        if (present(no_error)) quiet = no_error
        if (present(moved)) moved = .true.
        call goto_min()
        call fwd_line(1_int64)
        fold = bind_case_fold(.true.)
        ! three possibilities, in order of priority:
        !     1. next node is in a menu in this node (but not in an index)
        !     2. next node is next at same level
        !     3. next node is up and next
        has_menu = .false.
        if (down) has_menu = menu_after_point()
        if (has_menu) has_menu = .not. info_index_node()
        opoint = point()
        has_next = sbwd("next:")
        call goto_char(opoint)
        has_up = .false.

        if (upward .and. .not. (has_menu .or. has_next)) then
            has_up = sbwd("up:")
            call goto_char(opoint)

            if (has_up) then
                call info_extract_pointer("up", up, status)

                if (.not. status%ok()) then
                    fold = bind_case_fold(fold)
                    return
                end if

                ! Use string-equal, not equal, to ignore text props.
                has_up = lower(up) /= "top"
            end if
        end if

        if (has_menu) then
            call extract_menu_counting(1, node, status)
            if (status%ok()) call info_goto_node(node, status)
        else if (has_next) then
            call info_next(status)
        else if (has_up) then
            old_node = current_node_or_empty()
            call info_up(status)

            if (status%ok()) then
                k = here()
                old_history = locals(k)%history
                call info_forward_node(status, .true., .false., quiet, success)
                if (.not. status%ok()) success = .false.
                if (.not. success) call info_goto_node(old_node, st)

                if (info_history_skip_intermediate_nodes%value) then
                    k = here()
                    locals(k)%history = old_history
                end if

                if (present(moved)) moved = success
            end if
        else if (quiet) then
            if (present(moved)) moved = .false.
        else
            call user_error(status, "No pointer forward from this node")
        end if

        fold = bind_case_fold(fold)
    end subroutine info_forward_node

    !> Go backward one node, considering all nodes as forming one sequence.
    !> If the current node has a "previous" node, go to it, descending
    !> into its last sub-node, if any; otherwise go "up" to the parent
    !> node.  Port of info.el:Info-backward-node.
    recursive subroutine info_backward_node(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prevnode, upnode, node
        type(info_history_entry_type), allocatable :: old_history(:)
        logical :: fold
        integer :: k

        call info_extract_pointer("prev[ious]*", prevnode, status, noerror=.true.)
        call info_extract_pointer("up", upnode, status, noerror=.true.)
        fold = bind_case_fold(.true.)

        if (allocated(upnode) .and. index(upnode_or_empty(), "(") > 0) then
            call user_error(status, "First node in file")
        else if (allocated(upnode) .and. (.not. allocated(prevnode) .or. &
                                          lower(prevnode_or_empty()) == lower(upnode_or_empty()))) then
            call info_up(status)
        else if (allocated(prevnode)) then
            ! If we move back at the same level, go down to find the last
            ! subnode*.
            call info_prev(status)

            if (status%ok()) then
                k = here()
                old_history = locals(k)%history

                do while (.not. info_index_node() .and. menu_after_point())
                    call extract_menu_counting(0, node, status)
                    if (status%ok()) call info_goto_node(node, status)
                    if (.not. status%ok()) exit
                end do

                if (info_history_skip_intermediate_nodes%value) then
                    k = here()
                    locals(k)%history = old_history
                end if
            end if
        else
            call user_error(status, "No pointer backward from this node")
        end if

        fold = bind_case_fold(fold)

    contains

        function upnode_or_empty() result(s)
            character(len=:), allocatable :: s

            s = ""
            if (allocated(upnode)) s = upnode
        end function upnode_or_empty

        function prevnode_or_empty() result(s)
            character(len=:), allocatable :: s

            s = ""
            if (allocated(prevnode)) s = prevnode
        end function prevnode_or_empty
    end subroutine info_backward_node

    !> Go to the node of the next menu item.  Port of
    !> info.el:Info-next-menu-item.
    recursive subroutine info_next_menu_item(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node
        integer(int64) :: opoint
        logical :: fold, found, ok

        status = status_type()
        ! Bind this in case the user sets it to nil.
        fold = bind_case_fold(.true.)
        opoint = point()
        call fwd_line(-1_int64)
        ok = sfwd(LF//"* menu:")
        found = sfwd(LF//"* ")
        if (found) call info_extract_menu_node_name(node)
        call goto_char(opoint)
        fold = bind_case_fold(fold)

        if (found) then
            call info_goto_node(node, status)
        else
            call user_error(status, "No more items in menu")
        end if
    end subroutine info_next_menu_item

    !> Go to the node of the previous menu item.  Port of
    !> info.el:Info-last-menu-item.
    recursive subroutine info_last_menu_item(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node
        integer(int64) :: opoint, beg, after
        logical :: fold, has_beg, found

        status = status_type()
        opoint = point()
        call fwd_line(1_int64)
        ! Bind this in case the user sets it to nil.
        fold = bind_case_fold(.true.)
        after = point()
        has_beg = sbwd(LF//"* menu:")
        beg = point()
        call goto_char(after)
        found = .false.
        if (has_beg) found = sbwd(LF//"* ", beg)

        if (.not. found) then
            call goto_char(opoint)
            fold = bind_case_fold(fold)
            call user_error(status, "No previous items in menu")
            return
        end if

        call goto_char(mend(0))
        call info_extract_menu_node_name(node)
        call goto_char(opoint)
        fold = bind_case_fold(fold)
        call info_goto_node(node, status)
    end subroutine info_last_menu_item

    !> Go to the next subnode or the next node, or go up a level.  Port of
    !> info.el:Info-next-preorder.
    recursive subroutine info_next_preorder(status)
        type(status_type), intent(out) :: status
        type(info_history_entry_type), allocatable :: old_history(:)
        integer :: k

        call info_next_menu_item(status)
        if (status%ok()) return
        call info_next(status)
        if (status%ok()) return
        call info_up(status, .true.)

        if (status%ok()) then
            ! Since we have already gone thru all the items in this menu,
            ! go up to the end of this node.
            call goto_char(point_max())
            ! Since logically we are done with the node with that menu,
            ! move on from it.  But don't add intermediate nodes to the
            ! history on recursive calls.
            k = here()
            old_history = locals(k)%history
            call info_next_preorder(status)

            if (info_history_skip_intermediate_nodes%value) then
                k = here()
                locals(k)%history = old_history
            end if

            return
        end if

        status = status_type()
        call user_error(status, "No more nodes")
    end subroutine info_next_preorder

    !> Go to the last node, popping up a level if there is none.  Port of
    !> info.el:Info-last-preorder.
    recursive subroutine info_last_preorder(status)
        type(status_type), intent(out) :: status
        type(info_history_entry_type), allocatable :: old_history(:)
        character(len=:), allocatable :: prev, up
        type(status_type) :: st
        logical :: fold
        integer :: k

        status = status_type()

        if (info_scroll_prefer_subnodes%value) then
            call info_last_menu_item(st)

            if (st%ok()) then
                ! If we go down a menu item, go to the end of the node so
                ! we can scroll back through it.
                call goto_char(point_max())
                ! Keep going down, as long as there are nested menu nodes.
                k = here()
                old_history = locals(k)%history

                do
                    call info_last_menu_item(st)
                    if (.not. st%ok()) exit
                    call goto_char(point_max())
                end do

                if (info_history_skip_intermediate_nodes%value) then
                    k = here()
                    locals(k)%history = old_history
                end if

                call recenter_last_line()
                return
            end if
        end if

        call info_extract_pointer("prev", prev, st)

        if (st%ok()) then
            call info_extract_pointer("up", up, st)
            if (.not. st%ok()) up = achar(0)

            if (up /= prev) then
                call info_prev(st)
                call goto_char(point_max())
                k = here()
                old_history = locals(k)%history

                do
                    call info_last_menu_item(st)
                    if (.not. st%ok()) exit
                    ! If we go down a menu item, go to the end of the node
                    ! so we can scroll back through it.
                    call goto_char(point_max())
                end do

                if (info_history_skip_intermediate_nodes%value) then
                    k = here()
                    locals(k)%history = old_history
                end if

                call recenter_last_line()
                return
            end if
        end if

        call info_up(st, .true.)

        if (st%ok()) then
            call goto_min()
            fold = bind_case_fold(.true.)
            if (.not. sfwd(LF//"* Menu:")) call goto_char(point_max())
            fold = bind_case_fold(fold)
            return
        end if

        call user_error(status, "No previous nodes")
    end subroutine info_last_preorder

    ! (recenter -1).  Not a port.
    subroutine recenter_last_line()
        type(status_type) :: st

        call recenter(make_fixnum(-1_int64), .false., st)
    end subroutine recenter_last_line

    !> Scroll one screenful forward in Info, considering all nodes as one
    !> sequence.  Once you scroll far enough in a node that its menu
    !> appears on the screen but after point, the next scroll moves into
    !> its first subnode, unless `info_scroll_prefer_subnodes' is false.
    !>
    !> When you scroll past the end of a node, that goes to the next node
    !> if `info_scroll_prefer_subnodes' is true and to the first subnode
    !> otherwise; if this node has no successor, it moves to the parent
    !> node's successor, and so on.  If `info_scroll_prefer_subnodes' is
    !> true and point is inside the menu of a node, it moves to subnode
    !> indicated by the following menu item.  (That case won't normally
    !> result from this command, but can happen in other ways.)  Port of
    !> info.el:Info-scroll-up.
    recursive subroutine info_scroll_up(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node
        type(status_type) :: st
        integer(int64) :: virtual_end, opoint, start
        logical :: fold

        status = status_type()
        start = sel_window_start()

        if (start < point_min() .or. start > point_max()) then
            call set_sel_window_start(point(), status)
            start = point()
        end if

        fold = bind_case_fold(.true.)
        opoint = point()
        call goto_min()

        if (info_scroll_prefer_subnodes%value .and. sfwd(LF//"* Menu:")) then
            virtual_end = point()
        else
            virtual_end = point_max()
        end if

        call goto_char(opoint)
        fold = bind_case_fold(fold)

        ! hazard-ok(short-circuit): visible() takes any position.
        if (virtual_end < start .or. visible(virtual_end)) then
            if (info_scroll_prefer_subnodes%value) then
                call info_next_preorder(status)
            else
                call extract_menu_counting(1, node, st)
                if (st%ok()) call info_goto_node(node, st)
                if (.not. st%ok()) call info_next_preorder(status)
            end if
        else
            call scroll_up(nil(), status)
        end if
    end subroutine info_scroll_up

    !> Scroll one screenful back in Info, considering all nodes as one
    !> sequence.  If point is within the menu of a node, and
    !> `info_scroll_prefer_subnodes' is true, this goes to its last
    !> subnode.  When you scroll past the beginning of a node, that goes to
    !> the previous node or back up to the parent node.  Port of
    !> info.el:Info-scroll-down.
    recursive subroutine info_scroll_down(status)
        type(status_type), intent(out) :: status
        integer(int64) :: current_point, opoint, start
        logical :: fold, virtual_end

        status = status_type()
        start = sel_window_start()

        if (start < point_min() .or. start > point_max()) then
            call set_sel_window_start(point(), status)
        end if

        fold = bind_case_fold(.true.)
        virtual_end = .false.

        if (info_scroll_prefer_subnodes%value) then
            opoint = point()
            current_point = line_beginning_position(1_int64)
            call goto_min()
            virtual_end = sfwd(LF//"* Menu:", current_point)
            call goto_char(opoint)
        end if

        fold = bind_case_fold(fold)

        if (virtual_end .or. visible(point_min())) then
            call info_last_preorder(status)
        else
            call scroll_down(nil(), status)
        end if
    end subroutine info_scroll_down

    ! Move point to the next pattern-based cross-reference or
    ! property-based link.  The next cross-reference is searched using
    ! the regexp PAT, and the next link is searched using the text
    ! property PROP.  Move point to the closest found position of either
    ! a cross-reference found by `re-search-forward' or a link found by
    ! `next-single-char-property-change'.  Return true if point moved.
    ! Port of info.el:Info-next-reference-or-link.
    logical function next_reference_or_link(pat, prop) result(moved)
        character(len=*), intent(in) :: pat, prop
        type(value_type) :: v
        integer(int64) :: opoint, pxref, plink, mb
        logical :: has_xref

        opoint = point()
        has_xref = refwd(pat)
        pxref = point()
        mb = mbeg(1)
        if (mb == 0) mb = mbeg(0)
        call goto_char(opoint)
        plink = next_single_char_property_change(point(), sym(prop))

        if (plink < point_max()) then
            v = get_char_property(plink, sym(prop))
            if (nilp(v)) plink = next_single_char_property_change(plink, sym(prop))
        end if

        moved = .true.

        if (plink < point_max()) then
            if (has_xref .and. pxref <= plink) then
                call goto_char(mb)
            else
                call goto_char(plink)
            end if
        else if (has_xref) then
            call goto_char(mb)
        else
            moved = .false.
        end if
    end function next_reference_or_link

    ! Move point to the previous pattern-based cross-reference or
    ! property-based link.  Port of info.el:Info-prev-reference-or-link.
    logical function prev_reference_or_link(pat, prop) result(moved)
        character(len=*), intent(in) :: pat, prop
        type(value_type) :: v
        integer(int64) :: opoint, pxref, plink, mb
        logical :: has_xref

        opoint = point()
        has_xref = rebwd(pat)
        pxref = point()
        mb = mbeg(1)
        if (mb == 0) mb = mbeg(0)
        call goto_char(opoint)
        plink = previous_single_char_property_change(point(), sym(prop))

        if (plink > point_min()) then
            v = get_char_property(plink, sym(prop))
            if (nilp(v)) plink = previous_single_char_property_change(plink, sym(prop))
        end if

        moved = .true.

        if (plink > point_min()) then
            if (has_xref .and. pxref >= plink) then
                call goto_char(mb)
            else
                call goto_char(plink)
            end if
        else if (has_xref) then
            call goto_char(mb)
        else
            moved = .false.
        end if
    end function prev_reference_or_link

    !> Move cursor to the next cross-reference or menu item in the node.
    !> If COUNT is present (interactively with a prefix arg), jump over
    !> COUNT cross-references.  Port of info.el:Info-next-reference.
    recursive subroutine info_next_reference(status, recur, count)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recur
        integer(int64), intent(in), optional :: count
        character(len=*), parameter :: pat = "\*note[ "//LF//TAB//"]+\([^:]+\):\|^\* .*:\|[hf]t?tps?://"
        integer(int64) :: n, old_pt
        logical :: rec, fold

        status = status_type()
        rec = .false.
        if (present(recur)) rec = recur
        n = 1
        if (present(count)) n = count

        if (n < 0) then
            call info_prev_reference(status, rec, -n)
            return
        end if

        fold = bind_case_fold(.true.)

        do while (n > 0)
            n = n - 1
            old_pt = point()
            if (.not. eobp()) call goto_char(point() + 1)

            if (.not. next_reference_or_link(pat, "link")) then
                call goto_min()

                if (.not. next_reference_or_link(pat, "link")) then
                    call goto_char(old_pt)
                    call user_error(status, "No cross references in this node")
                    exit
                end if
            end if

            if (looking("\* Menu:")) then
                if (rec) then
                    call user_error(status, "No cross references in this node")
                    exit
                end if

                call info_next_reference(status, .true.)
                if (.not. status%ok()) exit
            else if (looking("^\* ")) then
                call goto_char(point() + 2)
            end if
        end do

        fold = bind_case_fold(fold)
    end subroutine info_next_reference

    !> Move cursor to the previous cross-reference or menu item in the
    !> node.  If COUNT is present (interactively with a prefix arg), jump
    !> over COUNT cross-references.  Port of info.el:Info-prev-reference.
    recursive subroutine info_prev_reference(status, recur, count)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recur
        integer(int64), intent(in), optional :: count
        character(len=*), parameter :: pat = "\*note[ "//LF//TAB//"]+\([^:]+\):\|^\* .*:\|[hf]t?tps?://"
        integer(int64) :: n, old_pt
        logical :: rec, fold

        status = status_type()
        rec = .false.
        if (present(recur)) rec = recur
        n = 1
        if (present(count)) n = count

        if (n < 0) then
            call info_next_reference(status, rec, -n)
            return
        end if

        fold = bind_case_fold(.true.)

        do while (n > 0)
            n = n - 1
            old_pt = point()

            if (.not. prev_reference_or_link(pat, "link")) then
                call goto_char(point_max())

                if (.not. prev_reference_or_link(pat, "link")) then
                    call goto_char(old_pt)
                    call user_error(status, "No cross references in this node")
                    exit
                end if
            end if

            if (looking("\* Menu:")) then
                if (rec) then
                    call user_error(status, "No cross references in this node")
                    exit
                end if

                call info_prev_reference(status, .true.)
                if (.not. status%ok()) exit
            else if (looking("^\* ")) then
                call goto_char(point() + 2)
            end if
        end do

        fold = bind_case_fold(fold)
    end subroutine info_prev_reference

    recursive subroutine cmd_info_next_reference(status)
        type(status_type), intent(out) :: status

        call info_next_reference(status, count=prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_info_next_reference

    recursive subroutine cmd_info_prev_reference(status)
        type(status_type), intent(out) :: status

        call info_prev_reference(status, count=prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_info_prev_reference

    ! ------------------------------------------------------------------
    ! Index nodes
    ! ------------------------------------------------------------------

    !> Return in NODES the names of all index nodes in Info FILE (the
    !> current Info file if absent).
    !> First look in a list of cached index node names.  Then scan Info
    !> file and its subfiles for nodes with the index cookie.  Then try to
    !> find index nodes starting from the first node in the top level menu
    !> whose name contains the word "Index", plus any immediately following
    !> nodes whose names also contain the word "Index".  Port of
    !> info.el:Info-index-nodes.
    recursive subroutine info_index_nodes(nodes, file)
        type(string_type), allocatable, intent(out) :: nodes(:)
        character(len=*), intent(in), optional :: file
        character(len=:), allocatable :: f, main_file, found, node
        type(string_type), allocatable :: subfiles(:)
        type(index_nodes_type), allocatable :: grown(:)
        type(handle_type) :: old, temp
        type(status_type) :: st
        integer(int64) :: opoint, bound
        integer :: i, saved_kind
        logical :: fold, has_main

        if (present(file)) then
            f = file
        else
            f = current_file_or_empty()
        end if

        allocate (nodes(0))
        if (.not. allocated(index_nodes_cache)) allocate (index_nodes_cache(0))

        do i = 1, size(index_nodes_cache)
            if (index_nodes_cache(i)%file == f) then
                nodes = index_nodes_cache(i)%nodes
                return
            end if
        end do

        ! Skip virtual Info files
        if (info_virtual_file_p(f)) then
            call cache(f)
            return
        end if

        old = current_buffer()
        saved_kind = point_loc_kind

        if (file_supports_index_cookies_of(f)) then
            ! Find nodes with index cookie
            call info_find_file(f, found, st)
            if (.not. allocated(found)) found = f
            call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st, inhibit_buffer_hooks=.true.)
            call set_buffer(temp, st)
            buffers(cur)%directory = file_name_directory(found)
            main_file = found
            has_main = .true.
            allocate (subfiles(0))

            do while (has_main .or. size(subfiles) > 0)
                call erase_buffer(st)

                if (has_main) then
                    call info_insert_file_contents(main_file, st)
                else
                    call info_insert_file_contents(subfiles(1)%s, st)
                end if

                if (.not. st%ok()) exit
                call goto_min()

                do while (sfwd(NUL//BS//"[index"//NUL//BS//"]", move=.true.))
                    opoint = point()

                    if (rebwd("^"//US)) then
                        if (sfwd("Node: ")) call add_string(nodes, following_node_name())
                    end if

                    call goto_char(opoint)
                end do

                if (has_main) then
                    call goto_min()

                    if (sfwd(LF//US//LF//"Indirect:")) then
                        opoint = point()
                        bound = point_max()
                        if (sfwd(LF//US)) bound = point()
                        call goto_char(opoint)

                        do while (refwd("^\(.*\): [0-9]+$", bound))
                            call add_string(subfiles, match_string_no_properties(1))
                        end do
                    end if

                    has_main = .false.
                else
                    subfiles = subfiles(2:)
                end if
            end do

            call set_buffer(old, st)
            call kill_buffer(temp, st)
        else
            ! Else find nodes with the word "Index" in the node name
            fold = bind_case_fold(.true.)
            call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st, inhibit_buffer_hooks=.true.)
            call set_buffer(temp, st)
            call info_mode(st)
            if (st%ok()) call info_find_node(f, "Top", st)

            if (st%ok()) then
                if (sfwd(LF//"* menu:")) then
                    if (refwd(LF//"\* \(.*\<Index\>\)")) then
                        call goto_char(mbeg(1))
                        call info_extract_menu_node_name(node)
                        call add_string(nodes, node)
                        call info_goto_node(node, st)

                        do while (st%ok())
                            call info_extract_pointer("next", node, st, noerror=.true.)
                            if (.not. allocated(node)) exit
                            if (.not. smatch("\<Index\>", node)) exit
                            call add_string(nodes, node)
                            call info_goto_node(node, st)
                        end do
                    end if
                end if
            end if

            call info_kill_buffer(st)
            call set_buffer(old, st)
            call kill_buffer(temp, st)
            fold = bind_case_fold(fold)
        end if

        point_loc_kind = saved_kind
        ! If file has no index nodes, still add it to the cache
        call cache(f)

    contains

        ! (setq Info-index-nodes (cons (cons FILE NODES) Info-index-nodes)).
        subroutine cache(name)
            character(len=*), intent(in) :: name
            integer :: m

            allocate (grown(size(index_nodes_cache) + 1))
            grown(1)%file = name
            grown(1)%nodes = nodes

            do m = 1, size(index_nodes_cache)
                grown(m + 1) = index_nodes_cache(m)
            end do

            call move_alloc(grown, index_nodes_cache)
        end subroutine cache
    end subroutine info_index_nodes

    ! Whether FILE supports index cookies, reading its beginning if it is
    ! not known yet.  Not a port.
    recursive logical function file_supports_index_cookies_of(file) result(supports)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: found
        type(handle_type) :: old, temp
        type(status_type) :: st
        integer :: i

        if (.not. allocated(cookies_cache)) allocate (cookies_cache(0))

        do i = 1, size(cookies_cache)
            if (cookies_cache(i)%file == file) then
                supports = cookies_cache(i)%supports
                return
            end if
        end do

        if (info_virtual_file_p(file)) then
            supports = file_supports_index_cookies(file)
            return
        end if

        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st, inhibit_buffer_hooks=.true.)
        call set_buffer(temp, st)
        call info_find_file(file, found, st, noerror=.true.)
        if (allocated(found)) call info_insert_file_contents(found, st)
        supports = file_supports_index_cookies(file)
        call set_buffer(old, st)
        call kill_buffer(temp, st)
    end function file_supports_index_cookies_of

    !> Return true if NODE is an index node.  If NODE is absent, check the
    !> current Info node.  If FILE is absent, check the current Info file.
    !> Port of info.el:Info-index-node.
    recursive logical function info_index_node(node, file) result(is_index)
        character(len=*), intent(in), optional :: node, file
        type(string_type), allocatable :: nodes(:)
        character(len=:), allocatable :: f, n
        integer(int64) :: opoint, from, bound
        logical :: cached
        integer :: i, k

        if (present(file)) then
            f = file
        else
            f = current_file_or_empty()
        end if

        if (present(node)) then
            n = node
        else
            n = current_node_or_empty()
        end if

        cached = .false.

        if (allocated(index_nodes_cache)) then
            do i = 1, size(index_nodes_cache)
                if (index_nodes_cache(i)%file == f) cached = .true.
            end do
        end if

        k = here()
        is_index = .false.

        if ((present(node) .and. n /= current_node_or_empty()) .or. cached) then
            if (.not. locals(k)%current_node_virtual) then
                call info_index_nodes(nodes, f)
                is_index = has_string(nodes, n)
                return
            end if
        end if

        ! Don't search all index nodes if request is only for the current
        ! node and file is not in the cache of index nodes
        if (supports_cookies(f)) then
            opoint = point()
            from = point_min()
            if (sbwd(LF//US)) from = point()
            call goto_char(opoint)
            bound = point_max()
            if (sfwd(LF//US)) bound = point()
            call goto_char(from + 2)
            is_index = sfwd(NUL//BS//"[index"//NUL//BS//"]", bound)
            call goto_char(opoint)
        else
            is_index = smatch("\<Index\>", n)
        end if

    contains

        ! (Info-file-supports-index-cookies FILE) in the current buffer.
        logical function supports_cookies(name)
            character(len=*), intent(in) :: name
            integer(int64) :: p

            p = point()
            supports_cookies = file_supports_index_cookies(name)
            call goto_char(p)
        end function supports_cookies
    end function info_index_node

    ! ------------------------------------------------------------------
    ! Help, following the nearest node and copying its name
    ! ------------------------------------------------------------------

    !> Make command be undefined in Info.  Port of info.el:Info-undefined.
    recursive subroutine info_undefined(status)
        type(status_type), intent(out) :: status

        call ding(.false., status)
    end subroutine info_undefined

    !> Enter the Info tutorial.  Port of info.el:Info-help.
    recursive subroutine info_help(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        window = selected_window()

        if (windows(window%index)%total_lines < 23) then
            call info_find_node("info", "Help-Small-Screen", status)
        else
            call info_find_node("info", "Help", status)
        end if
    end subroutine info_help

    !> Display a brief summary of all Info commands.  Port of
    !> info.el:Info-summary.
    recursive subroutine info_summary(status)
        type(status_type), intent(out) :: status
        type(window_configuration_type) :: config
        type(handle_type) :: help
        type(value_type) :: event
        character(len=:), allocatable :: doc
        type(status_type) :: st
        logical :: flag, found, have

        status = status_type()
        call current_window_configuration(config)
        call switch_to_buffer("*Help*", status)

        if (status%ok()) then
            help = current_buffer()
            buffers(cur)%read_only = .false.
            call erase_buffer(status)
            doc = ""
            if (associated(documentation_hook)) call documentation_hook("info_mode", doc, found)
            if (status%ok()) call insert(substitute_command_keys(doc), status)
            if (status%ok()) call help_mode(status)
        end if

        if (status%ok()) then
            call goto_min()

            do
                flag = .not. visible(point_max())

                if (flag) then
                    call message("Type Space to see more")
                else
                    call message("Type Space to return to Info")
                end if

                call read_event(event, have, status)
                if (.not. status%ok() .or. .not. have) exit

                if (.not. fixnump(event)) then
                    call unread_command_events([event])
                    exit
                end if

                if (xfixnum(event) /= iachar(" ")) then
                    call unread_command_events([event])
                    exit
                end if

                if (.not. flag) exit
                call scroll_up(nil(), status)
                if (.not. status%ok()) exit
            end do

            call bury_buffer(st, help)
        end if

        call set_window_configuration(config)
        call free_window_configuration(config)
    end subroutine info_summary

    ! Return the token around POS.
    ! POS must be somewhere inside the token.
    ! START is a regular expression which will match the beginning of the
    ! tokens delimited string.
    ! ALL is a regular expression with a single parenthesized subpattern
    ! which is the token to be returned.  E.g. `{(.*)}' would return any
    ! string enclosed in braces around POS.
    ! FOUND is false on no match.  Port of info.el:Info-get-token
    ! (ERRORSTRING nil).
    subroutine get_token(pos, start, all, token, found)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: start, all
        character(len=:), allocatable, intent(out) :: token
        logical, intent(out) :: found
        integer(int64) :: opoint
        logical :: fold, ok

        token = ""
        found = .false.
        fold = bind_case_fold(.true.)
        opoint = point()
        call goto_char(pos)

        ! First look for a match for START that goes across POS.
        do while (.not. bobp() .and. point() > pos - len(start))
            if (looking(start)) exit
            call goto_char(point() - 1)
        end do

        ! If we did not find one, search back for START (this finds only
        ! matches that end at or before POS).
        if (.not. looking(start)) then
            call goto_char(pos)
            ok = rebwd(start, max(point_min(), pos - 200), move=.true.)
        end if

        do while (refwd(all, min(point_max(), pos + 200), move=.true.))
            found = mbeg(0) <= pos .and. mend(0) > pos
            if (found) exit
        end do

        if (found) token = match_string_no_properties(1)
        call goto_char(opoint)
        fold = bind_case_fold(fold)
    end subroutine get_token

    !> Follow a node reference near point.
    !> If point is on a reference, follow that reference.  Otherwise, if
    !> point is in a menu item description, follow that menu item.  (FORK,
    !> showing the node in a new Info buffer, is Deferred with
    !> `clone_buffer'.)  Port of info.el:Info-follow-nearest-node.
    recursive subroutine info_follow_nearest_node(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node
        integer(int64) :: opoint
        logical :: followed, in_menu

        call try_follow_nearest_node(followed, status)
        if (followed .or. .not. status%ok()) return
        opoint = point()
        in_menu = sbwd(LF//"* menu:")
        call goto_char(opoint)

        if (in_menu) then
            call beginning_of_line(1_int64)

            do while (.not. (bobp() .or. looking("[^ "//TAB//"]\|[ "//TAB//"]*$")))
                call beginning_of_line(0_int64)
            end do

            if (looking("\* +\([^"//TAB//LF//"]*\):")) then
                call extract_menu_item(match_string_no_properties(1), node, status)
                call goto_char(opoint)
                if (.not. status%ok()) return
                call info_goto_node(node, status)
                return
            end if

            call goto_char(opoint)
        end if

        call user_error(status, "Point neither on reference nor in menu item description")
    end subroutine info_follow_nearest_node

    ! Follow a node reference near point.  FOLLOWED is true if
    ! successful.  Port of info.el:Info-try-follow-nearest-node (following
    ! URLs is Deferred with browse-url).
    recursive subroutine try_follow_nearest_node(followed, status)
        logical, intent(out) :: followed
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: node
        integer(int64) :: old_point, new_point, opoint
        logical :: found, is_index

        status = status_type()
        followed = .true.
        call get_token(point(), "\*note[ "//LF//TAB//"]+", &
                       "\*note[ "//LF//TAB//"]+\([^:]*\):\(:\|[ "//LF//TAB//"]*(\)?", node, found)

        if (found) then
            call info_follow_reference(node, status)
            return
        end if

        ! footnote
        call get_token(point(), "(", "\(([0-9]+)\)", node, found)

        if (found) then
            old_point = point()
            new_point = 0
            call goto_min()

            if (refwd("^[ "//TAB//"]*-+ Footnotes -+$")) then
                if (old_point < point()) then
                    ! Go to footnote reference
                    if (sfwd(node)) new_point = mbeg(0)
                else
                    ! Go to footnote definition
                    if (sbwd(node)) new_point = point()
                end if
            end if

            call goto_char(old_point)

            if (new_point > 0) then
                call goto_char(new_point)
                return
            end if
        end if

        ! menu item: node name
        call get_token(point(), "\* +", "\* +\([^:]*\)::", node, found)

        if (found) then
            call info_goto_node(node, status)
            return
        end if

        ! menu item: node name or index entry
        call get_token(point(), "\* +", "\* +\(.*\): ", node, found)

        if (found) then
            opoint = point()
            call beginning_of_line(1_int64)
            call goto_char(point() + 2)
            is_index = info_index_node()
            call info_extract_menu_node_name(node, index_node=is_index)
            call info_goto_node(node, status)
            return
        end if

        call get_token(point(), "Up: ", "Up: \([^,"//LF//TAB//"]*\)", node, found)

        if (found) then
            call info_goto_node(node, status)
            return
        end if

        call get_token(point(), "Next: ", "Next: \([^,"//LF//TAB//"]*\)", node, found)

        if (found) then
            call info_goto_node(node, status)
            return
        end if

        call get_token(point(), "File: ", "File: \([^,"//LF//TAB//"]*\)", node, found)

        if (found) then
            call info_goto_node("Top", status)
            return
        end if

        call get_token(point(), "Prev: ", "Prev: \([^,"//LF//TAB//"]*\)", node, found)

        if (found) then
            call info_goto_node(node, status)
            return
        end if

        followed = .false.
    end subroutine try_follow_nearest_node

    !> Put the name of the current Info node into the kill ring.
    !> The name of the Info file is prepended to the node name in
    !> parentheses.  With a zero prefix arg, put the name inside a function
    !> call to `info'.  Port of info.el:Info-copy-current-node-name.
    recursive subroutine info_copy_current_node_name(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        character(len=:), allocatable :: node, file

        status = status_type()

        if (.not. has_current_node()) then
            call user_error(status, "No current Info node")
            return
        end if

        file = info_current_file()

        if (allocated(file)) then
            node = "("//file_name_sans_extension(file_name_nondirectory(file))//") "//info_current_node()
        else
            node = ""
        end if

        if (present(arg)) then
            if (arg == 0) node = "(info """//node//""")"
        end if

        call kill_new(node)
        call message(node)
    end subroutine info_copy_current_node_name

    recursive subroutine cmd_info_copy_current_node_name(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call info_copy_current_node_name(status)
        else
            call info_copy_current_node_name(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_info_copy_current_node_name

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

    ! Search for REGEXP from point (backward if BACKWARD) up to BOUND (0
    ! for none), skipping the matches `info_isearch_filter' rejects;
    ! return the position of the match found (0 if none).  Port of
    ! info.el:Info--search-loop.
    integer(int64) function search_loop(regexp, bound, backward) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: bound
        logical, intent(in) :: backward
        character(len=:), allocatable :: saved_spaces
        type(status_type) :: st
        integer(int64) :: beg_found, pos, head
        logical :: give_up, had_spaces, ok

        if (backward) then
            ! Hide Info file header for backward search.
            pos = point()
            call goto_min()
            ok = sfwd(LF//US)
            head = point() - 1
            call goto_char(pos)
            call narrow_to_region(head, point_max(), st)
        end if

        give_up = .false.
        found = 0
        beg_found = 0
        had_spaces = allocated(search_spaces_regexp)
        if (had_spaces) saved_spaces = search_spaces_regexp

        do
            if (give_up) exit

            if (found /= 0) then
                if (info_isearch_filter(beg_found, found)) exit
            end if

            if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
            if (allocated(info_search_whitespace_regexp)) search_spaces_regexp = info_search_whitespace_regexp

            if (backward) then
                if (bound > 0) then
                    ok = rebwd(regexp, bound)
                else
                    ok = rebwd(regexp)
                end if
            else
                if (bound > 0) then
                    ok = refwd(regexp, bound)
                else
                    ok = refwd(regexp)
                end if
            end if

            if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
            if (had_spaces) search_spaces_regexp = saved_spaces

            if (ok) then
                found = point()

                if (backward) then
                    beg_found = mend(0)
                else
                    beg_found = mbeg(0)
                end if
            else
                give_up = .true.
                found = 0
            end if
        end do
    end function search_loop

    ! Test whether the current search hit is a visible useful text.
    ! Return true if the text from BEG_FOUND to FOUND is visible and is
    ! not in the header line or a tag table.  Port of
    ! info.el:Info-isearch-filter (`search_invisible' being `open').
    recursive logical function info_isearch_filter(beg_found, found) result(useful)
        integer(int64), intent(in) :: beg_found, found
        type(status_type) :: st
        integer(int64) :: opoint, r, a, b
        logical :: backward, invisible, header, tags, f1, f2

        backward = found < beg_found
        a = min(beg_found, found)
        b = max(beg_found, found)
        call text_property_not_all(a, b, sym("invisible"), nil(), r, f1, st)
        call text_property_not_all(a, b, sym("display"), nil(), r, f2, st)
        invisible = f1 .or. f2
        ! Skip node header line
        opoint = point()
        call fwd_line(-1_int64)
        header = looking(US)
        call goto_char(opoint)

        if (header) then
            if (backward) then
                call fwd_line(-1_int64)
            else
                call fwd_line(1_int64)
            end if
        end if

        ! Skip Tag Table node
        opoint = point()
        tags = .false.
        if (sbwd(US)) tags = looking(US//LF//"\(Tag Table\|Local Variables\)")
        call goto_char(opoint)
        useful = .not. (invisible .or. header .or. tags)
    end function info_isearch_filter

    !> Search for REGEXP, starting from point, and select node it's found
    !> in.  If BACKWARD is true, search in the reverse direction.  BOUND
    !> limits the search.  Port of info.el:Info-search.
    recursive subroutine info_search(regexp, status, bound, backward)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: re, onode, ofile, osubfile, thisfile
        type(string_type), allocatable :: names(:)
        integer(int64), allocatable :: positions(:)
        type(handle_type) :: rb, re_m, old, tagbuf
        type(status_type) :: st
        integer(int64) :: opoint, opoint_min, opoint_max, ostart, found, bnd, mpos, p
        logical :: back, has_bound, ok
        integer :: i, k

        status = status_type()
        re = regexp

        if (len(re) == 0) then
            if (.not. history_first("info_search_history", re)) return
        end if

        info_search_case_fold = case_fold_search%value
        back = .false.
        if (present(backward)) back = backward
        has_bound = .false.
        bnd = 0

        if (present(bound)) then
            has_bound = bound > 0
            bnd = bound
        end if

        onode = current_node_or_empty()
        ofile = current_file_or_empty()
        opoint = point()
        opoint_min = point_min()
        opoint_max = point_max()
        ostart = sel_window_start()
        osubfile = info_current_subfile()
        if (.not. allocated(osubfile)) osubfile = achar(0)
        call save_restriction(rb, re_m)
        call widen()
        found = search_loop(re, bnd, back)
        call restore_restriction(rb, re_m)
        call goto_char(opoint)

        if (isearch_active .and. info_isearch_search%value .and. .not. allocated(isearch_initial_node) .and. &
            .not. has_bound .and. .not. (found > opoint_min .and. found < opoint_max)) then
            call search_failed(status, re, "end of node")
            return
        end if

        ! If no subfiles, give error now.
        if (found == 0 .and. osubfile == achar(0)) then
            if (isearch_active) then
                call search_failed(status, re, "end of manual")
                return
            end if

            call with_spaces(ok)

            if (.not. ok) then
                call search_failed(status, re)
                return
            end if
        end if

        if ((has_bound .or. osubfile == achar(0)) .and. found == 0) then
            call search_failed(status, re)
            return
        end if

        if (found == 0 .and. .not. has_bound) then
            ! Try other subfiles.
            k = here()
            old = current_buffer()
            call marker_buffer(locals(k)%tag_table_marker, tagbuf, st)
            call set_buffer(tagbuf, st)
            call goto_min()
            allocate (names(0), positions(0))

            if (sfwd(LF//US//LF//"Indirect:")) then
                p = point()
                ok = sfwd(LF//US)
                call narrow_to_region(p, point() - 1, st)
                call goto_min()

                ! Find the subfile we just searched.
                if (sfwd(LF//osubfile//": ")) then
                    ! Skip that one.
                    if (back) then
                        call fwd_line(0_int64)
                        call goto_char(point() - 1)
                    else
                        call fwd_line(1_int64)
                    end if

                    ! Make a list of all following subfiles.
                    do while (.not. merge(bobp(), eobp(), back))
                        if (back) then
                            ok = rebwd("\(^.*\): [0-9]+$")
                        else
                            ok = refwd("\(^.*\): [0-9]+$")
                        end if

                        if (.not. ok) exit
                        thisfile = match_string_no_properties(1)
                        call goto_char(mend(1) + 2)
                        mpos = read_number()
                        call add_string(names, thisfile)
                        positions = [positions, mpos]

                        if (back) then
                            call goto_char(mbeg_of_line() - 1)
                        else
                            call goto_char(min(point_max(), line_end_position(1_int64) + 1))
                        end if
                    end do
                end if

                call widen()
            end if

            call set_buffer(old, st)

            do i = 1, size(names)
                call message("Searching subfile "//names(i)%s//"...")
                call read_subfile_named(names(i)%s, st)
                if (back) call goto_char(point_max())
                found = search_loop(re, 0_int64, back)
                if (found /= 0) exit
            end do

            if (found /= 0) then
                call message("")
            else
                call read_subfile_named(osubfile, st)
                call goto_char(opoint)
                call info_select_node(st)
                call set_sel_window_start(ostart, st)

                if (isearch_active) then
                    call search_failed(status, re, "end of manual")
                else
                    call search_failed(status, re)
                end if

                return
            end if
        end if

        thisfile = info_current_subfile()
        if (.not. allocated(thisfile)) thisfile = achar(0)

        if (osubfile == thisfile .and. found > opoint_min .and. found < opoint_max) then
            ! Search landed in the same node
            call goto_char(found)
        else
            call deactivate_mark()
            call widen()
            call goto_char(found)
            call info_select_node(status)
            if (.not. status%ok()) return
        end if

        ! Use string-equal, not equal, to ignore text props.
        if (.not. (onode == current_node_or_empty() .and. ofile == current_file_or_empty())) then
            if (.not. (isearch_active .and. isearch_wrapped .and. &
                       opoint == merge(opoint_min, opoint_max, isearch_forward))) then
                k = here()
                call push_history(locals(k)%history, ofile, onode, opoint)
            end if
        end if

    contains

        ! The search of the current buffer without subfiles, for the
        ! message: (re-search-forward regexp nil t) with the whitespace
        ! regexp.
        subroutine with_spaces(matched)
            logical, intent(out) :: matched
            character(len=:), allocatable :: saved
            logical :: had

            had = allocated(search_spaces_regexp)
            if (had) saved = search_spaces_regexp
            if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
            if (allocated(info_search_whitespace_regexp)) search_spaces_regexp = info_search_whitespace_regexp

            if (back) then
                matched = rebwd(re)
            else
                matched = refwd(re)
            end if

            if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
            if (had) search_spaces_regexp = saved
        end subroutine with_spaces

        ! The beginning of the match's line.
        integer(int64) function mbeg_of_line()
            mbeg_of_line = mbeg(0)
        end function mbeg_of_line
    end subroutine info_search

    ! Signal user-search-failed for REGEXP.
    ! Signal user-search-failed for REGEXP, with the second datum MSG if
    ! present.  Not a port.
    subroutine search_failed(status, regexp, msg)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: regexp
        character(len=*), intent(in), optional :: msg

        if (present(msg)) then
            call set_error(status, FM_USER_SEARCH_FAILED, quoted(regexp)//", "//quoted(msg))
        else
            call set_error(status, FM_USER_SEARCH_FAILED, quoted(regexp))
        end if

    contains

        ! STR as prin1 prints it.
        function quoted(str) result(q)
            character(len=*), intent(in) :: str
            character(len=:), allocatable :: q
            integer :: j

            q = '"'

            do j = 1, len(str)
                if (str(j:j) == '"' .or. str(j:j) == "\") q = q//"\"
                q = q//str(j:j)
            end do

            q = q//'"'
        end function quoted
    end subroutine search_failed

    ! `isearch_search_fun_function' of Info mode: searching through
    ! multiple nodes if `info_isearch_search'.  Port of
    ! info.el:Info-isearch-search.
    recursive subroutine info_isearch_search_fn(fn)
        procedure(isearch_search_proc), pointer, intent(out) :: fn

        if (info_isearch_search%value) then
            fn => info_isearch_search_nodes
        else
            fn => isearch_search_fun_default()
        end if
    end subroutine info_isearch_search_fn

    ! The search function `info_isearch_search_fn' returns (its lambda;
    ! `isearch_regexp_function' is Deferred).
    ! Port of info.el:Info-isearch-search, in part.
    recursive subroutine info_isearch_search_nodes(string, bound, noerror, pos, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: bound
        logical, intent(in) :: noerror
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved, re
        logical :: had

        had = allocated(info_search_whitespace_regexp)
        if (had) saved = info_search_whitespace_regexp
        if (allocated(info_search_whitespace_regexp)) deallocate (info_search_whitespace_regexp)
        if (merge(isearch_regexp_lax_whitespace, isearch_lax_whitespace, isearch_regexp) .and. &
            allocated(search_whitespace_regexp)) info_search_whitespace_regexp = search_whitespace_regexp

        if (isearch_regexp) then
            re = string
        else
            re = regexp_quote(string)
        end if

        call info_search(re, status, bound, backward=.not. isearch_forward)
        if (allocated(info_search_whitespace_regexp)) deallocate (info_search_whitespace_regexp)
        if (had) info_search_whitespace_regexp = saved
        pos = 0
        if (status%ok()) pos = point()
        if (.false. .and. noerror) pos = 0
    end subroutine info_isearch_search_nodes

    ! `isearch_wrap_function' of Info mode.  Port of
    ! info.el:Info-isearch-wrap.
    recursive subroutine info_isearch_wrap()
        type(status_type) :: st

        if (info_isearch_search%value) then
            if (allocated(isearch_initial_node)) then
                if (isearch_forward) then
                    call info_top_node(st)
                    call goto_char(point_min())
                else
                    call info_final_node(st)
                    call goto_char(point_max())
                end if
            else
                isearch_initial_node = current_node_or_empty()
                call set_isearch_wrapped(.false.)
            end if
        else if (isearch_forward) then
            call goto_char(point_min())
        else
            call goto_char(point_max())
        end if
    end subroutine info_isearch_wrap

    ! `isearch_push_state_function' of Info mode: the current file and
    ! node.  Port of info.el:Info-isearch-push-state.
    recursive subroutine info_isearch_push_state(data, pop)
        character(len=:), allocatable, intent(out) :: data
        procedure(isearch_pop_proc), pointer, intent(out) :: pop
        character(len=:), allocatable :: file, node

        file = info_current_file()
        node = info_current_node()
        data = merge("1", "0", allocated(file))//merge("1", "0", allocated(node))
        if (allocated(file)) data = data//file
        data = data//achar(0)
        if (allocated(node)) data = data//node
        pop => info_isearch_pop_state
    end subroutine info_isearch_push_state

    ! Go back to the file and node of DATA.  Port of
    ! info.el:Info-isearch-pop-state.
    recursive subroutine info_isearch_pop_state(data)
        character(len=*), intent(in) :: data
        character(len=:), allocatable :: file, node, cfile, cnode
        type(status_type) :: st
        integer :: z

        z = index(data, achar(0))
        file = data(3:z - 1)
        node = data(z + 1:)
        cfile = info_current_file()
        cnode = info_current_node()
        if (same(cfile, data(1:1), file) .and. same(cnode, data(2:2), node)) return
        call info_find_node(file, node, st)
        call sit_for(0)

    contains

        ! (equal CURRENT SAVED), FLAG "0" for a nil SAVED.
        logical function same(current, flag, saved)
            character(len=:), allocatable, intent(in) :: current
            character(len=*), intent(in) :: flag, saved

            if (flag == "0") then
                same = .not. allocated(current)
            else if (allocated(current)) then
                same = current == saved .and. len(current) == len(saved)
            else
                same = .false.
            end if
        end function same
    end subroutine info_isearch_pop_state

    ! `isearch_mode_hook' of Info mode.  Port of
    ! info.el:Info-isearch-start.
    recursive subroutine info_isearch_start(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        ! Don't stop at initial node for nonincremental search.
        ! Otherwise this variable is set after first search failure.
        if (allocated(isearch_initial_node)) deallocate (isearch_initial_node)
        if (isearch_nonincremental) isearch_initial_node = current_node_or_empty()
        k = here()
        isearch_initial_history = locals(k)%history

        if (allocated(info_history_list)) then
            isearch_initial_history_list = info_history_list
        else
            allocate (isearch_initial_history_list(0))
        end if

        call isearch_mode_end_hook%add(info_isearch_end, local=.true.)
    end subroutine info_isearch_start

    ! Remove intermediate nodes (visited while searching) from the
    ! history.  Add only the last node (where Isearch ended).  Port of
    ! info.el:Info-isearch-end.
    recursive subroutine info_isearch_end(status)
        type(status_type), intent(out) :: status
        type(info_node_ref_type), allocatable :: list(:)
        integer :: k, n, m, i

        status = status_type()
        k = here()
        n = size(locals(k)%history)
        m = 0
        if (allocated(isearch_initial_history)) m = size(isearch_initial_history)
        if (n > m) locals(k)%history = locals(k)%history(n - m:n)
        if (.not. allocated(info_history_list)) allocate (info_history_list(0))
        m = 0
        if (allocated(isearch_initial_history_list)) m = size(isearch_initial_history_list)

        if (size(info_history_list) > m) then
            allocate (list(m + 1))
            list(1) = info_history_list(1)

            do i = 1, m
                list(i + 1) = isearch_initial_history_list(i)
            end do

            call move_alloc(list, info_history_list)
        end if

        call isearch_mode_end_hook%remove(info_isearch_end, local=.true.)
    end subroutine info_isearch_end

    ! The most recent element of the minibuffer history NAME (FOUND false
    ! if it is empty).  Not a port.
    logical function history_first(name, value) result(found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: value
        type(value_type) :: hist
        type(status_type) :: st

        found = .false.
        value = ""
        call symbol_value(sym(name), hist, st)
        if (.not. st%ok()) return
        if (.not. consp(hist)) return
        if (.not. stringp(list_elt(hist, 1_int64))) return
        value = xstring(list_elt(hist, 1_int64))
        found = .true.
    end function history_first

    ! Read the regexp of `info_search' and `info_search_backward'.
    ! Port of info.el:Info-search, in part (its interactive spec).
    recursive subroutine read_search_regexp(regexp, status, backward)
        character(len=:), allocatable, intent(out) :: regexp
        type(status_type), intent(out) :: status
        logical, intent(in) :: backward
        character(len=:), allocatable :: last, prompt
        logical :: has_last

        has_last = history_first("info_search_history", last)
        prompt = "Regexp search"
        if (.not. case_fold_search%value) prompt = prompt//" case-sensitively"
        if (backward) prompt = prompt//" backward"

        if (has_last) then
            prompt = format_prompt(prompt, last)
        else
            prompt = prompt//": "
        end if

        call read_string(prompt, regexp, status, history="info_search_history")
    end subroutine read_search_regexp

    recursive subroutine cmd_info_search(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_search_regexp(regexp, status, .false.)
        if (.not. status%ok()) return
        call info_search(regexp, status)
    end subroutine cmd_info_search

    !> Search for a regexp case-sensitively.  Port of
    !> info.el:Info-search-case-sensitively.
    recursive subroutine info_search_case_sensitively(status)
        type(status_type), intent(out) :: status
        logical :: fold

        fold = bind_case_fold(.false.)
        call cmd_info_search(status)
        fold = bind_case_fold(fold)
    end subroutine info_search_case_sensitively

    !> Search for next regexp from a previous `info_search' command.
    !> Port of info.el:Info-search-next.
    recursive subroutine info_search_next(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: last
        logical :: fold

        fold = bind_case_fold(info_search_case_fold)

        if (history_first("info_search_history", last)) then
            call info_search(last, status)
        else
            call cmd_info_search(status)
        end if

        fold = bind_case_fold(fold)
    end subroutine info_search_next

    !> Search for REGEXP in the reverse direction.  Port of
    !> info.el:Info-search-backward.
    recursive subroutine info_search_backward(regexp, status, bound)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: bound

        call info_search(regexp, status, bound, backward=.true.)
    end subroutine info_search_backward

    recursive subroutine cmd_info_search_backward(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp

        call read_search_regexp(regexp, status, .true.)
        if (.not. status%ok()) return
        call info_search_backward(regexp, status)
    end subroutine cmd_info_search_backward

    ! ------------------------------------------------------------------
    ! The indexes
    ! ------------------------------------------------------------------

    ! Go to the first index node.  Port of info.el:Info-goto-index.
    recursive subroutine goto_index(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: nodes(:)

        status = status_type()
        call info_index_nodes(nodes)

        if (size(nodes) == 0) then
            call plain_error(status, "No index")
            return
        end if

        call info_goto_node(nodes(1)%s, status)
    end subroutine goto_index

    ! Signal an error in the directory node, which has no index.  Port of
    ! info.el:info--ensure-not-in-directory-node.
    subroutine ensure_not_in_directory_node(status)
        type(status_type), intent(inout) :: status

        if (lower(file_name_nondirectory(current_file_or_empty())) == "dir") then
            call user_error(status, substitute_command_keys("The Info directory node has no index; "// &
                                                            "type `\<info_mode_map>\[info_menu]' to select a manual"))
        end if
    end subroutine ensure_not_in_directory_node

    ! Read an index topic with completion over the entries of the index
    ! nodes of the current Info file.  (Upstream completes in a clone of
    ! the Info buffer; this uses a temporary Info buffer on the same file,
    ! `clone_buffer' not being ported.)  Port of info.el:Info-index, in part
    ! (its interactive spec).
    recursive subroutine read_index_topic(topic, status)
        character(len=:), allocatable, intent(out) :: topic
        type(status_type), intent(out) :: status
        type(handle_type) :: old, temp
        type(string_type), allocatable :: nodes(:)
        type(info_node_ref_type), allocatable :: saved_history_list(:)
        character(len=:), allocatable :: file
        type(status_type) :: st
        logical :: saved_ignore
        integer :: k

        status = status_type()
        call ensure_not_in_directory_node(status)
        if (.not. status%ok()) return
        call info_index_nodes(nodes)
        file = current_file_or_empty()
        ! (let ((Info-history-list nil)) ...)
        if (allocated(info_history_list)) saved_history_list = info_history_list
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *info index*"), temp, st)
        call set_buffer(temp, st)
        call info_mode(st)
        k = here()

        if (size(nodes) > 0) then
            call info_find_node(file, nodes(1)%s, status)
        else
            call plain_error(status, "No index")
        end if

        if (status%ok()) then
            complete_menu_buffer = temp
            complete_nodes = nodes
            saved_ignore = completion_ignore_case%value
            completion_ignore_case%value = .true.
            call completing_read("Index topic: ", completion_function_table(complete_menu_item), topic, status)
            completion_ignore_case%value = saved_ignore
            deallocate (complete_nodes)
        end if

        call info_kill_buffer(st)
        call set_buffer(old, st)
        call kill_buffer(temp, st)

        if (allocated(saved_history_list)) then
            info_history_list = saved_history_list
        else if (allocated(info_history_list)) then
            deallocate (info_history_list)
        end if
    end subroutine read_index_topic

    !> Look up a string TOPIC in the index for this manual and go to that
    !> entry.  If there are no exact matches to the specified topic, this
    !> chooses the first match which is a case-insensitive substring of a
    !> topic.  Use the \<info_mode_map>\[info_index_next] command to see
    !> the other matches.  Give an empty topic name to go to the Index node
    !> itself.  Port of info.el:Info-index.
    recursive subroutine info_index(topic, status)
        character(len=*), intent(in) :: topic
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: t, orignode, pattern, entry, nodename
        type(string_type), allocatable :: nodes(:)
        type(info_node_ref_type), allocatable :: ohist_list(:)
        type(index_match_type), allocatable :: matches(:), exact(:)
        type(info_history_entry_type), allocatable :: saved_history(:)
        type(index_match_type) :: m
        integer(int64) :: line
        logical :: fold
        integer :: i, k, ios

        status = status_type()
        call ensure_not_in_directory_node(status)
        if (.not. status%ok()) return
        ! Strip leading colon in topic; index format does not allow them.
        t = topic

        if (len(t) > 0) then
            if (t(1:1) == ":") t = t(2:)
        end if

        orignode = current_node_or_empty()
        pattern = LF//"\* +\([^"//LF//"]*\("//regexp_quote(t)//"\)[^"//LF//"]*\):["//" "//TAB//"]+\([^"//LF// &
                  "]*\)\.\(?:[ "//TAB//LF//"]*(line +\([0-9]+\))\)?"
        call info_index_nodes(nodes)
        if (allocated(info_history_list)) ohist_list = info_history_list
        fold = bind_case_fold(.true.)
        call goto_index(status)

        if (.not. status%ok() .or. len(t) == 0) then
            fold = bind_case_fold(fold)
            return
        end if

        allocate (matches(0), exact(0))
        ! We bind Info-history to nil for internal node-switches so that we
        ! don't put junk in the history.  In the first Info-goto-index call,
        ! above, we do update the history because that is what the user's
        ! previous node choice into it.
        k = here()
        saved_history = locals(k)%history
        deallocate (locals(k)%history)
        allocate (locals(k)%history(0))
        i = 1

        do
            call goto_min()

            do while (refwd(pattern))
                m%entry = match_string_no_properties(1)
                m%nodename = match_string_no_properties(3)
                m%index_node = current_node_or_empty()
                m%line = 0

                if (mbeg(4) > 0) then
                    entry = match_string_no_properties(4)
                    read (entry, *, iostat=ios) line
                    if (ios == 0) m%line = line
                end if

                m%match_beg = nchars_of(text(mbeg(1), mbeg(2)))
                m%match_end = nchars_of(text(mbeg(1), mend(2)))
                call prepend_match(matches, m)
            end do

            i = i + 1
            if (i > size(nodes)) exit
            call info_goto_node(nodes(i)%s, status)
            if (.not. status%ok()) exit
        end do

        k = here()
        locals(k)%history = saved_history

        if (status%ok() .and. size(matches) == 0) then
            call info_goto_node(orignode, status)
            if (status%ok()) call user_error(status, "No `"//t//"' in index")
        end if

        if (.not. status%ok()) then
            fold = bind_case_fold(fold)
            return
        end if

        ! Here it is a feature that assoc is case-sensitive.
        do
            k = 0

            do i = 1, size(matches)
                if (matches(i)%entry == t) then
                    k = i
                    exit
                end if
            end do

            if (k == 0) exit
            call prepend_match(exact, matches(k))

            if (size(matches) == 1) then
                deallocate (matches)
                allocate (matches(0))
            else
                matches = [matches(1:k - 1), matches(k + 1:)]
            end if
        end do

        if (allocated(ohist_list)) then
            info_history_list = ohist_list
        else if (allocated(info_history_list)) then
            deallocate (info_history_list)
        end if

        k = here()
        ! (nconc exact (nreverse matches))
        if (allocated(locals(k)%index_alternatives)) deallocate (locals(k)%index_alternatives)
        allocate (locals(k)%index_alternatives(0))

        do i = 1, size(exact)
            call append_match(locals(k)%index_alternatives, exact(i))
        end do

        do i = size(matches), 1, -1
            call append_match(locals(k)%index_alternatives, matches(i))
        end do

        locals(k)%current_index_alternative = 0
        fold = bind_case_fold(fold)
        call info_index_next(0_int64, status)
        if (.false.) nodename = ""
    end subroutine info_index

    ! (push M LIST).  Not a port.
    subroutine prepend_match(list, m)
        type(index_match_type), allocatable, intent(inout) :: list(:)
        type(index_match_type), intent(in) :: m
        type(index_match_type), allocatable :: grown(:)
        integer :: i

        allocate (grown(size(list) + 1))
        grown(1) = m

        do i = 1, size(list)
            grown(i + 1) = list(i)
        end do

        call move_alloc(grown, list)
    end subroutine prepend_match

    ! Append M to LIST.  Not a port.
    subroutine append_match(list, m)
        type(index_match_type), allocatable, intent(inout) :: list(:)
        type(index_match_type), intent(in) :: m
        type(index_match_type), allocatable :: grown(:)
        integer :: i

        allocate (grown(size(list) + 1))

        do i = 1, size(list)
            grown(i) = list(i)
        end do

        grown(size(list) + 1) = m
        call move_alloc(grown, list)
    end subroutine append_match

    recursive subroutine cmd_info_index(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: topic

        call read_index_topic(topic, status)
        if (.not. status%ok()) return
        call info_index(topic, status)
    end subroutine cmd_info_index

    !> Go to the next matching index item from the last
    !> \<info_mode_map>\[info_index] command.  If given a numeric prefix,
    !> skip that many index items forward (or backward).
    !>
    !> Also see the `info_warn_on_index_alternatives_wrap' user option.
    !> Port of info.el:Info-index-next.
    recursive subroutine info_index_next(num, status)
        integer(int64), intent(in) :: num
        type(status_type), intent(out) :: status
        type(index_match_type) :: entry
        character(len=:), allocatable :: next_key
        type(value_type) :: keys
        integer(int64) :: idx, total
        logical :: wrapped
        integer :: k

        status = status_type()
        k = here()

        if (.not. allocated(locals(k)%index_alternatives)) then
            call user_error(status, "No previous `i' command")
            return
        end if

        if (size(locals(k)%index_alternatives) == 0) then
            call user_error(status, "No previous `i' command")
            return
        end if

        idx = locals(k)%current_index_alternative + num
        total = size(locals(k)%index_alternatives)
        call where_is_internal(sym("info_index_next"), keys, .true.)
        next_key = ""
        if (.not. nilp(keys)) next_key = key_description(vector_elements(keys))
        wrapped = .false.

        if (info_warn_on_index_alternatives_wrap%value .and. total > 1) then
            if (idx < 0) then
                locals(k)%current_index_alternative = total - 2
                call message("No previous matches, use `"//next_key//"' to continue from end of list")
                wrapped = .true.
            else if (idx >= total) then
                locals(k)%current_index_alternative = -1
                call message("No previous matches, use `"//next_key//"' to continue from start of list")
                wrapped = .true.
            end if
        end if

        if (wrapped) return
        idx = modulo(idx, total)
        locals(k)%current_index_alternative = idx
        entry = locals(k)%index_alternatives(idx + 1)
        call info_goto_node(entry%nodename, status)
        if (.not. status%ok()) return

        if (entry%line > 0) then
            ! Forward 2 lines less because `Info-find-node-2' initially
            ! puts point to the 2nd line.
            call fwd_line(entry%line - 2)
        else
            ! don't search in headers
            call fwd_line(3_int64)
            call find_index_name(entry%entry)
        end if

        if (total > 1) then
            call message("Found `"//entry%entry//"' in "//entry%index_node//".  ("//int_text(total)// &
                         " total; use `"//next_key//"' for next)")
        else
            call message("Found `"//entry%entry//"' in "//entry%index_node//".  (Only match)")
        end if
    end subroutine info_index_next

    recursive subroutine cmd_info_index_next(status)
        type(status_type), intent(out) :: status

        call info_index_next(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_info_index_next

    ! The decimal digits of N.  Not a port.
    function int_text(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    ! The keys of the key sequence V (a vector or list of events).
    ! Not a port.
    function vector_elements(v) result(keys)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: keys(:)

        call list_elements(v, keys)
    end function vector_elements

    ! Index-specific implementation of `Info-find-node-2'.  Port of
    ! info.el:Info-virtual-index-find-node.
    recursive subroutine virtual_index_find_node(filename, nodename, no_going_back, status)
        character(len=*), intent(in) :: filename, nodename
        logical, intent(in) :: no_going_back
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: topic, label
        type(status_type) :: st
        integer :: i, j

        status = status_type()
        if (no_going_back) continue

        if (smatch("^\*Index for ‘\(.+\)’\*$", nodename, .true.)) then
            ! Generate Index-like menu of matches
            topic = match_string_no_properties(1, nodename)
            call insert(LF//US//LF//"File: "//filename//",  Node: "//nodename//",  Up: *Index*"//LF//LF, status)
            call insert("Info Virtual Index"//LF, st)
            call insert("******************"//LF//LF, st)
            call insert("Index entries that match ‘"//topic//"’:"//LF//LF, st)
            call insert(NUL//BS//"[index"//NUL//BS//"]"//LF, st)
            j = 0

            do i = 1, size(virtual_index_nodes)
                if (virtual_index_nodes(i)%file == filename .and. virtual_index_nodes(i)%topic == topic) j = i
                if (j > 0) exit
            end do

            if (j == 0) then
                call insert("No matches found."//LF, st)
            else if (size(virtual_index_nodes(j)%matches) == 0) then
                call insert("No matches found."//LF, st)
            else
                call insert("* Menu:"//LF//LF, st)

                do i = 1, size(virtual_index_nodes(j)%matches)
                    associate (e => virtual_index_nodes(j)%matches(i))
                        label = e%entry//" ["//e%index_node//"]:"
                        call insert_entry("* ", label, 38, e)
                        call insert(" "//e%nodename//".", st)
                        if (e%line > 0) call insert(" (line "//int_text(e%line)//")", st)
                        call insert(LF, st)
                    end associate
                end do
            end if
        else
            ! Else, Generate a list of previous search results
            call insert(LF//US//LF//"File: "//filename//",  Node: "//nodename//",  Up: Top"//LF//LF, status)
            call insert("Info Virtual Index"//LF, st)
            call insert("******************"//LF//LF, st)
            call insert("This is a list of search results produced by"//LF// &
                        "‘Info-virtual-index’ for the current manual."//LF//LF, st)
            call insert("* Menu:"//LF//LF, st)

            do i = size(virtual_index_nodes), 1, -1
                if (virtual_index_nodes(i)%file /= filename) cycle
                label = "*Index for ‘"//virtual_index_nodes(i)%topic//"’*::"
                call insert("* "//pad_to(label, 20)//" "//virtual_index_nodes(i)%topic//"."//LF, st)
            end do
        end if
    end subroutine virtual_index_find_node

    ! Insert PREFIX and LABEL padded to WIDTH characters, the part of the
    ! entry of E that matched the topic in `info_index_match' (the
    ! property `Info-index' puts on its entries).  Not a port.
    subroutine insert_entry(prefix, label, width, e)
        character(len=*), intent(in) :: prefix, label
        integer, intent(in) :: width
        type(index_match_type), intent(in) :: e
        type(status_type) :: st
        integer(int64) :: start

        call insert(prefix, st)
        start = point()
        call insert(pad_to(label, width), st)

        if (e%match_end > e%match_beg) then
            call put_text_property(start + e%match_beg, start + e%match_end, sym("face"), &
                                   symbol_value_of("info_index_match"), st)
        end if
    end subroutine insert_entry

    ! S padded with spaces to WIDTH characters ((format "%-WIDTHs" S)).
    ! Not a port.
    function pad_to(s, width) result(t)
        character(len=*), intent(in) :: s
        integer, intent(in) :: width
        character(len=:), allocatable :: t

        t = s//repeat(" ", max(0, width - int(nchars_of(s))))
    end function pad_to

    !> Show a node with all lines in the index containing a string TOPIC.
    !> Like `info_index' but displays a node with index search results.
    !> Give an empty topic name to go to the node with links to previous
    !> search results.  Port of info.el:Info-virtual-index.
    recursive subroutine info_virtual_index(topic, status)
        character(len=*), intent(in) :: topic
        type(status_type), intent(out) :: status
        type(virtual_index_type), allocatable :: grown(:)
        type(info_node_ref_type), allocatable :: ohist_list(:)
        character(len=:), allocatable :: orignode, file
        logical :: cached
        integer :: i, k

        status = status_type()
        file = current_file_or_empty()

        if (len(topic) == 0) then
            call info_find_node(file, "*Index*", status)
            return
        end if

        cached = .false.

        do i = 1, size(virtual_index_nodes)
            if (virtual_index_nodes(i)%file == file .and. virtual_index_nodes(i)%topic == topic) cached = .true.
        end do

        if (.not. cached) then
            orignode = current_node_or_empty()
            if (allocated(info_history_list)) ohist_list = info_history_list
            ! Reuse `Info-index' to set `Info-index-alternatives'.
            call info_index(topic, status)
            if (.not. status%ok()) return
            k = here()
            allocate (grown(size(virtual_index_nodes) + 1))
            grown(1)%file = file
            grown(1)%topic = topic
            grown(1)%matches = locals(k)%index_alternatives

            do i = 1, size(virtual_index_nodes)
                grown(i + 1) = virtual_index_nodes(i)
            end do

            call move_alloc(grown, virtual_index_nodes)

            ! Clean up unnecessary side-effects of `Info-index'.
            if (allocated(ohist_list)) then
                info_history_list = ohist_list
            else if (allocated(info_history_list)) then
                deallocate (info_history_list)
            end if

            call info_goto_node(orignode, status)
            if (.not. status%ok()) return
            call message("")
        end if

        call info_find_node(file, "*Index for ‘"//topic//"’*", status)
    end subroutine info_virtual_index

    recursive subroutine cmd_info_virtual_index(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: topic

        call read_index_topic(topic, status)
        if (.not. status%ok()) return
        call info_virtual_index(topic, status)
    end subroutine cmd_info_virtual_index

    ! Apropos-specific implementation of `Info-toc-nodes'.  Port of
    ! info.el:Info-apropos-toc-nodes.
    recursive subroutine apropos_toc_nodes(filename, nodes)
        character(len=*), intent(in) :: filename
        type(toc_node_type), allocatable, intent(out) :: nodes(:)
        integer :: i, n

        if (len(filename) < 0) continue
        n = size(apropos_nodes)
        allocate (nodes(n + 1))
        nodes(1)%name = "Top"
        allocate (nodes(1)%children(0))

        do i = n, 1, -1
            call add_string(nodes(1)%children, apropos_nodes(i)%nodename)
            nodes(n - i + 2)%name = apropos_nodes(i)%nodename
            nodes(n - i + 2)%has_parent = .true.
            nodes(n - i + 2)%parent = "Top"
            allocate (nodes(n - i + 2)%children(0))
        end do
    end subroutine apropos_toc_nodes

    ! Apropos-specific implementation of `Info-find-node-2'.  Port of
    ! info.el:Info-apropos-find-node.
    recursive subroutine apropos_find_node(filename, nodename, no_going_back, status)
        character(len=*), intent(in) :: filename, nodename
        logical, intent(in) :: no_going_back
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: label
        type(status_type) :: st
        integer :: i, j

        status = status_type()
        if (no_going_back .or. len(filename) < 0) continue

        if (nodename == "Top") then
            ! Generate Top menu
            call insert(LF//US//LF//"File: "//INFO_APROPOS_FILE//",  Node: "//nodename//",  Up: (dir)"//LF//LF, st)
            call insert("Apropos Index"//LF, st)
            call insert("*************"//LF//LF, st)
            call insert("This is a list of search results produced by ‘info-apropos’."//LF//LF, st)
            call insert("* Menu:"//LF//LF, st)

            do i = size(apropos_nodes), 1, -1
                call insert("* "//pad_to(apropos_nodes(i)%nodename//"::", 20)//" "//apropos_nodes(i)%string//"."//LF, st)
            end do

            return
        end if

        ! Else, Generate Index-like menu of matches
        j = 0

        do i = 1, size(apropos_nodes)
            if (apropos_nodes(i)%nodename == nodename) then
                j = i
                exit
            end if
        end do

        if (j == 0) return
        if (.not. apropos_nodes(j)%has_matches) return
        call insert(LF//US//LF//"File: "//INFO_APROPOS_FILE//",  Node: "//nodename//",  Up: Top"//LF//LF, st)
        call insert("Apropos Index"//LF, st)
        call insert("*************"//LF//LF, st)
        call insert("Index entries that match ‘"//apropos_nodes(j)%string//"’:"//LF//LF, st)
        call insert(NUL//BS//"[index"//NUL//BS//"]"//LF, st)

        if (size(apropos_nodes(j)%matches) == 0) then
            call insert("No matches found."//LF, st)
        else
            call insert("* Menu:"//LF//LF, st)

            do i = 1, size(apropos_nodes(j)%matches)
                associate (e => apropos_nodes(j)%matches(i))
                    label = e%entry//" ["//e%index_node//"]:"
                    call insert_entry("* ", label, 38, e)
                    call insert(" ("//e%index_node//")"//e%nodename//".", st)
                    if (e%line > 0) call insert(" (line "//int_text(e%line)//")", st)
                    call insert(LF, st)
                end associate
            end do
        end if
    end subroutine apropos_find_node

    ! Collect STRING matches from all known Info files on your system.  If
    ! REGEXP, use regexp matching instead of literal matching.  Return the
    ! matches, each with the manual (in INDEX_NODE), the index text, the
    ! node name and the line number.  Port of info.el:Info-apropos-matches.
    recursive subroutine apropos_matches(string, regexp, matches, status)
        character(len=*), intent(in) :: string
        logical, intent(in) :: regexp
        type(index_match_type), allocatable, intent(out) :: matches(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern, current_node, current_file, found, entry
        type(string_type), allocatable :: manuals(:), nodes(:)
        type(info_history_entry_type), allocatable :: ohist(:)
        type(info_node_ref_type), allocatable :: ohist_list(:)
        type(index_match_type) :: m
        type(status_type) :: st
        integer(int64) :: line
        integer :: i, j, k, saved_size, ios

        status = status_type()
        allocate (matches(0))
        if (len(string) == 0) return

        if (regexp) then
            pattern = string
        else
            pattern = regexp_quote(string)
        end if

        pattern = LF//"\* +\([^"//LF//"]*\("//pattern//"\)[^"//LF//"]*\):[ "//TAB//"]+\([^"//LF// &
                  "]+\)\.\(?:[ "//TAB//LF//"]*(line +\([0-9]+\))\)?"
        k = here()
        ohist = locals(k)%history
        if (allocated(info_history_list)) ohist_list = info_history_list
        current_node = info_current_node()
        current_file = info_current_file()
        saved_size = info_fontify_maximum_menu_size
        info_fontify_maximum_menu_size = INFO_FONTIFY_MAXIMUM_MENU_SIZE_NIL
        call info_directory(st)

        ! current-node and current-file are nil when they invoke
        ! info-apropos as the first Info command, i.e. info-apropos loads
        ! info.el.  In that case, we use (DIR)Top instead, to avoid
        ! signaling an error after the search is complete.
        if (.not. allocated(current_node)) then
            current_file = current_file_or_empty()
            current_node = current_node_or_empty()
        end if

        call message("Searching indices...")
        call goto_min()
        allocate (manuals(0))
        if (refwd("\* Menu: *"//LF)) continue

        do while (refwd("\*.*: *(\([^)]+\))"))
            ! Make sure we don't have duplicates in `manuals', so that the
            ! following loop runs faster.
            if (.not. has_string(manuals, match_string_no_properties(1))) then
                call add_string(manuals, match_string_no_properties(1))
            end if
        end do

        do i = 1, size(manuals)
            call message("Searching "//manuals(i)%s)
            call info_find_file(manuals(i)%s, found, st, noerror=.true.)
            if (.not. allocated(found)) cycle
            call info_index_nodes(nodes, found)
            if (size(nodes) == 0) cycle
            call info_find_node(manuals(i)%s, nodes(1)%s, st, noerror=.true.)

            if (.not. st%ok()) then
                call message(format_error(st))
                cycle
            end if

            j = 1

            do
                call goto_min()

                do while (refwd(pattern))
                    m%entry = match_string_no_properties(1)
                    m%nodename = match_string_no_properties(3)
                    m%index_node = manuals(i)%s
                    m%line = 0

                    if (mbeg(4) > 0) then
                        entry = match_string_no_properties(4)
                        read (entry, *, iostat=ios) line
                        if (ios == 0) m%line = line
                    end if

                    m%match_beg = nchars_of(text(mbeg(1), mbeg(2)))
                    m%match_end = nchars_of(text(mbeg(1), mend(2)))
                    call prepend_match(matches, m)
                end do

                j = j + 1
                if (j > size(nodes)) exit
                call info_goto_node(nodes(j)%s, st)
                if (.not. st%ok()) exit
            end do
        end do

        info_fontify_maximum_menu_size = saved_size
        call info_find_node(current_file, current_node, st)
        k = here()
        locals(k)%history = ohist

        if (allocated(ohist_list)) then
            info_history_list = ohist_list
        else if (allocated(info_history_list)) then
            deallocate (info_history_list)
        end if

        call message("Searching indices...done")
        ! (nreverse matches)
        matches = matches(size(matches):1:-1)
    end subroutine apropos_matches

    !> Search indices of all known Info files on your system for STRING.
    !> If REGEXP (interactively, the prefix), use a regexp match.
    !>
    !> Display a menu of the possible matches.  Port of
    !> info.el:info-apropos.
    recursive subroutine info_apropos(string, status, regexp)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: regexp
        type(apropos_node_type), allocatable :: grown(:)
        type(index_match_type), allocatable :: matches(:)
        character(len=:), allocatable :: nodename
        logical :: re
        integer :: i

        status = status_type()

        if (len(string) == 0) then
            call info_find_node(INFO_APROPOS_FILE, "Top", status)
            return
        end if

        do i = 1, size(apropos_nodes)
            if (apropos_nodes(i)%string == string) then
                call info_find_node(INFO_APROPOS_FILE, apropos_nodes(i)%nodename, status, noerror=.true.)
                return
            end if
        end do

        re = .false.
        if (present(regexp)) re = regexp
        nodename = "Index for ‘"//string//"’"
        call apropos_matches(string, re, matches, status)
        if (.not. status%ok()) return
        allocate (grown(size(apropos_nodes) + 1))
        grown(1)%nodename = nodename
        grown(1)%string = string
        grown(1)%has_matches = .true.
        grown(1)%matches = matches

        do i = 1, size(apropos_nodes)
            grown(i + 1) = apropos_nodes(i)
        end do

        call move_alloc(grown, apropos_nodes)
        call info_find_node(INFO_APROPOS_FILE, nodename, status)
    end subroutine info_apropos

    recursive subroutine cmd_info_apropos(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string

        call read_string("Index apropos: ", string, status)
        if (.not. status%ok()) return
        call info_apropos(string, status,.not. nilp(current_prefix_arg))
    end subroutine cmd_info_apropos

    ! ------------------------------------------------------------------
    ! The documentation of commands
    ! ------------------------------------------------------------------

    ! Return the locations documenting COMMAND: elements (FILENAME
    ! NODENAME BUFFERPOS), where BUFFERPOS is the line number in the
    ! first element of the returned list (which is treated specially in
    ! `info_goto_emacs_command_node'), and 0 for the rest elements of the
    ! list.  The `info_file' of COMMAND says which Info manual to search.
    ! If COMMAND has none, `info_file_list_for_emacs' defines heuristics
    ! for which Info manual to try.  Port of
    ! info.el:Info-find-emacs-command-nodes.
    recursive subroutine find_emacs_command_nodes(command, places, status)
        character(len=*), intent(in) :: command
        type(info_history_entry_type), allocatable, intent(out) :: places(:)
        type(status_type), intent(out) :: status
        type(command_type), pointer :: cmd
        type(string_type), allocatable :: nodes(:)
        type(info_history_entry_type), allocatable :: saved_history(:)
        type(info_node_ref_type), allocatable :: saved_history_list(:)
        character(len=:), allocatable :: cmd_desc, info_file, name, digits
        integer(int64) :: line_number
        logical :: fold, has_line, found
        integer :: i, k, ios

        status = status_type()
        allocate (places(0))
        cmd_desc = "^\* +"//regexp_quote(command)//"\( <[0-9]+>\)?:\s *\(.*\)\." &
                   //"\(?:[ "//TAB//LF//"]+(line +\([0-9]+\))\)?"
        ! default
        info_file = "emacs"
        ! Determine which Info file this command is documented in.
        cmd => find_command(command)
        found = .false.
        if (associated(cmd)) found = allocated(cmd%info_file)

        if (found) then
            info_file = cmd%info_file
        else
            ! If it doesn't say explicitly, test its name against various
            ! prefixes that we know.
            fold = bind_case_fold(.false.)

            do i = 1, size(info_file_list_for_emacs)
                name = trim(info_file_list_for_emacs(i)%prefix)

                if (smatch("\`"//regexp_quote(name)//"\(\'\|_\)", command)) then
                    info_file = trim(info_file_list_for_emacs(i)%file)
                    exit
                end if
            end do

            fold = bind_case_fold(fold)
        end if

        call info_find_node(info_file, "Top", status)
        if (.not. status%ok()) return
        ! Bind Info-history to nil, to prevent the index nodes from getting
        ! into the node history.
        k = here()
        saved_history = locals(k)%history
        deallocate (locals(k)%history)
        allocate (locals(k)%history(0))
        if (allocated(info_history_list)) saved_history_list = info_history_list
        if (allocated(info_history_list)) deallocate (info_history_list)
        call info_index_nodes(nodes)
        has_line = .false.
        line_number = 0
        if (size(nodes) > 0) call info_goto_node(nodes(1)%s, status)
        i = 1

        do while (status%ok() .and. size(nodes) > 0)
            call goto_min()

            do while (refwd(cmd_desc))
                call prepend_entry(places, current_file_or_empty(), match_string_no_properties(2), 0_int64)
                has_line = mbeg(3) > 0

                if (has_line) then
                    digits = match_string_no_properties(3)
                    read (digits, *, iostat=ios) line_number
                    if (ios /= 0) line_number = 0
                end if
            end do

            i = i + 1
            if (i > size(nodes)) exit
            call info_goto_node(nodes(i)%s, status)
        end do

        k = here()
        locals(k)%history = saved_history

        if (allocated(saved_history_list)) then
            info_history_list = saved_history_list
        else if (allocated(info_history_list)) then
            deallocate (info_history_list)
        end if

        if (has_line .and. size(places) > 0) places(1)%pos = line_number

    contains

        ! (push (list FILE NODE POS) LIST).
        subroutine prepend_entry(list, file, node, pos)
            type(info_history_entry_type), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: file, node
            integer(int64), intent(in) :: pos

            call push_history(list, file, node, pos)
        end subroutine prepend_entry
    end subroutine find_emacs_command_nodes

    !> Go to the Info node in the Emacs manual for command COMMAND.  The
    !> command is found by looking up in Emacs manual's indices or in
    !> another manual found via COMMAND's `info_file' or the variable
    !> `info_file_list_for_emacs'.  COMMAND is the name of a command.
    !> Port of info.el:Info-goto-emacs-command-node.
    recursive subroutine info_goto_emacs_command_node(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        type(info_history_entry_type), allocatable :: places(:), saved_history(:)
        type(info_node_ref_type), allocatable :: saved_history_list(:)
        type(window_configuration_type) :: config
        type(handle_type) :: info_buffer
        type(status_type) :: st
        integer(int64) :: line_number
        integer :: num_matches, i, k

        status = status_type()

        if (.not. associated(find_command(command))) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "commandp, "//command)
            return
        end if

        call find_emacs_command_nodes(command, places, status)
        if (.not. status%ok()) return

        if (size(places) == 0) then
            call plain_error(status, "Couldn't find documentation for "//command)
            return
        end if

        num_matches = size(places)
        ! Get Info running, and pop to it in another window.
        call current_window_configuration(config)
        call info(st)
        call set_window_configuration(config)
        call free_window_configuration(config)

        if (.not. derived_mode_p(major_modes%info_mode)) then
            info_buffer = get_buffer("*info*")
            if (info_buffer%index == 0) call get_buffer_create("*info*", info_buffer, st)
            call pop_to_buffer(info_buffer, status)
            if (.not. status%ok()) return
        end if

        ! Bind Info-history to nil, to prevent the last Index node visited
        ! by Info-find-emacs-command-nodes from being pushed onto the
        ! history.
        k = here()
        saved_history = locals(k)%history
        deallocate (locals(k)%history)
        allocate (locals(k)%history(0))
        if (allocated(info_history_list)) saved_history_list = info_history_list
        if (allocated(info_history_list)) deallocate (info_history_list)
        line_number = places(1)%pos
        call info_find_node(places(1)%file, places(1)%node, status)
        if (status%ok() .and. line_number > 0) call fwd_line(line_number - 1)
        k = here()
        locals(k)%history = saved_history

        if (allocated(saved_history_list)) then
            info_history_list = saved_history_list
        else if (allocated(info_history_list)) then
            deallocate (info_history_list)
        end if

        if (.not. status%ok()) return

        if (num_matches > 1) then
            ! (car where) will be pushed onto Info-history when/if they go
            ! to another node.  Put the other nodes that were found on the
            ! history.
            do i = num_matches, 2, -1
                call push_history(locals(k)%history, places(i)%file, places(i)%node, places(i)%pos)
            end do

            if (num_matches > 2) then
                call message("Found "//int_text(int(num_matches - 1, int64))//" other entries.  Use "// &
                             substitute_command_keys("\[info_history_back]")//" to see them.")
            else
                call message("Found "//int_text(int(num_matches - 1, int64))//" other entry.  Use "// &
                             substitute_command_keys("\[info_history_back]")//" to see it.")
            end if
        end if
    end subroutine info_goto_emacs_command_node

    recursive subroutine cmd_info_goto_emacs_command_node(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call completing_read("Find documentation for command: ", completion_obarray_table(), name, status, &
                             commandp, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        call info_goto_emacs_command_node(name, status)
    end subroutine cmd_info_goto_emacs_command_node

    !> Go to the node in the Emacs manual which describes the command bound
    !> to KEY.  KEY is a key sequence.  Interactively, if the binding is
    !> `execute_extended_command', a command is read.  The command is found
    !> by looking up in Emacs manual's indices or in another manual found
    !> via COMMAND's `info_file' or the variable
    !> `info_file_list_for_emacs'.  Port of
    !> info.el:Info-goto-emacs-key-command-node.
    recursive subroutine info_goto_emacs_key_command_node(key, status, interactive)
        type(value_type), intent(in) :: key(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(value_type) :: command
        character(len=:), allocatable :: name
        logical :: inter

        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        call key_binding(key, command, status)
        if (.not. status%ok()) return

        if (nilp(command)) then
            call message(key_description(key)//" is undefined")
        else if (symbolp(command)) then
            name = trim(symbols(xsymbol(command))%name)

            if (inter .and. name == "execute_extended_command") then
                call completing_read("Find documentation for command: ", completion_obarray_table(), name, status, &
                                     commandp, require_match=REQUIRE_MATCH_T)
                if (.not. status%ok()) return
            end if

            call info_goto_emacs_command_node(name, status)
        else
            call message(substitute_command_keys("\`"//key_description(key)// &
                                                 "' invokes an anonymous command defined with `lambda'"))
        end if
    end subroutine info_goto_emacs_key_command_node

    recursive subroutine cmd_info_goto_emacs_key_command_node(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof

        call read_key_sequence_vs(keys, binding, eof, status, prompt="Find documentation for key: ")
        if (.not. status%ok() .or. eof) return
        call info_goto_emacs_key_command_node(keys, status, .true.)
    end subroutine cmd_info_goto_emacs_key_command_node

    !> Display an Info buffer displaying MANUAL.
    !> If there is an existing Info buffer for MANUAL, display it.
    !> Otherwise, visit the manual in a new Info buffer.  In interactive
    !> use, a prefix argument directs this command to limit the
    !> completion alternatives to currently visited manuals.  Port of
    !> info.el:info-display-manual (one frame).
    recursive subroutine info_display_manual(manual, status)
        character(len=*), intent(in) :: manual
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: blist(:)
        type(handle_type) :: found, old, window
        character(len=:), allocatable :: manual_re, file
        type(status_type) :: st
        logical :: saved_fold, has
        integer :: i

        status = status_type()
        manual_re = "\(/\|\`\)"//manual//"\(\.\|\'\)"
        saved_fold = case_fold_search%value
        case_fold_search = .true.
        blist = buffer_list()
        old = current_buffer()
        has = .false.

        do i = 1, size(blist)
            call set_buffer(blist(i), st)

            if (derived_mode_p(major_modes%info_mode)) then
                file = info_current_file()

                if (allocated(file)) then
                    if (smatch(manual_re, file)) then
                        found = blist(i)
                        has = .true.
                    end if
                end if
            end if

            if (has) exit
        end do

        call set_buffer(old, st)
        case_fold_search = saved_fold

        if (has) then
            window = get_buffer_window(found)

            ! If the buffer is already displayed in a window somewhere,
            ! then select that window (and pop its frame to the top).
            if (window%index /= 0) then
                call select_window(window, status)
            else
                call info_pop_to_buffer(status, buffer_handle=found)
            end if
        else
            ! The buffer doesn't exist; create it.
            call info_initialize()
            call info_find_file(manual, file, status)
            if (.not. status%ok()) return
            call info(status, file, generate_new_buffer_name("*info*"))
        end if
    end subroutine info_display_manual

    ! The manuals visited in Info buffers, and unless VISITED_ONLY those
    ! in `info_directory_list' and `info_additional_directory_list'.
    ! Port of info.el:info--manual-names.
    subroutine info_manual_names(visited_only, names)
        logical, intent(in) :: visited_only
        type(string_type), allocatable, intent(out) :: names(:)
        type(handle_type), allocatable :: blist(:)
        type(handle_type) :: old
        type(string_type), allocatable :: dirs(:), more(:), extra(:)
        character(len=:), allocatable :: file, bname
        type(status_type) :: st
        integer :: i

        allocate (names(0))
        blist = buffer_list()
        old = current_buffer()

        do i = 1, size(blist)
            call set_buffer(blist(i), st)
            if (.not. derived_mode_p(major_modes%info_mode)) cycle
            file = info_current_file()
            if (.not. allocated(file)) cycle
            bname = buffers(cur)%name

            if (len(bname) > 0) then
                if (bname(1:1) == " ") cycle
            end if

            if (.not. has_string(names, file_name_sans_extension(file_name_nondirectory(file)))) then
                call add_string(names, file_name_sans_extension(file_name_nondirectory(file)))
            end if
        end do

        call set_buffer(old, st)
        if (visited_only) return
        call setting_dirs(info_directory_list, dirs)
        call setting_dirs(info_additional_directory_list, extra)

        do i = 1, size(extra)
            call add_string(dirs, extra(i)%s)
        end do

        call info_file_names(dirs, more)

        do i = 1, size(more)
            if (.not. has_string(names, more(i)%s)) call add_string(names, more(i)%s)
        end do
    end subroutine info_manual_names

    ! NAMES without subfiles and other files, sorted by length, the
    ! shorter of the names differing only by an Info or compression
    ! suffix kept.  Port of info.el:info--filter-manual-names.
    subroutine info_filter_manual_names(names, filtered)
        type(string_type), intent(in) :: names(:)
        type(string_type), allocatable, intent(out) :: filtered(:)
        type(string_type), allocatable :: kept(:)
        type(string_type) :: tmp
        logical :: dup
        integer :: i, j

        allocate (kept(0), filtered(0))

        do i = 1, size(names)
            if (smatch("info-[0-9]", names(i)%s)) cycle
            if (names(i)%s == "./" .or. names(i)%s == "../" .or. names(i)%s == "ChangeLog" .or. &
                names(i)%s == "NEWS" .or. names(i)%s == "README") cycle
            call add_string(kept, names(i)%s)
        end do

        ! We prefer the shorter names ("foo" over "foo.gz"): a stable
        ! sort by length.
        do i = 2, size(kept)
            j = i

            do while (j > 1)
                if (len(kept(j - 1)%s) <= len(kept(j)%s)) exit
                tmp = kept(j)
                kept(j) = kept(j - 1)
                kept(j - 1) = tmp
                j = j - 1
            end do
        end do

        do i = 1, size(kept)
            dup = .false.

            do j = 1, size(filtered)
                if (strip(filtered(j)%s) == strip(kept(i)%s)) dup = .true.
            end do

            if (.not. dup) call add_string(filtered, kept(i)%s)
        end do

    contains

        ! NAME without "-info", ".info" and ".gz" at its end (case
        ! folded, as replace-regexp-in-string is).
        function strip(name) result(r)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: r
            integer :: n

            r = name
            n = len(r)

            if (n >= 3) then
                if (lower(r(n - 2:n)) == ".gz") r = r(1:n - 3)
            end if

            n = len(r)

            if (n >= 5) then
                if (lower(r(n - 3:n)) == "info" .and. (r(n - 4:n - 4) == "-" .or. r(n - 4:n - 4) == ".")) r = r(1:n - 5)
            end if
        end function strip
    end subroutine info_filter_manual_names

    recursive subroutine cmd_info_display_manual(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: names(:), filtered(:)
        character(len=:), allocatable :: manual

        call info_initialize()
        call info_manual_names(.not. nilp(current_prefix_arg), names)
        call info_filter_manual_names(names, filtered)
        call completing_read("Manual name: ", completion_list_table(strings_value(filtered)), manual, status, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        call info_display_manual(manual, status)
    end subroutine cmd_info_display_manual

    ! ------------------------------------------------------------------
    ! The table of contents and the history node
    ! ------------------------------------------------------------------

    ! History-specific implementation of `Info-find-node-2'.  Port of
    ! info.el:Info-history-find-node.
    recursive subroutine history_find_node(filename, nodename, no_going_back, status)
        character(len=*), intent(in) :: filename, nodename
        logical, intent(in) :: no_going_back
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, node, dir
        type(status_type) :: st
        integer(int64) :: start
        integer :: i

        status = status_type()
        if (no_going_back) continue
        call insert(LF//US//LF//"File: "//filename//",  Node: "//nodename//",  Up: (dir)"//LF//LF, status)
        call insert("History of Visited Nodes"//LF, status)
        call insert("************************"//LF//LF, status)
        call insert("* Menu:"//LF//LF, status)
        if (.not. allocated(info_history_list)) return

        do i = 1, size(info_history_list)
            file = info_history_list(i)%file
            node = info_history_list(i)%node
            if (file == "*History*" .and. node == "Top") cycle
            call insert("* "//node//": (", st)
            dir = file_name_directory(file)
            start = point()
            call insert(dir, st)
            if (len(dir) > 0) call put_text_property(start, point(), sym("invisible"), t_value(), st)
            call insert(file_name_nondirectory(file)//")"//node//"."//LF, st)
        end do
    end subroutine history_find_node

    !> Go to a node with a menu of visited nodes.  Port of
    !> info.el:Info-history.
    recursive subroutine info_history(status)
        type(status_type), intent(out) :: status

        call info_find_node("*History*", "Top", status)
        if (.not. status%ok()) return
        call info_next_reference(status)
        if (.not. status%ok()) return
        call info_next_reference(status)
    end subroutine info_history

    ! Toc-specific implementation of `Info-find-node-2'.  Port of
    ! info.el:Info-toc-find-node.
    recursive subroutine toc_find_node(filename, nodename, no_going_back, status)
        character(len=*), intent(in) :: filename, nodename
        logical, intent(in) :: no_going_back
        type(status_type), intent(out) :: status
        type(toc_node_type), allocatable :: node_list(:)
        type(status_type) :: st
        integer :: saved_hide, k
        logical :: saved_visited

        status = status_type()
        if (no_going_back) continue
        call info_toc_nodes(filename, node_list)
        call insert(LF//US//LF//"File: "//filename//",  Node: "//nodename//",  Up: Top"//LF//LF, status)
        call insert("Table of Contents"//LF, status)
        call insert("*****************"//LF//LF, status)
        call insert("*Note Top::"//LF, status)
        ! Get Top nodes
        k = toc_index(node_list, "Top")

        if (k > 0) then
            if (allocated(node_list(k)%children)) call toc_insert(node_list(k)%children, node_list, 0, filename)
        end if

        if (.not. bobp()) then
            saved_hide = info_hide_note_references
            saved_visited = info_fontify_visited_nodes%value
            info_hide_note_references = INFO_HIDE_NOTE_REFERENCES_HIDE
            info_fontify_visited_nodes%value = .false.
            call set_current_file(filename)
            call set_current_node("*TOC*")
            call goto_min()

            if (refwd(LF//"["//US//FF//"]"//LF)) then
                call narrow_to_region(point(), point_max(), st)
            else
                call narrow_to_region(point_min(), point_max(), st)
            end if

            call info_fontify_node()
            call widen()
            info_hide_note_references = saved_hide
            info_fontify_visited_nodes%value = saved_visited
        end if
    end subroutine toc_find_node

    ! The index of the node NAME in NODES (0 if none).  Not a port.
    integer function toc_index(nodes, name) result(k)
        type(toc_node_type), intent(in) :: nodes(:)
        character(len=*), intent(in) :: name

        do k = 1, size(nodes)
            if (nodes(k)%name == name) return
        end do

        k = 0
    end function toc_index

    !> Go to a node with table of contents of the current Info file.
    !> Table of contents is created from the tree structure of menus.
    !> Port of info.el:Info-toc.
    recursive subroutine info_toc(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prev_node
        integer(int64) :: p
        integer :: k

        call info_find_node(current_file_or_empty(), "*TOC*", status)
        if (.not. status%ok()) return
        k = here()
        prev_node = ""
        if (size(locals(k)%history) > 0) prev_node = locals(k)%history(1)%node
        call goto_min()
        p = 0
        if (sfwd("*Note "//prev_node//":")) p = point() - len(prev_node) - 2

        if (p > 0) then
            call goto_char(p)
        else
            call goto_min()
        end if
    end subroutine info_toc

    ! Insert table of contents with references to nodes.  Port of
    ! info.el:Info-toc-insert.
    recursive subroutine toc_insert(nodes, node_list, level, curr_file)
        type(string_type), intent(in) :: nodes(:)
        type(toc_node_type), intent(in) :: node_list(:)
        integer, intent(in) :: level
        character(len=*), intent(in) :: curr_file
        character(len=:), allocatable :: section, indentation
        type(status_type) :: st
        integer :: i, k

        section = "Top"
        indentation = repeat(TAB, level)

        do i = 1, size(nodes)
            k = toc_index(node_list, nodes(i)%s)

            if (k > 0) then
                if (node_list(k)%has_section) then
                    if (node_list(k)%section /= section .and. &
                        .not. (node_list(k)%has_parent .and. parent_is_section(node_list(k)))) then
                        section = node_list(k)%section
                        call insert(indentation//section//LF, st)
                    end if
                end if
            end if

            call insert(indentation//"*Note "//nodes(i)%s//":: "//LF, st)

            if (k > 0) then
                if (allocated(node_list(k)%children)) then
                    call toc_insert(node_list(k)%children, node_list, level + 1, curr_file)
                end if
            end if
        end do

    contains

        ! (equal (nth 1 node) (nth 2 node)).
        logical function parent_is_section(n)
            type(toc_node_type), intent(in) :: n

            parent_is_section = n%parent == n%section
        end function parent_is_section
    end subroutine toc_insert

    ! Build table of contents from menus of Info FILE and its subfiles.
    ! Port of info.el:Info-toc-build.
    recursive subroutine toc_build(file, nodes)
        character(len=*), intent(in) :: file
        type(toc_node_type), allocatable, intent(out) :: nodes(:)
        character(len=:), allocatable :: found, main_file, nodename, upnode, menu_node_name, section
        type(string_type), allocatable :: subfiles(:), menu_items(:), section_names(:), section_of(:)
        type(handle_type) :: old, temp
        type(status_type) :: st
        integer(int64) :: bound, opoint, detail
        logical :: has_main, has_up

        allocate (nodes(0), section_names(0), section_of(0))
        call add_string(section_names, "Top")
        call add_string(section_of, "Top")
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st, inhibit_buffer_hooks=.true.)
        call set_buffer(temp, st)
        call info_find_file(file, found, st, noerror=.true.)

        if (allocated(found)) then
            buffers(cur)%directory = file_name_directory(found)
            main_file = found
            has_main = .true.
        else
            has_main = .false.
        end if

        allocate (subfiles(0))

        do while (has_main .or. size(subfiles) > 0)
            call erase_buffer(st)

            if (has_main) then
                call info_insert_file_contents(main_file, st)
            else
                call info_insert_file_contents(subfiles(1)%s, st)
            end if

            call goto_min()

            do
                if (.not. sfwd(LF//US//LF//"File:", move=.true.)) exit
                if (.not. sfwd("Node: ", move=.true.)) exit
                nodename = following_node_name()
                opoint = point()
                bound = point_max()
                if (sfwd(LF//US)) bound = point()
                bound = bound - 2
                call goto_char(opoint)
                has_up = refwd("Up:"//following_node_name_re(), bound)
                if (has_up) upnode = match_string_no_properties(1)

                if (has_up) then
                    if (index(upnode, "(") > 0) has_up = .false.
                end if

                section = "Top"
                allocate (menu_items(0))

                if (.not. info_index_node(nodename, file)) then
                    if (refwd("^\* Menu:", bound)) then
                        call fwd_line(1_int64)
                        call beginning_of_line(1_int64)

                        if (nodename == "Top") then
                            opoint = point()

                            if (refwd("^[ "//TAB//"—-]*The Detailed Node Listing")) then
                                detail = point()
                                bound = detail
                            end if

                            call goto_char(opoint)
                        end if

                        do while (point() < bound)
                            if (looking("^\* +[^:]+:")) then
                                ! Menu item line
                                call beginning_of_line(1_int64)
                                call goto_char(point() + 2)
                                call info_extract_menu_node_name(menu_node_name)
                                call add_string(menu_items, menu_node_name)

                                if (nodename == "Top") then
                                    call add_string(section_names, menu_node_name)
                                    call add_string(section_of, section)
                                end if
                            else if (nodename == "Top" .and. looking("^\([^ "//TAB//LF//"*=.-][^:"//LF//"]*\)")) then
                                ! Other non-empty strings in the Top node are
                                ! section names
                                section = match_string_no_properties(1)
                            end if

                            call fwd_line(1_int64)
                            call beginning_of_line(1_int64)
                        end do
                    end if
                end if

                call add_toc_node(nodes, nodename, has_up, upnode, menu_items)
                deallocate (menu_items)
                call goto_char(bound)
            end do

            if (has_main) then
                opoint = point()
                call goto_min()

                if (sfwd(LF//US//LF//"Indirect:")) then
                    opoint = point()
                    bound = point_max()
                    if (sfwd(LF//US)) bound = point()
                    call goto_char(opoint)

                    do while (refwd("^\(.*\): [0-9]+$", bound))
                        call add_string(subfiles, match_string_no_properties(1))
                    end do
                end if

                has_main = .false.
            else
                subfiles = subfiles(2:)
            end if
        end do

        call set_buffer(old, st)
        call kill_buffer(temp, st)
        call set_sections()

    contains

        ! (push (list NODENAME UPNODE nil MENU-ITEMS) NODES), in order.
        subroutine add_toc_node(list, name, has_parent, parent, children)
            type(toc_node_type), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: name
            logical, intent(in) :: has_parent
            character(len=:), allocatable, intent(in) :: parent
            type(string_type), intent(in) :: children(:)
            type(toc_node_type), allocatable :: grown(:)
            integer :: m

            allocate (grown(size(list) + 1))

            do m = 1, size(list)
                grown(m) = list(m)
            end do

            m = size(list) + 1
            grown(m)%name = name
            grown(m)%has_parent = has_parent
            if (has_parent) grown(m)%parent = parent
            grown(m)%children = children
            call move_alloc(grown, list)
        end subroutine add_toc_node

        ! The section of each node: (cadr (assoc nodename sections)),
        ! the most recent entry winning as with assoc on a pushed list.
        subroutine set_sections()
            integer :: m, j

            do m = 1, size(nodes)
                do j = size(section_names), 1, -1
                    if (section_names(j)%s == nodes(m)%name) then
                        nodes(m)%has_section = .true.
                        nodes(m)%section = section_of(j)%s
                        exit
                    end if
                end do
            end do
        end subroutine set_sections
    end subroutine toc_build

    ! Return in NODES a node list of Info FILENAME with parent-children
    ! information.  This information is cached in `Info-toc-nodes' with
    ! the help of `toc_build'.  Port of info.el:Info-toc-nodes.
    recursive subroutine info_toc_nodes(filename, nodes)
        character(len=*), intent(in) :: filename
        type(toc_node_type), allocatable, intent(out) :: nodes(:)
        type(toc_file_type), allocatable :: grown(:)
        integer :: i, k

        k = virtual_file_index(filename)

        if (k > 0) then
            if (associated(info_virtual_files(k)%toc_nodes)) then
                call info_virtual_files(k)%toc_nodes(filename, nodes)
                return
            end if
        end if

        if (.not. allocated(toc_cache)) allocate (toc_cache(0))

        do i = 1, size(toc_cache)
            if (toc_cache(i)%file == filename) then
                nodes = toc_cache(i)%nodes
                return
            end if
        end do

        ! Skip virtual Info files
        if (info_virtual_file_p(filename)) then
            allocate (nodes(0))
        else
            ! Scan the entire manual and cache the result in Info-toc-nodes
            call toc_build(filename, nodes)
        end if

        allocate (grown(size(toc_cache) + 1))
        grown(1)%file = filename
        grown(1)%nodes = nodes

        do i = 1, size(toc_cache)
            grown(i + 1) = toc_cache(i)
        end do

        call move_alloc(grown, toc_cache)
    end subroutine info_toc_nodes

    ! ------------------------------------------------------------------
    ! The display of nodes
    ! ------------------------------------------------------------------

    ! The breadcrumbs line of the current node: links to its ancestors up
    ! to `info_breadcrumbs_depth' of them, from the Top node, as a string
    ! with faces (empty if none).  Port of info.el:Info-breadcrumbs.
    recursive function breadcrumbs() result(line)
        type(value_type) :: line
        type(toc_node_type), allocatable :: nodes(:)
        type(string_type), allocatable :: crumbs(:), rev(:)
        character(len=:), allocatable :: node, current, txt, manual
        type(value_type) :: piece
        type(status_type) :: st
        logical :: has_line, has_node
        integer :: depth, i, k

        call info_toc_nodes(current_file_or_empty(), nodes)
        current = current_node_or_empty()
        node = current
        has_node = .true.
        allocate (rev(0))
        depth = info_breadcrumbs_depth

        ! Get ancestors from the cached parent-children node info
        do while (has_node .and. node /= "Top" .and. depth > 0)
            k = toc_index(nodes, node)
            has_node = .false.

            if (k > 0) then
                if (nodes(k)%has_parent) then
                    node = nodes(k)%parent
                    has_node = .true.
                    call add_string(rev, node)
                end if
            end if

            depth = depth - 1
        end do

        ! (push node crumbs): the farthest ancestor first.
        allocate (crumbs(0))

        do i = size(rev), 1, -1
            call add_string(crumbs, rev(i)%s)
        end do

        ! Add bottom node.
        ! Let it disappear if crumbs is nil.
        if (info_use_header_line%value .and. size(crumbs) > 0) call add_string(crumbs, current)
        line = make_string("")
        has_line = .false.

        if (info_use_header_line%value .or. size(crumbs) > 0) then
            ! Add top node (and continuation if needed).
            call add_top(crumbs)
            ! Eliminate duplicate.
            call fwd_line(1_int64)
            manual = file_name_sans_extension(file_name_nondirectory(current_file_or_empty()))

            do i = 1, size(crumbs)
                if (crumbs(i)%s /= "Top") then
                    txt = crumbs(i)%s
                else
                    txt = "("//manual//")Top"
                end if

                if (has_line) then
                    line = propertized_concat(line, make_string(" > "))
                end if

                if (crumbs(i)%s == achar(0)) then
                    piece = make_string("...")
                else if (crumbs(i)%s == current) then
                    ! No point linking to ourselves.
                    piece = make_string(txt)
                    call string_put_text_property(0_int64, nchars_of(txt), sym("font_lock_face"), &
                                                  make_symbol_ref(intern("info_header_node")), piece, st)
                else
                    piece = make_string(txt)
                    call string_put_text_property(0_int64, nchars_of(txt), sym("mouse_face"), &
                                                  make_symbol_ref(intern("highlight")), piece, st)
                    call string_put_text_property(0_int64, nchars_of(txt), sym("font_lock_face"), &
                                                  make_symbol_ref(intern("info_header_xref")), piece, st)
                    call string_put_text_property(0_int64, nchars_of(txt), sym("help_echo"), &
                                                  make_string("mouse-2: Go to node"), piece, st)
                    call string_put_text_property(0_int64, nchars_of(txt), sym("link_args"), &
                                                  make_string(txt), piece, st)
                end if

                line = propertized_concat(line, piece)
                has_line = .true.
            end do

            line = propertized_concat(line, make_string(LF))
        end if

    contains

        ! (cons "Top" (if (member (pop crumbs) '(nil "Top")) crumbs (cons
        ! nil crumbs))), nil being NUL.
        subroutine add_top(list)
            type(string_type), allocatable, intent(inout) :: list(:)
            type(string_type), allocatable :: rest(:)
            integer :: m
            logical :: first_is_top

            first_is_top = .true.
            if (size(list) > 0) first_is_top = list(1)%s == "Top"
            allocate (rest(0))
            call add_string(rest, "Top")
            if (.not. first_is_top) call add_string(rest, achar(0))

            do m = 2, size(list)
                call add_string(rest, list(m)%s)
            end do

            call move_alloc(rest, list)
        end subroutine add_top
    end function breadcrumbs

    ! The number of characters of the UTF-8 text S.  Not a port.
    integer(int64) function nchars_of(s)
        type(status_type) :: st
        character(len=*), intent(in) :: s

        call multibyte_chars_in_text(s, nchars_of, st)
    end function nchars_of

    !> Fontify the node.  Port of info.el:Info-fontify-node
    !> (`info_refill_paragraphs' and the keymaps of the links, which only
    !> the mouse uses, are Deferred).
    recursive subroutine info_fontify_node()
        type(value_type) :: saved_ro, crumbs, prop
        type(handle_type) :: ov
        type(status_type) :: st
        character(len=:), allocatable :: tag, header, manual, other_tag, s1, node
        integer(int64) :: opoint, where, nbeg, nend, tbeg, header_end, start, next, rbeg, rend, b1, e1, mb, me
        integer(int64) :: limit, fncount, footnote_num, n, p0
        logical :: fold, not_fontified_p, fontify_visited_p, found, cont, ok, external_link_p
        integer :: hide, i, ios
        character :: c

        opoint = point()
        saved_ro = bind_inhibit_read_only()
        fold = bind_case_fold(.true.)
        ! the node hasn't already been fontified
        call next_single_property_change(point_min(), sym("font_lock_face"), where, found, st)
        not_fontified_p = .true.
        if (found) not_fontified_p = where == point_max()
        ! visited nodes need to be re-fontified
        fontify_visited_p = .false.

        if (info_fontify_visited_nodes%value .and. info_fontify_maximum_menu_size /= INFO_FONTIFY_MAXIMUM_MENU_SIZE_NIL) then
            ! Don't take time to refontify visited nodes in huge nodes
            fontify_visited_p = info_fontify_maximum_menu_size == INFO_FONTIFY_MAXIMUM_MENU_SIZE_T .or. &
                                point_max() - point_min() < info_fontify_maximum_menu_size
        end if

        ! Disable Info-hide-note-references in nodes that are incompatible
        ! with that feature.
        hide = info_hide_note_references
        manual = file_name_sans_extension(file_name_nondirectory(current_file_or_empty()))

        if (lower(manual) == "texinfo" .and. current_node_or_empty() == "Cross Reference Commands") then
            hide = INFO_HIDE_NOTE_REFERENCES_NIL
        end if

        ! Fontify header line
        call goto_min()

        if (not_fontified_p .and. looking("^\(File: [^,: "//TAB//"]+,?[ "//TAB//"]+\)?")) then
            call goto_char(mend(0))

            do while (looking("[ "//TAB//"]*\([^:, "//TAB//LF//"]+\):[ "//TAB//"]+\([^:,"//TAB//LF//"]+\),?"))
                call goto_char(mend(0))
                nbeg = mbeg(2)
                nend = mend(2)
                tbeg = mbeg(1)
                tag = match_string_no_properties(1)

                if (lower(tag) == "node") then
                    call put_text_property(nbeg, nend, sym("font_lock_face"), symbol_value_of("info_header_node"), st)
                else
                    call put_text_property(nbeg, nend, sym("font_lock_face"), symbol_value_of("info_header_xref"), st)
                    call put_text_property(tbeg, nend, sym("mouse_face"), symbol_value_of("header_line_highlight"), st)
                    call put_text_property(tbeg, nend, sym("help_echo"), &
                                           make_string("mouse-2: Go to node "//text(nbeg, nend)), st)
                    ! (The keymap of the link, used by the mouse, is
                    ! Deferred.)  A symbol value of the `link-args'
                    ! property is handled specially by
                    ! `Info-mouse-follow-link'.
                    call put_text_property(tbeg, nend, sym("link_args"), symbol_value_of(lower(tag)), st)
                end if
            end do

            ! Treat header line.
            if (info_use_header_line%value) then
                call goto_min()
                header_end = line_end_position(1_int64)

                ! If we find neither Next: nor Prev: link, show the entire
                ! node header.  Otherwise, don't show the File: and Node:
                ! parts, to avoid wasting precious space on information that
                ! is available in the mode line.
                if (refwd("\(next\|up\|prev[ious]*\): ", header_end)) then
                    call goto_char(mbeg(1))
                    header = text(point(), header_end)
                    call note_header(point(), header_end, "")
                else if (refwd("node:[ "//TAB//"]*[^ "//TAB//"]+[ "//TAB//"]*", header_end)) then
                    header = "No next, prev or up links  --  "//text(point(), header_end)
                    call note_header(point(), header_end, "No next, prev or up links  --  ")
                else
                    header = text(point(), header_end)
                    call note_header(point(), header_end, "")
                end if

                call put_text_property(point_min(), point_min() + 1, sym("header_line"), &
                                       make_string(replace_all("%", "%%", header)), st)

                ! Hide the part of the first line that is in the header, if
                ! it is just part.
                if (info_breadcrumbs_depth > 0) then
                    crumbs = breadcrumbs()
                    call make_overlay(ov, point_min(), header_end + 1, st)
                    call overlay_put(ov, "display", crumbs, st)
                    call overlay_put(ov, "evaporate", t_value(), st)
                else if (.not. bobp()) then
                    ! Hide the punctuation at the end, too.
                    p0 = skip_chars_backward(" "//TAB//",", st)
                    call put_text_property(point(), header_end, sym("invisible"), t_value(), st)
                    ! Hide the suffix of the Info file name.
                    call beginning_of_line(1_int64)

                    if (refwd("File: "//manual//"\([^,"//LF//TAB//"]+\),", header_end)) then
                        call put_text_property(mbeg(1), mend(1), sym("invisible"), t_value(), st)
                    end if
                end if
            end if
        end if

        ! Fontify titles
        call goto_min()

        if (font_lock_mode_enabled() .and. not_fontified_p) then
            do while (refwd(LF//"\([^ "//TAB//LF//"].+\)"//LF//"\(\*\*+\|==+\|--+\|\.\.+\)$"))
                ! Only consider it as an underlined title if the ASCII
                ! underline has the same size as the text.  A typical
                ! counter example is when a continuation "..." is alone on
                ! a line.
                if (string_width(match_string_no_properties(1)) == string_width(match_string_no_properties(2))) then
                    c = text(point() - 1, point())

                    select case (c)
                    case ("*")
                        prop = symbol_value_of("info_title_1")
                    case ("=")
                        prop = symbol_value_of("info_title_2")
                    case ("-")
                        prop = symbol_value_of("info_title_3")
                    case default
                        prop = symbol_value_of("info_title_4")
                    end select

                    call put_text_property(mbeg(1), mend(1), sym("font_lock_face"), prop, st)

                    ! This is a serious problem for trying to handle
                    ! multiple frame types at once.  We want this text to
                    ! be invisible on frames that can display the font
                    ! above.
                    if (frame_window_p(selected_frame)) then
                        b1 = mbeg(2) - 1
                        e1 = mend(2)
                        call put_text_property(b1, e1, sym("invisible"), t_value(), st)
                        call put_text_property(b1, e1, sym("front_sticky"), nil(), st)
                        call put_text_property(b1, e1, sym("rear_nonsticky"), t_value(), st)
                    end if
                end if
            end do
        end if

        ! Fontify cross references
        call goto_min()

        if (not_fontified_p .or. fontify_visited_p) then
            do while (refwd("\(\*Note["//" "//LF//TAB//"]+\)\([^:]*\)\(:[ "//TAB//"]*\([^.,:(]*\)\(\(([^)]*)\)" &
                            //"[^.,:]*\)?[,:]?"//LF//"?\)"))
                call cross_reference()
            end do
        end if

        ! (Refilling paragraphs, `info_refill_paragraphs', is Deferred.)
        ! Fontify menu items
        call goto_min()
        ok = not_fontified_p .or. fontify_visited_p
        if (ok) ok = sfwd(LF//"* Menu:")

        if (ok) then
            ! Don't take time to annotate huge menus
            ok = info_fontify_maximum_menu_size /= INFO_FONTIFY_MAXIMUM_MENU_SIZE_NIL
            if (ok) ok = info_fontify_maximum_menu_size == INFO_FONTIFY_MAXIMUM_MENU_SIZE_T .or. &
                         point_max() - point() < info_fontify_maximum_menu_size
        end if

        if (ok) then
            n = 0

            do while (refwd("^\* Menu:\|\(?:^\* +\("//INFO_MENU_ENTRY_NAME_RE//"\)\(:"//node_spec_re() &
                            //"\([ "//TAB//"]*\)\)\)"))
                if (mbeg(1) > 0) call menu_item()
            end do
        end if

        ! Fontify menu headers
        ! Add the face `info-menu-header' to any header before a menu entry
        call goto_min()

        if (not_fontified_p .and. refwd("^\* Menu:")) then
            call put_text_property(mbeg(0), mend(0), sym("font_lock_face"), symbol_value_of("info_menu_header"), st)

            do while (refwd(LF//LF//"\([^*"//LF//" ].*\)"//LF//LF//"?[*]"))
                call put_text_property(mbeg(1), mend(1), sym("font_lock_face"), symbol_value_of("info_menu_header"), st)
            end do
        end if

        ! Hide index line numbers
        call goto_min()

        if (not_fontified_p) then
            if (info_index_node()) then
                do while (refwd("[ "//TAB//LF//"]*(line +[0-9]+)"))
                    call put_text_property(mbeg(0), mend(0), sym("invisible"), t_value(), st)
                end do
            end if
        end if

        ! Fontify http and ftp references
        call goto_min()

        if (not_fontified_p) then
            do while (refwd("\(https?\|ftp\)://[^ "//TAB//LF//"""`‘({<>})’']+"))
                call put_text_property(mbeg(0), mend(0), sym("font_lock_face"), symbol_value_of("info_xref"), st)
                call put_text_property(mbeg(0), mend(0), sym("mouse_face"), symbol_value_of("highlight"), st)
                call put_text_property(mbeg(0), mend(0), sym("help_echo"), make_string("mouse-2: go to this URL"), st)
            end do
        end if

        ! Fontify footnotes
        call goto_min()

        if (not_fontified_p .and. refwd("^[ "//TAB//"]*-+ Footnotes -+$")) then
            limit = point()
            fncount = 0

            ! How many footnotes do we have in this node?
            do while (refwd("^ [ "//TAB//"]*([0-9]+) "))
                fncount = fncount + 1
            end do

            call goto_min()

            do while (refwd("\((\([0-9]+\))\)"))
                s1 = match_string_no_properties(2)
                read (s1, *, iostat=ios) footnote_num
                if (ios /= 0) footnote_num = 0

                ! Don't fontify parenthesized numbers that cannot possibly
                ! be one of this node's footnotes.  This still doesn't
                ! catch unrelated numbers that happen to be small enough,
                ! but in that case they should use "@footnotestyle
                ! separate" in the Texinfo sources.
                if (footnote_num > 0 .and. footnote_num <= fncount) then
                    mb = mbeg(0)
                    me = mend(0)
                    call put_text_property(mb, me, sym("font_lock_face"), symbol_value_of("info_xref"), st)
                    call put_text_property(mb, me, sym("link"), t_value(), st)
                    call put_text_property(mb, me, sym("mouse_face"), symbol_value_of("highlight"), st)

                    if (point() < limit) then
                        call put_text_property(mb, me, sym("help_echo"), &
                                               make_string("mouse-2: go to footnote definition"), st)
                    else
                        call put_text_property(mb, me, sym("help_echo"), &
                                               make_string("mouse-2: go to footnote reference"), st)
                    end if
                end if
            end do
        end if

        ! Hide empty lines at the end of the node.
        call goto_char(point_max())
        p0 = skip_chars_backward(LF, st)
        if (point() < point_max() - 1) call put_text_property(point(), point_max() - 1, sym("invisible"), t_value(), st)
        call restore_buffer_modified_p(.false.)
        fold = bind_case_fold(fold)
        call unbind_inhibit_read_only(saved_ro)
        call goto_char(min(max(opoint, point_min()), point_max()))

    contains

        ! Record the header line made of the text from B to E, after
        ! PREFIX, with its faces (the `header-line' property, which
        ! `update_header_line' shows).
        subroutine note_header(b, e, prefix)
            integer(int64), intent(in) :: b, e
            character(len=*), intent(in) :: prefix
            type(header_line_format_type) :: h
            type(value_type) :: f
            integer(int64) :: pos, nxt
            logical :: has
            integer :: k

            if (len(prefix) > 0) call h%add(prefix)
            pos = b

            do while (pos < e)
                call next_single_property_change(pos, sym("font_lock_face"), nxt, has, st, e)
                if (.not. has .or. nxt > e) nxt = e
                call get_text_property(pos, sym("font_lock_face"), f, st)

                if (symbolp(f)) then
                    call h%add(text(pos, nxt), symbol_name_of(f))
                else
                    call h%add(text(pos, nxt))
                end if

                pos = nxt
            end do

            if (.not. allocated(h%segments)) call h%add("")
            k = here()
            locals(k)%header = h
        end subroutine note_header

        ! Fontify the cross reference matched.
        subroutine cross_reference()
            integer(int64) :: m(0:6, 2)
            integer :: g
            character(len=:), allocatable :: m4, m5, m2
            logical :: fold2

            do g = 0, 6
                m(g, 1) = mbeg(g)
                m(g, 2) = mend(g)
            end do

            start = m(0, 1)
            next = point()
            m2 = text(m(2, 1), m(2, 2))
            m4 = ""
            if (m(4, 1) > 0) m4 = text(m(4, 1), m(4, 2))
            m5 = ""
            if (m(5, 1) > 0) m5 = text(m(5, 1), m(5, 2))

            if (not_fontified_p) then
                if (hide /= INFO_HIDE_NOTE_REFERENCES_NIL) then
                    other_tag = achar(0)

                    if (hide /= INFO_HIDE_NOTE_REFERENCES_HIDE .and. line_number_at_pos() > 4) then
                        ! Skip breadcrumbs
                        ! *Note is often used where *note should have been
                        call goto_char(start)
                        p0 = skip_syntax_backward(" ")

                        if (point() > point_min()) then
                            c = text(point() - 1, point())

                            if (c == "(" .or. c == "[" .or. c == "{") then
                                ! Check whether the paren is preceded by an end
                                ! of sentence
                                p0 = skip_syntax_backward(" (")
                            end if
                        end if

                        if (looking_back("\(^\|[ (]\)see", st, point() - 4)) then
                            other_tag = ""
                        else
                            ! We want "Also *note" to produce "Also see", but
                            ! "See also *note" to produce "See also", so match
                            ! case-sensitively.
                            fold2 = bind_case_fold(.false.)
                            ok = looking_back("\(^\| \)also", st, point() - 5)
                            fold2 = bind_case_fold(fold2)

                            if (ok) then
                                other_tag = ""
                            else if (looking_back("\(^\| \)in", st, point() - 3)) then
                                other_tag = ""
                            else
                                c = " "
                                if (point() > point_min()) c = text(point() - 1, point())

                                if (point() == point_min() .or. c == "." .or. c == "!" .or. c == "?") then
                                    other_tag = "See "
                                else if (sfwd(LF//LF, start)) then
                                    other_tag = "See "
                                else
                                    other_tag = "see "
                                end if
                            end if
                        end if
                    end if

                    call goto_char(next)
                    ! Don't hide \n after *Note
                    e1 = m(1, 2)
                    s1 = text(m(1, 1), m(1, 2))
                    i = index(s1, LF)
                    if (i > 0) e1 = m(1, 1) + string_char_count(s1, i)

                    if (other_tag /= achar(0)) then
                        call put_text_property(m(1, 1), e1, sym("display"), make_string(other_tag), st)
                    else
                        call put_text_property(m(1, 1), e1, sym("invisible"), t_value(), st)
                    end if

                    call put_text_property(m(1, 1), e1, sym("front_sticky"), nil(), st)
                    call put_text_property(m(1, 1), e1, sym("rear_nonsticky"), t_value(), st)
                end if

                if (m(5, 2) > 0 .or. len(m4) > 0) then
                    if (len(m5) > 0) then
                        s1 = "mouse-2: go to "//m5
                    else
                        s1 = "mouse-2: go to "//m4
                    end if
                else
                    s1 = "mouse-2: go to this node"
                end if

                call put_text_property(m(2, 1), m(2, 2), sym("help_echo"), make_string(s1), st)
                call put_text_property(m(2, 1), m(2, 2), sym("mouse_face"), symbol_value_of("highlight"), st)
            end if

            rbeg = m(2, 1)
            rend = m(2, 2)

            ! Display visited nodes in a different face
            if (len(m5) > 0) then
                node = m5
            else if (len(m4) > 0) then
                node = m4
            else
                node = m2
            end if

            node = trim_leading_space(canonicalize_whitespace(node))

            if (info_fontify_visited_nodes%value .and. visited(node)) then
                call put_text_property(rbeg, rend, sym("font_lock_face"), symbol_value_of("info_xref_visited"), st)
            else
                call put_text_property(rbeg, rend, sym("font_lock_face"), symbol_value_of("info_xref"), st)
            end if

            ! For multiline ref, unfontify newline and surrounding whitespace
            p0 = point()
            call goto_char(rbeg)

            do while (refwd("\s-*"//LF//"\s-*", rend))
                call remove_property(mbeg(0), mend(0), "font_lock_face")
            end do

            call goto_char(p0)

            if (not_fontified_p) then
                if (hide == INFO_HIDE_NOTE_REFERENCES_T .or. hide == INFO_HIDE_NOTE_REFERENCES_HIDE) then
                    call put_text_property(m(3, 1), m(3, 2), sym("invisible"), t_value(), st)
                    call put_text_property(m(3, 1), m(3, 2), sym("front_sticky"), nil(), st)
                    call put_text_property(m(3, 1), m(3, 2), sym("rear_nonsticky"), t_value(), st)

                    ! Unhide the file name of the external reference in parens
                    if (m(6, 1) > 0 .and. hide /= INFO_HIDE_NOTE_REFERENCES_HIDE) then
                        call remove_property(m(6, 1), m(6, 2), "invisible")
                        call remove_property(m(6, 1), m(6, 2), "front_sticky")
                        call remove_property(m(6, 1), m(6, 2), "rear_nonsticky")
                    end if

                    ! Unhide newline because hidden newlines cause too long
                    ! lines
                    p0 = point()
                    call goto_char(m(3, 1))

                    if (refwd(LF//"[ "//TAB//"]*", m(3, 2))) then
                        b1 = mbeg(0)
                        e1 = mend(0)
                        call goto_char(m(3, 2) + 1)

                        if (.not. looking("[.)]*$")) then
                            call remove_property(b1, e1, "invisible")
                            call remove_property(b1, e1, "front_sticky")
                            call remove_property(b1, e1, "rear_nonsticky")
                        end if
                    end if

                    call goto_char(p0)
                end if
            end if
        end subroutine cross_reference

        ! Fontify the menu item matched.
        subroutine menu_item()
            integer(int64) :: m(0:6, 2), ln_start
            integer :: g
            character(len=:), allocatable :: m3

            do g = 0, 6
                m(g, 1) = mbeg(g)
                m(g, 2) = mend(g)
            end do

            m3 = ""
            if (m(3, 1) > 0) m3 = text(m(3, 1), m(3, 2))

            if (not_fontified_p) then
                n = n + 1

                ! visual aids to help with 1-9 keys
                if (n <= 9 .and. mod(n, 3_int64) == 0) then
                    call put_text_property(m(0, 1), m(0, 1) + 1, sym("font_lock_face"), &
                                           symbol_value_of("info_menu_star"), st)
                end if

                if (m(3, 2) > 0 .and. len(m3) > 0) then
                    s1 = "mouse-2: go to "//m3
                else
                    s1 = "mouse-2: go to this node"
                end if

                call put_text_property(m(1, 1), m(1, 2), sym("help_echo"), make_string(s1), st)
                call put_text_property(m(1, 1), m(1, 2), sym("mouse_face"), symbol_value_of("highlight"), st)
            end if

            ! Display visited menu items in a different face
            if (len(m3) == 0) then
                node = text(m(1, 1), m(1, 2))
            else
                node = m3
            end if

            if (info_fontify_visited_nodes%value .and. visited(node)) then
                call put_text_property(m(1, 1), m(1, 2), sym("font_lock_face"), symbol_value_of("info_xref_visited"), st)
            else
                call put_text_property(m(1, 1), m(1, 2), sym("font_lock_face"), symbol_value_of("info_xref"), st)
            end if

            if (not_fontified_p .and. (hide == INFO_HIDE_NOTE_REFERENCES_T .or. hide == INFO_HIDE_NOTE_REFERENCES_HIDE)) then
                if (.not. info_index_node()) then
                    call put_text_property(m(2, 1), m(6, 2) - 1, sym("invisible"), t_value(), st)

                    ! Unhide the file name in parens
                    if (m(4, 2) > 0) then
                        if (text(m(4, 2), m(4, 2) + 1) /= ".") call remove_property(m(4, 1), m(4, 2), "invisible")
                    end if

                    ! We need a stretchable space like :align-to but with a
                    ! minimum value.
                    if (m(1, 2) - m(0, 1) <= 22) then
                        call put_text_property(m(6, 2) - 1, m(6, 2), sym("display"), space_spec_value("align_to", 24), st)
                    else
                        call put_text_property(m(6, 2) - 1, m(6, 2), sym("display"), space_spec_value("width", 2), st)
                    end if

                    cont = looking(".")

                    do
                        if (forward_line(1_int64) /= 0) exit
                        if (.not. looking("\([ "//TAB//"]+\)[^*"//LF//"]")) exit
                        ln_start = mbeg(1)
                        call put_text_property(ln_start, mend(1) - 1, sym("invisible"), t_value(), st)

                        if (cont) then
                            call put_text_property(mend(1) - 1, mend(1), sym("display"), space_spec_value("align_to", 26), st)
                        else
                            call put_text_property(mend(1) - 1, mend(1), sym("display"), space_spec_value("align_to", 24), st)
                        end if

                        cont = .true.
                    end do
                end if
            end if
        end subroutine menu_item

        ! Whether NODE (of the current file, or "(FILE)NODE") is in
        ! `info_history_list'.
        logical function visited(name)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: nm, fl, hfile
            integer :: k

            nm = name
            external_link_p = smatch("(\([^)]+\))\([^)]*\)", nm, .true.)

            if (external_link_p) then
                fl = file_name_nondirectory(match_string_no_properties(1, nm))

                if (len(match_string_no_properties(2, nm)) == 0) then
                    nm = "Top"
                else
                    nm = match_string_no_properties(2, nm)
                end if
            else
                fl = current_file_or_empty()
            end if

            visited = .false.
            if (.not. allocated(info_history_list)) return

            do k = 1, size(info_history_list)
                if (info_history_list(k)%node /= nm) cycle
                hfile = info_history_list(k)%file
                if (external_link_p) hfile = file_name_nondirectory(hfile)

                if (hfile == fl) then
                    visited = .true.
                    return
                end if
            end do
        end function visited
    end subroutine info_fontify_node

    ! S without its leading spaces and tabs.  Not a port.
    function trim_leading_space(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i

        i = verify(s, " "//TAB)

        if (i == 0) then
            t = ""
        else
            t = s(i:)
        end if
    end function trim_leading_space

    ! The number of characters in the first N bytes of the UTF-8 text S
    ! (of the text before byte N+1).  Not a port.
    integer(int64) function string_char_count(s, n) result(count)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n

        count = nchars_of(s(1:n - 1))
    end function string_char_count

    ! The symbol named NAME as a value.  Not a port.
    function symbol_value_of(name) result(v)
        character(len=*), intent(in) :: name
        type(value_type) :: v

        v = make_symbol_ref(intern(name))
    end function symbol_value_of

    ! The name of the symbol V.  Not a port.
    function symbol_name_of(v) result(name)
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: name

        name = trim(symbols(xsymbol(v))%name)
    end function symbol_name_of

    ! The display spec (space :KEY N).  Not a port.
    function space_spec_value(key, n) result(v)
        character(len=*), intent(in) :: key
        integer, intent(in) :: n
        type(value_type) :: v

        v = list(sym("space"), sym(":"//replace_all("_", "-", key)), make_fixnum(int(n, int64)))
    end function space_spec_value

    ! Remove the text property PROP from START to END.  Not a port.
    subroutine remove_property(start, end, prop)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: prop
        type(status_type) :: st
        logical :: changed

        call remove_text_properties(start, end, list(sym(prop), t_value()), changed, st)
    end subroutine remove_property

    ! Show the header line of the current node: the `header-line' property
    ! of the node's first character, as Info mode's header-line-format
    ! (:eval (get-text-property (point-min) 'header-line)) does, with the
    ! faces of the node's header.  Not a port.
    subroutine update_header_line()
        type(value_type) :: prop
        type(status_type) :: st
        integer :: k

        k = here()
        if (.not. locals(k)%header_from_property) return
        call get_text_property(point_min(), sym("header_line"), prop, st)

        if (nilp(prop) .or. .not. allocated(locals(k)%header%segments)) then
            call buffers(cur)%header_line_format%clear()
        else
            buffers(cur)%header_line_format = locals(k)%header
        end if
    end subroutine update_header_line

    ! Define the faces of Info.  Not a port.
    subroutine define_info_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR

        f => info_node_face
        call f%define("info_node", "Face for Info node names.")
        call f%spec_entry(display_when(classes=[C], backgrounds=[FACE_BACKGROUND_LIGHT]), &
                          face_attrs(foreground="brown", weight="bold", slant="italic"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[FACE_BACKGROUND_DARK]), &
                          face_attrs(foreground="white", weight="bold", slant="italic"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", slant="italic"))
        f => info_title_1_face
        call f%define("info_title_1", "Face for info titles at level 1.")
        call f%spec_entry(display_when(types=[character(len=3) :: "tty", "pc"], classes=[C], &
                                       backgrounds=[FACE_BACKGROUND_LIGHT]), face_attrs(foreground="green", weight="bold"))
        call f%spec_entry(display_when(types=[character(len=3) :: "tty", "pc"], classes=[C], &
                                       backgrounds=[FACE_BACKGROUND_DARK]), face_attrs(foreground="yellow", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(relative_height=1.2, inherits=[character(len=12) :: "info_title_2"]))
        f => info_title_2_face
        call f%define("info_title_2", "Face for info titles at level 2.")
        call f%spec_entry(display_when(types=[character(len=3) :: "tty", "pc"], classes=[C]), &
                          face_attrs(foreground="lightblue", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(relative_height=1.2, inherits=[character(len=12) :: "info_title_3"]))
        f => info_title_3_face
        call f%define("info_title_3", "Face for info titles at level 3.")
        call f%spec_entry(display_when(types=[character(len=3) :: "tty", "pc"], classes=[C]), face_attrs(weight="bold"))
        call f%spec_entry(display_when(), face_attrs(relative_height=1.2, inherits=[character(len=12) :: "info_title_4"]))
        f => info_title_4_face
        call f%define("info_title_4", "Face for info titles at level 4.")
        call f%spec_entry(display_when(types=[character(len=3) :: "tty", "pc"], classes=[C]), face_attrs(weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", inherits=[character(len=14) :: "variable_pitch"]))
        f => info_menu_header_face
        call f%define("info_menu_header", "Face for headers in Info menus.")
        call f%spec_entry(display_when(types=[character(len=3) :: "tty", "pc"]), face_attrs(underline=.true., weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", inherits=[character(len=14) :: "variable_pitch"]))
        f => info_menu_star_face
        call f%define("info_menu_star", "Face used to emphasize `*' in an Info menu."//LF// &
                      "The face is assigned to the third, sixth, and ninth `*' for easier"//LF// &
                      "orientation.  See `info_nth_menu_item'.")
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="red1"))
        call f%spec_entry(display_when(), face_attrs(underline=.true.))
        f => info_xref_face
        call f%define("info_xref", "Face for unvisited Info cross-references.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=4) :: "link"]))
        f => info_xref_visited_face
        call f%define("info_xref_visited", "Face for visited Info cross-references.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=12) :: "link_visited", "info_xref"]))
        f => info_header_xref_face
        call f%define("info_header_xref", "Face for Info cross-references in a node header.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=9) :: "info_xref"]))
        f => info_header_node_face
        call f%define("info_header_node", "Face for Info nodes in a node header.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=9) :: "info_node"]))
        f => info_index_match_face
        call f%define("info_index_match", "Face used to highlight matches in an index entry.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=5) :: "match"]))
        f => info_quoted_face
        call f%define("info_quoted", "Face used for quoted elements.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=17) :: "fixed_pitch_serif"]))
    end subroutine define_info_faces

    ! ------------------------------------------------------------------
    ! Info mode
    ! ------------------------------------------------------------------

    !> Info mode provides commands for browsing through the Info
    !> documentation tree.  Documentation in Info is divided into "nodes",
    !> each of which discusses one topic and contains references to other
    !> nodes which discuss related topics.  Info has commands to follow
    !> the references and show you other nodes.
    !>
    !> \<info_mode_map>\[info_help]        Invoke the Info tutorial.
    !> \[quit_window]        Quit Info: reselect previously selected buffer.
    !>
    !> Selecting other nodes:
    !> \[info_follow_nearest_node]        Follow a node reference near point.
    !> \[info_next]        Move to the "next" node of this node.
    !> \[info_prev]        Move to the "previous" node of this node.
    !> \[info_up]        Move "up" from this node.
    !> \[info_menu]        Pick menu item specified by name (or abbreviation).
    !>           Picking a menu item causes another node to be selected.
    !> \[info_directory]        Go to the Info directory node.
    !> \[info_top_node]        Go to the Top node of this file.
    !> \[info_final_node]        Go to the final node in this file.
    !> \[info_backward_node]        Go backward one node, considering all nodes as forming one sequence.
    !> \[info_forward_node]        Go forward one node, considering all nodes as forming one sequence.
    !> \[info_next_reference]        Move cursor to next cross-reference or menu item.
    !> \[info_prev_reference]        Move cursor to previous cross-reference or menu item.
    !> \[info_follow_reference]        Follow a cross reference.  Reads name of reference.
    !> \[info_history_back]        Move back in history to the last node you were at.
    !> \[info_history_forward]        Move forward in history to the node you returned from after using \[info_history_back].
    !> \[info_history]        Go to menu of visited nodes.
    !> \[info_toc]        Go to table of contents of the current Info file.
    !>
    !> Moving within a node:
    !> \[info_scroll_up]        Normally, scroll forward a full screen.
    !>           Once you scroll far enough in a node that its menu appears on the
    !>           screen but after point, the next scroll moves into its first
    !>           subnode.  When after all menu items (or if there is no menu),
    !>           move up to the parent node.
    !> \[info_scroll_down]        Normally, scroll backward.  If the beginning of the buffer is
    !>           already visible, try to go to the previous menu entry, or up
    !>           if there is none.
    !> \[beginning_of_buffer]        Go to beginning of node.
    !> \[end_of_buffer]        Go to end of node.
    !>
    !> Advanced commands:
    !> \[info_search]        Search through this Info file for specified regexp,
    !>           and select the node in which the next occurrence is found.
    !> \[info_search_case_sensitively]        Search through this Info file for specified regexp case-sensitively.
    !> \[isearch_forward], \[isearch_forward_regexp]        Use Isearch to search through multiple Info nodes.
    !> \[info_index]        Search for a topic in this manual's Index and go to index entry.
    !> \[info_index_next]        (comma) Move to the next match from a previous \<info_mode_map>\[info_index] command.
    !> \[info_virtual_index]        Look for a string and display the index node with results.
    !> \[info_apropos]        Look for a string in the indices of all manuals.
    !> \[info_goto_node]        Move to node specified by name.
    !>           You may include a filename as well, as (FILENAME)NODENAME.
    !> 1 .. 9        Pick first ... ninth item in node's menu.
    !>           Every third `*' is highlighted to help pick the right number.
    !> \[info_copy_current_node_name]        Put name of current Info node in the kill ring.
    !> \[universal_argument] \[info]        Move to new Info file with completion.
    !> \[universal_argument] N \[info]        Select Info buffer with prefix number in the name *info*<N>.
    !>
    !> Port of info.el:Info-mode (the commands of the Deferred features
    !> are not listed).
    recursive subroutine info_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved
        integer :: k

        if (.not. associated(major_modes%info_mode%run)) call syms_of_info()
        call major_modes%info_mode%begin(saved, status)
        if (.not. status%ok()) return
        call tab_width%set_local(8)
        call case_fold_search%set_local(.true.)
        buffers(cur)%read_only = .true.
        k = here()
        call make_marker(locals(k)%tag_table_marker)
        ! (The display table for terminals that can't show the Unicode
        ! symbols of Info files, the tool bar, desktop saving, the
        ! context menu, bookmarks and `isearch_fold_quotes_mode' are
        ! Deferred.)
        ! do not override global header lines
        if (info_use_header_line%value) locals(k)%header_from_property = .true.
        call info_mode_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true., case_fold=.true.)
        call kill_buffer_hook%add(info_kill_buffer, local=.true.)
        call isearch_mode_hook%add(info_isearch_start, local=.true.)
        call isearch_search_fun_function%set_local(info_isearch_search_fn)
        call isearch_wrap_function%set_local(info_isearch_wrap)
        call isearch_push_state_function%set_local(info_isearch_push_state)
        call isearch_filter_predicate%set_local(info_isearch_filter)
        call revert_buffer_function%set_local(info_revert_buffer_function)
        call info_set_mode_line()
        call major_modes%info_mode%finish(saved, status)
    end subroutine info_mode

    ! The font lock keywords of Info mode: the text inside curved quotes
    ! in the face `info_quoted'.  We deliberately fontify only ‘..’
    ! quoting, and not `..', because the former can be done much more
    ! reliably, i.e. without risking false positives.  (It doesn't
    ! handle nested quotes.)  Port of info.el Info-mode-font-lock-keywords.
    subroutine info_mode_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        type(font_lock_highlight_type) :: h(1)

        allocate (keywords(1))
        h(1) = font_lock_highlight(1, face="info_quoted")
        keywords(1) = font_lock_keyword("‘\([‘’]\|[^‘’]*\)’", highlights=h)
    end subroutine info_mode_font_lock_keywords

    !> When an Info buffer is killed, make sure the associated tags buffer
    !> is killed too.  Port of info.el:Info-kill-buffer.
    recursive subroutine info_kill_buffer(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k

        status = status_type()
        if (.not. derived_mode_p(major_modes%info_mode)) return
        k = local_index(.false.)
        if (k == 0) return
        if (buffer_live_p(locals(k)%tag_table_buffer)) call kill_buffer(locals(k)%tag_table_buffer, st)
    end subroutine info_kill_buffer

    recursive subroutine cmd_info_up(status)
        type(status_type), intent(out) :: status

        call info_up(status)
    end subroutine cmd_info_up

    recursive subroutine cmd_info_forward_node(status)
        type(status_type), intent(out) :: status

        call info_forward_node(status)
    end subroutine cmd_info_forward_node

    recursive subroutine cmd_info_follow_nearest_node(status)
        type(status_type), intent(out) :: status

        call info_follow_nearest_node(status)
    end subroutine cmd_info_follow_nearest_node

    ! Define the syntax table of Info mode: text mode's, with punctuation
    ! syntax for apostrophe because of extensive use of quotes like
    ! `this' in Info manuals.  Port of info.el Info-mode-syntax-table.
    subroutine init_info_mode_syntax_table()
        type(status_type) :: st

        if (.not. syntax_table_live_p(text_mode_syntax_table)) call syms_of_text_mode()
        call copy_syntax_table(info_mode_syntax_table, st, text_mode_syntax_table)
        call modify_syntax_entry(iachar("'"), iachar("'"), ".", st, info_mode_syntax_table)
    end subroutine init_info_mode_syntax_table

    !> Define Info mode and its keymap and register the commands.
    !> Not a port.
    subroutine syms_of_info()
        type(status_type) :: st
        character(len=1) :: digit
        integer :: i

        if (associated(major_modes%info_mode%run)) return
        call define_info_faces()
        call info_mode_hook%add(turn_on_font_lock)
        call init_info_mode_syntax_table()
        call major_modes%info_mode%define("info_mode", "Info", info_mode, info_mode_hook, &
                                          parent=major_modes%special_mode, syntax_table=info_mode_syntax_table, &
                                          abbrev_table=text_mode_abbrev_table)
        info_mode_map = major_modes%info_mode%map
        commands%info_mode%run => info_mode
        commands%info%run => cmd_info
        commands%info_other_window%run => cmd_info_other_window
        commands%info_emacs_manual%run => info_emacs_manual
        commands%info_emacs_bug%run => info_emacs_bug
        commands%info_on_current_buffer%run => cmd_info_on_current_buffer
        commands%info_goto_node%run => cmd_info_goto_node
        commands%info_next%run => info_next
        commands%info_prev%run => info_prev
        commands%info_up%run => cmd_info_up
        commands%info_history_back%run => info_history_back
        commands%info_history_forward%run => info_history_forward
        commands%info_directory%run => info_directory
        commands%info_follow_reference%run => cmd_info_follow_reference
        commands%info_menu%run => cmd_info_menu
        commands%info_nth_menu_item%run => cmd_info_nth_menu_item
        commands%info_top_node%run => info_top_node
        commands%info_final_node%run => info_final_node
        commands%info_forward_node%run => cmd_info_forward_node
        commands%info_backward_node%run => info_backward_node
        commands%info_next_menu_item%run => info_next_menu_item
        commands%info_last_menu_item%run => info_last_menu_item
        commands%info_next_preorder%run => info_next_preorder
        commands%info_last_preorder%run => info_last_preorder
        commands%info_scroll_up%run => info_scroll_up
        commands%info_scroll_down%run => info_scroll_down
        commands%info_next_reference%run => cmd_info_next_reference
        commands%info_prev_reference%run => cmd_info_prev_reference
        commands%info_follow_nearest_node%run => cmd_info_follow_nearest_node
        commands%info_copy_current_node_name%run => cmd_info_copy_current_node_name
        commands%info_undefined%run => info_undefined
        commands%info_help%run => info_help
        commands%info_summary%run => info_summary
        commands%info_toc%run => info_toc
        commands%info_history%run => info_history
        commands%info_search%run => cmd_info_search
        commands%info_search_case_sensitively%run => info_search_case_sensitively
        commands%info_search_next%run => info_search_next
        commands%info_search_backward%run => cmd_info_search_backward
        commands%info_index%run => cmd_info_index
        commands%info_index_next%run => cmd_info_index_next
        commands%info_virtual_index%run => cmd_info_virtual_index
        commands%info_apropos%run => cmd_info_apropos
        commands%info_goto_emacs_command_node%run => cmd_info_goto_emacs_command_node
        commands%info_goto_emacs_key_command_node%run => cmd_info_goto_emacs_key_command_node
        commands%info_display_manual%run => cmd_info_display_manual
        commands%info%info_file = "emacs"
        commands%info_goto_emacs_command_node%info_file = "emacs"
        commands%info_goto_emacs_key_command_node%info_file = "emacs"
        call keymap_set(info_mode_map, ".", "beginning_of_buffer", st)
        call keymap_set(info_mode_map, "SPC", "info_scroll_up", st)
        call keymap_set(info_mode_map, "S-SPC", "info_scroll_down", st)
        call keymap_set(info_mode_map, "RET", "info_follow_nearest_node", st)
        call keymap_set(info_mode_map, "TAB", "info_next_reference", st)
        call keymap_set(info_mode_map, "M-TAB", "info_prev_reference", st)
        call keymap_set(info_mode_map, "<backtab>", "info_prev_reference", st)

        do i = 1, 9
            write (digit, "(i1)") i
            call keymap_set(info_mode_map, digit, "info_nth_menu_item", st)
        end do

        call keymap_set(info_mode_map, "0", "undefined", st)
        call keymap_set(info_mode_map, "?", "info_summary", st)
        call keymap_set(info_mode_map, "]", "info_forward_node", st)
        call keymap_set(info_mode_map, "[", "info_backward_node", st)
        call keymap_set(info_mode_map, "<", "info_top_node", st)
        call keymap_set(info_mode_map, ">", "info_final_node", st)
        call keymap_set(info_mode_map, "b", "beginning_of_buffer", st)
        call keymap_set(info_mode_map, "d", "info_directory", st)
        call keymap_set(info_mode_map, "e", "end_of_buffer", st)
        call keymap_set(info_mode_map, "f", "info_follow_reference", st)
        call keymap_set(info_mode_map, "g", "info_goto_node", st)
        call keymap_set(info_mode_map, "h", "info_help", st)
        ! This is for compatibility with standalone info (>~ version 5.2).
        ! Though for some time, standalone info had H and h reversed.
        ! See <https://debbugs.gnu.org/16455>.
        call keymap_set(info_mode_map, "H", "describe_mode", st)
        call keymap_set(info_mode_map, "i", "info_index", st)
        call keymap_set(info_mode_map, "I", "info_virtual_index", st)
        call keymap_set(info_mode_map, "l", "info_history_back", st)
        call keymap_set(info_mode_map, "L", "info_history", st)
        call keymap_set(info_mode_map, "m", "info_menu", st)
        call keymap_set(info_mode_map, "n", "info_next", st)
        call keymap_set(info_mode_map, "p", "info_prev", st)
        call keymap_set(info_mode_map, "q", "quit_window", st)
        call keymap_set(info_mode_map, "r", "info_history_forward", st)
        call keymap_set(info_mode_map, "s", "info_search", st)
        call keymap_set(info_mode_map, "S", "info_search_case_sensitively", st)
        call keymap_set(info_mode_map, "t", "info_top_node", st)
        call keymap_set(info_mode_map, "T", "info_toc", st)
        call keymap_set(info_mode_map, "u", "info_up", st)
        ! `w' for consistency with `dired-copy-filename-as-kill'.
        call keymap_set(info_mode_map, "w", "info_copy_current_node_name", st)
        call keymap_set(info_mode_map, "c", "info_copy_current_node_name", st)
        ! `^' for consistency with `dired-up-directory'.
        call keymap_set(info_mode_map, "^", "info_up", st)
        call keymap_set(info_mode_map, ",", "info_index_next", st)
        call keymap_set(info_mode_map, "DEL", "info_scroll_down", st)
        call add_local_variables_killer(kill_info_locals)
    end subroutine syms_of_info
end module info_fm
