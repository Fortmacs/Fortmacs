! quail_fm.f90 -- provides simple input method for multilingual text.
! Ported from GNU Emacs 31.1 lisp/international/quail.el.
!
! Copyright (C) 1997-1998, 2000-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
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

!> Quail: input methods that translate ASCII key sequences to
!> multilingual text through a translation table (a Quail map).
!>
!> A Quail package (`quail_package_type') is defined with
!> `quail_define_package', which also registers it as an input method in
!> mule_fm's `input_method_alist'; its rules are built in a
!> `quail_map_type' with `%add' (quail-define-rules) and installed with
!> `quail_install_map', or added one at a time with `quail_defrule'.  A
!> Quail map is a trie kept flat: an array of nodes linked by indices
!> (each node's first child and next sibling), with the candidates of
!> the translations in a pool.  The packages of the input methods that
!> come with Fortmacs are built in: leim_list_fm registers them (as
!> upstream's leim-list.el does) with the procedure that defines the
!> packages of their file (generated from upstream's leim/quail files by
!> tools/gen-quail.sh), run when one of them is first used.
!>
!> `quail_input_method' is the buffer's `input_method_function' while a
!> Quail input method is active: it reads the rest of the key sequence
!> with `quail_translation_keymap' (or `quail_simple_translation_keymap')
!> as the overriding terminal keymap, inserting the current translation
!> underlined (an overlay in the face `underline') and showing guidance in
!> the echo area, and returns the translated text as events.
!>
!> Deferred: conversion (CONVERSION-KEYS, `quail_conversion_keymap',
!> quail-start-conversion and its commands), UPDATE-TRANSLATION-FUNCTION,
!> OVERLAY-PLIST, decode maps (CREATE-DECODE-MAP, quail-install-decode-map,
!> quail-find-key, quail-show-key), the annotations of
!> quail-define-rules (append, face, advice, no-decode-map) and the
!> `advice' event, translations and maps given as functions (and so
!> quail-map-from-table), a title given as a list, GUIDANCE given as an
!> alist, `quail-completion' (TAB) and the *Quail Completions* window,
!> `quail-translation-help' (C-h), `quail-mouse-choose-completion',
!> `quail-error' (no ported command signals it), the guidance frame for a
!> minibuffer-only frame (no window-system frames), the minibuffer exit
!> hook that turns the input method off (minibuffer-exit-hook is not
!> ported), the `highlight' face of the current candidate in the echo
!> area (messages carry no text properties), `translation-table-for-input'
!> (obsolete), the default (setq-default) value of
!> `quail_current_package', quail-show-keyboard-layout and the keyboard
!> layout in `quail_help' (SHOW-LAYOUT), its buttons, and
!> quail-update-leim-list-file (tools/gen-quail.sh generates leim_list_fm).
module quail_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, format_message
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, make_fixnum, fixnump, xfixnum, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use character_fm, only: string_char_and_length, char_string, MAX_MULTIBYTE_LENGTH
    use commands_fm, only: commands
    use callint_fm, only: call_interactively, commandp, this_command
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, buffer_modified_p, restore_buffer_modified_p, &
                         barf_if_buffer_read_only, inhibit_read_only, get_buffer_create, set_buffer, buffer_live_p, &
                         buffer_serial_live, deactivate_mark_variable, input_method_proc
    use insdel_fm, only: insert, inhibit_modification_hooks
    use editfns_fm, only: point, point_max, delete_region, goto_char
    use itree_fm, only: make_overlay, move_overlay, delete_overlay, overlay_put, overlayp, overlay_start, overlay_end
    use textprop_fm, only: get_char_property
    use keymap_fm, only: make_keymap, define_key, define_key_range, key_parse, lookup_key, copy_keymap, keymap_live_p, &
                         overriding_terminal_local_map
    use keyboard_fm, only: read_key_sequence, unread_command_events, unread_command_events_p, &
                           unread_post_input_method_events_p, this_single_command_raw_keys, last_command_event, &
                           last_command, sit_for, input_method_previous_message, post_command_hook
    use xdisp_fm, only: message_nolog
    use dispnew_fm, only: ding
    use subr_fm, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use mule_fm, only: register_input_method, input_method_verbose_flag, input_method_highlight_flag, &
                       input_method_after_insert_chunk_hook, input_method_exit_on_first_char, &
                       input_method_use_echo_area, INPUT_METHOD_VERBOSE_NEVER, INPUT_METHOD_VERBOSE_ALWAYS, &
                       INPUT_METHOD_VERBOSE_COMPLEX_ONLY, INPUT_METHOD_VERBOSE_DEFAULT
    use minibuf_fm, only: minibufferp
    use frame_fm, only: selected_window, selected_frame
    use window_fm, only: windows, window_live_p, window_text_cols, bury_buffer
    use fill_fm, only: string_width
    implicit none(type, external)
    private

    public :: syms_of_quail, register_quail_input_method, quail_define_package, quail_install_map, quail_defrule, &
              quail_select_package, quail_use_package, quail_package_index, quail_activate, quail_deactivate, &
              quail_input_method, quail_title, quail_name, quail_lookup_key, quail_get_translations, &
              quail_keyboard_translate, quail_set_keyboard_layout, quail_library_proc, quail_copy_package
    public :: quail_help_proc

    !> Kinds of translation of a Quail map node: none, one character, or a
    !> vector of candidates (upstream's (INDEX . VECTOR)).
    integer, parameter :: TRANS_NONE = 0, TRANS_CHAR = 1, TRANS_VECTOR = 2

    !> The control flags of `quail_update_translation' other than a number
    !> of keys: nil (go on with more keys) and t (terminate).
    integer, parameter :: CONTROL_NIL = -1, CONTROL_T = -2

    !> A candidate of a translation: a string, or a character (CHAR true).
    type :: quail_candidate_type
        character(len=:), allocatable :: s
        logical :: char = .false.
    end type quail_candidate_type

    !> A node of a Quail map: the key character that leads to it, its first
    !> child and next sibling (0 for none), its translation (KIND, and the
    !> candidates FIRST to FIRST + COUNT - 1 of the map's pool), and for a
    !> vector the indices (CURRENT START END BLOCK BLOCKS) of
    !> quail-current-translations (BLOCKS -1 for nil), 0-based as upstream.
    type :: quail_node_type
        integer(int64) :: ch = 0
        integer :: child = 0, sibling = 0
        integer :: kind = TRANS_NONE
        integer :: first = 0, count = 0
        integer :: current = 0, start = 0, end = 0, block = 0, blocks = -1
    end type quail_node_type

    !> A Quail map: how key sequences are translated.  The first node is
    !> the root (the empty key).  Port of the data structure that
    !> quail.el:quail-map-p describes, kept flat (no recursive types).
    type, public :: quail_map_type
        type(quail_node_type), allocatable :: nodes(:)
        integer :: nnodes = 0
        type(quail_candidate_type), allocatable :: cands(:)
        integer :: ncands = 0
    contains
        !> Define a rule KEY -> TRANSLATION (one string: each character a
        !> candidate) or KEY -> CANDIDATES (a vector of strings), appending
        !> to the translations already defined for KEY, as each rule of
        !> quail-define-rules does.
        generic :: add => map_add_string, map_add_vector
        procedure, private :: map_add_string, map_add_vector
    end type quail_map_type

    !> A key binding added to the translation keymap of a package (an
    !> element of TRANSLATION-KEYS): KEY in `key_parse' syntax and the
    !> name of its command.
    type, public :: quail_translation_key_type
        character(len=32) :: key = ""
        character(len=64) :: command = ""
    end type quail_translation_key_type

    !> A Quail package: an element of `quail_package_alist' (NAME, TITLE,
    !> QUAIL-MAP, GUIDANCE, DOCSTRING, TRANSLATION-KEYS as its keymap,
    !> FORGET-LAST-SELECTION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT,
    !> MAXIMUM-SHORTEST, SIMPLE).
    type, public :: quail_package_type
        character(len=:), allocatable :: name, title, docstring
        type(quail_map_type) :: map
        logical :: guidance = .false.
        type(handle_type) :: translation_keymap
        logical :: forget_last_selection = .false., deterministic = .false., kbd_translate = .false., &
                   show_layout = .false., maximum_shortest = .false., simple = .false.
    end type quail_package_type

    !> List of Quail packages.  See the documentation of
    !> `quail_define_package' for the elements.  Packages are added at the
    !> end and the most recently added is the last (upstream's first), so
    !> that `quail_current_package' can hold a package's index.  Port of
    !> quail.el quail-package-alist.
    type(quail_package_type), allocatable, public :: quail_package_alist(:)
    integer, public :: quail_npackages = 0

    abstract interface
        !> A procedure that defines the Quail packages of an input method
        !> library (what loading a leim/quail file does upstream).
        subroutine quail_library_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine quail_library_proc
    end interface

    !> A built-in input method and the library that defines its package.
    type :: quail_library_type
        character(len=64) :: name = ""
        procedure(quail_library_proc), pointer, nopass :: load => null()
    end type quail_library_type

    type(quail_library_type), allocatable :: quail_libraries(:)

    !> Keymap used processing translation in complex Quail modes.
    !> Only a few especially complex input methods use this map;
    !> most use `quail_simple_translation_keymap' instead.
    !> This map is activated while translation region is active.  Port of
    !> quail.el quail-translation-keymap.
    type(handle_type), public :: quail_translation_keymap

    !> Keymap used while processing translation in simple Quail modes.
    !> A few especially complex input methods use `quail_translation_keymap' instead.
    !> This map is activated while translation region is active.  Port of
    !> quail.el quail-simple-translation-keymap.
    type(handle_type), public :: quail_simple_translation_keymap

    ! A line feed.
    character(len=*), parameter :: LF = new_line("a")

    !> How to select a candidate, for the docstrings of input methods
    !> (\<quail_translation_docstring>).  Port of quail.el
    !> quail-translation-docstring.
    character(len=*), parameter, public :: quail_translation_docstring = &
                                           "When you type keys, the echo area shows the possible characters"//LF// &
                                           "which correspond to that key sequence, each preceded by a digit.  You"//LF// &
                                           "can select one of the characters shown by typing the corresponding"//LF// &
                                           "digit.  Alternatively, you can use C-f and C-b to move through the"//LF// &
                                           "line to select the character you want, then type a letter to begin"//LF// &
                                           "entering another Chinese character or type a space or punctuation"//LF// &
                                           "character."//LF//LF// &
                                           "If there are more than ten possible characters for the given spelling,"//LF// &
                                           "the echo area shows ten characters at a time; you can use C-n to move"//LF// &
                                           "to the next group of ten, and C-p to move back to the previous group"//LF// &
                                           "of ten."

    !> The column of the guidance where the list of candidates starts.
    !> Port of quail.el quail-guidance-translations-starting-column.
    integer, public :: quail_guidance_translations_starting_column = 20

    !> Hook run when a Quail input method is activated (`quail_activate').
    !> Not defined upstream: quail.el runs the hook quail-activate-hook.
    type(hook_type), target, public :: quail_activate_hook = hook_type(name="quail_activate_hook")

    !> Hook run when a Quail input method is deactivated.
    !> Not defined upstream: quail.el runs the hook quail-deactivate-hook.
    type(hook_type), target, public :: quail_deactivate_hook = hook_type(name="quail_deactivate_hook")

    !> Standard keyboard layout of printable characters Quail assumes.
    !> See the documentation of `quail_keyboard_layout' for this format.
    !> This layout is almost the same as that of VT100,
    !> but the location of key \ (backslash) is just right of key ' (single-quote),
    !> not right of RETURN key.  Port of quail.el
    !> quail-keyboard-layout-standard.
    character(len=*), parameter, public :: QUAIL_KEYBOARD_LAYOUT_STANDARD = &
                                           "                              "// &
                                           "  1!2@3#4$5%6^7&8*9(0)-_=+`~  "// &
                                           "  qQwWeErRtTyYuUiIoOpP[{]}    "// &
                                           "  aAsSdDfFgGhHjJkKlL;:'""\|    "// &
                                           "  zZxXcCvVbBnNmM,<.>/?        "// &
                                           "                              "

    ! The other layouts of quail-keyboard-layout-alist.
    character(len=*), parameter :: LAYOUT_SUN_TYPE3 = &
                                   "                              "// &
                                   "  1!2@3#4$5%6^7&8*9(0)-_=+\|`~"// &
                                   "  qQwWeErRtTyYuUiIoOpP[{]}    "// &
                                   "  aAsSdDfFgGhHjJkKlL;:'""      "// &
                                   "  zZxXcCvVbBnNmM,<.>/?        "// &
                                   "                              "
    character(len=*), parameter :: LAYOUT_ATARI_GERMAN = &
                                   "                              "// &
                                   "  1!2""3§4$5%6&7/8(9)0=ß?'`#^  "// &
                                   "  qQwWeErRtTzZuUiIoOpPüÜ+*    "// &
                                   "  aAsSdDfFgGhHjJkKlLöÖäÄ~|    "// &
                                   "<>yYxXcCvVbBnNmM,;.:-_        "// &
                                   "                              "
    character(len=*), parameter :: LAYOUT_PC102_DE = &
                                   "                              "// &
                                   "^°1!2""3§4$5%6&7/8(9)0=ß?´`#'  "// &
                                   "  qQwWeErRtTzZuUiIoOpPüÜ+*    "// &
                                   "  aAsSdDfFgGhHjJkKlLöÖäÄ      "// &
                                   "<>yYxXcCvVbBnNmM,;.:-_        "// &
                                   "                              "
    character(len=*), parameter :: LAYOUT_JP106 = &
                                   "                              "// &
                                   "  1!2""3#4$5%6&7'8(9)0~-=^~\|  "// &
                                   "  qQwWeErRtTyYuUiIoOpP@`[{    "// &
                                   "  aAsSdDfFgGhHjJkKlL;+:*]}    "// &
                                   "  zZxXcCvVbBnNmM,<.>/?\_      "// &
                                   "                              "
    character(len=*), parameter :: LAYOUT_PC105_UK = &
                                   "                              "// &
                                   "`¬1!2""3£4$5%6^7&8*9(0)-_=+    "// &
                                   "  qQwWeErRtTyYuUiIoOpP[{]}    "// &
                                   "  aAsSdDfFgGhHjJkKlL;:'@#~    "// &
                                   "\|zZxXcCvVbBnNmM,<.>/?        "// &
                                   "                              "

    !> The number of characters of a keyboard layout.  Port of quail.el
    !> quail-keyboard-layout-len.
    integer, parameter :: QUAIL_KEYBOARD_LAYOUT_LEN = 180

    !> A keyboard name and its layout string.
    type, public :: quail_keyboard_layout_entry_type
        character(len=16) :: name = ""
        character(len=400) :: layout = ""
    end type quail_keyboard_layout_entry_type

    ! The entries of quail-keyboard-layout-alist.
    type(quail_keyboard_layout_entry_type), parameter :: &
        ENTRY_STANDARD = quail_keyboard_layout_entry_type("standard", QUAIL_KEYBOARD_LAYOUT_STANDARD), &
        ENTRY_SUN_TYPE3 = quail_keyboard_layout_entry_type("sun-type3", LAYOUT_SUN_TYPE3), &
        ENTRY_ATARI_GERMAN = quail_keyboard_layout_entry_type("atari-german", LAYOUT_ATARI_GERMAN), &
        ENTRY_PC102_DE = quail_keyboard_layout_entry_type("pc102-de", LAYOUT_PC102_DE), &
        ENTRY_JP106 = quail_keyboard_layout_entry_type("jp106", LAYOUT_JP106), &
        ENTRY_PC105_UK = quail_keyboard_layout_entry_type("pc105-uk", LAYOUT_PC105_UK)

    !> Alist of keyboard names and corresponding layout strings.
    !> See the documentation of `quail_keyboard_layout' for the format of
    !> the layout string.  Port of quail.el quail-keyboard-layout-alist.
    type(quail_keyboard_layout_entry_type), parameter, public :: quail_keyboard_layout_alist(6) = &
                                                                 [ENTRY_STANDARD, ENTRY_SUN_TYPE3, ENTRY_ATARI_GERMAN, &
                                                                  ENTRY_PC102_DE, ENTRY_JP106, ENTRY_PC105_UK]

    !> A string which represents physical key layout of a particular keyboard.
    !> We assume there are six rows and each row has 15 keys (columns),
    !>         the first row is above the `1' - `0' row,
    !>         the first column of the second row is left of key `1',
    !>         the first column of the third row is left of key `q',
    !>         the first column of the fourth row is left of key `a',
    !>         the first column of the fifth row is left of key `z',
    !>         the sixth row is below the `z' - `/' row.
    !> Nth (N is even) and (N+1)th characters in the string are non-shifted
    !> and shifted characters respectively at the same location.
    !> The location of Nth character is row (N / 30) and column ((N mod 30) / 2).
    !> The command `quail_set_keyboard_layout' usually sets this variable.
    !> Port of quail.el quail-keyboard-layout (characters past the 180th
    !> are ignored, missing ones are blanks).
    character(len=400), public :: quail_keyboard_layout = QUAIL_KEYBOARD_LAYOUT_STANDARD

    !> Type of keyboard layout used in Quail base input method.
    !> Available types are listed in the variable `quail_keyboard_layout_alist'.
    !> Port of quail.el quail-keyboard-layout-type (assigning it does not
    !> change `quail_keyboard_layout': `quail_set_keyboard_layout' does both,
    !> as its Custom setter does upstream).
    character(len=16), public :: quail_keyboard_layout_type = "standard"

    ! quail-keyboard-layout-substitution: additional key locations of a
    ! non-standard layout (0-based) and the missing locations they stand
    ! for (-1 for none).
    integer, allocatable :: subst_from(:), subst_to(:)

    ! The state of the current translation (buffer-local upstream, but
    ! only meaningful while `quail_input_method' runs in one buffer):
    ! quail-current-key (the key codes), quail-current-str (unallocated
    ! for nil), quail-current-translations (the node of the current
    ! package's map, 0 for nil), quail-guidance-str, quail-translating.
    integer(int64), allocatable :: quail_current_key(:)
    character(len=:), allocatable :: quail_current_str
    integer :: quail_current_translations = 0
    character(len=:), allocatable :: quail_guidance_str
    logical :: quail_translating = .false.
    ! Set when quail-update-translation throws to `quail-tag' (a character
    ! that a unibyte buffer can't hold).
    logical :: quail_tag_thrown = .false.

    ! quail-overlay: one overlay per buffer (BUFFER_SERIALS, OVERLAYS).
    integer(int64), allocatable :: overlay_serials(:)
    type(handle_type), allocatable :: overlays(:)

    ! quail-completion-buf.
    type(handle_type) :: quail_completion_buf

contains

    !> Set up Quail: its translation keymaps and commands.  Port of the
    !> top-level forms of quail.el.
    subroutine syms_of_quail()
        type(status_type) :: st

        commands%quail_other_command%run => cmd_quail_other_command
        commands%quail_self_insert_command%run => cmd_quail_self_insert_command
        commands%quail_delete_last_char%run => cmd_quail_delete_last_char
        commands%quail_next_translation%run => cmd_quail_next_translation
        commands%quail_prev_translation%run => cmd_quail_prev_translation
        commands%quail_next_translation_block%run => cmd_quail_next_translation_block
        commands%quail_prev_translation_block%run => cmd_quail_prev_translation_block
        commands%quail_select_current%run => cmd_quail_select_current
        commands%quail_abort_translation%run => cmd_quail_abort_translation
        commands%quail_deactivate%run => cmd_quail_deactivate
        commands%quail_set_keyboard_layout%run => cmd_quail_set_keyboard_layout
        if (keymap_live_p(quail_translation_keymap)) return
        call make_keymap(quail_translation_keymap)
        call define_key_range(quail_translation_keymap, 0_int64, 31_int64, sym("quail_other_command"), st)
        call define_key_range(quail_translation_keymap, 32_int64, 126_int64, sym("quail_self_insert_command"), st)
        call define_key_range(quail_translation_keymap, 128_int64, 255_int64, sym("quail_self_insert_command"), st)
        call bind(quail_translation_keymap, "DEL", "quail_delete_last_char")
        call bind(quail_translation_keymap, "C-f", "quail_next_translation")
        call bind(quail_translation_keymap, "C-b", "quail_prev_translation")
        call bind(quail_translation_keymap, "C-n", "quail_next_translation_block")
        call bind(quail_translation_keymap, "C-p", "quail_prev_translation_block")
        call bind(quail_translation_keymap, "<right>", "quail_next_translation")
        call bind(quail_translation_keymap, "<left>", "quail_prev_translation")
        call bind(quail_translation_keymap, "<down>", "quail_next_translation_block")
        call bind(quail_translation_keymap, "<up>", "quail_prev_translation_block")
        call bind(quail_translation_keymap, "TAB", "quail_completion")
        call bind(quail_translation_keymap, "C-@", "quail_select_current")
        ! Following simple.el, Enter key on numeric keypad selects the
        ! current translation just like `C-SPC', and `mouse-2' chooses
        ! any completion visible in the *Quail Completions* buffer.
        call bind(quail_translation_keymap, "<kp-enter>", "quail_select_current")
        call bind(quail_translation_keymap, "<mouse-2>", "quail_mouse_choose_completion")
        call bind(quail_translation_keymap, "<down-mouse-2>", "")
        call bind(quail_translation_keymap, "C-h", "quail_translation_help")
        call bind(quail_translation_keymap, "C-SPC", "quail_select_current")
        call bind(quail_translation_keymap, "<tab>", "quail_completion")
        call bind(quail_translation_keymap, "<delete>", "quail_delete_last_char")
        call bind(quail_translation_keymap, "<backspace>", "quail_delete_last_char")
        call make_keymap(quail_simple_translation_keymap)
        call define_key_range(quail_simple_translation_keymap, 0_int64, 31_int64, sym("quail_other_command"), st)
        call define_key_range(quail_simple_translation_keymap, 32_int64, 126_int64, sym("quail_self_insert_command"), st)
        call define_key_range(quail_simple_translation_keymap, 128_int64, 255_int64, sym("quail_self_insert_command"), &
                              st)
        call bind(quail_simple_translation_keymap, "DEL", "quail_delete_last_char")
        call bind(quail_simple_translation_keymap, "<delete>", "quail_delete_last_char")
        call bind(quail_simple_translation_keymap, "<backspace>", "quail_delete_last_char")
    end subroutine syms_of_quail

    !> Bind KEYS (`key_parse' syntax) to the command CMD in MAP ("" for
    !> nil).  Not a port.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        if (.not. status%ok()) return

        if (len(cmd) == 0) then
            call define_key(map, key, nil(), status)
        else
            call define_key(map, key, sym(cmd), status)
        end if
    end subroutine bind

    ! ------------------------------------------------------------------
    ! Strings of characters
    ! ------------------------------------------------------------------

    !> The multibyte form of character C.  Port of the Lisp function
    !> char-to-string.
    function char_to_string(c) result(s)
        integer(int64), intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: n
        type(status_type) :: st

        call char_string(int(c, int32), buf, n, st)

        if (st%ok()) then
            s = buf(1:n)
        else
            s = ""
        end if
    end function char_to_string

    !> The string of the characters CODES.  Not a port (the Lisp function
    !> string).
    function codes_to_string(codes) result(s)
        integer(int64), intent(in) :: codes(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(codes)
            s = s//char_to_string(codes(k))
        end do
    end function codes_to_string

    !> The characters of STRING.  Not a port (string-to-vector).
    subroutine string_to_codes(string, codes)
        character(len=*), intent(in) :: string
        integer(int64), allocatable, intent(out) :: codes(:)
        integer(int64) :: i
        integer(int32) :: c, clen
        integer :: n

        allocate (codes(len(string)))
        n = 0
        i = 1

        do while (i <= len(string, kind=int64))
            call string_char_and_length(string, i, c, clen)
            n = n + 1
            codes(n) = c
            i = i + max(1, clen)
        end do

        codes = codes(1:n)
    end subroutine string_to_codes

    !> The number of characters of STRING.  Not a port (length).
    integer function string_chars(string) result(n)
        character(len=*), intent(in) :: string
        integer(int64), allocatable :: codes(:)

        call string_to_codes(string, codes)
        n = size(codes)
    end function string_chars

    ! ------------------------------------------------------------------
    ! Quail maps
    ! ------------------------------------------------------------------

    !> Make MAP an empty Quail map (upstream's (list nil)).  Not a port.
    subroutine map_init(map)
        type(quail_map_type), intent(inout) :: map

        if (allocated(map%nodes)) deallocate (map%nodes)
        if (allocated(map%cands)) deallocate (map%cands)
        allocate (map%nodes(16), map%cands(16))
        map%nodes(1) = quail_node_type()
        map%nnodes = 1
        map%ncands = 0
    end subroutine map_init

    !> The child of NODE in MAP reached by the key character CH, 0 if none.
    !> Not a port (assq on the map's alist).
    integer function map_child(map, node, ch) result(k)
        type(quail_map_type), intent(in) :: map
        integer, intent(in) :: node
        integer(int64), intent(in) :: ch

        k = map%nodes(node)%child

        do while (k /= 0)
            if (map%nodes(k)%ch == ch) return
            k = map%nodes(k)%sibling
        end do
    end function map_child

    !> Add a child to NODE for the key character CH, first among its
    !> children (as upstream conses the entry on); K is the new node.  Not
    !> a port.
    subroutine map_add_child(map, node, ch, k)
        type(quail_map_type), intent(inout) :: map
        integer, intent(in) :: node
        integer(int64), intent(in) :: ch
        integer, intent(out) :: k
        type(quail_node_type), allocatable :: grown(:)

        if (map%nnodes == size(map%nodes)) then
            allocate (grown(2*size(map%nodes)))
            grown(1:map%nnodes) = map%nodes(1:map%nnodes)
            call move_alloc(grown, map%nodes)
        end if

        map%nnodes = map%nnodes + 1
        k = map%nnodes
        map%nodes(k) = quail_node_type()
        map%nodes(k)%ch = ch
        map%nodes(k)%sibling = map%nodes(node)%child
        map%nodes(node)%child = k
    end subroutine map_add_child

    !> Give NODE of MAP the translation KIND with the candidates CANDS and
    !> fresh indices.  Not a port.
    subroutine map_set_translation(map, node, kind, cands)
        type(quail_map_type), intent(inout) :: map
        integer, intent(in) :: node, kind
        type(quail_candidate_type), intent(in) :: cands(:)
        type(quail_candidate_type), allocatable :: grown(:)
        integer :: k, n

        n = size(cands)

        if (map%ncands + n > size(map%cands)) then
            allocate (grown(2*size(map%cands) + n))

            do k = 1, map%ncands
                call move_alloc(map%cands(k)%s, grown(k)%s)
                grown(k)%char = map%cands(k)%char
            end do

            call move_alloc(grown, map%cands)
        end if

        do k = 1, n
            map%cands(map%ncands + k)%s = cands(k)%s
            map%cands(map%ncands + k)%char = cands(k)%char
        end do

        associate (nd => map%nodes(node))
            nd%kind = kind
            nd%first = map%ncands + 1
            nd%count = n
            nd%current = 0
            nd%start = 0
            nd%end = 0
            nd%block = 0
            nd%blocks = -1
        end associate

        map%ncands = map%ncands + n
    end subroutine map_set_translation

    !> The candidates of NODE's translation in MAP (none for no
    !> translation; a character's translation is that character).  Not a
    !> port.
    subroutine node_candidates(map, node, cands)
        type(quail_map_type), intent(in) :: map
        integer, intent(in) :: node
        type(quail_candidate_type), allocatable, intent(out) :: cands(:)
        integer :: k

        allocate (cands(map%nodes(node)%count))

        do k = 1, size(cands)
            cands(k)%s = map%cands(map%nodes(node)%first + k - 1)%s
            cands(k)%char = map%cands(map%nodes(node)%first + k - 1)%char
        end do
    end subroutine node_candidates

    !> Whether candidate lists A and B are equal.  Not a port (equal).
    logical function candidates_equal(a, b) result(same)
        type(quail_candidate_type), intent(in) :: a(:), b(:)
        integer :: k

        same = size(a) == size(b)
        if (.not. same) return

        do k = 1, size(a)
            if (a(k)%char .neqv. b(k)%char) then
                same = .false.
                return
            end if

            if (a(k)%s /= b(k)%s .or. len(a(k)%s) /= len(b(k)%s)) then
                same = .false.
                return
            end if
        end do
    end function candidates_equal

    !> The candidates of V1 then those of V2, without duplicates (the first
    !> occurrence kept).  Port of quail.el:quail-vunion.
    subroutine quail_vunion(v1, v2, union)
        type(quail_candidate_type), intent(in) :: v1(:), v2(:)
        type(quail_candidate_type), allocatable, intent(out) :: union(:)
        type(quail_candidate_type), allocatable :: all(:)
        integer :: k, j, n
        logical :: dup

        allocate (all(size(v1) + size(v2)), union(size(v1) + size(v2)))

        do k = 1, size(v1)
            all(k)%s = v1(k)%s
            all(k)%char = v1(k)%char
        end do

        do k = 1, size(v2)
            all(size(v1) + k)%s = v2(k)%s
            all(size(v1) + k)%char = v2(k)%char
        end do

        n = 0

        do k = 1, size(all)
            dup = .false.

            do j = 1, n
                if (candidates_equal(all(k:k), union(j:j))) then
                    dup = .true.
                    exit
                end if
            end do

            if (.not. dup) then
                n = n + 1
                union(n)%s = all(k)%s
                union(n)%char = all(k)%char
            end if
        end do

        union = union(1:n)
    end subroutine quail_vunion

    !> Define KEY as TRANS in a Quail map MAP.
    !>
    !> If APPEND is true, TRANS is appended to the current translations for
    !> KEY instead of replacing them.  TRANS is the candidates CANDS: the
    !> characters of a string (FROM_STRING true; one character is the sole
    !> translation) or the elements of a vector.  With DECODE_MAP true a
    !> string's characters become strings (as the decode map of
    !> quail-define-rules makes them).  Port of
    !> quail.el:quail-defrule-internal (the decode map itself and PROPS are
    !> not ported; a translation is normalized as quail-get-translation
    !> does it when it is defined).
    subroutine quail_defrule_internal(key, cands, from_string, map, append, decode_map)
        integer(int64), intent(in) :: key(:)
        type(quail_candidate_type), intent(in) :: cands(:)
        logical, intent(in) :: from_string, append, decode_map
        type(quail_map_type), intent(inout) :: map
        type(quail_candidate_type), allocatable :: trans(:), prevchars(:), new(:)
        integer :: node, k, idx
        logical :: string

        if (.not. allocated(map%nodes)) call map_init(map)
        node = 1

        do idx = 1, size(key)
            k = map_child(map, node, key(idx))
            if (k == 0) call map_add_child(map, node, key(idx), k)
            node = k
        end do

        allocate (trans(size(cands)))

        do k = 1, size(cands)
            trans(k)%s = cands(k)%s
            trans(k)%char = cands(k)%char
        end do

        string = from_string

        ! The decode map makes TRANS a vector of strings.
        if (decode_map) then
            trans(:)%char = .false.
            string = .false.
        end if

        if (map%nodes(node)%kind /= TRANS_NONE .and. append) then
            call node_candidates(map, node, prevchars)
            call quail_vunion(prevchars, trans, new)
            ! Nothing to change, get back to orig value.
            if (candidates_equal(new, prevchars)) return
            call map_set_translation(map, node, TRANS_VECTOR, new)
            return
        end if

        ! quail-get-translation: one character is the translation itself.
        if (string) then
            if (size(trans) == 1) then
                call map_set_translation(map, node, TRANS_CHAR, trans)
            else
                call map_set_translation(map, node, TRANS_VECTOR, trans)
            end if
        else if (size(trans) == 1) then
            if (string_chars(trans(1)%s) == 1) then
                trans(1)%char = .true.
                call map_set_translation(map, node, TRANS_CHAR, trans)
            else
                call map_set_translation(map, node, TRANS_VECTOR, trans)
            end if
        else
            call map_set_translation(map, node, TRANS_VECTOR, trans)
        end if
    end subroutine quail_defrule_internal

    !> The candidates of TRANSLATION, each of its characters (characters if
    !> CHARS).  Not a port.
    subroutine string_candidates(translation, chars, cands)
        character(len=*), intent(in) :: translation
        logical, intent(in) :: chars
        type(quail_candidate_type), allocatable, intent(out) :: cands(:)
        integer(int64), allocatable :: codes(:)
        integer :: k

        call string_to_codes(translation, codes)
        allocate (cands(size(codes)))

        do k = 1, size(codes)
            cands(k)%s = char_to_string(codes(k))
            cands(k)%char = chars
        end do
    end subroutine string_candidates

    !> The candidates of the vector TRANSLATIONS (strings).  Not a port.
    subroutine vector_candidates(translations, cands)
        character(len=*), intent(in) :: translations(:)
        type(quail_candidate_type), allocatable, intent(out) :: cands(:)
        integer :: k

        allocate (cands(size(translations)))

        do k = 1, size(translations)
            cands(k)%s = trim(translations(k))
        end do
    end subroutine vector_candidates

    !> Define the translation rule KEY -> TRANSLATION in SELF: a string,
    !> each of whose characters is a candidate (one character is the sole
    !> translation of KEY).  One rule of quail.el:quail-define-rules
    !> (quail-defrule-internal with APPEND and a decode map).
    subroutine map_add_string(self, key, translation)
        class(quail_map_type), intent(inout) :: self
        character(len=*), intent(in) :: key, translation
        type(quail_candidate_type), allocatable :: cands(:)
        integer(int64), allocatable :: codes(:)

        call string_to_codes(key, codes)
        call string_candidates(translation, .true., cands)
        call quail_defrule_internal(codes, cands, .true., self, .true., .true.)
    end subroutine map_add_string

    !> Define the translation rule KEY -> CANDIDATES in SELF: a vector of
    !> strings, each a candidate.  One rule of quail.el:quail-define-rules
    !> (quail-defrule-internal with APPEND and a decode map).
    subroutine map_add_vector(self, key, candidates)
        class(quail_map_type), intent(inout) :: self
        character(len=*), intent(in) :: key, candidates(:)
        type(quail_candidate_type), allocatable :: cands(:)
        integer(int64), allocatable :: codes(:)

        call string_to_codes(key, codes)
        call vector_candidates(candidates, cands)
        call quail_defrule_internal(codes, cands, .false., self, .true., .true.)
    end subroutine map_add_vector

    ! ------------------------------------------------------------------
    ! Packages
    ! ------------------------------------------------------------------

    !> The index in `quail_package_alist' of the Quail package named NAME,
    !> 0 if there is none.  Port of quail.el:quail-package.
    integer function quail_package_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = quail_npackages, 1, -1
            if (quail_package_alist(k)%name == name) return
        end do

        k = 0
    end function quail_package_index

    !> The index of the current Quail package (0 for none).  Not a port.
    integer function current_package() result(k)
        k = 0
        if (cur /= 0) k = buffers(cur)%quail_current_package
        if (k > quail_npackages) k = 0
    end function current_package

    !> Return the name of the current Quail package.  Port of
    !> quail.el:quail-name.
    function quail_name() result(name)
        character(len=:), allocatable :: name
        integer :: k

        name = ""
        k = current_package()
        if (k > 0) name = quail_package_alist(k)%name
    end function quail_name

    !> Return the title of the current Quail package.  Port of
    !> quail.el:quail-title (a title given as a list is not ported).
    function quail_title() result(title)
        character(len=:), allocatable :: title
        integer :: k

        title = ""
        k = current_package()
        if (k > 0) title = quail_package_alist(k)%title
    end function quail_title

    !> Add Quail package PACKAGE to `quail_package_alist'.  Port of
    !> quail.el:quail-add-package.
    subroutine quail_add_package(package)
        type(quail_package_type), intent(in) :: package
        type(quail_package_type), allocatable :: grown(:)
        integer :: k

        k = quail_package_index(package%name)

        if (k > 0) then
            quail_package_alist(k) = package
            return
        end if

        if (.not. allocated(quail_package_alist)) allocate (quail_package_alist(8))

        if (quail_npackages == size(quail_package_alist)) then
            allocate (grown(2*size(quail_package_alist)))

            do k = 1, quail_npackages
                grown(k) = quail_package_alist(k)
            end do

            call move_alloc(grown, quail_package_alist)
        end if

        quail_npackages = quail_npackages + 1
        quail_package_alist(quail_npackages) = package
    end subroutine quail_add_package

    !> Add a copy of the Quail package OLD named NEW to
    !> `quail_package_alist', as latin-post.el does for a name kept for
    !> backwards compatibility: (push (cons NEW (cdr (assoc OLD
    !> quail-package-alist))) quail-package-alist).  The copy is the most
    !> recent package, and has its own Quail map.  Not a port.
    subroutine quail_copy_package(new, old)
        character(len=*), intent(in) :: new, old
        type(quail_package_type) :: package
        integer :: k

        k = quail_package_index(old)
        if (k == 0) return
        package = quail_package_alist(k)
        package%name = new
        call quail_add_package(package)
    end subroutine quail_copy_package

    !> Select Quail package named NAME as the current Quail package.  Port
    !> of quail.el:quail-select-package (the setq-default is not ported).
    subroutine quail_select_package(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k

        k = quail_package_index(name)

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, format_message("No Quail package `"//name//"'"))
            return
        end if

        if (cur /= 0) buffers(cur)%quail_current_package = k
    end subroutine quail_select_package

    !> Register the built-in input method NAME (a Quail package) for
    !> language environment LANG_ENV, with its mode line TITLE and the
    !> first line of its docstring DESCRIPTION; LIBRARY defines its
    !> package (and the others of its file) when it is first used.  The
    !> form register-input-method takes in leim-list.el (with ACTIVATE-FUNC
    !> quail-use-package and the library as its argument).  Not a port.
    subroutine register_quail_input_method(name, lang_env, title, description, library)
        character(len=*), intent(in) :: name, lang_env, title, description
        procedure(quail_library_proc) :: library
        type(quail_library_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(quail_libraries)) allocate (quail_libraries(0))
        n = size(quail_libraries)

        do k = 1, n
            if (quail_libraries(k)%name == name) exit
        end do

        if (k > n) then
            allocate (grown(n + 1))
            grown(1:n) = quail_libraries
            call move_alloc(grown, quail_libraries)
            quail_libraries(n + 1)%name = name
            k = n + 1
        end if

        quail_libraries(k)%load => library
        call register_input_method(name, lang_env, quail_use_package, title, description)
    end subroutine register_quail_input_method

    !> Start using Quail package INPUT_METHOD.
    !> The package's library is loaded first if the package is not defined
    !> yet.
    !>
    !> This activates input method defined by INPUT_METHOD by running
    !> `quail_activate', which see.  Port of quail.el:quail-use-package
    !> (the activate procedure of the input methods in
    !> `input_method_alist'; a library is built in, so loading it can't
    !> fail).
    recursive subroutine quail_use_package(input_method, status)
        character(len=*), intent(in) :: input_method
        type(status_type), intent(out) :: status
        integer :: k

        if (quail_package_index(input_method) == 0 .and. allocated(quail_libraries)) then
            ! Perhaps we have not yet loaded necessary libraries.
            do k = 1, size(quail_libraries)
                if (quail_libraries(k)%name /= input_method) cycle

                if (associated(quail_libraries(k)%load)) then
                    call quail_libraries(k)%load(status)
                    if (.not. status%ok()) return
                end if

                exit
            end do
        end if

        call quail_select_package(input_method, status)
        if (.not. status%ok()) return
        buffers(cur)%current_input_method_title = quail_title()
        call quail_activate(status)
        if (.not. status%ok()) return
        ! Hide all '... loaded' message.
        call message_nolog("")
    end subroutine quail_use_package

    !> Define NAME as a new Quail package for input LANGUAGE.
    !> TITLE is a string to be displayed at mode-line to indicate this package.
    !> Optional arguments are GUIDANCE, DOCSTRING, TRANSLATION_KEYS,
    !> FORGET_LAST_SELECTION, DETERMINISTIC, KBD_TRANSLATE, SHOW_LAYOUT,
    !> MAXIMUM_SHORTEST and SIMPLE.
    !>
    !> GUIDANCE specifies how a guidance string is shown in echo area.
    !> If it is true, list of all possible translations for the current key is shown
    !> with the currently selected translation being highlighted.
    !> If it is false, the current key is shown.
    !>
    !> DOCSTRING is the documentation string of this package.  The command
    !> `describe_input_method' shows this string while replacing the form
    !> \=\<VAR> in the string by the value of VAR.  That value should be a
    !> string.  For instance, the form \=\<quail_translation_docstring> is
    !> replaced by a description about how to select a translation from a
    !> list of candidates.
    !>
    !> TRANSLATION_KEYS specifies additional key bindings used while translation
    !> region is active.  It is an array of keys and the corresponding
    !> commands to be called.
    !>
    !> FORGET_LAST_SELECTION true means a selected translation is not kept
    !> for the future to translate the same key.  If this flag is false, a
    !> translation selected for a key is remembered so that it can be the
    !> first candidate when the same key is entered later.
    !>
    !> DETERMINISTIC true means the first candidate of translation is
    !> selected automatically without allowing users to select another
    !> translation for a key.  In this case, unselected translations are of
    !> no use for an interactive use of Quail but can be used by some other
    !> programs.  If this flag is true, FORGET_LAST_SELECTION is also set
    !> to true.
    !>
    !> KBD_TRANSLATE true means input characters are translated from a
    !> user's keyboard layout to the standard keyboard layout.  See the
    !> documentation of `quail_keyboard_layout' and
    !> `QUAIL_KEYBOARD_LAYOUT_STANDARD' for more detail.
    !>
    !> SHOW_LAYOUT true means the function `quail_help' (as used by
    !> the command `describe_input_method') should show the user's keyboard
    !> layout visually with translated characters.  If KBD_TRANSLATE is
    !> set, it is desirable to also set this flag, unless this package
    !> defines no translations for single character keys.
    !>
    !> MAXIMUM_SHORTEST true means break key sequence to get maximum
    !> length of the shortest sequence.  When we don't have a translation of
    !> key "..ABCD" but have translations of "..AB" and "CD..", break
    !> the key at "..AB" and start translation of "CD..".  Hangul
    !> packages, for instance, use this facility.  If this flag is false, we
    !> break the key just at "..ABC" and start translation of "D..".
    !>
    !> If SIMPLE is true, then we do not alter the meanings of
    !> commands such as \[forward_char], \[backward_char], \[next_line],
    !> \[previous_line] and \[indent_for_tab_command]; they are treated as
    !> non-Quail commands.  Port of quail.el:quail-define-package
    !> (CREATE-DECODE-MAP, OVERLAY-PLIST, UPDATE-TRANSLATION-FUNCTION and
    !> CONVERSION-KEYS are not ported).
    subroutine quail_define_package(name, language, title, guidance, docstring, translation_keys, &
                                    forget_last_selection, deterministic, kbd_translate, show_layout, &
                                    maximum_shortest, simple)
        character(len=*), intent(in) :: name, language, title
        logical, intent(in), optional :: guidance
        character(len=*), intent(in), optional :: docstring
        type(quail_translation_key_type), intent(in), optional :: translation_keys(:)
        logical, intent(in), optional :: forget_last_selection, deterministic, kbd_translate, show_layout, &
                                         maximum_shortest, simple
        type(quail_package_type) :: package
        type(handle_type) :: keymap
        type(status_type) :: st
        integer :: k

        call syms_of_quail()
        package%name = name
        package%title = title
        package%docstring = ""
        if (present(docstring)) package%docstring = docstring
        if (present(guidance)) package%guidance = guidance
        if (present(forget_last_selection)) package%forget_last_selection = forget_last_selection
        if (present(deterministic)) package%deterministic = deterministic
        if (present(kbd_translate)) package%kbd_translate = kbd_translate
        if (present(show_layout)) package%show_layout = show_layout
        if (present(maximum_shortest)) package%maximum_shortest = maximum_shortest
        if (present(simple)) package%simple = simple
        if (package%deterministic) package%forget_last_selection = .true.

        if (package%simple) then
            package%translation_keymap = quail_simple_translation_keymap
        else
            package%translation_keymap = quail_translation_keymap
        end if

        if (present(translation_keys)) then
            if (size(translation_keys) > 0) then
                call copy_keymap(quail_simple_translation_keymap, keymap, st)
                if (.not. package%simple) call copy_keymap(quail_translation_keymap, keymap, st)
                package%translation_keymap = keymap

                do k = 1, size(translation_keys)
                    call bind(package%translation_keymap, trim(translation_keys(k)%key), trim(translation_keys(k)%command))
                end do
            end if
        end if

        call map_init(package%map)
        call quail_add_package(package)
        ! Update input-method-alist.
        call register_input_method(name, language, quail_use_package, title, package%docstring)
        call quail_select_package(name, st)
    end subroutine quail_define_package

    !> Install the Quail map MAP in the current Quail package.
    !>
    !> The installed map can be referred by the function `quail_map'.  Port
    !> of quail.el:quail-install-map.
    subroutine quail_install_map(map, status)
        type(quail_map_type), intent(in) :: map
        type(status_type), intent(out) :: status
        integer :: k

        k = current_package()

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No current Quail package")
            return
        end if

        quail_package_alist(k)%map = map
        if (.not. allocated(quail_package_alist(k)%map%nodes)) call map_init(quail_package_alist(k)%map)
    end subroutine quail_install_map

    !> Add one translation rule, KEY to TRANSLATION, in the current Quail package.
    !> KEY is a string meaning a sequence of keystrokes to be translated.
    !> TRANSLATION is a string: each character is a candidate for the
    !> translation, and a single character is the sole translation of KEY.
    !> CANDIDATES instead is a vector, each element (a string) a candidate
    !> for the translation.
    !>
    !> NAME, if specified, says which Quail package
    !> to define this translation rule in.  The default is to define it in the
    !> current Quail package.
    !>
    !> APPEND, if true, appends TRANSLATION
    !> to the current translations for KEY instead of replacing them.  Port
    !> of quail.el:quail-defrule (TRANSLATION as a Quail map, a function or
    !> a cons is not ported).
    subroutine quail_defrule(key, translation, status, candidates, name, append)
        character(len=*), intent(in) :: key
        character(len=*), intent(in), optional :: translation
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: candidates(:)
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: append
        type(quail_candidate_type), allocatable :: cands(:)
        integer(int64), allocatable :: codes(:)
        logical :: app
        integer :: k

        if (present(name)) then
            k = quail_package_index(name)

            if (k == 0) then
                call set_error(status, FM_GENERIC_ERROR, format_message("No Quail package `"//name//"'"))
                return
            end if

            if (cur /= 0) buffers(cur)%quail_current_package = k
        end if

        k = current_package()

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No current Quail package")
            return
        end if

        app = .false.
        if (present(append)) app = append
        call string_to_codes(key, codes)

        if (present(candidates)) then
            call vector_candidates(candidates, cands)
            call quail_defrule_internal(codes, cands, .false., quail_package_alist(k)%map, app, .false.)
        else if (present(translation)) then
            call string_candidates(translation, .true., cands)
            call quail_defrule_internal(codes, cands, .true., quail_package_alist(k)%map, app, .false.)
        else
            call set_error(status, FM_GENERIC_ERROR, "Invalid Quail translation `nil'")
        end if
    end subroutine quail_defrule

    ! ------------------------------------------------------------------
    ! Activation
    ! ------------------------------------------------------------------

    !> The overlay of the current buffer's translation region (made empty
    !> at point if the buffer has none yet).  Not a port (the buffer-local
    !> quail-overlay).
    subroutine buffer_overlay(ov, made)
        type(handle_type), intent(out) :: ov
        logical, intent(out) :: made
        integer(int64), allocatable :: s(:)
        type(handle_type), allocatable :: o(:)
        type(status_type) :: st
        integer :: k, n

        made = .false.
        if (.not. allocated(overlay_serials)) allocate (overlay_serials(0), overlays(0))

        do k = 1, size(overlay_serials)
            if (overlay_serials(k) == buffers(cur)%serial .and. overlayp(overlays(k))) then
                ov = overlays(k)
                return
            end if
        end do

        ! Forget the overlays of killed buffers.
        n = 0
        allocate (s(size(overlay_serials) + 1), o(size(overlay_serials) + 1))

        do k = 1, size(overlay_serials)
            if (buffer_serial_live(overlay_serials(k))) then
                n = n + 1
                s(n) = overlay_serials(k)
                o(n) = overlays(k)
            end if
        end do

        call make_overlay(ov, point(), point(), st)
        made = .true.
        n = n + 1
        s(n) = buffers(cur)%serial
        o(n) = ov
        overlay_serials = s(1:n)
        overlays = o(1:n)
    end subroutine buffer_overlay

    !> Setup overlays used in Quail mode.  Port of
    !> quail.el:quail-setup-overlays (no conversion region).
    subroutine quail_setup_overlays()
        type(handle_type) :: ov
        type(status_type) :: st
        logical :: made

        call buffer_overlay(ov, made)

        if (.not. made) then
            call move_overlay(ov, point(), point(), st, current_buffer())
        else if (input_method_highlight_flag) then
            call overlay_put(ov, "face", sym("underline"), st)
        end if
    end subroutine quail_setup_overlays

    !> Delete overlays used in Quail mode.  Port of
    !> quail.el:quail-delete-overlays.
    subroutine quail_delete_overlays()
        integer :: k

        if (.not. allocated(overlay_serials) .or. cur == 0) return

        do k = 1, size(overlay_serials)
            if (overlay_serials(k) == buffers(cur)%serial) then
                if (overlay_start(overlays(k)) /= 0) call delete_overlay(overlays(k))
            end if
        end do
    end subroutine quail_delete_overlays

    !> The current buffer's translation region (0 and 0 if it has none).
    !> Not a port.
    subroutine translation_region(start, end)
        integer(int64), intent(out) :: start, end
        integer :: k

        start = 0
        end = 0
        if (.not. allocated(overlay_serials) .or. cur == 0) return

        do k = 1, size(overlay_serials)
            if (overlay_serials(k) == buffers(cur)%serial) then
                start = overlay_start(overlays(k))
                end = overlay_end(overlays(k))
                return
            end if
        end do
    end subroutine translation_region

    !> Deactivate Quail input method.
    !>
    !> This function runs the normal hook `quail_deactivate_hook'.  Port of
    !> quail.el:quail-deactivate.
    recursive subroutine quail_deactivate(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()
        ! Let's deactivate Quail input method.
        call quail_delete_overlays()
        buffers(cur)%describe_current_input_method_function => null()
        call quail_hide_guidance()
        call post_command_hook%remove(quail_show_guidance_hook, local=.true.)
        call quail_deactivate_hook%run(st)
        buffers(cur)%input_method_function => null()
    end subroutine quail_deactivate

    recursive subroutine cmd_quail_deactivate(status)
        type(status_type), intent(out) :: status

        call quail_deactivate(status)
    end subroutine cmd_quail_deactivate

    !> Activate Quail input method.
    !>
    !> This function runs `quail_activate_hook' when it activates the input
    !> method.
    !>
    !> While this input method is active, the variable
    !> `input_method_function' is bound to the function `quail_input_method'.
    !> Port of quail.el:quail-activate (deactivating is `quail_deactivate';
    !> minibuffer-exit-hook is not ported).
    recursive subroutine quail_activate(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()

        if (current_package() == 0) then
            ! Quail package is not yet selected.  Select one now.
            if (quail_npackages == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No Quail package loaded")
                return
            end if

            call quail_select_package(quail_package_alist(quail_npackages)%name, status)
            if (.not. status%ok()) return
        end if

        buffers(cur)%deactivate_current_input_method_function => quail_deactivate
        buffers(cur)%describe_current_input_method_function => quail_help_proc
        call quail_delete_overlays()
        quail_guidance_str = ""
        call quail_show_guidance()
        ! If we are in minibuffer, show the guidance after each command.
        if (selected_frame%minibuffer_selected) call post_command_hook%add(quail_show_guidance_hook, local=.true.)
        call quail_activate_hook%run(st)
        buffers(cur)%input_method_function => quail_input_method
    end subroutine quail_activate

    ! ------------------------------------------------------------------
    ! Keyboard layouts
    ! ------------------------------------------------------------------

    !> The characters of the keyboard layout LAYOUT (blanks past its end).
    !> Not a port.
    subroutine layout_codes(layout, codes)
        character(len=*), intent(in) :: layout
        integer(int64), intent(out) :: codes(QUAIL_KEYBOARD_LAYOUT_LEN)
        integer(int64), allocatable :: all(:)
        integer :: n

        call string_to_codes(layout, all)
        codes = 32
        n = min(size(all), QUAIL_KEYBOARD_LAYOUT_LEN)
        codes(1:n) = all(1:n)
    end subroutine layout_codes

    !> Make `quail_keyboard_layout' the layout of KBD_TYPE and compute the
    !> substitutions of its key locations.  Port of
    !> quail.el:quail-update-keyboard-layout.
    subroutine quail_update_keyboard_layout(kbd_type, status)
        character(len=*), intent(in) :: kbd_type
        type(status_type), intent(out) :: status
        integer(int64) :: layout(QUAIL_KEYBOARD_LAYOUT_LEN), standard(QUAIL_KEYBOARD_LAYOUT_LEN)
        integer, allocatable :: missing(:)
        integer :: i, k, j

        do k = 1, size(quail_keyboard_layout_alist)
            if (trim(quail_keyboard_layout_alist(k)%name) == kbd_type) exit
        end do

        if (k > size(quail_keyboard_layout_alist)) then
            ! Here, we had better ask a user to define his own keyboard
            ! layout interactively.
            call set_error(status, FM_GENERIC_ERROR, format_message("Unknown keyboard type `"//kbd_type//"'"))
            return
        end if

        quail_keyboard_layout = quail_keyboard_layout_alist(k)%layout
        call layout_codes(quail_keyboard_layout, layout)
        call layout_codes(QUAIL_KEYBOARD_LAYOUT_STANDARD, standard)
        allocate (missing(0))
        if (allocated(subst_from)) deallocate (subst_from, subst_to)
        allocate (subst_from(0), subst_to(0))

        ! Sum up additional key locations not in the standard layout in
        ! subst-list, and missing key locations in missing-list.
        do i = 1, QUAIL_KEYBOARD_LAYOUT_LEN
            if (layout(i) == 32) then
                if (standard(i) /= 32) missing = [missing, i - 1]
            else if (standard(i) == 32) then
                subst_from = [subst_from, i - 1]
                subst_to = [subst_to, -1]
            end if
        end do

        ! If there are additional key locations, map them to missing
        ! key locations.
        j = 1

        do k = 1, size(missing)
            do while (j <= size(subst_to))
                if (subst_to(j) < 0) exit
                j = j + 1
            end do

            if (j <= size(subst_to)) subst_to(j) = missing(k)
        end do
    end subroutine quail_update_keyboard_layout

    !> Set the current keyboard layout to the same as keyboard KBD_TYPE.
    !>
    !> Since some Quail packages depends on a physical layout of keys (not
    !> characters generated by them), those are created by assuming the
    !> standard layout defined in `QUAIL_KEYBOARD_LAYOUT_STANDARD'.  This
    !> function tells Quail system the layout of your keyboard so that what
    !> you type is correctly handled.  Port of
    !> quail.el:quail-set-keyboard-layout.
    subroutine quail_set_keyboard_layout(kbd_type, status)
        character(len=*), intent(in) :: kbd_type
        type(status_type), intent(out) :: status

        call quail_update_keyboard_layout(kbd_type, status)
        if (.not. status%ok()) return
        quail_keyboard_layout_type = kbd_type
    end subroutine quail_set_keyboard_layout

    recursive subroutine cmd_quail_set_keyboard_layout(status)
        use minibuf_fm, only: completion_list_table, completion_ignore_case
        use minibuffer_fm, only: completing_read
        use lisp_fm, only: make_string, make_list
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: kbd_type
        logical :: saved_case
        integer :: k

        allocate (items(size(quail_keyboard_layout_alist)))

        do k = 1, size(items)
            items(k) = make_string(trim(quail_keyboard_layout_alist(k)%name))
        end do

        saved_case = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read("Keyboard type: ", completion_list_table(make_list(items)), kbd_type, status)
        completion_ignore_case%value = saved_case
        if (.not. status%ok()) return
        call quail_set_keyboard_layout(kbd_type, status)
    end subroutine cmd_quail_set_keyboard_layout

    !> Translate CHAR to the one in the standard keyboard layout.  Port of
    !> quail.el:quail-keyboard-translate.
    integer(int64) function quail_keyboard_translate(char) result(ch)
        integer(int64), intent(in) :: char
        integer(int64) :: layout(QUAIL_KEYBOARD_LAYOUT_LEN), standard(QUAIL_KEYBOARD_LAYOUT_LEN)
        integer :: i, k

        ch = char
        ! All Quail packages are designed based on
        ! `quail-keyboard-layout-standard'.
        if (quail_keyboard_layout == QUAIL_KEYBOARD_LAYOUT_STANDARD) return
        call layout_codes(quail_keyboard_layout, layout)
        call layout_codes(QUAIL_KEYBOARD_LAYOUT_STANDARD, standard)
        ! Find the key location on the current keyboard layout.
        i = findloc(layout, char, 1)
        ! CHAR is not in quail-keyboard-layout, which means that a
        ! user typed a key which generated a character code to be
        ! handled out of Quail.  Just return CHAR.
        if (i == 0) return

        if (standard(i) /= 32) then
            ch = standard(i)
            return
        end if

        ! This location not available in the standard keyboard
        ! layout.  Check if the location is used to substitute
        ! for the other location of the standard layout.
        if (.not. allocated(subst_from)) return

        do k = 1, size(subst_from)
            if (subst_from(k) == i - 1) then
                if (subst_to(k) >= 0) ch = standard(subst_to(k) + 1)
                return
            end if
        end do
    end function quail_keyboard_translate

    !> KEYSEQ with each key translated to the standard keyboard layout.
    !> Port of quail.el:quail-keyseq-translate.
    function quail_keyseq_translate(keyseq) result(s)
        integer(int64), intent(in) :: keyseq(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(keyseq)
            s = s//char_to_string(quail_keyboard_translate(keyseq(k)))
        end do
    end function quail_keyseq_translate

    !> The current key as text: translated to the standard layout if the
    !> package wants it.  Not a port.
    function current_key_string() result(s)
        character(len=:), allocatable :: s
        integer :: p

        p = current_package()
        s = ""
        if (.not. allocated(quail_current_key)) return

        if (p > 0) then
            if (quail_package_alist(p)%kbd_translate) then
                s = quail_keyseq_translate(quail_current_key)
                return
            end if
        end if

        s = codes_to_string(quail_current_key)
    end function current_key_string

    ! ------------------------------------------------------------------
    ! Lookup
    ! ------------------------------------------------------------------

    !> Lookup KEY of length LEN in the current Quail map and return the definition.
    !> The returned value is the node of the Quail map specific to KEY (0
    !> for none).  Unless NOT_RESET_INDICES, a package that forgets the last
    !> selection starts again from the first candidate.  Port of
    !> quail.el:quail-lookup-key (maps given as functions are not ported).
    integer function quail_lookup_key(key, len, not_reset_indices) result(node)
        integer(int64), intent(in) :: key(:)
        integer, intent(in) :: len
        logical, intent(in), optional :: not_reset_indices
        integer :: p, idx
        integer(int64) :: ch
        logical :: not_reset

        node = 0
        quail_current_translations = 0
        p = current_package()
        if (p == 0) return
        not_reset = .false.
        if (present(not_reset_indices)) not_reset = not_reset_indices

        associate (pkg => quail_package_alist(p))
            if (.not. allocated(pkg%map%nodes)) return
            node = 1
            idx = 0

            do while (node /= 0 .and. idx < len)
                idx = idx + 1
                ch = key(idx)
                if (pkg%kbd_translate) ch = quail_keyboard_translate(ch)
                node = map_child(pkg%map, node, ch)
            end do

            if (node == 0) return

            if (pkg%map%nodes(node)%kind == TRANS_VECTOR) then
                quail_current_translations = node
                if (.not. not_reset .and. pkg%forget_last_selection) pkg%map%nodes(node)%current = 0
            end if
        end associate
    end function quail_lookup_key

    !> Whether NODE of the current package's map has a translation (a
    !> definition, quail-map-definition).  Not a port.
    logical function node_has_definition(node) result(has)
        integer, intent(in) :: node
        integer :: p

        has = .false.
        p = current_package()
        if (node == 0 .or. p == 0) return
        has = quail_package_alist(p)%map%nodes(node)%kind /= TRANS_NONE
    end function node_has_definition

    !> Whether NODE of the current package's map has longer keys (the cdr
    !> of the map).  Not a port.
    logical function node_has_children(node) result(has)
        integer, intent(in) :: node
        integer :: p

        has = .false.
        p = current_package()
        if (node == 0 .or. p == 0) return
        has = quail_package_alist(p)%map%nodes(node)%child /= 0
    end function node_has_children

    !> Return string to be shown as current translation of key sequence.
    !> LEN is the length of the sequence.  NODE holds the definition part
    !> of the Quail map for the sequence.  STR is unallocated for nil.
    !> Port of quail.el:quail-get-current-str.
    recursive subroutine quail_get_current_str(len, node, str)
        integer, intent(in) :: len, node
        character(len=:), allocatable, intent(out) :: str
        character(len=:), allocatable :: str1, s
        integer :: p, n

        p = current_package()
        if (p == 0) return

        if (node_has_definition(node)) then
            associate (nd => quail_package_alist(p)%map%nodes(node))
                if (nd%kind == TRANS_VECTOR) then
                    if (nd%count > nd%current) then
                        str = quail_package_alist(p)%map%cands(nd%first + nd%current)%s
                    else
                        str = ""
                    end if
                else
                    str = quail_package_alist(p)%map%cands(nd%first)%s
                end if
            end associate

            return
        end if

        if (len <= 1) return
        n = quail_lookup_key(quail_current_key, len - 1)
        call quail_get_current_str(len - 1, n, s)

        if (quail_package_alist(p)%deterministic) then
            n = quail_lookup_key(quail_current_key(len:len), 1)
            call quail_get_current_str(1, n, str1)
        end if

        if (.not. allocated(s)) return

        if (allocated(str1)) then
            str = s//str1
        else
            str = s//char_to_string(quail_current_key(len))
        end if
    end subroutine quail_get_current_str

    !> The width of the selected window's text area (window-width).  Not a
    !> port.
    integer function window_width() result(w)
        type(handle_type) :: sw

        w = 80
        sw = selected_window()
        if (window_live_p(sw)) w = window_text_cols(windows(sw%index))
    end function window_width

    !> Update `quail_current_translations'.
    !> Make RELATIVE_INDEX the current translation.  Port of
    !> quail.el:quail-update-current-translations.
    subroutine quail_update_current_translations(relative_index)
        integer, intent(in), optional :: relative_index
        integer :: p, node, cur_idx, start, end, len, maxcol, block, col, idx, width, num_items

        p = current_package()
        node = quail_current_translations
        if (p == 0 .or. node == 0) return

        associate (map => quail_package_alist(p)%map)
            associate (nd => map%nodes(node))
                cur_idx = nd%current
                start = nd%start
                end = nd%end
                len = nd%count

                ! Validate the index number of current translation.
                if (cur_idx < 0) then
                    cur_idx = 0
                    nd%current = 0
                else if (cur_idx >= len) then
                    cur_idx = len - 1
                    nd%current = cur_idx
                end if

                ! We moved to the previous or the next block.
                if (cur_idx < start .or. cur_idx >= end) then
                    maxcol = window_width() - quail_guidance_translations_starting_column
                    block = nd%block

                    if (cur_idx < start) then
                        ! We must calculate from the head.
                        start = 0
                        block = 0
                    else
                        start = end
                    end if

                    idx = start
                    col = 0
                    end = start
                    num_items = 0

                    ! Loop until we hit the tail, or reach the block of CUR.
                    do while (idx < len .and. cur_idx >= end)
                        if (num_items == 0) then
                            start = idx
                            col = 0
                            block = block + 1
                        end if

                        width = int(string_width(map%cands(nd%first + idx)%s))
                        col = col + width + 3
                        num_items = num_items + 1

                        if (num_items > 0 .and. (col >= maxcol .or. num_items > 10)) then
                            end = idx
                            num_items = 0
                        else
                            idx = idx + 1
                        end if
                    end do

                    nd%block = block

                    if (idx >= len) then
                        ! We hit the tail before reaching MAXCOL.
                        end = idx
                        nd%blocks = block
                    end if

                    nd%start = start
                    nd%end = end
                end if

                if (present(relative_index)) then
                    if (start + relative_index >= end) then
                        nd%current = end - 1
                    else
                        nd%current = start + relative_index
                    end if
                end if

                quail_current_str = map%cands(nd%first + nd%current)%s
            end associate
        end associate
    end subroutine quail_update_current_translations

    !> Translate the current key sequence according to the current Quail map.
    !> Return CONTROL_T if we can terminate the translation.
    !> Return CONTROL_NIL if the current key sequence may be followed by more keys.
    !> Return number if we can't find any translation for the current key
    !> sequence.  The number is the count of valid keys in the current
    !> sequence counting from the head.  Port of quail.el:quail-translate-key.
    integer function quail_translate_key() result(flag)
        integer :: len, node, p, n, def, ch
        logical :: terminate

        p = current_package()
        len = size(quail_current_key)
        node = quail_lookup_key(quail_current_key, len)

        if (node /= 0) then
            call quail_get_current_str(len, node, quail_current_str)
            flag = CONTROL_NIL

            ! Return t only if we can terminate the current translation.
            associate (nd => quail_package_alist(p)%map%nodes(node))
                ! No alternative translations.
                terminate = nd%kind /= TRANS_VECTOR .or. nd%count == 1
                ! No translation for the longer key.
                if (terminate) terminate = nd%child == 0
            end associate

            ! No shorter breaking point.
            if (terminate .and. quail_package_alist(p)%maximum_shortest .and. len >= 3) then
                if (quail_lookup_key(quail_current_key, len - 1) /= 0) then
                    terminate = quail_lookup_key(quail_current_key(len - 1:len - 1), 1) == 0
                end if
            end if

            if (terminate) flag = CONTROL_T
            return
        end if

        ! There's no translation for the current key sequence.  Before
        ! giving up, we must check two possibilities.
        if (quail_package_alist(p)%maximum_shortest .and. len >= 3) then
            def = quail_lookup_key(quail_current_key, len - 2)

            if (node_has_definition(def)) then
                if (quail_lookup_key(quail_current_key(len - 1:len), 2) /= 0) then
                    ! Now the sequence is "...ABCD", which can be split into
                    ! "...AB" and "CD..." to get valid translation.
                    ! At first, get translation of "...AB".
                    call quail_get_current_str(len - 2, def, quail_current_str)
                    ! Then, return the length of "...AB".
                    flag = len - 2
                    return
                end if
            end if
        end if

        if (len > 0) then
            n = quail_lookup_key(quail_current_key(1:len - 1), len - 1)

            if (n /= 0 .and. quail_current_translations /= 0 .and. .not. quail_package_alist(p)%deterministic) then
                ch = int(quail_current_key(len))

                if (ch >= iachar("0") .and. ch <= iachar("9")) then
                    ! A numeric key is entered to select a desirable translation.
                    quail_current_key = quail_current_key(1:len - 1)

                    ! We treat key 1,2..,9,0 as specifying 0,1,..8,9.
                    if (ch == iachar("0")) then
                        ch = 9
                    else
                        ch = ch - iachar("1")
                    end if

                    call quail_update_current_translations(ch)
                    ! And, we can terminate the current translation.
                    flag = CONTROL_T
                    return
                end if
            end if
        end if

        if (quail_package_alist(p)%deterministic) then
            ! No way to handle the last character in this context.
            ! Commit the longest successfully translated characters, and
            ! handle the remaining characters in a new loop.
            def = 0

            do while (.not. node_has_definition(def) .and. len > 1)
                len = len - 1
                def = quail_lookup_key(quail_current_key, len)
            end do

            if (node_has_definition(def)) then
                call quail_get_current_str(len, def, quail_current_str)
            else
                quail_current_str = char_to_string(quail_current_key(1))
            end if

            flag = len
            return
        end if

        ! No way to handle the last character in this context.
        n = quail_lookup_key(quail_current_key, len - 1)
        if (node_has_definition(n)) call quail_get_current_str(len - 1, n, quail_current_str)
        flag = len - 1
    end function quail_translate_key

    ! ------------------------------------------------------------------
    ! Translation
    ! ------------------------------------------------------------------

    !> Add KEYS to `unread_command_events', but avoid recording them a
    !> second time (the (no-record . EVENT) form).  Port of
    !> quail.el:quail-add-unread-command-events (without RESET).
    subroutine quail_add_unread_command_events(keys)
        type(value_type), intent(in) :: keys(:)

        call unread_command_events(keys, no_record=.true.)
    end subroutine quail_add_unread_command_events

    !> Add the character KEY to `unread_command_events', not to be recorded
    !> again.  Not a port.
    subroutine unread_char(key)
        integer(int64), intent(in) :: key
        type(value_type) :: ev(1)

        ev(1) = make_fixnum(key)
        call quail_add_unread_command_events(ev)
    end subroutine unread_char

    !> The input method of Quail: translate the character KEY and the keys
    !> that follow it with the current package, inserting the translation
    !> in progress, and return the result as EVENTS.  Port of
    !> quail.el:quail-input-method (no conversion; `overriding_local_map'
    !> is not ported).
    recursive subroutine quail_input_method(key, events, status)
        integer(int64), intent(in) :: key
        type(value_type), allocatable, intent(out) :: events(:)
        type(status_type), intent(out) :: status
        type(silent_modifications_type) :: silent
        type(value_type) :: binding, ev(1)
        type(status_type) :: st
        character(len=:), allocatable :: input_string
        integer(int64), allocatable :: codes(:)
        logical :: read_only
        integer :: k

        read_only = buffers(cur)%read_only

        if (.not. read_only) then
            if (.not. nilp(get_char_property(point(), sym("read-only")))) then
                read_only = .not. nilp(get_char_property(point(), sym("front-sticky")))
            end if
        end if

        if (read_only) then
            if (.not. nilp(inhibit_read_only())) then
                read_only = .false.
            else if (.not. nilp(get_char_property(point(), sym("inhibit-read-only")))) then
                read_only = .false.
            end if
        end if

        ! If the overriding map is `universal-argument-map', that
        ! must mean the user has pressed 'C-u KEY'.  If KEY has a
        ! binding in it just return (list KEY).
        if (.not. read_only .and. keymap_live_p(overriding_terminal_local_map)) then
            ev(1) = make_fixnum(key)
            call lookup_key(make_keymap_ref(overriding_terminal_local_map), ev, binding, st)
            read_only = .not. nilp(binding)
        end if

        if (read_only) then
            allocate (events(1))
            events(1) = make_fixnum(key)
            return
        end if

        call quail_setup_overlays()
        call silent_modifications_begin(silent)
        ! `with-silent-modifications' inhibits the modification
        ! hooks, but that's a part of `with-silent-modifications'
        ! we don't actually want here (bug#70541).
        inhibit_modification_hooks = .false.
        call quail_start_translation(key, input_string, status)
        inhibit_modification_hooks = .true.

        if (status%ok()) then
            quail_guidance_str = ""

            if (allocated(input_string)) then
                if (len(input_string) > 0) then
                    call string_to_codes(input_string, codes)

                    if (input_method_exit_on_first_char) then
                        allocate (events(1))
                        events(1) = make_fixnum(codes(1))
                    else
                        ! quail-input-string-to-events
                        allocate (events(size(codes)))

                        do k = 1, size(codes)
                            events(k) = make_fixnum(codes(k))
                        end do
                    end if
                end if
            end if
        end if

        if (.not. allocated(events)) allocate (events(0))
        call quail_delete_overlays()
        ! Run this hook only when the current input method doesn't require
        ! conversion.
        call input_method_after_insert_chunk_hook%run(st)
        call silent_modifications_end(silent)
    end subroutine quail_input_method

    !> Delete the text in the current translation region of Quail.  Port
    !> of quail.el:quail-delete-region.
    subroutine quail_delete_region()
        integer(int64) :: start, end
        type(status_type) :: st

        call translation_region(start, end)
        if (start /= 0) call delete_region(start, end, st)
    end subroutine quail_delete_region

    !> Start translation of the typed character KEY by the current Quail package.
    !> Return the input string (unallocated for nil).  Port of
    !> quail.el:quail-start-translation (echoing and `help_char' are not
    !> ported; the prompt of read-key-sequence is shown with
    !> `message_nolog').
    recursive subroutine quail_start_translation(key, input_string, status)
        integer(int64), intent(in) :: key
        character(len=:), allocatable, intent(out) :: input_string
        type(status_type), intent(out) :: status
        procedure(input_method_proc), pointer :: saved_method
        type(handle_type) :: saved_overriding, buf
        type(value_type) :: saved_event, saved_last_command, saved_this_command, cmd
        type(value_type), allocatable :: keys(:), raw(:)
        type(status_type) :: st
        character(len=:), allocatable :: prompt
        integer(int64) :: k
        integer(int64) :: bi
        integer :: p
        logical :: modified, eof

        p = current_package()
        k = key
        if (quail_package_alist(p)%kbd_translate) k = quail_keyboard_translate(key)

        if (map_child(quail_package_alist(p)%map, 1, k) == 0) then
            ! Since KEY doesn't start any translation, just return it.
            ! But translate KEY if necessary.
            input_string = char_to_string(k)
            return
        end if

        ! OK, we can start translation.
        bi = cur
        buf = current_buffer()
        saved_overriding = overriding_terminal_local_map
        overriding_terminal_local_map = quail_package_alist(p)%translation_keymap
        saved_method => buffers(cur)%input_method_function
        buffers(cur)%input_method_function => null()
        call buffer_modified_p(buf, modified, st)
        saved_event = last_command_event
        saved_last_command = last_command
        saved_this_command = this_command
        last_command_event = nil()
        last_command = nil()
        this_command = nil()
        quail_current_key = [integer(int64) ::]
        quail_current_str = ""
        quail_translating = .true.
        call unread_char(key)

        do while (quail_translating)
            call restore_buffer_modified_p(modified)
            call quail_show_guidance()

            if (input_method_use_echo_area) then
                prompt = ""
                if (allocated(input_method_previous_message)) prompt = input_method_previous_message
                if (allocated(quail_current_str)) prompt = prompt//quail_current_str
                call message_nolog(prompt//" "//quail_guidance_str)
            end if

            call read_key_sequence(keys, cmd, eof, status)
            if (.not. status%ok()) exit
            cmd = nil()
            if (size(keys) > 0) call lookup_key(make_keymap_ref(overriding_terminal_local_map), keys, cmd, st)

            if (commandp(cmd) .and. .not. value_eq(cmd, sym("quail_other_command"))) then
                last_command_event = keys(size(keys))
                last_command = this_command
                this_command = cmd
                call call_interactively(cmd, status)
                if (.not. status%ok()) exit
            else
                ! KEYSEQ is not defined in the translation keymap.
                ! Let's return the event(s) to the caller.
                call this_single_command_raw_keys(raw)
                call quail_add_unread_command_events(raw)
                quail_translating = .false.
            end if
        end do

        quail_translating = .false.
        if (status%ok()) call quail_delete_region()
        if (allocated(quail_current_str)) input_string = quail_current_str
        last_command_event = saved_event
        last_command = saved_last_command
        this_command = saved_this_command
        overriding_terminal_local_map = saved_overriding
        if (buffer_live_p(buf)) buffers(bi)%input_method_function => saved_method
    end subroutine quail_start_translation

    !> Terminate the translation of the current key.  Port of
    !> quail.el:quail-terminate-translation.
    subroutine quail_terminate_translation()
        quail_translating = .false.
        quail_guidance_str = " "
    end subroutine quail_terminate_translation

    !> Accept the currently selected translation.  Port of
    !> quail.el:quail-select-current.
    recursive subroutine cmd_quail_select_current(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call quail_terminate_translation()
    end subroutine cmd_quail_select_current

    !> Update the current translation status according to CONTROL_FLAG.
    !> If CONTROL_FLAG is a number, it is the number of keys in the
    !> head `quail_current_key' which can be translated.  The remaining keys
    !> are put back to `unread_command_events' to be handled again.  If
    !> CONTROL_FLAG is CONTROL_T, terminate the translation for the whole keys in
    !> `quail_current_key'.  If CONTROL_FLAG is CONTROL_NIL, proceed the translation
    !> with more keys.  Port of quail.el:quail-update-translation (no
    !> UPDATE-TRANSLATION-FUNCTION or conversion region).
    subroutine quail_update_translation(control_flag)
        integer, intent(in) :: control_flag
        character(len=:), allocatable :: saved
        integer(int64), allocatable :: codes(:)
        type(handle_type) :: ov
        type(status_type) :: st
        integer(int64) :: pos, ch
        integer :: flag, len, p, k
        logical :: made

        flag = control_flag
        p = current_package()

        if (flag >= 0) then
            len = size(quail_current_key)
            if (flag == 0) quail_current_str = current_key_string()

            if (.not. input_method_exit_on_first_char) then
                do while (len > flag)
                    len = len - 1
                    call unread_char(quail_current_key(len + 1))
                end do
            end if
        else if (flag == CONTROL_NIL) then
            if (.not. allocated(quail_current_str)) then
                quail_current_str = current_key_string()
                if (input_method_exit_on_first_char .and. quail_package_alist(p)%simple) flag = CONTROL_T
            end if
        end if

        if (.not. input_method_use_echo_area) then
            call quail_delete_region()
            pos = point()

            if (.not. buffers(cur)%multibyte .and. allocated(quail_current_str)) then
                call string_to_codes(quail_current_str, codes)
                ch = -1

                do k = 1, size(codes)
                    if (codes(k) > 255) then
                        ch = codes(k)
                        exit
                    end if
                end do

                if (ch >= 0) then
                    call message_nolog("Can't input "//char_to_string(ch)//" in the current unibyte buffer")
                    call ding()
                    call sit_for(2000_int32)
                    call message_nolog("")
                    if (allocated(quail_current_str)) deallocate (quail_current_str)
                    quail_tag_thrown = .true.
                    return
                end if
            end if

            if (allocated(quail_current_str)) call insert(quail_current_str, st)
            call buffer_overlay(ov, made)
            call move_overlay(ov, pos, point(), st, current_buffer())
        end if

        if (allocated(quail_current_str)) call move_alloc(quail_current_str, saved)
        call quail_update_guidance()
        if (allocated(saved)) call move_alloc(saved, quail_current_str)
        if (flag /= CONTROL_NIL) call quail_terminate_translation()
    end subroutine quail_update_translation

    !> Translate the typed key by the current Quail map, and insert.  Port
    !> of quail.el:quail-self-insert-command.
    recursive subroutine cmd_quail_self_insert_command(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        quail_current_key = [quail_current_key, xfixnum(last_command_event)]
        quail_tag_thrown = .false.
        call quail_update_translation(quail_translate_key())
        ! If someone throws for `quail-tag' by value nil, we exit from
        ! translation mode.
        if (quail_tag_thrown) quail_translating = .false.
    end subroutine cmd_quail_self_insert_command

    !> Not documented upstream (a dummy definition): keys bound to it end
    !> the translation.  Port of quail.el:quail-other-command.
    recursive subroutine cmd_quail_other_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine cmd_quail_other_command

    !> Put the last command event back and terminate the translation (what
    !> the translation commands do without candidates).  Not a port.
    subroutine unread_last_and_terminate()
        type(value_type) :: ev(1)

        ev(1) = last_command_event
        call quail_add_unread_command_events(ev)
        call quail_terminate_translation()
    end subroutine unread_last_and_terminate

    !> Select next translation in the current batch of candidates.  Port
    !> of quail.el:quail-next-translation.
    recursive subroutine cmd_quail_next_translation(status)
        type(status_type), intent(out) :: status
        integer :: p

        status = status_type()
        p = current_package()

        if (quail_current_translations == 0 .or. p == 0) then
            call unread_last_and_terminate()
            return
        end if

        associate (nd => quail_package_alist(p)%map%nodes(quail_current_translations))
            if (nd%current + 1 == nd%count) then
                ! We are already at the tail.
                call ding()
                return
            end if

            nd%current = nd%current + 1
        end associate

        call quail_update_current_translations()
        call quail_update_translation(CONTROL_NIL)
    end subroutine cmd_quail_next_translation

    !> Select previous translation in the current batch of candidates.
    !> Port of quail.el:quail-prev-translation.
    recursive subroutine cmd_quail_prev_translation(status)
        type(status_type), intent(out) :: status
        integer :: p

        status = status_type()
        p = current_package()

        if (quail_current_translations == 0 .or. p == 0) then
            call unread_last_and_terminate()
            return
        end if

        associate (nd => quail_package_alist(p)%map%nodes(quail_current_translations))
            if (nd%current == 0) then
                ! We are already at the head.
                call ding()
                return
            end if

            nd%current = nd%current - 1
        end associate

        call quail_update_current_translations()
        call quail_update_translation(CONTROL_NIL)
    end subroutine cmd_quail_prev_translation

    !> Select from the next block of translations.  Port of
    !> quail.el:quail-next-translation-block.
    recursive subroutine cmd_quail_next_translation_block(status)
        type(status_type), intent(out) :: status
        integer :: p, offset

        status = status_type()
        p = current_package()

        if (quail_current_translations == 0 .or. p == 0) then
            call unread_last_and_terminate()
            return
        end if

        associate (nd => quail_package_alist(p)%map%nodes(quail_current_translations))
            offset = nd%current - nd%start

            if (nd%end >= nd%count) then
                ! We are already at the last block.
                call ding()
                return
            end if

            nd%current = nd%end + offset
        end associate

        call quail_update_current_translations()
        call quail_update_translation(CONTROL_NIL)
    end subroutine cmd_quail_next_translation_block

    !> Select the previous batch of 10 translation candidates.  Port of
    !> quail.el:quail-prev-translation-block.
    recursive subroutine cmd_quail_prev_translation_block(status)
        type(status_type), intent(out) :: status
        integer :: p, offset, node

        status = status_type()
        p = current_package()
        node = quail_current_translations

        if (node == 0 .or. p == 0) then
            call unread_last_and_terminate()
            return
        end if

        offset = quail_package_alist(p)%map%nodes(node)%current - quail_package_alist(p)%map%nodes(node)%start

        if (quail_package_alist(p)%map%nodes(node)%start == 0) then
            ! We are already at the first block.
            call ding()
            return
        end if

        quail_package_alist(p)%map%nodes(node)%current = quail_package_alist(p)%map%nodes(node)%start - 1
        call quail_update_current_translations()

        associate (nd => quail_package_alist(p)%map%nodes(node))
            if (nd%start + offset < nd%end) then
                nd%current = nd%start + offset
                call quail_update_current_translations()
            end if
        end associate

        call quail_update_translation(CONTROL_NIL)
    end subroutine cmd_quail_prev_translation_block

    !> Abort translation and delete the current Quail key sequence.  Port
    !> of quail.el:quail-abort-translation.
    recursive subroutine cmd_quail_abort_translation(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call quail_delete_region()
        if (allocated(quail_current_str)) deallocate (quail_current_str)
        call quail_terminate_translation()
    end subroutine cmd_quail_abort_translation

    !> Delete the last input character from the current Quail key sequence.
    !> Port of quail.el:quail-delete-last-char.
    recursive subroutine cmd_quail_delete_last_char(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (size(quail_current_key) == 1) then
            call cmd_quail_abort_translation(status)
            return
        end if

        quail_current_key = quail_current_key(1:size(quail_current_key) - 1)
        call quail_delete_region()
        call quail_update_translation(quail_translate_key())
    end subroutine cmd_quail_delete_last_char

    ! ------------------------------------------------------------------
    ! Guidance
    ! ------------------------------------------------------------------

    !> Setup Quail completion buffer.  Port of
    !> quail.el:quail-setup-completion-buf.
    subroutine quail_setup_completion_buf()
        type(handle_type) :: old, ov
        type(status_type) :: st
        logical :: mb, made

        if (buffer_live_p(quail_completion_buf)) return
        mb = buffers(cur)%multibyte
        old = current_buffer()
        call get_buffer_create("*Quail Completions*", quail_completion_buf, st)
        if (.not. st%ok()) return
        call set_buffer(quail_completion_buf, st)
        buffers(cur)%multibyte = mb
        buffers(cur)%read_only = .true.
        call buffer_overlay(ov, made)
        call overlay_put(ov, "face", sym("highlight"), st)
        call set_buffer(old, st)
    end subroutine quail_setup_completion_buf

    !> Return true if the current Quail package requires showing guidance
    !> buffer.  Port of quail.el:quail-require-guidance-buf.
    logical function quail_require_guidance_buf() result(required)
        integer :: p
        logical :: simple

        p = current_package()
        simple = .false.
        if (p > 0) simple = quail_package_alist(p)%simple

        select case (input_method_verbose_flag)
        case (INPUT_METHOD_VERBOSE_NEVER)
            required = .false.
        case (INPUT_METHOD_VERBOSE_DEFAULT)
            required = .not. (selected_frame%minibuffer_selected .and. simple)
        case (INPUT_METHOD_VERBOSE_COMPLEX_ONLY)
            required = .not. simple
        case (INPUT_METHOD_VERBOSE_ALWAYS)
            required = .true.
        case default
            required = .true.
        end select
    end function quail_require_guidance_buf

    !> Display STRING after the minibuffer's contents until the next input
    !> (a Quail specific version of minibuffer-message, waiting 1000000
    !> seconds instead of two).  Port of quail.el:quail-minibuffer-message
    !> (C-g is an ordinary key while waiting, so it needs no special case).
    subroutine quail_minibuffer_message(string)
        character(len=*), intent(in) :: string
        integer(int64) :: pmax, pt
        logical :: saved_deactivate
        type(status_type) :: st

        call message_nolog("")
        pmax = point_max()
        pt = point()
        saved_deactivate = deactivate_mark_variable
        call goto_char(pmax)
        call insert(string, st)
        call goto_char(pt)
        call sit_for(1000000000_int32)
        call delete_region(pmax, point_max(), st)
        deactivate_mark_variable = saved_deactivate
    end subroutine quail_minibuffer_message

    !> Display a guidance for Quail input method in some window.
    !> The guidance is normally displayed at the echo area,
    !> or in a newly created frame (if the current buffer is a
    !> minibuffer and the selected frame has no other windows).  Port of
    !> quail.el:quail-show-guidance (frames with only a minibuffer are not
    !> ported).
    subroutine quail_show_guidance()
        type(status_type) :: st
        logical :: mode_ok

        ! At first, setup a buffer for completion.
        call quail_setup_completion_buf()
        call bury_buffer(st, quail_completion_buf)
        ! Then, show the guidance.  Don't try to display guidance on an
        ! expired minibuffer.
        mode_ok = .true.

        if (allocated(buffers(cur)%major_mode)) then
            mode_ok = buffers(cur)%major_mode /= "minibuffer_inactive_mode"
        end if

        if (.not. mode_ok .or. .not. quail_require_guidance_buf() .or. input_method_use_echo_area) return
        if (unread_command_events_p() .or. unread_post_input_method_events_p()) return
        if (.not. allocated(quail_guidance_str)) quail_guidance_str = ""

        if (minibufferp()) then
            ! Show the guidance in the next line of the current
            ! minibuffer.
            call quail_minibuffer_message("  ["//buffer_title()//"]"//new_line("a")//quail_guidance_str)
        else
            ! Show the guidance in echo area without logging.
            call message_nolog(quail_guidance_str)
        end if
    end subroutine quail_show_guidance

    !> The current buffer's `current_input_method_title' ("" for nil).
    !> Not a port.
    function buffer_title() result(title)
        character(len=:), allocatable :: title

        title = ""
        if (allocated(buffers(cur)%current_input_method_title)) title = buffers(cur)%current_input_method_title
    end function buffer_title

    !> `quail_show_guidance' as a function of `post_command_hook'.  Not a
    !> port.
    recursive subroutine quail_show_guidance_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call quail_show_guidance()
    end subroutine quail_show_guidance_hook

    !> Hide the Quail guidance.  Port of quail.el:quail-hide-guidance: the
    !> guidance only has a frame of its own on a frame with only a
    !> minibuffer, which is not ported, so there is nothing to hide.
    subroutine quail_hide_guidance()
    end subroutine quail_hide_guidance

    !> Update the Quail guidance string.  Port of
    !> quail.el:quail-update-guidance (the *Quail Completions* window is not
    !> ported).
    subroutine quail_update_guidance()
        integer :: p

        ! Update the guidance string.
        if (.not. quail_require_guidance_buf()) return
        p = current_package()
        if (p == 0) return

        if (quail_package_alist(p)%guidance) then
            ! Show the current possible translations.
            quail_guidance_str = quail_get_translations()
        else
            ! Show the current input keys.
            quail_guidance_str = current_key_string()
        end if
    end subroutine quail_update_guidance

    !> Return a string containing the current possible translations.  Port
    !> of quail.el:quail-get-translations (the current candidate is not
    !> highlighted; GUIDANCE as an alist is not ported).
    function quail_get_translations() result(str)
        character(len=:), allocatable :: str
        integer(int64), allocatable :: chars(:)
        character(len=8) :: num
        integer(int64) :: t
        integer :: map, p, k, j, n, idx, pad
        logical :: deterministic

        if (.not. allocated(quail_current_key)) allocate (quail_current_key(0))
        p = current_package()
        map = quail_lookup_key(quail_current_key, size(quail_current_key), .true.)
        str = codes_to_string(quail_current_key)
        if (p == 0) return
        deterministic = quail_package_alist(p)%deterministic
        if (quail_current_translations /= 0) call quail_update_current_translations()

        ! Show followable keys.
        if (size(quail_current_key) > 0 .and. node_has_children(map)) then
            associate (m => quail_package_alist(p)%map)
                n = 0
                k = m%nodes(map)%child

                do while (k /= 0)
                    n = n + 1
                    k = m%nodes(k)%sibling
                end do

                allocate (chars(n))
                n = 0
                k = m%nodes(map)%child

                do while (k /= 0)
                    n = n + 1
                    chars(n) = m%nodes(k)%ch
                    k = m%nodes(k)%sibling
                end do
            end associate

            do k = 2, n
                t = chars(k)
                j = k - 1

                do while (j >= 1)
                    if (chars(j) <= t) exit
                    chars(j + 1) = chars(j)
                    j = j - 1
                end do

                chars(j + 1) = t
            end do

            str = str//"["//codes_to_string(chars)//"]"
        end if

        ! Show list of translations.
        if (quail_current_translations /= 0 .and. .not. deterministic) then
            associate (nd => quail_package_alist(p)%map%nodes(quail_current_translations), &
                       cands => quail_package_alist(p)%map%cands)
                pad = quail_guidance_translations_starting_column - 7 - int(string_width(str))
                if (pad > 0) str = str//repeat(" ", pad)
                write (num, "(i2.2)") nd%block
                str = str//"("//trim(num)//"/"

                if (nd%blocks >= 0) then
                    write (num, "(i2.2)") nd%blocks
                    str = str//trim(num)//")"
                else
                    str = str//"??)"
                end if

                do idx = nd%start, nd%end - 1
                    if (idx - nd%start == 9) then
                        num = "0"
                    else
                        write (num, "(i0)") idx - nd%start + 1
                    end if

                    str = str//" "//trim(num)//"."//cands(nd%first + idx)%s
                end do
            end associate
        end if
    end function quail_get_translations

    ! ------------------------------------------------------------------
    ! Help
    ! ------------------------------------------------------------------

    !> `quail_help' for the current package, as the buffer's
    !> `describe_current_input_method_function'.  Not a port.
    recursive subroutine quail_help_proc(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine quail_help_proc
end module quail_fm
