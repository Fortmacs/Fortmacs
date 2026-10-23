! bindings_fm.f90 -- the default global key bindings.
! Ported from GNU Emacs 31.1 lisp/bindings.el, with the global keymap
! tree from lisp/subr.el and the self-inserting range from
! lisp/international/mule-conf.el.
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

!> `init_bindings` registers the ported commands and builds global-map,
!> esc-map, ctl-x-map and function-key-map with the bindings upstream
!> gives them, as far as their commands exist.  Bindings to commands not
!> ported yet are left out rather than failing with void-function.
!> Until `move-beginning-of-line` and `move-end-of-line` are ported,
!> <home> and <end> use `beginning-of-line` and `end-of-line`.
module bindings_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, make_keymap_ref, list
    use lread_fm, only: sym
    use character_fm, only: MAX_CHAR, CHAR_CTL, CHAR_META
    use keymap_fm, only: make_keymap, make_sparse_keymap, define_key, &
                         define_key_range, use_global_map, set_keymap_parent, &
                         key_parse, keymap_live_p, free_keymap
    use callint_fm, only: defcommand, current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: local_function_key_map
    use cmds_fm, only: syms_of_cmds, forward_char, backward_char
    use simple_fm, only: syms_of_simple, backward_word
    use syntax_fm, only: syms_of_syntax, forward_word
    use paragraphs_fm, only: syms_of_paragraphs
    use search_fm, only: syms_of_search
    use isearch_fm, only: syms_of_isearch
    use window_fm, only: syms_of_window
    use files_fm, only: syms_of_files
    use help_fm, only: syms_of_help
    use help_fns_fm, only: syms_of_help_fns
    use minibuf_fm, only: init_minibuf, minibuffer_local_map
    use minibuffer_fm, only: syms_of_minibuffer
    implicit none(type, external)
    private

    public :: init_bindings

    integer(int64), parameter :: M_CTL = int(CHAR_CTL, int64), &
                                 M_META = int(CHAR_META, int64)

    !> subr.el:global-map, esc-map, ctl-x-map; keymap.c:Vfunction_key_map.
    type(handle_type), public :: global_map, esc_map, ctl_x_map, &
                                 function_key_map

contains

    !> Move point N characters to the right (to the left if N is negative).
    !> On reaching beginning or end of buffer, stop and signal error.
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward-char].  Port of bindings.el:right-char (left-to-right
    !> paragraphs only).
    subroutine cmd_right_char(status)
        type(status_type), intent(out) :: status
        call forward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_char

    !> Move point N characters to the left (to the right if N is negative).
    !> On reaching beginning or end of buffer, stop and signal error.
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward-char].  Port of bindings.el:left-char (left-to-right
    !> paragraphs only).
    subroutine cmd_left_char(status)
        type(status_type), intent(out) :: status
        call backward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_char

    !> Move point N words to the right (to the left if N is negative).
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward-word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char-script-table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit-field-text-motion' is
    !> non-nil.  Port of bindings.el:right-word (left-to-right paragraphs
    !> only).
    subroutine cmd_right_word(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call forward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_word

    !> Move point N words to the left (to the right if N is negative).
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward-word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char-script-table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit-field-text-motion' is
    !> non-nil.  Port of bindings.el:left-word (left-to-right paragraphs
    !> only).
    subroutine cmd_left_word(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call backward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_word

    !> Bind `keys` (kbd syntax) to command `cmd` in `map`.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status
        call key_parse(keys, key, status)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind

    !> Bind the single event `c` to command `cmd` in `map`.
    subroutine bind_char(map, c, cmd)
        type(handle_type), intent(in) :: map
        integer(int64), intent(in) :: c
        character(len=*), intent(in) :: cmd
        type(value_type) :: key(1)
        type(status_type) :: status
        key(1) = make_fixnum(c)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind_char

    !> In `map`, translate `from` (kbd syntax) to the event `to`.
    subroutine translate(map, from, to)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: from
        type(value_type), intent(in) :: to
        type(value_type), allocatable :: key(:)
        type(status_type) :: status
        call key_parse(from, key, status)
        call define_key(map, key, list(to), status)
    end subroutine translate

    !> Register the commands and install the default keymaps.  Safe to
    !> call again (the maps are rebuilt).
    subroutine init_bindings()
        type(status_type) :: status
        type(value_type) :: key(1)
        integer(int64) :: c
        ! Rebuilding: drop the old maps.
        if (keymap_live_p(global_map)) call free_keymap(global_map, status)
        if (keymap_live_p(esc_map)) call free_keymap(esc_map, status)
        if (keymap_live_p(ctl_x_map)) call free_keymap(ctl_x_map, status)
        if (keymap_live_p(function_key_map)) call free_keymap(function_key_map, status)
        call syms_of_cmds()
        call syms_of_simple()
        call syms_of_window()
        call defcommand("right-char", cmd_right_char)
        call defcommand("left-char", cmd_left_char)
        call defcommand("right-word", cmd_right_word)
        call defcommand("left-word", cmd_left_word)
        call syms_of_syntax()
        call syms_of_paragraphs()
        call syms_of_search()
        ! subr.el: esc-map, ctl-x-map, global-map.
        call make_keymap(esc_map)
        call make_keymap(ctl_x_map)
        call make_keymap(global_map)
        key(1) = make_fixnum(27_int64)
        call define_key(global_map, key, make_keymap_ref(esc_map), status)
        key(1) = make_fixnum(24_int64)
        call define_key(global_map, key, make_keymap_ref(ctl_x_map), status)
        call bind_char(global_map, 9_int64, "self-insert-command")
        do c = 32, 126
            call bind_char(global_map, c, "self-insert-command")
        end do
        ! mule-conf.el: all non-ASCII characters self-insert (this covers
        ! subr.el's 0240..0377 loop too).
        call define_key_range(global_map, 128_int64, int(MAX_CHAR, int64), &
                              sym("self-insert-command"), status)
        call bind(global_map, "C-a", "beginning-of-line")
        call bind(global_map, "C-b", "backward-char")
        call bind(global_map, "C-e", "end-of-line")
        call bind(global_map, "C-f", "forward-char")
        call bind(global_map, "C-n", "next-line")
        call bind(global_map, "C-p", "previous-line")
        ! bindings.el
        call bind(global_map, "C-g", "keyboard-quit")
        call bind(global_map, "C-m", "newline")
        call bind(global_map, "C-o", "open-line")
        call bind(global_map, "C-q", "quoted-insert")
        call bind(global_map, "C-u", "universal-argument")
        do c = 0, 9
            call bind_char(esc_map, iachar("0") + c, "digit-argument")
            call bind_char(global_map, M_CTL + iachar("0") + c, "digit-argument")
            call bind_char(global_map, M_CTL + M_META + iachar("0") + c, "digit-argument")
        end do
        call bind(esc_map, "-", "negative-argument")
        call bind_char(global_map, M_CTL + iachar("-"), "negative-argument")
        call bind_char(global_map, M_CTL + M_META + iachar("-"), "negative-argument")
        call bind(global_map, "DEL", "delete-backward-char")
        call bind(global_map, "C-d", "delete-char")
        call bind(global_map, "<home>", "beginning-of-line")
        call bind(global_map, "<end>", "end-of-line")
        call bind(global_map, "<left>", "left-char")
        call bind(global_map, "<right>", "right-char")
        call bind(global_map, "<down>", "next-line")
        call bind(global_map, "<up>", "previous-line")
        call bind(global_map, "<deletechar>", "delete-forward-char")
        call bind(esc_map, "x", "execute-extended-command")
        call bind(global_map, "<next>", "scroll-up-command")
        call bind(global_map, "<prior>", "scroll-down-command")
        call bind(global_map, "C-v", "scroll-up-command")
        call bind(esc_map, "v", "scroll-down-command")
        ! window.el
        call bind(global_map, "C-l", "recenter-top-bottom")
        call bind(esc_map, "r", "move-to-window-line-top-bottom")
        ! Undo (bindings.el).
        call bind(global_map, "C-_", "undo")
        call bind(global_map, "C-/", "undo")
        call bind(ctl_x_map, "u", "undo")
        call bind(global_map, "C-?", "undo-redo")
        call bind(global_map, "C-M-_", "undo-redo")
        ! The mark and killing (bindings.el).
        call bind(global_map, "C-@", "set-mark-command")
        call bind(global_map, "C-SPC", "set-mark-command")
        call bind(ctl_x_map, "C-x", "exchange-point-and-mark")
        call bind(ctl_x_map, "h", "mark-whole-buffer")
        ! Windows (window.el) and buffers (subr.el ctl-x-map).
        call bind(ctl_x_map, "0", "delete-window")
        call bind(ctl_x_map, "1", "delete-other-windows")
        call bind(ctl_x_map, "2", "split-window-below")
        call bind(ctl_x_map, "3", "split-window-right")
        call bind(ctl_x_map, "o", "other-window")
        call bind(ctl_x_map, "b", "switch-to-buffer")
        call bind(ctl_x_map, "k", "kill-buffer")
        call bind(global_map, "C-w", "kill-region")
        call bind(esc_map, "w", "kill-ring-save")
        call bind(global_map, "C-k", "kill-line")
        call bind(global_map, "C-y", "yank")
        call bind(esc_map, "y", "yank-pop")
        call bind(global_map, "<C-S-backspace>", "kill-whole-line")
        ! Words and paragraphs (bindings.el).
        call bind(esc_map, "@", "mark-word")
        call bind(esc_map, "f", "forward-word")
        call bind(esc_map, "b", "backward-word")
        call bind(esc_map, "d", "kill-word")
        call bind(esc_map, "DEL", "backward-kill-word")
        call bind(global_map, "M-<right>", "right-word")
        call bind(esc_map, "<right>", "forward-word")
        call bind(global_map, "M-<left>", "left-word")
        call bind(esc_map, "<left>", "backward-word")
        call bind(global_map, "C-<right>", "right-word")
        call bind(global_map, "C-<left>", "left-word")
        call bind(global_map, "C-<delete>", "kill-word")
        call bind(global_map, "C-<backspace>", "backward-kill-word")
        call bind(global_map, "C-<up>", "backward-paragraph")
        call bind(global_map, "C-<down>", "forward-paragraph")
        call bind(esc_map, "{", "backward-paragraph")
        call bind(esc_map, "}", "forward-paragraph")
        call bind(esc_map, "h", "mark-paragraph")
        call bind(esc_map, "a", "backward-sentence")
        call bind(esc_map, "e", "forward-sentence")
        call bind(esc_map, "k", "kill-sentence")
        call bind(ctl_x_map, "DEL", "backward-kill-sentence")
        call use_global_map(global_map, status)
        call syms_of_files(ctl_x_map)
        call syms_of_isearch(global_map)
        ! The minibuffer: minibuffer-local-map's keys from minibuffer.el,
        ! then its history keys from bindings.el (<up> and <down> are
        ! previous/next-line-or-history-element upstream, the same in a
        ! one-line minibuffer).
        if (keymap_live_p(minibuffer_local_map)) call free_keymap(minibuffer_local_map, status)
        call init_minibuf()
        call syms_of_minibuffer()
        call bind(minibuffer_local_map, "M-n", "next-history-element")
        call bind(minibuffer_local_map, "<next>", "next-history-element")
        call bind(minibuffer_local_map, "<down>", "next-history-element")
        call bind(minibuffer_local_map, "M-p", "previous-history-element")
        call bind(minibuffer_local_map, "<prior>", "previous-history-element")
        call bind(minibuffer_local_map, "<up>", "previous-history-element")
        ! Help (help.el, help-fns.el), after the maps it names exist.
        call syms_of_help(global_map)
        call syms_of_help_fns()
        ! bindings.el: function-key-map, the parent of every terminal's
        ! local-function-key-map.
        call make_sparse_keymap(function_key_map)
        call translate(function_key_map, "<backspace>", make_fixnum(127_int64))
        call translate(function_key_map, "<delete>", make_fixnum(127_int64))
        call translate(function_key_map, "<kp-delete>", make_fixnum(127_int64))
        if (.not. keymap_live_p(local_function_key_map)) then
            call make_sparse_keymap(local_function_key_map)
        end if
        call set_keymap_parent(local_function_key_map, function_key_map, status)
    end subroutine init_bindings
end module bindings_fm
