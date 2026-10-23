! help_fm.f90 -- help commands: the help map, C-h k, C-h c, C-h w.
! Ported from GNU Emacs 31.1 lisp/help.el.
!
! Copyright (C) 1985-1986, 1993-1994, 1998-2026 Free Software Foundation, Inc.
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

!> The help commands of help.el: the help map on C-h and <f1>,
!> `describe_key' (C-h k), `describe_key_briefly' (C-h c), `where_is'
!> (C-h w) and `help_quit'; `substitute_command_keys', which turns a
!> docstring's \[COMMAND], \<MAP>, \`KEY', \= and quotes into what the
!> reader sees; and `with_help_window', which shows text in *Help*.
!>
!> The help buffer is shown in the selected window (there is one window
!> until windows can be split), and `q' there goes back to the buffer
!> shown before.  `describe_function' and `describe_variable' are in
!> help_fns_fm, which installs the text `describe_key' shows through
!> `describe_function_1_hook' (help-fns.el is loaded after help.el).
!>
!> Deferred: `help_for_help' and the rest of the help map, the
!> `help_key_binding' face, \{MAP} keymap summaries, key translations in
!> `describe_key''s report, mouse and menu events, the help-xref stack,
!> and `function_called_at_point' defaults.
module help_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, symbolp, keymap_ref_p, xhandle, list_elements, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq, symbol_name
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, free_keymap, keymap_set, define_key, key_parse, &
                         key_valid_p, key_description, where_is_internal, lookup_key, current_active_maps
    use keyboard_fm, only: read_key_sequence
    use callint_fm, only: commandp
    use buffer_fm, only: current_buffer, get_buffer_create, set_buffer, set_buffer_read_only, erase_buffer, &
                         restore_buffer_modified_p, buffers, cur
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char
    use xdisp_fm, only: message
    use minibuf_fm, only: completion_obarray_table, minibuffer_local_map
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use help_mode_fm, only: help_mode, help_mode_map, init_help_mode
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, set_window_buffer
    implicit none(type, external)
    private

    public :: substitute_command_keys, register_keymap_name, keymap_name, with_help_window, &
              help_buffer_name, describe_key, describe_key_briefly, where_is, help_quit, syms_of_help

    !> Keymap for characters following the Help key.  Port of help.el
    !> help-map (f, v, k, c, w and q so far).
    type(handle_type), public :: help_map

    abstract interface
        !> The text describing function `name' after "NAME is ".
        subroutine describe_function_1_proc(name, text)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: text
        end subroutine describe_function_1_proc

        !> The raw docstring of function `name' (`found' false if none).
        subroutine documentation_proc(name, doc, found)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: doc
            logical, intent(out) :: found
        end subroutine documentation_proc
    end interface

    !> help-fns.el:describe-function-1, set by help_fns_fm.
    procedure(describe_function_1_proc), pointer, public :: describe_function_1_hook => null()

    !> doc.c:Fdocumentation (RAW), set by help_fns_fm, which holds the
    !> compiled-in docstrings.
    procedure(documentation_proc), pointer, public :: documentation_hook => null()

    !> Name of the help buffer.  help.el (help-buffer).
    character(len=*), parameter :: help_buffer_name = "*Help*"

    !> A keymap that \<NAME> can name in a docstring (a Lisp variable
    !> holding a keymap upstream).
    type :: named_keymap_type
        character(len=:), allocatable :: name
        type(handle_type) :: map
    end type named_keymap_type

    type(named_keymap_type), allocatable :: named_keymaps(:)

    !> Left and right single quotes (text-quoting-style `curve').
    character(len=*), parameter :: LQUOTE = char(226)//char(128)//char(152), &
                                   RQUOTE = char(226)//char(128)//char(153)

contains

    !> Let \<`name'> and "found in `name'" refer to keymap `map'.
    subroutine register_keymap_name(name, map)
        character(len=*), intent(in) :: name
        type(handle_type), intent(in) :: map
        type(named_keymap_type), allocatable :: grown(:)
        integer :: k
        if (.not. allocated(named_keymaps)) allocate (named_keymaps(0))
        do k = 1, size(named_keymaps)
            if (named_keymaps(k)%name == name) then
                named_keymaps(k)%map = map
                return
            end if
        end do
        allocate (grown(size(named_keymaps) + 1))
        grown(1:size(named_keymaps)) = named_keymaps
        grown(size(grown))%name = name
        grown(size(grown))%map = map
        call move_alloc(grown, named_keymaps)
    end subroutine register_keymap_name

    !> The registered name of keymap `map', or "".
    function keymap_name(map) result(name)
        type(handle_type), intent(in) :: map
        character(len=:), allocatable :: name
        integer :: k
        name = ""
        if (.not. allocated(named_keymaps)) return
        do k = 1, size(named_keymaps)
            if (named_keymaps(k)%map%index == map%index .and. &
                named_keymaps(k)%map%generation == map%generation) then
                name = named_keymaps(k)%name
                return
            end if
        end do
    end function keymap_name

    !> The keymap registered as `name'; `found' false if there is none.
    subroutine named_keymap(name, map, found)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: map
        logical, intent(out) :: found
        integer :: k
        found = .false.
        if (.not. allocated(named_keymaps)) return
        do k = 1, size(named_keymaps)
            if (named_keymaps(k)%name == name) then
                map = named_keymaps(k)%map
                found = keymap_live_p(map)
                return
            end if
        end do
    end subroutine named_keymap

    !> The keys that run command `name', as "C-x C-f", searching `map' and
    !> the global map if `have_map', else the active maps; "M-x NAME" if
    !> none.
    function command_keys(name, map, have_map) result(s)
        character(len=*), intent(in) :: name
        type(handle_type), intent(in) :: map
        logical, intent(in) :: have_map
        character(len=:), allocatable :: s
        type(value_type) :: keys
        type(value_type), allocatable :: events(:)
        if (have_map) then
            call where_is_internal(sym(name), keys, firstonly=.true., keymap=map)
        else
            call where_is_internal(sym(name), keys, firstonly=.true.)
        end if
        if (nilp(keys)) then
            s = "M-x "//name
        else
            call list_elements(keys, events)
            s = key_description(events)
        end if
    end function command_keys

    !> Substitute key descriptions for command names in STRING.
    !> Each substring of the form \[COMMAND] is replaced by either a
    !> keystroke sequence that invokes COMMAND, or "M-x COMMAND" if COMMAND
    !> is not on any keys.
    !>
    !> Each substring of the form \`KEYBINDING' will be replaced by
    !> KEYBINDING.
    !>
    !> Each substring of the form \<MAPVAR> specifies the use of MAPVAR
    !> as the keymap for future \[COMMAND] substrings.
    !>
    !> Each grave accent ` is replaced by left quote, and each apostrophe '
    !> is replaced by right quote.  Left and right quote characters are
    !> specified by `text_quoting_style'.
    !>
    !> \= quotes the following character and is discarded; thus, \=\= puts \=
    !> into the output, \=\[ puts \[ into the output, and \=` puts ' into the
    !> output.  Port of help.el:substitute-command-keys (no faces; \{MAPVAR}
    !> summaries are left out; text-quoting-style is `curve').
    function substitute_command_keys(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out
        type(handle_type) :: map
        integer :: i, j, n, clen
        logical :: have_map, found
        character :: c, nx
        out = ""
        have_map = .false.
        n = len(string)
        i = 1
        do while (i <= n)
            c = string(i:i)
            if (c == "\" .and. i < n) then
                nx = string(i + 1:i + 1)
                select case (nx)
                case ("=")
                    if (i + 2 <= n) then
                        clen = utf8_length(string(i + 2:i + 2))
                        out = out//string(i + 2:min(n, i + 1 + clen))
                        i = i + 2 + clen
                    else
                        ! \= at the end of the string stays.
                        out = out//"\="
                        i = i + 2
                    end if
                    cycle
                case ("[")
                    j = index(string(i + 2:), "]")
                    if (j > 0) then
                        out = out//command_keys(string(i + 2:i + j), map, have_map)
                        i = i + 2 + j
                        cycle
                    end if
                case ("<")
                    j = index(string(i + 2:), ">")
                    if (j > 0) then
                        call named_keymap(string(i + 2:i + j), map, found)
                        have_map = found
                        i = i + 2 + j
                        cycle
                    end if
                case ("{")
                    j = index(string(i + 2:), "}")
                    if (j > 0) then
                        i = i + 2 + j
                        cycle
                    end if
                case ("`")
                    j = index(string(i + 2:), "'")
                    if (j > 0) then
                        if (literal_key_p(string(i + 2:i + j))) then
                            out = out//string(i + 2:i + j)
                            i = i + 2 + j
                            cycle
                        end if
                    end if
                case default
                end select
                out = out//c
                i = i + 1
            else if (c == "`") then
                out = out//LQUOTE
                i = i + 1
            else if (c == "'") then
                out = out//RQUOTE
                i = i + 1
            else
                out = out//c
                i = i + 1
            end if
        end do
    end function substitute_command_keys

    !> Whether \`KEY' in a docstring shows KEY: a valid key sequence, a
    !> mouse button or "M-x COMMAND".
    logical function literal_key_p(key)
        character(len=*), intent(in) :: key
        literal_key_p = .false.
        if (len(key) == 0) return
        literal_key_p = key_valid_p(key)
        if (len(key) >= 7) literal_key_p = literal_key_p .or. (key(1:6) == "mouse-" .and. verify(key(7:7), "123456789") == 0)
        if (len(key) > 4) literal_key_p = literal_key_p .or. key(1:4) == "M-x "
    end function literal_key_p

    !> Length of the UTF-8 sequence that starts with byte `b'.
    integer function utf8_length(b)
        character, intent(in) :: b
        integer :: code
        code = iachar(b)
        if (code >= 240) then
            utf8_length = 4
        else if (code >= 224) then
            utf8_length = 3
        else if (code >= 192) then
            utf8_length = 2
        else
            utf8_length = 1
        end if
    end function utf8_length

    !> Show `text' in the help buffer, in Help mode, and say how to get
    !> back.  Port of help.el:with-help-window with help-window-setup (the
    !> help buffer is shown in the selected window, which shows the
    !> current buffer: so it becomes current).
    subroutine with_help_window(text, status)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, previous
        type(window_type), pointer :: sw
        previous = current_buffer()
        call get_buffer_create(help_buffer_name, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        buffers(cur)%undo_enabled = .false.
        call set_buffer_read_only(buf, .false., status)
        call erase_buffer(status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return
        call restore_buffer_modified_p(.false.)
        call goto_char(1_int64)
        call help_mode(status)
        if (.not. status%ok()) return
        ! display-buffer: the selected window (the root window, unless the
        ! minibuffer is active) shows it, recording the buffer it showed.
        if (.not. selected_frame%minibuffer_selected) then
            sw => frame_selected_window_ref()
            call set_window_buffer(sw, buf, status, record=.false.)
            if (.not. status%ok()) return
            ! The window showed the buffer that was current.
            if (previous%index /= buf%index .or. previous%generation /= buf%generation) then
                sw%prev_buffer = previous
            end if
        end if
        call message(substitute_command_keys("Type \<help_mode_map>\[quit_window] to restore previous buffer"))
    end subroutine with_help_window

    !> " (found in MAP)" for the active map in which `keys' is bound to
    !> `binding', if that map has a name; else "".
    function found_in(keys, binding) result(s)
        type(value_type), intent(in) :: keys(:), binding
        character(len=:), allocatable :: s
        type(value_type) :: active, val
        type(value_type), allocatable :: maps(:)
        type(status_type) :: status
        integer :: k
        s = ""
        active = current_active_maps()
        if (keymap_ref_p(active)) then
            allocate (maps(1))
            maps(1) = active
        else
            call list_elements(active, maps)
        end if
        do k = 1, size(maps)
            if (.not. keymap_ref_p(maps(k))) cycle
            call lookup_key(maps(k), keys, val, status)
            if (.not. status%ok()) cycle
            if (.not. value_eq(val, binding)) cycle
            s = keymap_name(xhandle(maps(k)))
            if (len(s) > 0) s = " (found in "//s//")"
            return
        end do
    end function found_in

    !> Read a key sequence for a help command; `ok' false at end of input.
    subroutine read_help_key(prompt, keys, binding, ok, status)
        character(len=*), intent(in) :: prompt
        type(value_type), allocatable, intent(out) :: keys(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status
        logical :: eof
        call message(prompt)
        call read_key_sequence(keys, binding, eof, status)
        call message("")
        ok = status%ok() .and. .not. eof
    end subroutine read_help_key

    !> Display documentation of the function invoked by KEY-LIST.
    !> Interactively, this command can't describe prefix commands, but
    !> will always wait for the user to type the complete key sequence.
    !> Port of help.el:describe-key (one key sequence, no translations).
    subroutine describe_key(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        character(len=:), allocatable :: desc, name, text
        logical :: ok
        call read_help_key("Describe the following key, mouse click, or menu item: ", keys, binding, ok, status)
        if (.not. ok) return
        desc = key_description(keys)
        if (.not. symbolp(binding) .or. nilp(binding)) then
            if (nilp(binding)) then
                call message(desc//" is undefined")
            else
                call message(desc//" runs a keyboard macro")
            end if
            return
        end if
        call symbol_name(binding, name, status)
        if (.not. status%ok()) return
        text = ""
        if (associated(describe_function_1_hook)) call describe_function_1_hook(name, text)
        call with_help_window(desc//" runs the command "//name//found_in(keys, binding)//", which is "//text, &
                              status)
    end subroutine describe_key

    !> Print the name of the function KEY-LIST invokes.
    !> Port of help.el:describe-key-briefly (no INSERT).
    subroutine describe_key_briefly(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        character(len=:), allocatable :: desc, name
        logical :: ok
        call read_help_key("Describe the following key, mouse click, or menu item briefly: ", keys, binding, &
                           ok, status)
        if (.not. ok) return
        desc = key_description(keys)
        if (nilp(binding)) then
            call message(desc//" is undefined")
        else if (symbolp(binding)) then
            call symbol_name(binding, name, status)
            if (.not. status%ok()) return
            call message(desc//" runs the command "//name)
        else
            call message(desc//" runs a keyboard macro")
        end if
    end subroutine describe_key_briefly

    !> Print message listing key sequences that invoke the command DEFINITION.
    !> Port of help.el:where-is (no INSERT, aliases or remapping).
    recursive subroutine where_is(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, keys_text
        type(value_type) :: all
        type(value_type), allocatable :: seqs(:), events(:)
        integer :: k
        call completing_read("Where is command: ", completion_obarray_table(), name, status, commandp, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok() .or. len(name) == 0) return
        call where_is_internal(sym(name), all)
        if (nilp(all)) then
            call message(name//" is not on any key")
            return
        end if
        call list_elements(all, seqs)
        keys_text = ""
        do k = 1, size(seqs)
            call list_elements(seqs(k), events)
            if (k > 1) keys_text = keys_text//", "
            keys_text = keys_text//key_description(events)
        end do
        call message(name//" is on "//keys_text)
    end subroutine where_is

    !> Just exit from the Help command's command loop.
    !> Port of help.el:help-quit.
    subroutine help_quit(status)
        type(status_type), intent(out) :: status
        status = status_type()
    end subroutine help_quit

    !> Make `help_map' and put it on C-h and <f1> in `global_map'; name
    !> the keymaps docstrings refer to; register the commands.
    subroutine syms_of_help(global_map)
        type(handle_type), intent(in) :: global_map
        type(status_type) :: status
        type(value_type), allocatable :: key(:)
        if (keymap_live_p(help_map)) call free_keymap(help_map, status)
        call make_sparse_keymap(help_map)
        call keymap_set(help_map, "c", "describe_key_briefly", status)
        call keymap_set(help_map, "f", "describe_function", status)
        call keymap_set(help_map, "k", "describe_key", status)
        call keymap_set(help_map, "q", "help_quit", status)
        call keymap_set(help_map, "v", "describe_variable", status)
        call keymap_set(help_map, "w", "where_is", status)
        call key_parse("C-h", key, status)
        call define_key(global_map, key, make_keymap_ref(help_map), status)
        call key_parse("<f1>", key, status)
        call define_key(global_map, key, make_keymap_ref(help_map), status)
        call init_help_mode()
        call register_keymap_name("global_map", global_map)
        call register_keymap_name("help_map", help_map)
        call register_keymap_name("help_mode_map", help_mode_map)
        call register_keymap_name("minibuffer_local_map", minibuffer_local_map)
        commands%describe_key%run => cmd_describe_key
        commands%describe_key_briefly%run => cmd_describe_key_briefly
        commands%where_is%run => cmd_where_is
        commands%help_quit%run => help_quit
    end subroutine syms_of_help

    subroutine cmd_describe_key(status)
        type(status_type), intent(out) :: status
        call describe_key(status)
    end subroutine cmd_describe_key

    subroutine cmd_describe_key_briefly(status)
        type(status_type), intent(out) :: status
        call describe_key_briefly(status)
    end subroutine cmd_describe_key_briefly

    recursive subroutine cmd_where_is(status)
        type(status_type), intent(out) :: status
        call where_is(status)
    end subroutine cmd_where_is
end module help_fm
