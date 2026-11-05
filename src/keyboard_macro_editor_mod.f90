! keyboard_macro_editor_mod.f90 -- keyboard macros as text.
! Ported from GNU Emacs 31.1 lisp/edmacro.el.
!
! Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.
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

!> The printed form of keyboard macros (`format_keyboard_macro'), which
!> keyboard_macros_mod shows in the echo area.
!>
!> Deferred: the macro editor (`edit_keyboard_macro', `edmacro_mode', C-x C-k
!> e and RET, `insert_keyboard_macro'), `read_keyboard_macro' and the parsing of
!> macro text (`edmacro_parse_keys'), and menu-bar and mouse events in
!> macros (`edmacro_fix_menu_commands': macros here hold characters and
!> function keys only).
module keyboard_macro_editor_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use utility_functions_mod, only: count_characters
    use error_mod, only: status_type
    use value_mod, only: value_type, xfixnum, xsymbol, xstring, symbols, vtag, V_ARRAY, list_elements, make_keymap_reference, &
                         make_symbol_reference, V_NULL, V_INTEGER, V_NAME, V_STRING
    use names_mod, only: intern_soft
    use data_mod, only: value_eq
    use character_mod, only: CHARACTER_ALT, CHARACTER_CONTROL, CHARACTER_HYPER, CHARACTER_META, CHARACTER_SUPER, CHARACTER_SHIFT, &
                             string_character_and_length, character_string, MAXIMUM_MULTIBYTE_LENGTH
    use keymap_mod, only: current_active_maps, lookup_key, key_binding, is_keymap, is_keymap_live
    use execute_interactively_mod, only: is_command
    use keyboard_mod, only: local_function_key_map
    implicit none(type, external)
    private

    public :: format_keyboard_macro

    !> Values of `format_keyboard_macro''s VERBOSE: EDMACRO_COMPACT (a compact 80-column
    !> format), EDMACRO_ONE_LINE (everything on one line) and EDMACRO_VERBOSE (one command per line,
    !> with comments).
    integer, parameter, public :: EDMACRO_COMPACT = 0, EDMACRO_ONE_LINE = 1, EDMACRO_VERBOSE = 2

    !> True if `edit_keyboard_macro' should leave 8-bit characters intact.
    !> Default false means to write characters above \177 in octal
    !> notation.  Port of edmacro.el edmacro-eight-bits.
    logical, public :: edmacro_eight_bits = .false.

contains

    !> Return the keyboard macro MACRO as a human-readable string.
    !> This string is suitable for passing to `read_keyboard_macro'.
    !> Second argument VERBOSE means to put one command per line with
    !> comments (EDMACRO_VERBOSE).  If VERBOSE is EDMACRO_ONE_LINE, put
    !> everything on one line.  If VERBOSE is omitted or EDMACRO_COMPACT,
    !> use a compact 80-column format.  Port of edmacro.el:format-kbd-macro
    !> with edmacro-format-keys (MACRO is an array of events).
    recursive function format_keyboard_macro(macro, verbose) result(res)
        type(value_type), intent(in) :: macro(:)
        integer, intent(in), optional :: verbose
        character(len=:), allocatable :: res
        type(value_type), allocatable :: rest(:), maps(:), key(:), tkey(:), fkey(:), one(:)
        type(value_type) :: b, bind, fk, first
        type(status_type) :: st
        character(len=:), allocatable :: prefix, description, bind_name, text_characters
        character(len=24) :: count_text
        integer(int64) :: n, position, i, k, bind_length, text, tlen, times, p, j, maxkey, length_rest, ch, line_length
        logical :: one_line, verb, has_prefix, found, use_fkey, done, uarg, same

        verb = .false.
        one_line = .false.

        if (present(verbose)) then
            verb = verbose == EDMACRO_VERBOSE
            one_line = verbose == EDMACRO_ONE_LINE
        end if

        maxkey = 127
        if (edmacro_eight_bits) maxkey = 255
        n = size(macro, kind=int64)
        allocate (rest(n + 1))
        if (n > 0) rest(1:n) = macro
        rest(n + 1) = value_type(name="end-macro")
        call active_map_list(maps)
        res = ""
        line_length = 0
        position = 1

        do while (position <= n)
            length_rest = n + 2 - position
            ! A prefix argument: M-DIGITS, C-u C-u ... or C-u DIGITS.
            has_prefix = .false.
            prefix = ""

            if (in_mdigs(rest(position), .false.)) then
                one = [rest(position)]
                call key_binding(one, b, st)

                if (value_eq(b, value_type(name="digit_argument")) .or. value_eq(b, value_type(name="negative_argument"))) then
                    i = 1

                    do while (in_mdigs(rest(position + i), .true.))
                        i = i + 1
                    end do

                    if (.not. in_pkeys(rest(position + i))) then
                        prefix = "M-"

                        do k = position, position + i - 1
                            prefix = prefix//achar(iand(xfixnum(rest(k)), 127_int64))
                        end do

                        prefix = prefix//" "
                        position = position + i
                        has_prefix = .true.
                    end if
                end if
            end if

            if (.not. has_prefix .and. integer_event(rest(position)) == 21) then
                one = [rest(position)]
                call key_binding(one, b, st)
                uarg = value_eq(b, value_type(name="universal_argument"))

                if (uarg) then
                    i = 1

                    do while (integer_event(rest(position + i)) == 21)
                        i = i + 1
                    end do

                    if (.not. in_pkeys(rest(position + i))) then
                        prefix = repeat("C-u ", int(i))
                        position = position + i
                        has_prefix = .true.
                    end if
                end if

                if (uarg .and. .not. has_prefix) then
                    i = 1
                    if (integer_event(rest(position + i)) == iachar("-")) i = i + 1

                    do while (integer_event(rest(position + i)) >= iachar("0") .and. integer_event(rest(position + i)) <= iachar( &
                              "9"))
                        i = i + 1
                    end do

                    if (.not. in_pkeys(rest(position + i))) then
                        prefix = "C-u "

                        do k = position + 1, position + i - 1
                            prefix = prefix//achar(xfixnum(rest(k)))
                        end do

                        prefix = prefix//" "
                        position = position + i
                        has_prefix = .true.
                    end if
                end if
            end if

            length_rest = n + 2 - position
            ! The longest bound key sequence at the start.
            bind_length = 1

            do k = 1, size(maps)
                call lookup_key(maps(k), rest(position:n + 1), b, st)
                if (vtag(b) == V_INTEGER) bind_length = max(bind_length, xfixnum(b))
            end do

            key = rest(position:position + bind_length - 1)
            bind = value_type()
            found = .false.
            use_fkey = .false.

            do k = 1, size(maps)
                call lookup_key(maps(k), key, b, st)

                if (allocated(b%a) .and. vtag(b) /= V_INTEGER) then
                    bind = b
                    found = .true.
                    exit
                end if
            end do

            if (.not. found .and. is_keymap_live(local_function_key_map)) then
                call lookup_key(make_keymap_reference(local_function_key_map), rest(position:n + 1), fk, st)

                if (vtag(fk) == V_INTEGER) then
                    tlen = xfixnum(fk)
                    tkey = rest(position:position + tlen - 1)
                    call lookup_key(make_keymap_reference(local_function_key_map), tkey, fk, st)

                    if (allocated(fk%a) .and. vtag(fk) /= V_INTEGER .and. .not. is_keymap(fk)) then
                        call translation_events(fk, fkey)

                        do k = 1, size(maps)
                            call lookup_key(maps(k), fkey, b, st)

                            if (allocated(b%a) .and. vtag(b) /= V_INTEGER) then
                                bind_length = tlen
                                key = tkey
                                bind = b
                                use_fkey = .true.
                                exit
                            end if
                        end do
                    end if
                end if
            end if

            first = key(1)
            ! The self-inserting characters that follow (not three alike).
            i = bind_length

            do while (i < length_rest)
                ch = integer_event(rest(position + i))
                if (ch <= 32 .or. ch >= maxkey .or. ch == 92) exit
                if (.not. self_inserting(ch)) exit

                if (i <= length_rest - 2) then
                    if (integer_event(rest(position + i + 1)) == ch) then
                        if (integer_event(rest(position + i + 2)) == ch) exit
                    end if
                end if

                i = i + 1
            end do

            text = i
            bind_name = ""
            if ((vtag(bind) == V_NAME .or. vtag(bind) == V_NULL) .and. allocated(bind%a)) bind_name = symbols(xsymbol(bind))%name
            if (vtag(bind) == V_STRING) bind = value_type()
            done = .false.
            ch = integer_event(first)

            if (value_eq(bind, value_type(name="self_insert_command")) .and. .not. has_prefix .and. text > 1 .and. &
                ch > 32 .and. ch <= maxkey .and. ch /= 92) then
                if (text > 30) text = 30
                description = event_characters(rest(position:position + text - 1))

                if (len(description) >= 3) then
                    if (index("ACHMsS", &
                              description(1:1)) > 0 .and. description(2:2) == "-" .and. description(3:3) /= achar(10)) then
                        text = 2
                        description = description(1:2)
                    end if
                end if

                if (.not. reserved_description(description)) then
                    if (len(description) == 2) then
                        if (description(1:1) == "^") description = description(1:1)//" "//description(2:2)
                    end if

                    if (res_word(description)) then
                        text_characters = description
                        description = ""

                        do k = 1, len(text_characters)
                            if (k > 1) description = description//" "
                            description = description//text_characters(k:k)
                        end do
                    end if

                    if (verb) then
                        write (count_text, '(" * ", i0)') text
                        bind_name = bind_name//trim(count_text)
                    end if

                    bind_length = text
                    done = .true.
                end if
            end if

            if (.not. done .and. value_eq(bind, value_type(name="execute_extended_command")) .and. text > bind_length) then
                if (integer_event(rest(position + text)) == 13 .or. value_eq(rest(position + text), value_type(name="return"))) then
                    description = event_characters(rest(position + bind_length:position + text - 1))
                    k = intern_soft(description)

                    if (k > 0) then
                        if (is_command(make_symbol_reference(k))) then
                            bind_name = description
                            description = "<<"//description//">>"
                            bind_length = text + 1
                            done = .true.
                        end if
                    end if
                end if
            end if

            if (.not. done) then
                description = ""

                if (use_fkey) then
                    do k = 1, size(fkey)
                        if (k > 1) description = description//" "
                        description = description//event_description(fkey(k), maxkey)
                    end do
                else
                    do k = 1, size(key)
                        if (k > 1) description = description//" "
                        description = description//event_description(key(k), maxkey)
                    end do
                end if
            end if

            if (has_prefix) description = prefix//description

            ! Repetitions of a key without spaces: 3*C-f.
            if (index(description, " ") == 0) then
                times = 1
                p = bind_length

                do
                    same = .true.

                    do j = 0, bind_length - 1
                        if (position + p + j > n + 1) then
                            same = .false.
                            exit
                        end if

                        if (.not. value_eq(rest(position + j), rest(position + p + j))) then
                            same = .false.
                            exit
                        end if
                    end do

                    if (.not. same) exit
                    times = times + 1
                    p = p + bind_length
                end do

                if (times > 1) then
                    write (count_text, '(i0, "*")') times
                    description = trim(count_text)//description
                    bind_length = bind_length*times
                end if
            end if

            position = position + bind_length

            if (verb) then
                if (len(res) > 0) res = res//achar(10)
                res = res//description

                if (len(bind_name) > 0) then
                    res = res//repeat(achar(9), int(max(3 - count_characters(description)/8, 1_int64)))//";; "//bind_name
                end if

                line_length = 0
            else
                if (line_length + count_characters(description) + 2 > 72 .and. .not. one_line) then
                    res = res//achar(10)//" "
                    line_length = 1
                else if (len(res) > 0) then
                    res = res//" "
                    line_length = line_length + 1
                end if

                res = res//description
                line_length = line_length + count_characters(description)
            end if
        end do
    end function format_keyboard_macro

    !> The active keymaps (current-active-maps) as an array.  Not a port.
    subroutine active_map_list(maps)
        type(value_type), allocatable, intent(out) :: maps(:)
        type(value_type) :: all

        all = current_active_maps()

        if (vtag(all) == V_ARRAY) then
            call list_elements(all, maps)
        else
            allocate (maps(1))
            maps(1) = all
        end if
    end subroutine active_map_list

    !> The events of a key translation (a list of events or a string).
    !> Not a port.
    subroutine translation_events(v, evs)
        type(value_type), intent(in) :: v
        type(value_type), allocatable, intent(out) :: evs(:)
        character(len=:), allocatable :: s
        integer(int64) :: position, n
        integer(int32) :: c, length

        if (vtag(v) == V_ARRAY) then
            call list_elements(v, evs)
            return
        end if

        if (vtag(v) /= V_STRING) then
            allocate (evs(1))
            evs(1) = v
            return
        end if

        s = xstring(v)
        allocate (evs(len(s)))
        n = 0
        position = 1

        do while (position <= len(s, kind=int64))
            call string_character_and_length(s, position, c, length)
            n = n + 1
            evs(n) = value_type(int(c, int64))
            position = position + length
        end do

        evs = evs(1:n)
    end subroutine translation_events

    !> The character code of event EV, or -1 if it is not a character.
    !> Not a port.
    integer(int64) function integer_event(ev)
        type(value_type), intent(in) :: ev

        integer_event = -1
        if (vtag(ev) == V_INTEGER) integer_event = xfixnum(ev)
    end function integer_event

    !> Whether EV is one of M-- M-0 ... M-9 (only M-0 ... M-9 if DIGITS_ONLY).
    !> Not a port.
    logical function in_mdigs(ev, digits_only)
        type(value_type), intent(in) :: ev
        logical, intent(in) :: digits_only
        integer(int64) :: c

        c = integer_event(ev)
        in_mdigs = .false.
        if (c < 0) return
        if (iand(c, int(CHARACTER_META, int64)) == 0) return
        c = c - int(CHARACTER_META, int64)
        in_mdigs = c >= iachar("0") .and. c <= iachar("9")
        if (.not. digits_only .and. c == iachar("-")) in_mdigs = .true.
    end function in_mdigs

    !> Whether EV is one of the prefix-argument keys (pkeys): the end of
    !> the macro, 0 ... 9, -, C-u, M--, M-0 ... M-9.  Not a port.
    logical function in_pkeys(ev)
        type(value_type), intent(in) :: ev
        integer(int64) :: c

        in_pkeys = value_eq(ev, value_type(name="end-macro"))
        if (in_pkeys) return
        c = integer_event(ev)
        if (c < 0) return
        in_pkeys = (c >= iachar("0") .and. c <= iachar("9")) .or. c == iachar("-") .or. c == 21 .or. &
                   in_mdigs(ev, .false.)
    end function in_pkeys

    !> Whether character CH, as a key, runs `self_insert_command'.
    !> Not a port.
    recursive logical function self_inserting(ch)
        integer(int64), intent(in) :: ch
        type(value_type) :: key(1), b
        type(status_type) :: st

        key(1) = value_type(ch)
        call key_binding(key, b, st)
        self_inserting = value_eq(b, value_type(name="self_insert_command"))
    end function self_inserting

    !> The characters EVS (all ASCII) as a string.  Not a port.
    function event_characters(evs) result(s)
        type(value_type), intent(in) :: evs(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(evs)
            s = s//achar(integer_event(evs(k)))
        end do
    end function event_characters

    !> Whether DESCRIPTION matches "^;;\|^<.*>$\|^\\[0-9]+$\|^[0-9]+\*.": text
    !> that would read back as something else.
    !> Port of part of edmacro.el:edmacro-format-keys.
    logical function reserved_description(description)
        character(len=*), intent(in) :: description
        integer :: k

        reserved_description = .false.
        if (index(description, ";;") == 1) reserved_description = .true.

        if (len(description) >= 2) then
            if (description(1:1) == "<" .and. description(len(description):len(description)) == ">") reserved_description = .true.
        end if

        if (len(description) >= 2) then
            if (description(1:1) == "\" .and. verify(description(2:), "0123456789") == 0) reserved_description = .true.
        end if

        k = verify(description, "0123456789")

        if (k > 1 .and. k < len(description)) then
            if (description(k:k) == "*" .and. description(k + 1:k + 1) /= achar(10)) reserved_description = .true.
        end if
    end function reserved_description

    !> Whether DESCRIPTION is one of the reserved words NUL TAB LFD RET ESC SPC DEL
    !> REM.  Port of part of edmacro.el:edmacro-format-keys.
    logical function res_word(description)
        character(len=*), intent(in) :: description

        select case (description)
        case ("NUL", "TAB", "LFD", "RET", "ESC", "SPC", "DEL", "REM")
            res_word = .true.
        case default
            res_word = .false.
        end select
    end function res_word

    !> The description of event EV in a macro: modifiers ACHMsS, then NUL,
    !> TAB, LFD, RET, ESC, SPC, C-x, DEL, the character, or \OCTAL; <NAME>
    !> for a function key.  Port of part of edmacro.el:edmacro-format-keys.
    function event_description(ev, maxkey) result(s)
        type(value_type), intent(in) :: ev
        integer(int64), intent(in) :: maxkey
        character(len=:), allocatable :: s
        character(len=*), parameter :: pfs = "ACHMsS"
        integer(int64) :: bits(6), ch, ch2
        character(len=24) :: buffer
        character(len=MAXIMUM_MULTIBYTE_LENGTH) :: mb
        integer(int32) :: nb
        type(status_type) :: st
        integer :: k

        if ((vtag(ev) == V_NAME .or. vtag(ev) == V_NULL)) then
            s = "<"//symbols(xsymbol(ev))%name//">"
            return
        end if

        bits = [int(CHARACTER_ALT, int64), int(CHARACTER_CONTROL, int64), int(CHARACTER_HYPER, int64), int(CHARACTER_META, int64), &
                int(CHARACTER_SUPER, int64), int(CHARACTER_SHIFT, int64)]
        ch = integer_event(ev)
        s = ""

        do k = 1, 6
            if (iand(ch, bits(k)) /= 0) s = s//pfs(k:k)//"-"
        end do

        ch2 = iand(ch, 2_int64**18 - 1)

        if (ch2 <= 32) then
            select case (ch2)
            case (0)
                s = s//"NUL"
            case (9)
                s = s//"TAB"
            case (10)
                s = s//"LFD"
            case (13)
                s = s//"RET"
            case (27)
                s = s//"ESC"
            case (32)
                s = s//"SPC"
            case default
                if (ch2 <= 26) then
                    s = s//"C-"//achar(96 + ch2)
                else
                    s = s//"C-"//achar(64 + ch2)
                end if
            end select
        else if (ch2 == 127) then
            s = s//"DEL"
        else if (ch2 <= maxkey) then
            call character_string(int(ch2, int32), mb, nb, st)
            s = s//mb(1:nb)
        else
            write (buffer, "(o0)") ch2
            s = s//"\"//trim(buffer)
        end if
    end function event_description
end module keyboard_macro_editor_mod
