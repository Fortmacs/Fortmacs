! edmacro_fm.f90 -- keyboard macros as text.
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

!> The printed form of keyboard macros (`format_kbd_macro'), which
!> kmacro_fm shows in the echo area.
!>
!> Deferred: the macro editor (`edit_kbd_macro', `edmacro_mode', C-x C-k
!> e and RET, `insert_kbd_macro'), `read_kbd_macro' and the parsing of
!> macro text (`edmacro_parse_keys'), and menu-bar and mouse events in
!> macros (`edmacro_fix_menu_commands': macros here hold characters and
!> function keys only).
module edmacro_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, nil, nilp, fixnump, symbolp, stringp, xfixnum, xsymbol, xstring, symbols, vtag, &
                       V_LIST, list_elements, make_fixnum, make_keymap_ref, make_symbol_ref
    use lread_fm, only: sym, intern_soft
    use data_fm, only: value_eq
    use character_fm, only: CHAR_ALT, CHAR_CTL, CHAR_HYPER, CHAR_META, CHAR_SUPER, CHAR_SHIFT, &
                            string_char_and_length, char_string, MAX_MULTIBYTE_LENGTH
    use keymap_fm, only: current_active_maps, lookup_key, key_binding, keymapp, keymap_live_p
    use callint_fm, only: commandp
    use keyboard_fm, only: local_function_key_map
    implicit none(type, external)
    private

    public :: format_kbd_macro

    !> Values of `format_kbd_macro''s VERBOSE: EDMACRO_COMPACT (a compact 80-column
    !> format), EDMACRO_ONE_LINE (everything on one line) and EDMACRO_VERBOSE (one command per line,
    !> with comments).
    integer, parameter, public :: EDMACRO_COMPACT = 0, EDMACRO_ONE_LINE = 1, EDMACRO_VERBOSE = 2

    !> True if `edit_kbd_macro' should leave 8-bit characters intact.
    !> Default false means to write characters above \177 in octal
    !> notation.  Port of edmacro.el edmacro-eight-bits.
    logical, public :: edmacro_eight_bits = .false.

contains

    !> Return the keyboard macro MACRO as a human-readable string.
    !> This string is suitable for passing to `read_kbd_macro'.
    !> Second argument VERBOSE means to put one command per line with
    !> comments (EDMACRO_VERBOSE).  If VERBOSE is EDMACRO_ONE_LINE, put
    !> everything on one line.  If VERBOSE is omitted or EDMACRO_COMPACT,
    !> use a compact 80-column format.  Port of edmacro.el:format-kbd-macro
    !> with edmacro-format-keys (MACRO is an array of events).
    recursive function format_kbd_macro(macro, verbose) result(res)
        type(value_type), intent(in) :: macro(:)
        integer, intent(in), optional :: verbose
        character(len=:), allocatable :: res
        type(value_type), allocatable :: rest(:), maps(:), key(:), tkey(:), fkey(:), one(:)
        type(value_type) :: b, bind, fk, first
        type(status_type) :: st
        character(len=:), allocatable :: prefix, desc, bind_name, text_chars
        integer(int64) :: n, pos, i, k, bind_len, text, tlen, times, p, j, maxkey, len_rest, ch, line_len
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
        rest(n + 1) = sym("end-macro")
        call active_map_list(maps)
        res = ""
        line_len = 0
        pos = 1

        do while (pos <= n)
            len_rest = n + 2 - pos
            ! A prefix argument: M-DIGITS, C-u C-u ... or C-u DIGITS.
            has_prefix = .false.
            prefix = ""

            if (in_mdigs(rest(pos), .false.)) then
                one = [rest(pos)]
                call key_binding(one, b, st)

                if (value_eq(b, sym("digit_argument")) .or. value_eq(b, sym("negative_argument"))) then
                    i = 1

                    do while (in_mdigs(rest(pos + i), .true.))
                        i = i + 1
                    end do

                    if (.not. in_pkeys(rest(pos + i))) then
                        prefix = "M-"

                        do k = pos, pos + i - 1
                            prefix = prefix//achar(iand(xfixnum(rest(k)), 127_int64))
                        end do

                        prefix = prefix//" "
                        pos = pos + i
                        has_prefix = .true.
                    end if
                end if
            end if

            if (.not. has_prefix .and. int_event(rest(pos)) == 21) then
                one = [rest(pos)]
                call key_binding(one, b, st)
                uarg = value_eq(b, sym("universal_argument"))

                if (uarg) then
                    i = 1

                    do while (int_event(rest(pos + i)) == 21)
                        i = i + 1
                    end do

                    if (.not. in_pkeys(rest(pos + i))) then
                        prefix = repeat("C-u ", int(i))
                        pos = pos + i
                        has_prefix = .true.
                    end if
                end if

                if (uarg .and. .not. has_prefix) then
                    i = 1
                    if (int_event(rest(pos + i)) == iachar("-")) i = i + 1

                    do while (int_event(rest(pos + i)) >= iachar("0") .and. int_event(rest(pos + i)) <= iachar("9"))
                        i = i + 1
                    end do

                    if (.not. in_pkeys(rest(pos + i))) then
                        prefix = "C-u "

                        do k = pos + 1, pos + i - 1
                            prefix = prefix//achar(xfixnum(rest(k)))
                        end do

                        prefix = prefix//" "
                        pos = pos + i
                        has_prefix = .true.
                    end if
                end if
            end if

            len_rest = n + 2 - pos
            ! The longest bound key sequence at the start.
            bind_len = 1

            do k = 1, size(maps)
                call lookup_key(maps(k), rest(pos:n + 1), b, st)
                if (fixnump(b)) bind_len = max(bind_len, xfixnum(b))
            end do

            key = rest(pos:pos + bind_len - 1)
            bind = nil()
            found = .false.
            use_fkey = .false.

            do k = 1, size(maps)
                call lookup_key(maps(k), key, b, st)

                if (.not. nilp(b) .and. .not. fixnump(b)) then
                    bind = b
                    found = .true.
                    exit
                end if
            end do

            if (.not. found .and. keymap_live_p(local_function_key_map)) then
                call lookup_key(make_keymap_ref(local_function_key_map), rest(pos:n + 1), fk, st)

                if (fixnump(fk)) then
                    tlen = xfixnum(fk)
                    tkey = rest(pos:pos + tlen - 1)
                    call lookup_key(make_keymap_ref(local_function_key_map), tkey, fk, st)

                    if (.not. nilp(fk) .and. .not. fixnump(fk) .and. .not. keymapp(fk)) then
                        call translation_events(fk, fkey)

                        do k = 1, size(maps)
                            call lookup_key(maps(k), fkey, b, st)

                            if (.not. nilp(b) .and. .not. fixnump(b)) then
                                bind_len = tlen
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
            i = bind_len

            do while (i < len_rest)
                ch = int_event(rest(pos + i))
                if (ch <= 32 .or. ch >= maxkey .or. ch == 92) exit
                if (.not. self_inserting(ch)) exit

                if (i <= len_rest - 2) then
                    if (int_event(rest(pos + i + 1)) == ch) then
                        if (int_event(rest(pos + i + 2)) == ch) exit
                    end if
                end if

                i = i + 1
            end do

            text = i
            bind_name = ""
            if (symbolp(bind) .and. .not. nilp(bind)) bind_name = symbols(xsymbol(bind))%name
            if (stringp(bind)) bind = nil()
            done = .false.
            ch = int_event(first)

            if (value_eq(bind, sym("self_insert_command")) .and. .not. has_prefix .and. text > 1 .and. &
                ch > 32 .and. ch <= maxkey .and. ch /= 92) then
                if (text > 30) text = 30
                desc = event_chars(rest(pos:pos + text - 1))

                if (len(desc) >= 3) then
                    if (index("ACHMsS", desc(1:1)) > 0 .and. desc(2:2) == "-" .and. desc(3:3) /= achar(10)) then
                        text = 2
                        desc = desc(1:2)
                    end if
                end if

                if (.not. reserved_desc(desc)) then
                    if (len(desc) == 2) then
                        if (desc(1:1) == "^") desc = desc(1:1)//" "//desc(2:2)
                    end if

                    if (res_word(desc)) then
                        text_chars = desc
                        desc = ""

                        do k = 1, len(text_chars)
                            if (k > 1) desc = desc//" "
                            desc = desc//text_chars(k:k)
                        end do
                    end if

                    if (verb) bind_name = bind_name//" * "//int_text(text)
                    bind_len = text
                    done = .true.
                end if
            end if

            if (.not. done .and. value_eq(bind, sym("execute_extended_command")) .and. text > bind_len) then
                if (int_event(rest(pos + text)) == 13 .or. value_eq(rest(pos + text), sym("return"))) then
                    desc = event_chars(rest(pos + bind_len:pos + text - 1))
                    k = intern_soft(desc)

                    if (k > 0) then
                        if (commandp(make_symbol_ref(k))) then
                            bind_name = desc
                            desc = "<<"//desc//">>"
                            bind_len = text + 1
                            done = .true.
                        end if
                    end if
                end if
            end if

            if (.not. done) then
                desc = ""

                if (use_fkey) then
                    do k = 1, size(fkey)
                        if (k > 1) desc = desc//" "
                        desc = desc//event_description(fkey(k), maxkey)
                    end do
                else
                    do k = 1, size(key)
                        if (k > 1) desc = desc//" "
                        desc = desc//event_description(key(k), maxkey)
                    end do
                end if
            end if

            if (has_prefix) desc = prefix//desc

            ! Repetitions of a key without spaces: 3*C-f.
            if (index(desc, " ") == 0) then
                times = 1
                p = bind_len

                do
                    same = .true.

                    do j = 0, bind_len - 1
                        if (pos + p + j > n + 1) then
                            same = .false.
                            exit
                        end if

                        if (.not. value_eq(rest(pos + j), rest(pos + p + j))) then
                            same = .false.
                            exit
                        end if
                    end do

                    if (.not. same) exit
                    times = times + 1
                    p = p + bind_len
                end do

                if (times > 1) then
                    desc = int_text(times)//"*"//desc
                    bind_len = bind_len*times
                end if
            end if

            pos = pos + bind_len

            if (verb) then
                if (len(res) > 0) res = res//achar(10)
                res = res//desc

                if (len(bind_name) > 0) then
                    res = res//repeat(achar(9), int(max(3 - nchars(desc)/8, 1_int64)))//";; "//bind_name
                end if

                line_len = 0
            else
                if (line_len + nchars(desc) + 2 > 72 .and. .not. one_line) then
                    res = res//achar(10)//" "
                    line_len = 1
                else if (len(res) > 0) then
                    res = res//" "
                    line_len = line_len + 1
                end if

                res = res//desc
                line_len = line_len + nchars(desc)
            end if
        end do
    end function format_kbd_macro

    !> The active keymaps (current-active-maps) as an array.  Not a port.
    subroutine active_map_list(maps)
        type(value_type), allocatable, intent(out) :: maps(:)
        type(value_type) :: all

        all = current_active_maps()

        if (vtag(all) == V_LIST) then
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
        integer(int64) :: pos, n
        integer(int32) :: c, length

        if (vtag(v) == V_LIST) then
            call list_elements(v, evs)
            return
        end if

        if (.not. stringp(v)) then
            allocate (evs(1))
            evs(1) = v
            return
        end if

        s = xstring(v)
        allocate (evs(len(s)))
        n = 0
        pos = 1

        do while (pos <= len(s, kind=int64))
            call string_char_and_length(s, pos, c, length)
            n = n + 1
            evs(n) = make_fixnum(int(c, int64))
            pos = pos + length
        end do

        evs = evs(1:n)
    end subroutine translation_events

    !> The character code of event EV, or -1 if it is not a character.
    !> Not a port.
    integer(int64) function int_event(ev)
        type(value_type), intent(in) :: ev

        int_event = -1
        if (fixnump(ev)) int_event = xfixnum(ev)
    end function int_event

    !> Whether EV is one of M-- M-0 ... M-9 (only M-0 ... M-9 if DIGITS_ONLY).
    !> Not a port.
    logical function in_mdigs(ev, digits_only)
        type(value_type), intent(in) :: ev
        logical, intent(in) :: digits_only
        integer(int64) :: c

        c = int_event(ev)
        in_mdigs = .false.
        if (c < 0) return
        if (iand(c, int(CHAR_META, int64)) == 0) return
        c = c - int(CHAR_META, int64)
        in_mdigs = c >= iachar("0") .and. c <= iachar("9")
        if (.not. digits_only .and. c == iachar("-")) in_mdigs = .true.
    end function in_mdigs

    !> Whether EV is one of the prefix-argument keys (pkeys): the end of
    !> the macro, 0 ... 9, -, C-u, M--, M-0 ... M-9.  Not a port.
    logical function in_pkeys(ev)
        type(value_type), intent(in) :: ev
        integer(int64) :: c

        in_pkeys = value_eq(ev, sym("end-macro"))
        if (in_pkeys) return
        c = int_event(ev)
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

        key(1) = make_fixnum(ch)
        call key_binding(key, b, st)
        self_inserting = value_eq(b, sym("self_insert_command"))
    end function self_inserting

    !> The characters EVS (all ASCII) as a string.  Not a port.
    function event_chars(evs) result(s)
        type(value_type), intent(in) :: evs(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(evs)
            s = s//achar(int_event(evs(k)))
        end do
    end function event_chars

    !> Whether DESC matches "^;;\|^<.*>$\|^\\[0-9]+$\|^[0-9]+\*.": text
    !> that would read back as something else.
    !> Port of part of edmacro.el:edmacro-format-keys.
    logical function reserved_desc(desc)
        character(len=*), intent(in) :: desc
        integer :: k

        reserved_desc = .false.
        if (index(desc, ";;") == 1) reserved_desc = .true.

        if (len(desc) >= 2) then
            if (desc(1:1) == "<" .and. desc(len(desc):len(desc)) == ">") reserved_desc = .true.
        end if

        if (len(desc) >= 2) then
            if (desc(1:1) == "\" .and. verify(desc(2:), "0123456789") == 0) reserved_desc = .true.
        end if

        k = verify(desc, "0123456789")

        if (k > 1 .and. k < len(desc)) then
            if (desc(k:k) == "*" .and. desc(k + 1:k + 1) /= achar(10)) reserved_desc = .true.
        end if
    end function reserved_desc

    !> Whether DESC is one of the reserved words NUL TAB LFD RET ESC SPC DEL
    !> REM.  Port of part of edmacro.el:edmacro-format-keys.
    logical function res_word(desc)
        character(len=*), intent(in) :: desc

        select case (desc)
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
        character(len=24) :: buf
        character(len=MAX_MULTIBYTE_LENGTH) :: mb
        integer(int32) :: nb
        type(status_type) :: st
        integer :: k

        if (symbolp(ev)) then
            s = "<"//symbols(xsymbol(ev))%name//">"
            return
        end if

        bits = [int(CHAR_ALT, int64), int(CHAR_CTL, int64), int(CHAR_HYPER, int64), int(CHAR_META, int64), &
                int(CHAR_SUPER, int64), int(CHAR_SHIFT, int64)]
        ch = int_event(ev)
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
            call char_string(int(ch2, int32), mb, nb, st)
            s = s//mb(1:nb)
        else
            write (buf, "(o0)") ch2
            s = s//"\"//trim(buf)
        end if
    end function event_description

    !> N in decimal.  Not a port.
    function int_text(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    !> The number of characters of the UTF-8 text S.  Not a port.
    integer(int64) function nchars(s)
        character(len=*), intent(in) :: s
        integer :: k

        nchars = 0

        do k = 1, len(s)
            if (iand(ichar(s(k:k)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars
end module edmacro_fm
