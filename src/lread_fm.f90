! lread_fm.f90 -- the obarray: interning symbols by name.
! Ported from GNU Emacs 31.1 src/lread.c (the obarray part; there is no
! Lisp reader).
!
! Copyright (C) 1985-1989, 1993-1995, 1997-2026 Free Software Foundation,
! Inc.
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

!> One global obarray, an open-addressing hash table of symbol ids.
!> Symbols are never removed (upstream's `unintern' is not ported).
!> Names starting with ':' are keywords: constants whose value is
!> themselves, as in lread.c:intern_sym.
!>
!> `read_string_literal' reads the text of a string constant, as `read'
!> does after an opening double quote.
module lread_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use lisp_fm, only: symbols, nsymbols, symbol_type, value_type, &
                       make_symbol_ref, nil, SYM_NIL, SYM_T
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use character_fm, only: string_char_and_length, char_string, char_hexdigit, byte8_to_char, char_byte8_p, &
                            char_surrogate_p, MAX_MULTIBYTE_LENGTH, CHAR_SHIFT, CHAR_CTL, CHAR_META, &
                            CHAR_ALT, CHAR_SUPER, CHAR_HYPER, CHAR_MODIFIER_MASK
    implicit none(type, external)
    private

    public :: intern, intern_soft, sym, ensure_obarray, read_string_literal

    !> File name, including directory, of user's initialization file.
    !> While the init file's `configure' runs, value is the real name
    !> of the file.  Port of lread.c user-init-file (unallocated is nil:
    !> -q, or no init file compiled in).
    character(len=:), allocatable, public :: user_init_file

    !> Hash slots holding symbol ids (0 = empty); size is a power of two.
    integer(int64), allocatable :: slots(:)

contains

    !> Hash of `name' (polynomial, kept below 2**31 so it cannot overflow).
    pure integer(int64) function hash_name(name)
        character(len=*), intent(in) :: name
        integer :: k

        hash_name = 0

        do k = 1, len(name)
            hash_name = mod(hash_name*31 + ichar(name(k:k), int64), 2147483647_int64)
        end do
    end function hash_name

    !> Slot where `name' is, or the empty slot where it would go.
    integer(int64) function find_slot(name)
        character(len=*), intent(in) :: name
        integer(int64) :: mask, id

        mask = size(slots, kind=int64) - 1
        find_slot = iand(hash_name(name), mask) + 1

        do
            id = slots(find_slot)
            if (id == 0) return

            if (len(symbols(id)%name) == len(name)) then
                if (symbols(id)%name == name) return
            end if

            find_slot = iand(find_slot, mask) + 1
        end do
    end function find_slot

    !> Create the tables and the symbols nil and t.
    !> Port of lread.c:init_obarray_once.
    subroutine init_obarray()
        integer(int64) :: id

        allocate (slots(64))
        slots = 0
        allocate (symbols(32))
        nsymbols = 0
        id = add_symbol("nil")
        symbols(id)%constant = .true.
        symbols(id)%bound = .true.
        symbols(id)%val = nil()
        id = add_symbol("t")
        symbols(id)%constant = .true.
        symbols(id)%bound = .true.
        symbols(id)%val = make_symbol_ref(SYM_T)
    end subroutine init_obarray

    !> Append a new symbol named `name' and index it.
    integer(int64) function add_symbol(name) result(id)
        character(len=*), intent(in) :: name
        type(symbol_type), allocatable :: grown(:)

        if (nsymbols == size(symbols, kind=int64)) then
            allocate (grown(2*nsymbols))
            grown(1:nsymbols) = symbols(1:nsymbols)
            call move_alloc(grown, symbols)
        end if

        ! Keep the table at most half full.
        if (2*(nsymbols + 1) > size(slots, kind=int64)) call rehash(2*size(slots, kind=int64))
        nsymbols = nsymbols + 1
        id = nsymbols
        symbols(id) = symbol_type()
        symbols(id)%name = name
        slots(find_slot(name)) = id

        ! Keywords are constants that evaluate to themselves.
        if (len(name) > 0) then
            if (name(1:1) == ":") then
                symbols(id)%constant = .true.
                symbols(id)%bound = .true.
                symbols(id)%val = make_symbol_ref(id)
            end if
        end if
    end function add_symbol

    subroutine rehash(new_size)
        integer(int64), intent(in) :: new_size
        integer(int64) :: id

        deallocate (slots)
        allocate (slots(new_size))
        slots = 0

        do id = 1, nsymbols
            slots(find_slot(symbols(id)%name)) = id
        end do
    end subroutine rehash

    !> Create nil and t if no symbol exists yet.  Anything that indexes
    !> `symbols' without interning first must call this.
    subroutine ensure_obarray()
        if (.not. allocated(slots)) call init_obarray()
    end subroutine ensure_obarray

    !> Id of the symbol named `name', creating it if needed.
    !> Port of lread.c:Fintern (initial obarray).
    integer(int64) function intern(name) result(id)
        character(len=*), intent(in) :: name

        if (.not. allocated(slots)) call init_obarray()
        id = slots(find_slot(name))
        if (id == 0) id = add_symbol(name)
    end function intern

    !> Id of the symbol named `name', or 0 if there is none.
    !> Port of lread.c:Fintern_soft.
    integer(int64) function intern_soft(name) result(id)
        character(len=*), intent(in) :: name

        if (.not. allocated(slots)) call init_obarray()
        id = slots(find_slot(name))
    end function intern_soft

    !> The symbol named `name' as a value (nil for "nil"); the Fortmacs
    !> spelling of 'name.  Interns it if needed.
    function sym(name) result(v)
        character(len=*), intent(in) :: name
        type(value_type) :: v

        v = make_symbol_ref(intern(name))
    end function sym

    ! ------------------------------------------------------------------
    ! Reading string constants
    ! ------------------------------------------------------------------
    !> Read a string constant from TEXT (internal encoding), starting at
    !> byte START, just after the opening double quote: VALUE is the
    !> string read and NEXT the byte after the closing quote.  Escape
    !> sequences are as in Emacs Lisp: \a \b \d \e \f \n \r \t \v, \s
    !> (space), octal (1 to 3 digits) and \x hex escapes (values 128 to
    !> 255 with fewer than 3 hex digits are raw bytes), \uHHHH, \UHHHHHHHH,
    !> \N{U+X}, \C- and \^ (control), \M- (meta: a raw byte), \S- (shift
    !> on letters), \SPC and \<newline> (no character); any other character
    !> stands for itself.  Port of lread.c:read_string_literal with
    !> read_char_escape (the value is multibyte text, raw bytes as
    !> eight-bit characters; \N{NAME} with a character name is deferred;
    !> the invalid-read-syntax and end-of-file conditions are errors).
    subroutine read_string_literal(text, start, value, next, status)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: start
        character(len=:), allocatable, intent(out) :: value
        integer(int64), intent(out) :: next
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        integer(int32) :: ch, n, modifiers
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        type(status_type) :: st

        value = ""
        pos = start
        next = len(text) + 1

        do
            if (pos > len(text)) then
                call set_error(status, FM_GENERIC_ERROR, "End of file during parsing")
                return
            end if

            call string_char_and_length(text, pos, ch, n)
            pos = pos + n
            if (ch == iachar('"')) exit

            if (ch == iachar("\")) then
                if (pos > len(text)) then
                    call set_error(status, FM_GENERIC_ERROR, "End of file during parsing")
                    return
                end if

                call string_char_and_length(text, pos, ch, n)
                pos = pos + n

                if (ch == iachar("s")) then
                    ! `\s' is always a space in strings.
                    ch = iachar(" ")
                else if (ch == iachar(" ") .or. ch == 10) then
                    ! `\SPC' and `\LF' generate no characters at all.
                    cycle
                else
                    call read_char_escape(text, pos, ch, status)
                    if (.not. status%ok()) return
                end if

                modifiers = iand(ch, CHAR_MODIFIER_MASK)
                ch = iand(ch, not(CHAR_MODIFIER_MASK))

                if (ch < 128) then
                    ! Allow `\C-SPC' and `\^SPC'.
                    if (modifiers == CHAR_CTL .and. ch == iachar(" ")) then
                        ch = 0
                        modifiers = 0
                    end if

                    if (iand(modifiers, CHAR_SHIFT) /= 0) then
                        ! Shift modifier is valid only with [A-Za-z].
                        if (ch >= iachar("A") .and. ch <= iachar("Z")) then
                            modifiers = iand(modifiers, not(CHAR_SHIFT))
                        else if (ch >= iachar("a") .and. ch <= iachar("z")) then
                            ch = ch - (iachar("a") - iachar("A"))
                            modifiers = iand(modifiers, not(CHAR_SHIFT))
                        end if
                    end if

                    if (iand(modifiers, CHAR_META) /= 0) then
                        ! Move the meta bit to the right place for a string.
                        modifiers = iand(modifiers, not(CHAR_META))
                        ch = byte8_to_char(ior(ch, 128))
                    end if
                end if

                ! Any modifiers remaining are invalid.
                if (modifiers /= 0) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid modifier in string")
                    return
                end if
            end if

            call char_string(ch, buf, n, st)
            value = value//buf(1:n)
        end do

        next = pos
    end subroutine read_string_literal

    !> Read a character escape sequence from TEXT at POS, the character C
    !> after the backslash having been read: C becomes the character with
    !> its modifier bits.  Port of lread.c:read_char_escape.
    recursive subroutine read_char_escape(text, pos, c, status)
        character(len=*), intent(in) :: text
        integer(int64), intent(inout) :: pos
        integer(int32), intent(inout) :: c
        type(status_type), intent(out) :: status
        integer(int32) :: modifiers, ncontrol, chr, c1, i, count, digit, nhex, k, n
        integer(int64) :: save
        logical :: done

        status = status_type()
        modifiers = 0
        ncontrol = 0
        chr = 0
        done = .false.

        do while (.not. done)
            done = .true.

            select case (c)
            case (iachar("a"))
                chr = 7
            case (iachar("b"))
                chr = 8
            case (iachar("d"))
                chr = 127
            case (iachar("e"))
                chr = 27
            case (iachar("f"))
                chr = 12
            case (iachar("n"))
                chr = 10
            case (iachar("r"))
                chr = 13
            case (iachar("t"))
                chr = 9
            case (iachar("v"))
                chr = 11
            case (10)
                call set_error(status, FM_GENERIC_ERROR, "Invalid escape char syntax: \<newline>")
                return
            case (iachar("M"), iachar("S"), iachar("H"), iachar("A"), iachar("s"))
                if (.not. next_char(c1)) return

                if (c1 /= iachar("-")) then
                    if (c == iachar("s")) then
                        ! \s not followed by a hyphen is SPC.
                        pos = save
                        chr = iachar(" ")
                    else
                        call set_error(status, FM_GENERIC_ERROR, "Invalid escape char syntax: \"// &
                                       achar(c)//" not followed by -")
                        return
                    end if
                else
                    select case (c)
                    case (iachar("M"))
                        modifiers = ior(modifiers, CHAR_META)
                    case (iachar("S"))
                        modifiers = ior(modifiers, CHAR_SHIFT)
                    case (iachar("H"))
                        modifiers = ior(modifiers, CHAR_HYPER)
                    case (iachar("A"))
                        modifiers = ior(modifiers, CHAR_ALT)
                    case default
                        modifiers = ior(modifiers, CHAR_SUPER)
                    end select

                    if (.not. next_char(c1)) return

                    if (c1 == iachar("\")) then
                        if (.not. next_char(c)) return
                        done = .false.
                    else
                        chr = c1
                    end if
                end if
            case (iachar("C"), iachar("^"))
                if (c == iachar("C")) then
                    if (.not. next_char(c1)) return

                    if (c1 /= iachar("-")) then
                        call set_error(status, FM_GENERIC_ERROR, "Invalid escape char syntax: \C not followed by -")
                        return
                    end if
                end if

                ncontrol = ncontrol + 1
                if (.not. next_char(c1)) return

                if (c1 == iachar("\")) then
                    if (.not. next_char(c)) return
                    done = .false.
                else
                    chr = c1
                end if
            case (iachar("0"):iachar("7"))
                ! 1-3 octal digits.  Values in 0x80..0xff are raw bytes.
                i = c - iachar("0")

                do count = 1, 2
                    if (pos > len(text)) exit
                    c1 = iachar(text(pos:pos))
                    if (c1 < iachar("0") .or. c1 > iachar("7")) exit
                    i = i*8 + (c1 - iachar("0"))
                    pos = pos + 1
                end do

                if (i >= 128 .and. i < 256) i = byte8_to_char(i)
                chr = i
            case (iachar("x"))
                ! 1 or more hex digits.
                i = 0
                count = 0

                do
                    if (pos > len(text)) exit
                    digit = char_hexdigit(iachar(text(pos:pos), int32))
                    if (digit < 0) exit

                    if (i > ior(CHAR_META, CHAR_META - 1)/16) then
                        call set_error(status, FM_GENERIC_ERROR, "Hex character out of range")
                        return
                    end if

                    i = i*16 + digit
                    pos = pos + 1
                    count = min(count + 1, 3)
                end do

                if (count == 0) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid escape char syntax: \x not followed by hex digit")
                    return
                end if

                if (count < 3 .and. i >= 128) i = byte8_to_char(i)
                modifiers = ior(modifiers, iand(i, CHAR_MODIFIER_MASK))
                chr = iand(i, not(CHAR_MODIFIER_MASK))
            case (iachar("u"), iachar("U"))
                nhex = 4
                if (c == iachar("U")) nhex = 8
                i = 0

                do k = 1, nhex
                    if (pos > len(text)) then
                        call set_error(status, FM_GENERIC_ERROR, "Malformed Unicode escape")
                        return
                    end if

                    digit = char_hexdigit(iachar(text(pos:pos), int32))

                    if (digit < 0) then
                        call set_error(status, FM_GENERIC_ERROR, "Non-hex character used for Unicode escape: "// &
                                       text(pos:pos))
                        return
                    end if

                    if (i > int(z'10FFFF', int32)) then
                        call set_error(status, FM_GENERIC_ERROR, "Non-Unicode character")
                        return
                    end if

                    i = i*16 + digit
                    pos = pos + 1
                end do

                if (i > int(z'10FFFF', int32)) then
                    call set_error(status, FM_GENERIC_ERROR, "Non-Unicode character")
                    return
                end if

                chr = i
            case (iachar("N"))
                ! Named character: only \N{U+X} here.
                if (.not. next_char(c1)) return

                if (c1 /= iachar("{")) then
                    call set_error(status, FM_GENERIC_ERROR, "Expected opening brace after \N")
                    return
                end if

                k = index(text(pos:), "}")

                if (k == 0) then
                    call set_error(status, FM_GENERIC_ERROR, "End of file during parsing")
                    return
                end if

                n = k - 1
                i = -1

                if (n >= 3) then
                    if (text(pos:pos + 1) == "U+") then
                        i = 0

                        do count = 2, n - 1
                            digit = char_hexdigit(iachar(text(pos + count:pos + count), int32))

                            if (digit < 0 .or. i > int(z'10FFFF', int32)) then
                                i = -1
                                exit
                            end if

                            i = i*16 + digit
                        end do
                    end if
                end if

                if (i < 0 .or. i > int(z'10FFFF', int32)) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid read syntax: \N{"//text(pos:pos + n - 1)//"}")
                    return
                end if

                if (char_surrogate_p(i)) then
                    call set_error(status, FM_GENERIC_ERROR, "Invalid read syntax: \N{"//text(pos:pos + n - 1)//"}")
                    return
                end if

                pos = pos + k
                chr = i
            case default
                chr = c
            end select
        end do

        ! Apply Control modifiers.
        do while (ncontrol > 0)
            if ((chr >= iachar("@") .and. chr <= iachar("_")) .or. (chr >= iachar("a") .and. chr <= iachar("z"))) then
                chr = iand(chr, 31)
            else if (chr == iachar("?")) then
                chr = 127
            else
                modifiers = ior(modifiers, CHAR_CTL)
            end if

            ncontrol = ncontrol - 1
        end do

        c = ior(chr, modifiers)

    contains

        !> Read the next character into CH (false at the end of TEXT, with
        !> the end-of-file error); SAVE is where it started.
        logical function next_char(ch) result(ok)
            integer(int32), intent(out) :: ch
            integer(int32) :: len1

            save = pos
            ok = pos <= len(text)
            ch = -1

            if (.not. ok) then
                call set_error(status, FM_GENERIC_ERROR, "End of file during parsing")
                return
            end if

            call string_char_and_length(text, pos, ch, len1)
            pos = pos + len1
        end function next_char
    end subroutine read_char_escape
end module lread_fm
