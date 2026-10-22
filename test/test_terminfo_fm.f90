! test_terminfo_fm.f90 -- tests for terminfo_fm.  Own tests: upstream has
! no terminfo tests (it relies on ncurses).
!
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

module test_terminfo_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use terminfo_fm, only: builtin_xterm, delete_padding, parse_terminfo, terminfo_type, tgetent, &
                           tgetflag, tgetnum, tgetstr, tgoto, tigetflag, tigetnum, tigetstr, tparam, &
                           tparm
    implicit none(type, external)
    private

    public :: collect_terminfo_fm

    character, parameter :: ESC = achar(27), NUL = achar(0)

contains

    subroutine collect_terminfo_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("tparm-xterm-strings", test_tparm_xterm), &
                    new_unittest("tparm-language", test_tparm_language), &
                    new_unittest("tparm-formats", test_tparm_formats), &
                    new_unittest("builtin-xterm", test_builtin), &
                    new_unittest("parse-compiled-entry", test_parse), &
                    new_unittest("parse-32-bit-numbers", test_parse_32), &
                    new_unittest("parse-invalid", test_parse_invalid), &
                    new_unittest("tgetent", test_tgetent), &
                    new_unittest("delete-padding", test_delete_padding) &
                    ]
    end subroutine collect_terminfo_fm

    function le16(v) result(s)
        integer, intent(in) :: v
        character(len=2) :: s
        integer :: u
        u = v
        if (u < 0) u = u + 65536
        s = char(mod(u, 256))//char(u/256)
    end function le16

    function le32(v) result(s)
        integer, intent(in) :: v
        character(len=4) :: s
        s = le16(mod(v, 65536))//le16(v/65536)
    end function le32

    !> A compiled entry: "test|Test terminal", am, cols#80, bel, cup, and
    !> extended Tc and smxx.  With `wide`, numbers are 32-bit.
    function sample_entry(wide) result(data)
        logical, intent(in) :: wide
        character(len=:), allocatable :: data, names, strtab, ext
        integer :: k
        names = "test|Test terminal"//NUL
        strtab = achar(7)//NUL//ESC//"[%i%p1%d;%p2%dH"//NUL
        if (wide) then
            data = le16(int(o'01036'))
        else
            data = le16(int(o'0432'))
        end if
        data = data//le16(len(names))//le16(2)//le16(1)//le16(11)//le16(len(strtab))
        data = data//names//char(0)//char(1)
        if (mod(len(data), 2) == 1) data = data//NUL
        if (wide) then
            data = data//le32(80)
        else
            data = data//le16(80)
        end if
        ! bel (2) at 0, cup (11) at 2, the rest absent.
        do k = 1, 11
            select case (k)
            case (2)
                data = data//le16(0)
            case (11)
                data = data//le16(2)
            case default
                data = data//le16(-1)
            end select
        end do
        data = data//strtab
        if (mod(len(data), 2) == 1) data = data//NUL
        ! Extended: one boolean (Tc), one string (smxx).
        ext = ESC//"[9m"//NUL//"Tc"//NUL//"smxx"//NUL
        data = data//le16(1)//le16(0)//le16(1)//le16(3)//le16(len(ext))
        data = data//char(1)//NUL
        data = data//le16(0)//le16(0)//le16(3)//ext
    end function sample_entry

    subroutine test_tparm_xterm(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: setaf = &
                                       ESC//"[%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m"
        call check(error, tparm(ESC//"[%i%p1%d;%p2%dH", 4_int64, 9_int64), ESC//"[5;10H")
        if (allocated(error)) return
        call check(error, tgoto(ESC//"[%i%p1%d;%p2%dH", 9_int64, 4_int64), ESC//"[5;10H")
        if (allocated(error)) return
        call check(error, tparm(setaf, 1_int64), ESC//"[31m")
        if (allocated(error)) return
        call check(error, tparm(setaf, 9_int64), ESC//"[91m")
        if (allocated(error)) return
        call check(error, tparm(setaf, 200_int64), ESC//"[38;5;200m")
        if (allocated(error)) return
        call check(error, tparm("%p1%c"//ESC//"[%p2%{1}%-%db", 65_int64, 5_int64), "A"//ESC//"[4b")
        if (allocated(error)) return
        call check(error, tparam(ESC//"[%i%p1%d;%p2%dr", 0_int64, 23_int64, 0_int64, 0_int64), &
                   ESC//"[1;24r")
    end subroutine test_tparm_xterm

    subroutine test_tparm_language(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: cond = "%?%p1%t1%e%p2%t2%e3%;"
        call check(error, tparm(cond, 1_int64, 0_int64), "1")
        if (allocated(error)) return
        call check(error, tparm(cond, 0_int64, 1_int64), "2")
        if (allocated(error)) return
        call check(error, tparm(cond, 0_int64, 0_int64), "3")
        if (allocated(error)) return
        ! Nested conditionals are skipped as a unit.
        call check(error, tparm("%?%p1%t%?%p2%tA%eB%;%eC%;", 0_int64, 1_int64), "C")
        if (allocated(error)) return
        call check(error, tparm("%?%p1%t%?%p2%tA%eB%;%eC%;", 1_int64, 0_int64), "B")
        if (allocated(error)) return
        call check(error, tparm("%'A'%p1%+%c", 1_int64), "B")
        if (allocated(error)) return
        call check(error, tparm("%p1%Pa%ga%ga%*%d", 3_int64), "9")
        if (allocated(error)) return
        call check(error, tparm("%p1%{3}%m%d %p1%{3}%/%d", 17_int64), "2 5")
        if (allocated(error)) return
        call check(error, tparm("%p1%p2%&%d %p1%p2%|%d %p1%p2%^%d", 12_int64, 10_int64), "8 14 6")
        if (allocated(error)) return
        call check(error, tparm("%p1%!%d %p1%~%d %p1%p2%A%d %p1%p2%O%d", 0_int64, 5_int64), &
                   "1 -1 0 1")
        if (allocated(error)) return
        call check(error, tparm("%p1%p2%>%d%p1%p2%<%d%p1%p1%=%d", 2_int64, 3_int64), "011")
        if (allocated(error)) return
        call check(error, tparm("100%%"), "100%")
        if (allocated(error)) return
        ! Division by zero gives 0; popping an empty stack gives 0.
        call check(error, tparm("%p1%{0}%/%d %d", 7_int64), "0 0")
    end subroutine test_tparm_language

    subroutine test_tparm_formats(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, tparm("%p1%02d", 5_int64), "05")
        if (allocated(error)) return
        call check(error, tparm("%p1%3d|", 5_int64), "  5|")
        if (allocated(error)) return
        call check(error, tparm("%p1%:-4d|", 3_int64), "3   |")
        if (allocated(error)) return
        call check(error, tparm("%p1%:+d", 3_int64), "+3")
        if (allocated(error)) return
        call check(error, tparm("%p1%d", -42_int64), "-42")
        if (allocated(error)) return
        call check(error, tparm("%p1%x %p1%X %p1%#x %p1%o", 255_int64), "ff FF 0xff 377")
        if (allocated(error)) return
        call check(error, tparm("%p1%.3d", 7_int64), "007")
    end subroutine test_tparm_formats

    subroutine test_builtin(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminfo_type) :: ti
        logical :: found
        call builtin_xterm(ti)
        call check(error, tgetstr(ti, "cm"), ESC//"[%i%p1%d;%p2%dH")
        if (allocated(error)) return
        call check(error, tigetstr(ti, "cup"), ESC//"[%i%p1%d;%p2%dH")
        if (allocated(error)) return
        call check(error, tgetnum(ti, "Co"), 256_int32)
        if (allocated(error)) return
        call check(error, tgetflag(ti, "am") .and. tigetflag(ti, "xenl"), "flags")
        if (allocated(error)) return
        call check(error, tigetstr(ti, "smxx"), ESC//"[9m")
        if (allocated(error)) return
        call check(error, tgetstr(ti, "smxx"), ESC//"[9m")
        if (allocated(error)) return
        call check(error, tgetstr(ti, "ku"), ESC//"OA")
        if (allocated(error)) return
        call check(error, tgetstr(ti, "XY", found) == "" .and. .not. found, "absent")
        if (allocated(error)) return
        call check(error, tgetnum(ti, "sg"), -1_int32)
    end subroutine test_builtin

    subroutine test_parse(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminfo_type) :: ti
        type(status_type) :: st
        call parse_terminfo(sample_entry(.false.), ti, st)
        call check(error, st%ok(), "parse")
        if (allocated(error)) return
        call check(error, ti%names, "test|Test terminal")
        if (allocated(error)) return
        call check(error, tigetflag(ti, "am") .and. .not. tigetflag(ti, "bw"), "flags")
        if (allocated(error)) return
        call check(error, tigetnum(ti, "cols"), 80_int32)
        if (allocated(error)) return
        call check(error, tgetstr(ti, "bl"), achar(7))
        if (allocated(error)) return
        call check(error, tgetstr(ti, "cm"), ESC//"[%i%p1%d;%p2%dH")
        if (allocated(error)) return
        call check(error, tigetstr(ti, "cr"), "")
        if (allocated(error)) return
        call check(error, tigetflag(ti, "Tc"), "extended flag")
        if (allocated(error)) return
        call check(error, tigetstr(ti, "smxx"), ESC//"[9m")
    end subroutine test_parse

    subroutine test_parse_32(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminfo_type) :: ti
        type(status_type) :: st
        call parse_terminfo(sample_entry(.true.), ti, st)
        call check(error, st%ok() .and. tigetnum(ti, "cols") == 80, "32-bit")
        if (allocated(error)) return
        call check(error, tigetstr(ti, "smxx"), ESC//"[9m")
    end subroutine test_parse_32

    subroutine test_parse_invalid(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminfo_type) :: ti
        type(status_type) :: st
        character(len=:), allocatable :: data
        call parse_terminfo("not a terminfo file", ti, st)
        call check(error, st%detail, "Invalid terminfo entry")
        if (allocated(error)) return
        ! Truncated in the string table.
        data = sample_entry(.false.)
        call parse_terminfo(data(1:40), ti, st)
        call check(error,.not. st%ok(), "truncated")
    end subroutine test_parse_invalid

    !> The real database when installed (CI has ncurses-base), else the
    !> built-in entry: either way xterm-256color works.
    subroutine test_tgetent(error)
        type(error_type), allocatable, intent(out) :: error
        type(terminfo_type) :: ti
        type(status_type) :: st
        call tgetent("xterm-256color", ti, st)
        call check(error, st%ok(), "xterm-256color")
        if (allocated(error)) return
        call check(error, index(ti%names, "xterm-256color") == 1, "names")
        if (allocated(error)) return
        call check(error, tgetstr(ti, "cm"), ESC//"[%i%p1%d;%p2%dH")
        if (allocated(error)) return
        call check(error, tgetnum(ti, "Co"), 256_int32)
        if (allocated(error)) return
        call tgetent("no-such-terminal-type", ti, st)
        call check(error, st%detail, "Terminal type no-such-terminal-type is not defined")
    end subroutine test_tgetent

    subroutine test_delete_padding(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, delete_padding(ESC//"[?5h$<100/>"//ESC//"[?5l"), &
                   ESC//"[?5h"//ESC//"[?5l")
        if (allocated(error)) return
        call check(error, delete_padding("$5"), "$5")
    end subroutine test_delete_padding
end module test_terminfo_fm
