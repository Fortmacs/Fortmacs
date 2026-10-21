! terminfo_fm.f90 -- terminal capabilities from the terminfo database.
! Ported from GNU Emacs 31.1 src/terminfo.c, with the parts of the
! terminfo library that Emacs calls (tgetent, tgetstr, tgetnum, tgetflag,
! tigetstr, tparm) reimplemented in Fortran.
!
! Copyright (C) 1985-1986, 2001-2026 Free Software Foundation, Inc.
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

!> Upstream links ncurses' libtinfo; Fortmacs stays pure Fortran and reads
!> the compiled terminfo files itself (format: term(5)), including
!> extended (user-defined) capabilities, and evaluates parameterized
!> strings (terminfo(5) "Parameterized Strings") in `tparm`.
!>
!>     call tgetent("xterm-256color", ti, status)
!>     out = tgoto(tgetstr(ti, "cm"), col, row)   ! termcap names, as term.c
!>     out = tparm(tigetstr(ti, "setaf"), 1_int64) ! terminfo names
!>
!> The search order is ncurses': $TERMINFO, ~/.terminfo, $TERMINFO_DIRS
!> (an empty element means the system directories), then /etc/terminfo,
!> /lib/terminfo, /usr/share/terminfo.  If no entry is found for a name
!> starting with "xterm", a built-in xterm-256color subset is used, so
!> Fortmacs still runs without a terminfo database.
!>
!> The capability name tables are the standard terminfo order (SVr4/X/Open
!> Curses), as compiled into every terminfo file; they were taken from
!> ncurses 6.4.
!>
!> Not supported: string parameters in `tparm` (%s and %l see empty
!> strings), padding delays (`delete_padding` drops them; modern terminals
!> need none), and the termcap database.
module terminfo_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_FILE_ERROR
    implicit none(type, external)
    private

    public :: terminfo_type, tgetent, read_terminfo, parse_terminfo, &
              builtin_xterm
    public :: tigetflag, tigetnum, tigetstr, tgetflag, tgetnum, tgetstr
    public :: tparm, tparam, tgoto, delete_padding
    public :: BOOLCOUNT, NUMCOUNT, STRCOUNT

    integer, parameter :: BOOLCOUNT = 44, NUMCOUNT = 39, STRCOUNT = 414

    !> Magic numbers of compiled entries: 16-bit and 32-bit numbers.
    integer(int32), parameter :: MAGIC = int(o'0432', int32), &
                                 MAGIC2 = int(o'01036', int32)

    character(len=5), parameter :: BOOL_NAMES(44) = [character(len=5) :: &
                                                     "bw", "am", "xsb", "xhp", "xenl", "eo", "gn", "hc", &
                                                     "km", "hs", "in", "da", "db", "mir", "msgr", "os", &
                                                     "eslok", "xt", "hz", "ul", "xon", "nxon", "mc5i", "chts", &
                                                     "nrrmc", "npc", "ndscr", "ccc", "bce", "hls", "xhpa", "crxm", &
                                                     "daisy", "xvpa", "sam", "cpix", "lpix", "OTbs", "OTns", "OTnc", &
                                                     "OTMT", "OTNL", "OTpt", "OTxr"]
    character(len=2), parameter :: BOOL_CODES(44) = [character(len=2) :: &
                                                     "bw", "am", "xb", "xs", "xn", "eo", "gn", "hc", &
                                                     "km", "hs", "in", "da", "db", "mi", "ms", "os", &
                                                     "es", "xt", "hz", "ul", "xo", "nx", "5i", "HC", &
                                                     "NR", "NP", "ND", "cc", "ut", "hl", "YA", "YB", &
                                                     "YC", "YD", "YE", "YF", "YG", "bs", "ns", "nc", &
                                                     "MT", "NL", "pt", "xr"]
    character(len=6), parameter :: NUM_NAMES(39) = [character(len=6) :: &
                                                    "cols", "it", "lines", "lm", "xmc", "pb", "vt", "wsl", &
                                                    "nlab", "lh", "lw", "ma", "wnum", "colors", "pairs", "ncv", &
                                                    "bufsz", "spinv", "spinh", "maddr", "mjump", "mcs", "mls", "npins", &
                                                    "orc", "orl", "orhi", "orvi", "cps", "widcs", "btns", "bitwin", &
                                                    "bitype", "OTug", "OTdC", "OTdN", "OTdB", "OTdT", "OTkn"]
    character(len=2), parameter :: NUM_CODES(39) = [character(len=2) :: &
                                                    "co", "it", "li", "lm", "sg", "pb", "vt", "ws", &
                                                    "Nl", "lh", "lw", "ma", "MW", "Co", "pa", "NC", &
                                                    "Ya", "Yb", "Yc", "Yd", "Ye", "Yf", "Yg", "Yh", &
                                                    "Yi", "Yj", "Yk", "Yl", "Ym", "Yn", "BT", "Yo", &
                                                    "Yp", "ug", "dC", "dN", "dB", "dT", "kn"]
    character(len=8), parameter :: STR_NAMES(414) = [character(len=8) :: &
                                                     "cbt", "bel", "cr", "csr", "tbc", "clear", "el", "ed", &
                                                     "hpa", "cmdch", "cup", "cud1", "home", "civis", "cub1", "mrcup", &
                                                     "cnorm", "cuf1", "ll", "cuu1", "cvvis", "dch1", "dl1", "dsl", &
                                                     "hd", "smacs", "blink", "bold", "smcup", "smdc", "dim", "smir", &
                                                     "invis", "prot", "rev", "smso", "smul", "ech", "rmacs", "sgr0", &
                                                     "rmcup", "rmdc", "rmir", "rmso", "rmul", "flash", "ff", "fsl", &
                                                     "is1", "is2", "is3", "if", "ich1", "il1", "ip", "kbs", &
                                                     "ktbc", "kclr", "kctab", "kdch1", "kdl1", "kcud1", "krmir", "kel", &
                                                     "ked", "kf0", "kf1", "kf10", "kf2", "kf3", "kf4", "kf5", &
                                                     "kf6", "kf7", "kf8", "kf9", "khome", "kich1", "kil1", "kcub1", &
                                                     "kll", "knp", "kpp", "kcuf1", "kind", "kri", "khts", "kcuu1", &
                                                     "rmkx", "smkx", "lf0", "lf1", "lf10", "lf2", "lf3", "lf4", &
                                                     "lf5", "lf6", "lf7", "lf8", "lf9", "rmm", "smm", "nel", &
                                                     "pad", "dch", "dl", "cud", "ich", "indn", "il", "cub", &
                                                     "cuf", "rin", "cuu", "pfkey", "pfloc", "pfx", "mc0", "mc4", &
                                                     "mc5", "rep", "rs1", "rs2", "rs3", "rf", "rc", "vpa", &
                                                     "sc", "ind", "ri", "sgr", "hts", "wind", "ht", "tsl", &
                                                     "uc", "hu", "iprog", "ka1", "ka3", "kb2", "kc1", "kc3", &
                                                     "mc5p", "rmp", "acsc", "pln", "kcbt", "smxon", "rmxon", "smam", &
                                                     "rmam", "xonc", "xoffc", "enacs", "smln", "rmln", "kbeg", "kcan", &
                                                     "kclo", "kcmd", "kcpy", "kcrt", "kend", "kent", "kext", "kfnd", &
                                                     "khlp", "kmrk", "kmsg", "kmov", "knxt", "kopn", "kopt", "kprv", &
                                                     "kprt", "krdo", "kref", "krfr", "krpl", "krst", "kres", "ksav", &
                                                     "kspd", "kund", "kBEG", "kCAN", "kCMD", "kCPY", "kCRT", "kDC", &
                                                     "kDL", "kslt", "kEND", "kEOL", "kEXT", "kFND", "kHLP", "kHOM", &
                                                     "kIC", "kLFT", "kMSG", "kMOV", "kNXT", "kOPT", "kPRV", "kPRT", &
                                                     "kRDO", "kRPL", "kRIT", "kRES", "kSAV", "kSPD", "kUND", "rfi", &
                                                     "kf11", "kf12", "kf13", "kf14", "kf15", "kf16", "kf17", "kf18", &
                                                     "kf19", "kf20", "kf21", "kf22", "kf23", "kf24", "kf25", "kf26", &
                                                     "kf27", "kf28", "kf29", "kf30", "kf31", "kf32", "kf33", "kf34", &
                                                     "kf35", "kf36", "kf37", "kf38", "kf39", "kf40", "kf41", "kf42", &
                                                     "kf43", "kf44", "kf45", "kf46", "kf47", "kf48", "kf49", "kf50", &
                                                     "kf51", "kf52", "kf53", "kf54", "kf55", "kf56", "kf57", "kf58", &
                                                     "kf59", "kf60", "kf61", "kf62", "kf63", "el1", "mgc", "smgl", &
                                                     "smgr", "fln", "sclk", "dclk", "rmclk", "cwin", "wingo", "hup", &
                                                     "dial", "qdial", "tone", "pulse", "hook", "pause", "wait", "u0", &
                                                     "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", &
                                                     "u9", "op", "oc", "initc", "initp", "scp", "setf", "setb", &
                                                     "cpi", "lpi", "chr", "cvr", "defc", "swidm", "sdrfq", "sitm", &
                                                     "slm", "smicm", "snlq", "snrmq", "sshm", "ssubm", "ssupm", "sum", &
                                                     "rwidm", "ritm", "rlm", "rmicm", "rshm", "rsubm", "rsupm", "rum", &
                                                     "mhpa", "mcud1", "mcub1", "mcuf1", "mvpa", "mcuu1", "porder", "mcud", &
                                                     "mcub", "mcuf", "mcuu", "scs", "smgb", "smgbp", "smglp", "smgrp", &
                                                     "smgt", "smgtp", "sbim", "scsd", "rbim", "rcsd", "subcs", "supcs", &
                                                     "docr", "zerom", "csnm", "kmous", "minfo", "reqmp", "getm", "setaf", &
                                                     "setab", "pfxl", "devt", "csin", "s0ds", "s1ds", "s2ds", "s3ds", &
                                                     "smglr", "smgtb", "birep", "binel", "bicr", "colornm", "defbi", "endbi", &
                                                     "setcolor", "slines", "dispc", "smpch", "rmpch", "smsc", "rmsc", "pctrm", &
                                                     "scesc", "scesa", "ehhlm", "elhlm", "elohlm", "erhlm", "ethlm", "evhlm", &
                                                     "sgr1", "slength", "OTi2", "OTrs", "OTnl", "OTbc", "OTko", "OTma", &
                                                     "OTG2", "OTG3", "OTG1", "OTG4", "OTGR", "OTGL", "OTGU", "OTGD", &
                                                     "OTGH", "OTGV", "OTGC", "meml", "memu", "box1"]
    character(len=2), parameter :: STR_CODES(414) = [character(len=2) :: &
                                                     "bt", "bl", "cr", "cs", "ct", "cl", "ce", "cd", &
                                                     "ch", "CC", "cm", "do", "ho", "vi", "le", "CM", &
                                                     "ve", "nd", "ll", "up", "vs", "dc", "dl", "ds", &
                                                     "hd", "as", "mb", "md", "ti", "dm", "mh", "im", &
                                                     "mk", "mp", "mr", "so", "us", "ec", "ae", "me", &
                                                     "te", "ed", "ei", "se", "ue", "vb", "ff", "fs", &
                                                     "i1", "is", "i3", "if", "ic", "al", "ip", "kb", &
                                                     "ka", "kC", "kt", "kD", "kL", "kd", "kM", "kE", &
                                                     "kS", "k0", "k1", "k;", "k2", "k3", "k4", "k5", &
                                                     "k6", "k7", "k8", "k9", "kh", "kI", "kA", "kl", &
                                                     "kH", "kN", "kP", "kr", "kF", "kR", "kT", "ku", &
                                                     "ke", "ks", "l0", "l1", "la", "l2", "l3", "l4", &
                                                     "l5", "l6", "l7", "l8", "l9", "mo", "mm", "nw", &
                                                     "pc", "DC", "DL", "DO", "IC", "SF", "AL", "LE", &
                                                     "RI", "SR", "UP", "pk", "pl", "px", "ps", "pf", &
                                                     "po", "rp", "r1", "r2", "r3", "rf", "rc", "cv", &
                                                     "sc", "sf", "sr", "sa", "st", "wi", "ta", "ts", &
                                                     "uc", "hu", "iP", "K1", "K3", "K2", "K4", "K5", &
                                                     "pO", "rP", "ac", "pn", "kB", "SX", "RX", "SA", &
                                                     "RA", "XN", "XF", "eA", "LO", "LF", "@1", "@2", &
                                                     "@3", "@4", "@5", "@6", "@7", "@8", "@9", "@0", &
                                                     "%1", "%2", "%3", "%4", "%5", "%6", "%7", "%8", &
                                                     "%9", "%0", "&1", "&2", "&3", "&4", "&5", "&6", &
                                                     "&7", "&8", "&9", "&0", "*1", "*2", "*3", "*4", &
                                                     "*5", "*6", "*7", "*8", "*9", "*0", "#1", "#2", &
                                                     "#3", "#4", "%a", "%b", "%c", "%d", "%e", "%f", &
                                                     "%g", "%h", "%i", "%j", "!1", "!2", "!3", "RF", &
                                                     "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", &
                                                     "F9", "FA", "FB", "FC", "FD", "FE", "FF", "FG", &
                                                     "FH", "FI", "FJ", "FK", "FL", "FM", "FN", "FO", &
                                                     "FP", "FQ", "FR", "FS", "FT", "FU", "FV", "FW", &
                                                     "FX", "FY", "FZ", "Fa", "Fb", "Fc", "Fd", "Fe", &
                                                     "Ff", "Fg", "Fh", "Fi", "Fj", "Fk", "Fl", "Fm", &
                                                     "Fn", "Fo", "Fp", "Fq", "Fr", "cb", "MC", "ML", &
                                                     "MR", "Lf", "SC", "DK", "RC", "CW", "WG", "HU", &
                                                     "DI", "QD", "TO", "PU", "fh", "PA", "WA", "u0", &
                                                     "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", &
                                                     "u9", "op", "oc", "Ic", "Ip", "sp", "Sf", "Sb", &
                                                     "ZA", "ZB", "ZC", "ZD", "ZE", "ZF", "ZG", "ZH", &
                                                     "ZI", "ZJ", "ZK", "ZL", "ZM", "ZN", "ZO", "ZP", &
                                                     "ZQ", "ZR", "ZS", "ZT", "ZU", "ZV", "ZW", "ZX", &
                                                     "ZY", "ZZ", "Za", "Zb", "Zc", "Zd", "Ze", "Zf", &
                                                     "Zg", "Zh", "Zi", "Zj", "Zk", "Zl", "Zm", "Zn", &
                                                     "Zo", "Zp", "Zq", "Zr", "Zs", "Zt", "Zu", "Zv", &
                                                     "Zw", "Zx", "Zy", "Km", "Mi", "RQ", "Gm", "AF", &
                                                     "AB", "xl", "dv", "ci", "s0", "s1", "s2", "s3", &
                                                     "ML", "MT", "Xy", "Zz", "Yv", "Yw", "Yx", "Yy", &
                                                     "Yz", "YZ", "S1", "S2", "S3", "S4", "S5", "S6", &
                                                     "S7", "S8", "Xh", "Xl", "Xo", "Xr", "Xt", "Xv", &
                                                     "sA", "YI", "i2", "rs", "nl", "bc", "ko", "ma", &
                                                     "G2", "G3", "G1", "G4", "GR", "GL", "GU", "GD", &
                                                     "GH", "GV", "GC", "ml", "mu", "bx"]

    type :: cap_string
        character(len=:), allocatable :: s
    end type cap_string

    !> One terminal description.  Absent numbers are -1; absent strings are
    !> unallocated.
    type :: terminfo_type
        !> The names line, "xterm-256color|xterm with 256 colors".
        character(len=:), allocatable :: names
        logical :: flags(BOOLCOUNT) = .false.
        integer(int32) :: numbers(NUMCOUNT) = -1
        type(cap_string) :: strings(STRCOUNT)
        !> Extended capabilities: names and values.
        type(cap_string), allocatable :: ext_flag_names(:), ext_num_names(:), &
                                         ext_str_names(:), ext_strs(:)
        logical, allocatable :: ext_flags(:)
        integer(int32), allocatable :: ext_nums(:)
    end type terminfo_type

contains

    ! ------------------------------------------------------------------
    ! Finding and reading entries
    ! ------------------------------------------------------------------

    function getenv_str(name) result(v)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: v
        integer :: length, stat

        v = ""
        call get_environment_variable(name, length=length, status=stat)
        if (stat /= 0 .or. length == 0) return
        deallocate (v)
        allocate (character(len=length) :: v)
        call get_environment_variable(name, v, status=stat)
        if (stat /= 0) v = ""
    end function getenv_str

    logical function file_exists(path)
        character(len=*), intent(in) :: path

        inquire (file=path, exist=file_exists)
    end function file_exists

    !> The file holding entry `term` in directory `dir`, or "".
    function entry_path(dir, term) result(path)
        character(len=*), intent(in) :: dir, term
        character(len=:), allocatable :: path
        character(len=2) :: hex

        path = dir//"/"//term(1:1)//"/"//term
        if (file_exists(path)) return
        write (hex, "(z2.2)") iachar(term(1:1))
        path = dir//"/"//to_lower(hex)//"/"//term
        if (file_exists(path)) return
        path = ""
    end function entry_path

    function to_lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: k

        t = s
        do k = 1, len(s)
            if (s(k:k) >= "A" .and. s(k:k) <= "Z") t(k:k) = achar(iachar(s(k:k)) + 32)
        end do
    end function to_lower

    !> Where entry `term` lives, searching as ncurses does; "" if nowhere.
    function find_entry(term) result(path)
        character(len=*), intent(in) :: term
        character(len=:), allocatable :: path, dirs, home, dir
        character(len=*), parameter :: SYSTEM_DIRS = "/etc/terminfo:/lib/terminfo:/usr/share/terminfo"
        integer :: k

        path = ""
        dir = getenv_str("TERMINFO")
        if (len(dir) > 0) then
            path = entry_path(dir, term)
            if (len(path) > 0) return
        end if
        home = getenv_str("HOME")
        if (len(home) > 0) then
            path = entry_path(home//"/.terminfo", term)
            if (len(path) > 0) return
        end if
        dirs = getenv_str("TERMINFO_DIRS")
        if (len(dirs) == 0) then
            dirs = SYSTEM_DIRS
        else
            dirs = dirs//":"
        end if
        do while (len(dirs) > 0)
            k = index(dirs, ":")
            if (k == 0) k = len(dirs) + 1
            dir = dirs(1:k - 1)
            if (k > len(dirs)) then
                dirs = ""
            else
                dirs = dirs(k + 1:)
            end if
            if (len(dir) == 0) then
                ! An empty element stands for the system directories.
                if (index(dirs, SYSTEM_DIRS) /= 1) dirs = SYSTEM_DIRS//":"//dirs
                cycle
            end if
            path = entry_path(dir, term)
            if (len(path) > 0) return
        end do
    end function find_entry

    !> Load the description of terminal type `term`.  Port of the tgetent
    !> call in term.c:init_tty (the library side is reimplemented); the
    !> error text is term.c's.
    subroutine tgetent(term, ti, status)
        character(len=*), intent(in) :: term
        type(terminfo_type), intent(out) :: ti
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: path

        if (len(term) == 0 .or. index(term, "/") > 0) then
            call set_error(status, FM_GENERIC_ERROR, &
                           "Terminal type "//term//" is not defined")
            return
        end if
        path = find_entry(term)
        if (len(path) > 0) then
            call read_terminfo(path, ti, status)
            return
        end if
        if (index(term, "xterm") == 1) then
            call builtin_xterm(ti)
            return
        end if
        call set_error(status, FM_GENERIC_ERROR, "Terminal type "//term//" is not defined")
    end subroutine tgetent

    !> Load the compiled entry in file `path`.
    subroutine read_terminfo(path, ti, status)
        character(len=*), intent(in) :: path
        type(terminfo_type), intent(out) :: ti
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: data
        integer :: unit, ios
        integer(int64) :: size

        inquire (file=path, size=size)
        if (size <= 0) then
            call set_error(status, FM_FILE_ERROR, "Cannot read terminfo entry: "//path)
            return
        end if
        allocate (character(len=size) :: data)
        open (newunit=unit, file=path, access="stream", form="unformatted", &
              action="read", status="old", iostat=ios)
        if (ios /= 0) then
            call set_error(status, FM_FILE_ERROR, "Cannot read terminfo entry: "//path)
            return
        end if
        read (unit, iostat=ios) data
        close (unit)
        if (ios /= 0) then
            call set_error(status, FM_FILE_ERROR, "Cannot read terminfo entry: "//path)
            return
        end if
        call parse_terminfo(data, ti, status)
    end subroutine read_terminfo

    !> Little-endian signed 16-bit integer at 0-based offset `off`.
    integer(int32) function get16(data, off) result(v)
        character(len=*), intent(in) :: data
        integer(int64), intent(in) :: off

        v = ichar(data(off + 1:off + 1), int32) + 256*ichar(data(off + 2:off + 2), int32)
        if (v >= 32768) v = v - 65536
    end function get16

    !> Little-endian signed 32-bit integer at 0-based offset `off`.
    integer(int32) function get32(data, off) result(v)
        character(len=*), intent(in) :: data
        integer(int64), intent(in) :: off
        integer(int64) :: u
        integer :: k

        u = 0
        do k = 3, 0, -1
            u = 256*u + ichar(data(off + k + 1:off + k + 1), int64)
        end do
        if (u >= 2_int64**31) u = u - 2_int64**32
        v = int(u, int32)
    end function get32

    !> The NUL-terminated string at 0-based `off` within `table`.
    function cstring(table, off) result(s)
        character(len=*), intent(in) :: table
        integer(int64), intent(in) :: off
        character(len=:), allocatable :: s
        integer :: e

        e = index(table(off + 1:), achar(0))
        if (e == 0) then
            s = table(off + 1:)
        else
            s = table(off + 1:off + e - 1)
        end if
    end function cstring

    !> Decode a compiled terminfo entry (term(5)).
    subroutine parse_terminfo(data, ti, status)
        character(len=*), intent(in) :: data
        type(terminfo_type), intent(out) :: ti
        type(status_type), intent(out) :: status
        integer(int32) :: magic_number, names_size, nbools, nnums, nstrs, tab_size, &
                          ebools, enums, estrs, eitems, etab_size, o
        integer(int64) :: off, n, k, numsize, tab, names_start, last
        character(len=:), allocatable :: table

        n = len(data, kind=int64)
        if (n < 12) then
            call bad(status)
            return
        end if
        magic_number = get16(data, 0_int64)
        if (magic_number == MAGIC) then
            numsize = 2
        else if (magic_number == MAGIC2) then
            numsize = 4
        else
            call bad(status)
            return
        end if
        names_size = get16(data, 2_int64)
        nbools = get16(data, 4_int64)
        nnums = get16(data, 6_int64)
        nstrs = get16(data, 8_int64)
        tab_size = get16(data, 10_int64)
        if (names_size < 0 .or. nbools < 0 .or. nnums < 0 .or. nstrs < 0 .or. tab_size < 0) then
            call bad(status)
            return
        end if
        off = 12
        if (off + names_size + nbools + 1 + numsize*nnums + 2*nstrs + tab_size > n + 1) then
            call bad(status)
            return
        end if
        ti%names = cstring(data(off + 1:off + names_size), 0_int64)
        off = off + names_size
        do k = 1, min(int(nbools, int64), int(BOOLCOUNT, int64))
            ti%flags(k) = ichar(data(off + k:off + k)) == 1
        end do
        off = off + nbools
        if (mod(off, 2_int64) == 1) off = off + 1
        do k = 1, nnums
            if (k <= NUMCOUNT) then
                if (numsize == 2) then
                    ti%numbers(k) = get16(data, off)
                else
                    ti%numbers(k) = get32(data, off)
                end if
                if (ti%numbers(k) < 0) ti%numbers(k) = -1
            end if
            off = off + numsize
        end do
        tab = off + 2*nstrs
        if (tab + tab_size > n) then
            call bad(status)
            return
        end if
        table = data(tab + 1:tab + tab_size)
        do k = 1, nstrs
            o = get16(data, off + 2*(k - 1))
            if (k <= STRCOUNT .and. o >= 0 .and. o < tab_size) then
                ti%strings(k)%s = cstring(table, int(o, int64))
            end if
        end do
        off = tab + tab_size

        ! The extended section (ncurses), if present.
        allocate (ti%ext_flag_names(0), ti%ext_num_names(0), ti%ext_str_names(0), &
                  ti%ext_strs(0), ti%ext_flags(0), ti%ext_nums(0))
        if (mod(off, 2_int64) == 1) off = off + 1
        if (off + 10 > n) return
        ebools = get16(data, off)
        enums = get16(data, off + 2)
        estrs = get16(data, off + 4)
        eitems = get16(data, off + 6)
        etab_size = get16(data, off + 8)
        if (ebools < 0 .or. enums < 0 .or. estrs < 0 .or. etab_size < 0 .or. &
            eitems /= estrs + ebools + enums + estrs) return
        off = off + 10
        deallocate (ti%ext_flag_names, ti%ext_num_names, ti%ext_str_names, &
                    ti%ext_strs, ti%ext_flags, ti%ext_nums)
        allocate (ti%ext_flag_names(ebools), ti%ext_num_names(enums), &
                  ti%ext_str_names(estrs), ti%ext_strs(estrs), ti%ext_flags(ebools), &
                  ti%ext_nums(enums))
        if (off + ebools > n) then
            call bad(status)
            return
        end if
        do k = 1, ebools
            ti%ext_flags(k) = ichar(data(off + k:off + k)) == 1
        end do
        off = off + ebools
        if (mod(off, 2_int64) == 1) off = off + 1
        if (off + numsize*enums + 2*eitems + etab_size > n) then
            call bad(status)
            return
        end if
        do k = 1, enums
            if (numsize == 2) then
                ti%ext_nums(k) = get16(data, off)
            else
                ti%ext_nums(k) = get32(data, off)
            end if
            if (ti%ext_nums(k) < 0) ti%ext_nums(k) = -1
            off = off + numsize
        end do
        tab = off + 2*eitems
        table = data(tab + 1:tab + etab_size)
        ! Values first; the names follow the last value string.
        names_start = 0
        do k = 1, estrs
            o = get16(data, off + 2*(k - 1))
            if (o >= 0 .and. o < etab_size) then
                ti%ext_strs(k)%s = cstring(table, int(o, int64))
                last = o + len(ti%ext_strs(k)%s) + 1
                names_start = max(names_start, last)
            end if
        end do
        do k = 1, ebools + enums + estrs
            o = get16(data, off + 2*(estrs + k - 1))
            if (o < 0 .or. names_start + o >= etab_size) cycle
            if (k <= ebools) then
                ti%ext_flag_names(k)%s = cstring(table, names_start + o)
            else if (k <= ebools + enums) then
                ti%ext_num_names(k - ebools)%s = cstring(table, names_start + o)
            else
                ti%ext_str_names(k - ebools - enums)%s = cstring(table, names_start + o)
            end if
        end do
    contains
        subroutine bad(status)
            type(status_type), intent(inout) :: status

            call set_error(status, FM_GENERIC_ERROR, "Invalid terminfo entry")
        end subroutine bad
    end subroutine parse_terminfo

    ! ------------------------------------------------------------------
    ! Capabilities by name
    ! ------------------------------------------------------------------

    integer function find_name(table, name) result(k)
        character(len=*), intent(in) :: table(:), name

        do k = 1, size(table)
            if (table(k) == name) return
        end do
        k = 0
    end function find_name

    integer function find_ext(names, name) result(k)
        type(cap_string), intent(in) :: names(:)
        character(len=*), intent(in) :: name

        do k = 1, size(names)
            if (.not. allocated(names(k)%s)) cycle
            if (names(k)%s == name) return
        end do
        k = 0
    end function find_ext

    !> Boolean capability `name` (terminfo name).  Port of the terminfo
    !> library's tigetflag, as term.c uses it.
    logical function tigetflag(ti, name) result(v)
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: name
        integer :: k

        v = .false.
        k = find_name(BOOL_NAMES, name)
        if (k > 0) then
            v = ti%flags(k)
        else if (allocated(ti%ext_flag_names)) then
            k = find_ext(ti%ext_flag_names, name)
            if (k > 0) v = ti%ext_flags(k)
        end if
    end function tigetflag

    !> Numeric capability `name`, or -1.
    integer(int32) function tigetnum(ti, name) result(v)
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: name
        integer :: k

        v = -1
        k = find_name(NUM_NAMES, name)
        if (k > 0) then
            v = ti%numbers(k)
        else if (allocated(ti%ext_num_names)) then
            k = find_ext(ti%ext_num_names, name)
            if (k > 0) v = ti%ext_nums(k)
        end if
    end function tigetnum

    !> String capability `name`; "" (and `found` false) if absent.
    function tigetstr(ti, name, found) result(s)
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: name
        logical, intent(out), optional :: found
        character(len=:), allocatable :: s
        integer :: k

        s = ""
        if (present(found)) found = .false.
        k = find_name(STR_NAMES, name)
        if (k > 0) then
            if (.not. allocated(ti%strings(k)%s)) return
            s = ti%strings(k)%s
        else
            if (.not. allocated(ti%ext_str_names)) return
            k = find_ext(ti%ext_str_names, name)
            if (k == 0) return
            if (.not. allocated(ti%ext_strs(k)%s)) return
            s = ti%ext_strs(k)%s
        end if
        if (present(found)) found = .true.
    end function tigetstr

    !> Boolean capability by termcap code ("am"), falling back to an
    !> extended capability of that name.
    logical function tgetflag(ti, code) result(v)
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: code
        integer :: k

        k = 0
        if (len(code) == 2) k = find_name(BOOL_CODES, code)
        if (k > 0) then
            v = ti%flags(k)
        else
            v = tigetflag(ti, code)
        end if
    end function tgetflag

    !> Numeric capability by termcap code ("co"), or -1.
    integer(int32) function tgetnum(ti, code) result(v)
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: code
        integer :: k

        k = 0
        if (len(code) == 2) k = find_name(NUM_CODES, code)
        if (k > 0) then
            v = ti%numbers(k)
        else
            v = tigetnum(ti, code)
        end if
    end function tgetnum

    !> String capability by termcap code ("cm"); "" if absent.
    function tgetstr(ti, code, found) result(s)
        type(terminfo_type), intent(in) :: ti
        character(len=*), intent(in) :: code
        logical, intent(out), optional :: found
        character(len=:), allocatable :: s
        integer :: k

        k = 0
        if (len(code) == 2) k = find_name(STR_CODES, code)
        if (k > 0) then
            s = tigetstr(ti, trim(STR_NAMES(k)), found)
        else
            s = tigetstr(ti, code, found)
        end if
    end function tgetstr

    ! ------------------------------------------------------------------
    ! The built-in fallback
    ! ------------------------------------------------------------------

    subroutine set_flag(ti, name)
        type(terminfo_type), intent(inout) :: ti
        character(len=*), intent(in) :: name
        integer :: k

        k = find_name(BOOL_NAMES, name)
        if (k > 0) ti%flags(k) = .true.
    end subroutine set_flag

    subroutine set_num(ti, name, v)
        type(terminfo_type), intent(inout) :: ti
        character(len=*), intent(in) :: name
        integer, intent(in) :: v
        integer :: k

        k = find_name(NUM_NAMES, name)
        if (k > 0) ti%numbers(k) = v
    end subroutine set_num

    subroutine set_str(ti, name, v)
        type(terminfo_type), intent(inout) :: ti
        character(len=*), intent(in) :: name, v
        type(cap_string), allocatable :: names(:), values(:)
        integer :: k, n

        k = find_name(STR_NAMES, name)
        if (k > 0) then
            ti%strings(k)%s = v
            return
        end if
        n = size(ti%ext_str_names)
        allocate (names(n + 1), values(n + 1))
        names(1:n) = ti%ext_str_names
        values(1:n) = ti%ext_strs
        names(n + 1)%s = name
        values(n + 1)%s = v
        call move_alloc(names, ti%ext_str_names)
        call move_alloc(values, ti%ext_strs)
    end subroutine set_str

    !> A built-in xterm-256color description: the capabilities term.c uses,
    !> plus cursor and function keys (generated from ncurses 6.4's entry).
    subroutine builtin_xterm(ti)
        type(terminfo_type), intent(out) :: ti

        ti%names = "xterm-256color|xterm with 256 colors (built in)"
        allocate (ti%ext_flag_names(0), ti%ext_num_names(0), ti%ext_str_names(0), &
                  ti%ext_strs(0), ti%ext_flags(0), ti%ext_nums(0))
        call set_flag(ti, "OTbs")
        call set_flag(ti, "am")
        call set_flag(ti, "km")
        call set_flag(ti, "mir")
        call set_flag(ti, "msgr")
        call set_flag(ti, "xenl")
        call set_num(ti, "colors", 256)
        call set_num(ti, "cols", 80)
        call set_num(ti, "lines", 24)
        call set_num(ti, "pairs", 65536)
        call set_str(ti, "bel", achar(7))
        call set_str(ti, "bold", achar(27)//"[1m")
        call set_str(ti, "cbt", achar(27)//"[Z")
        call set_str(ti, "civis", achar(27)//"[?25l")
        call set_str(ti, "clear", achar(27)//"[H"//achar(27)//"[2J")
        call set_str(ti, "cnorm", achar(27)//"[?12l"//achar(27)//"[?25h")
        call set_str(ti, "cr", achar(13))
        call set_str(ti, "csr", achar(27)//"[%i%p1%d;%p2%dr")
        call set_str(ti, "cub", achar(27)//"[%p1%dD")
        call set_str(ti, "cub1", achar(8))
        call set_str(ti, "cud", achar(27)//"[%p1%dB")
        call set_str(ti, "cud1", achar(10))
        call set_str(ti, "cuf", achar(27)//"[%p1%dC")
        call set_str(ti, "cuf1", achar(27)//"[C")
        call set_str(ti, "cup", achar(27)//"[%i%p1%d;%p2%dH")
        call set_str(ti, "cuu", achar(27)//"[%p1%dA")
        call set_str(ti, "cuu1", achar(27)//"[A")
        call set_str(ti, "cvvis", achar(27)//"[?12;25h")
        call set_str(ti, "dch", achar(27)//"[%p1%dP")
        call set_str(ti, "dch1", achar(27)//"[P")
        call set_str(ti, "dim", achar(27)//"[2m")
        call set_str(ti, "dl", achar(27)//"[%p1%dM")
        call set_str(ti, "dl1", achar(27)//"[M")
        call set_str(ti, "ed", achar(27)//"[J")
        call set_str(ti, "el", achar(27)//"[K")
        call set_str(ti, "flash", achar(27)//"[?5h$<100/>"//achar(27)//"[?5l")
        call set_str(ti, "home", achar(27)//"[H")
        call set_str(ti, "hpa", achar(27)//"[%i%p1%dG")
        call set_str(ti, "ht", achar(9))
        call set_str(ti, "ich", achar(27)//"[%p1%d@")
        call set_str(ti, "il", achar(27)//"[%p1%dL")
        call set_str(ti, "il1", achar(27)//"[L")
        call set_str(ti, "ind", achar(10))
        call set_str(ti, "kbs", achar(127))
        call set_str(ti, "kcbt", achar(27)//"[Z")
        call set_str(ti, "kcub1", achar(27)//"OD")
        call set_str(ti, "kcud1", achar(27)//"OB")
        call set_str(ti, "kcuf1", achar(27)//"OC")
        call set_str(ti, "kcuu1", achar(27)//"OA")
        call set_str(ti, "kdch1", achar(27)//"[3~")
        call set_str(ti, "kend", achar(27)//"OF")
        call set_str(ti, "kf1", achar(27)//"OP")
        call set_str(ti, "kf10", achar(27)//"[21~")
        call set_str(ti, "kf11", achar(27)//"[23~")
        call set_str(ti, "kf12", achar(27)//"[24~")
        call set_str(ti, "kf2", achar(27)//"OQ")
        call set_str(ti, "kf3", achar(27)//"OR")
        call set_str(ti, "kf4", achar(27)//"OS")
        call set_str(ti, "kf5", achar(27)//"[15~")
        call set_str(ti, "kf6", achar(27)//"[17~")
        call set_str(ti, "kf7", achar(27)//"[18~")
        call set_str(ti, "kf8", achar(27)//"[19~")
        call set_str(ti, "kf9", achar(27)//"[20~")
        call set_str(ti, "khome", achar(27)//"OH")
        call set_str(ti, "kich1", achar(27)//"[2~")
        call set_str(ti, "knp", achar(27)//"[6~")
        call set_str(ti, "kpp", achar(27)//"[5~")
        call set_str(ti, "op", achar(27)//"[39;49m")
        call set_str(ti, "rep", "%p1%c"//achar(27)//"[%p2%{1}%-%db")
        call set_str(ti, "rev", achar(27)//"[7m")
        call set_str(ti, "ri", achar(27)//"M")
        call set_str(ti, "rmacs", achar(27)//"(B")
        call set_str(ti, "rmcup", achar(27)//"[?1049l"//achar(27)//"[23;0;0t")
        call set_str(ti, "rmir", achar(27)//"[4l")
        call set_str(ti, "rmkx", achar(27)//"[?1l"//achar(27)//">")
        call set_str(ti, "rmso", achar(27)//"[27m")
        call set_str(ti, "rmul", achar(27)//"[24m")
        call set_str(ti, "setab", achar(27)//"[%?%p1%{8}%<%t4%p1%d%e%p1%{16}%<%t10%p1%{8}%-%d%e48;5;%p1%d%;m")
        call set_str(ti, "setaf", achar(27)//"[%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m")
        call set_str(ti, "sgr0", achar(27)//"(B"//achar(27)//"[m")
        call set_str(ti, "sitm", achar(27)//"[3m")
        call set_str(ti, "smacs", achar(27)//"(0")
        call set_str(ti, "smcup", achar(27)//"[?1049h"//achar(27)//"[22;0;0t")
        call set_str(ti, "smir", achar(27)//"[4h")
        call set_str(ti, "smkx", achar(27)//"[?1h"//achar(27)//"=")
        call set_str(ti, "smso", achar(27)//"[7m")
        call set_str(ti, "smul", achar(27)//"[4m")
        call set_str(ti, "vpa", achar(27)//"[%i%p1%dd")
        call set_str(ti, "rmxx", achar(27)//"[29m")
        call set_str(ti, "smxx", achar(27)//"[9m")
    end subroutine builtin_xterm

    ! ------------------------------------------------------------------
    ! Parameterized strings
    ! ------------------------------------------------------------------

    !> `n` in base `base` (2..16), at least `mindigits` digits.
    function int_digits(n, base, upper, mindigits) result(s)
        integer(int64), intent(in) :: n
        integer, intent(in) :: base, mindigits
        logical, intent(in) :: upper
        character(len=:), allocatable :: s
        character(len=16), parameter :: lower_digits = "0123456789abcdef", &
                                        upper_digits = "0123456789ABCDEF"
        integer(int64) :: m
        integer :: d

        s = ""
        m = n
        do while (m > 0)
            d = int(mod(m, int(base, int64)))
            if (upper) then
                s = upper_digits(d + 1:d + 1)//s
            else
                s = lower_digits(d + 1:d + 1)//s
            end if
            m = m/base
        end do
        if (len(s) < mindigits) s = repeat("0", mindigits - len(s))//s
    end function int_digits

    !> printf-style formatting of `v` for conversion `conv` (d o x X) with
    !> `flags` (- + # space), `width` and `precision` (-1: none), and
    !> zero padding.
    function format_int(v, conv, flags, width, precision, zero) result(s)
        integer(int64), intent(in) :: v
        character, intent(in) :: conv
        character(len=*), intent(in) :: flags
        integer, intent(in) :: width, precision
        logical, intent(in) :: zero
        character(len=:), allocatable :: s, sign, digits, prefix
        integer(int64) :: u
        integer :: mindigits

        sign = ""
        prefix = ""
        mindigits = 1
        if (precision >= 0) mindigits = precision
        select case (conv)
        case ("d")
            u = abs(v)
            if (v < 0) then
                sign = "-"
            else if (index(flags, "+") > 0) then
                sign = "+"
            else if (index(flags, " ") > 0) then
                sign = " "
            end if
            digits = int_digits(u, 10, .false., mindigits)
        case default
            ! Unsigned conversions see the value as a C unsigned int.
            u = iand(v, 4294967295_int64)
            select case (conv)
            case ("o")
                digits = int_digits(u, 8, .false., mindigits)
                if (index(flags, "#") > 0 .and. digits(1:1) /= "0") digits = "0"//digits
            case ("x")
                digits = int_digits(u, 16, .false., mindigits)
                if (index(flags, "#") > 0 .and. u /= 0) prefix = "0x"
            case default
                digits = int_digits(u, 16, .true., mindigits)
                if (index(flags, "#") > 0 .and. u /= 0) prefix = "0X"
            end select
        end select
        if (precision == 0 .and. v == 0) digits = ""
        s = sign//prefix//digits
        if (len(s) < width) then
            if (index(flags, "-") > 0) then
                s = s//repeat(" ", width - len(s))
            else if (zero .and. precision < 0) then
                s = sign//prefix//repeat("0", width - len(s))//digits
            else
                s = repeat(" ", width - len(s))//s
            end if
        end if
    end function format_int

    !> Instantiate parameterized string `str` with integer parameters
    !> `p1`..`p9` (absent ones are 0).  The terminfo(5) language: %p %d %c
    !> %{n} %'c' %P %g %+ %- %* %/ %m %& %| %^ %= %< %> %A %O %! %~ %i
    !> %? %t %e %; and printf conversions %[[:]flags][width][.prec][doxX].
    !> Reimplements the terminfo library's tparm, which terminfo.c calls.
    function tparm(str, p1, p2, p3, p4, p5, p6, p7, p8, p9) result(out)
        character(len=*), intent(in) :: str
        integer(int64), intent(in), optional :: p1, p2, p3, p4, p5, p6, p7, p8, p9
        character(len=:), allocatable :: out
        integer(int64) :: params(9), stack(64), dynamic(26), static_vars(26), a, b, v
        integer :: i, n, sp, width, precision
        character :: c
        character(len=:), allocatable :: flags
        logical :: zero

        params = 0
        if (present(p1)) params(1) = p1
        if (present(p2)) params(2) = p2
        if (present(p3)) params(3) = p3
        if (present(p4)) params(4) = p4
        if (present(p5)) params(5) = p5
        if (present(p6)) params(6) = p6
        if (present(p7)) params(7) = p7
        if (present(p8)) params(8) = p8
        if (present(p9)) params(9) = p9
        dynamic = 0
        static_vars = 0
        sp = 0
        out = ""
        n = len(str)
        i = 1
        do while (i <= n)
            c = str(i:i)
            i = i + 1
            if (c /= "%") then
                out = out//c
                cycle
            end if
            if (i > n) exit
            c = str(i:i)
            i = i + 1
            select case (c)
            case ("%")
                out = out//"%"
            case ("c")
                v = pop()
                ! A NUL can't be sent through a C string; terminfo sends 0200.
                if (iand(v, 255_int64) == 0) then
                    out = out//char(128)
                else
                    out = out//char(int(iand(v, 255_int64)))
                end if
            case ("s")
                v = pop()
            case ("l")
                v = pop()
                call push(0_int64)
            case ("p")
                if (i <= n) then
                    v = iachar(str(i:i)) - iachar("0")
                    if (v >= 1 .and. v <= 9) call push(params(v))
                    i = i + 1
                end if
            case ("P")
                if (i <= n) then
                    call set_var(str(i:i), pop())
                    i = i + 1
                end if
            case ("g")
                if (i <= n) then
                    call push(get_var(str(i:i)))
                    i = i + 1
                end if
            case ("'")
                if (i <= n) call push(int(iachar(str(i:i)), int64))
                i = i + 2
            case ("{")
                v = 0
                do while (i <= n)
                    if (str(i:i) == "}") exit
                    v = 10*v + iachar(str(i:i)) - iachar("0")
                    i = i + 1
                end do
                i = i + 1
                call push(v)
            case ("+", "-", "*", "/", "m", "&", "|", "^", "=", "<", ">", "A", "O")
                b = pop()
                a = pop()
                call push(binary(c, a, b))
            case ("!")
                call push(merge(1_int64, 0_int64, pop() == 0))
            case ("~")
                call push(not(pop()))
            case ("i")
                params(1) = params(1) + 1
                params(2) = params(2) + 1
            case ("?", ";")
            case ("t")
                if (pop() == 0) call skip(.true.)
            case ("e")
                call skip(.false.)
            case default
                ! %[[:]flags][width[.precision]][doxX]
                i = i - 1
                if (str(i:i) == ":") i = i + 1
                flags = ""
                do while (i <= n)
                    if (index("-+# ", str(i:i)) == 0) exit
                    flags = flags//str(i:i)
                    i = i + 1
                end do
                zero = .false.
                if (i <= n) zero = str(i:i) == "0"
                width = read_int()
                precision = -1
                if (i <= n) then
                    if (str(i:i) == ".") then
                        i = i + 1
                        precision = read_int()
                    end if
                end if
                if (i > n) exit
                c = str(i:i)
                i = i + 1
                if (index("doxX", c) > 0) then
                    out = out//format_int(pop(), c, flags, width, precision, zero)
                else if (c == "s") then
                    v = pop()
                end if
            end select
        end do
    contains
        subroutine push(x)
            integer(int64), intent(in) :: x

            if (sp < size(stack)) then
                sp = sp + 1
                stack(sp) = x
            end if
        end subroutine push

        integer(int64) function pop() result(x)
            x = 0
            if (sp > 0) then
                x = stack(sp)
                sp = sp - 1
            end if
        end function pop

        subroutine set_var(name, x)
            character, intent(in) :: name
            integer(int64), intent(in) :: x

            if (name >= "a" .and. name <= "z") then
                dynamic(iachar(name) - iachar("a") + 1) = x
            else if (name >= "A" .and. name <= "Z") then
                static_vars(iachar(name) - iachar("A") + 1) = x
            end if
        end subroutine set_var

        integer(int64) function get_var(name) result(x)
            character, intent(in) :: name

            x = 0
            if (name >= "a" .and. name <= "z") then
                x = dynamic(iachar(name) - iachar("a") + 1)
            else if (name >= "A" .and. name <= "Z") then
                x = static_vars(iachar(name) - iachar("A") + 1)
            end if
        end function get_var

        integer function read_int() result(x)
            x = 0
            do while (i <= n)
                if (str(i:i) < "0" .or. str(i:i) > "9") exit
                x = 10*x + iachar(str(i:i)) - iachar("0")
                i = i + 1
            end do
        end function read_int

        !> Skip to after the matching %e (if `to_else`) or %;.
        subroutine skip(to_else)
            logical, intent(in) :: to_else
            integer :: level

            level = 0
            do while (i < n)
                if (str(i:i) /= "%") then
                    i = i + 1
                    cycle
                end if
                select case (str(i + 1:i + 1))
                case ("?")
                    level = level + 1
                case (";")
                    if (level == 0) then
                        i = i + 2
                        return
                    end if
                    level = level - 1
                case ("e")
                    if (level == 0 .and. to_else) then
                        i = i + 2
                        return
                    end if
                case default
                    ! Nothing to do for other values.
                end select
                i = i + 2
            end do
            i = n + 1
        end subroutine skip
    end function tparm

    integer(int64) function binary(op, a, b) result(v)
        character, intent(in) :: op
        integer(int64), intent(in) :: a, b

        select case (op)
        case ("+")
            v = a + b
        case ("-")
            v = a - b
        case ("*")
            v = a*b
        case ("/")
            v = 0
            if (b /= 0) v = a/b
        case ("m")
            v = 0
            if (b /= 0) v = mod(a, b)
        case ("&")
            v = iand(a, b)
        case ("|")
            v = ior(a, b)
        case ("^")
            v = ieor(a, b)
        case ("=")
            v = merge(1_int64, 0_int64, a == b)
        case ("<")
            v = merge(1_int64, 0_int64, a < b)
        case (">")
            v = merge(1_int64, 0_int64, a > b)
        case ("A")
            v = merge(1_int64, 0_int64, a /= 0 .and. b /= 0)
        case default
            v = merge(1_int64, 0_int64, a /= 0 .or. b /= 0)
        end select
    end function binary

    !> Port of terminfo.c:tparam: tparm with up to four arguments.
    function tparam(str, arg1, arg2, arg3, arg4) result(out)
        character(len=*), intent(in) :: str
        integer(int64), intent(in) :: arg1, arg2, arg3, arg4
        character(len=:), allocatable :: out

        out = tparm(str, arg1, arg2, arg3, arg4)
    end function tparam

    !> Cursor motion to `col`, `row` (0-based): the terminfo library's
    !> tgoto, which takes its arguments column first.
    function tgoto(cm, col, row) result(out)
        character(len=*), intent(in) :: cm
        integer(int64), intent(in) :: col, row

        character(len=:), allocatable :: out

        out = tparm(cm, row, col)
    end function tgoto

    !> `s` without $<...> padding specifications.  Modern terminals need no
    !> padding (the terminfo library's tputs would delay instead).
    function delete_padding(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i, e

        t = ""
        i = 1
        do while (i <= len(s))
            if (i < len(s)) then
                if (s(i:i + 1) == "$<") then
                    e = index(s(i:), ">")
                    if (e > 0) then
                        i = i + e
                        cycle
                    end if
                end if
            end if
            t = t//s(i:i)
            i = i + 1
        end do
    end function delete_padding

end module terminfo_fm
