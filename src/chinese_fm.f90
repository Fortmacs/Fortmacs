! chinese_fm.f90 -- the chinese coding systems of type `charset', `shift-jis' and `big5'.
! Ported from GNU Emacs 31.1 lisp/language/chinese.el.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
! Copyright (C) 2003
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> The coding systems of type `charset', `shift-jis' and `big5' that chinese.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) chinese_fm
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of chinese.el.
    module subroutine syms_of_chinese()
        call define_charset_coding_system("chinese-big5", iachar("B"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "big5"], &
                                          mime_charset="big5", &
                                          doc="BIG5 8-bit encoding for Chinese (MIME:Big5)", &
                                          coding_type="big5")
        call define_coding_system_alias("big5", "chinese-big5")
        call define_coding_system_alias("cn-big5", "chinese-big5")
        call define_coding_system_alias("cp950", "chinese-big5")
        call define_charset_coding_system("chinese-big5-hkscs", iachar("B"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "big5-hkscs"], &
                                          mime_charset="big5-hkscs", &
                                          doc="BIG5-HKSCS 8-bit encoding for Chinese, Hong Kong supplement " &
                                          //"(MIME:Big5-HKSCS)")
        call define_coding_system_alias("big5-hkscs", "chinese-big5-hkscs")
        call define_coding_system_alias("cn-big5-hkscs", "chinese-big5-hkscs")
        call define_charset_coding_system("chinese-gbk", iachar("c"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-gbk"], &
                                          mime_charset="gbk", &
                                          doc="GBK encoding for Chinese (MIME:GBK).")
        call define_coding_system_alias("gbk", "chinese-gbk")
        call define_coding_system_alias("cp936", "chinese-gbk")
        call define_coding_system_alias("windows-936", "chinese-gbk")
        call define_charset_coding_system("chinese-gb18030", iachar("c"), &
                                     charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "gb18030-2-byte", "gb18030-4-byte-bmp", &
                                                    "gb18030-4-byte-smp", "gb18030-4-byte-ext-1", "gb18030-4-byte-ext-2"], &
                                          mime_charset="gb18030", &
                                          doc="GB18030 encoding for Chinese (MIME:GB18030).")
        call define_coding_system_alias("gb18030", "chinese-gb18030")
    end subroutine syms_of_chinese
end submodule chinese_fm
