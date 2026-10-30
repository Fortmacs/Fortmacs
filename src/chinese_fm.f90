! chinese_fm.f90 -- the chinese coding systems of type `charset', `shift-jis', `big5' and `iso-2022'.
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

!> The coding systems of type `charset', `shift-jis', `big5' and `iso-2022' that chinese.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) chinese_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of chinese.el.
    module subroutine syms_of_chinese()
        call define_iso_2022_coding_system("iso-2022-cn", iachar("C"), &
                                           doc="ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:IS" &
                                           //"O-2022-CN).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                  request_charsets=[character(len=CHARSET_NAME_LEN) :: "chinese-cns11643-2", "chinese-cns11643-1", &
                                                             "chinese-gb2312", "ascii"], &
                                           request_registers=[integer :: 2, 1, 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) :: "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "locking-shift", "single-shift", "init-at-bol"], &
                                     charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-gb2312", "chinese-cns11643-1", &
                                                     "chinese-cns11643-2"], &
                                           mime_charset="iso-2022-cn")
        call define_coding_system_alias("chinese-iso-7bit", "iso-2022-cn")
        call define_iso_2022_coding_system("iso-2022-cn-ext", iachar("C"), &
                                           doc="ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:IS" &
                                           //"O-2022-CN-EXT).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                  request_charsets=[character(len=CHARSET_NAME_LEN) :: "chinese-cns11643-7", "chinese-cns11643-6", &
                                           "chinese-cns11643-5", "chinese-cns11643-4", "chinese-cns11643-3", "chinese-cns11643-2", &
                                                             "chinese-cns11643-1", "chinese-gb2312", "ascii"], &
                                           request_registers=[integer :: 3, 3, 3, 3, 3, 2, 1, 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) :: "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "locking-shift", "single-shift", "init-at-bol"], &
                                     charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-gb2312", "chinese-cns11643-1", &
                                           "chinese-cns11643-2", "chinese-cns11643-3", "chinese-cns11643-4", "chinese-cns11643-5", &
                                                     "chinese-cns11643-6", "chinese-cns11643-7"], &
                                           mime_charset="iso-2022-cn-ext")
        call define_iso_2022_coding_system("chinese-iso-8bit", iachar("c"), &
                                           doc="ISO 2022 based EUC encoding for Chinese GB2312 (MIME:GB2312)" &
                                           //".", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-gb2312", "", ""], &
                                           request_charsets=[character(len=CHARSET_NAME_LEN) :: "chinese-gb2312", "ascii"], &
                                           request_registers=[integer :: 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) ::], &
                                           charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-gb2312"], &
                                           mime_charset="gb2312")
        call define_coding_system_alias("cn-gb-2312", "chinese-iso-8bit")
        call define_coding_system_alias("euc-china", "chinese-iso-8bit")
        call define_coding_system_alias("euc-cn", "chinese-iso-8bit")
        call define_coding_system_alias("cn-gb", "chinese-iso-8bit")
        call define_coding_system_alias("gb2312", "chinese-iso-8bit")
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
        call define_iso_2022_coding_system("euc-tw", iachar("Z"), &
                                           doc="ISO 2022 based EUC encoding for Chinese CNS11643.", &
                             initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-cns11643-1", "chinese-cns11643-1", ""], &
                                  request_charsets=[character(len=CHARSET_NAME_LEN) :: "chinese-cns11643-7", "chinese-cns11643-6", &
                                           "chinese-cns11643-5", "chinese-cns11643-4", "chinese-cns11643-3", "chinese-cns11643-2", &
                                                             "chinese-cns11643-1", "chinese-cns11643-1", "ascii"], &
                                           request_registers=[integer :: 2, 2, 2, 2, 2, 2, 2, 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) ::], &
                                 charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "chinese-cns11643-1", "chinese-cns11643-2", &
                                           "chinese-cns11643-3", "chinese-cns11643-4", "chinese-cns11643-5", "chinese-cns11643-6", &
                                                     "chinese-cns11643-7"], &
                                           mime_charset="euc-tw")
        call define_coding_system_alias("euc-taiwan", "euc-tw")
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

    !> Port of the language environments of chinese.el (set-language-info-alist).
    module subroutine language_info_chinese()
        call set_language_info_alist("Chinese-GB", &
                    documentation="Support for Chinese GB2312 character set.", &
                    charsets=[character(len=40) :: "chinese-gb2312", "chinese-sisheng"], &
                    sample_text="Chinese (中文,普通话,汉语)"//achar(9)//"你好", &
                    coding_systems=[character(len=40) :: "chinese-iso-8bit", "iso-2022-cn", "chinese-hz"], &
                    coding_priority=[character(len=40) :: "chinese-iso-8bit", "chinese-gbk", "chinese-big5", "iso-2022-cn"], &
                    input_method="chinese-py-punct", &
                    tutorial="TUTORIAL.cn", &
                    iso639_language="zh")
        call set_language_info_alist("Chinese-BIG5", &
                    documentation="Support for Chinese Big5 character set.", &
                    charsets=[character(len=40) :: "chinese-big5-1", "chinese-big5-2"], &
                    sample_text="Cantonese (粵語,廣東話)"//achar(9)//"早晨, 你好", &
                    coding_systems=[character(len=40) :: "chinese-big5", "chinese-iso-7bit"], &
                    coding_priority=[character(len=40) :: "chinese-big5", "iso-2022-cn", "chinese-iso-8bit", "chinese-gbk"], &
                    input_method="chinese-py-punct-b5", &
                    tutorial="TUTORIAL.zh", &
                    iso639_language="zh")
        call set_language_info_alist("Chinese-CNS", &
                    documentation="Support for Chinese CNS character sets.  Note that the EUC-T" &
                    //"W coding system"//achar(10)//"accepts Big5 for input also (which is then c" &
                    //"onverted to CNS).", &
                    charsets=[character(len=40) :: "chinese-cns11643-1", "chinese-cns11643-2", "chinese-cns11643-3", &
                            "chinese-cns11643-4", "chinese-cns11643-5", "chinese-cns11643-6", "chinese-cns11643-7"], &
                    coding_systems=[character(len=40) :: "iso-2022-cn", "euc-tw"], &
                    coding_priority=[character(len=40) :: "iso-2022-cn", "euc-tw", "chinese-big5", "chinese-iso-8bit", &
                            "chinese-gbk"], &
                    input_method="chinese-cns-quick", &
                    iso639_language="zh")
        call set_language_info_alist("Chinese-EUC-TW", &
                    documentation="Support for Chinese, preferring the EUC-TW character set.  N" &
                    //"ote that"//achar(10)//"the EUC-TW coding system accepts Big5 for input als" &
                    //"o (which is then"//achar(10)//"converted to CNS).", &
                    charsets=[character(len=40) :: "chinese-cns11643-1", "chinese-cns11643-2", "chinese-cns11643-3", &
                            "chinese-cns11643-4", "chinese-cns11643-5", "chinese-cns11643-6", "chinese-cns11643-7", &
                            "chinese-big5-1", "chinese-big5-2"], &
                    coding_systems=[character(len=40) :: "euc-tw", "iso-2022-cn"], &
                    coding_priority=[character(len=40) :: "euc-tw", "chinese-big5", "iso-2022-cn", "chinese-iso-8bit", &
                            "chinese-gbk"], &
                    input_method="chinese-cns-quick", &
                    iso639_language="zh")
        call set_language_info_alist("Chinese-GBK", &
                    documentation="Support for Chinese GBK character set.", &
                    charsets=[character(len=40) :: "chinese-gbk"], &
                    sample_text="Chinese (中文,普通话,汉语) 妳好", &
                    coding_systems=[character(len=40) :: "chinese-gbk"], &
                    coding_priority=[character(len=40) :: "gbk", "iso-2022-cn", "chinese-big5", "chinese-iso-8bit"], &
                    input_method="chinese-py-punct", &
                    tutorial="TUTORIAL.cn", &
                    iso639_language="zh")
        call set_language_info_alist("Chinese-GB18030", &
                    documentation="Support for Chinese GB18030 character set.", &
                    charsets=[character(len=40) :: "gb18030"], &
                    sample_text="Chinese (中文,普通话,汉语) 你好", &
                    coding_systems=[character(len=40) :: "chinese-gb18030"], &
                    coding_priority=[character(len=40) :: "gb18030", "gbk", "iso-2022-cn", "chinese-big5", "chinese-iso-8bit"], &
                    input_method="chinese-py-punct", &
                    tutorial="TUTORIAL.cn", &
                    iso639_language="zh")
    end subroutine language_info_chinese
end submodule chinese_fm
