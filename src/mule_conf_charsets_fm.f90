! mule_conf_charsets_fm.f90 -- the charsets of mule-conf.el.
! Ported from GNU Emacs 31.1 lisp/international/mule-conf.el.
!
! Copyright (C) 1997-2026 Free Software Foundation, Inc.
! Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

!> The charsets mule-conf.el defines, in its order, with the properties
!> it puts on charset.c's own charsets and its `unify-charset' calls.  A
!> submodule of charset_fm, which defines them when it starts.
submodule(charset_fm) mule_conf_charsets_fm
    implicit none(type, external)

contains

    !> Port of the charset part of mule-conf.el.
    module subroutine define_standard_charsets()
        type(status_type) :: status

        call set_charset_properties("ascii", &
                                    docstring="ASCII (ISO646 IRV)", &
                                    short_name="ASCII", long_name="ASCII (ISO646 IRV)")
        call set_charset_properties("iso-8859-1", &
                                    docstring="Latin-1 (ISO/IEC 8859-1)", &
                                    short_name="Latin-1", long_name="Latin-1")
        call set_charset_properties("unicode", &
                                    docstring="Unicode (ISO10646)", &
                                    short_name="Unicode", long_name="Unicode (ISO10646)")
        call set_charset_properties("emacs", &
                                    docstring="Full Emacs charset (excluding eight bit chars)", &
                                    short_name="Emacs", long_name="Emacs")
        call set_charset_properties("eight-bit", docstring="Raw bytes 128-255", short_name="Raw bytes")
        call define_charset_alias("ucs", "unicode", status)
        call define_charset("latin-iso8859-1", &
                            "Right-Hand Part of ISO/IEC 8859/1 (Latin-1): ISO-IR-100", status, &
                            code_space=[32, 127], &
                            short_name="RHP of Latin-1", &
                            long_name="RHP of ISO/IEC 8859/1 (Latin-1): ISO-IR-100", &
                            iso_final_char=iachar("A"), &
                            emacs_mule_id=129, &
                            code_offset=160)
        call define_charset("control-1", &
                            "8-bit control code (0x80..0x9F)", status, &
                            code_space=[128, 159], &
                            short_name="8-bit control code", &
                            long_name="8-bit control code", &
                            code_offset=128)
        call define_charset("eight-bit-control", &
                            "Raw bytes in the range 0x80..0x9F (usually produced from inv" &
                            //"alid encodings)", status, &
                            code_space=[128, 159], &
                            short_name="Raw bytes 0x80..0x9F", &
                            long_name="Raw bytes 0x80..0x9F", &
                            supplementary_p=.true., &
                            code_offset=4194176)
        call define_charset("eight-bit-graphic", &
                            "Raw bytes in the range 0xA0..0xFF (usually produced from inv" &
                            //"alid encodings)", status, &
                            code_space=[160, 255], &
                            short_name="Raw bytes 0xA0..0xFF", &
                            long_name="Raw bytes 0xA0..0xFF", &
                            supplementary_p=.true., &
                            code_offset=4194208)
        call define_charset("iso-8859-2", &
                            "ISO/IEC 8859/2", status, &
                            code_space=[0, 255], &
                            short_name="Latin-2", &
                            long_name="ISO/IEC 8859/2", &
                            ascii_compatible_p=.true., &
                            map="8859-2")
        call define_charset("latin-iso8859-2", &
                            "Right-Hand Part of ISO/IEC 8859/2 (Latin-2): ISO-IR-101", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/2", &
                            long_name="RHP of ISO/IEC 8859/2 (Latin-2)", &
                            iso_final_char=iachar("B"), &
                            emacs_mule_id=130, &
                            subset_parent="iso-8859-2", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-3", &
                            "ISO/IEC 8859/3", status, &
                            code_space=[0, 255], &
                            short_name="Latin-3", &
                            long_name="ISO/IEC 8859/3", &
                            ascii_compatible_p=.true., &
                            map="8859-3")
        call define_charset("latin-iso8859-3", &
                            "Right-Hand Part of ISO/IEC 8859/3 (Latin-3): ISO-IR-109", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/3", &
                            long_name="RHP of ISO/IEC 8859/3 (Latin-3)", &
                            iso_final_char=iachar("C"), &
                            emacs_mule_id=131, &
                            subset_parent="iso-8859-3", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-4", &
                            "ISO/IEC 8859/4", status, &
                            code_space=[0, 255], &
                            short_name="Latin-4", &
                            long_name="ISO/IEC 8859/4", &
                            ascii_compatible_p=.true., &
                            map="8859-4")
        call define_charset("latin-iso8859-4", &
                            "Right-Hand Part of ISO/IEC 8859/4 (Latin-4): ISO-IR-110", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/4", &
                            long_name="RHP of ISO/IEC 8859/4 (Latin-4)", &
                            iso_final_char=iachar("D"), &
                            emacs_mule_id=132, &
                            subset_parent="iso-8859-4", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-5", &
                            "ISO/IEC 8859/5", status, &
                            code_space=[0, 255], &
                            short_name="Latin/Cyrillic", &
                            long_name="ISO/IEC 8859/5", &
                            ascii_compatible_p=.true., &
                            map="8859-5")
        call define_charset("cyrillic-iso8859-5", &
                            "Right-Hand Part of ISO/IEC 8859/5 (Latin/Cyrillic): ISO-IR-1" &
                            //"44", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/5", &
                            long_name="RHP of ISO/IEC 8859/5 (Latin/Cyrillic)", &
                            iso_final_char=iachar("L"), &
                            emacs_mule_id=140, &
                            subset_parent="iso-8859-5", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-6", &
                            "ISO/IEC 8859/6", status, &
                            code_space=[0, 255], &
                            short_name="Latin/Arabic", &
                            long_name="ISO/IEC 8859/6", &
                            ascii_compatible_p=.true., &
                            map="8859-6")
        call define_charset("arabic-iso8859-6", &
                            "Right-Hand Part of ISO/IEC 8859/6 (Latin/Arabic): ISO-IR-127", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/6", &
                            long_name="RHP of ISO/IEC 8859/6 (Latin/Arabic)", &
                            iso_final_char=iachar("G"), &
                            emacs_mule_id=135, &
                            subset_parent="iso-8859-6", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-7", &
                            "ISO/IEC 8859/7", status, &
                            code_space=[0, 255], &
                            short_name="Latin/Greek", &
                            long_name="ISO/IEC 8859/7", &
                            ascii_compatible_p=.true., &
                            map="8859-7")
        call define_charset("greek-iso8859-7", &
                            "Right-Hand Part of ISO/IEC 8859/7 (Latin/Greek): ISO-IR-126", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/7", &
                            long_name="RHP of ISO/IEC 8859/7 (Latin/Greek)", &
                            iso_final_char=iachar("F"), &
                            emacs_mule_id=134, &
                            subset_parent="iso-8859-7", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-8", &
                            "ISO/IEC 8859/8", status, &
                            code_space=[0, 255], &
                            short_name="Latin/Hebrew", &
                            long_name="ISO/IEC 8859/8", &
                            ascii_compatible_p=.true., &
                            map="8859-8")
        call define_charset("hebrew-iso8859-8", &
                            "Right-Hand Part of ISO/IEC 8859/8 (Latin/Hebrew): ISO-IR-138", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/8", &
                            long_name="RHP of ISO/IEC 8859/8 (Latin/Hebrew)", &
                            iso_final_char=iachar("H"), &
                            emacs_mule_id=136, &
                            subset_parent="iso-8859-8", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-9", &
                            "ISO/IEC 8859/9", status, &
                            code_space=[0, 255], &
                            short_name="Latin-5", &
                            long_name="ISO/IEC 8859/9", &
                            ascii_compatible_p=.true., &
                            map="8859-9")
        call define_charset("latin-iso8859-9", &
                            "Right-Hand Part of ISO/IEC 8859/9 (Latin-5): ISO-IR-148", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/9", &
                            long_name="RHP of ISO/IEC 8859/9 (Latin-5)", &
                            iso_final_char=iachar("M"), &
                            emacs_mule_id=141, &
                            subset_parent="iso-8859-9", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-10", &
                            "ISO/IEC 8859/10", status, &
                            code_space=[0, 255], &
                            short_name="Latin-6", &
                            long_name="ISO/IEC 8859/10", &
                            ascii_compatible_p=.true., &
                            map="8859-10")
        call define_charset("latin-iso8859-10", &
                            "Right-Hand Part of ISO/IEC 8859/10 (Latin-6): ISO-IR-157", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/10", &
                            long_name="RHP of ISO/IEC 8859/10 (Latin-6)", &
                            iso_final_char=iachar("V"), &
                            subset_parent="iso-8859-10", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-11", &
                            "ISO/IEC 8859/11", status, &
                            code_space=[0, 255], &
                            short_name="Latin/Thai", &
                            long_name="ISO/IEC 8859/11", &
                            ascii_compatible_p=.true., &
                            map="8859-11")
        call define_charset("thai-iso8859-11", &
                            "Right-Hand Part of ISO/IEC 8859/11 (Latin/Thai): ISO-IR-166", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/11", &
                            long_name="RHP of ISO/IEC 8859/11 (Latin/Thai)", &
                            iso_final_char=iachar("T"), &
                            subset_parent="iso-8859-11", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-13", &
                            "ISO/IEC 8859/13", status, &
                            code_space=[0, 255], &
                            short_name="Latin-7", &
                            long_name="ISO/IEC 8859/13", &
                            ascii_compatible_p=.true., &
                            map="8859-13")
        call define_charset("latin-iso8859-13", &
                            "Right-Hand Part of ISO/IEC 8859/13 (Latin-7): ISO-IR-179", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/13", &
                            long_name="RHP of ISO/IEC 8859/13 (Latin-7)", &
                            iso_final_char=iachar("Y"), &
                            subset_parent="iso-8859-13", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-14", &
                            "ISO/IEC 8859/14", status, &
                            code_space=[0, 255], &
                            short_name="Latin-8", &
                            long_name="ISO/IEC 8859/14", &
                            ascii_compatible_p=.true., &
                            map="8859-14")
        call define_charset("latin-iso8859-14", &
                            "Right-Hand Part of ISO/IEC 8859/14 (Latin-8): ISO-IR-199", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/14", &
                            long_name="RHP of ISO/IEC 8859/14 (Latin-8)", &
                            iso_final_char=iachar("_"), &
                            emacs_mule_id=143, &
                            subset_parent="iso-8859-14", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-15", &
                            "ISO/IEC 8859/15", status, &
                            code_space=[0, 255], &
                            short_name="Latin-9", &
                            long_name="ISO/IEC 8859/15", &
                            ascii_compatible_p=.true., &
                            map="8859-15")
        call define_charset("latin-iso8859-15", &
                            "Right-Hand Part of ISO/IEC 8859/15 (Latin-9): ISO-IR-203", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/15", &
                            long_name="RHP of ISO/IEC 8859/15 (Latin-9)", &
                            iso_final_char=iachar("b"), &
                            emacs_mule_id=142, &
                            subset_parent="iso-8859-15", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("iso-8859-16", &
                            "ISO/IEC 8859/16", status, &
                            code_space=[0, 255], &
                            short_name="Latin-10", &
                            long_name="ISO/IEC 8859/16", &
                            ascii_compatible_p=.true., &
                            map="8859-16")
        call define_charset("latin-iso8859-16", &
                            "Right-Hand Part of ISO/IEC 8859/16 (Latin-10): ISO-IR-226", status, &
                            code_space=[32, 127], &
                            short_name="RHP of ISO/IEC 8859/16", &
                            long_name="RHP of ISO/IEC 8859/16 (Latin-10)", &
                            iso_final_char=iachar("f"), &
                            subset_parent="iso-8859-16", subset_min_code=160, subset_max_code=255, subset_offset=-128)
        call define_charset("thai-tis620", &
                            "MULE charset for TIS620.2533", status, &
                            code_space=[32, 127], &
                            short_name="TIS620.2533", &
                            long_name="TIS620.2533", &
                            iso_final_char=iachar("T"), &
                            emacs_mule_id=133, &
                            code_offset=3584)
        call define_charset("tis620-2533", &
                            "TIS620.2533, a.k.a. TIS-620.  Like `thai-iso8859-11', but wi" &
                            //"thout NBSP.", status, &
                            code_space=[0, 255], &
                            short_name="TIS620.2533", &
                            long_name="TIS620.2533", &
                            ascii_compatible_p=.true., &
                            superset=[character(len=CHARSET_NAME_LEN) :: "ascii", "thai-tis620"], &
                            superset_offsets=[0, 128])
        call define_charset("jisx0201", &
                            "JISX0201", status, &
                            code_space=[0, 223], &
                            short_name="JISX0201", &
                            long_name="JISX0201", &
                            map="JISX0201")
        call define_charset("latin-jisx0201", &
                            "Roman Part of JISX0201.1976", status, &
                            code_space=[33, 126], &
                            short_name="JISX0201 Roman", &
                            long_name="Japanese Roman (JISX0201.1976)", &
                            iso_final_char=iachar("J"), &
                            emacs_mule_id=138, &
                            supplementary_p=.true., &
                            subset_parent="jisx0201", subset_min_code=33, subset_max_code=126, subset_offset=0)
        call define_charset("katakana-jisx0201", &
                            "Katakana Part of JISX0201.1976", status, &
                            code_space=[33, 126], &
                            short_name="JISX0201 Katakana", &
                            long_name="Japanese Katakana (JISX0201.1976)", &
                            iso_final_char=iachar("I"), &
                            emacs_mule_id=137, &
                            supplementary_p=.true., &
                            subset_parent="jisx0201", subset_min_code=161, subset_max_code=254, subset_offset=-128)
        call define_charset("chinese-gb2312", &
                            "GB2312 Chinese simplified: ISO-IR-58", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="GB2312", &
                            long_name="GB2312: ISO-IR-58", &
                            iso_final_char=iachar("A"), &
                            emacs_mule_id=145, &
                            code_offset=1114112, &
                            unify_map="GB2312")
        call define_charset("chinese-gbk", &
                            "GBK Chinese simplified.", status, &
                            code_space=[64, 254, 129, 254], &
                            short_name="GBK", &
                            long_name="GBK", &
                            code_offset=1441792, &
                            unify_map="GBK")
        call define_charset_alias("cp936", "chinese-gbk", status)
        call define_charset_alias("windows-936", "chinese-gbk", status)
        call define_charset("chinese-cns11643-1", &
                            "CNS11643 Plane 1 Chinese traditional: ISO-IR-171", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-1", &
                            long_name="CNS11643-1 (Chinese traditional): ISO-IR-171", &
                            iso_final_char=iachar("G"), &
                            emacs_mule_id=149, &
                            code_offset=1130496, &
                            unify_map="CNS-1")
        call define_charset("chinese-cns11643-2", &
                            "CNS11643 Plane 2 Chinese traditional: ISO-IR-172", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-2", &
                            long_name="CNS11643-2 (Chinese traditional): ISO-IR-172", &
                            iso_final_char=iachar("H"), &
                            emacs_mule_id=150, &
                            code_offset=1146880, &
                            unify_map="CNS-2")
        call define_charset("chinese-cns11643-3", &
                            "CNS11643 Plane 3 Chinese Traditional: ISO-IR-183", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-3", &
                            long_name="CNS11643-3 (Chinese traditional): ISO-IR-183", &
                            iso_final_char=iachar("I"), &
                            emacs_mule_id=246, &
                            code_offset=1163264, &
                            unify_map="CNS-3")
        call define_charset("chinese-cns11643-4", &
                            "CNS11643 Plane 4 Chinese Traditional: ISO-IR-184", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-4", &
                            long_name="CNS11643-4 (Chinese traditional): ISO-IR-184", &
                            iso_final_char=iachar("J"), &
                            emacs_mule_id=247, &
                            code_offset=1179648, &
                            unify_map="CNS-4")
        call define_charset("chinese-cns11643-5", &
                            "CNS11643 Plane 5 Chinese Traditional: ISO-IR-185", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-5", &
                            long_name="CNS11643-5 (Chinese traditional): ISO-IR-185", &
                            iso_final_char=iachar("K"), &
                            emacs_mule_id=248, &
                            code_offset=1196032, &
                            unify_map="CNS-5")
        call define_charset("chinese-cns11643-6", &
                            "CNS11643 Plane 6 Chinese Traditional: ISO-IR-186", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-6", &
                            long_name="CNS11643-6 (Chinese traditional): ISO-IR-186", &
                            iso_final_char=iachar("L"), &
                            emacs_mule_id=249, &
                            code_offset=1212416, &
                            unify_map="CNS-6")
        call define_charset("chinese-cns11643-7", &
                            "CNS11643 Plane 7 Chinese Traditional: ISO-IR-187", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-7", &
                            long_name="CNS11643-7 (Chinese traditional): ISO-IR-187", &
                            iso_final_char=iachar("M"), &
                            emacs_mule_id=250, &
                            code_offset=1228800, &
                            unify_map="CNS-7")
        call define_charset("big5", &
                            "Big5 (Chinese traditional)", status, &
                            code_space=[64, 254, 161, 254], &
                            short_name="Big5", &
                            long_name="Big5", &
                            code_offset=1245184, &
                            unify_map="BIG5")
        call define_charset("chinese-big5-1", &
                            "Frequently used part (A141-C67E) of Big5 (Chinese traditiona" &
                            //"l)", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="Big5 (Level-1)", &
                            long_name="Big5 (Level-1) A141-C67F", &
                            iso_final_char=iachar("0"), &
                            emacs_mule_id=152, &
                            supplementary_p=.true., &
                            code_offset=1265664, &
                            unify_map="BIG5-1")
        call define_charset("chinese-big5-2", &
                            "Less frequently used part (C940-FEFE) of Big5 (Chinese tradi" &
                            //"tional)", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="Big5 (Level-2)", &
                            long_name="Big5 (Level-2) C940-FEFE", &
                            iso_final_char=iachar("1"), &
                            emacs_mule_id=153, &
                            supplementary_p=.true., &
                            code_offset=1275904, &
                            unify_map="BIG5-2")
        call define_charset("japanese-jisx0208", &
                            "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0208", &
                            long_name="JISX0208.1983/1990 (Japanese): ISO-IR-87", &
                            iso_final_char=iachar("B"), &
                            emacs_mule_id=146, &
                            code_offset=1310720, &
                            unify_map="JISX0208")
        call define_charset("japanese-jisx0208-1978", &
                            "JISX0208.1978 Japanese Kanji (so called ""old JIS""): ISO-IR-4" &
                            //"2", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0208.1978", &
                            long_name="JISX0208.1978 (JISC6226.1978): ISO-IR-42", &
                            iso_final_char=iachar("@"), &
                            emacs_mule_id=144, &
                            code_offset=1327104, &
                            unify_map="JISC6226")
        call define_charset("japanese-jisx0212", &
                            "JISX0212 Japanese supplement: ISO-IR-159", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0212", &
                            long_name="JISX0212 (Japanese): ISO-IR-159", &
                            iso_final_char=iachar("D"), &
                            emacs_mule_id=148, &
                            code_offset=1343488, &
                            unify_map="JISX0212")
        call define_charset("japanese-jisx0213-1", &
                            "JISX0213.2000 Plane 1 (Japanese)", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0213-1", &
                            long_name="JISX0213-1", &
                            iso_final_char=iachar("O"), &
                            emacs_mule_id=151, &
                            code_offset=1359872, &
                            unify_map="JISX2131")
        call define_charset("japanese-jisx0213-2", &
                            "JISX0213.2000 Plane 2 (Japanese)", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0213-2", &
                            long_name="JISX0213-2", &
                            iso_final_char=iachar("P"), &
                            emacs_mule_id=254, &
                            code_offset=1376256, &
                            unify_map="JISX2132")
        call define_charset("japanese-jisx0213-a", &
                            "JISX0213.2004 adds these characters to JISX0213.2000.", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0213A", &
                            long_name="JISX0213A", &
                            supplementary_p=.true., &
                            map="JISX213A")
        call define_charset("japanese-jisx0213.2004-1", &
                            "JISX0213.2004 Plane1 (Japanese)", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="JISX0213.2004-1", &
                            long_name="JISX0213.2004-1", &
                            iso_final_char=iachar("Q"), &
                            superset=[character(len=CHARSET_NAME_LEN) :: "japanese-jisx0213-a", "japanese-jisx0213-1"])
        call define_charset("katakana-sjis", &
                            "Katakana part of Shift-JIS", status, &
                            code_space=[161, 223], &
                            short_name="katakana-sjis", &
                            long_name="katakana-sjis", &
                            supplementary_p=.true., &
                            subset_parent="jisx0201", subset_min_code=161, subset_max_code=223, subset_offset=0)
        call define_charset("cp932-2-byte", &
                            "2-byte part of CP932", status, &
                            code_space=[64, 252, 129, 252], &
                            short_name="cp932-2-byte", &
                            long_name="cp932-2-byte", &
                            supplementary_p=.true., &
                            map="CP932-2BYTE")
        call define_charset("cp932", &
                            "CP932 (Microsoft shift-jis)", status, &
                            code_space=[0, 255, 0, 254], &
                            short_name="CP932", &
                            long_name="CP932", &
                            superset=[character(len=CHARSET_NAME_LEN) :: "ascii", "katakana-sjis", "cp932-2-byte"])
        call define_charset("korean-ksc5601", &
                            "KSC5601 Korean Hangul and Hanja: ISO-IR-149", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="KSC5601", &
                            long_name="KSC5601 (Korean): ISO-IR-149", &
                            iso_final_char=iachar("C"), &
                            emacs_mule_id=147, &
                            code_offset=2596756, &
                            unify_map="KSC5601")
        call define_charset("big5-hkscs", &
                            "Big5-HKSCS (Chinese traditional, Hong Kong supplement)", status, &
                            code_space=[64, 254, 161, 254], &
                            short_name="Big5", &
                            long_name="Big5", &
                            code_offset=2605592, &
                            unify_map="BIG5-HKSCS")
        call define_charset("cp949-2-byte", &
                            "2-byte part of CP949", status, &
                            code_space=[65, 254, 129, 253], &
                            short_name="cp949-2-byte", &
                            long_name="cp949-2-byte", &
                            supplementary_p=.true., &
                            map="CP949-2BYTE")
        call define_charset("cp949", &
                            "CP949 (Korean)", status, &
                            code_space=[0, 254, 0, 253], &
                            short_name="CP949", &
                            long_name="CP949 (Korean)", &
                            superset=[character(len=CHARSET_NAME_LEN) :: "ascii", "cp949-2-byte"])
        call define_charset("chinese-sisheng", &
                            "SiSheng characters for PinYin/ZhuYin", status, &
                            code_space=[33, 126], &
                            short_name="SiSheng", &
                            long_name="SiSheng (PinYin/ZhuYin)", &
                            iso_final_char=iachar("0"), &
                            emacs_mule_id=160, &
                            supplementary_p=.true., &
                            code_offset=2097152, &
                            unify_map="MULE-sisheng")
        call define_charset("ipa", &
                            "IPA (International Phonetic Association)", status, &
                            code_space=[32, 127], &
                            short_name="IPA", &
                            long_name="IPA", &
                            iso_final_char=iachar("0"), &
                            emacs_mule_id=161, &
                            supplementary_p=.true., &
                            code_offset=2097280, &
                            unify_map="MULE-ipa")
        call define_charset("viscii", &
                            "VISCII1.1", status, &
                            code_space=[0, 255], &
                            short_name="VISCII", &
                            long_name="VISCII 1.1", &
                            map="VISCII")
        call define_charset("vietnamese-viscii-lower", &
                            "VISCII1.1 lower-case", status, &
                            code_space=[32, 127], &
                            short_name="VISCII lower", &
                            long_name="VISCII lower-case", &
                            iso_final_char=iachar("1"), &
                            emacs_mule_id=162, &
                            supplementary_p=.true., &
                            code_offset=2097664, &
                            unify_map="MULE-lviscii")
        call define_charset("vietnamese-viscii-upper", &
                            "VISCII1.1 upper-case", status, &
                            code_space=[32, 127], &
                            short_name="VISCII upper", &
                            long_name="VISCII upper-case", &
                            iso_final_char=iachar("2"), &
                            emacs_mule_id=163, &
                            supplementary_p=.true., &
                            code_offset=2097792, &
                            unify_map="MULE-uviscii")
        call define_charset("vscii", &
                            "VSCII1.1 (TCVN-5712 VN1)", status, &
                            code_space=[0, 255], &
                            short_name="VSCII", &
                            long_name="VSCII", &
                            map="VSCII")
        call define_charset_alias("tcvn-5712", "vscii", status)
        call define_charset("vscii-2", &
                            "VSCII-2 (TCVN-5712 VN2)", status, &
                            code_space=[0, 255], &
                            short_name="vscii-2", &
                            long_name="vscii-2", &
                            map="VSCII-2")
        call define_charset("koi8-r", &
                            "KOI8-R", status, &
                            code_space=[0, 255], &
                            short_name="KOI8-R", &
                            long_name="KOI8-R", &
                            ascii_compatible_p=.true., &
                            map="KOI8-R")
        call define_charset_alias("koi8", "koi8-r", status)
        call define_charset("alternativnyj", &
                            "ALTERNATIVNYJ", status, &
                            code_space=[0, 255], &
                            short_name="alternativnyj", &
                            long_name="alternativnyj", &
                            ascii_compatible_p=.true., &
                            map="ALTERNATIVNYJ")
        call define_charset("cp866", &
                            "CP866", status, &
                            code_space=[0, 255], &
                            short_name="cp866", &
                            long_name="cp866", &
                            ascii_compatible_p=.true., &
                            map="IBM866")
        call define_charset_alias("ibm866", "cp866", status)
        call define_charset("koi8-u", &
                            "KOI8-U", status, &
                            code_space=[0, 255], &
                            short_name="KOI8-U", &
                            long_name="KOI8-U", &
                            ascii_compatible_p=.true., &
                            map="KOI8-U")
        call define_charset("koi8-t", &
                            "KOI8-T", status, &
                            code_space=[0, 255], &
                            short_name="KOI8-T", &
                            long_name="KOI8-T", &
                            ascii_compatible_p=.true., &
                            map="KOI8-T")
        call define_charset("georgian-ps", &
                            "GEORGIAN-PS", status, &
                            code_space=[0, 255], &
                            short_name="GEORGIAN-PS", &
                            long_name="GEORGIAN-PS", &
                            ascii_compatible_p=.true., &
                            map="KA-PS")
        call define_charset("georgian-academy", &
                            "GEORGIAN-ACADEMY", status, &
                            code_space=[0, 255], &
                            short_name="GEORGIAN-ACADEMY", &
                            long_name="GEORGIAN-ACADEMY", &
                            ascii_compatible_p=.true., &
                            map="KA-ACADEMY")
        call define_charset("windows-1250", &
                            "WINDOWS-1250 (Central Europe)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1250", &
                            long_name="WINDOWS-1250", &
                            ascii_compatible_p=.true., &
                            map="CP1250")
        call define_charset_alias("cp1250", "windows-1250", status)
        call define_charset("windows-1251", &
                            "WINDOWS-1251 (Cyrillic)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1251", &
                            long_name="WINDOWS-1251", &
                            ascii_compatible_p=.true., &
                            map="CP1251")
        call define_charset_alias("cp1251", "windows-1251", status)
        call define_charset("windows-1252", &
                            "WINDOWS-1252 (Latin I)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1252", &
                            long_name="WINDOWS-1252", &
                            ascii_compatible_p=.true., &
                            map="CP1252")
        call define_charset_alias("cp1252", "windows-1252", status)
        call define_charset("windows-1253", &
                            "WINDOWS-1253 (Greek)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1253", &
                            long_name="WINDOWS-1253", &
                            ascii_compatible_p=.true., &
                            map="CP1253")
        call define_charset_alias("cp1253", "windows-1253", status)
        call define_charset("windows-1254", &
                            "WINDOWS-1254 (Turkish)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1254", &
                            long_name="WINDOWS-1254", &
                            ascii_compatible_p=.true., &
                            map="CP1254")
        call define_charset_alias("cp1254", "windows-1254", status)
        call define_charset("windows-1255", &
                            "WINDOWS-1255 (Hebrew)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1255", &
                            long_name="WINDOWS-1255", &
                            ascii_compatible_p=.true., &
                            map="CP1255")
        call define_charset_alias("cp1255", "windows-1255", status)
        call define_charset("windows-1256", &
                            "WINDOWS-1256 (Arabic)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1256", &
                            long_name="WINDOWS-1256", &
                            ascii_compatible_p=.true., &
                            map="CP1256")
        call define_charset_alias("cp1256", "windows-1256", status)
        call define_charset("windows-1257", &
                            "WINDOWS-1257 (Baltic)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1257", &
                            long_name="WINDOWS-1257", &
                            ascii_compatible_p=.true., &
                            map="CP1257")
        call define_charset_alias("cp1257", "windows-1257", status)
        call define_charset("windows-1258", &
                            "WINDOWS-1258 (Viet Nam)", status, &
                            code_space=[0, 255], &
                            short_name="WINDOWS-1258", &
                            long_name="WINDOWS-1258", &
                            ascii_compatible_p=.true., &
                            map="CP1258")
        call define_charset_alias("cp1258", "windows-1258", status)
        call define_charset("next", &
                            "NEXT", status, &
                            code_space=[0, 255], &
                            short_name="NEXT", &
                            long_name="NEXT", &
                            ascii_compatible_p=.true., &
                            map="NEXTSTEP")
        call define_charset("cp1125", &
                            "CP1125", status, &
                            code_space=[0, 255], &
                            short_name="CP1125", &
                            long_name="CP1125", &
                            ascii_compatible_p=.true., &
                            map="CP1125")
        call define_charset_alias("ruscii", "cp1125", status)
        call define_charset_alias("cp866u", "cp1125", status)
        call define_charset("cp437", &
                            "CP437 (MS-DOS United States, Australia, New Zealand, South A" &
                            //"frica)", status, &
                            code_space=[0, 255], &
                            short_name="CP437", &
                            long_name="CP437", &
                            ascii_compatible_p=.true., &
                            map="IBM437")
        call define_charset("cp720", &
                            "CP720 (Arabic)", status, &
                            code_space=[0, 255], &
                            short_name="CP720", &
                            long_name="CP720", &
                            ascii_compatible_p=.true., &
                            map="CP720")
        call define_charset("cp737", &
                            "CP737 (PC Greek)", status, &
                            code_space=[0, 255], &
                            short_name="CP737", &
                            long_name="CP737", &
                            ascii_compatible_p=.true., &
                            map="CP737")
        call define_charset("cp775", &
                            "CP775 (PC Baltic)", status, &
                            code_space=[0, 255], &
                            short_name="CP775", &
                            long_name="CP775", &
                            ascii_compatible_p=.true., &
                            map="CP775")
        call define_charset("cp851", &
                            "CP851 (Greek)", status, &
                            code_space=[0, 255], &
                            short_name="CP851", &
                            long_name="CP851", &
                            ascii_compatible_p=.true., &
                            map="IBM851")
        call define_charset("cp852", &
                            "CP852 (MS-DOS Latin-2)", status, &
                            code_space=[0, 255], &
                            short_name="CP852", &
                            long_name="CP852", &
                            ascii_compatible_p=.true., &
                            map="IBM852")
        call define_charset("cp855", &
                            "CP855 (IBM Cyrillic)", status, &
                            code_space=[0, 255], &
                            short_name="CP855", &
                            long_name="CP855", &
                            ascii_compatible_p=.true., &
                            map="IBM855")
        call define_charset("cp857", &
                            "CP857 (IBM Turkish)", status, &
                            code_space=[0, 255], &
                            short_name="CP857", &
                            long_name="CP857", &
                            ascii_compatible_p=.true., &
                            map="IBM857")
        call define_charset("cp858", &
                            "CP858 (Multilingual Latin I + Euro)", status, &
                            code_space=[0, 255], &
                            short_name="CP858", &
                            long_name="CP858", &
                            ascii_compatible_p=.true., &
                            map="CP858")
        call define_charset_alias("cp00858", "cp858", status)
        call define_charset("cp860", &
                            "CP860 (MS-DOS Portuguese)", status, &
                            code_space=[0, 255], &
                            short_name="CP860", &
                            long_name="CP860", &
                            ascii_compatible_p=.true., &
                            map="IBM860")
        call define_charset("cp861", &
                            "CP861 (MS-DOS Icelandic)", status, &
                            code_space=[0, 255], &
                            short_name="CP861", &
                            long_name="CP861", &
                            ascii_compatible_p=.true., &
                            map="IBM861")
        call define_charset("cp862", &
                            "CP862 (PC Hebrew)", status, &
                            code_space=[0, 255], &
                            short_name="CP862", &
                            long_name="CP862", &
                            ascii_compatible_p=.true., &
                            map="IBM862")
        call define_charset("cp863", &
                            "CP863 (MS-DOS Canadian French)", status, &
                            code_space=[0, 255], &
                            short_name="CP863", &
                            long_name="CP863", &
                            ascii_compatible_p=.true., &
                            map="IBM863")
        call define_charset("cp864", &
                            "CP864 (PC Arabic)", status, &
                            code_space=[0, 255], &
                            short_name="CP864", &
                            long_name="CP864", &
                            ascii_compatible_p=.true., &
                            map="IBM864")
        call define_charset("cp865", &
                            "CP865 (MS-DOS Nordic)", status, &
                            code_space=[0, 255], &
                            short_name="CP865", &
                            long_name="CP865", &
                            ascii_compatible_p=.true., &
                            map="IBM865")
        call define_charset("cp869", &
                            "CP869 (IBM Modern Greek)", status, &
                            code_space=[0, 255], &
                            short_name="CP869", &
                            long_name="CP869", &
                            ascii_compatible_p=.true., &
                            map="IBM869")
        call define_charset("cp874", &
                            "CP874 (IBM Thai)", status, &
                            code_space=[0, 255], &
                            short_name="CP874", &
                            long_name="CP874", &
                            ascii_compatible_p=.true., &
                            map="IBM874")
        call define_charset("arabic-digit", &
                            "Arabic digit", status, &
                            code_space=[34, 42], &
                            short_name="Arabic digit", &
                            long_name="Arabic digit", &
                            iso_final_char=iachar("2"), &
                            emacs_mule_id=164, &
                            supplementary_p=.true., &
                            code_offset=1536)
        call define_charset("arabic-1-column", &
                            "Arabic 1-column", status, &
                            code_space=[33, 126], &
                            short_name="Arabic 1-col", &
                            long_name="Arabic 1-column", &
                            iso_final_char=iachar("3"), &
                            emacs_mule_id=165, &
                            supplementary_p=.true., &
                            code_offset=2097408)
        call define_charset("arabic-2-column", &
                            "Arabic 2-column", status, &
                            code_space=[33, 126], &
                            short_name="Arabic 2-col", &
                            long_name="Arabic 2-column", &
                            iso_final_char=iachar("4"), &
                            emacs_mule_id=224, &
                            supplementary_p=.true., &
                            code_offset=2097536)
        call define_charset("lao", &
                            "Lao characters (ISO10646 0E81..0EDF)", status, &
                            code_space=[33, 126], &
                            short_name="Lao", &
                            long_name="Lao", &
                            iso_final_char=iachar("1"), &
                            emacs_mule_id=167, &
                            supplementary_p=.true., &
                            code_offset=3713)
        call define_charset("mule-lao", &
                            "Lao characters (ISO10646 0E81..0EDF)", status, &
                            code_space=[0, 255], &
                            short_name="Lao", &
                            long_name="Lao", &
                            supplementary_p=.true., &
                            superset=[character(len=CHARSET_NAME_LEN) :: "ascii", "eight-bit-control", "lao"], &
                            superset_offsets=[0, 0, 128])
        call define_charset("indian-is13194", &
                            "7-bit representation of IS 13194 (ISCII) for Devanagari", status, &
                            code_space=[33, 126], &
                            short_name="IS 13194 (DEV)", &
                            long_name="Indian IS 13194 (DEV)", &
                            iso_final_char=iachar("5"), &
                            emacs_mule_id=225, &
                            supplementary_p=.true., &
                            code_offset=1572864, &
                            unify_map="MULE-is13194")
        call define_charset("devanagari-cdac", &
                            "Glyphs of Devanagari script for CDAC font.  Subset of `india" &
                            //"n-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Devanagari glyphs", &
                            long_name="CDAC Devanagari glyphs", &
                            supplementary_p=.true., &
                            code_offset=1573120)
        call define_charset("sanskrit-cdac", &
                            "Glyphs of Sanskrit script for CDAC font.  Subset of `indian-" &
                            //"glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Sanskrit glyphs", &
                            long_name="CDAC Sanskrit glyphs", &
                            supplementary_p=.true., &
                            code_offset=1573376)
        call define_charset("bengali-cdac", &
                            "Glyphs of Bengali script for CDAC font.  Subset of `indian-g" &
                            //"lyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Bengali glyphs", &
                            long_name="CDAC Bengali glyphs", &
                            supplementary_p=.true., &
                            code_offset=1573632)
        call define_charset("tamil-cdac", &
                            "Glyphs of Tamil script for CDAC font.  Subset of `indian-gly" &
                            //"ph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Tamil glyphs", &
                            long_name="CDAC Tamil glyphs", &
                            supplementary_p=.true., &
                            code_offset=1573888)
        call define_charset("telugu-cdac", &
                            "Glyphs of Telugu script for CDAC font.  Subset of `indian-gl" &
                            //"yph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Telugu glyphs", &
                            long_name="CDAC Telugu glyphs", &
                            supplementary_p=.true., &
                            code_offset=1574144)
        call define_charset("assamese-cdac", &
                            "Glyphs of Assamese script for CDAC font.  Subset of `indian-" &
                            //"glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Assamese glyphs", &
                            long_name="CDAC Assamese glyphs", &
                            supplementary_p=.true., &
                            code_offset=1574400)
        call define_charset("oriya-cdac", &
                            "Glyphs of Oriya script for CDAC font.  Subset of `indian-gly" &
                            //"ph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Oriya glyphs", &
                            long_name="CDAC Oriya glyphs", &
                            supplementary_p=.true., &
                            code_offset=1574656)
        call define_charset("kannada-cdac", &
                            "Glyphs of Kannada script for CDAC font.  Subset of `indian-g" &
                            //"lyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Kannada glyphs", &
                            long_name="CDAC Kannada glyphs", &
                            supplementary_p=.true., &
                            code_offset=1574912)
        call define_charset("malayalam-cdac", &
                            "Glyphs of Malayalam script for CDAC font.  Subset of `indian" &
                            //"-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Malayalam glyphs", &
                            long_name="CDAC Malayalam glyphs", &
                            supplementary_p=.true., &
                            code_offset=1575168)
        call define_charset("gujarati-cdac", &
                            "Glyphs of Gujarati script for CDAC font.  Subset of `indian-" &
                            //"glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Gujarati glyphs", &
                            long_name="CDAC Gujarati glyphs", &
                            supplementary_p=.true., &
                            code_offset=1575424)
        call define_charset("punjabi-cdac", &
                            "Glyphs of Punjabi script for CDAC font.  Subset of `indian-g" &
                            //"lyph'.", status, &
                            code_space=[0, 255], &
                            short_name="CDAC Punjabi glyphs", &
                            long_name="CDAC Punjabi glyphs", &
                            supplementary_p=.true., &
                            code_offset=1575680)
        call define_charset("devanagari-akruti", &
                            "Glyphs of Devanagari script for AKRUTI font.  Subset of `ind" &
                            //"ian-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Devanagari glyphs", &
                            long_name="AKRUTI Devanagari glyphs", &
                            supplementary_p=.true., &
                            code_offset=1575936)
        call define_charset("bengali-akruti", &
                            "Glyphs of Bengali script for AKRUTI font.  Subset of `indian" &
                            //"-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Bengali glyphs", &
                            long_name="AKRUTI Bengali glyphs", &
                            supplementary_p=.true., &
                            code_offset=1576192)
        call define_charset("punjabi-akruti", &
                            "Glyphs of Punjabi script for AKRUTI font.  Subset of `indian" &
                            //"-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Punjabi glyphs", &
                            long_name="AKRUTI Punjabi glyphs", &
                            supplementary_p=.true., &
                            code_offset=1576448)
        call define_charset("gujarati-akruti", &
                            "Glyphs of Gujarati script for AKRUTI font.  Subset of `india" &
                            //"n-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Gujarati glyphs", &
                            long_name="AKRUTI Gujarati glyphs", &
                            supplementary_p=.true., &
                            code_offset=1576704)
        call define_charset("oriya-akruti", &
                            "Glyphs of Oriya script for AKRUTI font.  Subset of `indian-g" &
                            //"lyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Oriya glyphs", &
                            long_name="AKRUTI Oriya glyphs", &
                            supplementary_p=.true., &
                            code_offset=1576960)
        call define_charset("tamil-akruti", &
                            "Glyphs of Tamil script for AKRUTI font.  Subset of `indian-g" &
                            //"lyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Tamil glyphs", &
                            long_name="AKRUTI Tamil glyphs", &
                            supplementary_p=.true., &
                            code_offset=1577216)
        call define_charset("telugu-akruti", &
                            "Glyphs of Telugu script for AKRUTI font.  Subset of `indian-" &
                            //"glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Telugu glyphs", &
                            long_name="AKRUTI Telugu glyphs", &
                            supplementary_p=.true., &
                            code_offset=1577472)
        call define_charset("kannada-akruti", &
                            "Glyphs of Kannada script for AKRUTI font.  Subset of `indian" &
                            //"-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Kannada glyphs", &
                            long_name="AKRUTI Kannada glyphs", &
                            supplementary_p=.true., &
                            code_offset=1577728)
        call define_charset("malayalam-akruti", &
                            "Glyphs of Malayalam script for AKRUTI font.  Subset of `indi" &
                            //"an-glyph'.", status, &
                            code_space=[0, 255], &
                            short_name="AKRUTI Malayalam glyphs", &
                            long_name="AKRUTI Malayalam glyphs", &
                            supplementary_p=.true., &
                            code_offset=1577984)
        call define_charset("indian-glyph", &
                            "Glyphs for Indian characters.", status, &
                            code_space=[32, 127, 32, 127], &
                            short_name="Indian glyph", &
                            long_name="Indian glyph", &
                            iso_final_char=iachar("4"), &
                            emacs_mule_id=240, &
                            supplementary_p=.true., &
                            code_offset=1573120)
        call define_charset("indian-1-column", &
                            "Indian charset for 1-column width glyphs.", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="Indian 1-col", &
                            long_name="Indian 1 Column", &
                            iso_final_char=iachar("6"), &
                            emacs_mule_id=251, &
                            supplementary_p=.true., &
                            code_offset=1589248)
        call define_charset("indian-2-column", &
                            "Indian charset for 2-column width glyphs.", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="Indian 2-col", &
                            long_name="Indian 2 Column", &
                            iso_final_char=iachar("5"), &
                            emacs_mule_id=251, &
                            supplementary_p=.true., &
                            code_offset=1589248)
        call define_charset("tibetan", &
                            "Tibetan characters", status, &
                            code_space=[33, 126, 33, 37], &
                            short_name="Tibetan 2-col", &
                            long_name="Tibetan 2 column", &
                            iso_final_char=iachar("7"), &
                            emacs_mule_id=252, &
                            supplementary_p=.true., &
                            code_offset=1638400, &
                            unify_map="MULE-tibetan")
        call define_charset("tibetan-1-column", &
                            "Tibetan 1 column glyph", status, &
                            code_space=[33, 126, 33, 37], &
                            short_name="Tibetan 1-col", &
                            long_name="Tibetan 1 column", &
                            iso_final_char=iachar("8"), &
                            emacs_mule_id=241, &
                            supplementary_p=.true., &
                            code_offset=1638400)
        call define_charset("mule-unicode-2500-33ff", &
                            "Unicode characters of the range U+2500..U+33FF.", status, &
                            code_space=[32, 127, 32, 71], &
                            short_name="Unicode subset 2", &
                            long_name="Unicode subset (U+2500..U+33FF)", &
                            iso_final_char=iachar("2"), &
                            emacs_mule_id=242, &
                            supplementary_p=.true., &
                            code_offset=9472)
        call define_charset("mule-unicode-e000-ffff", &
                            "Unicode characters of the range U+E000..U+FFFF.", status, &
                            code_space=[32, 127, 32, 117], &
                            short_name="Unicode subset 3", &
                            long_name="Unicode subset (U+E000+FFFF)", &
                            max_code=30015_int64, &
                            iso_final_char=iachar("3"), &
                            emacs_mule_id=243, &
                            supplementary_p=.true., &
                            code_offset=57344)
        call define_charset("mule-unicode-0100-24ff", &
                            "Unicode characters of the range U+0100..U+24FF.", status, &
                            code_space=[32, 127, 32, 127], &
                            short_name="Unicode subset", &
                            long_name="Unicode subset (U+0100..U+24FF)", &
                            iso_final_char=iachar("1"), &
                            emacs_mule_id=244, &
                            supplementary_p=.true., &
                            code_offset=256)
        call define_charset("unicode-bmp", &
                            "Unicode Basic Multilingual Plane (U+0000..U+FFFF)", status, &
                            code_space=[0, 255, 0, 255], &
                            short_name="Unicode BMP", &
                            long_name="Unicode BMP", &
                            code_offset=0)
        call define_charset("unicode-smp", &
                            "Unicode Supplementary Multilingual Plane (U+10000..U+1FFFF)", status, &
                            code_space=[0, 255, 0, 255], &
                            short_name="Unicode SMP ", &
                            long_name="Unicode SMP ", &
                            code_offset=65536)
        call define_charset("unicode-sip", &
                            "Unicode Supplementary Ideographic Plane (U+20000..U+2FFFF)", status, &
                            code_space=[0, 255, 0, 255], &
                            short_name="Unicode SIP", &
                            long_name="Unicode SIP", &
                            code_offset=131072)
        call define_charset("unicode-ssp", &
                            "Unicode Supplementary Special-purpose Plane (U+E0000..U+EFFF" &
                            //"F)", status, &
                            code_space=[0, 255, 0, 255], &
                            short_name="Unicode SSP", &
                            long_name="Unicode SSP", &
                            code_offset=917504)
        call define_charset("ethiopic", &
                            "Ethiopic characters for Amharic and Tigrigna.", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="Ethiopic", &
                            long_name="Ethiopic characters", &
                            iso_final_char=iachar("3"), &
                            emacs_mule_id=245, &
                            supplementary_p=.true., &
                            code_offset=1703936, &
                            unify_map="MULE-ethiopic")
        call define_charset("mac-roman", &
                            "Mac Roman charset", status, &
                            code_space=[0, 255], &
                            short_name="Mac Roman", &
                            long_name="Mac Roman", &
                            ascii_compatible_p=.true., &
                            map="MACINTOSH")
        call define_charset("ebcdic-us", &
                            "US version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="EBCDIC-US", &
                            long_name="EBCDIC-US", &
                            map="EBCDICUS")
        call define_charset("ebcdic-uk", &
                            "UK version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="EBCDIC-UK", &
                            long_name="EBCDIC-UK", &
                            map="EBCDICUK")
        call define_charset("ibm038", &
                            "International version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM038", &
                            long_name="IBM038", &
                            map="IBM038")
        call define_charset_alias("ebcdic-int", "ibm038", status)
        call define_charset_alias("cp038", "ibm038", status)
        call define_charset("ibm256", &
                            "Netherlands version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM256", &
                            long_name="IBM256", &
                            map="IBM256")
        call define_charset("ibm273", &
                            "Austrian / German version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM273", &
                            long_name="IBM273", &
                            map="IBM273")
        call define_charset("ibm274", &
                            "Belgian version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM274", &
                            long_name="IBM274", &
                            map="IBM274")
        call define_charset("ibm275", &
                            "Brazilian version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM275", &
                            long_name="IBM275", &
                            map="IBM275")
        call define_charset("ibm277", &
                            "Danish / Norwegian version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM277", &
                            long_name="IBM277", &
                            map="IBM277")
        call define_charset("ibm278", &
                            "Finnish / Swedish version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM278", &
                            long_name="IBM278", &
                            map="IBM278")
        call define_charset("ibm280", &
                            "Italian version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM280", &
                            long_name="IBM280", &
                            map="IBM280")
        call define_charset("ibm281", &
                            "Japanese-E version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM281", &
                            long_name="IBM281", &
                            map="IBM281")
        call define_charset("ibm284", &
                            "Spanish version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM284", &
                            long_name="IBM284", &
                            map="IBM284")
        call define_charset("ibm285", &
                            "UK english version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM285", &
                            long_name="IBM285", &
                            map="IBM285")
        call define_charset("ibm290", &
                            "Japanese katakana version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM290", &
                            long_name="IBM290", &
                            map="IBM290")
        call define_charset("ibm297", &
                            "French version of EBCDIC", status, &
                            code_space=[0, 255], &
                            short_name="IBM297", &
                            long_name="IBM297", &
                            map="IBM297")
        call define_charset("ibm1047", &
                            "IBM1047, `EBCDIC Latin 1/Open Systems' used by OS/390 Unix.", status, &
                            code_space=[0, 255], &
                            short_name="IBM1047", &
                            long_name="IBM1047", &
                            map="IBM1047")
        call define_charset_alias("cp1047", "ibm1047", status)
        call define_charset("hp-roman8", &
                            "Encoding used by Hewlet-Packard printer software", status, &
                            code_space=[0, 255], &
                            short_name="HP-ROMAN8", &
                            long_name="HP-ROMAN8", &
                            ascii_compatible_p=.true., &
                            map="HP-ROMAN8")
        call define_charset("adobe-standard-encoding", &
                            "Adobe `standard encoding' used in PostScript", status, &
                            code_space=[32, 255], &
                            short_name="ADOBE-STANDARD-ENCODING", &
                            long_name="ADOBE-STANDARD-ENCODING", &
                            map="stdenc")
        call define_charset("symbol", &
                            "Adobe symbol encoding used in PostScript", status, &
                            code_space=[32, 255], &
                            short_name="ADOBE-SYMBOL", &
                            long_name="ADOBE-SYMBOL", &
                            map="symbol")
        call define_charset("ibm850", &
                            "DOS codepage 850 (Latin-1)", status, &
                            code_space=[0, 255], &
                            short_name="IBM850", &
                            long_name="IBM850", &
                            ascii_compatible_p=.true., &
                            map="IBM850")
        call define_charset_alias("cp850", "ibm850", status)
        call define_charset("mik", &
                            "Bulgarian DOS codepage", status, &
                            code_space=[0, 255], &
                            short_name="MIK", &
                            long_name="MIK", &
                            ascii_compatible_p=.true., &
                            map="MIK")
        call define_charset("ptcp154", &
                            "ParaType codepage (Asian Cyrillic)", status, &
                            code_space=[0, 255], &
                            short_name="PT154", &
                            long_name="PT154", &
                            ascii_compatible_p=.true., &
                            map="PTCP154")
        call define_charset_alias("pt154", "ptcp154", status)
        call define_charset_alias("cp154", "ptcp154", status)
        call define_charset("gb18030-2-byte", &
                            "GB18030 2-byte (0x814E..0xFEFE)", status, &
                            code_space=[64, 254, 129, 254], &
                            short_name="gb18030-2-byte", &
                            long_name="gb18030-2-byte", &
                            supplementary_p=.true., &
                            map="GB180302")
        call define_charset("gb18030-4-byte-bmp", &
                            "GB18030 4-byte for BMP (0x81308130-0x8431A439)", status, &
                            code_space=[48, 57, 129, 254, 48, 57, 129, 132], &
                            short_name="gb18030-4-byte-bmp", &
                            long_name="gb18030-4-byte-bmp", &
                            supplementary_p=.true., &
                            map="GB180304")
        call define_charset("gb18030-4-byte-smp", &
                            "GB18030 4-byte for SMP (0x90308130-0xE3329A35)", status, &
                            code_space=[48, 57, 129, 254, 48, 57, 144, 227], &
                            short_name="gb18030-4-byte-smp", &
                            long_name="gb18030-4-byte-smp", &
                            min_code=2419097904_int64, &
                            max_code=3811744309_int64, &
                            supplementary_p=.true., &
                            code_offset=65536)
        call define_charset("gb18030-4-byte-ext-1", &
                            "GB18030 4-byte (0x8431A530-0x8F39FE39)", status, &
                            code_space=[48, 57, 129, 254, 48, 57, 132, 143], &
                            short_name="gb18030-4-byte-ext-1", &
                            long_name="gb18030-4-byte-ext-1", &
                            min_code=2217846064_int64, &
                            max_code=2402942521_int64, &
                            supplementary_p=.true., &
                            code_offset=2097152)
        call define_charset("gb18030-4-byte-ext-2", &
                            "GB18030 4-byte (0xE3329A36-0xFE39FE39)", status, &
                            code_space=[48, 57, 129, 254, 48, 57, 227, 254], &
                            short_name="gb18030-4-byte-ext-2", &
                            long_name="gb18030-4-byte-ext-2", &
                            min_code=3811744310_int64, &
                            max_code=4265213497_int64, &
                            supplementary_p=.true., &
                            code_offset=2246732)
        call define_charset("gb18030", &
                            "GB18030", status, &
                            code_space=[0, 255, 0, 254, 0, 254, 0, 254], &
                            short_name="gb18030", &
                            long_name="gb18030", &
                            min_code=0_int64, &
                            max_code=4265213497_int64, &
                            superset=[character(len=CHARSET_NAME_LEN) :: "ascii", "gb18030-2-byte", "gb18030-4-byte-bmp", &
                                      "gb18030-4-byte-smp", "gb18030-4-byte-ext-1", "gb18030-4-byte-ext-2"])
        call define_charset("chinese-cns11643-15", &
                            "CNS11643 Plane 15 Chinese Traditional", status, &
                            code_space=[33, 126, 33, 126], &
                            short_name="CNS11643-15", &
                            long_name="CNS11643-15 (Chinese traditional)", &
                            code_offset=2623546, &
                            unify_map="CNS-F")
        call unify_charset("chinese-gb2312", status)
        call unify_charset("chinese-gbk", status)
        call unify_charset("chinese-cns11643-1", status)
        call unify_charset("chinese-cns11643-2", status)
        call unify_charset("chinese-cns11643-3", status)
        call unify_charset("chinese-cns11643-4", status)
        call unify_charset("chinese-cns11643-5", status)
        call unify_charset("chinese-cns11643-6", status)
        call unify_charset("chinese-cns11643-7", status)
        call unify_charset("chinese-cns11643-15", status)
        call unify_charset("big5", status)
        call unify_charset("chinese-big5-1", status)
        call unify_charset("chinese-big5-2", status)
        call unify_charset("big5-hkscs", status)
        call unify_charset("korean-ksc5601", status)
        call unify_charset("vietnamese-viscii-lower", status)
        call unify_charset("vietnamese-viscii-upper", status)
        call unify_charset("chinese-sisheng", status)
        call unify_charset("ipa", status)
        call unify_charset("tibetan", status)
        call unify_charset("ethiopic", status)
        call unify_charset("indian-is13194", status)
        call unify_charset("japanese-jisx0208-1978", status)
        call unify_charset("japanese-jisx0208", status)
        call unify_charset("japanese-jisx0212", status)
        call unify_charset("japanese-jisx0213-1", status)
        call unify_charset("japanese-jisx0213-2", status)
    end subroutine define_standard_charsets
end submodule mule_conf_charsets_fm
