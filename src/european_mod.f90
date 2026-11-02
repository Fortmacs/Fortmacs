! european_mod.f90 -- the european coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/european.el.
!
! Copyright (C) 1997-1998, 2000-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `charset' that european.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_mod, which defines them when it
!> starts.
submodule(coding_mod) european_mod
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of european.el.
    module subroutine init_european()
        call define_charset_coding_system("iso-latin-2", iachar("2"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-2"], &
                                          mime_charset="iso-8859-2", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-2 (MIME:ISO-8859-2).")
        call define_coding_system_alias("iso-8859-2", "iso-latin-2")
        call define_coding_system_alias("latin-2", "iso-latin-2")
        call define_charset_coding_system("iso-latin-3", iachar("3"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-3"], &
                                          mime_charset="iso-8859-3", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-3 (MIME:ISO-8859-3).")
        call define_coding_system_alias("iso-8859-3", "iso-latin-3")
        call define_coding_system_alias("latin-3", "iso-latin-3")
        call define_charset_coding_system("iso-latin-4", iachar("4"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-4"], &
                                          mime_charset="iso-8859-4", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-4 (MIME:ISO-8859-4).")
        call define_coding_system_alias("iso-8859-4", "iso-latin-4")
        call define_coding_system_alias("latin-4", "iso-latin-4")
        call define_charset_coding_system("iso-latin-5", iachar("9"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-9"], &
                                          mime_charset="iso-8859-9", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-5 (MIME:ISO-8859-9).")
        call define_coding_system_alias("iso-8859-9", "iso-latin-5")
        call define_coding_system_alias("latin-5", "iso-latin-5")
        call define_charset_coding_system("iso-latin-6", iachar("9"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-10"], &
                                          mime_charset="iso-8859-10", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-6 (MIME:ISO-8859-10)" &
                                          //".")
        call define_coding_system_alias("iso-8859-10", "iso-latin-6")
        call define_coding_system_alias("latin-6", "iso-latin-6")
        call define_charset_coding_system("iso-latin-7", iachar("9"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-13"], &
                                          mime_charset="iso-8859-13", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-7 (MIME:ISO-8859-13)" &
                                          //".")
        call define_coding_system_alias("iso-8859-13", "iso-latin-7")
        call define_coding_system_alias("latin-7", "iso-latin-7")
        call define_charset_coding_system("iso-latin-8", iachar("W"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-14"], &
                                          mime_charset="iso-8859-14", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-8 (MIME:ISO-8859-14)" &
                                          //".")
        call define_coding_system_alias("iso-8859-14", "iso-latin-8")
        call define_coding_system_alias("latin-8", "iso-latin-8")
        call define_charset_coding_system("iso-latin-9", iachar("0"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-15"], &
                                          mime_charset="iso-8859-15", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-9 (MIME:ISO-8859-15)" &
                                          //".")
        call define_coding_system_alias("iso-8859-15", "iso-latin-9")
        call define_coding_system_alias("latin-9", "iso-latin-9")
        call define_coding_system_alias("latin-0", "iso-latin-9")
        call define_charset_coding_system("windows-1250", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1250"], &
                                          mime_charset="windows-1250", &
                                          doc="windows-1250 (Central European) encoding (MIME: WINDOWS-1250" &
                                          //")")
        call define_coding_system_alias("cp1250", "windows-1250")
        call define_charset_coding_system("windows-1252", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1252"], &
                                          mime_charset="windows-1252", &
                                          doc="windows-1252 (Western European) encoding (MIME: WINDOWS-1252" &
                                          //")")
        call define_coding_system_alias("cp1252", "windows-1252")
        call define_charset_coding_system("windows-1254", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1254"], &
                                          mime_charset="windows-1254", &
                                          doc="windows-1254 (Turkish) encoding (MIME: WINDOWS-1254)")
        call define_coding_system_alias("cp1254", "windows-1254")
        call define_charset_coding_system("windows-1257", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1257"], &
                                          mime_charset="windows-1257", &
                                          doc="windows-1257 (Baltic) encoding (MIME: WINDOWS-1257)")
        call define_coding_system_alias("cp1257", "windows-1257")
        call define_charset_coding_system("ibm256", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm256"], &
                                          doc="Netherlands version of EBCDIC")
        call define_coding_system_alias("ebcdic-int1", "ibm256")
        call define_coding_system_alias("cp256", "ibm256")
        call define_charset_coding_system("ibm273", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm273"], &
                                          doc="Austrian / German version of EBCDIC")
        call define_coding_system_alias("cp273", "ibm273")
        call define_charset_coding_system("ibm274", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm274"], &
                                          doc="Belgian version of EBCDIC")
        call define_coding_system_alias("ebcdic-be", "ibm274")
        call define_coding_system_alias("cp274", "ibm274")
        call define_charset_coding_system("ibm275", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm275"], &
                                          doc="Brazilian version of EBCDIC")
        call define_coding_system_alias("ebcdic-br", "ibm275")
        call define_coding_system_alias("cp275", "ibm275")
        call define_charset_coding_system("ibm277", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm277"], &
                                          doc="Danish / Norwegian version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-dk", "ibm277")
        call define_coding_system_alias("ebcdic-cp-no", "ibm277")
        call define_coding_system_alias("cp277", "ibm277")
        call define_charset_coding_system("ibm278", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm278"], &
                                          doc="Finnish / Swedish version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-fi", "ibm278")
        call define_coding_system_alias("ebcdic-cp-se", "ibm278")
        call define_coding_system_alias("cp278", "ibm278")
        call define_charset_coding_system("ibm280", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm280"], &
                                          doc="Italian version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-it", "ibm280")
        call define_coding_system_alias("cp280", "ibm280")
        call define_charset_coding_system("ibm284", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm284"], &
                                          doc="Spanish version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-es", "ibm284")
        call define_coding_system_alias("cp284", "ibm284")
        call define_charset_coding_system("ibm285", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm285"], &
                                          doc="UK English version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-gb", "ibm285")
        call define_coding_system_alias("cp285", "ibm285")
        call define_charset_coding_system("ibm297", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm297"], &
                                          doc="French version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-fr", "ibm297")
        call define_coding_system_alias("cp297", "ibm297")
        call define_charset_coding_system("cp775", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp775"], &
                                          mime_charset="cp775", &
                                          doc="DOS codepage 775 (PC Baltic, MS-DOS Baltic Rim)")
        call define_coding_system_alias("ibm775", "cp775")
        call define_charset_coding_system("cp850", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp850"], &
                                          mime_charset="cp850", &
                                          doc="DOS codepage 850 (Western European)")
        call define_coding_system_alias("ibm850", "cp850")
        call define_charset_coding_system("cp852", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp852"], &
                                          mime_charset="cp852", &
                                          doc="DOS codepage 852 (Slavic)")
        call define_coding_system_alias("ibm852", "cp852")
        call define_charset_coding_system("cp857", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp857"], &
                                          mime_charset="cp857", &
                                          doc="DOS codepage 857 (Turkish)")
        call define_coding_system_alias("ibm857", "cp857")
        call define_charset_coding_system("cp858", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp858"], &
                                          mime_charset="cp858", &
                                          doc="Codepage 858 (Multilingual Latin I + Euro)")
        call define_charset_coding_system("cp860", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp860"], &
                                          mime_charset="cp860", &
                                          doc="DOS codepage 860 (Portuguese)")
        call define_coding_system_alias("ibm860", "cp860")
        call define_charset_coding_system("cp861", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp861"], &
                                          mime_charset="cp861", &
                                          doc="DOS codepage 861 (Icelandic)")
        call define_coding_system_alias("ibm861", "cp861")
        call define_charset_coding_system("cp863", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp863"], &
                                          mime_charset="cp863", &
                                          doc="DOS codepage 863 (French Canadian)")
        call define_coding_system_alias("ibm863", "cp863")
        call define_charset_coding_system("cp865", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp865"], &
                                          mime_charset="cp865", &
                                          doc="DOS codepage 865 (Norwegian/Danish)")
        call define_coding_system_alias("ibm865", "cp865")
        call define_charset_coding_system("cp437", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp437"], &
                                          mime_charset="cp437", &
                                          doc="DOS codepage 437")
        call define_coding_system_alias("ibm437", "cp437")
        call define_charset_coding_system("mac-roman", iachar("M"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "mac-roman"], &
                                          mime_charset="macintosh", &
                                          doc="Mac Roman Encoding (MIME:MACINTOSH).")
        call define_coding_system_alias("macintosh", "mac-roman")
        call define_charset_coding_system("next", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "next"], &
                                          mime_charset="next", &
                                          doc="NeXTstep encoding")
        call define_charset_coding_system("hp-roman8", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "hp-roman8"], &
                                          mime_charset="hp-roman8", &
                                          doc="Hewlet-Packard roman-8 encoding (MIME:ROMAN-8)")
        call define_coding_system_alias("roman8", "hp-roman8")
        call define_charset_coding_system("adobe-standard-encoding", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "adobe-standard-encoding"], &
                                          mime_charset="adobe-standard-encoding", &
                                          doc="Adobe `standard' encoding for PostScript")
    end subroutine init_european
end submodule european_mod
