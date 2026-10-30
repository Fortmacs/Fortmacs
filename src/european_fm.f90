! european_fm.f90 -- the european coding systems of type `charset'.
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
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) european_fm
    use mule_fm, only: set_language_info_alist
    use syntax_fm, only: modify_syntax_entry, standard_syntax_table
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of european.el.
    module subroutine syms_of_european()
        call define_charset_coding_system("iso-latin-2", iachar("2"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-2"], &
                                          mime_charset="iso-8859-2", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-2 (MIME:ISO-8859-2).")
        call define_coding_system_alias("iso-8859-2", "iso-latin-2")
        call define_coding_system_alias("latin-2", "iso-latin-2")
        call define_charset_coding_system("iso-latin-3", iachar("3"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-3"], &
                                          mime_charset="iso-8859-3", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-3 (MIME:ISO-8859-3).")
        call define_coding_system_alias("iso-8859-3", "iso-latin-3")
        call define_coding_system_alias("latin-3", "iso-latin-3")
        call define_charset_coding_system("iso-latin-4", iachar("4"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-4"], &
                                          mime_charset="iso-8859-4", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-4 (MIME:ISO-8859-4).")
        call define_coding_system_alias("iso-8859-4", "iso-latin-4")
        call define_coding_system_alias("latin-4", "iso-latin-4")
        call define_charset_coding_system("iso-latin-5", iachar("9"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-9"], &
                                          mime_charset="iso-8859-9", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-5 (MIME:ISO-8859-9).")
        call define_coding_system_alias("iso-8859-9", "iso-latin-5")
        call define_coding_system_alias("latin-5", "iso-latin-5")
        call define_charset_coding_system("iso-latin-6", iachar("9"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-10"], &
                                          mime_charset="iso-8859-10", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-6 (MIME:ISO-8859-10)" &
                                          //".")
        call define_coding_system_alias("iso-8859-10", "iso-latin-6")
        call define_coding_system_alias("latin-6", "iso-latin-6")
        call define_charset_coding_system("iso-latin-7", iachar("9"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-13"], &
                                          mime_charset="iso-8859-13", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-7 (MIME:ISO-8859-13)" &
                                          //".")
        call define_coding_system_alias("iso-8859-13", "iso-latin-7")
        call define_coding_system_alias("latin-7", "iso-latin-7")
        call define_charset_coding_system("iso-latin-8", iachar("W"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-14"], &
                                          mime_charset="iso-8859-14", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-8 (MIME:ISO-8859-14)" &
                                          //".")
        call define_coding_system_alias("iso-8859-14", "iso-latin-8")
        call define_coding_system_alias("latin-8", "iso-latin-8")
        call define_charset_coding_system("iso-latin-9", iachar("0"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-15"], &
                                          mime_charset="iso-8859-15", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-9 (MIME:ISO-8859-15)" &
                                          //".")
        call define_coding_system_alias("iso-8859-15", "iso-latin-9")
        call define_coding_system_alias("latin-9", "iso-latin-9")
        call define_coding_system_alias("latin-0", "iso-latin-9")
        call define_charset_coding_system("windows-1250", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "windows-1250"], &
                                          mime_charset="windows-1250", &
                                          doc="windows-1250 (Central European) encoding (MIME: WINDOWS-1250" &
                                          //")")
        call define_coding_system_alias("cp1250", "windows-1250")
        call define_charset_coding_system("windows-1252", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "windows-1252"], &
                                          mime_charset="windows-1252", &
                                          doc="windows-1252 (Western European) encoding (MIME: WINDOWS-1252" &
                                          //")")
        call define_coding_system_alias("cp1252", "windows-1252")
        call define_charset_coding_system("windows-1254", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "windows-1254"], &
                                          mime_charset="windows-1254", &
                                          doc="windows-1254 (Turkish) encoding (MIME: WINDOWS-1254)")
        call define_coding_system_alias("cp1254", "windows-1254")
        call define_charset_coding_system("windows-1257", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "windows-1257"], &
                                          mime_charset="windows-1257", &
                                          doc="windows-1257 (Baltic) encoding (MIME: WINDOWS-1257)")
        call define_coding_system_alias("cp1257", "windows-1257")
        call define_charset_coding_system("ibm256", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm256"], &
                                          doc="Netherlands version of EBCDIC")
        call define_coding_system_alias("ebcdic-int1", "ibm256")
        call define_coding_system_alias("cp256", "ibm256")
        call define_charset_coding_system("ibm273", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm273"], &
                                          doc="Austrian / German version of EBCDIC")
        call define_coding_system_alias("cp273", "ibm273")
        call define_charset_coding_system("ibm274", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm274"], &
                                          doc="Belgian version of EBCDIC")
        call define_coding_system_alias("ebcdic-be", "ibm274")
        call define_coding_system_alias("cp274", "ibm274")
        call define_charset_coding_system("ibm275", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm275"], &
                                          doc="Brazilian version of EBCDIC")
        call define_coding_system_alias("ebcdic-br", "ibm275")
        call define_coding_system_alias("cp275", "ibm275")
        call define_charset_coding_system("ibm277", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm277"], &
                                          doc="Danish / Norwegian version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-dk", "ibm277")
        call define_coding_system_alias("ebcdic-cp-no", "ibm277")
        call define_coding_system_alias("cp277", "ibm277")
        call define_charset_coding_system("ibm278", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm278"], &
                                          doc="Finnish / Swedish version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-fi", "ibm278")
        call define_coding_system_alias("ebcdic-cp-se", "ibm278")
        call define_coding_system_alias("cp278", "ibm278")
        call define_charset_coding_system("ibm280", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm280"], &
                                          doc="Italian version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-it", "ibm280")
        call define_coding_system_alias("cp280", "ibm280")
        call define_charset_coding_system("ibm284", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm284"], &
                                          doc="Spanish version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-es", "ibm284")
        call define_coding_system_alias("cp284", "ibm284")
        call define_charset_coding_system("ibm285", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm285"], &
                                          doc="UK English version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-gb", "ibm285")
        call define_coding_system_alias("cp285", "ibm285")
        call define_charset_coding_system("ibm297", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm297"], &
                                          doc="French version of EBCDIC")
        call define_coding_system_alias("ebcdic-cp-fr", "ibm297")
        call define_coding_system_alias("cp297", "ibm297")
        call define_charset_coding_system("cp775", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp775"], &
                                          mime_charset="cp775", &
                                          doc="DOS codepage 775 (PC Baltic, MS-DOS Baltic Rim)")
        call define_coding_system_alias("ibm775", "cp775")
        call define_charset_coding_system("cp850", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp850"], &
                                          mime_charset="cp850", &
                                          doc="DOS codepage 850 (Western European)")
        call define_coding_system_alias("ibm850", "cp850")
        call define_charset_coding_system("cp852", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp852"], &
                                          mime_charset="cp852", &
                                          doc="DOS codepage 852 (Slavic)")
        call define_coding_system_alias("ibm852", "cp852")
        call define_charset_coding_system("cp857", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp857"], &
                                          mime_charset="cp857", &
                                          doc="DOS codepage 857 (Turkish)")
        call define_coding_system_alias("ibm857", "cp857")
        call define_charset_coding_system("cp858", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp858"], &
                                          mime_charset="cp858", &
                                          doc="Codepage 858 (Multilingual Latin I + Euro)")
        call define_charset_coding_system("cp860", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp860"], &
                                          mime_charset="cp860", &
                                          doc="DOS codepage 860 (Portuguese)")
        call define_coding_system_alias("ibm860", "cp860")
        call define_charset_coding_system("cp861", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp861"], &
                                          mime_charset="cp861", &
                                          doc="DOS codepage 861 (Icelandic)")
        call define_coding_system_alias("ibm861", "cp861")
        call define_charset_coding_system("cp863", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp863"], &
                                          mime_charset="cp863", &
                                          doc="DOS codepage 863 (French Canadian)")
        call define_coding_system_alias("ibm863", "cp863")
        call define_charset_coding_system("cp865", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp865"], &
                                          mime_charset="cp865", &
                                          doc="DOS codepage 865 (Norwegian/Danish)")
        call define_coding_system_alias("ibm865", "cp865")
        call define_charset_coding_system("cp437", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp437"], &
                                          mime_charset="cp437", &
                                          doc="DOS codepage 437")
        call define_coding_system_alias("ibm437", "cp437")
        call define_charset_coding_system("mac-roman", iachar("M"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "mac-roman"], &
                                          mime_charset="macintosh", &
                                          doc="Mac Roman Encoding (MIME:MACINTOSH).")
        call define_coding_system_alias("macintosh", "mac-roman")
        call define_charset_coding_system("next", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "next"], &
                                          mime_charset="next", &
                                          doc="NeXTstep encoding")
        call define_charset_coding_system("hp-roman8", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "hp-roman8"], &
                                          mime_charset="hp-roman8", &
                                          doc="Hewlet-Packard roman-8 encoding (MIME:ROMAN-8)")
        call define_coding_system_alias("roman8", "hp-roman8")
        call define_charset_coding_system("adobe-standard-encoding", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "adobe-standard-encoding"], &
                                          mime_charset="adobe-standard-encoding", &
                                          doc="Adobe `standard' encoding for PostScript")
    end subroutine syms_of_european

    !> Port of the language environments of european.el (set-language-info-alist).
    module subroutine language_info_european()
        call set_language_info_alist("Latin-1", &
                    documentation="This language environment is a generic one for the Latin-1 (" &
                    //"ISO-8859-1)"//achar(10)//"character set which supports the following Europ" &
                    //"ean languages:"//achar(10)//" Albanian, Basque, Breton, Catalan, Danish, D" &
                    //"utch, English, Faeroese,"//achar(10)//" Finnish, French (with restrictions" &
                    //" -- see Latin-9), Frisian, Galician,"//achar(10)//" German, Greenlandic, I" &
                    //"celandic, Irish Gaelic (new orthography),"//achar(10)//" Italian, Latin, L" &
                    //"uxemburgish, Norwegian, Portuguese, Rhaeto-Romanic,"//achar(10)//" Scottis" &
                    //"h Gaelic, Spanish, and Swedish."//achar(10)//"We also have specific langua" &
                    //"ge environments for the following languages:"//achar(10)//"  For Dutch, ""D" &
                    //"utch""."//achar(10)//"  For German, ""German""."//achar(10)//"  For French, &
&                            ""French""."//achar(10)//"  For" &
                    //" Italian, ""Italian""."//achar(10)//"  For Slovenian, ""Slovenian""."//achar(10)//"  For Spa" &
                    //"nish, ""Spanish""."//achar(10)//achar(10)//"Latin-1 also covers several written langua" &
                    //"ges outside Europe, including"//achar(10)//"Indonesian/Malay, Tagalog (Phi" &
                    //"lippines), Swahili and Afrikaans.", &
                    charsets=[character(len=40) :: "iso-8859-1"], &
                    sample_text="Hello, Hej, Tere, Hei, Bonjour, Grüß Gott, Ciao, ¡Hola!", &
                    coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9", "windows-1252"], &
                    coding_priority=[character(len=40) :: "iso-latin-1"], &
                    nonascii_translation="iso-8859-1", &
                    input_method="latin-1-prefix", &
                    unibyte_display="iso-latin-1")
        call set_language_info_alist("Latin-2", &
                                     documentation="This language environment is a generic one for the Latin-2 (" &
                                     //"ISO-8859-2)"//achar(10)//"character set which supports the following langu" &
                                     //"ages:"//achar(10)//" Albanian, Czech, English, German, Hungarian, Polish, " &
                                     //"Romanian,"//achar(10)//" Serbo-Croatian or Croatian, Slovak, Slovene, Sorb" &
                                     //"ian (upper and lower),"//achar(10)//" and Swedish."//achar(10)//"We also have specific l" &
                                     //"anguage environments for the following languages:"//achar(10)//"  For Czec" &
                                 //"h, ""Czech""."//achar(10)//"  For Croatian, ""Croatian""."//achar(10)//"  For Polish, ""Polis" &
                                     //"h""."//achar(10)//"  For Romanian, ""Romanian""."//achar(10)//"  For Slovak, ""Slovak"".", &
                                     charsets=[character(len=40) :: "iso-8859-2"], &
                                     coding_systems=[character(len=40) :: "iso-latin-2", "windows-1250"], &
                                     coding_priority=[character(len=40) :: "iso-latin-2"], &
                                     nonascii_translation="iso-8859-2", &
                                     input_method="latin-2-prefix", &
                                     unibyte_display="iso-latin-2")
        call set_language_info_alist("Latin-3", &
                                     documentation="These languages are supported with the Latin-3 (ISO-8859-3) " &
                                     //"character set:"//achar(10)//" Afrikaans, Catalan, Dutch, English, Esperant" &
                                     //"o, French, Galician,"//achar(10)//" German, Italian, Maltese, Spanish, and" &
                                     //" Turkish.", &
                                     charsets=[character(len=40) :: "iso-8859-3"], &
                                     coding_systems=[character(len=40) :: "iso-latin-3"], &
                                     coding_priority=[character(len=40) :: "iso-latin-3"], &
                                     nonascii_translation="iso-8859-3", &
                                     input_method="latin-3-prefix", &
                                     unibyte_display="iso-latin-3")
        call set_language_info_alist("Latin-4", &
                                     documentation="These languages are supported with the Latin-4 (ISO-8859-4) " &
                                     //"character set:"//achar(10)//" Danish, English, Estonian, Finnish, German, " &
                                     //"Greenlandic, Latvian,"//achar(10)//" Lithuanian, Norwegian, and Sami.", &
                                     charsets=[character(len=40) :: "iso-8859-4"], &
                                     coding_systems=[character(len=40) :: "iso-8859-4"], &
                                     coding_priority=[character(len=40) :: "iso-8859-4"], &
                                     nonascii_translation="iso-8859-4", &
                                     input_method="latin-4-postfix", &
                                     unibyte_display="iso-8859-4")
        call set_language_info_alist("Latin-5", &
                                     documentation="Support for Latin-5.See also the Turkish environment.", &
                                     charsets=[character(len=40) :: "iso-8859-9"], &
                                     coding_systems=[character(len=40) :: "iso-latin-5"], &
                                     coding_priority=[character(len=40) :: "iso-latin-5"], &
                                     nonascii_translation="iso-8859-9", &
                                     input_method="latin-5-postfix", &
                                     unibyte_display="iso-latin-5")
        call set_language_info_alist("Latin-6", &
                                     documentation="Support for Latin-6.", &
                                     charsets=[character(len=40) :: "iso-8859-10"], &
                                     coding_systems=[character(len=40) :: "latin-6"], &
                                     coding_priority=[character(len=40) :: "latin-6"], &
                                     nonascii_translation="nil", &
                                     input_method="latin-prefix", &
                                     unibyte_display="iso-latin-6")
        call set_language_info_alist("Latin-7", &
                                     documentation="Support for Latin-7, e.g. Latvian, Lithuanian.", &
                                     charsets=[character(len=40) :: "iso-8859-13"], &
                                     coding_systems=[character(len=40) :: "latin-7"], &
                                     coding_priority=[character(len=40) :: "latin-7"], &
                                     nonascii_translation="iso-8859-13", &
                                     input_method="latin-prefix", &
                                     unibyte_display="iso-latin-7")
        call set_language_info_alist("Latin-8", &
                                     documentation="This language environment is a generic one for the Latin-8 (" &
                                     //"ISO-8859-14)"//achar(10)//"character set which supports the Celtic languag" &
                                     //"es, including those not"//achar(10)//"covered by other ISO-8859 character " &
                                     //"sets:"//achar(10)//" Welsh, Manx Gaelic and Irish Gaelic (old orthography)" &
                                     //".", &
                                     charsets=[character(len=40) :: "iso-8859-14"], &
                                     sample_text="ḃ ŵ Ŷ", &
                                     coding_systems=[character(len=40) :: "iso-latin-8"], &
                                     coding_priority=[character(len=40) :: "iso-latin-8"], &
                                     nonascii_translation="iso-8859-14", &
                                     input_method="latin-8-prefix", &
                                     unibyte_display="iso-latin-8")
        call set_language_info_alist("Latin-9", &
                                     documentation="This language environment is a generic one for the Latin-9 (" &
                                     //"ISO-8859-15)"//achar(10)//"character set which supports the same languages" &
                                     //" as Latin-1 with the"//achar(10)//"addition of the Euro sign and some addi" &
                                     //"tional French and Finnish letters."//achar(10)//"Latin-9 is sometimes nick" &
                                     //"named `Latin-0'.", &
                                     charsets=[character(len=40) :: "iso-8859-15"], &
                                     sample_text="AVE. ŠšŽžŒœŸ €", &
                                     coding_systems=[character(len=40) :: "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-9"], &
                                     nonascii_translation="iso-8859-15", &
                                     input_method="latin-9-prefix", &
                                     unibyte_display="iso-latin-9")
        call set_language_info_alist("Esperanto", &
                                     documentation="Support for Esperanto with ISO-8859-3 character set.", &
                                     charsets=[character(len=40) :: "iso-8859-3"], &
                                     coding_systems=[character(len=40) :: "iso-latin-3"], &
                                     coding_priority=[character(len=40) :: "iso-latin-3"], &
                                     nonascii_translation="latin-iso8859-3", &
                                     input_method="latin-3-prefix", &
                                     unibyte_display="iso-latin-3", &
                                     tutorial="TUTORIAL.eo")
        call set_language_info_alist("Dutch", &
                                     documentation="This language environment is almost the same as Latin-1,"//achar(10)//"but" &
                                     //" it selects the Dutch tutorial and input method.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="Er is een aantal manieren waarop je dit kan doen", &
                                     coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="dutch", &
                                     unibyte_display="iso-latin-1", &
                                     tutorial="TUTORIAL.nl")
        call set_language_info_alist("German", &
                                     documentation="This language environment is almost the same as Latin-1,"//achar(10)//"but" &
                                     //" sets the default input method to ""german-postfix""."//achar(10)//"Addition" &
                                     //"ally, it selects the German tutorial.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="German (Deutsch Nord)"//achar(9)//"Guten Tag"//achar(10) &
                                     //"German (Deutsch Süd)" &
                                     //achar(9)//"Grüß Go" &
                                     //"tt", &
                                     coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="german-postfix", &
                                     unibyte_display="iso-latin-1", &
                                     tutorial="TUTORIAL.de")
        call set_language_info_alist("French", &
                                     documentation="This language environment is almost the same as Latin-1,"//achar(10)//"but" &
                                     //" it selects the French tutorial and input method.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="French (Français)"//achar(9)//"Bonjour, Salut", &
                                     coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="latin-1-prefix", &
                                     unibyte_display="iso-latin-1", &
                                     tutorial="TUTORIAL.fr")
        call set_language_info_alist("Italian", &
                                     documentation="This language environment is almost the same as Latin-1,"//achar(10)//"but" &
                                     //" sets the default input method to ""italian-postfix""."//achar(10)//"Additio" &
                                     //"nally, it selects the Italian tutorial.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="Salve, ciao!", &
                                     coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="italian-postfix", &
                                     unibyte_display="iso-latin-1", &
                                     tutorial="TUTORIAL.it")
        call set_language_info_alist("Slovenian", &
                                     documentation="This language environment is almost the same as Latin-2,"//achar(10)//"but" &
                                     //" it selects the Slovenian tutorial and input method.", &
                                     charsets=[character(len=40) :: "iso-8859-2"], &
                                     sample_text="Želimo vam uspešen dan!", &
                                     coding_systems=[character(len=40) :: "iso-8859-2", "windows-1250"], &
                                     coding_priority=[character(len=40) :: "iso-8859-2"], &
                                     nonascii_translation="iso-8859-2", &
                                     input_method="slovenian", &
                                     unibyte_display="iso-8859-2", &
                                     tutorial="TUTORIAL.sl")
        call set_language_info_alist("Spanish", &
                                     documentation="This language environment is almost the same as Latin-1,"//achar(10)//"but" &
                                     //" it sets the default input method to ""spanish-postfix"","//achar(10)//"and " &
                                     //"it selects the Spanish tutorial.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="Spanish (Español)"//achar(9)//"¡Hola!", &
                                     coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="spanish-postfix", &
                                     unibyte_display="iso-latin-1", &
                                     tutorial="TUTORIAL.es")
        call set_language_info_alist("Turkish", &
                                     documentation="Support for Turkish."//achar(10)//"Differs from the Latin-5 environment in" &
                                     //" using the `turkish-postfix' input"//achar(10)//"method and applying Turki" &
                                     //"sh case rules for the characters i, I, ı, İ.", &
                                     charsets=[character(len=40) :: "iso-8859-9"], &
                                     sample_text="Turkish (Türkçe)"//achar(9)//"Esenlikler / Merhaba", &
                                     coding_systems=[character(len=40) :: "iso-latin-5", "windows-1254", "iso-latin-3"], &
                                     coding_priority=[character(len=40) :: "iso-latin-5"], &
                                     nonascii_translation="iso-8859-9", &
                                     input_method="turkish-postfix", &
                                     unibyte_display="iso-latin-5")
        call set_language_info_alist("Polish", &
                                     documentation_t=.true., &
                                     charsets=[character(len=40) :: "iso-8859-2"], &
                                     sample_text="Pójdź, kiń-że tę chmurność w głąb flaszy", &
                                     coding_systems=[character(len=40) :: "iso-8859-2", "windows-1250"], &
                                     coding_priority=[character(len=40) :: "iso-8859-2"], &
                                     nonascii_translation="iso-8859-2", &
                                     input_method="polish-slash", &
                                     unibyte_display="iso-8859-2", &
                                     tutorial="TUTORIAL.pl")
        call set_language_info_alist("Welsh", &
                                     documentation="Support for Welsh, using Unicode.", &
                                     coding_systems=[character(len=40) :: "utf-8", "latin-8"], &
                                     coding_priority=[character(len=40) :: "utf-8", "latin-8"], &
                                     nonascii_translation="iso-8859-14", &
                                     input_method="welsh")
        call set_language_info_alist("Lithuanian", &
                                     documentation="Support for Lithuanian.", &
                                     coding_systems=[character(len=40) :: "latin-7", "windows-1257"], &
                                     coding_priority=[character(len=40) :: "latin-7"], &
                                     nonascii_translation="iso-8859-13", &
                                     input_method="lithuanian-keyboard")
        call set_language_info_alist("Latvian", &
                                     documentation="Support for Latvian.", &
                                     coding_systems=[character(len=40) :: "latin-7", "windows-1257"], &
                                     coding_priority=[character(len=40) :: "latin-7"], &
                                     nonascii_translation="iso-8859-13", &
                                     input_method="latvian-keyboard")
        call set_language_info_alist("Swedish", &
                                     documentation="Support for Swedish", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="Goddag Hej", &
                                     coding_systems=[character(len=40) :: "iso-latin-1"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     unibyte_display="iso-latin-1", &
                                     tutorial="TUTORIAL.sv")
        call set_language_info_alist("Croatian", &
                                     documentation="Support for Croatian with Latin-2 encoding.", &
                                     charsets=[character(len=40) :: "iso-8859-2"], &
                                     coding_systems=[character(len=40) :: "iso-8859-2"], &
                                     coding_priority=[character(len=40) :: "iso-8859-2"], &
                                     nonascii_translation="iso-8859-2", &
                                     input_method="croatian", &
                                     unibyte_display="iso-8859-2")
        call set_language_info_alist("Brazilian Portuguese", &
                                     documentation="Support for Brazilian Portuguese.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="Oi", &
                                     coding_systems=[character(len=40) :: "iso-latin-1", "iso-latin-9"], &
                                     coding_priority=[character(len=40) :: "iso-latin-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="latin-1-prefix", &
                                     unibyte_display="iso-8859-1", &
                                     tutorial="TUTORIAL.pt_BR")
        call set_language_info_alist("Catalan", setup_function=catalan_setup, exit_function=catalan_exit, &
                                     documentation="This language environment uses the ISO-8859-1 character set," &
                                     //achar(10)//"sets the default input method to ""catalan-prefix"", and sets" &
                                     //achar(10)//"the syntax of the middle dot character `·' to word.", &
                                     charsets=[character(len=40) :: "iso-8859-1"], &
                                     sample_text="Catalan (Català)  Avui demà i ahir s'esfullarà una rosa.", &
                                     coding_systems=[character(len=40) :: "iso-8859-1", "iso-8859-15"], &
                                     coding_priority=[character(len=40) :: "iso-8859-1"], &
                                     nonascii_translation="iso-8859-1", &
                                     input_method="catalan-prefix", &
                                     unibyte_display="iso-8859-1")
    end subroutine language_info_european

    !> `setup-function' of the Catalan language environment: make the
    !> middle dot `·' a word constituent.
    subroutine catalan_setup(status)
        type(status_type), intent(out) :: status

        call modify_syntax_entry(183, 183, "w", status, standard_syntax_table())
    end subroutine catalan_setup

    !> `exit-function' of the Catalan language environment: make the
    !> middle dot `·' a symbol constituent again.
    subroutine catalan_exit(status)
        type(status_type), intent(out) :: status

        call modify_syntax_entry(183, 183, "_", status, standard_syntax_table())
    end subroutine catalan_exit
end submodule european_fm
