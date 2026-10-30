! indian_fm.f90 -- the indian language environments.
! Ported from GNU Emacs 31.1 lisp/language/indian.el.
!
! Copyright (C) 1997, 1999, 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
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

!> The language environments indian.el defines.  A submodule of coding_fm,
!> which defines them when mule_fm asks for them.
submodule(coding_fm) indian_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the language environments of indian.el (set-language-info-alist).
    module subroutine language_info_indian()
        call set_language_info_alist("Devanagari", &
                    documentation="Such languages using Devanagari script as Hindi, Marathi and" &
                    //" Nepali"//achar(10)//"are supported in this language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Devanagari (देवनागरी)"//achar(9)//"नमस्ते / नमस्कार", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="devanagari-aiba")
        call set_language_info_alist("Bengali", &
                    documentation="Such languages using Bengali script as Bengali and Assamese"//achar(10) &
                    //"are supported in this language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Bengali (বাংলা)"//achar(9)//"নমস্কার", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="bengali-itrans")
        call set_language_info_alist("Gurmukhi", &
                    documentation="North Indian language Punjabi is supported in this language " &
                    //"environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Gurmukhi (ਗੁਰਮੁਖੀ)"//achar(9)//"ਸਤ ਸ੍ਰੀ ਅਕਾਲ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="punjabi-itrans")
        call set_language_info_alist("Gujarati", &
                    documentation="North Indian language Gujarati is supported in this language" &
                    //" environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Gujarati (ગુજરાતી)"//achar(9)//"નમસ્તે", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="gujarati-itrans")
        call set_language_info_alist("Odia", &
                    documentation="Such languages using the Odia script as Odia, Khonti, and Sa" &
                    //"ntali"//achar(10)//"are supported in this language environment.  (This lan" &
                    //"guage"//achar(10)//"environment was formerly known as ""Oriya"").", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Odia (ଓଡ଼ିଆ)"//achar(9)//"ନମସ୍କାର", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="odia")
        call set_language_info_alist("Oriya", &
                    documentation="Such languages using the Odia script as Odia, Khonti, and Sa" &
                    //"ntali"//achar(10)//"are supported in this language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Odia (ଓଡ଼ିଆ)"//achar(9)//"ନମସ୍କାର", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="odia")
        call set_language_info_alist("Tamil", &
                    documentation="South Indian Language Tamil is supported in this language en" &
                    //"vironment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Tamil (தமிழ்)"//achar(9)//"வணக்கம்", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="tamil-phonetic")
        call set_language_info_alist("Telugu", &
                    documentation="South Indian Language Telugu is supported in this language e" &
                    //"nvironment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Telugu (తెలుగు)"//achar(9)//"నమస్కారం", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="telugu-itrans")
        call set_language_info_alist("Kannada", &
                    documentation="Kannada language and script are supported in this language"//achar(10)//"e" &
                    //"nvironment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Kannada (ಕನ್ನಡ)"//achar(9)//"ನಮಸ್ಕಾರ", &
                    coding_systems=[character(len=40) :: "mule-utf-8"], &
                    coding_priority=[character(len=40) :: "mule-utf-8"], &
                    input_method="kannada-itrans")
        call set_language_info_alist("Malayalam", &
                    documentation="South Indian language Malayalam is supported in this languag" &
                    //"e environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Malayalam (മലയാളം)"//achar(9)//"നമസ്കാരം", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="malayalam-itrans")
        call set_language_info_alist("Brahmi", &
                    documentation="The ancient Brahmi script is supported in this language envi" &
                    //"ronment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Brahmi (𑀩𑁆𑀭𑀸𑀳𑁆𑀫𑀻)"//achar(9)//"𑀦𑀫𑀲𑁆𑀢𑁂", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="brahmi")
        call set_language_info_alist("Kaithi", &
                    documentation="Languages such as Awadhi, Bhojpuri, Magahi and Maithili"//achar(10)//"whic" &
                    //"h used the Kaithi script are supported in this language envi" &
                    //"ronment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Kaithi (𑂍𑂶𑂟𑂲)"//achar(9)//"𑂩𑂰𑂧𑂩𑂰𑂧", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="kaithi")
        call set_language_info_alist("Tirhuta", &
                    documentation="Maithili language and its script Tirhuta are supported in th" &
                    //"is"//achar(10)//"language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Tirhuta (𑒞𑒱𑒩𑒯𑒳𑒞𑒰)"//achar(9) &
                    //"𑒣𑓂𑒩𑒢𑒰𑒧 / 𑒮𑒲𑒞𑒰𑒩𑒰𑒧", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="tirhuta")
        call set_language_info_alist("Sharada", &
                    documentation="Kashmiri language and its script Sharada are supported in th" &
                    //"is"//achar(10)//"language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Sharada (𑆯𑆳𑆫𑆢𑆳)"//achar(9)//"𑆤𑆩𑆱𑇀𑆑𑆳𑆫", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="sharada")
        call set_language_info_alist("Siddham", &
                    documentation="Sanskrit language and one of its script Siddham are supporte" &
                    //"d"//achar(10)//"in this language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Siddham (𑖭𑖰𑖟𑖿𑖠𑖽)"//achar(9)//"𑖡𑖦𑖭𑖿𑖝𑖸", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="siddham")
        call set_language_info_alist("Syloti Nagri", &
                    documentation="Sylheti language and its script Syloti Nagri are supported"//achar(10)//"i" &
                    //"n this language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Syloti Nagri (ꠍꠤꠟꠐꠤ " &
                    //"ꠘꠣꠉꠞꠤ)"//achar(9)//"ꠀꠌ꠆ꠍꠣꠟꠣꠝꠥ ꠀꠟꠣꠁꠇꠥꠝ / " &
                    //"ꠘꠝꠡ꠆ꠇꠣꠞ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="syloti-nagri")
        call set_language_info_alist("Modi", &
                    documentation="Marathi language and one of its script Modi are supported"//achar(10)//"in" &
                    //" this language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Modi (𑘦𑘻𑘚𑘲)"//achar(9)//"𑘡𑘦𑘭𑘿𑘎𑘰𑘨", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="modi")
        call set_language_info_alist("Limbu", &
                    documentation="Limbu language and its script are supported in this"//achar(10)//"language" &
                    //" environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Limbu (ᤕᤠᤰᤌᤢᤱ ᤐᤠᤴ)"//achar(9)//"ᤛᤣᤘᤠᤖᤥ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="limbu")
        call set_language_info_alist("Grantha", &
                    documentation="Languages such as Sanskrit and Manipravalam, when they use t" &
                    //"he"//achar(10)//"Grantha script, are supported in this language environmen" &
                    //"t.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Grantha (𑌗𑍍𑌰𑌨𑍍𑌥)"//achar(9) &
                    //"𑌨𑌮𑌸𑍍𑌤𑍇 / 𑌨𑌮𑌸𑍍𑌕𑌾"//"𑌰𑌃", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="grantha")
        call set_language_info_alist("Lepcha", &
                    documentation="Lepcha language and its script are supported in this"//achar(10)//"languag" &
                    //"e environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Lepcha (ᰛᰩᰵᰛᰧᰵᰶ)"//achar(9)//"ᰂᰦᰕᰥᰬ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="lepcha")
        call set_language_info_alist("Meetei Mayek", &
                    documentation="Meetei language and its script Meetei Mayek are supported in" &
                    //" this"//achar(10)//"language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Meetei Mayek (ꯃꯤꯇꯩ ꯃꯌꯦꯛ)"//achar(9)//"ꯈꯨꯔꯨꯝꯖꯔꯤ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="meetei-mayek")
        call set_language_info_alist("Wancho", &
                    documentation="Wancho language and its script are supported in this languag" &
                    //"e"//achar(10)//"environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Wancho (𞋒𞋀𞋉𞋃𞋕)"//achar(9)//"𞋂𞋈𞋛", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="wancho")
        call set_language_info_alist("Toto", &
                    documentation="Toto language using the Toto script is supported in this lan" &
                    //"guage"//achar(10)//"environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="toto")
    end subroutine language_info_indian
end submodule indian_fm
