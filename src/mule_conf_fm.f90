! mule_conf_fm.f90 -- the words and characters of password prompts.
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

!> The options of mule-conf.el used outside the character sets:
!> `password_word_equivalents' and `password_colon_equivalents', which
!> Comint uses to recognize password prompts.
!>
!> Deferred: the rest of mule-conf.el (the charsets and coding systems it
!> defines, roadmap step 41).
module mule_conf_fm
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none(type, external)
    private

    public :: syms_of_mule_conf

    !> List of words equivalent to "password".
    !> This is used by Shell mode and other parts of Emacs to recognize
    !> password prompts, including prompts in languages other than
    !> English.  Different case choices should not be assumed to be
    !> included; callers should bind `case_fold_search' to true.  Port of
    !> mule-conf.el password-word-equivalents.
    character(len=48), allocatable, public :: password_word_equivalents(:)

    !> List of characters equivalent to trailing colon in "password"
    !> prompts.  Port of mule-conf.el password-colon-equivalents.
    integer(int32), allocatable, public :: password_colon_equivalents(:)

contains

    !> Give `password_word_equivalents' and `password_colon_equivalents'
    !> their default values (unless set already).
    subroutine syms_of_mule_conf()
        if (.not. allocated(password_word_equivalents)) then
            password_word_equivalents = [character(len=48) :: &
                                        "password", &
                                        "passcode", &
                                        "passphrase", &
                                        "pass phrase", &
                                        "pin", &
                                        "decryption key", &
                                        "encryption key", &  ! From ccrypt.
                                        "암호", &  ! ko
                                        "パスワード", &  ! ja
                                        "ପ୍ରବେଶ ସଙ୍କେତ", &  ! or
                                        "ពាក្យសម្ងាត់", &  ! km
                                        "adgangskode", &  ! da
                                        "contraseña", &  ! es
                                        "contrasenya", &  ! ca
                                        "geslo", &  ! sl
                                        "hasło", &  ! pl
                                        "heslo", &  ! cs, sk
                                        "iphasiwedi", &  ! zu
                                        "jelszó", &  ! hu
                                        "lösenord", &  ! sv
                                        "lozinka", &  ! hr, sr
                                        "mật khẩu", &  ! vi
                                        "mot de passe", &  ! fr
                                        "parola", &  ! tr
                                        "pasahitza", &  ! eu
                                        "passord", &  ! nb
                                        "passwort", &  ! de
                                        "pasvorto", &  ! eo
                                        "salasana", &  ! fi
                                        "senha", &  ! pt
                                        "slaptažodis", &  ! lt
                                        "wachtwoord", &  ! nl
                                        "كلمة السر", &  ! ar
                                        "ססמה", &  ! he
                                        "лозинка", &  ! sr
                                        "пароль", &  ! kk, ru, uk
                                        "गुप्तशब्द", &  ! mr
                                        "शब्दकूट", &  ! hi
                                        "પાસવર્ડ", &  ! gu
                                        "సంకేతపదము", &  ! te
                                        "ਪਾਸਵਰਡ", &  ! pa
                                        "ಗುಪ್ತಪದ", &  ! kn
                                        "கடவுச்சொல்", &  ! ta
                                        "അടയാളവാക്ക്", &  ! ml
                                        "গুপ্তশব্দ", &  ! as
                                        "পাসওয়ার্ড", &  ! bn_IN
                                        "රහස්පදය", &  ! si
                                        "密码", &  ! zh_CN
                                        "密碼"]  ! zh_TW
        end if
        if (.not. allocated(password_colon_equivalents)) then
            password_colon_equivalents = [int(z'003a', int32), & ! COLON
                                          int(z'ff1a', int32), & ! FULLWIDTH COLON
                                          int(z'fe55', int32), & ! SMALL COLON
                                          int(z'fe13', int32), & ! PRESENTATION FORM FOR VERTICAL COLON
                                          int(z'17d6', int32)] ! KHMER SIGN CAMNUC PII KUUH
        end if
    end subroutine syms_of_mule_conf
end module mule_conf_fm
