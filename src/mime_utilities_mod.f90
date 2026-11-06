! mime_utilities_mod.f90 -- utility functions for Mule and low level things.
! Ported from GNU Emacs 31.1 lisp/gnus/mm-util.el (in part: the MIME
! charsets and coding systems rfc2047.el needs).
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> MIME charsets and the coding systems behind them: which coding system
!> decodes a MIME charset (`mime_character_set_to_coding_system') and which MIME
!> charset encodes a region (`find_mime_character_set_region').  What is not
!> ported yet is listed in docs/status/mime_utilities_mod.md.
module mime_utilities_mod
    use error_mod, only: status_type, set_error, VOID_FUNCTION_CODE
    use buffer_mod, only: buffers, current
    use editing_functions_mod, only: buffer_substring_no_properties
    use coding_mod, only: is_coding_system, coding_system_mime_character_set, coding_system_base, coding_system_list, &
                          find_coding_systems_text, coding_system_list_entry_type
    use multilingual_environment_mod, only: coding_system_from_name, current_language_environment
    use search_mod, only: string_match
    use character_mod, only: string_character_and_length, is_character_byte8
    use display_engine_mod, only: message
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none(type, external)
    private

    public :: mime_character_set_to_coding_system, find_mime_character_set_region

    !> A MIME charset name and the coding system that stands for it, as
    !> `mime_character_set_synonyms' and `mime_character_set_overrides' hold them.
    type, public :: mime_character_set_coding_type
        character(len=24) :: character_set = ""
        character(len=24) :: coding_system = ""
    end type mime_character_set_coding_type

    !> A mapping from unknown or invalid charset names to the real charset
    !> names (unallocated: computed on first use from the coding systems
    !> Fortmacs has).  Port of mm-util.el mm-charset-synonym-alist.
    type(mime_character_set_coding_type), allocatable, public :: mime_character_set_synonyms(:)

    !> A mapping from undesired charset names to their replacement.
    !>
    !> You may add entries like mime_character_set_coding_type("iso-8859-1",
    !> "windows-1252") here, i.e. treat iso-8859-1 as windows-1252.
    !> windows-1252 is a superset of iso-8859-1.  Port of mm-util.el
    !> mm-charset-override-alist.
    type(mime_character_set_coding_type), allocatable, public :: mime_character_set_overrides(:)

    !> Preferred coding systems for encoding outgoing messages.
    !>
    !> More than one suitable coding system may be found for some text.  By
    !> default, the coding system with the highest priority is used to
    !> encode outgoing messages (see `sort_coding_systems').  If this
    !> variable is set, it overrides the default priority (unallocated:
    !> computed on first use, from `current_language_environment').
    !> Port of mm-util.el mm-coding-system-priorities.
    character(len=24), allocatable, public :: mime_coding_system_priorities(:)

    !> Use `find_coding_systems_region' to find proper coding systems.
    !>
    !> Setting it to .false. is useful on Fortmacs supporting Unicode if
    !> sending mail with multiple parts is preferred to sending a Unicode
    !> one.  Port of mm-util.el mm-use-find-coding-systems-region.
    logical, public :: mime_use_find_coding_systems_region = .true.

contains

    ! Give the variables computed when upstream loads mm-util.el their
    ! values, if unallocated.  Port of the defvars of
    ! mm-util.el:mm-charset-synonym-alist, mm-charset-override-alist and
    ! mm-coding-system-priorities.
    subroutine initialize_defaults()
        if (.not. allocated(mime_character_set_synonyms)) call make_synonyms()

        if (.not. allocated(mime_character_set_overrides)) then
            mime_character_set_overrides = [mime_character_set_coding_type("gb2312", "gbk"), &
                                            mime_character_set_coding_type("iso-8859-1", "windows-1252"), &
                                            mime_character_set_coding_type("iso-8859-8", "windows-1255"), &
                                            mime_character_set_coding_type("iso-8859-9", "windows-1254")]
        end if

        if (.not. allocated(mime_coding_system_priorities)) then
            ! Japanese users prefer iso-2022-jp to others usually used for
            ! `buffer_file_coding_system', however iso-8859-1 should be used
            ! when there are only ASCII and Latin-1 characters.
            if (is_japanese()) then
                mime_coding_system_priorities = [character(len=24) :: "iso-8859-1", "iso-2022-jp", "utf-8"]
            else
                allocate (mime_coding_system_priorities(0))
            end if
        end if
    end subroutine initialize_defaults

    ! Whether the language environment is a Japanese one.  Not a port.
    logical function is_japanese()
        is_japanese = .false.

        if (allocated(current_language_environment%value)) then
            is_japanese = index(current_language_environment%value, "Japanese") == 1
        end if
    end function is_japanese

    ! The charset synonyms of the coding systems Fortmacs lacks.  Port of
    ! the value of mm-util.el mm-charset-synonym-alist.
    subroutine make_synonyms()
        type(mime_character_set_coding_type) :: entries(12)
        integer :: n

        n = 0
        ! Not in XEmacs, but it's not a proper MIME charset anyhow.
        if (.not. is_coding_system("x-ctext")) call add("x-ctext", "ctext")
        ! ISO-8859-15 is very similar to ISO-8859-1.  But it's _different_
        ! in 8 positions!
        if (.not. is_coding_system("iso-8859-15")) call add("iso-8859-15", "iso-8859-1")
        ! BIG-5HKSCS is similar to, but different from, BIG-5.
        if (.not. is_coding_system("big5-hkscs")) call add("big5-hkscs", "big5")
        ! A Microsoft misunderstanding.
        if (.not. is_coding_system("unicode") .and. is_coding_system("utf-16-le")) call add("unicode", "utf-16-le")

        ! A Microsoft misunderstanding.
        if (.not. is_coding_system("ks_c_5601-1987")) then
            if (is_coding_system("cp949")) then
                call add("ks_c_5601-1987", "cp949")
            else
                call add("ks_c_5601-1987", "euc-kr")
            end if
        end if

        ! Windows-31J is Windows Codepage 932.
        if (.not. is_coding_system("windows-31j") .and. is_coding_system("cp932")) call add("windows-31j", "cp932")
        ! Charset name: GBK, Charset aliases: CP936, MS936, windows-936
        ! https://www.iana.org/assignments/charset-reg/GBK
        if (.not. is_coding_system("gbk") .and. is_coding_system("cp936")) call add("gbk", "cp936")
        ! UTF8 is a bogus name for UTF-8
        if (.not. is_coding_system("utf8") .and. is_coding_system("utf-8")) call add("utf8", "utf-8")
        ! ISO8859-1 is a bogus name for ISO-8859-1
        if (.not. is_coding_system("iso8859-1") .and. is_coding_system("iso-8859-1")) call add("iso8859-1", "iso-8859-1")
        ! ISO_8859-1 is a bogus name for ISO-8859-1
        if (.not. is_coding_system("iso_8859-1") .and. is_coding_system("iso-8859-1")) call add("iso_8859-1", "iso-8859-1")
        mime_character_set_synonyms = entries(1:n)

    contains

        ! Add CHARACTER_SET and CODING_SYSTEM.
        subroutine add(character_set, coding_system)
            character(len=*), intent(in) :: character_set, coding_system

            n = n + 1
            entries(n) = mime_character_set_coding_type(character_set, coding_system)
        end subroutine add
    end subroutine make_synonyms

    ! TEXT with ASCII upper-case letters in lower case.  Not a port.
    pure function ascii_downcase(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower
        integer :: k

        lower = text

        do k = 1, len(text)
            if (lge(text(k:k), "A") .and. lle(text(k:k), "Z")) lower(k:k) = achar(iachar(text(k:k)) + 32)
        end do
    end function ascii_downcase

    !> Return the coding system corresponding to CHARACTER_SET, a MIME charset
    !> name (any case), or "" if Fortmacs has none.  US-ASCII (and the
    !> names matching "ansi.x3.4") gives "ascii", which is not a coding
    !> system.
    !>
    !> If ALLOW_OVERRIDE is true, use `mime_character_set_overrides' to map
    !> undesired charset names to their replacement.  This should only be
    !> used for decoding, not for encoding.
    !>
    !> A charset no coding system stands for shows "Unknown charset: NAME".
    !> Port of mm-util.el:mm-charset-to-coding-system.
    function mime_character_set_to_coding_system(character_set, allow_override) result(coding_system)
        character(len=*), intent(in) :: character_set
        logical, intent(in), optional :: allow_override
        character(len=:), allocatable :: coding_system
        type(coding_system_list_entry_type), allocatable :: names(:)
        type(status_type) :: st
        character(len=:), allocatable :: name, from_name
        integer :: k

        call initialize_defaults()
        name = ascii_downcase(character_set)
        coding_system = ""
        if (len(name) == 0) return

        ! Check override list quite early.  Should only used for decoding,
        ! not for encoding!
        if (present(allow_override)) then
            if (allow_override) then
                k = findloc(mime_character_set_overrides%character_set == name, .true., dim=1)

                if (k > 0) then
                    if (len_trim(mime_character_set_overrides(k)%coding_system) > 0) then
                        if (is_coding_system(trim(mime_character_set_overrides(k)%coding_system))) then
                            coding_system = trim(mime_character_set_overrides(k)%coding_system)
                            return
                        end if
                    end if
                end if
            end if
        end if

        ! ascii
        if (name == "us-ascii" .or. string_match("ansi.x3.4", name, st) > 0) then
            coding_system = "ascii"
            return
        end if

        ! Check to see whether we can handle this charset.  (This depends on
        ! there being some coding system matching each `mime-charset'
        ! property defined, as there should be.)
        if (is_coding_system(name)) then
            coding_system = name
            return
        end if

        ! Use coding system Fortmacs knows.
        from_name = coding_system_from_name(name)

        if (len(from_name) > 0) then
            coding_system = from_name
            return
        end if

        ! Translate invalid charsets.
        k = findloc(mime_character_set_synonyms%character_set == name, .true., dim=1)

        if (k > 0) then
            if (len_trim(mime_character_set_synonyms(k)%coding_system) > 0) then
                if (is_coding_system(trim(mime_character_set_synonyms(k)%coding_system))) then
                    coding_system = trim(mime_character_set_synonyms(k)%coding_system)
                    return
                end if
            end if
        end if

        ! Last resort: search the coding system list for entries which have
        ! the right mime-charset in case the canonical name isn't defined
        ! (though it should be).
        call coding_system_list(names)

        do k = 1, size(names)
            if (coding_system_mime_character_set(names(k)%name) == name) then
                coding_system = names(k)%name
                return
            end if
        end do

        ! Warn the user about unknown charset:
        call message("Unknown charset: "//name)
    end function mime_character_set_to_coding_system

    !> Return the MIME charsets needed to encode the region between B and E
    !> in CHARACTER_SETS: none means ASCII, one element represents an appropriate
    !> MIME charset, and more elements mean no appropriate charset.  Text
    !> that no coding system with a MIME charset encodes and that is not
    !> ASCII or raw bytes is a `void_function' error condition in STATUS
    !> (mm-find-charset-region and mm-mime-charset are not ported yet).
    !> Port of mm-util.el:mm-find-mime-charset-region (no HACK-CHARSETS).
    subroutine find_mime_character_set_region(b, e, character_sets, status)
        integer(int64), intent(in) :: b, e
        character(len=:), allocatable, intent(out) :: character_sets(:)
        type(status_type), intent(out) :: status
        type(coding_system_list_entry_type), allocatable :: systems(:)
        character(len=:), allocatable :: text, character_set
        integer, allocatable :: ranks(:), order(:)
        integer :: k, j, rank
        logical :: all

        call initialize_defaults()
        allocate (character(len=0) :: character_sets(0))
        call buffer_substring_no_properties(b, e, text, status)
        if (.not. status%ok()) return

        if (buffers(current)%multibyte .and. mime_use_find_coding_systems_region) then
            ! Find the mime-charset of the most preferred coding system
            ! that has one.
            call find_coding_systems_text(text, systems, all)

            if (.not. all) then
                ! A stable sort by `mime_coding_system_priorities'
                ! (invalid entries are dropped silently).
                allocate (ranks(size(systems)), order(size(systems)))

                do k = 1, size(systems)
                    ranks(k) = huge(0)

                    do j = 1, size(mime_coding_system_priorities)
                        if (len_trim(mime_coding_system_priorities(j)) == 0) cycle
                        if (.not. is_coding_system(trim(mime_coding_system_priorities(j)))) cycle

                        if (coding_system_base(trim(mime_coding_system_priorities(j))) &
                            == coding_system_base(systems(k)%name)) then
                            ranks(k) = j
                            exit
                        end if
                    end do
                end do

                order = [(k, k=1, size(systems))]

                do k = 2, size(order)
                    j = k
                    rank = order(k)
                    do while (j > 1)
                        if (ranks(order(j - 1)) <= ranks(rank)) exit
                        order(j) = order(j - 1)
                        j = j - 1
                    end do

                    order(j) = rank
                end do

                do k = 1, size(order)
                    if (systems(order(k))%name == "compound-text") cycle
                    character_set = coding_system_mime_character_set(systems(order(k))%name)
                    if (len(character_set) == 0) cycle
                    ! The mime-charset (`x-ctext') of `compound-text' is not
                    ! in the IANA list.  We shouldn't normally use anything
                    ! here with a mime-charset having an `x-' prefix.  Also
                    ! UTF-16 of any variety is invalid for text parts.
                    if (index(character_set, "x-") == 1 .or. index(character_set, "X-") == 1) cycle
                    if (index(character_set, "utf-16") > 0) cycle
                    deallocate (character_sets)
                    allocate (character(len=len(character_set)) :: character_sets(1))
                    character_sets(1) = character_set
                    return
                end do
            end if
        end if

        ! We're not multibyte, or a single coding system won't cover it:
        ! ASCII (and raw bytes) need no charset.
        if (.not. is_ascii_or_raw(text)) call set_error(status, VOID_FUNCTION_CODE, "mm-find-charset-region")
    end subroutine find_mime_character_set_region

    ! Whether TEXT, in the current buffer's representation, holds only
    ! ASCII characters and, in a multibyte buffer, raw bytes.  Not a port.
    logical function is_ascii_or_raw(text)
        character(len=*), intent(in) :: text
        integer(int32) :: c, length
        integer(int64) :: k

        is_ascii_or_raw = .false.
        k = 1

        do while (k <= len(text))
            if (iachar(text(k:k)) < 128) then
                k = k + 1
                cycle
            end if

            if (.not. buffers(current)%multibyte) return
            call string_character_and_length(text, k, c, length)
            if (.not. is_character_byte8(c)) return
            k = k + length
        end do

        is_ascii_or_raw = .true.
    end function is_ascii_or_raw
end module mime_utilities_mod
