! mule_fm.f90 -- commands and functions for coding systems.
! Ported from GNU Emacs 31.1 lisp/international/mule.el,
! lisp/international/mule-cmds.el and lisp/international/mule-diag.el.
!
! Copyright (C) 1997-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
!   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

!> The coding-system side of Mule: finding the coding system a file asks
!> for (`set_auto_coding', the `coding:' tag, `auto_coding_alist' and
!> `auto_coding_regexp_alist'), setting `buffer_file_coding_system' after
!> a file is read, choosing a coding system that can encode the text
!> being written (`select_safe_coding_system', with its *Warning* buffer
!> and prompt), `prefer_coding_system', and the commands C-x RET f
!> (`set_buffer_file_coding_system'), C-x RET r
!> (`revert_buffer_with_coding_system') and C-h C (`describe_coding_system').
!>
!> Deferred: `universal_coding_system_argument' (C-x RET c: the command
!> loop has no anonymous wrapper commands), the process, terminal,
!> keyboard, selection and file-name coding system commands (C-x RET p,
!> t, k, x, X, F), input methods and language environments (C-x RET l,
!> C-x RET C-\, C-\), `auto_coding_functions' (the xml, html and sgml
!> detection), `enable_character_translation' tags, the help-xref
!> buttons of the *Warning* and *Help* buffers, completion of only the
!> coding systems that encode the buffer first (completion-table-in-turn),
!> `list_coding_systems' and `describe_current_coding_system_briefly'.
module mule_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_CODING_SYSTEM_ERROR, format_message
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, stringp, xstring, make_string, make_list, t_value
    use lread_fm, only: sym
    use character_fm, only: str_to_multibyte
    use data_fm, only: forward_setting, default_value, set_default
    use fns_fm, only: put
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg
    use buffer_fm, only: buffers, cur, buf_bytes, BEG_BYTE, current_buffer, buffer_live_p, set_buffer, &
                         restore_buffer_modified_p
    use keymap_fm, only: define_key, key_parse, make_sparse_keymap
    use search_fm, only: string_match, match_beginning, match_end, case_fold_search
    use minibuf_fm, only: completion_list_table
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use coding_fm, only: syms_of_coding, buffer_file_coding_system, last_coding_system_used, &
                         coding_system_for_read, inhibit_eol_conversion, default_process_coding_system, &
                         file_coding_system_alist, coding_system_p, check_coding_system, coding_system_base, &
                         coding_system_type, coding_system_eol_type, coding_system_subsidiaries, &
                         coding_system_change_eol_conversion, coding_system_change_text_conversion, &
                         coding_system_mnemonic, coding_system_doc_string, coding_system_aliases, coding_system_equal, &
                         coding_system_priority_list, set_coding_system_priority, coding_system_mime_charset, &
                         coding_system_charset_list, coding_system_list, coding_system_prefer_utf_8_p, &
                         coding_system_ascii_compatible_p, find_coding_systems_text, check_coding_systems_text, &
                         coding_system_list_entry_type, unencodable_type, EOL_UNIX, EOL_DOS, EOL_MAC, EOL_UNDECIDED
    use fileio_fm, only: set_auto_coding_function, after_insert_file_set_coding, &
                         select_safe_coding_system_function, file_name_match_p
    use files_fm, only: revert_buffer
    use xdisp_fm, only: message
    use help_fm, only: with_help_window
    implicit none(type, external)
    private

    public :: syms_of_mule, find_auto_coding, set_auto_coding, auto_coding_alist_lookup, after_insert_file_set_coding_1, &
              find_new_buffer_file_coding_system, merge_coding_systems, set_buffer_file_coding_system, &
              revert_buffer_with_coding_system, prefer_coding_system, set_default_coding_systems, &
              select_safe_coding_system, describe_coding_system, describe_current_coding_system, read_coding_system, &
              buffer_file_coding_system_explicit

    !> An entry of `auto_coding_alist' or `auto_coding_regexp_alist'.
    type, public :: auto_coding_type
        character(len=:), allocatable :: pattern, coding
    end type auto_coding_type

    !> Alist of filename patterns vs corresponding coding systems.
    !> Each element looks like (REGEXP . CODING-SYSTEM).
    !> A file whose name matches REGEXP is decoded by CODING-SYSTEM on reading.
    !>
    !> The settings in this alist take priority over `coding:' tags
    !> in the file (see the function `set_auto_coding')
    !> and the contents of `file_coding_system_alist'.  Port of mule.el
    !> auto-coding-alist.
    type(auto_coding_type), allocatable, public :: auto_coding_alist(:)

    !> Alist of patterns vs corresponding coding systems.
    !> Each element looks like (REGEXP . CODING-SYSTEM).
    !> A file whose first bytes match REGEXP is decoded by CODING-SYSTEM on reading.
    !>
    !> The settings in this alist take priority over `coding:' tags
    !> in the file (see the function `set_auto_coding')
    !> and the contents of `file_coding_system_alist'.  Port of mule.el
    !> auto-coding-regexp-alist.
    type(auto_coding_type), allocatable, public :: auto_coding_regexp_alist(:)

    !> If true, a coding system selected automatically is accepted without
    !> asking (the default coding systems that encode the text are always
    !> accepted: ACCEPT-DEFAULT-P has no function here).  Port of
    !> mule-cmds.el select-safe-coding-system-accept-default-p (Deferred:
    !> its function value).
    logical, public :: select_safe_coding_system_accept_default_p = .false.

    !> The most recent coding system specified explicitly by the user when
    !> asked to choose one.  Port of mule-cmds.el
    !> last-coding-system-specified.
    character(len=:), allocatable, public :: last_coding_system_specified

    !> Keymap for Mule (Multilingual environment) specific commands.
    !> Port of mule-cmds.el mule-keymap (the ported commands: f and r).
    type(handle_type), public :: mule_keymap

    !> Per-buffer `buffer_file_coding_system_explicit': the coding systems
    !> the user gave for reading and for writing the buffer's file
    !> (mule.el's permanent-local (READ . WRITE) cons), by buffer serial.
    type :: explicit_type
        integer(int64) :: serial = -1
        character(len=:), allocatable :: read, write
    end type explicit_type
    type(explicit_type), allocatable :: explicit(:)

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), CR = achar(13)

contains

    !> Set up the coding systems and the Mule commands: C-x RET (C-x C-m)
    !> in CTL_X_MAP and C-h C in HELP_MAP.  Port of the top-level forms of
    !> mule.el, mule-cmds.el and mule-diag.el.
    subroutine syms_of_mule(ctl_x_map, help_map)
        type(handle_type), intent(in) :: ctl_x_map, help_map
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call syms_of_coding()
        ! buffer.c: buffer-file-coding-system is local when set, and
        ! permanently local.
        call forward_setting(buffer_file_coding_system, local_if_set=.true.)
        call put(sym("buffer_file_coding_system"), sym("permanent_local"), t_value(), st)
        set_auto_coding_function => set_auto_coding
        after_insert_file_set_coding => after_insert_file_set_coding_1
        select_safe_coding_system_function => select_safe_coding_system

        if (.not. allocated(auto_coding_alist)) then
            allocate (auto_coding_alist(8))
            auto_coding_alist(1) = auto_coding_type("\.\(arc\|zip\|lzh\|lha\|zoo\|[jew]ar\|xpi\|rar\|7z\|squashfs\|"// &
                                                    "ARC\|ZIP\|LZH\|LHA\|ZOO\|[JEW]AR\|XPI\|RAR\|7Z\|SQUASHFS\)\'", &
                                                    "no-conversion-multibyte")
            auto_coding_alist(2) = auto_coding_type("\.\(exe\|EXE\)\'", "no-conversion")
            auto_coding_alist(3) = auto_coding_type("\.\(sx[dmicw]\|odt\|tar\|t[bg]z\)\'", "no-conversion")
            auto_coding_alist(4) = auto_coding_type("\.\(gz\|Z\|bz\|bz2\|xz\|gpg\)\'", "no-conversion")
            auto_coding_alist(5) = auto_coding_type("\.\(jpe?g\|png\|gif\|tiff?\|p[bpgn]m\)\'", "no-conversion")
            auto_coding_alist(6) = auto_coding_type("\.pdf\'", "no-conversion")
            auto_coding_alist(7) = auto_coding_type("/\.editorconfig\'", "utf-8")
            auto_coding_alist(8) = auto_coding_type("/#[^/]+#\'", "utf-8-emacs-unix")
        end if

        if (.not. allocated(auto_coding_regexp_alist)) then
            allocate (auto_coding_regexp_alist(5))
            auto_coding_regexp_alist(1) = auto_coding_type("\`BABYL OPTIONS:[ "//TAB//"]*-\*-[ "//TAB// &
                                                           "]*rmail[ "//TAB//"]*-\*-", "no-conversion")
            auto_coding_regexp_alist(2) = auto_coding_type("\`"//char(int(z'FE'))//char(int(z'FF')), &
                                                           "utf-16be-with-signature")
            auto_coding_regexp_alist(3) = auto_coding_type("\`"//char(int(z'FF'))//char(int(z'FE')), &
                                                           "utf-16le-with-signature")
            auto_coding_regexp_alist(4) = auto_coding_type("\`"//char(int(z'EF'))//char(int(z'BB'))//char(int(z'BF')), &
                                                           "utf-8-with-signature")
            ! Emacs 20-compiled.
            auto_coding_regexp_alist(5) = auto_coding_type("\`;ELC"//achar(20)//achar(0)//achar(0)//achar(0), &
                                                           "emacs-mule")
        end if

        commands%set_buffer_file_coding_system%run => cmd_set_buffer_file_coding_system
        commands%revert_buffer_with_coding_system%run => cmd_revert_buffer_with_coding_system
        commands%prefer_coding_system%run => cmd_prefer_coding_system
        commands%describe_coding_system%run => cmd_describe_coding_system
        commands%describe_current_coding_system%run => cmd_describe_current_coding_system
        call make_sparse_keymap(mule_keymap)
        call bind(mule_keymap, "f", "set_buffer_file_coding_system")
        call bind(mule_keymap, "r", "revert_buffer_with_coding_system")
        ! Keep "C-x C-m ..." for mule specific commands.
        call key_parse("C-m", key, st)
        call define_key(ctl_x_map, key, keymap_value(mule_keymap), st)
        call bind(help_map, "C", "describe_coding_system")
    end subroutine syms_of_mule

    !> MAP as a keymap value, for binding it as a prefix.
    function keymap_value(map) result(v)
        use lisp_fm, only: make_keymap_ref
        type(handle_type), intent(in) :: map
        type(value_type) :: v

        v = make_keymap_ref(map)
    end function keymap_value

    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind

    ! ------------------------------------------------------------------
    ! Coding systems a file specifies
    ! ------------------------------------------------------------------
    !> Return the coding system specified by `auto_coding_alist' for FILENAME.
    !> Port of mule.el:auto-coding-alist-lookup (file names are case
    !> sensitive on Linux).
    function auto_coding_alist_lookup(filename) result(coding)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: coding
        integer :: k

        coding = ""
        if (.not. allocated(auto_coding_alist)) return

        do k = 1, size(auto_coding_alist)
            if (file_name_match_p(auto_coding_alist(k)%pattern, filename)) then
                coding = auto_coding_alist(k)%coding
                return
            end if
        end do
    end function auto_coding_alist_lookup

    !> Lookup `auto_coding_regexp_alist' for TEXT (bytes, as a unibyte
    !> buffer holds them, unless MULTIBYTE).  Port of
    !> mule.el:auto-coding-regexp-alist-lookup.
    function auto_coding_regexp_alist_lookup(text, multibyte) result(coding)
        character(len=*), intent(in) :: text
        logical, intent(in) :: multibyte
        character(len=:), allocatable :: coding, subject
        type(status_type) :: st
        logical :: saved
        integer :: k

        coding = ""
        if (.not. allocated(auto_coding_regexp_alist)) return

        ! The regexps hold bytes: as raw-byte characters on both sides.
        if (multibyte) then
            subject = text
        else
            subject = str_to_multibyte(text)
        end if

        saved = case_fold_search%value
        case_fold_search%value = .true.

        do k = 1, size(auto_coding_regexp_alist)
            if (string_match(str_to_multibyte(auto_coding_regexp_alist(k)%pattern), subject, st, &
                             inhibit_modify=.true.) > 0) then
                coding = auto_coding_regexp_alist(k)%coding
                exit
            end if
        end do

        case_fold_search%value = saved
    end function auto_coding_regexp_alist_lookup

    !> Find a coding system for a file FILENAME of which TEXT follows point.
    !> These bytes should include at least the first 1k of the file
    !> and the last 3k of the file, but the middle may be omitted.
    !>
    !> FILENAME should be an absolute file name
    !> or "" (which means that there is no associated file).
    !> The function checks FILENAME against the variable `auto_coding_alist'.
    !> If FILENAME doesn't match any entries in the variable, it checks the
    !> contents of the current buffer following point against
    !> `auto_coding_regexp_alist'.  If no match is found, it checks for a
    !> `coding:' tag in the first one or two lines following point.  If no
    !> `coding:' tag is found, it checks any local variables list in the last
    !> 3K bytes out of the SIZE bytes.
    !>
    !> If a coding system is specified, CODING is it and SOURCE says by
    !> what it is specified: "auto-coding-alist", "auto-coding-regexp-alist"
    !> or ":coding".  Note that the validity of CODING is not checked;
    !> it's the caller's responsibility to check it.
    !>
    !> If nothing is specified, CODING is "".  Port of
    !> mule.el:find-auto-coding (no `auto_coding_functions', no
    !> `enable_character_translation' and no obsolete `unibyte:' tag).
    subroutine find_auto_coding(filename, text, coding, source, multibyte)
        character(len=*), intent(in) :: filename, text
        character(len=:), allocatable, intent(out) :: coding, source
        logical, intent(in), optional :: multibyte
        character(len=:), allocatable :: head, tail, spec
        integer :: size_, head_end, tail_start, k
        logical :: mb, head_found, tail_found

        mb = .false.
        if (present(multibyte)) mb = multibyte
        source = ""
        coding = auto_coding_alist_lookup(filename)

        if (len(coding) > 0) then
            source = "auto-coding-alist"
            return
        end if

        ! Try using `auto-coding-regexp-alist'.
        coding = auto_coding_regexp_alist_lookup(text, mb)

        if (len(coding) > 0) then
            source = "auto-coding-regexp-alist"
            return
        end if

        size_ = len(text)
        head_end = min(size_, 1024)
        tail_start = max(size_ - 3072, 0)
        head = text(1:head_end)
        tail = text(tail_start + 1:)
        ! Try a short cut by searching for the string "coding:" at the
        ! head and tail of SIZE bytes.
        head_found = index(lower(head), "coding:") > 0
        tail_found = index(lower(tail), "coding:") > 0

        ! At first check the head.
        if (head_found) then
            spec = auto_mode_region(head)
            if (len(spec) > 0) coding = coding_tag(spec)
        end if

        ! If no coding: tag in the head, check the tail.
        if (tail_found .and. len(coding) == 0) coding = local_variables_coding(tail)

        if (len(coding) > 0) then
            ! If the coding-system name ends with "!", remove it.
            k = len(coding)
            if (coding(k:k) == "!") coding = coding(1:k - 1)
            source = ":coding"
        end if
    end subroutine find_auto_coding

    !> The text of the -*- spec of TEXT, between the markers and without
    !> the spaces next to them ("" if none): looked for in the first line,
    !> or the first two when the text starts with "#!" or '\" (exec
    !> interpreter magic).  Port of files.el:set-auto-mode-1 (no
    !> `inhibit_local_variables_regexps').
    function auto_mode_region(text) result(spec)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: spec, line, rest
        integer :: nl, i, j, a, b
        logical :: skip

        spec = ""
        nl = index(text, LF)

        if (nl == 0) then
            line = text
        else
            line = text(1:nl - 1)
        end if

        skip = .false.
        if (len(text) >= 2) skip = text(1:2) == "#!"
        if (len(text) >= 3) skip = skip .or. text(1:3) == "'\"""
        i = index(line, "-*-")

        if (i == 0 .and. skip .and. nl > 0) then
            line = text(nl + 1:)
            nl = index(line, LF)
            if (nl > 0) line = line(1:nl - 1)
            i = index(line, "-*-")
        end if

        if (i == 0) return
        rest = line(i + 3:)
        j = index(rest, "-*-")
        if (j == 0) return
        rest = rest(1:j - 1)
        a = verify(rest, " "//TAB)
        if (a == 0) return
        b = verify(rest, " "//TAB, back=.true.)
        spec = rest(a:b)
    end function auto_mode_region

    !> The coding system the `coding:' tag of -*- spec SPEC names, or "".
    function coding_tag(spec) result(coding)
        character(len=*), intent(in) :: spec
        character(len=:), allocatable :: coding
        type(status_type) :: st
        integer(int64) :: b, e
        logical :: saved

        coding = ""
        saved = case_fold_search%value
        case_fold_search%value = .true.

        if (string_match("\(.*;\)?[ "//TAB//"]*coding:[ "//TAB//"]*\([^ ;]+\)", spec, st) > 0) then
            b = match_beginning(2, st)
            e = match_end(2, st)
            if (st%ok() .and. b > 0 .and. e >= b) coding = spec(b:e - 1)
        end if

        case_fold_search%value = saved
    end function coding_tag

    !> The coding system the local variables list in TAIL (the last 3K
    !> bytes) names, or "".  Here we must pay attention to the case that
    !> the end-of-line is just "\r" and we can't use "^" nor "$" in regexp.
    !> Part of mule.el:find-auto-coding.
    function local_variables_coding(tail) result(coding)
        character(len=*), intent(in) :: tail
        character(len=:), allocatable :: coding, prefix, suffix, re_coding, re_end, rest
        type(status_type) :: st
        integer(int64) :: k, start, pos, endpos
        logical :: saved

        coding = ""
        saved = case_fold_search%value
        case_fold_search%value = .true.
        start = 1
        ! (re-search-forward "[\r\n]\^L" tail-end t)
        k = string_match("["//CR//LF//"]"//achar(12), tail, st)
        if (k > 0) start = match_end(0, st)
        rest = tail(start:)
        k = string_match("["//CR//LF//"]\([^"//CR//LF//"]*\)[ "//TAB//"]*Local Variables:[ "//TAB//"]*\([^"//CR//LF// &
                         "]*\)["//CR//LF//"]", rest, st)

        if (k > 0) then
            ! The prefix is what comes before "local variables:" in its
            ! line.  The suffix is what comes after "local variables:"
            ! in its line.
            prefix = regexp_quote(rest(match_beginning(1, st):match_end(1, st) - 1))
            suffix = regexp_quote(rest(match_beginning(2, st):match_end(2, st) - 1))
            ! (forward-char -1): from the \r or \n ending that line.
            pos = match_end(0, st) - 1
            re_coding = "["//CR//LF//"]"//prefix//"[ "//TAB//"]*coding[ "//TAB//"]*:[ "//TAB//"]*\([^ "//TAB//CR//LF// &
                        "]+\)[ "//TAB//"]*"//suffix//"["//CR//LF//"]"
            re_end = "["//CR//LF//"]"//prefix//"[ "//TAB//"]*End *:[ "//TAB//"]*"//suffix//"["//CR//LF//"]?"
            rest = rest(pos:)
            endpos = len(rest, kind=int64) + 1
            k = string_match(re_end, rest, st)
            if (k > 0) endpos = match_end(0, st)
            rest = rest(1:endpos - 1)
            if (string_match(re_coding, rest, st) > 0) coding = rest(match_beginning(1, st):match_end(1, st) - 1)
        end if

        case_fold_search%value = saved
    end function local_variables_coding

    !> S with the regexp special characters quoted.  Port of
    !> search.c:Fregexp_quote.
    function regexp_quote(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        integer :: i

        q = ""

        do i = 1, len(s)
            if (index("[*.\?+^$", s(i:i)) > 0) q = q//"\"
            q = q//s(i:i)
        end do
    end function regexp_quote

    !> S in lower case (ASCII).
    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i

        t = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") t(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function lower

    !> Return coding system for a file FILENAME whose first bytes HEAD
    !> follow point.
    !> See `find_auto_coding' for how the coding system is found.
    !> Return "" if an invalid coding system is found.
    !>
    !> The variable `set_auto_coding_function' (which see) is set to this
    !> function by default.  Port of mule.el:set-auto-coding.
    subroutine set_auto_coding(filename, head, coding)
        character(len=*), intent(in) :: filename, head
        character(len=:), allocatable, intent(out) :: coding
        character(len=:), allocatable :: source

        call find_auto_coding(filename, head, coding, source)

        if (len(coding) > 0) then
            if (.not. coding_system_p(coding)) coding = ""
        end if
    end subroutine set_auto_coding

    !> The explicit coding systems of the current buffer (an entry is
    !> made if MAKE).
    integer function explicit_index(make) result(k)
        logical, intent(in) :: make
        type(explicit_type), allocatable :: grown(:)
        integer :: n

        k = 0
        if (cur == 0) return
        if (.not. allocated(explicit)) allocate (explicit(0))

        do n = 1, size(explicit)
            if (explicit(n)%serial == buffers(cur)%serial) then
                k = n
                return
            end if
        end do

        if (.not. make) return
        n = size(explicit)
        allocate (grown(n + 1))
        grown(1:n) = explicit
        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%read = ""
        grown(n + 1)%write = ""
        call move_alloc(grown, explicit)
        k = n + 1
    end function explicit_index

    !> The coding systems the user explicitly specified for reading
    !> (READ) and writing (WRITE) the current buffer's file, "" if none.
    !> Port of mule.el buffer-file-coding-system-explicit.
    subroutine buffer_file_coding_system_explicit(read, write)
        character(len=:), allocatable, intent(out) :: read, write
        integer :: k

        read = ""
        write = ""
        k = explicit_index(.false.)
        if (k == 0) return
        read = explicit(k)%read
        write = explicit(k)%write
    end subroutine buffer_file_coding_system_explicit

    !> Set `buffer_file_coding_system' of current buffer after text is inserted.
    !> INSERTED is the number of characters that were inserted, as figured
    !> in the situation before this function.
    !> VISIT true means that we are visiting a file.  Port of
    !> mule.el:after-insert-file-set-coding.
    subroutine after_insert_file_set_coding_1(inserted, visit)
        integer(int64), intent(in) :: inserted
        logical, intent(in) :: visit
        character(len=:), allocatable :: coding
        integer :: k

        if (inserted < 0) return

        if (visit .and. allocated(coding_system_for_read%value)) then
            k = explicit_index(.true.)
            explicit(k)%read = coding_system_for_read%value
            explicit(k)%write = ""
        end if

        if (.not. allocated(last_coding_system_used%value)) return
        if (len(last_coding_system_used%value) == 0) return
        coding = find_new_buffer_file_coding_system(last_coding_system_used%value)
        if (len(coding) > 0) call buffer_file_coding_system%set_local(coding)
    end subroutine after_insert_file_set_coding_1

    !> Return a coding system for a buffer when a file of CODING is inserted.
    !> The local variable `buffer_file_coding_system' of the current buffer
    !> is set to the returned value.
    !> Return "" if there's no need to set `buffer_file_coding_system'.
    !> Port of mule.el:find-new-buffer-file-coding-system.
    function find_new_buffer_file_coding_system(coding) result(new)
        character(len=*), intent(in) :: coding
        character(len=:), allocatable :: new
        character(len=:), allocatable :: bfcs, local_coding, found_coding, new_coding
        integer :: local_eol, found_eol, new_eol
        logical :: local

        new = ""
        if (len(coding) == 0) return
        bfcs = ""
        if (allocated(buffer_file_coding_system%value)) bfcs = buffer_file_coding_system%value
        local = buffer_file_coding_system%local_p()
        ! Get information of `buffer-file-coding-system' in LOCAL-EOL and
        ! LOCAL-CODING.
        local_eol = coding_system_eol_type(bfcs)
        local_coding = ""

        if (len(bfcs) > 0) then
            if (coding_system_type(bfcs) /= "undecided") local_coding = coding_system_base(bfcs)
        end if

        ! The current buffer has already set full coding-system, we had
        ! better not change it.
        if (local .and. local_eol /= EOL_UNDECIDED .and. len(local_coding) > 0) return
        found_eol = coding_system_eol_type(coding)
        ! If EOL conversions are inhibited, force unix eol-type.
        if (found_eol == EOL_UNDECIDED .and. inhibit_eol_conversion%value) found_eol = EOL_UNIX
        found_coding = coding_system_base(coding)
        ! No valid coding information found.
        if (found_eol == EOL_UNDECIDED .and. found_coding == "undecided") return

        ! The local setting takes precedence over the found one.
        if (local) then
            new_coding = local_coding
            if (len(new_coding) == 0) new_coding = found_coding
            new_eol = local_eol
            if (new_eol == EOL_UNDECIDED) new_eol = found_eol
        else
            new_coding = found_coding
            if (len(new_coding) == 0) new_coding = local_coding
            new_eol = found_eol
            if (new_eol == EOL_UNDECIDED) new_eol = local_eol
        end if

        new = new_coding

        if (new_eol /= EOL_UNDECIDED .and. coding_system_eol_type(new_coding) == EOL_UNDECIDED) then
            new = coding_system_change_eol_conversion(new_coding, new_eol)
        end if
    end function find_new_buffer_file_coding_system

    !> Fill in any unspecified aspects of coding system FIRST from SECOND.
    !> Return the resulting coding system.  Port of
    !> mule.el:merge-coding-systems.
    function merge_coding_systems(first, second) result(cs)
        character(len=*), intent(in) :: first, second
        character(len=:), allocatable :: cs
        integer :: eol

        cs = first
        ! If FIRST doesn't specify text conversion, merge with that of SECOND.
        if (coding_system_base(cs) == "undecided") cs = coding_system_change_text_conversion(cs, second)
        ! If FIRST doesn't specify eol conversion, merge with that of SECOND.
        eol = coding_system_eol_type(cs)

        if (eol == EOL_UNDECIDED .and. coding_system_eol_type(second) /= EOL_UNDECIDED) then
            cs = coding_system_change_eol_conversion(cs, coding_system_eol_type(second))
        end if
    end function merge_coding_systems

    ! ------------------------------------------------------------------
    ! Reading coding systems
    ! ------------------------------------------------------------------
    !> Every coding system name, as completion candidates
    !> (Vcoding_system_alist).
    function coding_system_alist() result(list)
        type(value_type) :: list
        type(coding_system_list_entry_type), allocatable :: names(:), aliases(:)
        type(value_type), allocatable :: items(:)
        character(len=40) :: subs(0:2)
        integer :: k, j, i, n

        call coding_system_list(names)
        allocate (items(4*size(names) + 16))
        n = 0

        do k = 1, size(names)
            call coding_system_aliases(names(k)%name, aliases)

            do j = 1, size(aliases)
                if (any_name(items(1:n), aliases(j)%name)) cycle
                n = n + 1
                items(n) = make_string(aliases(j)%name)
            end do

            call coding_system_subsidiaries(names(k)%name, subs)

            do i = 0, 2
                if (len_trim(subs(i)) == 0) cycle
                if (any_name(items(1:n), trim(subs(i)))) cycle
                n = n + 1
                items(n) = make_string(trim(subs(i)))
            end do
        end do

        list = make_list(items(1:n))

    contains

        logical function any_name(vs, name)
            type(value_type), intent(in) :: vs(:)
            character(len=*), intent(in) :: name
            integer :: m

            any_name = .false.

            do m = 1, size(vs)
                if (xstring(vs(m)) == name) then
                    any_name = .true.
                    return
                end if
            end do
        end function any_name
    end function coding_system_alist

    !> Read a coding system from the minibuffer, prompting with string PROMPT.
    !> If the user enters null input, return second argument DEFAULT_CODING_SYSTEM.
    !> Return the coding-system's name, or "" if both the user input and
    !> DEFAULT_CODING_SYSTEM are empty.
    !> Ignores case when completing coding systems (all Emacs coding systems
    !> are lower-case).  Port of coding.c:Fread_coding_system.
    subroutine read_coding_system(prompt, default_coding_system, coding, status)
        character(len=*), intent(in) :: prompt, default_coding_system
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status

        call completing_read(prompt, completion_list_table(coding_system_alist()), coding, status, &
                             require_match=REQUIRE_MATCH_T, history="coding_system_history", &
                             default=default_coding_system)
        if (.not. status%ok()) coding = ""
    end subroutine read_coding_system

    !> The prompt "PROMPT (default DEFAULT): ", or "PROMPT: ".
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function format_prompt

    !> The whole text of the current buffer.
    function buffer_text() result(text)
        character(len=:), allocatable :: text

        text = buf_bytes(buffers(cur), BEG_BYTE, buffers(cur)%text%z_byte)
    end function buffer_text

    !> Whether NAME is in NAMES.
    logical function member(name, names)
        character(len=*), intent(in) :: name
        type(coding_system_list_entry_type), intent(in) :: names(:)
        integer :: k

        member = .false.

        do k = 1, size(names)
            if (names(k)%name == name) then
                member = .true.
                return
            end if
        end do
    end function member

    !> Read the coding system for saving the current buffer, offering as
    !> the default the coding system the buffer's text asks for, or the
    !> first preferred one that encodes it and differs from the current
    !> one.  Port of mule.el:read-buffer-file-coding-system (completion
    !> offers every coding system).
    subroutine read_buffer_file_coding_system(coding, status)
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status
        type(coding_system_list_entry_type), allocatable :: bcss(:), prio(:)
        character(len=:), allocatable :: auto_cs, filename, bfcs_default, current, default, cs
        type(value_type) :: v
        type(status_type) :: vst
        logical :: all, any_safe
        integer :: k

        call find_coding_systems_text(buffer_text(), bcss, all)
        filename = ""
        if (allocated(buffers(cur)%filename)) filename = buffers(cur)%filename
        call set_auto_coding(filename, buffer_text(), auto_cs)
        call default_value(sym("buffer_file_coding_system"), v, vst)
        bfcs_default = ""
        if (stringp(v)) bfcs_default = xstring(v)
        current = ""
        if (allocated(buffer_file_coding_system%value)) current = coding_system_base(buffer_file_coding_system%value)
        ! Generally use as a default the first preferred coding-system
        ! different from the current coding-system, except for the case of
        ! auto-cs since choosing anything else is asking for trouble.
        default = auto_cs

        if (len(default) == 0) then
            any_safe = all .or. member(coding_system_base(bfcs_default), bcss)

            if (len(bfcs_default) > 0 .and. any_safe .and. coding_system_base(bfcs_default) /= current) then
                default = bfcs_default
            else
                call coding_system_priority_list(prio)

                do k = 1, size(prio)
                    cs = prio(k)%name
                    if (len(coding_system_mime_charset(cs)) == 0) cycle
                    if (.not. (all .or. member(coding_system_base(cs), bcss))) cycle
                    if (coding_system_base(cs) == current) cycle
                    default = cs
                    exit
                end do
            end if
        end if

        call completing_read(format_prompt("Coding system for saving file", default), &
                             completion_list_table(coding_system_alist()), coding, status, &
                             require_match=REQUIRE_MATCH_T, history="coding_system_history", default=default)
        if (.not. status%ok()) coding = ""
    end subroutine read_buffer_file_coding_system

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------
    !> Set the file coding-system of the current buffer to CODING-SYSTEM.
    !> This means that when you save the buffer, it will be converted
    !> according to CODING-SYSTEM.  For a list of possible values of
    !> CODING-SYSTEM, use \[list_coding_systems].
    !>
    !> If CODING-SYSTEM leaves the text conversion unspecified, or if it leaves
    !> the end-of-line conversion unspecified, FORCE controls what to do.
    !> If FORCE is false, get the unspecified aspect (or aspects) from the buffer's
    !> previous `buffer_file_coding_system' value (if it is specified there).
    !> Otherwise, leave it unspecified.
    !>
    !> This marks the buffer modified so that the succeeding \[save_buffer]
    !> surely saves the buffer with CODING-SYSTEM.  From a program, if you
    !> don't want to mark the buffer modified, specify true for NOMODIFY.
    !> If you know exactly what coding system you want to use,
    !> just set the variable `buffer_file_coding_system' directly.  Port of
    !> mule.el:set-buffer-file-coding-system (INTERACTIVE: called
    !> interactively, which checks that saving would succeed).
    recursive subroutine set_buffer_file_coding_system(coding_system, force, nomodify, interactive, status)
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: force, nomodify, interactive
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs, chosen
        type(coding_system_list_entry_type), allocatable :: css(:), unsafe(:)
        logical :: all
        integer :: k

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        cs = coding_system

        if (len(cs) > 0 .and. allocated(buffer_file_coding_system%value) .and. .not. force) then
            if (len(buffer_file_coding_system%value) > 0) cs = merge_coding_systems(cs, buffer_file_coding_system%value)
        end if

        if (interactive .and. coding_system_charset_list(cs) /= "emacs") then
            ! Check whether save would succeed, and jump to the offending
            ! char(s) if not.
            call find_coding_systems_text(buffer_text(), css, all)

            if (.not. all .and. .not. member(coding_system_base(cs), css)) then
                allocate (unsafe(1))
                unsafe(1)%name = cs
                call select_safe_coding_system_interactively(1_int64, buffers(cur)%text%z, css, unsafe, "", chosen, &
                                                             status)
                if (.not. status%ok()) return
                cs = chosen
            end if
        end if

        call buffer_file_coding_system%set_local(cs)
        k = explicit_index(.true.)
        explicit(k)%write = cs
        if (.not. nomodify) call restore_buffer_modified_p(.true.)
    end subroutine set_buffer_file_coding_system

    recursive subroutine cmd_set_buffer_file_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs
        logical :: force

        force = .not. nilp(current_prefix_arg)
        call read_buffer_file_coding_system(cs, status)
        if (.not. status%ok()) return
        call set_buffer_file_coding_system(cs, force, .false., .true., status)
    end subroutine cmd_set_buffer_file_coding_system

    !> Visit the current buffer's file again using coding system CODING-SYSTEM.
    !> For a list of possible values of CODING-SYSTEM, use \[list_coding_systems].
    !>
    !> If CODING-SYSTEM leaves the text conversion unspecified, or if it leaves
    !> the end-of-line conversion unspecified, FORCE controls what to do.
    !> If FORCE is false, get the unspecified aspect (or aspects) from the buffer's
    !> previous `buffer_file_coding_system' value (if it is specified there).
    !> Otherwise, determine it from the file contents as usual for visiting a file.
    !> Port of mule.el:revert-buffer-with-coding-system.
    recursive subroutine revert_buffer_with_coding_system(coding_system, force, status)
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: force
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs, saved
        logical :: had

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        cs = coding_system

        if (len(cs) > 0 .and. allocated(buffer_file_coding_system%value) .and. .not. force) then
            if (len(buffer_file_coding_system%value) > 0) cs = merge_coding_systems(cs, buffer_file_coding_system%value)
        end if

        ! (let ((coding-system-for-read coding-system)) (revert-buffer))
        had = allocated(coding_system_for_read%value)
        if (had) saved = coding_system_for_read%value

        if (len(cs) > 0) then
            coding_system_for_read = cs
        else
            call coding_system_for_read%clear()
        end if

        call revert_buffer(status)

        if (had) then
            coding_system_for_read = saved
        else
            call coding_system_for_read%clear()
        end if
    end subroutine revert_buffer_with_coding_system

    recursive subroutine cmd_revert_buffer_with_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs
        logical :: force

        force = .not. nilp(current_prefix_arg)
        call read_coding_system("Coding system for visited file (default nil): ", "", cs, status)
        if (.not. status%ok()) return
        call revert_buffer_with_coding_system(cs, force, status)
    end subroutine cmd_revert_buffer_with_coding_system

    !> Set default value of various coding systems to CODING-SYSTEM.
    !> This sets the following coding systems:
    !>   o coding system of a newly created buffer
    !>   o default coding system for subprocess I/O
    !> Port of mule-cmds.el:set-default-coding-systems (the file name,
    !> terminal and keyboard coding systems are Deferred).
    subroutine set_default_coding_systems(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: output, input

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        call set_default(sym("buffer_file_coding_system"), make_string(coding_system), status)
        if (.not. status%ok()) return
        ! Preserve eol-type from existing default-process-coding-systems.
        output = coding_system_change_text_conversion(trim(default_process_coding_system(1)), coding_system)
        input = coding_system_change_text_conversion(trim(default_process_coding_system(2)), coding_system)
        default_process_coding_system(1) = output
        default_process_coding_system(2) = input
    end subroutine set_default_coding_systems

    !> Add CODING-SYSTEM at the front of the priority list for automatic detection.
    !> This also sets the following coding systems:
    !>   o coding system of a newly created buffer
    !>   o default coding system for subprocess I/O
    !>
    !> If CODING-SYSTEM specifies a certain type of EOL conversion, the coding
    !> systems set by this function will use that type of EOL conversion.
    !>
    !> A coding system that requires automatic detection of text+encoding
    !> (e.g. undecided, unix) can't be preferred.
    !>
    !> To prefer, for instance, utf-8, say the following:
    !>
    !>   call prefer_coding_system("utf-8", status)
    !>
    !> Port of mule-cmds.el:prefer-coding-system.
    subroutine prefer_coding_system(coding_system, status, interactive)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        !> Called interactively: say when the base was preferred.
        logical, intent(in), optional :: interactive
        character(len=:), allocatable :: base, cs, t
        integer :: eol

        if (len(coding_system) == 0 .or. .not. coding_system_p(coding_system)) then
            call set_error(status, FM_GENERIC_ERROR, &
                           format_message("Invalid coding system `")//coding_system//format_message("'"))
            return
        end if

        t = coding_system_type(coding_system)

        if (t == "raw-text" .or. t == "undecided") then
            call set_error(status, FM_GENERIC_ERROR, &
                           format_message("Can't prefer the coding system `")//coding_system//format_message("'"))
            return
        end if

        base = coding_system_base(coding_system)
        eol = coding_system_eol_type(coding_system)
        call set_coding_system_priority([character(len=len(base)) :: base], status)
        if (.not. status%ok()) return

        if (present(interactive)) then
            if (interactive .and. base /= coding_system) then
                call message("Highest priority is set to "//base//" (base of "//coding_system//")")
            end if
        end if

        ! If they asked for specific EOL conversion, honor that.
        cs = base
        if (eol /= EOL_UNDECIDED) cs = coding_system_change_eol_conversion(base, eol)
        call set_default_coding_systems(cs, status)
    end subroutine prefer_coding_system

    recursive subroutine cmd_prefer_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs

        do
            call completing_read("Coding system: ", completion_list_table(coding_system_alist()), cs, status, &
                                 require_match=REQUIRE_MATCH_T, history="coding_system_history")
            if (.not. status%ok()) return
            if (len(cs) > 0) exit
        end do

        call prefer_coding_system(cs, status, interactive=.true.)
    end subroutine cmd_prefer_coding_system

    ! ------------------------------------------------------------------
    ! Choosing a coding system that can encode the text
    ! ------------------------------------------------------------------
    !> Ask a user to select a safe coding system from candidates.
    !> The candidates of coding systems which can safely encode a text
    !> between FROM and TO are shown in a popup window.  Among them, the most
    !> proper one is suggested as the default.
    !>
    !> The list of `buffer_file_coding_system' of the current buffer, the
    !> default `buffer_file_coding_system', and the most preferred coding
    !> system (if it corresponds to a MIME charset) is treated as the
    !> default coding system list.  Among them, the first one that safely
    !> encodes the text is normally selected silently and returned without
    !> any user interaction.  See also the command `prefer_coding_system'.
    !>
    !> However, the user is queried if the chosen coding system is
    !> inconsistent with what would be selected by `find_auto_coding' from
    !> coding cookies &c. if the contents of the region were read from a
    !> file.  (That could lead to data corruption in a file subsequently
    !> re-visited and edited.)
    !>
    !> DEFAULT_CODING_SYSTEM specifies a coding system to be prepended to
    !> the default coding system list.  ONLY_DEFAULT true is upstream's list
    !> whose first element is t: then only DEFAULT_CODING_SYSTEM is the
    !> default list.
    !>
    !> FILE is the file name to use for this purpose.
    !> That is different from `buffer_file_name' when handling `write_region'
    !> (for example).  Port of mule-cmds.el:select-safe-coding-system (no
    !> ACCEPT-DEFAULT-P function, `find_file_literally' or string FROM).
    recursive subroutine select_safe_coding_system(from, to, default_coding_system, only_default, file, coding, status)
        integer(int64), intent(in) :: from, to
        character(len=*), intent(in) :: default_coding_system, file
        logical, intent(in) :: only_default
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status
        type(coding_system_list_entry_type), allocatable :: defaults(:), codings(:), safe(:), unsafe(:), rejected(:), &
                                                            prio(:)
        character(len=:), allocatable :: auto_cs, source, text, fname, base, dflt, read, write, preferred, cs, by
        type(value_type) :: v
        type(status_type) :: vst
        logical :: no_other_defaults, all, ok
        integer :: k, nd, eol, n
        integer(int64) :: tick

        coding = ""
        status = status_type()
        no_other_defaults = only_default
        allocate (defaults(0))
        nd = 0
        if (len(default_coding_system) > 0) call append(defaults, nd, default_coding_system)
        ! Find an auto-coding that is specified for the current buffer and
        ! file from the region FROM and TO.
        text = region_text(from, to)
        fname = file
        if (len(fname) == 0 .and. allocated(buffers(cur)%filename)) fname = buffers(cur)%filename
        call find_auto_coding(fname, text, auto_cs, source, multibyte=buffers(cur)%multibyte)

        if (len(auto_cs) > 0) then
            if (.not. coding_system_p(auto_cs)) then
                if (source == ":coding") then
                    by = ":coding tag"
                else
                    by = format_message("variable `")//source//format_message("'")
                end if

                call message(format_message("Warning (mule): Invalid coding system `")//auto_cs// &
                             format_message("' is specified")//LF//"for the current buffer/file by the "//by//"."//LF// &
                             "It is highly recommended to fix it before writing to a file.")
                call yes_or_no(ok, status)
                if (.not. status%ok()) return

                if (.not. ok) then
                    call set_error(status, FM_GENERIC_ERROR, "Save aborted")
                    return
                end if

                auto_cs = ""
            end if
        end if

        if (len(auto_cs) > 0 .and. .not. no_other_defaults) then
            ! If the file has a coding cookie, use it regardless of any
            ! other setting.
            base = coding_system_base(auto_cs)

            if (base /= "undecided") then
                ! For encoding, no-conversion-multibyte is the same as
                ! no-conversion.
                if (base == "no-conversion-multibyte") then
                    auto_cs = "no-conversion"
                    base = "no-conversion"
                end if

                deallocate (defaults)
                allocate (defaults(0))
                nd = 0
                call append(defaults, nd, auto_cs)
                no_other_defaults = .true.
            end if
        end if

        if (.not. no_other_defaults) then
            ! If buffer-file-coding-system is not nil nor undecided, append
            ! it to the defaults.
            if (allocated(buffer_file_coding_system%value)) then
                if (len(buffer_file_coding_system%value) > 0) then
                    base = coding_system_base(buffer_file_coding_system%value)

                    if (base /= "undecided" .and. .not. has_base(defaults(1:nd), base)) then
                        call append(defaults, nd, buffer_file_coding_system%value)
                    end if
                end if
            end if

            call buffer_file_coding_system_explicit(read, write)

            if (len(write) == 0) then
                ! If default buffer-file-coding-system is not nil nor
                ! undecided, append it to the defaults.
                call default_value(sym("buffer_file_coding_system"), v, vst)
                dflt = ""
                if (stringp(v)) dflt = xstring(v)

                if (len(dflt) > 0) then
                    base = coding_system_base(dflt)
                    if (base /= "undecided" .and. .not. has_base(defaults(1:nd), base)) call append(defaults, nd, dflt)
                end if

                ! If the most preferred coding system has the property
                ! mime-charset, append it to the defaults.
                call coding_system_priority_list(prio, .true.)

                if (size(prio) > 0) then
                    preferred = prio(1)%name
                    base = coding_system_base(preferred)

                    if (len(coding_system_mime_charset(preferred)) > 0 .and. .not. has_base(defaults(1:nd), base)) then
                        call append(defaults, nd, preferred)
                    end if
                end if
            end if
        end if

        ! Decide the eol-type from the top of the default codings, current
        ! buffer-file-coding-system, or default buffer-file-coding-system.
        if (nd > 0) then
            eol = coding_system_eol_type(defaults(1)%name)

            if (eol == EOL_UNDECIDED .and. allocated(buffer_file_coding_system%value)) then
                eol = coding_system_eol_type(buffer_file_coding_system%value)
            end if

            if (eol == EOL_UNDECIDED) then
                call default_value(sym("buffer_file_coding_system"), v, vst)
                if (stringp(v)) eol = coding_system_eol_type(xstring(v))
            end if

            if (eol /= EOL_UNDECIDED) then
                do k = 1, nd
                    defaults(k)%name = coding_system_change_eol_conversion(defaults(k)%name, eol)
                end do
            end if
        end if

        call find_coding_systems_text(text, codings, all)
        tick = buffers(cur)%text%chars_modiff

        if (all) then
            ! Any coding system is ok.
            if (nd > 0) coding = defaults(1)%name
        else
            ! Classify the defaults into safe, rejected, and unsafe.
            allocate (safe(0), unsafe(0), rejected(0))
            n = 0

            do k = 1, nd
                if (member(coding_system_base(defaults(k)%name), codings)) then
                    call append(safe, n, defaults(k)%name)
                end if
            end do

            n = 0

            do k = 1, nd
                if (.not. member(coding_system_base(defaults(k)%name), codings)) call append(unsafe, n, defaults(k)%name)
            end do

            ! If there are safe ones, the first one is what we want.
            if (size(safe) > 0) coding = safe(1)%name

            ! If all the defaults failed, ask a user.
            if (len(coding) == 0) then
                ! If UTF-8 is in CODINGS, but is not its first member, make
                ! it the first one, so it is offered as the default.
                if (member("utf-8", codings)) call to_front(codings, "utf-8")
                cs = ""
                if (size(codings) > 0) cs = codings(1)%name
                call select_safe_coding_system_interactively(from, to, codings, unsafe, cs, coding, status)
                if (.not. status%ok()) return
            end if
        end if

        ! Check we're not inconsistent with what `coding:' spec &c would
        ! give when file is re-read.
        if (len(auto_cs) > 0 .and. .not. (len(coding) > 0 .and. no_other_defaults .and. coding == auto_cs)) then
            call merge_auto_cs(coding, auto_cs)

            if (len(auto_cs) > 0 .and. len(auto_coding_alist_lookup(fname)) == 0 &
                .and. .not. coding_system_equal(coding, auto_cs)) then
                if (coding_system_type(auto_cs) == "charset" .or. coding_system_type(coding) == "charset" &
                    .or. coding_system_type(auto_cs) /= coding_system_type(coding)) then
                    call yes_or_no_prompt("Selected encoding "//coding//" disagrees with "//auto_cs// &
                                          " specified by file contents.  Really save (else edit coding cookies "// &
                                          "and try again)? ", ok, status)
                    if (.not. status%ok()) return

                    if (.not. ok) then
                        call set_error(status, FM_GENERIC_ERROR, "Save aborted")
                        return
                    end if
                end if
            end if
        end if

        if (tick /= buffers(cur)%text%chars_modiff) then
            call set_error(status, FM_GENERIC_ERROR, "Canceled because the buffer was modified")
            return
        end if

        if (len(coding) > 0) then
            if (coding_system_type(coding) == "undecided" .and. coding_system_prefer_utf_8_p(coding) &
                .and. to - from < len(text, kind=int64)) then
                coding = coding_system_change_text_conversion(coding, "utf-8")
            end if
        end if

    contains

        !> Merge CODING and AUTO_CS as far as possible.
        subroutine merge_auto_cs(coding, auto_cs)
            character(len=:), allocatable, intent(inout) :: coding, auto_cs
            integer :: eol1, eol2

            if (len(coding) == 0) then
                coding = auto_cs
                return
            end if

            eol1 = coding_system_eol_type(coding)
            eol2 = coding_system_eol_type(auto_cs)

            if (coding_system_base(coding) == "undecided") then
                coding = coding_system_change_text_conversion(coding, auto_cs)
            else if (coding_system_base(auto_cs) == "undecided") then
                auto_cs = coding_system_change_text_conversion(auto_cs, coding)
            end if

            if (eol1 == EOL_UNDECIDED) then
                if (eol2 /= EOL_UNDECIDED) coding = coding_system_change_eol_conversion(coding, eol2)
            else if (eol2 == EOL_UNDECIDED) then
                auto_cs = coding_system_change_eol_conversion(auto_cs, eol1)
            end if
        end subroutine merge_auto_cs
    end subroutine select_safe_coding_system

    !> Ask "Really proceed with writing? ".
    subroutine yes_or_no(ok, status)
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status

        call yes_or_no_prompt("Really proceed with writing? ", ok, status)
    end subroutine yes_or_no

    subroutine yes_or_no_prompt(prompt, ok, status)
        use fns_fm, only: yes_or_no_p
        character(len=*), intent(in) :: prompt
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status

        call yes_or_no_p(prompt, ok, status)
    end subroutine yes_or_no_prompt

    !> The text between FROM and TO of the current buffer.
    function region_text(from, to) result(text)
        use marker_fm, only: char_to_byte
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable :: text

        text = buf_bytes(buffers(cur), char_to_byte(from), char_to_byte(to))
    end function region_text

    subroutine append(list, n, name)
        type(coding_system_list_entry_type), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: name
        type(coding_system_list_entry_type), allocatable :: grown(:)

        allocate (grown(n + 1))
        grown(1:n) = list(1:n)
        grown(n + 1)%name = name
        call move_alloc(grown, list)
        n = n + 1
    end subroutine append

    !> Whether a coding system in LIST has base BASE (rassq).
    logical function has_base(list, base)
        type(coding_system_list_entry_type), intent(in) :: list(:)
        character(len=*), intent(in) :: base
        integer :: k

        has_base = .false.

        do k = 1, size(list)
            if (coding_system_base(list(k)%name) == base) has_base = .true.
        end do
    end function has_base

    !> Move NAME to the front of LIST.
    subroutine to_front(list, name)
        type(coding_system_list_entry_type), intent(inout) :: list(:)
        character(len=*), intent(in) :: name
        integer :: k, j

        do k = 1, size(list)
            if (list(k)%name == name) exit
        end do

        if (k > size(list) .or. k == 1) return

        do j = k, 2, -1
            list(j)%name = list(j - 1)%name
        end do

        list(1)%name = name
    end subroutine to_front

    !> Select interactively a coding system for the region FROM ... TO.
    !> CODINGS is the list of base coding systems known to be safe for this region,
    !>   typically obtained with `find_coding_systems_region'.
    !> UNSAFE is a list of coding systems known to be unsafe for this region.
    !> DEFAULT is the coding system to use by default in the query.  Port
    !> of mule-cmds.el:select-safe-coding-system-interactively (no
    !> REJECTED coding systems; the problematic characters are not
    !> buttons).
    recursive subroutine select_safe_coding_system_interactively(from, to, codings, unsafe, default, coding, status)
        integer(int64), intent(in) :: from, to
        type(coding_system_list_entry_type), intent(in) :: codings(:), unsafe(:)
        character(len=*), intent(in) :: default
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status
        type(unencodable_type), allocatable :: bad(:)
        character(len=40) :: names(1)
        character(len=:), allocatable :: text, bufname, list_line, line
        integer(int64) :: p, firstpos
        integer :: k, j
        type(status_type) :: st

        coding = ""
        bufname = buffers(cur)%name
        text = "These default coding systems were tried to safely encode the following"//LF// &
               format_message("problematic characters in the buffer `")//bufname//format_message("'")//":"//LF
        firstpos = to

        do k = 1, size(unsafe)
            names(1) = unsafe(k)%name
            call check_coding_systems_text(region_text(from, to), from, names, bad, st)

            if (size(bad) > 0) then
                line = "  "//unsafe(k)%name//":"

                do j = 1, min(11, size(bad(1)%positions))
                    p = bad(1)%positions(j)
                    line = line//" ("//itoa(p)//" . "//char_at(p)//")"
                    firstpos = min(firstpos, p)
                end do

                text = text//line//LF
            end if
        end do

        text = text//"However, each of them encountered characters it couldn"//format_message("'")//"t encode safely:"//LF

        do k = 1, size(unsafe)
            names(1) = unsafe(k)%name
            call check_coding_systems_text(region_text(from, to), from, names, bad, st)
            if (size(bad) == 0) cycle
            line = "  "//unsafe(k)%name//" cannot safely encode these:"

            do j = 1, size(bad(1)%positions)
                if (j <= 10) then
                    line = line//" "//char_at(bad(1)%positions(j))
                else
                    line = line//" ..."
                    exit
                end if
            end do

            text = text//line//LF
        end do

        text = text//LF//"Click on a character (or switch to this window by "//format_message("`C-x o'")//LF// &
               "and select the characters by RET) to jump to the place it appears,"//LF// &
               "where "//format_message("`C-u C-x ='")//" will give information about it."//LF
        text = text//LF//"Select one of the safe coding systems listed below,"//LF// &
               "or cancel the writing with C-g and edit the buffer"//LF// &
               "   to remove or modify the problematic characters,"//LF// &
               "or specify any other coding system (and risk losing"//LF// &
               "   the problematic characters)."//LF//LF
        list_line = ""

        do k = 1, size(codings)
            list_line = list_line//"  "//codings(k)%name
        end do

        text = text//fill_list(list_line)//LF
        call show_warning(text, status)
        if (.not. status%ok()) return
        ! Read a coding system.
        call read_coding_system(format_prompt("Select coding system", default), default, coding, status)
        if (.not. status%ok()) return
        last_coding_system_specified = coding
        call kill_warning()

    contains

        function char_at(pos) result(s)
            use marker_fm, only: char_to_byte
            use character_fm, only: multibyte_length
            integer(int64), intent(in) :: pos
            character(len=:), allocatable :: s
            integer(int64) :: b
            integer :: n

            b = char_to_byte(pos)
            s = buf_bytes(buffers(cur), b, b + 1)
            n = multibyte_length(buf_bytes(buffers(cur), b, min(b + 5, buffers(cur)%text%z_byte)), 1_int64, .false.)
            if (n > 1) s = buf_bytes(buffers(cur), b, b + n)
        end function char_at
    end subroutine select_safe_coding_system_interactively

    !> The coding system names of LINE, filled as fill-region-as-paragraph
    !> fills them with a fill prefix of two spaces (at `fill_column' 70).
    function fill_list(line) result(filled)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: filled, word, current
        integer :: i, j

        filled = ""
        current = " "
        i = 1

        do while (i <= len(line))
            if (line(i:i) == " ") then
                i = i + 1
                cycle
            end if

            j = index(line(i:), " ")

            if (j == 0) then
                word = line(i:)
                i = len(line) + 1
            else
                word = line(i:i + j - 2)
                i = i + j
            end if

            if (len(current) + 1 + len(word) > 70 .and. len(current) > 2) then
                filled = filled//current//LF
                current = " "
            end if

            current = current//" "//word
        end do

        filled = filled//current
    end function fill_list

    !> Show TEXT in the "*Warning*" buffer (with-output-to-temp-buffer).
    subroutine show_warning(text, status)
        use buffer_fm, only: get_buffer_create
        use insdel_fm, only: insert
        use buffer_fm, only: erase_buffer
        use window_fm, only: temp_buffer_window_show
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, previous, window
        type(status_type) :: st

        previous = current_buffer()
        call get_buffer_create("*Warning*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call erase_buffer(status)
        call insert(text, status)
        call temp_buffer_window_show(buf, window, status)
        if (buffer_live_p(previous)) call set_buffer(previous, st)
    end subroutine show_warning

    subroutine kill_warning()
        use buffer_fm, only: get_buffer, kill_buffer
        type(handle_type) :: buf
        type(status_type) :: st

        buf = get_buffer("*Warning*")
        if (buffer_live_p(buf)) call kill_buffer(buf, st)
    end subroutine kill_warning

    function itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    ! ------------------------------------------------------------------
    ! Describing coding systems
    ! ------------------------------------------------------------------
    !> Print name and mnemonic letter of CODING-SYSTEM, and with DOC its
    !> docstring.  Port of mule-diag.el:print-coding-system-briefly.
    function print_coding_system_briefly(coding_system, doc) result(s)
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: doc
        character(len=:), allocatable :: s, alias_list
        type(coding_system_list_entry_type), allocatable :: aliases(:)
        character(len=40) :: subs(0:2)
        integer :: k, eol

        if (len(coding_system) == 0) then
            s = "nil"//LF
            return
        end if

        s = achar(coding_system_mnemonic(coding_system))//" -- "//coding_system
        call coding_system_aliases(coding_system, aliases)

        if (size(aliases) > 0) then
            if (aliases(1)%name == coding_system) then
                if (size(aliases) > 1) then
                    alias_list = ""

                    do k = 2, size(aliases)
                        alias_list = alias_list//" "//aliases(k)%name
                    end do

                    s = s//" (alias:"//alias_list//")"
                end if
            else if (member(coding_system, aliases)) then
                s = s//" (alias of "//aliases(1)%name//")"
            else
                eol = coding_system_eol_type(coding_system)
                call coding_system_subsidiaries(aliases(1)%name, subs)

                if (eol /= EOL_UNDECIDED .and. len_trim(subs(0)) > 0) then
                    if (trim(subs(eol)) /= coding_system) s = s//" (alias of "//trim(subs(eol))//")"
                end if
            end if
        end if

        s = s//LF
        if (doc) s = s//LF//coding_system_doc_string(coding_system)//LF
    end function print_coding_system_briefly

    !> Display information about CODING-SYSTEM.  Port of
    !> mule-diag.el:describe-coding-system (no xref buttons for charsets).
    recursive subroutine describe_coding_system(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s, t, charsets
        character(len=40) :: subs(0:2)

        if (len(coding_system) == 0) then
            call describe_current_coding_system(status)
            return
        end if

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        s = print_coding_system_briefly(coding_system, .true.)
        t = coding_system_type(coding_system)
        s = s//"Type: "//t

        select case (t)
        case ("undecided")
            s = s//" (do automatic conversion)"
        case ("utf-8")
            s = s//" (UTF-8: Emacs internal multibyte form)"
        case ("charset")
            s = s//" (charset)"
        case ("raw-text")
            s = s//" (text with random binary characters)"
        case default
            ! (UTF-16: nothing more.)
            continue
        end select

        s = s//LF//"EOL type: "

        select case (coding_system_eol_type(coding_system))
        case (EOL_UNDECIDED)
            call coding_system_subsidiaries(coding_system, subs)
            s = s//"Automatic selection from:"//LF//TAB//"["//trim(subs(0))//" "//trim(subs(1))//" "// &
                trim(subs(2))//"]"//LF
        case (EOL_UNIX)
            s = s//"LF"//LF
        case (EOL_DOS)
            s = s//"CRLF"//LF
        case default
            s = s//"CR"//LF
        end select

        charsets = coding_system_charset_list(coding_system)

        if (coding_system_base(coding_system) /= "raw-text" .and. len(charsets) > 0) then
            s = s//"This coding system encodes the following charsets:"//LF//"  "//charsets
        end if

        call with_help_window(s, status)
    end subroutine describe_coding_system

    recursive subroutine cmd_describe_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs

        call read_coding_system("Describe coding system (default current choices): ", "", cs, status)
        if (.not. status%ok()) return
        call describe_coding_system(cs, status)
    end subroutine cmd_describe_coding_system

    !> Display coding systems currently used, in detail.  Port of
    !> mule-diag.el:describe-current-coding-system (the keyboard and
    !> terminal coding systems are those of a UTF-8 terminal; no process
    !> coding systems of the buffer's process; the patterns are shown as
    !> Fortran strings).
    recursive subroutine describe_current_coding_system(status)
        use print_fm, only: fortran_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s, dflt, aliases_text, pair
        type(coding_system_list_entry_type), allocatable :: prio(:), aliases(:)
        type(value_type) :: v
        type(status_type) :: vst
        integer :: k, j

        s = "Coding system for saving this buffer:"//LF//"  "

        if (buffer_file_coding_system%local_p() .and. allocated(buffer_file_coding_system%value)) then
            s = s//print_coding_system_briefly(buffer_file_coding_system%value, .false.)
        else
            s = s//"Not set locally, use the default."//LF
        end if

        call default_value(sym("buffer_file_coding_system"), v, vst)
        dflt = ""
        if (stringp(v)) dflt = xstring(v)
        s = s//"Default coding system (for new files):"//LF//"  "//print_coding_system_briefly(dflt, .false.)
        s = s//"Coding system for keyboard input:"//LF//"  "//print_coding_system_briefly("utf-8-unix", .false.)
        s = s//"Coding system for terminal output:"//LF//"  "//print_coding_system_briefly("utf-8-unix", .false.)
        s = s//"Defaults for subprocess I/O:"//LF
        s = s//"  decoding: "//print_coding_system_briefly(trim(default_process_coding_system(1)), .false.)
        s = s//"  encoding: "//print_coding_system_briefly(trim(default_process_coding_system(2)), .false.)
        s = s//LF//"Priority order for recognizing coding systems when reading files:"//LF
        call coding_system_priority_list(prio)

        do k = 1, size(prio)
            s = s//"  "//itoa(int(k, int64))//". "//prio(k)%name//" "
            call coding_system_aliases(prio(k)%name, aliases)

            if (size(aliases) > 0) then
                if (aliases(1)%name == prio(k)%name) then
                    if (size(aliases) > 1) then
                        aliases_text = "(alias:"

                        do j = 2, size(aliases)
                            aliases_text = aliases_text//" "//aliases(j)%name
                        end do

                        s = s//aliases_text//")"
                    end if
                else
                    s = s//"(alias of "//aliases(1)%name//")"
                end if
            end if

            s = s//LF
        end do

        s = s//LF//"  Other coding systems cannot be distinguished automatically"//LF// &
            "  from these, and therefore cannot be recognized automatically"//LF// &
            "  with the present coding system priorities."//LF//LF
        s = s//"Particular coding systems specified for certain file names:"//LF//LF
        s = s//"  OPERATION"//TAB//"TARGET PATTERN"//TAB//TAB//"CODING SYSTEM(s)"//LF
        s = s//"  ---------"//TAB//"--------------"//TAB//TAB//"----------------"//LF
        s = s//"  File I/O"

        do k = 1, size(auto_coding_alist)
            s = s//table_row(fortran_string(auto_coding_alist(k)%pattern), auto_coding_alist(k)%coding)
        end do

        do k = 1, size(file_coding_system_alist)
            if (file_coding_system_alist(k)%decoding == file_coding_system_alist(k)%encoding) then
                pair = file_coding_system_alist(k)%decoding
            else if (len(file_coding_system_alist(k)%encoding) == 0) then
                pair = "("//file_coding_system_alist(k)%decoding//" . nil)"
            else
                pair = "("//file_coding_system_alist(k)%decoding//" . "//file_coding_system_alist(k)%encoding//")"
            end if

            s = s//table_row(fortran_string(file_coding_system_alist(k)%pattern), pair)
        end do

        s = s//"  Process I/O"//TAB//"nothing specified"//LF
        s = s//"  Network I/O"//TAB//"nothing specified"//LF
        call with_help_window(s, status)

    contains

        !> A row: the pattern from column 16 and the coding systems from
        !> column 40 (a pattern reaching column 40 gets a line of its own).
        function table_row(pattern, codings) result(row)
            character(len=*), intent(in) :: pattern, codings
            character(len=:), allocatable :: row
            integer :: col

            col = current_col()
            row = indent_to(col, 16)//pattern
            col = max(col, 16) + len(pattern)

            if (col >= 40) then
                row = row//LF
                col = 0
            end if

            row = row//indent_to(col, 40)//codings//LF
        end function table_row

        !> The column at the end of S.
        integer function current_col()
            integer :: nl, i

            nl = index(s, LF, back=.true.)
            current_col = 0

            do i = nl + 1, len(s)
                if (s(i:i) == TAB) then
                    current_col = (current_col/8 + 1)*8
                else
                    current_col = current_col + 1
                end if
            end do
        end function current_col

        !> Tabs and spaces from column FROM to column TO (indent-to with
        !> `indent_tabs_mode' true), or "" if FROM is beyond TO.
        function indent_to(from, to) result(ws)
            integer, intent(in) :: from, to
            character(len=:), allocatable :: ws
            integer :: col

            ws = ""
            col = from

            do while ((col/8 + 1)*8 <= to)
                ws = ws//TAB
                col = (col/8 + 1)*8
            end do

            if (col < to) ws = ws//repeat(" ", to - col)
        end function indent_to
    end subroutine describe_current_coding_system

    recursive subroutine cmd_describe_current_coding_system(status)
        type(status_type), intent(out) :: status

        call describe_current_coding_system(status)
    end subroutine cmd_describe_current_coding_system
end module mule_fm
